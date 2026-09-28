#include "robotstxtbing/policy.h"

#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "robotstxtbing/limits.h"
#include "robotstxtbing/metadata.h"
#include "robotstxtbing/result.h"

#include "access_grouping.h"
#include "candidate_rules.h"
#include "diagnostic_mapping.h"
#include "matcher.h"
#include "metadata_extraction.h"
#include "parser.h"
#include "selection.h"
#include "winner.h"

namespace robotstxtbing {
namespace {

// Maps the internal parser status onto its same-named public parse status. The
// switch is exhaustive with no default so that adding a ParserStatus fails to
// compile here rather than silently relying on enum declaration order; the
// terminal return is unreachable and only satisfies -Wreturn-type.
ParseStatus public_status(detail::ParserStatus status) noexcept {
  switch (status) {
    case detail::ParserStatus::parsed:
      return ParseStatus::parsed;
    case detail::ParserStatus::body_limit_exceeded:
      return ParseStatus::body_limit_exceeded;
    case detail::ParserStatus::line_length_limit_exceeded:
      return ParseStatus::line_length_limit_exceeded;
    case detail::ParserStatus::record_limit_exceeded:
      return ParseStatus::record_limit_exceeded;
    case detail::ParserStatus::rule_limit_exceeded:
      return ParseStatus::rule_limit_exceeded;
  }
  return ParseStatus::parsed;
}

// Locale-independent ASCII case-fold equality (spec 8, 12). Compares `token`
// against a lowercase reference spelling one byte at a time, folding only the
// ASCII letters A-Z on the input side; length must match exactly, so no affix
// or partial ever compares equal. Deliberately avoids std::tolower to stay
// locale-independent and byte-pure.
bool ascii_case_equals(std::string_view token, std::string_view lowercase_ref) {
  if (token.size() != lowercase_ref.size()) {
    return false;
  }
  for (std::size_t i = 0; i < token.size(); ++i) {
    unsigned char byte = static_cast<unsigned char>(token[i]);
    if (byte >= 'A' && byte <= 'Z') {
      byte = static_cast<unsigned char>(byte - 'A' + 'a');
    }
    if (byte != static_cast<unsigned char>(lowercase_ref[i])) {
      return false;
    }
  }
  return true;
}

// Resolves a caller product token to a supported public profile (spec 12).
// Version 0.1 supports exactly the complete tokens `bingbot` and `adidxbot`;
// anything else -- empty, affixed, historical, partial, or whitespace-bearing
// -- is unsupported. No aliases are recognized.
bool is_supported_profile_token(std::string_view product_token) {
  return ascii_case_equals(product_token, "bingbot") ||
         ascii_case_equals(product_token, "adidxbot");
}

// Validates request-target shape (spec 13): valid iff nonempty, first byte is
// `/`, and it contains no `#` fragment delimiter. Byte-transparent -- no
// percent-decoding, normalization, case-folding, dot-segment resolution, query
// stripping, or URL parsing. Absolute-form URLs fail the leading-slash test.
bool is_valid_request_target_shape(std::string_view request_target) {
  if (request_target.empty() || request_target.front() != '/') {
    return false;
  }
  return request_target.find('#') == std::string_view::npos;
}

}  // namespace

// Immutable owned storage. Populated once for a parsed body and never mutated
// afterward, so every copy shares a single const instance and the references
// returned by the metadata accessors stay valid until the owning Policy is
// destroyed or assigned (spec 6.2).
//
// `body` is an OWNED copy of the parsed bytes and `ir` is the retained
// access-grouping IR built over THAT owned copy. Every ClassifiedRecord in `ir`
// holds std::string_view into `body` (via source.payload), so `body` must
// outlive the Impl and must never be moved or reassigned once `ir` is built.
// Because Impl is held by shared_ptr<const Impl> and never mutated after
// construction, its address -- and thus the inline bytes of a small-string-
// optimized `body` -- stays stable for the Policy's lifetime, keeping every
// retained view valid (spec 6.2). Evaluate reads `ir` const-only, seeding a
// fresh local WorkBudget per call, so concurrent Evaluate is data-race free.
struct Policy::Impl {
  std::string body;
  detail::AccessGroupingIr ir;
  std::vector<SitemapEntry> sitemaps;
  std::vector<CrawlDelayEntry> crawl_delays;
};

Policy::Policy(std::shared_ptr<const Impl> impl) noexcept
    : impl_(std::move(impl)) {}

Policy::Policy(const Policy&) noexcept = default;
Policy::Policy(Policy&&) noexcept = default;
Policy& Policy::operator=(const Policy&) noexcept = default;
Policy& Policy::operator=(Policy&&) noexcept = default;
Policy::~Policy() = default;

ParseResult Policy::Parse(std::string_view body) {
  // Own the parsed bytes FIRST, inside the heap Impl, then parse THAT owned copy
  // so every retained ClassifiedRecord view points into impl->body (which
  // outlives the Policy) rather than into the caller's transient `body`. A
  // single parse feeds status, diagnostics, metadata, and the retained IR from
  // identical bytes; the owned copy is only wasted on the rare limit-exceeded
  // error paths, where no Policy is produced. Diagnostics carry byte offsets
  // (not views), so they remain valid once this function returns.
  auto impl = std::make_shared<Impl>();
  impl->body.assign(body.data(), body.size());
  detail::ParserResult parsed = detail::parse_to_ir(impl->body);

  ParseResult result;
  result.status = public_status(parsed.status);
  result.diagnostics = detail::make_diagnostics(parsed.diagnostics);

  // All-or-nothing (spec 6.2): a policy is present only for a fully parsed
  // body. Every limit status yields no policy; the single terminal limit
  // diagnostic is already carried in the seeds, so no partial policy is ever
  // exposed. On the parsed path, MOVE the IR into impl->ir -- its vectors move
  // but the string_views keep pointing into impl->body, which is never moved or
  // reassigned after this point. The metadata extractors deep-copy every value
  // into owned storage, so the metadata accessors stay valid independently.
  if (parsed.status == detail::ParserStatus::parsed) {
    impl->ir = std::move(*parsed.ir);
    impl->sitemaps = detail::extract_sitemaps(impl->ir);
    impl->crawl_delays = detail::extract_crawl_delays(impl->ir);
    result.policy = Policy(std::move(impl));
  }

  return result;
}

EvaluationResult Policy::Evaluate(std::string_view product_token,
                                  std::string_view request_target) const {
  // Fixed validation precedence (spec 13): resolve product_token, validate
  // target shape, then check target size, returning on the first failure. Any
  // non-evaluated status carries an absent decision (spec 6.3) -- never a
  // fabricated verdict and never a silent default allow. The ordering makes an
  // unsupported profile win over a simultaneously invalid target, and an
  // invalid target win over its size.

  // 1. Product token must resolve to a supported public profile.
  if (!is_supported_profile_token(product_token)) {
    return EvaluationResult{EvaluationStatus::unsupported_profile,
                            std::nullopt};
  }

  // 2. Request target must be shaped as a valid origin-form target.
  if (!is_valid_request_target_shape(request_target)) {
    return EvaluationResult{EvaluationStatus::invalid_request_target,
                            std::nullopt};
  }

  // 3. A shape-valid target must still be within the size limit.
  if (request_target.size() > kMaxRequestTargetBytes) {
    return EvaluationResult{EvaluationStatus::request_target_limit_exceeded,
                            std::nullopt};
  }

  // 4. All inputs valid: compute the real decision by composing the accepted
  // units over the retained access-grouping IR. Map the already-validated token
  // to its frozen §8 selection profile (bingbot has a `*` fallback, adidxbot --
  // the only other supported token -- does not), take the U1 candidate access
  // records, drop the inert ones with U5, and run the U4 winner against the
  // request target under a fresh per-call work budget. Reads of impl_->ir are
  // const and the budget is local, so concurrent Evaluate is data-race free.
  const detail::GroupSelectionProfile profile =
      ascii_case_equals(product_token, "bingbot")
          ? detail::bingbot_group_selection_profile()
          : detail::adidxbot_group_selection_profile();
  const std::vector<std::size_t> selected =
      detail::select_candidate_access_records(profile, impl_->ir);
  const std::vector<detail::MatchableRule> rules =
      detail::build_matchable_rules(impl_->ir, selected);
  detail::WorkBudget budget(kMaxMatcherWorkUnits);
  const detail::WinnerOutcome outcome =
      detail::decide_winner(rules, request_target, budget);
  if (outcome.status == detail::WinnerStatus::work_limit_exceeded) {
    return EvaluationResult{EvaluationStatus::work_limit_exceeded,
                            std::nullopt};
  }
  return EvaluationResult{EvaluationStatus::evaluated, outcome.decision};
}

const std::vector<SitemapEntry>& Policy::sitemaps() const noexcept {
  return impl_->sitemaps;
}

const std::vector<CrawlDelayEntry>& Policy::crawl_delays() const noexcept {
  return impl_->crawl_delays;
}

}  // namespace robotstxtbing
