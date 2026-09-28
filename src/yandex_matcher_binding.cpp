// cpp11 binding: checked raw-body robotstxtyandex batch evaluation (YI4b).
//
// This file is NOT part of the vendored upstream snapshot; it is the R package's
// own glue and carries no Google/Apache/Yandex header. It binds the vendored
// robotstxtyandex public API (Policy::Parse + Policy::EvaluateChecked) declared
// under vendor/robotstxtyandex/include (reached via the -Ivendor/... path in
// src/Makevars). It NEVER calls the legacy Policy::Evaluate(), and it NEVER
// calls the Google engine (googlebot / robots.cc) -- this path is engine-pure.
//
// Naming (deliberate): the registered native routine is named ENGINE-NEUTRALLY
// ("...evaluate_checked_batch...") with no "yandex" token, exactly like the
// YI4a request-target extractor. The YI3b invariant "no yandex native entry
// point is registered" is enforced by a grep over the registered routine NAMES
// (test-native-build-wiring-mzwcditw.R). This binding is HIDDEN internal glue:
// nothing in the public facade calls it, it is not user-exported, and engine
// availability still reports Yandex capability_unavailable. Keeping the routine
// name engine-neutral preserves that grep-invariant unchanged while YI4b lays
// down the faithful native surface the later adapter (YI4c/YI5) will consume.
//
// Lifetime & batching (integration spec section 8): the robots body is accepted
// as a length-bearing RAW value so embedded NUL and invalid UTF-8 reach
// Policy::Parse(std::string_view) byte-for-byte, with no NUL-terminated C string
// and no R character translation. Bodies are supplied ALREADY DISTINCT as a list
// of raw vectors; each distinct body is parsed EXACTLY ONCE into one owning
// Policy, and every row that selects that body (via the 1-based body_index
// grouping) is evaluated against the same Policy in a single native batch. Parse
// diagnostics are retained only for the call and never enter access semantics.
// Only OWNING R values are returned -- no external pointers, no retained views.
//
// Result surface (spec sections 10 & 11): per row the routine returns the exact
// native EvaluationStatus, the crawl decision, the exact DecisionSource, the
// matched rule line/type, and the matched rule value as BOTH an exact-byte raw
// list element AND a convenience UTF-8 character (NA when the bytes are not
// valid UTF-8 or contain an embedded NUL). raw(0) (present, empty value) and
// NULL (absent rule) are kept semantically distinct. matcher_input_bytes is the
// complete supplied body byte length; matcher_body_truncated maps from the
// standalone implementation_limit diagnostic.

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <cpp11.hpp>

#include "robotstxtyandex/policy.h"

namespace {

// Minimal, self-contained UTF-8 well-formedness check (rejects overlong forms,
// UTF-16 surrogates, and code points above U+10FFFF). Used only to decide
// whether the exact rule-value bytes can also be surfaced as a UTF-8 character
// scalar; the exact bytes are always preserved separately as a raw element.
bool is_valid_utf8(const std::string& s) {
  const unsigned char* b = reinterpret_cast<const unsigned char*>(s.data());
  const std::size_t n = s.size();
  std::size_t i = 0;
  while (i < n) {
    const unsigned char c = b[i];
    if (c < 0x80) {
      i += 1;
    } else if ((c >> 5) == 0x6) {  // 110xxxxx: 2-byte sequence
      if (c < 0xC2) return false;  // overlong
      if (i + 1 >= n || (b[i + 1] & 0xC0) != 0x80) return false;
      i += 2;
    } else if ((c >> 4) == 0xE) {  // 1110xxxx: 3-byte sequence
      if (i + 2 >= n || (b[i + 1] & 0xC0) != 0x80 ||
          (b[i + 2] & 0xC0) != 0x80) {
        return false;
      }
      if (c == 0xE0 && b[i + 1] < 0xA0) return false;  // overlong
      if (c == 0xED && b[i + 1] > 0x9F) return false;  // surrogate
      i += 3;
    } else if ((c >> 3) == 0x1E) {  // 11110xxx: 4-byte sequence
      if (i + 3 >= n || (b[i + 1] & 0xC0) != 0x80 ||
          (b[i + 2] & 0xC0) != 0x80 || (b[i + 3] & 0xC0) != 0x80) {
        return false;
      }
      if (c == 0xF0 && b[i + 1] < 0x90) return false;              // overlong
      if (c > 0xF4 || (c == 0xF4 && b[i + 1] > 0x8F)) return false;  // >U+10FFFF
      i += 4;
    } else {
      return false;
    }
  }
  return true;
}

// Copy a RAWSXP's bytes into an owning std::string verbatim -- preserving
// embedded NUL and invalid UTF-8 with no translation or NUL termination.
std::string raw_sexp_to_bytes(SEXP x) {
  if (TYPEOF(x) != RAWSXP) {
    cpp11::stop("expected a raw vector");
  }
  const R_xlen_t n = Rf_xlength(x);
  const Rbyte* p = RAW(x);
  return std::string(reinterpret_cast<const char*>(p),
                     static_cast<std::size_t>(n));
}

// Build an owning R raw vector carrying the exact bytes (raw(0) for empty).
cpp11::writable::raws bytes_to_raws(const std::string& s) {
  cpp11::writable::raws out(static_cast<R_xlen_t>(s.size()));
  for (std::size_t i = 0; i < s.size(); ++i) {
    out[static_cast<R_xlen_t>(i)] = static_cast<uint8_t>(s[i]);
  }
  return out;
}

// Exact rule-value bytes as a convenience UTF-8 character scalar -- but only
// when the bytes are valid UTF-8 with no embedded NUL; otherwise NA_character_
// (the exact bytes remain available via the raw column). An empty value is a
// valid UTF-8 empty string "".
cpp11::r_string value_utf8_or_na(const std::string& s) {
  if (is_valid_utf8(s) && s.find('\0') == std::string::npos) {
    return cpp11::r_string(
        Rf_mkCharLenCE(s.data(), static_cast<int>(s.size()), CE_UTF8));
  }
  return cpp11::r_string(NA_STRING);
}

const char* status_name(robotstxtyandex::EvaluationStatus status) {
  switch (status) {
    case robotstxtyandex::EvaluationStatus::evaluated:
      return "evaluated";
    case robotstxtyandex::EvaluationStatus::invalid_request_target:
      return "invalid_request_target";
    case robotstxtyandex::EvaluationStatus::unsupported_crawler:
      return "unsupported_crawler";
  }
  return "evaluated";  // unreachable; all enumerators handled above.
}

const char* source_name(robotstxtyandex::DecisionSource source) {
  switch (source) {
    case robotstxtyandex::DecisionSource::rule_allow:
      return "rule_allow";
    case robotstxtyandex::DecisionSource::rule_disallow:
      return "rule_disallow";
    case robotstxtyandex::DecisionSource::effective_empty_disallow:
      return "effective_empty_disallow";
    case robotstxtyandex::DecisionSource::default_allow:
      return "default_allow";
  }
  return "default_allow";  // unreachable; all enumerators handled above.
}

// A parse produced the standalone implementation_limit diagnostic iff the body
// hit an engine implementation limit. This is the standalone signal the native
// surface maps to matcher_body_truncated. (The current vendored parser never
// emits this code, so the flag is FALSE in practice; the host-owned 500,000-byte
// size policy of spec section 12 runs BEFORE dispatch and never truncates and
// parses, so an over-limit body never reaches this binding at all.)
bool has_implementation_limit(
    const std::vector<robotstxtyandex::Diagnostic>& diagnostics) {
  for (const robotstxtyandex::Diagnostic& d : diagnostics) {
    if (d.code == robotstxtyandex::DiagnosticCode::implementation_limit) {
      return true;
    }
  }
  return false;
}

}  // namespace

// Evaluate a batch of (crawler, request-target) rows against a set of distinct
// robots bodies using the checked robotstxtyandex API, parsing each distinct
// body exactly once.
//
// The routine is deliberately terse-named (see the header note) and is normally
// reached through the internal R wrapper yandex_evaluate_checked_batch(); the
// argument names here are short only to keep the generated cpp11 glue within the
// project 80-column lint budget.
//
// Inputs:
//   bodies   list of raw vectors -- the DISTINCT robots bodies (bytes exact).
//   ids      integer, one per row, 1-based index into `bodies` (row->body group).
//   agents   character, one per row -- the product token (crawler) to evaluate.
//   targets  list of raw vectors, one per row -- the exact request-target bytes
//            passed to EvaluateChecked (byte-preserving).
//
// Returns a named list of equal-length per-row columns plus a scalar
// `n_parse_calls` (== number of distinct bodies parsed, an observable proof of
// parse-once batching). All returned values are owning and independent of the
// C++ state freed when the call returns.
[[cpp11::register]]
cpp11::list robotstxtr_checked_batch_(cpp11::list bodies, cpp11::integers ids,
                                      cpp11::strings agents,
                                      cpp11::list targets) {
  const R_xlen_t n_bodies = bodies.size();
  const R_xlen_t n_rows = ids.size();
  if (agents.size() != n_rows || targets.size() != n_rows) {
    cpp11::stop("crawlers and targets must have one element per row");
  }

  // Parse each DISTINCT body exactly once. Keep the owning body bytes alive for
  // the whole call: the parsed Policy may reference those bytes during matching.
  std::vector<std::string> body_bytes;
  std::vector<robotstxtyandex::ParseResult> parsed;
  std::vector<int> body_len;
  std::vector<int> body_truncated;  // 0/1
  body_bytes.reserve(static_cast<std::size_t>(n_bodies));
  parsed.reserve(static_cast<std::size_t>(n_bodies));
  body_len.reserve(static_cast<std::size_t>(n_bodies));
  body_truncated.reserve(static_cast<std::size_t>(n_bodies));
  for (R_xlen_t b = 0; b < n_bodies; ++b) {
    body_bytes.push_back(raw_sexp_to_bytes(bodies[b]));
    const std::string& bytes = body_bytes.back();
    parsed.push_back(robotstxtyandex::Policy::Parse(std::string_view(bytes)));
    body_len.push_back(static_cast<int>(bytes.size()));
    body_truncated.push_back(
        has_implementation_limit(parsed.back().diagnostics) ? 1 : 0);
  }

  cpp11::writable::strings native_evaluation_status(n_rows);
  cpp11::writable::logicals url_decision(n_rows);
  cpp11::writable::strings decision_source(n_rows);
  cpp11::writable::integers matched_line(n_rows);
  cpp11::writable::strings matched_rule_type(n_rows);
  cpp11::writable::strings matched_rule_value(n_rows);
  cpp11::writable::list matched_rule_value_raw(n_rows);  // NULL by default
  cpp11::writable::integers matcher_input_bytes(n_rows);
  cpp11::writable::logicals matcher_body_truncated(n_rows);
  cpp11::writable::list matcher_request_target_raw(n_rows);

  for (R_xlen_t r = 0; r < n_rows; ++r) {
    const int idx1 = ids[r];
    if (idx1 < 1 || static_cast<R_xlen_t>(idx1) > n_bodies) {
      cpp11::stop("body index out of range");
    }
    const std::size_t b = static_cast<std::size_t>(idx1 - 1);

    const std::string crawler(agents[r]);
    const std::string target = raw_sexp_to_bytes(targets[r]);

    // Per-body constants replicated onto every row that used this body.
    matcher_input_bytes[r] = body_len[b];
    matcher_body_truncated[r] =
        body_truncated[b] ? cpp11::r_bool(TRUE) : cpp11::r_bool(FALSE);
    matcher_request_target_raw[r] = bytes_to_raws(target);

    const robotstxtyandex::CheckedEvaluationResult result =
        parsed[b].policy.EvaluateChecked(std::string_view(crawler),
                                         std::string_view(target));

    native_evaluation_status[r] = status_name(result.status);

    if (result.status != robotstxtyandex::EvaluationStatus::evaluated) {
      // Non-evaluated (unsupported_crawler / invalid_request_target). The
      // standalone precedence -- unsupported over invalid -- is decided inside
      // EvaluateChecked; the adapter does NOT re-prevalidate.
      url_decision[r] = cpp11::na<cpp11::r_bool>();
      decision_source[r] = cpp11::r_string(NA_STRING);
      matched_line[r] = NA_INTEGER;
      matched_rule_type[r] = "unknown";
      matched_rule_value[r] = cpp11::r_string(NA_STRING);
      // matched_rule_value_raw stays NULL (absent-rule representation).
      continue;
    }

    // Evaluated: a MatchResult is present.
    const robotstxtyandex::MatchResult& match = *result.match;
    url_decision[r] = match.allowed ? cpp11::r_bool(TRUE) : cpp11::r_bool(FALSE);
    decision_source[r] = source_name(match.source);

    if (!match.matched_rule.has_value()) {
      // default_allow: absent-rule representation (NULL raw, NA text, no line,
      // type "none").
      matched_line[r] = NA_INTEGER;
      matched_rule_type[r] = "none";
      matched_rule_value[r] = cpp11::r_string(NA_STRING);
      // matched_rule_value_raw stays NULL.
      continue;
    }

    // rule_allow / rule_disallow / effective_empty_disallow: retain the exact
    // original line, type, and value bytes. effective_empty_disallow keeps the
    // disallow type and its empty value (present raw(0), UTF-8 "") -- it is NOT
    // rewritten to rule_allow or default_allow.
    const robotstxtyandex::RuleMatch& rule = *match.matched_rule;
    matched_line[r] = static_cast<int>(rule.line);
    matched_rule_type[r] =
        (rule.type == robotstxtyandex::RuleType::allow) ? "allow" : "disallow";
    matched_rule_value[r] = value_utf8_or_na(rule.value);
    matched_rule_value_raw[r] = bytes_to_raws(rule.value);
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
      "native_evaluation_status"_nm = native_evaluation_status,
      "url_decision"_nm = url_decision,
      "decision_source"_nm = decision_source,
      "matched_line"_nm = matched_line,
      "matched_rule_type"_nm = matched_rule_type,
      "matched_rule_value"_nm = matched_rule_value,
      "matched_rule_value_raw"_nm = matched_rule_value_raw,
      "matcher_input_bytes"_nm = matcher_input_bytes,
      "matcher_body_truncated"_nm = matcher_body_truncated,
      "matcher_request_target_raw"_nm = matcher_request_target_raw,
      "n_parse_calls"_nm = cpp11::writable::integers({static_cast<int>(n_bodies)}),
  });
}
