#include "diagnostic_mapping.h"

#include <vector>

#include "parser.h"
#include "robotstxtbing/diagnostic.h"

namespace robotstxtbing::detail {
namespace {

// Maps an internal seed code to its same-named public code. The switch is
// exhaustive with no default so that adding a DiagnosticSeedCode fails to
// compile here rather than silently relying on enum declaration order.
DiagnosticCode public_code(DiagnosticSeedCode code) noexcept {
  switch (code) {
    case DiagnosticSeedCode::unknown_directive:
      return DiagnosticCode::unknown_directive;
    case DiagnosticSeedCode::missing_colon:
      return DiagnosticCode::missing_colon;
    case DiagnosticSeedCode::empty_field_name:
      return DiagnosticCode::empty_field_name;
    case DiagnosticSeedCode::non_ascii_field_name:
      return DiagnosticCode::non_ascii_field_name;
    case DiagnosticSeedCode::empty_user_agent:
      return DiagnosticCode::empty_user_agent;
    case DiagnosticSeedCode::orphan_access_rule:
      return DiagnosticCode::orphan_access_rule;
    case DiagnosticSeedCode::empty_access_rule:
      return DiagnosticCode::empty_access_rule;
    case DiagnosticSeedCode::access_value_missing_slash:
      return DiagnosticCode::access_value_missing_slash;
    case DiagnosticSeedCode::empty_sitemap:
      return DiagnosticCode::empty_sitemap;
    case DiagnosticSeedCode::empty_crawl_delay:
      return DiagnosticCode::empty_crawl_delay;
    case DiagnosticSeedCode::invalid_crawl_delay:
      return DiagnosticCode::invalid_crawl_delay;
    case DiagnosticSeedCode::crawl_delay_overflow:
      return DiagnosticCode::crawl_delay_overflow;
    case DiagnosticSeedCode::body_limit_exceeded:
      return DiagnosticCode::body_limit_exceeded;
    case DiagnosticSeedCode::line_length_limit_exceeded:
      return DiagnosticCode::line_length_limit_exceeded;
    case DiagnosticSeedCode::record_limit_exceeded:
      return DiagnosticCode::record_limit_exceeded;
    case DiagnosticSeedCode::rule_limit_exceeded:
      return DiagnosticCode::rule_limit_exceeded;
  }
  return DiagnosticCode::unknown_directive;
}

// Severity per the exhaustive spec 12 table: unknown_directive is the only
// note; every other code is a warning. Exhaustive switch, no default.
DiagnosticSeverity severity_of(DiagnosticCode code) noexcept {
  switch (code) {
    case DiagnosticCode::unknown_directive:
      return DiagnosticSeverity::note;
    case DiagnosticCode::missing_colon:
    case DiagnosticCode::empty_field_name:
    case DiagnosticCode::non_ascii_field_name:
    case DiagnosticCode::empty_user_agent:
    case DiagnosticCode::orphan_access_rule:
    case DiagnosticCode::empty_access_rule:
    case DiagnosticCode::access_value_missing_slash:
    case DiagnosticCode::empty_sitemap:
    case DiagnosticCode::empty_crawl_delay:
    case DiagnosticCode::invalid_crawl_delay:
    case DiagnosticCode::crawl_delay_overflow:
    case DiagnosticCode::body_limit_exceeded:
    case DiagnosticCode::line_length_limit_exceeded:
    case DiagnosticCode::record_limit_exceeded:
    case DiagnosticCode::rule_limit_exceeded:
      return DiagnosticSeverity::warning;
  }
  return DiagnosticSeverity::warning;
}

// Owning project text. Explanatory only, never an access verdict, and NOT
// stable before 1.0. Kept internal to this translation unit. Exhaustive
// switch, no default.
const char* detail_text(DiagnosticCode code) noexcept {
  switch (code) {
    case DiagnosticCode::unknown_directive:
      return "Unrecognized directive field name; the line is retained but "
             "not classified as a known directive.";
    case DiagnosticCode::missing_colon:
      return "Directive line has no ':' field separator, so no field name "
             "and value could be split.";
    case DiagnosticCode::empty_field_name:
      return "Directive field name is empty before the ':' separator.";
    case DiagnosticCode::non_ascii_field_name:
      return "Directive field name contains non-ASCII bytes.";
    case DiagnosticCode::empty_user_agent:
      return "User-agent directive has an empty value.";
    case DiagnosticCode::orphan_access_rule:
      return "Access rule appears with no preceding User-agent line and is "
             "not attached to any group.";
    case DiagnosticCode::empty_access_rule:
      return "Access rule has an empty path value.";
    case DiagnosticCode::access_value_missing_slash:
      return "Access rule path value does not begin with '/'.";
    case DiagnosticCode::empty_sitemap:
      return "Sitemap directive has an empty value.";
    case DiagnosticCode::empty_crawl_delay:
      return "Crawl-delay directive has an empty value.";
    case DiagnosticCode::invalid_crawl_delay:
      return "Crawl-delay value is not a valid non-negative number.";
    case DiagnosticCode::crawl_delay_overflow:
      return "Crawl-delay value exceeds the representable numeric range.";
    case DiagnosticCode::body_limit_exceeded:
      return "Body exceeds the maximum parsed byte length; parsing stopped "
             "at the limit.";
    case DiagnosticCode::line_length_limit_exceeded:
      return "A line exceeds the maximum parsed line length; parsing stopped "
             "at the limit.";
    case DiagnosticCode::record_limit_exceeded:
      return "The maximum number of parsed records was exceeded; parsing "
             "stopped at the limit.";
    case DiagnosticCode::rule_limit_exceeded:
      return "The maximum number of access rules was exceeded; parsing "
             "stopped at the limit.";
  }
  return "";
}

SourceLocation location_of(const ByteSpan& span) noexcept {
  return SourceLocation{span.begin.line, span.begin.byte_column,
                        span.begin.byte_offset, span.byte_length};
}

}  // namespace

Diagnostic make_diagnostic(const DiagnosticSeed& seed) {
  const DiagnosticCode code = public_code(seed.code);
  return Diagnostic{code, severity_of(code), location_of(seed.span),
                    detail_text(code)};
}

std::vector<Diagnostic> make_diagnostics(
    const std::vector<DiagnosticSeed>& seeds) {
  std::vector<Diagnostic> diagnostics;
  diagnostics.reserve(seeds.size());
  for (const DiagnosticSeed& seed : seeds) {
    diagnostics.push_back(make_diagnostic(seed));
  }
  return diagnostics;
}

}  // namespace robotstxtbing::detail
