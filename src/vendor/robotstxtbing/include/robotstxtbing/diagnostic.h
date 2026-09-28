#ifndef ROBOTSTXTBING_DIAGNOSTIC_H_
#define ROBOTSTXTBING_DIAGNOSTIC_H_

#include <cstddef>
#include <string>

namespace robotstxtbing {

enum class DiagnosticSeverity { note, warning };

enum class DiagnosticCode {
  unknown_directive,
  missing_colon,
  empty_field_name,
  non_ascii_field_name,
  empty_user_agent,
  orphan_access_rule,
  empty_access_rule,
  access_value_missing_slash,
  empty_sitemap,
  empty_crawl_delay,
  invalid_crawl_delay,
  crawl_delay_overflow,
  body_limit_exceeded,
  line_length_limit_exceeded,
  record_limit_exceeded,
  rule_limit_exceeded,
};

struct SourceLocation {
  std::size_t line;
  std::size_t byte_column;
  std::size_t byte_offset;
  std::size_t byte_length;
};

struct Diagnostic {
  DiagnosticCode code;
  DiagnosticSeverity severity;
  SourceLocation location;
  std::string detail;
};

}  // namespace robotstxtbing

#endif  // ROBOTSTXTBING_DIAGNOSTIC_H_
