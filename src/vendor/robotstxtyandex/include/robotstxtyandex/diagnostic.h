#ifndef ROBOTSTXTYANDEX_DIAGNOSTIC_H_
#define ROBOTSTXTYANDEX_DIAGNOSTIC_H_

#include <cstddef>
#include <string>

namespace robotstxtyandex {

enum class DiagnosticSeverity {
  warning,
  error,
};

enum class DiagnosticCode {
  malformed_line,
  orphan_directive,
  empty_user_agent,
  invalid_rule,
  unsupported_directive,
  ignored_directive,
  implementation_limit,
};

struct SourceLocation {
  std::size_t line;    // One-based physical line.
  std::size_t column;  // One-based byte column.
};

struct Diagnostic {
  DiagnosticSeverity severity;
  DiagnosticCode code;
  SourceLocation location;
  std::string detail;
};

}  // namespace robotstxtyandex

#endif  // ROBOTSTXTYANDEX_DIAGNOSTIC_H_
