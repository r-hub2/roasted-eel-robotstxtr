#ifndef ROBOTSTXTYANDEX_SRC_DIRECTIVE_LEXER_H_
#define ROBOTSTXTYANDEX_SRC_DIRECTIVE_LEXER_H_

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

#include "physical_line_scanner.h"

namespace robotstxtyandex::detail {

enum class DirectiveType {
  user_agent,
  allow,
  disallow,
  sitemap,
  clean_param,
  crawl_delay,
};

enum class LexedLineKind {
  blank,
  comment,
  directive,
  unknown_field,
  malformed,
};

struct LexedLine {
  std::size_t line;
  LexedLineKind kind;
  std::optional<DirectiveType> directive;
  std::size_t field_column;
  std::size_t value_column;
  std::optional<std::size_t> comment_column;
  std::string field;
  std::string value;
};

// Produces owning, source-located line classifications from an exact physical
// line scan. Field matching is ASCII-only and locale-independent. Unknown or
// malformed lines remain in the result for later diagnostics but do not stop
// lexing of subsequent lines.
class DirectiveLex {
 public:
  explicit DirectiveLex(const PhysicalLineScan& scan);

  const std::vector<LexedLine>& lines() const noexcept { return lines_; }

 private:
  std::vector<LexedLine> lines_;
};

}  // namespace robotstxtyandex::detail

#endif  // ROBOTSTXTYANDEX_SRC_DIRECTIVE_LEXER_H_
