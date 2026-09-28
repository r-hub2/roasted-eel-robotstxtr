#ifndef ROBOTSTXTYANDEX_SRC_PARSER_IR_H_
#define ROBOTSTXTYANDEX_SRC_PARSER_IR_H_

#include <string>
#include <vector>

#include "directive_lexer.h"
#include "robotstxtyandex/diagnostic.h"
#include "robotstxtyandex/metadata.h"
#include "robotstxtyandex/result.h"

namespace robotstxtyandex::detail {

struct CrawlerToken {
  SourceLocation location;  // First byte of the value.
  std::string value;
};

struct AccessRule {
  SourceLocation location;  // First byte of the value.
  RuleType type;
  std::string value;
};

struct Group {
  SourceLocation location;  // First User-agent field in the group.
  std::vector<CrawlerToken> crawlers;
  std::vector<AccessRule> rules;
};

// Converts the owning directive lex into an owning parser intermediate
// representation. The value has no mutating API; later parser stages consume
// its collections through const references.
class ParserIr {
 public:
  explicit ParserIr(const DirectiveLex& lex);

  const std::vector<Group>& groups() const noexcept { return groups_; }
  const std::vector<SitemapEntry>& sitemaps() const noexcept {
    return sitemaps_;
  }
  const std::vector<CleanParamRule>& clean_params() const noexcept {
    return clean_params_;
  }
  const std::vector<Diagnostic>& diagnostics() const noexcept {
    return diagnostics_;
  }

 private:
  std::vector<Group> groups_;
  std::vector<SitemapEntry> sitemaps_;
  std::vector<CleanParamRule> clean_params_;
  std::vector<Diagnostic> diagnostics_;
};

}  // namespace robotstxtyandex::detail

#endif  // ROBOTSTXTYANDEX_SRC_PARSER_IR_H_
