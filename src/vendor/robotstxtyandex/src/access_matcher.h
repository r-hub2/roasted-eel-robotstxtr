#ifndef ROBOTSTXTYANDEX_SRC_ACCESS_MATCHER_H_
#define ROBOTSTXTYANDEX_SRC_ACCESS_MATCHER_H_

#include <cstddef>
#include <string_view>
#include <vector>

#include "robotstxtyandex/result.h"

namespace robotstxtyandex::detail {

struct AccessRuleView {
  std::size_t line;
  RuleType type;
  std::string_view value;
};

// Matches one nonempty access-rule value against request-target bytes. A
// terminal '$' anchors the expression; otherwise a trailing '*' is implicit.
// Empty directive semantics are handled by rule selection, not this predicate.
// Runtime is linear in the combined pattern and request-target sizes.
bool AccessPatternMatches(std::string_view pattern,
                          std::string_view request_target);

// Returns the evidence-backed 0.1.0 specificity: the number of literal
// pattern bytes after excluding every '*' and a terminal '$'. Empty Disallow
// is assigned effective-root specificity by MatchAccessRules, not here.
std::size_t AccessRuleSpecificity(std::string_view pattern) noexcept;

// Matches all supported access patterns and returns the semantic winner.
// Equal specificity prefers Allow; otherwise identical winners report the
// earliest physical line as deterministic project behavior.
MatchResult MatchAccessRules(const std::vector<AccessRuleView>& rules,
                             std::string_view request_target);

}  // namespace robotstxtyandex::detail

#endif  // ROBOTSTXTYANDEX_SRC_ACCESS_MATCHER_H_
