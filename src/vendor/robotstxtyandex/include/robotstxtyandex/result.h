#ifndef ROBOTSTXTYANDEX_RESULT_H_
#define ROBOTSTXTYANDEX_RESULT_H_

#include <cstddef>
#include <optional>
#include <string>

namespace robotstxtyandex {

enum class DecisionSource {
  rule_allow,
  rule_disallow,
  // Yandex gives an original empty Disallow the access effect of Allow: /.
  // matched_rule retains the original disallow type, empty value, and line.
  effective_empty_disallow,
  default_allow,
};

enum class RuleType {
  allow,
  disallow,
};

struct RuleMatch {
  std::size_t line;
  RuleType type;
  std::string value;
};

struct MatchResult {
  bool allowed;
  DecisionSource source;
  std::optional<RuleMatch> matched_rule;
};

enum class EvaluationStatus {
  evaluated,
  invalid_request_target,
  unsupported_crawler,
};

struct CheckedEvaluationResult {
  EvaluationStatus status;
  std::optional<MatchResult> match;
};

}  // namespace robotstxtyandex

#endif  // ROBOTSTXTYANDEX_RESULT_H_
