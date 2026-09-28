#ifndef ROBOTSTXTBING_RESULT_H_
#define ROBOTSTXTBING_RESULT_H_

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

#include "robotstxtbing/diagnostic.h"
#include "robotstxtbing/policy.h"

namespace robotstxtbing {

enum class ParseStatus {
  parsed,
  body_limit_exceeded,
  line_length_limit_exceeded,
  record_limit_exceeded,
  rule_limit_exceeded,
};

enum class EvaluationStatus {
  evaluated,
  invalid_request_target,
  unsupported_profile,
  request_target_limit_exceeded,
  work_limit_exceeded,
};

enum class AccessDecision { allow, disallow };
enum class DecisionSource { rule, default_allow };
enum class RuleType { allow, disallow };

struct RuleMatch {
  RuleType type;
  std::string value;
  std::size_t line;
};

struct Decision {
  AccessDecision access;
  DecisionSource source;
  std::optional<RuleMatch> matched_rule;
};

struct EvaluationResult {
  EvaluationStatus status;
  std::optional<Decision> decision;
};

struct ParseResult {
  ParseStatus status;
  std::optional<Policy> policy;
  std::vector<Diagnostic> diagnostics;
};

}  // namespace robotstxtbing

#endif  // ROBOTSTXTBING_RESULT_H_
