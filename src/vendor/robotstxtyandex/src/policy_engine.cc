#include "policy_engine.h"

#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "access_matcher.h"
#include "crawler_selector.h"

namespace robotstxtyandex::detail {

MatchResult EvaluateAccess(const ParserIr& ir, std::string_view crawler,
                           std::string_view request_target) {
  CheckedEvaluationResult checked =
      EvaluateAccessChecked(ir, crawler, request_target);
  if (!checked.match.has_value()) {
    return {true, DecisionSource::default_allow, std::nullopt};
  }
  return std::move(*checked.match);
}

CheckedEvaluationResult EvaluateAccessChecked(
    const ParserIr& ir, std::string_view crawler,
    std::string_view request_target) {
  const std::optional<CrawlerProfile> profile = ParseCrawlerProfile(crawler);
  if (!profile.has_value()) {
    return {EvaluationStatus::unsupported_crawler, std::nullopt};
  }
  if (request_target.empty() || request_target.front() != '/') {
    return {EvaluationStatus::invalid_request_target, std::nullopt};
  }

  const std::vector<AccessRuleView> rules =
      SelectAccessRules(ir.groups(), *profile);
  return {EvaluationStatus::evaluated,
          MatchAccessRules(rules, request_target)};
}

}  // namespace robotstxtyandex::detail
