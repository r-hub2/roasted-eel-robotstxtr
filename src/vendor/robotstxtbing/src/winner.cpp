#include "winner.h"

#include <cstddef>
#include <optional>
#include <string_view>
#include <vector>

#include "candidate_rules.h"
#include "matcher.h"
#include "robotstxtbing/result.h"

namespace robotstxtbing::detail {

WinnerOutcome decide_winner(const std::vector<MatchableRule>& rules,
                            std::string_view target, WorkBudget& budget) {
  // Best matching rule so far, if any. The winner is the maximum matching value
  // byte length; at that length an Allow beats a Disallow; among equal
  // (length, type) rules the FIRST in document order wins attribution.
  const MatchableRule* winner = nullptr;

  for (const MatchableRule& rule : rules) {
    const CompiledPattern pattern = compile_pattern(rule.value);
    const MatchOutcome outcome = match_pattern(pattern, target, budget);
    if (outcome.status == MatchStatus::work_limit_exceeded) {
      return WinnerOutcome{WinnerStatus::work_limit_exceeded, Decision{}};
    }
    if (!outcome.matched) {
      continue;
    }

    if (winner == nullptr) {
      winner = &rule;
      continue;
    }

    // Specificity key is the raw value byte length (see winner.h).
    if (rule.value.size() > winner->value.size()) {
      winner = &rule;
    } else if (rule.value.size() == winner->value.size()) {
      // Equal length: Allow beats Disallow (tie-allow-wins). If both are the
      // same type, keep the first in document order (stable attribution ->
      // duplicate-inert). Only a later Allow can displace a current Disallow.
      if (rule.type == RuleType::allow &&
          winner->type == RuleType::disallow) {
        winner = &rule;
      }
    }
  }

  Decision decision;
  if (winner == nullptr) {
    decision.access = AccessDecision::allow;
    decision.source = DecisionSource::default_allow;
    decision.matched_rule = std::nullopt;
  } else {
    decision.access = winner->type == RuleType::allow ? AccessDecision::allow
                                                      : AccessDecision::disallow;
    decision.source = DecisionSource::rule;
    decision.matched_rule =
        make_rule_match(winner->type, winner->value, winner->line);
  }
  return WinnerOutcome{WinnerStatus::ok, decision};
}

}  // namespace robotstxtbing::detail
