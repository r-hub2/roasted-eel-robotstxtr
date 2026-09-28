#ifndef ROBOTSTXTBING_SRC_WINNER_H_
#define ROBOTSTXTBING_SRC_WINNER_H_

#include <cstddef>
#include <string_view>
#include <vector>

#include "candidate_rules.h"
#include "matcher.h"
#include "robotstxtbing/result.h"

namespace robotstxtbing::detail {

// §10 winner decision (BING-nukcvsjl, IMPL U4).
//
// Given the already-built MATCHABLE candidate rule list for a profile (U5's
// build_matchable_rules over U1's selection) and a request-target, this unit
// decides the WINNER and populates the public Decision. It is the pure winner
// step only: NO parsing, NO group selection, NO Policy::Evaluate (that is U6).
//
// WINNER ALGORITHM — the literal reading of the three accepted §17 winner cells
// (corpus/observations/{bingbot,adidxbot}-winner-v1/EXPECTATIONS.md), nothing
// more:
//   * SPECIFICITY KEY = the raw VALUE BYTE LENGTH of a rule (value.size()). This
//     is the literal "longer value wins" from the specificity cell — NOT a
//     matched-substring length, NOT a Google/RFC specificity heuristic, and with
//     NO weighting of `*` / `$` / path segments. Every candidate rule is run
//     against the target with the general §10 matcher (compile_pattern +
//     match_pattern); the winner is chosen among the MATCHING rules.
//   * At the maximum matching value byte length, an Allow beats a Disallow
//     (tie-allow-wins cell — Allow wins at equal length, in either source order).
//   * Among rules with equal (length, type), attribution picks the FIRST in
//     document order. This is a pure tie-break for the matched_rule; it makes a
//     duplicated rule inert (duplicate-inert cell) because the DECISION
//     (allow vs disallow) is already order-independent.
//   * If NO candidate rule matches: default-allow (DecisionSource::default_allow,
//     AccessDecision::allow, matched_rule == nullopt).
//
// KNOWN GAP — the `*` / `$` × specificity interaction is UNPROBED (deferred to
// winner-r2). The winner-v1 cells were captured with ordinary values; the
// uniform raw-value-length key is the least-invented rule reproducing them. It
// also applies to special-pattern values, but that is a DEFAULT, not an accepted
// claim — no special-case precedence for `*` / `$` is added here.
//
// WORK BUDGET: the shared WorkBudget is passed through to every match attempt. If
// ANY attempt returns MatchStatus::work_limit_exceeded, the winner stops and
// surfaces WinnerStatus::work_limit_exceeded; U6 maps that to
// EvaluationStatus::work_limit_exceeded. The Decision is meaningful only when the
// status is ok.

enum class WinnerStatus {
  ok,
  work_limit_exceeded,
};

// Result of a winner decision. `decision` is meaningful iff `status == ok`.
struct WinnerOutcome {
  WinnerStatus status;
  Decision decision;
};

// Decide the winner among `rules` (in document order) for `target`, charging all
// match work against `budget`. See the algorithm above.
WinnerOutcome decide_winner(const std::vector<MatchableRule>& rules,
                            std::string_view target, WorkBudget& budget);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_WINNER_H_
