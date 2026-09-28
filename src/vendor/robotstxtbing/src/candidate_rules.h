#ifndef ROBOTSTXTBING_SRC_CANDIDATE_RULES_H_
#define ROBOTSTXTBING_SRC_CANDIDATE_RULES_H_

#include <cstddef>
#include <string_view>
#include <vector>

#include "access_grouping.h"
#include "robotstxtbing/result.h"

namespace robotstxtbing::detail {

// §17 inert-record wiring (BING-wppwptlr, IMPL U5).
//
// This unit builds the MATCHABLE CANDIDATE RULE LIST that the winner (U4) and
// Policy::Evaluate (U6) run against a request-target. It consumes the record
// indices U1 selection returned (grouped, non-orphan allow/disallow records in
// document order) and DROPS the records that are INERT before any matching can
// happen. Dropping here — rather than at match time — guarantees an inert record
// can neither block a target nor override a real rule beside it.
//
// The single filtering rule this unit adds is: an access record whose VALUE is
// EMPTY is inert and contributes NO candidate rule. Only empty. This reproduces
// the accepted empty-disallow-inert / empty-allow-inert cells
// (corpus/observations/{bingbot,adidxbot}-empty-access-v1). Byte-transparent:
// inertness is decided on the raw value bytes being empty, never on any
// normalization.
//
// The other three accepted §17 cells in this unit's scope are ALREADY handled
// upstream and are NOT re-decided here:
//   * orphan-rule-discarded — U1 selection never selects an orphan access record
//     (an access record before the first User-agent belongs to no group), so it
//     never reaches this builder;
//   * missing-leading-slash — a value with no leading slash is NOT inert: it is a
//     genuine matchable rule that the U3 matcher matches LITERALLY (so it does not
//     prefix a rooted `/...` target). It is emitted like any canonical rule;
//   * field-name-case / optional-whitespace / inline-comment — classified by the
//     directive lexer before grouping; the values reaching here are already
//     recognized and trimmed.

// One matchable candidate rule: the type, the verbatim value bytes (a view into
// the owning AccessGroupingIr's record value), and the 1-based physical source
// line. U4 consumes a vector of these in document order.
struct MatchableRule {
  RuleType type;
  std::string_view value;
  std::size_t line;
};

// Whether a selected access record is INERT (contributes no candidate rule).
// True iff its value bytes are empty (AccessValueStatus::empty). A
// missing-leading-slash or canonical value is NOT inert.
bool is_inert_access_record(const ClassifiedRecord& record) noexcept;

// Build the ordered matchable candidate rule list for a profile from the U1
// selection. For each selected record index (already grouped/non-orphan and an
// allow/disallow access record), skip it if inert (empty value); otherwise emit
// a MatchableRule carrying its RuleType, verbatim value view, and source line, in
// document order.
std::vector<MatchableRule> build_matchable_rules(
    const AccessGroupingIr& ir,
    const std::vector<std::size_t>& selected_record_indices);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_CANDIDATE_RULES_H_
