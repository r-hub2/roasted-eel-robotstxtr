#ifndef ROBOTSTXTBING_SRC_MATCHER_H_
#define ROBOTSTXTBING_SRC_MATCHER_H_

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

#include "robotstxtbing/result.h"

namespace robotstxtbing::detail {

// Byte-oriented, NONRECURSIVE matcher core for Bing access-rule values.
//
// This unit (BING-mcwoupdm, IMPL U2) implements ORDINARY plain-value matching
// only: a rule value matches a request-target when the value is a byte-wise
// PREFIX of the target. There is NO `*`/`$`/percent handling, NO URL parsing,
// NO Unicode, NO normalization, NO backtracking, NO recursion — just raw byte
// comparison. The representation below leaves a clean seam (the primitive-kind
// enum) for U3 to add special primitives; U2 emits only `literal`.

// Kind of a compiled-pattern primitive. `compile_ordinary_pattern` (U2) emits
// only `literal`. `compile_pattern` (U3, BING-lkzrahxu) also emits `wildcard`
// (a `*`, absorbing any byte run including the empty run and `/`) and
// `end_anchor` (a terminal `$`, requiring the target to be fully consumed).
enum class PatternPrimitiveKind {
  literal,
  wildcard,
  end_anchor,
};

// A single primitive of a compiled pattern. For `literal`, `bytes` holds the
// verbatim rule-value bytes to be matched with no transformation whatsoever.
struct PatternPrimitive {
  PatternPrimitiveKind kind;
  std::string bytes;
};

// A compiled representation of one access-rule value. For an ordinary plain
// value this is exactly one `literal` primitive carrying the whole value. Keeping
// it a vector is the seam: U3 compiles a value with metacharacters into several
// primitives (literal / wildcard / anchor) in source order.
struct CompiledPattern {
  std::vector<PatternPrimitive> primitives;
};

// Compile an ordinary plain rule value into a CompiledPattern. Byte-transparent:
// the value bytes are copied verbatim into a single `literal` primitive with no
// decode/normalization. An empty value compiles to a single empty literal (the
// empty prefix); the *semantics* of an empty value being inert are wired later
// (U5), not decided here.
CompiledPattern compile_ordinary_pattern(std::string_view value);

// Compile a rule value that MAY carry the §10 special primitives into a
// CompiledPattern (BING-lkzrahxu, IMPL U3). Byte-oriented, no URL/Unicode/
// percent-decode/normalization. Recognizes exactly four metacharacter effects,
// all as tester_observed:
//   * `*`  -> a `wildcard` primitive (absorbs any run, incl. empty and `/`);
//   * a `$` that is the LAST byte of the value -> a terminal `end_anchor`;
//     a `$` anywhere else is a literal byte (neither wildcard nor anchor);
//   * a `%` immediately followed by two ASCII hex digits -> the `%` plus those
//     two digits UPPER-CASED into the literal (the pattern-hex-case-indifferent
//     rule: an upper-cased pattern triplet compared byte-wise to the verbatim
//     target reproduces both hex-case cells); a triplet is NEVER decoded, and a
//     `%` NOT followed by two hex digits stays a literal `%`;
//   * every other byte (including `?`, `#`, a non-terminal `$`, a percent-
//     encoded `*` such as `%2A`) is a verbatim literal byte.
// `#`-comment stripping and OWS-trim are already performed by the lexer on the
// value this receives, so no `#` handling lives here. Consecutive literal bytes
// are merged into one `literal` primitive in source order.
CompiledPattern compile_pattern(std::string_view value);

// Outcome of a single match attempt.
enum class MatchStatus {
  // The match ran to completion within budget; `matched` is meaningful.
  ok,
  // The work budget was exhausted mid-match; `matched` is NOT meaningful and the
  // caller must surface EvaluationStatus::work_limit_exceeded.
  work_limit_exceeded,
};

struct MatchOutcome {
  MatchStatus status;
  bool matched;
};

// A monotone work-unit budget shared across all match attempts of one
// evaluation. One unit is charged per byte comparison the matcher performs, so
// the total effort of matching every rule against a target is bounded by the
// budget the caller seeds (see kMaxMatcherWorkUnits in limits.h). This is the
// accounting seam U7 will stress; U2 wires the accounting only.
class WorkBudget {
 public:
  explicit WorkBudget(std::size_t max_units) noexcept : remaining_(max_units) {}

  // Charge `units` of work. Returns true if the budget absorbed the charge;
  // returns false and latches `exceeded()` when the charge would overrun. Once
  // exceeded, the budget stays exceeded (monotone) and `remaining()` is 0.
  bool spend(std::size_t units) noexcept {
    if (exceeded_ || units > remaining_) {
      remaining_ = 0;
      exceeded_ = true;
      return false;
    }
    remaining_ -= units;
    return true;
  }

  bool exceeded() const noexcept { return exceeded_; }
  std::size_t remaining() const noexcept { return remaining_; }

 private:
  std::size_t remaining_;
  bool exceeded_ = false;
};

// The core ordinary match primitive. Returns whether `pattern` is a byte-wise
// prefix of `target`, charging one work unit per byte compared against `budget`.
// NONRECURSIVE and single-pass: it scans forward once, stops at the first
// mismatch or when the target is exhausted, and never backtracks. If the budget
// is exhausted mid-scan the outcome is `work_limit_exceeded`.
MatchOutcome match_ordinary(const CompiledPattern& pattern,
                            std::string_view target, WorkBudget& budget) noexcept;

// The general special-primitive match (U3). START-ANCHORED like the ordinary
// path: the pattern must align from target byte 0. Literal segments are
// separated by `*` wildcards; the first segment (before any `*`) must match at
// position 0, and each later segment matches at its LEFTMOST occurrence at or
// after the current position (greedy, NONRECURSIVE, NO backtracking — a `*`
// absorbs the run between segments). A terminal `end_anchor` requires the whole
// target to be consumed: a `*$` tail matches to end (the `*` absorbs the rest),
// and an anchored final literal segment must land exactly at the target end
// (matched as a suffix, not leftmost). With no terminal anchor the semantics are
// PREFIX, identical to `match_ordinary` on a pure-literal pattern. One work unit
// is charged per byte comparison, so a pathological pattern exhausts the budget
// (returning `work_limit_exceeded`) instead of hanging.
MatchOutcome match_pattern(const CompiledPattern& pattern,
                           std::string_view target, WorkBudget& budget) noexcept;

// RuleMatch population scaffolding, consumable by the winner/Evaluate units
// (U4/U6). Builds the public RuleMatch describing which rule was matched. `value`
// bytes are copied verbatim (byte-transparent).
RuleMatch make_rule_match(RuleType type, std::string_view value,
                          std::size_t line);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_MATCHER_H_
