#ifndef ROBOTSTXTBING_SRC_SELECTION_H_
#define ROBOTSTXTBING_SRC_SELECTION_H_

#include <cstddef>
#include <string_view>
#include <vector>

#include "access_grouping.h"

namespace robotstxtbing::detail {

// §8 profile group-selection tables (BING-oskjaapn, IMPL U1).
//
// This unit encodes the TWO frozen §8 ordered group-selection tables that decide
// which retained access records are CANDIDATES for a profile. It consumes the
// AccessGroupingIr the parser already produced (group boundaries, including the
// §9.3 rule 2a seals from U0, are NOT recomputed here) and returns the SELECTED
// CANDIDATE RULE SET: the access records of the selected group(s). There is NO
// matching here — the winner decision (U4) and Policy::Evaluate (U6) are later.
//
// The two profiles share one selection ALGORITHM modulo a single fallback flag.
// Selection is EXACT-TOKEN only: there is no "most specific token", no
// longest-match, no cross-profile fallback. Token comparison is ASCII
// case-insensitive (A-Z<->a-z on bytes only; never locale-aware). A `*` group is
// the literal User-agent value `*`.

// One profile's frozen §8 selection configuration. The algorithm is identical
// across profiles except for `has_wildcard_fallback`.
struct GroupSelectionProfile {
  // The profile's exact product token, matched ASCII case-insensitively against a
  // group's User-agent values. Byte-transparent otherwise (no normalization).
  std::string_view token;

  // Whether a `User-agent: *` group is a fallback when no exactly-named group
  // matches. Bingbot: true. AdIdxBot: false (no `*` fallback whatsoever).
  bool has_wildcard_fallback;
};

// The two frozen profile configurations. U6 owns product_token -> profile
// resolution (and the unsupported_profile decision); these are exposed so it can
// pick. Bingbot has a `*` fallback; AdIdxBot does not.
GroupSelectionProfile bingbot_group_selection_profile() noexcept;
GroupSelectionProfile adidxbot_group_selection_profile() noexcept;

// The selected candidate rule set for a profile: record indices (into
// `ir.records`) of the access records of the selected group(s), in document
// order.
//
// Algorithm (frozen §8, verbatim from the accepted selection expectations):
//   (1) Collect every group whose agent tokens contain a case-insensitive exact
//       match to the profile token. If any, the selection is the union of those
//       groups' access records (repeated same-token groups merge in document
//       order; the `*` group is NOT additionally applied).
//   (2) Else, if the profile has a wildcard fallback, the selection is the union
//       of the `*` group(s)' access records.
//   (3) Else the selection is empty.
//
// An EMPTY selection means default-allow downstream; U1 returns the empty set and
// does NOT decide default-allow (that is U4/U6). Orphan access records belong to
// no group and are never selected here.
std::vector<std::size_t> select_candidate_access_records(
    const GroupSelectionProfile& profile, const AccessGroupingIr& ir);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_SELECTION_H_
