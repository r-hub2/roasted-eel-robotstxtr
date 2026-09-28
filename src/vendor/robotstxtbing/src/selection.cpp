#include "selection.h"

#include <cstddef>
#include <string_view>
#include <vector>

#include "directive_lexer.h"

namespace robotstxtbing::detail {
namespace {

// Fold a single byte A-Z -> a-z. ASCII only; leaves every other byte untouched.
// Deliberately NOT std::tolower/towlower (no locale, no wide characters).
char ascii_fold(char byte) noexcept {
  const unsigned char value = static_cast<unsigned char>(byte);
  if (value >= 'A' && value <= 'Z') {
    return static_cast<char>(value - 'A' + 'a');
  }
  return byte;
}

// ASCII case-insensitive byte-wise equality. Every non-letter byte must match
// exactly; letters match modulo A-Z<->a-z. No trimming or normalization here —
// the lexer already stripped OWS from the User-agent value.
bool ascii_equal_fold(std::string_view lhs, std::string_view rhs) noexcept {
  if (lhs.size() != rhs.size()) {
    return false;
  }
  for (std::size_t index = 0; index < lhs.size(); ++index) {
    if (ascii_fold(lhs[index]) != ascii_fold(rhs[index])) {
      return false;
    }
  }
  return true;
}

// Whether any of a group's User-agent tokens equals `token` (ASCII case-fold).
bool group_names_token(const AccessGroupingIr& ir, const AccessGroup& group,
                       std::string_view token) noexcept {
  for (const std::size_t agent_index : group.agent_record_indices) {
    if (ascii_equal_fold(ir.records[agent_index].value, token)) {
      return true;
    }
  }
  return false;
}

// Whether any of a group's User-agent tokens is the literal `*`. This is a
// case-sensitive byte compare (a wildcard token has no letters); a `*` group is
// exactly the OWS-trimmed User-agent value "*".
bool group_is_wildcard(const AccessGroupingIr& ir,
                       const AccessGroup& group) noexcept {
  for (const std::size_t agent_index : group.agent_record_indices) {
    if (ir.records[agent_index].value == std::string_view("*")) {
      return true;
    }
  }
  return false;
}

// Append a group's access record indices (already in document order) onto the
// running selection.
void append_group_access(const AccessGroup& group,
                         std::vector<std::size_t>& selection) {
  selection.insert(selection.end(), group.access_record_indices.begin(),
                   group.access_record_indices.end());
}

}  // namespace

GroupSelectionProfile bingbot_group_selection_profile() noexcept {
  return GroupSelectionProfile{/*token=*/"bingbot",
                               /*has_wildcard_fallback=*/true};
}

GroupSelectionProfile adidxbot_group_selection_profile() noexcept {
  return GroupSelectionProfile{/*token=*/"adidxbot",
                               /*has_wildcard_fallback=*/false};
}

std::vector<std::size_t> select_candidate_access_records(
    const GroupSelectionProfile& profile, const AccessGroupingIr& ir) {
  std::vector<std::size_t> selection;

  // (1) Exactly-named tier: union of every group naming the profile token, in
  // document order (repeated same-token groups merge). If any named group is
  // present, the `*` group is NOT additionally applied.
  bool named_present = false;
  for (const AccessGroup& group : ir.groups) {
    if (group_names_token(ir, group, profile.token)) {
      named_present = true;
      append_group_access(group, selection);
    }
  }
  if (named_present) {
    return selection;
  }

  // (2) Wildcard fallback tier: only when the profile has a `*` fallback.
  if (profile.has_wildcard_fallback) {
    for (const AccessGroup& group : ir.groups) {
      if (group_is_wildcard(ir, group)) {
        append_group_access(group, selection);
      }
    }
  }

  // (3) Otherwise empty => default-allow downstream (decided by U4/U6).
  return selection;
}

}  // namespace robotstxtbing::detail
