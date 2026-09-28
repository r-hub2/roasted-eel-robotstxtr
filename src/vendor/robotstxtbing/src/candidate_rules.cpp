#include "candidate_rules.h"

#include <cstddef>
#include <vector>

#include "directive_lexer.h"
#include "robotstxtbing/result.h"

namespace robotstxtbing::detail {
namespace {

// Map a recognized access directive to its public RuleType. Selected records are
// always allow/disallow access records (U1 selects only access records of
// selected groups), so no other directive reaches here.
RuleType rule_type_of(DirectiveName directive) noexcept {
  return directive == DirectiveName::allow ? RuleType::allow
                                           : RuleType::disallow;
}

}  // namespace

bool is_inert_access_record(const ClassifiedRecord& record) noexcept {
  // ONLY an empty value is inert. Byte-transparent: no normalization, no trim
  // (the lexer already trimmed OWS). A missing-leading-slash or canonical value
  // is a real matchable rule.
  return record.value.empty();
}

std::vector<MatchableRule> build_matchable_rules(
    const AccessGroupingIr& ir,
    const std::vector<std::size_t>& selected_record_indices) {
  std::vector<MatchableRule> rules;
  rules.reserve(selected_record_indices.size());
  for (const std::size_t record_index : selected_record_indices) {
    const ClassifiedRecord& record = ir.records[record_index];
    if (is_inert_access_record(record)) {
      continue;  // empty value => inert, contributes no candidate rule.
    }
    rules.push_back(MatchableRule{rule_type_of(record.directive), record.value,
                                  record.source.line});
  }
  return rules;
}

}  // namespace robotstxtbing::detail
