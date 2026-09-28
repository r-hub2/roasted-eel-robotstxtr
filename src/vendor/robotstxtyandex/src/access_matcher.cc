#include "access_matcher.h"

#include <optional>
#include <string>
#include <vector>

namespace robotstxtyandex::detail {
namespace {

struct Candidate {
  const AccessRuleView* rule;
  std::size_t specificity;
  bool allows;
  bool effective_empty_disallow;
};

bool IsBetter(const Candidate& candidate, const Candidate& winner) {
  if (candidate.specificity != winner.specificity) {
    return candidate.specificity > winner.specificity;
  }
  if (candidate.allows != winner.allows) {
    return candidate.allows;
  }
  // Identical semantic winners keep the first physical line. This is an
  // internal deterministic fallback, not a Yandex compatibility claim.
  return candidate.rule->line < winner.rule->line;
}

std::optional<std::size_t> FindLiteral(std::string_view bytes,
                                       std::string_view literal,
                                       std::size_t begin,
                                       std::size_t end) {
  if (literal.empty()) {
    return begin;
  }
  if (begin > end || literal.size() > end - begin) {
    return std::nullopt;
  }

  // Knuth-Morris-Pratt keeps a failed wildcard segment search linear even for
  // long repeated prefixes such as "aaaa...ab" against "aaaa...ac".
  std::vector<std::size_t> prefix(literal.size());
  for (std::size_t index = 1, matched = 0; index < literal.size(); ++index) {
    while (matched != 0 && literal[index] != literal[matched]) {
      matched = prefix[matched - 1];
    }
    if (literal[index] == literal[matched]) {
      ++matched;
    }
    prefix[index] = matched;
  }

  for (std::size_t index = begin, matched = 0; index < end; ++index) {
    while (matched != 0 && bytes[index] != literal[matched]) {
      matched = prefix[matched - 1];
    }
    if (bytes[index] == literal[matched]) {
      ++matched;
    }
    if (matched == literal.size()) {
      return index + 1 - literal.size();
    }
  }
  return std::nullopt;
}

}  // namespace

bool AccessPatternMatches(std::string_view pattern,
                          std::string_view request_target) {
  if (pattern.empty()) {
    return false;
  }

  const bool anchored = pattern.back() == '$';
  if (anchored) {
    pattern.remove_suffix(1);
  }

  const std::size_t first_star = pattern.find('*');
  if (first_star == std::string_view::npos) {
    if (anchored) {
      return request_target == pattern;
    }
    return request_target.size() >= pattern.size() &&
           request_target.substr(0, pattern.size()) == pattern;
  }

  const bool leading_star = pattern.front() == '*';
  const bool trailing_star = pattern.back() == '*';
  std::vector<std::string_view> literals;
  for (std::size_t begin = 0; begin < pattern.size();) {
    const std::size_t star = pattern.find('*', begin);
    const std::size_t end =
        star == std::string_view::npos ? pattern.size() : star;
    if (end != begin) {
      literals.push_back(pattern.substr(begin, end - begin));
    }
    if (star == std::string_view::npos) {
      break;
    }
    begin = star + 1;
  }

  std::size_t first_literal = 0;
  std::size_t last_literal = literals.size();
  std::size_t cursor = 0;
  std::size_t limit = request_target.size();

  if (!leading_star) {
    const std::string_view prefix = literals[first_literal++];
    if (request_target.size() < prefix.size() ||
        request_target.substr(0, prefix.size()) != prefix) {
      return false;
    }
    cursor = prefix.size();
  }

  if (anchored && !trailing_star) {
    const std::string_view suffix = literals[--last_literal];
    if (request_target.size() < suffix.size()) {
      return false;
    }
    limit = request_target.size() - suffix.size();
    if (request_target.substr(limit) != suffix || cursor > limit) {
      return false;
    }
  }

  for (; first_literal < last_literal; ++first_literal) {
    const std::string_view literal = literals[first_literal];
    const std::optional<std::size_t> found =
        FindLiteral(request_target, literal, cursor, limit);
    if (!found) {
      return false;
    }
    cursor = *found + literal.size();
  }
  return true;
}

std::size_t AccessRuleSpecificity(std::string_view pattern) noexcept {
  const bool has_terminal_anchor = !pattern.empty() && pattern.back() == '$';
  std::size_t specificity = 0;
  for (std::size_t index = 0; index < pattern.size(); ++index) {
    if (pattern[index] == '*' ||
        (has_terminal_anchor && index + 1 == pattern.size())) {
      continue;
    }
    ++specificity;
  }
  return specificity;
}

MatchResult MatchAccessRules(const std::vector<AccessRuleView>& rules,
                             std::string_view request_target) {
  if (request_target.empty() || request_target.front() != '/') {
    return {true, DecisionSource::default_allow, std::nullopt};
  }

  std::optional<Candidate> winner;
  for (const AccessRuleView& rule : rules) {
    const bool empty_disallow =
        rule.type == RuleType::disallow && rule.value.empty();
    if (rule.type == RuleType::allow && rule.value.empty()) {
      continue;
    }

    const bool matches =
        empty_disallow || AccessPatternMatches(rule.value, request_target);
    if (!matches) {
      continue;
    }

    const Candidate candidate{
        &rule,
        empty_disallow ? std::size_t{1}
                       : AccessRuleSpecificity(rule.value),
        rule.type == RuleType::allow || empty_disallow,
        empty_disallow,
    };
    if (!winner || IsBetter(candidate, *winner)) {
      winner = candidate;
    }
  }

  if (!winner) {
    return {true, DecisionSource::default_allow, std::nullopt};
  }

  const RuleMatch matched_rule{winner->rule->line, winner->rule->type,
                               std::string(winner->rule->value)};
  if (winner->effective_empty_disallow) {
    return {true, DecisionSource::effective_empty_disallow, matched_rule};
  }
  if (winner->allows) {
    return {true, DecisionSource::rule_allow, matched_rule};
  }
  return {false, DecisionSource::rule_disallow, matched_rule};
}

}  // namespace robotstxtyandex::detail
