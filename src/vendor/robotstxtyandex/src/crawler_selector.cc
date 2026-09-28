#include "crawler_selector.h"

#include <string_view>
#include <vector>

namespace robotstxtyandex::detail {
namespace {

char FoldAscii(char byte) noexcept {
  return byte >= 'A' && byte <= 'Z' ? static_cast<char>(byte + ('a' - 'A'))
                                    : byte;
}

bool AsciiEqual(std::string_view left, std::string_view right) noexcept {
  if (left.size() != right.size()) {
    return false;
  }
  for (std::size_t index = 0; index < left.size(); ++index) {
    if (FoldAscii(left[index]) != FoldAscii(right[index])) {
      return false;
    }
  }
  return true;
}

bool HasToken(const Group& group, std::string_view token) noexcept {
  for (const CrawlerToken& crawler : group.crawlers) {
    if (AsciiEqual(crawler.value, token)) {
      return true;
    }
  }
  return false;
}

void AppendRules(const Group& group, std::vector<AccessRuleView>* rules) {
  for (const AccessRule& rule : group.rules) {
    rules->push_back({rule.location.line, rule.type, rule.value});
  }
}

}  // namespace

std::optional<CrawlerProfile> ParseCrawlerProfile(
    std::string_view crawler) noexcept {
  if (AsciiEqual(crawler, "Yandex")) {
    return CrawlerProfile::yandex;
  }
  if (AsciiEqual(crawler, "YandexAdditionalBot")) {
    return CrawlerProfile::yandex_additional_bot;
  }
  return std::nullopt;
}

std::vector<AccessRuleView> SelectAccessRules(
    const std::vector<Group>& groups, CrawlerProfile profile) {
  const std::string_view exact_token =
      profile == CrawlerProfile::yandex ? "Yandex" : "YandexAdditionalBot";

  bool has_exact_group = false;
  for (const Group& group : groups) {
    if (HasToken(group, exact_token)) {
      has_exact_group = true;
      break;
    }
  }

  std::vector<AccessRuleView> rules;
  for (const Group& group : groups) {
    const bool selected =
        HasToken(group, exact_token) ||
        (profile == CrawlerProfile::yandex && !has_exact_group &&
         HasToken(group, "*"));
    if (selected) {
      AppendRules(group, &rules);
    }
  }
  return rules;
}

}  // namespace robotstxtyandex::detail
