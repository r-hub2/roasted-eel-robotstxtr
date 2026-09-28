#ifndef ROBOTSTXTYANDEX_SRC_CRAWLER_SELECTOR_H_
#define ROBOTSTXTYANDEX_SRC_CRAWLER_SELECTOR_H_

#include <optional>
#include <string_view>
#include <vector>

#include "access_matcher.h"
#include "parser_ir.h"

namespace robotstxtyandex::detail {

enum class CrawlerProfile {
  yandex,
  yandex_additional_bot,
};

// Recognizes only the two 0.1.0 evidence-backed public profile tokens using
// locale-independent ASCII case folding.
std::optional<CrawlerProfile> ParseCrawlerProfile(
    std::string_view crawler) noexcept;

// Projects all access rules from the selected tier in physical file order.
// Yandex uses exact groups or wildcard fallback. YandexAdditionalBot uses
// exact groups only. Returned views remain valid while groups remain alive.
std::vector<AccessRuleView> SelectAccessRules(
    const std::vector<Group>& groups, CrawlerProfile profile);

}  // namespace robotstxtyandex::detail

#endif  // ROBOTSTXTYANDEX_SRC_CRAWLER_SELECTOR_H_
