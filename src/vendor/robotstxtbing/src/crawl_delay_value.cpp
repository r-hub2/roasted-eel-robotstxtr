#include "crawl_delay_value.h"

#include <cstdint>
#include <limits>
#include <string_view>

#include "robotstxtbing/metadata.h"

namespace robotstxtbing::detail {

CrawlDelayValue classify_crawl_delay_value(std::string_view value) noexcept {
  if (value.empty()) {
    return {robotstxtbing::CrawlDelayValueStatus::empty, std::nullopt};
  }

  std::uint32_t parsed = 0;
  for (const char byte : value) {
    if (byte < '0' || byte > '9') {
      return {robotstxtbing::CrawlDelayValueStatus::invalid, std::nullopt};
    }

    const std::uint32_t digit = static_cast<std::uint32_t>(byte - '0');
    if (parsed >
        (std::numeric_limits<std::uint32_t>::max() - digit) / 10U) {
      return {robotstxtbing::CrawlDelayValueStatus::overflow, std::nullopt};
    }
    parsed = parsed * 10U + digit;
  }

  return {robotstxtbing::CrawlDelayValueStatus::parsed, parsed};
}

}  // namespace robotstxtbing::detail
