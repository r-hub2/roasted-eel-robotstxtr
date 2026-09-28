#ifndef ROBOTSTXTBING_SRC_CRAWL_DELAY_VALUE_H_
#define ROBOTSTXTBING_SRC_CRAWL_DELAY_VALUE_H_

#include <cstdint>
#include <optional>
#include <string_view>

#include "robotstxtbing/metadata.h"

namespace robotstxtbing::detail {

// Shared lexical classification of a Crawl-delay value. The parser diagnostic
// layer and the metadata extraction layer both call this so their status and
// diagnostic codes cannot drift (spec v2 SS11.2 / SS12).
struct CrawlDelayValue {
  robotstxtbing::CrawlDelayValueStatus status;
  std::optional<std::uint32_t> seconds;
};

CrawlDelayValue classify_crawl_delay_value(std::string_view value) noexcept;

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_CRAWL_DELAY_VALUE_H_
