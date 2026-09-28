#ifndef ROBOTSTXTBING_METADATA_H_
#define ROBOTSTXTBING_METADATA_H_

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace robotstxtbing {

struct SitemapEntry {
  std::string value;
  std::size_t line;
};

enum class CrawlDelayValueStatus { parsed, empty, invalid, overflow };

struct AgentReference {
  std::string value;
  std::size_t line;
};

struct CrawlDelayEntry {
  std::string raw_value;
  std::optional<std::uint32_t> seconds;
  CrawlDelayValueStatus value_status;
  std::vector<AgentReference> group_agents;
  std::size_t line;
};

}  // namespace robotstxtbing

#endif  // ROBOTSTXTBING_METADATA_H_
