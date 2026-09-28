#ifndef ROBOTSTXTYANDEX_METADATA_H_
#define ROBOTSTXTYANDEX_METADATA_H_

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace robotstxtyandex {

struct SitemapEntry {
  std::size_t line;  // One-based physical line.
  std::string value;
};

struct CleanParamRule {
  std::size_t line;  // One-based physical line.
  std::vector<std::string> parameters;
  std::optional<std::string> path_prefix;
};

}  // namespace robotstxtyandex

#endif  // ROBOTSTXTYANDEX_METADATA_H_
