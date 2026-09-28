#ifndef ROBOTSTXTBING_SRC_ACCESS_GROUPING_H_
#define ROBOTSTXTBING_SRC_ACCESS_GROUPING_H_

#include <cstddef>
#include <optional>
#include <vector>

#include "directive_lexer.h"

namespace robotstxtbing::detail {

enum class AccessAttachment {
  grouped,
  orphan,
};

enum class AccessValueStatus {
  canonical,
  empty,
  missing_leading_slash,
};

struct AccessRecordReference {
  std::size_t record_index;
  std::optional<std::size_t> group_index;
  AccessAttachment attachment;
  AccessValueStatus value_status;
};

struct MetadataRecordReference {
  std::size_t record_index;

  // Sitemap records are global and always have no group. Crawl-delay records
  // refer to the access group open at their physical position, if any. The
  // indirection deliberately observes that group's final stacked agents.
  std::optional<std::size_t> group_index;
};

struct AccessGroup {
  std::size_t group_index;
  std::vector<std::size_t> agent_record_indices;
  std::vector<std::size_t> access_record_indices;
};

struct AccessGroupingIr {
  // Records remain in physical source order. All indices in this IR refer to
  // this vector, so repeated declarations and records remain distinct.
  std::vector<ClassifiedRecord> records;
  std::vector<AccessGroup> groups;
  std::vector<AccessRecordReference> access_records;
  std::vector<MetadataRecordReference> metadata_records;
  std::size_t classified_record_count;
};

AccessGroupingIr build_access_grouping_ir(
    std::vector<ClassifiedRecord> records);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_ACCESS_GROUPING_H_
