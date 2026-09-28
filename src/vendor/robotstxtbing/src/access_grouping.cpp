#include "access_grouping.h"

#include <cstddef>
#include <optional>
#include <utility>
#include <vector>

namespace robotstxtbing::detail {
namespace {

bool is_recognized(const ClassifiedRecord& record,
                   DirectiveName directive) noexcept {
  return record.status == LexicalStatus::directive &&
         record.directive == directive;
}

bool is_access_record(const ClassifiedRecord& record) noexcept {
  return is_recognized(record, DirectiveName::allow) ||
         is_recognized(record, DirectiveName::disallow);
}

AccessValueStatus access_value_status(
    const ClassifiedRecord& record) noexcept {
  if (record.value.empty()) {
    return AccessValueStatus::empty;
  }
  if (record.value.front() != '/') {
    return AccessValueStatus::missing_leading_slash;
  }
  return AccessValueStatus::canonical;
}

}  // namespace

AccessGroupingIr build_access_grouping_ir(
    std::vector<ClassifiedRecord> records) {
  AccessGroupingIr result{std::move(records), {}, {}, {}, 0};
  std::optional<std::size_t> open_group;
  bool open_group_has_access = false;
  // §9.3 rule 2a: a recognized Sitemap or Crawl-delay record, or an unknown
  // field, appearing between stacked User-agent records (after a User-agent,
  // before this group's first retained access record) ends the stacked run.
  // The next recognized User-agent then starts a new group.
  bool stacked_run_sealed = false;

  for (std::size_t record_index = 0; record_index < result.records.size();
       ++record_index) {
    const ClassifiedRecord& record = result.records[record_index];
    if (record.status != LexicalStatus::blank) {
      ++result.classified_record_count;
    }

    if (is_recognized(record, DirectiveName::user_agent)) {
      if (record.value.empty()) {
        continue;
      }

      if (!open_group.has_value() || open_group_has_access ||
          stacked_run_sealed) {
        const std::size_t group_index = result.groups.size();
        result.groups.push_back({group_index, {}, {}});
        open_group = group_index;
        open_group_has_access = false;
        stacked_run_sealed = false;
      }
      result.groups[*open_group].agent_record_indices.push_back(record_index);
      continue;
    }

    if (is_access_record(record)) {
      const AccessAttachment attachment = open_group.has_value()
                                                ? AccessAttachment::grouped
                                                : AccessAttachment::orphan;
      result.access_records.push_back(
          {record_index, open_group, attachment, access_value_status(record)});
      if (open_group.has_value()) {
        result.groups[*open_group].access_record_indices.push_back(record_index);
        open_group_has_access = true;
      }
      continue;
    }

    // §9.3 rule 2a: a recognized Sitemap or Crawl-delay record, or an unknown
    // field, seals an open stacked User-agent run that has not yet retained an
    // access record. Malformed, blank, and comment records never seal.
    const bool seals_stacked_run =
        open_group.has_value() && !open_group_has_access;

    if (is_recognized(record, DirectiveName::sitemap)) {
      result.metadata_records.push_back({record_index, std::nullopt});
      if (seals_stacked_run) {
        stacked_run_sealed = true;
      }
      continue;
    }

    if (is_recognized(record, DirectiveName::crawl_delay)) {
      // The Crawl-delay group_agents (§11.2) are snapshotted here: it references
      // the group open at this line, which holds exactly the preceding stacked
      // agents it terminates when it seals the run.
      result.metadata_records.push_back({record_index, open_group});
      if (seals_stacked_run) {
        stacked_run_sealed = true;
      }
      continue;
    }

    if (is_recognized(record, DirectiveName::unknown)) {
      if (seals_stacked_run) {
        stacked_run_sealed = true;
      }
    }
  }

  return result;
}

}  // namespace robotstxtbing::detail
