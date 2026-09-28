#include "metadata_extraction.h"

#include <cstddef>
#include <string>
#include <vector>

#include "access_grouping.h"
#include "crawl_delay_value.h"
#include "directive_lexer.h"
#include "robotstxtbing/metadata.h"

namespace robotstxtbing::detail {

std::vector<robotstxtbing::SitemapEntry> extract_sitemaps(
    const AccessGroupingIr& ir) {
  std::vector<robotstxtbing::SitemapEntry> sitemaps;
  for (const MetadataRecordReference& reference : ir.metadata_records) {
    const ClassifiedRecord& record = ir.records[reference.record_index];
    if (record.directive != DirectiveName::sitemap) {
      continue;
    }
    if (record.value.empty()) {
      continue;
    }
    sitemaps.push_back(
        {std::string(record.value), record.source.line});
  }
  return sitemaps;
}

std::vector<robotstxtbing::CrawlDelayEntry> extract_crawl_delays(
    const AccessGroupingIr& ir) {
  std::vector<robotstxtbing::CrawlDelayEntry> crawl_delays;
  for (const MetadataRecordReference& reference : ir.metadata_records) {
    const ClassifiedRecord& record = ir.records[reference.record_index];
    if (record.directive != DirectiveName::crawl_delay) {
      continue;
    }

    const CrawlDelayValue classified =
        classify_crawl_delay_value(record.value);

    std::vector<robotstxtbing::AgentReference> group_agents;
    if (reference.group_index.has_value()) {
      const AccessGroup& group = ir.groups[*reference.group_index];
      group_agents.reserve(group.agent_record_indices.size());
      for (const std::size_t agent_index : group.agent_record_indices) {
        const ClassifiedRecord& agent = ir.records[agent_index];
        group_agents.push_back(
            {std::string(agent.value), agent.source.line});
      }
    }

    crawl_delays.push_back({std::string(record.value), classified.seconds,
                            classified.status, std::move(group_agents),
                            record.source.line});
  }
  return crawl_delays;
}

}  // namespace robotstxtbing::detail
