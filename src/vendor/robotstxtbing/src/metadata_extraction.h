#ifndef ROBOTSTXTBING_SRC_METADATA_EXTRACTION_H_
#define ROBOTSTXTBING_SRC_METADATA_EXTRACTION_H_

#include <vector>

#include "access_grouping.h"
#include "robotstxtbing/metadata.h"

namespace robotstxtbing::detail {

// Extracts the metadata contract (spec v2 SS11) from a parsed access-grouping
// IR. Extraction never inspects or mutates access grouping; it only reads the
// metadata references and the records/groups they point at.
//
// Sitemap (SS11.1): one entry per recognized Sitemap with a nonempty opaque
// value, preserving source order and duplicates. Empty values are omitted.
//
// Crawl-delay (SS11.2): one entry per recognized Crawl-delay, preserving every
// empty/invalid/overflow/zero/above-20 value and every duplicate. group_agents
// resolves the final stacked User-agent declarations of the open access group
// (empty when unscoped).
std::vector<robotstxtbing::SitemapEntry> extract_sitemaps(
    const AccessGroupingIr& ir);

std::vector<robotstxtbing::CrawlDelayEntry> extract_crawl_delays(
    const AccessGroupingIr& ir);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_METADATA_EXTRACTION_H_
