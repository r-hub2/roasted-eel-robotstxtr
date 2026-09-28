#include "parser.h"

#include <cstddef>
#include <cstdint>
#include <limits>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "access_grouping.h"
#include "crawl_delay_value.h"
#include "directive_lexer.h"
#include "physical_scanner.h"
#include "robotstxtbing/limits.h"

namespace robotstxtbing::detail {
namespace {

struct GroupingState {
  bool has_open_group = false;
  bool open_group_has_access = false;
};

bool is_recognized(const ClassifiedRecord& record,
                   DirectiveName directive) noexcept {
  return record.status == LexicalStatus::directive &&
         record.directive == directive;
}

bool is_access_record(const ClassifiedRecord& record) noexcept {
  return is_recognized(record, DirectiveName::allow) ||
         is_recognized(record, DirectiveName::disallow);
}

bool is_classified_record(const ClassifiedRecord& record) noexcept {
  return record.status != LexicalStatus::blank;
}

ByteSpan zero_length_span(BytePosition position) noexcept {
  return {position, 0};
}

void add_seed(std::vector<DiagnosticSeed>& diagnostics,
              DiagnosticSeedCode code, ByteSpan span) {
  diagnostics.push_back({code, span});
}

void add_crawl_delay_seed(const ClassifiedRecord& record,
                          std::vector<DiagnosticSeed>& diagnostics) {
  switch (classify_crawl_delay_value(record.value).status) {
    case robotstxtbing::CrawlDelayValueStatus::empty:
      add_seed(diagnostics, DiagnosticSeedCode::empty_crawl_delay,
               record.value_span);
      return;
    case robotstxtbing::CrawlDelayValueStatus::invalid:
      add_seed(diagnostics, DiagnosticSeedCode::invalid_crawl_delay,
               record.value_span);
      return;
    case robotstxtbing::CrawlDelayValueStatus::overflow:
      add_seed(diagnostics, DiagnosticSeedCode::crawl_delay_overflow,
               record.value_span);
      return;
    case robotstxtbing::CrawlDelayValueStatus::parsed:
      return;
  }
}

void add_record_seeds(const ClassifiedRecord& record,
                      const GroupingState& grouping,
                      std::vector<DiagnosticSeed>& diagnostics) {
  switch (record.status) {
    case LexicalStatus::blank:
      return;
    case LexicalStatus::missing_colon:
      add_seed(diagnostics, DiagnosticSeedCode::missing_colon,
               record.diagnostic_span);
      return;
    case LexicalStatus::empty_name:
      add_seed(diagnostics, DiagnosticSeedCode::empty_field_name,
               record.diagnostic_span);
      return;
    case LexicalStatus::non_ascii_name:
      add_seed(diagnostics, DiagnosticSeedCode::non_ascii_field_name,
               record.diagnostic_span);
      return;
    case LexicalStatus::directive:
      break;
  }

  switch (record.directive) {
    case DirectiveName::none:
      return;
    case DirectiveName::unknown:
      add_seed(diagnostics, DiagnosticSeedCode::unknown_directive,
               record.name_span);
      return;
    case DirectiveName::user_agent:
      if (record.value.empty()) {
        add_seed(diagnostics, DiagnosticSeedCode::empty_user_agent,
                 record.value_span);
      }
      return;
    case DirectiveName::allow:
    case DirectiveName::disallow:
      if (!grouping.has_open_group) {
        add_seed(diagnostics, DiagnosticSeedCode::orphan_access_rule,
                 record.name_span);
      }
      if (record.value.empty()) {
        add_seed(diagnostics, DiagnosticSeedCode::empty_access_rule,
                 record.value_span);
      } else if (record.value.front() != '/') {
        add_seed(diagnostics,
                 DiagnosticSeedCode::access_value_missing_slash,
                 record.value_span);
      }
      return;
    case DirectiveName::sitemap:
      if (record.value.empty()) {
        add_seed(diagnostics, DiagnosticSeedCode::empty_sitemap,
                 record.value_span);
      }
      return;
    case DirectiveName::crawl_delay:
      add_crawl_delay_seed(record, diagnostics);
      return;
  }
}

void commit_grouping(const ClassifiedRecord& record,
                     GroupingState& grouping) noexcept {
  if (is_recognized(record, DirectiveName::user_agent)) {
    if (record.value.empty()) {
      return;
    }
    if (!grouping.has_open_group || grouping.open_group_has_access) {
      grouping.has_open_group = true;
      grouping.open_group_has_access = false;
    }
    return;
  }

  if (is_access_record(record) && grouping.has_open_group) {
    grouping.open_group_has_access = true;
  }
}

ParserResult limit_failure(ParserStatus status,
                           DiagnosticSeedCode diagnostic_code,
                           ByteSpan diagnostic_span,
                           std::vector<DiagnosticSeed> diagnostics) {
  diagnostics.push_back({diagnostic_code, diagnostic_span});
  return {status, std::nullopt, std::move(diagnostics)};
}

}  // namespace

ParserResult parse_to_ir(std::string_view body) {
  const ScanResult scanned = scan_physical_lines(body);
  if (scanned.status == ScanStatus::body_limit_exceeded) {
    return limit_failure(
        ParserStatus::body_limit_exceeded,
        DiagnosticSeedCode::body_limit_exceeded,
        zero_length_span(scanned.limit_position), {});
  }

  std::vector<ClassifiedRecord> records;
  records.reserve(scanned.lines.size());
  std::vector<DiagnosticSeed> diagnostics;
  GroupingState grouping;
  std::size_t classified_record_count = 0;
  std::size_t access_rule_count = 0;

  for (const PhysicalLine& line : scanned.lines) {
    const ClassifiedRecord record = classify_directive(line);
    const bool counts_as_record = is_classified_record(record);
    const bool counts_as_rule = is_access_record(record);

    if (counts_as_record &&
        classified_record_count == kMaxClassifiedRecords) {
      return limit_failure(ParserStatus::record_limit_exceeded,
                           DiagnosticSeedCode::record_limit_exceeded,
                           record.name_span, std::move(diagnostics));
    }
    if (counts_as_rule && access_rule_count == kMaxAccessRules) {
      return limit_failure(ParserStatus::rule_limit_exceeded,
                           DiagnosticSeedCode::rule_limit_exceeded,
                           record.name_span, std::move(diagnostics));
    }

    add_record_seeds(record, grouping, diagnostics);
    commit_grouping(record, grouping);
    records.push_back(record);
    if (counts_as_record) {
      ++classified_record_count;
    }
    if (counts_as_rule) {
      ++access_rule_count;
    }
  }

  if (scanned.status == ScanStatus::line_length_limit_exceeded) {
    return limit_failure(
        ParserStatus::line_length_limit_exceeded,
        DiagnosticSeedCode::line_length_limit_exceeded,
        zero_length_span(scanned.limit_position), std::move(diagnostics));
  }

  return {ParserStatus::parsed,
          build_access_grouping_ir(std::move(records)),
          std::move(diagnostics)};
}

}  // namespace robotstxtbing::detail
