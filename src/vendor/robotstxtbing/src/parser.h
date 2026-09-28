#ifndef ROBOTSTXTBING_SRC_PARSER_H_
#define ROBOTSTXTBING_SRC_PARSER_H_

#include <optional>
#include <string_view>
#include <vector>

#include "access_grouping.h"
#include "directive_lexer.h"

namespace robotstxtbing::detail {

enum class ParserStatus {
  parsed,
  body_limit_exceeded,
  line_length_limit_exceeded,
  record_limit_exceeded,
  rule_limit_exceeded,
};

// Internal inputs for the later owning public-diagnostic layer. Keeping codes
// and byte spans here lets a limit failure retain diagnostics from committed
// lines without retaining or exposing a partial parser IR.
enum class DiagnosticSeedCode {
  unknown_directive,
  missing_colon,
  empty_field_name,
  non_ascii_field_name,
  empty_user_agent,
  orphan_access_rule,
  empty_access_rule,
  access_value_missing_slash,
  empty_sitemap,
  empty_crawl_delay,
  invalid_crawl_delay,
  crawl_delay_overflow,
  body_limit_exceeded,
  line_length_limit_exceeded,
  record_limit_exceeded,
  rule_limit_exceeded,
};

struct DiagnosticSeed {
  DiagnosticSeedCode code;
  ByteSpan span;
};

struct ParserResult {
  ParserStatus status;
  std::optional<AccessGroupingIr> ir;
  std::vector<DiagnosticSeed> diagnostics;
};

ParserResult parse_to_ir(std::string_view body);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_PARSER_H_
