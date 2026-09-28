#ifndef ROBOTSTXTBING_SRC_DIRECTIVE_LEXER_H_
#define ROBOTSTXTBING_SRC_DIRECTIVE_LEXER_H_

#include <cstddef>
#include <string_view>

#include "physical_scanner.h"

namespace robotstxtbing::detail {

struct ByteSpan {
  BytePosition begin;
  std::size_t byte_length;
};

enum class LexicalStatus {
  blank,
  directive,
  missing_colon,
  empty_name,
  non_ascii_name,
};

enum class DirectiveName {
  none,
  user_agent,
  allow,
  disallow,
  sitemap,
  crawl_delay,
  unknown,
};

struct ClassifiedRecord {
  PhysicalLine source;
  LexicalStatus status;
  DirectiveName directive;

  // These views refer to source.payload. `syntax` ends immediately before the
  // first '#'; `comment` begins with that '#'. The parsed name and value have
  // ASCII space/tab OWS removed from both ends, but retain every other byte.
  std::string_view syntax;
  std::string_view comment;
  std::string_view name;
  std::string_view value;

  ByteSpan name_span;
  ByteSpan value_span;

  // The source range used by a later diagnostic layer. Missing tokens and an
  // empty directive name use a zero-length span at the expected-token point.
  ByteSpan diagnostic_span;
};

ClassifiedRecord classify_directive(PhysicalLine line) noexcept;

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_DIRECTIVE_LEXER_H_
