#include "directive_lexer.h"

#include <cstddef>
#include <string_view>

namespace robotstxtbing::detail {
namespace {

struct ByteRange {
  std::size_t begin;
  std::size_t end;
};

constexpr bool is_ows(char byte) noexcept {
  return byte == ' ' || byte == '\t';
}

ByteRange trim_ows(std::string_view bytes, ByteRange range) noexcept {
  while (range.begin < range.end && is_ows(bytes[range.begin])) {
    ++range.begin;
  }
  while (range.end > range.begin && is_ows(bytes[range.end - 1])) {
    --range.end;
  }
  return range;
}

constexpr unsigned char ascii_lower(unsigned char byte) noexcept {
  if (byte >= static_cast<unsigned char>('A') &&
      byte <= static_cast<unsigned char>('Z')) {
    return static_cast<unsigned char>(byte + ('a' - 'A'));
  }
  return byte;
}

bool ascii_fold_equal(std::string_view actual,
                      std::string_view expected_lowercase) noexcept {
  if (actual.size() != expected_lowercase.size()) {
    return false;
  }
  for (std::size_t index = 0; index < actual.size(); ++index) {
    const auto byte = static_cast<unsigned char>(actual[index]);
    if (ascii_lower(byte) !=
        static_cast<unsigned char>(expected_lowercase[index])) {
      return false;
    }
  }
  return true;
}

DirectiveName classify_name(std::string_view name) noexcept {
  if (ascii_fold_equal(name, "user-agent")) {
    return DirectiveName::user_agent;
  }
  if (ascii_fold_equal(name, "allow")) {
    return DirectiveName::allow;
  }
  if (ascii_fold_equal(name, "disallow")) {
    return DirectiveName::disallow;
  }
  if (ascii_fold_equal(name, "sitemap")) {
    return DirectiveName::sitemap;
  }
  if (ascii_fold_equal(name, "crawl-delay")) {
    return DirectiveName::crawl_delay;
  }
  return DirectiveName::unknown;
}

ByteSpan span_at(const PhysicalLine& line, ByteRange range) noexcept {
  return {line.position_at(range.begin), range.end - range.begin};
}

ClassifiedRecord base_record(PhysicalLine line, std::string_view syntax,
                             std::string_view comment) noexcept {
  const ByteSpan start = {line.position_at(0), 0};
  return {line,
          LexicalStatus::blank,
          DirectiveName::none,
          syntax,
          comment,
          {},
          {},
          start,
          start,
          start};
}

}  // namespace

ClassifiedRecord classify_directive(PhysicalLine line) noexcept {
  const std::size_t hash = line.payload.find('#');
  const std::size_t syntax_end =
      hash == std::string_view::npos ? line.payload.size() : hash;
  const std::string_view syntax = line.payload.substr(0, syntax_end);
  const std::string_view comment =
      hash == std::string_view::npos ? std::string_view{}
                                     : line.payload.substr(hash);
  ClassifiedRecord result = base_record(line, syntax, comment);

  const ByteRange logical = trim_ows(line.payload, {0, syntax_end});
  if (logical.begin == logical.end) {
    const ByteSpan blank_position = span_at(line, logical);
    result.name_span = blank_position;
    result.value_span = blank_position;
    result.diagnostic_span = blank_position;
    return result;
  }

  const std::size_t colon = line.payload.find(':', logical.begin);
  if (colon == std::string_view::npos || colon >= logical.end) {
    result.status = LexicalStatus::missing_colon;
    result.name = line.payload.substr(logical.begin,
                                      logical.end - logical.begin);
    result.name_span = span_at(line, logical);
    result.value_span = span_at(line, {logical.end, logical.end});
    result.diagnostic_span = result.value_span;
    return result;
  }

  const ByteRange name_range =
      trim_ows(line.payload, {logical.begin, colon});
  const ByteRange value_range =
      trim_ows(line.payload, {colon + 1, logical.end});
  result.name = line.payload.substr(name_range.begin,
                                    name_range.end - name_range.begin);
  result.value = line.payload.substr(value_range.begin,
                                     value_range.end - value_range.begin);
  result.name_span = span_at(line, name_range);
  result.value_span = span_at(line, value_range);

  if (name_range.begin == name_range.end) {
    result.status = LexicalStatus::empty_name;
    result.diagnostic_span = result.name_span;
    return result;
  }

  for (std::size_t offset = name_range.begin; offset < name_range.end;
       ++offset) {
    if (static_cast<unsigned char>(line.payload[offset]) >= 0x80U) {
      result.status = LexicalStatus::non_ascii_name;
      result.diagnostic_span = span_at(line, {offset, offset + 1});
      return result;
    }
  }

  result.status = LexicalStatus::directive;
  result.directive = classify_name(result.name);
  result.diagnostic_span = result.name_span;
  return result;
}

}  // namespace robotstxtbing::detail
