#include "directive_lexer.h"

#include <array>
#include <string_view>

namespace robotstxtyandex::detail {
namespace {

struct KnownField {
  std::string_view name;
  DirectiveType type;
};

constexpr std::array<KnownField, 6> kKnownFields{{
    {"User-agent", DirectiveType::user_agent},
    {"Allow", DirectiveType::allow},
    {"Disallow", DirectiveType::disallow},
    {"Sitemap", DirectiveType::sitemap},
    {"Clean-param", DirectiveType::clean_param},
    {"Crawl-delay", DirectiveType::crawl_delay},
}};

bool IsSyntaxWhitespace(char byte) {
  return byte == ' ' || byte == '\t';
}

unsigned char FoldAscii(unsigned char byte) {
  if (byte >= static_cast<unsigned char>('A') &&
      byte <= static_cast<unsigned char>('Z')) {
    return static_cast<unsigned char>(byte + ('a' - 'A'));
  }
  return byte;
}

bool AsciiCaseEqual(std::string_view left, std::string_view right) {
  if (left.size() != right.size()) {
    return false;
  }
  for (std::size_t index = 0; index < left.size(); ++index) {
    if (FoldAscii(static_cast<unsigned char>(left[index])) !=
        FoldAscii(static_cast<unsigned char>(right[index]))) {
      return false;
    }
  }
  return true;
}

std::optional<DirectiveType> ClassifyField(std::string_view field) {
  for (const KnownField& known : kKnownFields) {
    if (AsciiCaseEqual(field, known.name)) {
      return known.type;
    }
  }
  return std::nullopt;
}

}  // namespace

DirectiveLex::DirectiveLex(const PhysicalLineScan& scan) {
  lines_.reserve(scan.lines().size());
  for (const PhysicalLine& physical : scan.lines()) {
    const std::string_view payload = scan.bytes().substr(
        physical.start_offset, physical.end_offset - physical.start_offset);
    const std::size_t comment_offset = payload.find('#');
    const bool has_comment = comment_offset != std::string_view::npos;
    std::size_t content_end = has_comment ? comment_offset : payload.size();
    // Only ASCII SP and HTAB are syntax padding. Do not consult the locale or
    // reinterpret any other byte as whitespace.
    while (content_end > 0 &&
           IsSyntaxWhitespace(payload[content_end - 1])) {
      --content_end;
    }

    const std::optional<std::size_t> comment_column =
        has_comment
            ? std::optional<std::size_t>{comment_offset + 1}
            : std::nullopt;
    if (content_end == 0) {
      lines_.push_back({physical.number,
                        has_comment ? LexedLineKind::comment
                                    : LexedLineKind::blank,
                        std::nullopt,
                        0,
                        0,
                        comment_column,
                        {},
                        {}});
      continue;
    }

    std::size_t content_begin = 0;
    while (content_begin < content_end &&
           IsSyntaxWhitespace(payload[content_begin])) {
      ++content_begin;
    }
    const std::string_view content =
        payload.substr(content_begin, content_end - content_begin);
    const std::size_t colon = content.find(':');
    if (colon == std::string_view::npos || colon == 0) {
      lines_.push_back({physical.number,
                        LexedLineKind::malformed,
                        std::nullopt,
                        content_begin + 1,
                        0,
                        comment_column,
                        std::string(content),
                        {}});
      continue;
    }

    const std::string_view field = content.substr(0, colon);
    std::size_t value_start_in_content = colon + 1;
    while (value_start_in_content < content.size() &&
           IsSyntaxWhitespace(content[value_start_in_content])) {
      ++value_start_in_content;
    }
    const std::size_t value_start = content_begin + value_start_in_content;
    const std::optional<DirectiveType> directive = ClassifyField(field);
    lines_.push_back({physical.number,
                      directive ? LexedLineKind::directive
                                : LexedLineKind::unknown_field,
                      directive,
                      content_begin + 1,
                      value_start + 1,
                      comment_column,
                      std::string(field),
                      std::string(content.substr(value_start_in_content))});
  }
}

}  // namespace robotstxtyandex::detail
