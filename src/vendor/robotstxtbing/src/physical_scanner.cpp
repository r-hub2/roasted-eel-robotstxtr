#include "physical_scanner.h"

#include <cstddef>
#include <string_view>
#include <utility>
#include <vector>

#include "robotstxtbing/limits.h"

namespace robotstxtbing::detail {

ScanResult scan_physical_lines(std::string_view body) {
  if (body.size() > kMaxBodyBytes) {
    return {ScanStatus::body_limit_exceeded, {}, {1, 1, 0}};
  }

  std::vector<PhysicalLine> lines;
  std::size_t line_start = 0;
  std::size_t line_number = 1;

  while (line_start < body.size()) {
    const std::size_t lf = body.find('\n', line_start);
    std::size_t payload_end = lf == std::string_view::npos ? body.size() : lf;
    if (payload_end > line_start && body[payload_end - 1] == '\r' &&
        lf != std::string_view::npos) {
      --payload_end;
    }

    const std::size_t payload_size = payload_end - line_start;
    if (payload_size > kMaxPhysicalLineBytes) {
      return {ScanStatus::line_length_limit_exceeded,
              std::move(lines),
              {line_number, kMaxPhysicalLineBytes + 1,
               line_start + kMaxPhysicalLineBytes}};
    }

    lines.push_back(
        {body.substr(line_start, payload_size), line_number, line_start});

    if (lf == std::string_view::npos) {
      break;
    }
    line_start = lf + 1;
    ++line_number;
  }

  return {ScanStatus::scanned, std::move(lines), {0, 0, 0}};
}

}  // namespace robotstxtbing::detail
