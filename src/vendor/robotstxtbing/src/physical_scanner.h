#ifndef ROBOTSTXTBING_SRC_PHYSICAL_SCANNER_H_
#define ROBOTSTXTBING_SRC_PHYSICAL_SCANNER_H_

#include <cstddef>
#include <string_view>
#include <vector>

namespace robotstxtbing::detail {

struct BytePosition {
  std::size_t line;
  std::size_t byte_column;
  std::size_t byte_offset;
};

struct PhysicalLine {
  std::string_view payload;
  std::size_t line;
  std::size_t byte_offset;

  BytePosition position_at(std::size_t payload_offset) const noexcept {
    return {line, payload_offset + 1, byte_offset + payload_offset};
  }
};

enum class ScanStatus {
  scanned,
  body_limit_exceeded,
  line_length_limit_exceeded,
};

struct ScanResult {
  ScanStatus status;
  std::vector<PhysicalLine> lines;
  BytePosition limit_position;
};

ScanResult scan_physical_lines(std::string_view body);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_PHYSICAL_SCANNER_H_
