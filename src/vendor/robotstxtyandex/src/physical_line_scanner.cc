#include "physical_line_scanner.h"

#include <string>

namespace robotstxtyandex::detail {

PhysicalLineScan::PhysicalLineScan(std::string_view body) : bytes_(body) {
  std::size_t start = 0;
  while (start < bytes_.size()) {
    const std::size_t lf = bytes_.find('\n', start);
    std::size_t end = bytes_.size();
    std::size_t next_start = bytes_.size();
    PhysicalLineTerminator terminator = PhysicalLineTerminator::none;

    if (lf != std::string::npos) {
      next_start = lf + 1;
      if (lf > start && bytes_[lf - 1] == '\r') {
        end = lf - 1;
        terminator = PhysicalLineTerminator::crlf;
      } else {
        end = lf;
        terminator = PhysicalLineTerminator::lf;
      }
    }

    lines_.push_back(
        {lines_.size() + 1, start, end, terminator});
    start = next_start;
  }
}

}  // namespace robotstxtyandex::detail
