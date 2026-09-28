#ifndef ROBOTSTXTYANDEX_SRC_PHYSICAL_LINE_SCANNER_H_
#define ROBOTSTXTYANDEX_SRC_PHYSICAL_LINE_SCANNER_H_

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace robotstxtyandex::detail {

enum class PhysicalLineTerminator {
  none,
  lf,
  crlf,
};

struct PhysicalLine {
  std::size_t number;
  std::size_t start_offset;
  std::size_t end_offset;
  PhysicalLineTerminator terminator;
};

// Owns the exact input bytes. PhysicalLine values contain offsets rather than
// borrowed views, so the result remains valid after the caller's input is
// destroyed and after this value is copied or moved.
class PhysicalLineScan {
 public:
  explicit PhysicalLineScan(std::string_view body);

  std::string_view bytes() const noexcept { return bytes_; }
  const std::vector<PhysicalLine>& lines() const noexcept { return lines_; }

 private:
  std::string bytes_;
  std::vector<PhysicalLine> lines_;
};

}  // namespace robotstxtyandex::detail

#endif  // ROBOTSTXTYANDEX_SRC_PHYSICAL_LINE_SCANNER_H_
