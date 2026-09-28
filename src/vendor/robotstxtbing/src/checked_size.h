#ifndef ROBOTSTXTBING_SRC_CHECKED_SIZE_H_
#define ROBOTSTXTBING_SRC_CHECKED_SIZE_H_

#include <cstddef>
#include <limits>

namespace robotstxtbing::detail {

struct CheckedSize {
  std::size_t value;
  bool overflow;
};

constexpr CheckedSize checked_add(std::size_t left,
                                  std::size_t right) noexcept {
  if (right > std::numeric_limits<std::size_t>::max() - left) {
    return {0, true};
  }
  return {left + right, false};
}

constexpr bool sum_exceeds(std::size_t left, std::size_t right,
                           std::size_t limit) noexcept {
  const CheckedSize sum = checked_add(left, right);
  return sum.overflow || sum.value > limit;
}

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_CHECKED_SIZE_H_
