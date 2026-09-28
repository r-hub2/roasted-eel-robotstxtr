#ifndef ROBOTSTXTBING_LIMITS_H_
#define ROBOTSTXTBING_LIMITS_H_

#include <cstddef>

namespace robotstxtbing {

inline constexpr std::size_t kMaxBodyBytes = 2'097'152;
inline constexpr std::size_t kMaxPhysicalLineBytes = 65'536;
inline constexpr std::size_t kMaxClassifiedRecords = 65'536;
inline constexpr std::size_t kMaxAccessRules = 16'384;
inline constexpr std::size_t kMaxRequestTargetBytes = 65'536;
inline constexpr std::size_t kMaxMatcherWorkUnits = 67'108'864;

}  // namespace robotstxtbing

#endif  // ROBOTSTXTBING_LIMITS_H_
