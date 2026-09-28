#include "robotstxtyandex/version.h"

namespace robotstxtyandex {

Version version() noexcept { return Version{0, 2, 0, "0.2.0"}; }

CompatibilityProfileIdentity compatibility_profile_identity() noexcept {
  return CompatibilityProfileIdentity{
      "0.2.0",
      "yandex-0.1.0",
      "337b9f3b886a92d6dc08c2fce84228d0cd6b801a",
      "9d69d361db81e7d236562dc056b41865da33d467d06f316e2c9a20988e007c96",
      "337b9f3b886a92d6dc08c2fce84228d0cd6b801a",
  };
}

}  // namespace robotstxtyandex
