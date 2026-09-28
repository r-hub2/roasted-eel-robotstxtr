#ifndef ROBOTSTXTYANDEX_VERSION_H_
#define ROBOTSTXTYANDEX_VERSION_H_

#include <string_view>

namespace robotstxtyandex {

struct Version {
  int major;
  int minor;
  int patch;
  std::string_view string;
};

struct CompatibilityProfileIdentity {
  std::string_view library_version;
  std::string_view profile_id;
  std::string_view accepted_corpus_revision;
  std::string_view evidence_snapshot;
  std::string_view profile_source_revision;
};

Version version() noexcept;
CompatibilityProfileIdentity compatibility_profile_identity() noexcept;

}  // namespace robotstxtyandex

#endif  // ROBOTSTXTYANDEX_VERSION_H_
