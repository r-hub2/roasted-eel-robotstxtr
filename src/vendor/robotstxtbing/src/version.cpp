#include "robotstxtbing/version.h"

// Compile-time contract inputs (spec v2 SS5). A release build injects each
// value as a PRIVATE compile definition (see CMakeLists.txt and
// cmake/ReleaseManifestDigest.cmake). An unconfigured build compiles these
// explicit development placeholders, so the static views are always
// well-defined. Runtime code never parses the manifest: the digest is injected
// here at build time and is non-self-referential.
#ifndef ROBOTSTXTBING_LIBRARY_VERSION
#define ROBOTSTXTBING_LIBRARY_VERSION "0.0.0-dev"
#endif
#ifndef ROBOTSTXTBING_CONTRACT_ID
#define ROBOTSTXTBING_CONTRACT_ID "robotstxtbing-v2"
#endif
#ifndef ROBOTSTXTBING_CONTRACT_REVISION
#define ROBOTSTXTBING_CONTRACT_REVISION "0.0.0-dev"
#endif
#ifndef ROBOTSTXTBING_PARSER_REVISION
#define ROBOTSTXTBING_PARSER_REVISION "0.0.0-dev"
#endif
#ifndef ROBOTSTXTBING_BINGBOT_PROFILE_REVISION
#define ROBOTSTXTBING_BINGBOT_PROFILE_REVISION "0.0.0-dev"
#endif
#ifndef ROBOTSTXTBING_ADIDXBOT_PROFILE_REVISION
#define ROBOTSTXTBING_ADIDXBOT_PROFILE_REVISION "0.0.0-dev"
#endif
#ifndef ROBOTSTXTBING_RELEASE_MANIFEST_SHA256
#define ROBOTSTXTBING_RELEASE_MANIFEST_SHA256 ""
#endif

namespace robotstxtbing {

// The ContractInfo lives in static storage with a single stable address: every
// string_view member points into a string literal (static storage duration),
// so the returned reference and its views stay valid for the program lifetime.
const ContractInfo& contract_info() noexcept {
  static constexpr ContractInfo info{
      ROBOTSTXTBING_LIBRARY_VERSION,
      ROBOTSTXTBING_CONTRACT_ID,
      ROBOTSTXTBING_CONTRACT_REVISION,
      ROBOTSTXTBING_PARSER_REVISION,
      ROBOTSTXTBING_BINGBOT_PROFILE_REVISION,
      ROBOTSTXTBING_ADIDXBOT_PROFILE_REVISION,
      ROBOTSTXTBING_RELEASE_MANIFEST_SHA256,
  };
  return info;
}

}  // namespace robotstxtbing
