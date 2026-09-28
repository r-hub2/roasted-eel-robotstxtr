#ifndef ROBOTSTXTBING_VERSION_H_
#define ROBOTSTXTBING_VERSION_H_

#include <string_view>

#include "robotstxtbing/export.h"

namespace robotstxtbing {

struct ContractInfo {
  std::string_view library_version;
  std::string_view contract_id;
  std::string_view contract_revision;
  std::string_view parser_revision;
  std::string_view bingbot_profile_revision;
  std::string_view adidxbot_profile_revision;
  std::string_view release_manifest_sha256;
};

ROBOTSTXTBING_EXPORT const ContractInfo& contract_info() noexcept;

}  // namespace robotstxtbing

#endif  // ROBOTSTXTBING_VERSION_H_
