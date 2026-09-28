#ifndef ROBOTSTXTYANDEX_SRC_POLICY_ENGINE_H_
#define ROBOTSTXTYANDEX_SRC_POLICY_ENGINE_H_

#include <string_view>

#include "parser_ir.h"
#include "robotstxtyandex/result.h"

namespace robotstxtyandex::detail {

// Evaluates an owning immutable parser IR through the complete 0.1.0 profile
// selector and access-rule winner pipeline. Unsupported crawler profiles and
// invalid request targets fail open with default_allow.
MatchResult EvaluateAccess(const ParserIr& ir, std::string_view crawler,
                           std::string_view request_target);

// Evaluates through the same profile selector and matcher while preserving
// caller-input failures outside MatchResult. Unsupported crawler takes
// precedence when both inputs are invalid.
CheckedEvaluationResult EvaluateAccessChecked(
    const ParserIr& ir, std::string_view crawler,
    std::string_view request_target);

}  // namespace robotstxtyandex::detail

#endif  // ROBOTSTXTYANDEX_SRC_POLICY_ENGINE_H_
