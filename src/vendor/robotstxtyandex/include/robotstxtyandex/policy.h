#ifndef ROBOTSTXTYANDEX_POLICY_H_
#define ROBOTSTXTYANDEX_POLICY_H_

#include <memory>
#include <string_view>
#include <vector>

#include "robotstxtyandex/diagnostic.h"
#include "robotstxtyandex/metadata.h"
#include "robotstxtyandex/result.h"

namespace robotstxtyandex {

struct ParseResult;

class Policy {
 public:
  Policy(const Policy&) noexcept;
  Policy(Policy&&) noexcept;
  Policy& operator=(const Policy&) noexcept;
  Policy& operator=(Policy&&) noexcept;
  ~Policy();

  static ParseResult Parse(std::string_view body);

  MatchResult Evaluate(std::string_view crawler,
                       std::string_view request_target) const;

  CheckedEvaluationResult EvaluateChecked(
      std::string_view crawler, std::string_view request_target) const;

  const std::vector<SitemapEntry>& sitemaps() const noexcept;
  const std::vector<CleanParamRule>& clean_params() const noexcept;

 private:
  struct Impl;
  explicit Policy(std::shared_ptr<const Impl> impl) noexcept;

  std::shared_ptr<const Impl> impl_;
};

struct ParseResult {
  Policy policy;
  std::vector<Diagnostic> diagnostics;
};

}  // namespace robotstxtyandex

#endif  // ROBOTSTXTYANDEX_POLICY_H_
