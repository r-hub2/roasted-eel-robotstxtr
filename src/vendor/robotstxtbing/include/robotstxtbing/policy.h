#ifndef ROBOTSTXTBING_POLICY_H_
#define ROBOTSTXTBING_POLICY_H_

#include <memory>
#include <string_view>
#include <vector>

#include "robotstxtbing/export.h"
#include "robotstxtbing/metadata.h"

namespace robotstxtbing {

struct ParseResult;
struct EvaluationResult;

class ROBOTSTXTBING_EXPORT Policy {
 public:
  Policy(const Policy&) noexcept;
  Policy(Policy&&) noexcept;
  Policy& operator=(const Policy&) noexcept;
  Policy& operator=(Policy&&) noexcept;
  ~Policy();

  static ParseResult Parse(std::string_view body);

  EvaluationResult Evaluate(std::string_view product_token,
                            std::string_view request_target) const;

  const std::vector<SitemapEntry>& sitemaps() const noexcept;
  const std::vector<CrawlDelayEntry>& crawl_delays() const noexcept;

 private:
  struct Impl;
  explicit Policy(std::shared_ptr<const Impl> impl) noexcept;

  std::shared_ptr<const Impl> impl_;
};

}  // namespace robotstxtbing

#endif  // ROBOTSTXTBING_POLICY_H_
