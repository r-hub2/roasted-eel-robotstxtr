#include "robotstxtyandex/policy.h"

#include <memory>
#include <utility>

#include "directive_lexer.h"
#include "parser_ir.h"
#include "physical_line_scanner.h"
#include "policy_engine.h"

namespace robotstxtyandex {

struct Policy::Impl {
  explicit Impl(detail::ParserIr parser_ir_value)
      : parser_ir(std::move(parser_ir_value)) {}

  const detail::ParserIr parser_ir;
};

Policy::Policy(std::shared_ptr<const Impl> impl) noexcept
    : impl_(std::move(impl)) {}

Policy::Policy(const Policy&) noexcept = default;
Policy::Policy(Policy&&) noexcept = default;
Policy& Policy::operator=(const Policy&) noexcept = default;
Policy& Policy::operator=(Policy&&) noexcept = default;
Policy::~Policy() = default;

ParseResult Policy::Parse(std::string_view body) {
  const detail::PhysicalLineScan scan(body);
  const detail::DirectiveLex lex(scan);
  detail::ParserIr parser_ir(lex);
  std::vector<Diagnostic> diagnostics = parser_ir.diagnostics();
  return {Policy(std::make_shared<Impl>(std::move(parser_ir))),
          std::move(diagnostics)};
}

MatchResult Policy::Evaluate(std::string_view crawler,
                             std::string_view request_target) const {
  return detail::EvaluateAccess(impl_->parser_ir, crawler, request_target);
}

CheckedEvaluationResult Policy::EvaluateChecked(
    std::string_view crawler, std::string_view request_target) const {
  return detail::EvaluateAccessChecked(impl_->parser_ir, crawler,
                                       request_target);
}

const std::vector<SitemapEntry>& Policy::sitemaps() const noexcept {
  return impl_->parser_ir.sitemaps();
}

const std::vector<CleanParamRule>& Policy::clean_params() const noexcept {
  return impl_->parser_ir.clean_params();
}

}  // namespace robotstxtyandex
