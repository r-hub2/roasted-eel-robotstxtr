#include "parser_ir.h"

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace robotstxtyandex::detail {
namespace {

bool IsSyntaxWhitespace(char byte) {
  return byte == ' ' || byte == '\t';
}

bool IsCanonicalAccessRule(std::string_view value) {
  return value.empty() || value.front() == '/' || value.front() == '*';
}

std::optional<CleanParamRule> ParseCleanParam(const LexedLine& line) {
  const std::string_view value = line.value;
  std::size_t parameters_end = 0;
  while (parameters_end < value.size() &&
         !IsSyntaxWhitespace(value[parameters_end])) {
    ++parameters_end;
  }
  if (parameters_end == 0) {
    return std::nullopt;
  }

  std::vector<std::string> parameters;
  const std::string_view parameter_list = value.substr(0, parameters_end);
  std::size_t parameter_start = 0;
  while (parameter_start <= parameter_list.size()) {
    const std::size_t separator = parameter_list.find('&', parameter_start);
    const std::size_t parameter_end =
        separator == std::string_view::npos ? parameter_list.size()
                                            : separator;
    if (parameter_end == parameter_start) {
      return std::nullopt;
    }
    parameters.emplace_back(parameter_list.substr(
        parameter_start, parameter_end - parameter_start));
    if (separator == std::string_view::npos) {
      break;
    }
    parameter_start = separator + 1;
  }

  std::size_t path_start = parameters_end;
  while (path_start < value.size() && IsSyntaxWhitespace(value[path_start])) {
    ++path_start;
  }

  std::optional<std::string> path_prefix;
  if (path_start < value.size()) {
    const std::string_view path = value.substr(path_start);
    if (path.front() != '/') {
      return std::nullopt;
    }
    for (const char byte : path) {
      if (IsSyntaxWhitespace(byte)) {
        return std::nullopt;
      }
    }
    path_prefix = std::string(path);
  }

  return CleanParamRule{line.line, std::move(parameters),
                        std::move(path_prefix)};
}

Diagnostic MakeDiagnostic(DiagnosticSeverity severity, DiagnosticCode code,
                          std::size_t line, std::size_t column,
                          std::string detail) {
  return Diagnostic{severity, code, SourceLocation{line, column},
                    std::move(detail)};
}

}  // namespace

ParserIr::ParserIr(const DirectiveLex& lex) {
  std::optional<std::size_t> current_group;
  bool current_group_has_rule_content = false;

  for (const LexedLine& line : lex.lines()) {
    if (line.kind == LexedLineKind::blank ||
        line.kind == LexedLineKind::comment) {
      continue;
    }
    if (line.kind == LexedLineKind::malformed) {
      diagnostics_.push_back(MakeDiagnostic(
          DiagnosticSeverity::error, DiagnosticCode::malformed_line,
          line.line, line.field_column, "line is not a valid directive"));
      continue;
    }
    if (line.kind == LexedLineKind::unknown_field) {
      diagnostics_.push_back(MakeDiagnostic(
          DiagnosticSeverity::warning, DiagnosticCode::unsupported_directive,
          line.line, line.field_column, "directive is not supported"));
      continue;
    }

    switch (*line.directive) {
      case DirectiveType::user_agent:
        if (line.value.empty()) {
          diagnostics_.push_back(MakeDiagnostic(
              DiagnosticSeverity::error, DiagnosticCode::empty_user_agent,
              line.line, line.value_column, "user-agent value is empty"));
          // An invalid attempted group boundary must not attach later rules to
          // the preceding valid group. This deterministic recovery fails open.
          current_group.reset();
          current_group_has_rule_content = false;
          break;
        }
        if (!current_group.has_value() || current_group_has_rule_content) {
          groups_.push_back(
              {SourceLocation{line.line, line.field_column}, {}, {}});
          current_group = groups_.size() - 1;
          current_group_has_rule_content = false;
        }
        groups_[*current_group].crawlers.push_back(
            {SourceLocation{line.line, line.value_column}, line.value});
        break;

      case DirectiveType::allow:
      case DirectiveType::disallow: {
        if (!current_group.has_value()) {
          diagnostics_.push_back(MakeDiagnostic(
              DiagnosticSeverity::error, DiagnosticCode::orphan_directive,
              line.line, line.field_column,
              "access rule has no preceding user-agent"));
          break;
        }
        // A recognized access directive ends the stacked User-agent phase
        // even when its value is rejected. Otherwise a later group could be
        // merged backward and make recovery more restrictive.
        current_group_has_rule_content = true;
        if (!IsCanonicalAccessRule(line.value)) {
          diagnostics_.push_back(MakeDiagnostic(
              DiagnosticSeverity::error, DiagnosticCode::invalid_rule,
              line.line, line.value_column,
              "access rule must be empty or begin with '/' or '*'"));
          break;
        }
        const RuleType type = *line.directive == DirectiveType::allow
                                  ? RuleType::allow
                                  : RuleType::disallow;
        groups_[*current_group].rules.push_back(
            {SourceLocation{line.line, line.value_column}, type, line.value});
        break;
      }

      case DirectiveType::sitemap:
        if (line.value.empty()) {
          diagnostics_.push_back(MakeDiagnostic(
              DiagnosticSeverity::error, DiagnosticCode::invalid_rule,
              line.line, line.value_column, "sitemap value is empty"));
          break;
        }
        sitemaps_.push_back({line.line, line.value});
        break;

      case DirectiveType::clean_param: {
        std::optional<CleanParamRule> rule = ParseCleanParam(line);
        if (!rule.has_value()) {
          diagnostics_.push_back(MakeDiagnostic(
              DiagnosticSeverity::error, DiagnosticCode::invalid_rule,
              line.line, line.value_column,
              "clean-param value does not use canonical syntax"));
          break;
        }
        clean_params_.push_back(std::move(*rule));
        break;
      }

      case DirectiveType::crawl_delay:
        diagnostics_.push_back(MakeDiagnostic(
            DiagnosticSeverity::warning, DiagnosticCode::ignored_directive,
            line.line, line.field_column, "crawl-delay is ignored"));
        break;
    }
  }
}

}  // namespace robotstxtyandex::detail
