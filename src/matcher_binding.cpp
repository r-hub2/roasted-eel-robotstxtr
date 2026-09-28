// cpp11 binding for the vendored googlebot robots.txt matcher.
//
// This file is NOT part of the vendored upstream snapshot; it is the R package's
// own glue and carries no Google/Apache header. The vendored engine
// (robots.{cc,h}, reporting_robots.{cc,h}) preserves its Apache-2.0 headers
// verbatim.
//
// Slice R1 surface only: a scalar robots.txt body and scalar matcher user agent
// are matched against a vector of URLs. Each element yields the matcher's
// allow/deny decision plus the upstream one-based matching line, computed from a
// single matcher run per URL so the R layer can classify the happy-path
// decision_source (rule_allow / rule_disallow / default_allow) without a second
// pass. matching_line() itself is not surfaced as the R-facing matched_line
// column here; that (and full callback correlation) is slice R3.

#include <string>
#include <string_view>

#include <cpp11.hpp>

#include "robots.h"

// Returns TRUE/FALSE per URL: is `url` allowed for `user_agent` under `body`.
[[cpp11::register]]
cpp11::writable::logicals robotstxtr_allowed_text_(std::string body,
                                                   cpp11::strings url,
                                                   std::string user_agent) {
  cpp11::writable::logicals out(static_cast<R_xlen_t>(url.size()));
  googlebot::RobotsMatcher matcher;
  const std::string_view body_view(body);
  for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(url.size()); ++i) {
    const std::string u(url[i]);
    const bool allowed =
        matcher.OneAgentAllowedByRobots(body_view, user_agent, u);
    out[i] = allowed ? TRUE : FALSE;
  }
  return out;
}

// Returns the upstream one-based matching line per URL, or 0 when no directive
// matched (the matcher's own sentinel). Used by R only to split rule_allow from
// default_allow; the R-facing matched_line column stays NA until slice R3.
[[cpp11::register]]
cpp11::writable::integers robotstxtr_matching_line_text_(std::string body,
                                                         cpp11::strings url,
                                                         std::string user_agent) {
  cpp11::writable::integers out(static_cast<R_xlen_t>(url.size()));
  googlebot::RobotsMatcher matcher;
  const std::string_view body_view(body);
  for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(url.size()); ++i) {
    const std::string u(url[i]);
    matcher.OneAgentAllowedByRobots(body_view, user_agent, u);
    out[i] = matcher.matching_line();
  }
  return out;
}
