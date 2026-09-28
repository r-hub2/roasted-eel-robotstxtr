// cpp11 binding for the vendored googlebot robots.txt parse collector.
//
// This file is NOT part of the vendored upstream snapshot; it is the R package's
// own glue and carries no Google/Apache header. The vendored engine
// (robots.{cc,h}, reporting_robots.{cc,h}) preserves its Apache-2.0 headers
// verbatim.
//
// Slice R3 surface: run ParseRobotsTxt() once over a robots.txt body with a
// private, read-only RobotsParseHandler collector and return the per-line
// directives the callbacks delivered -- (line_num, callback-derived type,
// callback value). The R layer joins a positive matching_line() to this
// per-source lookup to populate matched_rule_type / matched_rule_value.
//
// The vendored RobotsParsingReporter deliberately does NOT retain the callback
// value (see reporting_robots.h: RobotsParsedLine stores only line/tag/typo/
// metadata), and the PRD forbids assuming it does. This binding therefore
// defines its own lightweight handler that keeps the value each Handle* callback
// delivers. For Allow/Disallow keys that value is the canonical, post-
// MaybeEscapePattern string the matcher actually uses (robots.cc
// ParseAndEmitLine escapes the value before emitting it); R must not re-derive a
// pre-escape form.

#include <cstdint>
#include <map>
#include <string>
#include <string_view>
#include <vector>

#include <cpp11.hpp>

#include "robots.h"

namespace {

// Minimal, self-contained UTF-8 well-formedness check. Used only to decide the
// R string encoding mark (CE_UTF8 vs CE_BYTES) for a collected callback value,
// so that a non-UTF-8 value is surfaced verbatim as an `Encoding = "bytes"`
// string instead of triggering a translation error. Rejects overlong forms,
// UTF-16 surrogates, and code points above U+10FFFF.
bool is_valid_utf8(const std::string& s) {
  const unsigned char* b = reinterpret_cast<const unsigned char*>(s.data());
  const size_t n = s.size();
  size_t i = 0;
  while (i < n) {
    const unsigned char c = b[i];
    if (c < 0x80) {
      i += 1;
    } else if ((c >> 5) == 0x6) {  // 110xxxxx: 2-byte sequence
      if (c < 0xC2) return false;  // overlong
      if (i + 1 >= n || (b[i + 1] & 0xC0) != 0x80) return false;
      i += 2;
    } else if ((c >> 4) == 0xE) {  // 1110xxxx: 3-byte sequence
      if (i + 2 >= n || (b[i + 1] & 0xC0) != 0x80 ||
          (b[i + 2] & 0xC0) != 0x80) {
        return false;
      }
      if (c == 0xE0 && b[i + 1] < 0xA0) return false;  // overlong
      if (c == 0xED && b[i + 1] > 0x9F) return false;  // surrogate
      i += 3;
    } else if ((c >> 3) == 0x1E) {  // 11110xxx: 4-byte sequence
      if (i + 3 >= n || (b[i + 1] & 0xC0) != 0x80 ||
          (b[i + 2] & 0xC0) != 0x80 || (b[i + 3] & 0xC0) != 0x80) {
        return false;
      }
      if (c == 0xF0 && b[i + 1] < 0x90) return false;         // overlong
      if (c > 0xF4 || (c == 0xF4 && b[i + 1] > 0x8F)) return false;  // > U+10FFFF
      i += 4;
    } else {
      return false;
    }
  }
  return true;
}

// Read-only parse handler that records what each directive callback delivered:
// the one-based line number, a stable lowercase type string, and the exact
// callback value. It stores every directive kind so the lookup is complete, but
// a matching_line() only ever points at an Allow/Disallow, so those are the
// values that reach matched_rule_value.
class DirectiveCollector : public googlebot::RobotsParseHandler {
 public:
  void HandleRobotsStart() override {}
  void HandleRobotsEnd() override {}
  void HandleUserAgent(int line_num, std::string_view value) override {
    Record(line_num, "user-agent", value);
  }
  void HandleAllow(int line_num, std::string_view value) override {
    Record(line_num, "allow", value);
  }
  void HandleDisallow(int line_num, std::string_view value) override {
    Record(line_num, "disallow", value);
  }
  void HandleSitemap(int line_num, std::string_view value) override {
    Record(line_num, "sitemap", value);
  }
  void HandleUnknownAction(int line_num, std::string_view /*action*/,
                           std::string_view value) override {
    Record(line_num, "unknown", value);
  }

  std::vector<int> lines;
  std::vector<std::string> types;
  std::vector<std::string> values;

 private:
  void Record(int line_num, const char* type, std::string_view value) {
    lines.push_back(line_num);
    types.emplace_back(type);
    values.emplace_back(value);
  }
};

// Validation collector retaining every parser line, including metadata for
// blank, comment, malformed, accepted-typo, and overlong lines. Unlike the
// match-correlation collector above, this also retains an unknown directive's
// action so the public validation API can distinguish known-but-unsupported
// directives from genuinely unknown ones.
struct ValidationLine {
  int line = 0;
  std::string type = "none";
  std::string action;
  std::string value;
  googlebot::RobotsParseHandler::LineMetadata metadata;
};

class ValidationCollector : public googlebot::RobotsParseHandler {
 public:
  void HandleRobotsStart() override { lines.clear(); }
  void HandleRobotsEnd() override {}
  void HandleUserAgent(int line_num, std::string_view value) override {
    Record(line_num, "user-agent", "user-agent", value);
  }
  void HandleAllow(int line_num, std::string_view value) override {
    Record(line_num, "allow", "allow", value);
  }
  void HandleDisallow(int line_num, std::string_view value) override {
    Record(line_num, "disallow", "disallow", value);
  }
  void HandleSitemap(int line_num, std::string_view value) override {
    Record(line_num, "sitemap", "sitemap", value);
  }
  void HandleUnknownAction(int line_num, std::string_view action,
                           std::string_view value) override {
    Record(line_num, "unknown", action, value);
  }
  void ReportLineMetadata(
      int line_num,
      const googlebot::RobotsParseHandler::LineMetadata& metadata) override {
    ValidationLine& line = lines[line_num];
    line.line = line_num;
    line.metadata = metadata;
  }

  std::map<int, ValidationLine> lines;

 private:
  void Record(int line_num, const char* type, std::string_view action,
              std::string_view value) {
    ValidationLine& line = lines[line_num];
    line.line = line_num;
    line.type = type;
    line.action.assign(action.data(), action.size());
    line.value.assign(value.data(), value.size());
  }
};

cpp11::r_string encoded_string(const std::string& value) {
  const cetype_t enc = is_valid_utf8(value) ? CE_UTF8 : CE_BYTES;
  return cpp11::r_string(Rf_mkCharLenCE(
      value.data(), static_cast<int>(value.size()), enc));
}

}  // namespace

// Parse `body` once and return the collected directives as a named list of three
// equal-length columns: `line` (integer, one-based), `type` (character), and
// `value` (character; each element marked UTF-8 when well-formed, else bytes).
[[cpp11::register]]
cpp11::list robotstxtr_collect_directives_(std::string body) {
  DirectiveCollector collector;
  googlebot::ParseRobotsTxt(std::string_view(body), &collector);

  const R_xlen_t n = static_cast<R_xlen_t>(collector.lines.size());
  cpp11::writable::integers line_out(n);
  cpp11::writable::strings type_out(n);
  cpp11::writable::strings value_out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const size_t k = static_cast<size_t>(i);
    line_out[i] = collector.lines[k];
    type_out[i] = collector.types[k];  // ASCII type tokens: plain UTF-8.
    const std::string& val = collector.values[k];
    const cetype_t enc = is_valid_utf8(val) ? CE_UTF8 : CE_BYTES;
    value_out[i] = cpp11::r_string(
        Rf_mkCharLenCE(val.data(), static_cast<int>(val.size()), enc));
  }

  using namespace cpp11::literals;
  return cpp11::writable::list(
      {"line"_nm = line_out, "type"_nm = type_out, "value"_nm = value_out});
}

// Parse raw bytes once and return one row per parser line. Taking a raw vector
// rather than an R character scalar preserves embedded NUL and malformed UTF-8
// so the validator can diagnose the original document byte-for-byte.
[[cpp11::register]]
cpp11::list robotstxtr_validation_parse_(cpp11::raws body) {
  std::string bytes;
  bytes.reserve(static_cast<size_t>(body.size()));
  for (const uint8_t byte : body) {
    bytes.push_back(static_cast<char>(byte));
  }
  ValidationCollector collector;
  googlebot::ParseRobotsTxt(std::string_view(bytes), &collector);

  const R_xlen_t n = static_cast<R_xlen_t>(collector.lines.size());
  cpp11::writable::integers line_out(n);
  cpp11::writable::strings type_out(n);
  cpp11::writable::strings action_out(n);
  cpp11::writable::strings value_out(n);
  cpp11::writable::logicals is_empty_out(n);
  cpp11::writable::logicals has_comment_out(n);
  cpp11::writable::logicals is_comment_out(n);
  cpp11::writable::logicals has_directive_out(n);
  cpp11::writable::logicals is_typo_out(n);
  cpp11::writable::logicals line_too_long_out(n);
  cpp11::writable::logicals missing_colon_out(n);

  R_xlen_t i = 0;
  for (const auto& entry : collector.lines) {
    const ValidationLine& line = entry.second;
    line_out[i] = line.line;
    type_out[i] = line.type;
    action_out[i] = encoded_string(line.action);
    value_out[i] = encoded_string(line.value);
    is_empty_out[i] = line.metadata.is_empty;
    has_comment_out[i] = line.metadata.has_comment;
    is_comment_out[i] = line.metadata.is_comment;
    has_directive_out[i] = line.metadata.has_directive;
    is_typo_out[i] = line.metadata.is_acceptable_typo;
    line_too_long_out[i] = line.metadata.is_line_too_long;
    missing_colon_out[i] = line.metadata.is_missing_colon_separator;
    ++i;
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
      "line"_nm = line_out,
      "type"_nm = type_out,
      "action"_nm = action_out,
      "value"_nm = value_out,
      "is_empty"_nm = is_empty_out,
      "has_comment"_nm = has_comment_out,
      "is_comment"_nm = is_comment_out,
      "has_directive"_nm = has_directive_out,
      "is_typo"_nm = is_typo_out,
      "line_too_long"_nm = line_too_long_out,
      "missing_colon"_nm = missing_colon_out,
  });
}
