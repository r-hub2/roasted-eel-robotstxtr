// cpp11 binding: native robotstxtbing batch evaluation + request-target
// extractor + contract identity (ROBO-yhfjsbzo, BI4).
//
// This file is NOT part of the vendored upstream snapshot; it is the R package's
// own glue and carries no Bing/Apache header. It binds the vendored
// robotstxtbing public API (Policy::Parse + Policy::Evaluate + contract_info)
// declared under vendor/robotstxtbing/include (reached via the -Ivendor/... path
// in src/Makevars). It is engine-pure: it never calls the Google engine
// (robots.cc), the robotstxtyandex engine, or their URL behaviour, and it shares
// no parser/matcher/URL-normalizer with them.
//
// The three registered routines are Bing-named (there is no "no bing native
// entry point" grep invariant; the Yandex one existed only because Yandex reused
// a single engine-neutral routine name). Registering them does NOT activate the
// backend: nothing in the public facade calls them, they are not user-exported,
// and engine_matcher_availability_v1()[["bing"]] stays capability_unavailable
// until the atomic activation slice (BI5).
//
// Lifetime & batching (spec section 8): the robots body is accepted as a
// length-bearing RAW value so embedded NUL and invalid UTF-8 reach
// Policy::Parse(std::string_view) byte-for-byte, with no NUL-terminated C string
// and no R character translation. Bodies are supplied ALREADY DISTINCT as a list
// of raw vectors; each distinct body is parsed EXACTLY ONCE. When a parse hits a
// core ceiling (ParseStatus != parsed) there is no Policy to evaluate and every
// row on that body carries the parse status with no decision. Otherwise every row
// selecting that body (via the 1-based body_index grouping) is evaluated against
// the same Policy. Only OWNING R values are returned -- no external pointers, no
// retained views.
//
// Result surface (spec sections 10 & 11): per row the routine returns the exact
// native ParseStatus and EvaluationStatus, the crawl decision, the exact
// DecisionSource, the matched rule line/type, and the matched rule value as BOTH
// an exact-byte raw list element AND a convenience UTF-8 character (NA when the
// bytes are not valid UTF-8 or contain an embedded NUL). raw(0) (present, empty
// value) and NULL (absent rule) are kept semantically distinct. The reason and
// matcher_status mapping is a FACADE concern (BI5); this binding carries only the
// faithful native fields and never converts an absent decision to allow/disallow.

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <cpp11.hpp>

#include "robotstxtbing/policy.h"
#include "robotstxtbing/result.h"
#include "robotstxtbing/version.h"

namespace {

// Minimal, self-contained UTF-8 well-formedness check (rejects overlong forms,
// UTF-16 surrogates, and code points above U+10FFFF). Used only to decide
// whether the exact rule-value bytes can also be surfaced as a UTF-8 character
// scalar; the exact bytes are always preserved separately as a raw element.
// (Package glue, intentionally independent of the Yandex binding's copy -- these
// translation units share no code.)
bool is_valid_utf8(const std::string& s) {
  const unsigned char* b = reinterpret_cast<const unsigned char*>(s.data());
  const std::size_t n = s.size();
  std::size_t i = 0;
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
      if (c == 0xF0 && b[i + 1] < 0x90) return false;               // overlong
      if (c > 0xF4 || (c == 0xF4 && b[i + 1] > 0x8F)) return false;  // >U+10FFFF
      i += 4;
    } else {
      return false;
    }
  }
  return true;
}

// Copy a RAWSXP's bytes into an owning std::string verbatim -- preserving
// embedded NUL and invalid UTF-8 with no translation or NUL termination.
std::string raw_sexp_to_bytes(SEXP x) {
  if (TYPEOF(x) != RAWSXP) {
    cpp11::stop("expected a raw vector");
  }
  const R_xlen_t n = Rf_xlength(x);
  const Rbyte* p = RAW(x);
  return std::string(reinterpret_cast<const char*>(p),
                     static_cast<std::size_t>(n));
}

// Build an owning R raw vector carrying the exact bytes (raw(0) for empty).
cpp11::writable::raws bytes_to_raws(const std::string& s) {
  cpp11::writable::raws out(static_cast<R_xlen_t>(s.size()));
  for (std::size_t i = 0; i < s.size(); ++i) {
    out[static_cast<R_xlen_t>(i)] = static_cast<uint8_t>(s[i]);
  }
  return out;
}

// Exact rule-value bytes as a convenience UTF-8 character scalar -- but only
// when the bytes are valid UTF-8 with no embedded NUL; otherwise NA_character_
// (the exact bytes remain available via the raw column). An empty value is a
// valid UTF-8 empty string "".
cpp11::r_string value_utf8_or_na(const std::string& s) {
  if (is_valid_utf8(s) && s.find('\0') == std::string::npos) {
    return cpp11::r_string(
        Rf_mkCharLenCE(s.data(), static_cast<int>(s.size()), CE_UTF8));
  }
  return cpp11::r_string(NA_STRING);
}

const char* parse_status_name(robotstxtbing::ParseStatus status) {
  switch (status) {
    case robotstxtbing::ParseStatus::parsed:
      return "parsed";
    case robotstxtbing::ParseStatus::body_limit_exceeded:
      return "body_limit_exceeded";
    case robotstxtbing::ParseStatus::line_length_limit_exceeded:
      return "line_length_limit_exceeded";
    case robotstxtbing::ParseStatus::record_limit_exceeded:
      return "record_limit_exceeded";
    case robotstxtbing::ParseStatus::rule_limit_exceeded:
      return "rule_limit_exceeded";
  }
  return "parsed";  // unreachable; all enumerators handled above.
}

const char* evaluation_status_name(robotstxtbing::EvaluationStatus status) {
  switch (status) {
    case robotstxtbing::EvaluationStatus::evaluated:
      return "evaluated";
    case robotstxtbing::EvaluationStatus::invalid_request_target:
      return "invalid_request_target";
    case robotstxtbing::EvaluationStatus::unsupported_profile:
      return "unsupported_profile";
    case robotstxtbing::EvaluationStatus::request_target_limit_exceeded:
      return "request_target_limit_exceeded";
    case robotstxtbing::EvaluationStatus::work_limit_exceeded:
      return "work_limit_exceeded";
  }
  return "evaluated";  // unreachable; all enumerators handled above.
}

const char* decision_source_name(robotstxtbing::DecisionSource source) {
  switch (source) {
    case robotstxtbing::DecisionSource::rule:
      return "rule";
    case robotstxtbing::DecisionSource::default_allow:
      return "default_allow";
  }
  return "default_allow";  // unreachable; all enumerators handled above.
}

}  // namespace

// Evaluate a batch of (product_token, request-target) rows against a set of
// distinct robots bodies using the robotstxtbing public API, parsing each
// distinct body exactly once.
//
// The argument names here are short only to keep the generated cpp11 glue within
// the project 80-column lint budget; the readable signature is the internal R
// wrapper bing_evaluate_batch().
//
// Inputs:
//   bodies   list of raw vectors -- the DISTINCT robots bodies (bytes exact).
//   ids      integer, one per row, 1-based index into `bodies` (row->body group).
//   agents   character, one per row -- the product token to evaluate.
//   targets  list of raw vectors, one per row -- the exact request-target bytes
//            passed to Evaluate (byte-preserving).
//
// Returns a named list of equal-length per-row columns plus a scalar
// `n_parse_calls` (== number of distinct bodies parsed, an observable proof of
// parse-once batching). All returned values are owning and independent of the
// C++ state freed when the call returns.
[[cpp11::register]]
cpp11::list robotstxtr_bing_eval_batch_(cpp11::list bodies,
                                        cpp11::integers ids,
                                        cpp11::strings agents,
                                        cpp11::list targets) {
  const R_xlen_t n_bodies = bodies.size();
  const R_xlen_t n_rows = ids.size();
  if (agents.size() != n_rows || targets.size() != n_rows) {
    cpp11::stop("product tokens and targets must have one element per row");
  }

  // Parse each DISTINCT body exactly once. Keep the owning body bytes and the
  // owning ParseResult (which holds the Policy) alive for the whole call: the
  // Policy may reference the body bytes during matching.
  std::vector<std::string> body_bytes;
  std::vector<robotstxtbing::ParseResult> parsed;
  std::vector<int> body_len;
  body_bytes.reserve(static_cast<std::size_t>(n_bodies));
  parsed.reserve(static_cast<std::size_t>(n_bodies));
  body_len.reserve(static_cast<std::size_t>(n_bodies));
  for (R_xlen_t b = 0; b < n_bodies; ++b) {
    body_bytes.push_back(raw_sexp_to_bytes(bodies[b]));
    const std::string& bytes = body_bytes.back();
    parsed.push_back(robotstxtbing::Policy::Parse(std::string_view(bytes)));
    body_len.push_back(static_cast<int>(bytes.size()));
  }

  cpp11::writable::strings native_parse_status(n_rows);
  cpp11::writable::strings native_evaluation_status(n_rows);
  cpp11::writable::logicals url_decision(n_rows);
  cpp11::writable::strings decision_source(n_rows);
  cpp11::writable::integers matched_line(n_rows);
  cpp11::writable::strings matched_rule_type(n_rows);
  cpp11::writable::strings matched_rule_value(n_rows);
  cpp11::writable::list matched_rule_value_raw(n_rows);  // NULL by default
  cpp11::writable::integers matcher_input_bytes(n_rows);
  cpp11::writable::logicals matcher_body_truncated(n_rows);
  cpp11::writable::list matcher_request_target_raw(n_rows);

  for (R_xlen_t r = 0; r < n_rows; ++r) {
    const int idx1 = ids[r];
    if (idx1 < 1 || static_cast<R_xlen_t>(idx1) > n_bodies) {
      cpp11::stop("body index out of range");
    }
    const std::size_t b = static_cast<std::size_t>(idx1 - 1);

    const std::string product_token(agents[r]);
    const std::string target = raw_sexp_to_bytes(targets[r]);

    // Per-body / per-row constants. The Bing core never truncates a body (an
    // over-ceiling body yields ParseStatus::body_limit_exceeded and no Policy),
    // so matcher_body_truncated is structurally FALSE.
    matcher_input_bytes[r] = body_len[b];
    matcher_body_truncated[r] = cpp11::r_bool(FALSE);
    matcher_request_target_raw[r] = bytes_to_raws(target);
    native_parse_status[r] = parse_status_name(parsed[b].status);

    if (!parsed[b].policy.has_value()) {
      // A parse ceiling was hit: there is no Policy to evaluate. Every row on
      // this body is a non-decision carrying the parse status (spec section 8).
      native_evaluation_status[r] = cpp11::r_string(NA_STRING);
      url_decision[r] = cpp11::na<cpp11::r_bool>();
      decision_source[r] = cpp11::r_string(NA_STRING);
      matched_line[r] = NA_INTEGER;
      matched_rule_type[r] = "unknown";
      matched_rule_value[r] = cpp11::r_string(NA_STRING);
      // matched_rule_value_raw stays NULL (absent-rule representation).
      continue;
    }

    const robotstxtbing::EvaluationResult result =
        parsed[b].policy->Evaluate(std::string_view(product_token),
                                   std::string_view(target));
    native_evaluation_status[r] = evaluation_status_name(result.status);

    if (result.status != robotstxtbing::EvaluationStatus::evaluated) {
      // unsupported_profile / invalid_request_target /
      // request_target_limit_exceeded / work_limit_exceeded: no decision. The
      // precedence among these is decided inside the core; the adapter does not
      // re-prevalidate.
      url_decision[r] = cpp11::na<cpp11::r_bool>();
      decision_source[r] = cpp11::r_string(NA_STRING);
      matched_line[r] = NA_INTEGER;
      matched_rule_type[r] = "unknown";
      matched_rule_value[r] = cpp11::r_string(NA_STRING);
      // matched_rule_value_raw stays NULL.
      continue;
    }

    // Evaluated: a Decision is present.
    const robotstxtbing::Decision& decision = *result.decision;
    url_decision[r] = (decision.access == robotstxtbing::AccessDecision::allow)
                          ? cpp11::r_bool(TRUE)
                          : cpp11::r_bool(FALSE);
    decision_source[r] = decision_source_name(decision.source);

    if (!decision.matched_rule.has_value()) {
      // default_allow: absent-rule representation (NULL raw, NA text, no line,
      // type "none").
      matched_line[r] = NA_INTEGER;
      matched_rule_type[r] = "none";
      matched_rule_value[r] = cpp11::r_string(NA_STRING);
      // matched_rule_value_raw stays NULL.
      continue;
    }

    // A matched rule: retain the exact original line, type, and value bytes. An
    // effective empty Disallow surfaces natively as source = rule, access =
    // allow, type = disallow, value = "" (present raw(0), UTF-8 "") -- the
    // binding does NOT rewrite it; the reason is derived by the facade.
    const robotstxtbing::RuleMatch& rule = *decision.matched_rule;
    matched_line[r] = static_cast<int>(rule.line);
    matched_rule_type[r] =
        (rule.type == robotstxtbing::RuleType::allow) ? "allow" : "disallow";
    matched_rule_value[r] = value_utf8_or_na(rule.value);
    matched_rule_value_raw[r] = bytes_to_raws(rule.value);
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
      "native_parse_status"_nm = native_parse_status,
      "native_evaluation_status"_nm = native_evaluation_status,
      "url_decision"_nm = url_decision,
      "decision_source"_nm = decision_source,
      "matched_line"_nm = matched_line,
      "matched_rule_type"_nm = matched_rule_type,
      "matched_rule_value"_nm = matched_rule_value,
      "matched_rule_value_raw"_nm = matched_rule_value_raw,
      "matcher_input_bytes"_nm = matcher_input_bytes,
      "matcher_body_truncated"_nm = matcher_body_truncated,
      "matcher_request_target_raw"_nm = matcher_request_target_raw,
      "n_parse_calls"_nm =
          cpp11::writable::integers({static_cast<int>(n_bodies)}),
  });
}

// Convert one absolute HTTP(S) URL (UTF-8 bytes) into its origin-form request
// target, or NA_character_ on lexical failure (spec section 9). This is Bing's
// OWN extractor -- an independent lexical transform that reuses no Google or
// Yandex URL behaviour. It removes only scheme and authority, substitutes "/"
// when no explicit path exists, retains the query delimiter and exact query
// spelling (including a bare, empty "?"), and excludes the fragment delimiter
// and fragment. Percent-escape case, literal Unicode (UTF-8) boundary bytes,
// duplicate slashes, dot segments, parameters (";"), and query order are all
// preserved byte-for-byte; nothing is percent-decoded, re-encoded, Unicode-
// normalized, case-folded, or dot-segment-resolved. A URL rejected by ordinary
// facade validation never reaches this extractor; the NA sentinel is mapped to
// invalid_request_target (no decision) by the facade (BI5).
[[cpp11::register]]
cpp11::writable::strings robotstxtr_bing_extract_request_target_(
    std::string url) {
  cpp11::writable::strings out(static_cast<R_xlen_t>(1));

  // 1. Exclude the fragment delimiter and everything after it.
  const std::size_t hash = url.find('#');
  const std::string no_fragment =
      (hash == std::string::npos) ? url : url.substr(0, hash);

  // 2. Locate the scheme/authority boundary "://". A URL that passed ordinary
  //    HTTP(S) validation always contains it; its absence is a lexical failure.
  const std::size_t scheme = no_fragment.find("://");
  if (scheme == std::string::npos) {
    out[0] = cpp11::r_string(NA_STRING);
    return out;
  }
  const std::size_t authority_start = scheme + 3;

  // 3. The authority ends at the first '/' or '?' at or after its start (the
  //    fragment is already gone). Everything from that point on is the original
  //    path+query, copied verbatim -- no percent, dot-segment, slash, or
  //    Unicode rewriting.
  std::size_t path_query_start = no_fragment.size();
  for (std::size_t i = authority_start; i < no_fragment.size(); ++i) {
    const char c = no_fragment[i];
    if (c == '/' || c == '?') {
      path_query_start = i;
      break;
    }
  }
  const std::string path_query = no_fragment.substr(path_query_start);

  // 4. Substitute '/' when no explicit path exists, retaining any query
  //    (including a bare, empty '?').
  std::string target;
  if (path_query.empty()) {
    target = "/";
  } else if (path_query.front() == '?') {
    target = "/";
    target += path_query;
  } else {
    target = path_query;  // already begins with '/'
  }

  // 5. A produced target must be a non-empty, slash-prefixed string; otherwise
  //    signal lexical failure. (By construction the branches above guarantee
  //    this, but the guard makes the contract explicit.)
  if (target.empty() || target.front() != '/') {
    out[0] = cpp11::r_string(NA_STRING);
    return out;
  }

  // The output bytes are a contiguous slice of the input UTF-8 bytes (plus at
  // most a leading '/'), so they are valid UTF-8; mark them CE_UTF8 and carry
  // the exact byte length to preserve literal Unicode without re-encoding.
  out[0] = cpp11::r_string(Rf_mkCharLenCE(
      target.data(), static_cast<int>(target.size()), CE_UTF8));
  return out;
}

// Return the compile-time contract identity baked into the vendored library by
// the src/Makevars -D macros (spec section 5/14). This reads the static
// ContractInfo from the vendored version.cpp so a test can prove the frozen
// release values (not the development placeholders) were compiled in. The public
// identity is single-sourced in R (BI5); this accessor is the native ground truth
// that reconciliation checks against.
[[cpp11::register]]
cpp11::writable::strings robotstxtr_bing_contract_info_() {
  const robotstxtbing::ContractInfo& info = robotstxtbing::contract_info();
  auto sv = [](std::string_view v) {
    return cpp11::r_string(
        Rf_mkCharLenCE(v.data(), static_cast<int>(v.size()), CE_UTF8));
  };
  using namespace cpp11::literals;
  cpp11::writable::strings out({
      "library_version"_nm = sv(info.library_version),
      "contract_id"_nm = sv(info.contract_id),
      "contract_revision"_nm = sv(info.contract_revision),
      "parser_revision"_nm = sv(info.parser_revision),
      "bingbot_profile_revision"_nm = sv(info.bingbot_profile_revision),
      "adidxbot_profile_revision"_nm = sv(info.adidxbot_profile_revision),
      "release_manifest_sha256"_nm = sv(info.release_manifest_sha256),
  });
  return out;
}
