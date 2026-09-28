// cpp11 binding: byte-preserving HTTP(S) request-target extractor (YI4a).
//
// This file is NOT part of the vendored upstream snapshot; it is the R package's
// own glue and carries no Google/Apache header. It calls no engine code
// (neither googlebot nor robotstxtyandex): it is a pure lexical transform over
// the UTF-8 bytes cpp11 already produced from the original ordinary R character
// URL, per the integration spec section 9 ("Absolute URL to request-target
// contract").
//
// robotstxtyandex accepts an original slash-prefixed HTTP request target, not
// an absolute URL. This extractor converts one to the other LEXICALLY, with NO
// canonicalization of any kind: it removes only the scheme and authority,
// substitutes "/" when no explicit path exists, retains the query delimiter and
// exact query spelling (including an empty query "?"), and excludes the fragment
// delimiter and fragment. Percent-escape case, literal Unicode (UTF-8) boundary
// bytes, duplicate slashes, dot segments, parameters (";"), and query order are
// all preserved byte-for-byte. It MUST NOT percent-encode literal Unicode after
// the cpp11 translation, and it MUST NOT consult rurl's cleaned/parsed path or
// query, googlebot::GetPathParamsQuery(), or any other engine's URL behaviour.
//
// The routine stays HIDDEN: Yandex capability remains unavailable, nothing in
// the public facade calls it, and it is not user-exported. It is registered
// under a neutral, engine-agnostic name ("...extract_request_target...") on
// purpose -- it is a lexical URL transform, not a Yandex engine entry point, so
// the YI3b "no yandex native entry point registered" invariant (a grep over the
// registered routine names) continues to hold.
//
// On lexical failure -- an input from which no non-empty slash-prefixed target
// can be produced (e.g. malformed or empty input with no "://" boundary) -- the
// routine returns the NA_character_ sentinel. A later adapter (YI4c) maps that
// sentinel to invalid_request_target with no crawl decision. A URL rejected by
// ordinary facade validation never reaches this extractor.

#include <cstddef>
#include <string>

#include <cpp11.hpp>

// Convert one absolute HTTP(S) URL (UTF-8 bytes) into its slash-prefixed request
// target, or NA_character_ on lexical failure. Returns a length-1 character
// vector so the NA sentinel is representable.
[[cpp11::register]]
cpp11::writable::strings robotstxtr_extract_request_target_(std::string url) {
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
