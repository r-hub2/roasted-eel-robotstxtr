#' Construct the robots.txt fetch-origin grouping key for a URL
#'
#' Pure, offline helper (no HTTP) that turns a target URL into the per-call
#' robots.txt grouping key `scheme://host[:port]/robots.txt` (§6.3). It is used
#' by the URL-first and fetch functions to decide which `/robots.txt` document
#' governs a URL and to group rows that share an origin.
#'
#' An explicit-scheme guard runs first, on the untouched input string, before
#' any `rurl` parse: the input must begin (ASCII case-insensitively) with
#' `http://` or `https://`. Scheme-relative (`//host/...`) and scheme-less
#' (`host/...`) inputs are ineligible, because `rurl` would otherwise silently
#' default them to HTTP. The guard validates eligibility only; it never mutates
#' the string later passed to the matcher (§6.2).
#'
#' For an eligible URL, `rurl::safe_parse_url()` validates and decomposes it and
#' the origin is serialized with a lowercased scheme and host, IDN hostnames
#' converted to ASCII/punycode, non-default ports preserved and default ports
#' (http 80, https 443) omitted, IPv6 literals bracketed, userinfo dropped, and
#' path/params/query/fragment ignored in favour of exactly `/robots.txt`.
#'
#' Ineligible input (failing the scheme guard, or rejected by `rurl` as
#' unparseable or host-less) returns `NA_character_`. This is surfaced per row
#' by the caller as an `input_unknown`/`input_invalid` decision (§6.6); it never
#' a whole-call error, and no error metadata is built here.
#'
#' @param url A single URL string.
#'
#' @return The serialized grouping key `scheme://host[:port]/robots.txt`, or
#'   `NA_character_` for ineligible input.
#' @keywords internal
#' @noRd
robots_origin <- function(url) {
  # --- Explicit-scheme guard (§6.3): run FIRST, on the untouched input string,
  # BEFORE any rurl parse. rurl would otherwise assign HTTP to a scheme-less
  # input, so scheme-relative and scheme-less inputs must be rejected here.
  # Validate eligibility only; do not mutate the string.
  if (length(url) != 1L || !is.character(url) || is.na(url) ||
        !grepl("^https?://", url, ignore.case = TRUE)) {
    return(NA_character_)
  }

  # --- Eligible: decompose with rurl. `host_encoding = "idna"` yields the
  # ASCII/punycode host (subdomains preserved, IDN labels punycoded); the
  # default `case_handling = "lower_host"` lowercases the host and IPv6 literals
  # keep their brackets. userinfo is not part of the host field.
  parsed <- rurl::safe_parse_url(url, host_encoding = "idna")
  scheme <- parsed$scheme
  host <- parsed$host
  port <- parsed$port

  # --- rurl rejects unparseable / host-less input by returning an empty scheme
  # and a NULL/NA/empty host (§6.6: per-row invalid, not a call error).
  if (length(scheme) != 1L || is.na(scheme) ||
        !scheme %in% c("http", "https") ||
        is.null(host) || length(host) != 1L || is.na(host) || !nzchar(host)) {
    return(NA_character_)
  }

  scheme <- tolower(scheme)
  host <- tolower(host)

  # --- Preserve non-default ports; omit scheme defaults (http 80, https 443).
  port_part <- ""
  if (length(port) == 1L && !is.na(port)) {
    is_default <- (scheme == "http" && port == 80L) ||
      (scheme == "https" && port == 443L)
    if (!is_default) {
      port_part <- paste0(":", port)
    }
  }

  # --- Emit exactly `/robots.txt`; path, params, query, and fragment ignored.
  paste0(scheme, "://", host, port_part, "/robots.txt")
}
