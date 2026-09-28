#' Fetch robots.txt and match URLs under the deterministic fetch policy
#'
#' Fetches the governing `/robots.txt` for one or more target URLs and matches
#' each URL against the fetched body using the vendored, upstream-test-validated
#' Google robots.txt matcher. This is the URL-first headline path: it combines
#' the grouped fetch stage (`robots_fetch()`) with matching and rich
#' per-row decisions, returning a `robots_decisions` object.
#'
#' Each URL's fetch-origin grouping key `scheme://host[:port]/robots.txt` is
#' constructed (see the fetch-origin rules in the package PRD). Within one call
#' each distinct grouping key is fetched exactly once and every fetch-eligible
#' row sharing that key reuses the same source body; source IDs are assigned
#' `robots_1`, `robots_2`, and so on in first-occurrence order. There is no
#' persistent, cross-call, or hidden HTTP cache.
#'
#' The original input URL string is passed to the matcher unchanged. R does not
#' clean, decode, canonicalize, or reserialize it: percent escapes, Unicode,
#' path case, query strings, and fragments all reach the matcher as supplied.
#' `rurl` is used only to construct the fetch-origin grouping key, never to
#' transform the matcher input. The upstream matcher requires an appropriately
#' encoded, `%`-escaped full URL (per RFC 3986); callers are responsible for
#' supplying it in that form.
#'
#' `url` may have length zero (an empty result is returned). `user_agent` must
#' be a character vector of length one or `length(url)`: a scalar user agent
#' expands across every URL, and no other recycling is allowed. A wrong R type,
#' a `user_agent` length mismatch, or an invalid `timeout`, `max_bytes`, or
#' `fetch_user_agent` are call-level errors.
#'
#' A missing (`NA`), empty (`""`), malformed, or non-HTTP(S) URL, or a missing
#' or empty user-agent element, does not abort the call: that row yields a
#' per-row `input_unknown` decision with `allowed = NA`. URL validity is checked
#' before user-agent validity, so each invalid row has one deterministic primary
#' error class. An invalid row is fully detached: it never causes an HTTP
#' request and its `source_id`, `robots_url`, and `http_status` are `NA` with
#' `fetch_outcome = "input_invalid"`. A row with a valid URL but invalid user
#' agent stays detached even when a valid sibling row causes the same origin to
#' be fetched.
#'
#' Per-row decisions follow the fetch outcome: a `200`-class body is matched
#' (`rule_disallow`/`rule_allow`/`default_allow`; an empty body is valid and
#' yields `default_allow`); a `404`/`410` yields `missing_allow`/`TRUE`; any
#' fetch failure yields `fetch_unknown`/`NA`. For matched rows, `matched_line`,
#' `matched_rule_type`, and `matched_rule_value` are correlated from the parse
#' callbacks of the same source body (the canonical value the matcher used,
#' after upstream `MaybeEscapePattern` canonicalization).
#'
#' @inheritParams allowed_by_robots_text
#' @inheritParams robots_fetch
#' @param url A character vector of target URLs. May have length zero (an empty
#'   result is returned). The original string is passed to the matcher
#'   unchanged. A wrong R type is a call-level error.
#' @param max_bytes A single positive, finite, whole-number decoded-body byte
#'   limit no greater than `.Machine$integer.max`, coerced to integer. A
#'   whole-number double (for example `524288`) is accepted; fractional,
#'   non-positive, out-of-range, or non-scalar values are call-level errors.
#'   Defaults to `524288L`.
#' @param fetch_user_agent `NULL` (use the package fetch user agent) or a single
#'   non-empty character HTTP user agent to send instead. This is never the
#'   matcher `user_agent`.
#'
#' @return An S3 object of class `robots_decisions`: a named list with two
#'   components, `results` (one row per input URL, in input order) and `robots`
#'   (one row per fetched source body). Each fetched body is stored once as a
#'   raw vector in `robots$body`. Large body values never appear in `results`.
#'
#' @examples
#' # allowed_by_robots_url() fetches /robots.txt over HTTP and matches in one
#' # step. The transport is mocked here so the example runs offline; a real call
#' # needs no such wrapper.
#' httr2::with_mocked_responses(
#'   function(req) {
#'     httr2::response(
#'       status_code = 200L, url = req$url,
#'       body = charToRaw("user-agent: *\nDisallow: /private\n")
#'     )
#'   },
#'   allowed_by_robots_url(
#'     c("https://example.com/page", "https://example.com/private"),
#'     "my-bot"
#'   )
#' )
#'
#' @seealso [allowed_by_robots_text()] to match a supplied body without HTTP,
#'   and [robots_fetch()] for the fetch stage on its own.
#' @export
allowed_by_robots_url <- function(url, user_agent, timeout = 10,
                                  max_bytes = 524288L,
                                  fetch_user_agent = NULL,
                                  ssrf_guard = TRUE) {
  # --- Call-level validation / shared fetch controls (PRD 6.6). Complete ALL
  # validation before building the fetch plan so a bad argument aborts the whole
  # call even when no row is fetch-eligible. --------------------------------
  validate_url_type(url)
  n <- length(url)
  user_agent <- expand_user_agent(user_agent, n)
  timeout <- validate_timeout(timeout)
  validate_fetch_user_agent(fetch_user_agent)
  max_bytes_int <- validate_max_bytes(max_bytes)
  validate_ssrf_guard(ssrf_guard)

  # --- Per-row eligibility (PRD 6.6). A URL is invalid when missing, empty,
  # malformed, or non-HTTP(S): robots_origin() returns NA for exactly those (it
  # encodes the scheme guard + rurl parse). URL validity is checked BEFORE
  # user-agent validity so each invalid row has one deterministic primary error
  # class. A row is fetch-eligible only when BOTH are valid; a valid-URL /
  # invalid-UA row is excluded and never triggers a request (detachment rule).
  origin <- vapply(url, robots_origin, character(1L), USE.NAMES = FALSE)
  url_valid <- !is.na(origin)
  ua_valid <- !is.na(user_agent) & user_agent != ""
  eligible <- url_valid & ua_valid
  elig_idx <- which(eligible)

  # --- Fetch stage: REUSE robots_fetch() on the fetch-eligible subset (PRD 6.5
  # one-fetch-per-origin grouping + full request/redirect/classification policy
  # + stored bodies). Its `robots` table schema is identical to ours, so we
  # adopt it directly. `map` row k corresponds to original index elig_idx[k].
  fetch <- robots_fetch(
    url[elig_idx],
    timeout = timeout, max_bytes = max_bytes_int,
    fetch_user_agent = fetch_user_agent, ssrf_guard = ssrf_guard
  )
  robots <- fetch$robots

  # --- Per-input fetch columns, in ORIGINAL input order (PRD 6.6). Defaults are
  # the input-invalid state; fetch-eligible rows are filled back from the map.
  source_id_col <- rep(NA_character_, n)
  robots_url_col <- rep(NA_character_, n)
  http_status_col <- rep(NA_integer_, n)
  fetch_outcome_col <- rep("input_invalid", n)
  error_stage_col <- rep(NA_character_, n)
  error_class_col <- rep(NA_character_, n)
  error_message_col <- rep(NA_character_, n)

  if (length(elig_idx) > 0L) {
    map <- fetch$map
    source_id_col[elig_idx] <- map$source_id
    robots_url_col[elig_idx] <- map$robots_url
    http_status_col[elig_idx] <- map$http_status
    fetch_outcome_col[elig_idx] <- map$fetch_outcome
    error_stage_col[elig_idx] <- map$error_stage
    error_class_col[elig_idx] <- map$error_class
    error_message_col[elig_idx] <- map$error_message
  }

  # --- Input-invalid error metadata (PRD 6.6). URL error takes precedence over
  # user-agent error. These rows keep the input-invalid fetch defaults above.
  invalid_url <- which(!url_valid)
  invalid_ua <- which(url_valid & !ua_valid)
  error_stage_col[invalid_url] <- "origin"
  error_class_col[invalid_url] <- "robots_invalid_url"
  error_message_col[invalid_url] <-
    "URL is missing, empty, malformed, or not HTTP(S)."
  error_stage_col[invalid_ua] <- "input"
  error_class_col[invalid_ua] <- "robots_invalid_user_agent"
  error_message_col[invalid_ua] <- "User agent is missing or empty."

  # --- Decode each fetched source body to the matcher string ONCE (PRD 6.2
  # bridge, mirrors allowed_by_robots_text): `decode_matcher_body()` drops NUL
  # bytes and marks the result UTF-8 (R/body-decode.R). Non-fetched sources
  # store no body and are not matched. Keyed by source_id so the per-(source,
  # UA) matcher grouping below reuses one decoded body per source.
  source_body_utf8 <- vector("list", nrow(robots))
  names(source_body_utf8) <- robots$source_id
  for (i in seq_len(nrow(robots))) {
    if (identical(robots$fetch_outcome[[i]], "fetched")) {
      source_body_utf8[[i]] <- decode_matcher_body(robots$body[[i]])
    }
  }

  # --- Match fetched rows (PRD 6.2). Group by (source body, user_agent) so each
  # distinct body+UA combo runs the vectorized matcher exactly once, over the
  # rows' ORIGINAL url strings (unchanged). Non-fetched rows never reach the
  # matcher and keep allowed = NA / matching_line = NA.
  allowed <- rep(NA, n)
  matching_line <- rep(NA_integer_, n)
  fetched_idx <- which(fetch_outcome_col == "fetched")
  if (length(fetched_idx) > 0L) {
    for (sid in unique(source_id_col[fetched_idx])) {
      body_utf8 <- source_body_utf8[[sid]]
      src_rows <- fetched_idx[source_id_col[fetched_idx] == sid]
      for (ua in unique(user_agent[src_rows])) {
        g <- src_rows[user_agent[src_rows] == ua]
        allowed[g] <- robotstxtr_allowed_text_(body_utf8, url[g], ua)
        matching_line[g] <-
          robotstxtr_matching_line_text_(body_utf8, url[g], ua)
      }
    }
  }

  # --- Per-row decision_source + allowed (PRD 6.6). ------------------------
  allowed[fetch_outcome_col == "missing"] <- TRUE

  decision_source <- rep(NA_character_, n)
  decision_source[fetch_outcome_col == "input_invalid"] <- "input_unknown"
  decision_source[fetch_outcome_col == "missing"] <- "missing_allow"
  fail_outcomes <- c(
    "http_error", "redirect_error", "timeout", "network_error",
    "tls_error", "partial_response", "body_too_large", "ssrf_blocked"
  )
  decision_source[fetch_outcome_col %in% fail_outcomes] <- "fetch_unknown"
  fm <- fetch_outcome_col == "fetched"
  decision_source[fm & !allowed] <- "rule_disallow"
  decision_source[fm & allowed & matching_line > 0L] <- "rule_allow"
  decision_source[fm & allowed & matching_line <= 0L] <- "default_allow"

  # `matching_line() == 0` (or NA for non-fetched rows) maps to matched_line =
  # NA; positive lines are one-based and returned unchanged.
  matched_line <- ifelse(
    !is.na(matching_line) & matching_line > 0L, matching_line, NA_integer_
  )

  # matched_rule_type starts decision-derived; a positive matching line replaces
  # it below with the CALLBACK-derived type (PRD 6.6, reused R3 join).
  type_by_decision <- c(
    rule_allow = "allow", rule_disallow = "disallow", default_allow = "none",
    missing_allow = "unknown", fetch_unknown = "unknown",
    input_unknown = "unknown"
  )
  matched_rule_type <- unname(type_by_decision[decision_source])
  matched_rule_value <- rep(NA_character_, n)

  # --- Match-metadata correlation (PRD 6.6, REUSE R3). For rows with a positive
  # matching line, run the parse collector ONCE per DISTINCT source body and
  # join by line to fill the callback-derived type and canonical value. A
  # positive matching line absent from its lookup raises a package error.
  pos <- !is.na(matching_line) & matching_line > 0L
  if (any(pos)) {
    src_needed <- unique(source_id_col[pos])
    lookups <- stats::setNames(
      lapply(src_needed, function(sid) {
        collect_directive_lookup(source_body_utf8[[sid]])
      }),
      src_needed
    )
    correlated <- correlate_match_metadata(
      matching_line, source_id_col, matched_rule_type, matched_rule_value,
      lookups
    )
    matched_rule_type <- correlated$matched_rule_type
    matched_rule_value <- correlated$matched_rule_value

    # --- Ignored empty-path directives (PRD 6.6, R9). A matched row whose sole
    # matched directive has an empty path (callback value "") was ignored by
    # the matcher per Google, so restate it as default_allow/none/NA/NA. Runs
    # AFTER correlation and never changes `allowed`. Shared with the text path.
    normalized <- normalize_ignored_empty_path(
      decision_source, matched_rule_type, matched_line, matched_rule_value
    )
    decision_source <- normalized$decision_source
    matched_rule_type <- normalized$matched_rule_type
    matched_line <- normalized$matched_line
    matched_rule_value <- normalized$matched_rule_value
  }

  results <- data.frame(
    input_id = seq_len(n),
    url = url,
    user_agent = user_agent,
    allowed = allowed,
    decision_source = decision_source,
    source_id = source_id_col,
    robots_url = robots_url_col,
    http_status = http_status_col,
    fetch_outcome = fetch_outcome_col,
    error_stage = error_stage_col,
    error_class = error_class_col,
    error_message = error_message_col,
    matched_line = matched_line,
    matched_rule_type = matched_rule_type,
    matched_rule_value = matched_rule_value,
    stringsAsFactors = FALSE
  )

  new_robots_decisions(results, robots)
}
