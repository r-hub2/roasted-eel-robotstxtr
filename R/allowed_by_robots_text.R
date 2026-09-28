#' Match URLs against a supplied robots.txt body
#'
#' Matches one supplied robots.txt body against one or more URLs using the
#' vendored, upstream-test-validated Google robots.txt matcher. No HTTP request
#' is performed; the caller supplies the robots.txt text directly.
#'
#' The URL string is passed to the matcher unchanged. The upstream matcher
#' requires an appropriately encoded, `%`-escaped full URL (per RFC 3986);
#' callers are responsible for supplying it in that form. R does not clean,
#' decode, canonicalize, or reserialize the URL before matching.
#'
#' `url` may have length zero (an empty result is returned). `user_agent` must
#' be a character vector of length one or `length(url)`: a scalar user agent
#' expands across every URL, and no other recycling is allowed. A wrong R type
#' for any argument, a `user_agent` length mismatch, a non-scalar or missing
#' `robots_txt`, or an empty/missing `source_id` are call-level errors.
#'
#' A missing (`NA`) or empty (`""`) URL or user-agent element does not abort the
#' call: that row yields a per-row `input_unknown` decision with `allowed = NA`.
#' Such an invalid row is detached from the supplied body: its `source_id`,
#' `robots_url`, and `http_status` are `NA` and its `fetch_outcome` is
#' `"input_invalid"`. When both the URL and the user agent of a row are invalid,
#' the URL error is reported (URL validity is checked first).
#'
#' Match metadata is fully populated on the text path. `matched_line` carries
#' the upstream one-based matching line (or `NA` when no rule matched). For a
#' matched row, `matched_rule_type` and `matched_rule_value` are correlated from
#' the parse callbacks: the type (`"allow"`/`"disallow"`) and the canonical rule
#' value the matcher actually used, i.e. the value after upstream
#' `MaybeEscapePattern` canonicalization (non-ASCII bytes percent-escaped,
#' existing `%xx` escapes upper-cased). R does not reconstruct a pre-escape
#' form. A non-UTF-8 callback value is returned with `Encoding = "bytes"`.
#' Rows where no rule matched keep `matched_rule_value = NA` and a
#' decision-derived
#' `matched_rule_type` (`"none"` for `default_allow`, `"unknown"` for
#' `input_unknown`).
#'
#' @param robots_txt A single, non-missing character scalar holding the
#'   robots.txt body. An empty body is valid. It is converted to UTF-8 once and
#'   the exact bytes are stored in the returned `robots$body` and passed to the
#'   matcher; a body already carrying `Encoding = "bytes"` is used verbatim.
#' @param url A character vector of URL strings to match, passed to the matcher
#'   unchanged. May have length zero.
#' @param user_agent A matcher user-agent string, length one or `length(url)`,
#'   passed to the matcher unchanged (upstream extraction semantics apply, so
#'   e.g. `"Googlebot/2.1"` is not pre-trimmed by R).
#' @param source_id A single non-empty character scalar identifying the supplied
#'   body within the returned object. Defaults to `"supplied"`.
#'
#' @return An S3 object of class `robots_decisions`: a named list with two
#'   components, `results` (one row per input URL, in input order) and `robots`
#'   (one row for the supplied source body). The supplied body is stored once as
#'   a raw vector in `robots$body`.
#'
#' @examples
#' allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/x", "bot")
#'
#' @useDynLib robotstxtr, .registration = TRUE
#' @export
allowed_by_robots_text <- function(robots_txt, url, user_agent,
                                   source_id = "supplied") {
  # --- Call-level input validation (§6.6). Wrong type / shape aborts. --------
  validate_robots_txt(robots_txt)
  validate_source_id(source_id)
  validate_url_type(url)
  n <- length(url)
  user_agent <- expand_user_agent(user_agent, n)

  # --- Body: convert to UTF-8 once, store those exact bytes, and pass the same
  # byte sequence to C++ (§6.6 body handling). A body already flagged as
  # `Encoding = "bytes"` is used verbatim (it cannot be UTF-8-translated). The
  # matcher input is rebuilt from the raw bytes and marked UTF-8 so cpp11
  # forwards the exact bytes without a translation error or re-encoding.
  if (identical(Encoding(robots_txt), "bytes")) {
    raw_body <- charToRaw(robots_txt)
  } else {
    raw_body <- charToRaw(enc2utf8(robots_txt))
  }
  body_utf8 <- decode_matcher_body(raw_body)

  # --- Per-element validity (§6.6, text path): a URL or user agent is invalid
  # only when missing (`NA`) or empty (`""`). Other URL strings pass through to
  # the matcher unchanged. URL validity is checked before user-agent validity
  # so each invalid row has one deterministic primary error class.
  url_valid <- !is.na(url) & url != ""
  ua_valid <- !is.na(user_agent) & user_agent != ""
  valid <- url_valid & ua_valid

  # --- Matcher runs only on fully valid rows (detachment rule, §6.6). Group by
  # user agent so the vectorized binding runs once per distinct agent; invalid
  # rows never reach the matcher and keep `allowed = NA`.
  allowed <- rep(NA, n)
  matching_line <- rep(NA_integer_, n)
  if (any(valid)) {
    idx_valid <- which(valid)
    groups <- split(idx_valid, user_agent[idx_valid])
    for (g in groups) {
      ua <- user_agent[[g[[1L]]]]
      allowed[g] <- robotstxtr_allowed_text_(body_utf8, url[g], ua)
      matching_line[g] <- robotstxtr_matching_line_text_(body_utf8, url[g], ua)
    }
  }

  # --- decision_source (§6.6). Invalid rows are input_unknown; valid rows split
  # by matcher result: disallow won, allow won (a rule matched), or default.
  decision_source <- rep("default_allow", n)
  decision_source[!valid] <- "input_unknown"
  decision_source[valid & !allowed] <- "rule_disallow"
  decision_source[valid & allowed & matching_line > 0L] <- "rule_allow"

  # `matching_line() == 0` (or NA for detached rows) maps to matched_line = NA;
  # positive lines are already one-based and returned unchanged.
  matched_line <- ifelse(
    !is.na(matching_line) & matching_line > 0L, matching_line, NA_integer_
  )

  # matched_rule_type starts decision-derived; for rows with a positive matching
  # line it is replaced below by the CALLBACK-derived type (§6.6, R3). Rows that
  # never match (default_allow) or never ran (input_unknown) keep this value.
  type_by_decision <- c(
    rule_allow = "allow", rule_disallow = "disallow",
    default_allow = "none", input_unknown = "unknown"
  )
  matched_rule_type <- unname(type_by_decision[decision_source])
  matched_rule_value <- rep(NA_character_, n)

  # --- Detachment: invalid rows do not reference the supplied body (§6.6). Set
  # here (before correlation) so the join keys matched rows to their source.
  source_id_col <- ifelse(valid, source_id, NA_character_)

  # --- Match-metadata correlation (§6.6, R3). Run the parse collector once per
  # DISTINCT source body (here a single supplied body) and join each positive
  # matching line to its per-source lookup, filling the canonical callback value
  # and the callback-derived type. Skipped entirely when nothing matched (e.g.
  # zero-length input or all default_allow), so the collector never runs need-
  # lessly. A positive matching line absent from the lookup is a package error.
  if (any(!is.na(matching_line) & matching_line > 0L)) {
    lookups <- stats::setNames(
      list(collect_directive_lookup(body_utf8)), source_id
    )
    correlated <- correlate_match_metadata(
      matching_line, source_id_col, matched_rule_type, matched_rule_value,
      lookups
    )
    matched_rule_type <- correlated$matched_rule_type
    matched_rule_value <- correlated$matched_rule_value

    # --- Ignored empty-path directives (§6.6, R9). A matched row whose sole
    # matched directive has an empty path (callback value "") was ignored by
    # the matcher per Google, so restate it as default_allow/none/NA/NA. This
    # runs AFTER correlation (which fills matched_rule_value) and never changes
    # `allowed`. Shared with the URL path so the two cannot drift.
    normalized <- normalize_ignored_empty_path(
      decision_source, matched_rule_type, matched_line, matched_rule_value
    )
    decision_source <- normalized$decision_source
    matched_rule_type <- normalized$matched_rule_type
    matched_line <- normalized$matched_line
    matched_rule_value <- normalized$matched_rule_value
  }

  fetch_outcome <- ifelse(valid, "supplied", "input_invalid")

  # Per-row error metadata: URL invalidity takes precedence over user-agent
  # invalidity (§6.6 error mapping).
  error_stage <- rep(NA_character_, n)
  error_stage[!ua_valid] <- "input"
  error_stage[!url_valid] <- "origin"
  error_class <- rep(NA_character_, n)
  error_class[!ua_valid] <- "robots_invalid_user_agent"
  error_class[!url_valid] <- "robots_invalid_url"
  error_message <- rep(NA_character_, n)
  error_message[!ua_valid] <- "User agent is missing or empty."
  error_message[!url_valid] <- "URL is missing or empty."

  results <- data.frame(
    input_id = seq_len(n),
    url = url,
    user_agent = user_agent,
    allowed = allowed,
    decision_source = decision_source,
    source_id = source_id_col,
    # Fetch/network columns owned by later slices (R5-R8): text path has none.
    # Constant columns use rep() so a zero-length input yields zero rows.
    robots_url = rep(NA_character_, n),
    http_status = rep(NA_integer_, n),
    fetch_outcome = fetch_outcome,
    error_stage = error_stage,
    error_class = error_class,
    error_message = error_message,
    matched_line = matched_line,
    matched_rule_type = matched_rule_type,
    matched_rule_value = matched_rule_value,
    stringsAsFactors = FALSE
  )

  robots <- data.frame(
    source_id = source_id,
    source_type = "supplied",
    robots_url = NA_character_,
    effective_url = NA_character_,
    http_status = NA_integer_,
    fetch_outcome = "supplied",
    redirect_count = 0L,
    body_size = length(raw_body),
    timeout = NA_real_,
    max_bytes = NA_integer_,
    error_stage = NA_character_,
    error_class = NA_character_,
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
  # `body` is a list-of-raw column: the supplied body stored exactly once.
  robots$body <- list(raw_body)

  new_robots_decisions(results, robots)
}
