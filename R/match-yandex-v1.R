# Hidden engine-aware Yandex matcher adapter (ROBO-agrvzdnr, YI4c).
#
# `match_yandex_v1()` maps the checked robotstxtyandex native batch binding
# (YI4b, R/yandex-checked-batch.R) onto the host per-row result shape, applying
# the normative §10/§11 status/reason/error and exact-byte raw-value mappings of
# design/robotstxtyandex-integration-v1-spec.md.
#
# This adapter is HIDDEN. It is NOT registered in engine_matcher_registry_v1(),
# Yandex availability stays "capability_unavailable", the schema revision is
# unchanged, and nothing in the public dispatcher/facade calls it. Wiring it
# into the dispatcher, flipping availability, and activating schema revision
# 2026-07-18.2 are a LATER unit (YI5). This unit only defines and tests the
# adapter package-privately.
#
# BATCH SHAPE (deliberate, and a YI5 requirement). The existing dispatcher
# (match_backend_v1 -> entry$callable(body, url, product_token)) invokes each
# matcher callable ONCE PER ROW with scalar args. YI4b's hard mandate is
# parse-once per DISTINCT body: yandex_evaluate_checked_batch() parses each
# distinct body exactly once and evaluates all rows that select it in a single
# native call. A per-row callable would parse once per row and defeat that. So
# `match_yandex_v1()` is defined BATCH-shaped -- vector/list row inputs -> a
# per-row-mapped data.frame -- and dedupes bodies internally so parse-once holds
# YI5 MUST wire the dispatcher to invoke this adapter in BATCH (collecting every
# row whose matcher_backend is "yandex" under a use_rules policy action) rather
# than through the current per-row entry$callable path.

# Convert one row's absolute URL into the exact request-target BYTES the native
# checked binding expects. A missing/empty URL, or a URL from which the YI4a
# lexical extractor cannot derive a non-empty slash-prefixed target
# (NA_character_), becomes an empty target (raw(0)). An empty target is rejected
# by EvaluateChecked as invalid_request_target -- UNLESS the crawler is also
# unsupported, in which case EvaluateChecked returns unsupported_crawler first.
# Delegating that precedence to the engine (rather than pre-mapping the NA here)
# preserves the §10 unsupported-first rule without adapter re-prevalidation.
yandex_target_bytes_v1 <- function(url) {
  if (is.na(url) || !nzchar(url)) {
    return(raw(0))
  }
  target <- robotstxtr_extract_request_target_(url)
  if (is.na(target)) {
    return(raw(0))
  }
  charToRaw(target)
}

# Group per-row raw bodies into the distinct-body list + 1-based index the
# native batch binding needs. Keys are the full byte sequence rendered as hex,
# so embedded NUL and invalid UTF-8 are compared safely and deduped exactly.
yandex_body_groups_v1 <- function(bodies) {
  keys <- vapply(
    bodies,
    function(b) paste(as.character(b), collapse = ""),
    character(1L)
  )
  first <- !duplicated(keys)
  list(
    distinct_bodies = bodies[first],
    body_index = match(keys, keys[first])
  )
}

yandex_empty_result_v1 <- function() {
  out <- data.frame(
    native_evaluation_status = character(),
    matcher_status = character(),
    url_decision = character(),
    reason = character(),
    matched_line = integer(),
    matched_rule_type = character(),
    matched_rule_value = character(),
    matcher_input_bytes = integer(),
    matcher_body_truncated = logical(),
    error_stage = character(),
    error_class = character(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
  out$matched_rule_value_raw <- list()
  attr(out, "n_parse_calls") <- 0L
  out
}

#' Map checked Yandex native results onto the host per-row result shape
#'
#' Package-private, HIDDEN engine-aware adapter (YI4c). It runs the YI4a
#' request-target extractor per row, groups rows by distinct body, evaluates
#' them through the checked robotstxtyandex batch binding (parse-once per
#' distinct body), and maps native columns to the normative §10/§11 status,
#' reason, error, and exact-byte raw-value contract. It performs no
#' registration, no availability flip, and no schema-revision change; the
#' dispatcher is wired to
#' call it in batch by a later unit (YI5).
#'
#' @param bodies A list of raw vectors, one per row -- the use_rules robots body
#'   for that row (bytes verbatim). Deduped internally for parse-once batching.
#' @param urls A character vector of absolute HTTP(S) URLs, one per row (or a
#'   single value recycled).
#' @param product_tokens A character vector of robots product tokens, one per
#'   row (or a single value recycled). Passed through to the engine's
#'   EvaluateChecked; the supported-token boundary is the engine's behavior, not
#'   an adapter allowlist.
#'
#' @return A data.frame with one row per input carrying
#'   `native_evaluation_status`, `matcher_status`, `url_decision`, `reason`,
#'   `matched_line`, `matched_rule_type`, `matched_rule_value`, the
#'   `matched_rule_value_raw` list column (exact bytes / `raw(0)` / `NULL`),
#'   `matcher_input_bytes`, `matcher_body_truncated`, and `error_stage` /
#'   `error_class` / `error_message`. The distinct-body parse count is attached
#'   as the `n_parse_calls` attribute.
#' @keywords internal
#' @noRd
match_yandex_v1 <- function(bodies, urls, product_tokens) {
  if (!is.list(bodies)) {
    robots_abort(
      "`bodies` must be a list of raw vectors.",
      "robotstxtr_invalid_yandex_bodies"
    )
  }
  n <- length(bodies)
  if (n == 0L) {
    return(yandex_empty_result_v1())
  }
  urls <- rep(urls, length.out = n)
  product_tokens <- rep(product_tokens, length.out = n)

  targets_raw <- lapply(urls, yandex_target_bytes_v1)
  groups <- yandex_body_groups_v1(bodies)

  native <- yandex_evaluate_checked_batch(
    bodies = groups$distinct_bodies,
    body_index = groups$body_index,
    crawlers = product_tokens,
    targets = targets_raw
  )

  status <- native$native_evaluation_status
  evaluated <- status == "evaluated"
  unsupported <- status == "unsupported_crawler"
  invalid_target <- status == "invalid_request_target"

  matcher_status <- ifelse(evaluated, "evaluated", "not_evaluated")

  decided <- !is.na(native$url_decision)
  url_decision <- rep(NA_character_, n)
  url_decision[decided & native$url_decision] <- "allow"
  url_decision[decided & !native$url_decision] <- "disallow"

  # For evaluated rows the four native DecisionSource names are byte-identical
  # to the four required §10 reasons (default_allow / rule_allow /
  # rule_disallow / effective_empty_disallow); non-evaluated rows are mapped.
  reason <- rep(NA_character_, n)
  reason[evaluated] <- native$decision_source[evaluated]
  reason[unsupported] <- "unsupported_product_token"
  reason[invalid_target] <- "invalid_request_target"

  error_stage <- rep(NA_character_, n)
  error_class <- rep(NA_character_, n)
  error_message <- rep(NA_character_, n)
  error_stage[unsupported | invalid_target] <- "input"
  error_class[unsupported] <- "robots_unsupported_product_token"
  error_class[invalid_target] <- "robots_invalid_request_target"
  error_message[unsupported] <- sprintf(
    paste0(
      "Robots product token is not supported by the Yandex matcher backend. ",
      "Accepted selectors: %s. Resolve the token before evaluating with ",
      "robots_resolve_matcher_profile_v1()."
    ),
    accepted_selectors_phrase_v1("yandex")
  )
  error_message[invalid_target] <-
    "A request target could not be derived from the URL."

  # §10: matcher_body_truncated is FALSE for any non-evaluated row. Evaluated
  # rows carry the native per-body flag (FALSE in practice under §12 policy).
  matcher_body_truncated <- native$matcher_body_truncated
  matcher_body_truncated[!evaluated] <- FALSE

  out <- data.frame(
    native_evaluation_status = status,
    matcher_status = matcher_status,
    url_decision = url_decision,
    reason = reason,
    matched_line = native$matched_line,
    matched_rule_type = native$matched_rule_type,
    matched_rule_value = native$matched_rule_value,
    matcher_input_bytes = native$matcher_input_bytes,
    matcher_body_truncated = matcher_body_truncated,
    error_stage = error_stage,
    error_class = error_class,
    error_message = error_message,
    stringsAsFactors = FALSE
  )
  # Assign the native list THROUGH unchanged so the raw(0) (present-but-empty)
  # vs NULL (absent) distinction survives verbatim -- rebuilding it would drop
  # the NULL elements.
  out$matched_rule_value_raw <- native$matched_rule_value_raw
  attr(out, "n_parse_calls") <- native$n_parse_calls
  out
}
