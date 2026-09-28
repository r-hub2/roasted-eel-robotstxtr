# Engine-aware Bing matcher adapter (ROBO-qetsyvwv, BI5).
#
# `match_bing_v1()` maps the native robotstxtbing batch binding (BI4,
# R/bing-native.R) onto the host per-row result shape, applying the normative
# SS10/SS11 status/reason/error and exact-byte raw-value mappings of
# design/robotstxtbing-integration-v2-spec.md. It is the callable the atomic v2
# activation registers for the `bing` backend.
#
# BATCH SHAPE (parse-once per distinct body, BI-V2-DISPATCH). Like the Yandex
# adapter, this is batch-shaped: vector/list row inputs -> a per-row-mapped
# data.frame. `bing_evaluate_batch()` parses each DISTINCT body exactly once and
# evaluates every row selecting it in a single native call, so the adapter
# dedupes bodies internally to preserve parse-once. `evaluate_rows_v1()` gathers
# every `bing` row and invokes this adapter ONCE in batch form; the per-row
# `match_backend_v1()` guard rejects row-dispatching `bing` as an internal
# invariant violation.
#
# ENGINE-PURE. This adapter shares no parser, group selector, access matcher, or
# URL normalizer with Google or Yandex (spec SS4). It reuses only package-owned
# batching/transport infrastructure and, for clarity and independence, carries
# its own bing-named helpers rather than sharing the Yandex ones.

# Convert one row's absolute URL into the exact request-target BYTES the native
# binding evaluates. A missing/empty URL, or a URL from which the Bing lexical
# extractor cannot derive a target (NA_character_), becomes an empty target
# (raw(0)). An empty target is rejected by Policy::Evaluate as
# invalid_request_target -- UNLESS the product token is ALSO unsupported, in
# which case Evaluate returns unsupported_profile first (the fixed core
# precedence, spec SS6/SS16.3). Delegating that precedence to the engine rather
# than pre-mapping the NA here preserves the unsupported-first rule without
# adapter re-prevalidation (mirrors the approved Yandex extractor wiring).
bing_target_bytes_v1 <- function(url) {
  if (is.na(url) || !nzchar(url)) {
    return(raw(0))
  }
  target <- bing_extract_request_target(url)
  if (is.na(target)) {
    return(raw(0))
  }
  charToRaw(target)
}

# Group per-row raw bodies into the distinct-body list + 1-based index the
# native batch binding needs. Keys are the full byte sequence rendered as hex,
# so embedded NUL and invalid UTF-8 compare safely and dedupe exactly. (Package
# batching infrastructure; intentionally independent of the Yandex helper.)
bing_body_groups_v1 <- function(bodies) {
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

bing_empty_result_v1 <- function() {
  out <- data.frame(
    native_parse_status = character(),
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

#' Map native Bing results onto the host per-row result shape
#'
#' Package-private engine-aware adapter (BI5). It runs the Bing request-target
#' extractor per row, groups rows by distinct body, evaluates them through the
#' native robotstxtbing batch binding (parse-once per distinct body), and maps
#' native columns to the normative SS10/SS11 status, reason, error, and
#' exact-byte raw-value contract. It performs no fetch and shares nothing with
#' the Google or Yandex paths.
#'
#' @param bodies A list of raw vectors, one per row -- the use_rules robots body
#'   for that row (bytes verbatim). Deduped internally for parse-once batching.
#' @param urls A character vector of absolute HTTP(S) URLs, one per row (or a
#'   single value recycled).
#' @param product_tokens A character vector of robots product tokens, one per
#'   row (or a single value recycled). Passed through to `Policy::Evaluate`; the
#'   bounded-profile boundary (bingbot/adidxbot) is engine behavior, not an
#'   adapter allowlist -- every other non-empty token returns the native
#'   `unsupported_profile` outcome.
#'
#' @return A data.frame with one row per input carrying `native_parse_status`,
#'   `native_evaluation_status`, `matcher_status`, `url_decision`, `reason`,
#'   `matched_line`, `matched_rule_type`, `matched_rule_value`, the
#'   `matched_rule_value_raw` list column (exact bytes / `raw(0)` / `NULL`),
#'   `matcher_input_bytes`, `matcher_body_truncated`, and
#'   `error_stage` / `error_class` / `error_message`. The distinct-body parse
#'   count is attached as the `n_parse_calls` attribute.
#' @keywords internal
#' @noRd
match_bing_v1 <- function(bodies, urls, product_tokens) {
  if (!is.list(bodies)) {
    robots_abort(
      "`bodies` must be a list of raw vectors.",
      "robotstxtr_invalid_bing_bodies"
    )
  }
  n <- length(bodies)
  if (n == 0L) {
    return(bing_empty_result_v1())
  }
  urls <- rep(urls, length.out = n)
  product_tokens <- rep(product_tokens, length.out = n)

  targets_raw <- lapply(urls, bing_target_bytes_v1)
  groups <- bing_body_groups_v1(bodies)

  native <- bing_evaluate_batch(
    bodies = groups$distinct_bodies,
    body_index = groups$body_index,
    product_tokens = product_tokens,
    targets = targets_raw
  )

  parse_status <- native$native_parse_status
  eval_status <- native$native_evaluation_status

  # A parse ceiling yields no Policy: native_evaluation_status is NA and the
  # parse limit lives in native_parse_status. Otherwise the row carries an exact
  # EvaluationStatus.
  parse_failed <- is.na(eval_status)
  evaluated <- !parse_failed & eval_status == "evaluated"
  unsupported <- !parse_failed & eval_status == "unsupported_profile"
  invalid_target <- !parse_failed & eval_status == "invalid_request_target"
  target_limit <-
    !parse_failed & eval_status == "request_target_limit_exceeded"
  work_limit <- !parse_failed & eval_status == "work_limit_exceeded"

  # SS10.2 folding: the four parse limits + request-target-limit collapse into
  # matcher_input_limit_exceeded (distinguished by `reason`); work_limit is its
  # own status; unsupported_profile and invalid_request_target stand alone.
  matcher_status <- rep(NA_character_, n)
  matcher_status[evaluated] <- "evaluated"
  matcher_status[parse_failed | target_limit] <- "matcher_input_limit_exceeded"
  matcher_status[unsupported] <- "unsupported_profile"
  matcher_status[invalid_target] <- "invalid_request_target"
  matcher_status[work_limit] <- "matcher_work_limit_exceeded"

  decided <- !is.na(native$url_decision)
  url_decision <- rep(NA_character_, n)
  url_decision[decided & native$url_decision] <- "allow"
  url_decision[decided & !native$url_decision] <- "disallow"

  # SS10.1 reasons are DERIVED from the native decision source, rule type, and
  # access. The shipped Bing profile never emits the effective-empty native
  # shape (an empty Disallow is inert -> default_allow), but the branch is kept
  # for faithfulness: source rule + access allow + type disallow.
  src <- native$decision_source
  rule_type <- native$matched_rule_type
  reason <- rep(NA_character_, n)
  reason[evaluated & src == "default_allow"] <- "default_allow"
  reason[evaluated & src == "rule" & rule_type == "allow"] <- "rule_allow"
  reason[evaluated & src == "rule" & rule_type == "disallow" &
           url_decision == "disallow"] <- "rule_disallow"
  reason[evaluated & src == "rule" & rule_type == "disallow" &
           url_decision == "allow"] <- "effective_empty_disallow"
  # Non-evaluated reasons: the four parse limits pass their exact native name
  # through; the EvaluationStatus outcomes carry their own stable reason.
  reason[parse_failed] <- parse_status[parse_failed]
  reason[unsupported] <- "unsupported_profile"
  reason[invalid_target] <- "invalid_request_target"
  reason[target_limit] <- "request_target_limit_exceeded"
  reason[work_limit] <- "work_limit_exceeded"

  # SS10.2 errors: every non-evaluated outcome is an `input`-stage schema error
  # with a stable class; the message is human-readable and non-normative.
  input_limit <- parse_failed | target_limit
  error_stage <- rep(NA_character_, n)
  error_class <- rep(NA_character_, n)
  error_message <- rep(NA_character_, n)
  error_stage[!evaluated] <- "input"
  error_class[unsupported] <- "robots_unsupported_profile"
  error_class[invalid_target] <- "robots_invalid_request_target"
  error_class[input_limit] <- "robots_matcher_input_limit_exceeded"
  error_class[work_limit] <- "robots_matcher_work_limit_exceeded"
  error_message[unsupported] <- sprintf(
    paste0(
      "Robots product token is not a supported Bing profile. Accepted ",
      "selectors: %s. Resolve the token before evaluating with ",
      "robots_resolve_matcher_profile_v1()."
    ),
    accepted_selectors_phrase_v1("bing")
  )
  error_message[invalid_target] <-
    "A request target could not be derived from the URL."
  error_message[input_limit] <-
    "The robots body or request target exceeds a Bing matcher input limit."
  error_message[work_limit] <-
    "Evaluating the request exceeds the Bing matcher work budget."

  out <- data.frame(
    native_parse_status = parse_status,
    native_evaluation_status = eval_status,
    matcher_status = matcher_status,
    url_decision = url_decision,
    reason = reason,
    matched_line = native$matched_line,
    matched_rule_type = native$matched_rule_type,
    matched_rule_value = native$matched_rule_value,
    matcher_input_bytes = native$matcher_input_bytes,
    matcher_body_truncated = native$matcher_body_truncated,
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
