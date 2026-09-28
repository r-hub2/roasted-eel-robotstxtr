# Deterministic fetch policy for robots_fetch() (PRD 6.4 request/response
# behavior, 6.5 grouping). This file holds the request builder, the HTTP status
# and transport-condition classifiers, the manual redirect loop, the streaming
# decoded-byte limit enforcement (slice R7), and the per-outcome error-metadata
# mapping. It performs NO matching (slice R8). The redirect loop drives each hop
# over a streaming connection so the final body can be read chunk-by-chunk and
# reading aborted the moment decoded bytes exceed `max_bytes` (PRD 6.4 streaming
# note); the compressed entity is never fully downloaded before the check.

# --- Call-level validators (whole call aborts on violation, PRD 6.6) --------

# `timeout` must be one non-missing, finite, positive numeric value.
validate_timeout <- function(timeout) {
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
        !is.finite(timeout) || timeout <= 0) {
    robots_abort(
      "`timeout` must be a single, positive, finite numeric value (seconds).",
      "robotstxtr_invalid_timeout"
    )
  }
  invisible(as.double(timeout))
}

# `fetch_user_agent` must be NULL or one non-empty, non-missing character value.
validate_fetch_user_agent <- function(fetch_user_agent) {
  if (is.null(fetch_user_agent)) {
    return(invisible(NULL))
  }
  if (!is.character(fetch_user_agent) || length(fetch_user_agent) != 1L ||
        is.na(fetch_user_agent) || !nzchar(fetch_user_agent)) {
    robots_abort(
      paste(
        "`fetch_user_agent` must be `NULL` or a single, non-empty",
        "character value."
      ),
      "robotstxtr_invalid_fetch_user_agent"
    )
  }
  invisible(fetch_user_agent)
}

# `max_bytes` must be one non-missing, finite, POSITIVE, WHOLE-NUMBER value no
# greater than `.Machine$integer.max` (PRD 6.6 shared fetch controls). An R
# double that represents a valid whole number (e.g. `524288` or `1e5`) is
# accepted; fractional, non-positive, non-scalar, non-numeric, NA/Inf, or
# out-of-range values are call-level errors (not a silent NA). Coerced to
# integer exactly once, after validation.
validate_max_bytes <- function(max_bytes) {
  if (!is.numeric(max_bytes) || length(max_bytes) != 1L || is.na(max_bytes) ||
        !is.finite(max_bytes) || max_bytes <= 0 ||
        max_bytes != trunc(max_bytes) ||
        max_bytes > .Machine$integer.max) {
    robots_abort(
      paste(
        "`max_bytes` must be a single, positive, finite, whole-number value",
        "no greater than `.Machine$integer.max`."
      ),
      "robotstxtr_invalid_max_bytes"
    )
  }
  as.integer(max_bytes)
}

# `ssrf_guard` must be one non-missing logical value. It defaults to TRUE at the
# public surface; passing FALSE is a deliberate caller opt-out of the structural
# SSRF guard (e.g. fetching robots.txt from a trusted intranet host).
validate_ssrf_guard <- function(ssrf_guard) {
  if (!is.logical(ssrf_guard) || length(ssrf_guard) != 1L ||
        is.na(ssrf_guard)) {
    robots_abort(
      "`ssrf_guard` must be a single, non-missing logical (`TRUE`/`FALSE`).",
      "robotstxtr_invalid_ssrf_guard"
    )
  }
  invisible(ssrf_guard)
}

# The package fetch user agent (PRD 6.4): package name and version. Never the
# matcher user_agent (robots_fetch has no matcher user agent anyway).
package_fetch_user_agent <- function() {
  sprintf("robotstxtr/%s", as.character(getNamespaceVersion("robotstxtr")))
}

# --- Classifiers ------------------------------------------------------------

# Map a final (non-3xx) HTTP status to a stable fetch_outcome (PRD 6.4 table).
classify_status <- function(status) {
  if (status >= 100L && status <= 199L) {
    return("http_error")
  }
  if (status == 206L) {
    return("partial_response")
  }
  if (status >= 200L && status <= 299L) {
    return("fetched")
  }
  if (status == 404L || status == 410L) {
    return("missing")
  }
  if (status >= 400L && status <= 599L) {
    return("http_error")
  }
  # Any other final status is not a body and not a redirect; treat defensively
  # as an HTTP error rather than fabricating a decision.
  "http_error"
}

# Classify a transport failure signalled by httr2/curl into timeout, tls_error,
# or network_error (PRD 6.4). httr2 wraps the curl error as the `parent` of an
# `httr2_failure`; curl (>= 5) classes conditions after the CURLcode name (e.g.
# `curl_error_operation_timedout`, `curl_error_peer_failed_verification`). We
# walk the whole condition/parent chain and match on those class names so the
# classification is robust to nesting and testable offline.
classify_transport_condition <- function(cnd) {
  classes <- character(0)
  cur <- cnd
  while (!is.null(cur) && inherits(cur, "condition")) {
    classes <- c(classes, class(cur))
    cur <- cur$parent
  }
  if (any(grepl("timedout|timeout", classes, ignore.case = TRUE))) {
    return("timeout")
  }
  if (any(grepl("ssl|tls|peer_failed_verification|cacert|certificate|cert_",
                classes, ignore.case = TRUE))) {
    return("tls_error")
  }
  "network_error"
}

# --- Per-outcome error metadata (PRD 6.6 error mapping table) ---------------

fetch_error_meta <- function(outcome) {
  switch(outcome,
    fetched = c(stage = NA_character_, class = NA_character_),
    missing = c(stage = NA_character_, class = NA_character_),
    partial_response = c(stage = "response", class = "robots_partial_response"),
    http_error = c(stage = "response", class = "robots_http_error"),
    body_too_large = c(stage = "response", class = "robots_body_too_large"),
    ssrf_blocked = c(stage = "request", class = "robots_ssrf_blocked"),
    redirect_error = c(stage = "redirect", class = "robots_redirect_error"),
    timeout = c(stage = "request", class = "robots_timeout"),
    network_error = c(stage = "request", class = "robots_network_error"),
    tls_error = c(stage = "request", class = "robots_tls_error"),
    c(stage = NA_character_, class = NA_character_)
  )
}

# Assemble one source-fetch result (the fields a `robots` row needs, minus the
# grouping-assigned source_id). `body` is a raw vector for `fetched`, else NULL.
make_source_result <- function(outcome, http_status, effective_url,
                               redirect_count, body, error_message,
                               redirect_hops = list(),
                               terminal_redirect_reason = "none",
                               location_header = NA_character_,
                               observed_bytes = NULL,
                               body_truncated = FALSE,
                               transport_error_kind = NA_character_,
                               safety_block_reason = NA_character_,
                               termination_reason = "none",
                               final_http_status = NULL) {
  meta <- fetch_error_meta(outcome)
  stored_bytes <- if (is.null(body)) 0L else length(body)
  if (is.null(observed_bytes)) {
    observed_bytes <- if (is.null(body)) NA_integer_ else stored_bytes
  }
  list(
    fetch_outcome = outcome,
    http_status = if (is.null(http_status)) {
      NA_integer_
    } else {
      as.integer(http_status)
    },
    final_http_status = if (is.null(final_http_status)) {
      if (is.null(http_status)) NA_integer_ else as.integer(http_status)
    } else {
      as.integer(final_http_status)
    },
    effective_url = if (is.null(effective_url)) {
      NA_character_
    } else {
      effective_url
    },
    redirect_count = as.integer(redirect_count),
    redirect_hops = redirect_hops,
    terminal_redirect_reason = terminal_redirect_reason,
    location_header = location_header,
    body = body,
    body_size = if (is.null(body)) NA_integer_ else length(body),
    observed_bytes = as.integer(observed_bytes),
    stored_bytes = as.integer(stored_bytes),
    body_truncated = body_truncated,
    transport_error_kind = transport_error_kind,
    safety_block_reason = safety_block_reason,
    termination_reason = termination_reason,
    error_stage = unname(meta[["stage"]]),
    error_class = unname(meta[["class"]]),
    error_message = if (is.null(error_message)) NA_character_ else error_message
  )
}

# --- Request builder --------------------------------------------------------

# Build one clean GET request for a robots URL. The grouping key never carries
# userinfo (robots_origin drops it), and we build a fresh request each hop, so
# no Authorization, cookies, or caller session state is ever forwarded (PRD
# 6.4). Automatic redirect following is disabled so the manual loop can enforce
# the redirect policy; HTTP status is never treated as an error here because
# the response classifier owns status handling.
build_fetch_request <- function(url, timeout, fetch_ua) {
  req <- httr2::request(url)
  req <- httr2::req_method(req, "GET")
  req <- httr2::req_user_agent(req, fetch_ua)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_options(req, followlocation = FALSE)
  httr2::req_error(req, is_error = function(resp) FALSE)
}

# --- Streaming decoded-byte limit enforcement (PRD 6.4 streaming note) -------

# Pure decoded-byte accumulator. `read_chunk` is a nullary closure returning the
# next raw chunk of the DECODED entity (or a zero-length raw vector at stream
# end). Chunks are accumulated until the stream ends or the running total
# EXCEEDS `max_bytes`. A total at or under the limit yields the assembled body
# (empty stream -> raw(0)); the moment the total crosses the limit it stops
# pulling chunks and returns no body, so a truncated body is never assembled or
# stored. Factored out (and unit-tested with synthetic chunks) to prove the
# count/abort/no-store contract independently of any HTTP transport.
accumulate_within_limit <- function(read_chunk, max_bytes) {
  acc <- list()
  total <- 0
  repeat {
    chunk <- read_chunk()
    if (length(chunk) == 0L) {
      break
    }
    total <- total + length(chunk)
    if (total > max_bytes) {
      return(list(exceeded = TRUE, body = NULL, observed_bytes = total))
    }
    acc[[length(acc) + 1L]] <- chunk
  }
  list(
    exceeded = FALSE,
    body = if (length(acc) > 0L) do.call(c, acc) else raw(0),
    observed_bytes = total
  )
}

# Close a streaming-connection response. A mocked/buffered response (its `body`
# is a raw vector, not a live `StreamingBody`) has no connection to close, so
# this is a no-op there. Guarded so teardown never errors on any exit path.
close_stream_response <- function(resp) {
  if (inherits(resp$body, "StreamingBody")) {
    try(close(resp), silent = TRUE)
  }
  invisible(NULL)
}

# Read the final response body enforcing the decoded-byte limit. Real fetches
# yield a live `StreamingBody`, streamed chunk-by-chunk so reading aborts as
# soon as the DECODED total crosses `max_bytes` (curl decompresses before its
# write callback, so streamed chunks are already decoded). A mocked/buffered
# response (test harness) has its decoded bytes already in memory; there is no
# live transfer to abort, so its materialized length is measured directly.
read_body_within_limit <- function(resp, max_bytes) {
  if (inherits(resp$body, "StreamingBody")) {
    return(accumulate_within_limit(
      function() httr2::resp_stream_raw(resp, kb = 32L), max_bytes
    ))
  }
  buffered <- if (httr2::resp_has_body(resp)) {
    httr2::resp_body_raw(resp)
  } else {
    raw(0)
  }
  if (length(buffered) > max_bytes) {
    list(exceeded = TRUE, body = NULL, observed_bytes = length(buffered))
  } else {
    list(
      exceeded = FALSE, body = buffered, observed_bytes = length(buffered)
    )
  }
}

# --- Manual redirect loop + fetch of one origin -----------------------------

# Fetch a single robots URL under the deterministic policy and return a source
# result (see make_source_result). Grouping is by the ORIGINAL requested URL
# (PRD 6.5): the caller keys on `robots_url`, not on the final destination.
# Each hop is driven over a streaming connection (`req_perform_connection`) so
# the final body can be read chunk-by-chunk under the decoded-byte limit; the
# connection is closed on every exit path. `max_bytes` is the validated integer
# limit enforced on decoded entity bytes.
perform_fetch <- function(robots_url, timeout, fetch_ua, max_bytes,
                          ssrf_guard = TRUE) {
  current_url <- robots_url
  redirect_count <- 0L
  visited <- character(0)
  redirect_hops <- list()

  repeat {
    # SSRF guard (ROBO-quovenef): structurally reject fetches aimed at private/
    # loopback/link-local/metadata targets BEFORE any request is issued. One
    # check at the top of the loop covers both the initial origin and every
    # redirect target, because `current_url` is reassigned to each redirect hop
    # and the loop re-enters here before the next request. Pure + no-network, so
    # a blocked URL never opens a socket. `ssrf_guard = FALSE` is the caller's
    # deliberate opt-out (validated at the public surface); it skips the check.
    if (isTRUE(ssrf_guard)) {
      ssrf <- robots_ssrf_check(current_url)
      if (!isTRUE(ssrf$allowed)) {
        return(make_source_result(
          "ssrf_blocked", NULL, NULL, redirect_count, NULL,
          sprintf("Blocked by SSRF guard (%s).", ssrf$reason),
          redirect_hops = redirect_hops,
          safety_block_reason = ssrf$reason
        ))
      }
    }

    req <- build_fetch_request(current_url, timeout, fetch_ua)
    resp <- tryCatch(httr2::req_perform_connection(req), error = function(e) e)

    # Transport failure (DNS/connection/TLS/timeout): classify and stop.
    if (inherits(resp, "condition")) {
      outcome <- classify_transport_condition(resp)
      return(make_source_result(
        outcome, NULL, NULL, redirect_count, NULL, conditionMessage(resp),
        redirect_hops = redirect_hops,
        transport_error_kind = outcome
      ))
    }

    status <- httr2::resp_status(resp)

    # Redirect handling (PRD 6.4). Any 3xx we cannot legally follow, a loop, or
    # exceeding five redirects is a `redirect_error`. Read the Location header
    # (no body needed) and close the connection before continuing or returning.
    if (status >= 300L && status <= 399L) {
      loc <- httr2::resp_header(resp, "Location")
      close_stream_response(resp)
      if (is.null(loc) || !nzchar(loc)) {
        redirect_hops[[length(redirect_hops) + 1L]] <- list(
          from_url = current_url,
          status = as.integer(status),
          location_target = NA_character_
        )
        return(make_source_result(
          "redirect_error", NULL, NULL, redirect_count, NULL,
          sprintf("HTTP %d redirect without a usable Location header.", status),
          redirect_hops = redirect_hops,
          terminal_redirect_reason = "no_location",
          final_http_status = status
        ))
      }
      target <- httr2::url_modify_relative(current_url, loc)
      redirect_hops[[length(redirect_hops) + 1L]] <- list(
        from_url = current_url,
        status = as.integer(status),
        location_target = target
      )
      if (redirect_count >= 5L) {
        return(make_source_result(
          "redirect_error", NULL, NULL, redirect_count, NULL,
          "Exceeded the maximum of five redirects.",
          redirect_hops = redirect_hops,
          terminal_redirect_reason = "over_budget",
          location_header = loc,
          final_http_status = status
        ))
      }
      target_scheme <- tolower(as.character(httr2::url_parse(target)$scheme))
      current_scheme <- tolower(
        as.character(httr2::url_parse(current_url)$scheme)
      )
      if (length(target_scheme) != 1L ||
            !target_scheme %in% c("http", "https")) {
        return(make_source_result(
          "redirect_error", NULL, NULL, redirect_count, NULL,
          sprintf("Redirect to a non-HTTP(S) target (%s).", loc),
          redirect_hops = redirect_hops,
          terminal_redirect_reason = "non_http_target",
          location_header = loc,
          safety_block_reason = "scheme",
          final_http_status = status
        ))
      }
      if (identical(current_scheme, "https") &&
            identical(target_scheme, "http")) {
        return(make_source_result(
          "redirect_error", NULL, NULL, redirect_count, NULL,
          "Rejected an HTTPS-to-HTTP downgrade redirect.",
          redirect_hops = redirect_hops,
          terminal_redirect_reason = "downgrade",
          location_header = loc,
          safety_block_reason = "https_downgrade",
          final_http_status = status
        ))
      }
      if (target %in% c(visited, current_url)) {
        return(make_source_result(
          "redirect_error", NULL, NULL, redirect_count, NULL,
          "Redirect loop detected.",
          redirect_hops = redirect_hops,
          terminal_redirect_reason = "loop",
          location_header = loc,
          final_http_status = status
        ))
      }
      visited <- c(visited, current_url)
      current_url <- target
      redirect_count <- redirect_count + 1L
      next
    }

    # Final (non-3xx) response: classify by status.
    outcome <- classify_status(status)

    # A non-`fetched` final outcome stores no body; close and return.
    if (!identical(outcome, "fetched")) {
      close_stream_response(resp)
      return(make_source_result(
        outcome, status, current_url, redirect_count, NULL, NA_character_,
        redirect_hops = redirect_hops
      ))
    }

    # `fetched`: read the decoded body under the byte limit, aborting the read
    # the moment decoded bytes cross `max_bytes` (PRD 6.4). An empty body is a
    # valid `fetched` result (stored as raw(0)). Crossing the limit is
    # `body_too_large`: no body is stored (never a truncated body) and never
    # matched, but the real final status and effective URL are recorded.
    #
    # A transport failure raised DURING the body stream (e.g. the server drops
    # the connection mid-body) escapes the accumulator here; catch it at this
    # call site so the pure accumulator stays transport-unaware. Classify it
    # exactly like a connection-open failure (timeout / tls_error /
    # network_error): the fetch did not complete, so store no body and, like the
    # connection-open transport rows above, no http_status or effective_url.
    read <- tryCatch(
      read_body_within_limit(resp, max_bytes), error = function(e) e
    )
    if (inherits(read, "condition")) {
      close_stream_response(resp)
      return(make_source_result(
        classify_transport_condition(read), NULL, NULL, redirect_count, NULL,
        conditionMessage(read),
        redirect_hops = redirect_hops,
        transport_error_kind = classify_transport_condition(read)
      ))
    }
    close_stream_response(resp)
    if (isTRUE(read$exceeded)) {
      return(make_source_result(
        "body_too_large", status, current_url, redirect_count, NULL,
        sprintf(
          "Decoded response body exceeded the %d-byte limit (max_bytes).",
          max_bytes
        ),
        redirect_hops = redirect_hops,
        observed_bytes = read$observed_bytes,
        body_truncated = TRUE,
        termination_reason = "ceiling"
      ))
    }
    return(make_source_result(
      "fetched", status, current_url, redirect_count, read$body, NA_character_,
      redirect_hops = redirect_hops,
      observed_bytes = read$observed_bytes
    ))
  }
}
