# Slice R8: allowed_by_robots_url() combines the grouped fetch stage
# (robots_fetch(), reused) with matching and the full per-row result contract
# (PRD all of 6). Every case is mocked/offline via
# httr2::local_mocked_responses(); no live network is used. Coverage: the
# tracer (disallow + 404 missing_allow), fetch_unknown for failures, grouped
# reuse of one source body across rows sharing an origin, exact-URL preservation
# into the matcher, invalid-UA detachment with and without a valid sibling,
# input_unknown for a bad URL, combined input-order preservation, match-metadata
# carried through a fetched body, and the zero-length empty object.

# --- Test helpers (local to this file) --------------------------------------

mock_router <- function(routes, recorder = NULL) {
  function(req) {
    if (!is.null(recorder)) {
      recorder$urls <- c(recorder$urls, req$url)
    }
    handler <- routes[[req$url]]
    if (is.null(handler)) {
      stop(sprintf("unexpected request URL in mock: %s", req$url))
    }
    handler(req)
  }
}

new_recorder <- function() {
  e <- new.env(parent = emptyenv())
  e$urls <- character(0)
  e
}

ok_body <- function(body) {
  force(body)
  function(req) {
    httr2::response(status_code = 200L, url = req$url, body = charToRaw(body))
  }
}

status_resp <- function(code) {
  force(code)
  function(req) {
    httr2::response(status_code = code, url = req$url, body = raw())
  }
}

transport_fail <- function(curl_class, msg) {
  force(curl_class)
  force(msg)
  function(req) {
    parent <- structure(
      class = c(curl_class, "curl_error", "error", "condition"),
      list(message = msg, call = NULL)
    )
    structure(
      class = c("httr2_failure", "httr2_error", "error", "condition"),
      list(message = "Failed.", parent = parent, call = NULL)
    )
  }
}

# --- Tracer bullet ----------------------------------------------------------

test_that("tracer: a disallowed path is FALSE / rule_disallow", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /private"))
  ))
  x <- allowed_by_robots_url("http://a/private", "bot")

  expect_s3_class(x, "robots_decisions")
  expect_identical(nrow(x$results), 1L)
  expect_false(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_disallow")
  expect_identical(x$results$fetch_outcome, "fetched")
  expect_identical(x$results$source_id, "robots_1")
  expect_identical(x$results$http_status, 200L)
  expect_identical(x$results$matched_rule_type, "disallow")
})

test_that("tracer: a 404 origin is TRUE / missing_allow", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = status_resp(404L))
  ))
  x <- allowed_by_robots_url("http://a/private", "bot")

  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "missing_allow")
  expect_identical(x$results$fetch_outcome, "missing")
  expect_identical(x$results$http_status, 404L)
  expect_identical(x$results$matched_rule_type, "unknown")
  expect_true(is.na(x$results$matched_line))
  expect_true(is.na(x$results$matched_rule_value))
  # The source is still referenced (the fetch happened, just returned missing).
  expect_identical(x$results$source_id, "robots_1")
})

test_that("410 is also missing_allow", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = status_resp(410L))
  ))
  x <- allowed_by_robots_url("http://a/x", "bot")
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "missing_allow")
})

# --- fetch_unknown for failures ---------------------------------------------

test_that("a 500 response is fetch_unknown / NA", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = status_resp(500L))
  ))
  x <- allowed_by_robots_url("http://a/x", "bot")

  expect_true(is.na(x$results$allowed))
  expect_identical(x$results$decision_source, "fetch_unknown")
  expect_identical(x$results$fetch_outcome, "http_error")
  expect_identical(x$results$error_class, "robots_http_error")
  expect_identical(x$results$matched_rule_type, "unknown")
})

test_that("a timeout is fetch_unknown / NA", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = transport_fail(
      "curl_error_operation_timedout", "Timeout was reached"
    ))
  ))
  x <- allowed_by_robots_url("http://a/x", "bot")

  expect_true(is.na(x$results$allowed))
  expect_identical(x$results$decision_source, "fetch_unknown")
  expect_identical(x$results$fetch_outcome, "timeout")
})

# --- grouped reuse of one source body across rows sharing an origin ---------

test_that("one fetch is reused across rows sharing an origin", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /private")),
    recorder = rec
  ))
  x <- allowed_by_robots_url(
    c("http://a/private", "http://a/public"), "bot"
  )

  # Exactly one request for the shared origin.
  expect_length(rec$urls, 1L)
  expect_identical(rec$urls, "http://a/robots.txt")
  expect_identical(nrow(x$robots), 1L)
  # Both rows reference the same source, with correct per-URL decisions.
  expect_identical(x$results$source_id, c("robots_1", "robots_1"))
  expect_identical(x$results$allowed, c(FALSE, TRUE))
  expect_identical(
    x$results$decision_source, c("rule_disallow", "default_allow")
  )
  expect_identical(x$results$input_id, 1:2)
})

# --- exact URL preservation into the matcher --------------------------------

test_that("full path/query/case/escape reach the matcher, grouped by origin", {
  rec <- new_recorder()
  body <- paste0(
    "user-agent: *\nallow: /\n",
    "disallow: /*secret\n",   # query substring
    "disallow: /Private\n",   # path case
    "disallow: /a%2Fb\n",     # percent-escape
    "disallow: /café"    # unicode (canonicalized to %C3%A9 by the matcher)
  )
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body(body)),
    recorder = rec
  ))
  urls <- c(
    "http://a/page?token=secret", # query preserved -> disallowed
    "http://a/page?token=ok",     # -> allowed
    "http://a/Private",           # exact case -> disallowed
    "http://a/private",           # different case -> allowed
    "http://a/a%2Fb",             # escape preserved -> disallowed
    "http://a/caf%C3%A9",         # escaped unicode -> disallowed
    "http://a/plain#secret"       # fragment kept in url; not in matched path
  )
  x <- allowed_by_robots_url(urls, "bot")

  # Grouping still keys on origin: one request despite 7 distinct URLs.
  expect_length(rec$urls, 1L)
  expect_identical(
    x$results$allowed,
    c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
  # The original URL strings are preserved verbatim, fragment included.
  expect_identical(x$results$url, urls)
})

# --- invalid-UA detachment --------------------------------------------------

test_that("an invalid UA is detached with no sibling and never fetched", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /")),
    recorder = rec
  ))
  x <- allowed_by_robots_url("http://a/x", "")

  expect_length(rec$urls, 0L) # no request at all
  expect_identical(nrow(x$robots), 0L)
  expect_true(is.na(x$results$allowed))
  expect_identical(x$results$decision_source, "input_unknown")
  expect_identical(x$results$fetch_outcome, "input_invalid")
  expect_true(is.na(x$results$source_id))
  expect_true(is.na(x$results$robots_url))
  expect_true(is.na(x$results$http_status))
  expect_identical(x$results$error_stage, "input")
  expect_identical(x$results$error_class, "robots_invalid_user_agent")
})

test_that("an invalid-UA row detaches while a valid sibling still fetches", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /private")),
    recorder = rec
  ))
  # Row 1: valid URL + valid UA; row 2: same origin, empty UA (detached).
  x <- allowed_by_robots_url(
    c("http://a/private", "http://a/private"),
    c("bot", "")
  )

  # The valid sibling still triggers exactly one fetch.
  expect_length(rec$urls, 1L)
  expect_identical(nrow(x$robots), 1L)
  # Row 1 gets a real decision from the fetched body.
  expect_false(x$results$allowed[[1]])
  expect_identical(x$results$decision_source[[1]], "rule_disallow")
  expect_identical(x$results$source_id[[1]], "robots_1")
  # Row 2 is fully detached despite the shared origin being fetched.
  expect_true(is.na(x$results$allowed[[2]]))
  expect_identical(x$results$decision_source[[2]], "input_unknown")
  expect_identical(x$results$fetch_outcome[[2]], "input_invalid")
  expect_true(is.na(x$results$source_id[[2]]))
  expect_true(is.na(x$results$http_status[[2]]))
  expect_identical(x$results$error_class[[2]], "robots_invalid_user_agent")
})

# --- input_unknown for a bad URL --------------------------------------------

test_that("a malformed / non-HTTP URL is input_unknown, never fetched", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /")),
    recorder = rec
  ))
  x <- allowed_by_robots_url(c("not-a-url", NA, "", "ftp://a/x"), "bot")

  expect_length(rec$urls, 0L)
  expect_identical(nrow(x$robots), 0L)
  expect_true(all(is.na(x$results$allowed)))
  expect_identical(x$results$decision_source, rep("input_unknown", 4L))
  expect_identical(x$results$fetch_outcome, rep("input_invalid", 4L))
  expect_identical(x$results$error_stage, rep("origin", 4L))
  expect_identical(x$results$error_class, rep("robots_invalid_url", 4L))
})

test_that("URL error takes precedence over UA error", {
  x <- allowed_by_robots_url("not-a-url", "")
  expect_identical(x$results$error_class, "robots_invalid_url")
  expect_identical(x$results$error_stage, "origin")
})

# --- combined fetch + match input-order preservation ------------------------

test_that("combined fetch+match preserves input order across origins", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list(
      "http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /private"),
      "http://b/robots.txt" = status_resp(404L)
    ),
    recorder = rec
  ))
  # Scalar UA expands; mixed origins + an invalid row interleaved.
  urls <- c(
    "http://a/private", # rule_disallow (robots_1)
    "bad",              # input_unknown
    "http://b/x",       # missing_allow (robots_2)
    "http://a/public"   # default_allow (robots_1, reused)
  )
  x <- allowed_by_robots_url(urls, "bot")

  expect_identical(x$results$input_id, 1:4)
  expect_identical(x$results$url, urls)
  expect_identical(
    x$results$decision_source,
    c("rule_disallow", "input_unknown", "missing_allow", "default_allow")
  )
  expect_identical(x$results$allowed, c(FALSE, NA, TRUE, TRUE))
  expect_identical(
    x$results$source_id, c("robots_1", NA, "robots_2", "robots_1")
  )
  # a fetched once, b fetched once (a reused by rows 1 and 4).
  expect_identical(rec$urls, c("http://a/robots.txt", "http://b/robots.txt"))
  expect_identical(nrow(x$robots), 2L)
})

# --- match metadata carried through a fetched body --------------------------

test_that("matched line/type/value are correlated from a fetched body", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body(
      "user-agent: *\nallow: /public\ndisallow: /private"
    ))
  ))
  x <- allowed_by_robots_url(
    c("http://a/private", "http://a/public", "http://a/other"), "bot"
  )

  # disallow won on line 3; allow won on line 2; default on the third.
  expect_identical(x$results$decision_source,
                   c("rule_disallow", "rule_allow", "default_allow"))
  expect_identical(x$results$matched_line, c(3L, 2L, NA_integer_))
  expect_identical(x$results$matched_rule_type,
                   c("disallow", "allow", "none"))
  expect_identical(x$results$matched_rule_value,
                   c("/private", "/public", NA_character_))
})

test_that("an empty fetched body is default_allow", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body(""))
  ))
  x <- allowed_by_robots_url("http://a/x", "bot")
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "default_allow")
  expect_identical(x$results$fetch_outcome, "fetched")
  expect_identical(x$results$matched_rule_type, "none")
})

# --- zero-length input ------------------------------------------------------

test_that("zero-length url yields an empty robots_decisions object", {
  x <- allowed_by_robots_url(character(0), "bot")
  expect_s3_class(x, "robots_decisions")
  expect_identical(nrow(x$results), 0L)
  expect_identical(nrow(x$robots), 0L)
  expect_type(x$results$input_id, "integer")
  expect_type(x$results$allowed, "logical")
  expect_named(x$results, c(
    "input_id", "url", "user_agent", "allowed", "decision_source",
    "source_id", "robots_url", "http_status", "fetch_outcome",
    "error_stage", "error_class", "error_message", "matched_line",
    "matched_rule_type", "matched_rule_value"
  ))
})

# --- schema + call-level validation -----------------------------------------

test_that("results carries the full contract schema and no body column", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /"))
  ))
  x <- allowed_by_robots_url("http://a/x", "bot")
  expect_named(x$results, c(
    "input_id", "url", "user_agent", "allowed", "decision_source",
    "source_id", "robots_url", "http_status", "fetch_outcome",
    "error_stage", "error_class", "error_message", "matched_line",
    "matched_rule_type", "matched_rule_value"
  ))
  expect_null(x$results$body)
  # The fetched body lives once in the robots table, not in results.
  expect_type(x$robots$body[[1]], "raw")
})

test_that("call-level argument errors abort the whole call", {
  expect_error(
    allowed_by_robots_url(123, "bot"),
    class = "robotstxtr_invalid_url_type"
  )
  expect_error(
    allowed_by_robots_url(c("http://a/x", "http://a/y"), c("a", "b", "c")),
    class = "robotstxtr_length_mismatch"
  )
  expect_error(
    allowed_by_robots_url("http://a/x", "bot", timeout = -1),
    class = "robotstxtr_invalid_timeout"
  )
  expect_error(
    allowed_by_robots_url("http://a/x", "bot", max_bytes = 0.5),
    class = "robotstxtr_invalid_max_bytes"
  )
  expect_error(
    allowed_by_robots_url("http://a/x", "bot", fetch_user_agent = ""),
    class = "robotstxtr_invalid_fetch_user_agent"
  )
})
