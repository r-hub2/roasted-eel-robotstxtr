# Slice R6: robots_fetch() performs the grouped, deterministic robots.txt fetch
# stage (PRD 6.3-6.6) with NO matching. Every case is mocked/offline via
# httr2::local_mocked_responses(); no live network is used. Coverage: every
# reachable fetch_outcome, one-fetch-per-origin grouping, input order, invalid
# input detachment, redirect policy (upgrade allowed / downgrade rejected,
# loops, cap, invalid scheme), transport failures, zero-length input, and the
# package vs caller fetch user agent with no userinfo/cookie forwarding.

# --- Test helpers -----------------------------------------------------------

# Record every requested URL and (optionally) the sent user agent, and route to
# a per-URL response spec. `routes` maps a request URL to a function(req) that
# returns an httr2 response or a signalled condition.
mock_router <- function(routes, recorder = NULL) {
  function(req) {
    if (!is.null(recorder)) {
      ua <- req$options$useragent
      if (is.null(ua)) {
        ua <- NA_character_
      }
      recorder$urls <- c(recorder$urls, req$url)
      recorder$agents <- c(recorder$agents, ua)
      recorder$reqs <- c(recorder$reqs, list(req))
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
  e$agents <- character(0)
  e$reqs <- list()
  e
}

ok_body <- function(body = "user-agent: *\ndisallow: /") {
  function(req) {
    httr2::response(
      status_code = 200L, url = req$url, body = charToRaw(body)
    )
  }
}

status_resp <- function(code, headers = list(), body = NULL) {
  force(code)
  force(headers)
  force(body)
  function(req) {
    httr2::response(
      status_code = code, url = req$url, headers = headers,
      body = if (is.null(body)) raw() else charToRaw(body)
    )
  }
}

# A signalled transport failure shaped like httr2's httr2_failure wrapping a
# curl_error_* condition (curl >= 5 classes conditions after the CURLcode).
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
      list(
        message = "Failed to perform HTTP request.", parent = parent,
        call = NULL
      )
    )
  }
}

# --- Tracer bullet: one fetch per shared origin -----------------------------

test_that("shared origin is fetched exactly once (tracer bullet)", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body("user-agent: *\ndisallow: /")),
    recorder = rec
  ))

  x <- robots_fetch(c("http://a/x", "http://a/y"))

  expect_s3_class(x, "robots_fetches")
  expect_length(rec$urls, 1L)
  expect_identical(rec$urls, "http://a/robots.txt")
  expect_identical(nrow(x$robots), 1L)
  expect_identical(nrow(x$map), 2L)
  expect_identical(x$map$source_id, c("robots_1", "robots_1"))
  expect_identical(x$map$robots_url,
                   c("http://a/robots.txt", "http://a/robots.txt"))
  expect_identical(x$map$fetch_outcome, c("fetched", "fetched"))
  expect_identical(x$map$input_id, 1:2)
})

# --- Object schema -----------------------------------------------------------

test_that("map and robots carry the full contract schema and types", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body())
  ))
  x <- robots_fetch("http://a/x")

  expect_named(x$map, c(
    "input_id", "url", "source_id", "robots_url", "http_status",
    "fetch_outcome", "error_stage", "error_class", "error_message"
  ))
  expect_named(x$robots, c(
    "source_id", "source_type", "robots_url", "effective_url", "http_status",
    "fetch_outcome", "redirect_count", "body_size", "timeout", "max_bytes",
    "error_stage", "error_class", "error_message", "body"
  ))
  expect_type(x$map$input_id, "integer")
  expect_type(x$map$http_status, "integer")
  expect_type(x$robots$redirect_count, "integer")
  expect_type(x$robots$body_size, "integer")
  expect_type(x$robots$http_status, "integer")
  expect_type(x$robots$timeout, "double")
  expect_type(x$robots$max_bytes, "integer")
  expect_type(x$robots$body, "list")
  expect_type(x$robots$body[[1]], "raw")
  expect_identical(x$robots$source_type, "fetched")
  expect_identical(x$robots$max_bytes, 524288L)
})

# --- fetched, including an empty body ---------------------------------------

test_that("fetched stores the exact decoded bytes and body_size", {
  body <- "user-agent: *\ndisallow: /private"
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body(body))
  ))
  x <- robots_fetch("http://a/private")

  expect_identical(x$robots$fetch_outcome, "fetched")
  expect_identical(x$robots$body[[1]], charToRaw(body))
  expect_identical(x$robots$body_size, length(charToRaw(body)))
  expect_identical(x$robots$http_status, 200L)
  expect_identical(x$robots$effective_url, "http://a/robots.txt")
  expect_true(is.na(x$robots$error_class))
  expect_true(is.na(x$robots$error_stage))
})

test_that("an empty 200 body is a valid fetched result (raw(0))", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = status_resp(200L))
  ))
  x <- robots_fetch("http://a/x")

  expect_identical(x$robots$fetch_outcome, "fetched")
  expect_identical(x$robots$body[[1]], raw(0))
  expect_identical(x$robots$body_size, 0L)
})

# --- missing (404 / 410) -----------------------------------------------------

test_that("404 and 410 classify as missing with no body and NA metadata", {
  for (code in c(404L, 410L)) {
    httr2::local_mocked_responses(mock_router(
      list("http://a/robots.txt" = status_resp(code))
    ))
    x <- robots_fetch("http://a/x")
    expect_identical(x$robots$fetch_outcome, "missing")
    expect_identical(x$robots$http_status, code)
    expect_null(x$robots$body[[1]])
    expect_true(is.na(x$robots$body_size))
    expect_true(is.na(x$robots$error_class))
  }
})

# --- partial_response (206) --------------------------------------------------

test_that("206 classifies as partial_response with response-stage metadata", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = status_resp(206L, body = "partial"))
  ))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "partial_response")
  expect_identical(x$robots$http_status, 206L)
  expect_null(x$robots$body[[1]])
  expect_identical(x$robots$error_stage, "response")
  expect_identical(x$robots$error_class, "robots_partial_response")
})

# --- http_error (1xx, 4xx incl 408/429, 5xx) --------------------------------

test_that("1xx, 4xx (incl 408/429), and 5xx classify as http_error", {
  for (code in c(199L, 403L, 408L, 429L, 503L)) {
    httr2::local_mocked_responses(mock_router(
      list("http://a/robots.txt" = status_resp(code))
    ))
    x <- robots_fetch("http://a/x")
    expect_identical(x$robots$fetch_outcome, "http_error")
    expect_identical(x$robots$http_status, code)
    expect_identical(x$robots$error_stage, "response")
    expect_identical(x$robots$error_class, "robots_http_error")
    expect_null(x$robots$body[[1]])
  }
})

# --- redirect policy ---------------------------------------------------------

test_that("HTTP-to-HTTPS upgrade redirect is followed", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list(
      "http://a/robots.txt" = status_resp(
        301L, headers = list(Location = "https://a/robots.txt")
      ),
      "https://a/robots.txt" = ok_body("user-agent: *\nallow: /")
    ),
    recorder = rec
  ))
  x <- robots_fetch("http://a/x")

  expect_identical(x$robots$fetch_outcome, "fetched")
  expect_identical(x$robots$redirect_count, 1L)
  expect_identical(x$robots$effective_url, "https://a/robots.txt")
  # Grouping key stays the ORIGINAL requested robots URL, not the destination.
  expect_identical(x$robots$robots_url, "http://a/robots.txt")
  expect_length(rec$urls, 2L)
})

test_that("HTTPS-to-HTTP downgrade redirect is rejected", {
  httr2::local_mocked_responses(mock_router(
    list(
      "https://a/robots.txt" = status_resp(
        301L, headers = list(Location = "http://a/robots.txt")
      )
    )
  ))
  x <- robots_fetch("https://a/x")
  expect_identical(x$robots$fetch_outcome, "redirect_error")
  expect_identical(x$robots$error_stage, "redirect")
  expect_identical(x$robots$error_class, "robots_redirect_error")
  expect_true(is.na(x$robots$http_status))
})

test_that("a redirect to a non-HTTP(S) scheme is a redirect_error", {
  httr2::local_mocked_responses(mock_router(
    list(
      "http://a/robots.txt" = status_resp(
        302L, headers = list(Location = "ftp://a/robots.txt")
      )
    )
  ))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "redirect_error")
})

test_that("a 3xx without a Location header is a redirect_error", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = status_resp(301L))
  ))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "redirect_error")
})

test_that("a redirect loop is detected as a redirect_error", {
  httr2::local_mocked_responses(mock_router(
    list(
      "http://a/robots.txt" = status_resp(
        302L, headers = list(Location = "http://a/robots.txt")
      )
    )
  ))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "redirect_error")
  expect_identical(x$robots$redirect_count, 0L)
})

test_that("more than five redirects is a redirect_error at the cap", {
  routes <- list(
    "http://a/robots.txt" = status_resp(
      302L, headers = list(Location = "http://a/1")
    )
  )
  for (i in 1:6) {
    routes[[sprintf("http://a/%d", i)]] <- status_resp(
      302L, headers = list(Location = sprintf("http://a/%d", i + 1L))
    )
  }
  httr2::local_mocked_responses(mock_router(routes))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "redirect_error")
  expect_identical(x$robots$redirect_count, 5L)
})

# --- transport failures ------------------------------------------------------

test_that("a DNS/connection failure classifies as network_error", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = transport_fail(
      "curl_error_couldnt_connect", "Failed to connect"
    ))
  ))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "network_error")
  expect_identical(x$robots$error_stage, "request")
  expect_identical(x$robots$error_class, "robots_network_error")
  expect_true(is.na(x$robots$http_status))
})

test_that("a TLS failure classifies as tls_error", {
  httr2::local_mocked_responses(mock_router(
    list("https://a/robots.txt" = transport_fail(
      "curl_error_peer_failed_verification", "SSL certificate problem"
    ))
  ))
  x <- robots_fetch("https://a/x")
  expect_identical(x$robots$fetch_outcome, "tls_error")
  expect_identical(x$robots$error_stage, "request")
  expect_identical(x$robots$error_class, "robots_tls_error")
})

test_that("a timeout classifies as timeout", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = transport_fail(
      "curl_error_operation_timedout", "Timeout was reached"
    ))
  ))
  x <- robots_fetch("http://a/x")
  expect_identical(x$robots$fetch_outcome, "timeout")
  expect_identical(x$robots$error_stage, "request")
  expect_identical(x$robots$error_class, "robots_timeout")
})

# --- invalid input detachment + order ---------------------------------------

test_that("an invalid URL is detached and never fetched, order preserved", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body()),
    recorder = rec
  ))
  x <- robots_fetch(c("http://a/x", "not-a-url", "http://a/y"))

  expect_identical(nrow(x$map), 3L)
  expect_identical(x$map$input_id, 1:3)
  expect_identical(
    x$map$fetch_outcome, c("fetched", "input_invalid", "fetched")
  )
  # Only the one valid origin is fetched, exactly once.
  expect_length(rec$urls, 1L)
  # The invalid row is detached from any source.
  expect_identical(x$map$source_id, c("robots_1", NA, "robots_1"))
  expect_identical(x$map$robots_url[[2]], NA_character_)
  expect_true(is.na(x$map$http_status[[2]]))
  expect_identical(x$map$error_stage[[2]], "origin")
  expect_identical(x$map$error_class[[2]], "robots_invalid_url")
  # Exactly one source row exists (the invalid input added none).
  expect_identical(nrow(x$robots), 1L)
})

test_that("distinct origins get robots_1, robots_2 in first-occurrence order", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list(
      "http://a/robots.txt" = ok_body(),
      "http://b/robots.txt" = status_resp(404L)
    ),
    recorder = rec
  ))
  x <- robots_fetch(c("http://a/x", "http://b/y", "http://a/z"))

  expect_identical(x$robots$source_id, c("robots_1", "robots_2"))
  expect_identical(
    x$robots$robots_url, c("http://a/robots.txt", "http://b/robots.txt")
  )
  expect_identical(x$map$source_id, c("robots_1", "robots_2", "robots_1"))
  # a fetched once, b fetched once (a shared by rows 1 and 3).
  expect_identical(rec$urls, c("http://a/robots.txt", "http://b/robots.txt"))
})

# --- zero-length input -------------------------------------------------------

test_that("zero-length url yields correctly typed empty frames", {
  x <- robots_fetch(character(0))
  expect_s3_class(x, "robots_fetches")
  expect_identical(nrow(x$map), 0L)
  expect_identical(nrow(x$robots), 0L)
  expect_type(x$map$input_id, "integer")
  expect_type(x$map$url, "character")
  expect_type(x$robots$body, "list")
  expect_type(x$robots$timeout, "double")
})

# --- fetch user agent + no userinfo/cookie forwarding -----------------------

test_that("the default package fetch user agent is sent", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body()),
    recorder = rec
  ))
  robots_fetch("http://a/x")
  expect_match(rec$agents[[1]], "^robotstxtr/")
})

test_that("a caller-supplied fetch user agent overrides the default", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body()),
    recorder = rec
  ))
  robots_fetch("http://a/x", fetch_user_agent = "my-crawler/9")
  expect_identical(rec$agents[[1]], "my-crawler/9")
})

test_that("userinfo is dropped and no auth/cookie headers are forwarded", {
  rec <- new_recorder()
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body()),
    recorder = rec
  ))
  robots_fetch("http://user:pass@a/secret")
  # The requested URL is the userinfo-free grouping key.
  expect_identical(rec$urls, "http://a/robots.txt")
  req <- rec$reqs[[1]]
  expect_null(req$headers$Authorization)
  expect_null(req$headers$authorization)
  expect_null(req$headers$Cookie)
  expect_null(req$options$cookie)
})

# --- Call-level validation ---------------------------------------------------

test_that("wrong url type is a call-level error", {
  expect_error(robots_fetch(123), class = "robotstxtr_invalid_url_type")
})

test_that("a bad timeout is a call-level error", {
  expect_error(
    robots_fetch("http://a/x", timeout = -1),
    class = "robotstxtr_invalid_timeout"
  )
  expect_error(
    robots_fetch("http://a/x", timeout = c(1, 2)),
    class = "robotstxtr_invalid_timeout"
  )
  expect_error(
    robots_fetch("http://a/x", timeout = Inf),
    class = "robotstxtr_invalid_timeout"
  )
})

test_that("a bad fetch_user_agent is a call-level error", {
  expect_error(
    robots_fetch("http://a/x", fetch_user_agent = ""),
    class = "robotstxtr_invalid_fetch_user_agent"
  )
  expect_error(
    robots_fetch("http://a/x", fetch_user_agent = c("a", "b")),
    class = "robotstxtr_invalid_fetch_user_agent"
  )
})

# --- print method ------------------------------------------------------------

test_that("print.robots_fetches summarizes and returns invisibly", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body())
  ))
  x <- robots_fetch(c("http://a/x", "http://a/y"))
  expect_output(print(x), "<robots_fetches>")
  expect_invisible(print(x))
})

test_that("print.robots_fetches previews ten rows and counts the rest", {
  httr2::local_mocked_responses(mock_router(
    list("http://a/robots.txt" = ok_body())
  ))
  x <- robots_fetch(sprintf("http://a/p%02d", 1:12))
  out <- capture.output(print(x))

  expect_identical(out[[1]], "<robots_fetches>: 12 inputs, 1 source")
  # Summary + column header + exactly ten preview rows + the truncation line.
  expect_length(out, 13L)
  expect_identical(out[[13]], "  ... and 2 more rows")
  expect_true(grepl("p10", out[[12]], fixed = TRUE))
  expect_false(any(grepl("p11", out, fixed = TRUE)))

  # A single hidden row is reported in the singular.
  y <- robots_fetch(sprintf("http://a/p%02d", 1:11))
  expect_identical(
    utils::tail(capture.output(print(y)), 1L), "  ... and 1 more row"
  )

  # Exactly ten rows print in full with no truncation line at all.
  z <- robots_fetch(sprintf("http://a/p%02d", 1:10))
  expect_false(any(grepl("more row", capture.output(print(z)), fixed = TRUE)))
})

# --- Classifier fallbacks (fetch-policy internals) ---------------------------

test_that("classify_status treats any other final status as http_error", {
  expect_identical(classify_status(200L), "fetched")
  expect_identical(classify_status(206L), "partial_response")
  expect_identical(classify_status(404L), "missing")
  expect_identical(classify_status(410L), "missing")
  expect_identical(classify_status(503L), "http_error")
  # 3xx never reaches the classifier (the redirect loop owns it) and codes
  # outside 100-599 are not a body; both fall to the defensive tail rather
  # than fabricating a decision.
  expect_identical(classify_status(302L), "http_error")
  expect_identical(classify_status(600L), "http_error")
  expect_identical(classify_status(0L), "http_error")
})

test_that("fetch_error_meta falls back to no error metadata", {
  expect_identical(
    fetch_error_meta("http_error"),
    c(stage = "response", class = "robots_http_error")
  )
  expect_identical(
    fetch_error_meta("fetched"),
    c(stage = NA_character_, class = NA_character_)
  )
  # An outcome with no mapped metadata (e.g. the supplied-body path, which
  # never fetches) yields NA stage/class rather than erroring on the switch.
  expect_identical(
    fetch_error_meta("supplied"),
    c(stage = NA_character_, class = NA_character_)
  )
})
