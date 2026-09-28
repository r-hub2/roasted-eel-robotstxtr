# Slice R9 regression: a transport failure occurring DURING the final body
# stream (e.g. the server drops the connection mid-body) must be classified as a
# transport outcome (timeout / tls_error / network_error, PRD 6.4) and MUST NOT
# abort the whole robots_fetch()/allowed_by_robots_url() call. R7 moved the body
# read onto a streaming connection whose read happens OUTSIDE the tryCatch that
# guards connection-OPEN; without the call-site wrapper in perform_fetch a
# mid-stream drop escapes as an uncaught error.
#
# Mechanism (deterministic, fully offline): local_mocked_responses() drives the
# fetch to a 200 so perform_fetch reaches its `fetched` branch, and
# local_mocked_bindings() replaces read_body_within_limit() with one that
# signals a curl transport condition shaped exactly like the real mid-stream
# drop (an httr2_failure wrapping a curl_error_* condition). This exercises the
# regression's exact code path -- the tryCatch around the body read in
# perform_fetch -- without a live network or a flaky partial-body server. A
# real localhost server that lies about Content-Length and closes mid-transfer
# reproduces the same curl condition but is timing-sensitive; mocking the read
# reproduces it deterministically, so no skip guard is needed.

# A signalled mid-stream transport failure: an httr2_failure wrapping a
# curl_error_* condition (curl >= 5 classes conditions after the CURLcode).
# curl_error_partial_file is what curl raises when the peer closes before the
# declared body has fully arrived; it is neither a timeout nor a TLS error, so
# classify_transport_condition() maps it to network_error.
midstream_drop <- function(curl_class = "curl_error_partial_file",
                           msg = "transfer closed with bytes remaining") {
  force(curl_class)
  force(msg)
  function(resp, max_bytes) {
    parent <- structure(
      class = c(curl_class, "curl_error", "error", "condition"),
      list(message = msg, call = NULL)
    )
    stop(structure(
      class = c("httr2_failure", "httr2_error", "error", "condition"),
      list(
        message = "Failed to perform HTTP request.", parent = parent,
        call = NULL
      )
    ))
  }
}

test_that("a mid-stream transport drop is classified, not aborted", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L, url = req$url,
      body = charToRaw("user-agent: *\ndisallow: /private")
    )
  })
  testthat::local_mocked_bindings(
    read_body_within_limit = midstream_drop(),
    .package = "robotstxtr"
  )

  # The call must return normally (no uncaught abort), with one robots row.
  x <- robots_fetch("http://a/x")

  expect_s3_class(x, "robots_fetches")
  expect_identical(nrow(x$robots), 1L)
  expect_true(x$robots$fetch_outcome %in%
                c("network_error", "tls_error", "timeout"))
  # curl_error_partial_file -> network_error, with request-stage metadata.
  expect_identical(x$robots$fetch_outcome, "network_error")
  expect_identical(x$robots$error_stage, "request")
  expect_identical(x$robots$error_class, "robots_network_error")
  expect_true(nzchar(x$robots$error_message))
  # A mid-stream failure means the fetch did not complete: no body/status/URL.
  expect_null(x$robots$body[[1]])
  expect_true(is.na(x$robots$body_size))
  expect_true(is.na(x$robots$http_status))
  expect_true(is.na(x$robots$effective_url))
})

test_that("a mid-stream drop does not abort allowed_by_robots_url()", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L, url = req$url,
      body = charToRaw("user-agent: *\ndisallow: /private")
    )
  })
  testthat::local_mocked_bindings(
    read_body_within_limit = midstream_drop(),
    .package = "robotstxtr"
  )

  # The transport outcome propagates as a fetch-stage classification; the call
  # completes rather than erroring out. A failed fetch yields no body to match,
  # so the decision is fetch_unknown (allowed NA), never a crash.
  x <- allowed_by_robots_url("http://a/private", "bot")
  expect_s3_class(x, "robots_decisions")
  expect_identical(nrow(x$results), 1L)
  expect_identical(x$results$fetch_outcome, "network_error")
  expect_identical(x$results$decision_source, "fetch_unknown")
  expect_true(is.na(x$results$allowed))
})

# A mid-stream TLS renegotiation failure classifies as tls_error, and a
# mid-stream timeout classifies as timeout -- same classifier as connection
# open, proving the wrapper routes the whole condition chain, not just one code.
test_that("mid-stream tls and timeout drops classify accordingly", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L, url = req$url, body = charToRaw("user-agent: *\n")
    )
  })

  testthat::local_mocked_bindings(
    read_body_within_limit = midstream_drop(
      "curl_error_peer_failed_verification", "SSL peer certificate failed"
    ),
    .package = "robotstxtr"
  )
  x_tls <- robots_fetch("https://a/x")
  expect_identical(x_tls$robots$fetch_outcome, "tls_error")
  expect_identical(x_tls$robots$error_class, "robots_tls_error")

  testthat::local_mocked_bindings(
    read_body_within_limit = midstream_drop(
      "curl_error_operation_timedout", "Operation timed out mid-transfer"
    ),
    .package = "robotstxtr"
  )
  x_to <- robots_fetch("https://a/x")
  expect_identical(x_to$robots$fetch_outcome, "timeout")
  expect_identical(x_to$robots$error_class, "robots_timeout")
})
