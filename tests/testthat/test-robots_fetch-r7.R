# Slice R7: streaming decoded-byte `max_bytes` enforcement plus strict
# `max_bytes` call-level validation (PRD 6.4 streaming note, 6.6 shared fetch
# controls). No external network is used: the compressed/plain-body cases stand
# up a localhost httpuv server in a SEPARATE process (an in-process server would
# deadlock, since httpuv runs its R app callback on the main thread while the
# HTTP client blocks it). The decoded-byte accumulator is also unit-tested with
# synthetic chunks, proving count/abort/no-store independently of any transport.

# --- Localhost server harness (separate process via callr) ------------------

# Start a background httpuv server that returns `body_raw` with `headers` on
# every request, poll until it is ready, run `fn(srv)`, and always kill the
# server afterwards (on.exit fires even if an expectation inside `fn` fails).
with_body_server <- function(body_raw, headers, fn) {
  # This harness binds a real localhost server on 127.0.0.1, which the SSRF
  # guard (ROBO-quovenef) refuses by default. Override the guard binding for the
  # duration of this harness ONLY (test-scoped, no production disable path) so
  # the real fetch/streaming machinery can be exercised against loopback.
  testthat::local_mocked_bindings(
    robots_ssrf_check = function(url) list(allowed = TRUE, reason = NA)
  )
  port <- httpuv::randomPort()
  proc <- callr::r_bg(
    function(port, body_raw, headers) {
      httpuv::runServer("127.0.0.1", port, list(
        call = function(req) {
          list(status = 200L, headers = headers, body = body_raw)
        }
      ))
    },
    args = list(port = port, body_raw = body_raw, headers = headers)
  )
  on.exit(proc$kill(), add = TRUE)

  url <- sprintf("http://127.0.0.1:%d/robots.txt", port)
  ready <- FALSE
  for (i in seq_len(100L)) {
    if (!proc$is_alive()) {
      break
    }
    ok <- tryCatch({
      req <- httr2::req_error(httr2::request(url), is_error = function(r) FALSE)
      httr2::req_perform(httr2::req_timeout(req, 1))
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      ready <- TRUE
      break
    }
    Sys.sleep(0.05)
  }
  fn(list(url = url, port = port, ready = ready))
}

# --- Tracer bullet: gzip body under the limit, decoded body over it ---------

test_that("gzip body under limit but decoded over limit is body_too_large", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")

  # Highly compressible ~100 KB decoded body; its gzip encoding is a few hundred
  # bytes. With max_bytes between the two, a compressed Content-Length or
  # compressed-byte check would wrongly pass; only decoded counting fails it.
  decoded <- charToRaw(strrep("user-agent: *\ndisallow: /private\n", 3000L))
  gz <- memCompress(decoded, type = "gzip")
  expect_lt(length(gz), 50000L)
  expect_gt(length(decoded), 50000L)

  with_body_server(gz, list("Content-Encoding" = "gzip"), function(srv) {
    if (!srv$ready) {
      skip("localhost test server did not become ready")
    }
    x <- robots_fetch(srv$url, max_bytes = 50000L, timeout = 5)

    expect_identical(x$robots$fetch_outcome, "body_too_large")
    # No body is stored and none is matched: NULL body, NA size.
    expect_null(x$robots$body[[1]])
    expect_true(is.na(x$robots$body_size))
    # Response-stage error metadata for body_too_large.
    expect_identical(x$robots$error_stage, "response")
    expect_identical(x$robots$error_class, "robots_body_too_large")
    # The real final status and effective URL are still recorded.
    expect_identical(x$robots$http_status, 200L)
    expect_identical(x$robots$effective_url, srv$url)
    # And the per-input map row reflects the same outcome/metadata.
    expect_identical(x$map$fetch_outcome, "body_too_large")
    expect_identical(x$map$error_class, "robots_body_too_large")
  })
})

# --- Boundary: exactly at the limit vs one byte over ------------------------

test_that("a body exactly at the limit is fetched and stored", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")

  limit <- 4096L
  body <- charToRaw(strrep("a", limit)) # decoded length == limit
  with_body_server(body, list(), function(srv) {
    if (!srv$ready) {
      skip("localhost test server did not become ready")
    }
    x <- robots_fetch(srv$url, max_bytes = limit, timeout = 5)

    expect_identical(x$robots$fetch_outcome, "fetched")
    expect_identical(x$robots$body[[1]], body)
    expect_identical(x$robots$body_size, limit)
    expect_true(is.na(x$robots$error_class))
  })
})

test_that("a body one byte over the limit is body_too_large", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")

  limit <- 4096L
  body <- charToRaw(strrep("a", limit + 1L)) # one byte over
  with_body_server(body, list(), function(srv) {
    if (!srv$ready) {
      skip("localhost test server did not become ready")
    }
    x <- robots_fetch(srv$url, max_bytes = limit, timeout = 5)

    expect_identical(x$robots$fetch_outcome, "body_too_large")
    expect_null(x$robots$body[[1]])
    expect_true(is.na(x$robots$body_size))
    expect_identical(x$robots$error_class, "robots_body_too_large")
  })
})

# --- A small uncompressed body under the limit fetches normally -------------

test_that("a small uncompressed body under the limit is fetched", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")

  body <- charToRaw("user-agent: *\ndisallow: /admin")
  with_body_server(body, list(), function(srv) {
    if (!srv$ready) {
      skip("localhost test server did not become ready")
    }
    x <- robots_fetch(srv$url, timeout = 5) # default max_bytes = 524288

    expect_identical(x$robots$fetch_outcome, "fetched")
    expect_identical(x$robots$body[[1]], body)
    expect_identical(x$robots$body_size, length(body))
  })
})

# --- Decoded-byte accumulator unit tests (synthetic chunks, no transport) ---

test_that("accumulate_within_limit assembles a body under the limit", {
  chunks <- list(as.raw(1:10), as.raw(11:20), as.raw(21:30))
  state <- new.env(parent = emptyenv())
  state$i <- 0L
  read_chunk <- function() {
    state$i <- state$i + 1L
    if (state$i > length(chunks)) raw(0) else chunks[[state$i]]
  }
  res <- accumulate_within_limit(read_chunk, max_bytes = 100L)
  expect_false(res$exceeded)
  expect_identical(res$body, as.raw(1:30))
})

test_that("accumulate_within_limit accepts a body exactly at the limit", {
  chunks <- list(as.raw(1:10), as.raw(11:20))
  state <- new.env(parent = emptyenv())
  state$i <- 0L
  read_chunk <- function() {
    state$i <- state$i + 1L
    if (state$i > length(chunks)) raw(0) else chunks[[state$i]]
  }
  res <- accumulate_within_limit(read_chunk, max_bytes = 20L)
  expect_false(res$exceeded)
  expect_identical(res$body, as.raw(1:20))
})

test_that("accumulate_within_limit aborts early and stores no body when over", {
  # Total would be 30 bytes; limit is 15. Reading must stop as soon as the
  # running total crosses the limit and never assemble a truncated body.
  state <- new.env(parent = emptyenv())
  state$reads <- 0L
  chunks <- list(as.raw(1:10), as.raw(11:20), as.raw(21:30))
  read_chunk <- function() {
    state$reads <- state$reads + 1L
    if (state$reads > length(chunks)) raw(0) else chunks[[state$reads]]
  }
  res <- accumulate_within_limit(read_chunk, max_bytes = 15L)
  expect_true(res$exceeded)
  expect_null(res$body)
  # It crossed the limit on the second chunk and stopped pulling further chunks.
  expect_identical(state$reads, 2L)
})

test_that("accumulate_within_limit yields raw(0) for an empty stream", {
  res <- accumulate_within_limit(function() raw(0), max_bytes = 10L)
  expect_false(res$exceeded)
  expect_identical(res$body, raw(0))
})

# --- Strict max_bytes validation (PRD 6.6) ----------------------------------

test_that("validate_max_bytes accepts whole-number doubles as integer", {
  expect_identical(validate_max_bytes(524288L), 524288L)
  expect_identical(validate_max_bytes(524288), 524288L)
  expect_identical(validate_max_bytes(1e5), 100000L)
  expect_type(validate_max_bytes(1e5), "integer")
})

test_that("a fractional max_bytes is a call-level error", {
  expect_error(
    robots_fetch("http://a/x", max_bytes = 0.5),
    class = "robotstxtr_invalid_max_bytes"
  )
  expect_error(
    robots_fetch("http://a/x", max_bytes = 100.25),
    class = "robotstxtr_invalid_max_bytes"
  )
})

test_that("a non-positive max_bytes is a call-level error", {
  expect_error(
    robots_fetch("http://a/x", max_bytes = 0),
    class = "robotstxtr_invalid_max_bytes"
  )
  expect_error(
    robots_fetch("http://a/x", max_bytes = -1),
    class = "robotstxtr_invalid_max_bytes"
  )
})

test_that("an out-of-range max_bytes (> integer.max) is a call-level error", {
  expect_error(
    robots_fetch("http://a/x", max_bytes = .Machine$integer.max + 1),
    class = "robotstxtr_invalid_max_bytes"
  )
})

test_that("a non-scalar, NA, Inf, or non-numeric max_bytes is an error", {
  expect_error(
    robots_fetch("http://a/x", max_bytes = c(1, 2)),
    class = "robotstxtr_invalid_max_bytes"
  )
  expect_error(
    robots_fetch("http://a/x", max_bytes = NA_integer_),
    class = "robotstxtr_invalid_max_bytes"
  )
  expect_error(
    robots_fetch("http://a/x", max_bytes = Inf),
    class = "robotstxtr_invalid_max_bytes"
  )
  expect_error(
    robots_fetch("http://a/x", max_bytes = "1000"),
    class = "robotstxtr_invalid_max_bytes"
  )
})
