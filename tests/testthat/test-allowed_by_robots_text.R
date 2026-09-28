# Slice R1: happy-path decision_source values and body round-trip for
# allowed_by_robots_text(). Full input-validation, match-metadata correlation,
# and fetch paths are exercised by later slices (R2, R3, R5-R8).

test_that("Disallow that wins yields rule_disallow / FALSE (R1 acceptance)", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x", "http://a/x", "bot"
  )

  expect_s3_class(x, "robots_decisions")
  expect_named(x, c("results", "robots"))
  expect_identical(nrow(x$results), 1L)
  expect_false(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_disallow")
})

test_that("an Allow directive that wins yields rule_allow / TRUE", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /\nallow: /ok",
    "http://a/ok",
    "bot"
  )

  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_allow")
})

test_that("no matching directive yields default_allow / TRUE", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x", "http://a/y", "bot"
  )

  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "default_allow")
})

test_that("the exact supplied body round-trips through robots$body as raw", {
  body <- "user-agent: *\ndisallow: /x"
  x <- allowed_by_robots_text(body, "http://a/x", "bot")

  expect_type(x$robots$body, "list")
  expect_identical(nrow(x$robots), 1L)
  expect_identical(x$robots$body[[1]], charToRaw(body))
  expect_identical(rawToChar(x$robots$body[[1]]), body)
  expect_identical(x$robots$body_size, length(charToRaw(body)))
})

test_that("results and robots carry the R1 skeleton shape", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x", "http://a/x", "bot"
  )

  # Input identity and order preserved.
  expect_identical(x$results$input_id, 1L)
  expect_identical(x$results$url, "http://a/x")
  expect_identical(x$results$user_agent, "bot")

  # Supplied-source wiring.
  expect_identical(x$results$source_id, "supplied")
  expect_identical(x$results$fetch_outcome, "supplied")
  expect_identical(x$robots$source_id, "supplied")
  expect_identical(x$robots$source_type, "supplied")
  expect_identical(x$robots$redirect_count, 0L)

  # Fetch/network columns owned by later slices remain NA on the text path.
  expect_true(is.na(x$results$robots_url))
  expect_true(is.na(x$results$http_status))

  # Match metadata on the text path: matched_line is the upstream one-based
  # matching line (the disallow sits on line 2), matched_rule_type is the
  # callback-derived type, and R3 fills matched_rule_value with the canonical
  # callback value for the matched directive.
  expect_identical(x$results$matched_line, 2L)
  expect_identical(x$results$matched_rule_type, "disallow")
  expect_identical(x$results$matched_rule_value, "/x")
})

test_that("url is vectorized with a scalar user agent", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x",
    c("http://a/x", "http://a/y"),
    "bot"
  )

  expect_identical(nrow(x$results), 2L)
  expect_identical(x$results$input_id, c(1L, 2L))
  expect_identical(x$results$allowed, c(FALSE, TRUE))
  expect_identical(
    x$results$decision_source,
    c("rule_disallow", "default_allow")
  )
})
