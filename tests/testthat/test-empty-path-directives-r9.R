# Slice R9 fork-fix: an `allow`/`disallow` directive with an EMPTY PATH is
# ignored by the matcher (per Google; verified against upstream
# robots_test.cc:317-323). It wins nothing, so its match metadata must surface
# as default_allow/none/NA/NA even though the reporting layer reports a positive
# matching line for the ignored empty-path directive. The crawl decision
# (allowed = TRUE) is unchanged. This is normalized identically on BOTH the text
# path (allowed_by_robots_text) and the URL path (allowed_by_robots_url, mocked
# offline via httr2::local_mocked_responses).

# --- Local URL-path mock helper (offline) -----------------------------------

serve_body <- function(body) {
  force(body)
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200L, url = req$url, body = charToRaw(body))
  }, env = parent.frame())
}

expect_ignored_empty_path <- function(x) {
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "default_allow")
  expect_identical(x$results$matched_rule_type, "none")
  expect_true(is.na(x$results$matched_line))
  expect_true(is.na(x$results$matched_rule_value))
}

# --- Text path: ignored empty-path directives -------------------------------

test_that("text: a lone empty Disallow is default_allow (ignored)", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\ndisallow:", "http://e/x", "FooBot"
  )
  expect_ignored_empty_path(x)
})

test_that("text: a lone empty Allow is default_allow (ignored)", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\nallow:", "http://e/x", "FooBot"
  )
  expect_ignored_empty_path(x)
})

test_that("text: empty Disallow + empty Allow together are default_allow", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\ndisallow:\nallow:", "http://e/x", "FooBot"
  )
  expect_ignored_empty_path(x)
})

# --- URL path: ignored empty-path directives (mocked, offline) --------------

test_that("url: a lone empty Disallow is default_allow (ignored)", {
  serve_body("user-agent: *\ndisallow:")
  x <- allowed_by_robots_url("http://a/x", "bot")
  expect_ignored_empty_path(x)
})

test_that("url: a lone empty Allow is default_allow (ignored)", {
  serve_body("user-agent: *\nallow:")
  x <- allowed_by_robots_url("http://a/x", "bot")
  expect_ignored_empty_path(x)
})

test_that("url: empty Disallow + empty Allow together are default_allow", {
  serve_body("user-agent: *\ndisallow:\nallow:")
  x <- allowed_by_robots_url("http://a/x", "bot")
  expect_ignored_empty_path(x)
})

# --- Regression guards: real (non-empty-path) rules are NOT reclassified ----

test_that("text regression: a real Disallow: /x stays rule_disallow", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\ndisallow: /x", "http://e/x", "FooBot"
  )
  expect_false(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_disallow")
  expect_identical(x$results$matched_rule_type, "disallow")
  expect_identical(x$results$matched_rule_value, "/x")
})

test_that("text regression: Allow: /x + Disallow: /x stays rule_allow", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\nallow: /x\ndisallow: /x", "http://e/x", "FooBot"
  )
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_allow")
  expect_identical(x$results$matched_rule_type, "allow")
  expect_identical(x$results$matched_rule_value, "/x")
})

test_that("text regression: Disallow: / (non-empty) stays rule_disallow", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\ndisallow: /", "http://e/x", "FooBot"
  )
  expect_false(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_disallow")
  expect_identical(x$results$matched_rule_type, "disallow")
  expect_identical(x$results$matched_rule_value, "/")
})

test_that("text regression: no rules is default_allow/none", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot", "http://e/x", "FooBot"
  )
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "default_allow")
  expect_identical(x$results$matched_rule_type, "none")
  expect_true(is.na(x$results$matched_line))
  expect_true(is.na(x$results$matched_rule_value))
})
