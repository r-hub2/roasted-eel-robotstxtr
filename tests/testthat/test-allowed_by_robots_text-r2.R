# Slice R2: full text-path result contract and input rules for
# allowed_by_robots_text(). Covers every text-path decision_source value,
# scalar user-agent expansion, equal-length vectors, zero-length input, length
# and type errors, the detachment rule, byte-encoded bodies, and match metadata.
# matched_rule_value stays NA (callback correlation is slice R3).

# --- Full schema -------------------------------------------------------------

test_that("results carries the full contract schema with correct types", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/x", "b")

  expect_named(x$results, c(
    "input_id", "url", "user_agent", "allowed", "decision_source",
    "source_id", "robots_url", "http_status", "fetch_outcome",
    "error_stage", "error_class", "error_message",
    "matched_line", "matched_rule_type", "matched_rule_value"
  ))
  expect_type(x$results$input_id, "integer")
  expect_type(x$results$url, "character")
  expect_type(x$results$user_agent, "character")
  expect_type(x$results$allowed, "logical")
  expect_type(x$results$decision_source, "character")
  expect_type(x$results$http_status, "integer")
  expect_type(x$results$matched_line, "integer")
  expect_type(x$results$matched_rule_value, "character")
})

test_that("robots carries the full contract schema with a raw body column", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/x", "b")

  expect_named(x$robots, c(
    "source_id", "source_type", "robots_url", "effective_url", "http_status",
    "fetch_outcome", "redirect_count", "body_size", "timeout", "max_bytes",
    "error_stage", "error_class", "error_message", "body"
  ))
  expect_identical(nrow(x$robots), 1L)
  expect_type(x$robots$body, "list")
  expect_type(x$robots$body[[1]], "raw")
  expect_identical(x$robots$source_type, "supplied")
})

# --- Every text-path decision_source value ----------------------------------

test_that("rule_disallow: a Disallow that wins yields FALSE", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/x", "b")
  expect_false(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_disallow")
  expect_identical(x$results$matched_rule_type, "disallow")
  expect_identical(x$results$matched_line, 2L)
})

test_that("rule_allow: an Allow that wins yields TRUE", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /\nallow: /ok", "http://a/ok", "b"
  )
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_allow")
  expect_identical(x$results$matched_rule_type, "allow")
  expect_gt(x$results$matched_line, 0L)
})

test_that("default_allow: no match yields TRUE and matched_line NA", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/y", "b")
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "default_allow")
  expect_identical(x$results$matched_rule_type, "none")
  expect_true(is.na(x$results$matched_line))
})

test_that("input_unknown: an empty URL yields NA with detachment metadata", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "", "b")
  expect_true(is.na(x$results$allowed))
  expect_identical(x$results$decision_source, "input_unknown")
  expect_identical(x$results$matched_rule_type, "unknown")
  expect_identical(x$results$fetch_outcome, "input_invalid")
})

# --- Scalar user-agent expansion & equal-length vectors ---------------------

test_that("a scalar user agent expands across every URL", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x",
    c("http://a/x", "http://a/y", "http://a/z"),
    "bot"
  )
  expect_identical(nrow(x$results), 3L)
  expect_identical(x$results$user_agent, rep("bot", 3L))
  expect_identical(x$results$input_id, 1:3)
  expect_identical(
    x$results$decision_source,
    c("rule_disallow", "default_allow", "default_allow")
  )
})

test_that("equal-length url and user_agent vectors match per row", {
  body <- "user-agent: alpha\ndisallow: /x\nuser-agent: beta\nallow: /"
  x <- allowed_by_robots_text(
    body,
    c("http://a/x", "http://a/x"),
    c("alpha", "beta")
  )
  expect_identical(nrow(x$results), 2L)
  expect_identical(x$results$user_agent, c("alpha", "beta"))
  # alpha is disallowed on /x; beta is allowed everywhere.
  expect_identical(x$results$allowed, c(FALSE, TRUE))
})

# --- Zero-length input -------------------------------------------------------

test_that("zero-length url returns an empty results frame", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", character(0), "b")
  expect_s3_class(x, "robots_decisions")
  expect_identical(nrow(x$results), 0L)
  # The supplied body is still recorded as a source.
  expect_identical(nrow(x$robots), 1L)
})

test_that("zero-length url tolerates a zero-length user_agent", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x", character(0), character(0)
  )
  expect_identical(nrow(x$results), 0L)
})

# --- Call-level type and length errors --------------------------------------

test_that("a user_agent length mismatch is a call-level error", {
  expect_error(
    allowed_by_robots_text(
      "u", c("http://a/x", "http://a/y", "http://a/z"), c("a", "b")
    ),
    class = "robotstxtr_length_mismatch"
  )
})

test_that("a non-character url is a call-level error, not coercion", {
  expect_error(
    allowed_by_robots_text("u", 123, "b"),
    class = "robotstxtr_invalid_url_type"
  )
})

test_that("a non-character user_agent is a call-level error", {
  expect_error(
    allowed_by_robots_text("u", "http://a/x", 1L),
    class = "robotstxtr_invalid_user_agent_type"
  )
})

test_that("a non-scalar or missing robots_txt is a call-level error", {
  expect_error(
    allowed_by_robots_text(c("a", "b"), "http://a/x", "b"),
    class = "robotstxtr_invalid_robots_txt"
  )
  expect_error(
    allowed_by_robots_text(NA_character_, "http://a/x", "b"),
    class = "robotstxtr_invalid_robots_txt"
  )
})

test_that("an empty or missing source_id is a call-level error", {
  expect_error(
    allowed_by_robots_text("u", "http://a/x", "b", source_id = ""),
    class = "robotstxtr_invalid_source_id"
  )
  expect_error(
    allowed_by_robots_text("u", "http://a/x", "b", source_id = NA_character_),
    class = "robotstxtr_invalid_source_id"
  )
})

# --- Detachment rule in a mixed-validity vector -----------------------------

test_that("invalid rows are detached and input order is preserved", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x",
    c("http://a/x", "", NA_character_, "http://a/y"),
    "bot"
  )

  expect_identical(nrow(x$results), 4L)
  expect_identical(x$results$input_id, 1:4)

  # Rows 1 and 4 are valid; rows 2 (empty) and 3 (NA) are invalid.
  expect_identical(
    x$results$decision_source,
    c("rule_disallow", "input_unknown", "input_unknown", "default_allow")
  )
  expect_identical(x$results$allowed, c(FALSE, NA, NA, TRUE))
  expect_identical(x$results$fetch_outcome,
                   c("supplied", "input_invalid", "input_invalid", "supplied"))

  # Valid rows reference the supplied body; invalid rows are detached.
  expect_identical(
    x$results$source_id, c("supplied", NA, NA, "supplied")
  )
  # Detached rows carry the invalid-URL error metadata.
  expect_identical(
    x$results$error_class,
    c(NA, "robots_invalid_url", "robots_invalid_url", NA)
  )
  expect_identical(
    x$results$error_stage, c(NA, "origin", "origin", NA)
  )
  # The supplied body still exists in the robots table (single source).
  expect_identical(nrow(x$robots), 1L)
  expect_identical(x$robots$source_id, "supplied")
})

test_that("an invalid user agent detaches the row with input-stage metadata", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x",
    c("http://a/x", "http://a/x"),
    c("bot", "")
  )
  expect_identical(
    x$results$decision_source, c("rule_disallow", "input_unknown")
  )
  expect_identical(x$results$error_class, c(NA, "robots_invalid_user_agent"))
  expect_identical(x$results$error_stage, c(NA, "input"))
  expect_identical(x$results$source_id, c("supplied", NA))
})

test_that("URL invalidity takes precedence when both elements are invalid", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x", "", ""
  )
  # URL validity is checked first, so the URL error is the primary one.
  expect_identical(x$results$error_class, "robots_invalid_url")
  expect_identical(x$results$error_stage, "origin")
})

# --- matched_rule_value: filled for matched rows, NA otherwise (R3) ---------

test_that("matched_rule_value is filled for matched rows, NA otherwise", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x\nallow: /ok",
    c("http://a/x", "http://a/ok", "http://a/y", ""),
    "bot"
  )
  # R3 populates matched rows (disallow /x, allow /ok) with the canonical
  # callback value; unmatched (default_allow) and invalid (input_unknown) rows
  # stay NA.
  expect_identical(
    x$results$matched_rule_value,
    c("/x", "/ok", NA_character_, NA_character_)
  )
  expect_type(x$results$matched_rule_value, "character")
})

# --- Body handling: UTF-8 once and byte-encoded input -----------------------

test_that("the supplied body is stored once as UTF-8 bytes", {
  body <- "user-agent: *\ndisallow: /café"
  x <- allowed_by_robots_text(body, "http://a/x", "b")
  expect_identical(x$robots$body[[1]], charToRaw(enc2utf8(body)))
  expect_identical(x$robots$body_size, length(charToRaw(enc2utf8(body))))
})

test_that("a byte-encoded body is handled and matches, value stays NA", {
  # A non-UTF-8 (Latin-1) byte sequence supplied with Encoding = 'bytes' must
  # not trigger a UTF-8 translation error; it is used verbatim.
  raw_bytes <- as.raw(c(
    charToRaw("user-agent: *\ndisallow: /caf"), as.raw(0xe9)
  ))
  body <- rawToChar(raw_bytes)
  Encoding(body) <- "bytes"

  x <- allowed_by_robots_text(body, "http://a/x", "b")
  expect_s3_class(x, "robots_decisions")
  expect_identical(x$robots$body[[1]], raw_bytes)
  expect_true(is.na(x$results$matched_rule_value))
  # The URL is not under the disallowed prefix, so it is allowed.
  expect_true(x$results$allowed)
})

# --- S3 constructor + printer ------------------------------------------------

test_that("print.robots_decisions summarizes and returns invisibly", {
  x <- allowed_by_robots_text(
    "user-agent: *\ndisallow: /x",
    c("http://a/x", "http://a/y", ""),
    "bot"
  )
  expect_output(print(x), "<robots_decisions>")
  expect_output(print(x), "allowed: 1")
  expect_invisible(print(x))
})

test_that("print.robots_decisions truncates the preview beyond ten rows", {
  # The printer previews at most ten rows and reports the remainder as a
  # "... and N more rows" footer.
  urls <- sprintf("http://a/p%02d", seq_len(12))
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", urls, "bot")
  expect_output(print(x), "12 results")
  expect_output(print(x), "[.][.][.] and 2 more rows")
  expect_invisible(print(x))
})
