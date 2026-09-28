# Native checked robotstxtyandex batch binding (ROBO-jbhtvilo, YI4b).
#
# Exercises the hidden, engine-neutral-named native routine
# robotstxtr_evaluate_checked_batch_() directly. It binds the vendored
# robotstxtyandex public API (Policy::Parse + EvaluateChecked); these checks
# prove parse-once batching, byte-preserving raw bodies/targets, owning
# lifetimes, every checked EvaluationStatus with its DecisionSource, the exact
# empty/raw/non-UTF-8 rule-value representation, unsupported-first precedence,
# and that the Yandex (not Google) engine ran. Post-activation engine
# availability reports Yandex available.

skip_if_not(is.function(
  tryCatch(robotstxtr_checked_batch_,
           error = function(e) NULL)),
  "native batch binding not built (pure-R install)")

# Helper: run one row against one body, returning a per-row list of scalars.
eval_one <- function(body, crawler, target) {
  body_raw <- if (is.raw(body)) body else charToRaw(body)
  target_raw <- if (is.raw(target)) target else charToRaw(target)
  res <- yandex_evaluate_checked_batch(
    bodies = list(body_raw),
    body_index = 1L,
    crawlers = crawler,
    targets = list(target_raw)
  )
  lapply(res, function(col) if (is.list(col)) col[[1]] else col[[1]])
}

# ---------------------------------------------------------------------------
# Parse-once batching
# ---------------------------------------------------------------------------

test_that("a shared body parses exactly once across many rows", {
  body <- charToRaw("User-agent: Yandex\nDisallow: /private")
  res <- yandex_evaluate_checked_batch(
    bodies = list(body),
    body_index = c(1L, 1L, 1L, 1L),
    crawlers = rep("Yandex", 4),
    targets = list(charToRaw("/private/a"), charToRaw("/public"),
                   charToRaw("/private/b"), charToRaw("/x"))
  )
  expect_identical(res$n_parse_calls, 1L)
  expect_length(res$url_decision, 4L)
  # Rows 1 and 3 hit the disallow; rows 2 and 4 are default-allowed.
  expect_identical(res$url_decision, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("distinct bodies each parse once", {
  res <- yandex_evaluate_checked_batch(
    bodies = list(charToRaw("User-agent: Yandex\nDisallow: /a"),
                  charToRaw("User-agent: Yandex\nDisallow: /b")),
    body_index = c(1L, 2L, 1L),
    crawlers = rep("Yandex", 3),
    targets = list(charToRaw("/a"), charToRaw("/b"), charToRaw("/b"))
  )
  expect_identical(res$n_parse_calls, 2L)
  # body1 disallows /a, body2 disallows /b; row3 uses body1 so /b is allowed.
  expect_identical(res$url_decision, c(FALSE, FALSE, TRUE))
  # Per-body input bytes replicate onto rows by their body_index.
  expect_identical(res$matcher_input_bytes[c(1L, 3L)],
                   rep(nchar("User-agent: Yandex\nDisallow: /a"), 2L))
})

# ---------------------------------------------------------------------------
# Every evaluated DecisionSource (spec section 10)
# ---------------------------------------------------------------------------

test_that("rule_disallow maps to a disallow decision with rule metadata", {
  r <- eval_one("User-agent: Yandex\nDisallow: /private", "Yandex",
                "/private/x")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_false(r$url_decision)
  expect_identical(r$decision_source, "rule_disallow")
  expect_identical(r$matched_line, 2L)
  expect_identical(r$matched_rule_type, "disallow")
  expect_identical(r$matched_rule_value, "/private")
  expect_identical(r$matched_rule_value_raw, charToRaw("/private"))
})

test_that("rule_allow maps to an allow decision", {
  r <- eval_one("User-agent: Yandex\nAllow: /pub\nDisallow: /", "Yandex",
                "/pub")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_true(r$url_decision)
  expect_identical(r$decision_source, "rule_allow")
  expect_identical(r$matched_rule_type, "allow")
  expect_identical(r$matched_rule_value, "/pub")
})

test_that("default_allow uses the absent-rule representation", {
  r <- eval_one("User-agent: Yandex\nDisallow: /private", "Yandex", "/public")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_true(r$url_decision)
  expect_identical(r$decision_source, "default_allow")
  expect_identical(r$matched_line, NA_integer_)
  expect_identical(r$matched_rule_type, "none")
  expect_identical(r$matched_rule_value, NA_character_)
  expect_null(r$matched_rule_value_raw)
})

test_that("effective_empty_disallow retains the empty disallow rule", {
  r <- eval_one("User-agent: Yandex\nDisallow:", "Yandex", "/anything")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_true(r$url_decision)  # empty Disallow has Allow: / effect
  expect_identical(r$decision_source, "effective_empty_disallow")
  expect_identical(r$matched_rule_type, "disallow")  # type NOT rewritten
  expect_identical(r$matched_line, 2L)
  # Present, empty value: raw(0) (distinct from NULL) and UTF-8 "".
  expect_identical(r$matched_rule_value, "")
  expect_false(is.null(r$matched_rule_value_raw))
  expect_identical(r$matched_rule_value_raw, raw(0))
  expect_length(r$matched_rule_value_raw, 0L)
})

# ---------------------------------------------------------------------------
# Non-evaluated statuses + unsupported-first precedence (spec section 10)
# ---------------------------------------------------------------------------

test_that("invalid_request_target yields no decision", {
  r <- eval_one("User-agent: Yandex\nDisallow: /", "Yandex", "no-leading-slash")
  expect_identical(r$native_evaluation_status, "invalid_request_target")
  expect_identical(r$url_decision, NA)
  expect_identical(r$decision_source, NA_character_)
  expect_identical(r$matched_line, NA_integer_)
  expect_identical(r$matched_rule_type, "unknown")
  expect_identical(r$matched_rule_value, NA_character_)
  expect_null(r$matched_rule_value_raw)
})

test_that("unsupported_crawler yields no decision", {
  r <- eval_one("User-agent: Yandex\nDisallow: /", "Googlebot", "/x")
  expect_identical(r$native_evaluation_status, "unsupported_crawler")
  expect_identical(r$url_decision, NA)
  expect_identical(r$decision_source, NA_character_)
  expect_identical(r$matched_rule_type, "unknown")
  expect_null(r$matched_rule_value_raw)
})

test_that("unsupported_crawler takes precedence over invalid_request_target", {
  # Both inputs invalid: unsupported product token AND a non-slash target.
  r <- eval_one("User-agent: Yandex\nDisallow: /", "Googlebot", "bad-target")
  expect_identical(r$native_evaluation_status, "unsupported_crawler")
})

# ---------------------------------------------------------------------------
# Byte-preserving bodies and rule values (spec sections 8, 11)
# ---------------------------------------------------------------------------

test_that("a non-UTF-8 rule value keeps exact bytes but NA text", {
  # Disallow value with a lone 0xFF (invalid UTF-8). matched against the same
  # bytes so the rule is the winner.
  body <- c(charToRaw("User-agent: Yandex\nDisallow: /"), as.raw(0xFF))
  target <- c(charToRaw("/"), as.raw(0xFF))
  r <- eval_one(body, "Yandex", target)
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_identical(r$decision_source, "rule_disallow")
  expect_identical(r$matched_rule_value, NA_character_)  # not valid UTF-8
  expect_identical(r$matched_rule_value_raw, c(charToRaw("/"), as.raw(0xFF)))
})

test_that("embedded NUL and invalid UTF-8 bodies are parsed, not truncated", {
  # A NUL byte then an invalid byte inside a comment line, followed by a real
  # rule. If the body were truncated at NUL the Disallow would be lost.
  body <- c(charToRaw("# c"), as.raw(0x00), as.raw(0xFE),
            charToRaw("\nUser-agent: Yandex\nDisallow: /private"))
  r <- eval_one(body, "Yandex", "/private/x")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_false(r$url_decision)
  expect_identical(r$decision_source, "rule_disallow")
  # Whole body length reported; nothing truncated.
  expect_identical(r$matcher_input_bytes, length(body))
  expect_false(r$matcher_body_truncated)
})

test_that("matcher_request_target_raw carries the exact target bytes", {
  target <- c(charToRaw("/a"), as.raw(0x00), charToRaw("b"))
  r <- eval_one("User-agent: Yandex\nDisallow: /z", "Yandex", target)
  expect_identical(r$matcher_request_target_raw, target)
})

# ---------------------------------------------------------------------------
# Owning lifetimes, truncation flag, and engine identity
# ---------------------------------------------------------------------------

test_that("returned values survive garbage collection (owning, no views)", {
  res <- yandex_evaluate_checked_batch(
    bodies = list(charToRaw("User-agent: Yandex\nDisallow: /private")),
    body_index = 1L,
    crawlers = "Yandex",
    targets = list(charToRaw("/private/x"))
  )
  gc()
  gc()
  expect_identical(res$decision_source, "rule_disallow")
  expect_identical(res$matched_rule_value_raw[[1]], charToRaw("/private"))
})

test_that("matcher_body_truncated is FALSE and input bytes match the body", {
  body <- charToRaw("User-agent: Yandex\nDisallow: /private")
  r <- eval_one(body, "Yandex", "/x")
  expect_false(r$matcher_body_truncated)
  expect_identical(r$matcher_input_bytes, length(body))
})

test_that("the Yandex engine ran, not Google (effective-empty is Yandex)", {
  # Google's matcher has no effective_empty_disallow decision source; producing
  # it proves this path dispatched to robotstxtyandex, not googlebot.
  r <- eval_one("User-agent: Yandex\nDisallow:", "Yandex", "/x")
  expect_identical(r$decision_source, "effective_empty_disallow")
})

test_that("Yandex engine availability is now available", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
})
