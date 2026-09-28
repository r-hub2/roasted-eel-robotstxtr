# Hidden engine-aware Yandex matcher adapter (ROBO-agrvzdnr, YI4c).
#
# Exercises the package-private match_yandex_v1() -- the batch-shaped mapping of
# the checked robotstxtyandex native binding (YI4b) onto the host per-row result
# shape, per the normative §10/§11 status/reason/error and exact-byte raw-value
# contract. The adapter stays hidden: no registration, no availability flip, no
# schema-revision change. It composes the YI4a request-target extractor and the
# YI4b batch binding, both of which require the native library, so skip on a
# pure-R install.

skip_if_not(
  is.function(tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)) &&
    is.function(tryCatch(robotstxtr_extract_request_target_,
                         error = function(e) NULL)),
  "native binding not built (pure-R install)"
)

raw_body <- function(text) charToRaw(text)

# ---------------------------------------------------------------------------
# Every evaluated DecisionSource maps without reinterpretation (§10)
# ---------------------------------------------------------------------------

test_that("rule_disallow maps to a disallow decision with exact metadata", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /private")),
    urls = "https://example.test/private/x",
    product_tokens = "Yandex"
  )
  expect_identical(out$native_evaluation_status, "evaluated")
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "disallow")
  expect_identical(out$reason, "rule_disallow")
  expect_identical(out$matched_line, 2L)
  expect_identical(out$matched_rule_type, "disallow")
  expect_identical(out$matched_rule_value, "/private")
  expect_identical(out$matched_rule_value_raw[[1]], charToRaw("/private"))
  expect_true(is.na(out$error_stage))
  expect_true(is.na(out$error_class))
})

test_that("rule_allow maps to an allow decision", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nAllow: /pub\nDisallow: /")),
    urls = "https://example.test/pub",
    product_tokens = "Yandex"
  )
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "allow")
  expect_identical(out$reason, "rule_allow")
  expect_identical(out$matched_rule_type, "allow")
  expect_identical(out$matched_rule_value, "/pub")
  expect_identical(out$matched_rule_value_raw[[1]], charToRaw("/pub"))
})

test_that("default_allow uses the absent-rule representation with NULL raw", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /private")),
    urls = "https://example.test/public",
    product_tokens = "Yandex"
  )
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "allow")
  expect_identical(out$reason, "default_allow")
  expect_identical(out$matched_line, NA_integer_)
  expect_identical(out$matched_rule_type, "none")
  expect_identical(out$matched_rule_value, NA_character_)
  expect_null(out$matched_rule_value_raw[[1]])
})

test_that("effective_empty_disallow is not rewritten and keeps raw(0)", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow:")),
    urls = "https://example.test/anything",
    product_tokens = "Yandex"
  )
  expect_identical(out$matcher_status, "evaluated")
  # allow decision, but the distinct effective_empty_disallow reason/type/value.
  expect_identical(out$url_decision, "allow")
  expect_identical(out$reason, "effective_empty_disallow")
  expect_identical(out$matched_rule_type, "disallow")
  expect_identical(out$matched_line, 2L)
  expect_identical(out$matched_rule_value, "")
  # Present but empty: raw(0), distinct from NULL.
  expect_false(is.null(out$matched_rule_value_raw[[1]]))
  expect_identical(out$matched_rule_value_raw[[1]], raw(0))
})

# ---------------------------------------------------------------------------
# Checked non-evaluated shapes with exact error stage/class/reason (§10)
# ---------------------------------------------------------------------------

test_that("unsupported_crawler maps to the checked non-evaluated contract", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /")),
    urls = "https://example.test/x",
    product_tokens = "Googlebot"
  )
  expect_identical(out$native_evaluation_status, "unsupported_crawler")
  expect_identical(out$matcher_status, "not_evaluated")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$reason, "unsupported_product_token")
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_unsupported_product_token")
  expect_false(is.na(out$error_message))
  expect_identical(out$matched_line, NA_integer_)
  expect_identical(out$matched_rule_type, "unknown")
  expect_identical(out$matched_rule_value, NA_character_)
  expect_null(out$matched_rule_value_raw[[1]])
  expect_false(out$matcher_body_truncated)
})

test_that("invalid_request_target maps to the checked non-evaluated contract", {
  # A URL that passes ordinary HTTP(S) validity but the lexical extractor still
  # yields a slash-prefixed target -- so force an invalid target via a
  # non-extractable URL: an extractor NA becomes an empty target -> invalid.
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /")),
    urls = "not-a-url",
    product_tokens = "Yandex"
  )
  expect_identical(out$native_evaluation_status, "invalid_request_target")
  expect_identical(out$matcher_status, "not_evaluated")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$reason, "invalid_request_target")
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_invalid_request_target")
  expect_identical(out$matched_rule_type, "unknown")
  expect_null(out$matched_rule_value_raw[[1]])
  expect_false(out$matcher_body_truncated)
})

test_that("unsupported_crawler precedes invalid_request_target", {
  # Both inputs invalid: unsupported token AND an unextractable URL. The engine
  # decides precedence; the adapter does not re-prevalidate.
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /")),
    urls = "not-a-url",
    product_tokens = "Googlebot"
  )
  expect_identical(out$native_evaluation_status, "unsupported_crawler")
  expect_identical(out$reason, "unsupported_product_token")
})

test_that("YandexAdditionalBot is a supported token via the engine", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /private")),
    urls = "https://example.test/private/x",
    product_tokens = "YandexAdditionalBot"
  )
  expect_identical(out$native_evaluation_status, "evaluated")
})

# ---------------------------------------------------------------------------
# matcher_input_bytes + parse-once body counters
# ---------------------------------------------------------------------------

test_that("matcher_input_bytes is the complete supplied body length", {
  body <- raw_body("User-agent: Yandex\nDisallow: /private")
  out <- match_yandex_v1(
    bodies = list(body),
    urls = "https://example.test/x",   # default_allow
    product_tokens = "Yandex"
  )
  expect_identical(out$matcher_input_bytes, length(body))
  # Non-evaluated rows still report the full body length.
  out2 <- match_yandex_v1(
    bodies = list(body),
    urls = "https://example.test/x",
    product_tokens = "Googlebot"
  )
  expect_identical(out2$matcher_input_bytes, length(body))
})

test_that("a shared body is parsed exactly once across many rows", {
  body <- raw_body("User-agent: Yandex\nDisallow: /private")
  out <- match_yandex_v1(
    bodies = list(body, body, body, body),
    urls = c(
      "https://example.test/private/a", "https://example.test/public",
      "https://example.test/private/b", "https://example.test/x"
    ),
    product_tokens = "Yandex"
  )
  expect_identical(attr(out, "n_parse_calls"), 1L)
  expect_identical(nrow(out), 4L)
  expect_identical(out$url_decision,
                   c("disallow", "allow", "disallow", "allow"))
})

test_that("distinct bodies each parse once and map per row", {
  out <- match_yandex_v1(
    bodies = list(
      raw_body("User-agent: Yandex\nDisallow: /a"),
      raw_body("User-agent: Yandex\nDisallow: /b"),
      raw_body("User-agent: Yandex\nDisallow: /a")
    ),
    urls = c("https://example.test/a", "https://example.test/b",
             "https://example.test/b"),
    product_tokens = "Yandex"
  )
  expect_identical(attr(out, "n_parse_calls"), 2L)
  # body1 disallows /a, body2 disallows /b; row3 uses body1 so /b is allowed.
  expect_identical(out$url_decision, c("disallow", "disallow", "allow"))
})

# ---------------------------------------------------------------------------
# raw(0) vs NULL survives subsetting, concatenation, serialization (§11)
# ---------------------------------------------------------------------------

test_that("raw(0) and NULL stay distinct through adapter operations", {
  out <- match_yandex_v1(
    bodies = list(
      raw_body("User-agent: Yandex\nDisallow:"),          # eff-empty: raw(0)
      raw_body("User-agent: Yandex\nDisallow: /private")  # default_allow: NULL
    ),
    urls = c("https://example.test/x", "https://example.test/public"),
    product_tokens = "Yandex"
  )
  raws <- out$matched_rule_value_raw
  expect_identical(raws[[1]], raw(0))
  expect_null(raws[[2]])

  # Subsetting rows keeps each element's identity.
  sub1 <- out[1, , drop = FALSE]
  sub2 <- out[2, , drop = FALSE]
  expect_identical(sub1$matched_rule_value_raw[[1]], raw(0))
  expect_null(sub2$matched_rule_value_raw[[1]])

  # Concatenation (rbind) preserves both.
  combined <- rbind(sub2, sub1)
  expect_null(combined$matched_rule_value_raw[[1]])
  expect_identical(combined$matched_rule_value_raw[[2]], raw(0))

  # saveRDS/readRDS round-trip keeps raw(0) present and NULL absent.
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(out, path)
  restored <- readRDS(path)
  expect_identical(restored$matched_rule_value_raw[[1]], raw(0))
  expect_null(restored$matched_rule_value_raw[[2]])
})

# ---------------------------------------------------------------------------
# Byte-faithful values and embedded NUL / invalid UTF-8 body handling (§11)
# ---------------------------------------------------------------------------
#
# A matched rule VALUE that is itself invalid UTF-8 is unreachable through this
# URL-shaped adapter: the YI4a extractor only ever yields UTF-8 target bytes, so
# no ASCII/Unicode URL can match a rule value that holds a lone invalid byte.
# That native property (exact raw bytes + NA text for non-UTF-8 values) is
# covered directly at the binding layer (test-yandex-native-binding-jbhtvilo.R).
# Here we prove the adapter passes multibyte-UTF-8 values through byte-exactly
# and parses invalid/NUL-bearing BODIES without truncation.

test_that("a multibyte UTF-8 rule value survives byte-exactly", {
  out <- match_yandex_v1(
    bodies = list(raw_body("User-agent: Yandex\nDisallow: /café")),
    urls = "https://example.test/café/x",
    product_tokens = "Yandex"
  )
  expect_identical(out$reason, "rule_disallow")
  expect_identical(out$matched_rule_value, enc2utf8("/café"))
  expect_identical(out$matched_rule_value_raw[[1]],
                   charToRaw(enc2utf8("/café")))
})

test_that("embedded NUL/invalid-UTF-8 bodies are parsed, not truncated", {
  body <- c(raw_body("# c"), as.raw(0x00), as.raw(0xFE),
            raw_body("\nUser-agent: Yandex\nDisallow: /private"))
  out <- match_yandex_v1(
    bodies = list(body),
    urls = "https://example.test/private/x",
    product_tokens = "Yandex"
  )
  expect_identical(out$native_evaluation_status, "evaluated")
  expect_identical(out$url_decision, "disallow")
  expect_identical(out$reason, "rule_disallow")
  expect_identical(out$matcher_input_bytes, length(body))
  expect_false(out$matcher_body_truncated)
})

# ---------------------------------------------------------------------------
# Hidden-status invariants: no availability flip / registration / schema change
# ---------------------------------------------------------------------------

test_that("the adapter is active -- availability flipped and schema bumped", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
  # The registry now carries the batch-shaped Yandex callable.
  registry <- engine_matcher_registry_v1()
  expect_type(registry$yandex$callable, "closure")
  expect_identical(registry$yandex$availability, "available")
})
