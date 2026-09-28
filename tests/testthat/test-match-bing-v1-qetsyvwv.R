# Engine-aware Bing matcher adapter (ROBO-qetsyvwv, BI5).
#
# Exercises the package-private match_bing_v1() -- the batch-shaped mapping of
# the native robotstxtbing binding (BI4) onto the host per-row result shape, per
# the normative SS10/SS11 status/reason/error and exact-byte raw-value contract.
# It composes the Bing request-target extractor and the Bing batch binding, both
# native, so skip on a pure-R install.

skip_if_not(
  is.function(tryCatch(robotstxtr_bing_eval_batch_, error = function(e) NULL)),
  "native bing binding not built (pure-R install)"
)

raw_body <- function(text) charToRaw(text)

# ---------------------------------------------------------------------------
# Evaluated outcomes (SS10.1) map without reinterpretation
# ---------------------------------------------------------------------------

test_that("rule_disallow maps to a disallow decision with exact metadata", {
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /private")),
    urls = "https://example.test/private/x",
    product_tokens = "bingbot"
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
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nAllow: /pub\nDisallow: /")),
    urls = "https://example.test/pub",
    product_tokens = "bingbot"
  )
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "allow")
  expect_identical(out$reason, "rule_allow")
  expect_identical(out$matched_rule_type, "allow")
  expect_identical(out$matched_rule_value, "/pub")
  expect_identical(out$matched_rule_value_raw[[1]], charToRaw("/pub"))
})

test_that("default_allow uses the absent-rule representation with NULL raw", {
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /private")),
    urls = "https://example.test/public",
    product_tokens = "bingbot"
  )
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "allow")
  expect_identical(out$reason, "default_allow")
  expect_identical(out$matched_line, NA_integer_)
  expect_identical(out$matched_rule_type, "none")
  expect_identical(out$matched_rule_value, NA_character_)
  expect_null(out$matched_rule_value_raw[[1]])
})

test_that("an empty Disallow is inert (default_allow) per shipped profile", {
  # SS10.1: the shipped Bing profile treats an empty Disallow as inert, so the
  # native decision is default_allow -- the adapter reflects the shipped
  # decision and never fabricates an effective_empty_disallow.
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow:")),
    urls = "https://example.test/anything",
    product_tokens = "bingbot"
  )
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "allow")
  expect_identical(out$reason, "default_allow")
  expect_identical(out$matched_rule_type, "none")
  expect_null(out$matched_rule_value_raw[[1]])
})

test_that("the adidxbot profile evaluates its own rules", {
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: adidxbot\nDisallow: /ads")),
    urls = "https://example.test/ads/x",
    product_tokens = "adidxbot"
  )
  expect_identical(out$matcher_status, "evaluated")
  expect_identical(out$url_decision, "disallow")
  expect_identical(out$reason, "rule_disallow")
  expect_identical(out$matched_rule_value, "/ads")
})

# ---------------------------------------------------------------------------
# Non-evaluated outcomes (SS10.2) -- distinct statuses, never a decision
# ---------------------------------------------------------------------------

test_that("unsupported_profile maps with its stable reason/error", {
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /")),
    urls = "https://example.test/x",
    product_tokens = "Googlebot"
  )
  expect_identical(out$native_evaluation_status, "unsupported_profile")
  expect_identical(out$matcher_status, "unsupported_profile")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$reason, "unsupported_profile")
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_unsupported_profile")
  expect_false(is.na(out$error_message))
  expect_identical(out$matched_line, NA_integer_)
  expect_identical(out$matched_rule_type, "unknown")
  expect_identical(out$matched_rule_value, NA_character_)
  expect_null(out$matched_rule_value_raw[[1]])
  expect_false(out$matcher_body_truncated)
})

test_that("invalid_request_target maps with its stable reason/error", {
  # A non-extractable URL yields an NA target -> empty target -> the core
  # rejects it as invalid_request_target (the adapter never pre-prevalidates).
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /")),
    urls = "not-a-url",
    product_tokens = "bingbot"
  )
  expect_identical(out$native_evaluation_status, "invalid_request_target")
  expect_identical(out$matcher_status, "invalid_request_target")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$reason, "invalid_request_target")
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_invalid_request_target")
  expect_identical(out$matched_rule_type, "unknown")
  expect_null(out$matched_rule_value_raw[[1]])
  expect_false(out$matcher_body_truncated)
})

test_that("request_target_limit_exceeded folds into the input-limit status", {
  # A request target over kMaxRequestTargetBytes (65,536) is an EvaluationStatus
  # limit; it folds into matcher_input_limit_exceeded but keeps its own reason.
  big_url <- paste0("https://example.test/", strrep("a", 70000))
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /x")),
    urls = big_url,
    product_tokens = "bingbot"
  )
  expect_identical(
    out$native_evaluation_status, "request_target_limit_exceeded"
  )
  expect_identical(out$matcher_status, "matcher_input_limit_exceeded")
  expect_identical(out$reason, "request_target_limit_exceeded")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_matcher_input_limit_exceeded")
  expect_null(out$matched_rule_value_raw[[1]])
})

test_that("each parse limit folds into input-limit with its own reason", {
  # A physical line over kMaxPhysicalLineBytes (65,536) trips a parse ceiling:
  # native_evaluation_status is NA and the parse limit lives in the reason.
  long_line <- sprintf("Disallow: /%s", strrep("a", 70000))
  out <- match_bing_v1(
    bodies = list(raw_body(paste0("User-agent: bingbot\n", long_line))),
    urls = "https://example.test/x",
    product_tokens = "bingbot"
  )
  expect_identical(out$native_parse_status, "line_length_limit_exceeded")
  expect_true(is.na(out$native_evaluation_status))
  expect_identical(out$matcher_status, "matcher_input_limit_exceeded")
  expect_identical(out$reason, "line_length_limit_exceeded")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_matcher_input_limit_exceeded")

  # The rule-count ceiling is a distinct parse-limit reason under the same
  # folded status.
  rules <- paste(rep("Disallow: /x", 16385), collapse = "\n")
  out2 <- match_bing_v1(
    bodies = list(raw_body(paste0("User-agent: bingbot\n", rules))),
    urls = "https://example.test/x",
    product_tokens = "bingbot"
  )
  expect_identical(out2$matcher_status, "matcher_input_limit_exceeded")
  expect_identical(out2$reason, "rule_limit_exceeded")
})

test_that("unsupported_profile precedes invalid_request_target", {
  # Both inputs bad: unsupported token AND an unextractable URL. The core fixes
  # precedence (unsupported first); the adapter does not re-prevalidate.
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /")),
    urls = "not-a-url",
    product_tokens = "Googlebot"
  )
  expect_identical(out$matcher_status, "unsupported_profile")
  expect_identical(out$reason, "unsupported_profile")
})

# ---------------------------------------------------------------------------
# matcher_input_bytes + parse-once counters
# ---------------------------------------------------------------------------

test_that("matcher_input_bytes is the complete supplied body length", {
  body <- raw_body("User-agent: bingbot\nDisallow: /private")
  out <- match_bing_v1(
    bodies = list(body),
    urls = "https://example.test/x",  # default_allow
    product_tokens = "bingbot"
  )
  expect_identical(out$matcher_input_bytes, length(body))
  # Non-evaluated rows still report the full body length.
  out2 <- match_bing_v1(
    bodies = list(body),
    urls = "https://example.test/x",
    product_tokens = "Googlebot"
  )
  expect_identical(out2$matcher_input_bytes, length(body))
})

test_that("a shared body is parsed exactly once across many rows", {
  body <- raw_body("User-agent: bingbot\nDisallow: /private")
  out <- match_bing_v1(
    bodies = list(body, body, body, body),
    urls = c(
      "https://example.test/private/a", "https://example.test/public",
      "https://example.test/private/b", "https://example.test/x"
    ),
    product_tokens = "bingbot"
  )
  expect_identical(attr(out, "n_parse_calls"), 1L)
  expect_identical(nrow(out), 4L)
  expect_identical(out$url_decision,
                   c("disallow", "allow", "disallow", "allow"))
})

test_that("distinct bodies each parse once and map per row", {
  out <- match_bing_v1(
    bodies = list(
      raw_body("User-agent: bingbot\nDisallow: /a"),
      raw_body("User-agent: bingbot\nDisallow: /b"),
      raw_body("User-agent: bingbot\nDisallow: /a")
    ),
    urls = c("https://example.test/a", "https://example.test/b",
             "https://example.test/b"),
    product_tokens = "bingbot"
  )
  expect_identical(attr(out, "n_parse_calls"), 2L)
  # body1 disallows /a, body2 disallows /b; row3 uses body1 so /b is allowed.
  expect_identical(out$url_decision, c("disallow", "disallow", "allow"))
})

test_that("a parse-limit body and a good body mix correctly in one batch", {
  long_line <- sprintf("Disallow: /%s", strrep("a", 70000))
  out <- match_bing_v1(
    bodies = list(
      raw_body(paste0("User-agent: bingbot\n", long_line)),
      raw_body("User-agent: bingbot\nDisallow: /private"),
      raw_body("User-agent: bingbot\nDisallow: /private")
    ),
    urls = c("https://example.test/x", "https://example.test/private/a",
             "https://example.test/ok"),
    product_tokens = "bingbot"
  )
  # Rows 2 and 3 share one body (deduped), so two distinct parses.
  expect_identical(attr(out, "n_parse_calls"), 2L)
  expect_identical(
    out$matcher_status,
    c("matcher_input_limit_exceeded", "evaluated", "evaluated")
  )
  expect_identical(out$url_decision, c(NA_character_, "disallow", "allow"))
})

# ---------------------------------------------------------------------------
# Exact bytes: matched rule bytes vs absent NULL (SS11)
# ---------------------------------------------------------------------------

test_that("matched rule bytes stay distinct from the absent NULL element", {
  out <- match_bing_v1(
    bodies = list(
      raw_body("User-agent: bingbot\nDisallow: /private"),  # matched -> bytes
      raw_body("User-agent: bingbot\nDisallow: /private")   # default -> NULL
    ),
    urls = c("https://example.test/private/x", "https://example.test/public"),
    product_tokens = "bingbot"
  )
  raws <- out$matched_rule_value_raw
  expect_identical(raws[[1]], charToRaw("/private"))
  expect_null(raws[[2]])

  # saveRDS/readRDS round-trip keeps the bytes present and the NULL absent.
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(out, path)
  restored <- readRDS(path)
  expect_identical(restored$matched_rule_value_raw[[1]], charToRaw("/private"))
  expect_null(restored$matched_rule_value_raw[[2]])
})

test_that("a multibyte UTF-8 rule value survives byte-exactly", {
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /café")),
    urls = "https://example.test/café/x",
    product_tokens = "bingbot"
  )
  expect_identical(out$reason, "rule_disallow")
  expect_identical(out$matched_rule_value, enc2utf8("/café"))
  expect_identical(out$matched_rule_value_raw[[1]],
                   charToRaw(enc2utf8("/café")))
})

test_that("embedded NUL/invalid-UTF-8 bodies are parsed, not truncated", {
  body <- c(raw_body("# c"), as.raw(0x00), as.raw(0xFE),
            raw_body("\nUser-agent: bingbot\nDisallow: /private"))
  out <- match_bing_v1(
    bodies = list(body),
    urls = "https://example.test/private/x",
    product_tokens = "bingbot"
  )
  expect_identical(out$native_evaluation_status, "evaluated")
  expect_identical(out$url_decision, "disallow")
  expect_identical(out$reason, "rule_disallow")
  expect_identical(out$matcher_input_bytes, length(body))
  expect_false(out$matcher_body_truncated)
})

# ---------------------------------------------------------------------------
# work_limit_exceeded mapping (SS10.2). A live work-budget overflow needs a
# pathological body/target; the mapping itself is covered deterministically by
# feeding the adapter a synthetic native work_limit_exceeded row.
# ---------------------------------------------------------------------------

test_that("work_limit_exceeded maps to its own status and reason", {
  fake_native <- function(bodies, body_index, product_tokens, targets) {
    list(
      native_parse_status = "parsed",
      native_evaluation_status = "work_limit_exceeded",
      url_decision = NA,
      decision_source = NA_character_,
      matched_line = NA_integer_,
      matched_rule_type = "unknown",
      matched_rule_value = NA_character_,
      matched_rule_value_raw = list(NULL),
      matcher_input_bytes = 12L,
      matcher_body_truncated = FALSE,
      matcher_request_target_raw = list(raw(0)),
      n_parse_calls = 1L
    )
  }
  testthat::local_mocked_bindings(
    bing_evaluate_batch = fake_native, .package = "robotstxtr"
  )
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /")),
    urls = "https://example.test/x",
    product_tokens = "bingbot"
  )
  expect_identical(out$matcher_status, "matcher_work_limit_exceeded")
  expect_identical(out$reason, "work_limit_exceeded")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$error_stage, "input")
  expect_identical(out$error_class, "robots_matcher_work_limit_exceeded")
  expect_null(out$matched_rule_value_raw[[1]])
})

test_that("an empty batch returns the zero-row shape", {
  out <- match_bing_v1(list(), character(0), character(0))
  expect_identical(nrow(out), 0L)
  expect_identical(attr(out, "n_parse_calls"), 0L)
  expect_type(out$matched_rule_value_raw, "list")
})

test_that("a non-list `bodies` is a classed call error", {
  # The adapter is batch-shaped: a bare raw vector is a caller mistake, never
  # silently promoted to a one-row batch.
  expect_error(
    match_bing_v1(
      raw_body("User-agent: bingbot\nDisallow: /"),
      "https://example.test/x", "bingbot"
    ),
    class = "robotstxtr_invalid_bing_bodies"
  )
  expect_error(
    match_bing_v1("User-agent: bingbot", "https://example.test/x", "bingbot"),
    class = "robotstxtr_invalid_bing_bodies"
  )
  expect_error(
    match_bing_v1(NULL, "https://example.test/x", "bingbot"),
    "`bodies` must be a list of raw vectors.",
    fixed = TRUE
  )
})

test_that("an empty or missing URL becomes an empty request target", {
  expect_identical(bing_target_bytes_v1(""), raw(0))
  expect_identical(bing_target_bytes_v1(NA_character_), raw(0))
  # A well-formed URL still yields its extracted target bytes.
  expect_identical(
    bing_target_bytes_v1("https://example.test/a?b=c"), charToRaw("/a?b=c")
  )

  # End to end: the empty target is rejected by the core, not pre-mapped here.
  out <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /")),
    urls = "",
    product_tokens = "bingbot"
  )
  expect_identical(out$native_evaluation_status, "invalid_request_target")
  expect_identical(out$matcher_status, "invalid_request_target")
  expect_identical(out$reason, "invalid_request_target")
  expect_identical(out$url_decision, NA_character_)
  expect_identical(out$error_class, "robots_invalid_request_target")

  out_na <- match_bing_v1(
    bodies = list(raw_body("User-agent: bingbot\nDisallow: /")),
    urls = NA_character_,
    product_tokens = "bingbot"
  )
  expect_identical(out_na$matcher_status, "invalid_request_target")
})
