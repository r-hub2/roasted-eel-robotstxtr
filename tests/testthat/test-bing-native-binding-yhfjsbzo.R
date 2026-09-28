# Native robotstxtbing batch binding + request-target extractor (ROBO-yhfjsbzo,
# BI4). Exercises the hidden native routines directly through their internal R
# wrappers. These checks prove parse-once batching, byte-preserving raw
# bodies/targets, owning lifetimes, every native ParseStatus/EvaluationStatus
# with its DecisionSource, the exact empty/raw/non-UTF-8 rule-value
# representation, the absolute-URL -> origin-form target byte matrix, and the
# compile-time contract identity baked in at BI3. Bing stays
# capability_unavailable throughout: nothing here touches the public facade.

skip_if_not(is.function(
  tryCatch(robotstxtr_bing_eval_batch_,
           error = function(e) NULL)),
  "native bing binding not built (pure-R install)")

# Helper: run one row against one body, returning a per-row list of scalars.
eval_one <- function(body, product_token, target) {
  body_raw <- if (is.raw(body)) body else charToRaw(body)
  target_raw <- if (is.raw(target)) target else charToRaw(target)
  res <- bing_evaluate_batch(
    bodies = list(body_raw),
    body_index = 1L,
    product_tokens = product_token,
    targets = list(target_raw)
  )
  lapply(res, function(col) col[[1]])
}

# ---------------------------------------------------------------------------
# Parse-once batching
# ---------------------------------------------------------------------------

test_that("a shared body parses exactly once across many rows", {
  body <- charToRaw("User-agent: bingbot\nDisallow: /private")
  res <- bing_evaluate_batch(
    bodies = list(body),
    body_index = c(1L, 1L, 1L, 1L),
    product_tokens = rep("bingbot", 4),
    targets = list(charToRaw("/private/a"), charToRaw("/public"),
                   charToRaw("/private/b"), charToRaw("/x"))
  )
  expect_identical(res$n_parse_calls, 1L)
  expect_length(res$url_decision, 4L)
  # Rows 1 and 3 hit the disallow; rows 2 and 4 are default-allowed.
  expect_identical(res$url_decision, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("distinct bodies each parse once", {
  res <- bing_evaluate_batch(
    bodies = list(charToRaw("User-agent: bingbot\nDisallow: /a"),
                  charToRaw("User-agent: bingbot\nDisallow: /b")),
    body_index = c(1L, 2L, 1L),
    product_tokens = rep("bingbot", 3),
    targets = list(charToRaw("/a"), charToRaw("/b"), charToRaw("/b"))
  )
  expect_identical(res$n_parse_calls, 2L)
  # body1 disallows /a, body2 disallows /b; row3 uses body1 so /b is allowed.
  expect_identical(res$url_decision, c(FALSE, FALSE, TRUE))
  # Per-body input bytes replicate onto rows by their body_index.
  expect_identical(res$matcher_input_bytes[c(1L, 3L)],
                   rep(nchar("User-agent: bingbot\nDisallow: /a"), 2L))
})

# ---------------------------------------------------------------------------
# Evaluated outcomes (spec section 10.1)
# ---------------------------------------------------------------------------

test_that("a disallow rule maps to a disallow decision with rule metadata", {
  r <- eval_one("User-agent: bingbot\nDisallow: /private", "bingbot",
                "/private/x")
  expect_identical(r$native_parse_status, "parsed")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_false(r$url_decision)
  expect_identical(r$decision_source, "rule")
  expect_identical(r$matched_line, 2L)
  expect_identical(r$matched_rule_type, "disallow")
  expect_identical(r$matched_rule_value, "/private")
  expect_identical(r$matched_rule_value_raw, charToRaw("/private"))
})

test_that("an allow rule maps to an allow decision", {
  r <- eval_one("User-agent: bingbot\nAllow: /pub\nDisallow: /", "bingbot",
                "/pub")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_true(r$url_decision)
  expect_identical(r$decision_source, "rule")
  expect_identical(r$matched_rule_type, "allow")
  expect_identical(r$matched_rule_value, "/pub")
})

test_that("default_allow uses the absent-rule representation", {
  r <- eval_one("User-agent: bingbot\nDisallow: /private", "bingbot", "/public")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_true(r$url_decision)
  expect_identical(r$decision_source, "default_allow")
  expect_identical(r$matched_line, NA_integer_)
  expect_identical(r$matched_rule_type, "none")
  expect_identical(r$matched_rule_value, NA_character_)
  expect_null(r$matched_rule_value_raw)
})

test_that("an empty Disallow is inert (default_allow) per shipped profile", {
  # The shipped Bing profile treats an empty Disallow as inert rather than as an
  # effective-empty rule match, so the native decision is default_allow (spec
  # section 10.1: the adapter reflects the shipped decision, never fabricates
  # an effective-empty one). The binding carries this faithfully.
  r <- eval_one("User-agent: bingbot\nDisallow:", "bingbot", "/anything")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_true(r$url_decision)
  expect_identical(r$decision_source, "default_allow")
  expect_identical(r$matched_rule_type, "none")
  expect_null(r$matched_rule_value_raw)
})

test_that("the adidxbot profile is supported and evaluates its own rules", {
  r <- eval_one("User-agent: adidxbot\nDisallow: /ads", "adidxbot", "/ads/x")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_false(r$url_decision)
  expect_identical(r$decision_source, "rule")
  expect_identical(r$matched_rule_value, "/ads")
})

# ---------------------------------------------------------------------------
# Non-evaluated outcomes (spec section 10.2) — never a decision
# ---------------------------------------------------------------------------

test_that("an unsupported product token yields unsupported_profile", {
  r <- eval_one("User-agent: bingbot\nDisallow: /", "Googlebot", "/x")
  expect_identical(r$native_parse_status, "parsed")
  expect_identical(r$native_evaluation_status, "unsupported_profile")
  expect_identical(r$url_decision, NA)
  expect_identical(r$decision_source, NA_character_)
  expect_identical(r$matched_line, NA_integer_)
  expect_identical(r$matched_rule_type, "unknown")
  expect_identical(r$matched_rule_value, NA_character_)
  expect_null(r$matched_rule_value_raw)
})

test_that("a non-slash request target yields invalid_request_target", {
  r <- eval_one("User-agent: bingbot\nDisallow: /", "bingbot", "no-slash")
  expect_identical(r$native_evaluation_status, "invalid_request_target")
  expect_identical(r$url_decision, NA)
  expect_identical(r$decision_source, NA_character_)
  expect_identical(r$matched_rule_type, "unknown")
  expect_null(r$matched_rule_value_raw)
})

test_that("an over-limit request target yields request_target_limit_exceeded", {
  # A request target longer than kMaxRequestTargetBytes (65,536) is rejected by
  # the core with no decision (an EvaluationStatus limit, not a parse limit).
  big_target <- sprintf("/%s", strrep("a", 70000))
  r <- eval_one("User-agent: bingbot\nDisallow: /x", "bingbot", big_target)
  expect_identical(r$native_parse_status, "parsed")
  expect_identical(r$native_evaluation_status, "request_target_limit_exceeded")
  expect_identical(r$url_decision, NA)
  expect_null(r$matched_rule_value_raw)
})

test_that("a parse ceiling yields no Policy and no decision for every row", {
  # A single physical line longer than kMaxPhysicalLineBytes (65,536) trips a
  # parse ceiling; there is no Policy, so every row on that body is a
  # non-decision carrying the parse status (spec section 8).
  long_line <- sprintf("Disallow: /%s", strrep("a", 70000))
  r <- eval_one(paste0("User-agent: bingbot\n", long_line), "bingbot", "/x")
  expect_identical(r$native_parse_status, "line_length_limit_exceeded")
  expect_identical(r$native_evaluation_status, NA_character_)
  expect_identical(r$url_decision, NA)
  expect_identical(r$decision_source, NA_character_)
  expect_identical(r$matched_rule_type, "unknown")
  expect_null(r$matched_rule_value_raw)
})

test_that("too many access rules trips the rule-limit parse ceiling", {
  rules <- paste(rep("Disallow: /x", 16385), collapse = "\n")
  r <- eval_one(paste0("User-agent: bingbot\n", rules), "bingbot", "/x")
  expect_identical(r$native_parse_status, "rule_limit_exceeded")
  expect_identical(r$native_evaluation_status, NA_character_)
  expect_identical(r$url_decision, NA)
})

test_that("a parse-limit body and a good body mix correctly in one batch", {
  long_line <- sprintf("Disallow: /%s", strrep("a", 70000))
  res <- bing_evaluate_batch(
    bodies = list(charToRaw(paste0("User-agent: bingbot\n", long_line)),
                  charToRaw("User-agent: bingbot\nDisallow: /private")),
    body_index = c(1L, 2L, 2L),
    product_tokens = rep("bingbot", 3),
    targets = list(charToRaw("/x"), charToRaw("/private/a"), charToRaw("/ok"))
  )
  expect_identical(res$n_parse_calls, 2L)
  expect_identical(res$native_parse_status,
                   c("line_length_limit_exceeded", "parsed", "parsed"))
  expect_identical(res$url_decision, c(NA, FALSE, TRUE))
})

# ---------------------------------------------------------------------------
# Byte-preserving bodies and rule values (spec sections 8, 11)
# ---------------------------------------------------------------------------

test_that("a non-UTF-8 rule value keeps exact bytes but NA text", {
  # Disallow value with a lone 0xFF (invalid UTF-8), matched against the same
  # bytes so the rule is the winner.
  body <- c(charToRaw("User-agent: bingbot\nDisallow: /"), as.raw(0xFF))
  target <- c(charToRaw("/"), as.raw(0xFF))
  r <- eval_one(body, "bingbot", target)
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_identical(r$decision_source, "rule")
  expect_false(r$url_decision)
  expect_identical(r$matched_rule_value, NA_character_)  # not valid UTF-8
  expect_identical(r$matched_rule_value_raw, c(charToRaw("/"), as.raw(0xFF)))
})

test_that("embedded NUL and invalid UTF-8 bodies are parsed, not truncated", {
  # A NUL byte then an invalid byte inside a comment line, followed by a real
  # rule. If the body were truncated at NUL the Disallow would be lost.
  body <- c(charToRaw("# c"), as.raw(0x00), as.raw(0xFE),
            charToRaw("\nUser-agent: bingbot\nDisallow: /private"))
  r <- eval_one(body, "bingbot", "/private/x")
  expect_identical(r$native_evaluation_status, "evaluated")
  expect_false(r$url_decision)
  expect_identical(r$decision_source, "rule")
  # Whole body length reported; nothing truncated.
  expect_identical(r$matcher_input_bytes, length(body))
  expect_false(r$matcher_body_truncated)
})

test_that("matcher_request_target_raw carries the exact target bytes", {
  target <- c(charToRaw("/a"), as.raw(0x00), charToRaw("b"))
  r <- eval_one("User-agent: bingbot\nDisallow: /z", "bingbot", target)
  expect_identical(r$matcher_request_target_raw, target)
})

test_that("raw(0) present-empty stays distinct from NULL absent-rule", {
  # default_allow carries a NULL raw element (absent rule)...
  absent <- eval_one("User-agent: bingbot\nDisallow: /private", "bingbot",
                     "/public")
  expect_null(absent$matched_rule_value_raw)
  # ...while a matched rule always carries a present raw vector. Confirm the two
  # representations are never conflated in one batch.
  res <- bing_evaluate_batch(
    bodies = list(charToRaw("User-agent: bingbot\nDisallow: /private")),
    body_index = c(1L, 1L),
    product_tokens = rep("bingbot", 2),
    targets = list(charToRaw("/private/x"), charToRaw("/public"))
  )
  expect_false(is.null(res$matched_rule_value_raw[[1L]]))   # matched rule
  expect_identical(res$matched_rule_value_raw[[1L]], charToRaw("/private"))
  expect_null(res$matched_rule_value_raw[[2L]])             # default_allow
})

# ---------------------------------------------------------------------------
# Owning lifetimes
# ---------------------------------------------------------------------------

test_that("returned values survive garbage collection (owning, no views)", {
  res <- bing_evaluate_batch(
    bodies = list(charToRaw("User-agent: bingbot\nDisallow: /private")),
    body_index = 1L,
    product_tokens = "bingbot",
    targets = list(charToRaw("/private/x"))
  )
  gc()
  gc()
  expect_identical(res$decision_source, "rule")
  expect_identical(res$matched_rule_value_raw[[1]], charToRaw("/private"))
})

# ---------------------------------------------------------------------------
# Absolute URL -> origin-form request-target byte matrix (spec section 9)
# ---------------------------------------------------------------------------

test_that("the request-target extractor freezes the byte matrix", {
  extract <- function(u) bing_extract_request_target(u)
  expect_identical(extract("https://example.test"), "/")
  expect_identical(extract("https://example.test?"), "/?")
  expect_identical(extract("https://example.test?x=1"), "/?x=1")
  # Duplicate slashes and percent-escape case preserved; query kept verbatim.
  expect_identical(extract("https://example.test/a//b?x=%2f"), "/a//b?x=%2f")
  # Dot segments preserved (not resolved); fragment excluded.
  expect_identical(extract("https://example.test/a/../b#frag"), "/a/../b")
  # http scheme works the same.
  expect_identical(extract("http://example.test/p"), "/p")
})

test_that("the extractor preserves literal Unicode bytes without re-encoding", {
  url <- "https://example.test/café?q=✓"  # /café?q=✓
  got <- bing_extract_request_target(url)
  expect_identical(charToRaw(got),
                   charToRaw("/café?q=✓"))
  expect_true(validUTF8(got))
})

test_that("a URL with no scheme boundary is a lexical failure (NA)", {
  expect_identical(bing_extract_request_target("not-a-url"), NA_character_)
})

# ---------------------------------------------------------------------------
# Compile-time contract identity (spec sections 5/14) — proves the BI3 macros
# ---------------------------------------------------------------------------

test_that("the compiled contract identity carries the frozen release values", {
  ci <- bing_native_contract_info()
  expect_identical(ci[["library_version"]], "0.1.0")
  expect_identical(ci[["contract_id"]], "robotstxtbing-v2")
  expect_identical(ci[["contract_revision"]], "0.1.0")
  expect_identical(ci[["parser_revision"]], "0.1.0")
  expect_identical(ci[["bingbot_profile_revision"]], "bingbot-2026-07-23.1")
  expect_identical(ci[["adidxbot_profile_revision"]], "adidxbot-2026-07-23.1")
  expect_identical(
    ci[["release_manifest_sha256"]],
    "5e79ee5dcb1a22b73b5fa0f86766be529cf111fd5530d07b53fe1d6b7050a858"
  )
})

# ---------------------------------------------------------------------------
# Registration + availability invariant: native routines exist, Bing stays
# capability_unavailable (no facade/registry change at BI4)
# ---------------------------------------------------------------------------

test_that("the bing native routines are registered", {
  dll <- getLoadedDLLs()[["robotstxtr"]]
  skip_if(is.null(dll), "robotstxtr DLL not loaded (pure-R install)")
  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  names_registered <- vapply(routines, function(r) r$name, character(1))
  expect_true(all(c(
    "_robotstxtr_robotstxtr_bing_eval_batch_",
    "_robotstxtr_robotstxtr_bing_extract_request_target_",
    "_robotstxtr_robotstxtr_bing_contract_info_"
  ) %in% names_registered))
})

test_that("Bing engine availability is available after BI5 activation", {
  expect_identical(
    engine_matcher_availability_v1()[["bing"]],
    "available"
  )
})
