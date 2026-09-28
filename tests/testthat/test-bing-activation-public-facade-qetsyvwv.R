# Public-facade conformance for the activated Bing backend (ROBO-qetsyvwv, BI5).
#
# Drives the PUBLIC facade (robots_evaluate_text_v1 / robots_evaluate_url_v1)
# matcher_backend = "bing") end-to-end, asserting the published SS10/SS11
# decision/reason/rule metadata and the matched_rule_value_raw list column for
# both supplied and fetched tracer bullets and for every non-evaluated path. It
# also proves the v2 contract identity is published (robots_engine_contract_v2),
# reconciles the single-sourced Bing identity against the compiled ground truth,
# exercises the batch dispatch (rejecting row-dispatch), and proves Google and
# Yandex identities/behavior are byte-unchanged (SS15/SS16.5).

# ---------------------------------------------------------------------------
# v2 contract identity surface (no native binding required)
# ---------------------------------------------------------------------------

test_that("the v2 contract publishes the activation identity and status set", {
  v2 <- robots_engine_contract_v2()
  expect_s3_class(v2, "robots_engine_contract_v2")
  expect_identical(v2$contract_id, "robotstxtr.engine-aware/v2")
  expect_identical(v2$schema_revision, "2026-08-25.1")
  expect_identical(
    v2$matcher_status_set, robotstxtr:::engine_matcher_status_set_v2()
  )
  expect_identical(v2$matcher_availability[["bing"]], "available")
  expect_identical(
    v2$matcher_revisions[["bing"]], robotstxtr:::bing_matcher_revision_v1()
  )
  expect_identical(
    v2$matcher_identity$bing, robotstxtr:::bing_matcher_identity_v1()
  )
})

test_that("the composed Bing revision equals the frozen manifest form", {
  expect_identical(
    robotstxtr:::bing_matcher_revision_v1(),
    paste0(
      "robotstxtbing/0.1.0+payload.",
      "c82855d0756c748cc4770246a19282323cdfa331",
      ";contract=robotstxtbing-v2;contract-rev=0.1.0;parser=0.1.0",
      ";bingbot=bingbot-2026-07-23.1;adidxbot=adidxbot-2026-07-23.1",
      ";manifest=",
      "5e79ee5dcb1a22b73b5fa0f86766be529cf111fd5530d07b53fe1d6b7050a858"
    )
  )
})

test_that("v1 accessor identity and schema stay byte-unchanged (SS16.5)", {
  # Activating Bing must not touch the v1 facade's own contract_id or schema,
  # nor the Google/Yandex identities the v1 accessor publishes.
  v1 <- robots_engine_contract_v1()
  expect_identical(v1$contract_id, "robotstxtr.engine-aware/v1")
  expect_identical(v1$schema_revision, "2026-08-25.1")
  expect_match(v1$matcher_revisions[["google"]], "22b355ff")
  expect_identical(
    v1$matcher_revisions[["yandex"]],
    robotstxtr:::yandex_matcher_revision_v1()
  )
  expect_identical(
    v1$matcher_identity$yandex, robotstxtr:::yandex_matcher_identity_v1()
  )
})

# ---------------------------------------------------------------------------
# Everything below drives the native engine; skip on a pure-R install.
# ---------------------------------------------------------------------------

skip_if_not(
  is.function(tryCatch(robotstxtr_bing_eval_batch_, error = function(e) NULL)),
  "native bing binding not built (pure-R install)"
)

bing_text <- function(body, url, token, ruleset = "assumed_rfc9309") {
  robots_evaluate_text_v1(
    robots_txt = body, url = url, robots_product_token = token,
    robots_policy_ruleset = ruleset, matcher_backend = "bing"
  )
}

test_that("the single-sourced identity reconciles with the compiled truth", {
  id <- robotstxtr:::bing_matcher_identity_v1()
  ci <- robotstxtr:::bing_native_contract_info()
  expect_identical(id$library_version, ci[["library_version"]])
  expect_identical(id$contract_id, ci[["contract_id"]])
  expect_identical(id$contract_revision, ci[["contract_revision"]])
  expect_identical(id$parser_revision, ci[["parser_revision"]])
  expect_identical(
    id$bingbot_profile_revision, ci[["bingbot_profile_revision"]]
  )
  expect_identical(
    id$adidxbot_profile_revision, ci[["adidxbot_profile_revision"]]
  )
  expect_identical(
    id$release_manifest_sha256, ci[["release_manifest_sha256"]]
  )
})

# ---------------------------------------------------------------------------
# Supplied tracer bullets: evaluated decisions through the public facade
# ---------------------------------------------------------------------------

test_that("a supplied body yields evaluated Bing decisions with metadata", {
  body <- "User-agent: bingbot\nAllow: /pub\nDisallow: /"
  x <- bing_text(
    body,
    c("https://example.test/pub", "https://example.test/private"),
    "bingbot"
  )
  r <- x$results
  expect_identical(r$matcher_backend, rep("bing", 2L))
  expect_identical(r$matcher_availability, rep("available", 2L))
  expect_identical(r$matcher_status, rep("evaluated", 2L))
  expect_identical(r$url_decision, c("allow", "disallow"))
  expect_identical(r$reason, c("rule_allow", "rule_disallow"))
  expect_identical(r$matched_rule_value, c("/pub", "/"))
  expect_identical(r$matched_rule_value_raw[[1L]], charToRaw("/pub"))
  expect_identical(r$matched_rule_value_raw[[2L]], charToRaw("/"))
})

test_that("default_allow publishes an absent (NULL) raw element", {
  x <- bing_text(
    "User-agent: bingbot\nDisallow: /private",
    "https://example.test/public", "bingbot"
  )
  expect_identical(x$results$matcher_status, "evaluated")
  expect_identical(x$results$url_decision, "allow")
  expect_identical(x$results$reason, "default_allow")
  expect_identical(x$results$matched_rule_type, "none")
  expect_null(x$results$matched_rule_value_raw[[1L]])
})

test_that("one facade call over many rows sharing a body maps per row", {
  x <- bing_text(
    "User-agent: bingbot\nDisallow: /private",
    c("https://example.test/private/x", "https://example.test/public"),
    "bingbot"
  )
  expect_identical(x$results$matcher_status, rep("evaluated", 2L))
  expect_identical(x$results$url_decision, c("disallow", "allow"))
  expect_identical(x$results$reason, c("rule_disallow", "default_allow"))
  expect_identical(x$results$matched_rule_value_raw[[1]], charToRaw("/private"))
  expect_null(x$results$matched_rule_value_raw[[2L]])
})

# ---------------------------------------------------------------------------
# Fetched tracer bullet: a 200 body under a use_rules ruleset + Bing matcher
# ---------------------------------------------------------------------------

test_that("a fetched 200 body is evaluated through the Bing matcher", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L, url = req$url,
      body = charToRaw("user-agent: bingbot\ndisallow: /private")
    )
  })
  x <- robots_evaluate_url_v1(
    "http://example.test/private", "bingbot", "google", "bing"
  )
  expect_identical(x$results$policy_action, "use_rules")
  expect_identical(x$results$matcher_backend, "bing")
  expect_identical(x$results$matcher_status, "evaluated")
  expect_identical(x$results$url_decision, "disallow")
  expect_identical(x$results$reason, "rule_disallow")
  expect_identical(x$results$matched_rule_value_raw[[1]], charToRaw("/private"))
})

# ---------------------------------------------------------------------------
# Every non-evaluated path through the public facade (SS10.2)
# ---------------------------------------------------------------------------

test_that("an unsupported token maps to unsupported_profile with no decision", {
  x <- bing_text(
    "User-agent: bingbot\nDisallow: /", "https://example.test/x", "Googlebot"
  )
  expect_identical(x$results$matcher_status, "unsupported_profile")
  expect_identical(x$results$url_decision, NA_character_)
  expect_identical(x$results$reason, "unsupported_profile")
  expect_identical(x$results$error_stage, "input")
  expect_identical(x$results$error_class, "robots_unsupported_profile")
  expect_null(x$results$matched_rule_value_raw[[1L]])
})

test_that("a non-extractable URL maps to invalid_request_target", {
  x <- bing_text(
    "User-agent: bingbot\nDisallow: /", "not-a-url", "bingbot"
  )
  expect_identical(x$results$matcher_status, "invalid_request_target")
  expect_identical(x$results$url_decision, NA_character_)
  expect_identical(x$results$reason, "invalid_request_target")
  expect_identical(x$results$error_class, "robots_invalid_request_target")
})

test_that("an over-limit request target folds into the input-limit status", {
  big_url <- paste0("https://example.test/", strrep("a", 70000))
  x <- bing_text("User-agent: bingbot\nDisallow: /x", big_url, "bingbot")
  expect_identical(x$results$matcher_status, "matcher_input_limit_exceeded")
  expect_identical(x$results$reason, "request_target_limit_exceeded")
  expect_identical(x$results$url_decision, NA_character_)
  expect_identical(x$results$error_class, "robots_matcher_input_limit_exceeded")
})

test_that("a parse ceiling folds into the input-limit status with its reason", {
  long_line <- sprintf("Disallow: /%s", strrep("a", 70000))
  x <- bing_text(
    paste0("User-agent: bingbot\n", long_line),
    "https://example.test/x", "bingbot"
  )
  expect_identical(x$results$matcher_status, "matcher_input_limit_exceeded")
  expect_identical(x$results$reason, "line_length_limit_exceeded")
  expect_identical(x$results$url_decision, NA_character_)
  expect_identical(x$results$error_class, "robots_matcher_input_limit_exceeded")
})

test_that("every emitted Bing matcher_status is a member of the v2 set", {
  x <- bing_text(
    "User-agent: bingbot\nDisallow: /private",
    c("https://example.test/private", "https://example.test/pub", "not-a-url"),
    c("bingbot", "Googlebot", "bingbot")
  )
  expect_true(all(
    x$results$matcher_status %in% robotstxtr:::engine_matcher_status_set_v2()
  ))
  expect_identical(
    x$results$matcher_status,
    c("evaluated", "unsupported_profile", "invalid_request_target")
  )
})

# ---------------------------------------------------------------------------
# Missing/empty token stays facade-invalid: never fetch or invoke a backend
# ---------------------------------------------------------------------------

test_that("a missing or empty product token is facade-invalid, no backend", {
  x <- bing_text(
    "User-agent: bingbot\nDisallow: /private",
    "https://example.test/private", ""
  )
  expect_identical(x$results$matcher_status, "not_evaluated")
  expect_identical(x$results$reason, "input_invalid")
  expect_identical(x$results$error_class, "robots_invalid_product_token")
  expect_identical(x$results$url_decision, NA_character_)
})

# ---------------------------------------------------------------------------
# Dispatch: Bing is batch-shaped and must never be row-dispatched or fall back
# ---------------------------------------------------------------------------

test_that("Bing rejects per-row dispatch as an internal invariant violation", {
  registry <- robotstxtr:::engine_matcher_registry_v1()
  expect_error(
    robotstxtr:::match_backend_v1(
      "bing", raw(0), "https://example.test/", "bingbot", registry
    ),
    class = "robotstxtr_matcher_backend_not_row_dispatchable"
  )
})

# ---------------------------------------------------------------------------
# Cross-engine non-regression: Google and Yandex byte-unchanged (SS15/SS16.5)
# ---------------------------------------------------------------------------

test_that("a mixed Google+Bing call keeps each row on its own backend", {
  body <- "User-agent: *\nDisallow: /private"
  url <- rep("https://example.test/private/x", 2L)
  x <- robots_evaluate_text_v1(
    robots_txt = body, url = url,
    robots_product_token = c("Googlebot", "bingbot"),
    robots_policy_ruleset = c("google", "assumed_rfc9309"),
    matcher_backend = c("google", "bing")
  )
  expect_identical(x$results$matcher_backend, c("google", "bing"))
  expect_identical(x$results$matcher_status, rep("evaluated", 2L))
  expect_identical(x$results$url_decision, c("disallow", "disallow"))

  # The Google row equals a Google-only single-row evaluation exactly, and never
  # gains a raw list element -- the Bing row does not perturb it.
  g_only <- robots_evaluate_text_v1(
    robots_txt = body, url = url[[1L]], robots_product_token = "Googlebot",
    robots_policy_ruleset = "google", matcher_backend = "google"
  )
  expect_identical(
    x$results$url_decision[[1L]], g_only$results$url_decision[[1L]]
  )
  expect_identical(x$results$reason[[1L]], g_only$results$reason[[1L]])
  expect_null(x$results$matched_rule_value_raw[[1L]])
  expect_identical(x$results$matched_rule_value_raw[[2]], charToRaw("/private"))
})

test_that("a mixed Yandex+Bing call keeps both backends independent", {
  skip_if_not(
    is.function(tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)),
    "yandex native binding not built"
  )
  body <- "User-agent: *\nDisallow: /private"
  x <- robots_evaluate_text_v1(
    robots_txt = body,
    url = rep("https://example.test/private/x", 2L),
    robots_product_token = c("Yandex", "bingbot"),
    robots_policy_ruleset = c("yandex", "assumed_rfc9309"),
    matcher_backend = c("yandex", "bing")
  )
  expect_identical(x$results$matcher_backend, c("yandex", "bing"))
  expect_identical(x$results$matcher_status, rep("evaluated", 2L))
  expect_identical(x$results$url_decision, c("disallow", "disallow"))
  expect_identical(x$results$reason, c("rule_disallow", "rule_disallow"))
  # Each row carries its own owning-rule bytes from its own engine.
  expect_identical(x$results$matched_rule_value_raw[[1]], charToRaw("/private"))
  expect_identical(x$results$matched_rule_value_raw[[2]], charToRaw("/private"))
})
