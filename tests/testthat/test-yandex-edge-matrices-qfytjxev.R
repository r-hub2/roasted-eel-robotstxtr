# Non-corpus edge matrices for atomic public Yandex activation (ROBO-qfytjxev,
# YI6c).
#
# Freezes the NON-corpus edges the activation slice depends on: the §12 size
# policy (fetched-only >500000 downgrade, boundary, supplied-skips-downgrade,
# limit constants, end-to-end no-matcher), the v1 facade product-token
# validation (never fetches), the hidden adapter's mixed-status / invalid-target
# / empty / non-list edges plus case-insensitive supported tokens, the §9
# request-target extractor's bytes-marked boundary error, and the data-only
# dormancy proof.
#
# This unit freezes those edges. Post-activation (YI5) the Yandex backend is
# available, the engine-aware v1 schema is current, and the batch adapter is
# registered;
# the facade and data-only assertions below track that active state.
# Native-binding-dependent edges skip on a pure-R install, matching the sibling
# conformance test.

# ---------------------------------------------------------------------------
# Self-contained inline helpers (uniquely prefixed yc_ to avoid collision with
# the engine-contract test's globals of the same shape).
# ---------------------------------------------------------------------------

yc_recorder <- function() {
  out <- new.env(parent = emptyenv())
  out$urls <- character()
  out$agents <- character()
  out
}

yc_response <- function(status, body = "") {
  force(status)
  force(body)
  function(req) {
    httr2::response(
      status_code = status, url = req$url, headers = list(),
      body = charToRaw(body)
    )
  }
}

yc_mock_router <- function(routes, recorder = NULL) {
  function(req) {
    if (!is.null(recorder)) {
      recorder$urls <- c(recorder$urls, req$url)
      recorder$agents <- c(recorder$agents, req$options$useragent)
    }
    handler <- routes[[req$url]]
    if (is.null(handler)) {
      stop(sprintf("unexpected request URL in mock: %s", req$url))
    }
    handler(req)
  }
}

# A fetched-shaped evidence row carrying only the fields resolve_policy_v1()
# reads: it is NOT supplied/local, so the §12 size check applies to it.
yc_fetched_ev <- function(stored_bytes, status = 200L) {
  list(
    evidence_status = "usable_body",
    source_kind = "fetched",
    final_http_status = status,
    stored_bytes = stored_bytes
  )
}

# A supplied-shaped evidence row: resolve_policy_v1() short-circuits it to
# use_rules BEFORE the size check, so it never downgrades.
yc_supplied_ev <- function(stored_bytes) {
  list(
    evidence_status = "usable_body",
    source_kind = "supplied",
    final_http_status = NA_integer_,
    stored_bytes = stored_bytes
  )
}

# Reusable activation assertion: the adapter is registered, the schema bumped.
yc_expect_active <- function() {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]], "available"
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
  expect_type(engine_matcher_registry_v1()$yandex$callable, "closure")
}

yc_skip_without_native <- function() {
  skip_if_not(
    is.function(
      tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)
    ) &&
      is.function(tryCatch(robotstxtr_extract_request_target_,
                           error = function(e) NULL)),
    "native binding not built (pure-R install)"
  )
}

yc_raw_body <- function(text) charToRaw(text)

# ---------------------------------------------------------------------------
# A. Size policy (§12): fetched-only >500000-byte downgrade to allow_all.
# ---------------------------------------------------------------------------

test_that("A1 a fetched >500000-byte yandex body downgrades to allow_all", {
  policy <- resolve_policy_v1(
    yc_fetched_ev(500001L), "yandex", engine_policy_table_v1()
  )
  expect_identical(policy$policy_status, "evaluated")
  expect_identical(policy$policy_action, "allow_all")
  expect_identical(policy$policy_reason, "yandex_file_over_500000_bytes")
})

test_that("A2 the yandex size downgrade turns on strictly above 500000", {
  table <- engine_policy_table_v1()
  at_limit <- resolve_policy_v1(yc_fetched_ev(500000L), "yandex", table)
  over_limit <- resolve_policy_v1(yc_fetched_ev(500001L), "yandex", table)
  expect_identical(at_limit$policy_action, "use_rules")
  expect_identical(over_limit$policy_action, "allow_all")
})

test_that("A3 a supplied >500000-byte body keeps use_rules, never downgrades", {
  policy <- resolve_policy_v1(
    yc_supplied_ev(600000L), "yandex", engine_policy_table_v1()
  )
  expect_identical(policy$policy_action, "use_rules")
  expect_identical(policy$policy_reason, "supplied_body_use_rules")
})

test_that("A4 the policy body-limit constants are pinned per ruleset", {
  expect_identical(policy_limit_v1("yandex"), 500000L)
  expect_identical(policy_limit_v1("google"), 524288L)
})

test_that("A5 an over-limit yandex fetch allows all without any matcher", {
  skip_if_not_installed("httr2")
  calls <- new.env(parent = emptyenv())
  calls$google <- 0L
  fake_google <- function(body, url, product_token) {
    calls$google <- calls$google + 1L
    list(
      url_decision = "disallow", reason = "rule_disallow",
      matched_line = 1L, matched_rule_type = "disallow",
      matched_rule_value = "/", matcher_input_bytes = length(body),
      matcher_body_truncated = FALSE
    )
  }
  testthat::local_mocked_bindings(
    match_google_v1 = fake_google, .package = "robotstxtr"
  )
  big_body <- strrep("a", 500001L)
  httr2::local_mocked_responses(yc_mock_router(list(
    "http://example.test/robots.txt" = yc_response(200L, big_body)
  )))

  x <- robots_evaluate_url_v1(
    "http://example.test/path", "bot", "yandex", "yandex"
  )

  expect_identical(x$results$policy_action, "allow_all")
  expect_identical(x$results$policy_reason, "yandex_file_over_500000_bytes")
  expect_identical(x$results$matcher_status, "not_needed")
  expect_identical(x$results$url_decision, "allow")
  expect_identical(x$results$reason, "policy_allow_all")
  expect_gt(x$evidence$stored_bytes, 500000L)
  expect_false(isTRUE(x$evidence$body_truncated))
  expect_identical(calls$google, 0L)
})

# ---------------------------------------------------------------------------
# B. v1 facade product-token validation (pre-dispatch; never fetches).
# ---------------------------------------------------------------------------

test_that("B1 an empty or NA token is input_invalid in the text facade", {
  for (token in list("", NA_character_)) {
    x <- robots_evaluate_text_v1(
      "user-agent: *\ndisallow: /", "https://example.com/x", token,
      "yandex", "yandex"
    )
    expect_identical(x$results$reason, "input_invalid")
    expect_identical(x$results$error_stage, "input")
    expect_identical(x$results$error_class, "robots_invalid_product_token")
    expect_identical(x$results$matcher_status, "not_evaluated")
    expect_identical(x$results$url_decision, NA_character_)
  }
})

test_that("B1 a valid yandex token now evaluates to a real Yandex decision", {
  yc_skip_without_native()
  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /", "https://example.com/x", "Yandex",
    "yandex", "yandex"
  )
  expect_identical(x$results$matcher_status, "evaluated")
  expect_identical(x$results$url_decision, "disallow")
  expect_identical(x$results$reason, "rule_disallow")
})

test_that("B2 an invalid token never triggers a fetch in the url facade", {
  skip_if_not_installed("httr2")
  recorder <- yc_recorder()
  httr2::local_mocked_responses(yc_mock_router(
    list(
      "http://example.test/robots.txt" = yc_response(
        200L, "user-agent: *\ndisallow: /"
      )
    ),
    recorder
  ))

  x <- robots_evaluate_url_v1(
    "http://example.test/private", "", "yandex", "yandex"
  )

  expect_identical(x$results$reason, "input_invalid")
  expect_identical(x$results$error_class, "robots_invalid_product_token")
  expect_identical(x$results$matcher_status, "not_evaluated")
  expect_length(recorder$urls, 0L)
})

# ---------------------------------------------------------------------------
# C. Hidden adapter edges (match_yandex_v1 directly).
# ---------------------------------------------------------------------------

test_that("C1 a shared body maps mixed statuses and parses once", {
  yc_skip_without_native()
  body <- yc_raw_body("User-agent: Yandex\nDisallow: /private")
  out <- match_yandex_v1(
    bodies = list(body, body, body),
    urls = c(
      "https://example.test/private/x", "https://example.test/y", "not-a-url"
    ),
    product_tokens = c("Yandex", "Googlebot", "Yandex")
  )
  expect_identical(
    out$native_evaluation_status,
    c("evaluated", "unsupported_crawler", "invalid_request_target")
  )
  expect_identical(
    out$reason,
    c("rule_disallow", "unsupported_product_token", "invalid_request_target")
  )
  expect_identical(
    out$matcher_status, c("evaluated", "not_evaluated", "not_evaluated")
  )
  expect_identical(attr(out, "n_parse_calls"), 1L)
})

test_that("C2 both empty and unextractable URLs are invalid_request_target", {
  yc_skip_without_native()
  out <- match_yandex_v1(
    bodies = list(
      yc_raw_body("User-agent: Yandex\nDisallow: /"),
      yc_raw_body("User-agent: Yandex\nDisallow: /")
    ),
    urls = c("", "not-a-url"),
    product_tokens = "Yandex"
  )
  expect_identical(
    out$native_evaluation_status,
    rep("invalid_request_target", 2L)
  )
  expect_identical(out$reason, rep("invalid_request_target", 2L))
  expect_identical(out$error_class, rep("robots_invalid_request_target", 2L))
})

test_that("C3 a zero-row batch returns the documented empty shape", {
  out <- match_yandex_v1(list(), character(0), character(0))
  expect_identical(nrow(out), 0L)
  expect_true("matched_rule_value_raw" %in% names(out))
  expect_type(out$matched_rule_value_raw, "list")
  expect_identical(attr(out, "n_parse_calls"), 0L)
})

test_that("C4 a non-list bodies argument aborts before any native call", {
  expect_error(
    match_yandex_v1("notalist", "https://example.test/x", "Yandex"),
    class = "robotstxtr_invalid_yandex_bodies"
  )
})

test_that("C5 supported-token detection is case-insensitive", {
  yc_skip_without_native()
  body <- yc_raw_body("User-agent: Yandex\nDisallow: /private")
  for (token in c("yandex", "YANDEX")) {
    out <- match_yandex_v1(
      bodies = list(body),
      urls = "https://example.test/private/x",
      product_tokens = token
    )
    expect_identical(out$native_evaluation_status, "evaluated")
    expect_identical(out$reason, "rule_disallow")
  }
})

# ---------------------------------------------------------------------------
# D. §9 request-target extractor: bytes-marked input is a boundary error.
# ---------------------------------------------------------------------------

test_that("D1 a bytes-marked URL raises the cpp11 translation error", {
  yc_skip_without_native()
  # A pure-ASCII string cannot carry a "bytes" mark, so append a non-ASCII byte
  # and mark the result "bytes"; cpp11 refuses to translate it at the boundary.
  bytes_url <- rawToChar(c(charToRaw("https://example.test/caf"), as.raw(0xE9)))
  Encoding(bytes_url) <- "bytes"
  expect_identical(Encoding(bytes_url), "bytes")
  expect_error(robotstxtr_extract_request_target_(bytes_url))
})

# ---------------------------------------------------------------------------
# E. Data-only proof: the adapter is still dormant.
# ---------------------------------------------------------------------------

test_that("E the adapter is available with the activation schema", {
  yc_expect_active()
  expect_identical(engine_matcher_registry_v1()$yandex$availability,
                   "available")
})
