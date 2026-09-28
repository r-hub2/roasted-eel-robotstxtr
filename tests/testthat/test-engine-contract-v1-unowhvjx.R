# ROBO-unowhvjx: versioned engine-aware policy and matcher facade.

engine_mock_router <- function(routes, recorder = NULL) {
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

engine_response <- function(status, body = "", headers = list()) {
  force(status)
  force(body)
  force(headers)
  function(req) {
    httr2::response(
      status_code = status, url = req$url, headers = headers,
      body = charToRaw(body)
    )
  }
}

engine_recorder <- function() {
  out <- new.env(parent = emptyenv())
  out$urls <- character()
  out$agents <- character()
  out
}

test_that("contract metadata publishes revisions and sibling ranges", {
  contract <- robots_engine_contract_v1()
  expect_s3_class(contract, "robots_engine_contract_v1")
  expect_identical(contract$contract_id, "robotstxtr.engine-aware/v1")
  expect_identical(contract$schema_revision, "2026-08-25.1")
  expect_match(contract$matcher_revisions[["google"]], "22b355ff")
  expect_identical(
    contract$matcher_availability[["google"]], "available"
  )
  # Yandex and Bing are active post-activation; RFC 9309 remains unavailable.
  expect_identical(
    contract$matcher_availability[["yandex"]], "available"
  )
  expect_identical(
    contract$matcher_revisions[["yandex"]],
    robotstxtr:::yandex_matcher_revision_v1()
  )
  expect_identical(
    contract$matcher_availability[["bing"]], "available"
  )
  expect_identical(
    contract$matcher_revisions[["bing"]],
    robotstxtr:::bing_matcher_revision_v1()
  )
  expect_identical(
    contract$matcher_availability[["rfc9309"]], "capability_unavailable"
  )
  expect_named(
    contract$sibling_versions, c("sitemapr", "sitemap-validator")
  )
  expect_identical(
    contract$sibling_versions[["sitemapr"]], ">= 0.0.0.9000, < 0.1.0"
  )
  expect_identical(
    contract$sibling_versions[["sitemap-validator"]],
    ">= 1.0.0, < 2.0.0"
  )
  expect_true(all(nzchar(contract$policy_table$policy_source)))
})

test_that("Google v1 text path uses the pinned matcher end to end", {
  body <- paste(
    "user-agent: *", "allow: /private/public", "disallow: /private",
    sep = "\n"
  )
  url <- c(
    "https://example.com/", "https://example.com/private/report",
    "https://example.com/private/public"
  )
  legacy <- allowed_by_robots_text(body, url, "testbot")
  versioned <- robots_evaluate_text_v1(
    body, url, "testbot", "google", "google"
  )

  expect_s3_class(versioned, "robots_engine_decisions_v1")
  expect_identical(
    versioned$results$url_decision, c("allow", "disallow", "allow")
  )
  expect_identical(
    versioned$results$reason,
    c("default_allow", "rule_disallow", "rule_allow")
  )
  expect_identical(
    versioned$results$matcher_status, rep("evaluated", 3L)
  )
  adapted <- as_legacy_robots_decisions_v1(versioned)
  expect_identical(adapted$results, legacy$results)
  expect_identical(adapted$robots, legacy$robots)
})

test_that("policy ruleset and matcher backend remain independent", {
  has_batch <- is.function(
    tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)
  )
  has_extract <- is.function(
    tryCatch(robotstxtr_extract_request_target_, error = function(e) NULL)
  )
  skip_if_not(
    has_batch && has_extract, "native binding not built (pure-R install)"
  )
  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /", rep("https://example.com/private", 3L),
    "Yandex",
    c("rfc9309", "yandex", "bing"),
    c("rfc9309", "yandex", "bing")
  )

  # RFC 9309 remains capability_unavailable; the active Yandex backend evaluates
  # to a real decision; the active Bing backend rejects the non-Bing token
  # "Yandex" with unsupported_profile (no decision, never a fallthrough). Policy
  # still resolves independently of the backend on every row.
  expect_identical(x$results$policy_status, rep("evaluated", 3L))
  expect_identical(x$results$policy_action, rep("use_rules", 3L))
  expect_identical(
    x$results$matcher_status,
    c("capability_unavailable", "evaluated", "unsupported_profile")
  )
  expect_identical(
    x$results$matcher_availability,
    c("capability_unavailable", "available", "available")
  )
  expect_identical(
    x$results$url_decision, c(NA_character_, "disallow", NA_character_)
  )
  expect_identical(
    x$results$reason,
    c("matcher_capability_unavailable", "rule_disallow",
      "unsupported_profile")
  )
})

test_that("matcher dispatch uses only the explicitly selected backend", {
  calls <- new.env(parent = emptyenv())
  calls$google <- 0L
  fake_google <- function(body, url, product_token) {
    calls$google <- calls$google + 1L
    list(
      url_decision = "disallow",
      reason = "rule_disallow",
      matched_line = 2L,
      matched_rule_type = "disallow",
      matched_rule_value = "/private",
      matcher_input_bytes = length(body),
      matcher_body_truncated = FALSE
    )
  }
  testthat::local_mocked_bindings(
    match_google_v1 = fake_google,
    .package = "robotstxtr"
  )
  has_batch <- is.function(
    tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)
  )
  has_extract <- is.function(
    tryCatch(robotstxtr_extract_request_target_, error = function(e) NULL)
  )
  skip_if_not(
    has_batch && has_extract, "native binding not built (pure-R install)"
  )

  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /private",
    rep("https://example.com/private", 2L),
    "bot", "google", c("google", "yandex")
  )

  # Only the Google row hits the (mocked) Google matcher; the Yandex row is
  # routed to the active Yandex adapter, which -- token "bot" being unsupported
  # -- emits its own not_evaluated reason Google never produces. No fallthrough.
  expect_identical(calls$google, 1L)
  expect_identical(
    x$results$matcher_status, c("evaluated", "not_evaluated")
  )
  expect_identical(x$results$url_decision, c("disallow", NA_character_))
  expect_identical(
    x$results$reason,
    c("rule_disallow", "unsupported_product_token")
  )
})

test_that("unknown or unavailable matcher dispatch never falls through", {
  registry <- robotstxtr:::engine_matcher_registry_v1()
  expect_error(
    robotstxtr:::match_backend_v1(
      "unknown", raw(0), "https://example.com/", "bot", registry
    ),
    class = "robotstxtr_matcher_backend_unavailable"
  )
  # Yandex is now available but batch-shaped: the per-row dispatcher must refuse
  # it with the explicit not-row-dispatchable guard rather than run it per row.
  expect_error(
    robotstxtr:::match_backend_v1(
      "yandex", raw(0), "https://example.com/", "bot", registry
    ),
    class = "robotstxtr_matcher_backend_not_row_dispatchable"
  )
})

test_that("matcher registry metadata and registration fail closed on drift", {
  registry <- robotstxtr:::engine_matcher_registry_v1()

  wrong_names <- registry
  names(wrong_names)[[1L]] <- "other"

  missing_callable <- registry
  missing_callable$google$callable <- NULL

  # Yandex is now available with a registered callable, so the drift fixtures
  # invert: they must construct genuinely-INVALID registries from that base.
  # Coupling violation: an unavailable backend that still carries a callable.
  coupling_violation <- registry
  coupling_violation$yandex$availability <- "capability_unavailable"

  # Iff violation: an available backend carrying the unavailable sentinel
  # revision, so revision and availability disagree.
  iff_violation <- registry
  iff_violation$yandex$revision <- "capability-unavailable-v1"

  for (broken in list(
    wrong_names, missing_callable, coupling_violation, iff_violation
  )) {
    expect_error(
      robotstxtr:::validate_matcher_registry_v1(broken),
      class = "robotstxtr_matcher_registry_invariant"
    )
  }

  testthat::local_mocked_bindings(
    engine_matcher_registry_v1 = function() missing_callable,
    .package = "robotstxtr"
  )
  expect_error(
    robots_evaluate_text_v1(
      "", "https://example.com/", "bot", "google", "google"
    ),
    class = "robotstxtr_matcher_registry_invariant"
  )
})

test_that("HTTP User-Agent is distinct from the robots product token", {
  recorder <- engine_recorder()
  httr2::local_mocked_responses(engine_mock_router(
    list(
      "http://example.test/robots.txt" = engine_response(
        200L, "user-agent: crawler-token\ndisallow: /private"
      )
    ),
    recorder
  ))
  x <- robots_evaluate_url_v1(
    "http://example.test/private", "crawler-token", "google", "google",
    fetch_user_agent = "fetch-client/7"
  )

  expect_identical(recorder$agents, "fetch-client/7")
  expect_identical(x$results$http_user_agent, "fetch-client/7")
  expect_identical(x$results$robots_product_token, "crawler-token")
  expect_identical(x$results$url_decision, "disallow")
  expect_identical(x$evidence$http_user_agent, "fetch-client/7")
})

test_that("engine status policies do not fabricate universal behavior", {
  httr2::local_mocked_responses(engine_mock_router(list(
    "http://example.test/robots.txt" = engine_response(404L)
  )))
  x <- robots_evaluate_url_v1(
    rep("http://example.test/path", 4L), "bot",
    c("google", "yandex", "rfc9309", "bing"),
    c("google", "yandex", "rfc9309", "bing")
  )

  expect_identical(
    x$results$policy_status,
    c("evaluated", "evaluated", "evaluated", "documentation_gap")
  )
  expect_identical(
    x$results$url_decision, c("allow", "allow", "allow", NA_character_)
  )
  expect_identical(
    x$results$matcher_status,
    c("not_needed", "not_needed", "not_needed", "not_evaluated")
  )
})

test_that("lifecycle-dependent and undocumented outcomes stay unresolved", {
  httr2::local_mocked_responses(engine_mock_router(list(
    "http://example.test/robots.txt" = engine_response(500L)
  )))
  x <- robots_evaluate_url_v1(
    rep("http://example.test/path", 4L), "bot",
    c("google", "rfc9309", "yandex", "bing"),
    c("google", "rfc9309", "yandex", "bing")
  )

  expect_identical(
    x$results$policy_status,
    c(
      "context_required", "context_required", "evaluated",
      "documentation_gap"
    )
  )
  expect_identical(
    x$results$url_decision, c(NA_character_, NA_character_, "allow", NA)
  )
  expect_identical(
    x$results$policy_provenance,
    c("documented", "documented", "documented", "documentation_gap")
  )
})

test_that("safety refusal never becomes an allow or deny", {
  x <- robots_evaluate_url_v1(
    "http://127.0.0.1/private", "bot", "google", "google"
  )

  expect_identical(x$evidence$evidence_status, "safety_refused")
  expect_identical(x$evidence$safety_block_reason, "loopback")
  expect_identical(x$results$policy_status, "not_evaluated")
  expect_identical(x$results$policy_reason, "safety_refused")
  expect_identical(x$results$matcher_status, "not_evaluated")
  expect_true(is.na(x$results$url_decision))
})

test_that("over-ceiling evidence is incomplete and never matched", {
  httr2::local_mocked_responses(engine_mock_router(list(
    "http://example.test/robots.txt" = engine_response(
      200L, "user-agent: *\ndisallow: /"
    )
  )))
  x <- robots_evaluate_url_v1(
    "http://example.test/private", "bot", "google", "google",
    max_bytes = 8L
  )

  expect_identical(x$evidence$evidence_status, "incomplete")
  expect_identical(x$evidence$termination_reason, "ceiling")
  expect_gt(x$evidence$observed_bytes, 8L)
  expect_identical(x$evidence$stored_bytes, 0L)
  expect_false(x$evidence$body_present)
  expect_identical(x$results$policy_status, "not_evaluated")
  expect_identical(x$results$reason, "incomplete_evidence")
  expect_true(is.na(x$results$url_decision))
})

test_that("partial response resolves policy but does not run a matcher", {
  httr2::local_mocked_responses(engine_mock_router(list(
    "http://example.test/robots.txt" = engine_response(
      206L, "user-agent: *\ndisallow: /"
    )
  )))
  x <- robots_evaluate_url_v1(
    "http://example.test/private", "bot", "google", "google"
  )

  expect_identical(x$evidence$evidence_status, "partial")
  expect_identical(x$results$policy_status, "evaluated")
  expect_identical(x$results$policy_action, "use_rules")
  expect_identical(x$results$matcher_status, "not_evaluated")
  expect_identical(x$results$reason, "partial_evidence")
  expect_true(is.na(x$results$url_decision))
})

test_that("redirect evidence records terminal reason and ordered hops", {
  routes <- list(
    "http://example.test/robots.txt" = engine_response(
      302L, headers = list(Location = "http://example.test/one")
    ),
    "http://example.test/one" = engine_response(301L)
  )
  httr2::local_mocked_responses(engine_mock_router(routes))
  x <- robots_evaluate_url_v1(
    "http://example.test/path", "bot", "google", "google"
  )

  expect_identical(x$evidence$evidence_status, "http_protocol_error")
  expect_identical(x$evidence$terminal_redirect_reason, "no_location")
  expect_identical(x$evidence$final_http_status, 301L)
  expect_identical(x$evidence$redirect_count, 1L)
  expect_length(x$evidence$redirect_hops[[1L]], 2L)
  expect_identical(
    x$evidence$redirect_hops[[1L]][[1L]],
    list(
      from_url = "http://example.test/robots.txt", status = 302L,
      location_target = "http://example.test/one"
    )
  )
  expect_identical(x$results$policy_status, "documentation_gap")
  expect_true(is.na(x$results$url_decision))
})

test_that("HTTPS downgrade redirect is an engine-independent safety refusal", {
  httr2::local_mocked_responses(engine_mock_router(list(
    "https://example.test/robots.txt" = engine_response(
      302L, headers = list(Location = "http://example.test/robots.txt")
    )
  )))
  x <- robots_evaluate_url_v1(
    "https://example.test/path", "bot", "yandex", "yandex"
  )

  expect_identical(x$evidence$terminal_redirect_reason, "downgrade")
  expect_identical(x$evidence$safety_block_reason, "https_downgrade")
  expect_identical(x$evidence$evidence_status, "safety_refused")
  expect_identical(x$results$policy_status, "not_evaluated")
  expect_true(is.na(x$results$url_decision))
})

test_that("Google applies its 500 KiB matcher prefix independently", {
  prefix <- paste0("user-agent: *\ndisallow: /private\n", strrep("#", 524250L))
  body <- paste0(prefix, "\nallow: /private")
  expect_gt(nchar(body, type = "bytes"), 524288L)
  x <- robots_evaluate_text_v1(
    body, "https://example.com/private", "bot", "google", "google"
  )

  expect_identical(x$results$url_decision, "disallow")
  expect_true(x$results$matcher_body_truncated)
  expect_identical(x$results$matcher_input_bytes, 524288L)
  expect_false(x$evidence$body_truncated)
  expect_identical(x$evidence$stored_bytes, nchar(body, type = "bytes"))
})

test_that("legacy adapter retains generic HTTP failures as unknown", {
  httr2::local_mocked_responses(engine_mock_router(list(
    "http://example.test/robots.txt" = engine_response(403L)
  )))
  versioned <- robots_evaluate_url_v1(
    "http://example.test/path", "bot", "google", "google"
  )
  legacy <- as_legacy_robots_decisions_v1(versioned)

  expect_identical(versioned$results$url_decision, "allow")
  expect_true(is.na(legacy$results$allowed))
  expect_identical(legacy$results$decision_source, "fetch_unknown")
  expect_identical(legacy$results$fetch_outcome, "http_error")
})

test_that("versioned axes reject implicit or unknown selections", {
  expect_error(
    robots_evaluate_text_v1("", "http://a/", "bot", "unknown", "google"),
    class = "robotstxtr_invalid_robots_policy_ruleset"
  )
  expect_error(
    robots_evaluate_text_v1("", "http://a/", "bot", "google", "unknown"),
    class = "robotstxtr_invalid_matcher_backend"
  )
  mixed <- robots_evaluate_text_v1(
    "", "http://a/", "bot", "yandex", "yandex"
  )
  expect_error(
    as_legacy_robots_decisions_v1(mixed),
    class = "robotstxtr_incompatible_legacy_adapter"
  )
})

test_that("print method summarizes and previews engine decisions", {
  body <- "user-agent: *\nDisallow: /private\n"
  plural <- robots_evaluate_text_v1(
    body, c("https://example.com/a", "https://example.com/private"),
    "bot", "google", "google"
  )
  single <- robots_evaluate_text_v1(
    body, "https://example.com/private", "bot", "google", "google"
  )
  empty <- robots_evaluate_text_v1(body, character(), "bot", "google", "google")

  # Every printed field is either a frozen schema string or an echo of the
  # caller's own input: no timestamp, run id, or filesystem path reaches the
  # output, so the snapshot is stable across runs and machines.
  expect_snapshot(print(plural))
  expect_snapshot(print(single))
  expect_snapshot(print(empty))

  # The method returns its input invisibly.
  printed <- capture.output(returned <- print(plural))
  expect_identical(returned, plural)
  expect_gt(length(printed), 0L)
})

test_that("engine argument expansion rejects bad types and lengths", {
  expect_error(
    robotstxtr:::expand_product_token_v1(1L, 2L),
    class = "robotstxtr_invalid_robots_product_token_type",
    regexp = "must be a character vector"
  )
  expect_error(
    robotstxtr:::expand_product_token_v1(c("a", "b"), 3L),
    class = "robotstxtr_length_mismatch",
    regexp = "got length 2"
  )
  # length(token) == n passes the vector through unchanged, elementwise.
  expect_identical(
    robotstxtr:::expand_product_token_v1(c("a", "b"), 2L), c("a", "b")
  )
  expect_identical(
    robotstxtr:::expand_product_token_v1("a", 3L), rep("a", 3L)
  )

  backends <- robotstxtr:::engine_matchers_v1()
  expect_error(
    robotstxtr:::expand_engine_argument_v1(1L, 2L, "matcher_backend", backends),
    class = "robotstxtr_invalid_matcher_backend",
    regexp = "must be a character vector"
  )
  expect_error(
    robotstxtr:::expand_engine_argument_v1(
      c("google", "yandex"), 3L, "robots_policy_ruleset",
      robotstxtr:::engine_rulesets_v1()
    ),
    class = "robotstxtr_length_mismatch",
    regexp = "got length 2"
  )
  expect_identical(
    robotstxtr:::expand_engine_argument_v1(
      c("google", "yandex"), 2L, "matcher_backend", backends
    ),
    c("google", "yandex")
  )
})

test_that("supplied text bodies keep raw bytes when marked as bytes", {
  # A "bytes"-encoded string must not be routed through enc2utf8(): the
  # non-UTF-8 octet has to survive into the matcher input verbatim.
  body <- rawToChar(as.raw(c(0x2f, 0xff)))
  Encoding(body) <- "bytes"
  expect_identical(Encoding(body), "bytes")
  expect_identical(
    robotstxtr:::text_body_bytes_v1(body), as.raw(c(0x2f, 0xff))
  )
})

test_that("acquisition results map onto neutral evidence statuses", {
  status_of <- function(...) {
    robotstxtr:::source_evidence_status_v1(
      robotstxtr:::make_source_result(...)
    )
  }
  expect_identical(
    status_of(
      "redirect_error", NULL, NULL, 5L, NULL, "over budget",
      terminal_redirect_reason = "over_budget"
    ),
    "redirect_over_budget"
  )
  expect_identical(
    status_of("timeout", NULL, NULL, 0L, NULL, "timed out"), "transport_fail"
  )
  expect_identical(
    status_of("tls_error", NULL, NULL, 0L, NULL, "bad certificate"),
    "transport_fail"
  )
  # A 1xx response to the robots.txt request is a protocol error, never a
  # status the policy table can rule on.
  expect_identical(
    status_of("http_error", 100L, "http://example.test/robots.txt", 0L,
              NULL, NULL),
    "http_protocol_error"
  )
  # Nothing observed and no status band to classify: evidence is not
  # applicable rather than an implied allow or deny.
  expect_identical(
    status_of("missing", NULL, "http://example.test/robots.txt", 0L,
              NULL, NULL),
    "not_applicable"
  )
})

# The scattered evidence row resolve_policy_v1() consumes: evaluate_rows_v1()
# reduces one evidence data.frame row to scalars before handing it over. Only
# these four fields are read, so the fixture names exactly those.
engine_policy_evidence <- function(evidence_status, source_kind = "fetched",
                                   final_http_status = NA_integer_,
                                   stored_bytes = 0L) {
  list(
    evidence_status = evidence_status,
    source_kind = source_kind,
    final_http_status = final_http_status,
    stored_bytes = stored_bytes
  )
}

test_that("policy resolution covers every acquisition category", {
  policy_table <- robotstxtr:::engine_policy_table_v1()
  resolve <- function(evidence, ruleset = "google") {
    robotstxtr:::resolve_policy_v1(evidence, ruleset, policy_table)
  }

  # Evidence that classifies into no status band is never evaluated.
  expect_identical(
    resolve(engine_policy_evidence("not_applicable")),
    list(
      policy_status = "not_evaluated",
      policy_action = NA_character_,
      policy_reason = "evidence_not_applicable",
      policy_provenance = "application_choice",
      policy_source = "design/engine-profiles.md#neutral-fetch"
    )
  )
  # A transport failure is lifecycle-dependent for Google: no action.
  expect_identical(
    resolve(engine_policy_evidence("transport_fail")),
    list(
      policy_status = "context_required",
      policy_action = NA_character_,
      policy_reason = "crawler_lifecycle_context_required",
      policy_provenance = "documented",
      policy_source = "design/engine-profiles.md#status-policy"
    )
  )
  # Google documents an over-budget redirect chain as a 404-equivalent.
  expect_identical(
    resolve(engine_policy_evidence("redirect_over_budget")),
    list(
      policy_status = "evaluated",
      policy_action = "allow_all",
      policy_reason = "redirect_over_budget_as_404",
      policy_provenance = "documented",
      policy_source = "design/engine-profiles.md#redirect-handling"
    )
  )
  # A non-200 2xx still yields rules under Google.
  expect_identical(
    resolve(engine_policy_evidence("usable_body", final_http_status = 204L)),
    list(
      policy_status = "evaluated",
      policy_action = "use_rules",
      policy_reason = "http_2xx_use_rules",
      policy_provenance = "documented",
      policy_source = "design/engine-profiles.md#status-policy"
    )
  )
  # 429 is split out of the 4xx band; RFC 9309 allows all by application
  # choice rather than by documented vendor behavior.
  expect_identical(
    resolve(
      engine_policy_evidence("http_status", final_http_status = 429L),
      "rfc9309"
    ),
    list(
      policy_status = "evaluated",
      policy_action = "allow_all",
      policy_reason = "http_429_allow_all",
      policy_provenance = "application_choice",
      policy_source = "design/engine-profiles.md#status-policy"
    )
  )
  # Defensive fall-through: a final status matching no band is treated as a
  # protocol error, which no ruleset documents.
  expect_identical(
    resolve(engine_policy_evidence("http_status")),
    list(
      policy_status = "documentation_gap",
      policy_action = NA_character_,
      policy_reason = "policy_documentation_gap",
      policy_provenance = "documentation_gap",
      policy_source = "design/engine-profiles.md"
    )
  )
})

test_that("registry validation rejects field-level drift", {
  registry <- robotstxtr:::engine_matcher_registry_v1()

  bad_revision <- registry
  bad_revision$google$revision <- ""
  expect_error(
    robotstxtr:::validate_matcher_registry_v1(bad_revision),
    class = "robotstxtr_matcher_registry_invariant",
    regexp = "backend `google` must have one non-empty revision"
  )

  bad_availability <- registry
  bad_availability$google$availability <- "maybe"
  expect_error(
    robotstxtr:::validate_matcher_registry_v1(bad_availability),
    class = "robotstxtr_matcher_registry_invariant",
    regexp = "backend `google` has an invalid availability state"
  )

  # Keeping the `callable` name but filling it with a non-function passes the
  # field-name check, so the availability/callable coupling guard is what has
  # to catch it.
  not_callable <- registry
  not_callable$google$callable <- "match_google_v1"
  expect_error(
    robotstxtr:::validate_matcher_registry_v1(not_callable),
    class = "robotstxtr_matcher_registry_invariant",
    regexp = "available backend `google` must have a registered callable"
  )
})
