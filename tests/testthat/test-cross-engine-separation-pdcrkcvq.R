# Cross-engine separation + Google/Yandex non-regression sentinels
# (ROBO-pdcrkcvq, YI6d).
#
# A small, high-signal suite proving the Google and Yandex matchers stay
# behaviorally and operationally SEPARATE. It adds tests ONLY: it touches no
# source and no fixture. Post-activation (YI5) the Yandex backend is available,
# the engine-aware v1 schema is current, and the batch adapter is registered;
# the SEP/
# DISPATCH/DRIFT assertions below track that active state.
#
# The suite is written to FAIL on a swapped/fallback dispatch (Yandex routed
# through Google) and on either engine's metadata/identity drift: the SEP
# divergence, DISPATCH counter, and DRIFT freeze assertions are tight enough
# that such a change breaks them. It deliberately does NOT re-assert coverage
# owned by sibling suites (Google revision pin, legacy identity, empty-directive
# Google behavior, engine-aware policy fixtures, the google-only counter, the
# registry drift invariants, the full 140-case corpus conformance).
#
# Corpus-driven SEP cases exercise the native Yandex checked binding + the §9
# request-target extractor, so they skip on a pure-R install with the same guard
# the sibling conformance/edge suites use. The metadata-only DISPATCH/DRIFT/
# TOKEN/DATA-ONLY sections need no native Yandex binding.

# ---------------------------------------------------------------------------
# Inline, uniquely prefixed (xe_) helpers -- avoid collision with the sibling
# suites' globals of the same shape.
# ---------------------------------------------------------------------------

xe_skip_if_no_yandex_native <- function() {
  skip_if_not_installed("jsonlite")
  batch <- tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)
  extract <- tryCatch(
    robotstxtr_extract_request_target_, error = function(e) NULL
  )
  skip_if_not(
    is.function(batch) && is.function(extract),
    "native binding not built (pure-R install)"
  )
}

xe_corpus_record <- function(case_id) {
  records <- read_yandex_corpus(yandex_corpus_dir())
  for (rec in records) {
    if (identical(rec$case_id, case_id)) {
      return(rec)
    }
  }
  stop(sprintf("corpus case not found: %s", case_id))
}

# Run BOTH engines on one corpus case with byte-identical body input and a URL
# synthesized so the §9 extractor recovers exactly the stored request-target.
xe_both_engines <- function(case_id) {
  rec <- xe_corpus_record(case_id)
  path <- file.path(yandex_corpus_dir(), rec$body_file)
  body <- readBin(path, "raw", n = as.integer(rec$byte_size))
  url <- paste0("https://example.test", rec$request_target)
  token <- rec$crawler
  list(
    rec = rec,
    google = match_google_v1(body, url, token),
    yandex = match_yandex_v1(list(body), url, token)
  )
}

# The comparison harness under test: reduce each engine's per-row output to the
# same decision tuple and report whether they agree. SEP divergence tests assert
# this is FALSE; agreement controls assert it is TRUE.
xe_tuple <- function(res) {
  list(
    url_decision = as.character(res$url_decision),
    reason = as.character(res$reason),
    matched_line = as.integer(res$matched_line),
    matched_rule_type = as.character(res$matched_rule_type),
    matched_rule_value = as.character(res$matched_rule_value)
  )
}

xe_engines_agree <- function(both) {
  identical(xe_tuple(both$google), xe_tuple(both$yandex))
}

# Read an installed identity file (inst/ scope) with a source-tree dev fallback.
xe_read_installed <- function(name) {
  path <- system.file(name, package = "robotstxtr")
  if (!nzchar(path) || !file.exists(path)) {
    candidates <- c(
      file.path("inst", name),
      file.path("..", "..", "inst", name),
      file.path("..", "..", "..", "inst", name)
    )
    hit <- candidates[file.exists(candidates)]
    path <- if (length(hit) > 0L) hit[[1L]] else ""
  }
  if (!nzchar(path) || !file.exists(path)) {
    return(NA_character_)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

xe_canned_google <- function(body) {
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

# ---------------------------------------------------------------------------
# SEP -- cross-engine divergence and Yandex-owns-its-own metadata.
# ---------------------------------------------------------------------------

test_that("SEP1 effective-empty Disallow keeps Yandex metadata", {
  xe_skip_if_no_yandex_native()
  both <- xe_both_engines("ydx-06-yandex-open")
  y <- both$yandex
  g <- both$google
  expected <- both$rec$expected

  # Yandex yields its own effective-empty owning-rule metadata...
  expect_identical(y$url_decision, "allow")
  expect_identical(y$reason, "effective_empty_disallow")
  expect_identical(y$matched_rule_type, "disallow")
  expect_identical(y$matched_rule_value, "")
  expect_identical(y$matched_line, 2L)
  expect_identical(y$matched_rule_value_raw[[1L]], raw(0))
  # ...equal to its pinned corpus expectation.
  expect_identical(y$url_decision, expected$decision)
  expect_identical(y$reason, expected$source)

  # Google has no owning rule here: a plain default_allow.
  expect_identical(g$url_decision, "allow")
  expect_identical(g$reason, "default_allow")
  expect_identical(g$matched_rule_type, "none")
  expect_true(is.na(g$matched_line))
  expect_true(is.na(g$matched_rule_value))

  # Both ALLOW, yet the (reason, type, value) tuples DIFFER: the Yandex adapter
  # cannot be Google underneath.
  expect_false(xe_engines_agree(both))
  expect_false(identical(y$reason, g$reason))
})

test_that("SEP1B empty-Disallow-first flips the decision", {
  xe_skip_if_no_yandex_native()
  both <- xe_both_engines("ydx-11-yandex-open")
  y <- both$yandex
  g <- both$google

  # Yandex: the first (empty) Disallow makes the group effectively empty.
  expect_identical(y$url_decision, "allow")
  expect_identical(y$reason, "effective_empty_disallow")
  expect_identical(y$url_decision, both$rec$expected$decision)

  # Google: the later `Disallow: /` disallows outright.
  expect_identical(g$url_decision, "disallow")
  expect_identical(g$reason, "rule_disallow")

  # OPPOSITE decisions on the same body/target/token: a Yandex->Google swap
  # would flip allow -> disallow, so this is the sharpest swap sentinel.
  expect_false(identical(y$url_decision, g$url_decision))
  expect_false(xe_engines_agree(both))
})

test_that("SEP2 wildcard specificity: Yandex owns the rule", {
  xe_skip_if_no_yandex_native()
  both <- xe_both_engines("ydx-05-yandex-wild-b-x")
  y <- both$yandex
  matched_rule <- both$rec$expected$matched_rule

  # Yandex owns line/type/value/decision, equal to the pinned expectation.
  expect_identical(y$url_decision, both$rec$expected$decision)
  expect_identical(y$reason, both$rec$expected$source)
  expect_identical(y$matched_line, as.integer(matched_rule$line))
  expect_identical(y$matched_rule_type, matched_rule$type)
  expect_identical(y$matched_rule_value, matched_rule$value)

  # Observed ACTUAL relationship: Google resolves this wildcard tie identically.
  # A CONTROL -- the harness correctly reports agreement; no divergence forced.
  expect_true(xe_engines_agree(both))
})

test_that("SEP3 malformed recovery: Yandex owns the rule", {
  xe_skip_if_no_yandex_native()
  both <- xe_both_engines("ydx-07-malformed-recovery--yandex-03")
  y <- both$yandex
  matched_rule <- both$rec$expected$matched_rule

  expect_identical(y$url_decision, both$rec$expected$decision)
  expect_identical(y$reason, both$rec$expected$source)
  expect_identical(y$matched_line, as.integer(matched_rule$line))
  expect_identical(y$matched_rule_type, matched_rule$type)
  expect_identical(y$matched_rule_value, matched_rule$value)

  # Observed ACTUAL relationship: Google recovers the same rule. A CONTROL.
  expect_true(xe_engines_agree(both))
})

test_that("SEP-CTRL plain disallow agreement is detected", {
  xe_skip_if_no_yandex_native()
  both <- xe_both_engines("ydx-01-ua-specificity--yandex-02")

  expect_identical(both$yandex$url_decision, "disallow")
  expect_identical(both$google$url_decision, "disallow")
  # Positive control: agreement here plus the SEP1/SEP1B divergences prove the
  # harness discriminates, so a SEP divergence cannot be a false positive.
  expect_true(xe_engines_agree(both))
})

# ---------------------------------------------------------------------------
# DISPATCH -- no cross-invocation, no fallthrough between the two matchers.
# ---------------------------------------------------------------------------

test_that("D1 a Google request never touches the Yandex adapter", {
  calls <- new.env(parent = emptyenv())
  calls$google <- 0L
  calls$yandex <- 0L
  fake_google <- function(body, url, product_token) {
    calls$google <- calls$google + 1L
    xe_canned_google(body)
  }
  fake_yandex <- function(bodies, urls, product_tokens) {
    calls$yandex <- calls$yandex + 1L
    NULL
  }
  testthat::local_mocked_bindings(
    match_google_v1 = fake_google,
    match_yandex_v1 = fake_yandex,
    .package = "robotstxtr"
  )

  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /private",
    "https://example.com/private", "bot", "google", "google"
  )

  expect_identical(calls$google, 1L)
  expect_identical(calls$yandex, 0L)
  expect_identical(x$results$url_decision, "disallow")
})

test_that("D2 a Yandex row dispatches to the Yandex adapter, not Google", {
  calls <- new.env(parent = emptyenv())
  calls$google <- 0L
  calls$yandex <- 0L
  fake_google <- function(body, url, product_token) {
    calls$google <- calls$google + 1L
    xe_canned_google(body)
  }
  fake_yandex <- function(bodies, urls, product_tokens) {
    calls$yandex <- calls$yandex + 1L
    out <- data.frame(
      native_evaluation_status = "evaluated",
      matcher_status = "evaluated",
      url_decision = "disallow",
      reason = "rule_disallow",
      matched_line = 2L,
      matched_rule_type = "disallow",
      matched_rule_value = "/private",
      matcher_input_bytes = length(bodies[[1L]]),
      matcher_body_truncated = FALSE,
      error_stage = NA_character_,
      error_class = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )
    out$matched_rule_value_raw <- list(charToRaw("/private"))
    out
  }
  testthat::local_mocked_bindings(
    match_google_v1 = fake_google,
    match_yandex_v1 = fake_yandex,
    .package = "robotstxtr"
  )

  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /private",
    "https://example.com/private", "bot", "yandex", "yandex"
  )

  # The active Yandex backend is batch-dispatched once; Google is never touched.
  expect_identical(calls$google, 0L)
  expect_identical(calls$yandex, 1L)
  expect_identical(x$results$matcher_status, "evaluated")
  expect_identical(x$results$reason, "rule_disallow")
  expect_identical(x$results$url_decision, "disallow")
  expect_identical(
    x$results$matched_rule_value_raw[[1L]], charToRaw("/private")
  )
})

test_that("D3 separation is enforced at the registry", {
  registry <- engine_matcher_registry_v1()
  # Now active: registered with the batch-shaped adapter as its callable
  # (parse-once), still with the batch signature the dispatcher invokes.
  expect_type(registry$yandex$callable, "closure")
  expect_named(
    formals(match_yandex_v1),
    c("bodies", "urls", "product_tokens")
  )
})

# ---------------------------------------------------------------------------
# DRIFT -- fail closed on either engine's metadata / vendored identity drift.
# ---------------------------------------------------------------------------

test_that("M1 matcher revisions and schema stay pinned", {
  registry <- engine_matcher_registry_v1()
  expect_identical(
    registry$google$revision,
    "google-robotstxt-22b355ff855419e6a3ff8ff09c0ad7fdb17116f9"
  )
  expect_identical(
    registry$yandex$revision, robotstxtr:::yandex_matcher_revision_v1()
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
})

test_that("M2 PROVENANCE and NOTICE pin the Google commits", {
  provenance <- xe_read_installed("PROVENANCE")
  notice <- xe_read_installed("NOTICE")
  skip_if(
    is.na(provenance) || is.na(notice),
    "installed identity records absent"
  )
  upstream <- "22b355ff855419e6a3ff8ff09c0ad7fdb17116f9"
  replica <- "1cb8b047d81dfa0e9c1a1549b269fb5f196756c9"
  expect_true(grepl(upstream, provenance, fixed = TRUE))
  expect_true(grepl(replica, provenance, fixed = TRUE))
  expect_true(grepl(upstream, notice, fixed = TRUE))
  expect_true(grepl(replica, notice, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# TOKEN -- any well-formed token routes to Google and yields Google semantics.
# ---------------------------------------------------------------------------

test_that("T1 an arbitrary valid token yields Google semantics", {
  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /private",
    "https://example.com/private", "some-crawler/1.0", "google", "google"
  )

  # A real Google decision from the google reason vocabulary, not an error/
  # unsupported: Google group-selects on any valid token.
  expect_identical(x$results$matcher_status, "evaluated")
  expect_identical(x$results$url_decision, "disallow")
  expect_identical(x$results$reason, "rule_disallow")
  expect_identical(x$results$matched_rule_type, "disallow")
})

# ---------------------------------------------------------------------------
# DATA-ONLY -- dormancy proof: Yandex unavailable, schema pinned, Google live.
# ---------------------------------------------------------------------------

test_that("DATA-ONLY Yandex active, schema bumped, Google live", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")

  registry <- engine_matcher_registry_v1()
  expect_identical(registry$google$availability, "available")
  expect_type(registry$google$callable, "closure")
  expect_type(registry$yandex$callable, "closure")
})

# ===========================================================================
# BING cross-engine sentinels (ROBO-onwulhga, BI6; spec §16.4).
#
# Prove Bing requests reach the Bing backend and stay behaviorally +
# operationally SEPARATE from Google and Yandex, with representative cases in
# winner selection, empty-Disallow handling, and a limit outcome -- including
# genuine cross-engine differences (Bing != Yandex on empty-Disallow; Bing !=
# Google/Yandex on the request-target limit). Agreement cases are labeled
# controls, not Bing evidence. Behavioral cases go through the PUBLIC facade;
# DISPATCH cases mock the three adapters and count invocations.
# ===========================================================================

bxe_skip_if_no_bing_native <- function() {
  batch <- tryCatch(robotstxtr_bing_eval_batch_, error = function(e) NULL)
  skip_if_not(
    is.function(batch),
    "native bing binding not built (pure-R install)"
  )
}

bxe_skip_if_no_yandex_native <- function() {
  skip_if_not(
    is.function(tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)),
    "native yandex binding not built (pure-R install)"
  )
}

# One engine over one body/target/token through the public facade.
bxe_eval <- function(body, target, token, ruleset, backend) {
  robots_evaluate_text_v1(
    robots_txt = body,
    url = paste0("https://example.test", target),
    robots_product_token = token,
    robots_policy_ruleset = ruleset,
    matcher_backend = backend
  )$results
}

bxe_bing <- function(body, target, token = "bingbot") {
  bxe_eval(body, target, token, "assumed_rfc9309", "bing")
}
bxe_google <- function(body, target, token = "googlebot") {
  bxe_eval(body, target, token, "google", "google")
}
bxe_yandex <- function(body, target, token = "Yandex") {
  bxe_eval(body, target, token, "yandex", "yandex")
}

test_that("BSEP1 empty-Disallow: Bing (inert) diverges from Yandex", {
  bxe_skip_if_no_bing_native()
  bxe_skip_if_no_yandex_native()
  # An empty Disallow followed by `Disallow: /`. The shipped Bing profile
  # treats the empty Disallow as INERT, so `/` disallows; Yandex treats it as
  # effective-empty and the group allows everything.
  body <- "User-agent: *\nDisallow:\nDisallow: /"
  b <- bxe_bing(body, "/x")
  y <- bxe_yandex(body, "/x")
  g <- bxe_google(body, "/x")

  # Bing: inert empty -> the `/` rule owns the disallow.
  expect_identical(b$matcher_backend[[1L]], "bing")
  expect_identical(b$matcher_status[[1L]], "evaluated")
  expect_identical(b$url_decision[[1L]], "disallow")
  expect_identical(b$reason[[1L]], "rule_disallow")
  expect_identical(b$matched_rule_value[[1L]], "/")

  # Yandex: effective-empty -> allow, with its own reason vocabulary.
  expect_identical(y$url_decision[[1L]], "allow")
  expect_identical(y$reason[[1L]], "effective_empty_disallow")

  # OPPOSITE decisions on the same body/target: a Bing->Yandex swap would flip
  # disallow -> allow, so this is the sharpest empty-Disallow separation.
  expect_false(identical(b$url_decision[[1L]], y$url_decision[[1L]]))
  expect_false(identical(b$reason[[1L]], y$reason[[1L]]))
  # Bing agrees with Google here -- a CONTROL, not Bing-specific evidence.
  expect_identical(b$url_decision[[1L]], g$url_decision[[1L]])
})

test_that("BSEP2 limit outcome: Bing rejects an over-limit request target", {
  bxe_skip_if_no_bing_native()
  bxe_skip_if_no_yandex_native()
  # A request target beyond the 65,536-byte request-target limit.
  target <- paste0("/", strrep("a", 70000L))
  body <- "User-agent: *\nDisallow: /a"
  b <- bxe_bing(body, target)
  g <- bxe_google(body, target)
  y <- bxe_yandex(body, target)

  # Bing: the matcher rejects the input -> a non-evaluated limit status with no
  # decision, carrying the native request-target-limit reason.
  expect_identical(b$matcher_backend[[1L]], "bing")
  expect_identical(b$matcher_status[[1L]], "matcher_input_limit_exceeded")
  expect_true(is.na(b$url_decision[[1L]]))
  expect_identical(b$reason[[1L]], "request_target_limit_exceeded")
  expect_identical(b$error_class[[1L]], "robots_matcher_input_limit_exceeded")

  # Google and Yandex evaluate the same request to a decision: a limit-outcome
  # difference that a Bing->Google/Yandex swap could not reproduce.
  expect_identical(g$matcher_status[[1L]], "evaluated")
  expect_identical(y$matcher_status[[1L]], "evaluated")
  expect_false(identical(b$matcher_status[[1L]], g$matcher_status[[1L]]))
  expect_false(identical(b$matcher_status[[1L]], y$matcher_status[[1L]]))
})

test_that("BSEP3 unsupported profile: Bing rejects a token Google owns", {
  bxe_skip_if_no_bing_native()
  body <- "User-agent: *\nDisallow: /a"
  b <- bxe_bing(body, "/a", token = "googlebot")
  g <- bxe_google(body, "/a", token = "googlebot")

  # Bing's bounded profile: a non-bingbot/adidxbot token yields no decision.
  expect_identical(b$matcher_status[[1L]], "unsupported_profile")
  expect_true(is.na(b$url_decision[[1L]]))
  expect_identical(b$error_class[[1L]], "robots_unsupported_profile")

  # Google group-selects on any token and evaluates: a Bing->Google swap would
  # turn this no-decision into a disallow.
  expect_identical(g$matcher_status[[1L]], "evaluated")
  expect_identical(g$url_decision[[1L]], "disallow")
  expect_false(identical(b$matcher_status[[1L]], g$matcher_status[[1L]]))
})

test_that("BSEP4 winner selection: Bing owns its rule metadata (control)", {
  bxe_skip_if_no_bing_native()
  # Longest-match winner: the Allow at a deeper path beats the shorter Disallow.
  body <- "User-agent: bingbot\nDisallow: /a\nAllow: /a/b"
  b <- bxe_bing(body, "/a/b/c")

  # Bing evaluates through the Bing backend with its OWN owning-rule metadata
  # and the exact raw value bytes.
  expect_identical(b$matcher_backend[[1L]], "bing")
  expect_identical(b$matcher_availability[[1L]], "available")
  expect_identical(b$url_decision[[1L]], "allow")
  expect_identical(b$reason[[1L]], "rule_allow")
  expect_identical(b$matched_line[[1L]], 3L)
  expect_identical(b$matched_rule_type[[1L]], "allow")
  expect_identical(b$matched_rule_value[[1L]], "/a/b")
  expect_identical(b$matched_rule_value_raw[[1L]], charToRaw("/a/b"))

  # Google resolves this longest-match identically: a CONTROL. Agreement here,
  # with the BSEP1/BSEP2/BSEP3 divergences, proves the suite discriminates.
  g <- bxe_google(body, "/a/b/c", token = "bingbot")
  expect_identical(b$url_decision[[1L]], g$url_decision[[1L]])
})

# ---- BING DISPATCH: no cross-invocation between the three adapters ----------

bxe_canned_bing <- function(bodies) {
  out <- data.frame(
    native_parse_status = "evaluated",
    native_evaluation_status = "evaluated",
    matcher_status = "evaluated",
    url_decision = "disallow",
    reason = "rule_disallow",
    matched_line = 2L,
    matched_rule_type = "disallow",
    matched_rule_value = "/private",
    matcher_input_bytes = length(bodies[[1L]]),
    matcher_body_truncated = FALSE,
    error_stage = NA_character_,
    error_class = NA_character_,
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
  out$matched_rule_value_raw <- list(charToRaw("/private"))
  out
}

test_that("BD1 a Bing row dispatches to the Bing adapter only", {
  calls <- new.env(parent = emptyenv())
  calls$google <- 0L
  calls$yandex <- 0L
  calls$bing <- 0L
  testthat::local_mocked_bindings(
    match_google_v1 = function(body, url, product_token) {
      calls$google <- calls$google + 1L
      xe_canned_google(body)
    },
    match_yandex_v1 = function(bodies, urls, product_tokens) {
      calls$yandex <- calls$yandex + 1L
      NULL
    },
    match_bing_v1 = function(bodies, urls, product_tokens) {
      calls$bing <- calls$bing + 1L
      bxe_canned_bing(bodies)
    },
    .package = "robotstxtr"
  )

  x <- robots_evaluate_text_v1(
    "user-agent: bingbot\ndisallow: /private",
    "https://example.com/private", "bingbot", "assumed_rfc9309", "bing"
  )

  # Batch-dispatched to Bing exactly once; Google and Yandex never touched.
  expect_identical(calls$bing, 1L)
  expect_identical(calls$google, 0L)
  expect_identical(calls$yandex, 0L)
  expect_identical(x$results$matcher_backend, "bing")
  expect_identical(x$results$url_decision, "disallow")
  expect_identical(
    x$results$matched_rule_value_raw[[1L]], charToRaw("/private")
  )
})

test_that("BD2 Google and Yandex rows never touch the Bing adapter", {
  calls <- new.env(parent = emptyenv())
  calls$bing <- 0L
  testthat::local_mocked_bindings(
    match_bing_v1 = function(bodies, urls, product_tokens) {
      calls$bing <- calls$bing + 1L
      bxe_canned_bing(bodies)
    },
    .package = "robotstxtr"
  )

  # A Google row and a Yandex row in one call: neither may reach match_bing_v1.
  body <- "user-agent: *\ndisallow: /private"
  url <- rep("https://example.com/private", 2L)
  x <- robots_evaluate_text_v1(
    body, url,
    robots_product_token = c("googlebot", "Yandex"),
    robots_policy_ruleset = c("google", "yandex"),
    matcher_backend = c("google", "yandex")
  )

  expect_identical(calls$bing, 0L)
  expect_identical(x$results$matcher_backend, c("google", "yandex"))
})

test_that("BD3 Bing separation is enforced at the registry", {
  registry <- engine_matcher_registry_v1()
  expect_identical(registry$bing$availability, "available")
  expect_type(registry$bing$callable, "closure")
  # The Bing adapter keeps the batch signature the dispatcher invokes, distinct
  # from Google's per-row shape.
  expect_named(
    formals(match_bing_v1),
    c("bodies", "urls", "product_tokens")
  )
  expect_named(
    formals(match_google_v1),
    c("body", "url", "product_token")
  )
})
