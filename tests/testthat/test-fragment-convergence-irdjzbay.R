# ROBO-irdjzbay: every entry point resolves a fragment-bearing URL to the SAME
# matched path.
#
# Three INDEPENDENT fragment implementations exist in this package:
#
#   1. src/bing_binding.cpp  — the Bing adapter's lexical extractor finds the
#      first '#' and keeps everything before it, so the byte-transparent core
#      never sees a fragment (design/robotstxtbing-integration-v2-spec.md,
#      BI-V2-TARGET: URL -> origin-form conversion lives in the adapter so the
#      core stays source-preserving).
#   2. src/yandex_target_binding.cpp — the same shape, separately implemented.
#   3. R/allowed_by_robots_url.R and R/allowed_by_robots_text.R — the URL is
#      passed VERBATIM and Google's own GetPathParamsQuery (src/robots.cc)
#      strips the fragment internally.
#
# Their agreement is deliberate design, but until now it was inference from
# reading three files rather than an assertion. The edge where they could
# plausibly disagree: Google returns "/" OUTRIGHT when the '#' precedes the path
# start (src/robots.cc:205) instead of the truncation result, while the
# package-owned extractors truncate first and then derive the path.
#
# An R user calling three functions on one URL and getting three answers is the
# real-life failure this file rules out.
#
# HOW GOOGLE'S MATCHED PATH IS OBSERVED. GetPathParamsQuery() has no R binding,
# so the path is probed with an END-ANCHORED pattern: "disallow: <path>$" hits
# if and only if the matched path equals <path> exactly (prefix match from the
# start, '$' pinning the end, no wildcard in between). Every row is therefore
# asserted twice -- the exact pattern must DISALLOW and a near-miss pattern must
# not -- so a probe cannot pass because everything happens to be disallowed. A
# literal '#' can never appear in a pattern (the parser strips comments), which
# is exactly why the leak is detected as a FAILED end-anchored match rather than
# by spelling the leaked path out.

# --- The fragment shape table ------------------------------------------------

# Each row: the URL a caller supplies, and the request path every entry point
# must resolve it to.
fc_shapes <- list(
  list(label = "'#' after a path",
       url = "http://a/plain#secret", path = "/plain"),
  list(label = "'#' before the path start (Google's early-return edge)",
       url = "http://example.com#foo", path = "/"),
  list(label = "'#' right after the authority, trailing slash",
       url = "http://a/#foo", path = "/"),
  list(label = "empty fragment",
       url = "http://a/p#", path = "/p"),
  list(label = "'#' inside a query",
       url = "http://a/p?q=1#frag", path = "/p?q=1"),
  list(label = "multiple '#'",
       url = "http://a/p#one#two", path = "/p"),
  list(label = "'%23' in the path is NOT a fragment",
       url = "http://a/p%23name", path = "/p%23name"),
  list(label = "'#' before the path start, query inside the fragment",
       url = "http://a#?q=1", path = "/")
)

fc_shape_urls <- vapply(fc_shapes, `[[`, character(1L), "url")
fc_shape_paths <- vapply(fc_shapes, `[[`, character(1L), "path")

# --- Test helpers (local to this file) ---------------------------------------

# An end-anchored one-rule document: matches iff the matched path is exactly
# `path`. `fc_near_miss()` appends a byte, so it must never match the same path.
fc_anchored_body <- function(path) {
  sprintf("user-agent: *\ndisallow: %s$\n", path)
}

fc_near_miss <- function(path) {
  paste0(path, "x")
}

# The three engine triples as a caller supplies them: product token, policy
# ruleset, matcher backend. Bing's documented policy ruleset is
# "assumed_rfc9309"; "bing" is a matcher backend, not a ruleset.
fc_engine_triples <- list(
  google = c("Googlebot", "google", "google"),
  bing = c("bingbot", "assumed_rfc9309", "bing"),
  yandex = c("Yandex", "yandex", "yandex")
)

fc_engine_decision <- function(body, url, triple) {
  robots_evaluate_text_v1(
    robots_txt = body, url = url, robots_product_token = triple[[1L]],
    robots_policy_ruleset = triple[[2L]], matcher_backend = triple[[3L]]
  )$results
}

fc_mock_router <- function(routes) {
  function(req) {
    handler <- routes[[req$url]]
    if (is.null(handler)) {
      stop(sprintf("unexpected request URL in mock: %s", req$url))
    }
    handler(req)
  }
}

fc_ok_body <- function(body) {
  force(body)
  function(req) {
    httr2::response(
      status_code = 200L, url = req$url,
      headers = list(`content-type` = "text/plain"), body = charToRaw(body)
    )
  }
}

# The Bing and Yandex entry points are native; the Google path is not. Guard the
# same way the sibling engine suites do so a pure-R install skips rather than
# errors.
fc_skip_if_no_native_engines <- function() {
  bing <- tryCatch(robotstxtr_bing_eval_batch_, error = function(e) NULL)
  yandex <- tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)
  bing_target <- tryCatch(
    robotstxtr_bing_extract_request_target_, error = function(e) NULL
  )
  yandex_target <- tryCatch(
    robotstxtr_extract_request_target_, error = function(e) NULL
  )
  skip_if_not(
    is.function(bing) && is.function(yandex) &&
      is.function(bing_target) && is.function(yandex_target),
    "native binding not built (pure-R install)"
  )
}

# --- The two package-owned lexical extractors --------------------------------

test_that("both lexical extractors derive the same path for every shape", {
  fc_skip_if_no_native_engines()
  for (shape in fc_shapes) {
    bing <- robotstxtr_bing_extract_request_target_(shape$url)[[1L]]
    yandex <- robotstxtr_extract_request_target_(shape$url)[[1L]]

    # Byte-exact, so a platform-dependent re-encoding fails rather than passes.
    expect_identical(charToRaw(bing), charToRaw(shape$path), info = shape$label)
    expect_identical(charToRaw(yandex), charToRaw(shape$path),
                     info = shape$label)
    expect_identical(bing, yandex, info = shape$label)
  }
})

# --- Google's internal GetPathParamsQuery ------------------------------------

test_that("Google's own fragment stripping lands on the same path", {
  for (shape in fc_shapes) {
    exact <- allowed_by_robots_text(
      fc_anchored_body(shape$path), shape$url, "bot"
    )$results
    control <- allowed_by_robots_text(
      fc_anchored_body(fc_near_miss(shape$path)), shape$url, "bot"
    )$results

    expect_false(exact$allowed, info = shape$label)
    expect_identical(exact$decision_source, "rule_disallow", info = shape$label)
    # The rule the matcher reports is the anchored path itself, so the assertion
    # names the matched path rather than just its allow/deny consequence.
    expect_identical(exact$matched_rule_value, paste0(shape$path, "$"),
                     info = shape$label)
    expect_true(control$allowed, info = shape$label)
  }
})

# --- Convergence across all three matcher backends ---------------------------

test_that("all three matcher backends agree on the matched path", {
  fc_skip_if_no_native_engines()
  for (shape in fc_shapes) {
    exact <- vapply(
      fc_engine_triples,
      function(triple) {
        fc_engine_decision(
          fc_anchored_body(shape$path), shape$url, triple
        )$url_decision
      },
      character(1L)
    )
    control <- vapply(
      fc_engine_triples,
      function(triple) {
        fc_engine_decision(
          fc_anchored_body(fc_near_miss(shape$path)), shape$url, triple
        )$url_decision
      },
      character(1L)
    )

    expect_identical(
      exact, c(google = "disallow", bing = "disallow", yandex = "disallow"),
      info = shape$label
    )
    expect_identical(
      control, c(google = "allow", bing = "allow", yandex = "allow"),
      info = shape$label
    )
  }
})

# --- '%23' is a path byte on every path, never a fragment --------------------

test_that("'%23' reaches every matcher intact and truncates nothing", {
  fc_skip_if_no_native_engines()
  url <- "http://a/p%23name"

  # Neither extractor decodes it to '#' and cuts there.
  expect_identical(robotstxtr_bing_extract_request_target_(url)[[1L]],
                   "/p%23name")
  expect_identical(robotstxtr_extract_request_target_(url)[[1L]], "/p%23name")

  # No backend resolves the path to the truncated "/p" ...
  truncated <- vapply(
    fc_engine_triples,
    function(triple) {
      fc_engine_decision(fc_anchored_body("/p"), url, triple)$url_decision
    },
    character(1L)
  )
  expect_identical(
    truncated, c(google = "allow", bing = "allow", yandex = "allow")
  )

  # ... and the escaped bytes arrive spelled exactly as supplied.
  intact <- vapply(
    fc_engine_triples,
    function(triple) {
      fc_engine_decision(
        fc_anchored_body("/p%23name"), url, triple
      )$url_decision
    },
    character(1L)
  )
  expect_identical(
    intact, c(google = "disallow", bing = "disallow", yandex = "disallow")
  )
})

# --- The fetched entry point: allowed_by_robots_url() ------------------------

test_that("the fetched path resolves every shape alike, URL kept verbatim", {
  # One document carrying an end-anchored rule per distinct expected path.
  body <- paste0(
    "user-agent: *\n",
    paste0("disallow: ", unique(fc_shape_paths), "$", collapse = "\n"),
    "\n"
  )
  httr2::local_mocked_responses(fc_mock_router(list(
    "http://a/robots.txt" = fc_ok_body(body),
    "http://example.com/robots.txt" = fc_ok_body(body)
  )))
  x <- allowed_by_robots_url(fc_shape_urls, "bot")

  expect_identical(x$results$allowed, rep(FALSE, length(fc_shape_urls)))
  expect_identical(x$results$matched_rule_value, paste0(fc_shape_paths, "$"))
  # The fragment is stripped only on the way INTO the matcher; the reported URL
  # is the caller's string, byte for byte.
  expect_identical(x$results$url, fc_shape_urls)
})
