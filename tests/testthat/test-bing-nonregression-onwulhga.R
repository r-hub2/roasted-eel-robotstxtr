# Google + Yandex non-regression after Bing activation (ROBO-onwulhga, BI6;
# spec §16.5).
#
# Explicit, high-signal assertions that adding and activating the Bing backend
# left Google and Yandex byte- and behavior-UNCHANGED: their vendored trees
# still match their frozen manifests, their matcher revisions and identities are
# unchanged, only the NEW v2 accessor reports the v2 contract id, the 140-case
# Yandex corpus still verifies clean, and a
# Google/Yandex request neither loads nor invokes the Bing adapter.
#
# Tests ONLY -- no source, no fixture touched. This suite deliberately keeps to
# NON-REGRESSION cross-checks; the full per-file manifest table and per-engine
# conformance corpora are owned by their sibling suites.

nr_skip_if_no_sha256 <- function() {
  skip_if_not(
    exists("sha256sum", where = asNamespace("tools"), inherits = FALSE),
    "tools::sha256sum() unavailable (needs R >= 4.5.0)"
  )
}

# ---------------------------------------------------------------------------
# VENDORED BYTES -- the Yandex vendored tree is unchanged (byte fidelity).
# ---------------------------------------------------------------------------

nr_yandex_vendor_root <- function() {
  candidates <- c(
    "src/vendor/robotstxtyandex",
    file.path("..", "..", "src", "vendor", "robotstxtyandex"),
    file.path("..", "..", "..", "src", "vendor", "robotstxtyandex")
  )
  hit <- candidates[dir.exists(candidates)]
  if (length(hit) > 0L) hit[[1L]] else NA_character_
}

test_that("NR-VENDOR the Yandex vendored tree still matches its manifest", {
  nr_skip_if_no_sha256()
  root <- nr_yandex_vendor_root()
  # src/ is present in a source checkout, not in an installed package tree.
  skip_if(is.na(root), "vendored src tree not present (installed pkg)")
  res <- verify_yandex_vendor_tree(root = root)
  expect_true(res$ok)
  expect_length(res$missing, 0L)
  expect_length(res$extra, 0L)
  expect_length(res$mismatched, 0L)
})

# ---------------------------------------------------------------------------
# IDENTITY + SCHEMA -- Google/Yandex revisions and the v1 schema are pinned;
# only the new v2 accessor reports the v2 contract.
# ---------------------------------------------------------------------------

test_that("NR-IDENTITY Google + Yandex revisions and v1 schema are unchanged", {
  registry <- engine_matcher_registry_v1()
  expect_identical(
    registry$google$revision,
    "google-robotstxt-22b355ff855419e6a3ff8ff09c0ad7fdb17116f9"
  )
  expect_identical(
    registry$yandex$revision, robotstxtr:::yandex_matcher_revision_v1()
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")

  # The v1 accessor still reports the v1 contract id with the pre-Bing
  # Google/Yandex revisions. Its schema_revision is NOT frozen forever: §16.5
  # required it to survive the BING activation unchanged, which it did. It later
  # advanced to 2026-08-25.1 for the additive bounded-profile publication
  # (ROBO-qgxekgph), which touched no Google or Yandex matcher behaviour.
  v1 <- robots_engine_contract_v1()
  expect_identical(v1$contract_id, "robotstxtr.engine-aware/v1")
  expect_identical(v1$schema_revision, "2026-08-25.1")
  expect_match(v1$matcher_revisions[["google"]], "22b355ff")
  expect_identical(
    v1$matcher_revisions[["yandex"]],
    robotstxtr:::yandex_matcher_revision_v1()
  )
})

test_that("NR-V2 only the new v2 accessor reports the v2 contract", {
  v2 <- robots_engine_contract_v2()
  expect_identical(v2$contract_id, "robotstxtr.engine-aware/v2")
  expect_identical(v2$schema_revision, "2026-08-25.1")
  # Adding v2 did not mutate the v1 accessor's contract_id. Both accessors carry
  # the same schema_revision string today because the bounded-profile addition
  # landed in both on one date; a revision is only ever meaningful alongside its
  # contract_id, which is what actually separates the two surfaces.
  v1 <- robots_engine_contract_v1()
  expect_identical(v1$contract_id, "robotstxtr.engine-aware/v1")
  expect_identical(v1$schema_revision, "2026-08-25.1")
  # Google/Yandex revisions match across both accessors.
  expect_identical(
    v1$matcher_revisions[["yandex"]], v2$matcher_revisions[["yandex"]]
  )
  expect_identical(
    v1$matcher_revisions[["google"]], v2$matcher_revisions[["google"]]
  )
})

# ---------------------------------------------------------------------------
# YANDEX CORPUS -- the 140-case Yandex projection is untouched by BI6.
# ---------------------------------------------------------------------------

test_that("NR-CORPUS the Yandex corpus still verifies clean and complete", {
  skip_if_not_installed("jsonlite")
  nr_skip_if_no_sha256()
  res <- verify_yandex_corpus()
  expect_true(res$ok)
  expect_equal(res$n_records, 140L)
  expect_equal(res$n_bodies, 26L)
})

# ---------------------------------------------------------------------------
# NO BING INVOCATION -- Google and Yandex requests never call the Bing adapter.
# ---------------------------------------------------------------------------

test_that("NR-DISPATCH Google/Yandex requests never invoke Bing", {
  bing_calls <- new.env(parent = emptyenv())
  bing_calls$n <- 0L
  testthat::local_mocked_bindings(
    match_bing_v1 = function(bodies, urls, product_tokens) {
      bing_calls$n <- bing_calls$n + 1L
      stop("Bing adapter must not be reached by a Google/Yandex request.")
    },
    .package = "robotstxtr"
  )

  body <- "user-agent: *\ndisallow: /private"
  # A Google row and a Yandex row in one call, plus a Google-only call.
  x <- robots_evaluate_text_v1(
    body, rep("https://example.com/private", 2L),
    robots_product_token = c("googlebot", "Yandex"),
    robots_policy_ruleset = c("google", "yandex"),
    matcher_backend = c("google", "yandex")
  )
  g <- robots_evaluate_text_v1(
    body, "https://example.com/private", "googlebot", "google", "google"
  )

  expect_identical(bing_calls$n, 0L)
  expect_identical(x$results$matcher_backend, c("google", "yandex"))
  expect_identical(g$results$matcher_backend, "google")
})
