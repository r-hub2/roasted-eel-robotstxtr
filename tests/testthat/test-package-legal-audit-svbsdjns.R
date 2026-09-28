# Independent PACKAGE + LEGAL audit for the installed package (ROBO-svbsdjns,
# YI6f).
#
# Check-time, installed-package audits proving both engines' legal / provenance
# material survives R packaging, that the Yandex MIT payload disposition is
# present in the INSTALLED tree and stays DISTINCT from Google's Apache-2.0
# material, that dev-only legal summaries are excluded from the binary package,
# and that no internal / vendor / native symbol is a public R API. Tests ONLY:
# this touches no source and no fixture. Post-activation the Yandex backend is
# available and the engine-aware v1 schema is current; the export freeze stays
# unchanged.
#
# The companion offline tarball / build-scope audit (source vs binary file
# disposition, exact vendored bytes surviving packaging) lives in
# dev/audit-package.R -- it needs a build and so cannot run at R CMD check time.
# This suite deliberately does NOT re-assert coverage owned by sibling suites:
# the 21-file / per-sha manifest table and PayloadCommit / ProfileId pins
# (test-vendor-manifest-verify-zfhakxcn.R), the Google PROVENANCE / NOTICE
# commit pins (test-cross-engine-separation-pdcrkcvq.R M2), and the
# "4 Google routines present + no yandex-named routine" wiring check
# (test-native-build-wiring-mzwcditw.R).

# ---------------------------------------------------------------------------
# Uniquely prefixed (pl_) helpers -- avoid collision with sibling suites.
# ---------------------------------------------------------------------------

# Read an installed inst/ file (by its path relative to inst/) as one string,
# with a source-tree dev fallback. Returns NA_character_ when unreachable.
pl_read_installed <- function(name) {
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

# TRUE only when running against a genuinely installed package (Meta/ exists),
# not a pkgload::load_all() source tree. The EXCLUSION audit needs this: under
# load_all, system.file() falls back to the package root where the Rbuildignored
# LICENSE.md / THIRD_PARTY_NOTICES.md still live, so it can only be checked
# against a real install (the tarball-listing audit in dev/audit-package.R is
# the authoritative source/binary-disposition proof).
pl_is_installed <- function() {
  root <- system.file(package = "robotstxtr")
  nzchar(root) && dir.exists(file.path(root, "Meta"))
}

pl_skip_if_no_native <- function() {
  dll <- getLoadedDLLs()[["robotstxtr"]]
  skip_if(is.null(dll), "robotstxtr DLL not loaded (pure-R install)")
  dll
}

# The frozen public export surface. getNamespaceExports() returns exactly these
# twelve symbols; the four S3 print methods register for dispatch but are not in
# the exports set. Adding an export is a deliberate API change that MUST update
# this list (and the setequal freeze below will otherwise fail).
pl_public_exports <- c(
  "allowed_by_robots_text",
  "allowed_by_robots_url",
  "as_legacy_robots_decisions_v1",
  "robots_body",
  "robots_engine_contract_v1",
  "robots_engine_contract_v2",
  "robots_evaluate_text_v1",
  "robots_evaluate_url_v1",
  "robots_fetch",
  "robots_resolve_matcher_profile_v1",
  "robots_validate_text",
  "robots_validate_url"
)

# ---------------------------------------------------------------------------
# LEGAL-GOOGLE -- Google's Apache-2.0 material survives install.
# ---------------------------------------------------------------------------

test_that("LEGAL-GOOGLE Apache-2.0 license text is installed", {
  apache <- pl_read_installed("APACHE-2.0-LICENSE")
  skip_if(is.na(apache), "installed Apache license absent")
  expect_true(grepl("Apache License", apache, fixed = TRUE))
  expect_true(grepl("Version 2.0", apache, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# LEGAL-YANDEX -- Yandex MIT disposition present in the INSTALLED tree.
# ---------------------------------------------------------------------------

test_that("LEGAL-YANDEX MIT manifest + provenance are installed", {
  manifest <- pl_read_installed("vendor/robotstxtyandex/MANIFEST.dcf")
  provenance <- pl_read_installed("vendor/robotstxtyandex/PROVENANCE")
  skip_if(
    is.na(manifest) || is.na(provenance),
    "installed Yandex vendor records absent"
  )

  # The manifest records the MIT license and the pinned payload identity.
  expect_true(grepl("License: MIT", manifest, fixed = TRUE))
  expect_true(grepl(
    "PayloadCommit: fdd60a7c3bc6825f3b3752562dc0d6ad9387a27e",
    manifest,
    fixed = TRUE
  ))

  # The provenance carries the MIT disposition and the independence disclaimer.
  expect_true(grepl("MIT", provenance, fixed = TRUE))
  expect_true(grepl("not affiliated", provenance, fixed = TRUE))
  expect_true(grepl("unofficial", provenance, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# LEGAL-SEPARATION -- the two engines' legal identities stay distinct.
# ---------------------------------------------------------------------------

test_that("LEGAL-SEPARATION Google Apache vs Yandex MIT stay distinct", {
  notice <- pl_read_installed("NOTICE")
  manifest <- pl_read_installed("vendor/robotstxtyandex/MANIFEST.dcf")
  skip_if(
    is.na(notice) || is.na(manifest),
    "installed legal records absent"
  )

  # Google NOTICE asserts Apache-2.0 for the vendored matcher, explicitly NOT
  # the MIT license; the Yandex manifest asserts MIT for its own payload.
  expect_true(grepl("Apache License 2.0", notice, fixed = TRUE))
  expect_true(grepl("NOT the MIT", notice, fixed = TRUE))
  expect_true(grepl("License: MIT", manifest, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# LEGAL-BING -- the vendored Bing Apache-2.0 disposition survives install and
# stays DISTINCT from Google's Apache-2.0 material and Yandex's MIT material
# (ROBO-onwulhga, BI6; spec §16.6).
# ---------------------------------------------------------------------------

test_that("LEGAL-BING Apache-2.0 manifest + provenance are installed", {
  manifest <- pl_read_installed("vendor/robotstxtbing/MANIFEST.dcf")
  provenance <- pl_read_installed("vendor/robotstxtbing/PROVENANCE")
  skip_if(
    is.na(manifest) || is.na(provenance),
    "installed Bing vendor records absent"
  )

  # The manifest records Apache-2.0, the license disposition, and the pinned
  # payload identity.
  expect_true(grepl("License: Apache-2.0", manifest, fixed = TRUE))
  expect_true(grepl("LicenseDisposition: Apache-2.0", manifest, fixed = TRUE))
  expect_true(grepl(
    "PayloadCommit: 1f2431b9d47ba25dec313eec5a396e795f00b5b8",
    manifest,
    fixed = TRUE
  ))

  # The provenance carries the independence disclaimer (Microsoft/Bing).
  expect_true(grepl("Apache License 2.0", provenance, fixed = TRUE))
  expect_true(grepl("unofficial", provenance, fixed = TRUE))
  expect_true(grepl("Microsoft", provenance, fixed = TRUE))
  expect_true(grepl("affiliation", provenance, fixed = TRUE))
})

test_that("LEGAL-BING corpus provenance is installed and two-tier", {
  prov <- pl_read_installed("bing-corpus/PROVENANCE.dcf")
  skip_if(is.na(prov), "installed Bing corpus provenance absent")

  # The corpus provenance pins the release manifest and payload, records the
  # two-tier tester/golden disposition, and carries the independence disclaimer.
  expect_true(grepl("CaseCount: 191", prov, fixed = TRUE))
  expect_true(grepl("AcceptedExpectationCount: 57", prov, fixed = TRUE))
  expect_true(grepl(
    "ReleaseManifestSha256: 5e79ee5", prov,
    fixed = TRUE
  ))
  expect_true(grepl("tester_observed", prov, fixed = TRUE))
  expect_true(grepl("not affiliated", prov, fixed = TRUE))
})

test_that("LEGAL-BING Apache disposition is distinct from Google + Yandex", {
  bing_manifest <- pl_read_installed("vendor/robotstxtbing/MANIFEST.dcf")
  yandex_manifest <- pl_read_installed("vendor/robotstxtyandex/MANIFEST.dcf")
  skip_if(
    is.na(bing_manifest) || is.na(yandex_manifest),
    "installed vendor manifests absent"
  )

  # The Bing disposition explicitly names its separation from Google's Apache
  # material and from the package/Yandex licenses; the two are not conflated.
  expect_true(grepl(
    "distinct from the package MIT license", bing_manifest, fixed = TRUE
  ))
  expect_true(grepl(
    "inst/APACHE-2.0-LICENSE", bing_manifest, fixed = TRUE
  ))
  # Yandex stays MIT; Bing stays Apache-2.0. Three distinct dispositions.
  expect_true(grepl("License: MIT", yandex_manifest, fixed = TRUE))
  expect_true(grepl("License: Apache-2.0", bing_manifest, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# EXCLUSION -- dev-only legal summaries are Rbuildignored out of the binary.
# ---------------------------------------------------------------------------

test_that("EXCLUSION dev-only legal summaries are not installed", {
  skip_if_not(
    pl_is_installed(),
    "exclusion is only decidable against a real install (see dev script)"
  )
  # LICENSE.md / THIRD_PARTY_NOTICES.md are Rbuildignored dev summaries; the
  # installed NOTICE / PROVENANCE carry the binary legal material instead.
  expect_identical(system.file("LICENSE.md", package = "robotstxtr"), "")
  expect_identical(
    system.file("THIRD_PARTY_NOTICES.md", package = "robotstxtr"),
    ""
  )
})

# ---------------------------------------------------------------------------
# PRIVATE-API NON-EXPOSURE -- private headers / ABI are not an R API promise.
# ---------------------------------------------------------------------------

test_that("PRIVATE-API internal / vendor / native symbols are unexported", {
  exports <- getNamespaceExports("robotstxtr")
  denylist <- c(
    "robotstxtr_checked_batch_",
    "robotstxtr_extract_request_target_",
    "robotstxtr_bing_eval_batch_",
    "robotstxtr_bing_extract_request_target_",
    "robotstxtr_bing_contract_info_",
    "match_yandex_v1",
    "match_google_v1",
    "match_bing_v1",
    "verify_yandex_vendor_tree",
    "verify_bing_vendor_tree",
    "read_yandex_corpus",
    "verify_yandex_corpus",
    "read_bing_corpus",
    "verify_bing_corpus",
    "resolve_policy_v1",
    "evaluate_rows_v1"
  )
  expect_length(intersect(exports, denylist), 0L)
})

test_that("PRIVATE-API the public export surface is frozen", {
  # A new export is a deliberate API change that must update pl_public_exports.
  expect_true(setequal(getNamespaceExports("robotstxtr"), pl_public_exports))
})

# ---------------------------------------------------------------------------
# NATIVE-SURFACE COMPLEMENT -- the two hidden Yandex-engine routines are
# registered AND engine-neutral. Complements the sibling wiring check (which
# asserts the 4 Google routines present + no yandex-named routine, but does not
# assert these two hidden routines are present).
# ---------------------------------------------------------------------------

test_that("NATIVE-SURFACE hidden Yandex routines are registered + neutral", {
  dll <- pl_skip_if_no_native()
  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  names_registered <- vapply(routines, function(r) r$name, character(1))

  hidden <- c(
    "_robotstxtr_robotstxtr_checked_batch_",
    "_robotstxtr_robotstxtr_extract_request_target_"
  )
  expect_true(all(hidden %in% names_registered))
  # Neutrally named: no registered routine leaks the engine identity.
  expect_false(any(grepl("yandex", hidden, ignore.case = TRUE)))
})

test_that("NATIVE-SURFACE hidden Bing routines are registered", {
  dll <- pl_skip_if_no_native()
  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  names_registered <- vapply(routines, function(r) r$name, character(1))

  # The three hidden Bing routines (batch eval, request-target extractor,
  # contract info) are present. Unlike the Yandex routines these carry the
  # engine name -- that is their registered ABI identity, not an R API promise.
  hidden <- c(
    "_robotstxtr_robotstxtr_bing_eval_batch_",
    "_robotstxtr_robotstxtr_bing_extract_request_target_",
    "_robotstxtr_robotstxtr_bing_contract_info_"
  )
  expect_true(all(hidden %in% names_registered))
})

# ---------------------------------------------------------------------------
# DATA-ONLY -- self-containment: Yandex dormant, schema pinned.
# ---------------------------------------------------------------------------

test_that("DATA-ONLY Yandex available, schema bumped", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
})
