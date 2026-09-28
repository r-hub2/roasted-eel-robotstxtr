# OSS Index dependency vulnerability audit (oysteR / Sonatype).
#
# `oysteR::audit_description()` resolves the installed DESCRIPTION and audits
# robotstxtr's hard dependencies against the Sonatype OSS Index. It is a
# network test that requires OSS Index credentials (OSSINDEX_USER /
# OSSINDEX_TOKEN): the API rejects unauthenticated requests with HTTP 401.
#
# Scope: hard dependencies only -- `Depends` + `Imports`, never `Suggests`.
#
# `oysteR::expect_secure()` audits `Suggests` as well, which drags in the
# recursive dependency trees of the dev tooling -- including oysteR's own. A
# vulnerability in the auditor is not a vulnerability in what robotstxtr makes
# users install, and a package's security posture is the latter. Measured
# 2026-09-10: `Depends` + `Imports` audits 25 packages, the Suggests-inclusive
# scope 84. `audit_description()` is called directly because only it exposes
# `fields`; `expect_secure()` sets a CRAN mirror internally and
# `audit_description()` does not, hence the explicit `repos` option.
#
# The allow-list and rules A/B/C live in helper-security.R, next to the data
# they govern. Read that file before adding a row.
#
# WHERE THIS RUNS, AND WHY THE PRECONDITIONS ARE NOT ALWAYS SKIPS.
#
# Everywhere ordinary -- a local `testthat::test_local()`, a plain
# `R CMD check --as-cran` -- a missing precondition is a skip. That is right: a
# developer without OSS Index credentials is not a security regression.
#
# A job that exists to run this audit is different. If it lacks credentials,
# `skip_if()` fires, the job reports success, and nothing has been audited --
# the failure `PUNY-rsxtbbln` records in punycoder. A green job that audited
# nothing is worse than no job, because it answers the question it was never
# asked. So under OSSINDEX_AUDIT_REQUIRED=true every precondition below
# becomes a hard failure with a message naming what is missing.
#
# robotstxtr has no such job today: .gitlab-ci.yml carries no
# `security-audit` job and sets the flag nowhere, so in every current context
# the preconditions stay skips. The flag path is here so that a credentialed
# job, when one is added, sets OSSINDEX_AUDIT_REQUIRED=true and cannot go
# green on nothing (SEOR-fftbjnpl).
#
# NOTE ON THE PRE-PUSH HOOK. Measured 2026-09-24, the audit does not run on
# push. robotstxtr's pre-push gate is `dev/verify.sh`: lint, a docs-drift
# check, and `rcmdcheck::rcmdcheck(args = "--as-cran")` on a `git archive
# HEAD` export. No stage calls `testthat::test_local()`, and rcmdcheck does not
# set NOT_CRAN, so `skip_on_cran()` below fires before the credential guard is
# reached, even though `~/.Renviron` puts the credentials in scope inside R.

test_that("the OSS Index allow-list is well formed", {
  # No `expect_gt(length(oss_index_allowlist), 0)` here, although this repo
  # has rows. Emptiness is not a malformation: when curl ships a fix, rule B
  # deletes both rows and an empty list is the correct state. The other way
  # round -- rows removed while the advisories are still reported -- rule A
  # catches in the audit below, so a non-empty assertion would guard nothing
  # and would have to be remembered and deleted along with the last row.
  expect_equal(oss_index_allowlist_violations(oss_index_allowlist), character())
})

test_that("the allow-list validator rejects rows that are not decisions", {
  sound <- list(
    id = "CVE-2026-18924",
    package = "curl",
    version_seen = "8.0.0",
    review = as.Date("2026-12-01"),
    reason = paste(
      "A rationale long enough to be an argument rather than a placeholder,",
      "naming the advisory, the exposure assessed and why it is accepted."
    )
  )
  expect_equal(oss_index_allowlist_violations(list(sound)), character())

  broken <- function(field, value) {
    row <- sound
    row[[field]] <- value
    oss_index_allowlist_violations(list(row))
  }

  expect_match(
    broken("id", "GHSA-xxxx"),
    "single CVE identifier",
    fixed = TRUE
  )
  expect_match(broken("package", ""), "single package name", fixed = TRUE)
  expect_match(
    broken("version_seen", "not-a-version"),
    "parseable version",
    fixed = TRUE
  )
  expect_match(broken("review", "2026-12-01"), "single Date", fixed = TRUE)
  expect_match(
    broken("reason", "unfixable"),
    "too short to be an argument",
    fixed = TRUE
  )

  expect_match(
    oss_index_allowlist_violations(list(sound[-5])),
    "missing field(s): reason",
    fixed = TRUE
  )
  expect_match(
    oss_index_allowlist_violations(list(c(sound, list(owner = "me")))),
    "unknown field(s): owner",
    fixed = TRUE
  )
  expect_match(
    oss_index_allowlist_violations(list(sound, sound)),
    "duplicate allow-list id",
    fixed = TRUE
  )
})

test_that("hard dependencies report only allow-listed OSS Index advisories", {
  # The dedicated, credentialed audit job. A precondition it cannot meet is a
  # failure there, never a skip -- see the header.
  required <- identical(Sys.getenv("OSSINDEX_AUDIT_REQUIRED"), "true")
  no_credentials <- Sys.getenv("OSSINDEX_USER") == "" ||
    Sys.getenv("OSSINDEX_TOKEN") == ""

  if (required) {
    if (!requireNamespace("oysteR", quietly = TRUE)) {
      stop(
        "OSSINDEX_AUDIT_REQUIRED is set but {oysteR} is not installed, so ",
        "this job cannot audit anything. Install it or unset the flag; do ",
        "not let the job report success."
      )
    }
    if (no_credentials) {
      stop(
        "OSSINDEX_AUDIT_REQUIRED is set but OSSINDEX_USER / OSSINDEX_TOKEN ",
        "are absent, so OSS Index would reject every request with HTTP 401 ",
        "and this job would report success having audited nothing. Add both ",
        "as CI/CD variables (project Settings > CI/CD > Variables) to the ",
        "job that sets the flag."
      )
    }
    # Deliberately no skip_if_offline() on this path: a network the job cannot
    # reach is the same vacuous green as a credential it does not have, so let
    # the audit attempt the call and fail on the transport error.
  } else {
    skip_on_cran()
    skip_if_not_installed("oysteR")
    skip_if_offline()
    skip_if(
      no_credentials,
      "OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN) not set"
    )
  }

  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cran.rstudio.com"))

  audit <- oysteR::audit_description(
    dirname(system.file("DESCRIPTION", package = "robotstxtr")),
    fields = c("Depends", "Imports"),
    verbose = FALSE
  )
  found <- oss_index_reported(audit)
  allowed <- vapply(oss_index_allowlist, function(row) row$id, character(1))

  # An audit that resolved nothing is not a clean audit. Without this an empty
  # result satisfies rule A vacuously, which is the same green-on-nothing
  # failure the credential guard above exists to stop. (Rule B would also fail
  # here while this repo has rows, but only by accident of the rows existing.)
  expect_gt(nrow(audit), 0)

  # Rule A -- an advisory reported and not allow-listed.
  expect_equal(sort(setdiff(found$id, allowed)), character())

  # Rule B -- an allow-listed advisory no longer reported. The list may not
  # over-permit, so a row that has outlived its justification fails here.
  expect_equal(sort(setdiff(allowed, found$id)), character())

  # Rule C -- drift warns, never fails. See helper-security.R.
  for (row in oss_index_allowlist) {
    if (Sys.Date() > row$review) {
      warning(
        sprintf(
          "OSS Index allow-list row %s is past its %s review date.",
          row$id,
          format(row$review)
        ),
        call. = FALSE
      )
    }
    hit <- found[found$id == row$id, ]
    if (
      nrow(hit) > 0 &&
        package_version(hit$version[1]) > package_version(row$version_seen)
    ) {
      warning(
        sprintf(
          paste(
            "OSS Index allow-list row %s was written against %s %s;",
            "the audit now reports %s. Re-read the advisory."
          ),
          row$id,
          row$package,
          row$version_seen,
          hit$version[1]
        ),
        call. = FALSE
      )
    }
  }
})
