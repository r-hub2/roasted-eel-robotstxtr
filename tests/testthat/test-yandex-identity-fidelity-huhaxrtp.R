# Identity / provenance fidelity for the vendored Yandex payload (ROBO-huhaxrtp,
# YI6e). Check-time, base-R DCF reads only: no native binding, no compilation,
# no jsonlite. Proves the frozen identity constants are pinned to exact values,
# that the serialized MatcherRevision is exactly its components, and -- the new
# fidelity -- that the compact corpus PROVENANCE and the vendored payload
# MANIFEST declare ONE byte-identical pinned identity. The committed fixtures
# are never mutated: the fail-closed check operates on tempdir copies.

# Uniquely prefixed (hi_) helpers -- avoid collision with sibling suites.
# ---------------------------------------------------------------------------

# Resolve an inst/-relative path with a source-tree dev fallback (matches the
# pl_/xe_ helpers in the sibling suites). Returns "" when unreachable.
hi_dcf_path <- function(name) {
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
  path
}

# Read the single identity/header record of a DCF (the paragraph with no File:
# line) as a named character vector, dropping fields absent from that record.
hi_dcf_header <- function(path) {
  dcf <- read.dcf(path)
  if ("File" %in% colnames(dcf)) {
    header_rows <- which(is.na(dcf[, "File"]))
    rec <- dcf[header_rows[[1L]], ]
  } else {
    rec <- dcf[1L, ]
  }
  rec[!is.na(rec)]
}

# The six identity fields shared between the vendor MANIFEST header and the
# corpus PROVENANCE header (confirmed present in both by name).
hi_shared_fields <- c(
  "PayloadCommit", "ProfileId", "AcceptedCorpusRevision",
  "ProfileSourceRevision", "EvidenceSnapshot", "MatcherRevision"
)

# TRUE iff every shared identity field is present in both headers and byte
# identical between them. Fails closed (FALSE) on any missing field.
hi_identity_consistent <- function(manifest_path, provenance_path) {
  m <- hi_dcf_header(manifest_path)
  p <- hi_dcf_header(provenance_path)
  if (!all(hi_shared_fields %in% names(m)) ||
        !all(hi_shared_fields %in% names(p))) {
    return(FALSE)
  }
  all(vapply(
    hi_shared_fields,
    function(f) identical(unname(m[[f]]), unname(p[[f]])),
    logical(1)
  ))
}

# The exact frozen identity constants, composed so no line exceeds 80 chars.
hi_payload_commit <- "fdd60a7c3bc6825f3b3752562dc0d6ad9387a27e"
hi_library_version <- "0.2.0"
hi_profile_id <- "yandex-0.1.0"
hi_corpus_rev <- "337b9f3b886a92d6dc08c2fce84228d0cd6b801a"
hi_profile_source_rev <- "337b9f3b886a92d6dc08c2fce84228d0cd6b801a"
hi_evidence <-
  "9d69d361db81e7d236562dc056b41865da33d467d06f316e2c9a20988e007c96"
hi_archive_sha <-
  "366dd57e2319f572624bdddffc6ccc1e51f6358d1a011453aebb4541de24bd90"

# Compose the expected MatcherRevision from its parts. Doubling as the
# composition oracle: any drift in a component breaks this serialized string.
hi_matcher_rev <- paste0(
  "robotstxtyandex/", hi_library_version,
  "+payload.", hi_payload_commit,
  ";profile=", hi_profile_id,
  ";corpus=", hi_corpus_rev,
  ";evidence=", hi_evidence,
  ";profile-source=", hi_profile_source_rev
)

hi_manifest_path <- function() {
  hi_dcf_path("vendor/robotstxtyandex/MANIFEST.dcf")
}

hi_provenance_path <- function() {
  hi_dcf_path("yandex-corpus/PROVENANCE.dcf")
}

# ---------------------------------------------------------------------------
# IDENTITY-PIN -- the shipped MANIFEST header equals the exact frozen constants.
# ---------------------------------------------------------------------------

test_that("shipped manifest pins all eight identity constants exactly", {
  path <- hi_manifest_path()
  skip_if(!nzchar(path), "vendor MANIFEST.dcf unreachable")
  header <- hi_dcf_header(path)

  expect_identical(unname(header[["LibraryVersion"]]), hi_library_version)
  expect_identical(unname(header[["PayloadCommit"]]), hi_payload_commit)
  expect_identical(unname(header[["ArchiveSha256"]]), hi_archive_sha)
  expect_identical(unname(header[["ProfileId"]]), hi_profile_id)
  expect_identical(unname(header[["AcceptedCorpusRevision"]]), hi_corpus_rev)
  expect_identical(unname(header[["EvidenceSnapshot"]]), hi_evidence)
  expect_identical(
    unname(header[["ProfileSourceRevision"]]), hi_profile_source_rev
  )
  expect_identical(unname(header[["MatcherRevision"]]), hi_matcher_rev)
})

# ---------------------------------------------------------------------------
# MATCHER-REV COMPOSITION -- the serialized revision is exactly its components.
# ---------------------------------------------------------------------------

test_that("MatcherRevision equals the string composed from its parts", {
  path <- hi_manifest_path()
  skip_if(!nzchar(path), "vendor MANIFEST.dcf unreachable")
  header <- hi_dcf_header(path)

  composed <- paste0(
    "robotstxtyandex/", unname(header[["LibraryVersion"]]),
    "+payload.", unname(header[["PayloadCommit"]]),
    ";profile=", unname(header[["ProfileId"]]),
    ";corpus=", unname(header[["AcceptedCorpusRevision"]]),
    ";evidence=", unname(header[["EvidenceSnapshot"]]),
    ";profile-source=", unname(header[["ProfileSourceRevision"]])
  )
  expect_identical(unname(header[["MatcherRevision"]]), composed)
})

# ---------------------------------------------------------------------------
# CROSS-CHECK -- the corpus PROVENANCE and the vendor MANIFEST declare ONE
# byte-identical pinned identity across the six shared fields.
# ---------------------------------------------------------------------------

test_that("corpus PROVENANCE identity is byte-identical to the manifest", {
  manifest_path <- hi_manifest_path()
  provenance_path <- hi_provenance_path()
  skip_if(!nzchar(manifest_path), "vendor MANIFEST.dcf unreachable")
  skip_if(!nzchar(provenance_path), "corpus PROVENANCE.dcf unreachable")

  m <- hi_dcf_header(manifest_path)
  p <- hi_dcf_header(provenance_path)
  for (field in hi_shared_fields) {
    expect_true(field %in% names(p))
    expect_identical(unname(p[[field]]), unname(m[[field]]))
  }
})

# ---------------------------------------------------------------------------
# NO-LEAK / FROZEN -- the runtime never surfaces a live matcher revision, and
# reading the pinned identity is deterministic (no clock/git substitution).
# ---------------------------------------------------------------------------

test_that("Yandex revision is the composed frozen MatcherRevision", {
  registry <- engine_matcher_registry_v1()
  # Post-activation the revision is the composed, byte-frozen MatcherRevision --
  # never a live/clock/git substitution -- so it equals the composition oracle.
  expect_identical(registry$yandex$revision, hi_matcher_rev)
  expect_type(registry$yandex$callable, "closure")
  expect_false(identical(registry$yandex$revision, "capability-unavailable-v1"))
})

test_that("reading the pinned manifest identity is deterministic", {
  path <- hi_manifest_path()
  skip_if(!nzchar(path), "vendor MANIFEST.dcf unreachable")
  first <- hi_dcf_header(path)[hi_shared_fields]
  second <- hi_dcf_header(path)[hi_shared_fields]
  expect_identical(first, second)
})

# ---------------------------------------------------------------------------
# MUTATION FAIL-CLOSED -- the new cross-check rejects a drifted corpus identity.
# Operates on tempdir copies; the shipped fixtures are never mutated.
# ---------------------------------------------------------------------------

test_that("identity cross-check fails closed on a mutated corpus commit", {
  manifest_path <- hi_manifest_path()
  provenance_path <- hi_provenance_path()
  skip_if(!nzchar(manifest_path), "vendor MANIFEST.dcf unreachable")
  skip_if(!nzchar(provenance_path), "corpus PROVENANCE.dcf unreachable")

  # The shipped pair agrees.
  expect_true(hi_identity_consistent(manifest_path, provenance_path))

  tmp <- tempfile("yandex-identity-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  manifest_copy <- file.path(tmp, "MANIFEST.dcf")
  provenance_copy <- file.path(tmp, "PROVENANCE.dcf")
  file.copy(manifest_path, manifest_copy)
  file.copy(provenance_path, provenance_copy)

  # Flip the first character of PayloadCommit in the COPIED corpus provenance.
  lines <- readLines(provenance_copy, warn = FALSE)
  idx <- grep("^PayloadCommit:", lines)
  expect_length(idx, 1L)
  mutated_commit <- paste0("0", substring(hi_payload_commit, 2L))
  lines[[idx[[1L]]]] <- paste0("PayloadCommit: ", mutated_commit)
  writeLines(lines, provenance_copy)

  expect_false(hi_identity_consistent(manifest_copy, provenance_copy))
})

# ---------------------------------------------------------------------------
# DATA-ONLY -- availability and schema revision are unchanged by this unit.
# ---------------------------------------------------------------------------

test_that("Yandex is available and the schema is the activation revision", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]], "available"
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
})
