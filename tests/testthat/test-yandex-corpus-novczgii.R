# Tests for the offline Yandex conformance corpus projection (ROBO-novczgii).
#
# The committed fixtures under inst/yandex-corpus/ are never mutated: every
# fail-closed test operates on a fresh copy in a tempdir.

skip_if_not_installed("jsonlite")

# The corpus verifier relies on tools::sha256sum (R >= 4.5.0); skip cleanly
# otherwise (mirrors test-vendor-manifest-verify-zfhakxcn.R).
skip_if_no_sha256 <- function() {
  skip_if_not(
    exists("sha256sum", where = asNamespace("tools"), inherits = FALSE),
    "tools::sha256sum() unavailable (needs R >= 4.5.0)"
  )
}

corpus_dir <- function() {
  yandex_corpus_dir()
}

# Copy the corpus into a fresh tempdir so mutation tests never touch the
# committed fixtures.
copy_corpus <- function() {
  src <- corpus_dir()
  dest <- file.path(tempfile("yandex-corpus-"))
  dir.create(file.path(dest, "bodies"), recursive = TRUE)
  file.copy(file.path(src, "cases.json"), file.path(dest, "cases.json"))
  file.copy(file.path(src, "PROVENANCE.dcf"), file.path(dest, "PROVENANCE.dcf"))
  for (f in list.files(file.path(src, "bodies"), full.names = TRUE)) {
    file.copy(f, file.path(dest, "bodies", basename(f)))
  }
  dest
}

test_that("the committed corpus verifies clean and offline", {
  skip_if_no_sha256()
  res <- verify_yandex_corpus(corpus_dir())
  expect_true(res$ok)
  expect_equal(res$n_records, 140L)
  expect_equal(res$n_bodies, 26L)
  expect_length(res$errors, 0L)
})

test_that("corpus content matches the recorded invariants", {
  records <- read_yandex_corpus(corpus_dir())
  expect_length(records, 140L)

  crawlers <- vapply(records, function(r) r$crawler, character(1))
  expect_setequal(unique(crawlers), c("Yandex", "YandexAdditionalBot"))
  expect_equal(sum(crawlers == "Yandex"), 119L)
  expect_equal(sum(crawlers == "YandexAdditionalBot"), 21L)

  body_ids <- vapply(records, function(r) r$body_id, character(1))
  expect_length(unique(body_ids), 26L)

  case_ids <- vapply(records, function(r) r$case_id, character(1))
  expect_false(anyDuplicated(case_ids) > 0L)

  # matched_rule is null iff source == "default_allow".
  for (r in records) {
    is_default <- identical(r$expected$source, "default_allow")
    is_null <- is.null(r$expected$matched_rule)
    expect_equal(is_null, is_default)
    expect_true(r$expected$source %in% c(
      "default_allow", "rule_allow", "rule_disallow",
      "effective_empty_disallow"
    ))
  }

  # An empty matched_rule value (effective_empty_disallow) is distinct from a
  # null matched_rule (default_allow).
  empty_val <- Filter(
    function(r) identical(r$expected$source, "effective_empty_disallow"),
    records
  )
  expect_gt(length(empty_val), 0L)
  for (r in empty_val) {
    expect_false(is.null(r$expected$matched_rule))
    expect_identical(r$expected$matched_rule$value, "")
  }
})

test_that("every body file's SHA-256 matches its record", {
  skip_if_no_sha256()
  dir <- corpus_dir()
  records <- read_yandex_corpus(dir)
  for (r in records) {
    path <- file.path(dir, r$body_file)
    expect_true(file.exists(path))
    expect_identical(
      yandex_corpus_sha256_file(path),
      tolower(r$body_sha256)
    )
  }
})

test_that("re-serialization is byte-identical to the committed cases.json", {
  dir <- corpus_dir()
  records <- read_yandex_corpus(dir)
  produced <- charToRaw(enc2utf8(yandex_corpus_serialize(records)))
  cases_path <- file.path(dir, "cases.json")
  committed <- readBin(cases_path, "raw", n = file.size(cases_path))
  expect_identical(produced, committed)
})

# ---- Fail-closed mutation tests (operate on a copy) -------------------------

test_that("dropping a case fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  writeBin(
    charToRaw(enc2utf8(yandex_corpus_serialize(records[-1L]))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "Expected exactly 140 records, found 139."
  )
})

test_that("adding a 141st case fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  extra <- records[[1L]]
  extra$case_id <- "zzz-synthetic-extra-case"
  writeBin(
    charToRaw(enc2utf8(yandex_corpus_serialize(c(records, list(extra))))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "Expected exactly 140 records, found 141."
  )
})

test_that("corrupting one body byte fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  body_path <- file.path(dir, records[[1L]]$body_file)
  bytes <- readBin(body_path, "raw", n = file.size(body_path))
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, body_path)
  expect_corpus_rejects(verify_yandex_corpus(dir), "SHA-256 mismatch")
})

test_that("flipping a recorded sha256 fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  raw <- readLines(file.path(dir, "cases.json"), warn = FALSE)
  idx <- grep("\"body_sha256\"", raw, fixed = TRUE)[[1L]]
  raw[[idx]] <- sub(
    "[0-9a-f]{64}",
    strrep("0", 64L),
    raw[[idx]]
  )
  writeBin(charToRaw(enc2utf8(paste0(paste(raw, collapse = "\n"), "\n"))),
           file.path(dir, "cases.json"))
  expect_corpus_rejects(verify_yandex_corpus(dir), "SHA-256 mismatch")
})

test_that("nulling a non-default_allow matched_rule fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  target <- which(vapply(
    records, function(r) !identical(r$expected$source, "default_allow"),
    logical(1)
  ))[[1L]]
  records[[target]]$expected$matched_rule <- NULL
  # Serialize by hand so the null survives (build_record would reject the
  # mismatch shape only via verify, not serialization).
  writeBin(
    charToRaw(enc2utf8(yandex_corpus_serialize(records))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "matched_rule must be null iff default_allow"
  )
})

# ---- Fail-closed rejection matrix ------------------------------------------
#
# One test per rejection branch of verify_yandex_corpus(). Each tampers with a
# COPY and asserts the SPECIFIC diagnostic, so a verifier that rejected for
# the wrong reason would not pass. Re-writing the tampered records with the
# canonical serializer keeps the determinism check green, which isolates the
# invariant under test.

yc_tampered <- function(mutate) {
  # Verification hashes every body file, so a pre-4.5 R must skip outright.
  skip_if_no_sha256()
  corpus_tamper_verify(
    corpus_dir(), mutate, read_yandex_corpus, yandex_corpus_serialize,
    verify_yandex_corpus
  )
}

# Index of the first record carrying the given expected$source.
yc_source_idx <- function(records, source) {
  which(vapply(records, function(r) {
    identical(r$expected$source, source)
  }, logical(1)))[[1L]]
}

test_that("a corpus directory that does not exist is rejected", {
  expect_corpus_rejects(
    verify_yandex_corpus(tempfile("yandex-corpus-absent-")),
    "Corpus directory not found"
  )
})

test_that("a corpus directory without cases.json is rejected", {
  dir <- tempfile("yandex-corpus-nocases-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(verify_yandex_corpus(dir), "cases.json not found")
  expect_error(
    read_yandex_corpus(dir), "Yandex corpus cases.json not found",
    fixed = TRUE
  )
})

test_that("a cases.json that is not valid JSON is rejected", {
  dir <- corpus_minimal_dir("{ this is not json\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "cases.json is not valid JSON"
  )
})

test_that("a cases.json that is not a JSON array is rejected", {
  dir <- corpus_minimal_dir("123\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "cases.json must be a JSON array of records."
  )
})

test_that("a non-object record is rejected", {
  dir <- corpus_minimal_dir("[\"not-an-object\"]\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "Record 1 is not an object."
  )
})

test_that("a record that the canonical writer cannot serialize is rejected", {
  # A nested sources array makes yandex_corpus_build_record() throw, so the
  # determinism check reports the failure instead of silently passing.
  dir <- corpus_minimal_dir("[{\"sources\": [[\"a\", \"b\"]]}]\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(verify_yandex_corpus(dir), "Re-serialisation failed")
})

test_that("a record missing a required field is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  lines <- readLines(file.path(dir, "cases.json"), warn = FALSE)
  idx <- grep("^    \"byte_size\": ", lines)[[1L]]
  corpus_write_cases(dir, paste0(paste(lines[-idx], collapse = "\n"), "\n"))
  expect_corpus_rejects(
    verify_yandex_corpus(dir), "Record 1 is missing field(s): byte_size"
  )
})

test_that("an unknown crawler is rejected", {
  res <- yc_tampered(function(records) {
    records[[1L]]$crawler <- "Googlebot"
    records
  })
  expect_corpus_rejects(res, "has unknown crawler 'Googlebot'.")
})

test_that("an unknown expected.source is rejected", {
  res <- yc_tampered(function(records) {
    i <- yc_source_idx(records, "rule_allow")
    records[[i]]$expected$source <- "rule_bogus"
    records
  })
  expect_corpus_rejects(res, "has unknown expected.source 'rule_bogus'.")
})

test_that("an unknown decision is rejected", {
  res <- yc_tampered(function(records) {
    records[[1L]]$expected$decision <- "maybe"
    records
  })
  expect_corpus_rejects(res, "has unknown decision 'maybe'.")
})

test_that("a matched_rule on a default_allow record is rejected", {
  res <- yc_tampered(function(records) {
    i <- yc_source_idx(records, "default_allow")
    records[[i]]$expected$matched_rule <- list(
      line = 1L, type = "allow", value = "/x"
    )
    records
  })
  expect_corpus_rejects(res, "matched_rule must be null iff default_allow")
})

test_that("a missing body file is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  unlink(file.path(dir, records[[1L]]$body_file))
  expect_corpus_rejects(
    verify_yandex_corpus(dir),
    sprintf("references missing body file %s.", records[[1L]]$body_file)
  )
})

test_that("a body whose byte size drifted is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  body_path <- file.path(dir, records[[1L]]$body_file)
  bytes <- readBin(body_path, "raw", n = file.size(body_path))
  writeBin(c(bytes, as.raw(10L)), body_path)
  expect_corpus_rejects(verify_yandex_corpus(dir), "byte size mismatch")
})

test_that("a duplicate case_id is rejected", {
  res <- yc_tampered(function(records) {
    records[[2L]]$case_id <- records[[1L]]$case_id
    records
  })
  expect_corpus_rejects(res, "Duplicate case_id(s):")
})

test_that("a non-canonical cases.json rendering is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_yandex_corpus(dir)
  # Semantically identical JSON, non-canonical bytes (a stray blank line).
  corpus_write_cases(dir, paste0(yandex_corpus_serialize(records), "\n"))
  expect_corpus_rejects(
    verify_yandex_corpus(dir),
    "cases.json is not byte-identical to its canonical serialisation."
  )
})

# ---- Corpus-dir resolution and hashing helpers ------------------------------

test_that("yandex_corpus_dir() falls back to a source-tree candidate", {
  root <- tempfile("yandex-src-tree-")
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "inst", "yandex-corpus"), recursive = TRUE)
  resolve <- corpus_dir_fn_rooted(yandex_corpus_dir, root)
  expect_identical(
    resolve(),
    normalizePath(file.path(root, "inst", "yandex-corpus"), winslash = "/")
  )
})

test_that("yandex_corpus_dir() returns the install path when none hits", {
  resolve <- corpus_dir_fn_rooted(
    yandex_corpus_dir, tempfile("yandex-no-tree-")
  )
  expect_identical(resolve(), "")
})

test_that("hashing an unreadable file is a hard error", {
  skip_if_no_sha256()
  expect_error(
    yandex_corpus_sha256_file(tempfile("yandex-absent-")),
    "Could not read file for hashing",
    fixed = TRUE
  )
})

# ---- Data-only proof: nothing about availability or schema changed ----------

test_that("Yandex is available and the schema revision is the activation one", {
  registry <- engine_matcher_registry_v1()
  expect_identical(registry$yandex$availability, "available")
  expect_type(registry$yandex$callable, "closure")
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
})
