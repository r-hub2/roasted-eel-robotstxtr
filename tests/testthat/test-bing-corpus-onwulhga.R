# Tests for the offline Bing conformance corpus projection (ROBO-onwulhga, BI6).
#
# Two layers, mirroring the Yandex corpus + activation-facade tests:
#   1. Fail-closed OFFLINE data invariants over inst/bing-corpus/ (no engine):
#      counts, profile split, the matched_rule null-iff-default rule, the
#      tester<->golden decision link, per-file SHA-256, and byte-identical
#      canonical re-serialisation. Mutation tests operate on a fresh copy so the
#      committed fixtures are never touched.
#   2. PUBLIC v2 facade replay: every one of the 191 accepted observation-level
#      cases is driven end-to-end through robots_evaluate_text_v1 (backend
#      "bing", robots_policy_ruleset = "assumed_rfc9309"), asserting the
#      published status/decision/reason/matched-rule line/type/value and the
#      exact matched_rule_value_raw bytes equal the recorded golden snapshot AND
#      the independently observed tester decision (spec SS16.3).

skip_if_not_installed("jsonlite")

skip_if_no_sha256 <- function() {
  skip_if_not(
    exists("sha256sum", where = asNamespace("tools"), inherits = FALSE),
    "tools::sha256sum() unavailable (needs R >= 4.5.0)"
  )
}

corpus_dir <- function() bing_corpus_dir()

# Copy the corpus into a fresh tempdir so mutation tests never touch committed
# fixtures.
copy_corpus <- function() {
  src <- corpus_dir()
  dest <- file.path(tempfile("bing-corpus-"))
  dir.create(file.path(dest, "bodies"), recursive = TRUE)
  file.copy(file.path(src, "cases.json"), file.path(dest, "cases.json"))
  file.copy(file.path(src, "PROVENANCE.dcf"), file.path(dest, "PROVENANCE.dcf"))
  for (f in list.files(file.path(src, "bodies"), full.names = TRUE)) {
    file.copy(f, file.path(dest, "bodies", basename(f)))
  }
  dest
}

# ---- Offline data invariants -----------------------------------------------

test_that("the committed corpus verifies clean and offline", {
  skip_if_no_sha256()
  res <- verify_bing_corpus(corpus_dir())
  expect_true(res$ok)
  expect_equal(res$n_records, 191L)
  expect_equal(res$n_bodies, 95L)
  expect_equal(res$n_bingbot, 95L)
  expect_equal(res$n_adidxbot, 96L)
  expect_length(res$errors, 0L)
})

test_that("corpus content matches the recorded invariants", {
  records <- read_bing_corpus(corpus_dir())
  expect_length(records, 191L)

  profiles <- vapply(records, function(r) r$profile, character(1))
  expect_setequal(unique(profiles), c("bingbot", "adidxbot"))
  expect_equal(sum(profiles == "bingbot"), 95L)
  expect_equal(sum(profiles == "adidxbot"), 96L)

  body_ids <- vapply(records, function(r) r$body_ref, character(1))
  expect_length(unique(body_ids), 95L)

  case_ids <- vapply(records, function(r) r$case_id, character(1))
  expect_false(anyDuplicated(case_ids) > 0L)

  for (r in records) {
    exp <- r$matcher_expected
    # Every shipped case is evaluated and its tester decision equals the golden.
    expect_identical(exp$matcher_status, "evaluated")
    expect_identical(r$tester_observed$terminal_outcome, "evaluated")
    expect_identical(r$tester_observed$decision, exp$url_decision)
    # matched_rule is null iff default_allow.
    is_default <- identical(exp$reason, "default_allow")
    expect_equal(is.null(exp$matched_rule), is_default)
    expect_true(exp$reason %in%
      c("default_allow", "rule_allow", "rule_disallow"))
    if (!is_default) {
      expect_identical(exp$reason, paste0("rule_", exp$matched_rule$type))
      expect_identical(exp$url_decision, exp$matched_rule$type)
    }
  }
})

test_that("the shipped corpus never emits the effective-empty reason", {
  # The Bing profile treats an empty Disallow as inert (default_allow), so no
  # accepted case carries effective_empty_disallow (spec / handover note).
  records <- read_bing_corpus(corpus_dir())
  reasons <- vapply(records, function(r) r$matcher_expected$reason,
                    character(1))
  expect_false("effective_empty_disallow" %in% reasons)
})

test_that("every body file's SHA-256 matches its record", {
  skip_if_no_sha256()
  dir <- corpus_dir()
  records <- read_bing_corpus(dir)
  for (r in records) {
    path <- file.path(dir, r$body_file)
    expect_true(file.exists(path))
    expect_identical(
      bing_corpus_sha256_file(path),
      tolower(r$body_sha256)
    )
  }
})

test_that("re-serialization is byte-identical to the committed cases.json", {
  dir <- corpus_dir()
  records <- read_bing_corpus(dir)
  produced <- charToRaw(enc2utf8(bing_corpus_serialize(records)))
  cases_path <- file.path(dir, "cases.json")
  committed <- readBin(cases_path, "raw", n = file.size(cases_path))
  expect_identical(produced, committed)
})

# ---- Fail-closed mutation tests (operate on a copy) ------------------------

test_that("dropping a case fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  writeBin(
    charToRaw(enc2utf8(bing_corpus_serialize(records[-1L]))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_bing_corpus(dir), "Expected exactly 191 records, found 190."
  )
})

test_that("adding a 192nd case fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  extra <- records[[1L]]
  extra$case_id <- "zzz-synthetic-extra-case"
  writeBin(
    charToRaw(enc2utf8(bing_corpus_serialize(c(records, list(extra))))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_bing_corpus(dir), "Expected exactly 191 records, found 192."
  )
})

test_that("corrupting one body byte fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  body_path <- file.path(dir, records[[1L]]$body_file)
  bytes <- readBin(body_path, "raw", n = file.size(body_path))
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, body_path)
  expect_corpus_rejects(verify_bing_corpus(dir), "SHA-256 mismatch")
})

test_that("flipping a recorded body sha256 fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  raw <- readLines(file.path(dir, "cases.json"), warn = FALSE)
  idx <- grep("\"body_sha256\"", raw, fixed = TRUE)[[1L]]
  raw[[idx]] <- sub("[0-9a-f]{64}", strrep("0", 64L), raw[[idx]])
  writeBin(charToRaw(enc2utf8(paste0(paste(raw, collapse = "\n"), "\n"))),
           file.path(dir, "cases.json"))
  expect_corpus_rejects(verify_bing_corpus(dir), "SHA-256 mismatch")
})

test_that("breaking the tester<->golden decision link fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  # Flip one tester decision so it no longer equals the golden url_decision.
  flip <- function(d) if (identical(d, "allow")) "disallow" else "allow"
  records[[1L]]$tester_observed$decision <-
    flip(records[[1L]]$tester_observed$decision)
  writeBin(
    charToRaw(enc2utf8(bing_corpus_serialize(records))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_bing_corpus(dir), "!= golden url_decision"
  )
})

test_that("nulling a non-default matched_rule fails verification", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  target <- which(vapply(records, function(r) {
    !identical(r$matcher_expected$reason, "default_allow")
  }, logical(1)))[[1L]]
  records[[target]]$matcher_expected$matched_rule <- NULL
  writeBin(
    charToRaw(enc2utf8(bing_corpus_serialize(records))),
    file.path(dir, "cases.json")
  )
  expect_corpus_rejects(
    verify_bing_corpus(dir), "matched_rule must be null iff default_allow"
  )
})

# ---- Fail-closed rejection matrix ------------------------------------------
#
# One test per rejection branch of verify_bing_corpus(). Each tampers with a
# COPY of the corpus and asserts the SPECIFIC diagnostic, so a verifier that
# rejected for the wrong reason would not pass. Re-writing the tampered
# records with the canonical serializer keeps the determinism check green,
# which isolates the invariant under test (a few mutations unavoidably trip a
# second, dependent check; the assertion always names the target one).

bc_tampered <- function(mutate) {
  # Verification hashes every body file, so a pre-4.5 R must skip outright.
  skip_if_no_sha256()
  corpus_tamper_verify(
    corpus_dir(), mutate, read_bing_corpus, bing_corpus_serialize,
    verify_bing_corpus
  )
}

# Index of the first record carrying the given matcher_expected$reason.
bc_reason_idx <- function(records, reason) {
  which(vapply(records, function(r) {
    identical(r$matcher_expected$reason, reason)
  }, logical(1)))[[1L]]
}

test_that("a corpus directory that does not exist is rejected", {
  expect_corpus_rejects(
    verify_bing_corpus(tempfile("bing-corpus-absent-")),
    "Corpus directory not found"
  )
})

test_that("a corpus directory without cases.json is rejected", {
  dir <- tempfile("bing-corpus-nocases-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(verify_bing_corpus(dir), "cases.json not found")
  expect_error(
    read_bing_corpus(dir), "Bing corpus cases.json not found",
    fixed = TRUE
  )
})

test_that("a cases.json that is not valid JSON is rejected", {
  dir <- corpus_minimal_dir("{ this is not json\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(
    verify_bing_corpus(dir), "cases.json is not valid JSON"
  )
})

test_that("a cases.json that is not a JSON array is rejected", {
  dir <- corpus_minimal_dir("123\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(
    verify_bing_corpus(dir), "cases.json must be a JSON array of records."
  )
})

test_that("a non-object record is rejected", {
  dir <- corpus_minimal_dir("[\"not-an-object\"]\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(verify_bing_corpus(dir), "Record 1 is not an object.")
})

test_that("a record that the canonical writer cannot serialize is rejected", {
  # A nested expectation_ids array makes bing_corpus_build_record() throw, so
  # the determinism check reports the failure instead of silently passing.
  dir <- corpus_minimal_dir("[{\"expectation_ids\": [[\"a\", \"b\"]]}]\n")
  on.exit(unlink(dir, recursive = TRUE))
  expect_corpus_rejects(verify_bing_corpus(dir), "Re-serialisation failed")
})

test_that("a record missing a required field is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  lines <- readLines(file.path(dir, "cases.json"), warn = FALSE)
  idx <- grep("^    \"byte_size\": ", lines)[[1L]]
  corpus_write_cases(dir, paste0(paste(lines[-idx], collapse = "\n"), "\n"))
  expect_corpus_rejects(
    verify_bing_corpus(dir), "Record 1 is missing field(s): byte_size"
  )
})

test_that("an unknown profile is rejected", {
  res <- bc_tampered(function(records) {
    records[[1L]]$profile <- "googlebot"
    records
  })
  expect_corpus_rejects(res, "has unknown profile 'googlebot'.")
})

test_that("an empty expectation_ids list is rejected", {
  res <- bc_tampered(function(records) {
    records[[1L]]$expectation_ids <- list()
    records
  })
  expect_corpus_rejects(res, "has an empty expectation_ids list.")
})

test_that("a matcher_status other than evaluated is rejected", {
  res <- bc_tampered(function(records) {
    records[[1L]]$matcher_expected$matcher_status <- "capability_unavailable"
    records
  })
  expect_corpus_rejects(res, "has matcher_status 'capability_unavailable'")
})

test_that("an unknown url_decision is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "default_allow")
    records[[i]]$matcher_expected$url_decision <- "maybe"
    # Keep the tester decision in step so only the vocabulary check trips.
    records[[i]]$tester_observed$decision <- "maybe"
    records
  })
  expect_corpus_rejects(res, "has unknown url_decision 'maybe'.")
})

test_that("an unknown reason is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "rule_allow")
    records[[i]]$matcher_expected$reason <- "rule_bogus"
    records
  })
  expect_corpus_rejects(res, "has unknown reason 'rule_bogus'.")
})

test_that("a matched_rule on a default_allow record is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "default_allow")
    records[[i]]$matcher_expected$matched_rule <- list(
      line = 1L, type = "allow", value = "/x", value_raw_hex = "2f78"
    )
    records
  })
  expect_corpus_rejects(res, "matched_rule must be null iff default_allow")
})

test_that("an unknown matched rule type is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "rule_allow")
    records[[i]]$matcher_expected$matched_rule$type <- "redirect"
    records
  })
  expect_corpus_rejects(res, "has unknown matched rule type 'redirect'.")
})

test_that("a rule type that disagrees with the reason is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "rule_allow")
    records[[i]]$matcher_expected$reason <- "rule_disallow"
    records
  })
  expect_corpus_rejects(res, "rule type 'allow' disagrees with reason")
})

test_that("a decision that disagrees with the rule type is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "rule_allow")
    records[[i]]$matcher_expected$url_decision <- "disallow"
    records[[i]]$tester_observed$decision <- "disallow"
    records
  })
  expect_corpus_rejects(res, "decision 'disallow' disagrees with rule type")
})

test_that("a value_raw_hex that is not even-length hex is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "rule_disallow")
    records[[i]]$matcher_expected$matched_rule$value_raw_hex <- "zz"
    records
  })
  expect_corpus_rejects(res, "value_raw_hex is not even-length hex.")
})

test_that("a value_raw_hex that is not the UTF-8 of value is rejected", {
  res <- bc_tampered(function(records) {
    i <- bc_reason_idx(records, "rule_disallow")
    records[[i]]$matcher_expected$matched_rule$value_raw_hex <- "abcd"
    records
  })
  expect_corpus_rejects(
    res, "value_raw_hex does not match the UTF-8 of value."
  )
})

test_that("a tester terminal_outcome other than evaluated is rejected", {
  res <- bc_tampered(function(records) {
    records[[1L]]$tester_observed$terminal_outcome <- "not_evaluated"
    records
  })
  expect_corpus_rejects(res, "terminal_outcome is 'not_evaluated'")
})

test_that("a request_target_sha256 that does not pin the target is rejected", {
  skip_if_no_sha256()
  res <- bc_tampered(function(records) {
    records[[1L]]$request_target_sha256 <- strrep("0", 64L)
    records
  })
  expect_corpus_rejects(res, "request_target_sha256 mismatch.")
})

test_that("a missing body file is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  unlink(file.path(dir, records[[1L]]$body_file))
  expect_corpus_rejects(
    verify_bing_corpus(dir),
    sprintf("references missing body file %s.", records[[1L]]$body_file)
  )
})

test_that("a body whose byte size drifted is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  body_path <- file.path(dir, records[[1L]]$body_file)
  bytes <- readBin(body_path, "raw", n = file.size(body_path))
  writeBin(c(bytes, as.raw(10L)), body_path)
  expect_corpus_rejects(verify_bing_corpus(dir), "byte size mismatch")
})

test_that("a duplicate case_id is rejected", {
  res <- bc_tampered(function(records) {
    records[[2L]]$case_id <- records[[1L]]$case_id
    records
  })
  expect_corpus_rejects(res, "Duplicate case_id(s):")
})

test_that("a drifted distinct-body count is rejected", {
  res <- bc_tampered(function(records) {
    # Record 2 shares record 1's body, so re-pointing it adds a 96th body_ref.
    records[[2L]]$body_ref <- "zzz-synthetic-body-ref"
    records
  })
  expect_corpus_rejects(res, "Expected 95 distinct bodies, found 96.")
})

test_that("a drifted profile split is rejected", {
  res <- bc_tampered(function(records) {
    i <- which(vapply(records, function(r) {
      identical(r$profile, "bingbot")
    }, logical(1)))[[1L]]
    records[[i]]$profile <- "adidxbot"
    records
  })
  expect_corpus_rejects(res, "Profile split mismatch")
})

test_that("a non-canonical cases.json rendering is rejected", {
  skip_if_no_sha256()
  dir <- copy_corpus()
  on.exit(unlink(dir, recursive = TRUE))
  records <- read_bing_corpus(dir)
  # Semantically identical JSON, non-canonical bytes (a stray blank line).
  corpus_write_cases(dir, paste0(bing_corpus_serialize(records), "\n"))
  expect_corpus_rejects(
    verify_bing_corpus(dir),
    "cases.json is not byte-identical to its canonical serialisation."
  )
})

# ---- Corpus-dir resolution and hashing helpers ------------------------------

test_that("bing_corpus_dir() falls back to a source-tree candidate", {
  root <- tempfile("bing-src-tree-")
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "inst", "bing-corpus"), recursive = TRUE)
  resolve <- corpus_dir_fn_rooted(bing_corpus_dir, root)
  expect_identical(
    resolve(),
    normalizePath(file.path(root, "inst", "bing-corpus"), winslash = "/")
  )
})

test_that("bing_corpus_dir() returns the install path when no candidate hits", {
  resolve <- corpus_dir_fn_rooted(bing_corpus_dir, tempfile("bing-no-tree-"))
  expect_identical(resolve(), "")
})

test_that("hashing an unreadable file is a hard error", {
  skip_if_no_sha256()
  expect_error(
    bing_corpus_sha256_file(tempfile("bing-absent-")),
    "Could not read file for hashing",
    fixed = TRUE
  )
})

test_that("the hex encoder round-trips bytes and handles the empty vector", {
  expect_identical(bing_corpus_bytes_to_hex(raw(0)), "")
  expect_identical(bing_corpus_bytes_to_hex(charToRaw("/x")), "2f78")
})

# ---- Public-facade replay (skip on a pure-R install) -----------------------

skip_if_not(
  is.function(tryCatch(robotstxtr_bing_eval_batch_, error = function(e) NULL)),
  "native bing binding not built (pure-R install)"
)

bc_dir <- bing_corpus_dir()
bc_records <- read_bing_corpus(bc_dir)

bc_body_arg <- function(rec) {
  path <- file.path(bc_dir, rec$body_file)
  body <- readBin(path, "raw", n = as.integer(rec$byte_size))
  out <- rawToChar(body)
  Encoding(out) <- "bytes"
  out
}

bc_url <- function(rec) paste0("https://example.test", rec$request_target)

bc_hex2raw <- function(h) {
  if (!nzchar(h)) {
    return(raw(0))
  }
  as.raw(strtoi(substring(h, seq(1L, nchar(h), 2L), seq(2L, nchar(h), 2L)),
                16L))
}

test_that("all 191 accepted Bing cases match the public v2 facade output", {
  acc <- new.env(parent = emptyenv())
  acc$mismatches <- character(0)
  note <- function(cid, field, expected, actual) {
    acc$mismatches <- c(acc$mismatches, sprintf(
      "[%s] %s: expected %s, got %s",
      cid, field, format(expected), format(actual)
    ))
  }

  for (rec in bc_records) {
    cid <- rec$case_id
    x <- robots_evaluate_text_v1(
      robots_txt = bc_body_arg(rec),
      url = bc_url(rec),
      robots_product_token = rec$profile,
      robots_policy_ruleset = "assumed_rfc9309",
      matcher_backend = "bing"
    )
    r <- x$results
    exp <- rec$matcher_expected
    matched_rule <- exp$matched_rule
    raw_1 <- r$matched_rule_value_raw[[1L]]

    if (!identical(r$matcher_status[[1L]], "evaluated")) {
      note(cid, "matcher_status", "evaluated", r$matcher_status[[1L]])
    }
    if (!identical(r$matcher_backend[[1L]], "bing")) {
      note(cid, "matcher_backend", "bing", r$matcher_backend[[1L]])
    }
    if (!identical(r$matcher_availability[[1L]], "available")) {
      note(cid, "matcher_availability", "available",
           r$matcher_availability[[1L]])
    }

    # Decision equals the golden snapshot AND the independently observed tester
    # decision (the SS16.3 authority link).
    if (!identical(r$url_decision[[1L]], exp$url_decision)) {
      note(cid, "url_decision", exp$url_decision, r$url_decision[[1L]])
    }
    if (!identical(r$url_decision[[1L]], rec$tester_observed$decision)) {
      note(cid, "url_decision-vs-tester", rec$tester_observed$decision,
           r$url_decision[[1L]])
    }
    if (!identical(r$reason[[1L]], exp$reason)) {
      note(cid, "reason", exp$reason, r$reason[[1L]])
    }

    if (is.null(matched_rule)) {
      if (!identical(exp$reason, "default_allow")) {
        note(cid, "matched_rule-null-reason", "default_allow", exp$reason)
      }
      if (!is.na(r$matched_line[[1L]])) {
        note(cid, "matched_line", NA_integer_, r$matched_line[[1L]])
      }
      if (!is.na(r$matched_rule_value[[1L]])) {
        note(cid, "matched_rule_value", NA_character_,
             r$matched_rule_value[[1L]])
      }
      if (!identical(r$matched_rule_type[[1L]], "none")) {
        note(cid, "matched_rule_type", "none", r$matched_rule_type[[1L]])
      }
      if (!is.null(raw_1)) {
        note(cid, "matched_rule_value_raw", "NULL", "non-NULL")
      }
    } else {
      want_line <- as.integer(matched_rule$line)
      if (!identical(r$matched_line[[1L]], want_line)) {
        note(cid, "matched_line", want_line, r$matched_line[[1L]])
      }
      if (!identical(r$matched_rule_type[[1L]], matched_rule$type)) {
        note(cid, "matched_rule_type", matched_rule$type,
             r$matched_rule_type[[1L]])
      }
      if (!identical(r$matched_rule_value[[1L]], matched_rule$value)) {
        note(cid, "matched_rule_value", matched_rule$value,
             r$matched_rule_value[[1L]])
      }
      # SS11 exact bytes: the raw list element equals the recorded hex bytes and
      # is NUL-free.
      want_raw <- bc_hex2raw(matched_rule$value_raw_hex)
      if (!identical(raw_1, want_raw)) {
        note(cid, "matched_rule_value_raw", "recorded bytes", "different bytes")
      }
      if (!is.null(raw_1) && any(raw_1 == as.raw(0L))) {
        note(cid, "matched_rule_value_raw", "NUL-free", "contains NUL")
      }
    }
  }

  expect_equal(acc$mismatches, character(0))
})

# ---- Batch parse-once across a shared body through the facade --------------

test_that("one facade call over many rows sharing a body evaluates per row", {
  body <- "User-agent: bingbot\nDisallow: /private"
  x <- robots_evaluate_text_v1(
    robots_txt = body,
    url = c("https://example.test/private/x", "https://example.test/public"),
    robots_product_token = "bingbot",
    robots_policy_ruleset = "assumed_rfc9309",
    matcher_backend = "bing"
  )
  expect_identical(x$results$matcher_status, rep("evaluated", 2L))
  expect_identical(x$results$url_decision, c("disallow", "allow"))
  expect_identical(x$results$reason, c("rule_disallow", "default_allow"))
  expect_identical(
    x$results$matched_rule_value_raw[[1L]], charToRaw("/private")
  )
  expect_null(x$results$matched_rule_value_raw[[2L]])
})
