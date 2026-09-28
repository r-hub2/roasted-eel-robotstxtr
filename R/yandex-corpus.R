# Offline projection of the accepted Yandex conformance corpus.
#
# Introduced by ROBO-novczgii (YI6a). This unit ships a compact, byte-pinned,
# provenance-recorded copy of the 140 accepted Yandex conformance cases (and
# the 26 body files they reference) under inst/yandex-corpus/, plus a
# fail-closed offline validator. It adds test data and a validator only: the
# Yandex matcher backend stays `capability_unavailable` and the public engine
# schema revision is unchanged.
#
# Everything here is fully offline. No sibling checkout and no network access
# are required at build, test, or install time; the corpus files are consumed
# from the installed package tree (with a dev/testthat source-tree fallback).
#
# The canonical JSON writer (`yandex_corpus_serialize`) is the single
# serializer shared with the offline generator dev/gen-yandex-corpus.R. Sharing
# one writer is what makes the validator's determinism check meaningful: the
# generator emits cases.json with this function, and the validator re-emits the
# parsed records with the very same function and asserts the bytes are equal.

# The allowed value sets, kept as small internal helpers so the validator and
# generator agree on them.
yandex_corpus_crawlers <- function() {
  c("Yandex", "YandexAdditionalBot")
}

yandex_corpus_sources <- function() {
  c("default_allow", "rule_allow", "rule_disallow", "effective_empty_disallow")
}

yandex_corpus_decisions <- function() {
  c("allow", "disallow")
}

# Directory that holds the installed corpus. Prefers the installed package
# tree; falls back to the source tree for dev/testthat runs.
yandex_corpus_dir <- function() {
  installed <- system.file("yandex-corpus", package = "robotstxtr")
  if (nzchar(installed) && dir.exists(installed)) {
    return(installed)
  }
  candidates <- c(
    file.path("inst", "yandex-corpus"),
    file.path("..", "..", "inst", "yandex-corpus"),
    file.path("..", "..", "..", "inst", "yandex-corpus")
  )
  for (cand in candidates) {
    if (dir.exists(cand)) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  installed
}

# SHA-256 of a single file, lower-case hex, using base `tools::sha256sum`
# (R >= 4.5.0), matching the approach in R/vendor-manifest-verify.R. No
# external package and no system call are required.
yandex_corpus_sha256_file <- function(path) {
  if (!exists("sha256sum", where = asNamespace("tools"), inherits = FALSE)) {
    stop("tools::sha256sum() is required (R >= 4.5.0) to verify files.")
  }
  digest <- unname(tools::sha256sum(path))
  if (is.na(digest)) {
    stop(sprintf("Could not read file for hashing: %s", path))
  }
  tolower(digest)
}

# Canonicalise a single parsed record into a named list with a fixed key order
# and normalised scalar types. Deterministic serialisation depends on this: it
# is the one place that decides key order, integer coercion, the null-versus-
# object shape of `matched_rule`, and the array-ness of `sources`.
yandex_corpus_build_record <- function(rec) {
  expected <- rec[["expected"]]
  matched_rule <- expected[["matched_rule"]]
  if (is.null(matched_rule)) {
    # default_allow: a genuine JSON null, distinct from an object carrying an
    # empty value string (effective_empty_disallow).
    expected_out <- list(
      decision = as.character(expected[["decision"]]),
      source = as.character(expected[["source"]]),
      matched_rule = NULL
    )
  } else {
    expected_out <- list(
      decision = as.character(expected[["decision"]]),
      source = as.character(expected[["source"]]),
      matched_rule = list(
        line = as.integer(matched_rule[["line"]]),
        type = as.character(matched_rule[["type"]]),
        # value is preserved exactly as the upstream string, including "" for
        # effective_empty_disallow.
        value = as.character(matched_rule[["value"]])
      )
    )
  }

  out <- list(
    case_id = as.character(rec[["case_id"]]),
    body_id = as.character(rec[["body_id"]]),
    body_file = as.character(rec[["body_file"]]),
    body_sha256 = as.character(rec[["body_sha256"]]),
    byte_size = as.integer(rec[["byte_size"]]),
    crawler = as.character(rec[["crawler"]]),
    request_target = as.character(rec[["request_target"]]),
    expected = expected_out,
    # I() forces jsonlite to keep this as a JSON array even at length 1.
    sources = I(vapply(rec[["sources"]], as.character, character(1)))
  )

  obs <- rec[["observation_case_ids"]]
  if (!is.null(obs) && length(obs) > 0L) {
    obs_out <- lapply(obs, as.character)
    names(obs_out) <- names(obs)
    out[["observation_case_ids"]] <- obs_out
  }
  out
}

# The single canonical JSON writer. Produces a deterministic, byte-stable
# rendering of the records: stable key order (via build_record), records sorted
# by case_id under a locale-independent radix sort, UTF-8, LF line endings, and
# a final newline. Returns a single character string.
yandex_corpus_serialize <- function(records) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required to serialize the Yandex corpus.")
  }
  canon <- lapply(records, yandex_corpus_build_record)
  case_ids <- vapply(canon, function(r) r[["case_id"]], character(1))
  canon <- canon[order(case_ids, method = "radix")]
  json <- jsonlite::toJSON(canon, auto_unbox = TRUE, null = "null", pretty = 2)
  paste0(json, "\n")
}

#' Read the installed Yandex conformance corpus.
#'
#' Parses cases.json into a structured list of records (one per accepted case).
#' Fully offline; consumes the installed package tree by default.
#'
#' @param dir Corpus directory. Defaults to the installed corpus.
#' @return A list of parsed case records.
#' @keywords internal
#' @noRd
read_yandex_corpus <- function(dir = yandex_corpus_dir()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required to read the Yandex corpus.")
  }
  cases_path <- file.path(dir, "cases.json")
  if (!file.exists(cases_path)) {
    stop(sprintf("Yandex corpus cases.json not found: %s", cases_path))
  }
  jsonlite::fromJSON(cases_path, simplifyVector = FALSE)
}

#' Fail-closed offline validator for the Yandex conformance corpus.
#'
#' Verifies the installed corpus without any sibling checkout or network
#' access. Returns `ok = FALSE` (never errors) on any violation, except when
#' jsonlite is genuinely unavailable. Checks: exactly 140 records; unique
#' case_ids; every crawler in the allowed set; every expected.source in the
#' allowed set; every decision in the allowed set; `matched_rule` is null if
#' and only if source == "default_allow"; every referenced body file is present
#' with a SHA-256 and byte size matching the record; and deterministic
#' serialisation (re-serialising the parsed records with the shared canonical
#' writer reproduces the committed cases.json bytes exactly).
#'
#' @param dir Corpus directory. Defaults to the installed corpus.
#' @return A list with `ok` (logical) plus diagnostics.
#' @keywords internal
#' @noRd
verify_yandex_corpus <- function(dir = yandex_corpus_dir()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required to verify the Yandex corpus.")
  }

  verify_env <- environment()
  errors <- character(0)
  fail <- function(msg) {
    assign("errors", c(get("errors", verify_env), msg), verify_env)
  }
  result <- function() {
    list(
      ok = length(errors) == 0L,
      dir = dir,
      n_records = n_records,
      n_bodies = n_bodies,
      errors = errors
    )
  }
  n_records <- NA_integer_
  n_bodies <- NA_integer_

  if (!dir.exists(dir)) {
    fail(sprintf("Corpus directory not found: %s", dir))
    return(result())
  }
  cases_path <- file.path(dir, "cases.json")
  if (!file.exists(cases_path)) {
    fail(sprintf("cases.json not found: %s", cases_path))
    return(result())
  }

  records <- tryCatch(
    jsonlite::fromJSON(cases_path, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(records, "error")) {
    fail(sprintf("cases.json is not valid JSON: %s", conditionMessage(records)))
    return(result())
  }
  if (!is.list(records)) {
    fail("cases.json must be a JSON array of records.")
    return(result())
  }

  n_records <- length(records)
  if (n_records != 140L) {
    fail(sprintf("Expected exactly 140 records, found %d.", n_records))
  }

  # Per-record structural checks.
  case_ids <- character(0)
  body_ids <- character(0)
  required <- c(
    "case_id", "body_id", "body_file", "body_sha256", "byte_size",
    "crawler", "request_target", "expected", "sources"
  )
  for (i in seq_along(records)) {
    rec <- records[[i]]
    if (!is.list(rec)) {
      fail(sprintf("Record %d is not an object.", i))
      next
    }
    missing <- setdiff(required, names(rec))
    if (length(missing) > 0L) {
      fail(sprintf(
        "Record %d is missing field(s): %s", i, toString(missing)
      ))
      next
    }
    cid <- as.character(rec[["case_id"]])
    case_ids <- c(case_ids, cid)
    body_ids <- c(body_ids, as.character(rec[["body_id"]]))

    crawler <- as.character(rec[["crawler"]])
    if (!crawler %in% yandex_corpus_crawlers()) {
      fail(sprintf("Record %s has unknown crawler '%s'.", cid, crawler))
    }

    expected <- rec[["expected"]]
    src <- as.character(expected[["source"]])
    if (!src %in% yandex_corpus_sources()) {
      fail(sprintf("Record %s has unknown expected.source '%s'.", cid, src))
    }
    dec <- as.character(expected[["decision"]])
    if (!dec %in% yandex_corpus_decisions()) {
      fail(sprintf("Record %s has unknown decision '%s'.", cid, dec))
    }

    mr_null <- is.null(expected[["matched_rule"]])
    is_default <- identical(src, "default_allow")
    if (mr_null != is_default) {
      fail(sprintf(
        paste0(
          "Record %s violates the matched_rule rule: matched_rule is %s ",
          "but source is '%s' (matched_rule must be null iff default_allow)."
        ),
        cid, if (mr_null) "null" else "non-null", src
      ))
    }

    # Body file presence, SHA-256 and byte size.
    body_file <- as.character(rec[["body_file"]])
    body_path <- file.path(dir, body_file)
    if (file.exists(body_path)) {
      got_sha <- yandex_corpus_sha256_file(body_path)
      want_sha <- tolower(as.character(rec[["body_sha256"]]))
      if (!identical(got_sha, want_sha)) {
        fail(sprintf(
          "Body %s SHA-256 mismatch: file %s vs recorded %s.",
          body_file, got_sha, want_sha
        ))
      }
      got_size <- as.integer(file.size(body_path))
      want_size <- as.integer(rec[["byte_size"]])
      if (!identical(got_size, want_size)) {
        fail(sprintf(
          "Body %s byte size mismatch: file %d vs recorded %d.",
          body_file, got_size, want_size
        ))
      }
    } else {
      fail(sprintf(
        "Record %s references missing body file %s.", cid, body_file
      ))
    }
  }

  if (anyDuplicated(case_ids) > 0L) {
    dups <- unique(case_ids[duplicated(case_ids)])
    fail(sprintf("Duplicate case_id(s): %s", toString(dups)))
  }
  n_bodies <- length(unique(body_ids))

  # Determinism: re-serialise and compare bytes with the committed file.
  reserialized <- tryCatch(
    yandex_corpus_serialize(records),
    error = function(e) e
  )
  if (inherits(reserialized, "error")) {
    fail(sprintf(
      "Re-serialisation failed: %s", conditionMessage(reserialized)
    ))
  } else {
    committed <- readBin(cases_path, "raw", n = file.size(cases_path))
    produced <- charToRaw(enc2utf8(reserialized))
    if (!identical(committed, produced)) {
      fail("cases.json is not byte-identical to its canonical serialisation.")
    }
  }

  result()
}
