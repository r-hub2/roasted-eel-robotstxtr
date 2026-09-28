# Offline projection of the accepted Bing conformance corpus.
#
# Introduced by ROBO-onwulhga (BI6). This unit ships a compact, byte-pinned,
# provenance-recorded projection of the released Bing accepted expectations
# under inst/bing-corpus/, plus a fail-closed offline validator. The 57 accepted
# `tester_observed` cells (29 bingbot / 28 adidxbot), frozen by the standalone
# release manifest, expand to 191 concrete observation-level cases (one per
# distinct accepted observation): each carries the robots body, the exact
# request target, and the tester-observed allow/disallow decision.
#
# TWO-TIER PROVENANCE. The `tester_observed` decision is the independent
# authority: it is the standalone Bing tester's observed outcome and is imported
# byte-faithfully; it is NEVER rewritten to fit the adapter (spec SS16.3). The
# `matcher_expected` block (status/reason/matched-rule line/type/value/exact raw
# bytes) is a golden snapshot of the VENDORED engine's facade output, captured
# by the offline generator dev/gen-bing-corpus.R. The generator asserts that the
# engine's decision equals the tester-observed decision for every case and
# aborts on any divergence, so a shipped `matcher_expected.url_decision` always
# equals the recorded `tester_observed.decision`. This validator re-asserts that
# link offline (no engine required); the facade test
# (test-bing-corpus-onwulhga.R) replays every case through the public v2 facade
# and checks the golden snapshot.
#
# Everything here is fully offline. No sibling checkout and no network access
# are required at build, test, or install time; the corpus files are consumed
# from the installed package tree (with a dev/testthat source-tree fallback).
#
# The canonical JSON writer (`bing_corpus_serialize`) is the single serializer
# shared with the offline generator. Sharing one writer is what makes the
# validator's determinism check meaningful: the generator emits cases.json with
# this function, and the validator re-emits the parsed records with the very
# same function and asserts the bytes are equal.

# The allowed value sets, kept as small internal helpers so the validator and
# generator agree on them.
bing_corpus_profiles <- function() {
  c("bingbot", "adidxbot")
}

bing_corpus_reasons <- function() {
  c("default_allow", "rule_allow", "rule_disallow")
}

bing_corpus_decisions <- function() {
  c("allow", "disallow")
}

bing_corpus_rule_types <- function() {
  c("allow", "disallow")
}

# Frozen shape of the shipped projection. The validator is fail-closed against
# these exact counts so a dropped, added, or reclassified case fails.
bing_corpus_expected_counts <- function() {
  list(
    cases = 191L,
    bingbot = 95L,
    adidxbot = 96L,
    bodies = 95L
  )
}

# Directory that holds the installed corpus. Prefers the installed package tree;
# falls back to the source tree for dev/testthat runs.
bing_corpus_dir <- function() {
  installed <- system.file("bing-corpus", package = "robotstxtr")
  if (nzchar(installed) && dir.exists(installed)) {
    return(installed)
  }
  candidates <- c(
    file.path("inst", "bing-corpus"),
    file.path("..", "..", "inst", "bing-corpus"),
    file.path("..", "..", "..", "inst", "bing-corpus")
  )
  for (cand in candidates) {
    if (dir.exists(cand)) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  installed
}

# SHA-256 of a single file, lower-case hex, using base `tools::sha256sum`
# (R >= 4.5.0), matching R/vendor-manifest-verify.R and R/yandex-corpus.R.
bing_corpus_sha256_file <- function(path) {
  if (!exists("sha256sum", where = asNamespace("tools"), inherits = FALSE)) {
    stop("tools::sha256sum() is required (R >= 4.5.0) to verify files.")
  }
  digest <- unname(tools::sha256sum(path))
  if (is.na(digest)) {
    stop(sprintf("Could not read file for hashing: %s", path))
  }
  tolower(digest)
}

# SHA-256 of a UTF-8 string's bytes, lower-case hex. Used to re-derive the
# recorded request_target_sha256 (which pins the sibling probe target bytes).
bing_corpus_sha256_string <- function(s) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(s)), tf)
  bing_corpus_sha256_file(tf)
}

# Canonicalise a single parsed record into a named list with a fixed key order
# and normalised scalar types. Deterministic serialisation depends on this: it
# is the one place that decides key order, integer coercion, the null-versus-
# object shape of `matched_rule`, and the array-ness of `expectation_ids`.
bing_corpus_build_record <- function(rec) {
  expected <- rec[["matcher_expected"]]
  matched_rule <- expected[["matched_rule"]]
  if (is.null(matched_rule)) {
    # default_allow: a genuine JSON null, distinct from an object carrying a
    # value. Mirrors the Yandex projection's null-iff-default rule.
    expected_out <- list(
      matcher_status = as.character(expected[["matcher_status"]]),
      url_decision = as.character(expected[["url_decision"]]),
      reason = as.character(expected[["reason"]]),
      matched_rule = NULL
    )
  } else {
    expected_out <- list(
      matcher_status = as.character(expected[["matcher_status"]]),
      url_decision = as.character(expected[["url_decision"]]),
      reason = as.character(expected[["reason"]]),
      matched_rule = list(
        line = as.integer(matched_rule[["line"]]),
        type = as.character(matched_rule[["type"]]),
        value = as.character(matched_rule[["value"]]),
        # Lower-case hex of the exact matched-rule value bytes (SS11). Kept even
        # when it equals the UTF-8 of `value`, so the byte contract is explicit.
        value_raw_hex = tolower(as.character(matched_rule[["value_raw_hex"]]))
      )
    )
  }

  tester <- rec[["tester_observed"]]
  tester_out <- list(
    terminal_outcome = as.character(tester[["terminal_outcome"]]),
    decision = as.character(tester[["decision"]])
  )

  exp_ids <- vapply(rec[["expectation_ids"]], as.character, character(1))
  exp_ids <- sort(unique(exp_ids), method = "radix")

  list(
    case_id = as.character(rec[["case_id"]]),
    profile = as.character(rec[["profile"]]),
    unit = as.character(rec[["unit"]]),
    # I() forces jsonlite to keep this a JSON array even at length 1.
    expectation_ids = I(exp_ids),
    probe_case_id = as.character(rec[["probe_case_id"]]),
    body_ref = as.character(rec[["body_ref"]]),
    body_file = as.character(rec[["body_file"]]),
    body_sha256 = tolower(as.character(rec[["body_sha256"]])),
    byte_size = as.integer(rec[["byte_size"]]),
    request_target = as.character(rec[["request_target"]]),
    request_target_sha256 = tolower(as.character(
      rec[["request_target_sha256"]]
    )),
    tester_observed = tester_out,
    matcher_expected = expected_out
  )
}

# The single canonical JSON writer. Produces a deterministic, byte-stable
# rendering of the records: stable key order (via build_record), records sorted
# by case_id under a locale-independent radix sort, UTF-8, LF line endings, and
# a final newline. Returns a single character string.
bing_corpus_serialize <- function(records) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required to serialize the Bing corpus.")
  }
  canon <- lapply(records, bing_corpus_build_record)
  case_ids <- vapply(canon, function(r) r[["case_id"]], character(1))
  canon <- canon[order(case_ids, method = "radix")]
  json <- jsonlite::toJSON(canon, auto_unbox = TRUE, null = "null", pretty = 2)
  paste0(json, "\n")
}

#' Read the installed Bing conformance corpus.
#'
#' Parses cases.json into a structured list of records (one per accepted
#' observation-level case). Fully offline; consumes the installed package tree
#' by default.
#'
#' @param dir Corpus directory. Defaults to the installed corpus.
#' @return A list of parsed case records.
#' @keywords internal
#' @noRd
read_bing_corpus <- function(dir = bing_corpus_dir()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required to read the Bing corpus.")
  }
  cases_path <- file.path(dir, "cases.json")
  if (!file.exists(cases_path)) {
    stop(sprintf("Bing corpus cases.json not found: %s", cases_path))
  }
  jsonlite::fromJSON(cases_path, simplifyVector = FALSE)
}

#' Fail-closed offline validator for the Bing conformance corpus.
#'
#' Verifies the installed corpus without any sibling checkout, network access,
#' or the native matcher. Returns `ok = FALSE` (never errors) on any violation,
#' except when jsonlite is genuinely unavailable. Checks: exactly 191 records
#' with the frozen per-profile split; unique case_ids; every profile, reason,
#' decision, and rule type in the allowed set; `matched_rule` is null if and
#' only if `reason == "default_allow"` (and `matched_rule_type == "none"` at the
#' facade); rule-type agrees with reason (allow/disallow); the recorded
#' `value_raw_hex` is even-length lower-case hex whose UTF-8 decode equals
#' `value`; every referenced body file is present with a SHA-256 and byte size
#' matching the record; `request_target_sha256` equals the SHA-256 of the
#' request-target bytes (the sibling probe-case pin); the tester-observed
#' decision equals the golden `matcher_expected.url_decision` (the SS16.3
#' authority link) and every case is `evaluated`; and deterministic
#' serialisation (re-serialising the parsed records with the shared canonical
#' writer reproduces the committed cases.json bytes exactly).
#'
#' @param dir Corpus directory. Defaults to the installed corpus.
#' @return A list with `ok` (logical) plus diagnostics.
#' @keywords internal
#' @noRd
verify_bing_corpus <- function(dir = bing_corpus_dir()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required to verify the Bing corpus.")
  }

  verify_env <- environment()
  errors <- character(0)
  fail <- function(msg) {
    assign("errors", c(get("errors", verify_env), msg), verify_env)
  }
  n_records <- NA_integer_
  n_bodies <- NA_integer_
  n_bingbot <- NA_integer_
  n_adidxbot <- NA_integer_
  result <- function() {
    list(
      ok = length(errors) == 0L,
      dir = dir,
      n_records = n_records,
      n_bodies = n_bodies,
      n_bingbot = n_bingbot,
      n_adidxbot = n_adidxbot,
      errors = errors
    )
  }

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

  counts <- bing_corpus_expected_counts()
  n_records <- length(records)
  if (n_records != counts$cases) {
    fail(sprintf(
      "Expected exactly %d records, found %d.", counts$cases, n_records
    ))
  }

  case_ids <- character(0)
  body_ids <- character(0)
  profiles <- character(0)
  required <- c(
    "case_id", "profile", "unit", "expectation_ids", "probe_case_id",
    "body_ref", "body_file", "body_sha256", "byte_size", "request_target",
    "request_target_sha256", "tester_observed", "matcher_expected"
  )
  for (i in seq_along(records)) {
    rec <- records[[i]]
    if (!is.list(rec)) {
      fail(sprintf("Record %d is not an object.", i))
      next
    }
    missing <- setdiff(required, names(rec))
    if (length(missing) > 0L) {
      fail(sprintf("Record %d is missing field(s): %s", i, toString(missing)))
      next
    }
    cid <- as.character(rec[["case_id"]])
    case_ids <- c(case_ids, cid)
    body_ids <- c(body_ids, as.character(rec[["body_ref"]]))

    profile <- as.character(rec[["profile"]])
    profiles <- c(profiles, profile)
    if (!profile %in% bing_corpus_profiles()) {
      fail(sprintf("Record %s has unknown profile '%s'.", cid, profile))
    }

    exp_ids <- rec[["expectation_ids"]]
    if (!is.list(exp_ids) || length(exp_ids) == 0L) {
      fail(sprintf("Record %s has an empty expectation_ids list.", cid))
    }

    expected <- rec[["matcher_expected"]]
    status <- as.character(expected[["matcher_status"]])
    if (!identical(status, "evaluated")) {
      fail(sprintf(
        "Record %s has matcher_status '%s' (all shipped cases are evaluated).",
        cid, status
      ))
    }
    dec <- as.character(expected[["url_decision"]])
    if (!dec %in% bing_corpus_decisions()) {
      fail(sprintf("Record %s has unknown url_decision '%s'.", cid, dec))
    }
    reason <- as.character(expected[["reason"]])
    if (!reason %in% bing_corpus_reasons()) {
      fail(sprintf("Record %s has unknown reason '%s'.", cid, reason))
    }

    matched_rule <- expected[["matched_rule"]]
    mr_null <- is.null(matched_rule)
    is_default <- identical(reason, "default_allow")
    if (mr_null != is_default) {
      fail(sprintf(
        paste0(
          "Record %s violates the matched_rule rule: matched_rule is %s but ",
          "reason is '%s' (matched_rule must be null iff default_allow)."
        ),
        cid, if (mr_null) "null" else "non-null", reason
      ))
    }
    if (!mr_null) {
      rtype <- as.character(matched_rule[["type"]])
      if (!rtype %in% bing_corpus_rule_types()) {
        fail(sprintf("Record %s has unknown matched rule type '%s'.", cid,
                     rtype))
      }
      # reason encodes the rule type exactly: rule_allow<->allow,
      # rule_disallow<->disallow.
      want_reason <- paste0("rule_", rtype)
      if (!identical(reason, want_reason)) {
        fail(sprintf(
          "Record %s rule type '%s' disagrees with reason '%s'.",
          cid, rtype, reason
        ))
      }
      # A disallow rule always disallows; an allow rule always allows here (the
      # shipped profile never emits the effective-empty inversion).
      if (!identical(dec, rtype)) {
        fail(sprintf(
          "Record %s decision '%s' disagrees with rule type '%s'.",
          cid, dec, rtype
        ))
      }
      value <- as.character(matched_rule[["value"]])
      raw_hex <- tolower(as.character(matched_rule[["value_raw_hex"]]))
      if (!grepl("^([0-9a-f]{2})*$", raw_hex)) {
        fail(sprintf("Record %s value_raw_hex is not even-length hex.", cid))
      } else if (!identical(raw_hex, tolower(bing_corpus_bytes_to_hex(
        charToRaw(enc2utf8(value))
      )))) {
        fail(sprintf(
          "Record %s value_raw_hex does not match the UTF-8 of value.", cid
        ))
      }
    }

    tester <- rec[["tester_observed"]]
    t_outcome <- as.character(tester[["terminal_outcome"]])
    t_dec <- as.character(tester[["decision"]])
    if (!identical(t_outcome, "evaluated")) {
      fail(sprintf("Record %s tester terminal_outcome is '%s' (not evaluated).",
                   cid, t_outcome))
    }
    # The SS16.3 authority link: the independently observed tester decision must
    # equal the golden engine decision.
    if (!identical(t_dec, dec)) {
      fail(sprintf(
        "Record %s tester decision '%s' != golden url_decision '%s'.",
        cid, t_dec, dec
      ))
    }

    # request_target_sha256 pins the sibling probe target bytes.
    rt <- as.character(rec[["request_target"]])
    got_rt_sha <- tryCatch(bing_corpus_sha256_string(rt),
                           error = function(e) NA_character_)
    want_rt_sha <- tolower(as.character(rec[["request_target_sha256"]]))
    if (is.na(got_rt_sha) || !identical(got_rt_sha, want_rt_sha)) {
      fail(sprintf("Record %s request_target_sha256 mismatch.", cid))
    }

    # Body file presence, SHA-256 and byte size.
    body_file <- as.character(rec[["body_file"]])
    body_path <- file.path(dir, body_file)
    if (file.exists(body_path)) {
      got_sha <- bing_corpus_sha256_file(body_path)
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
      fail(sprintf("Record %s references missing body file %s.", cid,
                   body_file))
    }
  }

  if (anyDuplicated(case_ids) > 0L) {
    dups <- unique(case_ids[duplicated(case_ids)])
    fail(sprintf("Duplicate case_id(s): %s", toString(dups)))
  }
  n_bodies <- length(unique(body_ids))
  if (!is.na(n_bodies) && n_bodies != counts$bodies) {
    fail(sprintf("Expected %d distinct bodies, found %d.", counts$bodies,
                 n_bodies))
  }
  n_bingbot <- sum(profiles == "bingbot")
  n_adidxbot <- sum(profiles == "adidxbot")
  if (n_bingbot != counts$bingbot || n_adidxbot != counts$adidxbot) {
    fail(sprintf(
      "Profile split mismatch: bingbot=%d (want %d), adidxbot=%d (want %d).",
      n_bingbot, counts$bingbot, n_adidxbot, counts$adidxbot
    ))
  }

  # Determinism: re-serialise and compare bytes with the committed file.
  reserialized <- tryCatch(
    bing_corpus_serialize(records),
    error = function(e) e
  )
  if (inherits(reserialized, "error")) {
    fail(sprintf("Re-serialisation failed: %s",
                 conditionMessage(reserialized)))
  } else {
    committed <- readBin(cases_path, "raw", n = file.size(cases_path))
    produced <- charToRaw(enc2utf8(reserialized))
    if (!identical(committed, produced)) {
      fail("cases.json is not byte-identical to its canonical serialisation.")
    }
  }

  result()
}

# Lower-case hex encoding of a raw vector, no separators. Shared by the
# validator and generator so the value_raw_hex contract has one encoder.
bing_corpus_bytes_to_hex <- function(bytes) {
  if (length(bytes) == 0L) {
    return("")
  }
  paste(format(as.hexmode(as.integer(bytes)), width = 2L), collapse = "")
}
