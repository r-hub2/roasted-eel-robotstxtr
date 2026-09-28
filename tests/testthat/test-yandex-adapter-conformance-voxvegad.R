# Fixture-driven conformance validation of the hidden Yandex adapter
# (ROBO-voxvegad, YI6b).
#
# Drives match_yandex_v1() end-to-end -- INCLUDING the §9 request-target
# extractor -- against ALL 140 accepted Yandex conformance expectations shipped
# offline by YI6a (inst/yandex-corpus/). The adapter stays hidden: this unit
# only reads the committed corpus and asserts adapter output equals the pinned
# expectations. It edits no engine/adapter/binding/extractor/fixture and never
# rewrites an expectation to accommodate adapter behaviour -- a disagreement is
# reported as a failing case, never masked.
#
# The corpus stores request_target (slash-prefixed ASCII); the adapter takes
# URLs. Per §9 the extractor strips scheme+authority without changing later
# bytes, so "https://example.test" + request_target round-trips back to exactly
# request_target for these 140 verified targets. That round-trip is asserted
# directly as a sub-check that the URL synthesis is faithful.
#
# jsonlite parses the corpus; the extractor and checked batch binding are
# native, so skip on a pure-R install.

skip_if_not_installed("jsonlite")
skip_if_not(
  is.function(tryCatch(robotstxtr_checked_batch_, error = function(e) NULL)) &&
    is.function(tryCatch(robotstxtr_extract_request_target_,
                         error = function(e) NULL)),
  "native binding not built (pure-R install)"
)

# ---------------------------------------------------------------------------
# Shared setup: read the corpus, synthesize inputs in corpus order, and run the
# adapter ONCE as a single batch so row i aligns to record i and parse-once is
# exercised.
# ---------------------------------------------------------------------------

corpus_dir <- yandex_corpus_dir()
records <- read_yandex_corpus(corpus_dir)

bodies <- lapply(records, function(r) {
  path <- file.path(corpus_dir, r$body_file)
  readBin(path, "raw", n = as.integer(r$byte_size))
})
targets <- vapply(records, function(r) r$request_target, character(1L))
urls <- paste0("https://example.test", targets)
product_tokens <- vapply(records, function(r) r$crawler, character(1L))

result <- match_yandex_v1(bodies, urls, product_tokens)

# ---------------------------------------------------------------------------
# §9 extractor round-trips: synthesized URL -> exactly the stored target.
# ---------------------------------------------------------------------------

test_that("the synthesized URL round-trips to the stored request-target", {
  extracted <- vapply(
    urls,
    robotstxtr_extract_request_target_,
    character(1L),
    USE.NAMES = FALSE
  )
  mismatches <- character(0)
  for (i in seq_along(records)) {
    if (!identical(extracted[[i]], targets[[i]])) {
      mismatches <- c(mismatches, sprintf(
        "[%s] request-target: expected %s, got %s",
        records[[i]]$case_id, targets[[i]], extracted[[i]]
      ))
    }
  }
  expect_equal(mismatches, character(0))
})

# ---------------------------------------------------------------------------
# Per-case conformance. One data-driven loop accumulates a structured mismatch
# record per case and asserts none, so a single failing case names itself
# (via its stable case_id) without dumping all 140.
# ---------------------------------------------------------------------------

test_that("all 140 accepted expectations match the adapter output", {
  expect_identical(nrow(result), 140L)

  acc <- new.env(parent = emptyenv())
  acc$mismatches <- character(0)
  note <- function(cid, field, expected, actual) {
    acc$mismatches <- c(acc$mismatches, sprintf(
      "[%s] %s: expected %s, got %s",
      cid, field, format(expected), format(actual)
    ))
  }

  for (i in seq_along(records)) {
    rec <- records[[i]]
    cid <- rec$case_id
    expected <- rec$expected
    matched_rule <- expected$matched_rule
    raw_i <- result$matched_rule_value_raw[[i]]

    # Supported-crawler + valid-target => a fully evaluated row.
    if (!identical(result$matcher_status[[i]], "evaluated")) {
      note(cid, "matcher_status", "evaluated", result$matcher_status[[i]])
    }
    if (!identical(result$native_evaluation_status[[i]], "evaluated")) {
      note(cid, "native_evaluation_status", "evaluated",
           result$native_evaluation_status[[i]])
    }
    if (!is.na(result$error_stage[[i]])) {
      note(cid, "error_stage", NA_character_, result$error_stage[[i]])
    }
    if (!is.na(result$error_class[[i]])) {
      note(cid, "error_class", NA_character_, result$error_class[[i]])
    }
    if (!is.na(result$error_message[[i]])) {
      note(cid, "error_message", NA_character_, result$error_message[[i]])
    }
    if (!identical(result$matcher_body_truncated[[i]], FALSE)) {
      note(cid, "matcher_body_truncated", FALSE,
           result$matcher_body_truncated[[i]])
    }
    want_bytes <- as.integer(rec$byte_size)
    if (!identical(result$matcher_input_bytes[[i]], want_bytes)) {
      note(cid, "matcher_input_bytes", want_bytes,
           result$matcher_input_bytes[[i]])
    }
    if (!identical(result$matcher_input_bytes[[i]], length(bodies[[i]]))) {
      note(cid, "matcher_input_bytes/body-length", length(bodies[[i]]),
           result$matcher_input_bytes[[i]])
    }

    # Decision and reason are the pinned expectation values verbatim.
    if (!identical(result$url_decision[[i]], expected$decision)) {
      note(cid, "url_decision", expected$decision, result$url_decision[[i]])
    }
    if (!identical(result$reason[[i]], expected$source)) {
      note(cid, "reason", expected$source, result$reason[[i]])
    }

    if (is.null(matched_rule)) {
      # default_allow: the absent-rule representation.
      if (!identical(expected$source, "default_allow")) {
        note(cid, "matched_rule-null-source", "default_allow",
             expected$source)
      }
      if (!is.na(result$matched_line[[i]])) {
        note(cid, "matched_line", NA_integer_, result$matched_line[[i]])
      }
      if (!is.na(result$matched_rule_value[[i]])) {
        note(cid, "matched_rule_value", NA_character_,
             result$matched_rule_value[[i]])
      }
      # Observed once and pinned: a no-rule row carries type "none".
      if (!identical(result$matched_rule_type[[i]], "none")) {
        note(cid, "matched_rule_type", "none", result$matched_rule_type[[i]])
      }
      # Absent rule => NULL raw (distinct from present-empty raw(0)).
      if (!is.null(raw_i)) {
        note(cid, "matched_rule_value_raw", "NULL", "non-NULL")
      }
    } else {
      # A present rule: line/type/value carried through exactly.
      want_line <- as.integer(matched_rule$line)
      if (!identical(result$matched_line[[i]], want_line)) {
        note(cid, "matched_line", want_line, result$matched_line[[i]])
      }
      if (!identical(result$matched_rule_type[[i]], matched_rule$type)) {
        note(cid, "matched_rule_type", matched_rule$type,
             result$matched_rule_type[[i]])
      }
      if (!identical(result$matched_rule_value[[i]], matched_rule$value)) {
        note(cid, "matched_rule_value", matched_rule$value,
             result$matched_rule_value[[i]])
      }
      # §11 exact bytes: raw decodes to the value byte-for-byte.
      want_raw <- charToRaw(enc2utf8(matched_rule$value))
      if (!identical(raw_i, want_raw)) {
        note(cid, "matched_rule_value_raw", "value bytes", "different bytes")
      }
      # A matched value's raw bytes are NUL-free.
      if (!is.null(raw_i) && any(raw_i == as.raw(0L))) {
        note(cid, "matched_rule_value_raw", "NUL-free", "contains NUL")
      }
      if (!nzchar(matched_rule$value)) {
        # effective_empty_disallow: PRESENT-empty => raw(0), NOT NULL.
        if (is.null(raw_i)) {
          note(cid, "matched_rule_value_raw", "raw(0)", "NULL")
        }
        if (!identical(raw_i, raw(0))) {
          note(cid, "matched_rule_value_raw", "raw(0)", "other")
        }
      }
    }
  }

  expect_equal(acc$mismatches, character(0))
})

# ---------------------------------------------------------------------------
# Stable identity of the run: 140 rows, unique case_ids, 26 distinct bodies,
# parse-once (n_parse_calls == distinct bodies).
# ---------------------------------------------------------------------------

test_that("the run covers 140 unique cases across 26 parse-once bodies", {
  expect_identical(nrow(result), 140L)

  case_ids <- vapply(records, function(r) r$case_id, character(1L))
  expect_length(unique(case_ids), 140L)

  body_ids <- vapply(records, function(r) r$body_id, character(1L))
  expect_length(unique(body_ids), 26L)

  expect_identical(attr(result, "n_parse_calls"), 26L)
})

# ---------------------------------------------------------------------------
# Data-only proof that this validation stays within the hidden-adapter regime:
# no Google matcher is invoked (only match_yandex_v1 + the extractor run above),
# Yandex availability is unchanged, and the schema revision is unchanged.
# ---------------------------------------------------------------------------

test_that("Yandex is active with the activation schema", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")

  registry <- engine_matcher_registry_v1()
  expect_identical(registry$yandex$availability, "available")
  expect_type(registry$yandex$callable, "closure")
})
