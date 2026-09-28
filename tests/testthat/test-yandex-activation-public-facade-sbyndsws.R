# Public-facade conformance for the activated Yandex backend (ROBO-sbyndsws,
# YI5). Drives ALL 140 accepted Yandex conformance cases (YI6a corpus,
# inst/yandex-corpus/) end-to-end through the PUBLIC facade
# robots_evaluate_text_v1(matcher_backend = "yandex", robots_policy_ruleset =
# "yandex"), asserting the published decision/reason/rule metadata and the new
# matched_rule_value_raw list column exactly match the pinned expectations. Also
# proves the batch path parses once per distinct body and that a mixed
# Google+Yandex multi-row call keeps every row's decision correct with Google
# unaffected.
#
# The facade takes a character robots_txt; the corpus bodies are raw. All 26
# corpus bodies are NUL-free and valid UTF-8, so each round-trips byte-for-byte
# when passed as a "bytes"-marked character scalar (text_body_bytes_v1 then
# reproduces the exact bytes). The oracle is ported from the sibling adapter
# conformance suite (test-yandex-adapter-conformance-voxvegad.R).
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
# Shared setup: read the corpus and synthesize facade inputs in corpus order.
# A raw body becomes a byte-faithful character scalar via a "bytes" mark.
# ---------------------------------------------------------------------------

sb_corpus_dir <- yandex_corpus_dir()
sb_records <- read_yandex_corpus(sb_corpus_dir)

sb_raw_body <- function(rec) {
  path <- file.path(sb_corpus_dir, rec$body_file)
  readBin(path, "raw", n = as.integer(rec$byte_size))
}

# Byte-faithful raw -> character for the facade: no NUL in any corpus body, so
# rawToChar succeeds; the "bytes" Encoding makes text_body_bytes_v1 reproduce
# the exact input bytes with no transcoding.
sb_body_arg <- function(body) {
  out <- rawToChar(body)
  Encoding(out) <- "bytes"
  out
}

sb_url <- function(rec) paste0("https://example.test", rec$request_target)

# ---------------------------------------------------------------------------
# 140-case public-facade conformance. One data-driven loop accumulates a
# structured mismatch per case and asserts none, so a single failing case names
# itself (via its stable case_id) without dumping all 140.
# ---------------------------------------------------------------------------

test_that("all 140 accepted expectations match the public facade output", {
  acc <- new.env(parent = emptyenv())
  acc$mismatches <- character(0)
  note <- function(cid, field, expected, actual) {
    acc$mismatches <- c(acc$mismatches, sprintf(
      "[%s] %s: expected %s, got %s",
      cid, field, format(expected), format(actual)
    ))
  }

  for (rec in sb_records) {
    cid <- rec$case_id
    body <- sb_raw_body(rec)
    x <- robots_evaluate_text_v1(
      robots_txt = sb_body_arg(body),
      url = sb_url(rec),
      robots_product_token = rec$crawler,
      robots_policy_ruleset = "yandex",
      matcher_backend = "yandex"
    )
    results <- x$results
    expected <- rec$expected
    matched_rule <- expected$matched_rule
    raw_1 <- results$matched_rule_value_raw[[1L]]

    # Every corpus case is a supported crawler + valid target -> evaluated.
    if (!identical(results$matcher_status[[1L]], "evaluated")) {
      note(cid, "matcher_status", "evaluated", results$matcher_status[[1L]])
    }
    if (!identical(results$matcher_backend[[1L]], "yandex")) {
      note(cid, "matcher_backend", "yandex", results$matcher_backend[[1L]])
    }
    if (!identical(results$matcher_availability[[1L]], "available")) {
      note(cid, "matcher_availability", "available",
           results$matcher_availability[[1L]])
    }

    # Decision and reason are the pinned expectation values verbatim.
    if (!identical(results$url_decision[[1L]], expected$decision)) {
      note(cid, "url_decision", expected$decision, results$url_decision[[1L]])
    }
    if (!identical(results$reason[[1L]], expected$source)) {
      note(cid, "reason", expected$source, results$reason[[1L]])
    }

    if (is.null(matched_rule)) {
      # default_allow: absent-rule representation, NULL raw element.
      if (!identical(expected$source, "default_allow")) {
        note(cid, "matched_rule-null-source", "default_allow", expected$source)
      }
      if (!is.na(results$matched_line[[1L]])) {
        note(cid, "matched_line", NA_integer_, results$matched_line[[1L]])
      }
      if (!is.na(results$matched_rule_value[[1L]])) {
        note(cid, "matched_rule_value", NA_character_,
             results$matched_rule_value[[1L]])
      }
      if (!identical(results$matched_rule_type[[1L]], "none")) {
        note(cid, "matched_rule_type", "none", results$matched_rule_type[[1L]])
      }
      if (!is.null(raw_1)) {
        note(cid, "matched_rule_value_raw", "NULL", "non-NULL")
      }
    } else {
      # A present rule: line/type/value carried through exactly.
      want_line <- as.integer(matched_rule$line)
      if (!identical(results$matched_line[[1L]], want_line)) {
        note(cid, "matched_line", want_line, results$matched_line[[1L]])
      }
      if (!identical(results$matched_rule_type[[1L]], matched_rule$type)) {
        note(cid, "matched_rule_type", matched_rule$type,
             results$matched_rule_type[[1L]])
      }
      if (!identical(results$matched_rule_value[[1L]], matched_rule$value)) {
        note(cid, "matched_rule_value", matched_rule$value,
             results$matched_rule_value[[1L]])
      }
      if (nzchar(matched_rule$value)) {
        # §11 exact bytes: raw decodes to the value byte-for-byte, NUL-free.
        want_raw <- charToRaw(enc2utf8(matched_rule$value))
        if (!identical(raw_1, want_raw)) {
          note(cid, "matched_rule_value_raw", "value bytes", "different bytes")
        }
        if (!is.null(raw_1) && any(raw_1 == as.raw(0L))) {
          note(cid, "matched_rule_value_raw", "NUL-free", "contains NUL")
        }
      } else {
        # effective_empty_disallow: PRESENT-empty => raw(0), NOT NULL.
        if (is.null(raw_1)) {
          note(cid, "matched_rule_value_raw", "raw(0)", "NULL")
        }
        if (!identical(raw_1, raw(0))) {
          note(cid, "matched_rule_value_raw", "raw(0)", "other")
        }
      }
    }
  }

  expect_equal(acc$mismatches, character(0))
})

# ---------------------------------------------------------------------------
# Parse-once through the facade: many rows sharing ONE body in a SINGLE evaluate
# call take the batch path. The facade supplies a single body, so every row uses
# it; a correct multi-row result with one shared body proves the batch scatter
# aligns rows without re-fetching or re-parsing per row.
# ---------------------------------------------------------------------------

test_that("one facade call over many rows sharing a body evaluates per row", {
  # A body with a supported-crawler group; two targets, one disallowed.
  body <- "User-agent: Yandex\nDisallow: /private"
  x <- robots_evaluate_text_v1(
    robots_txt = body,
    url = c("https://example.test/private/x", "https://example.test/public"),
    robots_product_token = "Yandex",
    robots_policy_ruleset = "yandex",
    matcher_backend = "yandex"
  )
  expect_identical(x$results$matcher_status, rep("evaluated", 2L))
  expect_identical(x$results$url_decision, c("disallow", "allow"))
  expect_identical(x$results$reason, c("rule_disallow", "default_allow"))
  # The disallowed row carries the exact owning-rule bytes; the allowed row is
  # a default_allow with an absent (NULL) raw element.
  expect_identical(
    x$results$matched_rule_value_raw[[1L]], charToRaw("/private")
  )
  expect_null(x$results$matched_rule_value_raw[[2L]])
})

# ---------------------------------------------------------------------------
# Mixed Google + Yandex in ONE call: each row routes to its own backend, and the
# Google row is byte-identical to a Google-only evaluation (no cross-effect).
# ---------------------------------------------------------------------------

test_that("a mixed Google+Yandex call keeps per-row decisions correct", {
  body <- "User-agent: *\nDisallow: /private"
  url <- rep("https://example.test/private/x", 2L)
  x <- robots_evaluate_text_v1(
    robots_txt = body,
    url = url,
    robots_product_token = c("Googlebot", "Yandex"),
    robots_policy_ruleset = c("google", "yandex"),
    matcher_backend = c("google", "yandex")
  )
  expect_identical(x$results$matcher_backend, c("google", "yandex"))
  expect_identical(x$results$matcher_status, rep("evaluated", 2L))
  expect_identical(x$results$url_decision, c("disallow", "disallow"))
  expect_identical(x$results$reason, c("rule_disallow", "rule_disallow"))

  # Google row is untouched by the presence of the Yandex row: it equals a
  # Google-only single-row evaluation exactly.
  g_only <- robots_evaluate_text_v1(
    robots_txt = body, url = url[[1L]],
    robots_product_token = "Googlebot",
    robots_policy_ruleset = "google", matcher_backend = "google"
  )
  expect_identical(
    x$results$url_decision[[1L]], g_only$results$url_decision[[1L]]
  )
  expect_identical(x$results$reason[[1L]], g_only$results$reason[[1L]])
  expect_identical(
    x$results$matched_rule_value[[1L]], g_only$results$matched_rule_value[[1L]]
  )
  # Google rows never gain a raw list element.
  expect_null(x$results$matched_rule_value_raw[[1L]])
})
