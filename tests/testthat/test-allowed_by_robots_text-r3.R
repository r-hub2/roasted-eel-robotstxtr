# Slice R3: match-metadata correlation for allowed_by_robots_text(). A matched
# row's matched_rule_type is CALLBACK-derived and matched_rule_value carries the
# canonical (post-MaybeEscapePattern) callback value the matcher actually used.
# Unmatched rows keep matched_rule_value = NA and decision-derived type. A
# positive matching line absent from the parse collector raises a package error.

# --- Comments and whitespace ------------------------------------------------

test_that("comment lines are skipped and the matched value is filled", {
  body <- paste(
    "# leading comment",
    "user-agent: *",
    "disallow: /x  # trailing comment",
    sep = "\n"
  )
  x <- allowed_by_robots_text(body, "http://a/x", "bot")
  expect_identical(x$results$decision_source, "rule_disallow")
  expect_identical(x$results$matched_line, 3L)
  expect_identical(x$results$matched_rule_type, "disallow")
  # The trailing comment is stripped by the parser before the callback.
  expect_identical(x$results$matched_rule_value, "/x")
})

test_that("surrounding whitespace is stripped from the callback value", {
  body <- "user-agent: *\ndisallow:    /ws   "
  x <- allowed_by_robots_text(body, "http://a/ws", "bot")
  expect_identical(x$results$matched_line, 2L)
  expect_identical(x$results$matched_rule_type, "disallow")
  expect_identical(x$results$matched_rule_value, "/ws")
})

# --- Acceptable missing-colon syntax ----------------------------------------

test_that("a missing-colon directive is matched and its value correlated", {
  # `disallow /nc` (no colon separator) is an accepted syntax variant upstream.
  body <- "user-agent: *\ndisallow /nc"
  x <- allowed_by_robots_text(body, "http://a/nc", "bot")
  expect_false(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_disallow")
  expect_identical(x$results$matched_line, 2L)
  expect_identical(x$results$matched_rule_type, "disallow")
  expect_identical(x$results$matched_rule_value, "/nc")
})

# --- Allow that wins: callback-derived type ---------------------------------

test_that("an Allow that wins gets a callback-derived type and value", {
  body <- "user-agent: *\ndisallow: /\nallow: /ok"
  x <- allowed_by_robots_text(body, "http://a/ok", "bot")
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_allow")
  expect_identical(x$results$matched_rule_type, "allow")
  expect_identical(x$results$matched_rule_value, "/ok")
})

# --- Unicode: post-escape canonicalization is observable --------------------

test_that("a non-ASCII directive value is surfaced post-MaybeEscapePattern", {
  # Literal UTF-8 `é` in the pattern is canonicalized to %C3%A9 by the engine
  # before the callback. The URL must be %-encoded (upper hex) to match it.
  body <- "user-agent: *\ndisallow: /café"
  x <- allowed_by_robots_text(body, "http://a/caf%C3%A9", "bot")
  expect_false(x$results$allowed)
  expect_identical(x$results$matched_line, 2L)
  expect_identical(x$results$matched_rule_type, "disallow")
  # Canonical (percent-escaped) value, NOT the raw pre-escape `/café`.
  expect_identical(x$results$matched_rule_value, "/caf%C3%A9")
  expect_false(identical(x$results$matched_rule_value, "/café"))
})

# --- Percent escapes: lowercase hex is upper-cased --------------------------

test_that("a lowercase-hex percent escape is canonicalized to upper hex", {
  body <- "user-agent: *\ndisallow: /p%2fq"
  x <- allowed_by_robots_text(body, "http://a/p%2Fq", "bot")
  expect_false(x$results$allowed)
  expect_identical(x$results$matched_line, 2L)
  expect_identical(x$results$matched_rule_type, "disallow")
  # `%2f` in the directive is normalized to `%2F` before the callback.
  expect_identical(x$results$matched_rule_value, "/p%2Fq")
})

# --- Unmatched rows keep NA value and decision-derived type -----------------

test_that("default_allow and input_unknown rows keep NA value", {
  body <- "user-agent: *\ndisallow: /x\nallow: /ok"
  x <- allowed_by_robots_text(
    body,
    c("http://a/x", "http://a/ok", "http://a/free", ""),
    "bot"
  )
  expect_identical(
    x$results$decision_source,
    c("rule_disallow", "rule_allow", "default_allow", "input_unknown")
  )
  expect_identical(
    x$results$matched_rule_type,
    c("disallow", "allow", "none", "unknown")
  )
  expect_identical(
    x$results$matched_rule_value,
    c("/x", "/ok", NA_character_, NA_character_)
  )
})

# --- Collector binding: type/value and bytes encoding -----------------------

test_that("the collector reports one row per directive with callback values", {
  body <- "user-agent: *\ndisallow: /x\n# comment\nallow: /y"
  d <- robotstxtr_collect_directives_(body)
  expect_named(d, c("line", "type", "value"))
  expect_identical(d$line, c(1L, 2L, 4L))
  expect_identical(d$type, c("user-agent", "disallow", "allow"))
  expect_identical(d$value, c("*", "/x", "/y"))
})

test_that("a non-UTF-8 callback value is returned with Encoding = 'bytes'", {
  # A Latin-1 byte (0xe9) in an unescaped user-agent value is not valid UTF-8.
  # The body is passed as UTF-8-marked bytes exactly as allowed_by_robots_text
  # prepares it; the collector must surface the value verbatim, marked bytes.
  raw_body <- as.raw(c(charToRaw("user-agent: caf"), as.raw(0xe9)))
  body <- rawToChar(raw_body)
  Encoding(body) <- "UTF-8"

  d <- robotstxtr_collect_directives_(body)
  expect_identical(d$type, "user-agent")
  expect_identical(Encoding(d$value[[1]]), "bytes")
  expect_identical(
    charToRaw(d$value[[1]]),
    as.raw(c(charToRaw("caf"), as.raw(0xe9)))
  )
})

# --- Synthetic missing-line invariant failure -------------------------------

test_that("a positive matching line with no collected directive errors", {
  # Degenerate case: the per-source lookup lacks the matched line. This is an
  # internal invariant failure and must raise a classed package error, not NA.
  lookup <- data.frame(
    line = c(1L, 2L),
    type = c("user-agent", "allow"),
    value = c("*", "/ok"),
    stringsAsFactors = FALSE
  )
  expect_error(
    correlate_match_metadata(
      matching_line = 5L,
      source_id = "supplied",
      matched_rule_type = "allow",
      matched_rule_value = NA_character_,
      lookups = list(supplied = lookup)
    ),
    class = "robotstxtr_missing_collected_line"
  )
})

# --- Collector: every directive callback is recorded ------------------------

test_that("sitemap and unknown-action directives are collected", {
  # The collector stores every directive kind, not just Allow/Disallow, so its
  # per-line lookup is complete. Sitemap and unknown-action lines exercise the
  # HandleSitemap / HandleUnknownAction callbacks.
  body <- paste(
    "user-agent: *",
    "sitemap: http://a/sitemap.xml",
    "somethingelse: value-here",
    sep = "\n"
  )
  d <- collect_directive_lookup(body)
  expect_identical(d$type[d$line == 2L], "sitemap")
  expect_identical(d$value[d$line == 2L], "http://a/sitemap.xml")
  expect_identical(d$type[d$line == 3L], "unknown")
  expect_identical(d$value[d$line == 3L], "value-here")
})

# --- Collector: callback-value encoding marking -----------------------------

test_that("multi-byte UTF-8 directive values are marked UTF-8", {
  # Sitemap/user-agent values are NOT run through MaybeEscapePattern, so raw 2-,
  # 3-, and 4-byte UTF-8 sequences reach the collector verbatim and must be
  # marked Encoding = "UTF-8" (exercises the multi-byte branches of the
  # binding's is_valid_utf8 well-formedness check).
  body <- paste(
    "user-agent: bot-é",               # two-byte sequence
    "sitemap: http://a/€",             # three-byte sequence
    "sitemap: http://a/\U0001D11E",    # four-byte sequence
    sep = "\n"
  )
  d <- collect_directive_lookup(body)
  vals <- d$value[d$line %in% 1:3]
  expect_identical(Encoding(vals), rep("UTF-8", 3L))
  expect_true(any(grepl("€", vals, fixed = TRUE)))
})

test_that("a non-UTF-8 directive value is surfaced as Encoding = bytes", {
  # A body force-marked UTF-8 but carrying malformed bytes (an invalid leading
  # byte) must not trigger a translation error: the collector detects the
  # ill-formed value and surfaces it verbatim with Encoding = "bytes".
  body <- rawToChar(c(charToRaw("sitemap: http://a/"), as.raw(c(0xff, 0xfe))))
  Encoding(body) <- "UTF-8"
  d <- collect_directive_lookup(body)
  sm <- d$value[d$type == "sitemap"]
  expect_identical(Encoding(sm), "bytes")
  expect_identical(nchar(sm, type = "bytes"), 11L)
})
