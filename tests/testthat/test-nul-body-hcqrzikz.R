# ROBO-hcqrzikz: a NUL-bearing fetched robots.txt must not abort the call.
#
# Every decode of ACQUIRED bytes used to call a bare `rawToChar()`, which raises
# an unclassed base R `simpleError` ("embedded nul in string") as soon as a NUL
# is followed by another byte. A server serving a stray NUL at /robots.txt --
# a UTF-16-encoded file, a WAF challenge page, a truncated or NUL-padded
# response -- therefore aborted the whole call, and escaped downstream through
# the v1 engine facade.
#
# These tests pin the fix at each public entry point AND pin the rule the fix
# chose (R/body-decode.R): NUL bytes are REMOVED, the document is not truncated
# at them, and the malformation stays visible through `robots_validate_text()`.

# --- Test helpers (local to this file) --------------------------------------

# A 200 text/plain response whose body is supplied as RAW bytes, so a NUL can be
# served verbatim (charToRaw() of a string could never produce one).
nul_ok_bytes <- function(bytes) {
  force(bytes)
  function(req) {
    httr2::response(
      status_code = 200L, url = req$url,
      headers = list(`content-type` = "text/plain"), body = bytes
    )
  }
}

nul_router <- function(routes) {
  function(req) {
    handler <- routes[[req$url]]
    if (is.null(handler)) {
      stop(sprintf("unexpected request URL in mock: %s", req$url))
    }
    handler(req)
  }
}

# "user-agent: *\n" + NUL + "disallow: /x\n" + "disallow: /y\n". The NUL sits at
# the start of the first rule line, so a decode that truncated at it would lose
# BOTH rules, and one that kept it would leave that line an unknown directive.
nul_body_bytes <- c(
  charToRaw("user-agent: *\n"),
  as.raw(0),
  charToRaw("disallow: /x\ndisallow: /y\n")
)

# --- The two decode helpers --------------------------------------------------

test_that("render_body_bytes() honours its no-error contract on a NUL", {
  out <- render_body_bytes(as.raw(c(0x55, 0x73, 0x65, 0x72, 0x00, 0x61)))
  expect_identical(out, "Usera")

  # Bytes that are valid UTF-8 once the NUL is gone are still marked UTF-8 (an
  # all-ASCII result stays "unknown", which is R's own no-op for ASCII).
  utf8 <- render_body_bytes(as.raw(c(0x63, 0x61, 0x66, 0x00, 0xc3, 0xa9)))
  expect_identical(Encoding(utf8), "UTF-8")
  expect_identical(charToRaw(utf8), as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9)))
})

test_that("render_body_bytes() marks bytes for invalid UTF-8 past a NUL", {
  out <- render_body_bytes(as.raw(c(0x2f, 0x00, 0xff)))
  expect_identical(Encoding(out), "bytes")
  expect_identical(charToRaw(out), as.raw(c(0x2f, 0xff)))
})

test_that("matcher_string_v1() decodes NUL-bearing bytes without erroring", {
  out <- matcher_string_v1(nul_body_bytes)
  expect_identical(out, "user-agent: *\ndisallow: /x\ndisallow: /y\n")

  # The matcher input is marked UTF-8 so cpp11 forwards the exact bytes; a
  # non-ASCII body shows the mark (ASCII-only results stay "unknown" in R).
  marked <- matcher_string_v1(c(as.raw(0), charToRaw("disallow: /café")))
  expect_identical(Encoding(marked), "UTF-8")
})

test_that("the NUL rule removes bytes rather than truncating the document", {
  # Truncating at the NUL would drop both rules; keeping it would leave the
  # first rule line an unknown "\0disallow" directive. Removal keeps both.
  expect_identical(
    nul_free_bytes(nul_body_bytes),
    charToRaw("user-agent: *\ndisallow: /x\ndisallow: /y\n")
  )
})

# --- Entry point 1: robots_body() preview ------------------------------------

test_that("robots_body() previews a NUL-bearing fetched body byte-safely", {
  httr2::local_mocked_responses(nul_router(list(
    "http://a/robots.txt" = nul_ok_bytes(nul_body_bytes)
  )))
  x <- allowed_by_robots_url("http://a/x", "TestBot")

  # The preview renders; the NUL is gone from the string but the STORED bytes
  # are untouched, so raw = TRUE still round-trips the body byte-for-byte.
  expect_identical(
    robots_body(x, n = Inf),
    "user-agent: *\ndisallow: /x\ndisallow: /y\n"
  )
  expect_identical(robots_body(x, raw = TRUE), nul_body_bytes)
})

# --- Entry point 2: allowed_by_robots_url() ----------------------------------

test_that("allowed_by_robots_url() matches a NUL-bearing body, no abort", {
  httr2::local_mocked_responses(nul_router(list(
    "http://a/robots.txt" = nul_ok_bytes(nul_body_bytes)
  )))
  urls <- c("http://a/x", "http://a/y", "http://a/z")
  x <- allowed_by_robots_url(urls, "TestBot")

  # Both rules are live (NUL removed, nothing truncated) and the row keeps the
  # ordinary fetched-source contract rather than an error.
  expect_identical(x$results$allowed, c(FALSE, FALSE, TRUE))
  expect_identical(
    x$results$decision_source,
    c("rule_disallow", "rule_disallow", "default_allow")
  )
  expect_identical(x$robots$fetch_outcome, "fetched")
  expect_identical(x$robots$error_class, NA_character_)
  # Match-metadata correlation runs the parse collector over the same decoded
  # body; its value channel cannot carry a NUL either, so this pins that too.
  expect_identical(x$results$matched_rule_value, c("/x", "/y", NA_character_))
})

# --- Entry point 3: robots_evaluate_url_v1() (and sitemapr's caller) ---------

test_that("robots_evaluate_url_v1() evaluates a NUL-bearing body", {
  httr2::local_mocked_responses(nul_router(list(
    "http://a/robots.txt" = nul_ok_bytes(nul_body_bytes)
  )))
  x <- robots_evaluate_url_v1(
    "http://a/x", robots_product_token = "TestBot",
    robots_policy_ruleset = "google", matcher_backend = "google",
    ssrf_guard = FALSE
  )

  expect_identical(x$results$url_decision, "disallow")
  expect_identical(x$results$reason, "rule_disallow")
  expect_identical(x$results$matched_rule_value, "/x")
  expect_identical(x$results$matcher_status, "evaluated")
})

# --- The malformation stays visible -----------------------------------------

test_that("robots_validate_text() still reports the NUL it decoded past", {
  v <- robots_validate_text(nul_body_bytes)
  nul <- v$diagnostics[v$diagnostics$code == "nul_byte", , drop = FALSE]

  expect_identical(nrow(nul), 1L)
  expect_identical(nul$severity, "error")
  expect_identical(nul$line, 2L)
  # The byte itself is rendered, not silently dropped, in the diagnostic.
  expect_match(nul$raw_text, "\\\\x00", fixed = FALSE)
  expect_identical(v$documents$validation_status, "error")
})
