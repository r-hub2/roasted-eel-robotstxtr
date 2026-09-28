# Byte-preserving HTTP(S) request-target extractor (ROBO-rfkhiisf, YI4a).
#
# Characterizes the package-owned lexical extractor
# `robotstxtr_extract_request_target_()` against the integration spec section 9
# ("Absolute URL to request-target contract"). The extractor is hidden native
# glue (not user-exported); these tests reach it through the package namespace.
#
# All assertions are BYTE-EXACT and cross-platform: request targets are compared
# via charToRaw() so a locale- or platform-dependent re-encoding of literal
# Unicode would fail the test rather than pass silently.

# Force the UTF-8 native boundary the same way the facade does, so the C++ side
# receives the exact bytes regardless of the session locale.
utf8 <- function(x) enc2utf8(x)

# Compare the extracted target's raw bytes to an expected literal's raw bytes.
expect_target_bytes <- function(url, expected) {
  got <- robotstxtr_extract_request_target_(utf8(url))
  expect_length(got, 1L)
  expect_false(is.na(got))
  expect_identical(charToRaw(got), charToRaw(utf8(expected)))
}

test_that("the six normative spec examples extract byte-exactly", {
  expect_target_bytes("https://example.test", "/")
  expect_target_bytes("https://example.test?", "/?")
  expect_target_bytes("https://example.test?x=1", "/?x=1")
  expect_target_bytes("https://example.test/a//b?x=%2f", "/a//b?x=%2f")
  expect_target_bytes("https://example.test/a/../b#frag", "/a/../b")
  # café / ✓ compared as their UTF-8 bytes.
  expect_target_bytes("https://example.test/café?q=✓",
                      "/café?q=✓")
})

test_that("no explicit path yields a substituted '/'", {
  expect_target_bytes("https://example.test", "/")
  # Authority with a port but no path still substitutes '/'.
  expect_target_bytes("https://example.test:8443", "/")
  # Userinfo in the authority does not leak into the target.
  expect_target_bytes("https://user:pass@example.test", "/")
})

test_that("the query delimiter and exact spelling are retained", {
  expect_target_bytes("https://example.test/p?", "/p?")            # empty query
  expect_target_bytes("https://example.test?", "/?")              # bare '?'
  expect_target_bytes("https://example.test/p?a=1&a=2&b=3",
                      "/p?a=1&a=2&b=3")
})

test_that("query key order is preserved (no reordering)", {
  expect_target_bytes("https://example.test/p?z=1&a=2&m=3",
                      "/p?z=1&a=2&m=3")
})

test_that("percent-escape case is preserved (no normalization)", {
  # Mixed-case hex digits must survive verbatim in path and query.
  expect_target_bytes("https://example.test/%2F%2f?x=%AbcD",
                      "/%2F%2f?x=%AbcD")
})

test_that("literal Unicode is not percent-encoded after translation", {
  target <- robotstxtr_extract_request_target_(
    utf8("https://example.test/café"))
  # The 'é' remains its two raw UTF-8 bytes (0xC3 0xA9), not "%C3%A9".
  expect_identical(charToRaw(target), charToRaw(utf8("/café")))
  expect_false(grepl("%", target, fixed = TRUE))
  expect_identical(Encoding(target), "UTF-8")
})

test_that("parameters (';') are preserved verbatim", {
  expect_target_bytes("https://example.test/p;jsessionid=42;v=1?x=1",
                      "/p;jsessionid=42;v=1?x=1")
})

test_that("the fragment delimiter and fragment are excluded", {
  expect_target_bytes("https://example.test/p#frag", "/p")
  expect_target_bytes("https://example.test/p?q=1#frag", "/p?q=1")
  # A fragment that itself contains '?' does not reintroduce a query.
  expect_target_bytes("https://example.test/p#a?b", "/p")
  # A no-path URL with only a fragment substitutes '/'.
  expect_target_bytes("https://example.test#frag", "/")
})

test_that("duplicate slashes are preserved", {
  expect_target_bytes("https://example.test/a//b///c", "/a//b///c")
  expect_target_bytes("https://example.test//", "//")
})

test_that("dot segments are left untouched (no resolution)", {
  expect_target_bytes("https://example.test/a/../b", "/a/../b")
  expect_target_bytes("https://example.test/./a/./b", "/./a/./b")
  expect_target_bytes("https://example.test/../../x", "/../../x")
})

test_that("http and non-default schemes are handled by the '://' boundary", {
  expect_target_bytes("http://example.test/p?q=1", "/p?q=1")
})

test_that("malformed and empty input signal lexical failure (NA sentinel)", {
  # No "://" boundary -> cannot produce a slash-prefixed target.
  expect_true(is.na(robotstxtr_extract_request_target_("")))
  expect_true(is.na(robotstxtr_extract_request_target_("not-a-url")))
  expect_true(is.na(robotstxtr_extract_request_target_("example.test/path")))
  expect_true(is.na(
    robotstxtr_extract_request_target_("mailto:a@example.test")))
})

test_that("the extractor returns only its single owning value", {
  got <- robotstxtr_extract_request_target_(utf8("https://example.test/p?q=1"))
  expect_type(got, "character")
  expect_length(got, 1L)
})

test_that("Yandex is available but the extractor stays unexported", {
  # The lexical extractor now backs the active Yandex adapter, but it remains an
  # internal routine: the engine contract reports Yandex available while no
  # user-facing extractor export exists.
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
  expect_false("robotstxtr_extract_request_target_" %in%
                 getNamespaceExports("robotstxtr"))
})

test_that("no native routine name references Yandex (YI3b invariant holds)", {
  dll <- getLoadedDLLs()[["robotstxtr"]]
  skip_if(is.null(dll), "robotstxtr DLL not loaded (pure-R install)")
  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  names_registered <- vapply(routines, function(r) r$name, character(1))
  # The new extractor is registered under an engine-neutral name.
  expect_true("_robotstxtr_robotstxtr_extract_request_target_" %in%
                names_registered)
  expect_false(any(grepl("yandex", names_registered, ignore.case = TRUE)))
})
