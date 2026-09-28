# Slice R5: robots_origin() constructs the robots.txt fetch-origin grouping key
# `scheme://host[:port]/robots.txt` from a target URL (§6.3), and rejects
# ineligible input with NA before any rurl parse (explicit-scheme guard, §6.2).
# All cases are pure and offline; no HTTP request is performed.

test_that("IDN punycoded, path/query/fragment ignored (tracer bullet)", {
  expect_identical(
    robots_origin("https://例え.jp/a?b#c"),
    "https://xn--r8jz45g.jp/robots.txt"
  )
})

test_that("IDN subdomain labels are preserved and punycoded", {
  expect_identical(
    robots_origin("https://sub.例え.jp/a"),
    "https://sub.xn--r8jz45g.jp/robots.txt"
  )
})

test_that("IPv4 literal is preserved", {
  expect_identical(
    robots_origin("http://192.0.2.1/some/path"),
    "http://192.0.2.1/robots.txt"
  )
})

test_that("IPv6 literal keeps its brackets and is lowercased", {
  expect_identical(
    robots_origin("http://[2001:DB8::1]/x"),
    "http://[2001:db8::1]/robots.txt"
  )
})

test_that("userinfo is omitted", {
  expect_identical(
    robots_origin("http://user:pass@example.com/x"),
    "http://example.com/robots.txt"
  )
})

test_that("explicit non-default port is kept", {
  expect_identical(
    robots_origin("http://example.com:8080/x"),
    "http://example.com:8080/robots.txt"
  )
  expect_identical(
    robots_origin("https://[2001:db8::1]:8443/x"),
    "https://[2001:db8::1]:8443/robots.txt"
  )
})

test_that("default ports are omitted for http (80) and https (443)", {
  expect_identical(
    robots_origin("http://example.com:80/x"),
    "http://example.com/robots.txt"
  )
  expect_identical(
    robots_origin("https://example.com:443/x"),
    "https://example.com/robots.txt"
  )
})

test_that("fragment is ignored", {
  expect_identical(
    robots_origin("http://example.com/page#section"),
    "http://example.com/robots.txt"
  )
})

test_that("query string is ignored", {
  expect_identical(
    robots_origin("http://example.com/search?q=1&r=2"),
    "http://example.com/robots.txt"
  )
})

test_that("path is ignored", {
  expect_identical(
    robots_origin("https://example.com/deep/nested/path/file.html"),
    "https://example.com/robots.txt"
  )
})

test_that("uppercase scheme and host are lowercased", {
  expect_identical(
    robots_origin("HTTP://Example.COM/X"),
    "http://example.com/robots.txt"
  )
})

test_that("scheme-less input is rejected before any rurl parse (NA)", {
  expect_identical(robots_origin("example.com/x"), NA_character_)
})

test_that("scheme-relative input is rejected before any rurl parse (NA)", {
  expect_identical(robots_origin("//example.com/x"), NA_character_)
})

test_that("non-http(s) scheme is ineligible (NA)", {
  expect_identical(robots_origin("ftp://example.com/x"), NA_character_)
})

test_that("host-less http(s) input is rejected as unparseable (NA)", {
  expect_identical(robots_origin("http://"), NA_character_)
  expect_identical(robots_origin("https://"), NA_character_)
})

test_that("missing, empty, and non-scalar input return NA", {
  expect_identical(robots_origin(NA_character_), NA_character_)
  expect_identical(robots_origin(""), NA_character_)
  expect_identical(robots_origin(character(0)), NA_character_)
  expect_identical(robots_origin(c("http://a/x", "http://b/y")), NA_character_)
})
