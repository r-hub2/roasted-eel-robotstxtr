# Slice R9 test gate: a representative, mechanically portable set of the
# canonical upstream Google robots.txt matcher behaviors, exercised THROUGH the
# R binding `allowed_by_robots_text()` (the R API over the vendored, frozen C++
# engine). The full C++ gtest suite already gates the engine on the C++ side
# (slice C2); this file confirms the same behaviors are reachable and correct
# via the R decision contract, so a future engine re-vendor cannot silently
# change R-visible matching. Every case is offline and pure (no HTTP).
#
# Coverage: path-prefix semantics, directory patterns, `*` wildcard, `$`
# end-anchor, wildcard-in-middle, Allow-vs-Disallow longest-match precedence and
# the equal-length Allow-wins tie, case sensitivity of paths, case-INsensitive
# user-agent group selection, empty-disallow-means-allow, a local empty body,
# and percent-escape / Unicode path decisions. Precedence cases also assert
# decision_source / matched_rule_type / matched_rule_value so the metadata
# contract is gated on non-trivial patterns.

# Return the `allowed` logical vector for a body/url(s)/UA. `url` may be a
# vector (scalar UA expands across it), matching the R2 vectorization contract.
dec <- function(body, url, ua = "FooBot") {
  allowed_by_robots_text(body, url, ua)$results$allowed
}

# --- Path prefix semantics (Google's documented `/fish` examples) -----------

test_that("disallow /fish matches by path prefix, case-sensitively", {
  body <- "user-agent: FooBot\ndisallow: /fish"
  urls <- c(
    "http://e/fish",                 # exact
    "http://e/fish.html",            # prefix, then an extension
    "http://e/fish/salmon.html",     # prefix, then a subpath
    "http://e/fishheads",            # prefix, no separator
    "http://e/fishheads/yummy.html", # prefix, no separator
    "http://e/fish?id=anything",     # prefix, then a query
    "http://e/Fish.asp",             # different casing, allowed
    "http://e/catfish",              # does not start with /fish
    "http://e/?id=fish"              # /fish not a prefix of the path
  )
  expect_identical(
    dec(body, urls),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("disallow /fish/ constrains the match to the directory", {
  body <- "user-agent: FooBot\ndisallow: /fish/"
  urls <- c(
    "http://e/fish/",              # exact directory
    "http://e/fish/?id=anything",  # directory, then a query
    "http://e/fish/salmon.htm",    # inside the directory
    "http://e/fish",               # not the directory itself, allowed
    "http://e/fish.html",          # not the directory, allowed
    "http://e/Fish/Salmon.asp"     # different casing, allowed
  )
  expect_identical(dec(body, urls), c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
})

# --- `*` wildcard -----------------------------------------------------------

test_that("disallow /*.php matches the .php substring, case-sensitively", {
  body <- "user-agent: FooBot\ndisallow: /*.php"
  urls <- c(
    "http://e/index.php",
    "http://e/filename.php",
    "http://e/folder/filename.php",
    "http://e/folder/filename.php?parameters",
    "http://e/",             # no .php in the path, allowed
    "http://e/windows.PHP"   # different casing, allowed
  )
  expect_identical(dec(body, urls), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
})

# --- `$` end-anchor ---------------------------------------------------------

test_that("disallow /*.php$ only matches paths ENDING in .php", {
  body <- "user-agent: FooBot\ndisallow: /*.php$"
  urls <- c(
    "http://e/filename.php",            # ends in .php
    "http://e/folder/filename.php",     # ends in .php
    "http://e/filename.php?parameters", # does not end in .php, allowed
    "http://e/filename.php5",           # does not end in .php, allowed
    "http://e/windows.PHP"              # different casing, allowed
  )
  expect_identical(dec(body, urls), c(FALSE, FALSE, TRUE, TRUE, TRUE))
})

# --- Wildcard in the middle -------------------------------------------------

test_that("disallow /fish*.php matches prefix, wildcard, and suffix", {
  body <- "user-agent: FooBot\ndisallow: /fish*.php"
  urls <- c(
    "http://e/fish.php",
    "http://e/fishheads/catfish.php?parameters",
    "http://e/Fish.PHP" # different casing, allowed
  )
  expect_identical(dec(body, urls), c(FALSE, FALSE, TRUE))
})

# --- Allow-vs-Disallow longest-match precedence -----------------------------

test_that("longest match wins: a more specific Allow beats a broad Disallow", {
  # allow /p (len 2) is more specific than disallow / (len 1) for /page.
  x <- allowed_by_robots_text(
    "user-agent: FooBot\ndisallow: /\nallow: /p", "http://e/page", "FooBot"
  )
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_allow")
  expect_identical(x$results$matched_rule_type, "allow")
  expect_identical(x$results$matched_rule_value, "/p")
})

test_that("equal-length Allow and Disallow tie is resolved for Allow", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\nallow: /folder\ndisallow: /folder",
    "http://e/folder/page", "FooBot"
  )
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "rule_allow")
  expect_identical(x$results$matched_rule_value, "/folder")
})

test_that("allow /$ opens only the homepage against a blanket disallow", {
  body <- "user-agent: FooBot\nallow: /$\ndisallow: /"
  root <- allowed_by_robots_text(body, "http://e/", "FooBot")
  page <- allowed_by_robots_text(body, "http://e/page.htm", "FooBot")

  expect_true(root$results$allowed)
  expect_identical(root$results$decision_source, "rule_allow")

  expect_false(page$results$allowed)
  expect_identical(page$results$decision_source, "rule_disallow")
})

test_that("the canonical matched value is surfaced for a wildcard rule", {
  x <- allowed_by_robots_text(
    "user-agent: FooBot\ndisallow: /*.php$", "http://e/a.php", "FooBot"
  )
  expect_false(x$results$allowed)
  expect_identical(x$results$matched_rule_type, "disallow")
  # Plain-ASCII pattern: MaybeEscapePattern is a no-op, value passes through.
  expect_identical(x$results$matched_rule_value, "/*.php$")
})

# --- Empty disallow means allow ---------------------------------------------

test_that("an empty Disallow value allows everything", {
  body <- "user-agent: FooBot\ndisallow:"
  # Empty Disallow = allow all; the canonical, uncontested decision is TRUE for
  # every path. (The empty-path directive is ignored by the matcher, so its
  # corrected metadata is default_allow/none/NA/NA; that is asserted in
  # test-empty-path-directives-r9.R. Here only the decision is checked.)
  expect_identical(
    dec(body, c("http://e/", "http://e/anything/at/all", "http://e/x")),
    c(TRUE, TRUE, TRUE)
  )
})

# --- Local empty body -------------------------------------------------------

test_that("an empty supplied body allows everything (default_allow)", {
  x <- allowed_by_robots_text("", "http://e/anything", "FooBot")
  expect_true(x$results$allowed)
  expect_identical(x$results$decision_source, "default_allow")
  expect_identical(x$results$matched_rule_type, "none")
  expect_true(is.na(x$results$matched_line))
  # The empty body is still recorded as a zero-length raw source.
  expect_identical(x$robots$body[[1]], raw(0))
  expect_identical(x$robots$body_size, 0L)
})

# --- User-agent group selection & case-insensitive UA matching --------------

test_that("the most specific user-agent group is selected", {
  body <- paste(
    "user-agent: FooBot",
    "disallow: /",
    "",
    "user-agent: *",
    "disallow: /private",
    sep = "\n"
  )
  # FooBot is blocked everywhere by its own group.
  expect_false(dec(body, "http://e/anywhere", "FooBot"))
  # A different agent falls through to the `*` group.
  expect_true(dec(body, "http://e/public", "BarBot"))
  expect_false(dec(body, "http://e/private", "BarBot"))
})

test_that("user-agent group matching is exact and case-insensitive", {
  body <- "user-agent: FooBot\ndisallow: /x"
  # The caller token is compared to the group name with case-insensitive
  # EQUALITY, so different casings of the bare token match.
  expect_false(dec(body, "http://e/x", "foobot"))
  expect_false(dec(body, "http://e/x", "FOOBOT"))
  # Faithful upstream: the robots.txt file's agent is extracted, but the caller
  # agent is compared whole. A version suffix or extra label is NOT trimmed to
  # match, so the caller must supply the bare product token. These do NOT match
  # the FooBot group and are therefore allowed.
  expect_true(dec(body, "http://e/x", "FooBot/2.1"))
  expect_true(dec(body, "http://e/x", "FooBot-News"))
})

# --- Percent-escape / Unicode path decisions --------------------------------

test_that("a percent-escaped directive matches the escaped URL path", {
  body <- "user-agent: FooBot\ndisallow: /caf%C3%A9"
  expect_false(dec(body, "http://e/caf%C3%A9"))
})

test_that("a literal-Unicode directive matches its escaped URL form", {
  # `é` in the pattern is canonicalized to %C3%A9 by the engine before matching,
  # so the escaped URL path matches the literal-Unicode directive.
  body <- "user-agent: FooBot\ndisallow: /café"
  expect_false(dec(body, "http://e/caf%C3%A9"))
})

test_that("prefix matching holds across a percent-escaped subpath", {
  body <- "user-agent: FooBot\ndisallow: /path"
  expect_false(dec(body, "http://e/path/%E3%83%84"))
})
