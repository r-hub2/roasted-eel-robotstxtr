# Slice R4: robots_body() previews or extracts a stored robots.txt body from a
# robots_decisions object. Covers the default preview, full extraction
# (n = Inf), the raw round-trip (raw = TRUE returns the stored raw vector
# byte-for-byte), source selection by source_id, and the multi-source error when
# source_id is omitted. Multi-source objects are built by hand (no public
# multi-source constructor exists yet) using the stable robots-table shape.

# Build a robots_decisions carrying one row per supplied body. Only the fields
# robots_body() reads (source_id, body) need to be faithful; the rest mirror the
# R2 supplied-source schema so the object is realistic.
make_decisions <- function(bodies) {
  ids <- names(bodies)
  robots <- data.frame(
    source_id = ids,
    source_type = "supplied",
    robots_url = NA_character_,
    effective_url = NA_character_,
    http_status = NA_integer_,
    fetch_outcome = "supplied",
    redirect_count = 0L,
    body_size = vapply(bodies, length, integer(1)),
    timeout = NA_real_,
    max_bytes = NA_integer_,
    error_stage = NA_character_,
    error_class = NA_character_,
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
  robots$body <- unname(bodies)
  new_robots_decisions(results = data.frame(), robots = robots)
}

# --- Preview (default) -------------------------------------------------------

test_that("the default preview renders the first n = 20 bytes as text", {
  # Body is 26 bytes, so the n = 20 default truncates to the first 20.
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/x", "b")
  out <- robots_body(x)
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_identical(out, "user-agent: *\ndisall")
  expect_identical(nchar(out, type = "bytes"), 20L)
})

test_that("n limits the preview to the first n bytes", {
  x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://a/x", "b")
  out <- robots_body(x, n = 10)
  expect_identical(out, "user-agent")
  expect_identical(nchar(out, type = "bytes"), 10L)
})

test_that("n larger than the body returns the whole body", {
  x <- allowed_by_robots_text("user-agent: *", "http://a/x", "b")
  expect_identical(robots_body(x, n = 1000), "user-agent: *")
})

# --- Full extraction (n = Inf) ----------------------------------------------

test_that("n = Inf returns the full body", {
  body <- paste(
    rep("user-agent: *\ndisallow: /very-long-path", 20),
    collapse = "\n"
  )
  x <- allowed_by_robots_text(body, "http://a/x", "b")
  out <- robots_body(x, n = Inf)
  expect_identical(out, body)
  # And it is longer than the default preview would show.
  expect_gt(nchar(out, type = "bytes"), 20L)
})

# --- Raw round-trip (raw = TRUE) --------------------------------------------

test_that("raw = TRUE returns the stored raw vector unchanged, byte-for-byte", {
  body <- "user-agent: *\ndisallow: /x"
  x <- allowed_by_robots_text(body, "http://a/x", "b")
  out <- robots_body(x, raw = TRUE)
  expect_type(out, "raw")
  expect_identical(out, x$robots$body[[1L]])
  expect_identical(out, charToRaw(body))
})

test_that("raw = TRUE ignores n and returns the complete body", {
  body <- "user-agent: *\ndisallow: /x"
  x <- allowed_by_robots_text(body, "http://a/x", "b")
  expect_identical(robots_body(x, n = 3, raw = TRUE), charToRaw(body))
})

# --- Source selection --------------------------------------------------------

test_that("a single source is selected when source_id is omitted", {
  x <- make_decisions(list(only = charToRaw("A")))
  expect_identical(robots_body(x), "A")
  expect_identical(robots_body(x, raw = TRUE), charToRaw("A"))
})

test_that("source_id selects the named source on a multi-source object", {
  x <- make_decisions(list(
    robots_1 = charToRaw("first body"),
    robots_2 = charToRaw("second body")
  ))
  expect_identical(robots_body(x, source_id = "robots_1"), "first body")
  expect_identical(robots_body(x, source_id = "robots_2"), "second body")
  expect_identical(
    robots_body(x, source_id = "robots_2", raw = TRUE),
    charToRaw("second body")
  )
})

# --- Multi-source error ------------------------------------------------------

test_that("omitting source_id on a multi-source object errors", {
  x <- make_decisions(list(
    robots_1 = charToRaw("a"),
    robots_2 = charToRaw("b")
  ))
  expect_error(robots_body(x), class = "robotstxtr_ambiguous_source")
  expect_error(robots_body(x), class = "robotstxtr_error")
})

test_that("an unknown source_id raises a package error", {
  x <- make_decisions(list(robots_1 = charToRaw("a")))
  expect_error(
    robots_body(x, source_id = "nope"),
    class = "robotstxtr_unknown_source"
  )
})

# --- Safe byte rendering -----------------------------------------------------

test_that("invalid-UTF-8 bytes render without error, marked Encoding = bytes", {
  x <- make_decisions(list(only = as.raw(c(0xff, 0xfe, 0x41))))
  out <- expect_no_error(robots_body(x))
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_identical(Encoding(out), "bytes")
  # raw = TRUE still returns the exact bytes.
  expect_identical(robots_body(x, raw = TRUE), as.raw(c(0xff, 0xfe, 0x41)))
})

test_that("valid UTF-8 bodies render as UTF-8 text", {
  x <- make_decisions(list(only = charToRaw(enc2utf8("café"))))
  out <- robots_body(x)
  expect_identical(Encoding(out), "UTF-8")
  expect_identical(out, enc2utf8("café"))
})

test_that("a source that stored no body previews as NA, extracts as NULL", {
  # Every non-`fetched` acquisition outcome stores a NULL body; the preview
  # must report that absence as NA rather than an empty string.
  x <- make_decisions(list(only = NULL))
  expect_identical(robots_body(x), NA_character_)
  expect_identical(robots_body(x, n = Inf), NA_character_)
  expect_identical(robots_body(x, source_id = "only"), NA_character_)
  expect_null(robots_body(x, raw = TRUE))
})

# --- Argument validation -----------------------------------------------------

test_that("invalid n, raw, and object arguments raise package errors", {
  x <- make_decisions(list(only = charToRaw("a")))
  expect_error(robots_body(x, n = 0), class = "robotstxtr_invalid_n")
  expect_error(robots_body(x, n = -1), class = "robotstxtr_invalid_n")
  expect_error(robots_body(x, n = c(1, 2)), class = "robotstxtr_invalid_n")
  expect_error(robots_body(x, raw = NA), class = "robotstxtr_invalid_raw")
  expect_error(
    robots_body(x, source_id = ""),
    class = "robotstxtr_invalid_source_id"
  )
  expect_error(robots_body(list()), class = "robotstxtr_invalid_object")
})
