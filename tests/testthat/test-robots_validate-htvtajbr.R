# ROBO-htvtajbr: public document validation for supplied and fetched bodies.

validation_mock_response <- function(status = 200L, body = raw()) {
  function(req) {
    httr2::response(status_code = status, url = req$url, body = body)
  }
}

test_that("a valid document returns stable typed evidence", {
  body <- paste0(
    "# comment\n\n",
    "user-agent: *\n",
    "allow: /public\n",
    "disallow: /private\n",
    "sitemap: https://example.com/sitemap.xml\n"
  )
  x <- robots_validate_text(body)

  expect_s3_class(x, "robots_validations")
  expect_named(x, c("map", "documents", "diagnostics"))
  expect_identical(attr(x, "validation_profile"),
                   "google-parser-compatible")
  expect_identical(x$documents$validation_status, "valid")
  expect_identical(x$documents$byte_size, as.integer(nchar(body, "bytes")))
  expect_identical(x$documents$line_count, 6L)
  expect_identical(x$documents$active_line_count, 4L)
  expect_identical(x$documents$recognized_directives, 4L)
  expect_identical(x$documents$user_agent_directives, 1L)
  expect_identical(x$documents$allow_directives, 1L)
  expect_identical(x$documents$disallow_directives, 1L)
  expect_identical(x$documents$sitemap_directives, 1L)
  expect_identical(x$documents$encoding, "utf-8")
  expect_false(x$documents$has_bom)
  expect_true(x$documents$final_newline)
  expect_identical(nrow(x$diagnostics), 0L)

  expect_type(x$documents$byte_size, "integer")
  expect_type(x$documents$has_bom, "logical")
  expect_type(x$diagnostics$line, "integer")
  expect_type(x$diagnostics$severity, "character")
  expect_output(print(x), "<robots_validations>")
  expect_invisible(print(x))
})

test_that("comments and blank lines are ignored, not diagnosed", {
  x <- robots_validate_text(
    "\n  # first\r\nuser-agent: * # agent\rdisallow: /x # rule"
  )
  expect_identical(x$documents$line_count, 4L)
  expect_identical(x$documents$active_line_count, 2L)
  expect_identical(x$documents$recognized_directives, 2L)
  expect_identical(nrow(x$diagnostics), 0L)
})

test_that("malformed, unknown, and unsupported active lines are distinct", {
  x <- robots_validate_text(paste(
    "user-agent: *",
    "this is not a directive",
    "unicorn: value",
    "crawl-delay: 5",
    sep = "\n"
  ))

  expect_identical(x$documents$validation_status, "error")
  expect_identical(x$documents$active_line_count, 4L)
  expect_identical(x$documents$recognized_directives, 2L)
  expect_identical(x$documents$unknown_directives, 1L)
  expect_identical(x$documents$malformed_lines, 1L)
  expect_identical(x$documents$unsupported_directives, 1L)
  expect_setequal(
    x$diagnostics$code,
    c("malformed_line", "unknown_directive", "unsupported_directive")
  )
  expect_identical(
    x$diagnostics$raw_text[x$diagnostics$code == "malformed_line"],
    "this is not a directive"
  )
})

test_that("group and empty-value errors are reported per line", {
  x <- robots_validate_text(paste(
    "disallow: /before",
    "user-agent:",
    "allow:",
    "sitemap:",
    sep = "\n"
  ))

  expect_identical(x$documents$validation_status, "error")
  expect_true(all(c(
    "rule_without_user_agent", "empty_user_agent", "empty_rule_value",
    "empty_sitemap_value", "missing_user_agent"
  ) %in% x$diagnostics$code))
  expect_identical(
    x$diagnostics$line[x$diagnostics$code == "rule_without_user_agent"],
    c(1L, 3L)
  )
  expect_true(is.na(
    x$diagnostics$line[x$diagnostics$code == "missing_user_agent"]
  ))
})

test_that("accepted Google parser variants are explicit warnings", {
  x <- robots_validate_text(paste(
    "useragent: *",
    "disallow /private",
    sep = "\n"
  ))
  expect_identical(x$documents$validation_status, "warning")
  expect_setequal(
    x$diagnostics$code,
    c("accepted_directive_typo", "accepted_missing_colon")
  )
  expect_identical(x$documents$recognized_directives, 2L)
})

test_that("BOM, NUL, and malformed UTF-8 hazards preserve raw evidence", {
  body <- as.raw(c(
    0xef, 0xbb, 0xbf,
    charToRaw("user-agent: *\ndisallow: /x"),
    0x00, 0xff
  ))
  x <- robots_validate_text(body)

  expect_true(x$documents$has_bom)
  expect_identical(x$documents$nul_byte_count, 1L)
  expect_identical(x$documents$encoding, "invalid-utf-8")
  expect_false(x$documents$final_newline)
  expect_true(all(c(
    "byte_order_mark", "nul_byte", "invalid_utf8"
  ) %in% x$diagnostics$code))
  nul_text <- x$diagnostics$raw_text[x$diagnostics$code == "nul_byte"]
  expect_true(grepl("\\x00", nul_text, fixed = TRUE))
  expect_true(grepl("\\xFF", nul_text, fixed = TRUE))
})

test_that("overlong parser lines are errors", {
  body <- sprintf("user-agent: *\ndisallow: /%s", strrep("x", 17000L))
  x <- robots_validate_text(body)
  expect_true("line_too_long" %in% x$diagnostics$code)
  expect_identical(x$documents$validation_status, "error")
})

test_that("invalid supplied validation inputs are classed call errors", {
  expect_error(
    robots_validate_text(NA_character_),
    class = "robotstxtr_invalid_validation_document"
  )
  expect_error(
    robots_validate_text(c("a", "b")),
    class = "robotstxtr_invalid_validation_document"
  )
  expect_error(
    robots_validate_text(list(charToRaw("user-agent: *"))),
    class = "robotstxtr_invalid_validation_document"
  )
})

test_that("URL validation reuses one fetched raw body per origin", {
  body <- charToRaw("user-agent: *\ndisallow: /private\n")
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  httr2::local_mocked_responses(function(req) {
    state$calls <- state$calls + 1L
    httr2::response(status_code = 200L, url = req$url, body = body)
  })
  x <- robots_validate_url(c(
    "https://example.com/one", "https://example.com/two"
  ))

  expect_identical(state$calls, 1L)
  expect_identical(nrow(x$map), 2L)
  expect_identical(nrow(x$documents), 1L)
  expect_identical(x$map$source_id, rep("robots_1", 2L))
  expect_identical(x$map$validation_status, rep("valid", 2L))
  expect_identical(x$documents$byte_size, as.integer(length(body)))
  expect_identical(x$documents$max_bytes, 524288L)
  expect_false(x$documents$body_truncated)
  expect_false(x$documents$limit_exceeded)
})

test_that("URL acquisition outcomes remain explicit validation evidence", {
  httr2::local_mocked_responses(validation_mock_response(206L,
                                                         charToRaw("partial")))
  partial <- robots_validate_url("https://example.com/x")
  expect_identical(partial$documents$validation_status, "not_validated")
  expect_identical(partial$diagnostics$code, "acquisition_partial_response")
  expect_false(partial$documents$body_truncated)

  large_body <- charToRaw(strrep("x", 100L))
  httr2::local_mocked_responses(validation_mock_response(200L, large_body))
  limited <- robots_validate_url("https://example.com/x", max_bytes = 10L)
  expect_identical(limited$documents$fetch_outcome, "body_too_large")
  expect_identical(limited$documents$validation_status, "not_validated")
  expect_identical(limited$diagnostics$code,
                   "acquisition_limit_exceeded")
  expect_true(limited$documents$limit_exceeded)
  expect_false(limited$documents$body_truncated)
})

test_that("an empty document is validated, not skipped", {
  x <- robots_validate_text(raw())
  expect_identical(x$documents$byte_size, 0L)
  expect_identical(x$documents$line_count, 0L)
  expect_identical(x$documents$active_line_count, 0L)
  expect_identical(x$documents$nul_byte_count, 0L)
  expect_identical(x$documents$encoding, "utf-8")
  expect_false(x$documents$final_newline)
  expect_identical(x$documents$validation_status, "error")
  expect_identical(x$diagnostics$code, "missing_user_agent")
  expect_identical(split_validation_lines(raw()), list())
})

test_that("bytes-encoded character input is used verbatim", {
  body <- rawToChar(c(
    charToRaw("user-agent: *\n"), as.raw(c(0xff, 0xfe)), charToRaw(": v\n")
  ))
  Encoding(body) <- "bytes"
  x <- robots_validate_text(body)

  # Used byte-for-byte: no re-encoding pass widened the 2 raw bytes.
  expect_identical(x$documents$byte_size, 20L)
  expect_identical(x$documents$encoding, "invalid-utf-8")
  expect_identical(x$documents$unknown_directives, 1L)
  # A bytes-encoded key is reported as the literal "unknown" action rather
  # than being lower-cased (tolower() would mangle the raw bytes).
  expect_identical(
    x$diagnostics$directive[x$diagnostics$code == "unknown_directive"],
    "unknown"
  )
  # A recognized type is passed through; an ASCII unknown key is lower-cased.
  expect_identical(validation_ascii_action("SiteMap", "sitemap"), "sitemap")
  expect_identical(validation_ascii_action("Crawl-Delay", "unknown"),
                   "crawl-delay")
})

test_that("a user-agent after a rule opens a fresh group", {
  x <- robots_validate_text(
    "user-agent: a\ndisallow: /x\nuser-agent: b\ndisallow: /y\n"
  )
  expect_identical(x$documents$validation_status, "valid")
  expect_identical(nrow(x$diagnostics), 0L)
  expect_identical(x$documents$user_agent_directives, 2L)
  expect_identical(x$documents$disallow_directives, 2L)
})

test_that("raw line rendering escapes tabs, control bytes, and empties", {
  expect_identical(render_validation_line(raw(0)), "")
  expect_identical(render_validation_line(charToRaw("a\tb")), "a\\tb")
  expect_identical(render_validation_line(charToRaw("ok /x")), "ok /x")
  expect_identical(render_validation_line(as.raw(c(0xff, 0x00))), "\\xFF\\x00")
})

test_that("validation_status reports not_validated when nothing was parsed", {
  expect_identical(
    validation_status(empty_validation_diagnostics(), validated = FALSE),
    "not_validated"
  )
  expect_identical(validation_status(empty_validation_diagnostics()), "valid")
})

test_that("a generic acquisition failure is one acquisition_failed error", {
  httr2::local_mocked_responses(validation_mock_response(500L))
  failed <- robots_validate_url("https://example.com/x")

  expect_identical(failed$documents$fetch_outcome, "http_error")
  expect_identical(failed$documents$validation_status, "not_validated")
  expect_identical(failed$diagnostics$severity, "error")
  expect_identical(failed$diagnostics$code, "acquisition_failed")
  # No transport message exists, so the outcome itself is the stated reason.
  expect_identical(
    failed$diagnostics$message,
    "Acquisition outcome 'http_error' supplied no document body."
  )
  expect_identical(failed$documents$error_count, 1L)
  expect_identical(failed$documents$warning_count, 0L)
  expect_false(failed$documents$limit_exceeded)
})

test_that("an acquisition error message is reported verbatim", {
  # The SSRF guard rejects before any request is issued, so this is offline.
  blocked <- robots_validate_url("http://127.0.0.1/x")

  expect_identical(blocked$documents$fetch_outcome, "ssrf_blocked")
  expect_identical(blocked$documents$validation_status, "not_validated")
  expect_identical(blocked$diagnostics$code, "acquisition_failed")
  expect_identical(
    blocked$diagnostics$message, "Blocked by SSRF guard (loopback)."
  )
  expect_identical(blocked$documents$error_count, 1L)
})

test_that("missing and invalid URL acquisitions are not document validation", {
  httr2::local_mocked_responses(validation_mock_response(404L))
  missing <- robots_validate_url("https://example.com/x")
  expect_identical(missing$documents$validation_status, "not_validated")
  expect_identical(missing$diagnostics$severity, "info")
  expect_identical(missing$diagnostics$code, "acquisition_missing")

  invalid <- robots_validate_url("")
  expect_identical(nrow(invalid$documents), 0L)
  expect_identical(invalid$map$validation_status, "not_validated")
  expect_identical(invalid$diagnostics$input_id, 1L)
  expect_identical(invalid$diagnostics$code, "acquisition_input_invalid")
})
