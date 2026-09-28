# Slice R9 test gate: the one redirect-policy row not pinned by R6. PRD 6.4
# allows same- OR cross-origin redirects to http/https; R6 covered the
# same-origin HTTP->HTTPS upgrade, the HTTPS->HTTP downgrade rejection, invalid
# targets, loops, and the five-redirect cap. This pins that a redirect to a
# DIFFERENT host is followed and that the grouping key stays the originally
# requested robots URL. Offline via httr2::local_mocked_responses().

test_that("a cross-origin redirect to another host is followed", {
  state <- new.env(parent = emptyenv())
  state$seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    state$seen[[length(state$seen) + 1L]] <- req$url
    if (identical(req$url, "http://a/robots.txt")) {
      httr2::response(
        status_code = 301L, url = req$url,
        headers = list(Location = "http://b/robots.txt")
      )
    } else if (identical(req$url, "http://b/robots.txt")) {
      httr2::response(
        status_code = 200L, url = req$url,
        body = charToRaw("user-agent: *\ndisallow: /")
      )
    } else {
      stop(sprintf("unexpected request URL in mock: %s", req$url))
    }
  })

  x <- robots_fetch("http://a/x")

  expect_identical(x$robots$fetch_outcome, "fetched")
  expect_identical(x$robots$redirect_count, 1L)
  # The final response came from the other origin.
  expect_identical(x$robots$effective_url, "http://b/robots.txt")
  # Grouping key stays the ORIGINAL requested robots URL, not the destination.
  expect_identical(x$robots$robots_url, "http://a/robots.txt")
  expect_identical(state$seen, c("http://a/robots.txt", "http://b/robots.txt"))
})
