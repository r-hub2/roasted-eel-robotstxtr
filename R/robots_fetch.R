#' Fetch robots.txt documents for URLs under the deterministic fetch policy
#'
#' Performs the grouped robots.txt fetch stage for one or more target URLs and
#' returns a `robots_fetches` object. No matching is performed (that is the
#' `allowed_by_robots_url()` path, a later slice); this function only computes
#' each URL's
#' fetch-origin, fetches each distinct `/robots.txt` once, and classifies the
#' HTTP outcome.
#'
#' For every input URL the fetch-origin grouping key
#' `scheme://host[:port]/robots.txt` is constructed (see the fetch-origin rules
#' in the package PRD). Within one call each distinct grouping key is fetched
#' exactly once and every input row sharing that key references the same source
#' row and stored body. Grouping is keyed on the requested robots URL, not on
#' the final redirect destination. Input order is preserved in `map`, and fetch
#' groups run sequentially in first-occurrence order; source IDs are assigned
#' `robots_1`, `robots_2`, and so on in that order. There is no persistent,
#' cross-call, or hidden HTTP cache.
#'
#' A URL whose fetch-origin cannot be constructed (missing, empty, malformed, or
#' non-HTTP(S)) is invalid: it never triggers an HTTP request and appears in
#' `map` with `fetch_outcome = "input_invalid"`, no `source_id`, and
#' invalid-URL error metadata.
#'
#' The request policy is deterministic and intentionally conservative: HTTP
#' `GET`; TLS verified with platform defaults; a total timeout per origin
#' including redirects; at most five redirects; redirects allowed only to
#' `http`/`https`; HTTP-to-HTTPS upgrades allowed but every HTTPS-to-HTTP
#' downgrade rejected as `redirect_error`; a structural SSRF guard that refuses,
#' before any request, both the initial origin and every redirect target that
#' resolves to a private, loopback, link-local, or cloud-metadata address
#' (outcome `ssrf_blocked`, no socket opened); no URL userinfo, authorization,
#' cookies, or caller session state forwarded; a package fetch user agent
#' (`robotstxtr/<version>`) unless the caller supplies one; and no automatic
#' retries. Each response is classified into a stable `fetch_outcome`. For a
#' `fetched` outcome the exact decoded entity bytes are stored once as a raw
#' vector in the source row's `body` (an empty body is valid); all other
#' outcomes store no body.
#'
#' `max_bytes` caps the decoded response body. The limit is enforced on decoded
#' entity bytes (after any HTTP content decoding, such as gzip) by a streaming
#' read that stops the moment the decoded byte count exceeds the limit; the
#' compressed entity is never fully downloaded before the check. A body over the
#' limit ends as `body_too_large` with no stored body (never a truncated body);
#' a body at or under the limit is stored normally as a `fetched` result.
#'
#' @param url A character vector of target URLs. May have length zero (an empty
#'   result is returned). A wrong R type is a call-level error.
#' @param timeout A single positive, finite numeric total timeout in seconds per
#'   origin, including redirects. Defaults to `10`.
#' @param max_bytes A single positive, finite, whole-number decoded-body byte
#'   limit no greater than `.Machine$integer.max`, coerced to integer and
#'   recorded on each source row. A whole-number double (for example `524288`)
#'   is accepted; fractional, non-positive, out-of-range, or non-scalar values
#'   are call-level errors. Defaults to `524288L`.
#' @param fetch_user_agent `NULL` (use the package fetch user agent) or a single
#'   non-empty character HTTP user agent to send instead.
#' @param ssrf_guard A single logical. When `TRUE` (the default) a structural
#'   SSRF guard refuses, before any request, both the initial origin and every
#'   redirect target that resolves to a private, loopback, link-local, or
#'   cloud-metadata address (outcome `ssrf_blocked`, no socket opened). Set to
#'   `FALSE` to deliberately opt out — for example to fetch `robots.txt` from a
#'   trusted intranet host. Opting out removes that protection; only disable it
#'   for hosts you control and trust.
#'
#' @return An S3 object of class `robots_fetches`: a named list with two
#'   components, `map` (one row per input URL, in input order) and `robots` (one
#'   row per fetched source, with the fetched body stored once as a raw vector
#'   in `robots$body`).
#'
#' @examples
#' # robots_fetch() performs live HTTP. To keep this example offline and
#' # deterministic, the transport is mocked with httr2 so every request returns
#' # the same robots.txt; a real call needs no such wrapper.
#' httr2::with_mocked_responses(
#'   function(req) {
#'     httr2::response(
#'       status_code = 200L, url = req$url,
#'       body = charToRaw("user-agent: *\nDisallow: /private\n")
#'     )
#'   },
#'   {
#'     # Both URLs share one fetch-origin, so /robots.txt is fetched once.
#'     robots_fetch(c("https://example.com/", "https://example.com/private"))
#'   }
#' )
#'
#' @seealso [allowed_by_robots_url()] to fetch and match in one step.
#' @export
robots_fetch <- function(url, timeout = 10, max_bytes = 524288L,
                         fetch_user_agent = NULL, ssrf_guard = TRUE) {
  # --- Call-level input validation / shared fetch controls (PRD 6.6). --------
  validate_url_type(url)
  timeout <- validate_timeout(timeout)
  validate_fetch_user_agent(fetch_user_agent)
  max_bytes_int <- validate_max_bytes(max_bytes)
  validate_ssrf_guard(ssrf_guard)
  fetch_ua <- if (is.null(fetch_user_agent)) {
    package_fetch_user_agent()
  } else {
    fetch_user_agent
  }

  n <- length(url)

  # --- Per-element fetch-origin (PRD 6.3). A URL is fetch-eligible only when it
  # has a grouping key; otherwise it is input-invalid and never fetched.
  origin <- vapply(url, robots_origin, character(1L), USE.NAMES = FALSE)
  eligible <- !is.na(origin)

  # --- Grouping (PRD 6.5): fetch each DISTINCT origin exactly once, in
  # first-occurrence order. Distinct keys among eligible rows only.
  distinct_origins <- unique(origin[eligible])
  n_src <- length(distinct_origins)
  source_ids <- if (n_src > 0L) {
    paste0("robots_", seq_len(n_src))
  } else {
    character(0)
  }

  # Fetch each distinct origin sequentially in first-occurrence order.
  results <- vector("list", n_src)
  for (i in seq_len(n_src)) {
    results[[i]] <- perform_fetch(
      distinct_origins[[i]], timeout, fetch_ua, max_bytes_int, ssrf_guard
    )
  }

  # --- robots source table (one row per fetched source). --------------------
  robots <- data.frame(
    source_id = source_ids,
    source_type = rep("fetched", n_src),
    robots_url = distinct_origins,
    effective_url = vapply(results, function(r) r$effective_url,
                           character(1L)),
    http_status = vapply(results, function(r) r$http_status, integer(1L)),
    fetch_outcome = vapply(results, function(r) r$fetch_outcome, character(1L)),
    redirect_count = vapply(results, function(r) r$redirect_count, integer(1L)),
    body_size = vapply(results, function(r) r$body_size, integer(1L)),
    timeout = rep(timeout, n_src),
    max_bytes = rep(max_bytes_int, n_src),
    error_stage = vapply(results, function(r) r$error_stage, character(1L)),
    error_class = vapply(results, function(r) r$error_class, character(1L)),
    error_message = vapply(results, function(r) r$error_message, character(1L)),
    stringsAsFactors = FALSE
  )
  # `body` is a list-of-raw column: each fetched body stored exactly once.
  robots$body <- lapply(results, function(r) r$body)

  # --- Per-input map (one row per input URL, in input order). ---------------
  # Match each eligible input to its source row by origin key; invalid inputs
  # are detached (input_invalid) and reference no source.
  src_index <- match(origin, distinct_origins)

  map_source_id <- rep(NA_character_, n)
  map_robots_url <- rep(NA_character_, n)
  map_http_status <- rep(NA_integer_, n)
  map_fetch_outcome <- rep("input_invalid", n)
  map_error_stage <- rep("origin", n)
  map_error_class <- rep("robots_invalid_url", n)
  map_error_message <- rep(
    "URL is missing, empty, malformed, or not HTTP(S).", n
  )

  if (any(eligible)) {
    idx <- which(eligible)
    si <- src_index[idx]
    map_source_id[idx] <- source_ids[si]
    map_robots_url[idx] <- distinct_origins[si]
    map_http_status[idx] <- robots$http_status[si]
    map_fetch_outcome[idx] <- robots$fetch_outcome[si]
    map_error_stage[idx] <- robots$error_stage[si]
    map_error_class[idx] <- robots$error_class[si]
    map_error_message[idx] <- robots$error_message[si]
  }

  map <- data.frame(
    input_id = seq_len(n),
    url = url,
    source_id = map_source_id,
    robots_url = map_robots_url,
    http_status = map_http_status,
    fetch_outcome = map_fetch_outcome,
    error_stage = map_error_stage,
    error_class = map_error_class,
    error_message = map_error_message,
    stringsAsFactors = FALSE
  )

  new_robots_fetches(map, robots)
}

#' Construct a `robots_fetches` object
#'
#' Internal low-level constructor. Assembles the two primary components into the
#' `robots_fetches` S3 object and stamps the package version as an attribute. No
#' validation of column shape is performed here; callers build conforming `map`
#' and `robots` frames.
#'
#' @param map A data frame with one row per input URL, in input order.
#' @param robots A data frame with one row per fetched source.
#'
#' @return An S3 object of class `robots_fetches`.
#' @keywords internal
#' @noRd
new_robots_fetches <- function(map, robots) {
  structure(
    list(map = map, robots = robots),
    class = "robots_fetches",
    package_version = as.character(getNamespaceVersion("robotstxtr"))
  )
}

#' Print a `robots_fetches` object
#'
#' Prints a compact one-line header (input and source counts) and a preview of
#' up to ten input-map rows. Body values are never shown. Returns the object
#' invisibly.
#'
#' @inheritParams print.robots_decisions
#' @param x A `robots_fetches` object.
#'
#' @return `x`, invisibly.
#' @examples
#' # A robots_fetches object comes from robots_fetch(); the transport is mocked
#' # here so the example runs offline.
#' fetches <- httr2::with_mocked_responses(
#'   function(req) {
#'     httr2::response(
#'       status_code = 200L, url = req$url,
#'       body = charToRaw("user-agent: *\nDisallow: /private\n")
#'     )
#'   },
#'   robots_fetch("https://example.com/page")
#' )
#' print(fetches)
#' @export
print.robots_fetches <- function(x, ...) {
  n <- nrow(x$map)
  n_src <- nrow(x$robots)
  cat(sprintf(
    "<robots_fetches>: %d input%s, %d source%s\n",
    n, if (n == 1L) "" else "s",
    n_src, if (n_src == 1L) "" else "s"
  ))
  if (n > 0L) {
    show_n <- min(n, 10L)
    preview <- x$map[
      seq_len(show_n),
      c("input_id", "url", "source_id", "http_status", "fetch_outcome"),
      drop = FALSE
    ]
    print(preview, row.names = FALSE)
    if (n > show_n) {
      extra <- n - show_n
      cat(sprintf(
        "  ... and %d more row%s\n", extra, if (extra == 1L) "" else "s"
      ))
    }
  }
  invisible(x)
}
