#' Construct a `robots_decisions` object
#'
#' Internal low-level constructor. Assembles the two primary components into the
#' `robots_decisions` S3 object and stamps the package version as an attribute.
#' No validation of column shape is performed here; callers build conforming
#' `results` and `robots` frames.
#'
#' @param results A data frame with one row per input URL, in input order.
#' @param robots A data frame with one row per supplied or fetched source body.
#'
#' @return An S3 object of class `robots_decisions`.
#' @keywords internal
#' @noRd
new_robots_decisions <- function(results, robots) {
  structure(
    list(results = results, robots = robots),
    class = "robots_decisions",
    package_version = as.character(getNamespaceVersion("robotstxtr"))
  )
}

#' Print a `robots_decisions` object
#'
#' Prints a compact one-line header (result and source counts), an
#' allow/disallow/unknown tally, and a preview of up to ten result rows. Large
#' body values are never shown. Returns the object invisibly.
#'
#' @inheritParams robots_body
#' @param ... Ignored; present for S3 compatibility.
#'
#' @return `x`, invisibly.
#' @examples
#' # allowed_by_robots_text() matches a supplied body with no network access.
#' decisions <- allowed_by_robots_text(
#'   "user-agent: *\nDisallow: /private\n",
#'   c("https://example.com/page", "https://example.com/private"),
#'   "my-bot"
#' )
#' print(decisions)
#' @export
print.robots_decisions <- function(x, ...) {
  results <- x$results
  n <- nrow(results)
  n_src <- nrow(x$robots)
  cat(sprintf(
    "<robots_decisions>: %d result%s, %d source%s\n",
    n, if (n == 1L) "" else "s",
    n_src, if (n_src == 1L) "" else "s"
  ))
  if (n > 0L) {
    n_allow <- sum(results$allowed, na.rm = TRUE)
    n_deny <- sum(!results$allowed, na.rm = TRUE)
    n_unknown <- sum(is.na(results$allowed))
    cat(sprintf(
      "  allowed: %d  disallowed: %d  unknown: %d\n",
      n_allow, n_deny, n_unknown
    ))
    show_n <- min(n, 10L)
    preview <- results[
      seq_len(show_n),
      c("input_id", "url", "user_agent", "allowed", "decision_source"),
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
