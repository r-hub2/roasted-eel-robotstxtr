#' Raise a classed package error
#'
#' Internal helper that stops with a condition carrying a stable, package-
#' specific subclass so callers can branch on `class`, plus the base
#' `robotstxtr_error` marker.
#'
#' @param message Diagnostic message.
#' @param class A single stable subclass string, e.g.
#'   `"robotstxtr_length_mismatch"`.
#'
#' @return Never returns; always signals an error.
#' @keywords internal
#' @noRd
robots_abort <- function(message, class) {
  stop(structure(
    class = c(class, "robotstxtr_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

# --- Call-level validators (whole call aborts on violation) -----------------

# `robots_txt` must be one non-missing character scalar. An empty body is valid.
validate_robots_txt <- function(robots_txt) {
  if (!is.character(robots_txt) || length(robots_txt) != 1L ||
        is.na(robots_txt)) {
    robots_abort(
      "`robots_txt` must be a single, non-missing character string.",
      "robotstxtr_invalid_robots_txt"
    )
  }
  invisible(robots_txt)
}

# `source_id` must be one non-empty, non-missing character scalar.
validate_source_id <- function(source_id) {
  if (!is.character(source_id) || length(source_id) != 1L ||
        is.na(source_id) || !nzchar(source_id)) {
    robots_abort(
      "`source_id` must be a single, non-empty character string.",
      "robotstxtr_invalid_source_id"
    )
  }
  invisible(source_id)
}

# `url` must be a character vector; length zero is allowed. Wrong type is a
# call-level error (no silent coercion).
validate_url_type <- function(url) {
  if (!is.character(url)) {
    robots_abort(
      "`url` must be a character vector.",
      "robotstxtr_invalid_url_type"
    )
  }
  invisible(url)
}

# `user_agent` must be a character vector of length one or `length(url)`. A
# scalar user agent expands across URLs; no other R recycling is allowed. Wrong
# type or a length mismatch is a call-level error. Returns the expanded vector.
expand_user_agent <- function(user_agent, n_url) {
  if (!is.character(user_agent)) {
    robots_abort(
      "`user_agent` must be a character vector.",
      "robotstxtr_invalid_user_agent_type"
    )
  }
  len <- length(user_agent)
  if (len == 1L) {
    return(rep(user_agent, length.out = n_url))
  }
  if (len == n_url) {
    return(user_agent)
  }
  robots_abort(
    sprintf(
      paste0(
        "`user_agent` must be length 1 or length(url) (%d); ",
        "got length %d. No other recycling is allowed."
      ),
      n_url, len
    ),
    "robotstxtr_length_mismatch"
  )
}
