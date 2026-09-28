#' Preview or extract a stored robots.txt body
#'
#' Returns the robots.txt body stored in a `robots_decisions` object. Each
#' source body is stored once as a raw vector in the object's `robots` table;
#' this helper selects one source and either renders a character preview
#' (`raw = FALSE`, the default) or returns the stored raw vector byte-for-byte
#' unchanged (`raw = TRUE`).
#'
#' Selection follows the `robots$source_id` column. If `source_id` is `NULL`
#' (the default) and the object holds exactly one source, that source is used.
#' If `source_id` is `NULL` and the object holds more than one source, the call
#' is
#' ambiguous and raises a package error rather than choosing silently. An
#' unknown `source_id` is also a package error.
#'
#' When `raw = FALSE`, the selected body's bytes are rendered to a character
#' string. Robots.txt bodies may be arbitrary bytes (for example non-UTF-8 or a
#' value carrying `Encoding = "bytes"`); rendering never errors on invalid
#' encoding. A body whose bytes form valid UTF-8 is returned as a UTF-8 string;
#' otherwise the result carries `Encoding = "bytes"` so R does not attempt to
#' reinterpret it. `n` limits the preview to the first `n` bytes of the body;
#' `n = Inf` returns the full body. `n` is ignored when `raw = TRUE`, which
#' always returns the complete stored raw vector unchanged.
#'
#' @param x A `robots_decisions` object.
#' @param source_id A single non-empty character string naming the source to
#'   read from `x$robots$source_id`, or `NULL` (the default) to select the only
#'   source when the object holds exactly one.
#' @param n A single positive number giving the maximum number of body bytes to
#'   include in the preview. Defaults to `20`. Use `n = Inf` for the full body.
#'   Ignored when `raw = TRUE`.
#' @param raw A single logical. When `TRUE`, return the stored raw vector
#'   unchanged (byte-for-byte identical to `x$robots$body[[i]]`) instead of a
#'   rendered preview. Defaults to `FALSE`.
#'
#' @return When `raw = FALSE`, a length-one character string (a preview, or the
#'   full body when `n = Inf`), marked `Encoding = "UTF-8"` or `Encoding =
#'   "bytes"`. When `raw = TRUE`, the stored raw vector unchanged (or `NULL`
#'   when the selected source stored no body).
#'
#' @examples
#' x <- allowed_by_robots_text("user-agent: *\ndisallow: /x", "http://x/x", "b")
#' robots_body(x)
#' robots_body(x, n = Inf)
#' robots_body(x, raw = TRUE)
#'
#' @export
robots_body <- function(x, source_id = NULL, n = 20, raw = FALSE) {
  # --- Argument validation (call-level; reuse the package error helper). ------
  if (!inherits(x, "robots_decisions") || !is.data.frame(x$robots) ||
        is.null(x$robots$source_id) || is.null(x$robots$body)) {
    robots_abort(
      "`x` must be a `robots_decisions` object with a `robots` source table.",
      "robotstxtr_invalid_object"
    )
  }
  if (!is.logical(raw) || length(raw) != 1L || is.na(raw)) {
    robots_abort(
      "`raw` must be a single, non-missing logical value.",
      "robotstxtr_invalid_raw"
    )
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n <= 0) {
    robots_abort(
      "`n` must be a single positive number (`Inf` for the full body).",
      "robotstxtr_invalid_n"
    )
  }

  # --- Source selection by `source_id` (PRD 6.6). --------------------------
  robots <- x$robots
  ids <- robots$source_id
  if (is.null(source_id)) {
    # `source_id` omitted: unambiguous only when the object has one source.
    if (nrow(robots) != 1L) {
      robots_abort(
        sprintf(
          paste0(
            "`x` has %d sources; `source_id` must be supplied to choose one ",
            "(one of: %s)."
          ),
          nrow(robots), paste0("\"", ids, "\"", collapse = ", ")
        ),
        "robotstxtr_ambiguous_source"
      )
    }
    i <- 1L
  } else {
    if (!is.character(source_id) || length(source_id) != 1L ||
          is.na(source_id) || !nzchar(source_id)) {
      robots_abort(
        "`source_id` must be `NULL` or a single, non-empty character string.",
        "robotstxtr_invalid_source_id"
      )
    }
    i <- match(source_id, ids)
    if (is.na(i)) {
      robots_abort(
        sprintf(
          "unknown `source_id` \"%s\"; available: %s.",
          source_id, paste0("\"", ids, "\"", collapse = ", ")
        ),
        "robotstxtr_unknown_source"
      )
    }
  }

  body <- robots$body[[i]]

  # --- raw = TRUE: return the stored raw vector unchanged, byte-for-byte. ----
  if (raw) {
    return(body)
  }

  # --- raw = FALSE: safe character preview of the first `n` bytes (or all when
  # `n = Inf`). A source that stored no body renders as an empty string. -------
  if (is.null(body)) {
    return(NA_character_)
  }
  if (is.finite(n) && n < length(body)) {
    body <- body[seq_len(n)]
  }
  render_body_bytes(body)
}

# Render raw bytes to a length-one character string without erroring on invalid
# encoding OR on an embedded NUL (PRD 6.6 body handling). NUL bytes are dropped
# first, the package-wide rule documented in R/body-decode.R; a bare
# `rawToChar()` here would raise on a NUL-bearing fetched body and break the
# no-error guarantee this preview is built on. Bytes that then form valid UTF-8
# are marked UTF-8 for readable output; otherwise the result is marked
# `Encoding = "bytes"` so R does not try to reinterpret arbitrary bytes as text.
render_body_bytes <- function(bytes) {
  out <- rawToChar(nul_free_bytes(bytes))
  if (validUTF8(out)) {
    Encoding(out) <- "UTF-8"
  } else {
    Encoding(out) <- "bytes"
  }
  out
}
