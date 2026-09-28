# Validation profile implemented by the exported validation APIs. Keep this a
# stable machine-readable token; prose documentation explains its boundaries.
robots_validation_profile <- "google-parser-compatible"

validation_unsupported <- c(
  "clean-param", "content-signal", "content-usage", "crawl-delay", "domain",
  "host", "noarchive", "nofollow", "noindex", "request-rate",
  "revisit-after", "visit-time"
)

#' Validate a supplied robots.txt document
#'
#' Parses a supplied document with the package's pinned Google-compatible
#' parser and returns byte evidence, directive counts, and stable per-line
#' diagnostics. This validates document syntax and structure; it does not
#' decide whether a URL is allowed, infer webmaster intent, or fetch sitemap
#' resources declared by the document.
#'
#' The validation profile is explicitly `"google-parser-compatible"`. It
#' reports what the vendored parser recognizes (including the parser's accepted
#' typo and missing-colon variants) and adds conservative structural checks. It
#' is not a claim of universal crawler behavior or complete RFC 9309 validity.
#'
#' `robots_txt` may be text or a raw vector. Raw input is useful when validating
#' acquisition bytes that contain malformed UTF-8, a byte-order mark, or NUL
#' bytes. Character input marked with `Encoding = "bytes"` is used verbatim;
#' other character input is converted to UTF-8 once.
#'
#' The result contains three typed data frames: `map` (the validation target),
#' `documents` (one summary row per document), and `diagnostics` (zero or more
#' findings). Diagnostic severities are `"info"`, `"warning"`, and `"error"`;
#' document statuses are `"valid"`, `"warning"`, `"error"`, and
#' `"not_validated"`.
#'
#' @param robots_txt A single non-missing character string or a raw vector.
#' @param source_id A single non-empty character string identifying the supplied
#'   document. Defaults to `"supplied"`.
#'
#' @return An S3 object of class `robots_validations`, containing `map`,
#'   `documents`, and `diagnostics` data frames.
#'
#' @examples
#' report <- robots_validate_text(
#'   "user-agent: *\ndisallow: /private\n"
#' )
#' report$documents
#' report$diagnostics
#'
#' malformed <- robots_validate_text(charToRaw(
#'   "disallow: /before-agent\nunknown-field: value"
#' ))
#' malformed$diagnostics
#'
#' @seealso [robots_validate_url()] to fetch and validate, [robots_fetch()] for
#'   acquisition evidence, and [allowed_by_robots_text()] for URL decisions.
#' @export
robots_validate_text <- function(robots_txt, source_id = "supplied") {
  validate_source_id(source_id)
  bytes <- validation_body_bytes(robots_txt)
  validated <- validate_document_bytes(
    bytes = bytes,
    source_id = source_id,
    source_type = "supplied",
    fetch_outcome = "supplied"
  )
  map <- validation_map_row(
    input_id = 1L,
    url = NA_character_,
    source_id = source_id,
    robots_url = NA_character_,
    http_status = NA_integer_,
    fetch_outcome = "supplied",
    error_stage = NA_character_,
    error_class = NA_character_,
    error_message = NA_character_,
    validation_status = validated$document$validation_status
  )
  new_robots_validations(map, validated$document, validated$diagnostics)
}

#' Fetch and validate robots.txt documents for URLs
#'
#' Uses [robots_fetch()] unchanged, then validates each distinct stored raw body
#' exactly once. Shared origins therefore remain shared and no second HTTP path
#' or refetch is introduced. Acquisition failures are retained as evidence and
#' produce `"not_validated"` document rows; a response exceeding `max_bytes` is
#' explicitly reported as `acquisition_limit_exceeded`, and no truncated body
#' is parsed.
#'
#' This function validates the acquired robots.txt document. It does not answer
#' whether any target URL is allowed; use [allowed_by_robots_url()] for that
#' separate question. It also does not infer webmaster intent or check whether
#' declared remote sitemaps exist.
#'
#' @inheritParams robots_fetch
#'
#' @return An S3 object of class `robots_validations`, containing `map` (one row
#'   per input URL), `documents` (one row per distinct fetch source), and
#'   `diagnostics` data frames.
#'
#' @examples
#' fetched_validation <- httr2::with_mocked_responses(
#'   function(req) {
#'     httr2::response(
#'       status_code = 200L, url = req$url,
#'       body = charToRaw("user-agent: *\nDisallow: /private\n")
#'     )
#'   },
#'   robots_validate_url("https://example.com/private")
#' )
#' fetched_validation$documents
#'
#' @seealso [robots_validate_text()] for supplied bytes and [robots_fetch()] for
#'   acquisition without document validation.
#' @export
robots_validate_url <- function(url, timeout = 10, max_bytes = 524288L,
                                fetch_user_agent = NULL, ssrf_guard = TRUE) {
  fetched <- robots_fetch(
    url = url,
    timeout = timeout,
    max_bytes = max_bytes,
    fetch_user_agent = fetch_user_agent,
    ssrf_guard = ssrf_guard
  )
  validate_fetched_documents(fetched)
}

#' Print a robots.txt validation result
#'
#' @param x A `robots_validations` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#' @examples
#' # A robots_validations object comes from robots_validate_url() or
#' # robots_validate_text(); the transport is mocked here so the example runs
#' # offline.
#' validations <- httr2::with_mocked_responses(
#'   function(req) {
#'     httr2::response(
#'       status_code = 200L, url = req$url,
#'       body = charToRaw("user-agent: *\nDisallow: /private\n")
#'     )
#'   },
#'   robots_validate_url("https://example.com/private")
#' )
#' print(validations)
#' @export
print.robots_validations <- function(x, ...) {
  n_documents <- nrow(x$documents)
  n_diagnostics <- nrow(x$diagnostics)
  cat(sprintf(
    "<robots_validations>: %d document%s, %d diagnostic%s\n",
    n_documents, if (n_documents == 1L) "" else "s",
    n_diagnostics, if (n_diagnostics == 1L) "" else "s"
  ))
  if (n_documents > 0L) {
    print(
      x$documents[c(
        "source_id", "validation_status", "byte_size", "line_count",
        "recognized_directives", "warning_count", "error_count"
      )],
      row.names = FALSE
    )
  }
  invisible(x)
}

validation_body_bytes <- function(robots_txt) {
  if (is.raw(robots_txt)) {
    return(robots_txt)
  }
  if (!is.character(robots_txt) || length(robots_txt) != 1L ||
        is.na(robots_txt)) {
    robots_abort(
      "`robots_txt` must be a single non-missing string or a raw vector.",
      "robotstxtr_invalid_validation_document"
    )
  }
  if (identical(Encoding(robots_txt), "bytes")) {
    charToRaw(robots_txt)
  } else {
    charToRaw(enc2utf8(robots_txt))
  }
}

new_robots_validations <- function(map, documents, diagnostics) {
  structure(
    list(map = map, documents = documents, diagnostics = diagnostics),
    class = "robots_validations",
    package_version = as.character(getNamespaceVersion("robotstxtr")),
    validation_profile = robots_validation_profile
  )
}

validation_map_row <- function(input_id, url, source_id, robots_url,
                               http_status, fetch_outcome, error_stage,
                               error_class, error_message, validation_status) {
  data.frame(
    input_id = as.integer(input_id),
    url = as.character(url),
    source_id = as.character(source_id),
    robots_url = as.character(robots_url),
    http_status = as.integer(http_status),
    fetch_outcome = as.character(fetch_outcome),
    error_stage = as.character(error_stage),
    error_class = as.character(error_class),
    error_message = as.character(error_message),
    validation_status = as.character(validation_status),
    stringsAsFactors = FALSE
  )
}

empty_validation_diagnostics <- function() {
  data.frame(
    source_id = character(),
    input_id = integer(),
    line = integer(),
    severity = character(),
    code = character(),
    directive = character(),
    raw_text = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

validation_diagnostic <- function(source_id, line, severity, code, directive,
                                  raw_text, message, input_id = NA_integer_) {
  data.frame(
    source_id = as.character(source_id),
    input_id = as.integer(input_id),
    line = as.integer(line),
    severity = as.character(severity),
    code = as.character(code),
    directive = as.character(directive),
    raw_text = as.character(raw_text),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

validation_status <- function(diagnostics, validated = TRUE) {
  if (!validated) {
    return("not_validated")
  }
  if (any(diagnostics$severity == "error")) {
    return("error")
  }
  if (any(diagnostics$severity == "warning")) {
    return("warning")
  }
  "valid"
}

validate_document_bytes <- function(bytes, source_id, source_type,
                                    robots_url = NA_character_,
                                    effective_url = NA_character_,
                                    http_status = NA_integer_,
                                    fetch_outcome = "supplied",
                                    max_bytes = NA_integer_) {
  parsed <- as.data.frame(
    robotstxtr_validation_parse_(bytes),
    stringsAsFactors = FALSE
  )
  raw_lines <- split_validation_lines(bytes)
  line_count <- length(raw_lines)
  if (nrow(parsed) > 0L) {
    parsed <- parsed[parsed$line <= line_count, , drop = FALSE]
  }

  diag_env <- new.env(parent = emptyenv())
  diag_env$items <- list()
  add_diagnostic <- function(line, severity, code, directive, message) {
    raw_text <- if (!is.na(line) && line <= line_count) {
      render_validation_line(raw_lines[[line]])
    } else {
      NA_character_
    }
    diag_env$items[[length(diag_env$items) + 1L]] <- validation_diagnostic(
      source_id, line, severity, code, directive, raw_text, message
    )
  }

  has_bom <- length(bytes) >= 3L && identical(
    bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf))
  )
  if (has_bom) {
    add_diagnostic(
      1L, "warning", "byte_order_mark", NA_character_,
      paste0("A UTF-8 byte-order mark is present; the Google-compatible ",
             "parser skips it.")
    )
  }

  nul_counts <- if (line_count == 0L) {
    integer()
  } else {
    vapply(raw_lines, function(line) sum(line == as.raw(0)), integer(1L))
  }
  nul_lines <- which(nul_counts > 0L)
  for (line in nul_lines) {
    add_diagnostic(
      line, "error", "nul_byte", NA_character_,
      sprintf("Line contains %d NUL byte(s), which can truncate parsing.",
              nul_counts[[line]])
    )
  }

  bytes_without_nul <- nul_free_bytes(bytes)
  encoding_valid <- validUTF8(rawToChar(bytes_without_nul))
  encoding <- if (encoding_valid) "utf-8" else "invalid-utf-8"
  if (!encoding_valid) {
    add_diagnostic(
      NA_integer_, "error", "invalid_utf8", NA_character_,
      "Document bytes are not well-formed UTF-8."
    )
  }

  active <- !parsed$is_empty & !parsed$is_comment
  malformed <- active & !parsed$has_directive
  for (i in which(malformed)) {
    add_diagnostic(
      parsed$line[[i]], "error", "malformed_line", NA_character_,
      "Active line is not a parseable key-value directive."
    )
  }
  for (i in which(parsed$line_too_long)) {
    add_diagnostic(
      parsed$line[[i]], "error", "line_too_long", parsed$type[[i]],
      "Line exceeds the parser's 16,663-byte buffer and is truncated."
    )
  }

  group_open <- FALSE
  group_has_rule <- FALSE
  nonempty_user_agents <- 0L
  unsupported <- logical(nrow(parsed))
  for (i in which(parsed$has_directive)) {
    line <- parsed$line[[i]]
    type <- parsed$type[[i]]
    value <- parsed$value[[i]]
    action <- validation_ascii_action(parsed$action[[i]], type)

    if (parsed$missing_colon[[i]]) {
      add_diagnostic(
        line, "warning", "accepted_missing_colon", action,
        "Missing colon was accepted by the Google-compatible parser."
      )
    }
    if (parsed$is_typo[[i]]) {
      add_diagnostic(
        line, "warning", "accepted_directive_typo", action,
        "Directive spelling is an accepted Google-parser typo variant."
      )
    }

    if (identical(type, "unknown")) {
      unsupported[[i]] <- !is.na(action) &&
        action %in% validation_unsupported
      if (unsupported[[i]]) {
        add_diagnostic(
          line, "warning", "unsupported_directive", action,
          paste0("Directive is known but ignored by this validation profile; ",
                 "another crawler may use it.")
        )
      } else {
        add_diagnostic(
          line, "warning", "unknown_directive", action,
          "Directive is not recognized by this validation profile."
        )
      }
    }

    if (identical(type, "user-agent")) {
      if (group_has_rule) {
        group_open <- FALSE
        group_has_rule <- FALSE
      }
      if (nzchar(value)) {
        group_open <- TRUE
        nonempty_user_agents <- nonempty_user_agents + 1L
      } else {
        add_diagnostic(
          line, "error", "empty_user_agent", type,
          "User-agent directive has an empty value."
        )
      }
    } else if (type %in% c("allow", "disallow")) {
      if (group_open) {
        group_has_rule <- TRUE
      } else {
        add_diagnostic(
          line, "error", "rule_without_user_agent", type,
          "Allow or Disallow appears before a non-empty User-agent group."
        )
      }
      if (!nzchar(value)) {
        add_diagnostic(
          line, "warning", "empty_rule_value", type,
          "Empty Allow or Disallow value is ignored by the matcher."
        )
      }
    } else if (identical(type, "sitemap") && !nzchar(value)) {
      add_diagnostic(
        line, "warning", "empty_sitemap_value", type,
        "Sitemap directive has an empty value."
      )
    }
  }

  if (nonempty_user_agents == 0L) {
    add_diagnostic(
      NA_integer_, "error", "missing_user_agent", "user-agent",
      "Document has no non-empty User-agent directive."
    )
  }

  diagnostic_frame <- if (length(diag_env$items) == 0L) {
    empty_validation_diagnostics()
  } else {
    do.call(rbind, diag_env$items)
  }
  core_recognized <- parsed$has_directive & parsed$type != "unknown"
  recognized <- sum(core_recognized) + sum(unsupported)
  unknown <- sum(parsed$has_directive & parsed$type == "unknown" & !unsupported)
  final_newline <- length(bytes) > 0L &&
    bytes[[length(bytes)]] %in% as.raw(c(0x0a, 0x0d))

  document <- data.frame(
    source_id = source_id,
    source_type = source_type,
    robots_url = robots_url,
    effective_url = effective_url,
    http_status = as.integer(http_status),
    fetch_outcome = fetch_outcome,
    validation_status = validation_status(diagnostic_frame),
    profile = robots_validation_profile,
    byte_size = as.integer(length(bytes)),
    line_count = as.integer(line_count),
    active_line_count = as.integer(sum(active)),
    recognized_directives = as.integer(recognized),
    unknown_directives = as.integer(unknown),
    malformed_lines = as.integer(sum(malformed)),
    user_agent_directives = as.integer(sum(parsed$type == "user-agent")),
    allow_directives = as.integer(sum(parsed$type == "allow")),
    disallow_directives = as.integer(sum(parsed$type == "disallow")),
    sitemap_directives = as.integer(sum(parsed$type == "sitemap")),
    unsupported_directives = as.integer(sum(unsupported)),
    encoding = encoding,
    has_bom = has_bom,
    nul_byte_count = as.integer(sum(nul_counts)),
    final_newline = final_newline,
    max_bytes = as.integer(max_bytes),
    body_truncated = FALSE,
    limit_exceeded = FALSE,
    diagnostic_count = as.integer(nrow(diagnostic_frame)),
    warning_count = as.integer(sum(diagnostic_frame$severity == "warning")),
    error_count = as.integer(sum(diagnostic_frame$severity == "error")),
    stringsAsFactors = FALSE
  )
  list(document = document, diagnostics = diagnostic_frame)
}

validation_ascii_action <- function(action, type) {
  if (!identical(type, "unknown")) {
    return(type)
  }
  if (identical(Encoding(action), "bytes")) {
    return("unknown")
  }
  tolower(action)
}

split_validation_lines <- function(bytes) {
  n <- length(bytes)
  if (n == 0L) {
    return(list())
  }
  lines <- list()
  start <- 1L
  i <- 1L
  while (i <= n) {
    byte <- as.integer(bytes[[i]])
    if (byte %in% c(0x0a, 0x0d)) {
      lines[[length(lines) + 1L]] <- validation_raw_slice(bytes, start, i - 1L)
      if (byte == 0x0d && i < n && as.integer(bytes[[i + 1L]]) == 0x0a) {
        i <- i + 1L
      }
      start <- i + 1L
    }
    i <- i + 1L
  }
  if (start <= n) {
    lines[[length(lines) + 1L]] <- bytes[start:n]
  }
  lines
}

validation_raw_slice <- function(bytes, first, last) {
  if (first > last) raw() else bytes[first:last]
}

render_validation_line <- function(bytes) {
  if (length(bytes) == 0L) {
    return("")
  }
  paste(vapply(as.integer(bytes), function(byte) {
    if (byte >= 0x20 && byte <= 0x7e) {
      rawToChar(as.raw(byte))
    } else if (byte == 0x09) {
      "\\t"
    } else {
      sprintf("\\x%02X", byte)
    }
  }, character(1L)), collapse = "")
}

validate_fetched_documents <- function(fetched) {
  documents <- list()
  diagnostics <- list()
  robots <- fetched$robots
  for (i in seq_len(nrow(robots))) {
    source <- robots[i, , drop = FALSE]
    body <- robots$body[[i]]
    if (!is.null(body)) {
      validated <- validate_document_bytes(
        bytes = body,
        source_id = source$source_id,
        source_type = source$source_type,
        robots_url = source$robots_url,
        effective_url = source$effective_url,
        http_status = source$http_status,
        fetch_outcome = source$fetch_outcome,
        max_bytes = source$max_bytes
      )
    } else {
      validated <- unavailable_validation(source)
    }
    documents[[i]] <- validated$document
    if (nrow(validated$diagnostics) > 0L) {
      diagnostics[[length(diagnostics) + 1L]] <- validated$diagnostics
    }
  }

  map <- fetched$map
  map$validation_status <- rep("not_validated", nrow(map))
  if (length(documents) > 0L) {
    document_frame <- do.call(rbind, documents)
    status <- document_frame$validation_status[match(
      map$source_id, document_frame$source_id
    )]
    map$validation_status[!is.na(status)] <- status[!is.na(status)]
  } else {
    document_frame <- empty_validation_documents()
  }

  invalid <- which(is.na(map$source_id))
  for (i in invalid) {
    diagnostics[[length(diagnostics) + 1L]] <- validation_diagnostic(
      source_id = NA_character_,
      input_id = map$input_id[[i]],
      line = NA_integer_,
      severity = "error",
      code = "acquisition_input_invalid",
      directive = NA_character_,
      raw_text = map$url[[i]],
      message = map$error_message[[i]]
    )
  }
  diagnostic_frame <- if (length(diagnostics) == 0L) {
    empty_validation_diagnostics()
  } else {
    do.call(rbind, diagnostics)
  }
  new_robots_validations(map, document_frame, diagnostic_frame)
}

unavailable_validation <- function(source) {
  outcome <- source$fetch_outcome
  if (identical(outcome, "missing")) {
    severity <- "info"
    code <- "acquisition_missing"
    message <- "No robots.txt body was acquired because the resource is absent."
  } else if (identical(outcome, "body_too_large")) {
    severity <- "error"
    code <- "acquisition_limit_exceeded"
    message <- sprintf(
      "Decoded body exceeded max_bytes (%d); no truncated body was validated.",
      source$max_bytes
    )
  } else if (identical(outcome, "partial_response")) {
    severity <- "error"
    code <- "acquisition_partial_response"
    message <- "Partial HTTP response was rejected; no body was validated."
  } else {
    severity <- "error"
    code <- "acquisition_failed"
    message <- if (is.na(source$error_message)) {
      sprintf("Acquisition outcome '%s' supplied no document body.", outcome)
    } else {
      source$error_message
    }
  }
  diagnostic <- validation_diagnostic(
    source$source_id, NA_integer_, severity, code, NA_character_,
    NA_character_, message
  )
  document <- empty_validation_documents()[NA_integer_, , drop = FALSE]
  document <- document[1L, , drop = FALSE]
  document$source_id <- source$source_id
  document$source_type <- source$source_type
  document$robots_url <- source$robots_url
  document$effective_url <- source$effective_url
  document$http_status <- source$http_status
  document$fetch_outcome <- outcome
  document$validation_status <- "not_validated"
  document$profile <- robots_validation_profile
  document$byte_size <- source$body_size
  document$max_bytes <- source$max_bytes
  document$body_truncated <- FALSE
  document$limit_exceeded <- identical(outcome, "body_too_large")
  document$diagnostic_count <- 1L
  document$warning_count <- as.integer(severity == "warning")
  document$error_count <- as.integer(severity == "error")
  list(document = document, diagnostics = diagnostic)
}

empty_validation_documents <- function() {
  data.frame(
    source_id = character(),
    source_type = character(),
    robots_url = character(),
    effective_url = character(),
    http_status = integer(),
    fetch_outcome = character(),
    validation_status = character(),
    profile = character(),
    byte_size = integer(),
    line_count = integer(),
    active_line_count = integer(),
    recognized_directives = integer(),
    unknown_directives = integer(),
    malformed_lines = integer(),
    user_agent_directives = integer(),
    allow_directives = integer(),
    disallow_directives = integer(),
    sitemap_directives = integer(),
    unsupported_directives = integer(),
    encoding = character(),
    has_bom = logical(),
    nul_byte_count = integer(),
    final_newline = logical(),
    max_bytes = integer(),
    body_truncated = logical(),
    limit_exceeded = logical(),
    diagnostic_count = integer(),
    warning_count = integer(),
    error_count = integer(),
    stringsAsFactors = FALSE
  )
}
