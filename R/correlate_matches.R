# Match-metadata correlation (slice R3).
#
# The matcher reports a positive one-based `matching_line()` for a matched row.
# To turn that line number into the callback-derived rule type and the canonical
# (post-MaybeEscapePattern) rule value, ParseRobotsTxt() is run once per
# distinct source body with a private, read-only collector (the C++ binding
# `robotstxtr_collect_directives_`), and the line number is joined to the
# resulting per-source lookup.

# Build a per-line directive lookup for one robots.txt body by running the
# vendored parser once. Returns a data frame with columns `line` (integer),
# `type` (character), and `value` (character; elements may carry
# `Encoding = "bytes"` when a callback value is not valid UTF-8). The
# `body_utf8` argument is the same UTF-8 (or byte-verbatim) string handed to the
# matcher.
collect_directive_lookup <- function(body_utf8) {
  collected <- robotstxtr_collect_directives_(body_utf8)
  data.frame(
    line = collected$line,
    type = collected$type,
    value = collected$value,
    stringsAsFactors = FALSE
  )
}

# Correlate matched rows against per-source directive lookups.
#
# For each row with a positive `matching_line` (non-NA), look up its source's
# per-line directive table and overwrite `matched_rule_type` with the callback-
# derived type and `matched_rule_value` with the canonical callback value. Rows
# with no positive matching line are left untouched (their decision-derived type
# and NA value stand).
#
# `lookups` is a named list keyed by `source_id`; each element is the data frame
# returned by `collect_directive_lookup()`. A positive matching line absent from
# its source lookup is an internal invariant failure and raises a package error
# (never NA), per PRD 6.6.
#
# Returns a list with the updated `matched_rule_type` and `matched_rule_value`
# vectors.
correlate_match_metadata <- function(matching_line, source_id,
                                     matched_rule_type, matched_rule_value,
                                     lookups) {
  matched_rows <- which(!is.na(matching_line) & matching_line > 0L)
  for (i in matched_rows) {
    lookup <- lookups[[source_id[[i]]]]
    line <- matching_line[[i]]
    j <- match(line, lookup$line)
    if (is.na(j)) {
      robots_abort(
        sprintf(
          paste0(
            "matched line %d for source \"%s\" has no collected directive; ",
            "the matcher and parse collector disagree."
          ),
          line, source_id[[i]]
        ),
        "robotstxtr_missing_collected_line"
      )
    }
    matched_rule_type[[i]] <- lookup$type[[j]]
    matched_rule_value[[i]] <- lookup$value[[j]]
  }
  list(
    matched_rule_type = matched_rule_type,
    matched_rule_value = matched_rule_value
  )
}

# Normalize matched rows whose sole matched directive has an EMPTY PATH (R9).
#
# Per Google's robots.txt spec (and the vendored upstream test
# robots_test.cc:317-323), an `allow` or `disallow` rule WITHOUT a path is
# ignored by the matcher: it imposes no restriction and no rule actually "wins".
# The engine correctly returns `allowed = TRUE` for such a body, but its
# reporting layer still surfaces a positive matching line pointing at the
# ignored empty-path directive. After correlation that row carries a
# `matched_rule_value` of exactly "" (a real rule always has a non-empty path,
# e.g. "/"), so an empty callback value uniquely identifies the ignored case.
#
# For each such row, restate the metadata per Google's "no rule matched"
# semantics: `decision_source = "default_allow"`, `matched_rule_type = "none"`,
# `matched_line = NA`, `matched_rule_value = NA`. This applies whether the
# ignored directive was an Allow or a Disallow. It never touches the crawl
# decision (`allowed` stays TRUE) and never touches rows with an NA
# `matched_rule_value` (missing_allow / fetch_unknown / input_unknown / already
# default). Called from BOTH matching entry points so they cannot drift.
#
# Returns a list with the normalized `decision_source`, `matched_rule_type`,
# `matched_line`, and `matched_rule_value` vectors.
normalize_ignored_empty_path <- function(decision_source, matched_rule_type,
                                         matched_line, matched_rule_value) {
  ignored <- !is.na(matched_rule_value) & matched_rule_value == ""
  if (any(ignored)) {
    decision_source[ignored] <- "default_allow"
    matched_rule_type[ignored] <- "none"
    matched_line[ignored] <- NA_integer_
    matched_rule_value[ignored] <- NA_character_
  }
  list(
    decision_source = decision_source,
    matched_rule_type = matched_rule_type,
    matched_line = matched_line,
    matched_rule_value = matched_rule_value
  )
}
