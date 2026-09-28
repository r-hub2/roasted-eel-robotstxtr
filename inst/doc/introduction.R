## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(robotstxtr)

## -----------------------------------------------------------------------------
robots <- "user-agent: *
disallow: /private
allow: /private/public
"

allowed_by_robots_text(robots, "https://example.com/private/report", "mybot")

## -----------------------------------------------------------------------------
decision <- allowed_by_robots_text(
  robots,
  c(
    "https://example.com/index.html",     # not disallowed, allowed
    "https://example.com/private/report", # under /private, disallowed
    "https://example.com/private/public"  # re-allowed by the Allow rule
  ),
  "mybot"
)

decision$results[c("url", "allowed", "decision_source")]

## -----------------------------------------------------------------------------
allowed_by_robots_text(
  "user-agent: newsbot\ndisallow: /\nuser-agent: *\nallow: /",
  c("https://example.com/a", "https://example.com/a"),
  c("newsbot", "otherbot")
)$results[c("url", "user_agent", "allowed")]

## -----------------------------------------------------------------------------
m <- allowed_by_robots_text(
  "user-agent: *\ndisallow: /*.pdf$",
  "https://example.com/manual.pdf",
  "mybot"
)
cols <- c("allowed", "matched_line", "matched_rule_type", "matched_rule_value")
m$results[cols]

## -----------------------------------------------------------------------------
robots_body(decision, n = 40)

## ----eval = FALSE-------------------------------------------------------------
# # Not evaluated here: this call performs a network request, which vignettes
# # must never require. Run it in an interactive session against a real site.
# allowed_by_robots_url("https://example.com/some/page", "mybot")

## -----------------------------------------------------------------------------
validation <- robots_validate_text(
  "user-agent: *\ndisallow: /private\nunknown-field: value\n"
)
validation$documents
validation$diagnostics

## ----eval = FALSE-------------------------------------------------------------
# # Performs live HTTP, so it is not evaluated in the vignette.
# robots_validate_url("https://example.com/some/page")

## -----------------------------------------------------------------------------
# Path case is significant: /Private is disallowed, /private is not.
allowed_by_robots_text(
  "user-agent: *\ndisallow: /Private",
  c("https://example.com/Private", "https://example.com/private"),
  "mybot"
)$results[c("url", "allowed")]

