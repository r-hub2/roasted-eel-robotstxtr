
<!-- README.md is generated from README.Rmd. Please edit that file and run
     devtools::build_readme() to regenerate README.md. -->

# robotstxtr

<!-- badges: start -->

[![Pipeline
status](https://gitlab.com/bart-turczynski/robotstxtr/badges/main/pipeline.svg)](https://gitlab.com/bart-turczynski/robotstxtr/-/pipelines)
[![CRAN
status](https://www.r-pkg.org/badges/version/robotstxtr)](https://CRAN.R-project.org/package=robotstxtr)
[![Coverage](https://gitlab.com/bart-turczynski/robotstxtr/badges/main/coverage.svg)](https://gitlab.com/bart-turczynski/robotstxtr/-/pipelines)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
<!-- badges: end -->

Faithful `robots.txt` parsing and URL-allowed decisions for R, powered
by Google’s own matcher.

`robotstxtr` evaluates whether a URL may be crawled according to a
site’s `robots.txt` rules. It wraps a vendored, upstream-test-validated
snapshot of Google’s open-source C++ `robots.txt` parser and matcher, so
allow and disallow decisions match Googlebot’s behavior — including the
quirky parts (longest-match wins, `Allow`-beats-`Disallow` ties, `*`/`$`
wildcards, case-sensitive paths, percent-escape canonicalization). It
can also fetch `robots.txt` for you under a deterministic, conservative
`httr2` policy, construct fetch origins with `rurl`, and return richly
typed, vectorized decisions with per-rule match metadata.

This is a faithful, upstream-test-validated replica of the pinned
matcher, not a claim of universal behavioral identity with Googlebot.

## Installation

Install the released version from CRAN:

``` r
install.packages("robotstxtr")
```

The development version lives on GitLab. Its dependencies, including
`rurl`, are all on CRAN, so it installs with no extra repository
configuration:

``` r
# with pak
pak::pak("gitlab::bart-turczynski/robotstxtr")

# or with remotes
remotes::install_gitlab("bart-turczynski/robotstxtr")
```

Installing from source compiles the vendored C++ matcher and requires a
C++17 toolchain. Once dependencies have been obtained, installing,
testing, and checking the package does not make live network requests.

## Usage

Match one or more URLs against a `robots.txt` body you already have,
fully offline, with `allowed_by_robots_text()`:

``` r
library(robotstxtr)

robots <- "user-agent: *
disallow: /private
allow: /private/public
"

x <- allowed_by_robots_text(
  robots,
  c(
    "https://example.com/index.html",     # not disallowed  -> TRUE
    "https://example.com/private/report", # under /private  -> FALSE
    "https://example.com/private/public"  # re-allowed      -> TRUE
  ),
  "mybot"
)

x$results[c("url", "allowed", "decision_source")]
#>                                  url allowed decision_source
#> 1     https://example.com/index.html    TRUE   default_allow
#> 2 https://example.com/private/report   FALSE   rule_disallow
#> 3 https://example.com/private/public    TRUE      rule_allow
```

`allowed` is the `TRUE`/`FALSE`/`NA` answer; `decision_source` explains
why.

If you do not already have the `robots.txt` text,
`allowed_by_robots_url()` derives the governing origin, fetches it once
per origin, and matches:

``` r
allowed_by_robots_url("https://example.com/some/page", "mybot")
```

See the “Introduction to robotstxtr” vignette for the full walkthrough,
including match metadata, the fetch policy, and body inspection with
`robots_body()`.

## Validate a robots.txt document

Document validation is separate from URL allow/disallow evaluation.
Validate supplied text (or raw acquisition bytes) with
`robots_validate_text()`:

``` r
validation <- robots_validate_text(
  "user-agent: *\ndisallow: /private\nunknown-field: value\n"
)

validation$documents
validation$diagnostics
```

Use `robots_validate_url()` when the document must first be fetched. It
calls the same deterministic `robots_fetch()` path and validates each
distinct stored raw body once; acquisition limits and failures remain
explicit evidence.

The profile is deliberately named `google-parser-compatible`. It
describes the pinned parser plus conservative structural checks, not
universal crawler or RFC 9309 validity. Validation reports file syntax
and structure only: it does not answer whether a particular URL is
allowed, infer whether rules match the webmaster’s intent, or fetch and
check remote sitemap resources.

## Engine-aware v1 contract

Multi-engine integrations should use the parallel versioned facade and
select both axes explicitly:

``` r
robots_evaluate_text_v1(
  robots,
  "https://example.com/private/report",
  robots_product_token = "Googlebot",
  robots_policy_ruleset = "google",
  matcher_backend = "google"
)
```

The result distinguishes neutral fetch evidence, engine policy, matcher
availability, and the final URL decision. Unsupported RFC 9309 and Bing
matchers return `capability_unavailable` rather than silently falling
back to Google.

The Yandex `matcher_backend` is available as of schema revision
`2026-07-18.2`, bounded to profile `yandex-0.1.0` and the `Yandex` and
`YandexAdditionalBot` crawlers (every other Yandex-backend token
resolves to a checked `unsupported_crawler` non-decision). It is an
independent, unofficial compatibility profile and makes no
production-crawler parity claim. See [the engine-aware
contract](https://gitlab.com/bart-turczynski/robotstxtr/-/blob/main/design/engine-contract-v1.md)
and `robots_engine_contract_v1()` for schema revisions and capability
metadata.
