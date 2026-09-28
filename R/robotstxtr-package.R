#' @keywords internal
#' @seealso
#' Engine-aware evaluation (versioned facade): [robots_evaluate_text_v1()],
#' [robots_evaluate_url_v1()].
#' Contract introspection: [robots_engine_contract_v1()],
#' [robots_engine_contract_v2()].
#' Google-matcher entry points: [allowed_by_robots_text()],
#' [allowed_by_robots_url()].
#' Acquisition evidence: [robots_fetch()].
#' Document validation: [robots_validate_text()], [robots_validate_url()].
#' Result inspection: [robots_body()], [as_legacy_robots_decisions_v1()].
#'
#' The bundled matcher is a vendored, upstream-test-validated snapshot of
#' Google's open-source C++ robots.txt matcher. Fetch origins are constructed
#' with the \pkg{rurl} package and HTTP is performed by \pkg{httr2}.
#'
#' @examples
#' # A short offline tour; no call below performs a network request.
#'
#' # 1. Match URLs against a supplied robots.txt body.
#' decisions <- allowed_by_robots_text(
#'   "user-agent: *\ndisallow: /private",
#'   c("https://example.com/page", "https://example.com/private"),
#'   "my-bot"
#' )
#' decisions$results[, c("url", "allowed", "matched_rule_value")]
#'
#' # 2. Inspect the stored body those decisions came from. The default
#' # `n = 20` prints a short preview, so ask for the whole body here.
#' robots_body(decisions, n = Inf)
#'
#' # 3. The same match through the engine-aware facade, which names the
#' # policy ruleset and the matcher backend explicitly.
#' engine <- robots_evaluate_text_v1(
#'   "user-agent: *\ndisallow: /private",
#'   "https://example.com/private",
#'   robots_product_token = "my-bot",
#'   robots_policy_ruleset = "google",
#'   matcher_backend = "google"
#' )
#' engine$results$url_decision
#'
#' # 4. Report parse-level problems in a document.
#' robots_validate_text("disallow: /before-agent\n")$diagnostics
#'
#' # 5. Inspect the contract: identifiers, value sets, backend capabilities.
#' contract <- robots_engine_contract_v2()
#' contract$contract_id
#' contract$matcher_backends
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
