# Construction-time resolution of a matcher-profile selector (ROBO-nvuwpktu,
# PT3; parent ROBO-pkpnvfzp).
#
# The engine contract publishes which selectors a `bounded_profiles` backend
# accepts, but publication only helps a consumer who remembers to read it.
# sitemapr did not: its `yandex` preset carried "YandexBot" from the day it was
# written, and every URL of every sitemap came back indeterminate.
#
# Inspecting results cannot be the guard. Profile support is a property of the
# MATCHER, and a row need never reach the matcher: an allow_all policy returns a
# decision for a nonsensical selector without ever consulting the profile. So
# the check belongs where the configuration is built, before anything is
# fetched or evaluated -- which is what this function is for.

# The condition class raised for an unaccepted selector on a bounded backend.
# Distinct from the per-row `error_class` values, which stay as they are: those
# describe a row that was evaluated and could not be decided, this describes a
# configuration that should never have been constructed.
robots_unresolvable_profile_class <- function() {
  "robotstxtr_unresolvable_matcher_profile"
}

# The accepted selectors for a bounded backend, rendered for an error message.
# Single-sourced from the published table so a message can never name a set the
# contract does not publish.
accepted_selectors_phrase_v1 <- function(backend) {
  entry <- engine_backend_capability_v1()[[backend]]
  toString(sprintf("\"%s\"", entry$supported_profiles$accepted_token))
}

# Recycle a length-1 vector to `n`, or pass a length-n vector through.
resolve_profile_recycle <- function(x, n, arg) {
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  if (length(x) != n) {
    robots_abort(
      sprintf(
        "`%s` must have length 1 or %d, not %d.", arg, n, length(x)
      ),
      "robotstxtr_invalid_input"
    )
  }
  x
}

# Is `token` plausibly a crawler identity that a caller mistook for a selector?
# Only used to sharpen the error message, never to decide acceptance.
resolve_profile_looks_like_crawler <- function(token, accepted) {
  lowered <- tolower(token)
  any(startsWith(lowered, tolower(accepted)) & lowered != tolower(accepted))
}

#' Resolve a matcher-profile selector before evaluating anything
#'
#' Checks, at configuration-construction time, that a product token is one the
#' named matcher backend will actually accept — and reports what it will DO,
#' not merely that it is accepted. Performs no fetch and no matching.
#'
#' A `bounded_profiles` backend (`"yandex"`, `"bing"`) accepts a closed set of
#' selectors. Passing an unaccepted one is not an error during evaluation: the
#' affected rows come back undecided, with `url_decision` `NA`, which reads like
#' a transport failure rather than a configuration mistake. This function turns
#' that quiet outcome into a loud one at the point where the mistake is
#' actually made.
#'
#' Checking the evaluation results instead is not equivalent. Profile support
#' is a property of the matcher, and a row need not reach the matcher at all —
#' an `allow_all` policy returns a decision for a nonsensical selector without
#' ever consulting the profile.
#'
#' The `robots_product_token` is a robots.txt `User-agent:` group selector, not
#' an HTTP `User-Agent` crawler identity. The distinction is easy to miss
#' because RFC 9309 treats them as one namespace, and vendors do not: Yandex's
#' `"Yandex"` is an umbrella group label that no crawler ever sends, while
#' `"YandexBot"` — a real crawler — is not an accepted selector at all.
#'
#' @param matcher_backend A character vector of matcher backends, length one or
#'   `length(robots_product_token)`: `"google"`, `"yandex"`, `"rfc9309"`, or
#'   `"bing"`.
#' @param robots_product_token A character vector of selectors, length one or
#'   `length(matcher_backend)`.
#'
#' @return A data frame with one row per resolved selector and the columns
#'   `matcher_backend`, `robots_product_token` (the value as supplied),
#'   `token_policy`, `profile_id`, `accepted_token` (the canonical spelling),
#'   `group_label`, `group_selection`, and `profile_revision`. For a backend
#'   whose `token_policy` is not `"bounded_profiles"` there is no closed set to
#'   resolve against, so the profile columns are `NA` and the token is returned
#'   unchanged.
#'
#'   `group_selection` is `"exact_else_wildcard"` when the profile falls back to
#'   `User-agent: *` groups if no group names it, and `"exact_only"` when it
#'   never does. Two accepted selectors on one backend are therefore not
#'   interchangeable.
#'
#' @seealso [robots_engine_contract_v1()], whose `matcher_capability` publishes
#'   the same data as a table.
#' @examples
#' # The umbrella group label and the Additional crawler are both accepted, but
#' # they select groups differently.
#' robots_resolve_matcher_profile_v1(
#'   "yandex", c("Yandex", "YandexAdditionalBot")
#' )
#'
#' # Selectors are compared ASCII case-insensitively; the canonical spelling
#' # comes back in `accepted_token`.
#' robots_resolve_matcher_profile_v1("bing", "BINGBOT")
#'
#' # A real crawler identity is not a selector. This is the mistake the
#' # function exists to catch, so it errors rather than returning a row.
#' try(robots_resolve_matcher_profile_v1("yandex", "YandexBot"))
#'
#' # An unbounded backend has no closed set to resolve against.
#' robots_resolve_matcher_profile_v1("google", "Googlebot")
#' @export
robots_resolve_matcher_profile_v1 <- function(matcher_backend,
                                              robots_product_token) {
  if (!is.character(matcher_backend) || anyNA(matcher_backend) ||
        length(matcher_backend) == 0L) {
    robots_abort(
      "`matcher_backend` must be a non-empty character vector without NA.",
      "robotstxtr_invalid_input"
    )
  }
  if (!is.character(robots_product_token) || anyNA(robots_product_token) ||
        length(robots_product_token) == 0L) {
    robots_abort(
      paste0(
        "`robots_product_token` must be a non-empty character vector ",
        "without NA."
      ),
      "robotstxtr_invalid_input"
    )
  }

  n <- max(length(matcher_backend), length(robots_product_token))
  backends <- resolve_profile_recycle(matcher_backend, n, "matcher_backend")
  tokens <- resolve_profile_recycle(
    robots_product_token, n, "robots_product_token"
  )

  known <- engine_matchers_v1()
  unknown <- !backends %in% known
  if (any(unknown)) {
    robots_abort(
      sprintf(
        "Unknown matcher backend: %s. Known backends: %s.",
        toString(unique(backends[unknown])), toString(known)
      ),
      "robotstxtr_invalid_input"
    )
  }

  capability <- validated_backend_capability_v1()
  out <- vector("list", n)

  for (i in seq_len(n)) {
    out[[i]] <- resolve_one_matcher_profile_v1(
      backends[i], tokens[i], capability[[backends[i]]]
    )
  }

  do.call(rbind, out)
}

# Resolve one (backend, token) pair against one published capability entry.
resolve_one_matcher_profile_v1 <- function(backend, token, entry) {
  unbounded <- data.frame(
    matcher_backend = backend,
    robots_product_token = token,
    token_policy = entry$token_policy,
    profile_id = NA_character_,
    accepted_token = NA_character_,
    group_label = NA_character_,
    group_selection = NA_character_,
    profile_revision = NA_character_,
    stringsAsFactors = FALSE
  )

  # An unbounded backend has no closed set: there is nothing to resolve, and
  # rejecting here would be inventing a boundary the backend does not have.
  if (!identical(entry$token_policy, "bounded_profiles")) {
    return(unbounded)
  }

  profiles <- entry$supported_profiles
  hit <- match(tolower(token), tolower(profiles$accepted_token))

  if (is.na(hit)) {
    accepted <- profiles$accepted_token
    detail <- if (resolve_profile_looks_like_crawler(token, accepted)) {
      paste0(
        " \"", token, "\" is a crawler identity, not a matcher-profile ",
        "selector: a robots.txt `User-agent:` group label is a different ",
        "namespace from an HTTP `User-Agent` header value."
      )
    } else {
      ""
    }
    robots_abort(
      sprintf(
        paste0(
          "Matcher backend \"%s\" does not accept the product token \"%s\". ",
          "Accepted selectors: %s.%s"
        ),
        backend, token, toString(sprintf("\"%s\"", accepted)), detail
      ),
      robots_unresolvable_profile_class()
    )
  }

  data.frame(
    matcher_backend = backend,
    robots_product_token = token,
    token_policy = entry$token_policy,
    profile_id = profiles$profile_id[hit],
    accepted_token = profiles$accepted_token[hit],
    group_label = profiles$group_label[hit],
    group_selection = profiles$group_selection[hit],
    profile_revision = profiles$profile_revision[hit],
    stringsAsFactors = FALSE
  )
}
