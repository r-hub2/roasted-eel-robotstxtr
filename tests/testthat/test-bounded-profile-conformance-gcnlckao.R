# Conformance gate for the published bounded-profile selectors (ROBO-gcnlckao,
# PT2; parent ROBO-pkpnvfzp).
#
# The contract publishes, per `bounded_profiles` backend, which selectors are
# accepted AND how each one selects a user-agent group. Both halves have to be
# true of the matcher, so this suite asserts SELECTION BEHAVIOUR rather than
# `matcher_status` alone.
#
# Why status alone is not enough: a suite that only checks "every published
# token evaluates" proves the table has no false POSITIVES and nothing else. It
# cannot see changed case-folding, a `group_selection` value that no longer
# matches what the core does, or an accidental broadening in which every token
# starts evaluating. The published behaviour is the contract, so the behaviour
# is what gets pinned here.
#
# KNOWN LIMIT, deliberate. This direction catches false positives only. A token
# newly accepted by a vendored core but absent from the published table will not
# surface here, because nothing enumerates the core's accepted set -- that is
# what option (a), a native enumeration binding, would have bought. An
# unpublished-but-accepted token is a conservative false negative: it makes the
# contract understate the backend, which cannot cause the sitemapr-class failure
# this table exists to prevent. Revisit if a profile library ever gains tokens
# out of step with a package release.
#
# Uniquely prefixed (bp_) helpers -- avoid collision with sibling suites.
# ---------------------------------------------------------------------------

bp_target <- "https://example.test/private"

# Evaluate `tokens` against one robots.txt body on one backend, forcing the
# supplied-body use_rules path. An allow_all, transport or other policy
# short-circuit never invokes the matcher, which would make every assertion
# below vacuously true, so the caller asserts policy_action too.
bp_eval <- function(backend, body, tokens, url = bp_target) {
  n <- length(tokens)
  robots_evaluate_text_v1(
    robots_txt = body,
    url = rep(url, n),
    robots_product_token = tokens,
    robots_policy_ruleset = rep(backend, n),
    matcher_backend = rep(backend, n)
  )$results
}

# Evaluate and assert the matcher was actually reached, then return the rows.
bp_eval_matched <- function(backend, body, tokens, url = bp_target) {
  res <- bp_eval(backend, body, tokens, url)
  expect_identical(
    unique(res$policy_action), "use_rules",
    info = "policy short-circuited; the matcher never ran"
  )
  res
}

bp_profiles <- function(backend) {
  robots_engine_contract_v1()$matcher_capability[[backend]]$supported_profiles
}

bp_available <- function(backend) {
  identical(engine_matcher_availability_v1()[[backend]], "available")
}

# The per-backend vocabulary for an unaccepted selector. The two backends do NOT
# report it identically and are deliberately not normalized: Yandex folds it
# into the generic not_evaluated status, Bing raises a v2 status of its own.
bp_unsupported <- list(
  yandex = list(
    matcher_status = "not_evaluated",
    reason = "unsupported_product_token",
    error_class = "robots_unsupported_product_token"
  ),
  bing = list(
    matcher_status = "unsupported_profile",
    reason = "unsupported_profile",
    error_class = "robots_unsupported_profile"
  )
)

# Selectors that must stay unsupported on each backend: real crawler identities
# (the sitemapr trap), affixed and partial spellings, whitespace-bearing values,
# and the wildcard, which is a group label but never a selector.
bp_near_misses <- list(
  yandex = c(
    "YandexBot", "YandexImages", "YandexMobileBot", "YandexAccessibilityBot",
    "YandexComBot", "YandexAdditional", "Yandex/1.0", "Yandex Additional Bot",
    " Yandex", "Yandex ", "*"
  ),
  bing = c(
    "msnbot", "BingPreview", "Bingbot/2.0", "bing", "adidx", "bingbot2",
    "bing bot", " bingbot", "bingbot ", "*"
  )
)

bp_backends <- c("yandex", "bing")

# ---------------------------------------------------------------------------
# The published table itself
# ---------------------------------------------------------------------------

test_that("BP1 the published profile tables are exactly the frozen sets", {
  expect_identical(
    bp_profiles("yandex")[, c("accepted_token", "group_selection")],
    data.frame(
      accepted_token = c("Yandex", "YandexAdditionalBot"),
      group_selection = c("exact_else_wildcard", "exact_only"),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(
    bp_profiles("bing")[, c("accepted_token", "group_selection")],
    data.frame(
      accepted_token = c("bingbot", "adidxbot"),
      group_selection = c("exact_else_wildcard", "exact_only"),
      stringsAsFactors = FALSE
    )
  )

  # A crawler identity must never appear as a published selector. `Yandex` is a
  # robots.txt group label that no crawler sends; `YandexBot` is the reverse.
  expect_false("YandexBot" %in% bp_profiles("yandex")$accepted_token)
  expect_false("msnbot" %in% bp_profiles("bing")$accepted_token)
})

test_that("BP2 only bounded backends publish selector data", {
  cap <- robots_engine_contract_v1()$matcher_capability
  fields <- c(
    "product_token_role", "product_token_comparison", "supported_profiles"
  )

  for (backend in bp_backends) {
    expect_identical(cap[[backend]]$token_policy, "bounded_profiles")
    expect_true(all(fields %in% names(cap[[backend]])))
    expect_identical(
      cap[[backend]]$product_token_role, "matcher_profile_selector"
    )
    expect_identical(
      cap[[backend]]$product_token_comparison, "ascii_case_insensitive_exact"
    )
  }

  # An unbounded backend has no closed set, so it publishes none of the three.
  for (backend in c("google", "rfc9309")) {
    expect_false(any(fields %in% names(cap[[backend]])))
  }
})

# ---------------------------------------------------------------------------
# Selection behaviour, driven from the published table
# ---------------------------------------------------------------------------

test_that("BP3 each published selector is selected by its own exact group", {
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))
    profiles <- bp_profiles(backend)

    for (i in seq_len(nrow(profiles))) {
      token <- profiles$accepted_token[i]
      body <- sprintf("User-agent: %s\nDisallow: /private", token)
      res <- bp_eval_matched(backend, body, token)

      expect_identical(res$matcher_status, "evaluated", info = token)
      expect_identical(res$url_decision, "disallow", info = token)
      expect_identical(res$reason, "rule_disallow", info = token)
    }
  }
})

test_that("BP4 a sibling profile's group never selects another profile", {
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))
    tokens <- bp_profiles(backend)$accepted_token
    expect_length(tokens, 2L)

    # Each token addressed by the OTHER token's group only. With no group of its
    # own and no wildcard present, nothing constrains it.
    for (i in seq_along(tokens)) {
      other <- tokens[setdiff(seq_along(tokens), i)]
      body <- sprintf("User-agent: %s\nDisallow: /private", other)
      res <- bp_eval_matched(backend, body, tokens[i])

      expect_identical(res$matcher_status, "evaluated", info = tokens[i])
      expect_identical(res$url_decision, "allow", info = tokens[i])
      expect_identical(res$reason, "default_allow", info = tokens[i])
    }
  }
})

test_that("BP5 wildcard fallback follows the published group_selection", {
  wildcard_body <- "User-agent: *\nDisallow: /private"

  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))
    profiles <- bp_profiles(backend)

    for (i in seq_len(nrow(profiles))) {
      token <- profiles$accepted_token[i]
      mode <- profiles$group_selection[i]
      res <- bp_eval_matched(backend, wildcard_body, token)

      expect_identical(res$matcher_status, "evaluated", info = token)
      if (identical(mode, "exact_else_wildcard")) {
        # No group names it, so the wildcard group applies.
        expect_identical(res$url_decision, "disallow", info = token)
        expect_identical(res$reason, "rule_disallow", info = token)
      } else {
        # exact_only: the wildcard group is invisible to this profile.
        expect_identical(res$url_decision, "allow", info = token)
        expect_identical(res$reason, "default_allow", info = token)
      }
    }
  }
})

test_that("BP6 an exact group beats a wildcard group for both modes", {
  # Both modes merge exactly-named groups; they differ only in the FALLBACK.
  # With both groups present, every profile must take its own.
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))

    for (token in bp_profiles(backend)$accepted_token) {
      body <- sprintf(
        "User-agent: *\nDisallow: /private\n\nUser-agent: %s\nAllow: /private",
        token
      )
      res <- bp_eval_matched(backend, body, token)
      expect_identical(res$matcher_status, "evaluated", info = token)
      expect_identical(res$url_decision, "allow", info = token)
    }
  }
})

# ---------------------------------------------------------------------------
# Comparison mode
# ---------------------------------------------------------------------------

test_that("BP7 selectors resolve ASCII case-insensitively and identically", {
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))

    for (token in bp_profiles(backend)$accepted_token) {
      spellings <- unique(c(token, tolower(token), toupper(token)))
      # The group is written in the canonical spelling; only the SELECTOR case
      # varies, so any difference is the comparison mode and nothing else.
      body <- sprintf("User-agent: %s\nDisallow: /private", token)
      res <- bp_eval_matched(backend, body, spellings)

      expect_identical(
        res$matcher_status, rep("evaluated", length(spellings)), info = token
      )
      expect_identical(
        res$url_decision, rep("disallow", length(spellings)), info = token
      )
      # Case folding is total: every spelling gives one distinct outcome.
      expect_length(unique(res$reason), 1L)
    }
  }
})

# ---------------------------------------------------------------------------
# Rejection, with each backend's own vocabulary
# ---------------------------------------------------------------------------

test_that("BP8 near misses stay unsupported in the backend's own vocabulary", {
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))
    tokens <- bp_near_misses[[backend]]
    expected <- bp_unsupported[[backend]]

    res <- bp_eval(backend, "User-agent: *\nDisallow: /private", tokens)

    expect_identical(
      res$matcher_status, rep(expected$matcher_status, length(tokens)),
      info = backend
    )
    expect_identical(
      res$reason, rep(expected$reason, length(tokens)), info = backend
    )
    expect_identical(
      res$error_class, rep(expected$error_class, length(tokens)), info = backend
    )
    # Nothing was decided for any of them.
    expect_true(all(is.na(res$url_decision)))
  }
})

test_that("BP8b an empty selector is invalid input, not an unknown profile", {
  # The empty string is rejected one layer EARLIER than an unknown selector, at
  # input validation, and both backends agree there. Kept separate from BP8 so
  # the two layers cannot be conflated: an unaccepted-but-well-formed token is a
  # profile question, an empty one never reaches the profile lookup at all.
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))
    res <- bp_eval(backend, "User-agent: *\nDisallow: /private", "")

    expect_identical(res$matcher_status, "not_evaluated", info = backend)
    expect_identical(res$reason, "input_invalid", info = backend)
    expect_identical(
      res$error_class, "robots_invalid_product_token", info = backend
    )
    expect_identical(res$error_stage, "input", info = backend)
    expect_true(is.na(res$url_decision))
  }
})

test_that("BP9 the two backends do not share a rejection vocabulary", {
  skip_if_not(bp_available("yandex"), "yandex unavailable")
  skip_if_not(bp_available("bing"), "bing unavailable")

  # Guards against a well-meaning future normalization of the two shapes. If
  # they are ever deliberately unified, this test is the place that records the
  # decision -- it must not be deleted silently.
  y <- bp_eval("yandex", "User-agent: *\nDisallow: /private", "YandexBot")
  b <- bp_eval("bing", "User-agent: *\nDisallow: /private", "msnbot")

  expect_false(identical(y$matcher_status, b$matcher_status))
  expect_false(identical(y$error_class, b$error_class))
  expect_identical(y$matcher_status, "not_evaluated")
  expect_identical(b$matcher_status, "unsupported_profile")
})

# ---------------------------------------------------------------------------
# Table-vs-matcher agreement, the property the publication actually claims
# ---------------------------------------------------------------------------

test_that("BP10 every published selector is accepted by its own backend", {
  for (backend in bp_backends) {
    skip_if_not(bp_available(backend), sprintf("%s unavailable", backend))
    tokens <- bp_profiles(backend)$accepted_token

    res <- bp_eval_matched(
      backend, "User-agent: *\nDisallow: /private", tokens
    )
    # None of them is rejected: whatever the decision, the matcher ran.
    expect_identical(res$matcher_status, rep("evaluated", length(tokens)))
    expect_false(any(res$reason %in% c(
      "unsupported_product_token", "unsupported_profile"
    )))
  }
})

test_that("BP11 a published selector is not accepted by the other backend", {
  skip_if_not(bp_available("yandex"), "yandex unavailable")
  skip_if_not(bp_available("bing"), "bing unavailable")

  # Bounded means bounded: the sets do not leak across vendors.
  body <- "User-agent: *\nDisallow: /private"
  crossed <- bp_eval("yandex", body, bp_profiles("bing")$accepted_token)
  expect_identical(
    crossed$matcher_status, rep("not_evaluated", 2L)
  )
  crossed <- bp_eval("bing", body, bp_profiles("yandex")$accepted_token)
  expect_identical(
    crossed$matcher_status, rep("unsupported_profile", 2L)
  )
})
