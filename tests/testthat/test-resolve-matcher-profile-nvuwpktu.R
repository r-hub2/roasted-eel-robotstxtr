# robots_resolve_matcher_profile_v1() (ROBO-nvuwpktu, PT3; parent
# ROBO-pkpnvfzp).
#
# The construction-time guard. The contract publishes which selectors a bounded
# backend accepts, but a table only helps a consumer who reads it; sitemapr did
# not, and shipped "YandexBot" as its yandex preset. This function makes the
# mistake loud where it is made rather than quiet where it surfaces.
#
# Uniquely prefixed (rp_) helpers -- avoid collision with sibling suites.
# ---------------------------------------------------------------------------

rp_resolve <- robotstxtr::robots_resolve_matcher_profile_v1

rp_cols <- c(
  "matcher_backend", "robots_product_token", "token_policy", "profile_id",
  "accepted_token", "group_label", "group_selection", "profile_revision"
)

test_that("RP1 an accepted selector resolves to its published profile row", {
  res <- rp_resolve("yandex", "Yandex")

  expect_s3_class(res, "data.frame")
  expect_named(res, rp_cols)
  expect_identical(nrow(res), 1L)
  expect_identical(res$token_policy, "bounded_profiles")
  expect_identical(res$profile_id, "yandex")
  expect_identical(res$accepted_token, "Yandex")
  expect_identical(res$group_selection, "exact_else_wildcard")
  expect_identical(res$profile_revision, "yandex-0.1.0")
})

test_that("RP2 resolution reports selection behaviour, not just acceptance", {
  # The whole reason the resolver returns a row rather than TRUE: two accepted
  # selectors on one backend do different things.
  res <- rp_resolve("yandex", c("Yandex", "YandexAdditionalBot"))

  expect_identical(nrow(res), 2L)
  expect_identical(
    res$group_selection, c("exact_else_wildcard", "exact_only")
  )
  expect_false(res$group_selection[1] == res$group_selection[2])
})

test_that("RP3 selectors resolve ASCII case-insensitively to canonical form", {
  for (spelling in c("yandex", "YANDEX", "YaNdEx")) {
    res <- rp_resolve("yandex", spelling)
    # The supplied value is echoed; the canonical spelling is separate.
    expect_identical(res$robots_product_token, spelling)
    expect_identical(res$accepted_token, "Yandex")
  }
  for (spelling in c("bingbot", "BINGBOT", "BingBot")) {
    expect_identical(rp_resolve("bing", spelling)$accepted_token, "bingbot")
  }
})

test_that("RP4 a crawler identity errors and the message says why", {
  err <- tryCatch(rp_resolve("yandex", "YandexBot"), error = function(e) e)

  expect_s3_class(err, "robotstxtr_unresolvable_matcher_profile")
  expect_s3_class(err, "robotstxtr_error")
  msg <- conditionMessage(err)
  # Names the accepted set, so the caller can fix it without reading docs.
  expect_match(msg, "Yandex", fixed = TRUE)
  expect_match(msg, "YandexAdditionalBot", fixed = TRUE)
  # And names the actual confusion, which is the namespace, not a typo.
  expect_match(msg, "crawler identity", fixed = TRUE)
  expect_match(msg, "User-agent", fixed = TRUE)
})

test_that("RP5 an unrelated unaccepted selector errors without the crawler
          explanation", {
  err <- tryCatch(rp_resolve("yandex", "nonsense"), error = function(e) e)

  expect_s3_class(err, "robotstxtr_unresolvable_matcher_profile")
  msg <- conditionMessage(err)
  expect_match(msg, "Yandex", fixed = TRUE)
  # The crawler-identity sentence is a targeted hint, not boilerplate: it is
  # only earned by a token that extends an accepted selector.
  expect_false(grepl("crawler identity", msg, fixed = TRUE))
})

test_that("RP6 every backend's rejection names that backend's own set", {
  y <- tryCatch(rp_resolve("yandex", "msnbot"), error = conditionMessage)
  b <- tryCatch(rp_resolve("bing", "Yandex"), error = conditionMessage)

  expect_match(y, "YandexAdditionalBot", fixed = TRUE)
  expect_false(grepl("bingbot", y, fixed = TRUE))
  expect_match(b, "adidxbot", fixed = TRUE)
  expect_false(grepl("YandexAdditionalBot", b, fixed = TRUE))
})

test_that("RP7 an unbounded backend has nothing to resolve against", {
  # google accepts any valid token, so rejecting here would invent a boundary
  # the backend does not have. The profile columns are NA, not an error.
  res <- rp_resolve("google", "Googlebot")
  expect_identical(res$token_policy, "arbitrary_valid")
  expect_true(is.na(res$profile_id))
  expect_true(is.na(res$group_selection))
  expect_identical(res$robots_product_token, "Googlebot")

  # Even a nonsense token is not the resolver's business on an unbounded
  # backend.
  expect_silent(rp_resolve("google", "not-a-real-crawler"))

  res <- rp_resolve("rfc9309", "anything")
  expect_identical(res$token_policy, "rfc9309")
  expect_true(is.na(res$profile_id))
})

test_that("RP8 the call is vectorized and recycles either argument", {
  res <- rp_resolve(c("yandex", "bing"), c("Yandex", "adidxbot"))
  expect_identical(nrow(res), 2L)
  expect_identical(res$matcher_backend, c("yandex", "bing"))
  expect_identical(res$group_selection, c("exact_else_wildcard", "exact_only"))

  # Backend recycled across many tokens.
  res <- rp_resolve("bing", c("bingbot", "adidxbot"))
  expect_identical(res$matcher_backend, c("bing", "bing"))

  # Token recycled across many backends.
  res <- rp_resolve(c("google", "rfc9309"), "Googlebot")
  expect_identical(nrow(res), 2L)
  expect_identical(res$robots_product_token, c("Googlebot", "Googlebot"))

  # One bad selector in a batch fails the whole call: a configuration is valid
  # or it is not, and a partially resolved preset is not useful.
  expect_error(
    rp_resolve("yandex", c("Yandex", "YandexBot")),
    class = "robotstxtr_unresolvable_matcher_profile"
  )
})

test_that("RP9 malformed input is rejected as input, not as a bad profile", {
  expect_error(rp_resolve(1L, "Yandex"), class = "robotstxtr_invalid_input")
  expect_error(rp_resolve("yandex", 1L), class = "robotstxtr_invalid_input")
  expect_error(
    rp_resolve(NA_character_, "Yandex"), class = "robotstxtr_invalid_input"
  )
  expect_error(
    rp_resolve("yandex", NA_character_), class = "robotstxtr_invalid_input"
  )
  expect_error(
    rp_resolve(character(0), "Yandex"), class = "robotstxtr_invalid_input"
  )
  expect_error(
    rp_resolve("nosuchbackend", "Yandex"), class = "robotstxtr_invalid_input"
  )
  # Length mismatch that is not a clean recycle.
  expect_error(
    rp_resolve(c("yandex", "bing", "google"), c("Yandex", "bingbot")),
    class = "robotstxtr_invalid_input"
  )
})

test_that("RP10 the resolver agrees with the contract table it reads", {
  # The resolver must never accept something the published table omits, nor
  # reject something it lists. Driven from the table so the two cannot drift.
  cap <- robots_engine_contract_v1()$matcher_capability

  for (backend in c("yandex", "bing")) {
    profiles <- cap[[backend]]$supported_profiles
    res <- rp_resolve(backend, profiles$accepted_token)

    expect_identical(res$accepted_token, profiles$accepted_token)
    expect_identical(res$group_selection, profiles$group_selection)
    expect_identical(res$profile_revision, profiles$profile_revision)
    expect_identical(res$profile_id, profiles$profile_id)
  }
})

test_that("RP11 resolution predicts what evaluation actually does", {
  # The claim the resolver makes is behavioural, so check it against the
  # matcher rather than against the table a second time.
  skip_if_not(
    identical(engine_matcher_availability_v1()[["yandex"]], "available"),
    "yandex unavailable"
  )
  skip_if_not(
    identical(engine_matcher_availability_v1()[["bing"]], "available"),
    "bing unavailable"
  )

  wildcard <- "User-agent: *\nDisallow: /private"

  for (backend in c("yandex", "bing")) {
    res <- rp_resolve(
      backend,
      robots_engine_contract_v1()$
        matcher_capability[[backend]]$supported_profiles$accepted_token
    )
    n <- nrow(res)
    evaluated <- robots_evaluate_text_v1(
      robots_txt = wildcard,
      url = rep("https://example.test/private", n),
      robots_product_token = res$accepted_token,
      robots_policy_ruleset = rep(backend, n),
      matcher_backend = rep(backend, n)
    )$results

    # Nothing the resolver accepted is rejected by the matcher.
    expect_identical(evaluated$matcher_status, rep("evaluated", n))
    # And group_selection predicts the wildcard outcome exactly.
    expected <- ifelse(
      res$group_selection == "exact_else_wildcard", "disallow", "allow"
    )
    expect_identical(evaluated$url_decision, expected)
  }
})

test_that("RP12 the unsupported-row message points at the resolver", {
  # The per-row failure keeps its status vocabulary, but the message now names
  # the accepted set and the way to check up front.
  skip_if_not(
    identical(engine_matcher_availability_v1()[["yandex"]], "available"),
    "yandex unavailable"
  )
  res <- robots_evaluate_text_v1(
    robots_txt = "User-agent: *\nDisallow: /private",
    url = "https://example.test/private",
    robots_product_token = "YandexBot",
    robots_policy_ruleset = "yandex",
    matcher_backend = "yandex"
  )$results

  expect_identical(res$error_class, "robots_unsupported_product_token")
  expect_match(res$error_message, "YandexAdditionalBot", fixed = TRUE)
  expect_match(
    res$error_message, "robots_resolve_matcher_profile_v1", fixed = TRUE
  )
})
