# ROBO-mhzmembp: matcher SEMANTICS vs. group-selection product TOKEN boundary.
#
# Dormant clarification slice: asserts the published capability boundary and its
# executable meaning (Google semantics, not crawler prediction) without changing
# any matching behavior and while keeping Yandex unavailable.

test_that("matcher_capability publishes the token/semantics boundary", {
  contract <- robots_engine_contract_v1()
  cap <- contract$matcher_capability

  expect_type(cap, "list")
  expect_setequal(names(cap), c("google", "yandex", "bing", "rfc9309"))

  expect_identical(cap$google$token_policy, "arbitrary_valid")
  expect_identical(cap$google$matcher_semantics, "google")
  expect_identical(cap$yandex$token_policy, "bounded_profiles")
  expect_identical(cap$yandex$matcher_semantics, "yandex")
  expect_identical(cap$bing$token_policy, "bounded_profiles")
  expect_identical(cap$bing$matcher_semantics, "bing")
  expect_identical(cap$rfc9309$token_policy, "rfc9309")
  expect_identical(cap$rfc9309$matcher_semantics, "rfc9309")

  # Google note is explicit that this is not a crawler prediction.
  expect_match(cap$google$note, "not.*prediction of the crawler", perl = TRUE)
  # Yandex/Bing notes are explicit that they are bounded to vendor profiles.
  expect_match(cap$yandex$note, "bounded", ignore.case = TRUE)
  expect_match(cap$bing$note, "bounded", ignore.case = TRUE)
})

test_that("boundary is executable: non-Google token yields Google semantics", {
  # A crawler name that is NOT Google-named, evaluated through the Google
  # backend, must resolve as Google matching semantics -- proving the token is
  # only a group-selection key, not a prediction of that crawler.
  x <- robots_evaluate_text_v1(
    "user-agent: *\ndisallow: /private",
    "https://example.com/private", "Yandex", "google", "google"
  )

  expect_identical(x$results$matcher_backend, "google")
  expect_identical(x$results$matcher_status, "evaluated")
  expect_identical(x$results$url_decision, "disallow")

  # The backend's owned semantics for the Google backend is Google, NOT yandex.
  cap <- robots_engine_contract_v1()$matcher_capability
  expect_identical(
    cap[[x$results$matcher_backend]]$matcher_semantics, "google"
  )
  expect_false(
    identical(cap[[x$results$matcher_backend]]$matcher_semantics, "yandex")
  )
})

test_that("Yandex is active: available, registered, schema bumped", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]], "available"
  )

  registry <- robotstxtr:::engine_matcher_registry_v1()
  expect_type(registry$yandex$callable, "closure")
  expect_identical(
    registry$yandex$revision, robotstxtr:::yandex_matcher_revision_v1()
  )

  # Google backend is untouched and the schema revision is the activation one.
  expect_identical(registry$google$availability, "available")
  expect_type(registry$google$callable, "closure")
  expect_identical(engine_schema_revision_v1(), "2026-08-25.1")
})
