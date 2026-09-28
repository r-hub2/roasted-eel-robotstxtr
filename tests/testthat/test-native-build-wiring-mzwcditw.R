# Load/build smoke test for the native build wiring (ROBO-mzwcditw, YI3b).
#
# YI3b wired src/Makevars(.win) to compile the nested vendored robotstxtyandex
# payload into the package shared object alongside the Google/cpp11 objects,
# WITHOUT activating any public Yandex matcher. These checks run against the
# installed/loaded package (src/ is not installed, so nothing here reads src/):
# they confirm the shared object linked and loaded, the expected cpp11 native
# routines are registered, and no Yandex native entry point leaked in.

test_that("the package shared object loaded with its native routines", {
  dll <- getLoadedDLLs()[["robotstxtr"]]
  skip_if(is.null(dll), "robotstxtr DLL not loaded (pure-R install)")

  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  names_registered <- vapply(routines, function(r) r$name, character(1))

  # The four cpp11-registered Google/collector routines must be present:
  # linking the vendored objects in must not drop or rename them.
  expect_true(all(c(
    "_robotstxtr_robotstxtr_allowed_text_",
    "_robotstxtr_robotstxtr_collect_directives_",
    "_robotstxtr_robotstxtr_matching_line_text_",
    "_robotstxtr_robotstxtr_validation_parse_"
  ) %in% names_registered))
})

test_that("no Yandex native entry point is registered (stays unavailable)", {
  dll <- getLoadedDLLs()[["robotstxtr"]]
  skip_if(is.null(dll), "robotstxtr DLL not loaded (pure-R install)")

  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  names_registered <- vapply(routines, function(r) r$name, character(1))

  # The vendored Yandex objects link in but are called by nothing yet; no
  # registered native routine may reference them.
  expect_false(any(grepl("yandex", names_registered, ignore.case = TRUE)))
})

test_that("the engine contract reports Yandex available post-activation", {
  expect_identical(
    engine_matcher_availability_v1()[["yandex"]],
    "available"
  )
})
