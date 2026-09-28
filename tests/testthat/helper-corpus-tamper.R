# Shared helpers for the fail-closed conformance-corpus verifier tests
# (test-bing-corpus-onwulhga.R and test-yandex-corpus-novczgii.R).
#
# The committed fixtures under inst/bing-corpus/ and inst/yandex-corpus/ are
# byte-exact and must never be touched: inst/yandex-corpus/ in particular is
# excluded from the whitespace-rewriting pre-commit hooks because its bodies
# carry intentional trailing spaces, CRLF endings and missing final newlines.
# Every negative test therefore copies the corpus into a fresh tempdir FIRST
# and tampers only with the copy.

# Copy a corpus directory (cases.json, PROVENANCE.dcf and bodies/) into a
# fresh tempdir. Returns the copy's path; the caller owns cleanup.
corpus_copy <- function(src) {
  dest <- tempfile("corpus-copy-")
  dir.create(file.path(dest, "bodies"), recursive = TRUE)
  file.copy(file.path(src, "cases.json"), file.path(dest, "cases.json"))
  file.copy(
    file.path(src, "PROVENANCE.dcf"), file.path(dest, "PROVENANCE.dcf")
  )
  for (f in list.files(file.path(src, "bodies"), full.names = TRUE)) {
    file.copy(f, file.path(dest, "bodies", basename(f)))
  }
  dest
}

# Write cases.json as exactly the given UTF-8 bytes.
corpus_write_cases <- function(dir, text) {
  writeBin(charToRaw(enc2utf8(text)), file.path(dir, "cases.json"))
  invisible(dir)
}

# A throwaway corpus directory holding nothing but the given cases.json text.
# Used for the malformed-input branches that need no bodies at all.
corpus_minimal_dir <- function(text) {
  dir <- tempfile("corpus-min-")
  dir.create(dir)
  corpus_write_cases(dir, text)
  dir
}

# Copy `src`, apply `mutate()` to the records parsed by `read()`, re-write
# cases.json with the canonical `serialize()`, and return `verify()`'s
# diagnostics for the copy. Writing the canonical bytes of the *tampered*
# records keeps the determinism check green, so the invariant under test is
# normally the only thing that can trip.
corpus_tamper_verify <- function(src, mutate, read, serialize, verify) {
  dir <- corpus_copy(src)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  corpus_write_cases(dir, serialize(mutate(read(dir))))
  verify(dir)
}

# Assert a verifier rejected, and rejected for the stated reason. A bare
# `ok == FALSE` would also pass if the corpus had been rejected for some
# unrelated reason, which defeats the point of a fail-closed matrix.
expect_corpus_rejects <- function(res, msg) {
  testthat::expect_false(res$ok)
  testthat::expect_match(
    paste(res$errors, collapse = "\n"), msg,
    fixed = TRUE
  )
}

# Re-bind `system.file()` and `file.path()` inside a *copy* of a corpus-dir
# resolver so its source-tree candidate loop runs against `root` instead of
# the real installed tree. Only the copy's enclosing environment changes; the
# package namespace is left untouched.
corpus_dir_fn_rooted <- function(f, root) {
  env <- new.env(parent = environment(f))
  env$system.file <- function(...) ""
  env$file.path <- function(...) base::file.path(root, ...)
  environment(f) <- env
  f
}
