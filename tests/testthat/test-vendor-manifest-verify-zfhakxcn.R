# Tests for the fail-closed vendored-payload verifier (ROBO-zfhakxcn, YI2).
#
# These tests are driven entirely by self-consistent synthetic fixtures in a
# temp dir. They do NOT depend on the sibling robotstxtyandex checkout, on the
# network, or on any real vendored tree (none exists yet at this slice).

# The verifier relies on tools::sha256sum (R >= 4.5.0); skip cleanly otherwise.
skip_if_no_sha256 <- function() {
  skip_if_not(
    exists("sha256sum", where = asNamespace("tools"), inherits = FALSE),
    "tools::sha256sum() unavailable (needs R >= 4.5.0)"
  )
}

# Base-R temp directory, cleaned up when the calling test frame exits. Avoids a
# withr dependency (this epic minimizes dependency footprint). The literal path
# is substituted into the deferred unlink, so cleanup is independent of caller
# variable names.
local_tmpdir <- function(envir = parent.frame()) {
  tmp <- tempfile("robotstxtr-vendor-")
  dir.create(tmp)
  do.call(
    "on.exit",
    list(bquote(unlink(.(tmp), recursive = TRUE, force = TRUE)), add = TRUE),
    envir = envir
  )
  tmp
}

# Build a synthetic vendor tree plus a matching DCF manifest under `dir`.
# Returns list(root = <tree dir>, manifest = <dcf path>, files = named chr of
# relative-path -> content).
make_fixture <- function(dir, vendor_root = "vendorx") {
  root <- file.path(dir, "tree")
  contents <- c(
    "include/pkg/a.h" = "public header alpha\n",
    "src/b.cc" = "translation unit beta\n",
    "src/b.h" = "private header beta\n",
    "LICENSE" = "MIT-ish synthetic license\n"
  )
  rel_paths <- names(contents)
  for (rel in rel_paths) {
    target <- file.path(root, rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    writeLines(sub("\n$", "", contents[[rel]]), target)
  }
  shas <- vapply(
    rel_paths,
    function(rel) unname(tolower(tools::sha256sum(file.path(root, rel)))),
    character(1)
  )

  manifest <- file.path(dir, "MANIFEST.dcf")
  header <- c(
    "Manifest: synthetic",
    paste0("VendorRoot: ", vendor_root),
    ""
  )
  records <- vapply(seq_along(rel_paths), function(i) {
    paste(
      paste0("File: ", vendor_root, "/", rel_paths[i]),
      paste0("Sha256: ", shas[i]),
      "",
      sep = "\n"
    )
  }, character(1))
  writeLines(c(header, records), manifest)

  list(root = root, manifest = manifest, contents = contents)
}

test_that("matching tree passes (positive case)", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)

  res <- verify_yandex_vendor_tree(fx$root, fx$manifest)

  expect_s3_class(res, "yandex_vendor_verification")
  expect_true(res$ok)
  expect_equal(res$n_expected, length(fx$contents))
  expect_length(res$missing, 0)
  expect_length(res$extra, 0)
  expect_length(res$mismatched, 0)
  expect_length(res$matched, length(fx$contents))
})

test_that("missing file fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)
  file.remove(file.path(fx$root, "src", "b.cc"))

  res <- verify_yandex_vendor_tree(fx$root, fx$manifest)

  expect_false(res$ok)
  expect_true("src/b.cc" %in% res$missing)
  expect_length(res$extra, 0)
  expect_length(res$mismatched, 0)
})

test_that("extra / package-owned file fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)
  writeLines("package-owned binding", file.path(fx$root, "src", "glue.cpp"))

  res <- verify_yandex_vendor_tree(fx$root, fx$manifest)

  expect_false(res$ok)
  expect_true("src/glue.cpp" %in% res$extra)
  expect_length(res$missing, 0)
  expect_length(res$mismatched, 0)
})

test_that("changed / corrupted file fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)
  writeLines("tampered content", file.path(fx$root, "include", "pkg", "a.h"))

  res <- verify_yandex_vendor_tree(fx$root, fx$manifest)

  expect_false(res$ok)
  expect_true("include/pkg/a.h" %in% res$mismatched)
  expect_length(res$missing, 0)
  expect_length(res$extra, 0)
})

test_that("multiple simultaneous anomalies all report", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)
  file.remove(file.path(fx$root, "LICENSE"))
  writeLines("tampered", file.path(fx$root, "src", "b.h"))
  writeLines("stray", file.path(fx$root, "stray.txt"))

  res <- verify_yandex_vendor_tree(fx$root, fx$manifest)

  expect_false(res$ok)
  expect_true("LICENSE" %in% res$missing)
  expect_true("src/b.h" %in% res$mismatched)
  expect_true("stray.txt" %in% res$extra)
})

test_that("missing manifest fails closed with an error", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)

  expect_error(
    verify_yandex_vendor_tree(fx$root, file.path(tmp, "no-such.dcf")),
    "manifest not found"
  )
})

test_that("missing tree directory fails closed with an error", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)

  expect_error(
    verify_yandex_vendor_tree(file.path(tmp, "no-such-dir"), fx$manifest),
    "directory not found"
  )
})

# --- Manifest PARSING: every malformed shape fails closed -------------------
#
# The tests above drive tree verification against a well-formed manifest. These
# drive read_yandex_vendor_manifest() itself: a manifest the package cannot
# trust must abort, never degrade into a partial or silently-empty expectation
# set. Every fixture is written to the caller's tempdir; nothing under inst/ or
# src/vendor/ is read or written.

# Write DCF `text` (one element per line, "" separating records) to a uniquely
# named file under `dir` and return its path.
write_yandex_manifest_text <- function(dir, name, text) {
  path <- file.path(dir, name)
  writeLines(text, path)
  path
}

test_that("a manifest that is not parseable DCF fails closed", {
  tmp <- local_tmpdir()
  path <- write_yandex_manifest_text(
    tmp, "garbage.dcf", c("this is not dcf at all", "@@@")
  )
  expect_error(read_yandex_vendor_manifest(path), "malformed", fixed = TRUE)
})

test_that("a manifest missing required fields fails closed", {
  tmp <- local_tmpdir()
  path <- write_yandex_manifest_text(tmp, "no-fields.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorx", "", "Other: value"
  ))
  expect_error(
    read_yandex_vendor_manifest(path),
    "Manifest is missing required field(s): File, Sha256",
    fixed = TRUE
  )
})

test_that("a manifest without exactly one header record fails closed", {
  tmp <- local_tmpdir()
  two <- write_yandex_manifest_text(tmp, "two-headers.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorx", "",
    "Manifest: second", "VendorRoot: vendorx", "",
    "File: vendorx/a", "Sha256: aa", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(two),
    "exactly one identity/header record",
    fixed = TRUE
  )

  none <- write_yandex_manifest_text(tmp, "no-header.dcf", c(
    "File: vendorx/a", "Sha256: aa", "VendorRoot: vendorx", "",
    "File: vendorx/b", "Sha256: bb", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(none),
    "exactly one identity/header record",
    fixed = TRUE
  )
})

test_that("an empty or absent VendorRoot fails closed", {
  tmp <- local_tmpdir()
  empty <- write_yandex_manifest_text(tmp, "empty-root.dcf", c(
    "Manifest: synthetic", "VendorRoot:", "",
    "File: vendorx/a", "Sha256: aa", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(empty),
    "non-empty VendorRoot",
    fixed = TRUE
  )

  # VendorRoot declared only on a FILE record leaves the header's value NA.
  absent <- write_yandex_manifest_text(tmp, "na-root.dcf", c(
    "Manifest: synthetic", "",
    "File: vendorx/a", "Sha256: aa", "VendorRoot: vendorx", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(absent),
    "non-empty VendorRoot",
    fixed = TRUE
  )
})

test_that("an empty or absent Sha256 fails closed", {
  tmp <- local_tmpdir()
  empty <- write_yandex_manifest_text(tmp, "empty-sha.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorx", "",
    "File: vendorx/a", "Sha256:", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(empty),
    "non-empty Sha256",
    fixed = TRUE
  )

  absent <- write_yandex_manifest_text(tmp, "na-sha.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorx", "",
    "File: vendorx/a", "Sha256: aa", "",
    "File: vendorx/b", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(absent),
    "non-empty Sha256",
    fixed = TRUE
  )
})

test_that("a duplicate declared vendored path fails closed", {
  tmp <- local_tmpdir()
  path <- write_yandex_manifest_text(tmp, "dup.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorx", "",
    "File: vendorx/a", "Sha256: aa", "",
    "File: vendorx/a", "Sha256: bb", ""
  ))
  expect_error(
    read_yandex_vendor_manifest(path),
    "duplicate vendored file path",
    fixed = TRUE
  )
})

test_that("a declared file escaping VendorRoot fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_fixture(tmp)
  path <- write_yandex_manifest_text(tmp, "escapes.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorx", "",
    "File: vendorx/include/pkg/a.h", "Sha256: aa", "",
    "File: elsewhere/evil.h", "Sha256: bb", ""
  ))
  expect_error(
    verify_yandex_vendor_tree(fx$root, path),
    "Manifest file(s) outside VendorRoot 'vendorx': elsewhere/evil.h",
    fixed = TRUE
  )
  # And the helper itself keeps a well-formed set of paths intact.
  expect_identical(
    yandex_vendor_relpath(c("vendorx/a", "vendorx/d/b"), "vendorx"),
    c("a", "d/b")
  )
})

test_that("a file that cannot be hashed fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  expect_error(
    yandex_sha256_file(file.path(tmp, "no-such-file")),
    "Could not read file for hashing",
    fixed = TRUE
  )
})

test_that("shipped manifest is well-formed and self-describing", {
  skip_if_no_sha256()
  manifest <- yandex_vendor_manifest_path()
  skip_if(identical(manifest, ""), "installed manifest not found")

  parsed <- read_yandex_vendor_manifest(manifest)
  expect_equal(parsed$vendor_root, "src/vendor/robotstxtyandex")
  expect_equal(nrow(parsed$files), 21L)
  expect_true(all(nchar(parsed$files$sha256) == 64L))
  expect_equal(
    parsed$header[["PayloadCommit"]],
    "fdd60a7c3bc6825f3b3752562dc0d6ad9387a27e"
  )
  expect_equal(parsed$header[["ProfileId"]], "yandex-0.1.0")
  expect_equal(parsed$header[["LibraryVersion"]], "0.2.0")
})
