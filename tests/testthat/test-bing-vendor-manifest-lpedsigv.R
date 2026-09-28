# ROBO-lpedsigv (BI2): Bing import-manifest freeze + fail-closed verifier.
#
# Two concerns:
#  1. The verifier (verify_bing_vendor_tree) fails closed on any anomaly. Driven
#     by self-consistent synthetic fixtures in a temp dir; no sibling checkout,
#     no network, no real vendored tree (none exists until BI3).
#  2. The SHIPPED manifest (inst/vendor/robotstxtbing/MANIFEST.dcf) parses and
#     carries the exact frozen 0.1.0 identity approved in the v2 spec (PIN/D1).

skip_if_no_sha256 <- function() {
  skip_if_not(
    exists("sha256sum", where = asNamespace("tools"), inherits = FALSE),
    "tools::sha256sum() unavailable (needs R >= 4.5.0)"
  )
}

local_tmpdir <- function(envir = parent.frame()) {
  tmp <- tempfile("robotstxtr-bing-vendor-")
  dir.create(tmp)
  do.call(
    "on.exit",
    list(bquote(unlink(.(tmp), recursive = TRUE, force = TRUE)), add = TRUE),
    envir = envir
  )
  tmp
}

# Synthetic vendor tree + matching DCF manifest under `dir`.
make_bing_fixture <- function(dir, vendor_root = "vendorb") {
  root <- file.path(dir, "tree")
  contents <- c(
    "include/robotstxtbing/policy.h" = "public header\n",
    "src/parser.cpp" = "translation unit\n",
    "src/parser.h" = "private header\n",
    "LICENSE" = "Apache-2.0 synthetic\n"
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
  header <- c("Manifest: synthetic", paste0("VendorRoot: ", vendor_root), "")
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
  fx <- make_bing_fixture(tmp)
  res <- robotstxtr:::verify_bing_vendor_tree(fx$root, fx$manifest)
  expect_s3_class(res, "bing_vendor_verification")
  expect_true(res$ok)
  expect_equal(res$n_expected, length(fx$contents))
  expect_length(res$missing, 0)
  expect_length(res$extra, 0)
  expect_length(res$mismatched, 0)
})

test_that("missing / extra / changed files each fail closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_bing_fixture(tmp)

  unlink(file.path(fx$root, "src/parser.cpp"))
  writeLines("stray", file.path(fx$root, "src/glue.cpp"))
  writeLines("tampered", file.path(fx$root, "include/robotstxtbing/policy.h"))

  res <- robotstxtr:::verify_bing_vendor_tree(fx$root, fx$manifest)
  expect_false(res$ok)
  expect_true("src/parser.cpp" %in% res$missing)
  expect_true("src/glue.cpp" %in% res$extra)
  expect_true("include/robotstxtbing/policy.h" %in% res$mismatched)
})

test_that("missing manifest or tree fails closed with an error", {
  tmp <- local_tmpdir()
  fx <- make_bing_fixture(tmp)
  expect_error(
    robotstxtr:::verify_bing_vendor_tree(
      fx$root, file.path(tmp, "no-such.dcf")
    ),
    "not found"
  )
  expect_error(
    robotstxtr:::verify_bing_vendor_tree(
      file.path(tmp, "no-tree"), fx$manifest
    ),
    "not found"
  )
})

# --- Manifest PARSING: every malformed shape fails closed -------------------
#
# read_bing_vendor_manifest() is the trust boundary in front of the verifier: a
# manifest it cannot fully trust must abort rather than degrade into a partial
# expectation set. Fixtures live in the caller's tempdir; the shipped
# inst/vendor/robotstxtbing/ payload is only ever read.

write_bing_manifest_text <- function(dir, name, text) {
  path <- file.path(dir, name)
  writeLines(text, path)
  path
}

test_that("a manifest that is not parseable DCF fails closed", {
  tmp <- local_tmpdir()
  path <- write_bing_manifest_text(
    tmp, "garbage.dcf", c("this is not dcf at all", "@@@")
  )
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(path), "malformed", fixed = TRUE
  )
})

test_that("a manifest missing required fields fails closed", {
  tmp <- local_tmpdir()
  path <- write_bing_manifest_text(tmp, "no-fields.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorb", "", "Other: value"
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(path),
    "Manifest is missing required field(s): File, Sha256",
    fixed = TRUE
  )
})

test_that("a manifest without exactly one header record fails closed", {
  tmp <- local_tmpdir()
  two <- write_bing_manifest_text(tmp, "two-headers.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorb", "",
    "Manifest: second", "VendorRoot: vendorb", "",
    "File: vendorb/a", "Sha256: aa", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(two),
    "exactly one identity/header record",
    fixed = TRUE
  )

  none <- write_bing_manifest_text(tmp, "no-header.dcf", c(
    "File: vendorb/a", "Sha256: aa", "VendorRoot: vendorb", "",
    "File: vendorb/b", "Sha256: bb", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(none),
    "exactly one identity/header record",
    fixed = TRUE
  )
})

test_that("an empty or absent VendorRoot fails closed", {
  tmp <- local_tmpdir()
  empty <- write_bing_manifest_text(tmp, "empty-root.dcf", c(
    "Manifest: synthetic", "VendorRoot:", "",
    "File: vendorb/a", "Sha256: aa", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(empty),
    "non-empty VendorRoot",
    fixed = TRUE
  )

  absent <- write_bing_manifest_text(tmp, "na-root.dcf", c(
    "Manifest: synthetic", "",
    "File: vendorb/a", "Sha256: aa", "VendorRoot: vendorb", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(absent),
    "non-empty VendorRoot",
    fixed = TRUE
  )
})

test_that("an empty or absent Sha256 fails closed", {
  tmp <- local_tmpdir()
  empty <- write_bing_manifest_text(tmp, "empty-sha.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorb", "",
    "File: vendorb/a", "Sha256:", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(empty),
    "non-empty Sha256",
    fixed = TRUE
  )

  absent <- write_bing_manifest_text(tmp, "na-sha.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorb", "",
    "File: vendorb/a", "Sha256: aa", "",
    "File: vendorb/b", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(absent),
    "non-empty Sha256",
    fixed = TRUE
  )
})

test_that("a duplicate declared vendored path fails closed", {
  tmp <- local_tmpdir()
  path <- write_bing_manifest_text(tmp, "dup.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorb", "",
    "File: vendorb/a", "Sha256: aa", "",
    "File: vendorb/a", "Sha256: bb", ""
  ))
  expect_error(
    robotstxtr:::read_bing_vendor_manifest(path),
    "duplicate vendored file path",
    fixed = TRUE
  )
})

test_that("a declared file escaping VendorRoot fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  fx <- make_bing_fixture(tmp)
  path <- write_bing_manifest_text(tmp, "escapes.dcf", c(
    "Manifest: synthetic", "VendorRoot: vendorb", "",
    "File: vendorb/src/parser.cpp", "Sha256: aa", "",
    "File: elsewhere/evil.h", "Sha256: bb", ""
  ))
  expect_error(
    robotstxtr:::verify_bing_vendor_tree(fx$root, path),
    "Manifest file(s) outside VendorRoot 'vendorb': elsewhere/evil.h",
    fixed = TRUE
  )
  expect_identical(
    robotstxtr:::bing_vendor_relpath(c("vendorb/a", "vendorb/d/b"), "vendorb"),
    c("a", "d/b")
  )
})

test_that("a file that cannot be hashed fails closed", {
  skip_if_no_sha256()
  tmp <- local_tmpdir()
  expect_error(
    robotstxtr:::bing_sha256_file(file.path(tmp, "no-such-file")),
    "Could not read file for hashing",
    fixed = TRUE
  )
})

test_that("the default manifest path resolves to the installed manifest", {
  # The verifier's default argument; every other test passes a fixture, so this
  # is the only exercise of the installed-path resolver itself.
  path <- robotstxtr:::bing_vendor_manifest_path()
  expect_identical(
    path,
    system.file(
      "vendor", "robotstxtbing", "MANIFEST.dcf", package = "robotstxtr"
    )
  )
  skip_if(identical(path, ""), "installed Bing manifest absent")
  expect_true(file.exists(path))
  expect_identical(basename(path), "MANIFEST.dcf")
  expect_identical(
    robotstxtr:::read_bing_vendor_manifest(path)$vendor_root,
    "src/vendor/robotstxtbing"
  )
})

test_that("shipped manifest parses to one header + the 35-file frozen set", {
  path <- system.file(
    "vendor", "robotstxtbing", "MANIFEST.dcf", package = "robotstxtr"
  )
  skip_if(identical(path, ""), "installed Bing manifest absent")
  dcf <- read.dcf(path)
  is_file <- !is.na(dcf[, "File"])
  expect_identical(sum(!is_file), 1L)
  expect_identical(sum(is_file), 35L)
  expect_true(all(startsWith(
    dcf[is_file, "File"], "src/vendor/robotstxtbing/"
  )))
  expect_false(anyNA(dcf[is_file, "Sha256"]))
  expect_identical(anyDuplicated(dcf[is_file, "File"]), 0L)
})

test_that("shipped manifest carries the frozen 0.1.0 pin identity", {
  path <- system.file(
    "vendor", "robotstxtbing", "MANIFEST.dcf", package = "robotstxtr"
  )
  skip_if(identical(path, ""), "installed Bing manifest absent")
  h <- read.dcf(path)[1, ]

  expect_identical(h[["LibraryVersion"]], "0.1.0")
  # D1: the pinned sibling contract_id is the shipped literal, not spec prose.
  expect_identical(h[["SiblingContractId"]], "robotstxtbing-v2")
  expect_identical(h[["BingbotProfileRevision"]], "bingbot-2026-07-23.1")
  expect_identical(h[["AdidxbotProfileRevision"]], "adidxbot-2026-07-23.1")
  # Owner-approved payload pin (the annotated tag object) + its target commit.
  expect_identical(
    h[["PayloadTagObject"]], "c82855d0756c748cc4770246a19282323cdfa331"
  )
  expect_identical(
    h[["PayloadCommit"]], "1f2431b9d47ba25dec313eec5a396e795f00b5b8"
  )
  expect_identical(
    h[["AuditedCommit"]], "8fc55919bacc297f40411f83c9fe819dece69c84"
  )
  # Full digests transcribed verbatim from the shipped release audit.
  expect_identical(
    h[["ReleaseManifestSha256"]],
    "5e79ee5dcb1a22b73b5fa0f86766be529cf111fd5530d07b53fe1d6b7050a858"
  )
  expect_identical(
    h[["StaticArchiveSha256"]],
    "d1552e1f146e5155b75f8ef30e8e8cbab3e96fa46de1455b3511cbea738aeed6"
  )
  expect_identical(
    h[["SharedArchiveSha256"]],
    "91aa203f8d35f41df5783709501c379f169e7b90bb7045d4a88e887e9c56aa7f"
  )
  expect_identical(h[["License"]], "Apache-2.0")
  expect_identical(h[["BehaviorImported"]], "none")
  # The composed matcher revision embeds the pin, literal contract, both
  # profiles, and the full manifest digest (single-sourced in R at BI5).
  mr <- h[["MatcherRevision"]]
  expect_match(mr, "^robotstxtbing/0\\.1\\.0\\+payload\\.c82855d0")
  expect_match(mr, "contract=robotstxtbing-v2", fixed = TRUE)
  expect_match(
    mr,
    "manifest=5e79ee5dcb1a22b73b5fa0f86766be529cf111fd5530d07b53fe1d6b7050a858",
    fixed = TRUE
  )
})
