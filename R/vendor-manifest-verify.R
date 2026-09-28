# Fail-closed verifier for the vendored robotstxtyandex payload tree.
#
# Introduced by ROBO-zfhakxcn (YI2). This unit ships the manifest and the
# verifier only; the production C++ payload is imported by a later unit
# (YI3a). The verifier runs fully offline: it reads a shipped DCF manifest and
# checks a given directory tree against that manifest, with no sibling checkout
# and no network access. Any discrepancy is a failure: a missing file, an
# extra or package-owned file, or a byte-level (SHA-256) change all report
# `ok = FALSE`.

# Path of the installed machine-readable manifest.
yandex_vendor_manifest_path <- function() {
  system.file(
    "vendor", "robotstxtyandex", "MANIFEST.dcf",
    package = "robotstxtr"
  )
}

# SHA-256 of a single file, lower-case hex. Uses base `tools::sha256sum`
# (R >= 4.5.0) so no external package or system call is required. Fails closed
# if the facility is unavailable rather than silently skipping the hash check.
yandex_sha256_file <- function(path) {
  if (!exists("sha256sum", where = asNamespace("tools"), inherits = FALSE)) {
    stop("tools::sha256sum() is required (R >= 4.5.0) to verify files.")
  }
  digest <- unname(tools::sha256sum(path))
  if (is.na(digest)) {
    stop(sprintf("Could not read file for hashing: %s", path))
  }
  tolower(digest)
}

# Read the DCF manifest and split it into the single identity/header record and
# the per-file records. Fails closed on a malformed manifest.
read_yandex_vendor_manifest <- function(manifest_path) {
  if (identical(manifest_path, "") || !file.exists(manifest_path)) {
    stop(sprintf("Vendor manifest not found: %s", manifest_path))
  }
  dcf <- read.dcf(manifest_path)
  cols <- colnames(dcf)
  required_cols <- c("File", "Sha256", "VendorRoot")
  missing_cols <- setdiff(required_cols, cols)
  if (length(missing_cols) > 0L) {
    stop(sprintf(
      "Manifest is missing required field(s): %s",
      toString(missing_cols)
    ))
  }
  is_file <- !is.na(dcf[, "File"])
  if (sum(!is_file) != 1L) {
    stop("Manifest must contain exactly one identity/header record.")
  }
  if (!any(is_file)) {
    stop("Manifest contains no file records.")
  }
  header <- dcf[!is_file, ]
  vendor_root <- header[["VendorRoot"]]
  if (is.na(vendor_root) || !nzchar(vendor_root)) {
    stop("Manifest header must declare a non-empty VendorRoot.")
  }
  files <- data.frame(
    file = dcf[is_file, "File"],
    sha256 = tolower(dcf[is_file, "Sha256"]),
    stringsAsFactors = FALSE
  )
  if (anyNA(files$sha256) || !all(nzchar(files$sha256))) {
    stop("Every manifest file record must declare a non-empty Sha256.")
  }
  if (anyDuplicated(files$file) > 0L) {
    stop("Manifest declares a duplicate vendored file path.")
  }
  list(header = header, vendor_root = vendor_root, files = files)
}

# Strip the "<vendor_root>/" prefix from a full vendored path, yielding a path
# relative to the vendor root. Fails closed if a declared file escapes the
# vendor root.
yandex_vendor_relpath <- function(paths, vendor_root) {
  prefix <- paste0(vendor_root, "/")
  ok <- startsWith(paths, prefix)
  if (!all(ok)) {
    stop(sprintf(
      "Manifest file(s) outside VendorRoot '%s': %s",
      vendor_root, toString(paths[!ok])
    ))
  }
  substring(paths, nchar(prefix) + 1L)
}

#' Verify a vendored robotstxtyandex tree against the shipped manifest.
#'
#' Offline, fail-closed check of a directory tree against the DCF manifest.
#' Reports a failure for any missing file, any extra or package-owned file, and
#' any file whose SHA-256 differs from the manifest. Requires no sibling
#' checkout and no network access.
#'
#' @param root Directory to check; the manifest paths, relative to VendorRoot,
#'   are resolved beneath it.
#' @param manifest_path Path to the DCF manifest. Defaults to the installed
#'   manifest.
#'
#' @return An object of class `yandex_vendor_verification`: a list with `ok`
#'   (logical) plus the `missing`, `extra`, `mismatched`, and `matched`
#'   relative paths.
#' @keywords internal
#' @noRd
verify_yandex_vendor_tree <- function(root,
                                      manifest_path =
                                        yandex_vendor_manifest_path()) {
  if (!dir.exists(root)) {
    stop(sprintf("Vendor tree directory not found: %s", root))
  }
  parsed <- read_yandex_vendor_manifest(manifest_path)
  expected_rel <- yandex_vendor_relpath(
    parsed$files$file, parsed$vendor_root
  )
  expected_sha <- parsed$files$sha256
  names(expected_sha) <- expected_rel

  actual_rel <- list.files(
    root, recursive = TRUE, all.files = TRUE, no.. = TRUE
  )

  missing <- setdiff(expected_rel, actual_rel)
  extra <- setdiff(actual_rel, expected_rel)
  common <- intersect(expected_rel, actual_rel)

  mismatched <- character(0)
  for (rel in common) {
    got <- yandex_sha256_file(file.path(root, rel))
    if (!identical(got, unname(expected_sha[[rel]]))) {
      mismatched <- c(mismatched, rel)
    }
  }
  matched <- setdiff(common, mismatched)

  ok <- length(missing) == 0L &&
    length(extra) == 0L &&
    length(mismatched) == 0L

  structure(
    list(
      ok = ok,
      root = root,
      manifest_path = manifest_path,
      n_expected = length(expected_rel),
      matched = sort(matched),
      missing = sort(missing),
      extra = sort(extra),
      mismatched = sort(mismatched)
    ),
    class = "yandex_vendor_verification"
  )
}

# --- Bing vendored payload (ROBO-lpedsigv, BI2) -----------------------------
#
# The Bing fidelity verifier is a self-contained sibling of the Yandex one
# above; the Yandex source is deliberately left untouched. Like YI2 for Yandex,
# BI2 ships the manifest and the verifier only. The production C++ payload is
# imported by a later slice (BI3): until then `src/vendor/robotstxtbing/` does
# not exist and this verifier is exercised against synthetic fixtures. It runs
# fully offline (no sibling checkout, no network) and fails closed on any
# missing, extra/package-owned, or byte-changed file.

# Path of the installed machine-readable Bing manifest.
bing_vendor_manifest_path <- function() {
  system.file(
    "vendor", "robotstxtbing", "MANIFEST.dcf",
    package = "robotstxtr"
  )
}

# SHA-256 of a single file, lower-case hex, via base `tools::sha256sum`
# (R >= 4.5.0). Fails closed if the facility is unavailable.
bing_sha256_file <- function(path) {
  if (!exists("sha256sum", where = asNamespace("tools"), inherits = FALSE)) {
    stop("tools::sha256sum() is required (R >= 4.5.0) to verify files.")
  }
  digest <- unname(tools::sha256sum(path))
  if (is.na(digest)) {
    stop(sprintf("Could not read file for hashing: %s", path))
  }
  tolower(digest)
}

# Read the Bing DCF manifest into its single header record and per-file records.
# Fails closed on a malformed manifest.
read_bing_vendor_manifest <- function(manifest_path) {
  if (identical(manifest_path, "") || !file.exists(manifest_path)) {
    stop(sprintf("Vendor manifest not found: %s", manifest_path))
  }
  dcf <- read.dcf(manifest_path)
  cols <- colnames(dcf)
  required_cols <- c("File", "Sha256", "VendorRoot")
  missing_cols <- setdiff(required_cols, cols)
  if (length(missing_cols) > 0L) {
    stop(sprintf(
      "Manifest is missing required field(s): %s",
      toString(missing_cols)
    ))
  }
  is_file <- !is.na(dcf[, "File"])
  if (sum(!is_file) != 1L) {
    stop("Manifest must contain exactly one identity/header record.")
  }
  if (!any(is_file)) {
    stop("Manifest contains no file records.")
  }
  header <- dcf[!is_file, ]
  vendor_root <- header[["VendorRoot"]]
  if (is.na(vendor_root) || !nzchar(vendor_root)) {
    stop("Manifest header must declare a non-empty VendorRoot.")
  }
  files <- data.frame(
    file = dcf[is_file, "File"],
    sha256 = tolower(dcf[is_file, "Sha256"]),
    stringsAsFactors = FALSE
  )
  if (anyNA(files$sha256) || !all(nzchar(files$sha256))) {
    stop("Every manifest file record must declare a non-empty Sha256.")
  }
  if (anyDuplicated(files$file) > 0L) {
    stop("Manifest declares a duplicate vendored file path.")
  }
  list(header = header, vendor_root = vendor_root, files = files)
}

# Strip the "<vendor_root>/" prefix; fail closed if a declared file escapes the
# vendor root.
bing_vendor_relpath <- function(paths, vendor_root) {
  prefix <- paste0(vendor_root, "/")
  ok <- startsWith(paths, prefix)
  if (!all(ok)) {
    stop(sprintf(
      "Manifest file(s) outside VendorRoot '%s': %s",
      vendor_root, toString(paths[!ok])
    ))
  }
  substring(paths, nchar(prefix) + 1L)
}

#' Verify a vendored robotstxtbing tree against the shipped manifest.
#'
#' Offline, fail-closed check of a directory tree against the Bing DCF manifest.
#' Reports a failure for any missing file, any extra or package-owned file, and
#' any file whose SHA-256 differs from the manifest. Requires no sibling
#' checkout and no network access.
#'
#' @param root Directory to check; the manifest paths, relative to VendorRoot,
#'   are resolved beneath it.
#' @param manifest_path Path to the DCF manifest. Defaults to the installed
#'   Bing manifest.
#'
#' @return An object of class `bing_vendor_verification`: a list with `ok`
#'   (logical) plus the `missing`, `extra`, `mismatched`, and `matched`
#'   relative paths.
#' @keywords internal
#' @noRd
verify_bing_vendor_tree <- function(root,
                                    manifest_path =
                                      bing_vendor_manifest_path()) {
  if (!dir.exists(root)) {
    stop(sprintf("Vendor tree directory not found: %s", root))
  }
  parsed <- read_bing_vendor_manifest(manifest_path)
  expected_rel <- bing_vendor_relpath(
    parsed$files$file, parsed$vendor_root
  )
  expected_sha <- parsed$files$sha256
  names(expected_sha) <- expected_rel

  actual_rel <- list.files(
    root, recursive = TRUE, all.files = TRUE, no.. = TRUE
  )

  missing <- setdiff(expected_rel, actual_rel)
  extra <- setdiff(actual_rel, expected_rel)
  common <- intersect(expected_rel, actual_rel)

  mismatched <- character(0)
  for (rel in common) {
    got <- bing_sha256_file(file.path(root, rel))
    if (!identical(got, unname(expected_sha[[rel]]))) {
      mismatched <- c(mismatched, rel)
    }
  }
  matched <- setdiff(common, mismatched)

  ok <- length(missing) == 0L &&
    length(extra) == 0L &&
    length(mismatched) == 0L

  structure(
    list(
      ok = ok,
      root = root,
      manifest_path = manifest_path,
      n_expected = length(expected_rel),
      matched = sort(matched),
      missing = sort(missing),
      extra = sort(extra),
      mismatched = sort(mismatched)
    ),
    class = "bing_vendor_verification"
  )
}
