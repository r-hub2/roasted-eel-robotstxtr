# Internal wrappers over the hidden native robotstxtbing bindings
# (ROBO-yhfjsbzo, BI4). Not exported and not wired into the public facade; Bing
# remains capability_unavailable until the atomic activation slice (BI5). These
# give the terse-named native routines readable, self-documenting signatures
# for tests and for the later engine-aware adapter (BI5) to compose with.
#
# Each distinct body is parsed exactly once. `bodies` is a list of raw vectors
# (the DISTINCT robots bodies, bytes preserved verbatim); `body_index` is a
# 1-based integer, one per row, selecting which body that row uses;
# `product_tokens` is a character vector of product tokens, one per row;
# `targets` is a list of raw vectors carrying the exact request-target bytes,
# one per row.
#
# Returns the native per-row result columns unchanged (see the TU header for the
# full surface), plus the scalar `n_parse_calls`.
bing_evaluate_batch <- function(bodies, body_index, product_tokens, targets) {
  robotstxtr_bing_eval_batch_(
    bodies = bodies,
    ids = as.integer(body_index),
    agents = as.character(product_tokens),
    targets = targets
  )
}

# Convert one absolute HTTP(S) URL to its origin-form request target using
# Bing's own byte-preserving lexical extractor, or NA_character_ on failure
# (spec section 9). Vectorised over `url` for convenience in tests.
bing_extract_request_target <- function(url) {
  vapply(
    as.character(url),
    function(u) robotstxtr_bing_extract_request_target_(u)[[1L]],
    character(1L),
    USE.NAMES = FALSE
  )
}

# The compile-time contract identity baked into the vendored robotstxtbing
# library by the src/Makevars -D macros (spec section 5/14): the native ground
# truth the single-sourced R identity (BI5) reconciles against.
bing_native_contract_info <- function() {
  robotstxtr_bing_contract_info_()
}
