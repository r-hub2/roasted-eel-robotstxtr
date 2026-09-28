# NUL-safe decoding of acquired robots.txt bytes (PRD 6.6 body handling).
#
# R's character type cannot carry a NUL byte. `rawToChar()` raises
# "embedded nul in string" as soon as a NUL is followed by another byte, and the
# parse collector's value channel (`Rf_mkCharLenCE()`) raises the same way. So
# every decode of ACQUIRED bytes drops NUL bytes before crossing into R's string
# world. Fetched bodies do carry NULs in the wild -- a UTF-16-encoded
# robots.txt, a WAF challenge page, a truncated or NUL-padded response -- and
# fetching is best-effort everywhere else in this package (a 404, an SSRF block
# and a transport failure all degrade), so a NUL-bearing body degrades too
# instead of aborting the call.
#
# THE RULE, one source of truth for the whole package: NUL bytes are REMOVED
# from the byte sequence; every remaining byte is decoded and matched unchanged,
# in order. The document is not truncated at the NUL and no other byte is
# rewritten. `robots_validate_text()` applies the same rule (see
# `nul_free_bytes()` there) before checking encoding validity.
#
# How this differs from the reference matcher, deliberately. Google's C++
# matcher is byte-transparent and does not stop at a NUL, so it reads
# "\0Disallow: /x" as the unknown directive "\0disallow" and IGNORES the line,
# where robotstxtr -- the NUL dropped -- reads an active "Disallow: /x". Byte
# transparency is not reachable through R's character type, and of the two
# representable options (drop the NUL, or truncate the document at it) dropping
# preserves every directive the document actually spells out; truncating would
# silently discard the rest of the file. The divergence is confined to
# NUL-bearing documents, which are malformed by any reading.
#
# The malformation stays VISIBLE rather than silent: `robots_validate_text()`
# reports a `nul_byte` diagnostic at severity "error" for every line carrying
# one (and renders it as `\x00` in the diagnostic's `raw_text`), so a caller who
# wants to reject such a document can, while matching and body previews degrade.
nul_free_bytes <- function(bytes) {
  bytes[bytes != as.raw(0)]
}

# Decode acquired bytes into the length-one string handed to the C++ matcher and
# the parse collector. The bytes are NOT validated as UTF-8: the result is
# marked UTF-8 so cpp11 forwards the exact bytes without translating or
# re-encoding them, which is how an invalid-UTF-8 body still reaches the
# byte-oriented matcher intact (PRD 6.2 bridge).
decode_matcher_body <- function(bytes) {
  out <- rawToChar(nul_free_bytes(bytes))
  Encoding(out) <- "UTF-8"
  out
}
