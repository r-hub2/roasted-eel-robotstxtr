#ifndef ROBOTSTXTBING_SRC_DIAGNOSTIC_MAPPING_H_
#define ROBOTSTXTBING_SRC_DIAGNOSTIC_MAPPING_H_

#include <vector>

#include "parser.h"
#include "robotstxtbing/diagnostic.h"

namespace robotstxtbing::detail {

// Converts one internal DiagnosticSeed into the public Diagnostic. The code
// mapping is exhaustive, the severity comes from the spec 12 table, and the
// location is derived from the seed's ByteSpan. The detail string is owning
// project text and is not stable before 1.0.
Diagnostic make_diagnostic(const DiagnosticSeed& seed);

// Converts a seed vector, preserving physical source order with NO
// deduplication. A limit run therefore keeps earlier committed diagnostics
// ahead of the single terminal limit diagnostic.
std::vector<Diagnostic> make_diagnostics(
    const std::vector<DiagnosticSeed>& seeds);

}  // namespace robotstxtbing::detail

#endif  // ROBOTSTXTBING_SRC_DIAGNOSTIC_MAPPING_H_
