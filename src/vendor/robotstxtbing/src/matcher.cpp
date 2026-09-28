#include "matcher.h"

#include <cstddef>
#include <string>
#include <string_view>

namespace robotstxtbing::detail {

CompiledPattern compile_ordinary_pattern(std::string_view value) {
  CompiledPattern pattern;
  // Ordinary plain value: one literal primitive holding the value bytes verbatim.
  // No decode, no normalization, no metacharacter recognition (that is U3).
  pattern.primitives.push_back(
      PatternPrimitive{PatternPrimitiveKind::literal, std::string(value)});
  return pattern;
}

namespace {

// True for an ASCII hex digit (0-9, a-f, A-F).
constexpr bool is_hex_digit(unsigned char byte) noexcept {
  return (byte >= '0' && byte <= '9') || (byte >= 'a' && byte <= 'f') ||
         (byte >= 'A' && byte <= 'F');
}

// Upper-case an ASCII hex letter; digits and already-upper letters are returned
// unchanged. Only ever called on bytes that passed is_hex_digit.
constexpr char upper_hex(unsigned char byte) noexcept {
  if (byte >= 'a' && byte <= 'f') {
    return static_cast<char>(byte - ('a' - 'A'));
  }
  return static_cast<char>(byte);
}

// Compare `literal` against the `count`-byte target window at `start`, charging
// one work unit per byte compared (stopping at the first mismatch, exactly like
// the ordinary path). Returns:
//   +1  window matches literal in full,
//    0  a byte mismatched,
//   -1  the budget was exhausted mid-compare (caller must surface
//       work_limit_exceeded).
// The caller guarantees start + literal.size() <= target.size().
int compare_window(std::string_view target, std::size_t start,
                   const std::string& literal, WorkBudget& budget) noexcept {
  for (std::size_t i = 0; i < literal.size(); ++i) {
    if (!budget.spend(1)) {
      return -1;
    }
    if (static_cast<unsigned char>(target[start + i]) !=
        static_cast<unsigned char>(literal[i])) {
      return 0;
    }
  }
  return 1;
}

}  // namespace

CompiledPattern compile_pattern(std::string_view value) {
  CompiledPattern pattern;
  std::string literal;

  auto flush_literal = [&]() {
    if (!literal.empty()) {
      pattern.primitives.push_back(
          PatternPrimitive{PatternPrimitiveKind::literal, std::move(literal)});
      literal.clear();
    }
  };

  const std::size_t size = value.size();
  for (std::size_t i = 0; i < size; ++i) {
    const unsigned char byte = static_cast<unsigned char>(value[i]);
    if (byte == '*') {
      flush_literal();
      pattern.primitives.push_back(
          PatternPrimitive{PatternPrimitiveKind::wildcard, std::string()});
      continue;
    }
    if (byte == '$' && i + 1 == size) {
      // A `$` only anchors when it is the final byte of the value; anywhere
      // else it is a literal byte handled by the fall-through below.
      flush_literal();
      pattern.primitives.push_back(
          PatternPrimitive{PatternPrimitiveKind::end_anchor, std::string()});
      continue;
    }
    if (byte == '%' && i + 2 < size &&
        is_hex_digit(static_cast<unsigned char>(value[i + 1])) &&
        is_hex_digit(static_cast<unsigned char>(value[i + 2]))) {
      // A `%HH` triplet: keep the `%`, UPPER-CASE the two hex digits into the
      // literal. Never decoded. This single normalization reproduces BOTH
      // hex-case cells (upper-pattern and pattern-indifferent): an upper-cased
      // pattern triplet compared byte-wise to the verbatim target matches an
      // upper-hex target and not a lower-hex one, regardless of pattern case.
      literal.push_back('%');
      literal.push_back(upper_hex(static_cast<unsigned char>(value[i + 1])));
      literal.push_back(upper_hex(static_cast<unsigned char>(value[i + 2])));
      i += 2;
      continue;
    }
    // Every other byte — including `?`, `#`, a non-terminal `$`, and a lone `%`
    // not followed by two hex digits — is a verbatim literal byte.
    literal.push_back(static_cast<char>(byte));
  }
  flush_literal();
  return pattern;
}

MatchOutcome match_pattern(const CompiledPattern& pattern,
                           std::string_view target,
                           WorkBudget& budget) noexcept {
  const auto& primitives = pattern.primitives;
  const std::size_t count = primitives.size();
  std::size_t pos = 0;
  // `floating` latches once a wildcard is seen, until the next literal is placed.
  // A floating literal may match anywhere at or after `pos`; a non-floating one
  // must match exactly at `pos` (start-anchored / after a fixed segment).
  bool floating = false;

  for (std::size_t i = 0; i < count; ++i) {
    const PatternPrimitive& primitive = primitives[i];

    if (primitive.kind == PatternPrimitiveKind::wildcard) {
      floating = true;
      continue;
    }

    if (primitive.kind == PatternPrimitiveKind::end_anchor) {
      // Terminal anchor. `*$` (a wildcard still floating) absorbs the remaining
      // bytes to the end; otherwise the target must be fully consumed.
      if (floating) {
        pos = target.size();
        floating = false;
      } else if (pos != target.size()) {
        return MatchOutcome{MatchStatus::ok, false};
      }
      continue;
    }

    // Literal primitive.
    const std::string& literal = primitive.bytes;
    const bool anchor_follows =
        (i + 1 < count &&
         primitives[i + 1].kind == PatternPrimitiveKind::end_anchor);

    if (!floating) {
      // Must match exactly at `pos` (prefix semantics for this segment).
      if (pos + literal.size() > target.size()) {
        return MatchOutcome{MatchStatus::ok, false};
      }
      const int cmp = compare_window(target, pos, literal, budget);
      if (cmp < 0) {
        return MatchOutcome{MatchStatus::work_limit_exceeded, false};
      }
      if (cmp == 0) {
        return MatchOutcome{MatchStatus::ok, false};
      }
      pos += literal.size();
      continue;
    }

    // Floating literal (a `*` preceded it).
    if (anchor_follows) {
      // A `*<literal>$` tail: the literal must be a SUFFIX of the target at or
      // after `pos`. Leftmost is wrong for an end-anchored final segment.
      if (target.size() < pos + literal.size()) {
        return MatchOutcome{MatchStatus::ok, false};
      }
      const std::size_t start = target.size() - literal.size();
      const int cmp = compare_window(target, start, literal, budget);
      if (cmp < 0) {
        return MatchOutcome{MatchStatus::work_limit_exceeded, false};
      }
      if (cmp == 0) {
        return MatchOutcome{MatchStatus::ok, false};
      }
      pos = target.size();
      floating = false;
      continue;
    }

    // Leftmost occurrence of the literal at a start >= pos. Greedy, no
    // backtracking: taking the earliest match maximizes room for later
    // segments, which only need a position. Each candidate compare charges per
    // byte, so a pathological pattern trips the budget instead of hanging.
    bool placed = false;
    if (literal.empty()) {
      // Defensive: an empty floating literal matches at pos with no work.
      placed = true;
    } else {
      const std::size_t last_start =
          target.size() >= literal.size() ? target.size() - literal.size()
                                          : 0;
      for (std::size_t start = pos;
           literal.size() <= target.size() && start <= last_start; ++start) {
        const int cmp = compare_window(target, start, literal, budget);
        if (cmp < 0) {
          return MatchOutcome{MatchStatus::work_limit_exceeded, false};
        }
        if (cmp == 1) {
          pos = start + literal.size();
          placed = true;
          break;
        }
      }
    }
    if (!placed) {
      return MatchOutcome{MatchStatus::ok, false};
    }
    floating = false;
  }

  // All primitives consumed. Prefix semantics unless a terminal anchor already
  // enforced full consumption above.
  return MatchOutcome{MatchStatus::ok, true};
}

MatchOutcome match_ordinary(const CompiledPattern& pattern,
                            std::string_view target,
                            WorkBudget& budget) noexcept {
  // Single forward pass over the target. Each primitive must match the target
  // bytes starting where the previous primitive left off; the pattern matches
  // when every primitive is consumed as a prefix. NONRECURSIVE, no backtracking.
  std::size_t pos = 0;
  for (const auto& primitive : pattern.primitives) {
    // U2 emits only `literal`. A byte-wise compare of the literal against the
    // target window at `pos`.
    const std::string& bytes = primitive.bytes;
    for (std::size_t i = 0; i < bytes.size(); ++i) {
      if (pos >= target.size()) {
        // The target ran out before the pattern was consumed: the target is a
        // proper prefix of the value (implicit-suffix cell) => no match.
        return MatchOutcome{MatchStatus::ok, false};
      }
      // Charge one unit for the byte comparison we are about to perform.
      if (!budget.spend(1)) {
        return MatchOutcome{MatchStatus::work_limit_exceeded, false};
      }
      if (static_cast<unsigned char>(target[pos]) !=
          static_cast<unsigned char>(bytes[i])) {
        return MatchOutcome{MatchStatus::ok, false};
      }
      ++pos;
    }
  }
  // Every primitive matched: the pattern is a byte-wise prefix of the target.
  return MatchOutcome{MatchStatus::ok, true};
}

RuleMatch make_rule_match(RuleType type, std::string_view value,
                          std::size_t line) {
  RuleMatch match;
  match.type = type;
  match.value = std::string(value);
  match.line = line;
  return match;
}

}  // namespace robotstxtbing::detail
