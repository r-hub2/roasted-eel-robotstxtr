# Internal wrapper over the hidden native checked robotstxtyandex batch binding
# (ROBO-jbhtvilo, YI4b). Not exported and not wired into the public facade;
# Yandex remains capability_unavailable. This gives the terse-named native
# routine `robotstxtr_checked_batch_()` a readable, self-documenting signature
# for tests and for the later engine-aware adapter (YI4c) to compose with.
#
# Each distinct body is parsed exactly once. `bodies` is a list of raw vectors
# (the DISTINCT robots bodies, bytes preserved verbatim); `body_index` is a
# 1-based integer, one per row, selecting which body that row uses; `crawlers`
# is a character vector of product tokens, one per row; `targets` is a list of
# raw vectors carrying the exact request-target bytes, one per row.
#
# Returns the native per-row result columns unchanged (see the TU header for the
# full surface), plus the scalar `n_parse_calls`.
yandex_evaluate_checked_batch <- function(bodies, body_index, crawlers,
                                          targets) {
  robotstxtr_checked_batch_(
    bodies = bodies,
    ids = as.integer(body_index),
    agents = as.character(crawlers),
    targets = targets
  )
}
