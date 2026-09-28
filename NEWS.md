# robotstxtr 0.3.0

* The engine contract now publishes which product tokens each
  `bounded_profiles` matcher backend accepts, and what each one does. Schema
  revision advances to `2026-08-25.1` on both `robots_engine_contract_v1()` and
  `robots_engine_contract_v2()`; both contract ids are unchanged and the
  addition is additive, so no existing field changes type or meaning.

  The accepted sets are small and not guessable from vendor documentation:

  | backend | accepted selectors |
  |---|---|
  | `yandex` | `Yandex`, `YandexAdditionalBot` |
  | `bing` | `bingbot`, `adidxbot` |

  Notably `YandexBot` — Yandex's main indexing crawler, and the value a caller
  is most likely to reach for — is **not** accepted, while `Yandex` is. That is
  not a typo in either direction: `robots_product_token` is a robots.txt
  `User-agent:` group label, not an HTTP `User-Agent` crawler identity, and
  `Yandex` is an umbrella group that no crawler ever sends. RFC 9309 treats the
  two as one namespace; vendors do not.

  Each backend's `matcher_capability` entry gains `product_token_role`,
  `product_token_comparison` (`ascii_case_insensitive_exact` for both backends)
  and `supported_profiles`, a data frame carrying `profile_id`,
  `accepted_token`, `group_label`, `group_selection` and `profile_revision`.
  `group_selection` matters: `Yandex` and `bingbot` fall back to a
  `User-agent: *` group when no group names them (`exact_else_wildcard`), while
  `YandexAdditionalBot` and `adidxbot` never do (`exact_only`), so a robots.txt
  containing only `User-agent: *` does not constrain them at all. The two
  accepted selectors on a backend are therefore not interchangeable, which is
  why the contract publishes rows rather than a bare token vector. Backends
  whose `token_policy` is `arbitrary_valid` or `rfc9309` publish none of the
  three fields: their accepted sets are not closed
  (#ROBO-qgxekgph, #ROBO-gcnlckao).

* New exported `robots_resolve_matcher_profile_v1()` checks a product token
  against a backend before anything is fetched or evaluated, returning the
  resolved profile row — including `group_selection` — or raising
  `robotstxtr_unresolvable_matcher_profile` with the accepted selectors named
  in the message. Passing an unaccepted selector to an evaluation call is still
  not an error: the affected rows come back undecided, with `url_decision`
  `NA`, which reads like a transport failure rather than a configuration
  mistake. Inspecting results is not an equivalent guard, because a row need
  never reach the matcher at all — an `allow_all` policy returns a decision for
  a nonsensical selector without consulting the profile. Unsupported-token
  `error_message` values now also name the accepted selectors and point at the
  resolver; the per-row status vocabulary is unchanged (#ROBO-nvuwpktu).

* Added `robots_engine_contract_v2()` and, through it, the Bing
  `matcher_backend` as an available end-to-end backend as of schema revision
  `2026-07-24.1`, bounded to the profiles `bingbot` and `adidxbot`; every other
  Bing-backend product token resolves to a checked `unsupported_profile`
  non-decision. The v2 schema extends the v1 status vocabulary with four Bing
  outcomes — `invalid_request_target`, `unsupported_profile`,
  `matcher_input_limit_exceeded`, and `matcher_work_limit_exceeded` — none of
  which can ever be an allow/disallow decision. Structured backend identity is
  published at `robots_engine_contract_v2()$matcher_identity$bing`. The
  `robots_engine_contract_v1()` accessor's own contract id is unchanged, and
  its schema revision was unchanged by this addition (it later advanced for the
  bounded-profile publication below). This is an independent, unofficial
  compatibility profile and does not claim production-crawler parity; it is not
  affiliated with or endorsed by Microsoft, Bing, Bingbot, or AdIdxBot
  (#ROBO-zcgprxtq, #ROBO-lpedsigv, #ROBO-xbwlsjzu, #ROBO-yhfjsbzo,
  #ROBO-qetsyvwv, #ROBO-onwulhga, #ROBO-ahilajyn).
* The SSRF guard now classifies the IPv6 unspecified and loopback addresses on
  the expanded address rather than the literal string, so every spelling of
  those 128 bits is treated alike — `::0.0.0.1` is recognized as loopback and
  `::0.0.0.0` as unspecified, matching `::1` and `::`. Previously the dotted
  forms were classified as neither special nor embedded IPv4 and reached the
  default allow. Fetches were not affected, because `rurl` canonicalizes such
  literals before the guard sees them; the guard is now correct on its own
  rather than relying on that (#ROBO-pzgzxkoj).
* The SSRF guard's IPv6 link-local and AWS cloud-metadata rules now match on
  the expanded address too, completing the change above. Because a hextet may
  be written with leading zeros, matching the literal string mis-decided both
  rules: `fd00:0ec2::254` was allowed through while the identical
  `fd00:ec2::254` was blocked, and addresses such as `fe8::` were reported as
  link-local despite lying far outside `fe80::/10`. Both are now decided by
  value — `fe80::/10` and `fd00:ec2::/32` — so every spelling agrees
  (#ROBO-pjvypcjk).
* The SSRF guard now decodes the three IPv6 transition mechanisms — 6to4
  (`2002::/16`), Teredo (`2001::/32`, whose embedded IPv4 is XOR-obfuscated),
  and ISATAP (a `*:5efe` marker under any prefix) — and classifies the IPv4
  address each one wraps. All three pack that address outside the low 32 bits,
  so the previous decoders missed them and, for example, `2002:a9fe:a9fe::`
  (6to4-wrapped `169.254.169.254`) was allowed. A wrapped *public* address
  still passes: these prefixes are globally reachable, so only what they carry
  decides the outcome (#ROBO-laydgesq).
* An IPv6 literal that cannot be expanded to exactly 8 hextets (`fe80:::1`,
  `::12345`) is now refused with the reason `malformed-address` instead of
  reaching the default allow. Such literals never survive URL parsing, so no
  reachable fetch changes; the guard simply no longer depends on `rurl`
  rejecting them first (#ROBO-udnyuuwn).
* An IPv6 literal ending in a single `:` is now refused with
  `malformed-address` rather than read as the address without it. R's
  `strsplit()` keeps a *leading* empty field but drops a *trailing* one, so the
  guard's arity check counted `1:2:3:4:5:6:7:8:` as eight groups and `::1:` as
  `::1`: the first was allowed, the second classified as loopback, and
  `::ffff:` decoded on to the IPv4-compatible `0.0.255.255`. Each landed on the
  same verdict as its well-formed spelling, so no blocked range became
  reachable — what failed was the `malformed-address` invariant above. A
  trailing `.` is deliberately still accepted, matching the WHATWG host parser
  (#ROBO-zavqklmi).
* Two SSRF reason codes are corrected. `100.64.0.0/10` — RFC 6598 Shared
  Address Space, i.e. carrier-grade NAT — was reported as `cloud-metadata`; it
  is not a metadata range, it merely contains one provider's endpoint, so a
  consumer keying off that code misattributed every CGNAT address it saw. It is
  now `shared`. And only `0.0.0.0/32` is the unspecified address (RFC 1122
  §3.2.1.3), so the rest of `0.0.0.0/8` is now `this-network` (RFC 791 §3.2)
  instead of reporting all 16 777 216 addresses as `unspecified`. **Both blocks
  remain blocked and no address changes verdict — only the reported code.**
  `169.254.169.254` keeps its deliberate `cloud-metadata` label inside the
  link-local `/16` (#ROBO-cjnsrmgd).
* A fetched `robots.txt` carrying an embedded NUL byte no longer aborts the
  call. `robots_body()`, `allowed_by_robots_url()` and
  `robots_evaluate_url_v1()` decoded acquired bytes with a bare `rawToChar()`,
  which raises `embedded nul in string`, so one stray NUL — a UTF-16-encoded
  file, a WAF challenge page, a truncated or NUL-padded response — turned a
  best-effort fetch into an error. NUL bytes are now removed before the bytes
  become an R string, the same rule `robots_validate_text()` already applied:
  the document is not truncated at the NUL and no other byte is rewritten, so
  every directive the file spells out still applies. The stored body stays
  byte-exact (`robots_body(raw = TRUE)`), and the malformation stays visible —
  `robots_validate_text()` still reports it as a `nul_byte` error diagnostic
  (#ROBO-hcqrzikz).

* The pkgdown documentation site is built and published by GitLab CI at
  <https://bart-turczynski.gitlab.io/robotstxtr/>, replacing the GitHub Pages
  site that went away with the suspended `bart-turczynski` GitHub account
  (#ROBO-qqwkldzv).

* The documentation site's address is now the GitLab Pages namespace path
  <https://bart-turczynski.gitlab.io/robotstxtr/>, the fleet-wide standard,
  instead of the project's unique Pages domain, which no longer resolves.
  `DESCRIPTION`, `CITATION.cff`, `.zenodo.json`, `codemeta.json` and
  `SECURITY-INSIGHTS.yml` all carry the new address (SEOR-hcmtspmv).

* Package metadata now points at GitLab. `URL:` is
  <https://gitlab.com/bart-turczynski/robotstxtr> plus the documentation site,
  `BugReports:` is <https://gitlab.com/bart-turczynski/robotstxtr/-/work_items>, and
  `codemeta.json`, `CITATION.cff`, `inst/CITATION`, `.zenodo.json`,
  `SECURITY-INSIGHTS.yml`, `THIRD_PARTY_NOTICES.md`, `inst/NOTICE`,
  `inst/PROVENANCE` and the README badges follow. The vulnerability-reporting
  instructions in `SECURITY-INSIGHTS.yml` now describe GitLab's confidential
  issue, which is what replaces GitHub private vulnerability reporting. Upstream
  citations of `github.com/google/robotstxt` are unchanged — that is the C++
  project this package vendors (#ROBO-bgcloodt).

* The `BugReports:` field in `DESCRIPTION` now reads
  `https://gitlab.com/bart-turczynski/robotstxtr/-/issues`, superseding the
  `/-/work_items` form the metadata entries elsewhere in this unreleased
  section record.
  `tools:::.check_package_CRAN_incoming()` accepts a gitlab.com bug tracker
  only when the path ends in `/-/issues`, and the `/-/work_items` form is what
  got a sibling package archived at the CRAN incoming pretest. GitLab answers
  `/-/issues` with 404 to a signed-out, non-browser client and redirects a
  browser to the work-items view, so the address a reader clicks still
  resolves; `cran-comments.md` explains the resulting URL note. The incoming
  check reads no file but `DESCRIPTION`, so `codemeta.json` — which is
  `.Rbuildignore`d and read only by humans — deliberately keeps the
  `/-/work_items` address, which returns 200 (ROBO-npiueuey).

* `rurl` is now required at `>= 3.0.1`, up from `>= 2.2.1`. The old floor
  named a version that was only ever a source-forge tag and never reached
  CRAN, whose `rurl` releases are 1.2.0 (archived) and 3.0.1; 3.0.1 is the
  lowest version a CRAN install can satisfy, and the full test suite passes
  against it installed from CRAN. The installation instructions no longer set
  up the R-universe staging repository, since every dependency now comes from
  CRAN (#ROBO-depmsgpz).

## Internal

* The agent instructions no longer import `FP_AGENTS.md` or `FP_CLAUDE.md`,
  the files the `fp` tracker generates, which are deleted. `AGENTS.md` points
  at the house `agent-workflow` and `fp` skills for the git workflow
  (SEOR-ipwcbcov).

* Two dependency vulnerability audit jobs, `osv-audit` and `security-audit`,
  run `tests/testthat/test-osv.R` and `tests/testthat/test-security.R` on a
  pipeline schedule that sets `SCHEDULE_KIND=dependency-audit`, or by hand
  from a web-started pipeline. `security-audit` sets
  `OSSINDEX_AUDIT_REQUIRED=true`. Both install the package first, because the
  OSV test resolves the dependency closure from installed packages and skips
  without it, and both fail if any audit test skipped (SEOR-fftbjnpl).

* `scripts/check-bugreports.py`, run on pre-push and in the
  `citation-version` CI job, keeps `DESCRIPTION`'s `BugReports:` on the
  CRAN-incoming-safe `/-/issues` form and fails if a human-facing file links
  that form instead of `/-/work_items` (SEOR-ocbtrrnl).

* The OSS Index allow-list in `tests/testthat/helper-security.R` is now
  checked by a validator, `oss_index_allowlist_violations()`, that returns
  findings and is itself tested against broken fixture rows, instead of a loop
  of expectations that asserts nothing once the list is empty. The two `curl`
  rows are unchanged and pass it. The audit test gains a loud-failure mode:
  under `OSSINDEX_AUDIT_REQUIRED=true`, missing `oysteR` or missing OSS Index
  credentials fail the test instead of skipping it, so a credentialed audit
  job cannot report success having audited nothing. No job sets the flag yet.
  An audit that resolves no packages now fails rather than passing vacuously
  (SEOR-fftbjnpl).

* `DESCRIPTION`'s `URL:` now lists the package's r-universe page. r-universe
  records this repository's upstream owner as `gitlab-bart-turczynski` because
  it is hosted on GitLab, which does not match the `bart-turczynski` universe,
  so the package was built and served but hidden from r-universe search. The
  URL claims it (SEOR-zfamoutf).

* CI now creates exactly one pipeline per merge, on `main`, instead of three
  (branch, merge-request, and main). Merge-request and feature-branch
  pipelines are suppressed via a top-level `workflow:` block; the dead
  `pkgdown-preview` MR-only job was removed along with them. Feature-branch
  pushes get no CI of their own — the local pre-push verify gate is what
  blocks bad code before it leaves the machine — and GitLab's per-line
  coverage annotations in merge-request diffs no longer appear, since no
  pipeline attaches to the MR. A pipeline started by hand from Build >
  Pipelines > Run pipeline is the exception: it works against any ref and runs
  the full gate there, so a branch can still be verified on the server before
  it merges. `pages` is pinned to `main` and is not reachable that way
  (SEOR-bmgkzhvy).

* `codemeta.json` now declares the `issueTracker` that `DESCRIPTION`'s
  `BugReports:` already did, <https://gitlab.com/bart-turczynski/robotstxtr/-/work_items>.
  It was the last tracked copy of the retired issues path, which GitLab has
  returned 404 for since issues moved to work items platform-wide; the file is
  `.Rbuildignore`d, so no shipped file changes (ROBO-zghpvlxu).

* CI installs `rurl` from its `v3.0.1` tag instead of tracking rurl's default
  branch, so the pipeline no longer tests robotstxtr against rurl's development
  head (ROBO-hzcnolhe).

* The OSS Index dependency audit in `tests/testthat/test-security.R` scopes to
  hard dependencies (`Depends` + `Imports`) instead of the `Suggests` tree, and
  allow-lists by ID the two `curl` advisories the narrower scope still reports.
  `curl` is a genuine hard dependency here, reached through `httr2`, and both
  CVE-2026-18924 and CVE-2026-3783 name libcurl ranges covering CRAN's current
  `curl` 8.0.0 — there is no version to upgrade to, so the gate had been
  blocking every push since 2026-09-06. Each allow-list row carries a written
  reason and a review date, and `helper-security.R` enforces three rules: an
  advisory that is reported and not allow-listed fails, an allow-listed
  advisory that is no longer reported fails, and drift past a review date or a
  package version warns (ROBO-oykjiyjj).

* CI folds the `lint`, `readme`, `docs`, `vendor-fidelity:yandex` and
  `vendor-fidelity:bing` verify-stage jobs into one `gates` job
  (`dev/gates.R`), cutting four runner pickups per pipeline while keeping the
  combined ~2m9s of gate compute unchanged. `citation-version` stays its own
  job, on its own `python:3.13-alpine` image, rather than folding into the R
  toolchain's image just to save one more pickup; `check`, `coverage` and
  `pages` also stay separate. The new job runs every folded gate regardless
  of an earlier one failing, then reports one summary naming every gate's
  verdict before exiting non-zero, so a red pipeline still names every broken
  gate rather than just the first one (SEOR-pgammbgo).

# robotstxtr 0.2.0

* Added `robots_validate_text()` and `robots_validate_url()` for stable,
  machine-readable document validation under an explicit
  `google-parser-compatible` profile. Reports include document byte/line and
  directive evidence, per-line syntax and group diagnostics, BOM/NUL/UTF-8
  hazards, and acquisition-limit outcomes without parsing truncated bodies
  (#ROBO-htvtajbr).

* Added the parallel `robotstxtr.engine-aware/v1` evaluation contract through
  `robots_evaluate_text_v1()` and `robots_evaluate_url_v1()`. Policy rulesets,
  matcher backends, robots product tokens, HTTP User-Agents, and acquisition
  limits are explicit, independent, and persisted in every result
  (#ROBO-unowhvjx).
* Added neutral acquisition evidence and explicit non-decision states for
  safety refusal, incomplete or partial evidence, crawler lifecycle context,
  documentation gaps, and unavailable matchers. The pinned Google matcher is
  the first available end-to-end backend; it applies its 500 KiB prefix limit
  without relabeling itself as another engine (#ROBO-unowhvjx).
* Added `robots_engine_contract_v1()` for revision/capability negotiation and
  `as_legacy_robots_decisions_v1()` as the explicit bridge to the unchanged
  Google-oriented legacy schema (#ROBO-unowhvjx).
* Added the Yandex `matcher_backend` as an available end-to-end backend as of
  schema revision `2026-07-18.2`, bounded to profile `yandex-0.1.0` and the
  crawlers `Yandex` and `YandexAdditionalBot`; every other Yandex-backend
  product token resolves to a checked `unsupported_crawler` non-decision.
  Yandex-evaluated results expose the new public `matched_rule_value_raw` list
  column (exact owning-rule bytes, `raw(0)` for an effective-empty `Disallow`,
  `NULL` for an absent rule), and structured backend identity is published at
  `robots_engine_contract_v1()$matcher_identity$yandex`. This is an
  independent, unofficial compatibility profile and does not claim
  production-crawler parity (#ROBO-sbyndsws, #ROBO-mhzmembp).
* Development installs and CI now resolve the pre-CRAN `rurl` dependency chain
  through the author's CRAN-like R-universe instead of nested GitHub remotes
  (#ROBO-yasmzyhl).

* The fetch policy now enforces a structural SSRF guard: `robots_fetch()` and
  `allowed_by_robots_url()` refuse — before opening any socket — both the
  initial origin and every redirect target that resolves to a private,
  loopback, link-local, or cloud-metadata address (including IPv6 loopback,
  IPv4-in-IPv6 embeddings, and numeric/hex/octal literal obfuscation). Such a
  URL is reported with the new `ssrf_blocked` fetch outcome and yields a
  `fetch_unknown` (`NA`) decision, never a silent allow (#ROBO-quovenef).
* The guard can be disabled per call with the new `ssrf_guard` argument
  (`TRUE` by default) on `robots_fetch()` and `allowed_by_robots_url()`, for
  deliberate use against trusted intranet hosts (#ROBO-quovenef).

# robotstxtr 0.1.0

* Initial release.
* Public API:
  * `allowed_by_robots_text()` — evaluate crawl permission against a
    `robots.txt` document supplied as text.
  * `allowed_by_robots_url()` — evaluate crawl permission for a URL, fetching
    the relevant `robots.txt` as needed.
  * `robots_fetch()` — fetch a `robots.txt` document under a deterministic,
    conservative policy.
  * `robots_body()` — preview or extract a stored `robots.txt` body.
