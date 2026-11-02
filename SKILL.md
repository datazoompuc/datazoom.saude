---
name: manifest-maintenance
description: Maintain the datazoom.amazonia dataset manifest system — write new resolvers, verify or fix a broken source, handle a deprecated link, or finish activating the PR-only refresh pipeline. Use whenever the task touches inst/extdata/manifest/v1/datasets_link.csv, actions/scrapers/, actions/scripts/, R/manifest.R, or .github/workflows/update-manifest.yaml.
---

# Maintaining the dataset manifest

Every download URL this package uses lives in one CSV —
`inst/extdata/manifest/v1/datasets_link.csv` — fetched fresh at runtime with a
silent fallback to the packaged snapshot. A scheduled robot
(`actions/scripts/build_manifest.R`, run by `.github/workflows/update-manifest.yaml`)
proposes updates; nothing is ever auto-committed; every change becomes a pull
request for a human to review.

Read this whole file before touching any of these paths. It assumes zero
memory of how this system was built — only what it looks like today.

## What goes in NEWS.md vs. this file

NEWS.md is for the package's users: `load_<source>()`'s output changed, a bug
that affected what it returns got fixed, a dataset was added or removed. It
is not a running log of internal maintenance work -- a new resolver's
verification technique, the exact wording of a `# FRAGILE:` tag, or an
architectural decision belongs here (this file), not there. A finding that
doesn't change what the package ships right now (a known bug not yet fixed,
an evaluated-but-rejected option) belongs in this file's "Known limitations"
section and/or a GitHub Issue, never in NEWS.md as if it were a change.
(2026-10-12: several existing NEWS.md entries, including DETER's and ANEEL's,
currently carry more internal detail than this rule says they should -- a
cleanup is planned but hasn't happened yet; don't take their current
length/detail as the pattern to copy for a new entry.)

## Before opening a PR

Two mechanical checks, easy to lose track of in the middle of a real fix,
that have already bitten this project at least once each:

- **Run the devtools round-trip before opening the PR, not just before
  committing `R/` changes.** `devtools::load_all()` then
  `devtools::document()` (plus whatever else the local workflow normally
  runs, e.g. `devtools::check()`) regenerates `man/*.Rd`, `NAMESPACE`, and
  any other roxygen-derived metadata from the current state of `R/`. A new or
  renamed exported function, a changed `@param`, or a new `@importFrom` only
  reaches those generated files through this step — editing `R/*.R` alone
  leaves them silently stale, and nothing short of actually running it
  catches that before a reviewer does.
- **A new call to another package's function from `R/*.R` means that package
  must be added to `DESCRIPTION`'s `Imports:`, in the same commit.** Easy to
  lose track of when the function is pulled in as a side effect of a larger
  change rather than the point of it — e.g. `R/balanca_tls.R`'s TLS fallback
  (2026-10-21) needed `curl::new_handle()`/`curl::curl_download()`, so `curl`
  was added to `Imports:` alongside it, even though `curl` was already on
  disk as a transitive dependency of `googledrive` — being installed already
  does not make a package an `Imports:` dependency, only a `DESCRIPTION`
  entry does, and `R CMD check`'s "undeclared dependency" check is what
  actually catches a miss, not whether the package happens to load fine
  during development.
  **This is for `R/` only — a dependency used only inside `actions/`** (a
  resolver, a watcher, a build script) **stays out of `DESCRIPTION`
  entirely**, since that whole directory is `.Rbuildignore`d and never ships
  to a package user (see point 6 under "Writing a new resolver" below —
  `jsonlite`, used by some resolvers, is correctly CI-only and absent from
  `Imports:`). Don't add a package to `DESCRIPTION` by reflex just because
  its name is new in a diff — check first whether the call is in `R/` or
  `actions/`.

## The one rule everything else follows from

**Every manifest row is self-sufficient.** A row carries every value a lookup
might need for it, explicitly, even if that means repeating a value that's
identical on a sibling row. A blank cell means *this field genuinely has no
value for this row* — never "look at another row instead."

There is **no inheritance, no tiering, and — since 2026-08-19 — no base row
either**. This replaced an earlier 5-tier field-coalescing design (survey
default → dataset → geo_level/year overrides), then a self-sufficient-rows
design that kept every dataset's own blank-key "base row" alongside its real
overrides. Both migrations were chasing the same bug shape: two rows of one
dataset silently disagreeing about a value because only one of them was ever
asked. The base row turned out to be just as capable of this as the tiers it
replaced — a live example: `mapbiomas_mining`'s committed base row stayed
pinned to Collection 8 for weeks after both its real override rows had
already moved to Collection 9 (see `NEWS.md`). If you ever find yourself
writing code that reads a value from one row to decide what another row
*should* imply, stop — that is the exact pattern both migrations removed.
Every row answers for itself.

Concretely, this means:

- `dataset` is **never** `NA`. There is no more "survey-default" row. If a
  value is shared by every dataset in a source, it is copied onto every one
  of that source's rows, redundantly, on purpose.
- A `(survey, dataset)` group is either **UNKEYED** (exactly one row,
  `geo_level` and `year` both blank — that one row IS the dataset) or
  **KEYED** (every row carries a real `geo_level` XOR `year` — no blank-key
  row at all). **Never both.** There is no more "base row + overrides" —
  a keyed dataset's rows must each carry a full, independent copy of
  everything they need, not just what differs from a row that no longer
  exists.
- A dataset **never mixes** geo_level overrides and year overrides. Pick one
  axis of variation per dataset (enforced by
  `tests/testthat/test-manifest-schema.R` and
  `actions/scripts/manifest_validate.R`'s `validate_key_completeness()`).
- If a keyed dataset varies by geo_level, **every** value its rows agree on
  in `available_geo` must have a matching row, and every row that exists
  must be declared there — both directions checked (same for `year` /
  `available_time`). There is no fallback left to catch a missing one — a
  lookup for a geo_level/year with no matching row returns `NA`, full stop.

## How a lookup resolves (`R/manifest.R`)

Two different functions now, deliberately kept separate so nothing has to
guess which row "wins":

```r
dataset_field(source, dataset, field, geo_level = NULL, year = NULL)
```

is a single exact-key match, not a search:

1. If the dataset has any geo_level-specific rows, match `geo_level` exactly
   — and if the caller passed none, **`stop()`**. There is no base row left
   to fall back to.
2. Else if it has any year-specific rows, match `year` exactly — same
   `stop()` if the caller passed none.
3. Else (the dataset is unkeyed) use its one real row, ignoring whatever
   geo_level/year the caller may have passed anyway (callers pass these
   unconditionally — see `R/mapbiomas.R`'s `sheet` lookup, which passes
   `geo_level` even for unkeyed datasets like `mapbiomas_fire`).
4. Read `field` off that ONE row. Return its value, even if that value is
   `NA`.

`dataset_url()` is `dataset_field(..., "url", ...)`.

```r
dataset_meta(source, dataset, field)
```

is the **one legal cross-row read**, for a field in `MANIFEST_META_COLS`
(`sidra_code, available_time, available_geo, layer_name, resolver` — notably
**not** `url`, `docs_url`, or `version`: several MapBiomas datasets
deliberately pin one geo_level to an older Dataverse collection than its
siblings, so those two genuinely vary per row now, permanently, not as a bug
— see that constant's comment in `R/manifest.R`). Returns the single value
every row of the dataset agrees on; **`stop()`s if they disagree** — it
cannot silently pick a winner, which is exactly why it can't reproduce the
old base-row bug. If you hit that error, the field isn't safe to read
without a key for that dataset — use `dataset_field()` with a real key
instead (see `R/mapbiomas.R`'s `version` lookup for a worked example of
exactly this call getting keyed instead of migrated to `dataset_meta()`,
once real per-row divergence was found).

`effective_table()` (what `datasets_link()` exposes) synthesizes ONE display
row per `(survey, dataset)` by collapsing every value column to "the value
every row agrees on, else `NA`" — the same collapse `dataset_meta()` does,
just silently NA'd instead of thrown, since building the whole display table
can't crash over one dataset's expected divergence. A `NA` here does not
mean "no data" — it can mean "this genuinely differs by geo_level/year, ask
`dataset_field()` for a specific one" (`mapbiomas_cover`'s `url`, for
example). Override rows are only ever reachable through `dataset_field()`/
`dataset_meta()`, never through `effective_table()`/`datasets_link()`.

## The manifest's 14 columns

`survey, dataset, geo_level, year` (key) · `sidra_code, url, docs_url,
available_time, available_geo, archive_file, sheet, layer_name, version,
resolver` (value). Full description of each: see the header comment of
`R/manifest.R` and the table in the published artifact guide (ask the user
for the link if you need it — it isn't stored in the repo).

`resolver` names which `resolve_<source>.R` owns a row. Blank means no robot
ever touches it — hand-maintained permanently (e.g. Google Drive links with
no discoverable version).

## Writing a new resolver

File: `actions/scrapers/resolve_<source>.R`, function `resolve_<source>(rows)`.
Auto-discovered by `build_manifest.R` via filename convention — there is no
registry to edit.

1. **Investigate the source manually first**, in a browser, before writing
   any code. Figure out the actual mechanism: a documented API the site's own
   page calls client-side (preferred — see `resolve_prodes.R`, `resolve_aneel.R`),
   a bucket-listing API (`resolve_mapbiomas.R`'s GCS half), URL probing for a
   version stamp (`resolve_baci.R`), or, only if nothing else exists, real
   HTML scraping (`resolve_mapbiomas.R`'s WordPress half). Record what you
   verified live and when, in the file's header comment — every existing
   resolver does this; it's how the next person avoids re-investigating from
   scratch when the site changes shape again.
2. **`rows` is the manifest rows currently tagged `resolver == "<source>"`.**
   Most resolvers ignore its contents and re-discover everything from the
   network, but you MUST use it to know **which dataset/geo_level rows to
   fan a shared value across** — this is the part that's easy to get wrong.
   If one URL or version applies to every real row of a dataset (or to every
   dataset a source owns), emit **one row per key**, not just one shared
   row — and **only for keys that actually exist as rows today** (see point
   3 below; there is no base row to also write anymore):
   ```r
   # WRONG — under self-sufficient rows this leaves siblings stale
   tibble(survey = "x", dataset = NA_character_, url = found_url)

   # ALSO WRONG for a keyed dataset — geo_level = NA_character_ here would
   # try to match/create a blank-key row, which no longer exists; it would
   # bind_rows() a brand-new, duplicate row instead of updating anything
   # (this exact bug was found live in resolve_mapbiomas.R's configs before
   # the base-row-deletion migration -- see that file's header)
   tibble(survey = "x", dataset = "keyed_dataset", geo_level = NA_character_, url = found_url)

   # RIGHT — fan out across every REAL row this resolver owns
   tibble(survey = "x", dataset = unique(rows$dataset), url = found_url)
   ```
   See `resolve_prodes.R`/`resolve_seeg.R`/`resolve_ips.R` for "fan across
   every dataset a source owns," and `resolve_epe.R`/`resolve_mapbiomas.R`
   for "fan across every geo_level row of one dataset" (`resolve_epe.R`'s
   consumer/industrial-consumption block reads `rows$geo_level` directly
   rather than hardcoding a geo_level list — that pattern automatically
   adapts if a dataset's real key set ever changes, prefer it over a
   hardcoded vector like `resolve_mapbiomas.R`'s `configs` list uses).
3. **Never target `dataset = NA`, and never emit a blank-key
   (`geo_level`/`year` both `NA`) row for a dataset that has real override
   rows.** Neither row type exists anymore; the candidate will fail
   `validate_row_grouping()` for real, immediately (see
   `actions/scripts/manifest_validate.R`).
4. **Return only the columns you want to change.** `NA` in a returned column
   means "leave whatever is already there alone" — it is not "clear this
   field." (This is enforced downstream by `build_manifest.R`'s
   `non_na_cols` guard, but don't rely on it to save you from a resolver
   that `dplyr::bind_rows()`s differently-shaped sub-tibbles together and
   accidentally NA-pads a column it never meant to touch — check what your
   own `bind_rows()` call actually produces before returning it.)
5. **`stop()` on any real failure.** Never return a partial or guessed
   result. A thrown error just leaves that source's rows untouched for this
   run and gets reported by name (`resolvers_failed` in the JSON report,
   exit code 30) — it does not fail the other resolvers or corrupt anything.

   **Exception — a resolver covering several independent sub-sources.**
   `resolve_epe.R` covers 3 unrelated pages (consumo-mensal, BEN Anexo IX,
   BEN Chapter 8) in one function. A single `stop()` at the end (only when
   *all* sub-sources fail) has a real gap: one sub-source silently failing
   while the others succeed produces no error at all — no `resolvers_failed`
   entry, no PR, no issue — and the affected row(s) just go stale with zero
   visibility. Confirmed live: this is very likely how `energy_state_panel`'s
   dead URL persisted undetected for as long as it did (see NEWS.md). The
   fix, now the pattern to follow for any resolver with more than one
   sub-source: track each sub-source's own failure reason (a
   `block_failures <- character(0)` accumulator is fine), still `stop()`
   only when *every* sub-source fails, but if some succeeded and some
   didn't, return the successful rows normally (**never discard them** —
   that would reintroduce the exact "one broken thing blocks everyone else"
   bug the resolver-level exit-code-11 machinery exists to prevent) with the
   failure reasons attached as `attr(result, "partial_failures")` —
   `build_manifest.R` reads that attribute, reports it as
   `resolvers_partial_failed` in the JSON report, and folds its length into
   `total_failed` so it still trips exit code 11 (changes + a failure)
   instead of looking like a clean run. See `resolve_epe.R`'s header and
   `build_manifest.R`'s `resolver_partial_failed` handling for the working
   example — verified live by forcing one of its three blocks to fail and
   confirming the other two still merged while the failure surfaced
   (exit code 30, `resolvers_partial_failed: {"epe": "..."}`), then
   reverting the forced failure. **Also applied to `resolve_mapbiomas.R`**
   (its ~10-config loop had the identical gap one layer further down —
   `dv_get_json()` silently swallowed a transport/parse failure into an
   empty result, indistinguishable from a config that genuinely found no
   new candidate; it now throws instead, letting the failure reach the
   same per-config `tryCatch`) — same live-verified-by-forcing-a-failure
   discipline, see that file's own header for the specifics.

   **A second, successful-but-noteworthy sibling: `fallback_notes`.**
   `partial_failures` signals something that didn't resolve; a resolver can
   also succeed through a looser, still-safe path than its usual precise
   one (e.g. `resolve_deter.R`'s keyword fallback when a known
   TerraBrasilis slug disappears and a single unambiguous match is found by
   the dataset's human-readable name instead, with every already-claimed or
   ambiguous candidate excluded). That is a completely normal success, not
   a failure, and must NEVER trip `resolver_failed`/`resolver_partial_failed`
   or affect the exit code. Attach `attr(result, "fallback_notes")` (a
   character vector) instead — `build_manifest.R` reads it into
   `resolver_fallback_notes` and adds a dedicated PR-body section naming
   exactly which value(s) came from the fallback, so a human reviewer knows
   to look closer before merging. Added 2026-10-08 alongside
   `resolve_deter.R`'s fallback: before this existed, the only trace of a
   fallback firing was a `message()` that reached the CI log, never the PR
   body a human actually reads. See `resolve_deter.R`'s header and
   `tests/testthat/test-deter-resolver.R` for the worked example, including
   the guard (a link another dataset's known slug still claims is never a
   candidate here, and two datasets resolving to the same URL is always a
   hard `stop()`) that keeps the fallback from ever picking an ambiguous or
   wrongly-shared candidate.
6. **CI-only dependencies are fine here** (`curl`, `jsonlite`, `stringr`) —
   `actions/` is `.Rbuildignore`d. Never add them to `DESCRIPTION`'s Imports.
7. If the source has a genuinely newer file the package **can't read yet**
   (a different sheet layout, a different column range), do **not** push
   that URL — see "Detect-only resolvers" below.

### Verifying a candidate: the sheet name is not enough either

`resolve_mapbiomas.R`'s own header states the first half of this rule: *"a
200 response is not enough, verify the sheet is really there."* There's a
sequel worth stating just as explicitly: **a matching sheet name isn't
enough either — the sheet has to have the columns the treatment code
actually needs.** Live
incident: Dataverse's newest "Collection 10.1" cover/municipality dataset has
a sheet named exactly right (`COVERAGE_10.1`, matches `sheet_pattern =
"^COVERAGE"`) but no municipality-code column anywhere in it — a resolver
that only checked the sheet name would have silently served that file with no
way to look up a municipality. `resolve_mapbiomas.R`'s `dv_file_layout()` now
also downloads the sheet's real header row and takes an optional
`required_col_pattern`; a candidate that matches the sheet name but not the
required column is treated exactly like a sheet-name mismatch — the
newest-first walk falls through to the next-older candidate, self-healing
onto whatever collection next actually has the column, no hardcoded
version number anywhere. Give a config a `required_col_pattern` whenever the
treatment code has a real, specific column dependency on that row (not just
"some sheet with a plausible name") — see `resolve_mapbiomas.R`'s
`mapbiomas_cover`/`municipality` config for the pattern.

### Detect-only resolvers

A detect-only resolver is the pattern for "found a newer file, but the
loader doesn't match its layout yet." It emits only `version` + `docs_url`,
never `url` — say so explicitly in the header comment. Pushing a URL the
package isn't ready to read would corrupt every download until someone
fixes the loader, silently. Follow this pattern for any source in the same
situation.

`resolve_seeg.R` USED TO be this category's canonical example, on an
assumption ("the live municipal-summary workbook is a different shape" than
`R/seeg.R`'s expected `NIVEL 1`..`NIVEL 6` layout) that was never actually
checked. 2026-09-15: checked it, live — the assumption was TRUE, and worse
than the resolver's header guessed (a full schema restructuring, not a
version bump). It now emits a real `url`, re-verified every run via a
ranged GET of the candidate xlsx's own `xl/workbook.xml` and
`xl/pivotCache/pivotCacheDefinition1.xml`, and `R/seeg.R` was rewritten
sector by sector against the new schema (see NEWS.md). Same lesson as IPS
below, learned twice now: verify a "different shape" assumption live before
committing to detect-only, not after.

`resolve_ips.R` USED TO be detect-only too, on an assumption (the
Strapi-hosted workbook it finds is "a different shape" than `R/ips.R`'s
expected layout) that was never actually checked. 2026-09-14: checked it,
live — the two workbooks are a drop-in match (same sheets, row counts,
cleaned column names) — so it now emits a real `url`, re-verified every run
via a ranged
GET of the workbook's own `xl/workbook.xml` (an xlsx is a zip; see
`zip_remote_listing.R`) rather than trusted from that one-time comparison.
Lesson for the next detect-only resolver: the "different shape" assumption
is exactly the kind of thing to verify live before committing to
detect-only, not after.

### R gotchas that have already bitten this codebase once

- **`tibble()` argument shadowing**: `tibble()` evaluates arguments left to
  right, and a later argument can reference an earlier one *by column name*.
  If you write `tibble(year = NA_character_, available_time = year)` meaning
  to reference an outer variable called `year`, you'll silently get the
  column you just defined instead. Rename the outer variable
  (`release_year`, not `year`) if it shares a name with any manifest column.
- **`dplyr::bind_rows()` of differently-shaped tibbles** pads missing columns
  with `NA` on every row that lacked them. Downstream code reads that `NA` as
  "leave this alone" (see point 4 above) — which is what saves you, but only
  if you don't also explicitly set that column to something wrong elsewhere
  in the same bind.
- **A short download timeout can silently corrupt a resolver's verdict, not
  just fail loudly.** If a resolver downloads a real file to verify it (see
  the sheet/column verification above), give the download a *generous*
  timeout — `curl`'s `timeout` handle option is a whole-transfer ceiling, not
  a stall detector, and on a slow connection a large candidate genuinely
  cannot finish inside a short one. Live incident: `dv_file_layout()` used
  `timeout = 120`; a legitimate 78MB candidate timed out at 43MB and was
  silently read as "doesn't qualify" (no error raised, no failure logged),
  which cascaded the newest-first walk down to a much older, wrong file.
  Raised to `1000`, matching the timeout `R/download.R`'s own
  `external_download()` already uses for its big downloads — match that
  convention for any new resolver that downloads real files to verify, don't
  invent a tighter one.
- **A download failure must never look like "this candidate doesn't
  qualify."** The bug above has a root cause worth generalizing: any
  `tryCatch(..., error = function(e) NULL)` around a network call, if `NULL`
  also means "sheet/column didn't match," conflates "we don't know" with "we
  checked and it's not right" — and a newest-first walk will treat both the
  same, silently falling through to a worse candidate on a transient
  hiccup. If a resolver has this shape, make the network failure `stop()`
  instead of returning the same sentinel a real content mismatch returns —
  it should propagate to the per-`cfg`/per-source `tryCatch` that already
  leaves a row untouched on error (see `resolve_mapbiomas.R`'s
  `resolve_via_dataverse()` for the pattern: a genuine sheet/column mismatch
  returns `NULL` and the walk continues; a download failure `stop()`s and
  the walk aborts for that config only, leaving its row exactly as it was).
- **`manifest_url()` defaults to GitHub's `master` branch, not whatever
  branch your local checkout is on.** `devtools::load_all(".")` loads your
  local R *code*, but `load_mapbiomas()` (and every other loader) still
  fetches the *manifest* from `master` at runtime unless you also set
  `options(datazoom.amazonia.use_remote_manifest = FALSE)` — these are two
  independent things, loading local code doesn't imply reading the local
  manifest. Live confusion this caused: `load_mapbiomas(dataset =
  "mapbiomas_cover", geo_level = "municipality")` returned a stale sheet
  version because the remote fetch silently pulled `master`'s manifest
  (which had none of this branch's fixes) instead of the local, already-
  correct file. Always set both options together when testing against local
  manifest edits.
- **An assumption about an OLDER collection's shape needs live verification
  just as much as a new one.** A `# FRAGILE:` comment in `R/mapbiomas.R`
  assumed Collection 9's transition columns used a `from_level_N`/
  `to_level_N` PREFIX (distinct from Collection 10's confirmed
  `class_level_N_from`/`_to` SUFFIX) — turned out false. Live-verified by
  streaming just the header row of the real 606MB Collection 9 GCS file via
  HTTP range requests (see the technique note below) instead of trusting the
  comment: Collection 9 uses the exact same suffixed shape. The pt-language
  rename rule that assumed the prefix form had been silently matching zero
  columns since long before Collection 10 existed, not "broken by the
  migration" as the comment implied. Don't inherit an unverified assumption
  about what an old, working collection looks like just because it predates
  the thing you're actually changing.
- **Roxygen docs drift out of sync with the manifest silently — nothing
  enforces they stay matched.** `devtools::check()` doesn't catch a doc that
  merely under-states valid options. Live incident: `mapbiomas_cover`'s
  `@param geo_level` said "can only be municipality" for the entire time an
  `indigenous_land` manifest row existed (added during the Dataverse
  migration, §12 in the published migration artifact). Whenever a keyed
  dataset's set of `geo_level`/`year` override rows changes, grep the
  corresponding `load_<source>()` roxygen block for a stale claim — don't
  assume it was updated alongside the manifest just because they usually
  should move together.
- **Closures nested inside `resolve_<source>(rows)` cannot be unit-tested by
  mocking.** `dv_file_layout()`, `resolve_via_dataverse()`, and similar
  helpers are typically defined *inside* the outer resolver function, not at
  file scope — there's no accessible binding for
  `testthat::local_mocked_bindings()` (or any binding-replacement mock) to
  intercept, since the call never leaves that function's own local
  environment. Don't spend time trying; pull out only the pieces that
  genuinely need no network (see `mapbiomas_resolver_cache.R`'s
  `cache_lookup()`/`cache_upsert()`/`rules_fingerprint()` for what *is*
  extractable and worth unit-testing) and verify the network-touching parts
  by actually running them live, cold and warm, and diffing the output —
  see "Verifying any change" above and the resolver cache section below.
  (**Counter-example worth knowing**: `resolve_deter.R`'s keyword fallback
  is a plain top-level function — not a closure captured inside a bigger
  helper — specifically so `test-deter-resolver.R` can mock `curl` at the
  package-function level, via `testthat::with_mocked_bindings()`, and
  exercise the fallback offline against a real-shaped fixture. Prefer a
  flat, mockable shape like this over a nested closure whenever a
  resolver's matching logic is complex enough to deserve its own regression
  tests.)
- **`dplyr::recode()` (and anything built on it, like
  `rename_with(recode, ...)`) treats an unmatched name or value as "leave it
  exactly as it was" — never an error, never a warning.** This is now a
  confirmed-three-times bug shape in this codebase: MapBiomas Collection
  10's `state`/`state_acronym` rename map, and (2026-09-21) TWO separate
  places in ANEEL's `energy_generation` migration — the eng column
  rename map in `R/aneel.R` and the dictionary's `~variable` matching
  column in `R/dictionary.R` both silently no-op'd for columns whose real
  post-`clean_names()` name had drifted from what the map's keys assumed.
  A `recode()`/`rename_with(recode, ...)` call whose input keys were written
  against one snapshot of a source's real column names is exactly as
  fragile as a sheet name or a required column — it needs the same live
  verification discipline (point 3 under "Investigating a broken or changed
  source" above), and probably deserves its own `# FRAGILE:` tag wherever it
  appears, not just a mental note.
- **The file-level download cache and the parsed-object cache in
  `R/download.R` are two independent mechanisms with two independent
  eligibility rules — a reason to exclude a source from one does not
  automatically apply to the other.** Live incident (2026-09-21): ANEEL was
  excluded from the file-level disk cache (`file_cache_get`/`file_cache_set`,
  keyed on the raw downloaded bytes, no shared object, no mutation risk) —
  but the actual reasoning on record was about the *parsed-object* cache
  (`parsed_cache_eligible()`, a 1-slot cache of an already-`fread()`'d R
  object, excluded for `.xlsx` sources with real mutation risk under
  `raw_data = TRUE`). The two got conflated at some point, so every
  `load_aneel()` call re-downloaded a ~100MB file from scratch even
  back-to-back in the same R session, with no actual risk being guarded
  against. Before excluding a source from either cache, name which specific
  mechanism the risk applies to and check whether the same reasoning
  actually holds for the other one too — don't assume "cache" is a single
  category just because both use that word.

## Graduating a detect-only resolver: migrating the loader onto the real schema

Once a live check confirms the "different shape" assumption behind a
detect-only resolver (previous section) — the file really did restructure,
not just rename or bump a version — the loader (`R/<source>.R`) has to be
rewritten to read the new layout before the resolver can start emitting a
real `url`. Worked example: SEEG's 2026-09-15 migration (`R/seeg.R`,
`actions/scrapers/resolve_seeg.R`, `tests/testthat/test-seeg-*.R`) — a
250MB+ workbook, 5 sectors, old `NIVEL 1`..`NIVEL 6` restructured into
`Setor de emissão`/`Categoria emissora`/`Sub-categoria emissora`/`Recorte`/
`Atividade geral`.

1. **Extract cheap metadata once, cache it, never re-parse the full file
   per question.** `readxl::read_xlsx(file, range = readxl::cell_cols("A:I"))`
   (or whatever span covers just the categorical columns, not the year
   columns) reads in seconds even on a 250MB+ workbook; save the result as
   an `.rds` under your scratch directory and answer every subsequent
   question — column names, distinct values, per-sector crosstabs — against
   that cached object. Do this for BOTH the old and new file so every
   mapping claim below is checked against real rows, not guessed from field
   names that merely sound similar.
2. **Rename columns by their real header text, never by position.**
   `readxl::read_xlsx()` already returns the real header as the column name
   — use that directly (or rename via `dplyr::rename(new = \`Old Header\`)`)
   rather than `names(df) <- c("a", "b", "c", ...)` against a guessed
   column order. A single miscounted or extra phantom column silently
   shifts every downstream field over by one and produces a wrong-but-not-
   obviously-wrong answer (a filter matching 0 rows is at least loud; a
   filter matching the WRONG column's values usually isn't). Caught twice
   during the SEEG migration this way — once from an extra guessed column,
   once from assuming the old file's column order without checking it.
3. **Per sector (or whatever the natural unit of the source is), build a
   real crosstab, not an assumed mapping.** `count()` every combination of
   the new file's categorical fields for that sector, and separately for
   the old file's equivalent fields, then read both outputs side by side to
   find the real correspondence from co-occurring values. Old and new
   labels are often NOT spelled identically at the same granularity (e.g.
   `"Efluentes Liquidos"` vs `"Efluentes domésticos"` may be different
   granularity, not just an accent difference) — this step is what catches
   that.
4. **Migrate smallest-to-largest, and verify each one fully before starting
   the next.** Two passes per unit: (a) offline, against a small synthetic
   fixture that mirrors the real new-schema column layout (see any
   `tests/testthat/test-seeg-*.R`'s `*_fixture()` — full real year-column
   span, not a truncated one, or `tidyselect` range specs like `x1970:x2024`
   will error on a fixture that only has 2 columns) with
   `external_download()` mocked; (b) live, against the real file, comparing
   exact row counts and reconciling totals across every `geo_level` the
   loader supports (municipality sum == state sum == country sum, for a
   fixed year). Don't batch all units and verify once at the end — a bug
   caught in the smallest unit (fast to fix, fast to re-verify) is cheap;
   the same bug caught after 4 more units are built on the same broken
   assumption is not.
5. **Never merge two distinct source values into one output value.**
   Grep the migrated code for `case_when()` and check every `group_by()`'s
   field list against the source's real grain — a `case_when()` bucketing
   multiple new-schema values into one old-schema label, or a `group_by()`
   grouping on fewer fields than the previous step's crosstab used, is
   silent granularity loss with no error to catch it. The correct move when
   a new-schema field has no clean 1:1 old-schema equivalent is a rename
   plus a straight value-for-value translation, not a bucketing chain.
6. **When a whole dimension is confirmed gone (not just relabeled), drop it
   and say so out loud at the time, not only in a code comment.** A
   `# FRAGILE:` comment documenting exactly what was lost (which old column,
   what it used to let you split by) is necessary but not sufficient — tell
   the person you're working with directly, in the moment, so a real data
   loss can't hide behind a diff nobody reads line by line. SEEG's biggest
   loss this way: `seeg_land`'s entire biome dimension and its full
   origin-to-destination land-cover transition matrix, collapsed into four
   coarser fields — surfaced explicitly before that sector was migrated,
   not after.
7. **Check any dictionary/vocabulary the new sector shares with
   already-migrated sectors for missing entries.** A shared translation
   table (e.g. SEEG's `SEEG_EMISSION_TYPE_EN`, covering the
   `Emissão/Remoção/Bunker` field every sector uses) built while migrating
   sector 1 may not cover a value that only sector 4 or 5 actually emits —
   confirmed live: `seeg_land` was the first sector to emit a plain
   `"Remoção"` value the dictionary didn't have an entry for yet. A
   "translate every field, assert zero NA leaks" offline test (see any
   `test-seeg-*.R`) catches this immediately if the fixture happens to
   include that value — but a live run against the real file is what
   actually proves every REAL value the source emits is covered, since a
   synthetic fixture can only assert against values you already thought to
   include.

## Verifying any change: capture first, then prove zero patches

Before changing anything about **how a lookup resolves** or the **shape of
the CSV**, capture current behavior first — don't trust that a refactor is
behavior-preserving, prove it:

1. Write (or extend) a script like
   `data-raw/capture_resolution_matrix_v2.R` (the current one — its
   predecessor, `capture_resolution_matrix.R`, is kept only for provenance
   and is no longer replayed, see `test-resolution-matrix.R`'s retirement
   note): query `dataset_field()` for every `(survey, dataset, geo_level,
   year, field)` combination that has a REAL row backing it, against the
   *unmodified* code and manifest. Save the answers to an `.rds` fixture
   under `tests/testthat/fixtures/`. **Only query real keys** — a blank-key
   query against a dataset that also has real override rows is not a valid
   query anymore (see above); if your capture script includes those anyway
   (easy to do by accident if it iterates whatever rows exist rather than
   filtering for "this dataset's real key set"), your replay test needs to
   assert those specific entries now `stop()` instead of comparing values
   — see `test-resolution-matrix-v2.R`'s `KNOWN_BASE_ROW_DATASETS` handling
   for the pattern, born from exactly this mistake.
2. Make your change.
3. Assert the new code reproduces every captured real-key answer
   **exactly** — see `tests/testthat/test-resolution-matrix-v2.R`. **Zero
   `case_when` patches.** If reproducing the old answer needs a documented
   exception, that is a real regression to go fix, not a deviation to write
   a comment excusing. (The times this project needed patches — the
   *original* 5-tier migration, and later the base-row-deletion migration's
   `datasets_link()` snapshot — every patch was a place the fixture and the
   new code legitimately disagreed on purpose, enumerated explicitly rather
   than absorbed into a blanket exception; see `test-datasets_link.R`'s
   delta list for the current one.)

Also re-run `Rscript actions/scripts/build_manifest.R --dry-run`
twice in a row after any resolver or schema change: the second run must
report `Changed keys: 0`. If it doesn't, the change isn't idempotent — that
exact bug (a resolver "finding" a change that was already applied) is what
originally motivated the self-sufficient-rows migration.

## Investigating a broken or changed source

1. `Rscript actions/scripts/build_manifest.R --dry-run --check-all`
   — probes every non-exempt URL in the manifest and reports failures
   without failing the run. Start here; it tells you what's actually broken
   right now, not what you assume is broken.
2. Open the URL in a browser. Read the page's own JS/network tab before
   assuming you need to scrape HTML — most of this system's working
   resolvers turned out to be reading a documented API the site's own
   frontend already calls (see `resolve_prodes.R`'s header for the canonical
   example of finding this).
3. **A resolver finding the right URL is not the same as the package being
   able to read what's at it.** A resolver verifying a sheet name (and now a
   required column, see above) only proves the file *exists* and has the
   right shape at the header level — it says nothing about whether the
   treatment code that reads the DOWNLOADED file (`R/prodes.R`,
   `R/mapbiomas.R`, `R/epe.R`, ...) can still parse it. A source can
   restructure a file's *contents* — not just move where it lives — the same
   way it can restructure a page's HTML: new column names, a new prefix
   convention on repeated columns, two previously-distinct columns
   colliding after a rename, a sheet that used to be long now wide. Live
   incident: MapBiomas's Collection 10 migration moved every link
   correctly, but broke 4 real things inside `mapbiomas_treat()` that
   nobody had touched, 3 of them hard errors — the resolver-level
   verification had no way to catch any of them, because none of them are
   visible from a sheet name or a header row's column names alone (a
   `state`/`state_acronym` collision, a window-column prefix change, a
   position-based extraction that silently broke when a column moved, an
   exact-name rename map that silently no-op'd). **Whenever a resolver
   change is prompted by an upstream restructuring (not just a URL going
   stale), re-run the actual loader (`load_<source>()`) against the new
   file and check its output shape, not just that the resolver's dry run
   exits clean.** `R/mapbiomas.R`'s `mapbiomas_treat()` (and `R/epe.R`'s
   `epe_energy_state_panel_treat()`) tags every shape-dependent assumption
   in its own parsing code with a `# FRAGILE:` comment for exactly this
   reason — `actions/scripts/fragility_notes.R`'s `collect_fragility_notes()`
   greps the live file for these tags, once per source listed in that same
   file's `FRAGILITY_SOURCES` table, and `build_manifest.R` lists the
   affected lines, by number, in the PR body whenever that source's
   manifest row changes, so a reviewer knows what to re-check even when the
   resolver's own diff looks like "just a URL." If you add a new
   collection-sensitive assumption to a loader's treatment code, tag it the
   same way rather than leaving it implicit — and if it's a source not yet
   in `FRAGILITY_SOURCES`, add a row for it there too, or the tag won't
   surface anywhere automated.

   **A second, independent confirmation of this same lesson: ANEEL, 2026-09-21.**
   `R/aneel.R` (already in `FRAGILITY_SOURCES`) had exactly the "resolver
   found the right URL, loader silently mis-reads what's there" shape the
   MapBiomas incident above describes, on a source the resolver had never
   flagged as broken at all — found not by a resolver dry-run but by
   reviewing a migration (`energy_generation` moving off a dead GitLab xlsx
   onto ANEEL's live CKAN CSV) against this checklist:
   - **An exact-name rename map (`dplyr::rename_with(dplyr::recode, ...)`)
     silently no-op'd for ~12 keys and had no entry at all for ~15 more
     columns** — the real post-`clean_names()` column names had drifted
     from what the map's keys assumed, so those columns passed straight
     through unrenamed with zero error, in both the eng rename map AND a
     separate dictionary's `~variable` matching column (`R/dictionary.R`).
     `dplyr::recode()`'s unmatched-value/name behavior is "leave it exactly
     as it was," never an error — this is now the THIRD confirmed live
     occurrence of this exact bug shape in this codebase (MapBiomas Collection
     10's state/state_acronym rename, and see the `dplyr::recode` R gotcha
     below), not a one-off.
   - **An encoding assumption (`fread(encoding = "Latin-1")` + a
     compensating `iconv(from = "ISO-8859-1")`) was wrong, and had been
     since the source's original 2023 commit** — both files are genuinely
     UTF-8. Nobody had verified this at the byte level; see the new
     technique bullet under point 5 below for how it was actually confirmed.
   - **A resource's real content-type was assumed from stale code, not
     checked** — `energy_enterprises_distributed`'s manifest URL now points
     at a ~106MB zip, but `R/download.R` hardcoded `.csv` for every ANEEL
     dataset. The unzip step never ran; every real download crashed. Same
     "a 200 response / matching sheet name is not enough" lesson as the
     rest of this section, just one layer earlier — the download dispatch
     itself was reading the wrong branch for that dataset's real file type.
   None of these were things a resolver's URL-probing or sheet/column check
   could have caught — the URL genuinely worked, returned real data, and
   validated fine at that layer. All three only surface by actually running
   the loader end to end against a real download and inspecting the output
   values, not just the output's shape.
4. If nothing serves the same data anymore and no narrower substitute is
   obviously correct, do **not** guess. Leave the row's `url` as whatever it
   was (or blank it deliberately if it's actively wrong), point `docs_url`
   at wherever a human should start looking, and write down exactly what you
   searched and ruled out in the resolver's (or the row's) nearest comment —
   `resolve_epe.R`'s `energy_state_panel` section was the precedent for a
   long time (see "Known limitations" below for how that specific case was
   later actually resolved, 2026-09-08 — the documented dead-end is what
   made the later fix fast, not wasted effort). This isn't a permanent
   state: re-investigating the same site again later, especially after
   something else about it is already mapped, can turn up a page nobody had
   reason to check the first time.
5. **To check a real column header without downloading a multi-hundred-MB
   file**, stream just the xlsx's internal XML via HTTP range requests
   instead — `xlsx` is a zip, and both `xl/workbook.xml` (sheet names) and
   the target sheet's own XML (header row) live well before the file's
   central directory in practice. A small Python helper works for this: a
   file-like object whose `read()`/`seek()` issue `Range: bytes=start-end`
   requests instead of reading a local buffer, handed to `zipfile.ZipFile`.
   Needs `seekable()` returning `True` (an easy miss — `zipfile` checks for
   it) and a resolved `r:id` → sheet-file mapping via `xl/workbook.xml` +
   `xl/_rels/workbook.xml.rels` (sheets aren't necessarily `sheet1.xml` in
   name order). Used this session to confirm the real header of Collection
   9's 606MB transition file in a few seconds instead of a multi-minute
   full download — this is purely an investigation tool for a human/session
   peeking at real shapes; it's not what a resolver itself should do at
   runtime, since `readxl` can't parse from a partial stream the way this
   ad-hoc reader can for a plain header check.
6. **To check whether a "mojibake" string is really the wrong encoding
   assumption (not just a rendering artifact), inspect the raw bytes
   directly** — `curl -r <start>-<end> <url> | xxd` on the exact byte range
   containing the suspect character, compared against what that character's
   real UTF-8 vs. Latin-1 encoding actually looks like at the byte level
   (e.g. "á" is `0xC3 0xA1` in genuine UTF-8; Latin-1 read as UTF-8 produces
   a visibly different, recognizably wrong byte pattern). Don't trust a
   garbled string printed in a terminal or by R's `print()` to diagnose
   *which* encoding is wrong — a string looks equally garbled whether the
   source is genuinely Latin-1 and being read as UTF-8, or genuinely UTF-8
   and being read as Latin-1 (which is what actually happened with ANEEL,
   see the entry above — the source files were correct all along;
   `fread()`'s `encoding` argument was wrong, and had been since the
   source's original 2023 commit, unverified). This is the string-encoding
   sibling of the ranged xlsx-header technique just above — same "verify the
   real bytes, don't trust an assumption inherited from working code
   elsewhere in the file" posture.

## The pipeline, end to end

`actions/scripts/build_manifest.R [--dry-run] [--only src]... [--skip src]... [--check-all] [--no-watch]`

1. Reads the committed manifest AND the committed site-link inventory
   (`actions/watch/site_links.csv` — see "Site-link inventory watchers"
   below).
2. Runs every registered **resolver** (`resolve_<source>.R`) inside
   `tryCatch`; one failure never touches another source's rows.
3. Merges each resolver's output over the rows it owns. A brand-new row is
   seeded from the **agreed value across its dataset's existing sibling
   rows** (not a base row — there isn't one; see `seed_new_row()` in
   `build_manifest.R`) before the resolver's own values are laid over it. A
   column the siblings disagree on seeds `NA` instead of a possibly-wrong
   value, forcing the resolver's own value to win.
4. Runs every registered **watcher** (`watch_<source>.R`) the same way —
   `--only`/`--skip` select by source name across both registries at once,
   `--no-watch` disables this pass entirely. Unlike a resolver, a watcher
   reports the FULL current link set it saw, so its output wholesale-
   replaces that source's inventory rows rather than merging field-by-field.
5. Re-sorts, then runs the full validation gate
   (`actions/scripts/manifest_validate.R`) against both the manifest
   candidate — `validate_schema`, `validate_row_count` (±10%),
   `validate_no_deletions`, `validate_row_grouping`,
   `validate_dataset_level_agreement`, `validate_placeholders`,
   `validate_key_completeness`, and (rows that changed only) `validate_http`
   — and the inventory candidate — `validate_inventory`.
6. Writes a JSON report + the candidate manifest + candidate inventory to
   `OUT_DIR` (`RUNNER_TEMP` in CI, `tempdir()` locally). **Never writes
   either committed path**, under any condition.
7. Exit codes (each now means "from a resolver, a watcher, OR a resolver's
   own partial-sub-source failure — see point 5's exception above"): `0` no
   changes · `10` changes present, needs a PR · `11` changes present AND
   something also failed — open the PR for the real changes anyway, and
   raise an issue for the failure, instead of the failure silently blocking
   the PR (this is what fixed the exact "mapbiomas failing blocks everyone
   else's PR" incident this system used to have — see `NEWS.md`) ·
   `20` validation failed · `30` something failed and there were no changes
   to report.

`.github/workflows/update-manifest.yaml` (Mondays 05:00 UTC +
`workflow_dispatch`) runs this with no `--skip` flags (SEEG used to be
skipped here; it no longer is — see Known limitations), and on exit `10`/`11`: stages BOTH candidates onto their real paths,
smoke-tests the manifest (`devtools::load_all(); link_table()`), then opens
one PR via `peter-evans/create-pull-request` covering whichever changed —
**before** the PR opens, not after. On exit `11`/`20`/`30` it also opens or
reuses an issue, with the real resolver/watcher/validation failure messages
in the body (not just a pointer at the step log). The candidate + report are
always uploaded as a workflow artifact.

**There is no auto-commit path anymore, for anything, ever.** If you're
tempted to add one back "just for the safe cases" — don't; that's exactly
the judgment call that turned into a recurring source of bugs and was
deliberately removed.

Two Slack pings also ride alongside this flow now, purely informational —
see "Slack notifications" below for what they are and why they never affect
any of the exit-code behavior above.

## Site-link inventory watchers

A second, parallel mechanism to resolvers, for sources whose data comes from
scraping a page rather than a documented API: `actions/scripts/site_inventory.R`
(generic fetch/normalize/diff/validate helpers) +
`actions/scrapers/watch_<source>.R` (the actual per-page extraction logic,
same auto-discovery convention as `resolve_<source>.R`) maintain
`actions/watch/site_links.csv` — every download link a watched page
currently advertises, diffed run over run.

This replaced an earlier design (still visible in git history) where
`resolve_mapbiomas.R` scraped its own WordPress page and wrote a `docs_url`
signal onto one manifest row when the site looked "ahead" of what the
resolver found. That design had three structural problems a resolver-only
system can't avoid: it could only ever speak for one dataset, it couldn't
see a link *disappearing* (no candidate found was indistinguishable from
"nothing new"), and comparing collection numbers like `10.1` vs `11`
numerically assumed a labeling convention the source doesn't actually owe
anyone. An inventory fixes all three: it's a plain diff of observed facts
(link appeared / disappeared / moved), not a computed verdict, and it covers
every link on the page, not just the ones a resolver's config happens to ask
about.

Identity for a row is `(source, page, label, collection)` — `url` is a VALUE
under that identity, not part of it, so a moved link reads as one changed
row instead of a delete-plus-unrelated-add. `first_seen` is set once and
carried forward forever; there is deliberately no `last_seen` (it would
change every run and turn this into a PR nobody reads).

Writing a `watch_<source>.R` mirrors writing a `resolve_<source>.R` (same
"investigate live first, `stop()` on real failure, never guess" posture,
same `rows` parameter shape — this source's previously-committed inventory
rows, so a watcher can compare "how many links did I see last time"), with
one extra requirement: **a scrape that finds nothing, or suspiciously
fewer than last time, must never write** — either failure mode would read
as "the site deleted everything," which is almost always wrong and would be
the single worst thing this mechanism could get wrong. See
`watch_mapbiomas.R`'s total-miss guard (zero links across every extraction
strategy) and shrink guard (>50% drop in one run) for the pattern; both
`stop()` rather than write a suspicious snapshot, and — being watcher
failures — feed the same `11`/`30` exit-code path a resolver failure would.

## Resolver verification caching

Verifying a Dataverse-style candidate means downloading the whole file —
there's no metadata API that exposes a workbook's sheets or columns, only
its checksum and size. That download is unavoidable the first time a given
file is checked. What isn't necessary is paying for it again every single
scheduled run for a file whose verdict is already known — most weeks nothing
upstream has changed, so a resolver that always re-derives from scratch is
mostly re-confirming the same answer at real bandwidth cost.

`actions/scripts/mapbiomas_resolver_cache.R` (a committed CSV,
`actions/cache/mapbiomas_resolver_cache.csv`) is the pattern for this: it
records, per Dataverse file actually inspected, whether it passed and which
sheet matched — keyed on `(dataset, geo_level, file_id)`, but only ever
*trusted* when two more things also match:

- **The file's own checksum** (Dataverse exposes this on the same
  `dv_dataset_files()` JSON call a resolver already makes to list a
  dataset's files — no extra request). If the source ever replaces a file's
  content under the same id, the checksum changes and the cached verdict is
  no longer trusted.
- **A fingerprint of the code's own matching rules**
  (`sheet_pattern`/`required_col_pattern`, hashed together). If a maintainer
  tightens either pattern later, every verdict computed under the old,
  looser rule is invalidated automatically — nobody has to remember to
  clear the cache by hand.

**Why both, not just the checksum:** a cache keyed on content alone would
happily keep serving a "pass" verdict computed under rules that no longer
apply, the moment someone edits `required_col_pattern` — exactly the kind of
silent staleness this whole manifest system exists to prevent elsewhere (see
"The one rule everything else follows from" at the top). Trusting a verdict
requires both the *thing being judged* and the *rule doing the judging* to
be provably unchanged since it was recorded.

Wiring it into a resolver: check the cache *before* the download, for every
candidate the newest-first walk considers — a cached "pass" builds the
result row directly (no download); a cached "fail" `next`s to the older
candidate exactly like a real check would; a miss (new file, changed
checksum, or changed fingerprint) falls through to a real check, which then
records its own verdict for next time. Cache updates are written to a
candidate path and folded into the same PR-diff/exit-code machinery the
manifest and site-link inventory already use — nothing about "the cache
changed" is ever auto-committed either, matching every other rule in this
file (see `build_manifest.R`'s `cache_diff_n` and `mapbiomas_cache_changed`
side-channel). A run whose *only* change is a fresh cache entry still opens
a small PR — a deliberate tradeoff (see that code's own comment) rather than
carving out a silent-auto-commit exception for "just this one safe case."

**Seeding and verifying:** same posture as the site-link inventory — the
first run after adding caching is necessarily cold (no committed cache to
read yet, full download cost), its candidate gets hand-reviewed and
committed as the seed, and every run after that should be warm. Prove the
speedup is real, not just that the code compiles: run once cold, seed, run
again warm, and diff the *manifest* output (not just check timing) — a cache
hit has to resolve to exactly the row a full download-and-check would have
produced. Live numbers from this pattern's first build: **38 seconds warm
against 2+ hours cold** for ~10 MapBiomas configs. Also worth proving
deliberately: temporarily set a config's `required_col_pattern` to something
no real file will ever satisfy, run it, and confirm the walk correctly
exhausts every real candidate and reports nothing (not a crash, not a wrong
value) rather than trusting this in theory — see `R/mapbiomas.R`'s treatment
code fix and `NEWS.md` for the write-up of doing exactly this.

## Slack notifications (informational side-channel, never gates anything)

Three Slack pings layered on top of the PR/issue mechanism above — they never
change what fires, when, or whether a PR/issue opens. **All three stay
silent on any failure**; failures always go through the issue path only,
exactly as before this existed.

- **Watcher-changed**: a watcher succeeds and finds a real inventory diff —
  fires *alongside* the PR that diff already opens, never instead of it.
  Also gated on validation having passed (`length(errors) == 0`), added
  after a live run caught the gap: a watcher can succeed and produce rows
  that `validate_inventory()` then rejects (see the SVG-markup live incident
  below), and without this gate the ping would fire for a diff that never
  actually became reviewable.
- **New-Dataverse-file-rejected**: the resolver checksums a Dataverse file
  it's never inspected before and the verdict is `fail`. Deliberately
  silent for a re-confirmed known fail (anti-noise, same reasoning as the
  inventory's `first_seen`) and for a new file that *passes* (that already
  opens a real manifest PR — a second ping would be redundant). **Note this
  does NOT mean "no PR opens" for that run** — recording the rejection still
  writes a new row into the resolver cache, which still trips the routine
  cache-only PR the caching mechanism already opens on any new entry (see
  "Resolver verification caching" above); only the manifest CSV itself stays
  untouched. Get this distinction right in any message text — an earlier
  draft of this ping's copy claimed "no PR was opened," which was false.
- **PRODES/EPE new candidate failed content verification** (2026-09-08): the
  same idea as the MapBiomas ping above, generalized instead of copied a
  second time — `actions/scripts/resolver_alert_cache.R`'s
  `new_resolver_alerts()`, a `source`-column-aware sibling of
  `new_failed_files()`. Fires when `resolve_prodes.R`'s `.qml` legend
  cross-check or `resolve_epe.R`'s per-block column checks reject a
  candidate they've never recorded before (or one whose *previous* recorded
  verdict wasn't already `fail` — a pass-to-fail flip on the SAME key, e.g.
  the same PRODES stamp suddenly failing, is a genuine regression and alerts
  too, not just a brand-new key; found and fixed while verifying this
  feature, since keying "is this new" on identity alone — matching
  `new_failed_files()`'s file-checksum model too literally — silently missed
  it). Silent for a repeat of the exact same known fail, same anti-noise
  reasoning as the MapBiomas ping. Committed cache:
  `actions/cache/resolver_alerts.csv`, shared by both sources (no per-source
  file) so a future source can plug in as a `FRAGILITY_SOURCES`-style
  addition, not a new copy of the mechanism.

**Mechanism**: message bodies are composed in R (`build_manifest.R`) with
`jsonlite::toJSON()` and handed to the workflow as ONE already-escaped line
via `GITHUB_OUTPUT`, never re-parsed in bash. This is the deliberate fix for
a real bug in the sibling `datazoom_viz` repo's own Slack workflow: it builds
a multi-line value with a bash heredoc containing real newlines, then
interpolates it straight into a `payload: |` JSON string literal — breaks
the instant there are 2+ items. Compose in R, escape with `jsonlite`, ship
one line — don't replicate the heredoc pattern for any future Slack message
here.

**Resolver instrumentation is purely additive.** `dv_file_layout()` now
records *why* it rejected a candidate via an enclosing `layout_reason`
binding (`<<-` from inside the closure), reset at the top of every call —
but every `return(NULL)`/`next`/`return()` decision in the walk is
byte-identical to before this existed. If you're touching this code and a
change affects which candidate wins, that's a different, much bigger change
than what this feature is — verify with the same "capture first, then prove
zero diff" discipline as any other resolver change.

**Cache schema gained a `reason` column.** `read_resolver_cache()` is now
schema-tolerant — it fills any column the current code expects but an older
committed CSV lacks with `NA` — so the committed cache never needs another
manual migration when a future column gets added. Keep this permanently, not
as a one-time shim.

**Prerequisite**: a `SLACK_WEBHOOK_URL` repo (or org) secret. The workflow
steps are `continue-on-error: true`, so a missing/revoked secret can never
turn a healthy run red — the pipeline's actual pass/fail is driven only by
`build_manifest.R`'s exit code, Slack delivery is never load-bearing.

## Known limitations (don't re-discover these from scratch)

- **COMEX onboarding (2026-10-17/18, branch `update-comex`, not yet
  merged) found `load_br_trade()` is broken in production right now, not
  just missing a resolver.** The 4 manifest rows (`export_mun`,
  `export_prod`, `import_mun`, `import_prod`) had no resolver and a stale
  `available_time` (`1997-2023`); the real range, confirmed live across
  all 4, is `1997-2026` (the 2026 file is partial, rewritten monthly). A
  `ncmv2/` folder mentioned in `R/comex.R`'s own comments (for
  `import_prod`) does NOT exist for any year 1995-2027, in either
  direction — a dead end, now documented in a comment so nobody
  re-investigates it. **The real finding: `balanca.economia.gov.br`'s TLS
  certificate expired 2026-10-03** — `load_br_trade()` fails with an SSL
  error for every user (confirmed live on Windows, not just a CI/Linux
  problem). The live replacement is `balanca.mdic.gov.br` (same files,
  confirmed by size and `Last-Modified`), which is what the resolver and
  the manifest now point to — but it has its own problems: (a) its
  certificate chain is missing an intermediate, which Windows silently
  completes but OpenSSL/Linux (GitHub Actions) does not — a CI workflow
  step adding the missing intermediate to the trust store was added
  alongside the resolver so this doesn't become a second weekly-noise
  case like SEEG; (b) **its own certificate expires 2026-10-22** — outside
  this project's control, worth rechecking if COMEX starts failing again
  after that date. Separately: this server returns HTTP 200 for
  literally everything, including a nonexistent year or the dead
  `ncmv2/` path — a soft 404 (a ~1420-byte Joomla error page, `text/plain`)
  rather than a real one — so `resolve_comex.R` checks the real
  `Content-Type` (`text/csv`) via a single ranged GET per year, never the
  bare HTTP status. `R/comex.R`'s own column assumptions (`co_ano`,
  `co_mes`, `co_pais`, `sg_uf_mun`, `co_mun`, `sh4`/`co_ncm`,
  `kg_liquido`, `vl_fob`, plus the NCM-only `sg_uf_ncm`, `co_via`,
  `co_unid`, `co_urf`, `qt_estat`) were verified against real samples from
  1997, 2025 and 2026 — all still match. **Also fixed in the same
  change**: `R/comex.R`'s own hardcoded dictionary URLs (`NCM.csv`,
  `NCM_SH.csv`) were still pointing at the dead host too — without this,
  the resolver fix alone would NOT have actually restored
  `load_br_trade()` for users (the main file downloads, but the
  dictionary join then fails). **Found but deliberately NOT fixed here
  (separate task, documented as an Issue)**: the manifest's `baci_dic`
  row points at the exact same dead host and file, with `resolver` blank
  — likely breaks `load_baci()` the same way (confirmed-equivalent on
  Windows, not yet confirmed on Linux).
- **SEEG's resolver fails from GitHub Actions specifically** (HTTP 403)
  but works from a normal connection. This is unrelated to SEEG's
  detect-only status (below, now resolved) — it's the live HTTPS download
  itself getting blocked, not a schema problem.
  **The `--skip seeg` workaround no longer exists** — it was removed from
  `.github/workflows/update-manifest.yaml` in commit `cfe7d6f`
  ("Re-enabled"). As of 2026-10-17, SEEG therefore runs every Monday in
  CI, fails, and posts a "Still failing" comment on Issue #344 each time.
  **2026-10-17 diagnostics (branch `fix-seeg-resolver-diagnostics`, not
  yet merged):** SEEG's edge is Amazon CloudFront, not Cloudflare as
  earlier guessed — a live 403 (no browser `User-Agent`) returns
  `server: CloudFront`, `x-cache: Error from cloudfront`, and AWS's
  standard "ERROR: The request could not be satisfied ... Request
  blocked" page, which AWS's own docs describe as what CloudFront serves
  when an AWS WAF rule blocks the request (not confirmed which rule — an
  IP/hosting-range rule and a bot-detection rule render the identical
  page, so this is consistent with the IP/ASN-block hypothesis but
  doesn't prove it). Confirmed NOT a missing-header issue: the same
  request succeeds from a non-CI connection with the identical headers.
  `resolve_seeg.R` now builds a detailed diagnostic into its `stop()`
  message on failure (status, URL, the handful of useful headers —
  `server`, `cf-*`/`x-*`, content-type/length, `retry-after`, `location`,
  `via` — cookie *names* only, the HTML `<title>`, and up to 600 chars of
  body, about 1.3 KB total; curl's own error text is preserved when there
  is no HTTP response at all, e.g. a timeout or TLS reset — the original,
  lost 2026-10-06 patch didn't handle that case). `update-manifest.yaml`
  was also fixed alongside it: the GitHub-issue-body Markdown rendering
  step used to mangle a multi-line diagnostic message with embedded HTML
  (evidence lines merged into one paragraph, the `<title>` text got
  interpreted as real HTML and stripped by GitHub) — now the detail goes
  into a properly-fenced code block (fence length chosen longer than any
  backtick run inside the body, so it can't be closed early); single-line
  messages render byte-identical to before. Next step: merge this branch
  to `master` so the next scheduled Monday 05:00 UTC run posts real
  diagnostics on #344 instead of the old bare failure — not done yet,
  pending review.
- **SEEG graduated out of the detect-only category 2026-09-15** (see
  above) — same as IPS's 2026-09-14 graduation. `R/seeg.R`'s five sectors
  were all rewritten against the real current schema and verified live.
- **`epe/energy_state_panel` HAD no working source — fixed 2026-09-08.**
  (Historical note, kept because it's the canonical worked example of the
  "investigate live, find nothing, leave `docs_url` pointing at where to
  start" pattern below — see point 4 of "Investigating a broken or changed
  source.") The old Anuário chapter workbook 404'd with no replacement
  found at the time. A later re-investigation found the same BEN dashboard
  book `national_energy_balance` already reads has its own Chapter 8 page
  serving a working consolidated table — now resolved and automated (see
  `resolve_epe.R`'s header and NEWS.md). Sanity-checked against the real
  OLD source too (live via a git worktree at the pre-migration commit): 585
  of 585 matched `(year, fonte)` cells for `national_energy_balance` came
  back an exact 0% delta — the only apparent mismatch turned out to be a
  one-year (2004) label inconsistency in the old hand-maintained workbook,
  not a data problem. Point worth generalizing: a dead link is not
  necessarily permanent — re-investigating the SAME site later, after
  something else about it has already been mapped (here, the BEN
  dashboard's URL structure from `national_energy_balance`'s own fix), can
  turn up a page nobody had reason to check before.
- **Google Drive-hosted sources can't be automated** (IEMA, Imazon) —
  opaque Drive IDs, no discoverable version. SEEG's and IPS's *actual*
  files graduated off Drive links entirely (SEEG: a direct `seeg.eco.br`
  HTTPS URL; IPS: `painel.ipsamazonia.org.br`'s Strapi-hosted workbook) —
  this limitation no longer applies to either. SEEG's separate
  per-gas-detail CSVs (`gases.csv`/`ar2..6.csv`, not currently read by
  `R/seeg.R` at all) remain Drive-hosted and still can't be automated.
- **PRODES's raster legend codes are hardcoded** in `R/prodes.R` — INPE's
  own encoding, not a URL token. `resolve_prodes.R` now cross-checks 5 of
  the 6 codes against the live `.qml` sidecar automatically on every run
  (2026-09-08, via `actions/scripts/zip_remote_listing.R` — a ranged-GET
  zip reader that recovers a small entry's real content without downloading
  the whole archive), `stop()`ing if a label no longer matches. `clouds = 99`
  is the one exception: no entry currently ships for it at all, so that
  specific code stays a documented, informational gap rather than a check
  that can ever pass — if PRODES ever adds it back, this becomes fully
  covered too. **2026-10-20**: the codes live in ONE function,
  `prodes_raster_code()` in `R/prodes.R`; the resolver keeps its own copy at
  file scope (it runs without the package) and
  `tests/testthat/test-prodes-resolver-legend.R` fails if the two disagree —
  change both together. A zip without a `.qml` now fails the resolver (it
  used to skip the check with a message), every `d<year>`/`r<year>` entry is
  checked, and `load_prodes()` itself warns (`prodes_check_qml()`) when the
  `.qml` in the user's own download disagrees. The codes stay hardcoded on
  purpose rather than read from the `.qml` at load time — see the comment
  above `prodes_raster_code()` for why.
- **`zip_remote_listing.R` must never pass `useragent = NULL` to
  `curl::new_handle()`** — on curl 7.1.0 that sends no User-Agent header at
  all (verified 2026-10-20 against an echo server), and TerraBrasilis answers
  404 to such requests. `zip_remote_handle()` sets the option only when a
  real value is given; use it for any new handle in that file.
- **The `# FRAGILE:` tag collector is generic** (`actions/scripts/fragility_notes.R`,
  `collect_fragility_notes(r_path, label)`) — it used to be hardcoded to
  `R/mapbiomas.R` only (`mapbiomas_fragility.R`, `sprintf("R/mapbiomas.R:%d", ...)`
  baked into the format string), which meant tagging a shape-dependent
  assumption anywhere else got no automated PR-body surface. `R/epe.R`'s
  `fonte_map`, its `" Do "/" De "` capitalization fix, and its
  `grupo != "Brasil"` filter (see `epe_energy_state_panel_treat()`) are now
  tagged and picked up the same way `R/mapbiomas.R`'s tags are. Adding a new
  source is a row in `FRAGILITY_SOURCES` (same file) — `changed_prefix`,
  `r_path`, `label`, `treat_fn` — not a new wrapper file or a new block in
  `build_manifest.R`. A shape-dependent assumption in an R/*.R file that
  hasn't adopted the tagging convention at all (or isn't listed in
  `FRAGILITY_SOURCES`) is still only as durable as the comment next to it.
- **`www.obt.inpe.br` (DEGRAD) supports neither Range nor a useful HEAD** —
  confirmed live (2026-10-06). A ranged GET returns `Accept-Ranges: none`
  and the full 200 response body instead of a 206, so `zip_remote_entries()`
  (the ranged-zip-listing technique `resolve_prodes.R`/`resolve_baci.R` rely
  on) can't be used here without downloading the whole zip first. The
  obvious fallback, a plain HEAD request, is *also* broken on this host: it
  returns `HTTP 200` with `Content-Length: 0` and `Content-Type: text/plain`
  for all 10 DEGRAD-year URLs, so it can't even confirm a file's size.
  `resolve_degrad.R`'s `degrad_peek_headers()` works around both: it opens a
  real GET as a streaming `curl` connection, reads only the response
  headers, then closes the connection without ever consuming the body —
  giving a real `Content-Length`/`Content-Type` at near-zero transfer cost,
  with neither Range nor HEAD involved. DEGRAD is closed/historical (10
  fixed year-rows, 2007-2016, will never grow), so this resolver only
  re-verifies existing manifest values against the live server rather than
  discovering new ones. If another source ever needs a resolver on this
  same `obt.inpe.br` domain, don't assume Range or HEAD will work there
  either — re-test from scratch.
- **A merged `fallback_notes` value keeps silently re-using the fallback,
  with no further PR, until someone updates the resolver's primary match.**
  If a resolver recovers a value through its `fallback_notes` path (see
  point 5's exception under "Writing a new resolver" above) and that PR
  gets merged WITHOUT also updating the resolver's own primary-match table
  (e.g. `resolve_deter.R`'s `slug_by_dataset`), every later run finds the
  exact same value again via the same fallback, reports zero changes, and
  opens no PR — so the `fallback_notes` warning, visible only in that one
  merged PR's body, that run's workflow step summary, and that run's JSON
  report, is easy to miss once the PR is old news. Confirmed live
  (2026-10-08) while building `resolve_deter.R`'s fallback: the PR body's
  own checklist already tells the reviewer to update the primary table
  before merging, which is the actual fix for this — nothing further was
  added on top of that single warning. If this recurs in practice (a
  merged fallback left un-updated for a while), a cheap next step would be
  a Slack ping modeled on the existing ones (see "Slack notifications"
  above) that fires specifically when `fallback_notes` is non-empty on a
  run that reports zero changes — nobody has needed this yet.
- **`load_deter()`'s CRS was fixed 2026-10-09 (was: hardcoded polyconic,
  no datum/towgs84, biasing `area`).** The projection choice never
  misaligned alerts and municipalities (both inputs are transformed through
  the same CRS from the same SIRGAS 2000/EPSG:4674 source), but the OLD
  `+proj=poly ... +ellps=aust_SA` string (near-identical to EPSG:5880
  "SIRGAS 2000 / Brazil Polyconic," general-purpose, not equal-area, with
  the ellipsoid additionally mistyped) made `st_area()` wrong, growing with
  distance from the projection's central meridian (lon -54): against IBGE's
  own official area-calculation projection ("Projeção Cônica Equivalente de
  Albers" — source: IBGE, "Malha Municipal Digital e Áreas Territoriais
  2024," Notas metodológicas 01/2025, Apêndice B), the old CRS's measured
  bias was +0.45% for `deter_amz`, +0.85% for `deter_cerrado`, +0.14% for
  `deter_pantanal`, +0.51% for `deter_non_forest`, growing to roughly +5%
  for alerts west of -70°. `load_deter()` now uses that same Albers string
  directly (`+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22
  +x_0=5000000 +y_0=10000000 +ellps=GRS80`) — verified live against two
  independent references (lwgeom's ellipsoidal geodesic area, a Lambert
  azimuthal equal-area projection): median ratio 1.00000, 98% of rows
  within ±0.001%. `R/degrad.R` had the IDENTICAL wrong string at its own
  `operation_crs` call site — fixed 2026-10-12, see the entry below.
  The newer named EPSG codes for this same projection (10856/10857, "SIRGAS
  2000 / Brazil Albers," registered 2025-05-16) resolve to `NA` silently (no
  error, no warning) on a representative install (PROJ 9.3.1, sf 1.0.19) —
  that's why the fix uses the explicit proj4 string, not the named EPSG
  code; re-test before ever switching to it. Separately, a small number of
  raw alerts have no row at all after the municipality `st_intersection()`
  — this moved slightly with the CRS change (boundary slivers differ by
  projection): 16 of `deter_amz`'s 461,071, 6 of `deter_cerrado`'s 134,165
  (was 7 under the old CRS), 2 of `deter_pantanal`'s 24,155, 0 of
  `deter_non_forest`'s — cause not investigated, numbers small enough not
  to block anything so far, but worth knowing before trusting a
  raw-vs-treated row count match exactly; compare by `alert_id`, not row
  count.
- **`load_degrad()`'s CRS fixed 2026-10-12 (same bug as DETER's above) — and,
  2026-10-16, the 2 bugs found while verifying it (tracked in
  [datazoompuc/datazoom.amazonia#350](https://github.com/datazoompuc/datazoom.amazonia/issues/350),
  related to the already-open #283) are now ALSO fixed, via one unified
  change.** `operation_crs` had the identical `+proj=poly ... +ellps=aust_SA`
  string as DETER's; replaced 2026-10-12 with the same IBGE Albers
  projection.

  Then, 2026-10-16: a new `harmonize_degrad_year_crs()` is applied to every
  year's raw geometry, right after download and **before** `bind_rows()`
  (and after the `raw_data = TRUE` early return, so that path stays
  genuinely unharmonized — see below), transforming it via
  `sf::st_transform()` to a single real, PROJ-registered CRS — SIRGAS 2000,
  EPSG:4674, the CRS 2015-2016 already use — instead of the old
  post-`bind_rows()` block that only ever handled the "missing CRS" case.
  This closes both #350 bugs at once, as a natural consequence of using a
  real target CRS instead of two separate fixes:
  1. **A `time_period` crossing a raw-CRS group boundary no longer crashes.**
     The 3 previously-failing ranges (`2009:2010`, `2014:2015`, `2007:2016`)
     all succeed now — verified live: row count and total km² for each
     equal the exact sum of loading every year separately (231,453 rows /
     145,554.45 km² for `2007:2016`), every geometry valid and non-empty,
     `sf::st_crs()` of the merged result is EPSG:4674, and `2010:2012`
     (inside one group) is unchanged. Still tracked, separately, as #283.
  2. **The SAD69→SIRGAS 2000 datum shift is now really applied.** Measured
     directly against the raw files: 2007-2014 polygons move 59.5-75.0 m
     (median 65-69 m depending on year); 2015-2016 move 0.0 m (already
     SIRGAS2000). The real source of the shift turned out to be the
     `TOWGS84` embedded in the 2010-2014 files' own BOUNDCRS (matching the
     pre-existing fallback string used for 2007-2009), not the separately
     registered EPSG:4618 operation (which agrees to within 0.3 m) — the
     `# FRAGILE:` tag was corrected accordingly.

  **Effect of the datum shift alone** (current code vs. the 2026-10-12
  Albers-only code, same projection, same downloads): total area over 10
  years +0.0009% (-0.0001% to +0.0026% per year, 2015-2016 exactly 0;
  corrected 2026-10-17 -- an earlier draft of this note and of the commit
  message below had the sign flipped);
  municipality-level area delta median 0.004-0.1%, 95th percentile
  1.2-2.2%, with outliers up to ~15% in 2011 on municipalities that only
  have 1.5-4 km² of DEGRAD that year (a fixed metric shift reads as a large
  percentage on a tiny area); row count after `st_intersection()` changes
  by -14 to +3 per year (-36 of 231,489 over 10 years); 7 municipality-year
  pairs appear/disappear, all tiny slivers (0.00002-0.017 km², 0.031 km²
  total over 10 years).

  **Combined effect of both fixes vs. the ORIGINAL polyconic code**
  (unchanged from the 2026-10-12 measurement for the area-bias figures — a
  translation doesn't change area): +0.41% total area bias (+0.30% to
  +0.74% by year; by longitude, up to +4.8% west of -70°, smallest —
  +0.06% — between -55 and -50); 231,453 output rows for 227,218 raw
  polygons (net +1.9%), vs. 231,483 under the original code. New code's
  area matches an independent geodesic reference and a Lambert
  equal-area projection to within ±0.00001 (1st-99th percentile), every
  year.

  `raw_data = TRUE` is unaffected — verified byte-identical to the
  pre-2026-10-16 code on `2010:2012`, `2007:2009`, `2015:2016` and
  `2010:2014` (harmonization runs after that early return, so it still
  returns each year's genuinely original per-year CRS, including the
  NA/SAD69/SIRGAS2000 mix). DETER is unaffected throughout (its own raw
  files are already SIRGAS 2000).

  **A separate, pre-existing data limitation surfaced (not introduced) by
  this fix**: 2007-2009's raw shapefiles have no `view_date` column at all,
  so a multi-year load spanning those years now returns `NA` in
  `date`/`data` for 2007-2009 rows instead of crashing — same
  column-selection shape as DETER's `alert_type` gap above. Not a bug in
  this fix, just newly visible now that cross-group loads actually
  complete; documented in NEWS.md as user-facing.

  **Status, 2026-10-17: the CRS/datum fix itself is committed and pushed**
  (commit `52bcc7d` on `update-degrad`, not yet merged to `master` — note
  its message contains "closes #350", so merging this branch as-is will
  auto-close the issue regardless of the test-coverage status below;
  that's fine once both are ready, but don't merge before then if the
  issue should stay open a while longer). `tests/testthat/test-degrad-crs.R`
  was added (3 synthetic-fixture tests / 18 expectations, no network,
  ~1.2s — fails on the pre-fix code, passes on the fix) and
  `check_all_loads.R`'s `TIME_PERIOD_BY_FN$load_degrad` was widened from
  `2016` alone to `c(2009, 2010, 2016)` (crosses both raw-CRS group
  boundaries against the real INPE servers, ~75s, fails on the pre-fix
  code) — both verified (full suite: 602 expectations, 0 failures) but, as
  of this writing, not yet committed; a second commit for these two files
  is prepared and pending.

- **TerraBrasilis has no English-language file for DETER/PRODES — confirmed
  live, 2026-10-10 (meeting task 6).** The catalog's PT/EN entries are
  catalog-metadata only: every PT/EN pair (68 GeoNetwork-UUID-matched
  groups, cross-checked again with a UUID-free pass across all 116 EN / 115
  PT entries in the live `/business/api/v1/download/all` catalog) shares
  the exact same underlying `link` — including all 4 DETER products
  `load_deter()` reads and the PRODES raster `load_prodes()` reads. The
  download API's own OpenAPI spec (`/business/v2/api-docs`) exposes no
  language parameter at all, and `?lang=`, `?language=`, `?locale=`, and
  `Accept-Language` were all tested against both the catalog endpoint and a
  real DETER Pantanal download — byte-identical response every time (same
  zip member hashes). Column headers are therefore fixed per product
  regardless of the catalog's display language: DETER Pantanal/non-forest
  and the PRODES vector shapefile are already English-named internally
  (`UUID`, `CLASS_NAME`, `AREA_KM`, ... — dBASE's 10-char field-name limit
  truncates a couple, e.g. `CREATE_DAT`), DETER Amazônia/Cerrado mix
  English technical names with Portuguese abbreviations (`AREAUCKM`,
  `MUNICIPALI`, `GEOCODIBGE`, `UF`, `UC`), and the PRODES raster
  `load_prodes()` actually reads has no attribute table at all (just
  `.tif` + a GRASS `.txt` report + a `.qml` legend, the legend itself
  holding 3 Portuguese labels). Conclusion: downloading "in English" from
  the source isn't a real option for either package — `R/deter.R`'s
  existing hand-written `dplyr::rename()` (only 2 of DETER's raw columns
  ever reach `load_deter()`'s output: `view_date`→`date`/`data`,
  `classname`→`alert_type`/`tipo_de_alerta`; everything else is dropped by
  the `select()`) is already the minimal correct approach. No code change
  made for this task.
- **A separate, real gap found during that same check: `alert_type` VALUES
  stay in Portuguese even when `language = "eng"`.** `load_deter()` renames
  the *column* but never recodes the *values* inside it, and the raw values
  aren't even consistent across the 4 products: upper-snake Portuguese
  (`DESMATAMENTO_CR`, `CICATRIZ_DE_QUEIMADA`, ...) for `deter_amz`, only
  `DESMATAMENTO_CR` for `deter_cerrado`, and accented lowercase Portuguese
  transliterated to ASCII (e.g. `supressao com solo exposto`) for
  `deter_pantanal`/`deter_non_forest`. Not one of the meeting's 6 tasks and
  not yet scoped — a real fix would need a cross-product value dictionary
  (3 different vocabularies to reconcile) plus a decision on what to do
  with an unrecognized label, similar in shape to SEEG's
  `SEEG_EMISSION_TYPE_EN` dictionary (see "Graduating a detect-only
  resolver" above).

## Finishing activation (the system exists but has never run on its own)

Nothing here has been merged to the shared repo's default branch. GitHub
only honors a workflow's `schedule` trigger from the file as it exists on
the default branch, so until this merges, nothing runs unattended anywhere.

1. Commit and push whatever's in the working tree.
2. Confirm **Settings → Actions → General → Workflow permissions → "Allow
   GitHub Actions to create and approve pull requests"** is enabled on the
   target repo — this overrides `permissions: pull-requests: write` in the
   YAML and is off by default.
3. Merge to the default branch.
4. Confirm the manifest CSV is reachable at its raw GitHub URL (the
   `manifest_url()` default in `R/manifest.R`).
5. Trigger `update-manifest.yaml` manually once (`workflow_dispatch`) and
   read the resulting PR/report before trusting the schedule.
6. Only then let the weekly cron run unattended.

## File map

| Path | What it is |
|---|---|
| `inst/extdata/manifest/v1/datasets_link.csv` | The manifest. Ships with the package AND is fetched live from GitHub. |
| `R/manifest.R` | Runtime resolution (`dataset_field`, `dataset_meta`, `dataset_url`, `datasets_link`'s backing `effective_table`, remote fetch + fallback). |
| `actions/scrapers/resolve_*.R` | One resolver per automatable source. `.Rbuildignore`d. |
| `actions/scrapers/watch_*.R` | One site-link-inventory watcher per scraped page (see "Site-link inventory watchers" above). Also `.Rbuildignore`d. |
| `actions/scripts/build_manifest.R` | The pipeline driver — runs both resolvers and watchers, composes both Slack notification bodies (see "Slack notifications" above), reads a resolver's `partial_failures` attribute into `resolver_partial_failed` and a resolver's `fallback_notes` attribute into `resolver_fallback_notes`, surfaced as its own PR-body section (see point 5's exception under "Writing a new resolver" above). |
| `actions/scripts/manifest_validate.R` | Manifest validators + change detection, shared by `build_manifest.R` and (indirectly) the test suite. |
| `actions/scripts/site_inventory.R` | Inventory-side infrastructure (fetch, normalize, diff, structural validation) shared by every `watch_*.R`. |
| `actions/watch/site_links.csv` | The committed site-link inventory snapshot. Seeded from one live, hand-reviewed run. |
| `actions/scripts/mapbiomas_resolver_cache.R` | Resolver verification cache: read/write/lookup/upsert/fingerprint helpers (see "Resolver verification caching" above), plus `new_failed_files()` for the new-Dataverse-file-rejected Slack signal (see "Slack notifications" above). |
| `actions/scripts/zip_remote_listing.R` | `zip_remote_entries()`/`zip_remote_extract_entry()` — lists a remote zip's files and pulls one small entry's real content via a couple of ranged HTTP GETs, no full download. Used by `resolve_prodes.R` (verify `layer_name`, cross-check the `.qml` legend) and `resolve_baci.R` (verify `archive_file`, derive `available_time`). Splits fetch (curl) from parse (pure, byte-range-in) so `test-zip-remote-listing.R` can exercise the parse layer with no network. |
| `actions/scripts/resolver_alert_cache.R` | Generalized `new_failed_files()` sibling (a `source` column instead of MapBiomas-only): `alert_cache_upsert()`, `merge_resolver_alert_source()`, `new_resolver_alerts()` — the PRODES/EPE "new candidate failed content verification" Slack signal (see "Slack notifications" above). |
| `actions/cache/resolver_alerts.csv` | The committed alert cache, shared by every source that plugs in. Seeded from one live, hand-reviewed run — same posture as the MapBiomas cache and the site-link inventory. |
| `actions/cache/mapbiomas_resolver_cache.csv` | The committed cache itself. Seeded from one live, hand-reviewed run — same posture as the site-link inventory. |
| `actions/scripts/fragility_notes.R` | Generic `# FRAGILE:` tag collector (`collect_fragility_notes(r_path, label)`) plus the `FRAGILITY_SOURCES` table driving which sources get a PR-body fragility check and from which file (`R/mapbiomas.R`, `R/epe.R`, `R/prodes.R`, `R/baci.R`, `R/ips.R`, `R/seeg.R`, `R/aneel.R`, `R/deter.R`, `R/degrad.R` today — see point 3 of "Investigating a broken or changed source" above). |
| `.github/workflows/update-manifest.yaml` | The scheduled/manual CI job — also fires the two Slack notification steps (see "Slack notifications" above), gated on `GITHUB_OUTPUT` booleans `build_manifest.R` sets, `continue-on-error` so a missing `SLACK_WEBHOOK_URL` secret never fails the run. |
| `data-raw/denormalize_manifest.R` | The (re-runnable) migration that produced the self-sufficient-rows CSV from the old tiered one (superseded by the later base-row-deletion migration, kept for provenance). |
| `data-raw/capture_resolution_matrix.R` | The ORIGINAL resolution-matrix capture script, kept for provenance only — no longer replayed by any test (see `test-resolution-matrix.R`'s retirement note). |
| `data-raw/capture_resolution_matrix_v2.R` | The CURRENT capture script (real-key queries only). Re-run this pattern before any future resolution-logic change. |
| `tests/testthat/test-manifest-schema.R` | Structural invariants: no default rows, no (survey,dataset,geo_level,year) duplicates, unkeyed-XOR-keyed grouping, key completeness (both directions), `MANIFEST_META_COLS` agreement, the `dataset_field()` no-key guard. |
| `tests/testthat/test-resolution-matrix.R` | RETIRED — see its own header. Kept (skipped) for why. |
| `tests/testthat/test-resolution-matrix-v2.R` | The current golden-equivalence test (real-key entries byte-identical; former-base-row entries now `stop()`; `datasets_link()` delta count). |
| `tests/testthat/test-mapbiomas-schema.R` | `mapbiomas_treat()`'s header-shape fixes, tested against real column headers transcribed from live workbooks — no network needed. Includes the pt-language `class_level_N_from`/`_to` rename regression test, keyed on the real Collection 9 header. |
| `tests/testthat/test-fragility-notes.R` | The `# FRAGILE:` tag collector, table-driven over every source in `FRAGILITY_SOURCES` (`R/mapbiomas.R`, `R/epe.R`, `R/prodes.R`, `R/baci.R`, `R/aneel.R`, `R/deter.R`, `R/degrad.R`) plus mechanism-only checks: read live (not cached), degrades to `character(0)` rather than erroring if the target file is missing. |
| `tests/testthat/test-zip-remote-listing.R` | `zip_remote_listing.R`'s parse layer (EOCD/central-directory parsing, the reconstruct-and-`unzip()` extraction path), exercised offline against a small zip built on the fly — no network, no test-server dependency. |
| `tests/testthat/test-mapbiomas-resolver-cache.R` | The cache module in isolation: fingerprint stability/sensitivity, lookup/upsert safety (checksum or fingerprint mismatch always misses), the `reason`-column schema migration (old-schema file reads clean, positional-column safety on upsert), and `new_failed_files()`'s dedupe/anti-noise rules. Does NOT attempt to mock the resolver's own download path — see the closures R gotcha above for why. |
| `tests/testthat/test-resolver-alert-cache.R` | `resolver_alert_cache.R` in isolation: upsert/merge safety across sources, `new_resolver_alerts()`'s anti-noise rules (including the pass-to-fail-flip case above) — same pure-function testing posture as `test-mapbiomas-resolver-cache.R`, no network. |
| `tests/testthat/test-epe-schema.R` | `epe_energy_state_panel_treat()`'s pivot/rename logic, tested against a small hand-built long-format fixture reproducing the real source shape — no network needed. Covers the fonte→column map, the `stop()` on an unrecognized fonte, the `"Brasil"`-row drop, and the `" Do "/" De "` capitalization fix. Same "extract the closure into a testable function" pattern as `mapbiomas_treat()`. |
| `tests/testthat/test-deter-resolver.R` | `resolve_deter.R`'s keyword fallback in isolation, mocked `curl` against a real-shaped fixture (`fixtures/terrabrasilis_download_api_deter.json`, 8 real `/deter` entries + 5 real non-`/deter` decoys that match a keyword — 135 of the live API's 231 entries do, which is why the `/deter` namespace scoping matters). Covers the happy path, a single renamed slug per dataset, the reserved-link guard (deter_amz's "amaz" keyword also matches the non-forest entry's English name), the vanished-entry case, an unrecoverable rename, two ambiguous renames at once, the same-URL-twice guard, and the `fallback_notes` attribute's content on both the happy path (`NULL`) and the fallback path. |
