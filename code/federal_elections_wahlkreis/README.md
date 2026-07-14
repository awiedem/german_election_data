# Federal elections at WAHLKREIS (constituency) level — pipeline

Bundestagswahlen at the level of the **299 Wahlkreise** (single-member constituencies) — a new
geographic level for the federal tree, parallel to `code/federal_elections/` (municipality and county
levels) and analogous to `code/state_elections_wahlkreis/`.

## Pipeline

| Stage | Script | Purpose |
|---|---|---|
| Stage 0 | `00_download_raw.R` | Download the official Bundeswahlleiterin constituency result files (kerg/kerg2) for 2002-2025 + the 2025 reference tables and the 2021→2025 recomputation → `data/federal_elections/wahlkreis_level/raw/BTW{02..25}/` |
| Stage 1 | `01_federal_wkr_unharm.R` | Parse both source formats, normalise party labels, build the unharmonized long + wide outputs |
| Crosswalk | `02_wkr_crosswalk_21_25.R` | Build the 2021→2025 boundary crosswalk + the 2021 result expressed on 2025 boundaries |
| helper | `_normalise_party.R` | Party-name → snake_case normaliser (reused verbatim from the state Wahlkreis pipeline) |
| audit | `99_audit.R` | 22 deterministic checks over all four final outputs (re-run after any change) |

Run order: `00_download_raw.R` → `01_federal_wkr_unharm.R` → `02_wkr_crosswalk_21_25.R` → `99_audit.R`.

## Source

All data is official open data from **Die Bundeswahlleiterin** (Wiesbaden), under *Datenlizenz
Deutschland – Namensnennung – Version 2.0*. Two result-file formats are handled:

- **`kerg2`** (flat/long, one row per Gebiet × Gruppe × Stimme): 2017, 2021, 2025.
- **`kerg`** (classic wide, multi-row header): 2002-2025. The parser auto-detects the 2002 "old" layout
  (2 columns per party, Erststimme-only tail parties) vs the 2005-2013 "modern" layout (4 columns per
  party incl. Vorperiode) from the column identities in the header rows, not from fixed strides.

## Outputs (`data/federal_elections/wahlkreis_level/final/`)

- **`federal_wkr_unharm_long.{csv,rds}`** — tidy long, one row per `election_year × Wahlkreis × stimme ×
  party`, with absolute vote **counts** + `vote_share` + turnout. The count-level source of truth
  (114,702 rows). Includes an explicit `party = "other"` residual row per Wahlkreis×stimme, so
  independents / small lists / Übrige are recoverable here (parties + `other` = `valid_votes` exactly).
- **`federal_wkr_unharm.{csv,rds}`** — wide, GERDA-style (residual/alias schema follows the sibling
  `ltw_wkr_unharm`): one row per `election_year × Wahlkreis × stimme`; party columns are vote **shares**
  of `valid_votes`, plus `other` (residual: independents / small lists / Übrige) and `cdu_csu` (Union
  alias). 4,186 rows. Uses `election_year` (as `ltw_wkr_unharm`; note `federal_cty_unharm` still uses
  `year`) and `state`/`state_name`.
- **`wkr_2021_to_2025_crosswalk.{csv,rds}`** — one row per 2025 Wahlkreis with `boundary_change`
  (`unchanged` / `redrawn` / `new`), a `renamed` flag, and its 2021 predecessor. **283 unchanged
  (2 of them renamed), 10 redrawn, 6 new.**
- **`federal_wkr_2021_on_2025.{csv,rds}`** — the 2021 result expressed on the 2025 boundaries (the
  official Bundeswahlleiterin recomputation), GERDA-style shares, so "previous-election district
  strength" is directly available for every 2025 Wahlkreis, including redrawn/new ones.

### Wide schema
`flag_no_valid_votes, flag_naive_turnout_above_1` · meta: `election_year` (integer), `election_date`,
`wkr_nr` (character, leading zeros), `wkr_name, state` (zero-padded "01".."16", matching
`federal_cty_unharm`/`ltw_wkr_unharm`), `state_name, stimme, eligible_voters, number_voters,
valid_votes, invalid_votes, turnout, elected_party` · sorted party-share columns · `other` · `cdu_csu`.
`stimme ∈ {erststimme, zweitstimme}`.

## Coverage

Seven Bundestagswahlen, **2002-2025** (the machine-readable open-data era). Pre-2002 constituency
results exist only in older/scanned formats and are out of scope (no OCR guess-work), matching the state
pipeline's "machine-readable only" rule. Note the **coverage asymmetry** with `federal_cty_unharm`
(county level, which reaches back to 1953) — the Wahlkreis layer starts in 2002.

Conventions:
- `cdu_csu` is an ALIAS (= `cdu` + `csu`); do **not** add it when summing party columns.
- **Structural absence is `NA`, not 0** (matching `federal_cty_unharm` / `ltw_wkr_unharm`): a party that
  did not contest a given election reads `NA` that whole year (e.g. `afd` in 2002-2009, `bsw` before
  2025), *not* a real 0% — so panel / first-difference designs are not corrupted by structural zeros. A
  party that ran but scored zero in a specific Wahlkreis keeps a real 0 (e.g. `csu` outside Bavaria).
  Named party shares + `other` still sum to 1.0 (`rowSums(..., na.rm = TRUE)`).

## Validation

- **National reconciliation**: for every one of the seven elections the sum of the 299 Wahlkreise per
  party equals the file's own national total exactly (Bundesgebiet row for classic `kerg`, `Bund` row
  for `kerg2`), enforced with a hard `stop()` in `01_...R` so the outputs cannot be written otherwise.
- National vote shares and all 299 Direktmandate reproduce the official published results for every
  election (independently re-derived from the raw files).
- `99_audit.R`: 28 checks (schema, unique key, shares sum to 1.0 in 4186/4186 rows, turnout range,
  every Direktmandat assigned, structural-absence NA vs real 0, long parties+other == valid, crosswalk
  categories/tally, recomputed shares). All pass. **0 FAIL.**

## Known limitations

- **Unharmonized**: each election on its own 299-Wahlkreis definition. Wahlkreise are redrawn between
  elections and are **not comparable across time** without a crosswalk, and only the 2021→2025 pair has
  one. Concretely: within-Wahlkreis panel / fixed-effects / event-study designs spanning multiple
  elections before 2021 are **out of scope** with this data. Cross-time harmonization of the full
  2002-2025 panel is not attempted (would need per-year Wahlkreis geometries).
- **Independents (Einzelbewerber) are not individually attributable.** They (and Übrige / tiny lists)
  are folded into the `other` residual — recoverable as a count in the long file, but not as named
  candidates. This matters mainly for Erststimme, where a strong local independent (e.g. Hohmann in
  Fulda 2005, ~21.5%) shows up only as `other`, not by name.
- `boundary_change` labels are **name-continuity / electorate flags, not change-magnitude**: `unchanged`
  = identical 2021 electorate (possibly renamed, see the `renamed` flag); `redrawn` = same name, changed
  territory; `new` = no name and no electorate match to any 2021 Wahlkreis. `prior_2021_*` is a single
  best-match predecessor, **not** a weighted set of territorial predecessors — for mapping arbitrary
  2021 covariates (beyond the recomputed vote) onto 2025 boundaries, use a municipality crosswalk. The
  official recomputation (`federal_wkr_2021_on_2025`) already supplies the assigned previous-election
  *vote* directly for all 299.
- The recomputation file recomputes the **certified (post-Wiederholungswahl) 2021 result**, consistent
  with the `w-btw21` vintage used in the main dataset (verified identical for the 12 Berlin Wahlkreise);
  it carries no `number_voters`, so `federal_wkr_2021_on_2025` has no turnout column.
