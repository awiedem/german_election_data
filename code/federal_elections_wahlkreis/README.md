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
  (110,518 rows).
- **`federal_wkr_unharm.{csv,rds}`** — wide, GERDA-style (mirrors `federal_cty_unharm`): one row per
  `election_year × Wahlkreis × stimme`; party columns are vote **shares** of `valid_votes`, plus
  `other` (residual: independents / small lists / Übrige) and `cdu_csu` (Union alias). 4,186 rows.
- **`wkr_2021_to_2025_crosswalk.{csv,rds}`** — one row per 2025 Wahlkreis with `boundary_change`
  (`unchanged` / `redrawn` / `new`) and its 2021 predecessor. 281 unchanged, 10 redrawn, 8 new.
- **`federal_wkr_2021_on_2025.{csv,rds}`** — the 2021 result expressed on the 2025 boundaries (the
  official Bundeswahlleiterin recomputation), GERDA-style shares, so "previous-election district
  strength" is directly available for every 2025 Wahlkreis, including redrawn/new ones.

### Wide schema
`flag_no_valid_votes, flag_naive_turnout_above_1` · meta: `election_year, wkr_nr` (character, leading
zeros), `wkr_name, land_nr, stimme, eligible_voters, number_voters, valid_votes, invalid_votes,
turnout, elected_party` · sorted party-share columns · `other` · `cdu_csu`.
`stimme ∈ {erststimme, zweitstimme}`.

## Coverage

Seven Bundestagswahlen, **2002-2025** (the machine-readable open-data era). Pre-2002 constituency
results exist only in older/scanned formats and are out of scope (no OCR guess-work), matching the state
pipeline's "machine-readable only" rule. `cdu_csu` is an ALIAS (= `cdu` + `csu`); do not add it when
summing party columns. Party columns are 0 (not NA) where a party did not contest a given
Wahlkreis-election, so the named party shares + `other` sum to 1.0.

## Validation

- **National reconciliation**: for 2002-2013 the sum of the 299 Wahlkreise per party equals each file's
  own Bundesgebiet total exactly (checked in `01_...R`); for 2017-2025 the kerg2 named-party totals
  reconcile Wahlkreis → Bund exactly.
- National vote shares reproduce the official published results for every election.
- `99_audit.R`: 22 checks (schema, unique key, shares sum to 1.0 in 4186/4186 rows, turnout range,
  every Direktmandat assigned, crosswalk categories, recomputed shares). All pass. **0 FAIL.**

## Known limitations

- **Unharmonized**: each election on its own 299-Wahlkreis definition; Wahlkreise are redrawn between
  elections and are not comparable across time without the crosswalk. Cross-time harmonization of the
  full 2002-2025 panel is not attempted (would need per-year Wahlkreis geometries).
- The crosswalk covers **2021→2025** only (the boundary change most users need). Earlier consecutive
  pairs can be added the same way if the Bundeswahlleiterin published an equivalent recomputation.
- `boundary_change` is derived by matching 2025 Wahlkreise to 2021 by name and comparing the 2021
  electorate; the fine-grained municipality-level predecessor weights for `new`/`redrawn` Wahlkreise are
  not reconstructed here (the official recomputation already supplies the assigned previous-election
  result directly).
