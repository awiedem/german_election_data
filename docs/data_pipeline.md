# Data Processing Pipeline Reference

This document provides an exhaustive reference for the data processing pipelines in the GERDA (German Election Database) repository. It covers the general architecture, the crosswalk system, per-election-type walkthroughs, and a cross-pipeline comparison.

**Last updated:** March 2026

---

## Table of Contents

1. [Overview: The Three-Stage Pattern](#1-overview-the-three-stage-pattern)
2. [The Crosswalk System](#2-the-crosswalk-system)
3. [Federal Elections — Municipality Level (reference pipeline)](#3-federal-elections--municipality-level-reference-pipeline)
4. [Federal Elections — County Level](#4-federal-elections--county-level)
5. [State Elections](#5-state-elections)
6. [Municipal Elections](#6-municipal-elections)
7. [Mayoral Elections](#7-mayoral-elections)
8. [County Elections (Kreistagswahlen)](#8-county-elections-kreistagswahlen)
9. [European Elections](#9-european-elections)
10. [Cross-Pipeline Comparison Table](#10-cross-pipeline-comparison-table)

---

## 1. Overview: The Three-Stage Pattern

Every election type in GERDA follows a common three-stage pipeline, implemented as numbered R scripts:

### Stage 0 — Raw ingestion (`00_*_raw`)

Reads source-format data (ballot-district CSVs, state-specific Excels, API downloads) and aggregates it to the desired geographic unit. Not all pipelines have a separate Stage 0; some fold ingestion into Stage 1.

**Key operations:**
- Parse heterogeneous source formats (encoding, delimiter, header layout)
- Construct standardized identifiers (8-digit AGS for municipalities, 5-digit codes for counties)
- Aggregate ballot districts up to municipality or county level
- Flag special cases (joint mail-in voting districts, unassignable absentee votes)

### Stage 1 — Standardization (`01_*_unharm`)

Produces a clean, rectangular dataset where each row is one geographic unit in one election year, using the **original** administrative boundaries of that year.

**Key operations:**
- Harmonize column names across election years (German party names, umlauts, encoding artifacts)
- Allocate mail-in votes to municipalities (federal only)
- Compute vote shares and turnout
- Recode zeros to `NA` for parties that did not contest a given election
- Add election dates, state identifiers, and quality flags
- Output: `*_unharm.rds` / `*_unharm.csv`

### Stage 2 — Boundary harmonization (`02_*_harm`)

Maps all election results onto a **fixed** set of administrative boundaries (typically 2021 or 2025) so that results are comparable across time despite municipal mergers and splits.

**Key operations:**
- Merge election data with population-weighted crosswalks
- Handle unsuccessful merges (manual AGS corrections, year-shifting)
- Aggregate via weighted sums (for vote counts) or weighted means (for shares)
- Recompute vote shares and turnout from harmonized counts
- Output: `*_harm.rds` / `*_harm.csv`

---

## 2. The Crosswalk System

Crosswalks are the foundation of all boundary harmonization. They map each historical municipality/county code to its corresponding code in a target boundary year, with population-based weights to handle splits and mergers.

### 2.1 Municipality crosswalks

**Script:** `code/crosswalks/01_ags_crosswalk.R`
**Source:** Official BBSR (Bundesinstitut für Bau-, Stadt- und Raumforschung) crosswalk Excel files in `data/crosswalks/raw/`.

**Structure of `ags_crosswalks.csv`:**

| Column | Description |
|---|---|
| `ags` | Historical municipality code (8-digit, character) |
| `year` | Year of the historical boundary |
| `ags_21` | Target municipality code in 2021 boundaries |
| `municipality_name` | Name in historical year |
| `municipality_name_21` | Name in 2021 |
| `pop_cw` | Population-based crosswalk weight |
| `area_cw` | Area-based crosswalk weight |
| `emp_cw` | Employment-based crosswalk weight |
| `population` | Population in the historical year |
| `area` | Area in km² |
| `employees` | Number of employees |

**How weights work:**
- If municipality A (pop 3000) splits into A1 (pop 2000) and A2 (pop 1000) in the target year, A gets two rows: one with `ags_21 = A1, pop_cw = 2/3` and one with `ags_21 = A2, pop_cw = 1/3`.
- If municipalities B and C merge into D, both get `pop_cw = 1.0` mapping to D.
- Municipalities with unchanged boundaries get a single row with `pop_cw = 1.0`.

**Coverage:** Annual crosswalks from 1990 through 2021.

### 2.2 Chaining crosswalks for later target years

**2021 → 2023:** `code/crosswalks/04_build_21_23_ags_crosswalks.R`
- Chains 2021→2022 and 2022→2023 boundary changes
- Weight multiplication: `pop_cw_1990_23 = pop_cw_1990_21 × pop_cw_21_23`
- Output: `ags_1990_to_2023_crosswalk.rds`

**2023 → 2025:** `code/crosswalks/05_build_23_25_ags_crosswalks.R`
- Uses Statistisches Bundesamt (StatBA) boundary change notices for 2024 and 2025
- Chains: `pop_cw_1990_25 = pop_cw_1990_23 × pop_cw_23_25`
- Output: `ags_1990_to_2025_crosswalk.rds`

### 2.3 County crosswalks

**Script:** `code/crosswalks/02_cty_crosswalk.R`
**Source:** BBSR county-level crosswalk Excels.

**Structure of `cty_crosswalks.csv`:**

| Column | Description |
|---|---|
| `county_code` | Historical 5-digit county code |
| `year` | Year of the historical boundary |
| `county_code_21` | Target county code in 2021 boundaries |
| `pop_cw` | Population-based weight |
| `area_cw` | Area-based weight |
| `population` | County population |
| `area` | County area |

**Coverage:** Annual from 1990 through 2021. Notable case: Eisenach (16056) merged into Wartburgkreis (16063) in 2021.

### 2.4 Party code mapping

**Script:** `code/crosswalks/03_partycodes.R`
**Source:** `data/crosswalks/raw/partyfacts-mapping.csv` (9 MB)
Maps party names across different data sources and election types.

---

## 3. Federal Elections — Municipality Level (reference pipeline)

This is the most developed and complex pipeline. It serves as the reference implementation for the other pipelines.

### 3.1 Raw ingestion

**Script:** `code/federal_elections/municipality_level/00_federal_muni_raw.R` (1633 lines)
**Raw data:** `data/federal_elections/municipality_level/raw/BTW{80..25}/` — CSVs and TXTs per election cycle.

**What it does:**

1. **Reads source files** for each election (1980–2025). Format varies across years:
   - Pre-2002: fixed-width or semicolon-delimited with German encoding (Latin-1)
   - 2002–2017: CSV with varying header structures
   - 2021, 2025: Modern CSV format

2. **Constructs 8-digit AGS** from component columns (Land, Regierungsbezirk, Kreis, Gemeinde). The exact column positions and names vary by election year, requiring year-specific parsing logic.

3. **Aggregates ballot districts to municipality level** via `group_by(ags) |> summarise_at(vars, sum, na.rm = TRUE)`. Each municipality may contain dozens of ballot districts (Wahlbezirke).

4. **Identifies joint mail-in voting districts** — absentee ballots that span multiple municipalities within a county. These are flagged by AGS patterns:
   - Older elections: AGS ending in `"999"` or `"9xx"` patterns
   - Later elections: 3rd-to-last digit is `"9"` (Bezirksart == 5)

5. **Computes population weights** for distributing mail-in votes: each municipality's share of the county's mail-in-eligible population.

6. **Defines party groupings:**
   - `far_right`: AfD, NPD, REP, DVU, Die Rechte, III. Weg, and others
   - `far_left`: DKP, KPD, MLPD, SGP, and others
   - `far_left_wLinke`: far_left + Linke/PDS
   - `cdu_csu`: CDU in non-Bavaria, CSU in Bavaria

**Output:** `federal_muni_raw.rds` / `federal_muni_raw.csv`

### 3.2 Unharmonized

**Script:** `code/federal_elections/municipality_level/01_federal_muni_unharm.R` (~2700 lines)

**Mail-in vote allocation algorithm:**

This is the most technically complex part of the entire repository. Joint mail-in voting districts report aggregate absentee votes for multiple municipalities that cannot be directly attributed. The algorithm distributes these votes proportionally:

1. **Identify mail-in districts** by `Bezirksart == 5` (or AGS ending patterns in older elections).
2. **Compute `blocked_weight`** for each municipality within a county:
   ```
   blocked_weight = (A2 + A3) / county_blocked
   ```
   where `A2` = Briefwähler mit Wahlschein (mail voters with polling card), `A3` = additional mail voters, and `county_blocked` = county-level sum of (A2 + A3) across non-mail-in districts.
3. **Pivot to long format** → join mail-in data → compute `weighted_value = round(mailin_value * blocked_weight)` → pivot back to wide.
4. **Add weighted mail-in votes** to each municipality's original (polling-station) totals, but only if the municipality had `unique_mailin == 0` (i.e., did not already have a dedicated mail-in district).

**Voter count adjustment:**
- `number_voters_orig = ifelse(Bezirksart == 5, 0, B)` — mail-in district rows are zeroed out in the original count, as their votes are distributed to the constituent municipalities.
- `eligible_voters_orig` similarly excludes mail-in-only rows.

**Vote shares and turnout:**
- Vote shares: `party / number_voters` (denominator is total voters including allocated mail-in)
- Turnout: `number_voters / eligible_voters_orig`

**Special cases:**
- Berlin and Hamburg: aggregated to single AGS (11000000 and 02000000)
- Parties that received zero votes nationwide in a given year: recoded from 0 to `NA`
- Election dates assigned via `case_when()` lookup

**Output:** `federal_muni_unharm.rds` / `federal_muni_unharm.csv`

### 3.3 Harmonization to 2021 boundaries

**Script:** `code/federal_elections/municipality_level/02_federal_muni_harm_21.R` (942 lines)

**Step-by-step process:**

1. **Convert vote shares back to counts:** `across(cdu:far_left_w_linke, ~ .x * number_voters)` — this is necessary because harmonization aggregates raw counts, not proportions.

2. **Merge with crosswalks:** `left_join_check_obs(cw, by = c("ags", "election_year" = "year"))` using `ags_crosswalks.csv`.

3. **Handle unsuccessful merges** — municipalities whose AGS changed between the election year and the crosswalk reference year. Three-step fallback:
   - If election year is 1990: try crosswalk year 1991
   - If election year > 1990: try year − 1
   - Remaining unmerged Berlin districts: remap to 11000000

4. **Manual AGS corrections** (~17 cases) for municipalities where mechanical merging fails:
   - Moorgrund (16067045 → 16063045)
   - Krumbach (09774137 → 09774151)
   - Naunhof (14383260 → 14729310)
   - And ~14 others, each documented with the specific AGS remapping

5. **Weighted sum aggregation:**
   ```r
   group_by(ags_21, election_year) |>
     summarise(across(eligible_voters:far_left_w_linke, ~ sum(.x * pop_cw, na.rm = TRUE)))
   ```
   This computes the population-weighted sum of all vote counts for each target-year municipality.

6. **Handle 2025 election data** (which postdates the 2021 target boundaries):
   - Build a reverse crosswalk: `cw_25_to_21 = cw_25_to_23 × cw_23_to_21`
   - Map 2025 AGS codes back to 2021 boundaries

7. **Recompute vote shares:**
   - `total_votes` = row sum of all individual party vote columns
   - Vote shares: `party / total_votes`
   - Turnout: `number_voters / eligible_voters_orig`

8. **Quality flags:**
   - `flag_unsuccessful_naive_merge`: 1 if the AGS required the year-shifting fallback
   - `flag_total_votes_incongruent`: 1 if `total_votes ≠ valid_votes`
   - `flag_naive_turnout_above_1`: 1 if raw turnout exceeded 100% before harmonization

**Output:** `federal_muni_harm_21.rds` / `federal_muni_harm_21.csv`

### 3.4 Harmonization to 2025 boundaries

**Script:** `code/federal_elections/municipality_level/03_federal_muni_harm_25.R` (702 lines)

Follows the same approach as `02_federal_muni_harm_21.R` but:
- Uses `ags_1990_to_2025_crosswalk.rds` instead of `ags_crosswalks.csv`
- Target column is `ags_25` instead of `ags_21`
- For 2025 election data: no reverse crosswalk needed (data is already in 2025 boundaries)
- For pre-2021 data: uses the chained 1990→2025 crosswalk directly

**Output:** `federal_muni_harm_25.rds` / `federal_muni_harm_25.csv`

---

## 4. Federal Elections — County Level

### 4.1 Unharmonized

**Script:** `code/federal_elections/county_level/01_federal_cty_unharm.R` (432 lines)
**Raw data:** `data/federal_elections/county_level/raw/btw{1953..2021}kreis.csv`

**How it differs from the municipality pipeline:**
- **Simpler ingestion:** Data arrives already at county level, so there is no ballot-district aggregation step.
- **No mail-in complexity:** County-level data already includes absentee votes in each county's totals.
- **Longer time span:** Covers 1953–2021 (vs. 1980–2025 for municipality level).
- **5-digit AGS:** Counties use 5-digit codes (first 2 = state, remaining 3 = county).

**Processing steps:**
1. `process_election_data()` function reads each CSV, finds the header row containing "Lfd. Nr." or "Statistische Kennziffer", strips Erststimmen columns, retains Zweitstimmen only.
2. Constructs 5-digit AGS from `land` + `lfd_nr` columns, with fallback to `statistische_kennziffer`.
3. Applies the same party name harmonization (`renames` vector) as the municipality pipeline.
4. Aggregates Berlin districts (11100 + 11200 → 11000).
5. Vote shares: `party / number_voters`; turnout: `number_voters / eligible_voters`.
6. Parties with zero votes nationwide in a year: recoded to `NA`.

**Output:** `federal_cty_unharm.rds` / `federal_cty_unharm.csv`

### 4.2 Harmonized

**Script:** `code/federal_elections/county_level/02_federal_cty_harm.R` (414 lines)

**Key steps:**
1. Reads unharmonized data, filters to 1990+ (crosswalks only available from 1990).
2. Converts vote shares back to counts: `across(cdu:far_left_w_linke, ~ .x * number_voters)`.
3. Merges with `cty_crosswalks.csv` — same three-step fallback for unsuccessful merges:
   - 1990 elections → try crosswalk year 1991
   - Other years → try year − 1
   - Berlin districts → remap to 11000
4. Weighted sum: `sum(.x * pop_cw)` for all vote and voter count columns.
5. Retrieves 2021 population and area from `04_KreiseVorjahr.xlsx`.
6. Recomputes vote shares: `party / total_votes`; turnout: `number_voters / eligible_voters`.
7. AfD set to `NA` for years prior to 2013.

**Output:** `federal_cty_harm.rds` / `federal_cty_harm.csv`

---

## 5. State Elections

### 5.1 Unharmonized

**Script:** `code/state_elections/01b_state_unharm_raw.R`
**Sources:** Raw files from each state's statistical office in `data/state_elections/raw/Landtagswahlen/`. Formats include XLSX, XLS, CSV, and OCR-extracted CSVs from scanned PDFs.

This unified script processes all 16 German states (1946–2025), replacing the earlier `01_state_unharm.R` (GENESIS API) + `03_state_2022-24.R` (manual collection) approach. Each state has dedicated parsing logic handling heterogeneous source formats.

**Why the pipeline was replaced:** The GENESIS Regionalstatistik API does not properly allocate Briefwahl (mail-in) votes from shared/pooled districts to individual municipalities. A systematic comparison of 27,503 overlapping municipality-year observations (2006–2019) found massive valid_votes undercounts in 5 eastern German states: SN (+10–17% recovery), MV (+7–19%), BB (+0.1–20%), TH (+4–6%), ST (+0–8%). In the worst cases, individual municipalities were missing up to 195% of their valid votes. See `data/state_elections/final/README.md` and `docs/state_pipeline_audit.md` for the full comparison.

**Key features:**
- **Party harmonization:** `normalise_party()` function maps 90+ raw party name patterns to standardized snake_case names aligned with the federal pipeline.
- **~440 individual party columns:** All parties that ever ran in any state election are preserved as separate columns (vs only 8 major party groups in the old pipeline).
- **Vote shares from source:** Computed as `party_votes / valid_votes` (not `number_voters`).
- **Briefwahl properly allocated:** State raw data is pre-aggregated to municipalities with Briefwahl assigned to the correct municipality by the Landeswahlleiter.
- **Bayern Gesamtstimmen:** BY uses combined Erst+Zweitstimme (both count for seat allocation). `valid_votes ≈ 2 × number_voters` for BY 1950+.
- **OCR-extracted elections:** BB 1990/1994/1999, SH 1983, NRW 1947/1950, SL 1970/1975 were digitized from scanned PDFs. NRW 1947-1970 are county-level only (synthetic AGS `050xx000`, ~84 Kreise per year, aggregated from 150 WK for 1947/1950). The NRW source PDF contains three elections in interleaved rows (a=1947, b=1949 Bundestag, c=1950 Landtag); row identification uses party presence patterns.
- **Percentage-only data:** HB 1946–1995 (converted BIFF→XLSX with graphical headers) and HB 2011 (hardcoded from official Faltblatt PDF) only have vote share percentages — `valid_votes`/`invalid_votes` are NA.
- **RP 1979–2016:** Added from Landeswahlleiter file (`LW_RLP_1979_2021.xlsx`), Landesstimmen only, no turnout metadata. 2021 retains separate source with full turnout data. Note: source data is missing 5–6 municipalities for 1979–2001 (founded after 1979), causing 0.07–0.31% shortfall vs official Landesergebnisse. From 2006 onward, deviations are ≤0.16%; 2011/2016 match exactly.
- **HH 2025:** Added from Statistik Nord Landesliste Gesamtstimmen (`BUE2025_e01_Landesliste.xlsx`).

**Output:** `state_unharm.rds` / `state_unharm.csv` (~148,000 rows, ~450 columns, 16 states, ~70 election years)

### 5.2 Harmonization to 2021

**Script:** `code/state_elections/02b_state_harm_21.R`

Harmonizes all state elections (1990–2025) to 2021 administrative boundaries using population-weighted crosswalks. Uses the weighted-sum-of-counts method (matching the federal pipeline):
1. Imputes weight for rows with NA `valid_votes` (e.g., HB pre-1999 percentage-only data) from `number_voters` or `eligible_voters`
2. Converts shares to counts: `party_share × valid_votes`
3. Applies weighted sum: `sum(counts × pop_cw)`
4. Reconverts to shares: `counts / valid_votes`

**AGS handling:**
- Explicit corrections for DDR-era codes (ST 1990), Kreisreform renumbering (TH/SN 1994), and post-2021 mergers
- Fuzzy time matching for AGS codes that exist in the crosswalk but not for the exact election year
- Self-mapping for AGS codes that are already valid 2021 target codes (e.g., NI SpreadsheetML retroactive coding)

**Derived columns:** `far_right`, `far_left`, `far_left_w_linke`, `total_vote_share`, `perc_total_votes_incogruence`, `flag_total_votes_incongruent`, `flag_unsuccessful_naive_merge`

**Output:** `state_harm_21.rds` / `state_harm_21.csv` (~81,600 rows, ~462 columns)

### 5.3 Harmonization to 2023

**Script:** `code/state_elections/04_state_harm_23.R`

Same method as `02b_state_harm_21.R`, targeting 2023 boundaries using `ags_1990_to_2023_crosswalk.rds`. Includes the same AGS corrections, fuzzy time matching, self-mapping, weight imputation, and derived columns.

**Output:** `state_harm_23.rds` / `state_harm_23.csv` (~81,500 rows, ~462 columns)

### 5.4 Harmonization to 2025

**Script:** `code/state_elections/05_state_harm_25.R`

Same method, targeting 2025 boundaries using `ags_1990_to_2025_crosswalk.rds` (chained 1990→2021→2023→2025).

**Output:** `state_harm_25.rds` / `state_harm_25.csv` (~81,400 rows, ~462 columns)

### 5.5 Data quality documentation

See `docs/state_pipeline_audit.md` for comprehensive documentation of known data quality issues, source data limitations, and all pipeline changes.

---

## 6. Municipal Elections

### 6.1 Unharmonized

**Script:** `code/municipality_elections/01_municipal_unharm.R` (~20,507 lines)
**Raw data:** `data/municipal_elections/raw/<state>/` — Excel and CSV files with highly heterogeneous formats.

**The 16-state format challenge:**

Municipal elections in Germany are organized at the state level, meaning each of the 16 states has its own format, column naming conventions, party lists, and data layout. This script contains dedicated parsing blocks for each state, making it by far the longest script in the repository.

**Common processing across states:**
- Parse state-specific headers and column layouts
- Standardize to: AGS, election year, eligible voters, number of voters, valid votes, invalid votes, and party-specific vote counts
- Vote shares: `party / valid_votes` (note: uses valid votes, not total voters)
- Zero vote counts for parties → `NA` (with indicator flag for whether the original value was genuinely zero)

**Output:** `municipal_unharm.rds` / `municipal_unharm.csv`

### 6.2 Harmonization to 2021 boundaries

**Script:** `code/municipality_elections/02_municipal_harm.R`

**Hybrid harmonization method:**

This script uses a mix of two approaches:
- **Weighted sum** for voter count variables (eligible_voters, number_voters, valid_votes, invalid_votes): `sum(.x * pop_cw)`
- **Weighted mean** for party vote shares: `weighted.mean(.x, w = pop_cw, na.rm = TRUE)`

This hybrid approach differs from both the federal pipeline (pure weighted sum of counts) and the state harm_21 pipeline (pure weighted mean of shares). It preserves the population-level voter counts while averaging the percentage-based party shares.

**Post-2021 elections (2023, 2024, 2025):** These use a reverse crosswalk chain to map back to 2021 boundaries:
1. Map 2023/2024 data → ags_25 via `ags_1990_to_2025_crosswalk.rds`
2. Map 2025 data → ags_25 (identity, already in 2025 boundaries)
3. Chain ags_25 → ags_23 → ags_21 using `crosswalk_ags_2023_to_2025.rds` (reversed) and `crosswalk_ags_2021_to_2023.rds`
4. Multiply chained weights: `final_pop_cw = pop_cw_to_25 × pop_w_25_to_21`

**Output:** `municipal_harm.rds` / `municipal_harm.csv`

### 6.3 Harmonization to 2025 boundaries

**Script:** `code/municipality_elections/03_municipal_harm_25.R`

Same hybrid method as `02_municipal_harm.R`, targeting 2025 boundaries using `ags_1990_to_2025_crosswalk.rds`. For 2025 election data, no crosswalk is needed (identity mapping). Same AGS corrections as the 2021 script.

**Output:** `municipal_harm_25.rds` / `municipal_harm_25.csv`

---

## 7. Mayoral Elections

### 7.1 Current state (unharmonized only)

**Script:** `code/mayoral_elections/01_mayoral_unharm.R` (489 lines)

**Coverage:** 6 states with varying completeness:
- **Fully processed:** Bayern (BY), NRW, Saarland (SAR), Sachsen (SN), Rheinland-Pfalz (RLP), Niedersachsen (NS)
- NS covers 2006–2025 (9 election years, 1,093 rows) using 3 PDF parsers: standard one-page-per-election (2011+), German-number format with full party names (2006), and tabular summary (2013)

**Key characteristics:**
- **Candidate-level data:** Unlike all other pipelines that track party vote shares, mayoral elections have candidate-level results (name, party affiliation, vote count, runoff status).
- **Multiple rounds:** Many mayoral elections have first-round and runoff elections.
- **Raw data:** `data/mayoral_elections/raw/<state>/` — state-specific Excel files and CSVs.

### 7.2 Why no harmonization exists yet

Mayoral elections pose unique harmonization challenges:
- Candidate-level results do not aggregate the same way party vote shares do — you cannot meaningfully sum candidates across merged municipalities.
- Election timing is highly irregular (not synchronized across municipalities).
- The relevant outcome variables (incumbent party, margin of victory, candidate characteristics) require different aggregation logic than vote counts.

A `02_mayoral_harm.R` script has not yet been written.

---

## 8. County Elections (Kreistagswahlen)

### Pipeline status

Processing code exists in `code/county_elections/01_county_elec_unharm.R` (Stage 1 — unharm only). Work in progress; 5 of ~10 states implemented.

**Script:** `01_county_elec_unharm.R`
**Output:** `data/county_elections/final/county_elec_unharm.{rds,csv}`

### Implemented states

| State | Code | Years | Format | Notes |
|---|---|---|---|---|
| Sachsen-Anhalt (ST) | 15 | 1994–2024 (8 elections) | Excel (.xlsx) + CSV (2024) | Multi-row headers; row 1 = party names |
| Thüringen (TH) | 16 | 2004–2024 (6 elections) | Excel (.xlsx) | Two-row headers (party + Stimmart); "Kreistag" filter |
| Mecklenburg-Vorpommern (MV) | 13 | 2014–2024 (3 elections) | XLSX (2014) + CSV (2019, 2024) | CSV: `Ausgabe=="A"` filter for absolute values; `x` → NA |
| Sachsen (SN) | 14 | 1999–2024 (6 elections) | Excel (.xlsx) | Legacy (1999–2014) vs modern (2019+) format; **SN 2014 has party names in row 5 not row 4** |
| Brandenburg (BB) | 12 | 2003–2024 (5 elections) | Excel (.xlsx) | Ballot-district level → municipality aggregation; **BB 2024 uses 12-digit ARS (not 8-digit AGS)** |

### Architecture

- **Party normalization**: `normalise_party_cty()` function maps raw party names to snake_case (separate from but similar to state election `normalise_party()`)
- **NA vs zero tracking**: Per-party NA means "party did not field candidates in this municipality" (distinct from 0.0 = ran but got zero votes). Implemented via `tapply` NA-tracker before aggregation, restored after `sum(na.rm=TRUE)`
- **Vote shares**: All party columns are shares (party_votes / valid_votes), not absolute counts. Absolute counts are in `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`
- **Deduplication**: When multiple raw party names normalize to the same label (e.g., BB 2003: "BV-BB (Zusammenfassung)" and "BV/BBS/FB" both → `bvb_fw`), a `_2` suffix is appended

### Known pitfalls encountered

- **Tibble name-mangling**: When `read_excel(col_names=FALSE)` produces `...N` column names, `names<-` / `colnames<-` / `setNames` on the resulting tibble or tibble-derived data.frame can silently append `....N` suffixes (e.g., `ags` → `ags....2`). This breaks `$` lookups and `data.table` `by=` operations. **Workaround**: build data frames column-by-column with `df[["name"]] <- vec` instead of bulk renaming
- **Header-row shifts**: Some states change which Excel row contains party names between election years (SN 2014: row 5 instead of row 4). Always validate that the detected header row actually contains party names
- **BB 2024 ARS→AGS**: Brandenburg 2024 switched from 8-digit AGS to 12-digit ARS (Amtlicher Regionalschlüssel). AGS = first 8 characters of ARS. Also uses an "aggregated party" section (cols 30–63) separate from individual candidate results (cols 65+)

### Remaining states (not yet implemented)

| State | Format | Notes |
|---|---|---|
| Baden-Württemberg (BW) | Excel (.xlsx) | 6 files |
| Hessen (HE) | Excel (.xlsx) | 1 file |
| Nordrhein-Westfalen (NRW) | ZIP archive | 1 archive |
| Niedersachsen (NS) | ZIP archive | 1 archive |
| Rheinland-Pfalz (RLP) | ZIP archive | 1 archive |
| Saarland (SAR) | PDF | Requires OCR |
| Schleswig-Holstein (SH) | PDF | Requires OCR |

---

## 9. European Elections

### Raw data inventory

Raw data for the 2024 European Parliament election exists in `data/european_elections/raw/ew24_wbz/` but **no processing code has been written**.

**Available files:**
- `ew24_wbz_ergebnisse.csv` (~13 MB) — ballot-district-level results
- `ew24_wbz_leitband.csv` — geographic reference file mapping ballot districts to municipalities
- Supporting PDFs with variable definitions

**Potential pipeline:**
The data structure (ballot districts with AGS identifiers) is similar to the federal election raw data, so the federal municipality pipeline (`00_federal_muni_raw.R`) could serve as a template. However, European elections have different party lists and no Erst-/Zweitstimmen distinction.

---

## 10. Cross-Pipeline Comparison Table

| Feature | Federal (Muni) | Federal (County) | State | Municipal | Mayoral |
|---|---|---|---|---|---|
| **Raw data source** | Ballot-district CSVs/TXTs | County-level CSVs | GENESIS API | 16 state Excel/CSVs | State Excel/CSVs |
| **Geographic unit** | Municipality (8-digit AGS) | County (5-digit) | Municipality (8-digit AGS) | Municipality (8-digit AGS) | Municipality (8-digit AGS) |
| **Time span** | 1980–2025 | 1953–2021 | 1946–2025 | 1990–2025 | Varies by state |
| **Vote share denominator** | `number_voters`† | `number_voters`† | `valid_votes` | `valid_votes` | N/A (candidate-level) |
| **Turnout formula** | `number_voters / eligible_voters_orig` | `number_voters / eligible_voters` | `number_voters / eligible_voters` | `number_voters / eligible_voters` | N/A |
| **Harm method** | Weighted sum of counts | Weighted sum of counts | Weighted mean of shares (02) / Weighted sum of counts (02b, 04, 05) | Hybrid: weighted sum (counts) + weighted mean (shares) | None |
| **Target boundaries** | 2021 and 2025 | 2021 | 2021, 2023, and 2025 | 2021 and 2025 | N/A |
| **Mail-in handling** | Yes (blocked_weight allocation) | No (included in county totals) | No | No | No |
| **Manual AGS corrections** | ~17 cases | ~50 via year-shifting | Present | Present | N/A |
| **Quality flags** | `flag_unsuccessful_naive_merge`, `flag_total_votes_incongruent`, `flag_naive_turnout_above_1` | `flag_unsuccessful_naive_merge` | Present | Zero-vote indicator flags | N/A |
| **Pipeline completeness** | Complete (raw → harm_21, harm_25) | Complete (unharm → harm) | Complete (unharm → harm_21, harm_23, harm_25) | Complete (unharm → harm_21, harm_25) | Partial (unharm only) |

† In the harmonized datasets, vote shares are recomputed as `party / total_votes` where `total_votes` = row sum of all party columns. This may differ slightly from `valid_votes` due to rounding during harmonization.

### Key methodological divergences

**1. Vote share denominator:**
The federal pipeline uses `number_voters` (all voters, including those who cast invalid ballots) as the denominator for vote shares in the unharmonized data. State and municipal pipelines use `valid_votes` (excluding invalid ballots). In harmonized federal data, the denominator becomes `total_votes` (the sum of all individual party vote columns after harmonization).

**2. Harmonization method:**
This is the most consequential divergence across pipelines:
- **Federal (muni + county) and State harm_21/harm_23/harm_25 (scripts 02b, 04, 05):** Convert shares → counts, compute weighted sum of counts, reconvert to shares. This preserves the absolute vote information and handles size differences between source municipalities correctly.
- **State harm (old script 02):** Compute weighted mean of shares directly. This can slightly over-weight small municipalities' vote shares relative to their population contribution. Retained for backward compatibility (2006–2019 only).
- **Municipal:** Hybrid approach — weighted sum for raw voter counts, weighted mean for party vote shares.

**3. Mail-in vote allocation:**
Only the federal municipality pipeline handles joint mail-in voting districts. This is because federal elections are the only type where ballot-district-level data with unassignable mail-in votes is the source format. County-level and state election data arrive with mail-in votes already allocated.

**4. Party groupings:**
The `far_right`, `far_left`, and `cdu_csu` composite variables are defined in the federal pipeline (`00_federal_muni_raw.R`) and reused in the county pipeline. State and municipal pipelines have their own party name mappings but follow the same categorization logic.

---

## 11. Possible Next Steps

As of February 2026, the following data gaps remain.

### Not yet integrated (raw data exists, no processing code)

**County elections (Kreistagswahlen)** — Raw data for ~10 states in `data/county_elections/raw/` (Excel + PDF). No processing scripts or final datasets exist. RLP has the richest source (time-series file covering 2000+). Some archives (TH) appear empty. This is the largest remaining gap and would require designing a new pipeline from scratch with heterogeneous state-specific formats, similar to the municipal elections pipeline.

**European elections 2024** — Clean ballot-district-level CSV in `data/european_elections/raw/ew24_wbz/` with 50+ party columns and accompanying documentation PDFs. Single election, well-structured data. Would need aggregation from ballot districts to municipality level, then boundary harmonization. Relatively straightforward compared to other gaps. (Note: an initial `european_muni_unharm`/`european_muni_harm` pipeline has been created but needs external validation.)

### Partially integrated

**Mayoral elections — no harmonization** — All 6 states have `mayoral_unharm` (41,298 rows), but no `02_mayoral_harm.R` exists. Harmonization is conceptually harder here: candidate-level results don't aggregate across municipal mergers the way party vote shares do. Would require different aggregation logic (e.g., tracking incumbent party or margin of victory rather than raw vote sums).

**Mayoral elections — Schleswig-Holstein** — Only a website URL (`wahlen-sh.de`) exists in `data/mayoral_elections/raw/sh/`. No actual data has been downloaded or processed.

**Mayoral elections — RLP data quality** — RLP provides percentages only (no absolute vote counts). All count columns are NA for the 1,147 RLP rows.

### Could be extended

**Federal county-level elections** — The county pipeline ends at 2021 (`btw2021kreis.csv`). BTW25 data exists at the municipality level and could be aggregated to county level. (Note: an initial `federal_cty_unharm`/`federal_cty_harm` pipeline has been created but needs external validation.)
