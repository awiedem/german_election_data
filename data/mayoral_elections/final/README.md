# Mayoral Elections Data

Seven datasets covering mayoral elections in 7 German states (Bayern, Niedersachsen, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland, Sachsen, Schleswig-Holstein), 1945--2025.

## Datasets

| File | Rows | Cols | Unit | Description |
|---|---|---|---|---|
| `mayoral_unharm` | 41,436 | 16 | Election | One row per election-round (winner-level summary), original boundaries |
| `mayoral_harm` | 38,667 | 23 | Election | Same as above, mapped to 2021 municipal boundaries |
| `mayoral_candidates` | 85,160 | 44 | Candidate | One row per candidate per election cycle (wide format), original boundaries, incl. candidate characteristics |
| `mayor_panel` | 34,495 | 18 | Person-election | Within-mayor panel with person IDs, original boundaries |
| `mayor_panel_harm` | 33,319 | 19 | Person-election | Same as above, mapped to 2021 municipal boundaries |
| `mayor_panel_annual` | 185,112 | 18 | Person-year | Annual panel (forward-filled from elections), original boundaries |
| `mayor_panel_annual_harm` | 179,011 | 19 | Person-year | Same as above, mapped to 2021 municipal boundaries |

All files are available as `.rds` and `.csv`.

---

## 1. `mayoral_unharm` -- Election-Level, Unharmonized

One row per (municipality, election date, election type, round). Reports the winner's party, votes, and vote share.

**Columns**: `ags`, `ags_name`, `state`, `state_name`, `election_year`, `election_date`, `election_type`, `round`, `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`, `winner_party`, `winner_votes`, `winner_voteshare`

**Election types**: `Buergermeisterwahl`, `Oberbuergermeisterwahl`, `Landratswahl`, `VG-Buergermeisterwahl` (RLP), `SG-Buergermeisterwahl` (NI)

**Round**: `"hauptwahl"` (first round) or `"stichwahl"` (runoff). All 7 states have Stichwahl data.

---

## 2. `mayoral_harm` -- Election-Level, Harmonized to 2021 Boundaries

Maps all AGS codes to fixed 2021 municipal boundaries using population-weighted crosswalks (`data/crosswalks/final/ags_crosswalks.csv`). This allows consistent panel analysis across time despite municipal mergers and splits.

Same 16 core columns as `mayoral_unharm` (including `round`), plus 7 flag/metadata columns:

| Column | Description |
|---|---|
| `flag_unsuccessful_naive_merge` | Crosswalk year-shift fallback was needed |
| `flag_pre_1990` | Pre-1990 election using 1990 crosswalk as fallback |
| `flag_aggregated` | Row aggregated from multiple predecessor municipalities |
| `flag_turnout_above_1` | Raw turnout exceeded 1 before capping |
| `flag_voteshare_above_1` | Raw winner voteshare exceeded 1 before capping |
| `flag_pct_only` | Percentage-only data (all counts NA; RLP + some old Bayern) |
| `n_predecessors` | Number of predecessor municipalities in aggregation group |

**Key design decisions**:

- **Grouping by `(ags_21, election_date)`** rather than `(ags_21, election_year)`, so runoff elections stay as separate rows and non-synchronized predecessor elections are not incorrectly merged.
- **Aggregation**: For N:1 mergers (103 rows), vote counts use `sum(x * pop_cw)` and the winner is taken from the largest predecessor by population. For percentage-only data (RLP), turnout and vote share use weighted means.
- **Pre-1990 fallback**: Bayern data from 1945--1989 (19,093 rows) uses the 1990 crosswalk since no earlier crosswalk exists.
- **Excluded types**: VG-Buergermeisterwahl, SG-Buergermeisterwahl, and Landratswahl are dropped because their pseudo-AGS codes cannot be mapped through the municipality crosswalk. Approximately 1,300 rows are excluded.

---

## 3. `mayoral_candidates` -- Candidate-Level (Wide Format)

One row per candidate per election cycle. Companion to `mayoral_unharm` -- same elections, but with all candidates and both Hauptwahl and Stichwahl results in columns.

**Election-level columns** (shared across candidates):

| Column | Description |
|---|---|
| `ags`, `ags_name`, `state`, `state_name` | Municipality identifiers |
| `election_year` | Year of the election cycle |
| `election_date` | Hauptwahl (first round) date |
| `election_date_sw` | Stichwahl (runoff) date, `NA` if no runoff |
| `election_type` | Type of election |
| `has_stichwahl` | `TRUE` if this election went to a runoff |
| `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes` | Hauptwahl vote metadata |
| `turnout` | Hauptwahl turnout as proportion (0--1) |
| `turnout_sw` | Stichwahl turnout (NA if no runoff) |

**Candidate-level columns**:

| Column | Description |
|---|---|
| `candidate_name` | Full name (available for NRW, RLP, NI, SL, SN partial, SH) |
| `candidate_last_name` | Last name |
| `candidate_first_name` | First name |
| `candidate_gender` | `"m"` / `"w"` — raw (RLP, SL) or predicted via `gender-guesser` (all named states) |
| `candidate_party` | Party or label (e.g., CSU, SPD, Parteilos, EB) |
| `candidate_votes_hw` | Hauptwahl vote count (NA for RLP) |
| `candidate_voteshare_hw` | Hauptwahl vote share (0--1) |
| `candidate_rank_hw` | Hauptwahl rank by votes (1 = most votes) |
| `n_candidates_hw` | Number of candidates in the Hauptwahl |
| `candidate_votes_sw` | Stichwahl vote count (NA if not in runoff) |
| `candidate_voteshare_sw` | Stichwahl vote share (NA if not in runoff) |
| `candidate_rank_sw` | Stichwahl rank (NA if not in runoff) |
| `n_candidates_sw` | Number of candidates in the Stichwahl |
| `is_winner` | Overall election winner (won HW outright OR won SW) |
| `candidate_birth_year` | Birth year (NI only) |
| `candidate_profession` | Profession (NI only) |
| `office_type` | Office type (BY and SL only) |

**Candidate characteristics columns** (added by `04_candidate_characteristics.R`):

| Column | Description |
|---|---|
| `candidate_gender_source` | `"raw"` (from source data: RLP, SL) or `"predicted"` (from name classification) |
| `candidate_gender_method` | Classification method: `raw`, `full_de`, `full_global`, `hyphen_first_de`, `hyphen_first_global`, `accent_norm_global`, `manual` |
| `candidate_gender_prob` | Confidence score: 1.0 for raw, 0.99 for `full_de`/`manual`, 0.95 for `hyphen_first_de`, 0.90 for global matches |
| `candidate_name_origin` | Fine-grained name origin: `"german"`, `"turkish"`, `"arabic"`, `"eastern_european"`, `"southern_european"` |
| `candidate_name_origin_conf` | Confidence in origin classification (0.50--0.95) |
| `candidate_name_origin_method` | Detection method: `"combined"`, `"surname_match"`, `"firstname_match"`, `"surname_pattern"`, `"default"` |
| `candidate_migration_bg` | Binary migration background indicator: 0 = German-origin, 1 = likely non-German origin |
| `candidate_migration_bg_prob` | Probability of migration background (continuous, 0--1) |
| `candidate_local_surname` | Placeholder (NA) — awaiting telephone directory data |
| `candidate_surname_county_share` | Placeholder (NA) |
| `candidate_surname_n_counties` | Placeholder (NA) |
| `candidate_surname_overrep_ratio` | Placeholder (NA) |

**Field availability by state**:

| State | Names | Gender (raw) | Gender (predicted) | Migration bg | Birth Year | Profession | Candidate Votes |
|---|---|---|---|---|---|---|---|
| Bayern | -- | -- | -- | -- | -- | -- | Yes |
| Niedersachsen | Yes | -- | Yes (99.9%) | Yes | Yes | Yes | Yes |
| Nordrhein-Westfalen | Yes | -- | Yes (100%) | Yes | -- | -- | Yes |
| Rheinland-Pfalz | Yes | Yes | Yes (100%) | Yes | -- | -- | -- (% only) |
| Saarland | Yes | Yes | Yes (100%) | Yes | -- | -- | Yes |
| Sachsen | Partial | -- | Yes (98.6%) | Yes | -- | -- | Yes |
| Schleswig-Holstein | Yes | -- | Yes (100%) | Yes | -- | -- | Yes |

---

## 4. `mayor_panel` / `mayor_panel_harm` -- Within-Mayor Panel

One row per person per election. Tracks individual mayors across multiple terms, enabling mayor fixed effects (person FE) estimation. The `_harm` version maps all AGS codes to 2021 municipal boundaries.

**Person identification**:

- **Bayern**: Groups consecutive terms via `Tag des ersten Amtsantritt` (same Amtsantritt date across elections = same person). No candidate names are available.
- **Named states** (NRW, NI, RLP, SL, SH): Name-key matching using `tolower(last_name)_first_initial_state` within each municipality.
- **Sachsen**: Partial name matching (~49% coverage); remaining winners get sequential IDs.

**Columns**:

| Column | Description |
|---|---|
| `person_id` | Unique mayor identifier (e.g., `p_09_00001` for Bayern, `p_05_mueller_k_05` for NRW) |
| `ags` | 8-digit municipality code (original boundaries) |
| `ags_21` | 8-digit municipality code mapped to 2021 boundaries (`_harm` only) |
| `state` | 2-digit state code |
| `election_year` | Year of the election |
| `election_date` | Date of the decisive round (Stichwahl date if applicable) |
| `term_number` | Sequential term count within (person, municipality), starting at 1 |
| `winner_party` | Party of the winning candidate |
| `winner_voteshare` | Vote share in the decisive round (0--1) |
| `winning_margin` | Vote share difference between winner and runner-up (0--1) |
| `n_candidates` | Number of candidates in the election |
| `is_incumbent` | 1 if `term_number >= 2`, else 0 |
| `next_runs_again` | 1 if this person wins the next election in this municipality, 0 if a different person wins, NA if no subsequent election observed |
| `tenure_start` | Year of the mayor's first election in this municipality |
| `years_in_office` | `election_year - tenure_start` |
| `term_start_date` | Date of first taking office (Bayern: Amtsantritt; others: first election date) |
| `n_terms` | Total number of terms observed for this person |
| `total_tenure_years` | Year span from first to last election |
| `has_margin_variation` | Whether winning margin varies across this person's terms (useful for FE feasibility) |

**Coverage**: 14,452 unique mayors (unharm) / 13,971 (harm), spanning 34,495 / 33,319 person-elections.

---

## 5. `mayor_panel_annual` / `mayor_panel_annual_harm` -- Annual Mayor Panel

One row per mayor per year. Forward-fills election-level data across the mayor's term, creating a balanced annual panel. The `_harm` version maps all AGS codes to 2021 boundaries.

Each mayor-election is expanded from `election_year` to the year before the next election in that municipality (or 2025 if no subsequent election is observed).

**Columns**:

| Column | Description |
|---|---|
| `ags` | 8-digit municipality code (original boundaries) |
| `ags_21` | 8-digit municipality code mapped to 2021 boundaries (`_harm` only) |
| `year` | Calendar year |
| `person_id` | Unique mayor identifier |
| `state` | 2-digit state code |
| `election_year` | Year of the election that started this term |
| `election_date` | Date of the decisive round |
| `term_number` | Term count within (person, municipality) |
| `winner_party` | Party of the mayor (constant within term) |
| `winner_voteshare` | Vote share in the decisive round (constant within term) |
| `winning_margin` | Winner-runner-up margin (constant within term) |
| `n_candidates` | Number of candidates (constant within term) |
| `is_incumbent` | 1 if `term_number >= 2` |
| `next_runs_again` | Whether this person wins the next election |
| `years_since_election` | `year - election_year` |
| `years_to_next_election` | Years until the next election in this municipality (NA if unknown) |
| `electoral_cycle_pos` | Position in the electoral cycle, 0 (election year) to <1 (year before next election) |
| `tenure_start` | Year of first election |
| `term_start_date` | Date of first taking office |

**Coverage**: 185,112 person-years (unharm) / 179,011 (harm), years 1945--2025.

---

## Stichwahl (Runoff) Coverage

| State | HW Elections | SW Elections | Detection Method |
|---|---|---|---|
| Bayern | 31,534 | 3,174 | `Wahlart` column in raw data |
| Sachsen | 1,576 | 300 | `Status` VE/EE + date matching |
| Nordrhein-Westfalen | 1,341 | 331 | 60-day cycle detection |
| Niedersachsen | 1,000 | 93 | Separate Stichwahl PDFs (2006/2013) + round detection |
| Rheinland-Pfalz | 693 | 227 | `Stichwahltag` column + separate HW/SW results |
| Saarland | 42 | 15 | `Wahlart...3` column in raw data |
| Schleswig-Holstein | 25 | 10 | Scraped with round info |

---

## Coverage by State

| State | Code | Year Range | Unharm Rows | Panel Persons | Panel Elections | Source |
|---|---|---|---|---|---|---|
| Bayern | 09 | 1945--2025 | 34,824 | 12,246 | 31,383 | Excel (Bayerisches Landesamt) |
| Sachsen | 14 | 2001--2024 | 2,176 | 447 | 524 | Excel (Buergermeisteratlas) |
| Nordrhein-Westfalen | 05 | 2009--2025 | 1,986 | 1,007 | 1,639 | Excel (IT.NRW) |
| Rheinland-Pfalz | 07 | 1994--2025 | 1,147 | 128 | 193 | Excel (Stat. Landesamt) |
| Niedersachsen | 03 | 2006--2025 | 1,186 | 532 | 664 | PDF extraction |
| Saarland | 10 | 2019--2025 | 72 | 57 | 57 | Excel |
| Schleswig-Holstein | 01 | 2023--2025 | 45 | 35 | 35 | Web scraping (wahlen-sh.de) |

**Note**: Panel person/election counts are from the unharmonized `mayor_panel`. The panel only includes Buergermeisterwahl and Oberbuergermeisterwahl (not Landratswahl, VG/SG-Buergermeisterwahl). Stichwahl-only records are collapsed into the parent election cycle. Bayern dominates with 74.7% of multi-term mayors.

---

## Known Issues and Caveats

**Rheinland-Pfalz has no absolute vote counts.** All count columns are NA; only turnout and vote shares (as percentages) are available. In `mayoral_harm`, these use weighted-mean aggregation instead of weighted-sum.

**Bayern has no candidate names.** The source data provides party and votes per candidate slot but not names. In `mayoral_candidates`, Bayern rows have `candidate_name = NA`.

**Niedersachsen data is PDF-extracted.** Parsing 3 different PDF layouts introduces minor issues: 91 elections where the sum of candidate votes falls below 50% of valid_votes (top candidates listed, not all), and 2 elections with a vote count discrepancy of 8 votes (source data rounding).

**Stichwahl completeness varies.** NRW and Bayern have both candidates in runoff elections. NS 2013 and RLP Stichwahl results list only the winner (1 candidate instead of 2). SH scrapes both Stichwahl candidates. NS only has separate Stichwahl PDFs for 2006 and 2013; other years have no candidate-level Stichwahl data.

**Sachsen year-boundary runoffs.** 2 elections where `election_year != year(election_date)` because the runoff crossed a calendar year (e.g., `election_year = 2008`, `election_date = 2009-01-11`).

**Sachsen "Stichwahl" is not a 2-person runoff.** In Sachsen, when no candidate wins >50% in the first round (VE), a full re-election (EE) is held with all candidates, not a 2-person runoff. This means `n_candidates_sw` can be 3--6 for Sachsen elections.

**Mayor panel: person identification is imperfect.** Bayern uses Amtsantritt dates, which correctly groups >99% of terms but cannot distinguish two different mayors with the same Amtsantritt date in the same municipality (extremely rare). Named states rely on name matching, which misses candidates without names in the data (1,384 elections, mostly Sachsen). 40 Bayern mayors show tenure spans >30 years, which are plausible given 6-year terms since 1945. One Bayern mayor has a 48-year span (10 terms).

**Mayor panel: 176 duplicate-winner elections resolved.** In 169 municipality-years, two different persons were both marked as election winners (HW winner vs SW winner from separate data records). The panel keeps the person with the highest vote share, which correctly identifies the overall winner.

For the full list of known issues including fixed bugs, see `docs/mayoral_elections_known_issues.md`.
