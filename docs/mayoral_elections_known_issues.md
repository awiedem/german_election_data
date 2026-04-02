# Mayoral Elections Known Issues

Reference document for users of `mayoral_unharm`, `mayoral_harm`, and `mayoral_candidates` (`data/mayoral_elections/final/`).

Last updated: March 2026

## 1. Dataset Overview

### `mayoral_unharm` -- Election-Level, Original Boundaries

**41,436 rows x 16 columns** covering **7 states**.

| Column | Type | Description |
|---|---|---|
| `ags` | character | 8-digit Amtlicher Gemeindeschluessel |
| `ags_name` | character | Municipality name |
| `state` | character | 2-digit state code |
| `state_name` | character | State name |
| `election_year` | numeric | Year of the election |
| `election_date` | Date | Date of the election |
| `election_type` | character | Type of election (Buergermeisterwahl, Landratswahl, etc.) |
| `round` | character | `"hauptwahl"` (first round) or `"stichwahl"` (runoff) |
| `eligible_voters` | numeric | Number of eligible voters |
| `number_voters` | numeric | Number of voters who cast a ballot |
| `valid_votes` | numeric | Number of valid votes |
| `invalid_votes` | numeric | Number of invalid votes |
| `turnout` | numeric | Turnout as proportion (0--1) |
| `winner_party` | character | Party of the election winner |
| `winner_votes` | numeric | Votes received by the winner |
| `winner_voteshare` | numeric | Winner's vote share as proportion (0--1) |

### Coverage Summary

| State | Year Range | Rows | Source Format | Notes |
|---|---|---|---|---|
| Bayern | 1945--2025 | 34,824 | Excel (Bayerisches Landesamt) | Longest series |
| Sachsen | 2001--2024 | 2,176 | Excel (Buergermeisteratlas) | Includes first-round + runoff rows |
| Nordrhein-Westfalen | 2009--2025 | 1,986 | Excel (IT.NRW) | Includes BM + OB elections |
| Niedersachsen | 2006--2025 | 1,186 | PDF extraction | 9 election years, 3 formats |
| Rheinland-Pfalz | 1994--2025 | 1,147 | Excel (Stat. Landesamt) | Percentages only, no absolute counts |
| Saarland | 2019--2025 | 72 | Excel | Smallest state by population |
| Schleswig-Holstein | 2023--2025 | 45 | Web scraping (wahlen-sh.de) | HTML pages, 35 municipalities |

### Stichwahl (Runoff) Distribution

| State | Hauptwahl | Stichwahl | Detection Method |
|---|---|---|---|
| Bayern | 31,643 | 3,181 | `Wahlart` column: "erster Wahlgang" vs "Stichwahl" |
| NRW | 1,641 | 345 | 60-day cycle detection by (ags, election_type) |
| Sachsen | 1,876 | 300 | `Status` VE/EE + date-gap matching |
| RLP | 920 | 227 | `is_stichwahl` flag from raw data |
| NS | 1,093 | 93 | Separate Stichwahl PDFs (2006/2013 only) |
| Saarland | 57 | 15 | `Wahlart...3` column |
| SH | 35 | 10 | Scraped with round info |

## 2. Boundary Harmonization (`mayoral_harm`)

The harmonized dataset (`mayoral_harm.rds`, **38,667 rows x 23 columns**) maps all AGS codes to **2021 municipal boundaries** using population-weighted crosswalks from `data/crosswalks/final/ags_crosswalks.csv`.

### How it works

- **1990--2020 elections**: Merged with annual crosswalk entries mapping historical AGS -> `ags_21`.
- **2021+ elections**: Identity mapping (already in 2021 boundaries).
- **Pre-1990 elections** (19,093 rows, all Bayern): Use the 1990 crosswalk as fallback. Flagged with `flag_pre_1990 = 1`.
- **N:1 mergers** (103 rows): When multiple predecessor municipalities had elections on the same date and map to the same `ags_21`, numeric counts are aggregated via `sum(x * pop_cw)` and the winner is taken from the largest predecessor by population. Flagged with `flag_aggregated = 1`.
- **1:N splits**: The election result is duplicated to each successor municipality with proportionally weighted counts.
- **Grouping**: By `(ags_21, election_date)`, which naturally preserves runoff elections as separate rows and keeps non-synchronized elections from different predecessors separate.
- **Round column**: The `round` column is carried through from `mayoral_unharm` without modification.

### Excluded from harmonization

- **VG-Buergermeisterwahl** (772 rows, RLP): VG pseudo-AGS codes cannot map through the municipality crosswalk.
- **SG-Buergermeisterwahl** (322 rows, NI): Samtgemeinde AGS codes are not in the municipality crosswalk.
- **Landratswahl** (218 rows, NI/NRW/RLP): County-level AGS codes, not municipality-level.
- **Unmatched AGS codes**: Mostly NRW county-level Landratswahlen, NI Samtgemeinde codes, and pre-1972 Bayern AGS codes that no longer exist in the 1990 crosswalk.

### Additional columns in `mayoral_harm`

| Column | Type | Description |
|---|---|---|
| `flag_unsuccessful_naive_merge` | integer | 1 if crosswalk year-shift fallback was needed |
| `flag_pre_1990` | integer | 1 if pre-1990 election using 1990 crosswalk fallback |
| `flag_aggregated` | integer | 1 if row was aggregated from multiple predecessor municipalities |
| `flag_turnout_above_1` | integer | 1 if raw turnout exceeded 1 before capping |
| `flag_voteshare_above_1` | integer | 1 if raw winner voteshare exceeded 1 before capping |
| `flag_pct_only` | integer | 1 if percentage-only data (all counts NA; applies to RLP) |
| `n_predecessors` | integer | Number of predecessor municipalities in the aggregation group |

## 3. Candidate-Level Dataset (`mayoral_candidates`)

**85,160 rows x 32 columns** in **wide format**: one row per candidate per election cycle, with separate `_hw` and `_sw` columns for Hauptwahl and Stichwahl results.

### Key design

- **Wide format**: Instead of separate rows for Hauptwahl and Stichwahl, each candidate has one row with both rounds' data in columns (e.g., `candidate_votes_hw`, `candidate_votes_sw`).
- **Election cycle matching**: Hauptwahl and Stichwahl for the same municipality are linked by date proximity (SW date within 60 days of HW date).
- **Candidate matching across rounds**: By `candidate_name` where available, `candidate_party` for Bayern (no names), `candidate_rank` as fallback.
- **SW-only candidates**: Candidates who appear in the Stichwahl but not the Hauptwahl (261 rows) are included with HW columns = NA.
- **`is_winner`**: Overall election winner -- won the Hauptwahl outright OR won the Stichwahl.

### Stichwahl in candidates dataset

| State | Elections with SW | Candidates with SW data | Notes |
|---|---|---|---|
| Bayern | 3,174 | ~6,300 | Matched by party (no names) |
| NRW | 331 | ~660 | Matched by candidate name |
| Sachsen | 300 | ~915 | EE is full re-election, not 2-person runoff |
| RLP | 227 | ~450 | Matched by name; SW often lists only winner |
| NS | 93 | ~180 | Only 2006+2013 have separate SW PDFs |
| Saarland | 15 | ~30 | Matched by name |
| SH | 10 | ~20 | Matched by name |

## 4. ~~NRW Column Mapping Bug~~ (Fixed, February 2026)

**Previously affected rows**: All NRW rows.

**Root cause was**: The NRW Excel files have 7 fixed columns (GKZ, Gemeinde, Datum, **Wahlberechtigte**, Waehler, Ungueltige, Gueltige) followed by repeating 3-column candidate blocks (Name, Wahlvorschlag, Stimmen). The old code assumed only 3 fixed columns before Waehler, missing the Wahlberechtigte column, which shifted all downstream mappings off by one. It also only read the first candidate block (columns 8-10) instead of all candidates.

**Fix**: Corrected the column mapping to include all 7 fixed columns, and reshaped candidate blocks from wide to long format so all candidates are properly parsed. `eligible_voters` now correctly maps to Wahlberechtigte, `number_voters` to Waehler, and vote counts are no longer misaligned.

## 5. ~~Saarland AGS Issue~~ (Fixed, February 2026)

**Previously affected rows**: All 72 Saarland rows.

**Root cause was**: The raw `AGS` column from the source file contains short numeric codes without the state prefix. The old code used `str_pad(AGS, 8, "0")`, left-padding to 8 digits with zeros, producing codes like `00000041` instead of `10000041`.

**Fix**: Changed to `paste0("10", str_pad(AGS, 6, "0"))` so the Saarland state code `10` is correctly prepended, matching the standard 8-digit AGS format.

## 6. State-Specific Data Limitations

### Rheinland-Pfalz (1,147 rows)

- **Percentages only**: All count columns (`eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `winner_votes`) are `NA`. The source data provides only percentage results.
- **VG pseudo-AGS**: Verbandsgemeinde (VG) elections use codes that are not real municipality AGS. They are stored as `"07" + padded VG code` (e.g., `07013203`). These cannot be merged with standard AGS crosswalks.
- **4 election types**: `Buergermeisterwahl`, `Landratswahl`, `Oberbuergermeisterwahl`, `VG-Buergermeisterwahl`.
- **3 rows with NA turnout**: A small number of elections have missing turnout even though RLP otherwise has turnout data (as percentages).

### Niedersachsen (1,186 rows)

- **9 election years**: 2006, 2011, 2013, 2014, 2016, 2019, 2021, 2024, 2025. Data extracted from PDFs via `pdftools`.
- **3 PDF formats**: (1) Standard one-page-per-election format (2011--2025), (2) German number formatting with full party names (2006), (3) tabular summary format (2013, ~33 elections).
- **2006 party name mapping**: Full party names (e.g., "Christlich Demokratische Union Deutschlands") are mapped to abbreviations (CDU, SPD, GRUENE, etc.). "Einzelwahlvorschlag" is mapped to "EB". Unmatched local party names are kept as-is.
- **2006 Stichwahl**: Separate PDF with two-column layout (first round + runoff). 82 Stichwahl results are available.
- **2013 Stichwahl**: Tabular format with Stichwahl results for ~30 elections.
- **No Stichwahl data for 2014--2025**: No separate Stichwahl PDFs available for these years. Elections requiring a Stichwahl are marked in `mayoral_unharm` (round = "hauptwahl" with winner_party indicating Stichwahl needed), but no candidate-level Stichwahl results are available.
- **4 election types**: `Buergermeisterwahl`, `Landratswahl`, `Oberbuergermeisterwahl`, `SG-Buergermeisterwahl` (Samtgemeinde).
- **2006 vote count inconsistency**: 1 row (ags 03462014) where `valid_votes + invalid_votes != number_voters` (off by 8). This appears to be a rounding/reporting issue in the source PDF.

### Sachsen (2,176 rows)

- **13.8% NA `winner_party`** (301 rows): Expected behavior for rows with `Status = "VE"` (Vorwahl / first round requiring runoff). These rows represent elections where no candidate won outright and a re-election was needed; no winner is recorded.
- **Stichwahl is full re-election**: When no candidate wins >50% in the first round (VE status), a full re-election (EE) is held with ALL candidates, not a 2-person runoff. This means `n_candidates_sw` can be 3--6 for Sachsen elections.
- **VE-EE date gaps**: The gap between first round and re-election varies: 14 days (164 elections), 21 days (116), 28 days (20).
- **2 year-boundary mismatches**: Rows where `election_year != year(election_date)`. These are runoff elections that span a year boundary (e.g., `election_year = 2008` but `election_date = 2009-01-11`). The `election_year` reflects the original election cycle, while `election_date` is the actual runoff date.

### Bayern (34,824 rows)

- **3--4% missing values** in vote columns (`eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `winner_votes`): Historical data from 1945 onward; older records are less complete.
- **No candidate names**: The source data provides party and votes per candidate slot but not names. In `mayoral_candidates`, Bayern rows have `candidate_name = NA`. Cross-round candidate matching uses party instead of name.
- **`Wahlart` column values**: "erster Wahlgang" (28,407 rows), "Stichwahl" (3,165), NA (3,193), "Hauptwahl ungueltig" (43), "Stichwahl ungueltig" (10), "Losentscheid" (6). The NA and "ungueltig" values default to "hauptwahl".
- **4 duplicate records** from 1948: Same `(ags, election_date, election_type)` combination appears twice for 4 municipalities. These are true duplicates in the source data.

### Schleswig-Holstein (45 rows)

- **Web-scraped data**: Results scraped from individual election pages on `wahlen-sh.de` by `code/mayoral_elections/00_sh_scrape.R`. Each election has its own URL; there is no bulk download.
- **35 municipalities**: Coverage limited to elections published on the portal (2023--2025). Historical data is not available online.
- **4 confirmation elections** (2024-06-09): Ja/Nein votes on a single incumbent. "Ja" votes treated as the candidate's votes; "Nein-Stimmen" dropped.
- **2 election types**: `Buergermeisterwahl` and `Oberbuergermeisterwahl`.
- **10 Stichwahl elections**: Stichwahl pages scraped separately from Hauptwahl pages.

## 7. ~~NRW Kreisfreie Staedte Missing~~ (Fixed, February 2026)

**Previously**: Major NRW cities such as Koeln, Duesseldorf, and other kreisfreie Staedte were absent because the processing code only read `Buergermeisterwahlen` files, explicitly filtering out `Oberbuergermeister-Landratswahlen` files.

**Fix**: The code now processes both file types. BM files (skip=2) produce `Buergermeisterwahl` rows; OB files (skip=3) produce `Oberbuergermeisterwahl` and `Landratswahl` rows, distinguished by whether the GKZ ends in `000` (kreisfreie Stadt) or not (Kreis).

## 8. Duplicate Records

- **4 Bayern duplicates** from 1948: Same `(ags, election_date, election_type)` -- true duplicates in source data.
- **Uniqueness key**: In `mayoral_unharm` and `mayoral_harm`, rows are uniquely identified by `(ags, election_date, election_type, round)`. In `mayoral_candidates`, the wide format means each row is a unique `(ags, election_date, candidate)` combination.

## 9. Election Date Caveats

- **NRW fallback dates**: When the date column in the source Excel cannot be parsed, the code falls back to the date from the filename (e.g., `KW 2009 Buergermeisterwahlen.xlsx`). If even that fails, it defaults to January 1st of the election year. Some rows may therefore have approximate rather than exact election dates.
- **Niedersachsen dates**: Each election year has a single date (e.g., `2006-09-10`, `2019-05-26`) assigned to all elections within that cycle. Stichwahl results use the Stichwahl date (e.g., `2006-09-24`). Individual election dates within a cycle are not distinguished.
- **Sachsen year mismatches**: 2 rows where `election_year` and `year(election_date)` differ due to runoff elections crossing a year boundary (see Section 6).
