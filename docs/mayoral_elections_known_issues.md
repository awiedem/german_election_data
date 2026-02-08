# Mayoral Elections Known Issues

Reference document for users of `mayoral_unharm` (`data/mayoral_elections/final/mayoral_unharm.rds`).

Last updated: February 2026

## 1. Dataset Overview

The unharmonized mayoral elections dataset contains **41,298 rows** and **15 columns** covering **6 states**.

### Columns

| Column | Type | Description |
|---|---|---|
| `ags` | character | 8-digit Amtlicher Gemeindeschlüssel |
| `ags_name` | character | Municipality name |
| `state` | character | 2-digit state code |
| `state_name` | character | State name |
| `election_year` | numeric | Year of the election |
| `election_date` | Date | Date of the election |
| `election_type` | character | Type of election (Bürgermeisterwahl, Landratswahl, etc.) |
| `eligible_voters` | numeric | Number of eligible voters |
| `number_voters` | numeric | Number of voters who cast a ballot |
| `valid_votes` | numeric | Number of valid votes |
| `invalid_votes` | numeric | Number of invalid votes |
| `turnout` | numeric | Turnout as proportion (0–1) |
| `winner_party` | character | Party of the election winner |
| `winner_votes` | numeric | Votes received by the winner |
| `winner_voteshare` | numeric | Winner's vote share as proportion (0–1) |

### Coverage Summary

| State | Year Range | Rows | Source Format | Notes |
|---|---|---|---|---|
| Bayern | 1945–2025 | 34,824 | Excel (Bayerisches Landesamt) | Longest series |
| Sachsen | 2001–2024 | 2,176 | Excel (Bürgermeisteratlas) | Includes first-round + runoff rows |
| Nordrhein-Westfalen | 2009–2025 | 1,986 | Excel (IT.NRW) | Includes BM + OB elections |
| Rheinland-Pfalz | 1994–2025 | 1,147 | Excel (Stat. Landesamt) | Percentages only, no absolute counts |
| Niedersachsen | 2006–2025 | 1,093 | PDF extraction | 9 election years, 3 formats |
| Saarland | 2019–2025 | 72 | Excel | Smallest state by population |

## 2. Structural Limitations

### No boundary harmonization

Unlike the federal, state, and municipal election pipelines, there is no `02_mayoral_harm.R` harmonization script. Mayoral election data is **candidate-level** (one winner per municipality per election), has **irregular timing** (elections happen on different dates across municipalities), and cannot be aggregated across boundaries the same way party vote counts can.

### No covariates

The dataset does not include population, area, or employment covariates. Other pipelines merge covariates from `ags_area_pop_emp.rds`, but this has not been implemented for mayoral elections.

### No codebook entry

Mayoral elections are not yet documented in `docs/codebook.md`.

## 3. ~~NRW Column Mapping Bug~~ (Fixed, February 2026)

**Previously affected rows**: All NRW rows.

**Root cause was**: The NRW Excel files have 7 fixed columns (GKZ, Gemeinde, Datum, **Wahlberechtigte**, Wähler, Ungültige, Gültige) followed by repeating 3-column candidate blocks (Name, Wahlvorschlag, Stimmen). The old code assumed only 3 fixed columns before Wähler, missing the Wahlberechtigte column, which shifted all downstream mappings off by one. It also only read the first candidate block (columns 8-10) instead of all candidates.

**Fix**: Corrected the column mapping to include all 7 fixed columns, and reshaped candidate blocks from wide to long format so all candidates are properly parsed. `eligible_voters` now correctly maps to Wahlberechtigte, `number_voters` to Wähler, and vote counts are no longer misaligned.

## 4. ~~Saarland AGS Issue~~ (Fixed, February 2026)

**Previously affected rows**: All 72 Saarland rows.

**Root cause was**: The raw `AGS` column from the source file contains short numeric codes without the state prefix. The old code used `str_pad(AGS, 8, "0")`, left-padding to 8 digits with zeros, producing codes like `00000041` instead of `10000041`.

**Fix**: Changed to `paste0("10", str_pad(AGS, 6, "0"))` so the Saarland state code `10` is correctly prepended, matching the standard 8-digit AGS format.

## 5. State-Specific Data Limitations

### Rheinland-Pfalz (1,147 rows)

- **Percentages only**: All count columns (`eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `winner_votes`) are `NA`. The source data provides only percentage results.
- **VG pseudo-AGS**: Verbandsgemeinde (VG) elections use codes that are not real municipality AGS. They are stored as `"07" + padded VG code` (e.g., `07013203`). These cannot be merged with standard AGS crosswalks.
- **4 election types**: `Bürgermeisterwahl`, `Landratswahl`, `Oberbürgermeisterwahl`, `VG-Bürgermeisterwahl`.
- **3 rows with NA turnout**: A small number of elections have missing turnout even though RLP otherwise has turnout data (as percentages).

### Niedersachsen (1,093 rows)

- **9 election years**: 2006, 2011, 2013, 2014, 2016, 2019, 2021, 2024, 2025. Data extracted from PDFs via `pdftools`.
- **3 PDF formats**: (1) Standard one-page-per-election format (2011–2025), (2) German number formatting with full party names (2006), (3) tabular summary format (2013, ~33 elections).
- **2006 party name mapping**: Full party names (e.g., "Christlich Demokratische Union Deutschlands") are mapped to abbreviations (CDU, SPD, GRÜNE, etc.). "Einzelwahlvorschlag" is mapped to "EB". Unmatched local party names are kept as-is.
- **2006 Stichwahl**: Separate PDF with two-column layout (first round + runoff). 82 Stichwahl results replace the Stichwahl-required entries from the main file.
- **2013 name-based AGS lookup**: The 2013 tabular format lacks Schlüssel codes; AGS is assigned via hardcoded name→AGS mapping for all 33 municipalities.
- **8.0% NA `winner_party`** (87 of 1,093 rows): Expected for Stichwahl-required elections without a separate Stichwahl results file (2014, 2016, 2019, 2024, 2025 have no separate Stichwahl PDF).
- **4 election types**: `Bürgermeisterwahl`, `Landratswahl`, `Oberbürgermeisterwahl`, `SG-Bürgermeisterwahl` (Samtgemeinde).
- **2006 vote count inconsistency**: 1 row (ags 03462014) where `valid_votes + invalid_votes != number_voters` (off by 8). This appears to be a rounding/reporting issue in the source PDF.

### Sachsen (2,176 rows)

- **13.8% NA `winner_party`** (301 rows): Expected behavior for rows with `Status = "VE"` (Vorwahl / first round requiring runoff). These rows represent elections where no candidate won outright and a runoff was needed; no winner is recorded.
- **2 year-boundary mismatches**: Rows where `election_year != year(election_date)`. These are runoff elections that span a year boundary (e.g., `election_year = 2008` but `election_date = 2009-01-11`). The `election_year` reflects the original election cycle, while `election_date` is the actual runoff date.

### Bayern (34,824 rows)

- **3–4% missing values** in vote columns (`eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `winner_votes`): Historical data from 1945 onward; older records are less complete.
- **4 duplicate records** from 1948: Same `(ags, election_date, election_type)` combination appears twice for 4 municipalities. These are true duplicates in the source data.

### Schleswig-Holstein

Raw data source identified (`data/mayoral_elections/raw/sh/website.txt` points to `wahlen-sh.de`) but **no processing code has been written**. Schleswig-Holstein is entirely absent from the dataset.

## 6. ~~NRW Kreisfreie Städte Missing~~ (Fixed, February 2026)

**Previously**: Major NRW cities such as Köln, Düsseldorf, and other kreisfreie Städte were absent because the processing code only read `Bürgermeisterwahlen` files, explicitly filtering out `Oberbürgermeister-Landratswahlen` files.

**Fix**: The code now processes both file types. BM files (skip=2) produce `Bürgermeisterwahl` rows; OB files (skip=3) produce `Oberbürgermeisterwahl` and `Landratswahl` rows, distinguished by whether the GKZ ends in `000` (kreisfreie Stadt) or not (Kreis).

## 7. Duplicate Records

- **4 Bayern duplicates** from 1948: Same `(ags, election_date, election_type)` — true duplicates in source data.
- **Multiple rounds on the same date**: First round and runoff elections for the same municipality can appear as separate rows with the same `(ags, election_date)`. This is intentional — they represent different stages of the election process.
- **Caution when grouping**: Users grouping by `(ags, election_date)` should be aware that this key is not unique. Grouping by `(ags, election_date, election_type)` is safer but still not unique due to the 4 Bayern duplicates.

## 8. Election Date Caveats

- **NRW fallback dates**: When the date column in the source Excel cannot be parsed, the code falls back to the date from the filename (e.g., `KW 2009 Bürgermeisterwahlen.xlsx`). If even that fails, it defaults to January 1st of the election year. Some rows may therefore have approximate rather than exact election dates.
- **Niedersachsen dates**: Each election year has a single date (e.g., `2006-09-10`, `2019-05-26`) assigned to all elections within that cycle. Stichwahl results use the Stichwahl date (e.g., `2006-09-24`). Individual election dates within a cycle are not distinguished.
- **Sachsen year mismatches**: 2 rows where `election_year` and `year(election_date)` differ due to runoff elections crossing a year boundary (see Section 5).
