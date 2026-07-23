# Mayoral Elections Known Issues

Reference document for users of `mayoral_unharm`, `mayoral_harm`, and `mayoral_candidates` (`data/mayoral_elections/final/`).

Last updated: May 2026

> **Recent structural change (May 2026)**: Landrat (county head) elections were moved out of the mayoral datasets and into a new standalone dataset at [`data/landrat_elections/final/`](../data/landrat_elections/final/). See Section 10 for details. Pipeline scripts in `code/mayoral_elections/` still ingest BM/OB/Landrat together (raw IT.NRW Excel files mix them) but split outputs at the end of stages 01 and 01b.

## 1. Dataset Overview

### `mayoral_unharm` -- Election-Level, Original Boundaries

**39,986 rows x 16 columns** covering **7 states** (BM + OB + VG-BM + SG-BM only; Landrat lives in `landrat_unharm`).

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

| State | Year Range | Rows (mayoral) | Rows (landrat) | Source Format | Notes |
|---|---|---|---|---|---|
| Bayern | 1945--2025 | 33,726 | 1,098 | Excel (Bayerisches Landesamt) | Longest series; classifier uses `Amtstitel` column |
| Sachsen | 2001--2024 | 2,176 | 0 | Excel (Bürgermeisteratlas) | Source data does not separate Landrat |
| Nordrhein-Westfalen | 2009--2025 | 1,854 | 151 | Excel (IT.NRW) | Classifier uses `gemeinde` name (§11) |
| Niedersachsen | 2006--2025 | 1,087 | 99 | PDF extraction | 9 election years, 3 formats |
| Rheinland-Pfalz | 1994--2025 | 1,028 | 119 | Excel (Stat. Landesamt) | Percentages only, no absolute counts; sheet-based split |
| Saarland | 2019--2025 | 70 | 2 | Excel | Regionalverband Saarbrücken treated as Landrat |
| Schleswig-Holstein | 2023--2025 | 45 | 0 | Web scraping (wahlen-sh.de) | HTML pages, 35 municipalities |

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

The harmonized dataset (`mayoral_harm.rds`, **38,650 rows x 23 columns**) maps all AGS codes to **2021 municipal boundaries** using population-weighted crosswalks from `data/crosswalks/final/ags_crosswalks.csv`.

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

**84,081 rows x 32 columns** in **wide format**: one row per candidate per election cycle, with separate `_hw` and `_sw` columns for Hauptwahl and Stichwahl results. (1,007 Landrat candidate rows split out to `landrat_candidates`.)

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

**Fix**: The code now processes both file types. BM files (skip=2) produce `Buergermeisterwahl` rows; OB files (skip=3) produce `Oberbuergermeisterwahl` and `Landratswahl` rows, distinguished using the `gemeinde` name column (NOT the AGS suffix — see Section 11).

## 8. Duplicate Records

- **4 Bayern duplicates** from 1948: Same `(ags, election_date, election_type)` -- true duplicates in source data.
- **Uniqueness key**: In `mayoral_unharm` and `mayoral_harm`, rows are uniquely identified by `(ags, election_date, election_type, round)`. In `mayoral_candidates`, the wide format means each row is a unique `(ags, election_date, candidate)` combination.

## 9. Election Date Caveats

- **NRW fallback dates**: When the date column in the source Excel cannot be parsed, the code falls back to the date from the filename (e.g., `KW 2009 Buergermeisterwahlen.xlsx`). If even that fails, it defaults to January 1st of the election year. Some rows may therefore have approximate rather than exact election dates.
- **Niedersachsen dates**: Each election year has a single date (e.g., `2006-09-10`, `2019-05-26`) assigned to all elections within that cycle. Stichwahl results use the Stichwahl date (e.g., `2006-09-24`). Individual election dates within a cycle are not distinguished.
- **Sachsen year mismatches**: 2 rows where `election_year` and `year(election_date)` differ due to runoff elections crossing a year boundary (see Section 6).

## 10. Mayoral / Landrat dataset split (May 2026)

Landrat (head-of-Landkreis) elections are now published as a **standalone dataset** at `data/landrat_elections/final/` with the same schema as the mayoral files. The split happens at the end of stages 01 and 01b in `code/mayoral_elections/`:

```r
mayoral_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl",
                   "VG-Bürgermeisterwahl", "SG-Bürgermeisterwahl")
landrat_unharm  <- combined %>% filter(election_type == "Landratswahl")
mayoral_unharm  <- combined %>% filter(election_type %in% mayoral_types)
```

**Why split**: Landrat elections cover different geographic units (Kreise, AGS ending in `000`), use different harmonization crosswalks (county-level), and have different downstream uses (county-level outcomes, not municipality-level). Keeping them in the mayoral dataset created confusion for downstream users.

**Why not Bayern**: The Bayern raw file mixes BM and Landrat without a flag; we don't currently have a reliable way to separate them. They remain in the mayoral dataset.

**Changes to existing scripts**:
- `02_mayoral_harm.R`: input no longer contains Landrat rows. The existing filter at line 54 (`harmonizable_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")`) was already excluding Landrat, but the upstream split is now the principled boundary.
- `03_mayor_panel.R` and `04_candidate_characteristics.R`: read `mayoral_candidates.rds` which no longer contains Landrat. NRW mayor panel count dropped from ~975 (with Landrat-as-OB miscoding) to 944 (true OB only).

## 11. NRW classifier — name-based (May 2026)

**Bug history**: Earlier versions of `process_nrw_file()` and `process_nrw_candidates()` distinguished kreisfreie Städte from Landkreise using the AGS suffix:

```r
# OLD (broken):
election_type = case_when(
  default_election_type == "Bürgermeisterwahl" ~ "Bürgermeisterwahl",
  grepl("000$", gkz_clean) ~ "Oberbürgermeisterwahl",
  TRUE ~ "Landratswahl"
)
```

This is broken because **both** kreisfreie Städte AND Landkreise have AGS ending in `"000"`:
- `05111000` Düsseldorf (kreisfreie Stadt)
- `05154000` Kreis Kleve (Landkreis)

The "ends in 000" rule swallowed all Landkreise, leaving the Landratswahl branch unreachable. Result: every NRW Landkreis (31 of them) plus Städteregion Aachen was tagged as `Oberbürgermeisterwahl` — affecting 5 election years × 32 entities ≈ 132 elections.

**Fix**: Use the `gemeinde` name column from the source file:

```r
# NEW:
election_type = case_when(
  default_election_type == "Bürgermeisterwahl" ~ "Bürgermeisterwahl",
  grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion", gemeinde) ~ "Landratswahl",
  grepl("Stadt", gemeinde) ~ "Oberbürgermeisterwahl",
  TRUE ~ "Oberbürgermeisterwahl"
)
```

Names like `"Krfr. Stadt Düsseldorf"` / `"Kreisfreie Stadt Düsseldorf"` match the `Stadt` rule. Names like `"Kreis Kleve"`, `"Rhein-Erft-Kreis"`, `"Hochsauerlandkreis"`, `"Städteregion Aachen"`, `"Paderborn, Kreis"` match the Kreis/Städteregion rule. The two rules are mutually exclusive — `grepl("Stadt", "Städte...")` returns FALSE because the German letter ä is distinct from a in the substring match.

## 12. NRW 2025 Stichwahl date typo (IT.NRW source-data error, patched in pipeline)

**Affected file**: `data/mayoral_elections/raw/nrw/KW 2025 Oberbürgermeister-Landratswahlen.xlsx`

**The error**: Every Stichwahl row in this file has its date encoded as Excel serial `44101` (= 2020-09-27) instead of the correct `45928` (= 2025-09-28). Verified by direct XML inspection of `xl/worksheets/sheet1.xml`:

```xml
<c r="C6" s="14"><v>45914</v>   <!-- Düsseldorf HW: 2025-09-14 ✓ -->
<c r="C7" s="17"><v>44101</v>   <!-- Düsseldorf SW: should be 45928, encoded as 44101 ✗ -->
<c r="C8" s="18"><v>45914</v>   <!-- Duisburg HW -->
<c r="C9" s="17"><v>44101</v>   <!-- Duisburg SW (wrong again) -->
... pattern repeats for all 36 SW rows in the file
```

**How we know it's the source data, not our code**:
1. The XML cell value is literally `44101`. Our R code reads what's there, correctly converts the Excel serial, and gets 2020-09-27.
2. The vote totals on these "2020-09-27" rows do not match the true 2020 SW results from the 2020 file:
   - Düsseldorf 2025-file row 2: Keller `120430` votes; 2020-file row 2: Keller `118308` votes (different).
   - Bonn 2025-file row 2: Déus + Dörner; 2020 SW: Sridharan + Dörner. Déus did not run in 2020.
3. The file's own title in `sharedStrings.xml` says: *"Oberbürgermeister- und Landratswahlen ... am 14.09.2025 bzw. 28.09.2025"* — the metadata says 2025-09-28 but the date column says 2020-09-27. Only the date column is wrong.
4. The voter-roll counts (`Wahlberechtigte`) on the misdated rows match the 2025 voter rolls within ~200 voters, not the 2020 voter rolls (which are ~800 voters apart).

Almost certainly someone copied the date column from the 2020 template when building the 2025 file and forgot to update it.

**Why this matters**: The per-file Stichwahl-cycle detector in `01b_mayoral_candidates.R` (lines ~319-333) groups by `(ags, election_type)` and assigns "hauptwahl" / "stichwahl" by date order **within a single file**. With the misdated rows, the 2025 file's "2020-09-27" row was the earliest date for each kreisfreie Stadt → got tagged as "hauptwahl" → after cross-file dedup it survived alongside the real 2020 SW row, producing two rows for the same `(ags, candidate_name)` combo with different vote counts. This is exactly the duplicate pattern Nils flagged for Bonn and Düsseldorf in May 2026.

**The patch** (in `process_nrw_file()` and `process_nrw_candidates()`):

```r
if (election_year == 2025 && grepl("Oberb", basename(file))) {
  n_patch <- sum(nrw_clean$election_date == as.Date("2020-09-27"), na.rm = TRUE)
  if (n_patch > 0) {
    cat("  IT.NRW 2025 patch:", n_patch, "rows 2020-09-27 → 2025-09-28\n")
    nrw_clean <- nrw_clean %>%
      mutate(
        election_date = if_else(election_date == as.Date("2020-09-27"),
                                as.Date("2025-09-28"),
                                election_date),
        election_year = year(election_date)
      )
  }
}
```

This is a **narrow, conditional patch**: only triggers on the 2025 OB file, only changes the specific value 2020-09-27, and prints a message every time it fires so you can audit. Remove this patch when IT.NRW publishes a corrected source file.

**How to detect a similar issue in future raw data**: For each NRW raw file, compare the year extracted from the filename with the year of the parsed `Datum` column. If a "KW YYYY" file contains rows whose `year(election_date) ≠ YYYY`, investigate before processing. The diagnostic block we used:

```r
# Quick audit: dates that don't match filename year
fname_year <- as.numeric(str_extract(basename(file), "20[0-9]{2}"))
mismatch <- sum(year(parsed_date) != fname_year, na.rm = TRUE)
if (mismatch > 0) warning(sprintf(
  "%s: %d rows have dates from a different year — investigate before processing",
  basename(file), mismatch))
```

Apply this as a sanity check whenever a new IT.NRW file lands.

## 13. Detection patterns for future similar issues

When IT.NRW (or any state agency) publishes new mayoral data, run these checks before merging into the production dataset:

1. **Date-year vs filename-year**: filter raw rows to where `year(parsed_date) ≠ year(in_filename)`. Any nonzero count is a red flag — except for the documented case of cross-year publication of historical SW data.
2. **Candidate name overlap across years**: if a "2020 Stichwahl" row contains candidates who first appear in 2025 (or vice versa), the date is suspect.
3. **Vote totals match raw `Gültige Stimmen`**: After processing, sum `candidate_votes_hw` per `(ags, election_year)` and compare against the file's own `Gültige` column. Mismatches indicate parsing errors.
4. **Cross-leakage between mayoral and landrat**: `grepl("Krfr|Kreisfreie Stadt", landrat$ags_name)` and `grepl("^Kreis |Städteregion|...", mayoral$ags_name)` should both be 0.
5. **Orphan SW candidates**: `is.na(candidate_votes_hw) & !is.na(candidate_votes_sw)` per state. Some legitimate cases exist (Bayern/NS/SN historical data); NRW should be 0 unless source data has new gaps.

The verification logic that catches all of these checks lives in `code/mayoral_elections/99_audit.R`. Run it after every pipeline update:

```bash
Rscript code/mayoral_elections/99_audit.R
```

It exits with status 0 if all checks pass, 1 otherwise.

A second audit at `code/landrat_elections/99_audit.R` covers the landrat dataset specifically (schema, AGS validity, date sanity, vote-count integrity, per-state coverage, external-truth spot checks, candidate-level integrity, duplicate detection, Stichwahl logic). Run both:

```bash
Rscript code/mayoral_elections/99_audit.R   # 28 checks on mayoral/cross-leakage
Rscript code/landrat_elections/99_audit.R   # 30+ checks on landrat outputs
```

## 14. Bayern + Saarland classifier (May 2026, follow-up to §10–11)

After splitting Landrat into a standalone dataset for NRW (§10), an audit of the other states' raw data revealed two more classifier bugs that had been silently grouping non-mayor elections under `Bürgermeisterwahl`:

### Bayern — 1098 Landrat + 557 Oberbürgermeister rows misclassified

The Bayerisches Landesamt Excel file (`raw/bayern/20251114_Wahlen_seit_1945.xlsx`) has an explicit `Amtstitel` column with values:

| Amtstitel value | Count | Should map to |
|---|---:|---|
| Ehrenamtliche(r) 1. Bürgermeister*in | 19,205 | Bürgermeisterwahl |
| Berufsmäßige(r) 1. Bürgermeister*in | 8,884 | Bürgermeisterwahl |
| (NA) | 5,080 | Bürgermeisterwahl (default) |
| Landrat/Landrätin | 1,098 | **Landratswahl** |
| Oberbürgermeister*in | 408 | **Oberbürgermeisterwahl** |
| Oberbürgermeister*in einer großen Kreisstadt | 149 | **Oberbürgermeisterwahl** |

The previous code at [code/mayoral_elections/01_mayoral_unharm.R:77](../code/mayoral_elections/01_mayoral_unharm.R) and [code/mayoral_elections/01b_mayoral_candidates.R:125](../code/mayoral_elections/01b_mayoral_candidates.R) hardcoded `election_type = "Bürgermeisterwahl"` for all Bayern rows, ignoring `Amtstitel`. Result: 1098 BY Landrat elections (covering 72 Landkreise across 1945–2025) and 557 BY OB elections were silently labeled Bürgermeisterwahl.

**Fix**:

```r
election_type = case_when(
  grepl("^Landrat|^Landrät", Amtstitel) ~ "Landratswahl",
  grepl("^Oberbürgermeister", Amtstitel) ~ "Oberbürgermeisterwahl",
  TRUE ~ "Bürgermeisterwahl"
)
```

After fix: BY contributes 1098 rows to `landrat_unharm` and the 557 OB rows are now correctly tagged in `mayoral_unharm` (BM 33,169 + OB 557 instead of one big BM bucket).

### Saarland — 15 Regionalverband Saarbrücken rows misclassified

The SL raw file (`raw/saarland/Wahldaten_Bürgermeisterwahlen_2019-2025.xlsx`) uses the `Gemeinde/Kreis` column to identify the unit. 15 rows have value `"Regionalverband Saarbrücken"` — this is the SL equivalent of a Landkreis (its head election is the *Regionalverbandsdirektor*, functionally identical to a Landrat). They were being silently labeled `Bürgermeisterwahl`.

**Fix**:

```r
election_type = case_when(
  grepl("Regionalverband", `Gemeinde/Kreis`) ~ "Landratswahl",
  TRUE ~ "Bürgermeisterwahl"
)
```

After fix: SL contributes 2 rows to `landrat_unharm` (the 2024 RVS election plus its Stichwahl). The "15" raw rows became 2 after the per-election summarisation collapses the Wahlberechtigte / Wähler / Gültige / party rows into a single row per (ags, election_date).

### Bottom line — landrat_unharm coverage

| State | Before May 2026 | After §10 NRW split | After §14 BY+SL recovery |
|---|---:|---:|---:|
| Niedersachsen | 99 | 99 | 99 |
| NRW | 0 (miscoded as OB) | 151 | 151 |
| Rheinland-Pfalz | 119 | 119 | 119 |
| Bayern | 0 (miscoded as BM) | 0 | **1,098** |
| Saarland | 0 (miscoded as BM) | 0 | **2** |
| **Total** | **218** | **369** | **1,469** |

The same lesson applies to all of these: **never trust hardcoded `election_type` defaults** when the raw data has an explicit type column. Future state additions (BB, MV, ST, TH, BW, HE, etc., if/when their data lands in this pipeline) should always check whether the source distinguishes Bürgermeister / Oberbürgermeister / Landrat at the row level before defaulting.
