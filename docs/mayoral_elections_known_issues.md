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

## 15. Hessen runoff pairing, failed Ja/Nein votes and panel terms (October 2026)

The 1993–2012 candidate names from Hessami (2018, *REStat*, doi:10.7910/DVN/FZWOMK) gave every Hessen runoff candidate a known first-round identity. They exposed four defects in the Hessen part of the mayoral files.

**1. Runoff results on the wrong row.**
- The wide pivot paired a Stichwahl candidate with "their" Hauptwahl row by name, else party, else rank. With redacted names and several `Einzelbewerbung` Wahlvorschläge, the key was ambiguous.
- So 190 runoff results (186 `Einzelbewerbung`) were published as runoff-only rows in 150 of 479 cycles. 109 of those rows carried the winner, so the winner's own first-round row said `is_winner = FALSE`.
- One 2008 runoff result sat on the third-placed candidate's row.
- **Fix:** `00_he_hist_parse.py` now emits an explicit `pair_id`, and 01b's match key uses it first. Order of evidence: names, then verified pins (`data/mayoral_elections/metadata/he_runoff_pairing_verified.csv`), then a distinctive Träger, then elimination, then ballot order.
- **Validation:** against the Hessami names (1993–2012), the name-free rule gets 570/573 runoff candidates right. For 2013–2026, every runoff that needed elimination or ballot order was checked against named results and pinned.

**2. Runoff winners without gender.** HSL records the elected person's gender on the decisive round only, so the Hauptwahl row that the pivot keeps had none. 285 Hessen mayoral and 26 Landrat winners had no gender; all now carry HSL's value.

**3. Failed Ja/Nein Hauptwahlen crowned.** Driedorf 2016-03-06 (49.7 % Ja) and Morschen 2022-03-06 (48.7 % Ja) seated nobody; a Neuwahl followed months later. 01b's winner repair made the failed sole candidate the winner. They now carry `flag_decisive_round_missing` (`is_winner = NA`).

**4. Panel.**
- Waldems 2000 was joined to the 1999 winner. In fact the 1999 election was annulled and the 1993 mayor won the 2000 Neuwahl (counter: 2nd term, 0 re-elections).
- The ags-year dedup dropped five genuine second terms in one year: Alsbach-Hähnlein 2007, Babenhausen 2002, Biebesheim am Rhein 1993, Bad Nauheim 1999 and Burghaun 2014. All carry official counters.
- **Fix:** Hessen is exempt from the year-level dedups. Only Bad Karlshafen's superseded 2017-05-21 runoff, which has no counter, is dropped.

**Also fixed in passing (all states):** the runoff-only branch of the pivot filed a January runoff under the next year. Two cycles carried two `election_year` values (Beselich 2009/10, Donauwörth 1948/49); they now take the Hauptwahl's year.

`99_audit.R` section 22 pins all of the above and is calibrated: it fails on the September-2026 release.

## 16. Thüringen: the Wahlvorschlag was a name, so `winner_party` was not a party (fixed, October 2026)

Found in the pre-publication audit of 2026-10-02 and fixed the same day. The defect had been in every release since Thüringen was added.

**What the source does.** The Landesamt database (`wahlen.thueringen.de`) redacts candidates' personal data (§ 50 Abs. 2 ThürKWO). It lists each candidate by the *Wahlvorschlag* only ("CDU", "Einzelbewerber", "Weitere Personen"). Where it names a person, the Wahlvorschlag reads "Nachname, Vorname" or "Nachname, Vorname (Träger)". It always names the elected person, and every candidate of a recent election. `00_th_scrape.py` stored that string whole as `candidate_party`.

**What was wrong.**
- `mayoral_candidates`: in 647 Thüringen rows (624 winners, mostly 2020–2026) the party was the candidate's own name. In 272 of them the given name also carried the bracket ("Vorname (CDU)").
- `mayor_panel`: `winner_party` was a person's name in 624 of 647 Thüringen rows. So `party_switch` and `is_new_party_mayor` compared names, not parties.
- 17 joint lists or group labels ("SPD, CDU, UWS", "FFW, ATG", "Bi, Zukunft Kammerforst") were read as "Nachname, Vorname". They became fake persons, 10 of them winners in `mayor_panel`.
- 8 elected persons were not recognised as names, because their names contain "ß" or a title ("Dr.").
- The kreisfreie-Stadt OB files write "Name, Vorname, Dr.", so 37 given names ended in a title.

**Fix.** `code/mayoral_elections/th_wahlvorschlag.R` splits the string in stages 01 and 01b; the scraped intermediate is unchanged.
- **Name:** the name loses the bracket, and academic titles leave the name parts.
- **Party:** the Träger in the bracket becomes `candidate_party`. When there is no bracket, the party is `NA`, because the source does not say which party nominated the elected person. It may not be "Einzelbewerber": the same election can list a separate, redacted "Einzelbewerber".
- **Groups:** a string is a person only if every name word is capitalised and lower-case after the first letter. That rule excludes the joint lists; one group that passes it is pinned as not a person.
- **Genuine labels kept:** a list named after its candidate, such as "Wenzl" (Eisenach 2012) or "MarcoSeidel" (Tanna 2021), is a genuine Träger and stays.

**Effect (Thüringen only; every other state and both Landrat files are unchanged).**
- `mayoral_candidates`: 761 named rows (770 before, −17 non-persons, +8 winners). All 761 now have a gender (570 before).
- `mayor_panel`: 645 rows (647). `winner_party` is a party or group in 294 rows and `NA` in 351. `party_switch` is TRUE in 9 rows (35 before) and `is_new_party_mayor` in 277 (630).
- `mayor_panel_annual`: 2,775 Thüringen rows (2,851).
- `mayoral_unharm` / `mayoral_harm`: `winner_party` changed in 669 Thüringen rows.
- Thüringen person groupings are unchanged, but the IDs are sequential, so 472 panel rows carry a new `person_id` label.

`98_full_audit.R` G8 fails if a Thüringen party contains "Nachname,", in `mayoral_candidates` or as the winner's party in `mayor_panel`. Calibration: 647 / 624 on the release of 2026-10-02, 0 after.

## 17. Panel identity, by-elections, Sachsen-Anhalt codes, Mecklenburg-Vorpommern (fixed, October 2026)

The audit of 2026-10-12 left seven pre-existing problems for a decision. Six are fixed; the seventh (labels) is kept as published, by decision.

**1. By-elections dropped.** The panel kept one winner per municipality-*year* (the highest vote share), so a second election in the same year disappeared together with its winner: Lützen 2010 kept Reichel (May) and lost Könnecke (December by-election). It now keeps one winner per election *cycle*: records less than 60 days apart are one cycle, later ones are kept. 116 municipality-years outside Hessen now hold two elections (Bayern 104, Sachsen-Anhalt 12). Hessen was already exempt (§15).
- Bayern's `flag_superseded` rounds (a Hauptwahl without majority repeated by a Neuwahl) used to vanish in the same dedup; they are now dropped explicitly before the panel (34 rounds, Tittling 2011). A date is dropped only if every round on it is superseded: Würzburg's 1948 council vote has a failed first ballot and the decisive one on one day.
- The same person elected twice in one year keeps one row, the later election (21 cases, mostly repeats after an annulled election and early post-war ballots).

**2. Bayern incumbents with two ids.** The person key is the workbook's first-Amtsantritt date. The 2026 Mandatsträger file dates a re-elected mayor's first Amtsantritt a few days off (Rattiszell: 2007-06-19 vs 2007-06-18), so the 2026 round started a new person. 03 now merges a key into the preceding key of the Gemeinde when the names agree, or when the dates lie within 120 days, the list is the same, the names do not contradict and the later key's Amtsantritt lies more than a year before its own first election (`first_amtsantritt_merge`: 13 keys, 19 rows). The last condition matters: without it the rule merged an appointed 1945 mayor of Straubing with the one elected in 1946, and repeated elections (1996, 2002) with their predecessors.

**3. Two people under one id.** A name key holds a surname and an initial. Split now on an incompatible given name plus a gender or birth-year conflict (Hotopp Angelika/Andy, Hampel Ingo/Ingeburg), and on reviewed pins in `metadata/person_split_pins.csv` (Vieker father/son, Espelkamp 2020). An independent review of all 37 people with incompatible given names (`docs/person_split_review_2026-10-12.csv`, with sources) found 33 to be one person each (Gerd/Gerhard, Pit = Peter, "K.-D." = Klaus-Dieter, added given names), 3 to be two people, and could not decide Hampel. The Statistisches Landesamt Sachsen-Anhalt settled it on 16.10.2026 after asking the Verbandsgemeinde Goldene Aue: Ingo and Ingeburg Hampel are two people (Ingo first, Ingeburg until Riethnordhausen joined Wallhausen on 01.09.2009). The split is now pinned in `person_split_pins.csv` too. Eggenstedt (Hotopp), asked in the same e-mail, was answered on 21.10.2026 after the office asked the Stadt Wanzleben-Börde: two people as well. Angelika Hotopp was mayor from 12.06.1994 to 11.07.2001; Andy Hotopp was appointed ("ernannt") mayor on 12.07.2001 and again on 12.07.2008. The appointments start the terms won at the direct elections of 06.05.2001 and 16.03.2008, so GERDA keeps those rows as elections dated on polling day, not as council choices (`flag_elected_by_council` = FALSE), and the annual panel gives 2001 to Andy Hotopp, as it gives every election year to the winner. GERDA does not use Sachsen-Anhalt Amtsantritt dates; the StaLA workbook dates the 2008 one 16.07.2008. The Hotopp pin already existed (gender rule plus workbook evidence) and now cites the answer (§30c: 4 pins). Both e-mails are in `raw/sachsen_anhalt/correspondence/` (gitignored: the Hampel answer gives birth dates). The alias table had joined Ellefeld's Heinrich Kerber (1994-2013) to Jörg Heinrich Kerber (from 2013), his son; it is gone. Sachsen lists every given name from about 2013 on; the August 2026 aliases assumed the last one is the name in use, which holds in 16 of 17 checked cases — Ellefeld, where the son carries his father's name last, is the exception, and the rule can fail the same way elsewhere.

**4. Positional ids.** Every added election or split renumbered the people after it (2026-10-09: 1,582 ids changed). Ids now come from `metadata/person_id_registry.csv`, seeded from the published panel (commit 176259e): 97.7 % of published person-elections keep their id, 976 published ids are retired (mostly one id of each pair the October 2026 lineage links merged), and new people get ids after the highest ever issued. A map from the uncommitted 2026-10-12 build to the registry ids (2,914 ids, mostly Brandenburg and Niedersachsen) is `docs/person_id_map_2026-10-12_to_registry.csv`, gitignored.

**5. List labels naming a withheld candidate — kept as published (decision 2026-10-12).** Bayern, Sachsen-Anhalt and Thüringen losers are withheld, but some lists are named after their candidate, and the label is published as the source prints it: 9 Bayern rows (8 mayoral, 1 Landrat) and two Thüringen labels ("Wenzl", Eisenach 2012, a genuine Träger, see §16; "Orzechowsk", a database label of 2004). The label is the official name of the Wahlvorschlag, not a candidate field.

**6. Sachsen-Anhalt: one code, two Gemeinden.** The source files some elections of then-independent Gemeinden under the code of the town they later joined. Where two Gemeinden shared a code on one day, the parser already re-coded them from the crosswalk, except three pairs whose "AGS aktuell" contradicts the crosswalk: Heideloh/Großzöberitz 2001, Cosa/Diebzig 2001, Dambeck/Dannefeld 1994. One elected mayor per pair stayed unseated (Blaha, Feuerborn, Blankau). A candidate code in the Kreis of the given code is now accepted. The source's "AGS aktuell" is itself wrong for Dannefeld: it carries Salzwedel's, copied with the code of its alphabetical neighbour Dambeck, while Dannefeld lies in Gardelegen (its own 2008 row, electorate 370 in 2001 and 350 in 2008). The same defect on *different* days is now repaired too: 17 elections, among them Liesten and Wieblitz-Eversdorf 2008 under Salzwedel, ten 2008/2009 Gardelegen villages, Kläden 2009 (of two namesakes only one harmonises to the source's town) and Schinne, Schwarzholz and Uchtdorf 2010 (merged during the election year, so their code is the previous year's). With the year dedup these had been read as one election of the town; with the cycle rule two villages' mayors within 60 days would have been one cycle. The remaining codes with two names are renames and spellings, plus Aue-Fallstein 2003 under Atzendorf's code with Atzendorf's "AGS aktuell" (no recovery possible).

**7. Mecklenburg-Vorpommern.** The Landeswahlleitung's overview (2026-10-12) showed Wismar's 2026 Bürgermeisterwahl missing (the LAIV publishes no Wismar 2026 file) and Rügen's Landrat of May 2001 seated although her Ja/Nein runoff failed the 25 % quorum. Wismar is transcribed from the city's official Bekanntmachungen; the Rügen cycle carries `flag_decisive_round_missing` and seats nobody.

`99_audit.R` §30 pins all seven (30a-30f; the labels are documented, not tested) and is calibrated: every check fails on the build of 2026-10-12 before the fixes. §26e and §28b were re-counted (164 predecessor terms / 109 people; 69 variant-link people / 176 terms).

## 18. Rheinland-Pfalz: the 2026 edition of the source, and errors in both editions (October 2026)

The Statistisches Landesamt Rheinland-Pfalz sent the 2026 edition of its Direktwahlen workbook on 12.10.2026 (Az. 111-AD26-1436). It covers every direct election of the Landräte, the Oberbürgermeister of the kreisfreie Städte and the Bürgermeister of the verbandsfreie Gemeinden and Verbandsgemeinden from 1994 to 13.09.2026, but names only the elected person (no other candidate), with term start and end and a marker for women. The 2025 edition, which lists every candidate with first-round and runoff shares, stays the base.

**Compared row by row** (910 of 928 elections matched on unit and date; name, share and runoff agree in 99 %), each edition has errors the other does not:

- **2025 edition wrong, corrected** (pinned in `metadata/rlp_source_corrections.csv`, each with its evidence; every pin checks the cell's old value first):
  - Landrat Rhein-Pfalz-Kreis 2017 (Körner) carried the code of Südliche Weinstraße;
  - Kusel-Altenglan and Rengsdorf-Waldbreitbach 2017 carried a predecessor's and an invalid code (the edition itself codes both differently in 2025);
  - Wörrstadt was dated 28.10.2019, a Monday after its own runoff date (true: 28.10.2018);
  - Koblenz 2017: Langner's first-round and runoff shares were in each other's columns (42.82 % and 69.8 %);
  - Leiningerland 2025: the two shares were swapped, so GERDA seated Rüttger (CDU); Krauß (SPD) won with 53.43 % (official notice);
  - Lauterecken-Wolfstein 2018: the runoff (Müller 52.23 %, Eckert 47.77 %) was missing;
  - Landrat Rhein-Lahn-Kreis: Denninghoff (not "Dennighoff") was elected on 13.03.2022, not on Ahrweiler's 23.01.2022; also Lars Denninghoff, Aar-Einrich 2022;
  - Andernach 2022: only the winner's share was given; the official notice adds the other three candidates.
- **2025 edition read wrongly by GERDA, fixed in code:** eight Verbandsgemeinde elections held with the Landtagswahl of 27.03.2011 (Altenahr, Treis-Karden, Rheinböllen, Monsheim, Wörrstadt, Eisenberg (Pfalz), Altenglan, Landau-Land) have their date typed as text. `read_excel()` read it as NA and the parsers' fill-down gave them the previous election's date, 07.11.2010. The October 2026 parser comment had blamed the source for printing the date only once per polling day.
- **2026 edition wrong, not used:** codes of Neustadt 2017 (that of Ludwigshafen), Budenheim 2018, Sinzig 2025, Monsheim and Nordpfälzer Land 2019; Loreley dated 2019 with its runoff in 2020; a runoff on a Tuesday (Wörth 2023); the names Orhten, Lüttkefedder, Fdenzer, Hechler "Dieter" (Ralf Dieter Hechler); share and turnout swapped (Bitburg-Prüm 2021, Morbach 2019, Daaden-Herdorf 2021, Hermeskeil and Oberes Glantal 2024); Kaiserslautern 2023 and Linz 2022 shares; Andernach 2022 marked as a runoff (it was decided in the first round); Brohltal 2023 missing.

**Added from the 2026 edition:** 35 elections, the 34 after the 2025 edition's last day (29.06.2025) and Vordereifel 2024 (Schomisch, who died in office on 03.01.2026, hence the election of 22.03.2026). They pass checks that stop the build otherwise: Sunday dates, a runoff 14 or 21 days after its first round, a majority winner, a unit the 2025 edition knows, no election twice. They are winner-only.

`99_audit.R` §31 pins all of the above and is calibrated: it fails on the build of 2026-10-12 before the 2026 edition.

## 19. Bayern: register and candidate-list names withheld (LfStat refusal, 16 October 2026)

The printed *Bürgermeisterverzeichnisse* 1978–2014 and the Statistische Berichte B VII 3-2 / 3-7 (candidate lists 2008–2026, OB and Landräte) name the elected mayors and Landräte with birth year and profession. The Bayerisches Landesamt für Statistik declined permission to redistribute these names, birth years and professions in GERDA (e-mail of its data protection officer, 16.10.2026). It cites § 9 Abs. 3 of its AGB (no electronic redistribution of Verzeichnisse) and argues that the GLKrWO reserves publication of the personal data to the Landesamt, so other databases lack a legal basis. The reply and the AGB are in `raw/bayern/lfstat_verzeichnisse/correspondence/` (gitignored).

- **Public files:** no Bayern name, birth year, profession, name origin or migration background before 2026, in `mayoral_candidates` or `landrat_candidates`. The winners' gender, derived from the withheld given name, IS published (user decision 2026-10-16): 04 copies it from the restricted twin (17,782 rows). That withholds 17,882 mayoral and 644 Landrat winner rows that the unreleased build had named; losing candidates were withheld already. The 2026 winners stay named from the Landesamt's *Mandatsträger* file, which is not covered by the request.
- **Restricted twins** (`final_restricted/`, gitignored) keep everything, as before.
- **Panel:** person ids unchanged. The register-based corrections of the first-Amtsantritt key (`first_amtsantritt_split`, `first_amtsantritt_merge`) read the names from the restricted twin, and only the pseudonymous ids are published. The Bayern gender columns are filled as before (19,688 terms); migration background and name origin only for the 1,914 terms won in 2026.
- **Not in git:** `by_verzeichnis_elections.csv` / `by_verzeichnis_persons.csv` (stay gitignored for good), the parser's name pins and the audit fixtures that name register entries (`lfstat_verzeichnisse/*_restricted.csv`).
- **Runoff names, 2026:** a runoff row joined to its first round by list elimination (01b Step 2c) now passes on the public *Mandatsträger* name. Before, 7 winners named in the published data had lost their name in the working tree.
- `99_audit.R` §24a (public file holds no Bayern name, birth year, profession or name-origin field before 2026; calibrated on the 2026-10-13 build, which published 17,882 named winners; and gender is present on ≥ 17,000 winner rows) and §24c–g, §29c (now on the restricted twin); Landrat `99_audit.R` (no BY Landrat personal field before 2026).

## 20. Brandenburg: hauptamtliche elections 2004–2013 from the Gemeinden's result reports (October 2026)

The Geschäftsstelle des Landeswahlleiters sent scans of the result reports (*Ergebnismeldungen*) the Gemeinden had filed with it, 2004–2018 (four e-mails of 15.10.2026; `raw/brandenburg/lwl_ergebnismeldungen_2004_2013/`, README there; PDFs gitignored). `00_bb_lwl_scans_parse.py` reads a transcription checked three ways, and checks every figure against each page's own subtotals:
- two independent reads (the OCR layer and a visual read);
- a blind third read of 71 pages, which agreed on every count, vote, date, name and finding.

- **Added:** 119 elections, 164 rounds, 115 Gemeinden, 2004–2012, with full results. The hauptamtliche gap of 2004–2009 (4 winner-only elections before) is closed for the elections the archive holds (85).
  - Four 2008 elections that the B VII 3-6 report held as winner-only now have full results. That parser drops its own copy once it has checked that both name the same elected person.
  - The first round of Schöneiche bei Berlin (22.04.2012) is missing from the scans. It comes from the Landeswahlleiter's own result page, archived by the Internet Archive (`wayback/`).
- **Panel:**
  - 115 new terms.
  - 96 existing Brandenburg terms end earlier, because an intervening election is now known; none runs over 12 years.
  - One published `person_id` changes: Groß Leine 1998 joins its mayor's Märkische Heide terms through a merger link via the newly known 2006 election. The old id is retired.
- **Checked against outside sources:**
  - 22 sampled winners: all 22 agree on winner and date, and on the figures wherever a source gives them. Sources: Amtsblätter, Wikipedia, archived LWL pages.
  - The LWL's own result pages, archived by the Internet Archive (984 captures, 2008–2017), show 139 of the 164 new rounds; the other 25 are mostly from before the site existed.
    - 135 agree in every figure.
    - 3 agree in every figure but carry the site's wrong date: it dates three Märkisch-Oderland elections of 27.09.2009 as 28.09.2008. The captures show the elections appearing only in 2009.
    - The one difference is Belzig's corrected electorate, which the site repeats from the Gemeinde.
    - Across all 738 archived rounds, no other figure differs from GERDA.
- **Overlap with the workbook (47 elections, 61 rounds), left in the workbook:** all candidate votes agree. Any further difference makes the parser stop until it is pinned.
  - Liebenwalde 11.09.2011: 3,704 Wahlberechtigte on the Gemeinde's form, which is consistent with its own turnout, threshold and the council and Landtag electorates. The workbook and the LWL's own website have 3,407. Kept as published, pending the Landeswahlleiter's answer.
- **Corrected with evidence:**
  - Belzig 28.09.2008, Wahlberechtigte 5,779 → 9,779. The evidence is the Gemeindevertretung election of the same day and register (AfS SB B VII 3-5 – 5j/08 p. 108) and the Kreistag (9,790). The LWL's website repeated the Gemeinde's misprint.
  - Schwarzheide 2008: the page labelled "Hauptwahl" on 12.10.2008 is the runoff; B VII 3-6 2008 dates the first round 28.09. That first round has no result here (`n_candidates_hw` NA).
- **Kept as printed, flagged:**
  - Eichwalde and Pritzwalk 2009: the two rounds print electorates 7 % and 10 % apart. The runoff figure fits the other counts, and the LWL website carries the same figures.
  - Four hand-computed thresholds are off by one, with no effect on any outcome.
- **One person, two spellings across rounds** (4 Bewerber in 3 cycles; Träger labels differ for 10 Bewerber in 8 cycles): linked by `pair_key` from the Hauptwahl's list of runoff qualifiers, so the wide pivot does not split them. Without it, Templin 2010 showed the winner twice.
- **Privacy:** losing Bewerber are named in the restricted twin only (295 people). The elected person's own rows are named, chosen by row and not by name, so a namesake is never published. Wahlvorschlag labels that carry a loser's surname ("Einzelwahlvorschlag …", 9 elections) were kept as printed (user decision 2026-10-16); since 2026-10-19 they are the plain list type in the public files (§21).
- **A runoff below the 15 % quorum** (none in the delivery) seats nobody: `flag_decisive_round_missing`, with `flag_elected_by_council` on the cycle. Both popular-vote rounds stay in `mayoral_unharm`, and the panel ends the previous term there. Tested end to end by mutation.
- **Open, user decision 2026-10-16:** the workbook's and the portal's own losing candidates 2010–2026 are still published (756 named non-winner rows in 271 elections), unlike every other Brandenburg source. They are to be restricted to the twin as well, in a separate change.
- **Separate fix, all states (audit 2026-10-19):** `fread()` left RFC-4180 doubled quotes in labels that contain quotes (`LV ""Wir für X""`). 01/01b now collapse them: 10 unharm/harm and 13 candidate labels outside Brandenburg change, and the Brandenburg ones with them. Nothing else changes.
- `99_audit.R` §34 pins coverage, vote integrity in both rounds, privacy and five fixtures. §27i (223 terms / 85 people across the 2001–2003 mergers) and §27j (exactly 223 result-less first rounds) moved with it.

## 21. Brandenburg: every losing candidate restricted; raw inputs out of the repository (19 October 2026)

Until 19.10.2026 the public files named the losing candidates of the Landeswahlleiter workbook 2010–2026 and its portal fallback (756 rows in 271 elections, gender or name origin derived for 747) and of the Landrat portal pages (124 rows in 34 elections). Every other Brandenburg source was restricted already. Vincent decided on 16 and 19 October 2026 that all of them go to the restricted twin.
- **01b and the Landrat combine** add Brandenburg to `withheld_loser_states`, the rule already used for Bayern, Sachsen-Anhalt and Thüringen: a non-winner row keeps votes, shares, rank and Wahlvorschlagsträger, but no name and no name-derived field.
- **Own-name labels (user decision 2026-10-19).** An independent candidate's label is often their own name ("EB Muster", "Einzelwahlvorschlag Max Muster"; invented), which would name the loser anyway. In the public files it becomes the plain list type when nothing but an independent-candidate prefix is left once the person's own name, initials and titles are removed (`code/shared/bb_own_name_label.R`). A list whose name merely contains a surname is unchanged; the twin keeps the label. 228 mayoral and 6 Landrat labels change, and 7 `winner_party` values in `mayoral_unharm`/`_harm` (the leader of a Hauptwahl who lost the runoff). The tracked copies carry the generic labels, so a clean clone reproduces this. `04` compares `candidate_party` with the twin except on these rows; `99_audit.R` §27n and the Landrat audit check the rule, and fail on the build before.
- **Raw files.** The workbook (supplied for research under § 25 BbgDSG), `bb_lwl_parsed.csv`, the portal scrape `bb_bm_parsed.csv` and the Landrat `BB_*.html` pages are gitignored. Their versions in earlier commits stay in the git history; whether to rewrite it is open.
- **Tracked copies.** `bb_lwl_parsed_public.csv` (written by 01b) and `bb_landrat_parsed_public.csv` (written by `01_landrat_combine.R`) hold the same rows with only the elected persons named. Every row carries an opaque pairing key that is unique within its Hauptwahl–Stichwahl cycle and links nobody across elections, so the two rounds pair exactly as by name. 01/01b and the combine read them when the full files are missing.
  - A build from the copies alone reproduced every public output, the person_id registry and the tracked lookups `identical()` to the full build (19.10.2026).
- **Effect.** Only these Brandenburg fields change, nothing else in any state: 756 + 124 names, 747 genders, 756 name origins and migration backgrounds. The tracked `name_origin_lookup.rds` no longer holds the 642 first-and-last-name pairs that came only from these losers, and `gender_guesser_lookup.csv` loses 78 first names.
- `99_audit.R` §27c checks every Brandenburg non-winner row in all years and all name-derived fields. §27m checks that the tracked copy names only people the public file names. The Landrat audit checks the same for its copy. All were calibrated on the previous build and with a planted name.

## 22. Brandenburg 2001–2017: the Landeswahlleiter's archived result pages (19 October 2026)

The Internet Archive kept the Landeswahlleiter's old result pages (`wahlen.brandenburg.de/bmwahlen/<AGS10>/h|s`, 984 captures 2008–2017). Each shows a Gemeinde's latest election on the same MAIS form as the result reports of §20. `00_bb_lwl_archive_parse.py` runs after the B VII 3-6 parser and decides election by election (`raw/brandenburg/lwl_bmwahlen_archive/README.md`):
- **Held with counts** by the workbook, the result reports or a B VII 3-6 table (218): compared, left out. Every count and vote agrees, except the pinned Belzig electorate.
- **Held winner-only by B VII 3-6** (411): replaced by the archived rounds. A round the archive lacks is carried over (18 elections), so each election comes from one file and its rounds pair by `pair_key`. The elected person must be the report's. The 472 replaced rounds are listed in `bb_lwl_archive_supersedes.csv`, which 01/01b drop, and only together with the archive file.
- **Not in GERDA** (7): added. Two runoffs come without their first round. One council choice after a rejected single candidate (Rosenau 2014). Pinnow 2008's failed Hauptwahl joins the report's council record.
- **Effect:**
  - `mayoral_unharm` gains 11 Brandenburg rounds; Brandenburg rounds without counts fall from 2,901 to 2,448;
  - `mayoral_candidates` gains 392 rows;
  - the panel gains 6 terms; Lychen's 2002 term is re-dated to its first round of 21.04;
  - no `person_id` changes;
  - nothing outside Brandenburg changes;
  - 387 more losing Bewerber are in the restricted twin only.
- **Site defects, pinned:**
  - five misdated pages, left out;
  - an incomplete early capture (Falkensee 2007);
  - three pages without a result;
  - five misprinted names, corrected to the report or the other round;
  - salutations in the 2014 pages;
  - off-by-one thresholds and misprinted percentages;
  - electorates differing between rounds.
- **A rule found on the way:** a Hauptwahl that went to a runoff elected nobody, so all its rows are `is_winner = FALSE`. Otherwise, with a winner-only runoff carried over, the first-round leader would stand as a second winner.
- `99_audit.R` §35 pins counts and fixtures (Lychen 2002, Guben 2016, Forst 2015, Pinnow 2008, Falkenberg/Elster 2017). §27f, §27i and §27j moved with it.

## 23. Party switches compared labels, not parties (fixed, 19 October 2026)

- **Problem:** `party_switch` and `is_new_party_mayor` compared the normalised Wahlvorschlag labels. These counted as switches:
  - two successive local lists ("Freie Wählergemeinschaft" → "Bürgerblock");
  - one list spelled differently by two sources;
  - a joint list and one of its members (CSU → "CSU/Freie Wähler").

  `party_switch` was 1 in 48.8 % of the comparable rows, 50.0 % in Bayern.
- **Fix** (user decision 19.10.2026): `code/shared/party_family.R` sorts the 14,700 labels into ten families, and the panels compare families. See the mayoral README, "Party families".
  - New columns: `candidate_party_family` in `mayoral_candidates` and `landrat_candidates`, and `party_family` next to `winner_party` in the four panels, `mayoral_unharm`, `mayoral_harm` and `landrat_unharm`.
  - The election-level files get the family in `01b_mayoral_candidates.R`, which matches each round's label to its candidate (the own-name rule needs the person; it decides 235 mayoral and 20 Landrat rounds). `02_mayoral_harm.R` takes it with `winner_party` from the dominant predecessor; the Landrat combine computes it for its own rows. 02 and the combine stop if 01b has not run after 01.
  - The labels are unchanged.
- **Rules worth knowing:**
  - a joint list counts as each of its members: CSU → CSU/Freie Wähler is no switch, CSU → Freie Wähler/SPD is one;
  - minor parties compare as parties: BP → ÖDP is a switch;
  - supporters ("CSU und ihre Freunde"), an open list beside a party and a party branch do not make a list joint;
  - Sachsen-Anhalt's blank label is an Einzelbewerber, so 2,766 Sachsen-Anhalt rows become comparable (105 before);
  - an own-name label (Rheinland-Pfalz: "Muster") is an Einzelbewerber only when the name is known. 01b computes the family on the full frame, so the restricted twin and the public file agree;
  - `NA`: no label (Baden-Württemberg), "Sonstige", or a category mixing families (Nordrhein-Westfalen's "Wählergr./Einzelbew.", 274 rows).
- **Effect:**
  - `party_switch` is 1 in 22.8 % of 39,228 comparable rows; Bayern 21.3 %, Brandenburg 34.8 %, Sachsen 33.0 %, Nordrhein-Westfalen 32.1 %, Hessen 30.1 %;
  - `is_new_party_mayor` is 1 in 11,801 rows (20,219 before);
  - every other column and row of every output is unchanged.
- **Audits:** `99_audit.R` §36 pins:
  - the families of 22 frequent labels;
  - the twin agreement;
  - the agreement of the panel with the winner's candidate row (all but 17 Bayern register labels);
  - the switch rule on 30,331 successive pairs;
  - the counts;
  - the election-level files against the candidates (§36g).

  The Landrat audit checks `landrat_candidates` and `landrat_unharm`.
