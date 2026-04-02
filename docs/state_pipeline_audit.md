# State Election Pipeline Audit

Tracking document for bringing the state election pipeline (`01b` + `02b`/`04`/`05`) to feature parity with the federal municipality-level pipeline. Created March 2026.

## Comparison: New pipeline (01b) vs old Regionalstatistik pipeline (01)

The old pipeline (`01_state_unharm.R`) queried the DESTATIS Regionalstatistik/GENESIS API for 2006–2019, covering 14 states (missing HH/HB) with only 8 major party groups. It was replaced by `01b_state_unharm_raw.R` which reads raw Landeswahlleiter files directly. A systematic comparison of 27,503 matched municipality-year observations found:

**Briefwahl recovery in 6 states (old data had massive undercounts):**
- SN (Sachsen): +10–17% valid_votes recovery; 822/1,337 municipalities had >10% undercount
- MV (Mecklenburg-Vorpommern): +7–19% recovery; 746/1,515 municipalities >10% off
- BB (Brandenburg): +0.1–20% recovery; 412/1,251 municipalities >10% off
- TH (Thüringen): +4–6% recovery; 971/2,460 municipalities >10% off
- ST (Sachsen-Anhalt): +0–8% recovery; 88/437 municipalities >10% off
- BY (Bayern): +100% (definitional: Gesamtstimmen vs single-vote; not a quality fix)

**Party share impact:** The Briefwahl recovery shifts party shares because mail-in voters have different preferences. Mean absolute deviations per municipality: AfD up to 0.95pp (MV), CDU up to 0.50pp (TH), Linke up to 0.51pp (TH). The "other" column diverges 4–13pp everywhere because old pipeline tracked only 8 parties vs all individual parties now.

**7 states unchanged (zero difference):** SH, NI, NRW, HE, SL, BE — confirming Regionalstatistik was correct where Briefwahl isn't pooled. BW also unchanged because old pipeline had already manually patched the known Briefwahl bug.

**No degradation in any state.**

## Pipeline files

| Script | Stage | Description |
|--------|-------|-------------|
| `code/state_elections/01b_state_unharm_raw.R` | 1 (unharm) | Raw ingestion, party harmonization, vote shares |
| `code/state_elections/02b_state_harm_21.R` | 2 (harm) | Boundary harmonization to 2021 |
| `code/state_elections/04_state_harm_23.R` | 2 (harm) | Boundary harmonization to 2023 |
| `code/state_elections/05_state_harm_25.R` | 2 (harm) | Boundary harmonization to 2025 |

Reference implementation: `code/federal_elections/municipality_level/` (00, 01, 02, 03)

---

## Change 1: Bayern — Report Zweitstimme Only

**Status:** DONE

**Resolution:** After research, we keep Gesamtstimmen (not Zweitstimme). In Bavaria, both Erst- and Zweitstimme count equally for proportional seat allocation and the 5% threshold — unlike Bundestagswahlen where only Zweitstimme matters. Gesamtstimmen is the official metric reported by the Bayerisches Landesamt für Statistik.

**What was fixed:** The `invalid_votes` computation was wrong. Since each voter casts 2 ballots (Erst + Zweit), total ballots = `number_voters × 2`, and `invalid_votes = number_voters × 2 - valid_votes`. Previously it was computed as `number_voters - valid_votes`, yielding large negative values.

- 1946: Single-ballot (Erststimmen columns all NA in source), so `invalid_votes = voters - valid_votes` (unchanged)
- 1950-2013: Fixed to `voters * 2 - valid_votes`
- 2018-2023: Already correct (`invalid_erst + invalid_zweit`)

**Identity:** For BY 1950+: `valid_votes + invalid_votes = number_voters × 2`

**Lines changed:** `01b_state_unharm_raw.R` lines ~5700 (Phase 1), ~5876 (Phase 2), plus documentation comment at ~5597

---

## Change 2: `flag_naive_turnout_above_1`

**Status:** DONE

**Problem:** Turnout > 1 is silently capped at 1.5 with no flag. Federal pipeline has explicit flag.

**File:** `01b_state_unharm_raw.R` (lines ~6189-6211)

**Implementation:** Add flag BEFORE existing capping logic.

**Verification:** Count flagged rows, cross-reference with known Briefwahl allocation states.

---

## Change 3: `far_right`, `far_left`, `far_left_wLinke` Pooled Columns

**Status:** DONE

**Problem:** Federal pipeline has these aggregated columns; state pipeline doesn't.

**Files:** `02b_state_harm_21.R`, `04_state_harm_23.R`, `05_state_harm_25.R`

**Federal definitions:**
- `far_right`: afd, npd, rep, die_rechte, dvu, iii_weg, fap, ddd, dsu, die_heimat_heimat
- `far_left`: dkp, kpd, mlpd, sgp, psg, kbw
- `far_left_w_linke`: linke_pds + pds + far_left

**Implementation:** Use `intersect()` with `names(df_harm)` to handle missing columns.

---

## Change 4: Zero-vote Party → NA Recoding

**Status:** DONE

**Problem:** Parties with zero votes in entire state-year are coded 0. Should be NA to distinguish "didn't participate" from "got 0 votes." Federal pipeline does this.

**Files:** `02b_state_harm_21.R`, `04_state_harm_23.R`, `05_state_harm_25.R`

**Implementation:** For each election_year × state, identify parties where all values = 0, recode to NA.

---

## Change 5: Improved `total_vote_share`

**Status:** DONE

**Resolution:** Now sums ALL individual party columns (excluding derived: `far_right`, `far_left`, `far_left_w_linke`, `cdu_csu`) instead of just 8 major parties. Also added `perc_total_votes_incogruence = total_vote_share - 1` for continuous deviation measure. Flag threshold widened to ±0.1% (`>1.001 | <0.999`) to avoid flagging rounding artifacts.

**Results (harm_21):** median=1.0, max=1.072, min=0.90, flagged=3,896/67,613 rows.

**Bug found and fixed:** `flag_naive_turnout_above_1` from unharm was not in `metadata_cols`, causing it to be treated as a party column and double-counting total_vote_share. Fixed by adding it to `metadata_cols` in all three harm scripts.

---

## Change 6: SH 1983 OCR Re-extraction

**Status:** DEFERRED — see investigation notes below

**Problem:** 136/1079 municipalities have `number_voters=0, eligible_voters=0` due to left-page matching failure in `00_sh_1983_extract.py`. Right-page extraction (party votes) works fine.

**Root cause:** The PDF text layer is heavily garbled on many left pages. The script uses "100,0" percentage markers as anchors to find data rows, but many markers are unrecognizable OCR artifacts (e.g., "1C'O,C:", ":>c,c.", "co,o" on page 71).

**Investigation (March 2026):** Three approaches attempted:
1. **y-position anchoring** (use right-page row positions): Failed because y-offsets between left/right pages vary by -23px to +10px across page pairs, making position matching unreliable.
2. **Row-scanning** (find all data rows, match by order): Found too many rows (includes Kreis aggregate rows), causing worse order misalignment (553→474→840 OK varies).
3. **Broader marker regex**: Caught non-"100,0" values from other columns, introducing false positives.

All three approaches degraded the baseline (412 OK / 531 BAD). The original code is the best achievable with the current text-layer quality.

**Impact:** 135/1079 municipalities lack EV/NV → turnout=NA. Party vote shares are unaffected (right-page extraction works). Statewide coverage: 87.5% of VV recovered.

**File:** `code/state_elections/00_sh_1983_extract.py`

**Possible future approach:** True OCR re-extraction using Tesseract on high-DPI rasters (bypassing the broken text layer entirely). This would require significant effort and is deferred.

---

## Change 7: HE 1958/1962 `number_voters` Investigation

**Status:** DOCUMENTED (source data limitation, no code change needed)

**Problem:** 417/426 municipalities in HE 1958 and 418/427 in 1962 have `number_voters=0`. Only kreisfreie Städte (9 cities) and the Land total have Wähler data.

**Root cause (confirmed):** Raw XLSX (`Hessen_1946-2018_Landtagswahl_Gemeindeebene .xlsx`, sheet "1958") stores voter counts only for kreisfreie Städte and the Land total. Non-kreisfreie municipality rows have NA in cols 10 (Wähler insgesamt), 11 (mit Wahlschein), and 12 (ungültig). Only EV insgesamt (col 9) and party vote columns (13+) have data.

**Impact:** turnout is NA for 417/418 municipalities in 1958/1962. Party vote shares are unaffected. This affects only 2 of 21 HE election years.

**Code change:** Recoded `number_voters=0` → `NA` for HE 1958/1962 non-kreisfreie municipalities in the final section of 01b. This correctly represents "data unavailable" rather than "zero voters". `turnout` and `invalid_votes` are also set to NA for these rows. Cannot be solved — the source statistics office only published full participation data at the Kreis aggregate level for these two elections.

---

## Change 8: BB Data Quality Investigation

**Status:** DOCUMENTED — known issues, no immediate code fix

**Investigation summary (March 2026):**

### BB 1990: 20 rows with turnout > 1, 270 rows VV > NV
- **Turnout > 1.24** (AGS 12034120): NV=9,549 but EV=7,678. Likely DDR-era voter registration artifact in the first free Landtagswahl (Oct 1990) — voter rolls may not have been up to date.
- **VV > NV** (270 rows, max excess=5,545): Widespread issue — suggests systematic Briefwahl data is included in VV but not NV. Source: `bb_1990_ocr.csv` from scanned PDF.
- **Action:** No fix possible without re-digitizing source material. Flagged by `flag_naive_turnout_above_1`.

### BB 1994: 19 rows turnout > 1, 46 rows VV >> NV (up to 10× ratio)
- **Extreme cases** (AGS 12060064: NV=1,375, VV=13,456): The 10× ratio is NOT OCR rounding — it indicates systematic Briefwahl misallocation. The OCR script reads per-municipality results from the PDF, which may report total valid votes (including proportionally allocated Briefwahl) without a corresponding increase in the NV count.
- **Source:** `bb_1994_ocr.csv` from scanned PDF `Brandenburg_1994_Landtagswahl.pdf`
- **Action:** Would require checking the PDF for how Briefwahl is reported per municipality. Deferred.

### BB 1999: 4 rows VV > NV (max excess=917)
- Minor issue, likely same Briefwahl allocation pattern.

### BB 2009: 223 rows VV > NV — FIXED
- **Source:** Official XLS (`Brandenburg_2009_Landtagswahl.xls`)
- **Root cause (found March 2026):** Bug in 01b — each XLS sheet has an Anzahl (absolute counts) section followed by a "%" (percentage) section. The code was reading BOTH sections, treating percentages (e.g., 63.3% turnout) as additional vote counts. When aggregated by AGS, these inflated valid_votes beyond number_voters.
- **Fix:** Added `pct_row` detection to stop reading before the "%" marker row. Raw rows went from 834→419. All 223 negative invalid_votes cases eliminated.
- **Remaining:** 0 negative invalid_votes for BB 2009.

---

## Execution Log

| Date | Change | Action | Result |
|------|--------|--------|--------|
| 2026-03-18 | 1 (BY invalid_votes) | Fixed computation for 1950-2013; documented Gesamtstimmen | invalid_votes now positive; identity voters×2 = valid+invalid holds |
| 2026-03-18 | 2 (turnout flag) | Added flag_naive_turnout_above_1 before capping | Flag inserted at line ~6204 in 01b |
| 2026-03-18 | 3 (pooled cols) | Added far_right, far_left, far_left_w_linke to all 3 harm scripts | 11 far_right, 6 far_left cols used |
| 2026-03-18 | 4 (zero→NA) | Added zero-vote party → NA recoding in all 3 harm scripts | Applied via pivot_longer/wider per state-year |
| 2026-03-18 | 5 (total_vote_share) | Sum all party cols + perc_total_votes_incogruence | Fixed metadata_cols bug; median=1.0, max=1.07 |
| 2026-03-18 | 6 (SH 1983) | Investigated 3 approaches; all degraded baseline | DEFERRED — text layer too garbled |
| 2026-03-18 | 7 (HE 1958/62) | Confirmed source data gap in raw XLSX | Documented; no code change |
| 2026-03-18 | 8 (BB quality) | Documented turnout/VV>NV issues by year | Known Briefwahl artifacts; flagged |
| 2026-03-18 | Audit: NA filter | Added explicit pre-harmonization filter to 02b, 05 | Consistency with 04 |
| 2026-03-18 | Audit: HE NV→NA | Recode HE 1958/62 number_voters 0→NA in 01b | Correct representation of missing source data |
| 2026-03-18 | Audit: neg IV clamp | Clamp negative invalid_votes→0 in 01b | BY 1954 (1 row), HE 1946 (1 row) |
| 2026-03-19 | BB 2009 % rows fix | Stop reading before "%" section in XLS sheets | Raw rows 834→419; 223 negative IV eliminated (was reading percentage rows as votes) |
| 2026-03-23 | RP 1979–2016 | Added 9 elections from `LW_RLP_1979_2021.xlsx` (Landesstimmen, no turnout) | 22,951 RP rows total (10 elections); party sums = 1.0000 |
| 2026-03-23 | HH 2025 | Added from `BUE2025_e01_Landesliste.xlsx` (Statistik Nord) | 1 row, 16 parties, EV=1,313,043 |
| 2026-03-23 | HB 1946–1995 | Added 14 post-war elections from converted BIFF→XLSX files | 27 rows (1 HB-only in 1946, 13×2 for HB+BHV); pct-only data |
| 2026-03-23 | HB 2011 | Hardcoded from Amtsblatt/Faltblatt PDF (wahlen-bremen.de offline) | 2 rows; 5-vote Gesamtstimmen |
| 2026-03-23 | HB 1995 Sonstige fix | Fixed double-counting: "Sonstige" included DVU+AfB as subcategories | other = Sonstige − DVU − AfB |
| 2026-03-23 | valid_votes NA filter | Changed `filter(!is.na(valid_votes) & valid_votes > 0)` → `filter(is.na(valid_votes) \| valid_votes > 0)` in 01b | Preserves HB pre-1999 pct-only rows |
| 2026-03-23 | Harm weight imputation | All 3 harm scripts: impute valid_votes from number_voters/eligible_voters when NA | 50 rows imputed; HB 1991/1995 now have proper party shares in harm |
| 2026-03-24 | NRW 1947 OCR | Visual reading of 150 WK from scanned PDF → 84 Kreise via `00_nrw_1947_compile.py` | 8 parties; VV=5,028,892 exact; CDU+4/FDP-1/KPD+2/Z-5 scan residuals |
| 2026-03-24 | NRW pre-1975 update | Merged 1947 into `nrw_pre1975_kreis.csv` (637 rows, 7 elections); added `rwvp` column and 1947 date to `01b` | County-level only, synthetic AGS, unharm only |

---

## Comprehensive Cross-Pipeline Audit (March 2026)

### Verified correct (no action needed)
- **Briefwahl handling**: Federal pipeline allocates shared mail-in districts; state data is pre-aggregated — no allocation needed
- **AGS corrections**: Each harm script has corrections appropriate for its target year (self-mapping handles newer codes)
- **HH 1982 dual elections**: Two rows (June + Dec) correctly in unharm; excluded from harm by year filter
- **CDU/CSU consistency**: cdu_csu = csu for BY, = cdu for others. No double-counting
- **rep/die_republikaner_rep**: Mutually exclusive across state-years (no double-counting in far_right)
- **far_left parties**: Federal includes v, spad, bsa but these don't exist in state data
- **Election dates**: None missing in any output
- **Party zero→NA**: Applied correctly per state-year

### Self-mapping approach (validated March 2026)
When an AGS code doesn't exist as a *source* in the crosswalk but IS a valid *target* code (ags_21/23/25), we map it to itself with `pop_cw=1`. This handles two cases:
1. **NI SpreadsheetML retroactive coding**: NI publishes all historical elections under current AGS codes. E.g., 03153019 (Stadt Langelsheim, formed 2016 from 4 old municipalities) is used for all NI elections 1998–2022. The crosswalk maps old codes → ags_21=03153019, but the NI data already uses 03153019. Self-mapping avoids an unnecessary split-and-reassemble round-trip.
2. **Post-reference-year elections**: When an election occurs after a merger and the data uses the new code (e.g., RP 07132502 formed 2021, TH 2024 mergers), self-mapping correctly recognizes the data is already at target boundaries.

### Known limitations (documented, no fix possible)
- **BY Gesamtstimmen**: valid_votes = Erst+Zweit combined (both count for proportional allocation in Bavaria). Identity: valid+invalid = voters×2 for 1950+
- **HE 1958/62**: number_voters/invalid_votes not reported in source for non-kreisfreie municipalities
- **SH 1983**: 135 municipalities lack EV/NV due to garbled PDF text layer
- **BB 1990/94**: Briefwahl misallocation artifacts (up to 10× VV/NV ratio in 1994)
- **NRW 1947-1970**: County-level only (synthetic AGS `050xx000`), cannot be harmonized. 1947/1950 extracted at Wahlkreis level (150 WK) then aggregated to ~84 Kreise. 1947 has minor scan-read residuals (CDU+4, FDP-1, KPD+2, Z-5 vs official statewide totals). No turnout data for any pre-1975 year (only valid_votes and party counts). The source PDF (`Nordrhein-Westfalen_1947_Landtagswahl.pdf`) contains three elections in interleaved rows (a=1947, b=1949 Bundestag, c=1950 Landtag) — row identification by party presence pattern (1947 has no DP/RSF; 1949 has RSF; 1950 has DP)
- **BY 1994-2013**: eligible_voters not available in Stimmabgabe source files
- **HB 1946–1995**: Only vote share percentages available (no absolute counts). `valid_votes`/`invalid_votes` = NA in unharm. Harm scripts use `number_voters` as proxy weight (exact for 1:1 municipality mapping).
- **HB 2011**: Per-party shares hardcoded from PDF; no per-Ortsteil granularity. wahlen-bremen.de was offline (503).
- **RP 1979–2016**: No turnout data (`eligible_voters`/`number_voters`/`invalid_votes` = NA). `valid_votes` available from Gesamtsumme LS. Source: Landeswahlleiter file with data pre-harmonized to current municipality structure.
- **RP 1979–2001: Source data deviations from official Landesergebnisse.** The Wahlleiter's Excel is missing 5–6 municipalities for these years (Langweiler, Trimbs, Hatzenport, Urbar, Leienkaul — founded after 1979; Dierfeld absent from all years). Municipal-level sums fall 0.07–0.31% below official totals. Party shares deviate by ~0.3pp (CDU under, SPD over) for 1979–1991, consistent with missing small rural CDU-leaning municipalities. From 2006: ≤0.16% gap; 2011/2016: exact match. This is a source data limitation.
- **RP 2021: 19 Zusammenlegungen.** Municipalities with very low voter counts were merged into neighbors for the first time in 2021 — these 19 municipalities are not separately listed in the source file.
- **RP 1979–1987 in harm**: Excluded because crosswalk coverage starts ~1990. The source data already uses current AGS but the pipeline cannot verify this via crosswalk matching.
- **HB pre-1991 in harm**: Excluded by crosswalk coverage. Only 1991+ appears in harmonized files.
- **BaWü GENESIS API Briefwahl undercount (fixed Feb 2026)**: The GENESIS Regionalstatistik API only reported Urne (in-person) valid votes for non-kreisfreie BaWü municipalities, missing all Briefwahl. This undercounted valid_votes by 13–17%. Fixed by replacing API data with raw Excel/CSV files from the Statistisches Landesamt for BW 2011/2016.
- **Fallback join duplication pattern (fixed Feb 2026)**: When harmonization scripts apply a year−1 fallback join for unmatched AGS codes, the fallback must be applied ONLY to unmatched rows. If applied to all rows (including already-matched multi-target AGS), it creates duplicate rows. For BaWü, this inflated eligible_voters by exactly 13,348 in harm_23/harm_25. Fixed by splitting into matched/unmatched before the fallback join in all three harm scripts.
- **Crosswalk weight rescaling (fixed Feb 2026)**: `05_build_23_25_ags_crosswalks.R` chained weights from 1990→2023 and 2023→2025 but never rescaled `pop_cw`/`area_cw` to sum to 1.0 per (ags, year). Affected 755 AGS-year combinations. All `_25` harmonized datasets were impacted.

---

## Change 12: Row removal → flag columns (March 2026)

**Status:** DONE

**Problem:** Three filters silently removed rows from final output files, breaking balanced panel structure for downstream users.

**Changes:**

1. **State unharm (`01b_state_unharm_raw.R`):**
   - `filter(is.na(valid_votes) | valid_votes > 0)` → new flag column `flag_no_valid_votes` (1 = gemeindefreie Gebiete / uninhabited areas with VV=0). Rows kept.
   - MV/ST Briefwahl-only entity removal → new flag column `flag_briefwahl_only` (1 = EV=0 but VV>0, e.g., BB 1990 mail-in misallocation, SH 1983 garbled PDF, NRW 1966 major cities missing EV).

2. **Federal county unharm (`01_federal_cty_unharm.R`):**
   - `filter(!grepl("999$", ags) | eligible_voters > 0 | ...)` → new flag column `flag_briefwahl_agg` (1 = fake county codes like 12999/13999 used for unattributable mail-in votes in 1994/1998). Rows kept.

3. **CDU/CSU logic fix** (same session):
   - State unharm: `cdu_csu` now sums CDU+CSU when both present (MV 1990 DSU, 976 rows affected).
   - Federal county unharm: Same fix for `cdu_csu = ifelse(csu > 0, csu, cdu)` → `rowSums(across(c(cdu, csu)))` (SL 1957, 8 rows affected).

4. **Federal muni harm_21:** `ags_21 = as.numeric()` → `as.character()` (preserves leading zeros).

**Note:** BW gemeindefreie Gebiete (AGS ending 99x) are still dropped inside the BW per-state parser (line ~3444 of `01b`). Changing this requires restructuring the BW aggregation logic — flagged for future work.

---

## Change 13: Municipal elections SN/ST 2024 — Stimmzettel vs Stimmen denominator (March 2026)

**Status:** DONE (code fix, data not yet regenerated)

**Problem:** SN 2024 and ST 2024 municipal elections produce party vote shares >1.0 (up to 1.46 for AfD). Both states use Kumulieren & Panaschieren (3 votes per voter), so total party votes ≈ 3× ballot papers. The pipeline used `gueltige_stimmzettel` (valid ballot papers) instead of `gueltige_stimmen` (valid total votes) as the denominator.

**Affected rows:** 235 rows with shares >1 (153 CDU, 75 AfD, 5 SPD, 2 FDP). All SN/ST 2024 only — earlier years already used the correct column.

**Fix:**
- `01_municipal_unharm.R` line ~15225: `gueltige_stimmzettel` → `gueltige_stimmen` (SN 2024)
- `01_municipal_unharm.R` line ~8357: `C2 - Gültige Stimmzettel` → `D - Gültige Stimmen` (ST 2024)

**Root cause:** The raw data's column names changed between 2019 and 2024; the pipeline author selected the wrong column (Stimmzettel = ballot papers, not Stimmen = votes).

**Note:** RLP 2004-2019 also assigns from a variable called `GueltigeStimmzettel`, but this is a pipeline variable name — the actual source data only has vote totals, not separate ballot-paper counts. No shares >1 in RLP. Variable naming is misleading but data is correct.

---

## Known issues from cross-dataset audit (March 2026)

### Crosswalk: `ags_21` stored as numeric — FIXED (March 2026)
`code/crosswalks/01_ags_crosswalk.R`: Changed `as.numeric(ags_21)` to `sprintf("%08.0f", as.numeric(ags_21))` for proper 8-digit character AGS. Also converted after all Excel reads via `mutate(ags_21 = sprintf(...))`. Crosswalk and all harm scripts re-run. Downstream harm scripts already had defensive `as.character(ags_21)` calls.

### Municipal harm: `ags_21` inherits numeric type — PARTIALLY FIXED
The crosswalk now stores `ags_21` as character. Municipal harm still shows `ags_21` as integer because all values are NA (the join produces no matches). When populated, it will correctly inherit character type.

### Municipal unharm: `state` column uses text names — FIXED (March 2026)
Added `state_name_to_code` mapping at end of `01_municipal_unharm.R`. State column now uses 2-digit codes consistent with all other pipelines.

### Municipal unharm: `election_year` is character — FIXED (March 2026)
Added `as.integer(election_year)` at end of `01_municipal_unharm.R`.

### European elections: Saarland Briefwahl-in-valid_votes
6 SL municipalities (2024) have `valid_votes + invalid_votes > number_voters` by 6-42%. The Saarland Wahlleiter appears to include Briefwahl in valid_votes but not in number_voters. Source data limitation.

### Sachsen-Anhalt municipal elections: coverage gaps
ST 2005-2008 have only 2-5 municipalities (vs ~200 normally). ST 2010 has 18. This creates extreme eligible_voter spikes and municipality count instability in the time series. Likely a data availability issue — only partial Ergänzungswahlen or Oberbürgermeisterwahlen were held in those years.
