# GERDA TODO / Status

Tracking outstanding work on the German Election Database. Last updated: 2026-04-20.

---

## Open Issues

### 1. OCR-induced vote share deviations in old state elections (GitHub #7)

Cross-validation against `bundeslaendeR` (Stelzle) identifies six state × party combinations with >1pp deviation from Bundeswahlleiter state totals after Vincent's April 2026 fixes (commit `29fd40e`). All six stem from OCR extraction of scanned PDFs.

| Case | Before Vincent's fix | After Vincent's fix | Problem addressed |
|---|---|---|---|
| BB 1994 SPD | -3.76pp | -3.93pp | No |
| BB 1990 SPD | -2.45pp | -2.45pp | No |
| BB 1990 Grüne | +2.02pp | +2.02pp | No |
| SH 1983 CDU | +1.65pp | +1.65pp | No |
| NRW 1970 CDU | -1.20pp | -1.20pp | No |
| BB 1990 CDU | -1.10pp | -1.10pp | No |
| BB 1994 Linke/PDS | -1.03pp | <1pp | Yes |

**Root causes by election:**
- **BB 1990 / BB 1994**: Erststimmen/Zweitstimmen row disambiguation on dense right-page tables. No labels distinguish E% from Z% rows on the right page; neither position counting, Tesseract, nor vision models (GPT-5.4-mini, Gemini 2.5 Flash) reliably pick the correct row. Detailed problem analysis: `docs/ocr_extraction_problem.qmd`.
- **SH 1983**: Municipality-level data in the source PDF excludes Briefwahl (mail-in) votes. Briefwahl totals are state-level only in Table 3. Partial fix applied via proportional state-level allocation — residual deviation is because CDU overperforms in Briefwahl.
- **NRW 1970**: Scattered OCR digit errors across ~90 Kreise from Tesseract extraction of scanned PDF.

**Tried and not working well enough:**
- PaddleOCR (too slow on Apple Silicon)
- pdfplumber column parsing (column alignment issues)
- GPT-5.4-mini single-page and page-pair extraction (E/Z confusion)
- Gemini 2.5 Flash with VV context and structured output (~same error rate)
- Tesseract + HMM Viterbi row-state decoding (works on clean pages but Tesseract fails to parse enough rows on ~30% of pages)
- Hybrid GPT (left) + Tesseract/HMM (right) with Gemini fallback (SPD still -5.6pp)

**Validated approach docs:** `/Users/hanno/.claude/projects/-Users-hanno-Documents-GitHub-german-election-data/memory/ocr_pipeline_approach.md`

**Next to try:**
- Constrained optimization with Wahlkreis-level subtotals as local anchors (would need machine-readable Bundeswahlleiter Wahlkreis data — Robert Stelzle may have access)
- Manual verification on the ~100-200 problematic municipalities per election
- Targeted re-extraction of the specific PDF pages that cause the most error

---

### 2. Requests from GitHub #7 (Robert Stelzle)

- [x] ✅ **Extend `code/checks/checks_state_harm.R` with bundeslaendeR cross-check** — flags state×party pairs with >1pp deviation and writes full deviations table to `data/data_checks/bundeslaendeR_deviations.csv`. Currently surfaces the 6 known OCR-induced deviations.
- [x] ✅ Move `flag_*` variables to the front of `state_unharm` — done in `01b_state_unharm_raw.R` (now at positions 10–12, previously 174–176)
- [ ] Reply to Robert with status update on remaining deviations

---

### 3. Other open / partially addressed enhancement requests

- [x] ✅ **`election_date` in federal municipality harmonized datasets** — added to `federal_muni_harm_21` and `federal_muni_harm_25`. Applied both as a post-hoc patch (`code/checks/add_missing_metadata.R`) and in the source scripts (`02_federal_muni_harm_21.R`, `03_federal_muni_harm_25.R`) so the change persists across pipeline runs.
- [x] ✅ **`state_name` and `ags_name` in federal municipality datasets** — added to `federal_muni_harm_21`, `federal_muni_harm_25`, `federal_muni_unharm`. Applied both as patch and in source scripts (`01_federal_muni_unharm.R`, `02_federal_muni_harm_21.R`, `03_federal_muni_harm_25.R`).
- [x] ✅ **`state_name` in municipal datasets** — added to `municipal_harm`, `municipal_unharm`, `municipal_harm_25`. Applied both as patch and in source scripts (`01_municipal_unharm.R`, `02_municipal_harm.R`, `03_municipal_harm_25.R`).

**Already done (confirmed):**
- ✅ `election_date` in `state_harm_21`, `state_harm_25`, `state_unharm`, `federal_muni_unharm`, `municipal_harm`, `municipal_unharm`
- ✅ `ags_name` and `state_name` in `state_harm_21`, `state_harm_25`
- ✅ `ags_name` in `municipal_harm`, `municipal_unharm`
- ✅ Codebook at `docs/codebook.md` (55KB, substantial)
- ✅ `population_ags` scale documented as thousands throughout the codebook (lines 86, 97, 159, 200, 277)

---

### 4. Pipeline completeness gaps

**Already done (since TODO was originally drafted):**
- ✅ **Mayoral harm**: `02_mayoral_harm.R` exists, produces `mayoral_harm.rds` plus `mayor_panel.rds` and annual panel
- ✅ **County elections (Kreistagswahlen)**: `01_county_elec_unharm.R` and `02_county_elec_harm_21.R` exist; `county_elec_unharm.rds`, `county_elec_harm_21.rds`, and muni/cty variants in final/
- ✅ **European elections**: `01_european_muni_unharm.R` and `02_european_muni_harm.R` exist; `european_muni_unharm.rds` and `european_muni_harm.rds` in final/

**Still outstanding:**
- [ ] **RLP mayoral elections**: only percentages (no absolute vote counts) due to source data limitations — would need original Excel/PDF sources with counts
- [ ] **SH mayoral elections**: `00_sh_scrape.R` exists but scraping status / inclusion in `mayoral_unharm` needs verification
- [ ] **Municipal 2024 missing states**: currently have BW, SL, BB, MV, SN, ST, TH. Missing **HE, NI, NRW, RLP, SH** for 2024. Some of these may not have held 2024 Gemeinderatswahlen.
  - **RLP 2024 specifically**: raw data not yet in repo. The Oct 2025 Gemeinderatswahlen zip covers BW, SL, SN, ST, TH but not RLP. Maurice Baudet von Gersdorff (email) was reaching out to the RLP statistical office. Blocked on data acquisition.
  - Note: RLP **mayoral** (Direktwahlen) 1994–2024 was delivered (Dec 2025 file) and is in `mayoral_unharm` (16 RLP mayoral elections in 2024).

---

### 5. User-reported bugs (status from emails)

| Reporter | Issue | Status |
|---|---|---|
| Pauly (Spiegel) | Kleinstgemeinden dropped in crosswalks | ❓ Needs verification — specific AGS list from Pauly required |
| Pauly | 0.1% missing votes in state 2016/2021 harmonized | ✅ Fixed — `total_vote_share ≈ 1.0000` across all RLP/BaWü rows |
| Pauly | RLP 2021 "Sonstige" double-counting (FW, ÖDP, Die Partei, Tierschutz, Piraten, Volt) | ✅ Fixed — each party has distinct non-overlapping values |
| Pauly | RLP 2021 missing `election_date` | ✅ Fixed — `election_date = 2021-03-14` |
| Pauly | Time-series loading support | ❓ Feature request, separate discussion |
| Oriola | RLP 1999 municipal missing vote shares | ✅ Fixed by Florian Sichart |
| Greinetz | 1949 municipality data | N/A — directed to Julian Voss's dataset |
| Lockwald | Shapefile location | N/A — answered directly |

---

### 6. Party coverage policy

**Decision:** we will not break out additional minor parties (e.g., Bündnis C, PfV, KlimalisteBW, and other small or single-election parties) as separate columns in GERDA. Minor parties change across elections and across states, and breaking them out would explode the column count and create many near-empty columns. They remain aggregated in the `other` column. Users needing specific minor parties should go to the raw source data.

---

### 7. Legacy items from internal doc (pre-2025)

Audited 2026-04-22 against current state of the data.

**Still outstanding:**

- [ ] **Small municipalities suppressed post-2021 in federal muni data.** Since BTW 2021, the Bundeswahlleiterin suppresses Zweitstimmen for some very small municipalities (e.g. Friedrichsgabekoog `01051033`, Zendscheid `07232331`) for Geheimhaltungsgründe, rolling their votes into a collective "einschl. Gemeinde X" row under the parent Gemeinde (Reinsbüttel, Sankt Thomas). These AGS are therefore missing from `federal_muni_unharm` and the harmonized 2021/2025 files. The votes are included in state totals under the parent Gemeinde. **Needs:** a note in the codebook / README documenting which AGS are affected and why.
- [x] ✅ **SH/ST municipal pre-2018 source verified** (Thomas's comment). Checked 2026-04-22:
  - SH raw files are named `sh_gemeindewahlen_YYYY.xlsx`; loaded via `01_municipal_unharm.R` as `sh_YYYY_gemeinderatswahlen_data`. Years 1994, 1998, 2003, 2008, 2013, 2018, 2023 match the SH Kommunalwahl schedule. Confirmed Gemeinderatswahlen.
  - ST raw file `sachsen-anhalt_1994_to_2019.xlsx` has sheets "1994", "1999", "2001-2004", "2005-2010", "2014-2015", "2019". Main Kommunalwahl years (1994, 1999, 2004, 2009, 2014, 2019, 2024) have normal row counts (218–1,289). Non-standard years (2005–2008, 2010) have only 2–18 rows each — these are special elections for individual newly-formed municipalities after the 2007–2010 Gemeindereform, not Kreistagswahlen. The drop from 1,121 rows (2004) to 190 (2009) to 218 (2014+) reflects the real municipal consolidation.
- [x] ✅ **Population scale.** Already documented as thousands in `docs/codebook.md` (Berlin `population_ags = 3,677.472` = 3.68M people). The Google Doc note "in 100s" was incorrect.
- [ ] **Kumulieren/Panaschieren documentation gap** (Andreas). Paper mentions NRW and Sachsen electoral systems but not Hessen, Baden-Württemberg, Bayern, Bremen, Hamburg. Document how GERDA treats vote counts from Kumulieren/Panaschieren states — specifically which denominator is used for `valid_votes` and how party shares are computed for these states.
- [ ] **Parquet export** (Sascha). Nice-to-have for smaller file sizes and faster downloads. No `.parquet` files in `data/` currently.
- [ ] **Link to external datasets.** Cornelius's dataset, Julian Voss's 1949 dataset — future integration opportunity.
- [ ] **Dissemination workflow.** Send a post via PolMeth / EPSA mailing lists when GERDA is updated with new elections.

**Confirmed done (for traceability):**

- ✅ `incogruence` typo in column names (Vincent's fix). No longer present.
- ✅ `Inf` turnout values in `municipal_harm.rds` (Vincent's fix). `turnout` range now 0.27–1.0 with zero Inf/NaN.
- ✅ Federal muni county level 1953–1967 coverage. `federal_cty_unharm` covers 1953–2025.
- ✅ Berlin AGS `1100000` only-in-1990 issue. Now appears across multiple years.
- ✅ Sachsen GE/TG duplicates (fixed Feb 2026).
- ✅ BSW column in harmonized state datasets.
- ✅ AfD in harmonized data starting 2013 (per paper appendix).
- ✅ R package on CRAN (`install.packages("gerda")`).
- ✅ Website uses HTTPS.
- ✅ Paper revisions (economic voting removed, PDF software description added, electoral system overview, ReadMe, Party Manifesto / ParlGov crosswalk, etc.). Paper published in Nature Scientific Data 2025.
- ✅ `election_date`, `state_name`, `ags_name` added to federal muni datasets (2026-04-22 via `code/checks/add_missing_metadata.R`).
- ✅ `state_name` added to municipal datasets (same patch).
- ✅ `flag_*` columns moved to the front of `state_unharm` (same patch).
- ✅ `population_ags` unit (thousands) already documented in codebook.

---

## Reference: State Election Datasets — Known Issues

(Folded in from the previous `docs/state_elections_known_issues.md`, February 2026. Methodology and changelog: `docs/state_elections_update_2026.md`. Automated checks: `code/checks/checks_state_harm.R`.)

### Dataset overview

Four state election datasets are available:

| Dataset | File | Rows | Cols | Elections | States | Target Boundaries |
|---------|------|------|------|-----------|--------|-------------------|
| `state_harm_21` | `data/state_elections/final/state_harm_21.rds` | ~33,286 | 44 | 2006–2024 | 15 | 2021 |
| `state_harm_23` | `data/state_elections/final/state_harm_23.rds` | ~27,000 | 43 | 2006–2023 | 14 | 2023 |
| `state_harm_25` | `data/state_elections/final/state_harm_25.rds` | ~33,189 | 44 | 2006–2024 | 15 | 2025 |
| `state_2224_unharm` | `data/state_elections/final/state_2224_unharm.rds` | ~6,405 | 35 | 2022–2024 | 11 | Original |

All three harmonized datasets use the weighted-sum-of-counts aggregation method. An older `state_harm.rds` (2006–2019, weighted mean of shares) is retained for backward compatibility.

### Key differences between datasets

| Feature | `harm_21` | `harm_23` | `harm_25` | `2224_unharm` |
|---------|-----------|-----------|-----------|---------------|
| `bsw` column | Yes | **No** | Yes | Yes |
| Bremen included | Yes | **No** | Yes | Yes |
| 2024 elections | Yes | **No** | Yes | Yes |
| Greens column name | `gruene` | `gruene` | `gruene` | **`grune`** |
| `other` column | Yes | Yes | Yes | **No** |
| Quality flags | Yes | Yes | Yes | **No** |
| Covariates | Yes | Yes | Yes | **No** |
| `state_name` column | Yes | Yes | Yes | **No** |

### Coverage

No dataset includes **Hamburg** (state `02`) — Hamburg lacks municipality-level state election data in the DESTATIS pipeline.

`state_harm_23` excludes Bremen (state `04`) and all 2024 elections. The 11 state-year combinations in `state_2224_unharm` are: SL 2022, SH 2022, NRW 2022, NI 2022, HB 2023, HE 2023, BY 2023, BE 2023, BB 2024, SN 2024, TH 2024.

### Turnout issues

**NA turnout (all datasets).** Four Thuringia municipalities: `16063033`, `16074082`, `16075087`, `16075105`. Source data issue in Thuringia 2024.

**Turnout > 1 (`state_harm_21`, `state_harm_25`).** 11 rows slightly exceed 1.0 in SH 2022 and SN 2024, likely from mail-in vote allocation. Vincent (April 2026) added `flag_harm_turnout_above_1` and caps >1.5 to NA.

**Turnout = 0 (`state_harm_23` only).** ~26,858 rows with `turnout = 0`, concentrated in pre-2022 election years. Fix is in `state_harm_21` and `state_harm_25` but not in `state_harm_23`. **Prefer `state_harm_21` or `state_harm_25` for turnout analyses.**

**Additional NA turnout (`state_harm_23` only).** AGS `07336021` (RLP).

### Vote share issues

`total_vote_share` deviates from 1.0 by >5pp for ~2% of rows — tracked party columns don't cover every minor party. 4 rows across `state_harm_21`/`state_harm_25` have `total_vote_share < 0.7` (elections with large local-party shares).

### Party-specific

- **Bremen AfD (2023)**: coded as 0 in harmonized datasets, `NA` in `state_2224_unharm`. AfD did not contest.
- **BSW**: not in `state_harm_23` (BSW first contested in 2024, outside coverage).
- **`grune` vs `gruene`**: `state_2224_unharm` uses `grune`; harmonized datasets use `gruene`.

### Quality flags

`flag_unsuccessful_naive_merge`: rate ~19% in `state_harm_21` (driven by 100% flag rate for 2022–2024 elections from the different `03_state_2022-24.R` source pipeline — mechanical, not a data issue). Rate ~0.05% in `state_harm_23`/`state_harm_25`.

`flag_total_votes_incongruent`: rate ~11–13%, concentrated in 2008 (~40%) and 2018 (~67%). Source data issue from DESTATIS pipeline.

### Covariate gaps

Covariates (`area_ags`, `population_ags`, `employees_ags`, `pop_density_ags`) are present in all three harmonized datasets but systematically NA for recent elections:

| Dataset | Gap | Reason |
|---------|-----|--------|
| `state_harm_21` | 100% NA for 2022–2024 | `ags_area_pop_emp.rds` covers through ~2021 |
| `state_harm_23` | `employees_ags` 100% NA for 2022–2023 | Employment data ends earlier |
| `state_harm_25` | 100% NA for 2024 | Covariate source covers through ~2023 |

Population in thousands.

### Cross-dataset vote total differences

Up to ~5% difference across target years for Sachsen, ~2.4% for RLP, <2% for others. Expected: different crosswalk targets use different population-weighted mappings for municipal boundary changes. Not data errors.

### `state_harm_23` specific issues

`state_harm_23` (produced by `04_state_harm_23.R`) has:
1. Turnout = 0 for ~26,858 pre-2022 rows (unfixed `number_voters` bug)
2. No BSW column
3. No Bremen
4. No 2024 elections
5. Additional NA turnout for AGS `07336021`
6. 14 states, 40 state-year combos (vs. 15/48 in `harm_21`/`harm_25`)

**For most uses, prefer `state_harm_21` or `state_harm_25`.**
