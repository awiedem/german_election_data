# GERDA TODO / Status

Tracking outstanding work on the German Election Database. Last updated: 2026-09-28.

---

## Open Issues

### 0. Data audit of all final datasets (2026-10-12)

A multi-agent audit found 13 high-severity and 32 medium-severity issues, plus labeling and documentation drift; see [audit report](docs/audit_2026-10-12.md) and `data/data_checks/audit_2026-10-12/`. Fixed on 2026-10-12: Greens merged with Die Grauen in Berlin 2021/2023, Hessen 1991 and BW 1992 (H1, M11); Rhein-Pfalz-Kreis Landrat 2017 AGS (H9); county covariates for Berlin 1991 and Thüringen 2016-2020 (H6, H7). Fixed on 2026-10-19, committed but not pushed (§0b): M12 (`far_right` without the Bavarian Republikaner, 0 instead of NA). Fixed upstream in the same period: `pop_density_ags` unit (H5), Schleswig-Holstein Kreiswahl rows (H10), Thüringen 2024 kreisfreie Städte and TH 2019/BB 2014 party columns (H11, H12). The other high-severity items are open and need decisions or new sources: Brandenburg 1999 state coverage (H2), postal votes missing in federal county 1957-65 (H3) and in eastern municipal federal data 1994-2002 (H4), city-state parliaments in `county_council_seats` (H8), and annual mayor panel forward-fill and duplicates (H13). Vincent: please pick up whichever of the open items (high or medium, see the report) you want to take on.

### 0b. Hanno's state-election notes (2026-10-16/18: `far_right` zeros, postal allocation)

**Claimed 2026-10-19 by the Claude session "Hanno's document issues". Applied in main and state files rebuilt on 2026-10-19 in two rounds (the patch, then the fixes from a five-reviewer audit: party-sum identity, Wahlkreis weights for ST pools, MV 1994/1998 pools by Wahlkreis, MV 2011 Amt column, Bremen 1999-2007 from the Wahlbereich totals); patches and HANDOVER.md in `review/state_farright_postal_2026-10-19/`. Rounds 1–4 committed together on 2026-10-19 (state paths and the state hunks of the shared docs only), not pushed. At the push: website update-log entry (draft in HANDOVER.md; add the round 3 and 4 items); render `codebook.pdf` at the next release (main's codebook also holds unreleased mayoral edits). Backups: `~/gerda_backups/2026-10-19_pre_state_farright_postal/` (before) and `..._round1_state_farright_postal/` (after round 1).** Scope: Bayern 1994-2013 REP/NPD under unmapped column names; SH 2000-2009 and HB 1999-2007 parties left in `other`; `far_right` NA where the source lumps parties into `other`; FDVP in `far_right`; MV postal allocation (flag, conservation); ST 2011 Brücken-Hackpfüffel postal district; ST 2011/2016 named postal pools; README allocation wording; crosswalk 2024 population note.

**Round 3 applied in main 2026-10-19 (13:45) by the integrator** (`review/state_farright_postal_2026-10-19/state_farright_postal_round3.patch`, Vincent's decisions of 2026-10-19: far_right counts pro NRW / pro Deutschland and the DLVH, not the DSU; far_left NA rule). The three harm files are rebuilt and all checks match the handover: far-right 50/50, accounting PASS 21/23/25, missingness 1 507 comparisons, `checks_state_harm.R` only the pre-existing turnout FAIL, Wahlkreis audit 0 FAIL, Excel exported and checked. Committed with rounds 1, 2 and 4. Backup: `~/gerda_backups/2026-10-19_pre_state_round3/`.

**Round 4 (Hanno's suggestions) applied in main 2026-10-19 by the session "Hanno's document issues"** (`review/state_farright_postal_2026-10-19/state_hanno_suggestions_round4.patch`): SH 2017/2022 and BB 2004/2014/2024 postal pools now go through `allocate_pooled_counts()` (flagged, exact; SH by Amt and Wahlkreis, A2), receivers capped at their electorate, new metadata `postal_allocation_rules.csv`, `state_postal_pools.csv`, `state_postal_receivers.csv`, `state_party_columns.csv`, crosswalk population notes in the codebook. Committed with rounds 1–3. Backup: `~/gerda_backups/2026-10-19_pre_state_round4/`. Not done from Hanno's list: per-row columns in the election files (the receivers table is the companion instead), capping in the Thüringen residual and the Sachsen lead pools.

### 1. Historical state-election errors (GitHub #7)

Brandenburg 1990 and 1994 now use verified official second-vote spreadsheets.
The 1990 reconstruction preserves election-day municipalities: 22 municipalities
that voted in Mecklenburg-Vorpommern are excluded, and Brunow/Dambeck are restored
from the original report. Both elections retain in-person coverage; pooled postal
votes are reconciled separately and are not allocated. The user approved these
geographic and postal-vote choices on 2026-09-28.

All 13 parties in 1990 and all 12 in 1994 reconcile with official statewide totals
when the separately retained postal counts are included. There are 1,792 municipal
records in 1990 (previously 1,793, including two false OCR identifiers) and 1,693 in
1994 (previously 1,644). See [repair notes](docs/sources/bb_1990_1994.md) and
`data/data_checks/bb_1990_1994/` for verification results and downstream coverage.

NRW 1966/1970 and SH 1983 were repaired in the September historical release.
SH 1983 still requires comparison with in-person totals, not totals including postal votes.

### 2. Follow-up on GitHub #7

- [x] Implement the `bundeslaendeR` comparison and move quality flags to the front.
- [x] Replace the Brandenburg OCR inputs and document the historical geography.
- [ ] Review the repaired exports and prepared reply in `docs/sources/bb_1990_1994.md`.
- [ ] Investigate the remaining Rheinland-Pfalz 2006 `linke_pds` comparison (2.557 percentage points) and the external BB 1994 BFWG/BürgerBündnis mapping; keep #7 open.
- [ ] Commit, push, and post the reply only after Hanno requests publication.

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
- [x] ✅ **SH mayoral elections**: `00_sh_scrape.R` has been run; output files exist at `data/mayoral_elections/raw/sh/sh_mayoral_scraped.{rds,csv}`; SH mayoral data integrated into `mayoral_unharm` (45 rows, years 2023–2025). Coverage is limited to 2023+ because `wahlen-sh.de` does not publish historical bulk data.
- [ ] **Municipal 2024 missing states**: currently have BW, SL, BB, MV, SN, ST, TH. Missing **HE, NI, NRW, RLP, SH** for 2024. Some of these may not have held 2024 Gemeinderatswahlen.
  - **RLP 2024 specifically**: raw data not yet in repo. The Oct 2025 Gemeinderatswahlen zip covers BW, SL, SN, ST, TH but not RLP. Maurice Baudet von Gersdorff (email) was reaching out to the RLP statistical office. Blocked on data acquisition.
  - Note: RLP **mayoral** (Direktwahlen) 1994–2024 was delivered (Dec 2025 file) and is in `mayoral_unharm` (16 RLP mayoral elections in 2024).

### 9. Wahlkreis-level state elections (August 2026 extension)

**Done (branch `ltw-wkr-clean-extensions`):** 28 elections added to `ltw_wkr_unharm`
from clean text-layer PDFs already in the repo, no OCR (BE 1999–2021, BW 2001–2011,
BY 2008/2013, HB 2003–2023, HE 2009, NI 2008, SN 2004/2009, SL 1980–2017, SH 2005).
Coverage now 103 elections, 1980–2026. Per-election recoverability of everything
still missing: `docs/ltw_wkr_recoverability.md`. Audit: 0 FAIL.

**Follow-ups:**
- [ ] Re-download the two corrupt raw PDFs (BW 1996 `BVII2.pdf`, BE 1995
  `Ergebnisbericht.pdf` — broken xref, gs-unrecoverable) and re-classify them.
- [ ] Source the Bremen Hefte for 1991/1995/1999 (full party detail; the long-series
  PDF alone was judged insufficient — seat-winning parties hidden in Sonstige).
- [ ] Larger recoverability scan of the OTHER pipelines' raw holdings (state
  Gemeinde-level historical PDFs, county elections, mayoral), same method:
  empirical text-layer tests + per-file classification.
- [ ] Investigate the pre-existing BW 2026 CSV parser warning: statewide mismatch of
  5,108 erststimme votes for "Anderer Kreiswahlvorschlag" (warns, doesn't halt).
- [ ] The remaining Wahlkreis gaps are OCR-class (see the recoverability doc); they
  join the OCR problem in item 1 above rather than the clean pipeline.

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
- [x] ✅ **Kumulieren/Panaschieren documentation gap** (Andreas). Added notes to `docs/codebook.md`: HH/HB 2011+ 5-vote state elections (near the BY Gesamtstimmen note) and a municipal-elections note covering BW/BY/HE/RP/MV/SH/SL and others — clarifying that `valid_votes` counts cast individual votes, party shares are proportions of these, and NRW is the single-vote exception.
- [ ] **Parquet export** (Sascha). Nice-to-have for smaller file sizes and faster downloads. No `.parquet` files in `data/` currently.
- [ ] **Link to external datasets.** Cornelius's dataset, Julian Voss's 1949 dataset — future integration opportunity.
- [ ] **Dissemination workflow.** Send a post via PolMeth / EPSA mailing lists when GERDA is updated with new elections.

---

### 8. Council seats follow-ups (merged to main 2026-07-23)

The council-seats branch (`feat/council-seats`, merge commit `8b20142`) added ten `seats_*` columns to `municipal_unharm` and the `county_council_seats` panel (400 counties x 2008-2025). Change record: `docs/council_seats_update_2026.md`. Remaining items:

- [ ] **Municipal voter-group seat columns (deferred).** Two columns (Wählergruppen, Gemeinsame Wahlvorschläge) are dropped by a `-contains()` step in `01_municipal_unharm.R` (~line 20205) AND are mislabeled at the source: Brandenburg 2003 vs 2008 write raw "Wählergruppen" seats into swapped output columns. Audit BB 2003/2008/2014/2019/2024 against official sources before exposing them.
- [ ] **Ask Vincent:** (a) is `government_party` in the hand-compiled county seats file the Landrat's party? (currently inferred, not confirmed); (b) provenance/citation line for `Sitzverteilungen_der_Parteien_2008-2022_v1-0-0` for the codebook.
- [ ] **Municipal seats coverage gaps** (optional extensions): RLP seats stop at 2019, SH at 2018 (newer elections need collection); no municipal seats at all for Bayern, Berlin, Sachsen.
- [ ] **County seats pre-reform backfill** (optional): the panel uses fixed current county codes, so abolished pre-reform councils are absent (all of MV 2008-2010 is NA; same for pre-2009 Städteregion Aachen and pre-2016 LK Göttingen predecessors). Backfilling would require mapping abolished county councils.
- [ ] **Refresh project `CLAUDE.md`.** Stale claims: "county elections: no processing code" (there are now unharm/harm_21/seats scripts and final datasets) and municipal coverage "1990-2021" (now 1984-2026 with seats columns).
- [ ] **Website/package release alignment.** gerda R package catalog updated 2026-07-23 (dev version: `county_council_seats` exposed, municipal ranges corrected; 47 datasets). When the next package version ships, mirror the NEWS entry in the website `update-log.md` / `r-package.md` (see gerda repo `CLAUDE.md` for the checklist) — 0.7.1 is still pending at CRAN.
- [ ] **Broken pointer file.** `meinungsbild/docs/methodology_notes.pdf` is a 130-byte Git-LFS pointer, not a real PDF (siblings `.qmd`/`.md`/`.html` are fine). Re-render from the `.qmd` or delete.

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
