# State Elections Final Data

## Current files

### Unharmonized (original municipal boundaries)

- **`state_unharm.rds/.csv`** -- Municipality-level state election results from official state statistics offices. Covers all 16 German states, 1946--2025 (availability varies by state). ~148,000 rows, ~450 columns (all individual party columns preserved). Produced by `code/state_elections/01b_state_unharm_raw.R`.

### Harmonized (fixed administrative boundaries)

All harmonized files use population-weighted crosswalks to map results onto fixed municipal boundaries, enabling comparison across time. Coverage: 1990--2025.

- **`state_harm_21.rds/.csv`** -- Harmonized to 2021 municipal boundaries. Produced by `code/state_elections/02b_state_harm_21.R`. ~81,600 rows.
- **`state_harm_23.rds/.csv`** -- Harmonized to 2023 municipal boundaries. Produced by `code/state_elections/04_state_harm_23.R`. ~81,500 rows.
- **`state_harm_25.rds/.csv`** -- Harmonized to 2025 municipal boundaries. Produced by `code/state_elections/05_state_harm_25.R`. ~81,400 rows.

## Archive: `_old_regionalstatistik/`

Contains older versions of the data that were based on the DESTATIS Regionalstatistik API (2006--2019) and manually compiled post-2020 elections. These have been superseded by the stats-office raw data pipeline above.

- `state_unharm.rds/.csv` -- Old DESTATIS API data (2006--2019)
- `state_2224_unharm.rds/.csv` -- Manually compiled post-2020 elections
- `state_2223_unharm.rds/.csv` -- Legacy version
- `state_harm.rds/.csv` -- Old harmonized data (deprecated)

### Why the pipeline was replaced

The GENESIS Regionalstatistik API does not properly allocate Briefwahl (mail-in) votes from shared/pooled voting districts to individual municipalities. This caused **massive valid_votes undercounts** in 6 states, concentrated in eastern Germany where Briefwahl is pooled at Amt or county level:

| State | VV undercount (old) | Municipalities >10% off | Worst case |
|-------|-------------------|------------------------|------------|
| Sachsen | 10--17% | 822 of 1,337 | +129% recovery |
| Mecklenburg-Vorp. | 7--19% | 746 of 1,515 | +195% recovery |
| Brandenburg | 0.1--20% | 412 of 1,251 | +169% recovery |
| Thüringen | 4--6% | 971 of 2,460 | +46% recovery |
| Sachsen-Anhalt | 0--8% | 88 of 437 | +27% recovery |

The new pipeline reads raw Landeswahlleiter files where Briefwahl is either already allocated to municipalities or is explicitly allocated proportionally based on Urnenwahl (in-person) results. This also shifts party shares: mail-in voters lean differently from in-person voters, with AfD shares dropping by up to ~1pp and CDU/Linke shifting by ~0.5pp at the municipality level.

States unaffected by Briefwahl pooling (SH, NI, NRW, HE, SL, BE) show **zero difference** between old and new data, confirming that the improvement is targeted and the pipeline is consistent where Regionalstatistik was already correct.

Additional improvements over the old pipeline:
- **Coverage**: 1946--2025 vs 2006--2019 (30x more election-years)
- **Party detail**: All individual parties preserved (~450 columns) vs only 8 major party groups
- **Bayern**: Reports Gesamtstimmen (Erst+Zweit combined) matching how Bavaria allocates seats, vs single-vote counts in the old data
- **Quality flags**: `flag_naive_turnout_above_1`, `flag_total_votes_incongruent`, zero→NA recoding for non-contesting parties
- **Derived columns**: `far_right`, `far_left`, `far_left_w_linke` aggregates

## Column structure

### Meta columns
`ags`, `election_year`, `state`, `election_date`, `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`

### Party columns
All individual party vote shares (e.g., `spd`, `cdu`, `gruene`, `fdp`, `linke_pds`, `afd`, `bsw`, ...) plus `other` (residual) and `cdu_csu` (combined). Party names are normalized to snake_case via `normalise_party()`.

### Harmonized files additionally include
`county`, `state_name`, `flag_unsuccessful_naive_merge`, `flag_total_votes_incongruent`, `perc_total_votes_incogruence`, `total_vote_share`, `far_right`, `far_left`, `far_left_w_linke`, `flag_naive_turnout_above_1`, and area/population covariates.

## State coverage

| State | Code | Unharm years | Harm years | Notes |
|-------|------|-------------|------------|-------|
| Thüringen | 16 | 1990--2024 | 1990--2024 | |
| Sachsen-Anhalt | 15 | 1990--2021 | 1990--2021 | |
| Sachsen | 14 | 1990--2024 | 1990--2024 | |
| Brandenburg | 12 | 1990--2024 | 1990--2024 | 1990/1994 OCR-digitized |
| Mecklenburg-Vorpommern | 13 | 1990--2021 | 1990--2021 | |
| Baden-Württemberg | 08 | 1952--2021 | 1992--2021 | |
| Hessen | 06 | 1946--2023 | 1991--2023 | |
| Saarland | 10 | 1970--2022 | 1990--2022 | 1970/1975 OCR-digitized |
| Hamburg | 02 | 1966--2025 | 1991--2025 | City-state (1 row/election); 5-vote system since 2011 |
| Niedersachsen | 03 | 1998--2022 | 1998--2022 | |
| Nordrhein-Westfalen | 05 | 1947--2022 | 1975--2022 | 1947--1970 county-level only |
| Berlin | 11 | 1990--2023 | 1990--2023 | City-state (1 row/election) |
| Bremen | 04 | 1946--2023 | 1991--2023 | City-state (2 rows: Stadt Bremen + Bremerhaven) |
| Schleswig-Holstein | 01 | 1983--2022 | 1996--2022 | 1983 OCR-digitized |
| Bayern | 09 | 1946--2023 | 1990--2023 | Reports Gesamtstimmen (Erst+Zweit combined) |
| Rheinland-Pfalz | 07 | 1979--2021 | 1991--2021 | 1979--2016 Landesstimmen only (no turnout data) |

## Known data limitations

### Missing absolute vote counts (percentages only)

- **Bremen 1946--1995**: Raw data only contains vote share percentages at Ortsteil level (column headers stored as graphical objects in Excel). `valid_votes` and `invalid_votes` are NA in `state_unharm`. In harmonized files, `number_voters` is used as the weighting proxy; since Bremen's 2 municipalities don't change boundaries, the share→count→share round-trip preserves the original percentages exactly.
- **Bremen 2011**: Per-party percentages hardcoded from Statistisches Landesamt Faltblatt PDF (wahlen-bremen.de was offline). Absolute Stimmen available from Amtsblatt for metadata (EV, voters, invalid, valid Gesamtstimmen).
- **Rheinland-Pfalz 1979--2016**: Source file (`LW_RLP_1979_2021.xlsx`) has Landesstimmen party counts and Gesamtsumme, but no eligible_voters/number_voters/invalid_votes. `valid_votes` is available (Gesamtsumme LS). Turnout data only available for 2021.

### Rheinland-Pfalz: source data deviations from official Landesergebnisse

The Wahlleiter's Excel maps all results to current (2021-era) municipality boundaries. For elections 1979--2001, 5--6 municipalities are missing because they were founded after 1979 (Langweiler, Trimbs, Hatzenport, Urbar, Leienkaul; plus Dierfeld is absent from all years). This causes the municipal-level sums to fall 0.07--0.31% below official Landesergebnisse. The shortfall disproportionately affects CDU (~-0.3pp) vs SPD (~+0.3pp) for 1979--1991, consistent with small rural CDU-leaning municipalities being underrepresented.

From 2006 onward (all 2,299 municipalities present, only Dierfeld missing), deviations are <=0.16%, and 2011/2016 match the official totals exactly. For 2021, 19 municipalities were merged into neighbors due to low voter counts (Zusammenlegungen) — these municipalities are not listed separately.

This is a source data limitation, not a pipeline issue.

### Bavaria Gesamtstimmen

Bayern reports combined Erst+Zweitstimme ("Gesamtstimmen") because both ballots count equally toward proportional seat allocation (unlike Bundestagswahlen). This means `valid_votes ≈ 2 × number_voters` for 1950+. The identity `valid_votes + invalid_votes = number_voters × 2` holds.

### Other known issues

- **HE 1958/62**: `number_voters`/`invalid_votes` not reported for non-kreisfreie municipalities (recoded to NA)
- **SH 1983**: 135 of 1,079 municipalities lack eligible_voters/number_voters (garbled PDF text layer)
- **BY 1994--2013**: `eligible_voters` NA (not in Stimmabgabe source files)
- **BB 1990/94**: Briefwahl misallocation artifacts in OCR-extracted data
- **NRW 1947--1970**: County-level only (synthetic AGS `050xx000`), present in unharm but cannot be harmonized. 1947/1950 were visually read from scanned PDFs at Wahlkreis level (150 WK) then aggregated to ~84 Kreise. No turnout data (only valid\_votes and party counts). 1947 parties: CDU, SPD, FDP, KPD, Zentrum, DRP, RWVP, Unabhängige. 1950 adds DP, RSF, SRP, CSAB. Minor scan-read residuals for 1947 (CDU+4, FDP-1, KPD+2, Z-5 vs official totals)

### Harmonization weight imputation

For elections where `valid_votes` is NA (Bremen pre-1999), the harmonization scripts impute a weight from `number_voters` or `eligible_voters` to enable the share→count→share conversion. Since Bremen's municipalities don't merge across boundary years, this produces exact results.

### Fixed issues (Feb--Mar 2026)

These issues have been resolved but are documented for provenance:

- **BaWü GENESIS API Briefwahl undercount**: The GENESIS Regionalstatistik API only reported Urne (in-person) valid votes for non-kreisfreie BaWü municipalities in 2011/2016, missing all Briefwahl (13--17% undercount). Fixed by replacing API data with raw Excel/CSV files from the Statistisches Landesamt.
- **Fallback join duplication**: Year−1 fallback join in harm scripts was applied to all rows including already-matched multi-target AGS, creating duplicate rows. For BaWü, this inflated eligible_voters by 13,348 in harm_23/harm_25. Fixed by splitting matched/unmatched before fallback.
- **Crosswalk weight rescaling**: The 2025 crosswalk (`ags_1990_to_2025_crosswalk`) had un-rescaled chained weights for 755 AGS-year combinations. All `_25` harmonized datasets were affected. Fixed in `05_build_23_25_ags_crosswalks.R`.
- **BB 2009 percentage rows**: Raw XLS parsing was reading both the Anzahl (absolute counts) and "%" (percentage) sections, treating percentages as additional vote counts. This inflated valid_votes for 223 municipalities. Fixed by adding `pct_row` detection to stop reading before the "%" marker.

See `docs/state_pipeline_audit.md` for the full audit log.
