# State Elections Final Data

## Current files

### Unharmonized (original municipal boundaries)

- **`state_unharm.rds/.csv`** -- Municipality-level state election results from official state statistics offices. Covers all 16 German states, 1946--2026 (availability varies by state). 153,030 rows (all individual party columns preserved). Produced by `code/state_elections/01b_state_unharm_raw.R`.

### Harmonized (fixed administrative boundaries)

All harmonized files use population-weighted crosswalks to map results onto fixed municipal boundaries, enabling comparison across time. Coverage: 1990--2026.

- **`state_harm_21.rds/.csv`** -- Harmonized to 2021 municipal boundaries. Produced by `code/state_elections/02b_state_harm_21.R`. 85,046 rows.
- **`state_harm_23.rds/.csv`** -- Harmonized to 2023 municipal boundaries. Produced by `code/state_elections/04_state_harm_23.R`. 84,955 rows.
- **`state_harm_25.rds/.csv`** -- Harmonized to 2025 municipal boundaries. Produced by `code/state_elections/05_state_harm_25.R`. 84,819 rows.

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

The new pipeline reads raw Landeswahlleiter files. Where a source counts postal votes above the municipality (Kreis, Amt, Samtgemeinde, Wahlkreis or Land), the pipeline distributes them over the municipalities the pool serves, mostly by Wahlschein holders (A2) where the source reports them and by eligible voters otherwise, and marks the receivers with `flag_postal_allocated`; see [Postal votes pooled above the municipality](#postal-votes-pooled-above-the-municipality). This also shifts party shares: mail-in voters lean differently from in-person voters, with AfD shares dropping by up to ~1pp and CDU/Linke shifting by ~0.5pp at the municipality level.

Source coverage and mail-in allocation vary by election. The historical comparison above does not establish that later versions or all municipalities agree with the API data.

Additional improvements over the old pipeline:
- **Coverage**: 1946--2026 vs 2006--2019 (30x more election-years)
- **Party detail**: All individual parties preserved  vs only 8 major party groups
- **Bayern**: Reports Gesamtstimmen (Erst+Zweit combined) matching how Bavaria allocates seats, vs single-vote counts in the old data
- **Quality flags**: `flag_naive_turnout_above_1`, `flag_other_party_residual`, zero→NA recoding for non-contesting parties
- **Derived columns**: `far_right`, `far_left`, `far_left_w_linke` aggregates (harmonized files; see [`far_right`](#far_right-and-far_left-zero-unknown-or-a-lower-bound))

## Column structure

### Meta columns
`ags`, `election_year`, `state`, `election_date`, `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`

### Party columns
Shares use `valid_votes` as denominator. All individual party vote shares (e.g., `spd`, `cdu`, `gruene`, `fdp`, `linke_pds`, `afd`, `bsw`, ...) plus `other` (residual) and `cdu_csu` (combined). Exclude `cdu_csu` and other derived aggregates from individual-party sums. Flags, participation counts, turnout and covariates are not parties. Party names are normalized within this pipeline; aliases may differ across files.

Use the explicit [column schema and example](../metadata/README.md), rather than selecting every numeric column. `state_unharm` has no `stimme` column. For constituency party results, select `zweitstimme` or `einzelstimme` and check one row per constituency/election before adding participation.

### Harmonized files additionally include
`state_name`, `flag_unsuccessful_naive_merge`, `flag_other_party_residual`, `perc_total_votes_incongruence`, `total_vote_share`, `far_right`, `far_left`, `far_left_w_linke`, `flag_harm_turnout_above_1`, and area/population covariates. Municipality names are in `ags_name_21` for 2021 boundaries and `ags_name` for 2023/2025 boundaries. `total_vote_share` excludes `other` and derived aggregates; `flag_other_party_residual` is 1 when it falls outside [0.999, 1.001]. The three source flags (`flag_briefwahl_only`, `flag_no_valid_votes`, `flag_naive_turnout_above_1`) are present only in `state_unharm`; `flag_pooled` (1 = the source counted the municipality together with another one — see the RP 2026 note below) is in all four files. In the harmonized files, four parties that the sources spell two ways are merged into one column — `pdh` (Partei der Humanisten) into `die_humanisten`, `freiewaehler` into `freie_wahler`, `tier_schutz_partei` into `tierschutz` and `volt_hamburg` into `volt` — while `state_unharm` keeps each source's own label.

`flag_briefwahl_only` retains its legacy name and values for compatibility. It flags zero electorate with positive valid votes before participation fields are neutralized. It also captures missing/corrupt participation, and does **not** establish that a row is an actual postal district. The separate [source limitation table](../metadata/source_limitations.csv) documents remaining coverage restrictions after the NRW and SH source repairs.

Municipality names (`ags_name_21` in `state_harm_21`, `ags_name` in `state_harm_23`/`_25`) are those of the target-boundary municipality, taken from the crosswalk's target-name column, and are filled in every row. The area/population covariates describe the election year; elections after the covariate panel's last year carry that year's values and have `flag_covars_carried_forward` = 1 (see [Known data limitations](#known-data-limitations)).

## State coverage

| State | Code | Unharm years | Harm years | Notes |
|-------|------|-------------|------------|-------|
| Thüringen | 16 | 1994--2024 | 1994--2024 | 1990 available at constituency level only |
| Sachsen-Anhalt | 15 | 1990--2026 | 1990--2026 | |
| Sachsen | 14 | 1990--2024 | 1990--2024 | |
| Brandenburg | 12 | 1990--2024 | 1990--2024 | 1990/1994 OCR-digitized |
| Mecklenburg-Vorpommern | 13 | 1990--2021 | 1990--2021 | |
| Baden-Württemberg | 08 | 1952--2026 | 1992--2026 | 1952 constituent assembly on 1979 boundaries; electorate and turnout unavailable |
| Hessen | 06 | 1946--2023 | 1991--2023 | |
| Saarland | 10 | 1970--2022 | 1990--2022 | 1970/1975 OCR-digitized |
| Hamburg | 02 | 1966--2025 | 1991--2025 | City-state (1 row/election); 5-vote system since 2011 |
| Niedersachsen | 03 | 1974--2022 | 1990--2022 | |
| Nordrhein-Westfalen | 05 | 1947--2022 | 1990--2022 | 1947--1970 county-level only |
| Berlin | 11 | 1990--2023 | 1990--2023 | City-state (1 row/election) |
| Bremen | 04 | 1946--2023 | 1991--2023 | City-state (2 rows: Stadt Bremen + Bremerhaven) |
| Schleswig-Holstein | 01 | 1983--2022 | 1996--2022 | 1983: in-person votes only; turnout unavailable |
| Bayern | 09 | 1946--2023 | 1990--2023 | Reports Gesamtstimmen (Erst+Zweit combined) |
| Rheinland-Pfalz | 07 | 1979--2026 | 1991--2026 | 1979--2016 Landesstimmen only (no turnout data); 2026: 58 small Gemeinden counted inside a neighbour |

Ranges are not promises of continuous coverage. [Election completeness](../metadata/election_completeness.csv) lists actual elections, geography, observed/zero counts and sums of known values. `NA` remains missing (empty in CSV/Excel); summing an all-missing field with `na.rm=TRUE` must not be interpreted as zero. Partial sums do not give statewide turnout.

## Known data limitations

### Covariates carried forward for recent elections

`area_ags`, `population_ags`, `employees_ags` and `pop_density_ags` describe the election year, taken from the municipality covariate panels in `data/covars_municipality/final/`: `ags_area_pop_emp.rds` (1990--2021) for `state_harm_21` and `ags_area_pop_emp_2023.rds` (1990--2023) for `state_harm_23`/`_25`. A later election carries the panel's last year (2021 or 2023) and has `flag_covars_carried_forward` = 1: in `state_harm_21` every election from 2022 on, in `state_harm_23`/`_25` Hamburg 2025 and the 2024 and 2026 elections. No newer year is in the repository: the latest Destatis Gemeindeverzeichnis (`AuszugGV4QAktuell_2024.xlsx`, Gebietsstand 31.12.2024) still reports population and area as of 31.12.2023, which is also what the federal files use for the 2025 Bundestagswahl (unflagged there).

- `employees_ags` stays `NA` in carried rows and before 1997: the panels have no employment figures for 1990--1996 or for their last years (2021 in `ags_area_pop_emp`; 2022--2023, and part of 2021, in `ags_area_pop_emp_2023`).
- Where municipalities merged after the file's boundary year, a later election's votes for the merged unit sit under the surviving code and the absorbed municipality has no row, while the carried covariates describe the surviving municipality's own boundary-year territory. The absorbed municipalities hold at most 1.05 % of a state's population (Thüringen 2024 in `state_harm_21`; 0.65 % in `state_harm_23`).
- `pop_density_ags` is inhabitants per km² in all three files. Until September 2026 `state_harm_25` published it in thousands of inhabitants per km², and published missing employee counts as 0.

### Postal votes pooled above the municipality

Rows with `flag_postal_allocated` = 1 received postal votes that the source counted in a Briefwahlbezirk serving several municipalities, so their counts, turnout and shares are partly modelled. Each pool goes to the municipalities it serves, weighted by Wahlschein holders (A2) where the source reports them and by eligible voters otherwise. Every pool is handed out exactly, in integer counts, and wherever the pool itself adds up a receiver keeps `number_voters` = `valid_votes` + `invalid_votes` and `valid_votes` = the sum of its party votes. No receiver gets more postal voters than its room (eligible voters minus the voters it already has); the excess goes to the pool's other members (6 receivers capped). Thüringen (Wahlkreis residual) and the Sachsen 2019/2024 lead-Gemeinde pools have code of their own: fractional counts, no cap.

Three tables in [`../metadata/`](../metadata/README.md) document the allocation:
- [`postal_allocation_rules.csv`](../metadata/postal_allocation_rules.csv): per state and election, whether postal votes were pooled in the source, how they were distributed (level, weight, integer or fractional), the number of pools, pooled voters, receivers and capped receivers;
- [`state_postal_pools.csv`](../metadata/state_postal_pools.csv): every pool with its source totals (voters, valid and invalid votes), weight and receivers, so other weights can be applied;
- [`state_postal_receivers.csv`](../metadata/state_postal_receivers.csv): per municipality and election, the voters and valid votes it received from pools, their share of its voters and valid votes, and the pool ids. Join on `state`, `election_year`, `ags`.

- **Mecklenburg-Vorpommern 1994--2021**: pools of the Landtagswahlkreis within each Kreis (1994, 1998; a Gemeinde part without a pool of its own Kreis there joins the other Kreis's, so Stralsund's part of Wahlkreis 25 votes by post in Nordvorpommern's) or of the Amt (2002--2021; 2011 by the source's own Amt code). A2 weights in 1994, 1998 and 2011. MV 1990 is in-person only.
- **Sachsen-Anhalt 1990--2016**: every pool goes to Gemeinde parts of its Landtagswahlkreis, which the source prints on each row, each weighted by its electorate there, since some pools serve only part of a Gemeinde (OT Allrode of Thale, Petersberg's OT Brachstedt). In 2006, 2011 and 2016 each pooled district names the Gemeinden or the Verwaltungs- or Verbandsgemeinde it serves and goes to exactly those ([`st_postal_pool_members.csv`](../metadata/st_postal_pool_members.csv)); the 2006 Verwaltungsgemeinschaft members come from the Gemeindeverzeichnis of 31.12.2005, whose Gemeindeverband key opens the pool's Wahlbezirk number (Storkau (Elbe) updated to its VG on election day), without the Gemeinden that count their own postal votes, and 14 Gemeinde parts that no pool names share the pools of their Wahlkreis at their average rate. The 1990--2002 pools name only the Kreis or the Land: they go to the Kreis's part of the Wahlkreis, or to the whole Wahlkreis where they are booked on the Land or a Kreis of the Wahlkreis has no pool of its own (1990 Wahlkreis 2, whose one Stendal pool also serves the Osterburg part). Summed over groups of Wahlkreise linked by shared Gemeinden, the Gemeinde file equals the Wahlkreis file `ltw_wkr_unharm` exactly for 1990--2006.
- **Schleswig-Holstein 2017 and 2022**: the source books each Amt's (or kreisfreie Stadt's) postal districts on pseudo-Gemeinde codes 900+ with the Amt and Landtagswahlkreis; they go to that Amt's Gemeinde parts in the Wahlkreis, by A2.
- **Brandenburg**: 2004 Wahlkreis postal votes by in-person valid votes (the source has no Wahlschein counts), 2009 Kreis and 2014/2019/2024 Amt postal votes by eligible voters (2019: A2).
- Until October 2026 MV used allocation loops of its own: no flag on any MV row, fractional counts (1994--2011), 13 valid votes lost to rounding in 2016 and 11 in 2021, Kreis-wide pools in 1994/1998 (Stralsund about 1,900 postal voters short), 2011 members from the 2013 Gemeindeverzeichnis (30 Gemeinden merged by then got none), and Neukalen 2002 postal votes twice. Sachsen-Anhalt spread its pools over the whole Kreis (1990 and 1994 Land pools over the whole state), so Quedlinburg, which counts its own postal votes, received part of Ballenstedt's in 2011 and 26 Gemeinden that count their own received 8,947 pooled voters in 2006, and Brücken-Hackpfüffel's 2011 postal district (34 voters) was dropped. Schleswig-Holstein 2017/2022 spread Amt postal votes over the whole Kreis, unrounded, and capped voters (2 and 25 voters short statewide); Brandenburg 2004/2014/2024 rounded Gemeinde by Gemeinde; none of these were flagged. Rounding each column separately also left receivers' party votes off their valid votes and voters off valid + invalid by a few votes (Sachsen-Anhalt 1990--2016 up to 3.7 points of the shares).

### `far_right` and `far_left`: zero, unknown or a lower bound

`far_right` (harmonized files) sums AfD, NPD (Die Heimat), REP, DVU, Die Rechte, III. Weg, FAP, DDD, FDVP (Freiheitliche Deutsche Volkspartei, a DVU split-off; Sachsen-Anhalt 2002), pro NRW / pro Deutschland (column `pro_deutschland`; rechtsextremistisch per Verfassungsschutz NRW 2011) the DLVH (`deutsche_liga`, which also holds Rheinland-Pfalz 1991's Deutsche Allianz – Heimatbündnis, close to the DLVH's forerunner) and, in Hamburg, the HLA (Hamburger Liste für Ausländerstopp, 1982--1991) and the NL (Nationale Liste, banned in 1995). Not counted: the Schill party (Partei Rechtsstaatlicher Offensive), the Freiheitliche Partei Deutschlands and the DSU (rechtskonservativ, never under Verfassungsschutz observation; until October 2026 it was counted and made up most of East German `far_right` in 1990, e.g. Sachsen 3.6 of 4.3 points). It is `NA` in a state-year whose source names none of these parties but books votes as `other`, where a far-right vote can hide: Niedersachsen 1990 and 1994 (REP 3.7 % in 1994), Bayern 1990 (REP 4.9 %) and Bremen 2011 (Faltblatt with six parties; NPD 1.6 %). A 0 means the source lists every party and none of these stood (Bremen 2023). Where a source names some of them but lumps others into `other`, `far_right` is a lower bound: NRW 1990--2005 come from a table that names REP but not the NPD (0.9 % in 2005), and Bremen 1995 and 2015 leave REP or NPD in `other`. Until October 2026 `far_right` was 0 in all of the NA cases, and also in Bayern 1994--2013 (REP and NPD sat under the unmapped labels `die_republikaner_rep` and `die_heimat_heimat`), Schleswig-Holstein 2000--2009 and Bremen 1999--2007 (only the larger parties were read, so NPD, DVU, REP and the SSW were in `other`).

`far_left` (DKP, KPD, MLPD, SGP, PSG, KBW) and `far_left_w_linke` follow the same rule: they are `NA` where the source names no far-left party but books votes as `other` (Schleswig-Holstein 1996, Niedersachsen 1990 and 1994, Bremen 1991, 1995 and 2011--2019, Nordrhein-Westfalen 1990--2005, Bayern 1990, Saarland 2022, Brandenburg 1994 and 1999), and otherwise a lower bound where some far-left lists may sit in `other`.

### Missing absolute vote counts (percentages only)

- **Bremen 1946--1995**: Raw data only contains vote share percentages at Ortsteil level (column headers stored as graphical objects in Excel). `valid_votes` and `invalid_votes` are NA in `state_unharm`. For the included 1991/1995 elections, harmonized files use `number_voters` as a proxy for `valid_votes`. Multiplication by this proxy does not recover observed party counts. Integer rounding causes small changes to the source percentages.
- **Bremen 2011**: Per-party percentages hardcoded from Statistisches Landesamt Faltblatt PDF (wahlen-bremen.de was offline). Absolute Stimmen available from Amtsblatt for metadata (EV, voters, invalid, valid Gesamtstimmen).
- **Rheinland-Pfalz 1979--2016**: Source file (`LW_RLP_1979_2021.xlsx`) has Landesstimmen party counts and Gesamtsumme, but no eligible_voters/number_voters/invalid_votes. `valid_votes` is available (Gesamtsumme LS). Turnout data only available for 2021 and 2026.

### Rheinland-Pfalz: source data deviations from official Landesergebnisse

The Wahlleiter's Excel maps all results to current (2021-era) municipality boundaries. For elections 1979--2001, 5--6 municipalities are missing because they were founded after 1979 (Langweiler, Trimbs, Hatzenport, Urbar, Leienkaul; plus Dierfeld is absent from all years). This causes the municipal-level sums to fall 0.07--0.31% below official Landesergebnisse. The shortfall disproportionately affects CDU (~-0.3pp) vs SPD (~+0.3pp) for 1979--1991, consistent with small rural CDU-leaning municipalities being underrepresented.

From 2006 onward (all 2,299 municipalities present, only Dierfeld missing), deviations are <=0.16%, and 2011/2016 match the official totals exactly. For 2021, 19 municipalities were merged into neighbors due to low voter counts (Zusammenlegungen) — these municipalities are not listed separately. For 2026 the source lists them: 58 Ortsgemeinden (§ 57 II LWO, § 10 III LWahlG) whose ballots were counted in one of 40 neighbouring Gemeinden of the same Verbandsgemeinde. They are kept as published -- donor rows have NA counts and shares, the receiving row holds the pooled unit including the donors' electorate -- and the pairs are listed in [`rp_2026_pooled_municipalities.csv`](../metadata/rp_2026_pooled_municipalities.csv). Use the 2026 CSV twin of the source file with care: it writes the 13-digit keys in scientific notation; GERDA reads the XLSX.

This is a source data limitation, not a pipeline issue.

### Bavaria Gesamtstimmen

Bayern reports combined Erst+Zweitstimme ("Gesamtstimmen") because both ballots count equally toward proportional seat allocation (unlike Bundestagswahlen). This means `valid_votes ≈ 2 × number_voters` for 1950+. Where all components are available, `valid_votes + invalid_votes = number_voters × 2` is the relevant identity for 1950+. Hamburg and Bremen use multi-vote totals under their five-vote systems. Turnout always counts people (`number_voters / eligible_voters`); no universal votes-equal-voters check applies.

### Other known issues

- **BW 1952**: Constituent-assembly results on 1 January 1979 municipal boundaries. Municipal electorate excludes unallocated certificate holders, so the full electorate and turnout are `NA`. The source retains a KPD +1 / SRP -1 discrepancy against official totals. See [source notes](../../../docs/sources/bw_1952.md).

- **HE 1958/62**: `number_voters`/`invalid_votes` not reported for most municipalities (source omissions preserved as NA)
- **SH 1983**: Verified in-person results for all 1,128 municipal records. The source cannot assign postal votes to municipalities, so overall turnout is `NA`. `llsh` separates Linke Liste Schleswig-Holstein from independents. Wiedenborstel voted in Hennstedt and has no separate row. See [source controls](../derived/sh_1983/README.md).
- **BY 1994--2013**: `eligible_voters` NA (not in Stimmabgabe source files), but `number_voters` is complete: 2,056 observations per election.
- **BB 1990/94**: Verified in-person second-vote results from official polling-station spreadsheets. The 1990 reconstruction preserves election-day boundaries, excludes the incoming Mecklenburg-Vorpommern ballots, and restores Brunow/Dambeck from the original report. Pooled postal votes remain excluded; `turnout` is in-person participation divided by eligible voters, not total election turnout. Statewide comparisons must use matching postal coverage. See [repair notes](../../../docs/sources/bb_1990_1994.md).
- **NRW 1966/1970**: Counts and turnout reconcile with the source tables. Results describe counties and county-free cities with synthetic, election-specific AGS; these are not official municipal identifiers.
- **MV 1990**: in-person-only municipality results; CSU and DSU are separate entries in both official municipality and statewide sources. `cdu_csu` combines CDU and CSU only.
- **NRW 1947--1970**: County-level only (synthetic AGS `050xx000`), present in unharm but cannot be harmonized. 1947/1950 were visually read from scanned PDFs at Wahlkreis level (150 WK) then aggregated to ~84 Kreise. 1947/1950 lack turnout data (only valid\_votes and party counts). 1947 parties: CDU, SPD, FDP, KPD, Zentrum, DRP, RWVP, Unabhängige. 1950 adds DP, RSF, SRP, CSAB. Minor scan-read residuals for 1947 (CDU+4, FDP-1, KPD+2, Z-5 vs official totals)

### Harmonization weight imputation

Where `valid_votes` is NA, the harmonization scripts use positive `number_voters`, then positive `eligible_voters`, then a unit weight. The imputed value remains in the output's `valid_votes` column. For Bremen 1991/1995 this is a voter-based proxy; multiplying party shares by it yields proxy counts. Harmonized counts are rounded to integers, which can slightly change the original shares even without boundary changes. The completeness table counts all nonmissing stored values, including imputed weights; it does not measure source-observed counts.

### Fixed issues (Feb--Mar 2026)

These issues have been resolved but are documented for provenance:

- **BaWü GENESIS API Briefwahl undercount**: The GENESIS Regionalstatistik API only reported Urne (in-person) valid votes for non-kreisfreie BaWü municipalities in 2011/2016, missing all Briefwahl (13--17% undercount). Fixed by replacing API data with raw Excel/CSV files from the Statistisches Landesamt.
- **Fallback join duplication**: Year−1 fallback join in harm scripts was applied to all rows including already-matched multi-target AGS, creating duplicate rows. For BaWü, this inflated eligible_voters by 13,348 in harm_23/harm_25. Fixed by splitting matched/unmatched before fallback.
- **Crosswalk weight rescaling**: The 2025 crosswalk (`ags_1990_to_2025_crosswalk`) had un-rescaled chained weights for 755 AGS-year combinations. All `_25` harmonized datasets were affected. Fixed in `05_build_23_25_ags_crosswalks.R`.
- **BB 2009 percentage rows**: Raw XLS parsing was reading both the Anzahl (absolute counts) and "%" (percentage) sections, treating percentages as additional vote counts. This inflated valid_votes for 223 municipalities. Fixed by adding `pct_row` detection to stop reading before the "%" marker.

See `docs/state_pipeline_audit.md` for the full audit log.
