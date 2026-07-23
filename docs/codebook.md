---
title: "GERDA Codebook"
date: "`r Sys.Date()`" # Or specify a fixed date
variables:
  documentclass: article
  fontsize: 11pt
  tables: true
  table-engine: longtable
# Remove column_widths as we try a different method
# column_widths: [0.3, 0.15, 0.55]
header-includes:
  - \usepackage[margin=1in]{geometry}
  - \usepackage{tabularx}
  - \usepackage{ragged2e}
  - \usepackage{booktabs}
  - \usepackage{longtable}
  - \usepackage{array} # Needed for p{} columns and new column types
  - \usepackage{etoolbox}
  # Remove custom column types and overrides
  - \setlength{\tabcolsep}{5pt}
  - \renewcommand{\arraystretch}{1.2}
  - \RaggedRight # Apply ragged right justification globally
---

# GERDA Codebook

## Table of Contents

- [General Notes](#general-notes)
- [Federal Elections (Municipality Level, Harmonized)](#federal-elections-municipality-level-harmonized)
- [State Elections (Municipality Level, Harmonized)](#state-elections-municipality-level-harmonized)
- [Municipal Elections (Municipality Level, Harmonized)](#municipal-elections-municipality-level-harmonized)
- [Municipal Council Seats (in `municipal_unharm`)](#municipal-council-seats-in-municipal_unharm)
- [County Council Seats (`county_council_seats`)](#county-council-seats-county_council_seats)
- [Mayoral Elections — Candidate-Level (`mayoral_candidates`)](#mayoral-elections--candidate-level)
- [Mayor Panel (`mayor_panel` / `mayor_panel_harm`)](#mayor-panel-mayor_panel--mayor_panel_harm)
- [Annual Mayor Panel (`mayor_panel_annual` / `mayor_panel_annual_harm`)](#annual-mayor-panel-mayor_panel_annual--mayor_panel_annual_harm)
- [European Elections (Municipality Level)](#european-elections-municipality-level)
- [Municipality Covariates (Area, Population, Employment)](#municipality-covariates-area-population-employment)

---

## General Notes

- **Identifier Structure (AGS):** The primary geographic identifier is the `ags` (Amtlicher Gemeindeschlüssel), representing municipalities. It is an 8-digit character string.
  - The first 2 digits represent the state (`state` variable).
  - The first 5 digits represent the county (`county` variable).
  - The `ags` often includes a leading zero (e.g., "01...") which is crucial for correct interpretation and merging with other administrative data.
- **Vote Shares:** Variables representing party vote shares (e.g., `cdu`, `spd`, `far_right`) are proportions, typically ranging from 0 to 1, calculated based on the number of valid votes or total voters as specified in the dataset notes.

---

This codebook describes the main datasets provided in the GERDA project.

## Federal Elections (Municipality Level, Harmonized)

**File:** `data/federal_elections/municipality_level/final/federal_muni_harm_21.rds` or `data/federal_elections/municipality_level/final/federal_muni_harm_21.csv`

This dataset contains federal election results from 1990 to 2025 at the municipality level, harmonized to 2021 administrative boundaries.

| Variable                       | Type      | Description                                                                                                                                                                                |
| :----------------------------- | :-------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ags`                          | character | Municipality identifier (Amtlicher Gemeindeschlüssel).                                                                                                                                    |
| `election_year`                | numeric   | Year of the federal election.                                                                                                                                                              |
| `state`                        | character | State identifier (numeric code).                                                                                                                                                           |
| `county`                       | character | County identifier (numeric code).                                                                                                                                                          |
| `eligible_voters`              | numeric   | Number of eligible voters (harmonized). Derived from `eligible_voters_orig` after potential adjustments for mail-in voting distribution.                                                    |
| `number_voters`                | numeric   | Number of voters (harmonized). Derived from `number_voters_orig` after potential adjustments for mail-in voting distribution.                                                              |
| `valid_votes`                  | numeric   | Number of valid votes cast (harmonized).                                                                                                                                                   |
| `turnout`                      | numeric   | Voter turnout. Calculated as `number_voters` / `eligible_voters_orig`. Recalculated as `number_voters_orig` / `eligible_voters_orig` if > 1, then capped at 1. See `01_federal_muni_unharm.R`. |
| `turnout_wo_mailin`            | numeric   | Voter turnout based on original (pre-mail-in distribution) counts. Calculated as `number_voters_orig` / `eligible_voters_orig` and capped at 1. See `01_federal_muni_unharm.R`.           |
| `cdu`                          | numeric   | Vote share for CDU (Christlich Demokratische Union). Calculated as votes / `number_voters`.                                                                                                |
| `csu`                          | numeric   | Vote share for CSU (Christlich-Soziale Union in Bayern). Calculated as votes / `number_voters`.                                                                                            |
| `spd`                          | numeric   | Vote share for SPD (Sozialdemokratische Partei Deutschlands). Calculated as votes / `number_voters`.                                                                                       |
| `gruene`                       | numeric   | Vote share for GRÜNE (Bündnis 90/Die Grünen). Includes votes for `B90/GR` from 1990. Calculated as votes / `number_voters`.                                                                |
| `fdp`                          | numeric   | Vote share for FDP (Freie Demokratische Partei). Calculated as votes / `number_voters`.                                                                                                     |
| `linke_pds`                    | numeric   | Vote share for Die Linke or predecessor PDS. Calculated as votes / `number_voters`.                                                                                                        |
| `afd`                          | numeric   | Vote share for AfD (Alternative für Deutschland). Calculated as votes / `number_voters`.                                                                                                     |
| `npd`                          | numeric   | Vote share for NPD (Nationaldemokratische Partei Deutschlands). Calculated as votes / `number_voters`.                                                                                       |
| `rep`                          | numeric   | Vote share for REP (Die Republikaner). Calculated as votes / `number_voters`.                                                                                                              |
| `dvu`                          | numeric   | Vote share for DVU (Deutsche Volksunion). Calculated as votes / `number_voters`.                                                                                                           |
| ... (many other parties)       | numeric   | Vote share for various smaller parties. Calculated as votes / `number_voters`.                                                                                                             |
| `cdu_csu`                      | numeric   | Combined vote share for CDU/CSU. Calculated as votes / `number_voters`.                                                                                                                    |
| `far_right`                    | numeric   | Aggregated vote share for designated far-right parties. Sum of shares for: `afd`, `npd`, `rep`, `die rechte`, `dvu`, `iii. weg`, `fap`, `ddd`, `dsu`. See `00_federal_muni_raw.R`.        |
| `far_left`                     | numeric   | Aggregated vote share for designated far-left parties. Sum of shares for: `dkp`, `kpd`, `mlpd`, `sgp`, `psg`, `kbw`, `v`, `spad`, `bsa`. See `00_federal_muni_raw.R`.                     |
| `far_left_w_linke`             | numeric   | Aggregated vote share for designated far-left parties including Linke/PDS. Sum of shares for: `far_left`, `die linke`, `pds`. See `00_federal_muni_raw.R`.                                  |
| `area`                         | character | Area of the municipality (km²). Sourced from official Gemeindeverzeichnis files, originally numeric but type may change during processing. |
| `population`                   | numeric   | Population of the municipality (thousands). Scaled by dividing raw counts by 1000 during harmonization (`02_federal_muni_harm_21.R`). Sourced from official Gemeindeverzeichnis files. |
| `ags_21`                       | numeric   | Municipality identifier harmonized to 2021 boundaries, based on crosswalk merge in `02_federal_muni_harm_21.R`. |
| `flag_naive_turnout_above_1`   | numeric   | Flag (1/0) indicating if the initial `turnout` calculation (`number_voters` / `eligible_voters_orig`) resulted in a value > 1. See `01_federal_muni_unharm.R`.                          |
| `flag_unsuccessful_naive_merge`| numeric   | Flag (1/0) indicating if the initial merge between election data and crosswalk data failed and required alternative years/AGS for matching. See `02_federal_muni_harm_21.R`.               |
| ... (other flag variables)     | numeric   | Various flags indicating data properties or processing steps (e.g., related to mail-in voting).                                                                                              |

**Notes:**

- Vote shares are proportions of `number_voters` (the potentially adjusted voter count).
- Harmonization refers to adjustments made to account for municipal boundary changes over time, mapping results onto consistent 2021 municipality definitions using population or area weights from `ags_crosswalks.csv`.
- The variables `eligible_voters_orig` and `number_voters_orig` (present in intermediate steps, e.g., `01_federal_muni_unharm.R`) represent counts before mail-in vote distribution adjustments.
- `area` and `population` are sourced from official municipality registers (Gemeindeverzeichnisse); `area` is km², `population` is scaled to thousands.

---

## State Elections (Municipality Level, Unharmonized)

**File:** `data/state_elections/final/state_unharm.rds` (or `.csv`)

This dataset contains state election results from 1946 to 2026 at the municipality level, using each election year's original administrative boundaries. Covers all 16 German states, with each party that ever ran preserved as its own column.

| Variable                        | Type      | Description                                                                                                                                    |
| :------------------------------ | :-------- | :--------------------------------------------------------------------------------------------------------------------------------------------- |
| `ags`                           | character | Municipality identifier (8-digit AGS), using that year's boundaries.                                                                           |
| `election_year`                 | numeric   | Year of the state election.                                                                                                                    |
| `state`                         | character | State identifier (2-digit code, e.g., "01"=SH, "09"=BY).                                                                                      |
| `election_date`                 | Date      | Date of the election.                                                                                                                          |
| `eligible_voters`               | numeric   | Number of eligible voters. NA for BY 1994–2013 (not in source) and HE 1958/62 non-kreisfreie municipalities.                                  |
| `number_voters`                 | numeric   | Number of voters (people who cast ballots). NA for HE 1958/62 (source gap). For BY: in-person + Briefwahl voters (each casts 2 ballots).      |
| `valid_votes`                   | numeric   | Number of valid votes. For BY 1950+: Gesamtstimmen (Erst+Zweit combined), so `valid_votes ≈ 2 × number_voters`.                               |
| `invalid_votes`                 | numeric   | Number of invalid votes. For BY 1950+: `number_voters × 2 - valid_votes`. Clamped to ≥ 0.                                                     |
| `turnout`                       | numeric   | Voter turnout (`number_voters / eligible_voters`). Capped at 1.5; values > 1.5 set to NA. NA where eligible_voters or number_voters missing.   |
| `cdu`, `csu`, `spd`, ...        | numeric   | Vote share for each party (proportion of `valid_votes`, range 0–1). 426 individual party columns. NA means the party did not run in that state-year (zero-vote → NA recoding applied). |
| `other`                         | numeric   | Residual vote share: `max(0, 1 - sum(all named parties))`.                                                                                     |
| `cdu_csu`                       | numeric   | Combined CDU/CSU share. Equals `csu` in BY, `cdu` elsewhere.                                                                                  |
| `flag_naive_turnout_above_1`    | numeric   | Flag (1/0): turnout > 1 before capping. Indicates Briefwahl allocation rounding or data quality issues.                                        |

**Notes:**

- **Bayern (BY)** uses Gesamtstimmen (Erst+Zweitstimme combined). Both ballots count equally for proportional seat allocation and the 5% threshold. Party vote shares are proportions of Gesamtstimmen. The identity `valid_votes + invalid_votes = number_voters × 2` holds for BY 1950+. The 1946 election was single-ballot.
- **Baden-Württemberg (BW) 2026** is the first BW Landtagswahl under the new **two-vote system** (Erst-/Zweitstimme). GERDA records the **Zweitstimme** (Landeslistenstimme) — the proportional list vote — as each party's vote, continuing the single-vote series of BW 1956–2021. `valid_votes`/`invalid_votes`/turnout are likewise the Zweitstimme figures. Source: Statistisches Landesamt BW GENESIS tables 14311_0009 (votes) + 14311_0008 (turnout).
- **Hamburg (HH) 2011+ and Bremen (HB) 2011+** use a 5-vote personalized-list system (Kumulieren/Panaschieren): each voter casts 5 Landesstimmen, which can be cumulated on one candidate or split across candidates and lists. GERDA reports party shares as proportions of cast Landesstimmen, so `valid_votes ≈ 5 × number_voters`. Shares sum to 1 within a municipality and are comparable across HH (or HB) municipalities, but the per-voter denominator differs from single-ballot states. Earlier HH/HB elections used a single-vote system.
- **NRW 1947–1970**: County-level only (synthetic AGS `050xx000`). Cannot be harmonized; only in unharm.
- **HH 1982**: Two elections (June + December) — both rows present, distinguished by `election_date`.

## State Elections (Municipality Level, Harmonized)

**Files:** `data/state_elections/final/state_harm_21.rds`, `state_harm_23.rds`, `state_harm_25.rds` (or `.csv`)

State election results from 1990 to 2026 harmonized to fixed administrative boundaries (2021, 2023, or 2025) using population-weighted crosswalks. BW 2026 (a 2025-vintage boundary year) maps onto each target via the year−1 crosswalk fallback; BW had no Gemeinde mergers in this window, so the mapping is effectively identity and totals are conserved exactly.

| Variable                        | Type      | Description                                                                                                                                    |
| :------------------------------ | :-------- | :--------------------------------------------------------------------------------------------------------------------------------------------- |
| `ags`                           | character | Municipality identifier, mapped to the target year's boundaries (ags_21, ags_23, or ags_25).                                                   |
| `election_year`                 | numeric   | Year of the state election.                                                                                                                    |
| `state`                         | character | State identifier (2-digit code).                                                                                                               |
| `state_name`                    | character | Name of the state holding the election.                                                                                                        |
| `election_date`                 | Date      | Date of the election.                                                                                                                          |
| `eligible_voters`               | numeric   | Number of eligible voters (harmonized via population-weighted crosswalk).                                                                      |
| `number_voters`                 | numeric   | Number of voters (harmonized).                                                                                                                 |
| `valid_votes`                   | numeric   | Number of valid votes cast (harmonized).                                                                                                       |
| `invalid_votes`                 | numeric   | Number of invalid votes (harmonized).                                                                                                          |
| `turnout`                       | numeric   | Voter turnout (`number_voters / eligible_voters`).                                                                                             |
| `cdu`, `csu`, `spd`, ...        | numeric   | Vote share for each party (proportion of `valid_votes`). 426 individual party columns. NA = party did not participate in that state-year.       |
| `other`                         | numeric   | Residual vote share for unlisted parties.                                                                                                      |
| `cdu_csu`                       | numeric   | Combined CDU/CSU share. Equals `csu` in BY, `cdu` elsewhere.                                                                                  |
| `far_right`                     | numeric   | Aggregated far-right vote share: sum of `afd`, `npd`, `rep`, `die_rechte`, `dvu`, `iii_weg`, `fap`, `ddd`, `dsu`, `die_heimat_heimat`, `die_republikaner_rep`. |
| `far_left`                      | numeric   | Aggregated far-left vote share: sum of `dkp`, `kpd`, `mlpd`, `sgp`, `psg`, `kbw`.                                                             |
| `far_left_w_linke`              | numeric   | Far-left including Linke/PDS: `far_left` + `linke_pds` + `pds`.                                                                               |
| `total_vote_share`              | numeric   | Sum of all individual party vote shares (excluding derived columns). Quality check — ideally = 1.0.                                            |
| `perc_total_votes_incogruence`  | numeric   | Continuous deviation: `total_vote_share - 1`. Positive = shares sum to > 1.                                                                   |
| `flag_total_votes_incongruent`  | numeric   | Flag (1/0): `total_vote_share` outside [0.999, 1.001].                                                                                        |
| `flag_unsuccessful_naive_merge` | numeric   | Flag (1/0): initial crosswalk merge failed (recovered via fuzzy time matching or self-mapping).                                                |
| `ags_name_21`                   | character | Municipality name (2021 boundaries). Only in `state_harm_21`.                                                                                  |
| `area_ags`                      | numeric   | Municipality area in km².                                                                                                                      |
| `population_ags`                | numeric   | Municipality population (thousands).                                                                                                           |
| `employees_ags`                 | numeric   | Number of employees subject to social insurance contributions.                                                                                 |
| `pop_density_ags`               | numeric   | Population density (persons per km²).                                                                                                          |

**Notes:**

- Vote shares are proportions of `valid_votes`.
- Harmonization uses population-weighted crosswalks from `data/crosswalks/`. Municipalities that merged are combined; municipalities that split have votes distributed proportionally by population.
- Zero-vote → NA recoding: parties that received zero votes across all municipalities in a state-year are recoded from 0 to NA (distinguishes "did not participate" from "ran but got 0 votes").
- BY Gesamtstimmen: `valid_votes ≈ 2 × number_voters` for BY. See unharmonized notes above.
- See `docs/state_pipeline_audit.md` for detailed data quality documentation.

---

## Municipal Elections (Municipality Level, Harmonized)

**File:** `data/municipal_elections/final/municipal_harm.rds` or `data/municipal_elections/final/municipal_harm.csv`

This dataset contains municipal council (Stadtrat/Gemeinderat) election results since 1990 at the municipality level, harmonized to 2021 administrative boundaries.

| Variable                        | Type      | Description                                                                                                                                                                         |
| :------------------------------ | :-------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ags`                           | character | Municipality identifier (Amtlicher Gemeindeschlüssel).                                                                                                                             |
| `ags_name`                      | character | Name of the municipality.                                                                                                                                                         |
| `election_year`                 | numeric   | Year of the municipal election.                                                                                                                                                   |
| `state`                         | character | State identifier (numeric code).                                                                                                                                                  |
| `county`                        | character | County identifier (numeric code). Derived from `substr(ags, 1, 5)` in `02_municipal_harm.R`.                                                    |
| `eligible_voters`               | numeric   | Number of eligible voters (harmonized). Source: `Wahlberechtigteinsgesamt` from various raw files.                                             |
| `number_voters`                 | numeric   | Number of voters (harmonized). Source: `Wähler` from various raw files.                                                                        |
| `valid_votes`                   | numeric   | Number of valid votes cast (harmonized). Source: `GültigeStimmen` from various raw files.                                                      |
| `turnout`                       | numeric   | Voter turnout. Calculated as `number_voters` / `eligible_voters` (`Wähler` / `Wahlberechtigteinsgesamt`) in `01_municipal_unharm.R`.            |
| `cdu_csu`                       | numeric   | Vote share for CDU/CSU.                                                                                                                                                           |
| `spd`                           | numeric   | Vote share for SPD.                                                                                                                                                               |
| `linke_pds`                     | numeric   | Vote share for Die Linke (or predecessor PDS).                                                                                                                                    |
| `gruene`                        | numeric   | Vote share for GRÜNE.                                                                                                                                                             |
| `afd`                           | numeric   | Vote share for AfD.                                                                                                                                                               |
| `piraten`                       | numeric   | Vote share for Piratenpartei Deutschland.                                                                                                                                         |
| `fdp`                           | numeric   | Vote share for FDP.                                                                                                                                                               |
| `die_partei`                    | numeric   | Vote share for Die PARTEI.                                                                                                                                                        |
| `flag_unsuccessful_naive_merge` | numeric   | Flag (1/0) indicating if the initial merge with crosswalk data failed. See `02_municipal_harm.R`.                                              |
| `area`                          | numeric   | Area of the municipality (km²). Sourced/calculated via crosswalk file (`ags_crosswalks.csv`) from official Gemeindeverzeichnis data.           |
| `population`                    | numeric   | Population of the municipality (thousands). Sourced/calculated/scaled via crosswalk file (`ags_crosswalks.csv`) from official Gemeindeverzeichnis data. |

**Notes:**

- Vote shares are proportions of `valid_votes` (calculated as `prop_*` variables in `01_municipal_unharm.R`).
- `area` and `population` are sourced from official municipality registers (Gemeindeverzeichnisse); `area` is km², `population` is scaled to thousands.
- **Kumulieren and Panaschieren.** Municipal council elections in Baden-Württemberg, Bayern, Hessen, Rheinland-Pfalz, Mecklenburg-Vorpommern, Schleswig-Holstein, Saarland, Sachsen, Sachsen-Anhalt, Thüringen, Brandenburg, Bremen and Niedersachsen allow voters to cumulate votes on a single candidate (Kumulieren) and to split votes across lists (Panaschieren). Each voter casts multiple votes equal to the number of council seats, so `valid_votes` counts cast individual votes (Stimmen), not ballots, and the ratio `valid_votes / number_voters` reflects seat-count × cumulation behavior rather than a ballot count. Party shares (`cdu_csu`, `spd`, …) are proportions of these cast individual votes and sum to 1 within a municipality. They are directly comparable across municipalities within a state but not to single-vote systems (NRW is the main exception, where each voter casts one list vote). State-specific rules (e.g., up-to-3 cumulation in BW vs. up-to-5 in HE/RP) affect the realized `valid_votes / number_voters` ratio but not the share interpretation.

---

## Municipal Council Seats (in `municipal_unharm`)

**File:** `data/municipal_elections/final/municipal_unharm.rds` (or `.csv`) — seat columns are present in the unharmonized dataset only.

The unharmonized municipal dataset carries council seat counts (`seats_*`) alongside the vote-share columns, for the major parties, where the state source reports them. A seat count is the number of council mandates a party won in that municipality's council (Gemeinderat / Stadtrat). Seats are provided only in `municipal_unharm`: seat counts cannot be meaningfully summed across municipalities that merged, so the harmonized datasets (`municipal_harm`, `municipal_harm_25`) do not include them.

| Variable | Type | Description |
| :-- | :-- | :-- |
| `seats_cdu_csu` | numeric | Council seats won by CDU/CSU. |
| `seats_spd` | numeric | Council seats won by SPD. |
| `seats_linke_pds` | numeric | Council seats won by Die Linke (or predecessor PDS). |
| `seats_gruene` | numeric | Council seats won by GRÜNE. |
| `seats_afd` | numeric | Council seats won by AfD. |
| `seats_piraten` | numeric | Council seats won by Piratenpartei. |
| `seats_fdp` | numeric | Council seats won by FDP. |
| `seats_die_partei` | numeric | Council seats won by Die PARTEI. |
| `seats_freie_wahler` | numeric | Council seats won by Freie Wähler. |
| `seats_bsw` | numeric | Council seats won by BSW. |

**Coverage.** Seat data exists only for the states and years where the source provides it, and is `NA` elsewhere. Present: Baden-Württemberg (1989–2024), Hessen (1993–2021), Thüringen (1994–2024), Nordrhein-Westfalen (1994–2025, kreisfreie Städte only from 2025), Brandenburg (2003–2024), Rheinland-Pfalz (2004–2019, excluding kreisfreie Städte), Schleswig-Holstein (2018 only), Mecklenburg-Vorpommern (2019, 2024), Saarland (2019), Sachsen-Anhalt (1994–2019, all six elections), Niedersachsen (2011 and 2016 for the ordinary Gemeinden, and 2021 for the eight kreisfreie Städte), Bremen and Hamburg (2025 only). No seat data: Bayern, Berlin, Sachsen. Two coverage notes: Niedersachsen's eight kreisfreie Städte (Braunschweig, Osnabrück, Oldenburg, Wolfsburg, Salzgitter, Wilhelmshaven, Delmenhorst, Emden — the Region Hannover is covered) are absent from the 2011/2016 seat source and so are `NA` those years, while the 2021 file covers only those cities — the two sources are complementary, never a complete year. In Sachsen-Anhalt 2014 and 2019 the AfD, Freie Wähler, Piraten and Die PARTEI seats are included; the older years carry only the parties that then existed.

**Party seats do not sum to council size.** Only the ten major parties above have seat columns. Local voter groups (Wählergruppen), joint nominations, independent candidates, and smaller parties hold a substantial share of German local council seats but are not represented here, so the row sum of `seats_*` is a lower bound on council size, not the total. Two source-side seat categories (local voter groups and joint nominations) were held back from this release because of an unresolved cross-year labeling inconsistency in the Brandenburg source; they are a planned addition.

---

## County Council Seats (`county_council_seats`)

**File:** `data/county_elections/final/county_council_seats.rds` (or `.csv`)

Seat distributions in German county councils (Kreistage) and the councils of kreisfreie Städte, as a **yearly panel** covering 2008–2025 (400 counties × 18 years = 7,200 rows). This is a council-composition panel, not an election-result table: a county's seat distribution is repeated for every year until the next election changes it, so values are constant between elections. It is published separately from `county_elec_unharm` (which holds election-level Kreistagswahl vote results) for that reason. The extension adds Schleswig-Holstein's 2023 election, the 2024 elections in Baden-Württemberg, Brandenburg, Mecklenburg-Vorpommern, Rheinland-Pfalz, Saarland, Sachsen, Sachsen-Anhalt, and Thüringen, and the Nordrhein-Westfalen 2025 Kreistags-/Ratswahl (53 counties).

| Variable | Type | Description |
| :-- | :-- | :-- |
| `county` | character | County identifier (5-digit Kreisschlüssel; matches `county` in `county_elec_unharm`). |
| `county_name` | character | Name of the county / kreisfreie Stadt. |
| `county_type` | character | `"Landkreis"` or `"kreisfreie Stadt"`. |
| `state` | character | State identifier (first two digits of `county`). |
| `state_name` | character | State name (English). |
| `year` | integer | Calendar year (2008–2025). |
| `government_party` | character | Party of the county executive (Landrat / Oberbürgermeister); `"parteilos"` = independent. Inferred from the historical source column `Regierungspartei`; interpretation is not documented upstream. It is `NA` in 2023–2025 because the new seat-result sources do not identify the governing party. |
| `seats_total` | integer | Total council size. `NA` where the source left the total blank (39 panel rows). |
| `seats_spd`, `seats_cdu_csu`, `seats_fdp`, `seats_gruene`, `seats_freie_wahler`, `seats_linke_pds`, `seats_afd` | integer | Seats won by SPD, CDU/CSU, FDP, GRÜNE, Freie Wähler, Die Linke, AfD. Blank in source = 0 seats. |
| `seats_regional` | integer | Seats won by regional parties (e.g. SSW in Schleswig-Holstein). Not comparable across the 2022/2023 boundary — see the note below; use `seats_local_other` for time series. |
| `seats_other` | integer | Seats won by all remaining parties combined. Not comparable across the 2022/2023 boundary — see the note below. |
| `seats_local_other` | integer | `seats_freie_wahler + seats_regional + seats_other`: all seats not held by the six major parties (CDU/CSU, SPD, Die Linke, GRÜNE, AfD, FDP). Equals `seats_total` minus those six. This sum is comparable across all years and states even though its three components are not (see note). Use it, not the three components, for cross-year comparisons of non-establishment strength. |
| `flag_seats_total_incongruent` | logical | `TRUE` where `seats_total` does not equal the sum of the nine party columns (the five historical source rows plus Donau-Ries carried forward through 2025, for 8 panel rows). Discrepancies in the source are kept as recorded. All newly parsed election rows are congruent. |
| `comment` | character | Free-text note from the source (`Kommentar`), if any. |
| `source` | character | Source URL(s) for the row (`Quelle(n)`). |
| `last_checked` | Date | Date the source entry was last verified. |

**Notes.**

- **Boundaries.** The panel uses a single fixed set of ~400 current (post-reform, roughly 2021) county codes for every year. Counties created by a reform inside the window — Städteregion Aachen (2009), the eight Mecklenburg-Vorpommern counties of the 2011 Kreisgebietsreform, and the merged Landkreis Göttingen (2016) — have `NA`/empty rows for the years before they existed, not backfilled figures. The councils those reforms abolished (for example Mecklenburg-Vorpommern's pre-2011 Landkreise) are not in the file, so the dataset carries no pre-reform county-council composition for reformed areas: Mecklenburg-Vorpommern is empty for 2008–2010, and the merged Göttingen is empty before 2016. Fixing identity at current boundaries is what lets `county` align with `county` in `county_elec_unharm`.
- Provenance: the 2008–2022 rows come from the hand-compiled dataset "Sitzverteilungen der Parteien 2008–2022" (v1.0.0), contributed by coauthor Vincent Heddesheimer. The 2023 and 2024 election rows are parsed from official state statistical-office and returning-officer files cached under `data/county_elections/raw/Kreistagswahlen/`. Per-row official URLs are in `source`; see `docs/county_seats_coverage_2025.md` for the source audit.
- **The three-way split of non-major-party seats is not comparable over time.** `seats_freie_wahler`, `seats_regional`, and `seats_other` are populated under different conventions in the hand-compiled 2008–2022 rows and the parsed 2023–2025 rows, and across states. The 2008–2022 source often folded Freie Wähler and local voter groups into `seats_regional`, while the newer rows assign Freie Wähler to `seats_freie_wahler`, local voter groups to `seats_other`, and reserve `seats_regional` for genuine regional parties (SSW). For example, Landkreis Böblingen shows `seats_regional` 26 and `seats_freie_wahler` 0 in 2019, then `seats_regional` 0 and `seats_freie_wahler` 24 in 2024, with no real change in who won those seats. The six major-party columns are consistent across all years; for any series involving Freie Wähler, regional, or other seats, use `seats_local_other` (their sum), which is defined identically everywhere. The old rows cannot be re-split because the source did not record the detail.
- The ~45 detailed `Sonstige: <party>` columns in the raw file (a decomposition of `seats_other`) are not carried into the published panel; they remain in the raw CSV.
- 328 of the 400 counties match a `county` code in `county_elec_unharm`; unmatched are chiefly the city-states (Hamburg, Bremen) and Rheinland-Pfalz counties, whose vote-level results are covered differently.

---

## European Elections (Municipality Level)

### Unharmonized

**File:** `data/european_elections/final/european_muni_unharm.rds` (or `.csv`)

This dataset contains European Parliament election results from 2009 to 2024 at the municipality level, using each election year's original administrative boundaries. 44,722 rows × 87 columns (71 party columns).

| Variable                     | Type      | Description                                                                                                    |
| :--------------------------- | :-------- | :------------------------------------------------------------------------------------------------------------- |
| `ags`                        | character | Municipality identifier (8-digit AGS), using that year's boundaries.                                           |
| `county`                     | character | County identifier (5-digit code).                                                                              |
| `state`                      | character | State identifier (2-digit code).                                                                               |
| `state_name`                 | character | State name in English.                                                                                         |
| `election_year`              | integer   | Year of the European election (2009, 2014, 2019, 2024).                                                       |
| `election_date`              | Date      | Date of the election.                                                                                          |
| `eligible_voters`            | numeric   | Number of eligible voters.                                                                                     |
| `number_voters`              | numeric   | Number of voters (including invalid ballots). Includes allocated mail-in votes.                                |
| `valid_votes`                | numeric   | Number of valid votes cast.                                                                                    |
| `invalid_votes`              | numeric   | Number of invalid votes cast.                                                                                  |
| `voters_wo_sperrvermerk`     | numeric   | Eligible voters without Sperrvermerk (A1).                                                                     |
| `voters_w_sperrvermerk`      | numeric   | Eligible voters with Sperrvermerk (A2, EU citizens).                                                           |
| `voters_par24_2`             | numeric   | Voters registered under § 24(2) EuWO (A3, Germans abroad).                                                    |
| `voters_w_wahlschein`        | numeric   | Voters with Wahlschein (absentee ballot certificate, B1).                                                      |
| `turnout`                    | numeric   | Voter turnout (`number_voters / eligible_voters`). Capped at 1.                                                |
| `cdu`, `spd`, `gruene`, ...  | numeric   | Vote share for each party (proportion of `number_voters`, range 0–1). 71 party columns across all 4 elections. 0 means the party ran but received no votes; parties that did not run in a given year are also 0.  |
| `flag_turnout_above_1`       | integer   | Flag (1/0): turnout exceeded 1 before capping (Briefwahl allocation rounding artifact).                        |

**Notes:**

- Vote shares use `number_voters` as denominator, consistent with the federal pipeline. Party shares sum to approximately `valid_votes / number_voters` (< 1.0 due to invalid votes).
- Berlin appears as 14 Bezirke rows per year (not aggregated in the unharm file).
- Mail-in votes from shared Briefwahl districts (Ämter/Verbandsgemeinden) are allocated proportionally by eligible voters within each `(county, BWBez)` group.

### Harmonized

**File:** `data/european_elections/final/european_muni_harm.rds` (or `.csv`)

This dataset contains the same European election results harmonized to 2021 municipality boundaries. 42,986 rows × 90 columns.

All variables from the unharmonized version, plus:

| Variable                        | Type    | Description                                                                                       |
| :------------------------------ | :------ | :------------------------------------------------------------------------------------------------ |
| `flag_unsuccessful_naive_merge` | integer | Flag (1/0): initial crosswalk merge failed; resolved via year-1 fallback, identity code, or manual mapping. |
| `flag_aggregated`               | integer | Flag (1/0): municipality was aggregated from multiple predecessor municipalities.                 |
| `n_predecessors`                | integer | Number of predecessor municipalities that were merged into this 2021 boundary.                    |

**Notes:**

- Berlin is aggregated to a single row (AGS 11000000) per year.
- Harmonization converts shares → counts, applies population-weighted crosswalk aggregation, then recomputes shares.
- Crosswalk year mapping: 2009→2009, 2014→2014, 2019→2019, 2024→2020.

---

## Municipality Covariates (Area, Population, Employment)

**File:** `data/covars_municipality/final/ags_area_pop_emp.rds` or `data/covars_municipality/final/ags_area_pop_emp.csv`

This dataset provides yearly time-series data for basic municipality characteristics, harmonized to 2021 administrative boundaries. It is generated by `code/crosswalks/01_ags_crosswalk.R`.

| Variable          | Type      | Description                                                                                                                                                                     |
| :---------------- | :-------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `ags_21`          | numeric   | Municipality identifier, harmonized to 2021 boundaries.                                                                                                                       |
| `ags_name_21`     | character | Name of the municipality (2021 definition).                                                                                                                                     |
| `year`            | numeric   | Year of observation.                                                                                                                                                            |
| `area_ags`        | numeric   | Area of the municipality (km²). Sourced from official Gemeindeverzeichnis files.                                                                                                 |
| `population_ags`  | numeric   | Population of the municipality (thousands). Scaled during crosswalk processing (`01_ags_crosswalk.R`). Sourced from official Gemeindeverzeichnis files.                            |
| `employees_ags`   | numeric   | Number of employees subject to social security contributions (thousands). Scaled during crosswalk processing (`01_ags_crosswalk.R`). Data available from 1997 onwards.           |
| `pop_density_ags` | numeric   | Population density (per km²). Calculated as (`population_ags` * 1000) / `area_ags` in `01_ags_crosswalk.R`.                                                                      |

**Notes:**

- The dataset is a panel covering years from 1990 onwards.
- Area, population, and employee data originate from official Gemeindeverzeichnis sources provided by BBSR.

---

## Mayoral Elections — Candidate-Level

**File:** `data/mayoral_elections/final/mayoral_candidates.rds` or `data/mayoral_elections/final/mayoral_candidates.csv`

This dataset contains candidate-level results for mayoral elections in 13 German states (Baden-Württemberg, Bayern, Brandenburg, Hessen, Mecklenburg-Vorpommern, Niedersachsen, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland, Sachsen, Sachsen-Anhalt, Schleswig-Holstein, Thüringen), 1945--2026. One row per candidate per election cycle (wide format, with both Hauptwahl and Stichwahl results in columns). Includes predicted candidate characteristics (gender, migration background). Note: Baden-Württemberg is a hybrid — Komm.ONE's votemanager portal supplies full candidate-level results (all candidates, both rounds; 274 elections) for the municipalities it publishes, and the Statistisches Landesamt report (winner-only) fills the rest; either way there is no party affiliation, and the elected winner's gender + birth year come from the official register. Brandenburg is a recent-cycle Landeswahlleiter-portal scrape (~2018–2026) with party + all candidates. **Sachsen-Anhalt is a full historical series, 1994–2026**, from the Statistisches Landesamt file "Bürgermeisterwahlen in Sachsen-Anhalt ab 1994" (all candidates + votes for both rounds, party, and the winner's gender/birth year/Amtsantritt), with the older Dezernat-13 extract kept authoritative for the 2019–2026 elections it covers; `ags` is the AGS at the time of the election (2,057 historical codes → 218 current Gemeinden), and 1994 is sparse by design (winner-only for many rows). Hessen is a most-recent-per-Gemeinde snapshot from the StaLA "B VII m Direktwahlen" report (~2020–2026), now its **May-2026 XLSX issue** which lists **every Wahlvorschlag with votes** (full counts, real `n_candidates`) — the elected winner is the highest-voted Wahlvorschlag (named, with register gender); losing Wahlvorschläge carry party + votes but no candidate name. The newest **2026 Kommunalwahlen** are added from sources newer than the historical files: Bayern (8/22 March 2026) from the official Landesamt Mandatsträger file (full votes; winner name/gender/birth year), and Hessen (15 March 2026 + Stichwahlen) as a **hybrid** — the ~12 Gemeinden already in the May-2026 XLSX carry full votes; the rest are from the hessenschau result pages and are **percentage-only** (candidate, party, vote share and turnout, but no absolute vote counts — `candidate_votes`/`valid_votes` NA, like RLP). See the mayoral README for coverage and limitations.

| Variable                          | Type      | Description                                                                                                          |
| :-------------------------------- | :-------- | :------------------------------------------------------------------------------------------------------------------- |
| `ags`                             | character | Municipality identifier (8-digit AGS), original boundaries.                                                          |
| `ags_name`                        | character | Municipality name.                                                                                                   |
| `state`                           | character | State identifier (2-digit code).                                                                                     |
| `state_name`                      | character | State name.                                                                                                          |
| `election_year`                   | numeric   | Year of the election cycle.                                                                                          |
| `election_date`                   | Date      | Hauptwahl (first round) date.                                                                                        |
| `election_date_sw`                | Date      | Stichwahl (runoff) date. NA if no runoff.                                                                            |
| `election_type`                   | character | Type of election (`Buergermeisterwahl`, `Oberbuergermeisterwahl`, `Landratswahl`, `VG-Buergermeisterwahl`, `SG-Buergermeisterwahl`). |
| `has_stichwahl`                   | logical   | TRUE if this election went to a runoff.                                                                              |
| `eligible_voters`                 | numeric   | Number of eligible voters (Hauptwahl). NA for RLP and Hessen 2026 (percentage-only data).                                            |
| `number_voters`                   | numeric   | Number of voters (Hauptwahl). NA for RLP and Hessen 2026.                                                                            |
| `valid_votes`                     | numeric   | Number of valid votes (Hauptwahl). NA for RLP and Hessen 2026.                                                                       |
| `invalid_votes`                   | numeric   | Number of invalid votes (Hauptwahl). NA for RLP and Hessen 2026.                                                                     |
| `turnout`                         | numeric   | Hauptwahl turnout as proportion (0--1).                                                                              |
| `turnout_sw`                      | numeric   | Stichwahl turnout. NA if no runoff.                                                                                  |
| `candidate_name`                  | character | Full candidate name. NA for Bayern (source data has no names).                                                       |
| `candidate_last_name`             | character | Last name.                                                                                                           |
| `candidate_first_name`            | character | First name.                                                                                                          |
| `candidate_gender`                | character | Gender: `"m"` (male) or `"w"` (female). From raw data (RLP, SL) or predicted via `gender-guesser`.                  |
| `candidate_party`                 | character | Party affiliation or label (e.g., CSU, SPD, Parteilos, EB).                                                         |
| `candidate_votes_hw`              | numeric   | Hauptwahl vote count. NA for RLP.                                                                                    |
| `candidate_voteshare_hw`          | numeric   | Hauptwahl vote share (0--1).                                                                                         |
| `candidate_rank_hw`               | numeric   | Hauptwahl rank by votes (1 = most votes).                                                                            |
| `n_candidates_hw`                 | numeric   | Number of candidates in the Hauptwahl.                                                                               |
| `candidate_votes_sw`              | numeric   | Stichwahl vote count. NA if not in runoff.                                                                           |
| `candidate_voteshare_sw`          | numeric   | Stichwahl vote share. NA if not in runoff.                                                                           |
| `candidate_rank_sw`               | numeric   | Stichwahl rank. NA if not in runoff.                                                                                 |
| `n_candidates_sw`                 | numeric   | Number of candidates in the Stichwahl.                                                                               |
| `is_winner`                       | numeric   | 1 if the candidate won the election (HW outright or SW), 0 otherwise.                                               |
| `flag_superseded`                 | logical   | TRUE for a Bayern round that did not seat the mayor and is superseded by a later valid round — either an annulled round (`Wahlart` "... ungültig") or a Hauptwahl with no absolute majority (winner < 50%) that was not resolved by a Stichwahl and is followed by a repeat Hauptwahl (*Neuwahl*) within 250 days. Duly-won (≥50%) Hauptwahlen preceding a later by-election are NOT flagged. Constant within an election. Rows are kept, not dropped; filter `== FALSE` for decisive rounds only. FALSE for all non-Bayern states. |
| `candidate_birth_year`            | numeric   | Birth year. NI only.                                                                                                 |
| `candidate_profession`            | character | Profession. NI only.                                                                                                 |
| `office_type`                     | character | Office type. BY and SL only.                                                                                         |
| `candidate_gender_source`         | character | Gender data source: `"raw"` (from election authority data) or `"predicted"` (from name classification).              |
| `candidate_gender_method`         | character | Classification method: `raw`, `full_de` (Germany-specific match), `full_global`, `hyphen_first_de`, `hyphen_first_global`, `accent_norm_global`, `manual`. |
| `candidate_gender_prob`           | numeric   | Confidence score for gender classification (0--1). 1.0 for raw data; 0.99 for `full_de`/`manual`; 0.95 for `hyphen_first_de`; 0.90 for global matches. |
| `candidate_name_origin`           | character | Predicted name origin: `"german"`, `"turkish"`, `"arabic"`, `"eastern_european"`, `"southern_european"`.             |
| `candidate_name_origin_conf`      | numeric   | Confidence in origin classification (0.50--0.95).                                                                    |
| `candidate_name_origin_method`    | character | Detection method: `"combined"` (first+last match), `"surname_match"`, `"firstname_match"`, `"surname_pattern"`, `"default"`. |
| `candidate_migration_bg`          | integer   | Binary migration background: 0 = German-origin name, 1 = likely non-German origin name.                             |
| `candidate_migration_bg_prob`     | numeric   | Probability of migration background (continuous, 0--1).                                                              |
| `candidate_local_surname`         | integer   | Placeholder (NA). Local surname rootedness — awaiting telephone directory data.                                      |
| `candidate_surname_county_share`  | numeric   | Placeholder (NA). Share of surname occurrences in the focal county.                                                  |
| `candidate_surname_n_counties`    | integer   | Placeholder (NA). Number of counties where this surname appears.                                                     |
| `candidate_surname_overrep_ratio` | numeric   | Placeholder (NA). Ratio of observed/expected surname frequency in focal county.                                      |

**Notes:**

- **Gender classification** uses the Python `gender-guesser` package (Jorg Michael's `nam_dict.txt`, ~70,000 names with country-specific gender codes). Raw gender data from RLP, SL, and BW (the latter from the official register) takes precedence over predictions. The lookup is pre-computed by `code/mayoral_elections/04a_build_gender_lookup.py`. Coverage: 100% of named candidates. Cross-validation accuracy: 99.79% against RLP raw data (F1 = 0.989), 100% against SL raw data.
- **Migration background** is a probabilistic estimate based on name patterns (Turkish, Arabic, Eastern European, Southern European surname/firstname lists and regex patterns). It should not be interpreted as verified migration status. Coverage: all candidates with last names (14,859). Low-confidence classifications (conf < 0.80) are predominantly Eastern European surname endings.
- **Local surname rootedness** columns are placeholders populated with NA values. Implementation is blocked on telephone directory data.
- **Bayern** has no candidate names in the source data, so gender, migration background, and local surname columns are all NA for Bayern rows.

---

## Mayor Panel (`mayor_panel` / `mayor_panel_harm`)

**Files:** `data/mayoral_elections/final/mayor_panel.rds` (or `.csv`), `data/mayoral_elections/final/mayor_panel_harm.rds`

One row per person per election. Tracks individual mayors across multiple terms using unique person IDs. The `_harm` version maps AGS codes to 2021 municipal boundaries. Includes the same candidate characteristics columns as `mayoral_candidates` (gender, migration background), carried forward from the winning candidate.

| Variable                          | Type      | Description                                                                                           |
| :-------------------------------- | :-------- | :---------------------------------------------------------------------------------------------------- |
| `person_id`                       | character | Unique mayor identifier (e.g., `p_09_00001` for Bayern, `p_05_00001` for NRW).                       |
| `ags`                             | character | Municipality identifier (8-digit AGS), original boundaries.                                           |
| `ags_21`                          | character | Municipality identifier mapped to 2021 boundaries (`_harm` only).                                     |
| `state`                           | character | State identifier (2-digit code).                                                                      |
| `election_year`                   | numeric   | Year of the election.                                                                                 |
| `election_date`                   | Date      | Date of the decisive round (Stichwahl date if applicable).                                            |
| `term_number`                     | numeric   | Sequential term count within (person, municipality), starting at 1.                                   |
| `consecutive_terms`               | numeric   | Number of consecutive terms (resets if gap > 1 election cycle).                                       |
| `winner_party`                    | character | Party of the winning candidate.                                                                       |
| `winner_voteshare`                | numeric   | Vote share in the decisive round (0--1).                                                              |
| `winning_margin`                  | numeric   | Vote share difference between winner and runner-up (0--1).                                            |
| `margin_change`                   | numeric   | Change in winning margin from previous election.                                                      |
| `n_candidates`                    | numeric   | Number of candidates in the election.                                                                 |
| `is_incumbent`                    | numeric   | 1 if `term_number >= 2`, else 0.                                                                      |
| `next_runs_again`                 | numeric   | 1 if this person wins the next election, 0 if different person wins, NA if no subsequent election.    |
| `party_switch`                    | numeric   | 1 if the winning party changed from the previous election.                                            |
| `is_new_party_mayor`              | numeric   | 1 if this is the first time this party wins in this municipality.                                     |
| `tenure_start`                    | numeric   | Year of the mayor's first election in this municipality.                                              |
| `years_in_office`                 | numeric   | `election_year - tenure_start`.                                                                       |
| `term_start_date`                 | Date      | Date of first taking office (Bayern: Amtsantritt; others: first election date).                       |
| `n_terms`                         | numeric   | Total number of terms observed for this person.                                                       |
| `total_tenure_years`              | numeric   | Year span from first to last election.                                                                |
| `has_margin_variation`            | logical   | Whether winning margin varies across this person's terms (useful for FE feasibility).                 |
| `candidate_gender`                | character | Mayor's gender: `"m"` / `"w"`. From raw data or predicted. NA for Bayern.                            |
| `candidate_gender_source`         | character | `"raw"` or `"predicted"`. NA for Bayern.                                                              |
| `candidate_gender_prob`           | numeric   | Confidence score (0--1). See `mayoral_candidates` section for details.                                |
| `candidate_gender_method`         | character | Classification method. See `mayoral_candidates` section for details.                                  |
| `candidate_migration_bg`          | integer   | Binary migration background (0/1). NA for Bayern.                                                     |
| `candidate_migration_bg_prob`     | numeric   | Probability of migration background (0--1).                                                           |
| `candidate_name_origin`           | character | Fine-grained name origin category.                                                                    |
| `candidate_name_origin_conf`      | numeric   | Confidence in origin classification (0.50--0.95).                                                     |
| `candidate_name_origin_method`    | character | Detection method for origin classification.                                                           |

**Coverage**: 14,452 unique mayors (unharm) / 13,971 (harm), spanning 34,495 / 33,319 person-elections. Candidate characteristics available for 3,089 person-elections (non-Bayern states).

---

## Annual Mayor Panel (`mayor_panel_annual` / `mayor_panel_annual_harm`)

**Files:** `data/mayoral_elections/final/mayor_panel_annual.rds` (or `.csv`), `data/mayoral_elections/final/mayor_panel_annual_harm.rds`

One row per mayor per year. Forward-fills election-level data across the mayor's term. The `_harm` version maps AGS codes to 2021 boundaries. Candidate characteristics (gender, migration background) are constant within each mayor-term.

| Variable                          | Type      | Description                                                                                           |
| :-------------------------------- | :-------- | :---------------------------------------------------------------------------------------------------- |
| `ags`                             | character | Municipality identifier (8-digit AGS), original boundaries.                                           |
| `ags_21`                          | character | Municipality identifier mapped to 2021 boundaries (`_harm` only).                                     |
| `year`                            | numeric   | Calendar year.                                                                                        |
| `person_id`                       | character | Unique mayor identifier.                                                                              |
| `state`                           | character | State identifier (2-digit code).                                                                      |
| `election_year`                   | numeric   | Year of the election that started this term.                                                          |
| `election_date`                   | Date      | Date of the decisive round.                                                                           |
| `term_number`                     | numeric   | Term count within (person, municipality).                                                             |
| `winner_party`                    | character | Party of the mayor (constant within term).                                                            |
| `winner_voteshare`                | numeric   | Vote share in the decisive round (constant within term).                                              |
| `winning_margin`                  | numeric   | Winner-runner-up margin (constant within term).                                                       |
| `n_candidates`                    | numeric   | Number of candidates (constant within term).                                                          |
| `is_incumbent`                    | numeric   | 1 if `term_number >= 2`.                                                                              |
| `next_runs_again`                 | numeric   | Whether this person wins the next election.                                                           |
| `years_since_election`            | numeric   | `year - election_year`.                                                                               |
| `years_to_next_election`          | numeric   | Years until the next election in this municipality (NA if unknown).                                   |
| `electoral_cycle_pos`             | numeric   | Position in the electoral cycle, 0 (election year) to <1 (year before next election).                 |
| `tenure_start`                    | numeric   | Year of first election.                                                                               |
| `term_start_date`                 | Date      | Date of first taking office.                                                                          |
| `candidate_gender`                | character | Mayor's gender (constant within term). NA for Bayern.                                                 |
| `candidate_gender_source`         | character | `"raw"` or `"predicted"`.                                                                             |
| `candidate_gender_prob`           | numeric   | Confidence score (0--1).                                                                              |
| `candidate_gender_method`         | character | Classification method.                                                                                |
| `candidate_migration_bg`          | integer   | Binary migration background (0/1, constant within term).                                              |
| `candidate_migration_bg_prob`     | numeric   | Probability of migration background (0--1).                                                           |
| `candidate_name_origin`           | character | Fine-grained name origin category.                                                                    |
| `candidate_name_origin_conf`      | numeric   | Confidence in origin classification.                                                                  |
| `candidate_name_origin_method`    | character | Detection method for origin classification.                                                           |

**Coverage**: 185,112 person-years (unharm) / 179,011 (harm), years 1945--2025.

---

## Work in progress

The database is work in progress. If you have any suggestions, comments, or issues, please feel free to email us or to file an issue.

---

## Citation

Please cite the accompanying [paper](https://www.nature.com/articles/s41597-025-04811-5) when using this dataset:

Heddesheimer, Vincent, Hanno Hilbig, Florian Sichart, & Andreas Wiedemann. 2025. *GERDA: German Election Database*. Nature: Scientific Data, 12: 618.

```
@article{Heddesheimer2025GERDA,
   author = {Vincent Heddesheimer and Hanno Hilbig and Florian Sichart and Andreas Wiedemann},
   doi = {10.1038/s41597-025-04811-5},
   issn = {2052-4463},
   issue = {1},
   journal = {Scientific Data},
   month = {4},
   pages = {618},
   title = {GERDA: The German Election Database},
   volume = {12},
   url = {https://www.nature.com/articles/s41597-025-04811-5},
   year = {2025}
}
```
