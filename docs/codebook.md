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
- [Mayoral Elections — Candidate-Level (`mayoral_candidates`)](#mayoral-elections--candidate-level)
- [Mayor Panel (`mayor_panel` / `mayor_panel_harm`)](#mayor-panel-mayor_panel--mayor_panel_harm)
- [Annual Mayor Panel (`mayor_panel_annual` / `mayor_panel_annual_harm`)](#annual-mayor-panel-mayor_panel_annual--mayor_panel_annual_harm)
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

This dataset contains state election results from 1946 to 2024 at the municipality level, using each election year's original administrative boundaries. Covers all 16 German states with 126,989 rows and 439 columns (426 individual party columns).

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
- **NRW 1947–1970**: County-level only (synthetic AGS `050xx000`). Cannot be harmonized; only in unharm.
- **HH 1982**: Two elections (June + December) — both rows present, distinguished by `election_date`.

## State Elections (Municipality Level, Harmonized)

**Files:** `data/state_elections/final/state_harm_21.rds`, `state_harm_23.rds`, `state_harm_25.rds` (or `.csv`)

State election results from 1990 to 2024 harmonized to fixed administrative boundaries (2021, 2023, or 2025) using population-weighted crosswalks. 67,393–67,613 rows, 451 columns.

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

This dataset contains candidate-level results for mayoral elections in 7 German states (Bayern, Niedersachsen, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland, Sachsen, Schleswig-Holstein), 1945--2025. One row per candidate per election cycle (wide format, with both Hauptwahl and Stichwahl results in columns). Includes predicted candidate characteristics (gender, migration background).

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
| `eligible_voters`                 | numeric   | Number of eligible voters (Hauptwahl). NA for RLP (percentage-only data).                                            |
| `number_voters`                   | numeric   | Number of voters (Hauptwahl). NA for RLP.                                                                            |
| `valid_votes`                     | numeric   | Number of valid votes (Hauptwahl). NA for RLP.                                                                       |
| `invalid_votes`                   | numeric   | Number of invalid votes (Hauptwahl). NA for RLP.                                                                     |
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

- **Gender classification** uses the Python `gender-guesser` package (Jorg Michael's `nam_dict.txt`, ~70,000 names with country-specific gender codes). Raw gender data from RLP and SL takes precedence over predictions. The lookup is pre-computed by `code/mayoral_elections/04a_build_gender_lookup.py`. Coverage: 100% of named candidates. Cross-validation accuracy: 99.79% against RLP raw data (F1 = 0.989), 100% against SL raw data.
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
