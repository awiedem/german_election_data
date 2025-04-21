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
- [Municipality Covariates (Area, Population, Employment)](#municipality-covariates-area-population-employment)
- [County Covariates (INKAR, Employment, Income, Unemployment)](#county-covariates-inkar-employment-income-unemployment)

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

**File:** `data/federal_elections/municipality_level/final/federal_muni_harm.rds` or `data/federal_elections/municipality_level/final/federal_muni_harm.csv`

This dataset contains federal election results from 1990 to 2021 at the municipality level, harmonized to 2021 administrative boundaries.

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
| `population`                   | numeric   | Population of the municipality (thousands). Scaled by dividing raw counts by 1000 during harmonization (`02_federal_muni_harm.R`). Sourced from official Gemeindeverzeichnis files. |
| `ags_21`                       | numeric   | Municipality identifier harmonized to 2021 boundaries, based on crosswalk merge in `02_federal_muni_harm.R`. |
| `flag_naive_turnout_above_1`   | numeric   | Flag (1/0) indicating if the initial `turnout` calculation (`number_voters` / `eligible_voters_orig`) resulted in a value > 1. See `01_federal_muni_unharm.R`.                          |
| `flag_unsuccessful_naive_merge`| numeric   | Flag (1/0) indicating if the initial merge between election data and crosswalk data failed and required alternative years/AGS for matching. See `02_federal_muni_harm.R`.               |
| ... (other flag variables)     | numeric   | Various flags indicating data properties or processing steps (e.g., related to mail-in voting).                                                                                              |

**Notes:**

- Vote shares are proportions of `number_voters` (the potentially adjusted voter count).
- Harmonization refers to adjustments made to account for municipal boundary changes over time, mapping results onto consistent 2021 municipality definitions using population or area weights from `ags_crosswalks.csv`.
- The variables `eligible_voters_orig` and `number_voters_orig` (present in intermediate steps, e.g., `01_federal_muni_unharm.R`) represent counts before mail-in vote distribution adjustments.
- `area` and `population` are sourced from official municipality registers (Gemeindeverzeichnisse); `area` is km², `population` is scaled to thousands.

---

## State Elections (Municipality Level, Harmonized)

**File:** `data/state_elections/final/state_harm.rds` or `data/state_elections/final/state_harm.csv`

This dataset contains state election results (since mid-2000s) at the municipality level, harmonized to 2021 administrative boundaries.

| Variable                        | Type      | Description                                                                                                                                    |
| :------------------------------ | :-------- | :--------------------------------------------------------------------------------------------------------------------------------------------- |
| `ags`                           | character | Municipality identifier (Amtlicher Gemeindeschlüssel).                                                                                          |
| `election_year`                 | numeric   | Year of the state election.                                                                                                                    |
| `state`                         | character | State identifier (numeric code) of the state holding the election.                                                                             |
| `state_name`                    | character | Name of the state holding the election.                                                                                                        |
| `eligible_voters`               | numeric   | Number of eligible voters (harmonized). Source: `WAHL01_val` from Regionalstatistik table `14*`.                                               |
| `valid_votes`                   | numeric   | Number of valid votes cast (harmonized). Source: `WAHL04_val` from Regionalstatistik table `14*`.                                              |
| `turnout`                       | numeric   | Voter turnout (%). Source: `WAHLSR_val` from Regionalstatistik table `14*`, divided by 100.                                                  |
| `cdu`                           | numeric   | Vote share for CDU. Calculated as party votes / `valid_votes`.                                                                                 |
| `csu`                           | numeric   | Vote share for CSU (non-missing only in Bavaria). Calculated as party votes / `valid_votes`.                                                   |
| `spd`                           | numeric   | Vote share for SPD. Calculated as party votes / `valid_votes`.                                                                                 |
| `gruene`                        | numeric   | Vote share for GRÜNE. Calculated as party votes / `valid_votes`.                                                                               |
| `fdp`                           | numeric   | Vote share for FDP. Calculated as party votes / `valid_votes`.                                                                                 |
| `linke_pds`                     | numeric   | Vote share for Die Linke (or predecessor PDS). Calculated as party votes / `valid_votes`.                                                        |
| `afd`                           | numeric   | Vote share for AfD. Calculated as party votes / `valid_votes`.                                                                                 |
| `other`                         | numeric   | Combined vote share for all parties not listed separately. Calculated as party votes / `valid_votes`.                                          |
| `cdu_csu`                       | numeric   | Combined vote share for CDU/CSU. Calculated as party votes / `valid_votes`.                                                                    |
| `flag_unsuccessful_naive_merge` | numeric   | Flag (1/0) indicating if the initial merge with crosswalk data failed. See `02_state_harm.R`.                                                  |
| `flag_total_votes_incongruent`  | numeric   | Flag (1/0) indicating if the sum of reported party vote shares exceeded 1. See `02_state_harm.R`.                                              |
| `total_vote_share`              | numeric   | Sum of all party vote shares (check variable, ideally close to 1).                                                                             |

**Notes:**

- Vote shares are proportions of `valid_votes`.
- Harmonization refers to adjustments made to account for municipal boundary changes over time, mapping results onto consistent 2021 municipality definitions.

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
