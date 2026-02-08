# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**GERDA (German Election Database)** — a comprehensive dataset of local, state, and federal election results in Germany at the municipality level, published in *Nature: Scientific Data* (2025). All code is R. Data files use Git LFS (CSV, RDS, PDF, TXT, ZIP, DTA).

Citation: Heddesheimer, Hilbig, Sichart & Wiedemann (2025). *GERDA: The German Election Database*. Scientific Data 12: 618.

## Running Code

- **RStudio project**: Open `german_election_data.Rproj`. The `.Rprofile` auto-loads core packages (tidyverse, readxl, data.table, conflicted, janitor, fixest, etc.) and sets conflict preferences (`dplyr::filter`, `dplyr::lag`, `dplyr::first`).
- **Scripts are standalone**: Each processing script is run via `source()` from the project root. There is no Makefile or build system.
- **State elections** require a DESTATIS GENESIS API account and the `wiesbaden` R package.
- **Check scripts**: Run individually, e.g. `source("code/checks/checks_federal_muni_raw_data_mapping.R")`. Outputs go to `data/data_checks/`.

## Architecture: Data Processing Pipelines

Each election type follows a numbered script pipeline: `00_*` (raw ingestion) → `01_*_unharm` (standardization) → `02_*_harm` (boundary harmonization). Final outputs land in `data/<type>/final/` as both CSV and RDS. For detailed pipeline documentation, see `docs/data_pipeline.md`.

### Crosswalks (foundation for all harmonization)
```
code/crosswalks/01_ags_crosswalk.R        → ags_crosswalks (1990→2021)
code/crosswalks/04_build_21_23_ags_crosswalks.R → ags_1990_to_2023_crosswalk
code/crosswalks/05_build_23_25_ags_crosswalks.R → ags_1990_to_2025_crosswalk
code/crosswalks/02_cty_crosswalk.R        → cty_crosswalks
code/crosswalks/03_partycodes.R           → party name mapping
```
Source: Official BBSR crosswalk files in `data/crosswalks/raw/`. Harmonization uses population-weighted aggregation for municipal mergers.

### Federal Elections — Municipality Level (most developed pipeline)
```
00_federal_muni_raw.R     → federal_muni_raw       (ballot-district → municipality aggregation)
01_federal_muni_unharm.R  → federal_muni_unharm     (AGS standardization, mail-in vote allocation)
02_federal_muni_harm_21.R → federal_muni_harm_21    (harmonized to 2021 boundaries)
03_federal_muni_harm_25.R → federal_muni_harm_25    (harmonized to 2025 boundaries)
```
Raw data: `data/federal_elections/municipality_level/raw/BTW{80..25}/` — CSVs/TXTs per election cycle.

### Federal Elections — County Level
```
01_federal_cty_unharm.R → federal_cty_unharm (1953–2021)
02_federal_cty_harm.R   → federal_cty_harm   (harmonized to 2021)
```
Raw data: `data/federal_elections/county_level/raw/btw{1953..2021}kreis.csv`

### State Elections
```
01_state_unharm.R    → state_unharm        (2006–2019, from DESTATIS GENESIS API)
02_state_harm.R      → state_harm          (harmonized to 2021, old weighted-mean method, 2006-2019 only)
02b_state_harm_21.R  → state_harm_21       (harmonized to 2021, weighted-sum method, all 2006-2024)
03_state_2022-24.R   → state_2224_unharm   (2022-24 elections: SL, SH, NRW, NI, HB, HE, BY, BE, BB, SN, TH)
04_state_harm_23.R   → state_harm_23       (harmonized to 2023)
05_state_harm_25.R   → state_harm_25       (harmonized to 2025, all 2006-2024)
```
Raw data: `data/state_elections/raw/Landtagswahlen/` — downloaded from Regionalstatistik via `wiesbaden` package.

### Municipal Elections
```
01_municipal_unharm.R → municipal_unharm (1990–2021, reads 16 state-specific formats)
02_municipal_harm.R   → municipal_harm   (harmonized to 2021)
```
Raw data: `data/municipal_elections/raw/<state>/` — Excel/CSV files with highly heterogeneous formats per state.

### Mayoral Elections
```
01_mayoral_unharm.R → mayoral_unharm (6 states: Bayern, NRW, Saarland, Sachsen, RLP, Niedersachsen)
```
Raw data: `data/mayoral_elections/raw/<state>/` — no harmonization script exists yet.

## Raw Data Integration Status

### Fully integrated (raw → final datasets)
- **Federal elections (municipality)**: BTW80–BTW25 → 4 final datasets (raw, unharm, harm_21, harm_25)
- **Federal elections (county)**: 1953–2021 → 2 final datasets (unharm, harm)
- **State elections**: 2006–2024 → 7 final datasets (unharm, harm, harm_21, harm_23, harm_25, 2223_unharm, 2224_unharm)
- **Municipal elections**: 16 states → 2 final datasets (unharm, harm)
- **Crosswalks**: BBSR files → 1990→2021, 1990→2023, 1990→2025 chained crosswalks
- **Municipality covariates**: Population registers → ags_area_pop_emp panel

### Partially integrated (raw → unharmonized only, no boundary harmonization)
- **Mayoral elections**: 6 states processed into `mayoral_unharm` (Bayern, NRW, Saarland, Sachsen, RLP, NS) but no `mayoral_harm` — missing harmonization step. RLP has only percentages (no absolute vote counts). NS covers 2006–2025 (9 election years, 1,093 rows, 3 PDF formats). SH raw data exists but no processing code yet.

### Not yet integrated (raw data exists, no processing scripts)
- **County elections (Kreistagswahlen)**: `data/county_elections/raw/` has zip archives and Excel/PDF files for ~10 states (BW, HE, MV, NRW, NS, RLP, SAR, SH, TH) — no processing code, no final datasets
- **European elections**: `data/european_elections/raw/ew24_wbz/` has 2024 ballot-level CSVs — no processing code, no final datasets

## Key Domain Concepts

- **AGS (Amtlicher Gemeindeschluessel)**: 8-digit municipality identifier. First 2 digits = state, first 5 = county. Always treat as character (leading zeros matter).
- **Boundary harmonization**: Maps election results to a fixed set of municipality boundaries (2021 or 2025) using population-weighted crosswalks, so results are comparable across time despite mergers/splits.
- **Mail-in vote allocation**: Joint mail-in voting districts (shared across municipalities) are disaggregated proportionally by polling-card voters.
- **Vote shares**: Proportions (0–1), denominator varies by election type — `number_voters` for federal, `valid_votes` for state/municipal.
- **Quality flags**: Columns like `flag_unsuccessful_naive_merge`, `flag_total_votes_incongruent` indicate data issues.

## Conventions

- Script numbering indicates execution order within each pipeline (00 → 01 → 02 → 03).
- `_unharm` = original boundaries; `_harm` = harmonized to fixed boundaries; `_harm_21` / `_harm_25` = specific target year.
- Party grouping variables (`far_right`, `far_left`, `cdu_csu`) are defined in `00_federal_muni_raw.R`.
- Deprecated scripts live in `old/` subdirectories.
- `partyfacts-mapping.csv` (9 MB, project root) maps party names across data sources.

## Companion R Package

Users can access the data via `gerda` R package: `devtools::install_github("hhilbig/gerda")`. The package function `load_gerda_web()` downloads datasets directly.
