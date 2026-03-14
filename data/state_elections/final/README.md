# State Elections Final Data

## Current files

### Unharmonized (original municipal boundaries)

- **`state_unharm.rds/.csv`** -- Municipality-level state election results from official state statistics offices. Covers all 16 German states, 1946--2024 (availability varies by state). ~125k rows, ~440 columns (all individual party columns preserved). Produced by `code/state_elections/01b_state_unharm_raw.R`.

### Harmonized (fixed administrative boundaries)

All harmonized files use population-weighted crosswalks to map results onto fixed municipal boundaries, enabling comparison across time. Coverage: 1990--2024.

- **`state_harm_21.rds/.csv`** -- Harmonized to 2021 municipal boundaries. Produced by `code/state_elections/02b_state_harm_21.R`.
- **`state_harm_23.rds/.csv`** -- Harmonized to 2023 municipal boundaries. Produced by `code/state_elections/04_state_harm_23.R`.
- **`state_harm_25.rds/.csv`** -- Harmonized to 2025 municipal boundaries. Produced by `code/state_elections/05_state_harm_25.R`.

## Archive: `_old_regionalstatistik/`

Contains older versions of the data that were based on the DESTATIS Regionalstatistik API (2006--2019) and manually compiled post-2020 elections. These have been superseded by the stats-office raw data pipeline above.

- `state_unharm.rds/.csv` -- Old DESTATIS API data (2006--2019)
- `state_2224_unharm.rds/.csv` -- Manually compiled post-2020 elections
- `state_2223_unharm.rds/.csv` -- Legacy version
- `state_harm.rds/.csv` -- Old harmonized data (deprecated)

## Column structure

### Meta columns
`ags`, `election_year`, `state`, `election_date`, `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`

### Party columns
All individual party vote shares (e.g., `spd`, `cdu`, `gruene`, `fdp`, `linke_pds`, `afd`, `bsw`, ...) plus `other` (residual) and `cdu_csu` (combined). Party names are normalized to snake_case via `normalise_party()`.

### Harmonized files additionally include
`county`, `state_name`, `flag_unsuccessful_naive_merge`, `flag_total_votes_incongruent`, and area/population covariates.
