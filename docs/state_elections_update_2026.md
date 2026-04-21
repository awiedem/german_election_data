# State Elections Pipeline: February 2026 Update

This document records all changes made to the state elections pipeline in February 2026. For data quality details and known issues in the resulting datasets, see [TODO.md](../TODO.md) at the repo root.

## 1. Election Coverage Expansion

`03_state_2022-24.R` was extended from 3 elections (SL, SH, NRW) to 11 elections:

| Election Year | State | Notes |
|---------------|-------|-------|
| 2022 | Saarland | |
| 2022 | Schleswig-Holstein | Non-standard Kennziffer; name-based AGS crosswalk |
| 2022 | North Rhine-Westphalia | Kreisfreie Stadte at 5-digit level only |
| 2022 | Niedersachsen | |
| 2023 | Bremen | AfD does not contest |
| 2023 | Hesse | |
| 2023 | Bavaria | |
| 2023 | Berlin | City-state with 12 Bezirke; uses AGH_W2 (Zweitstimmen) |
| 2024 | Brandenburg | Briefwahl at Amt level; ARS-to-AGS conversion |
| 2024 | Saxony | Both GE and TG rows; Listenstimmen (`_2` suffix) |
| 2024 | Thuringia | 7 header rows; kreisfreie Stadte split across Wahlkreise |

State election coverage now spans 2006-2024 across 15 states (all except Hamburg, which has no municipality-level data in the DESTATIS pipeline).

New parties tracked: **BSW** (Bundnis Sahra Wagenknecht), appearing from 2024 elections onward.

## 2. Aggregation Method Change

### Old method (`02_state_harm.R`)

```r
df_harm <- df_cw |>
  mutate(weights = pop_cw * population) |>
  group_by(ags_21, election_year) |>
  summarise_at(
    vars(turnout:cdu_csu), ~ weighted.mean(.x, weights, na.rm = TRUE)
  )
```

This directly averages vote shares weighted by crosswalk-adjusted population.

### New method (`04_state_harm_23.R`, `05_state_harm_25.R`, `02b_state_harm_21.R`)

```r
# 1. Convert shares to counts before harmonization
df <- df |> mutate(across(all_of(party_vars), ~ .x * valid_votes))

# 2. Weighted sum of counts
votes <- df_cw |>
  group_by(ags_target, election_year) |>
  summarise(across(c(eligible_voters, valid_votes, party_vars),
                   ~ sum(.x * pop_cw, na.rm = TRUE)))

# 3. Convert back to shares
df_harm <- votes |>
  mutate(across(all_of(party_vars), ~ .x / valid_votes))
```

### Why the change?

The weighted mean of shares over-weights small municipalities: a municipality with 100 voters and a municipality with 10,000 voters that merge together should not contribute equally to the resulting share. The weighted-sum-of-counts method:

- Treats votes as counts (which they are), not rates
- Is consistent with the federal elections harmonization pipeline
- Produces correct population-weighted aggregation for municipal mergers

### Backward compatibility

The old `state_harm.rds` (2006-2019) produced by `02_state_harm.R` is retained for backward compatibility. Users who need the 2006-2019 dataset with the original method can continue using it.

## 3. New Harmonization Scripts

| Script | Output | Coverage | Target Boundaries |
|--------|--------|----------|-------------------|
| `04_state_harm_23.R` | `state_harm_23.rds` | 2006-2023 | 2023 |
| `05_state_harm_25.R` | `state_harm_25.rds` | 2006-2024 | 2025 |
| `02b_state_harm_21.R` | `state_harm_21.rds` | 2006-2024 | 2021 |

All three scripts use the weighted-sum-of-counts method. They combine data from two input sources:
- `state_unharm.rds` (2006-2019, from DESTATIS GENESIS API via `01_state_unharm.R`)
- `state_2224_unharm.rds` (2022-2024, from manual processing via `03_state_2022-24.R`)

### Known issue: turnout for pre-2022 data

The `state_unharm.rds` dataset lacks `number_voters` (it has `eligible_voters` and `valid_votes` but not `number_voters`). After harmonization, `number_voters` becomes 0 via `sum(NA, na.rm = TRUE)`. The scripts handle this by retaining the harmonized turnout share (computed from counts) when `number_voters == 0`.

## 4. New Crosswalks

| File | Coverage | Description |
|------|----------|-------------|
| `ags_1990_to_2023_crosswalk.rds` | 1990-2022 | Chained from 1990-to-2021 + 2021-to-2023 |
| `ags_1990_to_2025_crosswalk.rds` | 1990-2024 | Chained from 1990-to-2021 + 2021-to-2023 + 2023-to-2025 |

Built by `04_build_21_23_ags_crosswalks.R` and `05_build_23_25_ags_crosswalks.R` respectively. The original `ags_crosswalks.rds` (1990-2020, target: 2021) remains unchanged and is used by `02_state_harm.R` and `02b_state_harm_21.R`.

## 5. Output Dataset Summary

| Dataset | Rows | Columns | Elections | States | Method |
|---------|------|---------|-----------|--------|--------|
| `state_harm.rds` | ~10,700 | ~30 | 2006-2019 | 15 | Weighted mean of shares |
| `state_harm_21.rds` | ~33,000 | ~44 | 2006-2024 | 15 | Weighted sum of counts |
| `state_harm_23.rds` | ~33,000 | ~44 | 2006-2023 | 15 | Weighted sum of counts |
| `state_harm_25.rds` | ~33,000 | ~44 | 2006-2024 | 15 | Weighted sum of counts |
