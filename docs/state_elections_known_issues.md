# State Elections (Harmonized): Known Issues

Reference document for users of the harmonized state election datasets. Based on automated checks in `code/checks/checks_state_harm.R`. For methodology and changelog details, see [state_elections_update_2026.md](state_elections_update_2026.md).

Last updated: February 2026

## 1. Dataset Overview

Four state election datasets are available:

| Dataset | File | Rows | Cols | Elections | States | Target Boundaries |
|---------|------|------|------|-----------|--------|-------------------|
| `state_harm_21` | `data/state_elections/final/state_harm_21.rds` | ~33,286 | 44 | 2006--2024 | 15 | 2021 |
| `state_harm_23` | `data/state_elections/final/state_harm_23.rds` | ~27,000 | 43 | 2006--2023 | 14 | 2023 |
| `state_harm_25` | `data/state_elections/final/state_harm_25.rds` | ~33,189 | 44 | 2006--2024 | 15 | 2025 |
| `state_2224_unharm` | `data/state_elections/final/state_2224_unharm.rds` | ~6,405 | 35 | 2022--2024 | 11 | Original |

All three harmonized datasets use the weighted-sum-of-counts aggregation method (see `docs/state_elections_update_2026.md` for details). An older `state_harm.rds` (2006--2019, weighted mean of shares) is retained for backward compatibility but is not covered here.

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

## 2. Coverage

No dataset includes **Hamburg** (state `02`). Hamburg lacks municipality-level state election data in the DESTATIS pipeline.

| Dataset | State-year combinations | States | Year range |
|---------|------------------------|--------|------------|
| `state_harm_21` | 48 | 15 | 2006--2024 |
| `state_harm_23` | 40 | 14 | 2006--2023 |
| `state_harm_25` | 48 | 15 | 2006--2024 |
| `state_2224_unharm` | 11 | 11 | 2022--2024 |

`state_harm_23` excludes Bremen (state `04`) and all 2024 elections. The 11 state-year combinations in `state_2224_unharm` are: SL 2022, SH 2022, NRW 2022, NI 2022, HB 2023, HE 2023, BY 2023, BE 2023, BB 2024, SN 2024, TH 2024.

## 3. Turnout Issues

### NA turnout (all datasets)

Four Thuringia municipalities have `NA` turnout across all harmonized datasets:

| AGS | Municipality |
|-----|-------------|
| `16063033` | |
| `16074082` | |
| `16075087` | |
| `16075105` | |

These are source data issues in the Thuringia state election results. Affects 4 rows per dataset where the election year is 2024.

### Turnout > 1 (`state_harm_21`, `state_harm_25`)

11 rows have turnout slightly exceeding 1.0 in both `state_harm_21` and `state_harm_25`. These originate from source data issues in Schleswig-Holstein 2022 and Sachsen 2024 (likely related to mail-in vote allocation at the municipality level). The excess is small (typically < 1.05).

### Turnout = 0 (`state_harm_23` only)

`state_harm_23` has ~26,858 rows with `turnout = 0`, concentrated in pre-2022 election years. This is a **known issue**: the `state_unharm.rds` input (2006--2019) lacks `number_voters`, which becomes 0 after `sum(NA, na.rm = TRUE)` during harmonization. The `state_harm_21` and `state_harm_25` scripts include a fix that retains the harmonized turnout share when `number_voters == 0`; `state_harm_23` does not include this fix.

**Recommendation**: For analyses requiring turnout, prefer `state_harm_21` or `state_harm_25` over `state_harm_23`.

### Additional NA turnout (`state_harm_23` only)

One additional AGS has `NA` turnout in `state_harm_23` only: `07336021` (Rheinland-Pfalz). This municipality is not present in the other harmonized datasets at this AGS code due to different crosswalk targets.

## 4. Vote Share Issues

### `total_vote_share` deviations

Approximately 2% of rows have `total_vote_share` deviating from 1.0 by more than 5 percentage points. This is expected behavior: the tracked party columns (CDU/CSU, SPD, Greens, FDP, Linke/PDS, AfD, BSW, other) do not cover every party that contests every election. Residual votes go to untracked minor parties.

### Very low `total_vote_share`

4 rows across `state_harm_21` / `state_harm_25` have `total_vote_share < 0.7`. These correspond to elections where a large share of votes went to local or regional parties not captured in the standard party columns.

## 5. Party-Specific Issues

### Bremen AfD (`state_harm_21`, `state_harm_25`)

The AfD did not contest the 2023 Bremen election. In `state_harm_21` and `state_harm_25`, the AfD vote share for Bremen rows is coded as **0** rather than `NA`. Users distinguishing between "party received zero votes" and "party did not contest" should treat Bremen AfD values accordingly. (The `state_2224_unharm` correctly codes these as `NA`.)

### BSW absent from `state_harm_23`

`state_harm_23` does not include a `bsw` (Bundnis Sahra Wagenknecht) column. BSW first contested elections in 2024, which is outside the 2006--2023 coverage of `state_harm_23`.

### `grune` vs. `gruene` naming

The `state_2224_unharm` dataset uses `grune` for the Greens column. All three harmonized datasets use `gruene`. Users merging unharmonized and harmonized data should rename accordingly.

## 6. Quality Flags

Two quality flag columns are present in the harmonized datasets (but not in `state_2224_unharm`):

### `flag_unsuccessful_naive_merge`

This flag indicates rows where the AGS could not be directly matched in the crosswalk and required fuzzy or manual matching.

| Dataset | Flagged rows | Rate | Notes |
|---------|-------------|------|-------|
| `state_harm_21` | ~6,400 | ~19% | **100% for 2022--2024 elections** (expected: these come from `state_2224_unharm`, a different source pipeline than 2006--2019) |
| `state_harm_23` | ~15 | ~0.05% | Low rate |
| `state_harm_25` | ~15 | ~0.05% | Low rate |

The high flag rate for 2022--2024 in `state_harm_21` is mechanical: the 2006--2019 data comes from the DESTATIS GENESIS API (`01_state_unharm.R`) while 2022--2024 data comes from manual processing (`03_state_2022-24.R`). The flag reflects the different source pipeline, not data quality problems.

### `flag_total_votes_incongruent`

This flag indicates rows where the sum of individual party vote counts does not match the reported total valid votes.

| Dataset | Flagged rows | Rate | Concentration |
|---------|-------------|------|---------------|
| `state_harm_21` | ~3,600 | ~11% | 2008 (~40%), 2018 (~67%) |
| `state_harm_23` | ~3,600 | ~13% | Same pattern |
| `state_harm_25` | ~3,600 | ~11% | Same pattern |

The concentration in 2008 and 2018 is a source data issue from the DESTATIS pipeline, not a harmonization artifact.

## 7. Covariate Gaps

Covariates (`area_ags`, `population_ags`, `employees_ags`, `pop_density_ags`) are present in all three harmonized datasets but have systematic gaps:

| Dataset | Covariate gap | Reason |
|---------|---------------|--------|
| `state_harm_21` | 100% NA for 2022--2024 elections | Covariate source (`ags_area_pop_emp.rds`) only covers through ~2021 |
| `state_harm_23` | `employees_ags` 100% NA for 2022--2023 | Employment data ends before other covariates |
| `state_harm_25` | 100% NA for 2024 elections | Covariate source only covers through ~2023 |

Population values are in **thousands** (e.g., Berlin ~ 3,782; Munchen ~ 1,510). The `state_2224_unharm` dataset has no covariate columns.

## 8. Cross-Dataset Differences

Aggregating valid votes by state and election year, then comparing across harmonization targets, reveals differences of up to ~5% for some state-years. This is expected: different crosswalk targets (2021 vs. 2023 vs. 2025) use different population-weighted mappings for municipal mergers and splits.

The largest differences are:

| State | Approx. max difference |
|-------|----------------------|
| Sachsen (SN) | ~4.6% |
| Rheinland-Pfalz (RLP) | ~2.4% |
| Other states | < 2% |

These differences are not data errors. They reflect genuine differences in how votes are allocated when municipality boundaries changed between the target years. Party-level vote totals show similar magnitudes.

## 9. `state_harm_23` Specific Issues

`state_harm_23` (produced by `04_state_harm_23.R`) has several issues not present in the other harmonized datasets:

1. **Turnout = 0 for pre-2022 data**: ~26,858 rows. The `number_voters` bug was not fixed in this script (see Section 3).
2. **No BSW column**: Coverage ends in 2023, before BSW contested elections.
3. **No Bremen**: Bremen (state `04`) is excluded entirely.
4. **No 2024 elections**: Brandenburg, Sachsen, and Thuringia 2024 elections are not included.
5. **Additional NA turnout**: AGS `07336021` (RLP) has NA turnout, not seen in other datasets.
6. **14 states, 40 state-year combos**: vs. 15 states and 48 combos in `state_harm_21`/`state_harm_25`.

For most use cases, `state_harm_21` or `state_harm_25` are preferred over `state_harm_23` due to broader coverage and the turnout fix.
