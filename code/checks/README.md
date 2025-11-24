# Data Quality Checks

This folder contains scripts to check data quality and diagnose issues in the German election data.

## Organization

### Federal Elections - Municipality Level

#### Raw Data & Mapping
- **`checks_federal_muni_raw_data_mapping.R`**
  - Checks if all municipalities from raw data are successfully mapped to `ags_21` or `ags_25`
  - Verifies that all raw data appears in harmonized datasets
  - Identifies unmapped AGS and missing municipalities
  - **Consolidates:** `checks_raw_data_mapping.R`, `checks_harmonization_coverage.R`, `checks_harmonization_mapping.R`

#### Harmonization Issues
- **`checks_federal_muni_missing.R`**
  - Diagnoses missing values in harmonized federal election data
  - Compares `federal_muni_harm_21` vs `federal_muni_harm_25`
  - Identifies municipalities with missing election results
  - Analyzes municipality count stability across years

- **`checks_federal_muni_harmonization_stability.R`**
  - Checks why the number of municipalities in `harm_21` is not stable across years
  - Should be exactly the same as 2021 (10641) for all years
  - Identifies extra municipalities that appear in earlier years but not in 2021
  - Analyzes crosswalk issues (ags_21 in crosswalk that don't exist in 2021 data)
  - **Consolidates:** `checks_harm_21_stability.R`, `checks_harm_21_discrepancy_cause.R`, 
    `checks_crosswalk_ags21_not_in_2021.R`, `checks_extra_municipalities_source.R`, 
    `checks_when_extra_disappear.R`

#### General Checks
- **`checks_federal_muni_21.R`** - General checks for harm_21 dataset
- **`checks_federal_muni_25.R`** - General checks for harm_25 dataset

### Crosswalks

- **`checks_crosswalks_zero_weights.R`**
  - Checks for zero weights (pop_cw = 0 or area_cw = 0) in crosswalks
  - Identifies municipalities with zero weights and analyzes why

- **`checks_crosswalks_raw_data.R`**
  - Checks raw data files used for crosswalk creation
  - Identifies missing population/area data in raw Excel files
  - Analyzes change types (law_short) with missing data

### Other Checks

- **`checks_muni_incomplete_elections.R`** - Checks for incomplete election coverage
- **`checks_muni_muni.R`** - Municipality-level checks
- **`checks_federal_county.R`** - County-level checks
- **`checks_state_muni.r`** - State election municipality checks
- **`check_agg_to_state.R`** - Aggregation to state level checks
- **`boundary_changes_2023.R`** - Boundary changes in 2023
- **`measurement_error_allocation.R`** - Measurement error analysis

## Usage

Run individual check scripts to diagnose specific issues:

```r
# Check raw data mapping
source("code/checks/checks_federal_muni_raw_data_mapping.R")

# Check harmonization stability
source("code/checks/checks_federal_muni_harmonization_stability.R")

# Check for missing values
source("code/checks/checks_federal_muni_missing.R")
```

## Output

Most check scripts save results to `data/data_checks/`:
- CSV files with problematic municipalities
- Summary statistics
- Diagnostic reports

## Old Scripts

Deprecated or consolidated scripts are moved to the `old/` folder.

