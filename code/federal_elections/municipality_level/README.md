# Federal Election Data Harmonization

This directory contains scripts for harmonizing German federal election data at the municipality level to both 2021 and 2025 boundaries. The harmonization process ensures consistent geographic units across time, accounting for municipal mergers, splits, and boundary changes.

## Harmonization Scripts

### 1. Harmonization to 2021 Boundaries (`02_federal_muni_harm_21.R`)

This script harmonizes federal election data from 1990-2025 to 2021 municipality boundaries. Key features:

- Uses population-weighted aggregation for merged municipalities
- Handles special cases of unsuccessful merges through manual corrections
- Processes mail-in voting districts
- Calculates vote shares and turnout rates
- Flags cases with potential data issues

### 2. Harmonization to 2025 Boundaries (`03_federal_muni_harm_25.R`)

This script harmonizes the same data to 2025 municipality boundaries. Key features:

- Similar process to 2021 harmonization but with updated boundaries
- Uses the latest crosswalk files for 2023-2025 changes
- Maintains consistency with the 2021 harmonized version
- Includes additional validation steps for 2025-specific changes

## Crosswalk Creation

The harmonization process relies on several crosswalk files that track municipality changes over time:

### Base Crosswalks (1990-2021)
- Created in `code/crosswalks/01_ags_crosswalk.R`
- Tracks municipality changes from 1990 to 2021
- Based on readily available official crosswalk files from BBSR
- Includes population and area weights for merged municipalities
- Handles special cases like Berlin and state-specific changes

### 2021-2023 Crosswalks
- Created in `code/crosswalks/04_build_21_23_ags_crosswalks.R`
- Uses official BBSR crosswalk file "ref-gemeinden-ab-2020.xlsx"
- Creates three main crosswalks:
  1. 2021 to 2022 (direct mapping)
  2. 2022 to 2023 (direct mapping)
  3. 2021 to 2023 (chained mapping)
- Includes multiple weighting schemes:
  - Population-based weights
  - Area-based weights
  - Employment-based weights (for 2021-2022)
- Handles data cleaning and standardization:
  - Pads AGS codes to 8 digits
  - Standardizes municipality names
  - Converts population to thousands
- Builds final 1990-2023 crosswalk by:
  1. Loading existing 1990-2021 crosswalk
  2. Chaining with 2021-2023 changes
  3. Multiplying weights for multi-period changes
  4. Preserving municipality names and identifiers

### 2023-2025 Crosswalks
- Created in `code/crosswalks/05_build_23_25_ags_crosswalks.R`
- Processes two types of changes:
  1. 2024 changes (Jan-Dec 2024)
  2. 2025 changes (Jan-Feb 2025)
- Uses official "Gebietsänderungen" files from the federal statistical office
- Classifies changes into three types:
  - Boundary shifts (same AGS, different boundaries)
  - Mergers/splits (different AGS)
  - Dissolutions (rare cases)
- Creates three crosswalks:
  1. 2023 to 2025 (for 2023 municipalities)
  2. 2024 to 2025 (for 2024 municipalities)
  3. Combined 2023-2024 to 2025
- Calculates population and area weights for merged municipalities:
  - Weights are based on official population and area data
  - Handles cases with zero population/area
  - Preserves total population and area in mergers
- Builds final 1990-2025 crosswalk by:
  1. Loading existing 1990-2023 crosswalk
  2. Chaining with 2023-2025 changes
  3. Multiplying weights for multi-period changes
  4. Preserving municipality names and identifiers

## Data Quality Checks

Both harmonization scripts include extensive validation steps:

1. **Merge Validation**
   - Checks for unsuccessful merges
   - Validates population and area weights
   - Ensures no data loss during harmonization

2. **Vote Validation**
   - Verifies total votes against individual party votes
   - Flags cases with incongruent vote totals
   - Handles special cases of mail-in voting districts

3. **Boundary Validation**
   - Ensures all municipalities are properly mapped
   - Validates population and area calculations
   - Checks for consistency across time periods

## Output Files

The harmonization process produces two main datasets:

1. `federal_muni_harm_21.rds`: Data harmonized to 2021 boundaries
2. `federal_muni_harm_25.rds`: Data harmonized to 2025 boundaries

Both files include:
- Election results (votes and shares)
- Turnout rates
- Population and area data
- Quality flags for potential issues
- State and county identifiers