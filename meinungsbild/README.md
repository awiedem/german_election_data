# Meinungsbild

Subnational public opinion estimates for Germany using multilevel regression and poststratification (MRP).

## Overview

Meinungsbild estimates public opinion on 43 policy issues at three geographic levels:

- **Bundeslaender** (16 federal states)
- **Wahlkreise** (299 electoral districts)
- **Kreise** (400 counties)

Estimates are derived from ~118,000 survey respondents across five survey programs, combined with Zensus 2022 population data for poststratification.

## Data sources

| Source | N | Years | Citation |
|--------|---|-------|----------|
| GLES Tracking (ZA6832) | 52,336 | 2009--2023 | GESIS |
| GLES Cross-Section 2025 (ZA10100) | 7,337 | 2025 | GESIS |
| GLES RCS 2025 (ZA10101) | 8,561 | 2025 | GESIS |
| GLES Cumulation (ZA6835) | 21,040 | 2009--2021 | GESIS |
| ALLBUS (ZA8974) | 29,112 | 2023--2024 | GESIS |

**Note:** Raw survey data are not included in this repository due to licensing restrictions. Users must obtain data directly from [GESIS](https://www.gesis.org/).

## Repository structure

```
meinungsbild/
+-- code/                        # R pipeline scripts
|   +-- 01_harmonize_all.R       # Pool and harmonize survey data
|   +-- 02_build_poststrat_frame.R
|   +-- 02b_download_zensus.R    # Download Zensus 2022 from API
|   +-- 03_load_covariates.R     # Load election + INKAR covariates
|   +-- 03b_build_adjacency.R    # County adjacency matrix
|   +-- 04b_fit_all_lme4.R      # Fit MRP models (lme4)
|   +-- 04_fit_mrp.R            # Experimental: brms/Stan models
|   +-- 07_export_estimates.R    # Export to JSON/CSV
|   +-- 08_check_pipeline.R     # Validation checks
+-- data/
|   +-- issue_concordance.csv    # Issue definitions and binary coding rules
|   +-- variable_inventory.csv   # Variable mapping across surveys
+-- docs/
|   +-- methodology_notes.md     # Model specification and design choices
+-- harmonization/               # Variable-specific harmonization notes
+-- output/
|   +-- checks/                  # Validation results
|   +-- tables/                  # CSV estimates for download
+-- web/                         # Next.js interactive map
    +-- public/data/             # GeoJSON boundaries + JSON estimates
    +-- src/                     # React components
```

## Pipeline

Run scripts in order (`01_` through `08_`). The pipeline requires raw survey data in `data/raw/` (not included).

```bash
# From the german_election_data root:
Rscript meinungsbild/code/01_harmonize_all.R
Rscript meinungsbild/code/02_build_poststrat_frame.R
Rscript meinungsbild/code/03_load_covariates.R
Rscript meinungsbild/code/04b_fit_all_lme4.R
Rscript meinungsbild/code/07_export_estimates.R
Rscript meinungsbild/code/08_check_pipeline.R
```

## Website

The interactive map is a Next.js application in `web/`.

```bash
cd meinungsbild/web
npm install
npm run dev
```

Open [http://localhost:3000](http://localhost:3000).

## Methodology

MRP with `lme4::glmer()`. See [`docs/methodology_notes.md`](docs/methodology_notes.md) for the full model specification.

**Validation:** Median correlation r = 0.899 and median RMSE = 5.5pp against direct Bundesland survey estimates across 43 issues.

## Output formats

- **JSON** (`web/public/data/`): Used by the interactive map
- **CSV** (`output/tables/`): Downloadable estimates at all three geographic levels

## License

Code: MIT. Survey data must be obtained separately under GESIS terms. Wahlkreis boundaries: Bundeswahlleiterin/BKG.
