# GERDA: German Election Database

## Table of Contents

- [Overview](#overview)
- [Citation](#citation)
- [Datasets](#datasets)
- [Harmonization](#harmonization)
- [Known Data Issues](#known-data-issues)
- [Quick Start](#quick-start)
- [R Package](#r-package)
- [Repository Structure](#repository-structure)
- [Data Sources](#data-sources)
- [Acknowledgements](#acknowledgements)

---

## Overview

The German Election Database (GERDA) contains local, state, and federal election results in Germany at the municipality level. Each dataset includes turnout and vote shares for all major parties. Harmonized versions account for municipal boundary changes and joint mail-in voting districts, so results are comparable across election years.

For variable descriptions, see the [Codebook](docs/codebook.md) (work in progress). For details on data processing, see the [Pipeline Reference](docs/data_pipeline.md).

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

## Datasets

**Municipal elections** cover 1990-2021. **State elections** cover 2006-2024 (15 states; all except Hamburg, which lacks municipality-level data). **Federal elections** are available at the municipality level since 1980 and at the county level since 1953. **Mayoral elections** cover 7 states (Bayern, NRW, Saarland, Sachsen, Rheinland-Pfalz, Niedersachsen, Schleswig-Holstein), 1945-2025, with election-level and candidate-level datasets in both unharmonized and harmonized (2021 boundaries) versions. A **mayor panel** tracks individual mayors across terms using unique person IDs, available as election-level and annual (forward-filled) panels for mayor fixed effects estimation. Raw data also exists for **county elections** (~10 states) and **European elections** (2024), but processing pipelines have not yet been built.

| **Data**                  | **Level**            | **Period**       | **Harmonization** | **File Name**                |
|---------------------------|----------------------|------------------|-------------------|------------------------------|
| Local Elections           | Municipality         | 1990-2021        | No                | `municipal_unharm`           |
| Local Elections           | Municipality         | 1990-2021        | Yes               | `municipal_harm`             |
| State Elections           | Municipality         | 2006-2019        | No                | `state_unharm`               |
| State Elections           | Municipality         | 2006-2019        | Yes (2021)        | `state_harm`                 |
| State Elections           | Municipality         | 2022-2023        | No                | `state_2223_unharm`          |
| State Elections           | Municipality         | 2022-2024        | No                | `state_2224_unharm`          |
| State Elections           | Municipality         | 2006-2023        | Yes (2023)        | `state_harm_23`              |
| State Elections           | Municipality         | 2006-2024        | Yes (2021)        | `state_harm_21`              |
| State Elections           | Municipality         | 2006-2024        | Yes (2025)        | `state_harm_25`              |
| Federal Elections         | Municipality         | 1980-2025        | No                | `federal_muni_raw`           |
| Federal Elections         | Municipality         | 1980-2025        | No                | `federal_muni_unharm`        |
| Federal Elections         | Municipality         | 1990-2025        | Yes (2021)        | `federal_muni_harm_21`       |
| Federal Elections         | Municipality         | 1990-2025        | Yes (2025)        | `federal_muni_harm_25`       |
| Federal Elections         | County               | 1953-2021        | No                | `federal_cty_unharm`         |
| Federal Elections         | County               | 1990-2021        | Yes               | `federal_cty_harm`           |
| Mayoral Elections         | Municipality         | 1945-2025        | No                | `mayoral_unharm`             |
| Mayoral Elections         | Municipality         | 1945-2025        | Yes (2021)        | `mayoral_harm`               |
| Mayoral Elections         | Municipality (cand.) | 1945-2025        | No                | `mayoral_candidates`         |
| Mayor Panel               | Person-election      | 1945-2025        | No                | `mayor_panel`                |
| Mayor Panel               | Person-election      | 1945-2025        | Yes (2021)        | `mayor_panel_harm`           |
| Mayor Panel               | Person-year          | 1945-2025        | No                | `mayor_panel_annual`         |
| Mayor Panel               | Person-year          | 1945-2025        | Yes (2021)        | `mayor_panel_annual_harm`    |
| Crosswalks                | Municipality         | 1990-2021        | --                | `ags_crosswalks`             |
| Crosswalks                | Municipality         | 1990-2023        | --                | `ags_1990_to_2023_crosswalk` |
| Crosswalks                | Municipality         | 1990-2025        | --                | `ags_1990_to_2025_crosswalk` |
| Crosswalks                | County               | 1990-2021        | --                | `cty_crosswalks`             |
| Shapefiles                | Municipality/County  | 2000, 2021       | --                | `VG250_GEM` / `VG250_KRS`    |
| Covariates                | Municipality/County  | 1990-2025        | Yes               | `ags_area_pop_emp` / `cty_area_pop_emp` |

## Harmonization

We provide datasets harmonized to 2021 and 2025 municipal and county boundaries. Official crosswalks track mergers, splits, and boundary shifts. When multiple municipalities merged, we aggregate votes to the new boundaries using population-based weights. For mail-in voting districts shared by multiple municipalities, mail-in votes are allocated proportionally based on polling-card voters.

The newer harmonized state election files (`state_harm_21`, `state_harm_23`, `state_harm_25`) use a weighted sum of vote counts, which correctly handles mergers between municipalities of different sizes. The older `state_harm` file (2006-2019) used a weighted mean of vote shares and is retained for backward compatibility; for new work, we recommend the `_21` or `_25` versions.

The mayoral election harmonization (`mayoral_harm`) uses the same crosswalk infrastructure but differs in key ways: it groups by `(ags_21, election_date)` rather than election year (since mayoral elections are not synchronized across municipalities), takes the winner from the largest predecessor by population for merged municipalities, and flags pre-1990 Bayern data that relies on the 1990 crosswalk as a fallback. VG, SG, and Landrat election types are excluded since their pseudo-AGS codes cannot be crosswalked.

For details on the federal election harmonization process, including crosswalk creation and validation steps, see the [Federal Election Harmonization README](code/federal_elections/municipality_level/README.md). For the mayoral harmonization approach, see the [Mayoral Elections README](data/mayoral_elections/final/README.md).

## Known Data Issues

- **Incongruent municipality keys**: Some official datasets used municipality identifiers absent from crosswalk files. We manually corrected these by matching election results to crosswalk entries and verifying against state archives.
- **Mail-in votes**: Joint mail-in voting districts complicate disaggregation. We distribute mail-in votes according to each municipality's share of polling-card voters. This is an approximation but avoids discarding mail-in votes altogether.
- **Varying reporting standards**: States sometimes lump small local parties or independent candidates into an "Other" category. We provide disaggregated results where possible but otherwise treat them as a single category.
- **Rounding errors**: Boundary harmonization and proportional allocation can cause minor discrepancies in total votes compared to official tallies. Differences are typically a handful of votes, and we flag these cases in the data.

## Quick Start

### Using the `gerda` R package

```R
# install.packages("gerda")
library(gerda)

# List available datasets
gerda_data_list()

# Load harmonized federal election data (municipality level)
data <- load_gerda_web("federal_muni_harm_25", verbose = TRUE, file_format = "rds")
glimpse(data)
```

### Direct download

```R
url <- "https://github.com/awiedem/german_election_data/raw/main/data/federal_elections/municipality_level/final/federal_muni_harm_21.rds"
download.file(url, "federal_muni_harm_21.rds", mode = "wb")
data <- readRDS("federal_muni_harm_21.rds")
```

### Usage notes

- **Harmonized datasets** are recommended for time-series analyses or comparisons across election cycles under stable geographic units.
- **Unharmonized datasets** are useful for single-election or cross-sectional analyses where original boundaries matter.
- **Small municipalities**: "Other" party votes can be large where major parties do not field candidates. Check the documentation on local reporting rules.

## R Package

The [`gerda`](https://cran.r-project.org/package=gerda) R package provides direct access to all GERDA datasets and several convenience functions for working with German election data.

```R
install.packages("gerda")
library(gerda)
```

Key functions:

- `load_gerda_web()` — download any GERDA dataset by name
- `gerda_data_list()` — list all available datasets
- `add_gerda_covariates()` — append 30 INKAR county-level socioeconomic variables
- `add_gerda_census()` — append 14 municipality-level indicators from Census 2022
- `party_crosswalk()` — map German party names to ParlGov IDs

Package source: [GitHub](https://github.com/hhilbig/gerda) | [CRAN](https://cran.r-project.org/package=gerda)

## Repository Structure

1. **Code**: Scripts for data ingestion, cleaning, harmonization, and analysis. See `docs/data_pipeline.md` for a walkthrough.
2. **Data**: Raw and processed datasets for municipal, state, and federal elections, plus boundary shapefiles and crosswalks. Ready-to-use files are in `final/` subdirectories (e.g., `data/federal_elections/municipality_level/final/`).
3. **Output**: Analysis results and visualizations.

## Data Sources

### Federal Elections

Bundeswahlleiterin. [https://www.bundeswahlleiterin.de/bundeswahlleiter.html](https://www.bundeswahlleiterin.de/bundeswahlleiter.html).

### State Elections

Statistische Ämter des Bundes und der Länder. Landtagswahlen. [https://www.regionalstatistik.de/genesis/online/](https://www.regionalstatistik.de/genesis/online/) (Regional data bank of the German Federal Statistical Office). Retrieved and imported using the wiesbaden R package and the SOAP XML web service of DESTATIS.

### Municipal Elections

| **State**               | **Source**                                                                                                    | **Procured via**                                          |
|-------------------------|---------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------|
| Baden-Wuerttemberg      | Statistisches Landesamt Baden-Württemberg                                                                     | email                                                     |
| Bayern                  | Bayerisches Landesamt für Statistik                                                                           | website                                                   |
| Brandenburg             | Amt für Statistik Berlin-Brandenburg                                                                          | website                                                   |
| Bremen                  | Statistisches Landesamt Bremen                                                                                | website                                                   |
| Hamburg                 | Statistik Nord                                                                                                 | website                                                   |
| Hessen                  | Hessisches Statistisches Landesamt                                                                            | website                                                   |
| Mecklenburg Vorpommern  | Mecklenburg-Vorpommern Landesamt für innere Verwaltung & Statistisches Amt                                    | website                                                   |
| Niedersachsen           | Landesamt für Statistik Niedersachsen                                                                         | website (post-2006), email (pre-2006)                     |
| NRW                     | Statistisches Landesamt Nordrhein-Westfalen                                                                   | email                                                     |
| RLP                     | Statistisches Landesamt Rheinland-Pfalz                                                                       | email                                                     |
| Saarland                | Statistisches Landesamt des Saarlandes                                                                        | email                                                     |
| Sachsen                 | Statistisches Landesamt des Freistaates Sachsen                                                              | website                                                   |
| Sachsen-Anhalt          | Statistisches Landesamt Sachsen-Anhalt                                                                       | website                                                   |
| Schleswig-Holstein      | Statistisches Amt für Hamburg und Schleswig-Holstein                                                          | website (except 2013), email for 2013                     |
| Thueringen              | Thüringer Landesamt für Statistik                                                                             | website                                                   |

### Mayoral Elections

| **State**               | **Source**                                                        | **Procured via**                         |
|-------------------------|------------------------------------------------------------------|------------------------------------------|
| Bayern                  | Bayerisches Landesamt für Statistik                              | website (Excel)                          |
| Niedersachsen           | Landesamt für Statistik Niedersachsen                            | website (PDF)                            |
| Nordrhein-Westfalen     | IT.NRW                                                           | website (Excel)                          |
| Rheinland-Pfalz         | Statistisches Landesamt Rheinland-Pfalz                          | website (Excel, percentages only)        |
| Saarland                | Statistisches Landesamt des Saarlandes                           | website (Excel)                          |
| Sachsen                 | Statistisches Landesamt des Freistaates Sachsen (Bürgermeisteratlas) | website (Excel)                      |
| Schleswig-Holstein      | Statistisches Amt für Hamburg und Schleswig-Holstein              | web scraping (wahlen-sh.de)              |

For details on data structure and known issues, see the [Mayoral Elections README](data/mayoral_elections/final/README.md) and [Known Issues](docs/mayoral_elections_known_issues.md).

### Crosswalks

Bundesinstitut für Bau-, Stadt- und Raumforschung. Umsteigeschlüssel für konsistente zeitreihen. [https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html](https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html) (2024).

### Shapefiles

Federal Agency for Cartography and Geodesy (BKG). Vg250: Administrative boundaries of germany. [http://www.bkg.bund.de](http://www.bkg.bund.de) (2021). Open Data Lizenz Deutschland – Namensnennung – Version 2.0. Source reference: © GeoBasis-DE / BKG (year of last data download).

---

## Acknowledgements

We thank Cornelius Erfort, Sascha Riaz and Moritz Marbach for helpful comments. We also thank the anonymous reviewers at *Scientific Data* for their constructive feedback. Thanks to Daniela Gaus for excellent research assistance and Victor Kreitman for providing code and data on election dates.
