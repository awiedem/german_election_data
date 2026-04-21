# GERDA: German Election Database

## Table of Contents

- [Overview](#overview)
- [Citation](#citation)
- [Datasets](#datasets)
- [Harmonization](#harmonization)
- [Known Data Issues](#known-data-issues)
- [Quick Start](#quick-start)
- [Repository Structure](#repository-structure)
- [Data Sources](#data-sources)
- [Acknowledgements](#acknowledgements)

---

## Overview

The German Election Database (GERDA) contains local, state, and federal election results in Germany at the municipality level. Each dataset includes turnout and vote shares for all major parties. Harmonized versions account for municipal boundary changes and joint mail-in voting districts, so results are comparable across election years.

For variable descriptions, see the [Codebook](docs/codebook.md). For details on data processing, see the [Pipeline Reference](docs/data_pipeline.md).

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

GERDA includes federal, state, municipal, European, mayoral, and county (Kreistag) election results at the municipality level, plus a county-level federal series going back to 1953. Each dataset is available in an unharmonized version (original boundaries) and, where applicable, harmonized to 2021 and/or 2025 municipal boundaries. The mayoral pipeline also provides candidate-level records and a person-level mayor panel. Exact coverage by dataset:

| **Data**                  | **Level**            | **Period**       | **Harmonization** | **File Name**                |
|---------------------------|----------------------|------------------|-------------------|------------------------------|
| Local Elections           | Municipality         | 1990-2021        | No                | `municipal_unharm`           |
| Local Elections           | Municipality         | 1990-2021        | Yes (2021)        | `municipal_harm`             |
| Local Elections           | Municipality         | 1990-2021        | Yes (2025)        | `municipal_harm_25`          |
| State Elections           | Municipality         | 1946-2024        | No                | `state_unharm`               |
| State Elections           | Municipality         | 2006-2019        | Yes (2021, legacy)| `state_harm`                 |
| State Elections           | Municipality         | 2006-2024        | Yes (2021)        | `state_harm_21`              |
| State Elections           | Municipality         | 2006-2023        | Yes (2023)        | `state_harm_23`              |
| State Elections           | Municipality         | 2006-2024        | Yes (2025)        | `state_harm_25`              |
| Federal Elections         | Municipality         | 1980-2025        | No                | `federal_muni_raw`           |
| Federal Elections         | Municipality         | 1980-2025        | No                | `federal_muni_unharm`        |
| Federal Elections         | Municipality         | 1990-2025        | Yes (2021)        | `federal_muni_harm_21`       |
| Federal Elections         | Municipality         | 1990-2025        | Yes (2025)        | `federal_muni_harm_25`       |
| Federal Elections         | County               | 1953-2025        | No                | `federal_cty_unharm`         |
| Federal Elections         | County               | 1990-2025        | Yes (2021)        | `federal_cty_harm`           |
| European Elections        | Municipality         | 2009-2024        | No                | `european_muni_unharm`       |
| European Elections        | Municipality         | 2009-2024        | Yes (2021)        | `european_muni_harm`         |
| County Elections          | Municipality         | 1948-2024        | No                | `county_elec_unharm`         |
| County Elections          | Municipality         | 1990-2024        | Yes (2021)        | `county_elec_harm_21`        |
| County Elections          | County (5-digit)     | 1990-2024        | Yes (2021)        | `county_elec_harm_21_cty`    |
| County Elections          | Municipality         | 1991-2024        | Yes (2021)        | `county_elec_harm_21_muni`   |
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
| Crosswalks                | Municipality         | 2021 → 2023      | --                | `crosswalk_ags_2021_to_2023` |
| Crosswalks                | Municipality         | 2021/2022 → 2023 | --                | `crosswalk_ags_2021_2022_to_2023` |
| Crosswalks                | Municipality         | 2023 → 2025      | --                | `crosswalk_ags_2023_to_2025` |
| Crosswalks                | Municipality         | 2023/2024 → 2025 | --                | `crosswalk_ags_2023_24_to_2025` |
| Crosswalks                | Municipality         | 2024 → 2025      | --                | `crosswalk_ags_2024_to_2025` |
| Crosswalks                | County               | 1990-2021        | --                | `cty_crosswalks`             |
| Shapefiles                | Municipality/County  | 2000, 2021       | --                | `VG250_GEM` / `VG250_KRS`    |
| Covariates                | Municipality/County  | 1990-2025        | Yes (2021)        | `ags_area_pop_emp` / `cty_area_pop_emp` |
| Covariates                | Municipality         | 1990-2023        | Yes (2023)        | `ags_area_pop_emp_2023`      |

## Harmonization

We provide datasets harmonized to fixed 2021 and 2025 municipal and county boundaries. Official BBSR crosswalks track mergers, splits, and boundary shifts; votes from merged municipalities are aggregated using population weights, and votes from shared mail-in districts are allocated proportionally by polling-card voters.

For state elections, the `state_harm_21`, `state_harm_23`, and `state_harm_25` files use a weighted sum of vote counts, which handles mergers of unequal-sized municipalities correctly. The legacy `state_harm` file (2006-2019, weighted mean of vote shares) is retained only for backward compatibility; for new work use the `_21` or `_25` version.

Mayoral harmonization uses the same crosswalks but groups by `(ags_21, election_date)` rather than election year, since mayoral elections are not synchronized across municipalities. See the [Mayoral Elections README](data/mayoral_elections/final/README.md) for the full set of mayoral-specific adjustments, and the [Federal Election Harmonization README](code/federal_elections/municipality_level/README.md) for federal-election details.

## Known Data Issues

- **Incongruent municipality keys**: Some official datasets used municipality identifiers absent from crosswalk files. We manually corrected these by matching election results to crosswalk entries and verifying against state archives.
- **Mail-in votes**: Joint mail-in voting districts complicate disaggregation. We distribute mail-in votes according to each municipality's share of polling-card voters. This is an approximation but avoids discarding mail-in votes altogether.
- **Varying reporting standards**: States sometimes lump small local parties or independent candidates into a single "Other" category. We disaggregate where the source permits.
- **Rounding errors**: Boundary harmonization and proportional allocation can cause minor discrepancies in total votes compared to official tallies. Differences are typically a handful of votes, and we flag these cases in the data.

## Quick Start

The recommended entry point is the [`gerda`](https://cran.r-project.org/package=gerda) R package, which downloads any GERDA dataset on demand.

```R
install.packages("gerda")
library(gerda)

# List available datasets
gerda_data_list()

# Load harmonized federal election data (municipality level)
data <- load_gerda_web("federal_muni_harm_25", verbose = TRUE, file_format = "rds")
```

Key functions: `load_gerda_web()` (download by name), `gerda_data_list()` (catalog), `add_gerda_covariates()` (30 INKAR county-level socioeconomic variables), `add_gerda_census()` (16 municipality-level indicators from Census 2022), `party_crosswalk()` (map German party names to ParlGov IDs). Package source: [GitHub](https://github.com/hhilbig/gerda) | [CRAN](https://cran.r-project.org/package=gerda).

### Direct download (without the R package)

```R
url <- "https://github.com/awiedem/german_election_data/raw/main/data/federal_elections/municipality_level/final/federal_muni_harm_21.rds"
download.file(url, "federal_muni_harm_21.rds", mode = "wb")
data <- readRDS("federal_muni_harm_21.rds")
```

### Usage notes

- Use harmonized datasets for time-series analyses under stable geographic units; use unharmonized datasets where original boundaries matter.
- Vote shares for "Other" can be large in small municipalities where major parties do not field candidates. See the [Codebook](docs/codebook.md) for per-state reporting rules.

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
