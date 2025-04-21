# GERDA: German Election Database

## :clipboard: Table of Contents

- [Overview](#overview)
- [Citation](#citation)
- [Repository Structure](#repository-structure)
- [Dataset Features](#dataset-features)
- [Data Files](#data-files)
- [Harmonization Details](#harmonization-details)
- [Known Data Issues and Resolutions](#known-data-issues-and-resolutions)
- [:information_source: Usage Notes](#️-usage-notes)
- [:computer: Example Usage](#-example-usage)
- [:file_folder: Code Availability](#-code-availability)
- [:scroll: Detailed Data Sources](#️-detailed-data-sources)

---

## Overview

The German Election Database (GERDA) provides a comprehensive dataset of local, state, and federal election results in Germany, enabling research on electoral behavior, representation, and political responsiveness across multiple levels of government. Each dataset includes turnout and vote shares for all major parties. We provide harmonized datasets that account for municipal boundary changes and joint mail-in voting districts, ensuring comparability over time.

For detailed descriptions of all variables in the main datasets, please consult the [Codebook](docs/codebook.md) (current work in progress.)

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

We aim to continuously update this repository as new elections become available.

## :file_folder: Repository Structure

The repository is structured into four main folders:

1. **Code**: Scripts for data processing, harmonization, and analyses.
2. **Data**: Raw and processed datasets for municipal, state, and federal elections, plus boundary shapefiles and crosswalks. The main ready-to-use datasets are typically found within `final` subdirectories (e.g., `data/federal_elections/municipality_level/final/`).
3. **Output**: Results of analyses and visualizations based on these datasets.

## Dataset Features

### Municipal Elections

- **Coverage**: 1990–2020  
- **Content**: Turnout and vote shares for major parties (SPD, CDU/CSU, FDP, Greens, Die Linke, AfD), plus smaller parties where available

### State Elections

- **Coverage**: 2006–2019 at the municipal level  
- **Content**: Turnout and party vote shares, including AfD from 2012 onward

### Federal Elections

- **Coverage**: Municipality-level data since 1980 and county-level data since 1953  
- **Content**: Turnout and vote shares for all parties, with special handling for mail-in votes

## Data Files

| **Data**                  | **Geographic Level** | **Time Period**  | **Harmonization** | **File Name**                |
|---------------------------|----------------------|------------------|-------------------|------------------------------|
| Local Elections           | Municipality         | 1990–2021        | No                | `municipal_unharm`           |
| Local Elections           | Municipality         | 1990–2021        | Yes               | `municipal_harm`             |
| State Elections           | Municipality         | 2006–2019        | No                | `state_unharm`               |
| State Elections           | Municipality         | 2006–2019        | Yes               | `state_harm`                 |
| Federal Elections         | Municipality         | 1980–2021        | No                | `federal_muni_raw`           |
| Federal Elections         | Municipality         | 1980–2021        | No                | `federal_muni_unharm`        |
| Federal Elections         | Municipality         | 1990–2021        | Yes               | `federal_muni_harm`          |
| Federal Elections         | County               | 1953–2021        | No                | `federal_cty_unharm`         |
| Federal Elections         | County               | 1990–2021        | Yes               | `federal_cty_harm`           |
| Crosswalks                | Municipality/County  | 1990–2021        | —                 | `ags_crosswalks` / `cty_crosswalks` |
| Shapefiles                | Municipality/County  | 2000, 2021       | —                 | `VG250_GEM` / `VG250_KRS`    |
| Crosswalk Covariates      | Municipality/County  | 1990–2021        | Yes               | `ags_area_pop_emp` / `cty_area_pop_emp` |

## Harmonization Details

To facilitate consistent comparisons across time and regions, we provide files harmonized to the 2021 municipal and county boundaries. We use official crosswalks to track mergers, splits, and boundary shifts. In cases where multiple municipalities merged, we apply population-based weighting to aggregate votes to the new municipality's boundaries. For mail-in voting districts shared by multiple municipalities, we allocate mail-in votes proportionally based on the number of polling-card voters in each municipality.

## Known Data Issues and Resolutions

- **Incongruent Municipality Keys**: Some official datasets used municipality identifiers that did not appear in crosswalk files. We manually corrected these keys by matching election results to the relevant crosswalk entries and verifying them against state archives.
- **Mail-in Votes**: Joint mail-in voting districts complicate disaggregation. We address this by distributing mail-in votes according to each municipality's share of polling-card voters. While this is an approximation, it avoids discarding mail-in votes altogether.
- **Varying Reporting Standards**: States sometimes lump small local parties or independent candidates into an "Other" category. In such cases, we provide disaggregated results where possible but otherwise treat them as a single category. Researchers should be mindful of this when comparing across states.
- **Rounding Errors**: Boundary harmonization and proportional allocation can cause minor discrepancies in total votes when comparing to official tallies. Any differences typically amount to fewer than a handful of votes, and we flag these cases in the data.

## :information_source: Usage Notes

- **Harmonized Datasets**: Recommended for time-series analyses or when comparing multiple election cycles under stable geographic units.  
- **Unharmonized Datasets**: Useful for single-election or cross-sectional analyses, especially where original boundaries are essential.  
- **Small Municipalities**: Be aware that "Other" party votes might be large in places where major parties do not field candidates. Check the documentation on local reporting rules.
- **Accessing Data**: See the examples below for how to load data into your own R project using the dedicated `gerda` R package or via direct file download.

## :computer: Example Usage

Below are examples showing how to load GERDA data into your R session.

### Option 1: Using the `gerda` R Package (Recommended)

The easiest way to access the data is via the companion R package.

```R
# Install the package from GitHub if you haven't already
# if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# devtools::install_github("hhilbig/gerda") 

library(gerda)
library(dplyr)

# List available datasets
# gerda_data_list()

# Load the harmonized federal election data (municipality level)
data_fed_harm <- load_gerda_web("federal_muni_harm", verbose = TRUE, file_format = "rds")

# View the structure
glimpse(data_fed_harm)

# Example: Summarize turnout for 2021 using dplyr
turnout_summary_2021 <- data_fed_harm %>%
  filter(election_year == 2021) %>%
  summarise(mean_turnout = mean(turnout, na.rm = TRUE),
            median_turnout = median(turnout, na.rm = TRUE),
            min_turnout = min(turnout, na.rm = TRUE),
            max_turnout = max(turnout, na.rm = TRUE))

print(turnout_summary_2021)

```

### Option 2: Direct File Download

You can also download specific data files directly from the repository URL.

```R
library(tidyverse) # For glimpse

# URL for the raw RDS file on GitHub (using the awiedem repository)
file_url <- "https://github.com/awiedem/german_election_data/raw/main/data/federal_elections/municipality_level/final/federal_muni_harm.rds"

# Destination path for the downloaded file
dest_path <- "federal_muni_harm.rds" # Or specify a full path

# Download the file (consider adding error handling for robust scripts)
download.file(file_url, destfile = dest_path, mode = "wb")

# Load the downloaded RDS file
federal_data <- readRDS(dest_path)

# View the structure using glimpse
glimpse(federal_data)

# Clean up the downloaded file (optional)
# file.remove(dest_path)

```

## :file_folder: Code Availability

All code is in the `Code` folder, including scripts for data ingestion, cleaning, harmonization, and visualizations. Researchers can replicate or adapt these scripts for custom analyses.

## :scroll: Detailed Data Sources

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
| BW                      | Statistisches Landesamt Baden-Württemberg                                                                     | email                                                     |
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

### Crosswalks

Bundesinstitut für Bau-, Stadt- und Raumforschung. Umsteigeschlüssel für konsistente zeitreihen. [https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html](https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html) (2024).

### Shapefiles

Federal Agency for Cartography and Geodesy (BKG). Vg250: Administrative boundaries of germany. [http://www.bkg.bund.de](http://www.bkg.bund.de) (2021). Open Data Lizenz Deutschland – Namensnennung – Version 2.0. Source reference: © GeoBasis-DE / BKG (year of last data download).
