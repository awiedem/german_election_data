# German Election Database

## Overview

The German Election Database provides a comprehensive dataset of local, state, and federal election results in Germany. This repository is intended to facilitate research on electoral behavior, representation, and political responsiveness at multiple levels of government. All datasets include turnout and vote shares for all major parties. Moreover, we provide geographically harmonized datasets that account for changes in municipal boundaries and mail-in voting districts.

The Github repository is organized into three main folders:
1. **Code**: Contains scripts for data processing, harmonization, and analysis.
2. **Data**: Includes raw and processed datasets for municipal, state, and federal elections.
3. **Output**: Contains the results of analyses and visualizations based on the data.

## Dataset Features

### 1. Municipal Elections
- **Coverage**: Election results for all municipalities across Germany from 1990 to 2020.
- **Content**: Turnout and vote shares for major national parties (SPD, CDU/CSU, FDP, Greens, Die Linke) and other parties such as AfD and Freie Wähler.

### 2. State Elections
- **Coverage**: State election results at the municipal level for the period 2006–2019.
- **Content**: Turnout and vote shares for major parties and additional parties such as AfD from 2012 onwards.

### 3. Federal Elections
- **Coverage**: Federal election results at the municipal level since 1980 and county level since 1953.
- **Content**: Turnout and vote shares for all parties that have contested elections, with special handling of mail-in votes.

## Harmonization to 2021 Boundaries
- We also provide all election results datasets in an adjusted format where we harmonize geographic entities (e.g. municipalities or counties) to 2021 boundaries.

## Data Files

The following datasets are included in the repository:

| **Data**                  | **Geographic Level** | **Time Period**  | **Harmonization** | **File Name**                |
|---------------------------|----------------------|-----------------|-------------------|-----------------------------|
| Local Elections           | Municipality         | 1990–2020       | No                | `municipal_unharm`          |
| Local Elections           | Municipality         | 1990–2020       | Yes               | `municipal_harm`            |
| State Elections           | Municipality         | 2006–2019       | No                | `state_unharm`              |
| State Elections           | Municipality         | 2006–2019       | Yes               | `state_harm`                |
| Federal Elections         | Municipality         | 1980–2021       | No                | `federal_muni_raw`          |
| Federal Elections         | Municipality         | 1980–2021       | No                | `federal_muni_unharm`       |
| Federal Elections         | Municipality         | 1990–2021       | Yes               | `federal_muni_harm`         |
| Federal Elections         | County               | 1953–2021       | No                | `federal_cty_unharm`        |
| Federal Elections         | County               | 1990–2021       | Yes               | `federal_cty_harm`          |
| Crosswalks                | Municipality/County  | 1990–2021       | —                 | `ags_crosswalks` / `cty_crosswalks` |
| Shapefiles                | Municipality/County  | 2000, 2021      | —                 | `VG250_GEM` / `VG250_KRS`   | 
| Crosswalk Covariates      | Municipality/County  | 1990–2021       | Yes               | `ags_area_pop_emp` / `cty_area_pop_emp` |


## Usage Notes

Researchers are encouraged to use the harmonized datasets for longitudinal studies and the unharmonized datasets for cross-sectional analyses. When analyzing smaller municipalities or comparing across states, be aware of differences in electoral rules and reporting practices.

## Applications

The dataset supports a wide range of research topics, including:
1. **Nationalization of Politics**: Study how voting behavior aligns across local, state, and federal levels.
2. **Economic Voting**: Analyze how local economic conditions influence voting patterns at different levels of government.

## Code Availability

The code used to generate the datasets and perform the analyses is available in the `Code` folder of this repository. Additional details and instructions are provided in the scripts.

## Citation

Please cite the accompanying paper when using this dataset:

Heddesheimer, Vincent, Hanno Hilbig, Florian Sichart, & Andreas Wiedemann. 2024. "German Election Database".

```         
@article{Heddesheimer2024GermanElection,
  author = {Heddesheimer Vincent, and Hanno Hilbig, and Florian Sichart and Andreas Wiedemann},
  title = {German Election Database},
  year = {2024}
}
```

For more information, visit the [project website](http://german-elections.com) or contact the authors.

---
