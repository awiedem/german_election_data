# GERDA: German Election Database

## Overview

The German Election Database (GERDA) provides a comprehensive dataset of local, state, and federal election results in Germany, enabling research on electoral behavior, representation, and political responsiveness across multiple levels of government. Each dataset includes turnout and vote shares for all major parties. We provide harmonized datasets that account for municipal boundary changes and joint mail-in voting districts, ensuring comparability over time.

We aim to continuously update this repository as new elections become available. The repository is structured into three main folders:

1. **Code**: Scripts for data processing, harmonization, and analyses.
2. **Data**: Raw and processed datasets for municipal, state, and federal elections, plus boundary shapefiles and crosswalks.
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
| Local Elections           | Municipality         | 1990–2020        | No                | `municipal_unharm`           |
| Local Elections           | Municipality         | 1990–2020        | Yes               | `municipal_harm`             |
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

To facilitate consistent comparisons across time and regions, we provide files harmonized to the 2021 municipal and county boundaries. We use official crosswalks to track mergers, splits, and boundary shifts. In cases where multiple municipalities merged, we apply population-based weighting to aggregate votes to the new municipality’s boundaries. For mail-in voting districts shared by multiple municipalities, we allocate mail-in votes proportionally based on the number of polling-card voters in each municipality.

## Municipal Elections Data Sources

| **Bundesland**            | **Source**                                                                                                                                           | **Notes**                                                                                                                                                 |
|---------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| Baden-Wuerttemberg (BW)   | email                                                                                                                                                 |                                                                                                                                                           |
| Bayern                    | [https://www.statistikdaten.bayern.de/genesis/online?operation=themes&levelindex=0&levelid=1638980673533&code=14#abreadcrumb](https://www.statistikdaten.bayern.de/genesis/online?operation=themes&levelindex=0&levelid=1638980673533&code=14#abreadcrumb) |                                                                                                                                                           |
| Brandenburg               | [https://www.statistik-berlin-brandenburg.de/kommunalwahlen/](https://www.statistik-berlin-brandenburg.de/kommunalwahlen/)                                                                  | Before 2003 (1998, 1993): only scanned PDFs                                                                                                              |
| Bremen                    | [https://www.statistik.bremen.de/datenangebote-8409](https://www.statistik.bremen.de/datenangebote-8409)                                                                                    |                                                                                                                                                           |
| Hamburg                   | [https://www.statistik-nord.de/wahlen/wahlen-in-hamburg/buergerschaftswahlen/2020#c8007](https://www.statistik-nord.de/wahlen/wahlen-in-hamburg/buergerschaftswahlen/2020#c8007)          |                                                                                                                                                           |
| Hessen                    | [https://statistik.hessen.de/zahlen-fakten/kommunalwahlen](https://statistik.hessen.de/zahlen-fakten/kommunalwahlen)                                                                        |                                                                                                                                                           |
| Mecklenburg-Vorpommern    | [https://www.laiv-mv.de/Statistik/Zahlen-und-Fakten/Gesellschaft-&-Staat/Wahlen-&-Volksabstimmungen](https://www.laiv-mv.de/Statistik/Zahlen-und-Fakten/Gesellschaft-&-Staat/Wahlen-&-Volksabstimmungen) |                                                                                                                                                           |
| Niedersachsen             | [https://www.statistik.niedersachsen.de/startseite/themen/wahlen/wahlen-in-niedersachsen-statistische-berichte-b-vii-3-179044.html](https://www.statistik.niedersachsen.de/startseite/themen/wahlen/wahlen-in-niedersachsen-statistische-berichte-b-vii-3-179044.html) | Email contact needed for data before 2006                                                                                                                |
| Nordrhein-Westfalen (NRW) | email                                                                                                                                                 |                                                                                                                                                           |
| Rheinland-Pfalz (RLP)     | email                                                                                                                                                 |                                                                                                                                                           |
| Saarland                  | email                                                                                                                                                 |                                                                                                                                                           |
| Sachsen                   | [https://www.statistik.sachsen.de/genonline/online?operation=find&suchanweisung_language=de&query=14431#abreadcrumb](https://www.statistik.sachsen.de/genonline/online?operation=find&suchanweisung_language=de&query=14431#abreadcrumb) |                                                                                                                                                           |
| Sachsen-Anhalt            | [https://wahlergebnisse.sachsen-anhalt.de/wahlen/kw19/and/kw.download.html](https://wahlergebnisse.sachsen-anhalt.de/wahlen/kw19/and/kw.download.html)                                          |                                                                                                                                                           |
| Schleswig-Holstein        | [https://www.statistischebibliothek.de/mir/receive/SHSerie_mods_00000466](https://www.statistischebibliothek.de/mir/receive/SHSerie_mods_00000466)                                              | Available and processed for 2008, 2018; machine-readable PDF for 2003; scanned PDFs before that; 2013 via email from Statistikamt Nord                   |
| Thueringen                | [https://www.wahlen.thueringen.de/kommunalwahlen/kw_wahlergebnisse_GW.asp](https://www.wahlen.thueringen.de/kommunalwahlen/kw_wahlergebnisse_GW.asp)                                          |                                                                                                                                                           |


## Known Data Issues and Resolutions

- **Incongruent Municipality Keys**: Some official datasets used municipality identifiers that did not appear in crosswalk files. We manually corrected these keys by matching election results to the relevant crosswalk entries and verifying them against state archives.
- **Mail-in Votes**: Joint mail-in voting districts complicate disaggregation. We address this by distributing mail-in votes according to each municipality’s share of polling-card voters. While this is an approximation, it avoids discarding mail-in votes altogether.
- **Varying Reporting Standards**: States sometimes lump small local parties or independent candidates into an “Other” category. In such cases, we provide disaggregated results where possible but otherwise treat them as a single category. Researchers should be mindful of this when comparing across states.
- **Rounding Errors**: Boundary harmonization and proportional allocation can cause minor discrepancies in total votes when comparing to official tallies. Any differences typically amount to fewer than a handful of votes, and we flag these cases in the data.

## Usage Notes

- **Harmonized Datasets**: Recommended for time-series analyses or when comparing multiple election cycles under stable geographic units.  
- **Unharmonized Datasets**: Useful for single-election or cross-sectional analyses, especially where original boundaries are essential.  
- **Small Municipalities**: Be aware that “Other” party votes might be large in places where major parties do not field candidates. Check the documentation on local reporting rules.

## Code Availability

All code is in the `Code` folder, including scripts for data ingestion, cleaning, harmonization, and visualizations. Researchers can replicate or adapt these scripts for custom analyses.

## Citation

Please cite the accompanying [paper](https://osf.io/preprints/socarxiv/q28ex) when using this dataset:

Heddesheimer, Vincent, Hanno Hilbig, Florian Sichart, & Andreas Wiedemann. 2025. *GERDA: German Election Database*. Nature: Scientific Data (forthcoming).

```bibtex
@article{Heddesheimer2025GermanElection,
  author = {Heddesheimer Vincent, and Hanno Hilbig, and Florian Sichart and Andreas Wiedemann},
  title = {German Election Database},
  year = {2025},
  journal = {Nature: Scientific Data}
  url = {https://osf.io/preprints/socarxiv/q28ex},
  doi = {https://doi.org/10.31235/osf.io/q28ex}
}
