# GERDA: The German Election Database

GERDA contains German election results at the municipality level: federal, state, municipal council, county council (Kreistag), European Parliament, mayoral, and Landrat elections. Each dataset reports turnout and vote shares for all major parties. Harmonized versions map results onto fixed 2021 or 2025 municipal boundaries, so that the same units can be compared across elections.

The easiest way to get the data is the R or Python package, or the download page at [german-elections.com](https://www.german-elections.com/election-data/). Variables are described in the [codebook](docs/codebook.md).

## Citation

Heddesheimer, Vincent, Hanno Hilbig, Florian Sichart, and Andreas Wiedemann. 2025. "GERDA: The German Election Database." *Scientific Data* 12: 618. https://doi.org/10.1038/s41597-025-04811-5

```
@article{Heddesheimer2025GERDA,
   author = {Vincent Heddesheimer and Hanno Hilbig and Florian Sichart and Andreas Wiedemann},
   title = {GERDA: The German Election Database},
   journal = {Scientific Data},
   volume = {12},
   pages = {618},
   year = {2025},
   doi = {10.1038/s41597-025-04811-5}
}
```

## Getting the data

R ([CRAN](https://cran.r-project.org/package=gerda), [source](https://github.com/hhilbig/gerda)):

```r
install.packages("gerda")
library(gerda)
gerda_data_list()                          # all datasets with years and boundaries
df <- load_gerda_web("federal_muni_harm_25")
```

Python ([PyPI](https://pypi.org/project/gerda/), [source](https://github.com/hhilbig/gerda-py)):

```python
# pip install gerda
import gerda
gerda.datasets()                           # all datasets
df = gerda.load("federal_muni_harm_25")
```

A read-only [MCP server](mcp_server/README.md) for AI agents serves the 2025-harmonized federal municipality data at `https://mcp.german-elections.com/mcp`.

Files can also be downloaded directly from the `final/` folders in `data/`, for example `data/federal_elections/municipality_level/final/federal_muni_harm_21.rds`. The repository is large (several GB), so we do not recommend cloning it to get the data.

## Coverage

| Election | Level | Unharmonized | Harmonized |
|---|---|---|---|
| Federal | Municipality | 1980–2025 | 1990–2025 (2021 and 2025 boundaries) |
| Federal | County | 1953–2025 | 1990–2025 (2021) |
| Federal | Constituency (Wahlkreis) | 2002–2025 | 2021 result on 2025 constituencies |
| State | Municipality | 1946–2026 | 1990–2026 (2021, 2023 and 2025) |
| State | Constituency (Wahlkreis) | 1980–2026 | – |
| Municipal council | Municipality | 1969–2026 | 2021 boundaries: 1990–2026; 2025 boundaries: 1969–2026 |
| County council (Kreistag) | Municipality and county | 1948–2026 | 1990–2026 (2021) |
| European Parliament | Municipality | 2009–2024 | 2009–2024 (2021) |
| Mayor | Municipality | 1945–2026 | 1945–2026 (2021) |
| Landrat | County | 1945–2026 | – |

Coverage within these ranges differs by state. The mayoral data also include candidate-level records and a panel of mayors. GERDA further provides municipal and county crosswalks, county council seat counts (2008–2025), shapefiles, and area, population, and employment covariates. `gerda_data_list()` lists every file with its exact years. State-election coverage by state and year is documented in [data/state_elections/metadata](data/state_elections/metadata/README.md).

## Harmonization

We use the official BBSR crosswalks, which track municipal mergers, splits, and boundary changes. Votes from merged municipalities are aggregated using population weights. Votes from joint mail-in voting districts are allocated to municipalities in proportion to their polling-card voters. Mayoral results are harmonized by municipality and election date, because mayoral elections are not held on the same day across municipalities. The legacy `state_harm` file is kept for backward compatibility; use `state_harm_21` or `state_harm_25` for new work.

Details are in the [pipeline reference](docs/data_pipeline.md), the [mayoral README](data/mayoral_elections/final/README.md), and the [federal harmonization README](code/federal_elections/municipality_level/README.md).

## Known data issues

- The allocation of joint mail-in votes is an approximation.
- Some states report small parties or independent candidates only as "Other". In small municipalities where major parties do not run, the "Other" share can be large.
- Harmonization and the allocation of mail-in votes can leave small differences from official vote totals. These cases are flagged in the data.
- Some official files use municipality codes that are missing from the crosswalks. We corrected these by hand against state archives.

State-specific limitations are listed in [source_limitations.csv](data/state_elections/metadata/source_limitations.csv) and the [mayoral known issues](docs/mayoral_elections_known_issues.md). Please report errors as [GitHub issues](https://github.com/awiedem/german_election_data/issues).

## License

The processed federal election datasets (municipality, county, and constituency level) are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), including commercial use. Please cite the paper above. This license does not cover the other election datasets, candidate data, covariates, shapefiles, or crosswalk source files; for these, the terms of the original providers apply. See [LICENSE](LICENSE) for details. The data are provided as is, without warranty.

## Sources

Federal results come from the Federal Returning Officer (Bundeswahlleiterin), state results from the statistical offices of the federal and state governments, and municipal, county, and mayoral results from the state statistical offices. The full list by state is in [docs/data_sources.md](docs/data_sources.md). Crosswalks come from the BBSR, and shapefiles from the BKG (VG250, © GeoBasis-DE / BKG, Datenlizenz Deutschland – Namensnennung – 2.0).

## Acknowledgements

We thank Cornelius Erfort, Sascha Riaz and Moritz Marbach for helpful comments. We also thank the anonymous reviewers at *Scientific Data* for their constructive feedback. Thanks to Daniela Gaus for excellent research assistance and Victor Kreitman for providing code and data on election dates.
