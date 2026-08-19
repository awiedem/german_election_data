# County election vote data: current status and next steps

**Last updated:** 4 August 2026

## Current state

The county-election vote pipeline builds successfully and contains verified rows for 96 of the 99 expected post-1990 territorial-state election events. Schleswig-Holstein 1990/1994 exists only as unparsed official scans, and Rheinland-Pfalz 1994 has no source in the repository. Municipality contributions are available for 72 events, partly available for Mecklenburg-Vorpommern 2011, and absent for 26 events.

The main unresolved work is acquisition or image verification, not a general parser failure. Schleswig-Holstein 1990/1994 and Brandenburg 1993/1998 have official scans, but their municipal tables cannot enter the main data until every retained value passes the printed arithmetic checks. Rheinland-Pfalz 1994 and the historical Rheinland-Pfalz municipality contributions require an official response. The Nordrhein-Westfalen 2025 municipality layer is complete.

No count has been inferred from a rounded percentage, no pooled postal result has been allocated to municipalities, and no ambiguous OCR value has entered the main datasets.

## Scope and assumptions

This document concerns votes in Kreistag elections and the separately identified city-council contests used as county equivalents. It does not describe the annual county-council seat panel.

The coverage manifest compares state-by-election-year events in the modern post-1990 election schedule. The unharmonized output also contains earlier observations, beginning with Hessen in 1948, so its date range is wider than the coverage universe.

The pipeline follows four rules:

1. Only exact official observations enter the main datasets.
2. Exact county rows take precedence when both county totals and lower-level contributions exist.
3. Municipality rows are not used to manufacture a complete event when postal pools or other components cannot be assigned exactly.
4. Kreisfreie-city council contests remain distinguishable from Kreistag contests through `contest_type`.

## Current outputs

The full pipeline was rebuilt on 4 August 2026.

| Output | Meaning | Rows | Columns | Duplicate key count |
| :-- | :-- | --: | --: | --: |
| `county_elec_unharm` | Results on the geography reported for each election | 56,251 | 672 | 0 `AGS × election_year` |
| `county_elec_harm_21_muni` | Municipality results mapped to 2021 boundaries | 39,191 | 683 | 0 `AGS × election_year` |
| `county_elec_harm_21_cty` | County results mapped to the harmonized county geography | 2,666 | 680 | 0 `county × election_year` |

The [coverage manifest](../data/county_elections/final/county_election_coverage.csv) is the authoritative event-level inventory. The [additions table](../output/tables/county_election_additions.csv) is the same inventory restricted to incomplete events and sorted by priority. The [coverage figure](../output/figures/county_election_coverage.pdf) is a visual summary, not the source for coverage decisions.

## Event coverage

The manifest contains 99 expected territorial-state election events and three city-state rows marked not applicable.

| Status | Events | Interpretation |
| :-- | --: | :-- |
| Municipality available | 72 | Exact municipality contributions are present. |
| Municipality partial | 1 | Exact municipality rows exist, but an unallocatable component remains. This is Mecklenburg-Vorpommern 2011. |
| County only | 23 | Exact county or county-equivalent totals exist, but municipality contributions do not. |
| Raw only | 2 | Official municipality scans exist, but no safe numeric extraction exists. These are Schleswig-Holstein 1990 and 1994. |
| Missing source | 1 | No exact event totals are in the repository. This is Rheinland-Pfalz 1994. |

State-level coverage is:

| State | Events | Municipality available | Partial | County only | Raw only | Missing |
| :-- | --: | --: | --: | --: | --: | --: |
| Schleswig-Holstein | 8 | 6 | 0 | 0 | 2 | 0 |
| Niedersachsen | 7 | 7 | 0 | 0 | 0 | 0 |
| Nordrhein-Westfalen | 7 | 6 | 0 | 1 | 0 | 0 |
| Hessen | 8 | 8 | 0 | 0 | 0 | 0 |
| Rheinland-Pfalz | 7 | 1 | 0 | 5 | 0 | 1 |
| Baden-Württemberg | 7 | 0 | 0 | 7 | 0 | 0 |
| Bayern | 7 | 0 | 0 | 7 | 0 | 0 |
| Saarland | 7 | 7 | 0 | 0 | 0 | 0 |
| Brandenburg | 7 | 5 | 0 | 2 | 0 | 0 |
| Mecklenburg-Vorpommern | 9 | 8 | 1 | 0 | 0 | 0 |
| Sachsen | 8 | 7 | 0 | 1 | 0 | 0 |
| Sachsen-Anhalt | 8 | 8 | 0 | 0 | 0 | 0 |
| Thüringen | 9 | 9 | 0 | 0 | 0 | 0 |

The implication is that strict event-level county coverage is nearly complete. The remaining substantive gap is municipality detail, especially in states that publish only county totals.

## Work completed in the latest recovery round

### Rheinland-Pfalz

The pipeline now contains exact county and county-equivalent totals for 1999–2024. The 2024 official portal also supplies 2,289 municipality contributions covering all 24 Landkreise. These rows aggregate exactly to the official county totals.

The public historical reports do not publish individual municipality contributions to the Kreistag totals for 1994–2019. A request for the exact 1994 event totals, Band 358, and municipality contributions for 1994–2019 was sent to `wahlen@statistik.rlp.de` on 31 July 2026. Vincent Heddesheimer was copied. No response has been incorporated into the repository.

### Mecklenburg-Vorpommern

Historical results for 1990–2011 are integrated. The 2011 workbook contains 804 exact municipality rows and 78 separate administrative-office postal pools. The municipality rows contain 1,524,412 valid votes, or 91.77% of the county totals; the postal pools contain the remaining 136,788 valid votes.

The pools are preserved separately in `county_elec_mv_2011_postal_pools`. They are not allocated or imputed. All six exact 2011 county rows remain authoritative in the county output.

### Nordrhein-Westfalen

Exact county and county-equivalent rows are integrated for 1994 and 2025. IT.NRW supplied `KW25_Stimmbezirke.csv` on 4 August 2026 after the project requested the missing municipality results. The official file contains 17,166 polling-district rows for all 396 municipalities.

The parser aggregates the file to 374 municipality contributions covering the 30 Landkreise and Städteregion Aachen. It excludes the 22 kreisfreie cities from this lower-level layer because their exact city-council results already appear as county-equivalent rows. The 374 municipality rows reproduce all 31 published county/county-equivalent totals for eligible voters, voters, valid and invalid votes, and every party column. Kleve, Viersen, and Wesel are therefore no longer missing. The [NRW source inventory](../output/tables/nrw_2025_municipality_source_inventory.csv) records the original portal audit and the statewide-file resolution.

### Brandenburg

The 2003–2024 municipality series and the exact 1993/1998 county summaries are integrated. The 1993/1998 municipal scans remain unparsed in the main data.

The bounded OCR pilot used Ahrensfelde and Eberswalde in both elections. All four rows pass their party-sum identities and remain below the exact Barnim county totals. Three of 44 numeric fields required image correction, a 6.82% correction rate. The pilot shows that full recovery is feasible with pagewise image verification; it also shows that unattended OCR is unsafe.

### Schleswig-Holstein

All 82 turnout/result page pairs per election are present, and the historical crosswalk identifies exactly 1,131 municipality rows per year. The row-location problem is solved, but the numeric OCR is not accurate enough for import.

In the best pass, only 340 of 1,131 rows in 1990 and 617 of 1,131 rows in 1994 pass both the ballot and party-sum identities. Retaining only those rows would drop 69.94% and 45.45% of municipalities, respectively. Both losses exceed the 20% reporting threshold and would create a selected sample. No Schleswig-Holstein 1990/1994 row was emitted.

## Remaining work

| Priority | Block | Current evidence | Next action | Completion gate |
| :-- | :-- | :-- | :-- | :-- |
| P0 | Rheinland-Pfalz 1994 | Exact event totals are absent; Band 358 is not available in a usable public copy. | Process the statistical office response. Follow up around 14 August 2026 if no response arrives. | Exact county/city totals match the official statewide checksum. |
| P0 | Schleswig-Holstein 1990/1994 | All rows are located, but hundreds of numeric cells remain unresolved across independent OCR passes. | Build a cell-crop workflow and image-verify every corrected value. Pages can be processed independently. | Every emitted row passes ballot and party identities; every county and state total matches the publication. |
| P1 | Rheinland-Pfalz 1999–2019 municipalities | Public reports stop above the individual-municipality contribution level. | Process any machine-readable files or scans supplied by the statistical office. | Each election covers all 24 Landkreise and reaggregates exactly. |
| P2 | Brandenburg 1993/1998 municipalities | Four-row pilot passed with a 6.82% cell-correction rate. | Run pagewise OCR and manual verification, parallelized by source page. | Municipality rows reproduce every printed unit, county, and state total. |
| P2 | Nordrhein-Westfalen 1994 municipalities | Exact county totals exist; municipal Kreistag contributions do not. | Request detail after the Rheinland-Pfalz requests are resolved. | Complete municipality coverage with exact county reconciliation. |
| Defer | Baden-Württemberg and Bayern municipality detail | Central publications report Kreistag results at Landkreis level. Municipality tables generally describe Gemeinderat elections instead. | Retain exact county rows. Recheck Bayern's 2026 regional report only if it explicitly contains municipality contributions to Kreistag results. | An official source explicitly identifies municipality contributions to the county contest. |
| Defer | Sachsen 1994 municipality detail | Exact county totals are already present. | Request detail after the higher-return blocks. | Complete exact municipality series. |

The two agency-dependent blocks can proceed in parallel with the two OCR blocks. Within each OCR block, pages are independent, but the final county reconciliation must be run centrally before integration.

## Validation and known limitations

The latest build and targeted tests establish the following:

- All three published vote outputs have unique geographic-unit-by-election-year keys.
- County turnout lies between 0.2841 and 0.8536.
- No county row has a total party share below 0.95 or above 1.05.
- Exact county precedence is tested directly for the 36 Rheinland-Pfalz 2024 rows and six Mecklenburg-Vorpommern 2011 rows.
- The 374 Nordrhein-Westfalen 2025 municipality contributions reproduce all 31 exact county/county-equivalent totals, including every party column. The 22 independent-city council rows remain exact county-equivalent observations.
- The unharmonized data contain two turnout values slightly above one, both retained as documented source anomalies rather than silently corrected.
- Baden-Württemberg 2024 has 35 county result rows without electorate, voter, or turnout fields in the available source.
- Sachsen 1994 has 19 county rows without exact valid/invalid ballot counts because the source reports only rounded percentages. These rows are flagged with `source_limitation`.
- Older Niedersachsen and Hessen sources often omit invalid ballots. Missing values remain missing; they are not coded as zero.
- The harmonization script emits crosswalk many-to-many warnings and reports one duplicate in an intermediate combined object. Both final outputs have zero duplicate keys. The intermediate warning should be investigated before treating the harmonization code as fully clean, even though the current published keys are unique.

## Rebuild and test commands

Run from the repository root in this order:

```sh
Rscript code/county_elections/01_county_elec_unharm.R
Rscript code/county_elections/02_county_elec_harm_21.R
Rscript code/county_elections/04_county_election_coverage.R
Rscript code/plots/county_election_coverage.R
```

The critical recovery tests are:

```sh
Rscript code/county_elections/tests/test_county_election_coverage.R
Rscript code/county_elections/tests/test_county_exact_precedence.R
Rscript code/county_elections/tests/test_nrw_2025_source_inventory.R
Rscript code/county_elections/tests/test_parse_nrw_2025.R
Rscript code/county_elections/tests/test_parse_rlp.R
Rscript code/county_elections/tests/test_parse_rlp_2024_muni.R
Rscript code/county_elections/tests/test_parse_mv_2011_muni.R
Rscript code/county_elections/tests/test_parse_sh_1990_1994.R
Rscript code/county_elections/tests/test_pilot_bb_1993_1998_muni_ocr.R
```

Each state-specific parser also has a test under `code/county_elections/tests/`. A new source should not be integrated until its standalone test passes and the complete pipeline rebuilds.

## Authoritative files

- Main unharmonized build: [`01_county_elec_unharm.R`](../code/county_elections/01_county_elec_unharm.R)
- Harmonization build: [`02_county_elec_harm_21.R`](../code/county_elections/02_county_elec_harm_21.R)
- Coverage manifest generator: [`04_county_election_coverage.R`](../code/county_elections/04_county_election_coverage.R)
- Coverage metadata and expected event universe: [`county_election_support.R`](../code/county_elections/county_election_support.R)
- State-specific parsers: [`code/county_elections/parsers/`](../code/county_elections/parsers/)
- Parser and integration tests: [`code/county_elections/tests/`](../code/county_elections/tests/)
- Final event inventory: [`county_election_coverage.csv`](../data/county_elections/final/county_election_coverage.csv)

`CLAUDE.md` still says that county elections have no processing code, and the county-election date ranges in `README.md` stop at 2024. Those statements are stale. They should be updated separately after this recovery branch is finalized.

## Glossary

- **AGS (Amtlicher Gemeindeschlüssel):** Eight-digit official municipality identifier. The first five digits identify the county.
- **County-equivalent contest:** A city-council election in a kreisfreie Stadt that is stored alongside Kreistag elections but identified separately by `contest_type`.
- **Kreistag:** Elected council of a Landkreis.
- **Municipality contribution:** The votes reported for one municipality as part of its Landkreis's Kreistag election, not the municipality's separate Gemeinderat election.
- **Postal pool:** Postal votes reported for a group of municipalities or an administrative office rather than one municipality. The pipeline preserves these pools separately when they cannot be allocated exactly.
- **Raw only:** An official source is present, but no verified parser output has entered the final data.
- **Unharmonized:** Results use the geographic units and identifiers reported at the election date.
- **Harmonized:** Results are mapped to a fixed later geography to support comparisons across boundary changes.
