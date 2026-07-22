# County council seat coverage through 2025

## Result

`county_council_seats` is a balanced panel with 7,200 unique county-years: 400 current-boundary counties in every year from 2008 through 2025. The extension adds 15 Schleswig-Holstein results from 2023 and 161 results from the 2024 elections in eight states. Each election result is carried forward through 2025.

The panel compares counties on a fixed set of current boundaries. It does not add abolished predecessor counties or fill municipal council-seat gaps.

## Source coverage audit

The source check asked whether each official file identifies the county and reports either party seats directly or elected candidates that can be counted by party. Files are cached below `data/county_elections/raw/Kreistagswahlen/`.

| State-year | Cached input | Format | Geographic identifier | Seat evidence | Rows | Status |
| :-- | :-- | :-- | :-- | :-- | --: | :-- |
| Schleswig-Holstein 2023 | `Schleswig-Holstein/Schleswig-Holstein_2023_Sitzverteilung.xlsx` | XLSX | Official county order, matched to the 15 state county codes | Total and party seat columns | 15 | Complete |
| Baden-Württemberg 2024 | `Baden-Württemberg/Baden-Württemberg_2024_Kommunalwahlen.pdf` | PDF | Three-digit suffix of the Kreisschlüssel | Final seat-distribution tables | 44 | Complete |
| Brandenburg 2024 | `Brandenburg/2024_html/kreis_*.html` | HTML | County number in the official URL | One row per elected candidate, counted by party | 18 | Complete |
| Mecklenburg-Vorpommern 2024 | `Mecklenburg-Vorpommern/Mecklenburg-Vorpommern_2024_Mandate.csv` | CSV | Official county number | Final mandate columns | 8 | Complete |
| Rheinland-Pfalz 2024 | `Rheinland-Pfalz/Rheinland-Pfalz_2024_Ergebnisse_Kreisebene.pdf` | PDF | County heading matched to the fixed code list | 2024 and 2019 party seats | 36 | Complete |
| Saarland 2024 | `Saarland/2024_html/kreis_*.html` | HTML | County number in the official URL | Embedded final seat-chart data | 6 | Complete |
| Sachsen 2024 | `Sachsen/Sachsen_2024_Sitzverteilung.xlsx` and `Sachsen/Sachsen_2024_Gemeinderat_Sitzverteilung.xlsx` | XLSX | Official five- or eight-digit area code | Final party seat columns | 13 | Complete |
| Sachsen-Anhalt 2024 | `Sachsen-Anhalt/Sachsen-Anhalt_2024_Ergebnisse_mit_Sitzen.csv` | CSV | `Schlüsselnummer` | `S - Sitze insgesamt` and party seat columns | 14 | Complete |
| Thüringen 2024 | `Thüringen/2024_html/kreis_*.html` | HTML | County number in the official URL | Final party seat table | 22 | Complete |

Regionalstatistik was checked first for a reusable county-level seat table. Its public catalogue did not yield a complete, directly usable table for these state-years. The build therefore uses the authoritative state files above and does not depend on Regionalstatistik. No acquisition failure remains.

All source-specific labels are assigned to one of the nine published categories. SPD, CDU, FDP, GRÜNE, Die Linke, AfD, and Freie Wähler receive their named columns; SSW receives `seats_regional`; local lists, joint nominations, independents, and minor parties receive `seats_other`, except that the established Rheinland-Pfalz local-list convention remains in `seats_regional`. There are no unmapped labels in the output.

This convention differs from the one used in the hand-compiled 2008–2022 rows, which frequently folded Freie Wähler and local voter groups into `seats_regional` rather than `seats_freie_wahler` and `seats_other`. As a result the three-way split of non-major-party seats (`seats_freie_wahler`, `seats_regional`, `seats_other`) is **not comparable across the 2022/2023 boundary** — for example Landkreis Böblingen reports `seats_regional` 26 in 2019 and `seats_freie_wahler` 24 in 2024 for the same kind of local lists. The 2008–2022 rows cannot be re-split because the source did not record the underlying detail. The panel therefore carries a derived column `seats_local_other = seats_freie_wahler + seats_regional + seats_other` (equivalently `seats_total` minus the six major parties), which is defined identically in every year and state and is the column to use for any time series of non-establishment seats. The six major-party columns are unaffected and comparable throughout.

## Validation

The complete script passed these checks on 22 July 2026:

- 7,200 rows, 400 counties, 18 years, and no duplicate county-year keys.
- Every year has exactly 400 rows; every county code is a five-character string.
- `year`, `seats_total`, and all nine party-seat fields have the intended integer types. All observed seat values are non-negative.
- All 176 newly parsed election rows have a reported total equal to the sum of the nine seat categories.
- The 2008–2022 rows are identical to the previously generated RDS object, including all fields.
- Counties outside the nine added state-years retain their 2022 vector through 2025. Schleswig-Holstein changes from 2023 onward; every 2024 result is unchanged in 2025.
- The RDS and typed CSV read-back have identical dimensions, keys, column names, and column types. The files are 1.9 MB and 1.4 MB, respectively.

There are 39 panel rows with missing council totals. All originate in the historical source or its carry-forward; no newly parsed election result has a missing total. Eight rows are explicitly total-incongruent: Groß-Gerau 2011, Landkreis Heilbronn 2008, and Donau-Ries 2020–2025. The latter includes the unchanged 2022 vector carried through the three new panel years.

## Overlap audit

The Rheinland-Pfalz report publishes 2019 and 2024 seats side by side. Reconstructing 2019 reproduces 33 of 36 complete historical seat vectors. The frozen hand-compiled rows disagree for `07140`, `07235`, and `07335`. The script records this exact exception set, leaves all historical values unchanged, and stops if a future source or parser produces any different set of disagreements.

## Official spot checks

| County | Official result | Total seats | Largest category | Panel comparison |
| :-- | :-- | --: | :-- | :-- |
| Vogtlandkreis (`14523`) | [Sachsen final Kreistag results](https://www.wahlen.sachsen.de/kreistagswahlen-2024-wahlergebnisse.php) | 86 | AfD, 25 | Match |
| Sömmerda (`16068`) | [Thüringen final county result](https://wahlen.thueringen.de/datenbank/wahl1/wahl.asp?wJahr=2024&wahlart=KW&wknr=068&zeigeErg=WK) | 40 | CDU, 14 | Match |
| Regionalverband Saarbrücken (`10041`) | [Saarland final county result](https://wahlergebnis.saarland.de/KTW/ergebnisse_kreis_41.html) | 45 | CDU, 15 | Match |

## Remaining gaps

The fixed-boundary contract still excludes pre-reform predecessor councils. Municipal seat gaps remain outside this dataset. `government_party` is carried forward from 2022 while a county's council is unchanged, so it is present for 2023–2025 in the states without a new election (NRW, Bayern, Niedersachsen, Hessen and the city-states); it is `NA` from a county's 2023 or 2024 election onward because the official seat-result sources for those elections do not identify the governing party (367 of the 1,200 rows in 2023–2025).
