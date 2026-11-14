# GERDA data sources

## Federal Elections

Bundeswahlleiterin. [https://www.bundeswahlleiterin.de/bundeswahlleiter.html](https://www.bundeswahlleiterin.de/bundeswahlleiter.html).

## State Elections

Statistische Ämter des Bundes und der Länder. Landtagswahlen. [https://www.regionalstatistik.de/genesis/online/](https://www.regionalstatistik.de/genesis/online/) (Regional data bank of the German Federal Statistical Office). Retrieved and imported using the wiesbaden R package and the SOAP XML web service of DESTATIS.

## Municipal Elections

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

## Mayoral Elections

| **State**               | **Source**                                                        | **Procured via**                         |
|-------------------------|------------------------------------------------------------------|------------------------------------------|
| Baden-Württemberg       | Statistisches Landesamt B VII 3-j/25; municipal votemanager/Komm.ONE results and archived notices | website (JSON, HTML, PDF); web archives |
| Bayern                  | Bayerisches Landesamt für Statistik                              | website (Excel)                          |
| Hessen                  | Hessisches Statistisches Landesamt, *Direktwahlen in Hessen seit 1993* (results, elected person's gender and term counters). Candidate names 1993–2012: Hessami, Zohal (2018), "Accountability and Incentives of Appointed and Elected Public Officials", *Review of Economics and Statistics* 100(1): 51–64, replication data [doi:10.7910/DVN/FZWOMK](https://doi.org/10.7910/DVN/FZWOMK) (CC0); losing candidates' names in the restricted twin only | email (workbook); Harvard Dataverse (names); municipal votemanager (75 additional full rounds) |
| Niedersachsen           | Landesamt für Statistik Niedersachsen                            | website (PDF)                            |
| Nordrhein-Westfalen     | IT.NRW                                                           | website (Excel)                          |
| Rheinland-Pfalz         | Statistisches Landesamt Rheinland-Pfalz                          | website (Excel, percentages only)        |
| Saarland                | Statistisches Landesamt des Saarlandes                           | website (Excel)                          |
| Sachsen                 | Statistisches Landesamt des Freistaates Sachsen (Bürgermeisteratlas) | website (Excel)                      |
| Schleswig-Holstein      | Statistisches Amt für Hamburg und Schleswig-Holstein              | web scraping (wahlen-sh.de)              |

For details on data structure and known issues, see the [Mayoral Elections README](data/mayoral_elections/final/README.md) and [Known Issues](docs/mayoral_elections_known_issues.md).

## Crosswalks

Bundesinstitut für Bau-, Stadt- und Raumforschung. Umsteigeschlüssel für konsistente zeitreihen. [https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/umstiegsschluessel/umsteigeschluessel.html](https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/umstiegsschluessel/umsteigeschluessel.html) (2024).

## Shapefiles

Federal Agency for Cartography and Geodesy (BKG). Vg250: Administrative boundaries of germany. [http://www.bkg.bund.de](http://www.bkg.bund.de) (2021). Open Data Lizenz Deutschland – Namensnennung – Version 2.0. Source reference: © GeoBasis-DE / BKG (year of last data download).

Sources for Landrat elections are in [landrat_data_sources.md](landrat_data_sources.md).
