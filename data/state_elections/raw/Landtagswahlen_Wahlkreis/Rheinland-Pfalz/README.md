# Rheinland-Pfalz — Landtagswahl results at constituency (Wahlkreis) level

**State:** Rheinland-Pfalz (RP / RLP), state code 07
**Constituency unit:** Wahlkreis. Since the 1991 reform the state is divided into **52 Wahlkreise**
(Direktstimme/Wahlkreisstimme + Landesstimme). Earlier elections (1947–1987) used a different
districting (4 Bezirke / list-based mandate allocation) and are documented only in printed volumes.

**Primary sources (verified):**
- Landeswahlleiter Rheinland-Pfalz — Ergebnisse der Landtagswahlen: https://www.wahlen.rlp.de/landtagswahl/ergebnisse
- Statistisches Landesamt Rheinland-Pfalz: https://www.statistik.rlp.de/themen/wahlen
- Result files live under `https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/<YEAR>/`

Each downloaded XLSX is the **complete official result workbook** ("Endgültiges Ergebnis") for that
election. It contains multiple sheets at all geographic levels; the constituency level is on a dedicated
sheet ("Bezirke und Wahlkreise" for 2001/2006/2011, "Wahlkreisergebnisse" for 2016/2021, "LW_2026_WK"
for 2026). Sheets carry eligible voters, voters, valid/invalid votes, and votes per party for both the
Wahlkreisstimme (Direktstimme) and the Landesstimme.

## Downloaded files

| Year | File | Format | Source URL | Geo unit / notes |
|------|------|--------|------------|------------------|
| 2001 | `RP_2001_Landtagswahl_Wahlkreis.xlsx` | xlsx | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2001/LW_2001_Endgueltiges_Ergebnis.xlsx | Workbook; sheet "Bezirke und Wahlkreise" = 52 Wahlkreise |
| 2006 | `RP_2006_Landtagswahl_Wahlkreis.xlsx` | xlsx | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2006/LW_2006_Endgueltges_Ergebnis.xlsx | Workbook; sheet "Bezirke und Wahlkreise" |
| 2011 | `RP_2011_Landtagswahl_Wahlkreis.xlsx` | xlsx | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2011/LW2011_Endgueltiges_Ergebnis.xlsx | Workbook; sheet "Bezirke und Wahlkreise" |
| 2016 | `RP_2016_Landtagswahl_Wahlkreis.xlsx` | xlsx | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2016/LW2016_Endgueltiges_Ergebnis.xlsx | Workbook; sheet "Wahlkreisergebnisse" |
| 2021 | `RP_2021_Landtagswahl_Wahlkreis.xlsx` | xlsx | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2021/LW2021_Endgueltiges_Ergebnis.xlsx | Workbook; sheet "Wahlkreisergebnisse" |
| 2026 | `RP_2026_Landtagswahl_Wahlkreis.xlsx` | xlsx | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2026/Endgueltiges_Ergebnis_LW_2026_Wahlkreise.xlsx | Dedicated Wahlkreis workbook (sheet "LW_2026_WK") |
| 2026 | `RP_2026_Landtagswahl_Stimmbezirksebene_alle_Ebenen.csv` | csv | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2026/LW_2026_Endergebnis_Stimmbezirksebene.csv | Machine-readable all-levels file (semicolon-sep, ISO-8859-1); includes Wahlkreis Gebietsebene |
| 2026 | `RP_2026_Satzbeschreibung.pdf` | pdf | https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/dokumente-wahlen/ltw/Ergebnisdateien/2026/SatzbeschreibungErgebnisseGesamtLW_2026.pdf | Record-layout / codebook for the 2026 CSV |
| 1996 | `RP_1996_Landtagswahl_Wahlkreis.pdf` | pdf | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/RPHeft_derivate_00003485/Band%20363%20Die%20Wahl%20zum%2013.%20Landtag%20in%20Rheinland-Pfalz%20am%2024.%20Maerz%201996.pdf | Official StaLA volume "Die Wahl zum 13. Landtag in RLP am 24. März 1996", Band 363 (ISSN 0174-3406). Tabellenteil = results for Land, Bezirke, **Wahlkreise**, Verwaltungskreise, Verbandsgemeinden, Gemeinden; incl. 1991-Landtagswahl comparison figures. 33 MB scanned PDF (text layer present). Added by completeness critic 2026-06-27. |

All XLSX files verified `Microsoft Excel 2007+`, CSV verified `text/csv`, PDF verified `application/pdf`.
Each year's Wahlkreis sheet was confirmed present by inspecting the workbook sheet names.

## Years that exist but are NOT downloadable at constituency level

The full election series is **1947, 1951, 1955, 1959, 1963, 1967, 1971, 1975, 1979, 1983, 1987, 1991, 1996**,
then 2001–2026 (above). For the years below, **no machine-readable or downloadable Wahlkreis-level result
file is published online** by the Landeswahlleiter or Statistisches Landesamt. Constituency-level results
exist only in the printed "Statistik von Rheinland-Pfalz" / "Die Wahl zum N. Landtag" volumes
(Statistisches Landesamt). The online `Ergebnisdateien/<YEAR>/` directory begins at 2001 (404 for all
earlier years, multiple naming patterns tried).

| Year | Reason | Where to find |
|------|--------|----------------|
| 1947 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series "Statistik von Rheinland-Pfalz" |
| 1951 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series |
| 1955 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series |
| 1959 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series |
| 1963 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series |
| 1967 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series |
| 1971 | Pre-1991 districting; printed volume only | Statistisches Landesamt RLP printed series |
| 1975 | 4-Bezirk system; printed volume only | Statistisches Landesamt RLP printed series |
| 1979 | 4-Bezirk system; printed volume only | Statistisches Landesamt RLP printed series |
| 1983 | 4-Bezirk system; printed volume only | Statistisches Landesamt RLP printed series |
| 1987 | 4-Bezirk system; printed volume only | Statistisches Landesamt RLP printed series |
| 1991 | First 52-Wahlkreis election, but no online result file (online files start 2001) | Statistisches Landesamt RLP printed volume "Die Wahl zum 12. Landtag" |
| 1996 | OBTAINED — see table above. Full StaLA volume Band 363 digitized in the Statistische Bibliothek (statistischebibliothek.de); contains Wahlkreis-level tables. (`Ergebnisdateien/1996/` on wahlen.rlp.de still 404, but the printed volume is online.) | n/a — downloaded |

Notes:
- wahlrecht.de and wahlen-in-deutschland.de aggregate pre-2001 figures but do not link official
  Wahlkreis-level source files; their stated source is the printed Statistisches-Landesamt series.
- The interactive results portal (23degrees.eu) renders 2021/2026 but exposes the same downloadable
  files captured here.

## Completeness-critic update (2026-06-27)

Re-attempted all pre-2001 years a second time. Key new source: the **Statistische Bibliothek**
(federated digital library of the German statistical offices, `statistischebibliothek.de`, MyCoRe/SOLR).
Its SOLR proxy (`/mir/servlets/solr/select?q=...&wt=json`) and the RLP Landtagswahl Wahlveröffentlichung
series object `RPSerie_mods_00001197` were enumerated exhaustively.

- **1996** (Band 363, `RPHeft_mods_00012593`, derivate `RPHeft_derivate_00003485`) — **newly downloaded** as
  a full Wahlkreis-level scanned volume. This corrects the first agent's "no online result file" verdict.
- **2001** (Band ~, `RPHeft_mods_00012589`) is also in the library as a PDF, but we already hold the superior
  machine-readable XLSX, so the PDF was intentionally NOT downloaded.
- **1991 and earlier (1947–1991): still not available online.** The library's RLP Landtag election series
  has exactly two members (1996, 2001). The 1991 volume ("Die Wahl zum 12. Landtag", Band ~345) and all
  earlier volumes (4-Bezirk system 1975–1987; pre-reform districting 1947–1971) are NOT digitized anywhere
  (StaLA, Landeswahlleiter, Statistische Bibliothek, regionalstatistik GENESIS all checked). They exist only
  as printed StaLA volumes. Note: the 1996 volume above *does* carry 1991 Landesstimmen comparison figures
  per Wahlkreis, partially recovering 1991 at the constituency level.

## Second gap-fill pass (2026-06-27, additional sources)

A further attempt targeted sources the earlier passes had not fully exhausted. **No new files
obtained** — all pre-2001 Wahlkreis volumes remain undigitized. Sources newly checked and ruled out:

- **dilibri** (`dilibri.de`, Landesbibliothekszentrum RLP digital library; OAI-PMH at `/oai` is open):
  holds only pre-1950 historical books plus the 2005 *Datenhandbuch zur Geschichte des Landtags
  Rheinland-Pfalz 1947–2003* (a secondary book, not raw Wahlkreis tables). No StaLA election Hefte.
  Front-end is behind a JS+cookie challenge; OAI/Google site-search confirm the holdings.
- **wahlen.rlp.de fileadmin** — re-probed `Ergebnisdateien/{1975,1979,1983,1987,1991}/` and the
  `LW_<YEAR>_Endgueltiges_Ergebnis.xlsx` naming pattern: all **HTTP 404**. Online files truly start 2001.
- **statistischebibliothek.de SOLR** — re-queried with variant terms ("Die Wahl zum Landtag",
  "Landtagswahl Rheinland-Pfalz", source filter `RP*`): the only RLP Landtag election volumes are
  **1996** (`RPHeft_mods_00012593`) and **2001** (`RPHeft_mods_00012589`). Nothing earlier exists in
  the federated library. (Analyse-Hefte for 2006–2026 also present but are not pre-1996 sources.)
- **Wikimedia Commons / archive.org** — no scanned StaLA Wahl-Hefte for any RP Landtag year.
- **statistik.rlp.de** Zeitreihen/Publikationen pages — only state-level aggregate time series, no
  constituency-level downloadable file for 1947–1991.

Conclusion unchanged: 1947–1991 constituency results exist **only in printed StaLA volumes** and would
require manual scanning/OCR of physical holdings. 1991 is partially recoverable via the 1996 volume's
per-Wahlkreis comparison figures (already held).

_Collected 2026-06-27 for the GERDA election database. Raw files stored verbatim._
