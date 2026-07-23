# Mecklenburg-Vorpommern — Landtagswahl results at constituency level (raw)

**State:** Mecklenburg-Vorpommern (state code 13)
**Constituency unit:** Wahlkreis (36 Wahlkreise). MV uses a two-vote system
(Erststimme = Wahlkreisstimme / direct candidate; Zweitstimme = Landesstimme / party list).
**Authoritative source:** Landesamt für innere Verwaltung MV (LAIV-MV) /
Landeswahlleiter MV — https://www.laiv-mv.de/Wahlen/Landtagswahlen/

All files downloaded verbatim from LAIV-MV on 2026-06-27. Verified HTTP 200 and
correct content-type before download.

## Downloaded files

| Year | Election date | File | Format | Geo unit | Source URL |
|------|---------------|------|--------|----------|------------|
| 1990 | 1990-10-14 | MV_1990_Landtagswahl_nach_Gemeinden.xls | XLS | Gemeinde (municipality; carries Wahlkreis assignment) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Dokumente/Landtagswahlen/Ergebnisseite/LW%201990%20Endg%C3%BCltiges%20Ergebnis%20nach%20Gemeinden.xls |
| 1990 | 1990-10-14 | MV_1990_Landtagswahl_Erst-Zweitstimmen_Landesergebnis.pdf | PDF | Land (state summary only) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Dokumente/Landtagswahlen/Ergebnisseite/LW%201990%20Erst-Zweitstimmen.pdf |
| 1990 | 1990-10-14 | MV_1990_Landtagswahl_Mandate.pdf | PDF | Land (seat allocation summary) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Dokumente/Landtagswahlen/Ergebnisseite/LW%201990%20Mandate.pdf |
| 1994 | 1994-10-16 | MV_1994_Landtagswahl_nach_Wahlbezirken.xls | XLS | Wahlbezirk (precinct; first column = Wahlkreis) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Dokumente/Landtagswahlen/Ergebnisseite/LW%201994%20Endg%C3%BCltiges%20Ergebnis%20nach%20Wahlbezirken.xls |
| 1994 | 1994-10-16 | MV_1994_Landtagswahl_nach_Gemeinden.xls | XLS | Gemeinde (municipality) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Dokumente/Landtagswahlen/Ergebnisseite/LW%201994%20Endg%C3%BCltiges%20Ergebnis%20nach%20Gemeinden.xls |
| 1998 | 1998-09-27 | MV_1998_Landtagswahl_nach_Wahlbezirken.xls | XLS | Wahlbezirk (precinct; first column = Wahlkreis) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724W%201998%2001.xls |
| 1998 | 1998-09-27 | MV_1998_Landtagswahl_nach_Gemeinden.xls | XLS | Gemeinde (municipality) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724G%201998%2001.xls |
| 2002 | 2002-09-22 | MV_2002_Landtagswahl_Wahlkreis.pdf | PDF | Statistischer Bericht B VII (incl. per-Wahlkreis tables) | https://www.laiv-mv.de/serviceassistent/download?id=1643535 |
| 2006 | 2006-09-17 | MV_2006_Landtagswahl_Wahlkreis.pdf | PDF | Statistischer Bericht B VII (incl. per-Wahlkreis tables) | https://www.laiv-mv.de/serviceassistent/download?id=1643531 |
| 2011 | 2011-09-04 | MV_2011_Landtagswahl_Wahlkreis.pdf | PDF | Statistischer Bericht B VII (incl. per-Wahlkreis tables) | https://www.laiv-mv.de/serviceassistent/download?id=1643527 |
| 2016 | 2016-09-04 | MV_2016_Landtagswahl_Wahlkreis.xlsx | XLSX | Statistischer Bericht B VII (all levels incl. Wahlkreis) | https://www.laiv-mv.de/serviceassistent/download?id=1643322 |
| 2016 | 2016-09-04 | MV_2016_Landtagswahl_Wahlkreis.pdf | PDF | Statistischer Bericht B VII | https://www.laiv-mv.de/serviceassistent/download?id=1643318 |
| 2021 | 2021-09-26 | MV_2021_Landtagswahl_Wahlkreis.xlsx | XLSX | Statistischer Bericht B VII (all levels incl. Wahlkreis) | https://www.laiv-mv.de/serviceassistent/download?id=1651136 |
| 2021 | 2021-09-26 | MV_2021_Landtagswahl_Wahlkreis.pdf | PDF | Statistischer Bericht B VII | https://www.laiv-mv.de/serviceassistent/download?id=1651135 |

## Notes on geographic units

- **2016 & 2021** are published as machine-readable XLSX (the official Statistischer
  Bericht "Endgültiges Ergebnis"), which include all reporting levels — Land,
  Wahlkreis, Landkreis/kreisfreie Stadt, and Gemeinde — in one workbook. The
  Wahlkreis breakdown is contained within. PDFs of the same report also captured.
- **2002, 2006, 2011** are only offered as the PDF Statistischer Bericht (no XLSX/CSV
  on the LAIV site). Each report contains per-Wahlkreis result tables (Erst-/Zweitstimmen,
  turnout, valid/invalid). These require OCR/table extraction.
- **1994 & 1998** predate the unified XLSX reports. LAIV offers two XLS files each:
  "nach Wahlbezirken" (precinct-level; **the first column is the Wahlkreis number**, so
  these aggregate cleanly to the 36 Wahlkreise) and "nach Gemeinden" (municipality-level).
  Both downloaded. The Wahlbezirke file is the constituency-resolvable one.
- **1990** (first post-reunification Landtagswahl): only a "nach Gemeinden" XLS plus two
  tiny state-level summary PDFs are published. There is no Wahlbezirk/Wahlkreis file for
  1990 on the LAIV site; the Gemeinden file carries the AGS and can be aggregated, and the
  Erst-/Zweitstimmen PDF gives the statewide first/second-vote split only.

## Years missing at clean Wahlkreis-level machine-readable format

None entirely missing — every election 1990–2021 has at least one downloaded file.
Limitations:
- **1990**: no precinct/Wahlkreis file; only Gemeinde XLS + state-summary PDFs.
- **2002, 2006, 2011**: only PDF (no XLSX/CSV); Wahlkreis tables exist inside the PDF.

## Addendum 2026-06-27 — machine-readable XLS for 2002, 2006, 2011 (completeness pass)

A second (critic) pass found that 2002, 2006 and 2011 — previously captured only as
the PDF Statistischer Bericht — are **also** published as the official machine-readable
Excel workbooks under the same `Publikationen/B721/` path that served the 1998 files
(`B724W ...` = nach Wahlbezirken, `B724G ...` = nach Gemeinden). The **Wahlbezirke** file
is the constituency-resolvable one (its first column is the Wahlkreis number, exactly
like 1994/1998), so each of these years now has clean Wahlkreis-aggregable XLS rather
than PDF-only. All six verified HTTP 200 / `application/vnd.ms-excel`, and `file`
confirms genuine Excel binaries with embedded titles "B724W/G Endgültiges Ergebnis der
Landtagswahl YYYY nach Wahlbezirken/Gemeinden".

| Year | File | Format | Geo unit | Source URL |
|------|------|--------|----------|------------|
| 2002 | MV_2002_Landtagswahl_nach_Wahlbezirken.xls | XLS | Wahlbezirk (precinct; first column = Wahlkreis) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724W%202002%2001.xls |
| 2002 | MV_2002_Landtagswahl_nach_Gemeinden.xls | XLS | Gemeinde (municipality) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724G%202002%2001.xls |
| 2006 | MV_2006_Landtagswahl_nach_Wahlbezirken.xls | XLS | Wahlbezirk (precinct; first column = Wahlkreis) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724W%202006%2001.xls |
| 2006 | MV_2006_Landtagswahl_nach_Gemeinden.xls | XLS | Gemeinde (municipality) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724G%202006%2001.xls |
| 2011 | MV_2011_Landtagswahl_nach_Wahlbezirken.xls | XLS | Wahlbezirk (precinct; first column = Wahlkreis) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724W%202011%2001.xls |
| 2011 | MV_2011_Landtagswahl_nach_Gemeinden.xls | XLS | Gemeinde (municipality) | https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Publikationen/B721/B724G%202011%2001.xls |

The earlier PDFs for these three years are retained (graphical context / cross-check).
Re-confirmed in this pass: 1990 has **no** Wahlbezirk/Wahlkreis file anywhere on LAIV
(checked the official 1990 results page directly) — only the Gemeinden XLS + the two
state-summary PDFs already present. 2016/2021 use the newer `serviceassistent` XLSX
(already present); no `B721` variant exists for them. The B721 path is specific to the
1998–2011 report era.

## Interactive-portal-only sources (not downloaded as files)

- `wahlen.mvnet.de` hosts an interactive HTML results browser for past elections
  (linked from the 1994/1998 result pages). These are HTML views of the same data
  already captured in the XLS files above; no additional bulk file download is offered.
- The next Landtagswahl is scheduled for 2026-09-20 (not yet held as of download date).
