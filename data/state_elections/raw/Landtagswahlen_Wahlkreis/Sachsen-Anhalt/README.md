# Sachsen-Anhalt — Landtagswahlen at Wahlkreis (constituency) level

**State:** Sachsen-Anhalt (ST, AGS state prefix `15`)
**Constituency unit:** Landtagswahlkreis — 49 in 1990; 49/45 in the 1990s; reduced over
time to **41 Wahlkreise** for the 2021 election.
**Primary source:** Statistisches Landesamt Sachsen-Anhalt / Landeswahlleiter,
election-results portal <https://wahlergebnisse.sachsen-anhalt.de/>

## What was downloaded

For each Landtagswahl the portal publishes a "Datei 2 — Endgültige Ergebnisse"
(for 2021 labelled "Datei 1") CSV that contains the final results **for the state as a
whole (Satzart `LAN`), for every Landtagswahlkreis (Satzart `WKR`), and for the
kreisfreie Städte / Landkreise (Satzart `KRS`)** in a single file. These are the
constituency-level files captured here. Each file is accompanied by its official
record-layout document (Datensatzbeschreibung, PDF), also downloaded for provenance.

The portal also offers Gemeinde-level (`*_GEM.csv`) and Wahlbezirk-level (`*_WBZ.xlsx`)
files; those are NOT constituency level and were not downloaded for this task.

All CSVs are semicolon-delimited, ISO-8859-1 (Latin-1) encoded, with CRLF line endings.
Columns are coded `A`=Wahlberechtigte, `B`=Wähler, `E`=ungültige Stimmen,
`F`=gültige Stimmen, then `F01…` per party (Zweitstimmen / Listenstimmen; older files
also carry Erststimmen blocks `C`/`D…`). See the matching Datensatzbeschreibung PDF for
the exact column map of each year.

| Year | File (CSV)                              | Source URL | Layout PDF |
|------|-----------------------------------------|------------|------------|
| 1990 | ST_1990_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt90/and/LT1990_LAN_KRS_WKR.csv | ST_1990_Datensatzbeschreibung.pdf |
| 1994 | ST_1994_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt94/and/LT1994_LAN_KRS_WKR.csv | ST_1994_Datensatzbeschreibung.pdf |
| 1998 | ST_1998_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt98/and/LT1998_LAN_KRS_WKR.csv | ST_1998_Datensatzbeschreibung.pdf |
| 2002 | ST_2002_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt02/and/LT2002_LAN_KRS_WKR.csv | ST_2002_Datensatzbeschreibung.pdf |
| 2006 | ST_2006_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt06/erg/csv/lt06dat2.csv | ST_2006_Datensatzbeschreibung.pdf |
| 2011 | ST_2011_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt11/erg/csv/lt11dat2.csv | ST_2011_Datensatzbeschreibung.pdf |
| 2016 | ST_2016_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt16/erg/csv/lt16dat2.csv | ST_2016_Datensatzbeschreibung.pdf |
| 2021 | ST_2021_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt21/erg/csv/lt21dat1.csv | ST_2021_Datensatzbeschreibung.pdf |

The per-year download landing pages are
`https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt{90,94,98,02,06,11,16,21}/and/lt.download.php`.

## Coverage

This is the **complete series** of Landtagswahlen for Sachsen-Anhalt since the state was
re-established in 1990: 1990, 1994, 1998, 2002, 2006, 2011, 2016, 2021. Every election
is available at constituency level in machine-readable CSV — no gaps.

## Years not downloadable

- **2026:** The next Landtagswahl is scheduled for 2026 and had not taken place as of the
  download date (2026-06-27), so no result file exists yet. When held, results will appear
  under `https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/` (preliminary pages already
  exist at `https://statistik.sachsen-anhalt.de/themen/gebiet-und-wahlen/wahlen/landtagswahl-2026`).

No historical (pre-1990) Landtagswahl exists for Sachsen-Anhalt — the state did not exist
as a Land between 1952 and 1990.

## Retrieval note

Downloaded 2026-06-27 with `curl` from the Statistisches Landesamt Sachsen-Anhalt portal.
All CSVs returned HTTP 200, `application/octet-stream`; all layout PDFs HTTP 200,
`application/pdf`. Raw files are stored verbatim.
