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
| 2026 | ST_2026_Landtagswahl_Wahlkreis.csv | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/downloads/Ergebnisse_Land_RKR_WKR_LT_2026.csv | ST_2026_Datensatzbeschreibung.pdf |

**2026** (Landtagswahl 6 September 2026; endgültiges Ergebnis, Landeswahlausschuss 22.09.2026;
downloaded 2026-09-25 from <https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/downloads.html>)
uses a new layout: **UTF-8**, every field quoted, headers `A.Wahlberechtigte` / `F01.CDU`,
and THREE rows per unit keyed by `Wahllokal` (`U` Urne, `B` Brief, empty = total). Only the
total rows are parsed; U + B = total holds for every cell. 41 Wahlkreise, numbered `001`…
(normalised to `01`…); all 41 sum exactly to the Land row for every party. Same numbers
and names as 2021; only WK 07 and WK 08 changed territory (the Gemeinde Niedere Börde
moved from WK 08 to WK 07). WK 35's −9 % electorate is demographic, not a re-cut. See
"Wahlkreiseinteilung 2021 → 2026" below and `flag_wkr_changed_since_prev`.

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

## Wahlkreiseinteilung 2021 → 2026: which Wahlkreise changed

Checked 2026-09-25. **Only WK 07 Haldensleben and WK 08 Wolmirstedt changed territory.**
The Gemeinde **Niedere Börde** (AGS 15083390, Landkreis Börde) moved from WK 08 to WK 07.
It had 5,815 Wahlberechtigte in 2021 and 5,525 in 2026. The other 39 Wahlkreise, including
all split cities, cover the same area in both years. In `ltw_wkr_unharm` this is
`flag_wkr_changed_since_prev` = 1 for ST 2026 WK 07 and 08, and 0 for the other 39.

**Legal basis.** The Anlage zu § 10 Abs. 1 Satz 3 LWG (GVBl. LSA 2019 S. 935, in force for
2021) was amended by § 1 Nr. 7 of the *Achtes Gesetz zur Änderung des Wahlgesetzes des
Landes Sachsen-Anhalt* of 7 February 2025 (GVBl. LSA S. 316, in force 21 February 2025).
Sources for the citation: the Landeswahlleiterin's Bekanntmachung "Vorbereitung und
Durchführung der Landtagswahl am 6. September 2026" (1 April 2026, section 2.1), and the
Stadt Halle's page "Wahlkreise und Wahlbezirke zur Landtagswahl 2026".

**How it was established.** Four independent official sources agree:

1. **Legal text, unit by unit.** The 2026 Anlage (Stand 21.02.2025) was diffed against the
   StaLA's 2021 per-Wahlkreis descriptions
   (`https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt21/wahlkreiseinteilung/gemeinden/lwg.NN.chart.php`,
   NN = 01…41). Every Wahlkreis lists the same Gemeinden, Stadtteile, Stadtviertel and
   Ortsteile in both years, except that *Niedere Börde* moves from the WK 08 list to the
   WK 07 list. The Wahlkreis names are unchanged.
2. **Per-Gemeinde assignment, 2021 vs 2026 Wahlbezirk files.** All 218 Gemeinden are the
   same in both years and none was merged. 213 lie wholly in one Wahlkreis in both years,
   and all of these keep it except Niedere Börde (08 → 07). Five Gemeinden are split, and
   there the check was made at Wahlbezirk level:
   - Magdeburg (WK 10–13): 158 → 148 Urnenwahlbezirke. The 148 that exist in both years
     keep their Wahlkreis. Each Stadtteil-coded number prefix maps to the same Wahlkreis in
     both years.
   - Halle (Saale) (WK 35–38): all 126 Urnenwahlbezirke keep their Stadtviertel-coded
     number and their Wahlkreis.
   - Dessau-Roßlau (WK 26/27): 55 → 57 Urnenwahlbezirke. All 55 keep their Wahlkreis. The
     two new ones (Brambach, Streetz/Natho) are in WK 27, whose legal list already had them.
   - Leuna (WK 33/34) and Petersberg (WK 29/34): the same Ortsteile sit in the same
     Wahlkreis in both years. Petersberg's Ortsteil Brachstedt and Leuna's ten Ortsteile
     Friedensdorf … Zweimen are in WK 34; the rest of each town is in WK 29 or WK 33.
3. **StaLA assignment workbook 2026.** The workbook "Zuordnung der Gemeinden zu den
   Landtags- und Bundestagswahlkreisen" agrees with the 2026 Wahlbezirk file for all 218
   Gemeinden.
4. **StaLA Vergleichstabellen 2026, sheet "Gewinn-Verlust Wahlkreisebene".** The "Vorjahr"
   (2021) Zweitstimmen equal the original 2021 Wahlkreis results exactly in 39 of 41
   Wahlkreise, for CDU, AfD, Die Linke, SPD, GRÜNE and FDP. In WK 07 and WK 08 they equal
   2021 *recomputed with Niedere Börde moved*, to the vote. For example, CDU WK 07 is
   10,122 = 8,689 + 1,433, and WK 08 is 10,416 = 11,849 − 1,433. So the StaLA itself compares
   2026 against 2021 on 2026 boundaries, and only these two Wahlkreise needed recomputing.

**What the electorate swings mean.** From 2021 to 2026 the electorate falls in almost every
Wahlkreis (range −9.1 % to −0.6 % over the 39 unchanged ones; −4.6 % statewide).
- WK 07 (+10.0 %) and WK 08 (−12.7 %) are the Niedere Börde transfer. On 2026 boundaries
  their change is −4.7 % and −1.0 %.
- **WK 35 Halle I (−9.1 %) was not re-cut.** Its Stadtviertel list is identical in both years
  (Halle-Neustadt, Heide-Nord/Blumenau, Dölau, Nietleben, Lettin …), and its 126-district
  Wahlbezirk map is unchanged. Its electorate simply shrank more than Halle's (−3.8 %).
- WK 36/37 (−0.6 % / −1.0 %) likewise kept their areas.

**Files kept for this check** (downloaded 2026-09-25, stored verbatim):

| File | Source |
|------|--------|
| `ST_2026_Wahlkreiseinteilung_Anlage_LWG.pdf` | https://wahlen.sachsen-anhalt.de/fileadmin/Bibliothek/Politik_und_Verwaltung/MI/wahlen/PDF/2026_Wahlkreiseinteilung_fuer_Landtagswahlen_in_Sachsen_Anhalt.pdf |
| `ST_2026_Wahlkreise_Gemeinden_Zuordnung.xlsx` | https://statistik.sachsen-anhalt.de/fileadmin/Bibliothek/Landesaemter/StaLa/startseite/Themen/Wahlen/Wahlkreise/Landtagswahl_2026_-_Wahlkreise___Gemeinden.xlsx |
| `ST_2026_Vergleichstabellen.xlsx` | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/downloads/Vergleichstabellen_LT_2026.xlsx (openpyxl chokes on a missing drawing part; `readxl` / pandas read it) |
| `ST_2026_Ergebnisse_Wahlbezirke.csv` | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/downloads/Ergebnisse_WBZ_LT_2026.csv (same layout as the Wahlkreis file: UTF-8, quoted, `Wahllokal` U/B per Wahlbezirk) |
| `ST_2021_Ergebnisse_Wahlbezirke.xlsx` | https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt21/erg/csv/LT2021_WBZ.xlsx (header in rows 5–7; Wahlkreis in col 1, AGS in col 5, Wahllokalart U/B in col 11) |

**Earlier ST years were not assessed** (`flag_wkr_changed_since_prev` is NA for 1990–2021).
The same method would work, because every year 1990–2021 publishes an `LT<YYYY>_WBZ.xlsx` with
the Wahlkreis per Wahlbezirk (`…/wahlen/lt<yy>/and/lt.download.php`). It is not a quick
job, though. The number of Wahlkreise went 49 → 45 (2006) → 43 (2016) → 41 (2021), so most
numbers change meaning at those elections. And the 2009–2011 Gemeindegebietsreform (about
1,000 → 218 Gemeinden) means per-Gemeinde assignments must be matched through a crosswalk.
