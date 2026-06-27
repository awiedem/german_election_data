# Brandenburg — Landtagswahl results at constituency (Wahlkreis) level

- **State:** Brandenburg (BB), state code `12`
- **Constituency unit:** Wahlkreis (44 Landtagswahlkreise; each elects one direct
  mandate by Erststimme, plus a Zweitstimme/Landesstimme list vote).
- **Coverage:** all 8 Landtagswahlen 1990, 1994, 1999, 2004, 2009, 2014, 2019, 2024 —
  **complete**, machine-readable for every year.
- **Downloaded:** 2026-06-27

## Primary sources

1. **Der Landeswahlleiter Brandenburg — Wahlergebnisse portal**
   (`https://wahlergebnisse.brandenburg.de/`). Hosts standardized open-data XLSX
   exports for every Landtagswahl 1990–2024 under
   `…/wahlen/publikationen/dowmies/DL_BB_LT<year>.xlsx`. These are the votemanager-style
   "Endgültiges Ergebnis nach Wahlkreisen" exports (one row per Wahlkreis × Erst-/Zweitstimme).
2. **Amt für Statistik Berlin-Brandenburg** (`https://www.statistik-berlin-brandenburg.de/landtagswahlen-brandenburg`)
   — direct XLSX downloads for the most recent elections and the long time series.
3. **Statistische Bibliothek der Statistischen Ämter** (`https://www.statistischebibliothek.de`),
   series `BBSerie_mods_00000816` = "Statistischer Bericht B VII 2-3 — Landtagswahl im Land
   Brandenburg, Endgültiges Ergebnis" — the official per-year Statistische Berichte (PDF)
   with full Wahlkreis tables (and candidate names / direct-mandate winners). Enumerated via
   the MyCoRe SOLR proxy `…/mir/servlets/solr/select?q=Landtagswahl+Brandenburg&wt=json`.

## Files

### Machine-readable (preferred) — one Wahlkreis result table per year

| File | Year | Format | Geo unit | Source URL |
|---|---|---|---|---|
| `BB_1990_Landtagswahl_Wahlkreis.xlsx` | 1990 | xlsx | Wahlkreis (Erst-/Zweitstimme sheets) | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT1990.xlsx |
| `BB_1994_Landtagswahl_Wahlkreis.xlsx` | 1994 | xlsx | Wahlkreis | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT1994.xlsx |
| `BB_1999_Landtagswahl_Wahlkreis.xlsx` | 1999 | xlsx | Wahlkreis | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT1999.xlsx |
| `BB_2004_Landtagswahl_Wahlkreis.xlsx` | 2004 | xlsx | Wahlkreis | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT2004.xlsx |
| `BB_2009_Landtagswahl_Wahlkreis.xlsx` | 2009 | xlsx | Wahlkreis | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT2009.xlsx |
| `BB_2014_Landtagswahl_Wahlkreis.xlsx` | 2014 | xlsx | Wahlkreis | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT2014.xlsx |
| `BB_2019_Landtagswahl_Wahlkreis.xlsx` | 2019 | xlsx | Wahlbezirk + Wahlkreis (sheets `..._W_1/W_2`) | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_LT2019.xlsx |
| `BB_2024_Landtagswahl_Wahlkreis.xlsx` | 2024 | xlsx | Wahlbezirk + Wahlkreis (sheets `..._W_1/W_2`) | https://download.statistik-berlin-brandenburg.de/762c4e1afe5b8f24/ad8d9ed95644/DL_BB_LT2024.xlsx |

Supplementary aggregate workbooks (results by Landkreis / kreisfreie Stadt / Gemeinde — also
carry Wahlkreis-level rows):

| File | Year | Source URL |
|---|---|---|
| `BB_2019_Landtagswahl_Kreise_Gemeinden.xlsx` | 2019 | https://wahlergebnisse.brandenburg.de/wahlen/publikationen/dowmies/DL_BB_2_LT2019.xlsx |
| `BB_2024_Landtagswahl_Kreise_Gemeinden.xlsx` | 2024 | https://download.statistik-berlin-brandenburg.de/24337cfdd3a6b8a6/ae7033fb6270/DL_BB_2_LT2024.xlsx |
| `BB_1990-2024_Landtagswahlen_Lange-Reihe.xlsx` (state-total long time series 1990–2024, not per-Wahlkreis) | — | https://download.statistik-berlin-brandenburg.de/6514e98b4ef3d2ac/e51455dd7b44/Landtagswahlen_Lange-Reihe_2024_Brandenburg.xlsx |

### Statistische Berichte (PDF) — official per-year reports with Wahlkreis tables + candidates

All from Statistische Bibliothek series `BBSerie_mods_00000816` (B VII 2-3), via
`https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/<derivate>/<file>`.

| File | Year | Notes |
|---|---|---|
| `BB_1990_Landtagswahl_Wahlkreis_StatBericht.pdf` | 1990 | derivate 00018153, scanned (image PDF, ~20 MB) — needs OCR |
| `BB_1994_Landtagswahl_Wahlkreis_StatBericht.pdf` | 1994 | derivate 00011849, scanned (no text layer) — needs OCR |
| `BB_1999_Landtagswahl_Wahlkreis_StatBericht.pdf` | 1999 | derivate 00011852, scanned/image — needs OCR |
| `BB_2004_Landtagswahl_Wahlkreis_StatBericht.pdf` | 2004 | derivate 00011853, has text layer |
| `BB_2009_Landtagswahl_Wahlkreis_StatBericht.pdf` | 2009 | derivate 00011855 |
| `BB_2014_Landtagswahl_Wahlkreis_StatBericht.pdf` | 2014 | derivate 00005974 |
| `BB_2019_Landtagswahl_Wahlkreis_StatBericht.pdf` | 2019 | derivate 00020261, has text layer (Wahlkreis 01..44 confirmed) |
| `BB_2024_Landtagswahl_Wahlkreis_StatBericht.pdf` | 2024 | https://download.statistik-berlin-brandenburg.de/5df95b54442a1194/610e9fd15db2/SB_B07-02-03_2024j05_BB.pdf |

## Notes

- The XLSX series (`DL_BB_LT<year>.xlsx`) provides per-Wahlkreis Erststimme and Zweitstimme
  tables for **every** election year and is the preferred input for the pipeline. The PDF
  Statistische Berichte are kept as the authoritative source-of-record and for data the XLSX
  omits (candidate/direct-mandate names, Repräsentative Wahlstatistik).
- 1990/1994/1999 PDFs are scanned images without a usable text layer; the corresponding
  XLSX files are the practical machine-readable source for those years.

## Years missing

None. All Brandenburg Landtagswahlen (1990, 1994, 1999, 2004, 2009, 2014, 2019, 2024)
were downloaded at the Wahlkreis level in machine-readable XLSX. Brandenburg was created
in 1990; there are no earlier state elections.
