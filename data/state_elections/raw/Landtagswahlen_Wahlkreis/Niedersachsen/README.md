# Niedersachsen — Landtagswahl results at the constituency level

**State:** Niedersachsen (Lower Saxony), state code 03 / abbr. NI
**Constituency unit:** Wahlkreis (87 Wahlkreise since the 17th Wahlperiode; 100 Wahlkreise in 1998/2003 era)
**Compiled:** 2026-06-27
**Collector:** automated GERDA raw-data grab

## Primary sources

- Wahlergebnisse in Niedersachsen (LSN WahlServer portal): https://wahlen.statistik.niedersachsen.de/
- Landesamt für Statistik Niedersachsen (LSN) — Landtagswahlen, Statistische Berichte: https://www.statistik.niedersachsen.de/themen/Landtagswahlen-niedersachsen/landtagswahlen-in-niedersachsen-statistische-berichte-179042.html
- LSN — Landtagswahlen, Tabellen und Wahlkreiskarten: https://www.statistik.niedersachsen.de/themen/Landtagswahlen-niedersachsen/landtagswahlen-in-niedersachsen-tabellen-und-wahlkreiskarten-227429.html
- LSN-Online regional database (survey 143 = Landtagswahlen): https://www1.nls.niedersachsen.de/statistik/

Note: every LSN download endpoint (`/download/NNNNN`) requires a browser `User-Agent`
header; a bare `curl`/HEAD request returns HTTP 303 → an error page. Downloads here
were fetched with a desktop-Chrome UA string.

## Downloaded files (constituency-level)

The LSN WahlServer portal publishes, per recent election, a `wahlergebnis.csv` and a
`wahlergebnis.xml` at `https://wahlen.statistik.niedersachsen.de/LW<YEAR>/`. The **CSV is
exactly one row per Wahlkreis** (88 lines = header + 87 Wahlkreise) with Erststimmen and
Zweitstimmen per party plus Wahlberechtigte / Wähler / gültige Erst- u. Zweitstimmen.
The **XML** is far richer (per-Wahlkreis winner, all candidates, full breakdown). This
portal only covers 2013, 2017, 2022.

| File | Year | Format | Geo unit | Source URL |
|---|---|---|---|---|
| NI_2013_Landtagswahl_Wahlkreis.csv | 2013 | csv | Wahlkreis (87) | https://wahlen.statistik.niedersachsen.de/LW2013/wahlergebnis.csv |
| NI_2013_Landtagswahl_Wahlkreis.xml | 2013 | xml | Wahlkreis (87, candidate-level) | https://wahlen.statistik.niedersachsen.de/LW2013/wahlergebnis.xml |
| NI_2017_Landtagswahl_Wahlkreis.csv | 2017 | csv | Wahlkreis (87) | https://wahlen.statistik.niedersachsen.de/LW2017/wahlergebnis.csv |
| NI_2017_Landtagswahl_Wahlkreis.xml | 2017 | xml | Wahlkreis (87, candidate-level) | https://wahlen.statistik.niedersachsen.de/LW2017/wahlergebnis.xml |
| NI_2022_Landtagswahl_Wahlkreis.csv | 2022 | csv | Wahlkreis (87) | https://wahlen.statistik.niedersachsen.de/LW2022/wahlergebnis.csv |
| NI_2022_Landtagswahl_Wahlkreis.xml | 2022 | xml | Wahlkreis (87, candidate-level) | https://wahlen.statistik.niedersachsen.de/LW2022/wahlergebnis.xml |

### Statistische Berichte (PDF) — contain Wahlkreis tables (Endgültige Ergebnisse)

| File | Year | Format | Geo unit | Source URL |
|---|---|---|---|---|
| NI_2008_Landtagswahl_StatBericht.pdf | 2008 | pdf | Wahlkreis + Gemeinde | https://www.statistik.niedersachsen.de/download/58357 |
| NI_2013_Landtagswahl_StatBericht.pdf | 2013 | pdf | Wahlkreis + Gemeinde | https://www.statistik.niedersachsen.de/download/85619 |
| NI_2017_Landtagswahl_StatBericht.pdf | 2017 | pdf | Wahlkreis + Gemeinde | https://www.statistik.niedersachsen.de/download/155423 (korrigierte Version vom 07.07.2023) |

The 2008 PDF (B VII 2.2 / 2.3-j / 2.4) is the only directly-downloadable constituency-level
source for 2008; there is no machine-readable open-data CSV/XML for 2008 or earlier.

### Supplementary historical overviews (state-level, not Wahlkreis)

Included for completeness — these summarise every Landtagswahl since 1947 but are NOT
broken down by Wahlkreis.

| File | Coverage | Format | Source URL |
|---|---|---|---|
| NI_Landtagswahlen_Uebersicht_seit_1947.xlsx | all LW since 1947 (state totals) | xlsx | https://www.statistik.niedersachsen.de/download/77946 |
| NI_Landtagswahlen_Parteien_seit_1990.xlsx | party results since 1990 (state totals) | xlsx | https://www.statistik.niedersachsen.de/download/77950 |
| NI_Landtag_Sitzverteilung_seit_1947.xlsx | seat distribution since 1947 | xlsx | https://www.statistik.niedersachsen.de/download/77953 |
| NI_2022_Landtagswahl_Repraesentativstatistik.xlsx | 2022 representative statistics (age/gender) | xlsx | https://www.statistik.niedersachsen.de/download/209826 |

## Completeness-critic additions (2026-06-27) — 1998 + 2003 recovered

A second pass corrected an earlier error: the old NLS web portal pages for 1998 and
2003 are **NOT** limited to the four largest parties. Each per-Wahlkreis HTML page
(`001.htm`–`100.htm`) carries the **complete** result — every party with both
Erststimmen and Zweitstimmen, plus Wahlberechtigte / Wähler / Wahlbeteiligung /
ungültige + gültige Stimmen, and the directly-elected candidate. Verified by inspecting
several pages per year (e.g. 1998 WK 001 lists SPD, CDU, GRÜNE, FDP, PDS, DKP, DEUTSCHE
PARTEI, REPUBLIKANER, Feministische Partei DIE FRAUEN, ÖDP, PBC, Soziale
Fortschritts-Partei, STATT Partei; 2003 WK 001 lists SPD, CDU, GRÜNE, FDP, PDS,
REPUBLIKANER, ÖDP, PBC, Partei Rechtsstaatlicher Offensive, Sonstige).

All 100 Wahlkreis pages per year were harvested (constituency count was 100 in the
1998/2003 era) plus the statewide Gesamt-/Endergebnis page and the index page, and
bundled into one gzip-tar per year. Files are ISO-8859-1 (latin-1) HTML; convert
encoding and `tr '\r' '\n'` line endings before parsing.

| File | Year | Format | Geo unit | Source URL (base) |
|---|---|---|---|---|
| NI_2003_Landtagswahl_Wahlkreis.tar.gz | 2003 | tar.gz of 100 HTML + Gesamtergebnis + index | Wahlkreis (100, candidate-level, all parties) | https://www.nls.niedersachsen.de/LW2003/NNN.htm (NNN = 001–100) |
| NI_1998_Landtagswahl_Wahlkreis.tar.gz | 1998 | tar.gz of 100 HTML + Endergebnis + index | Wahlkreis (100, candidate-level, all parties) | http://www.nls.niedersachsen.de/Landtagswahl/NNN.htm (NNN = 001–100) |

Archive layout: `LW2003/001.htm … 100.htm`, `LW2003/Gesamtergebnis.htm`,
`LW2003/index.html`; `LW1998/001.htm … 100.htm`, `LW1998/Endergebnis.htm`,
`LW1998/index.htm`.

Still genuinely missing at constituency level: **1994 and earlier** (1947, 1951, 1955,
1959, 1963, 1967, 1970, 1974, 1978, 1982, 1986, 1990, 1994). These predate the NLS web
portal and are not in the statistische Bibliothek (statistischebibliothek.de digitised
the NI Landtag B VII 2 series only back to **2008** — series NISerie_mods_00000203 /
older NISerie_mods_00000183, whose only issues are 2008/2013/2017). They exist only as
printed Statistische Berichte (Reihe B VII 2, Heft 2) / Wahlhefte and inside the
interactive LSN-Online database (survey 143), neither of which offers a static
constituency-level file download.

## Years that exist but are NOT directly downloadable at constituency level

Niedersachsen has held Landtagswahlen in: 1947, 1951, 1955, 1959, 1963, 1967, 1970,
1974, 1978, 1982, 1986, 1990, 1994, 1998, 2003, 2008, 2013, 2017, 2022.

| Year | Reason not downloaded | Source hint |
|---|---|---|
| 2003 | No machine-readable file. Old NLS web portal (http://www.nls.niedersachsen.de/LW2003/000.htm) shows only the state-level Amtliches Endergebnis as HTML; per-Wahlkreis pages exist only inside the interactive portal. | LSN-Online survey 143; per-WK HTML under nls.niedersachsen.de/LW2003/ (only 4 largest parties); full microdata only via Wahl@statistik.niedersachsen.de |
| 1998 | Old NLS portal (http://www.nls.niedersachsen.de/Landtagswahl/Wahlen.htm) lists 100 Wahlkreis HTML pages (001–100) but only the four largest parties per WK; no CSV/XLS/PDF download. | LSN-Online survey 143; Statistische Berichte Reihe B VII 2, Heft 2 (1998) — print only; Wahlbezirksstatistik via Wahl@statistik.niedersachsen.de |
| 1994 | No directly downloadable constituency file. Published only as printed Statistische Berichte (Reihe B VII 2, Heft 2, 14. WP, 13.03.1994). | LSN-Online survey 143; printed Heft; LSN library |
| 1990 | No directly downloadable constituency file (pre-web Statistische Berichte). | LSN-Online survey 143; printed Statistische Berichte Reihe B VII 2 |
| 1947–1986 (1947, 1951, 1955, 1959, 1963, 1967, 1970, 1974, 1978, 1982, 1986) | Pre-digital. Only state-level totals are in the "Übersicht seit 1947" xlsx (included above); constituency-level results exist only in printed Statistische Berichte / Wahlhefte and the LSN-Online database. | LSN-Online survey 143 (constituency + municipality level "since 1947" per LSN); printed Hefte; wahlen-in-deutschland.de mirrors state totals |

### Notes on access

- The LSN-Online database (https://www1.nls.niedersachsen.de/statistik/, survey/Erhebung
  143 "Landtagswahlen") is, per LSN, the canonical source for Wahlkreis- and
  Gemeinde-level results "seit 1947", but it is an interactive query interface — there is
  no single static file to download. Table exports would require driving the portal UI.
- LSN explicitly states that Wahlbezirksstatistik (electoral-district microdata) datasets
  from 1998 onward are available free of charge **only on request** via
  Wahl@statistik.niedersachsen.de — i.e. not a public direct download.

## Second-attempt gap-fill pass (2026-06-27) — nothing new obtainable

A second, independent gap-fill pass re-checked all 1951–1994 missing years against the
sources a first pass might have skipped. **No new files were downloaded.** Findings:

- **statistischebibliothek.de SOLR re-query** (MyCoRe proxy `…/servlets/solr/select`):
  searched `mods.title:Landtag AND mods.title:Wahl` filtered to `id:NI*`, and
  `mods.title:"Statistik von Niedersachsen"` — the only NI Landtag-election series are
  `NISerie_mods_00000203` and `NISerie_mods_00000183`, whose children are exactly
  **2008, 2013, 2017** (NIHeft 00002299 / 00008919 = 2008, 00002467 = 2013, 00008918 =
  2017). No pre-2008 Heft, no catalogued "Statistik von Niedersachsen" Bd. 233/296/367
  (the printed 1974/1978/1982 volumes) exists in the digital library.
- **LSN B VII 2 Statistische Berichte page** and **Tabellen/Wahlkreiskarten page**: only
  2008+ PDFs and the state-level "seit 1947"/"seit 1990" xlsx overviews (already held).
- **Legacy NLS static tables** (live, HTTP 200):
  `http://www.nls.niedersachsen.de/Tabellen/Wahlen/Lwab1947.html` (Landtagswahlen
  1947–2008) and `…/M50003170.html` (1990–2008 by party) are **state-level totals only**,
  no Wahlkreis breakdown, and contain no links to per-year/per-WK detail tables.
- **wiesmoor.de** 1994 mirror (`/wahlen/lw1994/…`): now **404** (gone).
- **LSN-Online DB** (`www1.nls.niedersachsen.de/statistik/`): JS-driven app, no static
  fetch; per LSN's own statement microdata only "since 1998" and only on request.
- **Deutsche Digitale Bibliothek / archive.org**: no NI Landtag Wahlkreis result volume
  for any pre-2008 year (DDB returns only unrelated administrative/Kommunalwahl items).

Conclusion: 1951, 1955, 1959, 1963, 1967, 1970, 1974, 1978, 1982, 1986, 1994 remain
obtainable only as **printed** Statistische Berichte / "Statistik von Niedersachsen"
Wahlhefte, or via the interactive LSN-Online survey 143 (no static download). No public
constituency-level digital file exists.
