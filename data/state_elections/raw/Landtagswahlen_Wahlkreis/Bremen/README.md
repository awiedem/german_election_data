# Bremen (HB) — Bürgerschaftswahl (Landtag) — constituency-level raw files

**State:** Freie Hansestadt Bremen (Bundesland Bremen), state abbr **HB**, AGS prefix `04`.

**Election:** Wahl zur Bremischen Bürgerschaft (Landtag) — the state parliament. Held
on the same day in both cities of the two-city state (Stadtgemeinde Bremen `04011000`
and Stadtgemeinde Bremerhaven `04012000`).

## Constituency unit — IMPORTANT

**Bremen has NO classic single-member Wahlkreise.** The Bürgerschaft is elected from
**exactly two multi-member electoral constituencies = Wahlbereiche:**

- **Wahlbereich Bremen** (Stadtgemeinde Bremen, AGS `04011000`)
- **Wahlbereich Bremerhaven** (Stadtgemeinde Bremerhaven, AGS `04012000`)

So the "constituency level" for Bremen is the **Wahlbereich** (= the city). Below the
Wahlbereich, the statistical office additionally publishes finer geographic breakdowns:
**Stadtteile** and **Ortsteile** (and the rawest **Wahlbezirke** = polling districts).
This folder captures both the canonical Wahlbereich series and the finer Ortsteil/
Stadtteil/Wahlbezirk machine-readable files where the state publishes them.

## Primary sources (verify, authoritative)

- **wahlen.bremen.de** — Landeswahlleiter results portal. Historical Wahlbereich PDFs
  served from `https://www.wahlen.bremen.de/sixcms/media.php/13/...`.
- **wahlen-bremen.de** — votemanager results microsites (per election). OpenData CSV
  exports for 2015 and 2019 under `/Wahlen/<DATE>/<AGS>/html5/Open-Data-*.csv`.
- **statistik.bremen.de** — Statistisches Landesamt Bremen, "Statistische Mitteilungen"
  Hefte (full statistical reports with Wahlbereich + Ortsteil tables), served from
  `https://www.statistik.bremen.de/sixcms/media.php/13/...`.

## Files downloaded

### A. Historical Wahlbereich series — covers EVERY election 1947–2023 (the master file)

These three PDFs (from wahlen.bremen.de) contain Table-4-style series with one row per
Wahlbereich (Bremen, Bremerhaven) for every Bürgerschaftswahl 1947→2023: Wahlberechtigte,
Wähler, Briefwahl, gültige/ungültige Stimmzettel, and party votes (Anzahl + %).

| File | Source URL | Content |
|---|---|---|
| `HB_1947-2023_Buergerschaftswahl_Wahlbereiche_Stimmen.pdf` | wahlen.bremen.de `/sixcms/media.php/13/NEU Bürgerschaftswahlen (Landtag) im Land Bremen 1947 bis 2023 nach Wahlbereichen.pdf` | Votes/turnout per Wahlbereich, all 18 elections 1947–2023 |
| `HB_1947-2023_Buergerschaftswahl_Wahlbereiche_Sitzverteilung.pdf` | wahlen.bremen.de `/sixcms/media.php/13/NEU Sitzverteilung in der Bremischen Bürgerschaft (Landtag) im Land Bremen 1947 bis 2023 nach Wahlbereichen.pdf` | Seat distribution per Wahlbereich, 1947–2023 |
| `HB_1991-2023_Buergerschaftswahl_Wahlbereiche_Parteien.pdf` | wahlen.bremen.de `/sixcms/media.php/13/NEU Parteien und Wählervereinigungen ... 1991 bis 2023 ... nach Wahlbereichen.pdf` | Participating parties per Wahlbereich, 1991–2023 |

### B. Per-election machine-readable OpenData CSV (Wahlbezirk / Stadtteil / Ortsteil)

votemanager OpenData exports. Files are semicolon-delimited UTF-8. Geo-level encoded in
the file suffix and in the `gebiet-nr`/`gebiet-name` columns. Levels: **3** = Gemeinde
(whole Wahlbereich), **6** = Wahlbezirk (polling district, finest), **8** = Stadtteil,
**11** = Ortsteil. One file per city (Bremen `04011000`, Bremerhaven `04012000`).

Source base: `https://www.wahlen-bremen.de/Wahlen/<DATE>/<AGS>/html5/Open-Data-Buergerschaft-Bremen<PREFIX><LEVEL>.csv`
(2019 PREFIX=`22`, 2015 PREFIX=`13`).

| File | Year | City / AGS | Geo level |
|---|---|---|---|
| `HB_2019_Buergerschaftswahl_Bremen_ebene3.csv` | 2019 | Bremen 04011000 | Gemeinde (Wahlbereich) |
| `HB_2019_Buergerschaftswahl_Bremen_ebene6.csv` | 2019 | Bremen 04011000 | Wahlbezirk |
| `HB_2019_Buergerschaftswahl_Bremen_ebene8.csv` | 2019 | Bremen 04011000 | Stadtteil |
| `HB_2019_Buergerschaftswahl_Bremen_ebene11.csv` | 2019 | Bremen 04011000 | Ortsteil |
| `HB_2019_Buergerschaftswahl_Bremerhaven_ebene3.csv` | 2019 | Bremerhaven 04012000 | Gemeinde (Wahlbereich) |
| `HB_2019_Buergerschaftswahl_Bremerhaven_ebene6.csv` | 2019 | Bremerhaven 04012000 | Wahlbezirk |
| `HB_2019_Buergerschaftswahl_Bremerhaven_ebene8.csv` | 2019 | Bremerhaven 04012000 | Stadtteil |
| `HB_2019_Buergerschaftswahl_Bremerhaven_ebene11.csv` | 2019 | Bremerhaven 04012000 | Ortsteil |
| `HB_2015_Buergerschaftswahl_Bremen_ebene3.csv` | 2015 | Bremen 04011000 | Gemeinde (Wahlbereich) |
| `HB_2015_Buergerschaftswahl_Bremen_ebene6.csv` | 2015 | Bremen 04011000 | Wahlbezirk |
| `HB_2015_Buergerschaftswahl_Bremen_ebene8.csv` | 2015 | Bremen 04011000 | Stadtteil |
| `HB_2015_Buergerschaftswahl_Bremen_ebene11.csv` | 2015 | Bremen 04011000 | Ortsteil |
| `HB_2015_Buergerschaftswahl_Bremerhaven_ebene3.csv` | 2015 | Bremerhaven 04012000 | Gemeinde (Wahlbereich) |
| `HB_2015_Buergerschaftswahl_Bremerhaven_ebene6.csv` | 2015 | Bremerhaven 04012000 | Wahlbezirk |
| `HB_2015_Buergerschaftswahl_Bremerhaven_ebene8.csv` | 2015 | Bremerhaven 04012000 | Stadtteil |
| `HB_2015_Buergerschaftswahl_Bremerhaven_ebene11.csv` | 2015 | Bremerhaven 04012000 | Ortsteil |

### C. Full statistical reports (PDF) with Wahlbereich + Ortsteil tables, per election

"Statistische Mitteilungen" Hefte from statistik.bremen.de. Each covers ONE election and
contains detailed tables down to Ortsteil. Source base
`https://www.statistik.bremen.de/sixcms/media.php/13/`.

| File | Year | Heft | Source filename |
|---|---|---|---|
| `HB_2023_Buergerschaftswahl_Heft126_Bericht_Ortsteile.pdf` | 2023 | 126 (2. korr. Auflage) | `Statistische Mitteilungen_126_pdfa_Auflage2.pdf` |
| `HB_2019_Buergerschaftswahl_Heft123_Bericht_Ortsteile.pdf` | 2019 | 123 | `StatistischeMitteilungen_123_pdfa.pdf` |
| `HB_2015_Buergerschaftswahl_Heft119_Bericht_Ortsteile.pdf` | 2015 | 119 | `StatistischeMitteilungen_119.pdf` |
| `HB_2011_Buergerschaftswahl_Heft113_Teil1_Analysen_Tabellen.pdf` | 2011 | 113 Teil 1 | `Stat Mitt113_Lw11_Teil 1 Analysen und Tabellen.pdf` |
| `HB_2011_Buergerschaftswahl_Heft113_Teil2_Ortsteile.pdf` | 2011 | 113 Teil 2 | `Stat Mitt113_Lw11_Teil 2 Ergebnisse in den Ortsteilen.pdf` |
| `HB_2007_Buergerschaftswahl_Heft110_Bericht.pdf` | 2007 | 110 | `lw07_heft110.pdf` |
| `HB_2003_Buergerschaftswahl_Heft106_Bericht.pdf` | 2003 | 106 | `lw_2003.pdf` |

## Coverage summary

- **Wahlbereich level (the canonical Bremen constituency): ALL elections 1947–2023** are
  covered by the historical PDF series in section A (12.10.1947, 07.10.1951, 09.10.1955,
  11.10.1959, 29.09.1963, 01.10.1967, 10.10.1971, 28.09.1975, 07.10.1979, 25.09.1983,
  13.09.1987, 29.09.1991, 14.05.1995, 06.06.1999, 25.05.2003, 13.05.2007, 22.05.2011,
  10.05.2015, 26.05.2019, 14.05.2023).
- **Per-election detailed reports (down to Ortsteil): 2003, 2007, 2011, 2015, 2019, 2023.**
- **Machine-readable CSV (Wahlbezirk/Stadtteil/Ortsteil): 2015 and 2019 only.**

## Years that exist but are NOT downloadable as separate constituency files

These years have constituency (Wahlbereich) results ONLY inside the 1947–2023 historical
PDF (section A), not as a standalone machine-readable or per-election report file:

- **1947, 1951, 1955, 1959, 1963, 1967, 1971, 1975, 1979, 1983, 1987, 1991, 1995, 1999.**
  Reason: predate the statistical office's per-election PDF Hefte archive (online archive
  starts at Heft 106 / 2003) and the votemanager OpenData (starts 2015). Wahlbereich-level
  data IS captured (master PDF, section A). Finer Ortsteil-level historical detail for
  these years may exist only in print-era Hefte or the "Bremen Infosystem" database.
  Source hint: statistik.bremen.de "Bremen Infosystem" / "Bremen kleinräumig Infosystem"
  (long time series, CSV/Excel export) and the Statistik Bremen library.

## Notes / things checked but NOT a downloadable constituency file

- **2023 had NO votemanager OpenData CSV export.** The 2023 results microsite
  (`wahlen-bremen.de/Wahlen/2023_05_14/`) is a static SPA without an OpenData section
  (unlike 2015/2019). 2023 constituency/Ortsteil data is captured via the Heft 126 report
  PDF (section C) and the master Wahlbereich PDF (section A). Finer 2023 Ortsteil results
  are also browsable in the InstantAtlas "Bremer Wahlatlas" at
  `statistik-bremen.de/Tabellen/Wahlen/WahlatlasBuergerschaft2023_Ortsteile/` — an
  interactive viewer, no direct flat-file download.
- **Bremerhaven "Stadtverordnetenversammlung" files were intentionally excluded** — that
  is the Bremerhaven city-council election, a DIFFERENT election from the Bürgerschaft.
- The "Bremen Infosystem" / "Bremen kleinräumig Infosystem" databases
  (statistik.bremen.de → Datenangebote) offer CSV/Excel export of long time series but
  only through an interactive query UI, not stable direct download URLs.

All files verified with `curl -sIL` (HTTP 200, application/pdf or text/csv) and confirmed
on disk with `ls`/`file`. Downloaded 2026-06-27.

---

## Completeness-critic additions (2026-06-27, second pass)

### D. Machine-readable 2023 results (InstantAtlas underlying data)

The first pass reported the 2023 InstantAtlas "Bremer Wahlatlas" as "interactive viewer,
no direct flat-file download." That is incorrect: the InstantAtlas report ships its full
underlying dataset as a single JSON file (`data.js`, served with HTTP 200,
`application/javascript`). It contains the **2023 Bürgerschaftswahl results at three
geographic levels** — 113 Ortsteile, 32 Stadtteile, 7 Stadtbezirke — each carrying the
Wahlbereich filter (Bremen vs Bremerhaven), with indicators for Wahlbeteiligung 2023,
Briefwahlanteil 2023, and party vote shares (CDU, SPD, …). This is the machine-readable
2023 constituency-level export the first pass said did not exist.

| File | Year | Source URL | Content |
|---|---|---|---|
| `HB_2023_Buergerschaftswahl_Wahlkreis_Ortsteile_data.js` | 2023 | `https://www.statistik-bremen.de/Tabellen/Wahlen/WahlatlasBuergerschaft2023_Ortsteile/data.js` | InstantAtlas JSON (version 1.3): Ortsteile/Stadtteile/Stadtbezirke results with Bremen/Bremerhaven Wahlbereich filters, turnout + Briefwahl + party shares. ~93 KB, valid JSON (UTF-8 BOM). |

Note: the file is named `data.js` upstream but the payload is a single JSON object (parses
cleanly with `json.load(open(f, encoding='utf-8-sig'))`).

### Re-checks that confirmed the first pass (no new files)

- **2023 votemanager OpenData CSV** — re-probed `wahlen-bremen.de/Wahlen/2023_05_14/...`
  under multiple path/prefix guesses (`html5/OpenDataInfo.html`, `praesentation/opendata.html`,
  `Open-Data-Buergerschaft-Bremen{14,23}_{3,11}.csv`): all HTTP 404. Confirmed: no
  votemanager OpenData CSV for 2023. (The 2021 microsite *does* return 200 for its
  opendata page, but 2021 was a Beirätewahl, not a Bürgerschaftswahl — out of scope.)
- **Pre-2003 per-election Hefte** — the Statistische Mitteilungen publications archive
  (`statistik.bremen.de/publikationen/statistische-mitteilungen-2059`) genuinely starts
  at Heft 106 (2003). No digitized per-election report for 1999/1995/1991 or earlier.
- **Bremen kleinräumig Infosystem** — query UI only (`statwizard_step*.cfm`); no stable
  direct CSV/Excel export URL reachable without session state. Has Bürgerschaft data back
  to 1951 but not as a fetchable flat file.
- **Older years (1947–1999)** — Wahlbereich-level results remain captured ONLY in the
  master 1947–2023 PDFs (section A). Confirmed no standalone per-election file exists for
  these years at any source.

Added file verified with `curl -sIL` (HTTP 200, `application/javascript`) and `python3
json.load` parse check; confirmed on disk with `ls -la` / `file`.
