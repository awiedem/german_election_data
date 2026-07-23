# Nordrhein-Westfalen — Landtagswahlen at Wahlkreis level (raw source files)

- **State:** Nordrhein-Westfalen (NRW), state abbreviation `NW`
- **Constituency unit:** Wahlkreis (Landtagswahlkreis). NRW currently has 128 Wahlkreise.
  Since 2010 each Wahlkreis reports Erststimmen (Direktstimme) and Zweitstimmen (Listenstimme);
  before 2010 the NRW Landtagswahl used a single-vote system (Erststimme only), so the
  pre-2010 files carry one set of votes per party.
- **Compiled:** 2026-06-27. Raw files only — stored verbatim, never edited.

## Primary sources

| Source | URL |
|---|---|
| Landeswahlleiterin NRW — election results portal (current) | https://www.wahlergebnisse.nrw/ |
| Landeswahlleiterin NRW — archived portal (2000, 2005, older) | https://alt.wahlergebnisse.nrw.de/ |
| Open.NRW / CKAN open-data dataset "Landtagswahlen" | https://ckan.open.nrw.de/dataset/landtagswahlen_1568203949 |
| Statistik.NRW / IT.NRW — Statistische Berichte (PDF) | https://statistik.nrw/service/veroeffentlichungen/statistische-berichte |
| Statistische Bibliothek (MyCoRe archive, historical Hefte) | https://www.statistischebibliothek.de/ |

License of the open-data files: **Datenlizenz Deutschland Namensnennung 2.0** (dl-de/by-2-0).

## Downloaded files

### Machine-readable Wahlkreis result files (open data, one row per Wahlkreis)

| File | Year | Format | Geo unit | Source URL |
|---|---|---|---|---|
| `NW_2022_Landtagswahl_Wahlkreis.txt` | 2022 | CSV (`;`-sep, UTF-8) | Wahlkreis | https://www.wahlergebnisse.nrw/landtagswahlen/2022/LW22_WK_insgesamt.txt |
| `NW_2017_Landtagswahl_Wahlkreis.txt` | 2017 | CSV (`;`-sep, UTF-8) | Wahlkreis | https://www.wahlergebnisse.nrw/landtagswahlen/2017/LW17_WK_insgesamt.txt |
| `NW_2012_Landtagswahl_Wahlkreis.txt` | 2012 | CSV (`;`-sep, ISO-8859-1) | Wahlkreis | https://www.wahlergebnisse.nrw/landtagswahlen/2012/txtdateien/a136lw1200.txt |
| `NW_2010_Landtagswahl_Wahlkreis.txt` | 2010 | CSV (`;`-sep, ISO-8859-1) | Wahlkreis | https://www.wahlergebnisse.nrw/landtagswahlen/2010/aktuell/txtdateien/a136lw1000.txt |
| `NW_2005_Landtagswahl_Wahlkreis.txt` | 2005 | CSV (`;`-sep, UTF-16 LE) | Wahlkreis | https://alt.wahlergebnisse.nrw.de/landtagswahlen/2005/lwahl/txtdateien/a135lw0500.txt |
| `NW_2000_Landtagswahl_Wahlkreis.txt` | 2000 | CSV (`;`-sep, ISO-8859-1) | Wahlkreis + Landesergebnis | https://alt.wahlergebnisse.nrw.de/landtagswahlen/2000/wahlkr/lw_gesamt1.txt |

Each `.txt` is a semicolon-delimited table with one line per Wahlkreis (plus a header line, a
statewide total row and — for some years — Regierungsbezirk subtotals): Wahlkreis number and
name, Wahlberechtigte, Wähler/-innen, ungültige and gültige Stimmen, and votes per party.
2010/2012/2017/2022 carry both Erst- and Zweitstimmen columns; 2000/2005 carry the single
Stimme of the old NRW one-vote system. Note the differing encodings (the 2005 file is UTF-16 LE).

### Statistical-report PDFs (constituency tables for years with no machine-readable file)

| File | Year | Format | Geo unit | Source URL |
|---|---|---|---|---|
| `NW_1962_Landtagswahl_Beitrag170_StatBericht.pdf` | 1962 | PDF | Wahlkreis (+ Gemeinde/Kreis) | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/NWHeft_derivate_00018794/Beitrag%20170_A.pdf |
| `NW_1958_Landtagswahl_Beitrag97_StatBericht.pdf` | 1958 | PDF | Wahlkreis (+ Gemeinde/Kreis) | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/NWHeft_derivate_00018792/Beitrag%2097_A.pdf |
| `NW_1954_Landtagswahl_Beitrag47_StatBericht.pdf` | 1954 | PDF | Wahlkreis (+ Gemeinde/Kreis) | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/NWHeft_derivate_00018793/Beitrag%2047_A.pdf |

These three are the scanned "Beiträge zur Statistik des Landes Nordrhein-Westfalen" Hefte
47 (Landtagswahl 27.06.1954), 97 (06.07.1958) and 170 (08.07.1962). They contain the full
official results down to Wahlkreis (and below) and are the earliest NRW Landtagswahl reports
digitised in the Statistische Bibliothek. They require OCR / visual reading to extract.

### Supplementary current-year PDFs (2022)

| File | Year | Format | Geo unit | Source URL |
|---|---|---|---|---|
| `NW_2022_Landtagswahl_Heft4_Wahlkreise_Gemeinden_StatBericht.pdf` | 2022 | PDF | Wahlkreis + Gemeinde | https://www.statistik.nrw/system/files/statistic_reports/b80.pdf |
| `NW_2022_Landtagswahl_Heft1_Ergebnisse_frueherer_Wahlen_StatBericht.pdf` | 2022 (covers 1947→) | PDF | state / time series | https://www.statistik.nrw/system/files/statistic_reports/b77.pdf |

`Heft 4` (b80) is the official 2022 Statistischer Bericht broken down by Wahlkreis **and**
Gemeinde — a richer, human-readable companion to the 2022 open-data CSV. `Heft 1` (b77)
"Ergebnisse früherer Wahlen" is a statewide historical time series (1947 onward) included as
a convenient reference for the years that are not available at constituency level (see below).

## Years that exist but were NOT downloadable at Wahlkreis level

NRW Landtagswahlen were held in 1947, 1950, 1954, 1958, 1962, 1966, 1970, 1975, 1980, 1985,
1990, 1995, 2000, 2005, 2010, 2012, 2017, 2022. Constituency-level files obtained above cover
1954, 1958, 1962, 2000, 2005, 2010, 2012, 2017, 2022. The remaining elections:

| Year | Reason not downloaded | Where to find it |
|---|---|---|
| 1947 | No machine-readable file and no constituency-level PDF digitised. The first NRW Landtag (20.04.1947) was elected partly via Wahlkreise; statewide/Kreis figures only are online. | Statewide series in `…Heft1…StatBericht.pdf` (b77); original report only in IT.NRW print archive / Landtag NRW historical pages (https://www.landtag.nrw.de/.../wahlergebnisse-im-ruckblick). |
| 1950 | Same — only statewide/Kreis HTML tables on alt.wahlergebnisse.nrw.de; no Wahlkreis CSV/PDF directly downloadable. | https://alt.wahlergebnisse.nrw.de/landtagswahlen/1950/index.html (state level); print Heft via IT.NRW webshop. |
| 1966, 1970, 1975, 1980, 1985, 1990, 1995 | No open-data CSV (the open-data series starts at 2000) and the corresponding "Beiträge zur Statistik" / Statistischer-Bericht Hefte for these years are **not** digitised in the Statistische Bibliothek (only 1954/1958/1962 carry descriptive Landtagswahl titles there). | IT.NRW webshop (https://webshop.it.nrw.de / https://statistik.nrw/.../statistische-berichte, "Ältere Ausgaben" → Statistische Bibliothek); the statewide figures are in `…Heft1…StatBericht.pdf` (b77) and at https://alt.wahlergebnisse.nrw.de/. The compendium *„40 Jahre Wahlen in Nordrhein-Westfalen 1947–1987"* (IT.NRW, 1987) covers these years but was not found as a direct PDF link. |

### Notes / verification

- All URLs were verified with `curl -sIL` (HTTP 200, correct content-type) before download;
  every file was re-checked on disk with `ls`/`file`. The three historical PDFs match their
  server Content-Length exactly (57 402 053 / 47 344 980 / 27 707 313 bytes).
- The live portal uses `www.wahlergebnisse.nrw` (no `.de`) for 2010–2022; the older 2000/2005
  files live on `alt.wahlergebnisse.nrw.de`. The `…nrw.de` host for 2010–2022 returns errors —
  use the `.nrw` host.
- Interactive map views (e.g. `http://karten.wahlergebnisse.nrw/?ltw_2022`) and the
  Landesdatenbank NRW (https://www.landesdatenbank.nrw.de/) offer the same data through a
  query portal but not as a single bulk constituency file beyond what is captured here.

## Completeness re-audit (2026-06-27, second pass)

A dedicated completeness pass re-attempted every missing year through additional
channels. No new **Wahlkreis-level** file was obtainable; nothing was added. Findings:

- **Statistische Bibliothek (MyCoRe SOLR, `…/servlets/solr/select`)** — exhaustively
  queried. The NW "Beiträge zur Statistik" series `NWSerie_mods_00000239` (841 issues)
  and the Landtagswahl series `NWSerie_mods_00000860` / `…_00000331` (Heft 4) were walked.
  Only **three** historical Landtagswahl Hefte are digitised: Heft 47 (1954), Heft 97
  (1958), Heft 170 (1962) — all already held. Full-text phrase searches for each
  election day (`"10. Juli 1966"`, `"14. Juni 1970"`, `"4. Mai 1975"`, `"11. Mai 1980"`,
  `"12. Mai 1985"`, `"13. Mai 1990"`, `"14. Mai 1995"`) restricted to `id:NWHeft*`
  returned **0** hits each → no 1966–1995 report exists in the library under any title.
  No 1947/1950 first-Landtag report is digitised either.
- **alt.wahlergebnisse.nrw.de** — only the `/landtagswahlen/1950/` landing page exists
  (HTTP 200); it carries **statewide totals only** and its `wahlkr/ kreise/ gemeinde/`
  subdirs all 404. 1947 and every year 1966–1995 return 404. No Wahlkreis file there.
- **wahlergebnisse.nrw (current portal)** — `/landtagswahlen/{1966..1995}/` all 404;
  open-data series genuinely begins at 2000.
- **Open.NRW / CKAN `landtagswahlen_1568203949`** — no resource pre-2000.
- **Landesdatenbank NRW** statistic `14311 Allgemeine Landtagswahlstatistik`: historical
  table `14311-11i` covers **04.05.1975 – 15.05.2022 but only at Gemeinde level**, behind
  an interactive table-builder (no stable bulk URL, not Wahlkreis). 1966/1970 absent.
- **regionalstatistik.de (Regionaldatenbank)** NRW Landtagswahl tables `14335-01-03-4`
  (Kreise/krfr. Städte, **14.05.1995**–2022) and `14335-01-03-5` (Gemeinde, 2010–2017).
  Neither is Wahlkreis level and neither reaches before 1995.

**Conclusion:** at the Wahlkreis level NRW coverage is complete w.r.t. all publicly
digitised sources — 1954, 1958, 1962 (PDF) and 2000, 2005, 2010, 2012, 2017, 2022 (CSV).
The gap years 1947, 1950, 1966, 1970, 1975, 1980, 1985, 1990, 1995 have **no** Wahlkreis
file online; for 1975–1995 only sub-Wahlkreis (Gemeinde/Kreis) aggregates exist in the
Landesdatenbank/Regionaldatenbank query portals, and the original Statistische Berichte
are available only as print via the IT.NRW webshop. Earliest constituency-level year held
remains **1954** (vs. theoretical earliest 1947).

## Gap-fill (June 2026)

A dedicated gap-fill run targeted the still-missing years **1947, 1950, 1966, 1970,
1975, 1980, 1985, 1990, 1995** and independently re-verified every lever.
**No new Wahlkreis-level file was obtainable; nothing was added.** Findings of this run:

- **statistischebibliothek.de (MyCoRe SOLR + MODS object XML)** — walked every NW
  election series via the `?XSL.Style=xml` object view: the three dedicated Landtagswahl
  Statistische-Berichte series `NWSerie_mods_00000325 / …327 / …329 / …331 / …333` plus
  the older-style series `NWSerie_mods_00000860` ("Die Landtagswahl am … in
  Nordrhein-Westfalen"). The 325/327/329/331/333 series each hold only **2010, 2012,
  2017, 2022**; series 860 holds only **1954, 1958, 1962** (= the already-held Beiträge
  Hefte 47/97/170). A MODS-title search `mods.title:"Landtagswahl"` across all
  `NWHeft_mods*` + `NWMonografie_mods*` returns **exactly 3 hits** — the 1954/1958/1962
  Beiträge. Title searches for `"Wahl zum Landtag"`, `"Abgeordneten des Landtags"`,
  `"Landtag … Wahlkreis"` return 0. Full-text (`content:`) searches for each gap-year
  election day ("10. Juli 1966", "14. Juni 1970", "4. Mai 1975", "11. Mai 1980",
  "12. Mai 1985", "13. Mai 1990", "14. Mai 1995", "20. April 1947", "18. Juni 1950")
  combined with "Wahlkreis" return **0** in NW Hefte. The gap dates DO occur in the NRW
  *Statistisches Jahrbuch* derivates (`Z029 …_A.pdf`) and in cross-state compendia
  (`ltw_erg_gesamt.pdf`, Destatis "40 Jahre Wahlen in der BRD"), but only as
  statewide/Regierungsbezirk summary tables, never a per-Wahlkreis breakdown.
- **"40 Jahre Wahlen in Nordrhein-Westfalen" (LDS NRW, 1987 — `NWMonografie_mods_00000003`,
  derivate `…29`, `B989 198700_A.pdf`, 329 MB / 158 pp.)** and **"50 Jahre Wahlen in
  NRW 1919–1968" (Beiträge Heft 244 — `NWMonografie_mods_00000005`, `B999 196800_A.pdf`,
  276 MB)** — these official compendia *do* span the gap years, so each was fetched and
  visually inspected. Both are **analytical** publications (Wählerstrukturen by
  Alter/Geschlecht, regional Gebietstyp comparisons: Ruhrgebiet / Rheinisches
  Verdichtungsgebiet / ländliche Gebiete, Stadt-Land, Gebietstypen). The Inhaltsverzeichnis
  and the Anhang-Tabellen confirm the finest geographic unit is Regierungsbezirk /
  Kreis / Gebietstyp — **not** the individual Landtagswahlkreis. They therefore do **not**
  satisfy the one-row-per-Wahlkreis requirement and were **not** added.
- **wahlergebnisse.nrw** (current) and **alt.wahlergebnisse.nrw.de** (archive) — every
  `…/landtagswahlen/{1947,1950,1966,1970,1975,1980,1985,1990,1995}/` path returns HTTP
  **404**; the machine-readable open-data series genuinely begins at 2000.
- **statistik.nrw** `…/system/files/statistic_reports/` — hosts only the *current*
  editions (b71–b80 = recent reports); no historical Landtagswahl Heft is served here.

**Conclusion (re-confirmed):** at the Wahlkreis level the gap years 1947, 1950, 1966,
1970, 1975, 1980, 1985, 1990, 1995 have **no** publicly digitised source. The original
"Die Landtagswahl … in Nordrhein-Westfalen" Statistische Berichte for these years exist
only as print (IT.NRW webshop / library holdings) and would need scanning + OCR.
Best next places to look: IT.NRW webshop / direct request to the Landeswahlleiterin NRW
for the historical Statistische Berichte, or a library copy of the per-year Heft for
manual digitisation.

## Second-attempt gap-fill (2026-06-27)

A second pass re-checked the channels suggested for a fresh look. **No new files
obtained** — every obtainable source was already present. Findings:

- **wahlergebnisse.nrw (live Landeswahlleiterin portal):** the Landtagswahlen index
  (`/landtagswahlen/index.shtml`) offers detailed results only for 2000, 2005, 2010,
  2012, 2017, 2022; everything earlier collapses into a single "Landtagswahlen ab 1950"
  page whose body states verbatim: *"Die Ergebnisse früherer Landtagswahlen sind nur
  über das Archiv erreichbar."* — i.e. no downloadable per-Wahlkreis file pre-2000.
- **statistischebibliothek.de re-query + direct Heft-ID probing:** the Solr proxy
  returns only `id` (no titles), so each candidate `NWHeft_mods_*` was opened directly
  (`/mir/receive/NWHeft_mods_<id>`, which renders the title server-side). The only three
  historical "Landtagswahl … in Nordrhein-Westfalen" Hefte that exist are
  `…00019937` (Beitrag 47 = 1954), `…00019938` (Beitrag 97 = 1958), `…00019939`
  (Beitrag 170 = 1962) — all already downloaded. No Heft exists for 1947, 1950, 1966,
  1970, 1975, 1980, 1985, 1990, or 1995.
- **Landtag NRW** (`landtag.nrw.de/.../wahlen`): 404 — no historical Wahlkreis archive.
- **Landesdatenbank NRW / GENESIS REST** (`landesdatenbank.nrw.de/ldbnrwws/rest/...`):
  requires auth and the Landtag tables are Kreis/Gemeinde-dimensioned, not Wahlkreis.
- **archive.org** (advancedsearch): zero NRW Landtagswahl scans.
- **Wikimedia Commons** (API search, File namespace): only Wahlkreis *boundary maps*
  (`Wahlkreise NRW 2010 WK*.svg`), no result tables.

**Conclusion unchanged:** the gap years cannot be obtained at Wahlkreis level from any
public digital source. They survive only as print Statistische Berichte.
