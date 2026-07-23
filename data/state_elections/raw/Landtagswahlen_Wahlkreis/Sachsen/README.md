# Sachsen — Landtagswahlen, Wahlkreis-level raw files

**State:** Sachsen (Saxony), state code 14
**Constituency unit:** Wahlkreis (Landtagswahlkreis). Saxony has 60 Wahlkreise.
**Elections held:** 1990, 1994, 1999, 2004, 2009, 2014, 2019, 2024 (8 since reunification).

Raw files are stored verbatim. Each machine-readable workbook contains a dedicated
Wahlkreis sheet (`..._SN&WK` / `LW14_Ergebnisse_WK`) with Direktstimmen and
Listenstimmen per party, plus Wahlberechtigte / Wähler / gültige / ungültige Stimmen.

## Primary sources

- Wahlen-Portal des Statistischen Landesamtes Sachsen — https://www.wahlen.sachsen.de/
- 2024 Informationen und Downloads — https://wahlen.sachsen.de/landtagswahlen-2024-informationen-und-downloads.html
- 2019 Informationen und Downloads — https://www.wahlen.sachsen.de/landtagswahl-2019-informationen-und-downloads.html
- 2014 Informationen und Downloads — https://www.wahlen.sachsen.de/landtagswahl-2014-informationen-und-downloads.html
- Statistische Bibliothek (Statistische Berichte, historische Hefte) — https://www.statistischebibliothek.de/

## Downloaded files

### 2024 (election 1 Sep 2024) — machine-readable
| File | Source URL | Format | Geo unit |
|---|---|---|---|
| `SN_2024_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx` | https://wahlen.sachsen.de/download/Landtag/statistik-sachsen_LW24_endgErgebniss.xlsx | xlsx | Land + **Wahlkreis** + Gemeinde/Ortsteil + Kreis (multi-sheet) |
| `SN_2024_Landtagswahl_Wahlbezirk.xlsx` | https://wahlen.sachsen.de/download/Landtag/statistik-sachsen_LW24_endgErgebnisse_WBZ.xlsx | xlsx | Wahlbezirk (finest level) |

### 2019 (election 1 Sep 2019) — machine-readable, original boundaries
| File | Source URL | Format | Geo unit |
|---|---|---|---|
| `SN_2019_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx` | https://wahlen.sachsen.de/download/Landtag/LW19_endgErgebnisse_Stand27092019.xlsx | xlsx | Land + **Wahlkreis** + Gemeinde/Ortsteil + Kreis |
| `SN_2019_Landtagswahl_Wahlgebiet.xlsx` | https://wahlen.sachsen.de/download/Landtag/LW19_Wahlgebiet.xlsx | xlsx | Wahlgebiet summary |
| `SN_2019_Landtagswahl_Wahlbezirk.xlsx` | https://wahlen.sachsen.de/download/Landtag/LW19_endgErgebnisse_WBZ.xlsx | xlsx | Wahlbezirk |
| `SN_2019_Landtagswahl_StatistischerBericht_Wahlgebiet.pdf` | https://wahlen.sachsen.de/download/Landtag/B_VII_2_WahlgebietA0-5j_19_SN.pdf | pdf | Statistischer Bericht B VII 2, Wahlgebiet/Wahlkreis |
| `SN_2019_Landtagswahl_nach_LTWK2024.xlsx` | https://wahlen.sachsen.de/download/Landtag/statistik-sachsen_landtag2019_nach_LTWK2024.xlsx | xlsx | Wahlkreis, **recalculated to 2024 districts** (comparability file) |

### 2014 (election 31 Aug 2014) — machine-readable, original boundaries
| File | Source URL | Format | Geo unit |
|---|---|---|---|
| `SN_2014_Landtagswahl_Wahlkreis_originale.xlsx` | https://wahlen.sachsen.de/download/Bundestag/LW14_Ergebnisse_ORIGINALE.xlsx | xlsx | Land + **Wahlkreis** + Gemeinde/Ortsteil + Wahlbezirk |
| `SN_2014_Landtagswahl_reine_Briefwahl_originale.xlsx` | https://wahlen.sachsen.de/download/Bundestag/LW14_reine_Briefwahlergebnisse_ORIGINALE.xlsx | xlsx | reine Briefwahlergebnisse |
| `SN_2014_Landtagswahl_nach_LTWK2024.xlsx` | https://wahlen.sachsen.de/download/Landtag/statistik-sachsen_landtag2014_nach_LTWK2024.xlsx | xlsx | Wahlkreis, **recalculated to 2024 districts** (comparability file) |
| `SN_2014_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf` | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SNHeft_derivate_00005762/B_VII_2_2_5j_14_SN.pdf | pdf | Statistischer Bericht B VII 2-2, Wahlkreis tables |

### Historical reports (PDF only — contain Wahlkreis tables + back-series)
| File | Source URL | Format | Geo unit | Notes |
|---|---|---|---|---|
| `SN_2009_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf` | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SNHeft_derivate_00005761/B_VII_2-2_5j09_SN.pdf | pdf | Wahlkreis | "Wahlen im Freistaat Sachsen 2009"; carries comparison to 1990–2004 |
| `SN_2004_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf` | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SNHeft_derivate_00005760/B_VII_2-2_5j04_SN.pdf | pdf | Wahlkreis | "Wahlen im Freistaat Sachsen 2004"; carries comparison to 1990/1994/1999 |

### 1990 (election 14 Oct 1990) — Wahlkreis-level PDF (ADDED 2026-06-27, completeness pass)
| File | Source URL | Format | Geo unit | Notes |
|---|---|---|---|---|
| `SN_1990_Landtagswahl_Wahlkreis.pdf` | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/DEMonografie_derivate_00002910/Wahlen_zu_den_Landtagen_1990.pdf | pdf | **Wahlkreis** + Stadt-/Landkreis | Statistisches Bundesamt monograph "Wahlen zu den Landtagen der Länder Mecklenburg-Vorpommern, Brandenburg, Sachsen-Anhalt, Thüringen, Sachsen — Gesamtübersicht; **Endgültige Ergebnisse nach Wahlkreisen, Stadt- und Landkreisen**" (1990). 156 pp, 37.8 MB. Covers the 14 Oct 1990 Landtagswahl for all five new Länder incl. Sachsen at constituency level — the only standalone digital source for SN 1990 Wahlkreis results. MyCoRe id `DEMonografie_mods_00005842` / derivate `DEMonografie_derivate_00002910`. |

## Years NOT downloadable at constituency level (and where to find them)

- **1999, 1994:** No dedicated digital file (machine-readable or standalone PDF) is
  published. The Statistische Bibliothek's digitized "Wahlen im Freistaat Sachsen"
  Landtag series (B VII 2-2, MyCoRe series `SNSerie_mods_00001393`) begins with the
  **2004** issue — verified by enumerating the full series: Hefte exist only for
  2004, 2009, 2014, 2019, 2024. Wahlkreis-level results for 1999 and 1994 are
  contained as historical-comparison tables **inside** the 2004 and 2009 Statistische
  Berichte PDFs already downloaded here, and as the original print "Sonderhefte" of
  the Statistisches Landesamt (Kamenz), which are not online. The online
  wahlen.sachsen.de portal only goes back to **2014** (probed download paths
  `B_VII_2-2_5j99_SN.pdf` / `5j94` under both `/download/Veroeffentlichungen/` and
  `/download/Landtag/` return 404). The old `statistik.sachsen.de/wahlen/lw/lwYYYY/`
  HTML archive that once held these tables is gone (all paths now serve the sachsen.de
  404 template). No Statistisches-Bundesamt federal Wahlkreis monograph exists for
  1994/1999 (the 1990 monograph above was a one-off Gesamtübersicht for the new Länder;
  subsequent Landtagswahlen reverted to per-state-only reporting).
  Source hint: order the print Sonderheft from Statistisches Landesamt Sachsen, Kamenz;
  or extract the back-series Wahlkreis tables from the already-downloaded 2004/2009 PDFs.

## Notes on file flavours

- "original boundaries" files (`_originale` / `_Stand...`) report each election on the
  Wahlkreis boundaries that were in force at that election.
- `_nach_LTWK2024` files are the office's own recalculation of 2014/2019 onto the
  60 Wahlkreise used in 2024, for cross-time comparability. Both flavours are kept.
- All `.xlsx` workbooks include a `Erklärung_Legende` sheet documenting the columns.

Retrieved 2026-06-27 via curl from the URLs above (all verified HTTP 200,
content-type application/vnd.openxmlformats / application/pdf).

## Gap-fill (June 2026)

Targeted second pass for the two missing historical years **1994** and **1999** at
Wahlkreis level. **No new files could be obtained** — both years remain genuinely
unavailable as a dedicated constituency-level digital artifact. The first-pass
assessment above was independently re-verified end-to-end this run:

- **statistischebibliothek.de** — Enumerated the full digitized Landtag series
  *Statistische Berichte B VII 2-2 "Sächsischer Landtag – Endgültige Ergebnisse"*
  (MyCoRe series `SNSerie_mods_00001393`): it contains **only** the 2004, 2009 and
  2014 Hefte; the earliest digitized Sächsischer-Landtag report is **2004**. SOLR
  queries for 1994/1999 SN Hefte return only unrelated reports (Mikrozensus A VII 10,
  Gebietsänderungen). No 1994/1999 Wahlkreis Heft exists in the library.
- **wahlen.sachsen.de** — Modern portal links only back to **2014**; the
  `landtagswahlen.html` page (Wayback history 2020→2024) never linked 1994/1999.
  Probed download paths under `/download/Landtag/`, `/download/Bundestag/` and
  `/download/Veroeffentlichungen/` for `LW94_*` / `LW99_*` / `LW1994_*` / `LW1999_*`
  patterns — all HTTP 404. The statewide time-series workbook
  (`statistik-sachsen_wahlen_zr_stimmenverteilung.xlsx`) carries 1994/1999 only as
  Land-level party percentages, **not** by Wahlkreis.
- **statistik.sachsen.de (old archive)** — `wahlen/allg/Seite_2.htm`,
  `wahlen/ltw/index.htm` etc. all now serve the generic sachsen.de 404 template.
- **Internet Archive / Wayback** — Domain-wide CDX scan of `statistik.sachsen.de`.
  The 1999 election lived in the dynamic `appsl1` Wahlen application (database-backed
  result servlets that Wayback could not snapshot as files); the only `/20wahl/`
  captures are the Wahlgesetz text (`seite5-Landtag.htm`, Sept 1999, pre-election),
  not results. The 1994 election predates the office's web presence and was never
  online. The `wahlen-html/` static dir only holds 2002+ cycles (`LW04_*`, `LW09_*`).
- **regionalstatistik.de (GENESIS)** — carries no Landtagswahl tables (confirmed; the
  portal exposes only Bundestag/Europa/Landtag aggregates without a downloadable
  Wahlkreis result table for SN 1994/1999).
- **Secondary aggregators** — wahlen-in-deutschland.de offers only Erststimmen-by-WK
  (not the full party breakdown), and is not the official raw report; not downloaded.

**Best next place to look:** the Wahlkreis back-series for 1994 and 1999 is already
present *as historical-comparison tables inside* the 2004 and 2009 Statistische
Berichte PDFs downloaded here (`SN_2004_…B-VII-2-2.pdf`, `SN_2009_…B-VII-2-2.pdf`) —
these can be extracted via OCR rather than re-downloaded. For the standalone original
artifact, order the print "Sonderheft / Statistischer Bericht B VII 2" for the
1.9.1994 and 19.9.1999 elections from Statistisches Landesamt des Freistaates
Sachsen, Kamenz (Macherstraße 63, 01917 Kamenz; info@statistik.sachsen.de) — these
print issues were never digitized into statistischebibliothek.de.

---

## 1994 & 1999 RECOVERED (gap-fill, 2026-06) — via Wayback static result pages

Correction to the note above: the 1994 and 1999 Wahlkreis results **were** captured by
the Internet Archive — not in the dynamic `appsl1` app, but as the **static** result
pages under `statistik.sachsen.de/wahlen/lw/lw1994/` and `.../lw1999/wahlerg/`. These
were found via a domain-wide CDX scan (`web.archive.org/cdx/search/cdx?url=
statistik.sachsen.de/wahlen/lw/lw1994/&matchType=prefix`). The live URLs now serve the
generic 404 template (50 922 bytes) — only the Wayback `…/<ts>id_/…` raw form returns
the original content.

### 1999 (election 19 Sep 1999) — machine-readable CSV
| File | Source (Internet Archive raw) | Format | Geo unit |
|---|---|---|---|
| `SN_1999_Landtagswahl_Wahlkreis.csv` | `web.archive.org/web/20030902133402id_/http://www.statistik.sachsen.de/wahlen/lw/lw1999/wahlerg/lw99wee.csv` | CSV (ISO-8859-1, `;`-sep) | **all 60 Wahlkreise** + Freistaat total |

The official "Amtliche Ergebnisse Landtagswahl Freistaat Sachsen 1999" CSV: per Wahlkreis
Wahlberechtigte, Wähler, gültige/ungültige Direkt- und Listenstimmen, and per-party
Direkt+Listen **vote counts** — for both **1999 and 1994** (it is a 1999-vs-1994
comparison file). 63 lines = title + header + 60 WK + Land total. This is the single
cleanest raw artifact for 1999, and additionally carries the 1994 figures for all 60 WK.

### 1994 (election 11 Sep 1994) — bundled WK result pages
| File | Source (Internet Archive raw) | Format | Geo unit |
|---|---|---|---|
| `SN_1994_Landtagswahl_Wahlkreis.html` | `web.archive.org/web/<ts>id_/https://www.statistik.sachsen.de/wahlen/lw/lw1994/LW94WERG<NN>.htm` (per-WK pages, various snapshot timestamps 2015–2025) | HTML (ISO-8859-1), 49 WK sections concatenated | **49 of 60 Wahlkreise** |

Each section is the official "Endgültiges Ergebnis – Wahlkreisergebnis" for one Landtags-
wahlkreis: Wahlberechtigte, Wähler, ungültige/gültige Direkt- und Listenstimmen, and the
full party breakdown (CDU, SPD, F.D.P., GRÜNE, DSU, REP, FORUM, PDS, SP, …) in Direkt-
**and** Listenstimmen, absolut und %.

**Coverage caveat:** 11 of the 60 per-WK pages — the city/urban Wahlkreise
**12–15 (Chemnitz 1–4), 24 (Leipziger Land 2), 29–31 (Leipzig 4–6), 51–52 (Bautzen 1–2),
58 (Görlitz)** — were never snapshotted by Wayback under any `LW94WERG`/`bewwk` name
(the `bewwk` pages now return "Diese Angaben werden … nicht mehr angeboten"). Their 1994
figures (Wahlberechtigte, gültige Stimmen, and the major-party Listenstimmen counts) are
nonetheless available for **all 60 WK** in the `… 1994` columns of
`SN_1999_Landtagswahl_Wahlkreis.csv`, so 1994 is effectively complete at WK level when the
two files are combined. The statewide summary page (`lw94wergsn14.htm`) was no longer
archivable as raw content and is not stored.
