# Hessen — Landtagswahl results at constituency (Wahlkreis) level

**State:** Hessen (HE, state code 06)
**Constituency unit:** Wahlkreis — 55 Wahlkreise (current boundaries since 2008/2009).
Hessen uses a two-vote system: **Wahlkreisstimme** (Erststimme, direct candidate) and
**Landesstimme** (Zweitstimme, party list).

These raw files were collected verbatim for the GERDA election database. Raw data is
read-only — do not modify.

## Primary sources

- **Hessisches Statistisches Landesamt (HSL)** — https://statistik.hessen.de/unsere-zahlen/wahlen
  and the topic page https://statistik.hessen.de/Themen-A-Z/Landtagswahl
- **HSL 2023 results presentation portal (23degrees)** — https://wahlen.hessen-ltw23.23degrees.eu/
  (hosts the machine-readable open-data CSV under `assets/Wahlergebnisse_Landtagswahl.csv`)
- **Statistische Bibliothek (MyCoRe / DESTATIS federation)** — https://www.statistischebibliothek.de
  Series "B VII 2-4 — Die Wahl zum Hessischen Landtag: Endgültige Ergebnisse"
  = `HESerie_mods_00000145` (only the 2018 issue is digitized here).
- **Landeswahlleiter Hessen** — https://wahlen.hessen.de/landtagswahlen

## Downloaded files

| File | Year(s) | Geo unit | Format | Source URL |
|------|---------|----------|--------|------------|
| `HE_2023_Landtagswahl_Wahlkreis_opendata.csv` | 2023 | Wahlbezirk / Gemeinde / **Wahlkreis** / Land (column `Gebietstyp`: SB, VF, KS, LK, **WK**, LD) | CSV (UTF-8, `;`-sep, CRLF) | https://wahlen.hessen-ltw23.23degrees.eu/assets/Wahlergebnisse_Landtagswahl.csv |
| `HE_2023_Landtagswahl_Wahlkreis_BVII2-4.pdf` | 2023 | Wahlkreis + Gemeinde, statistical report (B VII 2-4 – 5j/23, "Die Landtagswahl in Hessen") | PDF | https://statistik.hessen.de/sites/statistik.hessen.de/files/2024-09/bvii2_4_5j23.pdf |
| `HE_2018_Landtagswahl_Wahlkreis_BVII2-4.pdf` | 2018 | Wahlkreis + Gemeinde, statistical report (B VII 2-4 – 5j/18, 3. akt. Auflage) | PDF | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/HEHeft_derivate_00013713/BVII2_4_5j18_3aA.pdf |
| `HE_2018_2013_Landtagswahl_Wahlkreis_Vergleichszahlen_BVII2-1.pdf` | 2018 & 2013 (comparison) | **Wahlkreis** comparison tables (B VII 2-1 – 5j/18, "Vergleichszahlen") | PDF | https://statistik.hessen.de/sites/statistik.hessen.de/files/2022-06/bvii2-1-5j18.pdf |
| `HE_seit1946_Landtagswahl_Landesergebnisse_Tabelle1.xlsx` | 1946–2023 | **State (Land) level only** — not Wahlkreis. Historical series of all Landtagswahlen since 1946. Included for completeness/context. | XLSX | https://statistik.hessen.de/sites/statistik.hessen.de/files/2023-11/BVII2-4_5j2023_Landtagswahl_Tabelle_1.xlsx |

### Notes on the 2023 CSV
The open-data CSV is the live-results export from the HSL presentation portal
(`Stand: 27.10.2023`, i.e. the final/endgültiges result). It contains all reporting
levels in one file; the 55 Wahlkreis rows have `Gebietstyp = "WK"`. Columns provide
turnout, valid/invalid votes and per-party counts for both **Wahlkreisstimmen**
(Erststimme) and **Landesstimmen** (Zweitstimme). This is the machine-readable
constituency-level source for 2023.

## Status in the GERDA pipeline (August 2026)

**2023, 2018 and 2013 are all ingested.** 2023 comes from the open-data CSV; 2018 **and**
2013 are parsed out of `HE_2018_Landtagswahl_Wahlkreis_BVII2-4.pdf` by
`code/state_elections_wahlkreis/parsers/00_he_pdf_parse.py`. That report has a usable text
layer, so no OCR was needed, and its Table 12 gives both elections per Wahlkreis for
Wahlkreisstimmen (12.1) and Landesstimmen (12.2); Table 15 breaks out the six parties that
Table 12.2 lumps into "Sonstige" for 2018; Table 1 (statewide, parties named explicitly) is
used as the validation fixture — the 55 Wahlkreise must reproduce it exactly for every party
in all four series.

`HE_2018_2013_Landtagswahl_Wahlkreis_Vergleichszahlen_BVII2-1.pdf` is **not** a data source
but the independent cross-check: it reports the same 2013-on-2018-boundary results, and the
two documents agree exactly in 53 of 55 Wahlkreise. They differ only on Frankfurt am Main I
(WK 34) and IV (WK 37), by exactly the Stadtbezirk-531 (Schwanheim) transfer that the 2018
Wahlkreiseinteilung made — the July-2018 Vergleichszahlen recomputed it, the August-2024
results report did not. GERDA publishes the B VII 2-4 figures and flags the difference
(`flag_wkr_boundaries_recomputed`).

For anyone who needs 2013 strictly on the 2018 boundaries, the Vergleichszahlen values for
those two constituencies are: WK 34 Wahlberechtigte 62,530 / Wähler 38,927 (WK 37: 63,980 /
46,044). It also breaks REP out of the 2013 residual per Wahlkreis, which B VII 2-4 does not;
GERDA keeps the single-source B VII 2-4 breakdown, so REP 2013 sits inside `sonstige`.

## Election years that exist but are NOT downloadable at Wahlkreis level

Hessen has held Landtagswahlen in: 1946, 1950, 1954, 1958, 1962, 1966, 1970, 1974,
1978, 1982, 1983, 1987, 1991, 1995, 1999, 2003, 2008, 2009, 2013, 2018, 2023.

Only **2023** (CSV + PDF), **2018** (PDF), and partially **2013** (inside the 2018
Vergleichszahlen PDF) are available digitally at Wahlkreis level. For the others, the
constituency-level results exist only in the printed HSL statistical reports
("Statistische Berichte", series **B VII 2-4 — Die Wahl zum Hessischen Landtag:
Endgültige Ergebnisse"), which have **not** been digitized as standalone downloads.

| Year | Reason not downloadable | Source hint |
|------|--------------------------|-------------|
| 2013 | **Now ingested** from the 2018 report's comparison columns (see above), but recomputed onto the 2018 Wahlkreiseinteilung. A genuine 2013-boundary source would still require the printed report B VII 2-4 – 5j/13. | Printed report B VII 2-4 – 5j/13. Request from HSL (statistik.hessen.de). |
| 2009 | Results report (B VII 2-4 – 5j/09) not digitized; library has only the representative-statistics issue (B VII 2-5, a sample survey — not full Wahlkreis results). | HSL print archive; Statistische Bibliothek series HESerie_mods_00000153 (Wahlbeteiligung only). |
| 2008 | Same as 2009 (only B VII 2-5 Wahlbeteiligung digitized, not the full results report). | HSL print archive. |
| 2003 | Results published only as kreisfreie-Städte/Landkreise and Gemeinde tables; no Wahlkreis-level downloadable file. | Hessischer Landtag LIS archive (starweb.hessen.de/starweb/LIS/wahlenhessen.htm) links to HSL tables, but no Wahlkreis breakdown download. |
| 1999, 1995, 1991, 1987, 1983, 1982, 1978, 1974, 1970, 1966, 1962, 1958, 1954, 1950, 1946 | Pre-digital era. Constituency-level results only in printed Statistische Berichte; not digitized. State-level totals are in the included `...Tabelle1.xlsx`. | HSL print archive; Hessischer Landtag documentation (starweb.hessen.de/starweb/LIS/wahlenhessen.htm). |

### Sources checked that had nothing more
- **GovData.de** — no Hessen Landtagswahl Wahlkreis open-data datasets (search returned 0 hits).
- **wahlen.hessen.de / Landeswahlleiter** — links to the HSL presentation portal and the
  PDF reports above; no additional machine-readable Wahlkreis files.
- **Statistische Bibliothek (B VII 2-4 results series)** — only the 2018 issue digitized
  (`HEHeft_mods_00013194`). The Vergleichszahlen (B VII 2-2/2-1) and Wahlbeteiligung
  (B VII 2-5) sub-series have a few more years but the latter are sample-survey
  representative statistics, not full constituency results.

---

## Completeness pass — full historical Wahlkreis coverage added (2026-06-27, 2nd agent)

A second collection pass found the **complete historical archive** that the first pass
missed. The earlier conclusion that pre-2018 Wahlkreis results were "not digitized" was
incorrect: the **Hessischer Landtag — Landtagsinformationssystem (LIS)** document archive
(https://starweb.hessen.de/starweb/LIS/wahlenhessen.htm) hosts both the full HSL
statistical volumes ("Beiträge zur Statistik Hessens" / "Statistische Berichte B VII 2-4")
under `starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik<YEAR>.pdf` and the
official Landeswahlleiter Staatsanzeiger results (`.../cache/stanzwahlergebnis/<YEAR>wahlergebnis.{pdf,tif}`).
**All 21 Landtagswahlen from 1946 to 2023 are now present at constituency (Wahlkreis) level.**

> **CRITICAL — verify each file's actual cover, the URL year label is unreliable.**
> The `cache/hessen/...Statistik1974.pdf` and `...Statistik1978.pdf` URLs **both** serve the
> **1978** election volume (different scans, different MD5, but same election — 8 Oktober 1978).
> There is **no** genuine 1974 volume at the `cache/hessen` path. Every file below was
> visually verified by rendering its cover/data pages before being assigned a year.

### Files added in this pass

| File | Year | Election date | Geo unit | Format | Source URL |
|------|------|---------------|----------|--------|------------|
| `HE_1946_Landtagswahl_Wahlkreis.pdf` | 1946 | 01.12.1946 (Landtagswahl; vol. also covers 30.06.1946 Verf.-Landesversammlung & 25.04.1948 Kommunalwahl) | Wahlkreis + Gemeinde | PDF (scan, "Beiträge z. Statistik Hessens Nr. 12") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1946.pdf |
| `HE_1950_Landtagswahl_Wahlkreis.pdf` | 1950 | 19.11.1950 | Wahlkreis + Gemeinde | PDF (scan) | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1950.pdf |
| `HE_1954_Landtagswahl_Wahlkreis.pdf` | 1954 | 28.11.1954 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge Nr. 72") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1954.pdf |
| `HE_1958_Landtagswahl_Wahlkreis.pdf` | 1958 | 23.11.1958 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge Nr. 107") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1958.pdf |
| `HE_1962_Landtagswahl_Wahlkreis.pdf` | 1962 | 11.11.1962 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge Nr. 139") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1962.pdf |
| `HE_1966_Landtagswahl_Wahlkreis.pdf` | 1966 | 06.11.1966 | Wahlkreis + Gemeinde | PDF (scan, 93 MB) | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1966.pdf |
| `HE_1970_Landtagswahl_Wahlkreis.pdf` | 1970 | 08.11.1970 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge N.F. Nr. 42") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1970.pdf |
| `HE_1974_Landtagswahl_Wahlkreis.tif` | 1974 | 27.10.1974 | Wahlkreis (Landeswahlleiter Staatsanzeiger; per-WK direct mandates + party WK/LS columns) | multi-page TIFF (9 pp) | https://starweb.hessen.de/cache/stanzwahlergebnis/1974wahlergebnis.tif |
| `HE_1978_Landtagswahl_Wahlkreis.pdf` | 1978 | 08.10.1978 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge N.F. Nr. 104") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1978.pdf |
| `HE_1982_Landtagswahl_Wahlkreis.pdf` | 1982 | 26.09.1982 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge N.F. Nr. 144") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1982.pdf |
| `HE_1983_Landtagswahl_Wahlkreis.pdf` | 1983 | 25.09.1983 | Wahlkreis + Gemeinde | PDF (Statistische Berichte B VII 2-83/6, Endgültige Ergebnisse) | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1983.pdf |
| `HE_1987_Landtagswahl_Wahlkreis.pdf` | 1987 | 05.04.1987 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge N.F. Nr. 209") | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1987.pdf |
| `HE_1991_Landtagswahl_Wahlkreis.tif` | 1991 | 20.01.1991 | Wahlkreis (Landeswahlleiter Staatsanzeiger; per-WK Wahlkreisstimmen/Landesstimmen by party) | multi-page TIFF (29 pp) | https://starweb.hessen.de/cache/stanzwahlergebnis/1991wahlergebnis.tif |
| `HE_1995_Landtagswahl_Wahlkreis.tif` | 1995 | 19.02.1995 | Wahlkreis (Landeswahlleiter Staatsanzeiger; per-WK WK/LS by party, 1995 vs 1991 cols) | multi-page TIFF (17 pp) | https://starweb.hessen.de/cache/stanzwahlergebnis/1995wahlergebnis.tif |
| `HE_1999_Landtagswahl_Wahlkreis.pdf` | 1999 | 07.02.1999 | Wahlkreis + Gemeinde | PDF (scan, "Beiträge Nr. 333", 95 MB) | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik1999.pdf |
| `HE_2003_Landtagswahl_Wahlkreis.pdf` | 2003 | 02.02.2003 | Wahlkreis + Gemeinde | PDF (Statistische Berichte B VII 2-4-5j/03, Endgültige Ergebnisse, 89 MB) | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik2003.pdf |
| `HE_2008_Landtagswahl_Wahlkreis.pdf` | 2008 | 27.01.2008 | Wahlkreis + Gemeinde | PDF (Statistische Berichte B VII 2-4-5j/08, Endgültige Ergebnisse, 92 MB) | https://starweb.hessen.de/cache/hessen/Hessen_Landtagswahl_Statistik2008.pdf |
| `HE_2009_Landtagswahl_Wahlkreis.pdf` | 2009 | 18.01.2009 | Wahlkreis (Landeswahlleiter Staatsanzeiger Nr. 8/2009; per-WK WK/LS by party, all 55 WK) | PDF (text, 31 pp) | https://starweb.hessen.de/cache/stanzwahlergebnis/2009wahlergebnis.pdf |

This corrects the earlier "not downloadable" verdict for **2003, 2008, 2009** (full results
now obtained) and adds the complete pre-2000 series **1946–1999**. The pre-1995 PDFs are
scanned images (no text layer) → use OCR / visual reading for digitization. The 1974/1991/1995
TIFs are bi-level multi-page scans of the official Landeswahlleiter Staatsanzeiger.

### Still only via comparison columns
- **2013** — the standalone full B VII 2-4 results report for 2013 is genuinely *not*
  digitized (series `HESerie_mods_00000145` holds only the 2018 issue; the LIS
  `cache/hessen/...2013.pdf` and `...2013wahlergebnis.pdf` both 404). The 2013 Wahlkreis
  figures are fully present as comparison columns inside
  `HE_2018_2013_Landtagswahl_Wahlkreis_Vergleichszahlen_BVII2-1.pdf` (already downloaded).

### Not added (verified non-constituency, would mislabel the folder)
- **B VII 2-5 "Wahlbeteiligung und Wahlentscheidung … repräsentative Wahlstatistik"**
  issues for 2008 (`BVII-2-5-5j-08.xls`), 2009 (`BVII-2-5-5j-09.xls`) and 2013
  (`BVII2-5-5j-13.pdf`) on statistischebibliothek.de (`HESerie_mods_00000153`) were
  fetched and **inspected, then discarded**: they are demographic sample surveys broken
  down by **age group and gender, not by Wahlkreis**. The full constituency results for
  those years come from the LIS archive above instead.

### Coverage summary
**21 of 21 Landtagswahlen (1946–2023) now held at Wahlkreis level** (2013 via comparison
columns). Earliest year obtained = **1946** (= theoretical earliest). Nothing remains
genuinely missing at constituency level.

---
*Collected 2026-06-27 for GERDA. Verified each URL with `curl -sIL` (HTTP 200, correct
content-type) before download, and confirmed bytes/type on disk with `ls`/`file`. Every
historical scan's election year was visually verified from its cover/data pages, because
the `cache/hessen` URL year labels are unreliable (1974 URL == 1978 election).*

---

## 2009 ingested at Wahlkreis level (2026-08-19)

`HE_2009_Landtagswahl_Wahlkreis.pdf` (Staatsanzeiger Nr. 8/2009, digital text layer, 31
pages) is now parsed by `code/state_elections_wahlkreis/parsers/00_he09_pdf_parse.py` into
`data/state_elections/processed/wahlkreis/he_pdf/HE_2009_pdf_long.csv`, which
`parse_HE.R` appends as a third fixture alongside 2023 (open data) and 2018/2013 (B VII
2-4). All 55 Wahlkreise, both Wahlkreisstimmen (erststimme) and Landesstimmen
(zweitstimme), 10 Landesliste parties + 5 Wahlkreis-only individual/local candidates
(APPD in WK 13, Bürgerbewegung – WIR in WK 18 and 52, FAMILIE SCHMIDT in WK 8,
Menschlichkeit in WK 47, Wolf Ruppert – direkt in WK 37).

**2009 predates the Dec-2017 LWG re-cut** — its 55 Wahlkreise are on their own,
pre-2018 boundaries, not the same units as 2013/2018/2023 despite the shared 1–55
numbering. `flag_wkr_boundaries_recomputed = 0` throughout (nothing was recomputed;
2009 is simply a separate, self-contained boundary regime), and `parse_HE.R`
deliberately does not compare 2009 Wahlkreis names against later years.

Two source defects were found by hard validation (both kept as printed, each pinned as
the *only* instance of its kind before anything was written):
- **Wahlkreis 44 (Offenbach Land I):** printed gültige Wahlkreisstimmen (50,596) exceeds
  the sum of its own printed party columns by 11 votes. The statewide "Land Hessen"
  block's CDU/Wahlkreisstimmen total reflects the corrected figure (11 higher than
  summing the 55 printed per-Wahlkreis CDU values) — i.e. the per-Wahlkreis table, not
  the statewide total, is what's wrong.
- **Wahlkreis 1 (Kassel-Land I):** printed Landesstimmen "Wähler" (58,488) is 40 higher
  than that Wahlkreis's own gültige+ungültige Landesstimmen (58,448) — a typo,
  independent of the Wahlkreis-44 defect, confirmed via two independent extraction
  methods (`pdfplumber` word geometry and `pdftotext -layout`).
