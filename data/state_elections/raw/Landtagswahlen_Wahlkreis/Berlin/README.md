# Berlin — Wahl zum Abgeordnetenhaus von Berlin (Landtagswahl), constituency-level raw files

**State:** Berlin (BE)
**Election:** Wahl zum Abgeordnetenhaus von Berlin (the Berlin state parliament; the Land-level "Landtagswahl" equivalent)
**Constituency unit:** Wahlkreis (Abgeordnetenhaus). Berlin currently has ~78 Abgeordnetenhaus-Wahlkreise (12 Bezirke). The machine-readable files below report results at the **Wahlbezirk** (precinct) level, which is the finest unit and aggregates exactly to Wahlkreis; the statistical-report PDFs contain explicit per-Wahlkreis tables.
**Vote system:** Erststimme (Direktstimme, Wahlkreiskandidat) + Zweitstimme (Bezirksliste). Files carry both ("W1"/"Erststimme" and "W2"/"Zweitstimme").

Downloaded: 2026-06-27.

## Primary sources

- **Amt für Statistik Berlin-Brandenburg** — official statistical office / Landeswahlleiter data publisher: <https://www.statistik-berlin-brandenburg.de/abgeordnetenhauswahlen-bvv-berlin/>
- **Der Landeswahlleiter für Berlin (wahlen-berlin.de)** — per-election results presentation sites with Open-Data downloads: <https://www.wahlen-berlin.de/>
- **berlin.de — Ergebnisberichte seit 1990** — archive of Landeswahlleiter result reports (PDF): <https://www.berlin.de/wahlen/historie/berliner-wahlen/artikel.778846.php>
- **Statistische Bibliothek (statistischebibliothek.de)** — digitised historical "Berliner Statistik / Wahlen in Berlin" reports (pre-1990 West Berlin): <https://www.statistischebibliothek.de/>
- **Berlin Open Data Portal (daten.berlin.de)** — dataset catalog (note: several older resource links there now point to a retired `/opendata/` path, see "Years not downloadable").

## Downloaded files

| File | Year | Format | Geo unit | Source URL |
|------|------|--------|----------|------------|
| `BE_2023_Abgeordnetenhauswahl_Wahlbezirk.xlsx` | 2023 (Wiederholungswahl, 12 Feb 2023) | XLSX | Wahlbezirk (→ Wahlkreis); sheets AGH_W1/AGH_W2 + Kandidaturen | https://download.statistik-berlin-brandenburg.de/c6fffa8361dd1404/a8cc1bc593d9/DL_BE_AGHBVV2023.xlsx |
| `BE_2023_Abgeordnetenhauswahl_Ergebnisbericht.xlsx` | 2023 | XLSX | Statistischer Bericht (incl. Wahlkreis tables) | https://download.statistik-berlin-brandenburg.de/8350493567505a9d/e69917ec7c49/SB_B07-02-03_2023j05_BE.xlsx |
| `BE_2023_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 2023 | PDF | Statistischer Bericht (Wahlkreis tables) | https://download.statistik-berlin-brandenburg.de/538210b8454f4642/99e340a74910/SB_B07-02-03_2023j05_BE.pdf |
| `BE_2021_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 2021 (Hauptwahl 26 Sep 2021; later annulled) | PDF (text layer) | Statistischer Bericht (Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/sb_b07-02-03_2021j05_be_ah_bvv-2.pdf |
| `BE_2016_Abgeordnetenhauswahl_Wahlbezirk.xlsx` | 2016 (18 Sep 2016) | XLSX | Wahlbezirk (→ Wahlkreis); sheets Erststimme/Zweitstimme | https://www.wahlen-berlin.de/Wahlen/BE2016/afspraes/download/DL_BE_EE_WB_AH2016.xlsx |
| `BE_2011_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 2011 (18 Sep 2011) | PDF (text layer) | Statistischer Bericht (Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/sb_b7-2-3-j05-11_be.pdf |
| `BE_2006_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 2006 (17 Sep 2006) | PDF | Landeswahlleiterbericht (Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/landeswahlleiterbericht_ah06.pdf |
| `BE_2001_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 2001 (21 Oct 2001) | PDF | Landeswahlleiterbericht (Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/landeswahlleiterbericht_ah2001.pdf |
| `BE_1999_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 1999 (10 Oct 1999) | PDF | Landeswahlleiterbericht (Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/landeswahlleiterbericht_ah1999.pdf |
| `BE_1995_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 1995 (22 Oct 1995) | PDF | Landeswahlleiterbericht (AGH + BVV, Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/landeswahlleiter_ah_bvv_4j-95_bea.pdf |
| `BE_1990_Abgeordnetenhauswahl_Ergebnisbericht.pdf` | 1990 (2 Dec 1990, first all-Berlin AGH) | PDF (scanned, **no text layer → OCR needed**) | Landeswahlleiterbericht (Wahlkreis tables) | https://www.berlin.de/wahlen/historie/berliner-wahlen/ergebnisberichte/landeswahlleiter_bt_1990_bea.pdf |
| `BE_1989_Abgeordnetenhauswahl_Wahlkreis_StatBericht.pdf` | 1989 (29 Jan 1989, West Berlin; incl. 1985 comparison) | PDF (scanned) | "Wahlen in Berlin '89" — Wahlkreis results | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/BBHeft_derivate_00030699/Wahlen%20in%20Berlin%201989.pdf |
| `BE_1990-2023_Abgeordnetenhauswahl_lange_Reihe.xlsx` | 1990–2023 | XLSX | Time series (Berlin total + Bezirk; **not** Wahlkreis) | https://download.statistik-berlin-brandenburg.de/c427aa9c4178f92d/8466ce447431/abgeordnetenhauswahlen-lange-reihe-2023-berlin.xlsx |

Notes:
- The `BE_*_Wahlbezirk.xlsx` files (2016, 2023) are the cleanest machine-readable constituency-level sources; Wahlbezirke map deterministically into Wahlkreise.
- The `lange_Reihe` XLSX is a Berlin-wide / Bezirk time series included for convenience; it does **not** break down by Wahlkreis.
- 1990 (`...bt_1990_bea.pdf`) carries a legacy "bt" filename because the 1990 Abgeordnetenhaus election was held the same day as the Bundestagswahl; the berlin.de archive labels it as the Abgeordnetenhaus 1990 Landeswahlleiter report. It is a scanned image PDF requiring OCR.

## Election years that exist but were NOT downloadable at constituency level

| Year | Reason | Where to find it |
|------|--------|------------------|
| 2021 (machine-readable) | Official daten.berlin.de dataset still lists CSVs at `https://www.statistik-berlin-brandenburg.de/opendata/Berlin_AH21_W1.csv` / `_W2.csv`, but the entire `/opendata/` static directory was retired and now returns a soft-404 HTML landing page (HTTP 200, 69,384-byte HTML) for every path. Not recoverable from the Wayback Machine either (only one capture, itself the soft-404). The **2021 Ergebnisbericht PDF** (downloaded above) is the usable raw constituency source; 2021's Wahlkreise are identical to the 2023 Wiederholungswahl. | daten.berlin.de dataset page: https://daten.berlin.de/datensaetze/wahlen-in-berlin-2021-abgeordnetenhaus-und-bezirksverordnetenversammlungen-endgultiges-ergebnis (links dead). Request current hashed-server file from wahlstatistik@statistik-bbb.de. |
| 1985, 1981, 1979, 1975, 1971, 1967, 1963, 1958, 1954, 1950 (West Berlin) | Published in the "Berliner Statistik / Wahlen in Berlin" Statistische-Berichte series (per-Wahlkreis tables) but no direct, machine-discoverable download URL was found. Only the 1989 issue is openly hosted as a PDF on statistischebibliothek.de (it includes a 1989-vs-1985 comparison). Other years require catalog navigation / OCR of scanned Hefte. | statistischebibliothek.de (search "Wahlen in Berlin" / "Berliner Statistik B VII"); Amt für Statistik Berlin-Brandenburg historical archive; secondary tabulation: https://www.wahlen-in-deutschland.de/blBerlin.htm |
| 2026 | Election scheduled for 20 Sep 2026 — not yet held; only Strukturdaten and umgerechnete (recomputed) prior results published. | https://www.statistik-berlin-brandenburg.de/abgeordnetenhauswahlen-bvv-berlin/ |

Server reorganisation caveat: the Amt für Statistik now serves downloads from `download.statistik-berlin-brandenburg.de` via opaque hashed paths (e.g. `/c6fffa8361dd1404/.../DL_BE_AGHBVV2023.xlsx`). These hashes can change when the office re-publishes; the human landing page <https://www.statistik-berlin-brandenburg.de/abgeordnetenhauswahlen-bvv-berlin/> is the durable entry point.

## Completeness review (critic pass, 2026-06-27)

A second agent re-checked for missed AGH constituency-level files. Findings:

- **No genuinely-new downloadable AGH-Wahlkreis files were found.** The original grab is effectively complete for what is publicly downloadable.
- The statistischebibliothek.de digital library (MyCoRe; queried via `/mir/servlets/solr/select?q=Abgeordnetenhaus`) holds the constituency-level **"Statistischer Bericht B VII 2/3 — Endgültiges Ergebnis"** series (`BBSerie_mods_00001102`) starting only at **1989** (`BBHeft_mods_00047084`, already held). There is **no** B VII 2/3 issue for any West-Berlin AGH election 1950–1985 in the digital library.
- Two **1992** documents surfaced in the catalog (`BBHeft_mods_00032868` = `SB_B7-3_4j92_BE.pdf`; `BBHeft_mods_00032875` = `Landeswahlleiter_BVV_1992_BE.pdf`). Both were inspected page-by-page: the **24 May 1992 Berlin election was a Bezirksverordnetenversammlungen (BVV / district-council) re-run only — NOT an Abgeordnetenhaus election** (every results/seat-allocation table is headed "…zu den Bezirksverordnetenversammlungen … am 24. Mai 1992"). They are therefore **out of scope** for the AGH-Wahlkreis dataset and deliberately NOT downloaded.
- The 1946 (20 Oct 1946) and 1948 (5 Dec 1948) "Berliner Wahl" Sonderhefte exist in the library (`BBHeft_derivate_00031393` `SH_004.1-47_BE.pdf`, etc.) but cover the **Stadtverordnetenversammlung von Groß-Berlin**, the pre-AGH body (the Abgeordnetenhaus was constituted only with the 1950 Verfassung). Not downloaded — pre-AGH and a different parliament.
- berlin.de Ergebnisberichte confirmed to start at 1990; wahlen-berlin.de likewise. GESIS holds 1981 (ZA2313) etc. only as restricted survey/microdata, not as openly-downloadable constituency aggregates.
- **Coverage assessment:** earliest obtained AGH constituency-level file = **1989** (which also carries a 1985 comparison column). Theoretical earliest is the first West-Berlin AGH election of **3 Dec 1950**. The 1950–1985 gap is a genuine digitisation gap (printed "Berliner Statistik" Hefte only), not an oversight.

## Gap-fill (June 2026)

Targeted second pass to fill the missing historical West-Berlin Abgeordnetenhauswahl years **1950, 1954, 1958, 1963, 1967, 1971, 1975, 1979, 1981, 1985**.

**Result: no new files added — none of these years are available online at constituency (Wahlkreis) level.**

The official per-election report for each of these elections exists only as a printed Heft of the series *Berliner Statistik – Statistische Berichte B VII 3* ("Die Wahlen am … in Berlin (West). Endgültiges Ergebnis der Wahlen zum Abgeordnetenhaus und zu den Bezirksverordnetenversammlungen"), published by the Statistisches Landesamt Berlin. None of these pre-1989 Hefte have been digitised on any of the public portals:

- **statistischebibliothek.de** (MyCoRe SOLR proxy searched exhaustively): the digitised "Wahlen in Berlin" Abgeordnetenhaus Heft series (`BBSerie_mods_00001102`) and the curated "Sonderheft / Berliner Statistik" series (`BBSerie_mods_00001542`) begin only at **1989/1990**. The Sonderheft series holds the two early Groß-Berlin city-election Hefte (Heft 4 = 20.10.1946, Heft 8 = 5.12.1948) but **nothing for 1950–1985**. Searches by election date (e.g. "10. März 1985", "2. März 1975"), by year, and by title returned zero Berlin Abgeordnetenhaus Wahlkreis Hefte for the target years. (The earliest digitised full per-Wahlkreis result PDF is the 1989 "Wahlen in Berlin 1989", `BBHeft_derivate_00030699`, which is already held here as `BE_1989_Abgeordnetenhauswahl_Wahlkreis_StatBericht.pdf`.)
- **berlin.de/wahlen/historie** (Landeswahlleiter Ergebnisberichte archive): the listed AH reports start at **1990**; probing the `ergebnisberichte/landeswahlleiterbericht_ah*.pdf` naming for 1971–1989 returns HTTP 403 (file absent). Site is behind a `bo_vtoken` bot cookie (handled), confirming the 200-vs-403 distinction is real.
- **wahlen-berlin.de** (AfS per-election results app): only carries elections it ran digitally (1990s onward); no `be1985/`, `AH1985/` etc. directories exist (404).
- **daten.berlin.de** (Berlin Open Data): wahl datasets only from 2021/2025; nothing pre-1990.
- **Statistisches Bundesamt (DEHeft on statistischebibliothek)**: its national series do not publish Berlin Abgeordnetenhaus results broken down by Wahlkreis.
- **wahlen-in-deutschland.de / wahlrecht.de / Wikipedia**: statewide totals (and mandate distributions) only — no per-Wahlkreis vote tables, no downloadable raw files.

**Best next place to look** (offline / institutional): the printed *Berliner Statistik – Statistische Berichte B VII 3* Hefte for each year, held by the Staatsbibliothek zu Berlin and the Zentrale Bibliothek of the Amt für Statistik Berlin-Brandenburg (Alt-Friedrichsfelde 60, 10315 Berlin). A direct request to the AfS Berlin-Brandenburg (info@statistik-berlin-brandenburg.de) for scans of the B VII 3 Hefte 1950–1985, or an on-site scan at the Staatsbibliothek, is the only route to constituency-level results for these years.

## Second gap-fill pass (June 2026, independent re-verification)

A second gap-fill agent independently re-checked all 10 missing years (1950, 1954, 1958, 1963, 1967, 1971, 1975, 1979, 1981, 1985) using sources the first pass may not have exhausted. **Result: still no new files — confirmed the gap is genuine.** New verification done this pass:

- **statistischebibliothek.de MyCoRe SOLR (`/mir/servlets/solr/select?q=…&wt=json`, raw JSON via curl):** enumerated the *complete* child lists of every relevant series by `parent:` query, not just title search. The three Abgeordnetenhaus result series — `BBSerie_mods_00001142` (Statistischer Bericht B VII 2/5) and the B VII 2/3, 2/1, 2/4 sibling series — contain **zero** Hefte before 1989/1990 (earliest = 1989 `BBHeft_mods_00047084`, already held). The "Sonderheft / Berliner Statistik" series `BBSerie_mods_00001542` (32 Hefte enumerated) holds election Hefte only for **Heft 4 = 20.10.1946** and **Heft 8 = 5.12.1948** (both Stadtverordnetenversammlung von Groß-Berlin, pre-Abgeordnetenhaus, out of scope), then a gap from Heft 39 (1954, a census) straight to Heft 103 (1989). The 1975 election's printed report (*Berliner Statistik, Sonderheft 233*, confirmed to exist via web search) falls inside that undigitised gap.
- **berlin.de Ergebnisberichte:** re-confirmed the known-good naming `…/ergebnisberichte/landeswahlleiterbericht_ah06.pdf` returns HTTP 200 + `application/pdf` (5.2 MB), while every pre-1990 variant (`…_ah85.pdf`, `_ah1985.pdf`, `_ah81/79/75/71`) returns a 301→200 `text/html` soft-404 (file absent). The berlin.de article (`artikel.778846.php`) states the AH Ergebnisbericht archive begins 1990.
- **wahlen-berlin.de `/historie/Wahlen/`:** probed `Landeswahlleiterbericht_AH{71,75,79,81,85,89,90,95,99,01,06}.pdf` — all 404 (this path hosts only Bundestag historie PDFs, not AH).
- **statistik-berlin-brandenburg.de "abgeordnetenhauswahlen" portal:** only the 1990–2023 "Lange Reihe" XLSX (already held, Berlin/Bezirk totals, not Wahlkreis) and a pointer back to the statistischebibliothek series.
- **archive.org / Wikimedia Commons:** no scanned *Berliner Statistik B VII 3* Hefte for the target years surfaced.

Conclusion unchanged: 1950–1985 West-Berlin Abgeordnetenhaus per-Wahlkreis results exist only as undigitised printed Hefte; institutional scan request (AfS Berlin-Brandenburg / Staatsbibliothek zu Berlin) remains the only route.
