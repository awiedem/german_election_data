# Hamburg — Bürgerschaftswahl (constituency-level raw files)

- **State:** Hamburg (HH), state key `02`
- **Election:** Bürgerschaftswahl (Hamburg state parliament)
- **Constituency unit:** Wahlkreis (17 Wahlkreise, in use since the 2008 reform) + Landeslisten (5-vote system).
  Pre-2008 elections had **no Wahlkreise**: the finest published geographic unit was the
  Wahlbezirk (polling district) / Stadtteil; those files are included as the closest
  available constituency-level breakdown.
- **Vote types:** since 2008 each voter has 5 votes split across two ballots —
  *Landesstimmen* (Landesliste, state list) and *Wahlkreisstimmen* (Wahlkreisliste, constituency list).
- **Downloaded:** 2026-06-27

## Primary sources

- Statistik Nord — Wahlen in Hamburg / Bürgerschaftswahlen:
  https://www.statistik-nord.de/wahlen/wahlen-in-hamburg/buergerschaftswahlen
- Wahlen in Hamburg (Landeswahlleitung result portal): https://www.wahlen-hamburg.de/
- 2025 downloads page: https://www.wahlen-hamburg.de/B%C3%BCrgerschaftswahl_2025/downloads.html

## Downloaded files

### 2025 (17 Wahlkreise; Wahlbezirk-level CSV with `Wahlkreis` column)
Source: wahlen-hamburg.de result portal. CSVs cover all counted Wahlbezirke, all parties.
Columns are coded (D1, D1_1 …); the two `*_Feldbezeichner*.xlsx` map the codes to party/field names.
- `HH_2025_Buergerschaftswahl_Wahlkreis_Wahlkreisstimmen_WBZ.csv` — Wahlkreisstimmen (constituency-list votes), Wahlbezirk rows incl. Wahlkreis/Bezirk/Stadtteil cols.
  - https://www.wahlen-hamburg.de/Bürgerschaftswahl_2025/ergebnis-download-wahlkreis.csv
- `HH_2025_Buergerschaftswahl_Landesstimmen_WBZ.csv` — Landesstimmen (state-list votes), Wahlbezirk rows.
  - https://www.wahlen-hamburg.de/Bürgerschaftswahl_2025/ergebnis-download-land.csv
- `HH_2025_Feldbezeichner_Wahlkreislisten.xlsx` / `HH_2025_Feldbezeichner_Landesliste.xlsx` — column/field dictionaries.
  - https://www.wahlen-hamburg.de/Bürgerschaftswahl_2025/Wahlkreislisten_D_Feldbezeichner.xlsx
  - https://www.wahlen-hamburg.de/Bürgerschaftswahl_2025/Landesliste_F_Feldbezeichner.xlsx

### 2020 (17 Wahlkreise)
Source: statistik-nord.de (endgültiges Ergebnis).
- `HH_2020_Buergerschaftswahl_Landesliste_in_Wahlkreisen.xlsx` — Gesamtstimmen Landesliste, one section per Wahlkreis (Bue_e_007-001).
- `HH_2020_Buergerschaftswahl_Wahlkreisliste_in_Wahlkreisen.xlsx` — Gesamtstimmen Wahlkreisliste, per Wahlkreis (Bue_e_008-001).
- `HH_2020_Buergerschaftswahl_Landesliste_WBZ.xlsx` — Landesliste per Wahlbezirk incl. Brief (Bue_e_999-002).
- `HH_2020_Buergerschaftswahl_Wahlkreisliste_WBZ.xlsx` — Wahlkreisliste per Wahlbezirk incl. Brief (Bue_e_999-003).
  - Base path: https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/Bürgerschaftswahlen/2020/endgueltig/

### 2015 (17 Wahlkreise)
Source: statistik-nord.de (endgültiges Ergebnis).
- `HH_2015_Buergerschaftswahl_Landesliste_in_Wahlkreisen.xlsx` — Landesstimmen in den Wahlkreisen (buewa06_2015e).
- `HH_2015_Buergerschaftswahl_Wahlkreisliste_in_Wahlkreisen.xlsx` — Wahlkreisstimmen in den Wahlkreisen (buewa10_2015e).
- `HH_2015_Buergerschaftswahl_Landesliste_WBZ.csv` — Landesstimmen per Wahlbezirk (dintbue02_2015e).
  - Base path: https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/Bürgerschaftswahlen/2015/endgueltig/

### 2011 (17 Wahlkreise)
Source: statistik-nord.de.
- `HH_2011_Buergerschaftswahl_Wahlkreisstimmen_WBZ.csv` + `.xls` — Wahlkreisstimmen per Wahlbezirk (DINTBUE03_neu).
- `HH_2011_Buergerschaftswahl_Landesstimmen_WBZ.csv` — Landesstimmen (Gesamtstimmen) per Wahlbezirk (DINTBUE02_online).
  - Base path: https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/Bürgerschaftswahlen/2011/vorläufig/Wahlergebnisse_zum_Download/

### 2008 (first election with 17 Wahlkreise)
Source: statistik-nord.de.
- `HH_2008_Buergerschaftswahl_Landesliste_WBZ_in_Wahlkreisen.csv` — Landeslisten-Stimmen per Wahlbezirk, grouped by Wahlkreis (bue2008landesliste_wk01-17).
- `HH_2008_Buergerschaftswahl_Wahlkreislistenstimmen_in_Wahlkreisen.xls` — Wahlkreislisten-Stimmen in den Wahlkreisen, absolut.
  - Base path: https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/Bürgerschaftswahlen/2008/

### 2004 (no Wahlkreise — Wahlbezirk level)
Source: statistik-nord.de.
- `HH_2004_Buergerschaftswahl_Wahlbezirke.xls` — results per Wahlbezirk (dl_bue04).
  - https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/Bürgerschaftswahlen/2004/dl_bue04.xls

### 2001 (no Wahlkreise — Wahlbezirk level)
Source: statistik-nord.de.
- `HH_2001_Buergerschaftswahl_Wahlbezirke.xls` — results per Wahlbezirk, absolute (Bue2001_Wahlbezirke-abs).
  - https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/Bürgerschaftswahlen/2001/Bue2001_Wahlbezirke-abs.xls

### Pre-2001 historical reports (added 2026-06-27 — second pass)

The pre-2001 Bürgerschaftswahlen ARE available after all, as scanned full-text PDFs of the
official report series **"Statistische Mitteilungen über den hamburgischen Staat"** (each
election has its own "Heft"), digitized in the **Statistische Bibliothek**
(statistischebibliothek.de, MyCoRe repository, series HHAusgabe). These are the official
endgültige-Ergebnis reports and contain the sub-municipal breakdowns (Stadtteil /
Wahlbezirk / Ortsamt — the closest pre-2008 equivalent of a constituency level; Hamburg
had no Wahlkreise before 2008). One report = one election. Downloaded via the MyCoRe
`MCRFileNodeServlet` derivate path; the SOLR proxy
`https://www.statistischebibliothek.de/mir/servlets/solr/select?q=Bürgerschaftswahl&fq=objectType:mods&wt=json`
plus the `…/mir/api/v2/objects/<mods_id>/derivates` API resolve each Heft → derivate → PDF.

| Year (election) | Election date | Heft | File | mods id / derivate |
|---|---|---|---|---|
| 1957 | 10 Nov 1957 | 52 | `HH_1957_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001732 / der_00000620 (1221-7-052.pdf) |
| 1961 | 12 Nov 1961 | 63 | `HH_1961_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001743 / der_00000849 (1221-7-063.pdf) |
| 1966 | 27 Mar 1966 | 84 | `HH_1966_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001773 / der_00000661 (1221-7-084.pdf) |
| 1970 | 22 Mar 1970 | 97 | `HH_1970_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001795 / der_00000816 (1221-7-097.pdf) |
| 1974 | 3 Mar 1974 | 112 | `HH_1974_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001810 / der_00000800 (1221-7-112.pdf) |
| 1978 | 4 Jun 1978 | 124 | `HH_1978_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001822 / der_00000788 (1221-7-124.pdf) |
| 1982 (Jun) | 6 Jun 1982 | 132 | `HH_1982-06_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001830 / der_00000780 (1221-7-132.pdf) |
| 1982 (Dez) | 19 Dec 1982 | 139 | `HH_1982-12_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001831 / der_00000777 (1221-7-139.pdf) |
| 1986 | 9 Nov 1986 | 148 (Teil 1) | `HH_1986_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001781 / der_00000764 (1221-7-148-1.pdf) |
| 1991 | 2 Jun 1991 | 156 | `HH_1991_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001755 / der_00000756 (1221-7-156.pdf) |
| 1993 | 19 Sep 1993 | 157 | `HH_1993_Buergerschaftswahl_Wahlkreis.pdf` | mods_00001754 / der_00000755 (1221-7-157.pdf) |

URL pattern: `https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/<derivate_id>/<pdf_name>`
All verified HTTP 200, `application/pdf`, 132–315 MB each (high-resolution scans).

## Years that still cannot be obtained at constituency level

- **1949, 1953** — earliest postwar Bürgerschaftswahlen; **no digitized report** exists in
  the Statistische Bibliothek (the HHAusgabe series starts at the 1957 election / Heft 52).
  Statewide aggregates only (wahlen-in-deutschland.de / wahlrecht.de).
- **1987** — the 17 May 1987 re-run election (the 1986 Bürgerschaft failed to form a
  government) has **no separate Heft** in the library; only the 1986 election (Heft 148) is
  digitized. The "1987" hits in the catalogue are Bundestags-/Landtagswahl reports, not the
  HH Bürgerschaftswahl.
- **1997** — the Statistik Nord online file server and the Statistische Bibliothek do not
  carry a 1997 Bürgerschaftswahl constituency/Wahlbezirk file (no HHAusgabe Heft, gap
  between Heft 157 = 1993 and the 2001 online files). Aggregate totals only.

Note: 2004 and 2001 files are at Wahlbezirk level (no Wahlkreise existed then) and are
included as the closest available constituency-level breakdown for those years; likewise
the 1957–1993 reports above are sub-municipal (Stadtteil/Wahlbezirk), not Wahlkreis.
