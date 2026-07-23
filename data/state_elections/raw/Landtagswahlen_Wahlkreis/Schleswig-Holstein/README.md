# Schleswig-Holstein — Landtagswahl results at the Wahlkreis level (RAW)

**State:** Schleswig-Holstein (SH)
**Constituency unit:** Wahlkreis (35 Landtagswahlkreise; the count was 45 in 1950–1971, 22 in 1975–1988, 45 in 1992–2009, and 35 since 2012).
**Vote system:** Erststimme (Direktstimme, by Wahlkreis) + Zweitstimme (Landesstimme / Listenstimme). Older reports (pre-1992) use the one-/two-ballot conventions of their era.
**Scope of this folder:** every directly-downloadable Landtagswahl results file broken down BY WAHLKREIS, oldest to newest. Raw files are stored verbatim.

## Primary sources

1. **Statistikamt Nord** (Statistisches Amt für Hamburg und Schleswig-Holstein) — official election statistics for SH since 2004:
   https://www.statistik-nord.de/wahlen/wahlen-in-schleswig-holstein/landtagswahlen
   Per-election pages carry XLSX/CSV/PDF for 2005, 2009, 2012, 2017, 2022, 2027.
2. **Statistische Bibliothek** (Gemeinsamer Publikationenserver der Statistischen Ämter) — digitized historical
   *Statistische Berichte* series **B VII 2-5** ("Die Landtagswahl am … : endgültiges Ergebnis") plus older Wahlen-Sonderdienste:
   - Series page: https://www.statistischebibliothek.de/mir/receive/SHSerie_mods_00000485
   - Files served at `…/mir/servlets/MCRFileNodeServlet/SHHeft_derivate_NNNNNNNN/<file>.pdf`
3. **wahlen-sh.de** (Landeswahlleiterin Schleswig-Holstein) — live result portal; CSV export of the 2022 Landtagswahl
   (Wahlbezirk rows, each tagged with its Wahlkreis): https://www.wahlen-sh.de/ltw_2022/

All three are official. Each Wahlkreis-level statistical report (B VII 2-5 series) contains per-Wahlkreis tables of
Wahlberechtigte / Wähler / gültige / ungültige Stimmen and votes per party (Erst- and Zweitstimmen).

## Files downloaded

| Year | File | Format | Geo unit | Source |
|------|------|--------|----------|--------|
| 1954 | `SH_1954_Landtagswahl_Wahlkreis_Sonderdienst_Wahlen.pdf` | PDF | Wahlkreis (+Kreise) | statistischebibliothek.de SHHeft_derivate_00009938 (Sonderdienst Reihe Wahlen 7-80-9/54) |
| 1958 | `SH_1958_Landtagswahl_Wahlkreis_StatistikSH_Heft28.pdf` | PDF (scan) | Wahlkreis (+Kreise) | statistischebibliothek.de SHAusgabe_derivate_00000835 (Statistik von SH, Heft 28) |
| 1979 | `SH_1979_Landtagswahl_Wahlkreis_StatBericht_B_VII_2.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHHeft_derivate_00008893 (B VII 2-5/1979) |
| 1983 | `SH_1983_Landtagswahl_Wahlkreis_StatBericht_B_VII_2.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHHeft_derivate_00008895 (B VII 2-5/1983) |
| 1987 | `SH_1987_Landtagswahl_Wahlkreis_StatBericht_B_VII_2.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHHeft_derivate_00008894 (B VII 2-5/1987) |
| 1988 | `SH_1988_Landtagswahl_Wahlkreis_StatBericht_B_VII_2.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHHeft_derivate_00008896 (B VII 2-5/1988) |
| 1992 | `SH_1992_Landtagswahl_Wahlkreis_StatBericht_B_VII_2.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHHeft_derivate_00008897 (B VII 2-5/1992) |
| 1996 | `SH_1996_Landtagswahl_Wahlkreis_StatBericht_B_VII_2.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHHeft_derivate_00008898 (B VII 2-5/1996) |
| 2005 | `SH_2005_Landtagswahl_Bericht_B_VII_2_5_05.pdf` | PDF | Wahlkreis | statistik-nord.de (B VII 2-5/05) |
| 2009 | `SH_2009_Landtagswahl_Bericht_B_VII_2_5_09.pdf` | PDF | Wahlkreis | statistik-nord.de (B VII 2-5/09) |
| 2009 | `SH_2009_Landtagswahl_Endgueltige_Ergebnisse.xls` | XLS | Wahlkreis / Kreise / Wahlbezirk | statistik-nord.de (endgültige Ergebnisse workbook) |
| 2012 | `SH_2012_Landtagswahl_Wahlkreis_Erststimmen.xls` | XLS | Wahlkreis | statistik-nord.de (Inter_3_Erststimmen_e) |
| 2012 | `SH_2012_Landtagswahl_Wahlkreis_Zweitstimmen.xls` | XLS | Wahlkreis | statistik-nord.de (Inter_3_Zweitstimmen_e) |
| 2012 | `SH_2012_Landtagswahl_Endgueltiger_Bericht.pdf` | PDF | Wahlkreis | statistik-nord.de (endgültiger Bericht) |
| 2012 | `SH_2012_Landtagswahl_Wahlkreis_Wahlbericht.pdf` | PDF | Wahlkreis | statistischebibliothek.de SHMonografie_derivate_00000017 |
| 2017 | `SH_2017_Landtagswahl_Wahlkreis.xlsx` | XLSX | Wahlkreis (Erst+Zweit combined) | statistik-nord.de (Engültiges_Erg-WK) |
| 2017 | `SH_2017_Landtagswahl_Wahlkreis.csv` | CSV | Wahlkreis | statistik-nord.de (Engültiges_Erg-WK) |
| 2017 | `SH_2017_Landtagswahl_Wahlkreis_Erststimmen.xlsx` | XLSX | Wahlkreis | statistik-nord.de (LTW2017_Endg-Erststimmen) |
| 2017 | `SH_2017_Landtagswahl_Wahlkreis_Zweitstimmen.xlsx` | XLSX | Wahlkreis | statistik-nord.de (LTW2017_Endg-Zweitstimmen) |
| 2022 | `SH_2022_Landtagswahl_Wahlkreis_Erststimmen.xlsx` | XLSX | Wahlkreis | statistik-nord.de (e_Tab_81_LTW2022_SH) |
| 2022 | `SH_2022_Landtagswahl_Wahlkreis_Zweitstimmen.xlsx` | XLSX | Wahlkreis | statistik-nord.de (e_Tab_82_LTW2022_SH) |
| 2022 | `SH_2022_Landtagswahl_Wahlbericht.pdf` | PDF | Wahlkreis | statistik-nord.de (Wahlbericht endgültig) |
| 2022 | `SH_2022_Landtagswahl_Wahlbezirk_ergebnis-download.csv` | CSV | Wahlbezirk (carries Wahlkreis-Nr per row → aggregable to WK) | wahlen-sh.de (ltw_2022 ergebnis-download.csv) |

Notes:
- For 2017 and 2022 the machine-readable Wahlkreis tables come split into separate Erststimmen and Zweitstimmen
  files (plus, for 2017, one combined WK file in XLSX/CSV).
- 2009: the `Endgueltige_Ergebnisse.xls` workbook is the richest machine-readable file; it contains Wahlkreis,
  Kreis and Wahlbezirk sheets. The B VII 2-5/09 PDF is the narrative report.
- 2005 has **no machine-readable Wahlkreis file** published — only the PDF statistical report (which does contain
  the per-Wahlkreis tables).
- The 2022 Wahlbericht PDF on statistik-nord.de and on statistischebibliothek.de are byte-identical; only one copy is kept.

## Election years that exist but are NOT downloadable at Wahlkreis level

| Year | Reason | Where to look |
|------|--------|---------------|
| 1947 | First Landtagswahl (20 Apr 1947). Not digitized in any online archive found. | Statistisches Landesamt SH print archive; Landtag SH "Landtagsinformationssystem"; Landesarchiv SH |
| 1950 | Not digitized online. | as above |
| 1962 | Not digitized online (gap in statistischebibliothek.de B VII 2-5 holdings). | Statistisches Landesamt SH print archive |
| 1967 | Not digitized online. | as above |
| 1971 | Not digitized online. | as above |
| 1975 | Not digitized online. | as above |
| 2000 | B VII 2-5/2000 *Statistischer Bericht* exists but is NOT served (no derivate on statistik-nord.de nor a hit in the statistischebibliothek.de SOLR index; the candidate derivate path 404s). | Statistik Nord on request; possibly an offline B VII 2-5/2000 PDF |

The statistischebibliothek.de B VII 2-5 series (`SHSerie_mods_00000485`) digitizes only 1979, 1983, 1987, 1988,
1992, 1996, 2005, 2009 — all of which are captured here. 1954 and 1958 were recovered from the older
"Sonderdienst Reihe Wahlen" and "Statistik von Schleswig-Holstein" series respectively. 2012/2017/2022 come from
Statistik Nord's per-election download pages. No machine-readable or PDF Wahlkreis file for 1947, 1950, 1962, 1967,
1971, 1975 or 2000 is directly downloadable from the official portals as of the retrieval date.

_Retrieved: 2026-06-27._

## Completeness re-audit (second pass, 2026-06-27)

A second agent re-attempted every year the first pass gave up on (1947, 1950, 1962, 1967, 1971,
1975, 2000), exhaustively enumerating the statistischebibliothek.de MyCoRe holdings via the SOLR
`select` endpoint and probing statistik-nord.de's `fileadmin` tree. **No genuinely-new
Wahlkreis-level Landtagswahl file was found; nothing was added.** Details of the dead ends, so a
future pass need not repeat them:

- **B VII 2-5 series** (`SHSerie_mods_00000485`) has exactly 8 children: 1979, 1983, 1987, 1988,
  1992, 1996, 2005, 2009 (derivates 00008893–00008900) — all already captured. There is **no 2000
  Heft** and no pre-1979 Heft in this series.
- **Sonderdienst Reihe Wahlen** (`SHSerie_mods_00000481`) has exactly 3 children: 1951 (Kreistags-
  /Gemeindewahlen), 1953 (Kreistagswahl), 1954 (Landtagswahl — already captured). No other
  Landtagswahl.
- **Statistik von Schleswig-Holstein** (`SHSerie_mods_00000441`): Hefte 7–31, covering 1952–1960
  only. Heft 28 (1958 Landtagswahl) already captured; the series ends at 1960, so it cannot hold
  1962/1967/1971/1975, and starts at Heft 7 (1952), so it cannot hold 1950.
- **Fulltext SOLR** for "Landtagswahl <year> Wahlkreis" for 1962/1967/1971/1975 returns only
  *false positives*: the `G_IV_1_hj{n}_<year>.pdf` half-year health-statistics bundles
  ("Sommerhalbjahr"/"Winterhalbjahr" volumes) and the `1226-9-<year>.pdf` Statistisches Taschenbuch
  (state-level summaries, not Wahlkreis tables). None is an election report.
- **statistik-nord.de**: `…/Statistische_Berichte/B_VII_2/` serves only `B_VII_2_5_05_S.pdf` (2005);
  every probed 2000 filename variant 404s. Per-election download pages exist only for 2005→2027.
- **2000 is recoverable only indirectly**: the German Wikipedia article on the 2000 Landtagswahl
  cites the **2005** Statistischer Bericht (`SHHeft_derivate_00008899/B_VII_2_5_2005.pdf`,
  byte-identical to the locally-stored `SH_2005_Landtagswahl_Bericht_B_VII_2_5_05.pdf`,
  1 638 122 bytes) for the 2000 figures — i.e. the 2005 report carries the 2000 Wahlkreis results as
  its comparison column. There is no standalone 2000 Wahlkreis file anywhere on the official portals.

**Conclusion:** online coverage is complete at 1954 onward for every digitized Landtagswahl
(1954, 1958, 1979, 1983, 1987, 1988, 1992, 1996, 2005, 2009, 2012, 2017, 2022), with 2000's
Wahlkreis figures embedded in the 2005 report. The seven genuinely-missing years (1947, 1950, 1962,
1967, 1971, 1975, and a standalone 2000 file) are **not digitized at the constituency level by any
official online archive** and would require the Statistisches Landesamt SH / Landesarchiv SH print
holdings.

_Re-audited: 2026-06-27._

## Gap-fill (June 2026)

A third pass recovered the **2000** Landtagswahl at the constituency level — a year both prior
passes had marked unavailable. The official *amtliches Endergebnis* of the Landtagswahl on
**27 February 2000**, broken down by all **45** original 2000 Wahlkreise (the 35-seat constituency
map only began in 2005), was published by the Statistisches Landesamt Schleswig-Holstein on its
then-website `statistik-sh.de` as a set of 10 linked HTML tables in the `/m4/` ("Wahlen") section.
The live site is gone and statistik-nord.de only retains 2005+, but the pages survive in the
**Internet Archive Wayback Machine** (snapshots from 2002, captured shortly after the election, i.e.
the original-boundary results — *not* the recalculated-to-2005 version that other archived pages
carry). Each page was fetched via the Wayback `id_` raw-content endpoint and stored verbatim.

The 10 pages together are the complete official report: vote counts AND percentages for both
Erststimmen and Zweitstimmen per Wahlkreis, plus turnout, the d'Hondt seat calculation, and the list
of elected members. The Erststimmen table was spot-checked: 45 Wahlkreise (WK 1 Südtondern …
WK 45 Lauenburg-Süd) plus the statewide "Schleswig-Holstein" total row (gültige Erststimmen
1 449 908).

| Year | File | Format | Geo unit | Source |
|------|------|--------|----------|--------|
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_00_Uebersicht.htm` | HTML | overview / index | Wayback `web/20020623002646id_/http://www.statistik-sh.de/m4/M4_05_LW00.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_01_Wahlvorschlaege.htm` | HTML | party list (Wahlvorschläge) | Wayback `web/20021115044040id_/http://www.statistik-sh.de/m4/M4_05_LW00_1.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_02_Wahlberechtigte.htm` | HTML | Wahlkreis (Wahlberechtigte/Wähler) | Wayback `web/20020706235917id_/http://www.statistik-sh.de/m4/M4_05_LW00_2.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_03_Erststimmen_Anzahl.htm` | HTML | Wahlkreis (Erststimmen, counts) | Wayback `web/20020928083902id_/http://www.statistik-sh.de/m4/M4_05_LW00_3.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_04_Zweitstimmen_Anzahl.htm` | HTML | Wahlkreis (Zweitstimmen, counts) | Wayback `web/20020707194000id_/http://www.statistik-sh.de/m4/M4_05_LW00_4.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_05_Erststimmen_Prozent.htm` | HTML | Wahlkreis (Erststimmen, %) | Wayback `web/20020707192840id_/http://www.statistik-sh.de/m4/M4_05_LW00_5.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_06_Zweitstimmen_Prozent.htm` | HTML | Wahlkreis (Zweitstimmen, %) | Wayback `web/20020707192712id_/http://www.statistik-sh.de/m4/M4_05_LW00_6.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_07_Sitzverteilung_Parteien.htm` | HTML | parties at seat allocation | Wayback `web/20020707192816id_/http://www.statistik-sh.de/m4/M4_05_LW00_7.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_08_Sitzverteilung_Berechnung.htm` | HTML | d'Hondt seat calculation | Wayback `web/20020707193016id_/http://www.statistik-sh.de/m4/M4_05_LW00_8.htm` |
| 2000 | `SH_2000_Landtagswahl_Wahlkreis_StatLA_09_Gewaehlte_Bewerber.htm` | HTML | elected members (WK + list) | Wayback `web/20020707193348id_/http://www.statistik-sh.de/m4/M4_05_LW00_9.htm` |

Notes on this gap-fill:
- The core constituency vote tables are pages `_03`/`_04` (counts) and `_05`/`_06` (percentages);
  `_02` carries Wahlberechtigte/Wähler/turnout per Wahlkreis. These four/five files are the raw
  series for the GERDA Wahlkreis pipeline. Pages `_00`/`_01`/`_07`/`_08`/`_09` are kept for
  completeness (party labels, seat allocation, elected members).
- Files are HTML, not PDF/XLSX, because the Statistisches Landesamt SH never issued a downloadable
  B VII 2-5/2000 document — these web tables ARE the official endgültiges Ergebnis. Charset is
  Latin-1; German numbers use a thin-space thousands separator and `&#150;` (en-dash) for zero/none.
- The 45-Wahlkreis layout (1950–2003 era) differs from the 35-Wahlkreis map used since 2012; the
  prior README note already records the changing constituency counts.

Still NOT obtained at constituency level (unchanged — no online source exists): **1947, 1950, 1962,
1967, 1971, 1975**. The very early postwar reports (1947/1950) and the 1962–1975 Sonderdienst Reihe
Wahlen Hefte are not digitized in statistischebibliothek.de, were never on statistik-sh.de's `/m4/`
web section (which begins at 1998), and are absent from the Wayback Machine. They survive only as
printed Hefte in the Statistisches Landesamt SH / Statistikamt Nord library and the Landesarchiv
Schleswig-Holstein; recovery would require a physical scan request (e.g. via ZBW/GBV interlibrary
loan).

_Gap-fill retrieved: 2026-06-27._

## Second gap-fill pass (June 2026) — 1947/1950/1962/1967/1971/1975

A further pass re-attacked the six pre-1979 years using sources the earlier passes had not
exhausted (Landeswahlleiterin SH / Landtag historical archive, Wikimedia Commons, GBV/ZBW,
domain-wide Wayback CDX of statistik-sh.de, and a fresh full enumeration of the SH MyCoRe
holdings). **Nothing new was found; no file was added.** Dead ends, recorded so a future pass
need not repeat them:

- **statistischebibliothek.de — complete SH enumeration.** A SOLR query `q=Landtagswahl &
  fq=id:SH*` returns exactly 17 SH objects: the B VII 2-5 series + its 8 Hefte (1979–2009), the
  Statistik-von-SH Heft 28 (1958), the Sonderdienst Reihe Wahlen Hefte (1951 Kreistags-/Gemeinde,
  1953 Kreistag, 1954 Landtag), and the two 2022 Wahlbericht monographs. Verifying each MODS record
  confirms NONE corresponds to 1947, 1950, 1962, 1967, 1971 or 1975. These years are simply not in
  the digitized holdings.
- **Report series identified but print-only.** Per bibliographic citations the missing reports are
  *Statistische Berichte* **B III 2-5/62**, **B III 2-5/1967** and **B III 2-6/1971** (the old
  "B III" Wahlen code, predating the "B VII" renumbering), plus a 1975 B-series Heft; 1947/1950 were
  issued by the Statistisches Landesamt Kiel even earlier. statistischebibliothek.de digitizes the
  B VII 2-5 series only from 1979, and holds no B III Wahlen Hefte at all.
- **statistik-sh.de Wayback (domain-wide CDX, ~2046 captured URLs).** The archived election tables
  cover ONLY the web era: `LW00` (2000, already recovered), `LW05` (2005), `BW02` (2002 Bundestag),
  `KW03` (2003 Kommunal), plus the generic `LA/Aktuell/LWnn.htm` per-Wahlkreis pages of the then-
  current election. There is no `LW62`/`LW67`/`LW71`/`LW75` path — the office never put pre-2000
  Landtagswahl results on the web.
- **Wikimedia Commons / GBV / ZBW.** No scanned Wahlergebnis-Heft for any of the six years is hosted;
  catalog records point only to the physical print Hefte.

The six years survive solely as printed Hefte in the Statistikamt Nord / Statistisches Landesamt SH
library and the Landesarchiv Schleswig-Holstein; recovery requires a physical scan / interlibrary
loan, not an online download.

_Second gap-fill: 2026-06-27._
