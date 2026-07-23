# Bayern — Landtagswahl results at constituency level (Stimmkreis)

**State:** Bayern (Bavaria), state abbreviation **BY**, AGS state prefix `09`.

**Constituency unit:** **Stimmkreis** (91 Stimmkreise as of 2023/2018). Bavaria's
constituencies are called *Stimmkreise*. They are nested inside the 7 **Wahlkreise**
(= the Regierungsbezirke: Oberbayern, Niederbayern, Oberpfalz, Oberfranken,
Mittelfranken, Unterfranken, Schwaben), which serve as the seat-allocation bodies.
Bavaria uses a two-vote system: **Erststimme** (Direktstimme, candidate in the
Stimmkreis) + **Zweitstimme** (Listenstimme, party list at the Wahlkreis level); the
*Gesamtstimmen* = Erst + Zweit.

This folder collects, for every Landtagswahl, the raw source files that report results
**broken down by Stimmkreis** (one row/section per Stimmkreis with Erst-/Zweitstimmen
per party, turnout, valid/invalid votes). For the two most recent elections (2018,
2023) machine-readable files (CSV/XLSX/XML) exist; for 1970–2013 the constituency-level
results are published only in the official *Statistische Berichte* PDFs.

---

## Primary sources

- **Bayerisches Landesamt für Statistik — Landtagswahlen / Ergebnisse**
  https://www.statistik.bayern.de/wahlen/landtagswahlen/ergebnisse/index.html
- **Official 2023 results portal (Landeswahlleiter):** https://www.landtagswahl2023.bayern.de/downloads.html
- **Official 2018 results portal (now bot-blocked; via Internet Archive):** https://www.landtagswahl2018.bayern.de/downloads.html
- **Statistische Bibliothek (DESTATIS/MyCoRe) — historical Statistische Berichte B VII 2-4:**
  https://www.statistischebibliothek.de/  (Heft series "Statistische Berichte / B / VII / 2-4")
- **GENESIS-Online Bayern, table 14311 (Landtagswahlen seit 1946, interactive):**
  https://www.statistikdaten.bayern.de/genesis/online?code=14311

---

## Downloaded files

### Machine-readable (recent elections)

| Year | File | Format | Geo unit | Source |
|------|------|--------|----------|--------|
| 2023 | `BY_2023_Landtagswahl_Stimmkreise.csv` | CSV (`;`, ISO-8859-1) | Stimmkreis (91 + header) | landtagswahl2023.bayern.de |
| 2023 | `BY_2023_Landtagswahl_Stimmkreise.xlsx` | XLSX | Stimmkreis | landtagswahl2023.bayern.de |
| 2023 | `BY_2023_Landtagswahl_Wahlkreise.csv` | CSV | Wahlkreis (7 + Bayern total) | landtagswahl2023.bayern.de |
| 2023 | `BY_2023_Landtagswahl_Wahlkreise.xlsx` | XLSX | Wahlkreis | landtagswahl2023.bayern.de |
| 2023 | `BY_2023_Landtagswahl_Bayern.xml` | XML | all levels (Bayern/WK/SK) | landtagswahl2023.bayern.de |
| 2023 | `BY_2023_Landtagswahl_Stimmbezirksergebnisse.xlsx` | XLSX | Stimmbezirk (polling district; nests Stimmkreis) | statistik.bayern.de |
| 2018 | `BY_2018_Landtagswahl_Stimmkreise.csv` | CSV | Stimmkreis (91 + header) | Internet Archive of landtagswahl2018.bayern.de |
| 2018 | `BY_2018_Landtagswahl_Stimmkreise.xlsx` | XLSX | Stimmkreis | Internet Archive of landtagswahl2018.bayern.de |
| 2018 | `BY_2018_Landtagswahl_Wahlkreise.csv` | CSV | Wahlkreis (7 + Bayern total) | Internet Archive of landtagswahl2018.bayern.de |
| 2018 | `BY_2018_Landtagswahl_Wahlkreise.xlsx` | XLSX | Wahlkreis | Internet Archive of landtagswahl2018.bayern.de |
| 2018 | `BY_2018_Landtagswahl_Bayern.xml` | XML | all levels | Internet Archive of landtagswahl2018.bayern.de |
| 2018 | `BY_2018_Landtagswahl_Stimmbezirksergebnisse.xlsx` | XLSX | Stimmbezirk (nests Stimmkreis) | statistik.bayern.de |

The dedicated 2018 portal (`landtagswahl2018.bayern.de`) now returns HTTP 403 for all
direct requests; its data files were retrieved verbatim from the Wayback Machine snapshot
of 2023-10-25 (raw `id_` endpoint, byte-identical to the original). Filenames on the
source server were `14_10_2018_Landtagswahl_2018_Stimmkreise_Bayern.{csv,xlsx}` etc.

### Statistische Berichte PDFs (B VII 2-4 = endgültiges Ergebnis with Stimmkreis tables)

One main report per election year 1970–2023, plus the regional `/G` (Gemeinden &
Stimmkreise) variant where it is a separate, compact file (2008, 2013). All from the
Statistische Bibliothek MyCoRe file servlet.

| Year | File | Series | Pages/size |
|------|------|--------|-----------|
| 1970 | `BY_1970_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 24.8 MB (scan) |
| 1974 | `BY_1974_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 28.4 MB (scan) |
| 1978 | `BY_1978_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 32.3 MB (scan) |
| 1982 | `BY_1982_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 50.8 MB (scan) |
| 1986 | `BY_1986_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 57.0 MB (scan) |
| 1990 | `BY_1990_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 30.5 MB (scan) |
| 1994 | `BY_1994_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 48.6 MB (scan) |
| 1998 | `BY_1998_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 67.5 MB (scan) |
| 2003 | `BY_2003_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 | 57.7 MB (scan) |
| 2008 | `BY_2008_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 (B7241C) | 234 p, 9.3 MB |
| 2008 | `BY_2008_Landtagswahl_StatBericht_BVII2-4G_Regionalergebnisse.pdf` | B VII 2-4 /G (B7242C) | 10.4 MB |
| 2013 | `BY_2013_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 (B7241C) | 10.8 MB |
| 2013 | `BY_2013_Landtagswahl_StatBericht_BVII2-4G_Regionalergebnisse.pdf` | B VII 2-4 /G (B7242C) | 10.4 MB |
| 2018 | `BY_2018_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 (B7241C) | 14.2 MB |
| 2023 | `BY_2023_Landtagswahl_StatBericht_BVII2-4.pdf` | B VII 2-4 (B7241C) | 24.1 MB |

Report-series legend (B VII 2-4 sub-variants seen in the archive): no suffix /
main = endgültiges Ergebnis (Stimmkreis + Wahlkreis tables); `/G` = Gemeinden &
Stimmkreise regional results; `/T` = Text/Tabellen/Schaubilder; `/S` = Strukturdaten;
2-3 = vorläufiges Ergebnis; 2-5 = repräsentative Wahlstatistik; 2-0/2-1 =
Wahlvorschläge/Bewerber. Only the constituency-bearing reports (2-4 family) were grabbed.

---

## Years that exist but were NOT downloaded at constituency level

| Year(s) | Reason | Where to find |
|---------|--------|---------------|
| 1946, 1950, 1954, 1958, 1962, 1966 | No per-year Stimmkreis-level Statistische-Bericht PDF is digitized in the Statistische Bibliothek (the earliest digitized Landtagswahl Heft is 1970). Only aggregate "1946 bis 2008/2013" time-series Hefte exist online. | GENESIS-Online Bayern table **14311** ("Ergebnisse der Landtagswahlen seit 1946", interactive table builder with a Stimmkreis/regional dimension) at https://www.statistikdaten.bayern.de/genesis/online?code=14311 ; or the original print *Statistische Berichte / Beiträge zur Statistik Bayerns* held at the Bayerisches Landesamt für Statistik (Fürth). |

Notes:
- The constituency boundaries (Stimmkreis-Einteilung) were redrawn over time; the
  91-Stimmkreis layout applies to 2018 and 2023. Earlier elections had different
  Stimmkreis counts/boundaries — use each year's own report definitions.
- GENESIS table 14311 is interactive-portal-only: it has no stable direct-download URL
  without building a query/session, so it is documented here rather than downloaded.
- The `Stimmbezirksergebnisse` XLSX files (2018, 2023) are polling-district level (much
  finer than Stimmkreis) and include the Stimmkreis key for each row; kept as the most
  granular machine-readable source.

---

## Completeness-critic additions (2026-06-27) — historical pre-1970 volumes

A first pass treated 1970 as the earliest obtainable year. That was wrong: the official
**"Beiträge zur Statistik Bayerns"** monograph series (Bayerisches Statistisches
Landesamt) digitized on statistischebibliothek.de carries the full historical
Landtagswahl reports, each reporting results down to the smallest Gemeinde and giving
the **Stimmkreisstimmen of every Bewerber** (constituency level). Four previously-missing
years were recovered from this series and added here:

| Year | File | Source (Heft) | statistischebibliothek.de derivate | Bytes |
|------|------|---------------|-------------------------------------|-------|
| 1950 | `BY_1950_Landtagswahl_Wahlkreis.pdf` | Beiträge z. Statistik Bayerns, **Heft 163** — Wahl zum Bayerischen Landtag am 26.11.1950 | `BYMonografie_derivate_00001466/163_gesamt_1.1.pdf` | 38,658,589 |
| 1958 | `BY_1958_Landtagswahl_Wahlkreis.pdf` | Beiträge z. Statistik Bayerns, **Heft 211** — Wahl zum 19. Bayerischen Landtag am 23.11.1958 | `BYMonografie_derivate_00001068/Wahl zum Bay. Landtag Heft 211.pdf` | 56,270,623 |
| 1962 | `BY_1962_Landtagswahl_Wahlkreis.pdf` | Beiträge z. Statistik Bayerns, **Heft 237** — Wahl zum Bayerischen Landtag am 25.11.1962 | `BYMonografie_derivate_00001066/Heft 237.pdf` | 50,051,725 |
| 1966 | `BY_1966_Landtagswahl_Wahlkreis.pdf` | Beiträge z. Statistik Bayerns, **Heft 277 a** — Wahl zum Bayerischen Landtag am 20.11.1966 (Teil 1: Gemeindeergebnisse und Bewerberstimmen) | `BYMonografie_derivate_00001064/Heft 277a Beiträge.pdf` | 52,218,740 |

Base URL for all four: `https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/<derivate>/<file>`.
All verified HTTP 200, `application/pdf`; downloaded sizes match Content-Length exactly.
Content confirmed to be constituency-level (1962 Vorwort: *"Erstmals werden u. a. die
Stimmkreisstimmen eines jeden Bewerbers … aufgegliedert"*).

### Still missing at constituency level (genuinely not digitized)
- **1946** (1st Landtag, 1.12.1946): no dedicated digitized constituency-level monograph
  exists. The series carries only retrospective "1946 bis …" comparison summary Hefte
  (e.g. `BYHeft_derivate_00000656`), not per-Stimmkreis 1946 results.
- **1954** (3rd Landtag, 28.11.1954): published as **Beiträge z. Statistik Bayerns,
  Heft 201**, but only the table of contents is digitized (Deutsche Digitale Bibliothek);
  no full-text PDF is available on statistischebibliothek.de. Would require library
  scanning of the print volume.

Earliest year now obtained at constituency level: **1950**. Coverage gap remaining:
1946 and 1954 only (vs. theoretical earliest ~1946).

---

## Second-pass gap-fill (2026-06-27) — 1946 & 1954 recovered via the Statistisches Jahrbuch

The previous note declared 1946 and 1954 "genuinely not digitized." **Both are in fact
recoverable** — not from the dedicated *Beiträge* Wahl-Hefte (which are indeed missing
for those years), but from the **Statistisches Jahrbuch für Bayern**, also fully digitized
on statistischebibliothek.de (series `BYSerie_mods_00000917`, all editions from 1894).
Each annual Jahrbuch carries a Chapter XVII/XXIII "Wahlen" that reports the year's
Landtag election **by Stimmkreis** (one row per Stimmkreis, grouped under the 7
Wahlkreise = Regierungsbezirke), with party votes (Zahl + vH) and turnout.

| Year | File | Source volume | derivate / file | Bytes |
|------|------|---------------|-----------------|-------|
| 1946 | `BY_1946_Landtagswahl_Wahlkreis_StatJahrbuch1947.pdf` | Statistisches Jahrbuch für Bayern **1947** (23. Jg.), `BYHeft_mods_00013011`. Chapter XVII "Wahlen", print pp. 316–347 (PDF pages ~300–315). | `BYHeft_derivate_00007231/Statistisches Jahrbuch 1947.pdf` | 76,145,425 |
| 1954 | `BY_1954_Landtagswahl_Wahlkreis_StatJahrbuch1955.pdf` | Statistisches Jahrbuch für Bayern **1955**, `BYHeft_mods_00013036`. Chapter XXIII "Wahlen", tables 5 ("Die Wahlen seit 1950 nach Stimmkreisen und Stimmkreisverbänden", c = Landtagswahl 1954) and 6 ("Wahlberechtigte und abgegebene Stimmen zur Landtagswahl am 28. November 1954"), print pp. 384–393 (PDF pages ~372–380). | `BYHeft_derivate_00007260/Statistisches Jahrbuch 1955.pdf` | 91,275,082 |

Base URL: `https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/<derivate>`.
Both verified HTTP 200, `application/pdf`; downloaded sizes match Content-Length exactly.

**Content notes / caveats:**
- **1946**: the Jahrbuch covers BOTH 1946 ballots at Stimmkreis level — the
  *Verfassunggebende Landesversammlung* (30 June 1946) and the *1. Bayerischer Landtag*
  (1 December 1946) — each with full party columns (Christlich-Soziale, Sozialdemokratische,
  Bayernpartei/Landesaufbau, KPD, Demokratische/FDP …). Use the **1 December 1946** tables
  (Section 6, PDF p. ~310) for the Landtag election proper.
- **1954**: Jahrbuch table 5 is a Stimmkreis × major-party matrix (gültige Stimmen +
  SPD/CSU/BP/GB-BHE/FDP …, Zahl & vH); table 6 adds Wahlberechtigte/abgegebene Stimmen
  per Stimmkreis. This is genuine constituency-level coverage, but **slightly less
  exhaustive** than the un-digitized dedicated Heft 201 would be (the Heft also lists
  every individual Bewerber's Stimmkreisstimmen and Gemeinde-level breakdowns). For
  Stimmkreis-level party results it is complete.
- Both PDFs are scans with a noisy OCR text layer (digits interleave across columns);
  read the table images visually (600 DPI crops) for extraction, as with the 1950–1966
  Heft scans already in this folder.

After this pass the Bayern constituency-level series is **complete for every Landtag
election (1946 → 2023)** with no remaining gaps.

---

*Compiled automatically for the GERDA election database. Raw files are stored verbatim;
do not modify.*
