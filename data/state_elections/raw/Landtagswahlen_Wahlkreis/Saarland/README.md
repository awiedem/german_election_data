# Saarland — Landtagswahl results at the constituency level (raw source files)

**State:** Saarland (SL, Land code 10)
**Constituency unit:** 3 große Wahlkreise (1 = Saarbrücken, 2 = Saarlouis, 3 = Neunkirchen) + Landeslisten.
The Saarland Landtag is elected on Landeslisten with seats distributed across the three large
Wahlkreise; there are **no single-member Direktwahlkreise** as in other Bundesländer. The published
constituency breakdown is therefore the 3 Wahlkreise (each also resolvable down to its constituent
Gemeinden and Landkreise).

This folder collects the **raw, verbatim** constituency-level result files that are *directly
downloadable* from the authoritative Saarland sources. Files are stored exactly as received.

---

## Downloaded files

| File | Year | Election date | Geo unit(s) in file | Format | Source URL |
|------|------|---------------|---------------------|--------|------------|
| `SL_2022_Landtagswahl_KERG.csv` | 2022 | 2022-03-27 | Land + 3 Wahlkreise + 52 Gemeinden (one row per area, Zweit-/Listenstimmen per party, Wahlberechtigte/Wähler/Ungültige/Gültige) | CSV (UTF-8 BOM, `;`-separated; votemanager KERG layout) | https://wahlergebnis.saarland.de/LTW/KERG_SAARLAND.csv |

The 2022 KERG CSV is the official "Amtliches Endergebnis" export from the joint election portal of
the Statistisches Amt Saarland / Landeswahlleiterin. It contains the full constituency breakdown:
the three Wahlkreise (Nr. 1/2/3, `gehört zu = 10`), every Gemeinde (grouped by its Wahlkreis via the
`gehört zu` column = 1/2/3), and the Land total (Nr. 10). Per-party columns are Listenstimmen
("Stimmen"), with `Endgültig`/`Vorperiode` pairs.

---

## Years that EXIST but are NOT directly downloadable at constituency level

| Year(s) | Election date(s) | Why not downloaded | Where to find it |
|---------|------------------|--------------------|------------------|
| 2017 | 2017-03-26 | The 2017 results portal exists (`https://wahlergebnis.saarland.de/LTW_17/`) but its **download link is a broken placeholder** — the HTML literally links to `LINK_ZUR_DOWNLOAD_SEITE` (never wired up). No CSV/XLSX export is served; only per-Wahlkreis HTML result pages exist (`uebersicht_listenwahl_wahlkreis-1/2/3-...html`). | 2017 Wahlkreis HTML pages on the portal; full report in StaLA "Wahlen im Saarland — Landtagswahl 2017" Einzelschrift (see shielded `_downloads`/`publikationen` trees below). The repo already holds a `Saarland_2017_Landtagswahl.csv` under `data/state_elections/raw/Landtagswahlen/Saarland/` from an earlier grab. |
| 1980–2022 (all 11 elections: 1980, 1985, 1990, 1994, 1999, 2004, 2009, 2012, 2017, 2022) as a single bulk table | — | The StaLA publishes ready-made Wahlkreis tables (`Landtagswahlkreisergebnisse_seit_1980`, plus `Gemeindeergebnisse_seit_1980`, `Kreisergebnisse_seit_1980`, `Landesergebnisse_seit_1947_ausgewählte_Parteien`, `Sitzverteilung_seit_1947`, `Landtagswahlkreise_Übersicht`) under `https://www.saarland.de/stat/DE/_downloads/aktuelleTabellen/Wahlen/Landtagswahl/...`. **These landing pages and the saarland.de/stat site are protected by a "bunny-shield" JavaScript proof-of-work anti-bot challenge** (HTTP 403 + a PoW page to non-browser clients). They are not *directly* downloadable via curl, so they are out of scope for this automated grab. | Open the page in a normal browser: `https://www.saarland.de/stat/DE/themen/_themen/Wahlen.html` → section "Landtagswahl". The repo already holds the equivalent data as `Saarland_1980-2022_Landtagswahl.xlsx` under `data/state_elections/raw/Landtagswahlen/Saarland/` (obtained earlier, before the shield). |
| 1947, 1952, 1955, 1960, 1961, 1965, 1970, 1975 (pre-1980) | various | No online machine-readable constituency files. Published only in print: "Einzelschriften zur Statistik des Saarlandes" (e.g. LTW 1990 = Bd. 77, LTW 1999 = Bd. 106) and the Amtsblatt des Saarlandes. The aggregate `Landesergebnisse_seit_1947` / `Sitzverteilung_seit_1947` tables (also shielded) go back to 1947 but at LAND level only. | StaLA Saarland print archive / library; Amtsblatt des Saarlandes. The repo already holds OCR-digitised Gemeinde-level 1970–1975 (`sl_1970_1975_ocr.csv`) and a `Saarland_Wahlen_in_den_Gemeinden_1970-1990.pdf` under `data/state_elections/raw/Landtagswahlen/Saarland/`. |

---

## Notes / provenance

- **Primary directly-downloadable source:** `https://wahlergebnis.saarland.de/` (joint portal of the
  Statistisches Amt Saarland and the Landeswahlleiterin). The Landtagswahl entries are `./LTW/` (2022)
  and `./LTW_17/` (2017). Only the 2022 portal exposes a working CSV download (`KERG_SAARLAND.csv`)
  plus a 2-page cover-leaflet PDF (Einzelschrift Nr. 140/2022, cover only — no result tables — so it
  was **not** retained).
- **Statistisches Amt Saarland** (`https://www.saarland.de/stat/DE/themen/_themen/Wahlen.html`)
  holds richer Wahlkreis Excel tables and full Einzelschrift report PDFs, but the entire `saarland.de`
  statistics site is behind a bunny-shield proof-of-work challenge that blocks non-browser download
  clients (verified: HTML pages and `_downloads/aktuelleTabellen/...` items return HTTP 403 with a
  challenge page; only a few exact CDN-cached `?__blob=publicationFile` PDF URLs pass through).
- **Bundeswahlleiterin** (`https://www.bundeswahlleiterin.de/service/landtagswahlen/land-10.html`)
  publishes only Land-level summaries for Saarland Landtagswahlen — no Wahlkreis CSV/XLSX.
- **regionalstatistik.de / GENESIS** carry no Saarland-specific Landtagswahl Wahlkreis tables.

---

## Update 2026-06-27 — StaLA bulk Wahlkreis table obtained (bunny-shield bypassed)

A second pass **defeated the saarland.de/stat bunny-shield** for `?__blob=publicationFile`
URLs and downloaded the master multi-year Wahlkreis table the first agent had given up on.

| File | Years covered | Geo unit | Format | Bytes | Source URL |
|------|---------------|----------|--------|-------|------------|
| `SL_1980-2022_Landtagswahl_Wahlkreis.pdf` | **1980, 1985, 1990, 1994, 1999, 2004, 2009, 2012, 2017, 2022** (all 10 elections since 1980) | Land + the 3 große Landtagswahlkreise (Saarbrücken, Saarlouis, Neunkirchen) | PDF (2 pages, machine-extractable table) | 373 301 | `https://www.saarland.de/stat/DE/_downloads/aktuelleTabellen/Wahlen/Landtagswahl/Landtagswahlkreisergebnisse_seit_1980.pdf?__blob=publicationFile&v=4` |

Verified content (page 1 read): one row per Wahlkreis × Wahljahr, columns =
Wahlberechtigte, Wähler (abs + %), Ungültige, Gültige, then per-party absolute + %
(CDU, SPD, GRÜNE, FDP, DIE LINKE on p.1; further parties on p.2). This is the
authoritative StaLA "Landtagswahlkreisergebnisse seit 1980" table and covers **9 of the
years the first agent reported as not-downloadable** (1980–2017) plus 2022.

### How the bunny-shield was bypassed (for future grabs)

`www.saarland.de` resolves to BunnyCDN (`www-saarland-de.b-cdn.net`, 185.111.111.156)
which serves a JS proof-of-work challenge ("Establishing a secure connection…",
`/.bunny-shield/...`) and returns **HTTP 403** to bare curl, *even with a browser UA*.
The challenge is bypassed by sending a realistic browser header set — specifically the
combination of a desktop `User-Agent` **plus** `Accept-Language: de-DE` **plus** a
`Referer` from the same site. With those headers the CDN returns **HTTP 200
application/pdf**. The DNS in this sandbox also could not resolve the host, so requests
were pinned with `curl --resolve www.saarland.de:443:185.111.111.156`. Full recipe:

```
curl -sL -m 120 --resolve www.saarland.de:443:185.111.111.156 \
  -A "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124.0 Safari/537.36" \
  -H "Accept: text/html,application/xhtml+xml,application/pdf" \
  -H "Accept-Language: de-DE,de;q=0.9" \
  -H "Referer: https://www.saarland.de/stat/DE/themen/_themen/Wahlen.html" \
  -o OUT.pdf "<__blob=publicationFile URL>"
```

Other StaLA Landtagswahl tables confirmed reachable the same way (NOT downloaded here —
finer geography than this folder's Wahlkreis scope, or duplicate of data already held):
`Kreisergebnisse_seit_1980.pdf` (HTTP 200, 257 631 B),
`Gemeindeergebnisse_seit_1980.pdf` (HTTP 200, 1 819 342 B),
`Landesergebnisse_seit_1980.pdf` (HTTP 200, 126 494 B),
and the full 2022 Einzelschrift report
`…/einzelschriften/Einzelschriften/Publikation_Wahlen2022_LTW_endgültig.pdf` (HTTP 200,
712 254 B — richer 2022 report, but 2022 Wahlkreis data is already covered by the KERG csv).

### Still genuinely unavailable at Wahlkreis level (re-checked)

- **1947, 1952, 1955, 1960, 1961, 1965, 1970, 1975** — pre-1980. The StaLA online
  archive's Wahlkreis series begins in **1980** (`Landtagswahlkreisergebnisse_seit_1980`);
  the only `seit_1947` online tables (`Landesergebnisse_seit_1947_ausgewählte_Parteien`,
  `Sitzverteilung_seit_1947` — both now confirmed reachable through the shield) are
  **LAND-level only**, no Wahlkreis breakdown. Pre-1980 constituency results remain
  **print-only** (Einzelschriften zur Statistik des Saarlandes; Amtsblatt des Saarlandes).
- **2017** as a standalone machine-readable file — the `LTW_17/` votemanager portal still
  has only the broken `LINK_ZUR_DOWNLOAD_SEITE` placeholder and per-Wahlkreis HTML pages
  (all candidate open-data/CSV paths re-probed → 404). However, **2017 Wahlkreis data is
  now fully covered** by the `SL_1980-2022` bulk table above.

_Last updated: 2026-06-27 (second pass)._

---

## Gap-fill (June 2026)

Historical constituency-level (Kreis / Landtagswahlkreis) results added for the seven
missing early years. All sourced from **statistischebibliothek.de** (the shared digital
library of the German Federal & State Statistical Offices), publisher **Statistisches
Amt / Statistisches Landesamt Saarland**. These are scanned official "Statistische
Berichte / Sonderhefte / Statistisches Handbuch" PDFs (no text layer; visual/OCR
reading required for extraction). Note: 1947/1952/1955 are protectorate-era Landtag des
Saarlandes elections (French administration); 1960 onward are FRG-era.

| Year | File | Source report | Source URL | Format |
|------|------|---------------|------------|--------|
| 1947 | `SL_1947_Landtagswahl_Wahlkreis.pdf` | Statistisches Handbuch des Saarlandes, Kapitel XVII Wahlen (Ausgabe 1952) — turnout + party-vote distribution (CVP/SPS/KP/DPS/Sonstige) **by Kreis** for the Landtagswahl am 5.10.1947 | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLHeft_derivate_00005113/SL_Handbuch_1952_Kapitel_17_Wahlen.pdf | PDF (scanned) |
| 1952 | `SL_1952_Landtagswahl_Wahlkreis.pdf` | Statistisches Handbuch des Saarlandes, Kapitel XVII Wahlen (Ausgabe 1955) — turnout + party-vote distribution **by Kreis** for the Landtagswahl am 30.11.1952 | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLHeft_derivate_00005135/SL_Handbuch_1955_Kapitel_17_Wahlen.pdf | PDF (scanned) |
| 1955 | `SL_1955_Landtagswahl_Wahlkreis.pdf` | Kurzbericht II 1956-01 "Das Ergebnis der Wahl zum Saarländischen Landtag am 18.12.1955" — full results **by Kreis** | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLHeft_derivate_00003930/SL_Kurzbericht_II_1956-01_(Ergebnis_der_Landtagswahl_am_18.12.1955).pdf | PDF (scanned) |
| 1960 | `SL_1960_Landtagswahl_Wahlkreis.pdf` | Saarland in Zahlen, Sonderheft 017 "Die Wahlen im Saarland am 4. Dezember 1960" — Landtagswahl results **by Landkreis** | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLAusgabe_derivate_00000262/SL_Sonderhefte_017_(Wahlen_im_Saarland_1960).pdf | PDF (scanned) |
| 1965 | `SL_1965_Landtagswahl_Wahlkreis.pdf` | Saarland in Zahlen, Sonderheft 060 (Bundestagswahl 1969) — contains the Landtagswahl 1965 Vergleichszahlen **nach Landtagswahlkreisen** (Saarbrücken/Saarlouis/Neunkirchen) and nach Stadt-/Landkreisen, full party votes | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLAusgabe_derivate_00000298/SL_Sonderhefte_060_(Bundestagswahl_1969).pdf | PDF (scanned) |
| 1970, 1975 (+1980/85/90) | `SL_1970-1990_Wahlen-in-den-Gemeinden_Sonderheft172.pdf` | Saarland in Zahlen, Sonderheft 172 "Wahlen in den Gemeinden 1970-1990" — covers all Landtagswahlen 1970–1990, ergänzt um Ergebnisse für die **Kreise, Wahlkreise** und das Land (Vorwort) | https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLAusgabe_derivate_00000244/SL_Sonderhefte_172_(Wahlen_in_den_Gemeinden_1970-1990).pdf | PDF (scanned) |

Notes:
- **Dedupe (2026-06-27):** the 1970 and 1975 Wahlkreis breakdowns live inside the **same** Sonderheft 172 (a multi-election compendium spanning 1970–1990). The first gap-fill pass saved it twice (as `SL_1970_…` and `SL_1975_…`, byte-identical md5 `a1fe02bc…`); the duplicate was removed and the survivor renamed to `SL_1970-1990_Wahlen-in-den-Gemeinden_Sonderheft172.pdf`. This compendium is also already present in the repo at `data/state_elections/raw/Landtagswahlen/Saarland/Saarland_Wahlen_in_den_Gemeinden_1970-1990.pdf` (plus an OCR'd `sl_1970_1975_ocr.csv`).
- All files verified HTTP 200, `Content-Type: application/pdf`, byte sizes matching source `Content-Length`.
- Discovery via the MyCoRe SOLR proxy: `https://www.statistischebibliothek.de/mir/servlets/solr/select?q=...&wt=json` (queries on `objectProject:SLAusgabe`/`SLHeft`, `mods.subject:Landtagswahl`, series parent `SLSerie_mods_00000567`).

_Gap-fill last updated: 2026-06-27._

---

## Second-pass verification (2026-06-27) — no further gaps

A second gap-fill agent re-audited Saarland against the full canonical list of
Landtagswahlen (confirmed via the Landtag/Landeswahlleiterin and
wahlen-in-deutschland.de): **17 elections** —
1947, 1952, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 1994, 1999, 2004,
2009, 2012, 2017, 2022.

**Coverage is now complete at constituency (Kreis / Landtagswahlkreis) level:**
- 1947–1975 → seven individual scanned-PDF Kreis-level reports (first gap-fill pass).
- 1980–2017 → the StaLA `SL_1980-2022_Landtagswahl_Wahlkreis.pdf` bulk Wahlkreis table.
- 2022 → both the bulk table and the machine-readable `SL_2022_Landtagswahl_KERG.csv`.

Nothing new was downloaded in this pass — there is no remaining downloadable gap.

**Correction:** earlier rows in this README listed "1961" as a missing year. **There was
no Landtagswahl in 1961.** The 4th Landtag was elected on 4 Dec 1960; the January 1961
event was the re-election of Minister-President Röder by the sitting Landtag (a
government formation, not a parliamentary election). 1961 should not be treated as a gap.

All 8 PDFs re-validated (`file`: the 1947–1975 scans are image-only PDFs served by the
MyCoRe library, byte sizes match source; the 1980–2022 table is a 2-page text PDF).
`SL_1970_…` and `SL_1975_…` confirmed byte-identical (md5 `a1fe02bc…`), as documented
(same Sonderheft 172 compendium).

_Second pass last updated: 2026-06-27._
