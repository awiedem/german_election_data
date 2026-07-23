# Landtagswahlen — Wahlkreis (constituency) level — RAW

State-parliament election results (Landtagswahlen; *Abgeordnetenhauswahl* in Berlin, *Bürgerschaftswahl* in
Hamburg & Bremen) broken down by **constituency** — the geographic unit below the state and below the
Kreis: **Wahlkreis** in most states, **Stimmkreis** in Bayern, **Wahlbereich/Stadtteil/Ortsteil** in Bremen
(which has no single-member districts), **Wahlbezirk** for early Hamburg.

This is a **new geographic level** distinct from the existing Gemeinde-level data in
`../Landtagswahlen/`. It is kept in a parallel tree exactly like the federal
`municipality_level` / `county_level` split. **Raw files are stored verbatim** as received from the
official sources; cleaning/harmonization is a later pipeline stage (not yet built).

Folder layout mirrors the sibling `Landtagswahlen/` tree: one folder per state (full German name),
files named `<ABBR>_<YEAR>_<Election>_Wahlkreis.<ext>`, plus a per-state `README.md` documenting
provenance and any years that exist but were not downloadable.

## Sources

Per-state official sources only (each state's `README.md` has exact URLs):
- the **Landeswahlleiter** result portals (recent elections — usually machine-readable open data), and
- the **Statistische Landesämter** "Statistische Berichte / Hefte" series and statistical libraries
  (historical elections — mostly scanned PDF/TIF).

## Coverage (as actually downloaded to disk)

| State | Unit | Years on disk | Machine-readable | Scans (PDF/TIF) | Notes |
|---|---|---|---|---|---|
| Baden-Württemberg | Wahlkreis | 1952–2026 | 2001,2006,2011,2016,2021,2026 | 1952–1996 | a few early PDFs are Landtag results recomputed onto **federal** BTW-Wahlkreis geometry (flagged in files) |
| Bayern | **Stimmkreis** | 1946–2023 | 1990s–2023 | 1946–1986 | **complete**; 1946/1954 = full Stat. Jahrbücher (Stimmkreis tables inside, text-layer verified); large scans |
| Berlin | Wahlkreis (Abgeordnetenhaus) | 1989–2023 | recent | 1990s–2021 | machine-readable reports at **Wahlbezirk** level (aggregate to Wahlkreis); pre-1989 West-Berlin not on portal |
| Brandenburg | Wahlkreis (44) | 1990–2024 | most | some | **complete**; includes a `1990–2024 Lange-Reihe` workbook |
| Bremen | **Wahlbereich**/Stadtteil/Ortsteil | 1947, 1991, 2003–2023 | most | some | no single-member Wahlkreise; mid-history patchy |
| Hamburg | Wahlkreis (since 2008) / Wahlbezirk | 1957–2025 | 2001–2025 | 1957,1961,1974,1986 | Wahlkreise exist only since 2008; pre-2008 is Wahlbezirk/Stadtteil |
| Hessen | Wahlkreis (55) | 1946–2023 | 2 recent | 1946–2018 (PDF/TIF) | near-**complete** historical series; mostly scans |
| Mecklenburg-Vorpommern | Wahlkreis (36) | 1990–2021 | most | some | **complete** |
| Niedersachsen | Wahlkreis (87) | 1947, 1990–2022 | 1998–2022 (.tar.gz/.xml) | few | historical 1951–1986 not obtained (archive PDFs only) |
| Nordrhein-Westfalen | Wahlkreis (128) | 1954–2022 | 2000–2022 (**.txt**, IT.NRW) | 1954,1958,1962 | gap 1966–1995 not obtained |
| Rheinland-Pfalz | Wahlkreis (52) | 1996–2026 | 1996–2026 (.xlsx) | few | pre-1996 not obtained |
| Saarland | Wahlkreis (only 3) / Kreis | 1947–2022 | 2022 (.csv) | 1947–2022 (scans) | now **complete**; pre-1980 = Kreis-level scans; 1970–90 in one Sonderheft-172 compendium; only 3 large Wahlkreise |
| Sachsen | Wahlkreis (60) | 1990, 1994, 1999, 2004–2024 | most + 1994 (HTML/Wayback, 49/60 WK), 1999 (.csv) | some | **complete**; 1994 reconstructed from Internet Archive (11 WK only in 1999-csv "1994" cols) |
| Sachsen-Anhalt | Wahlkreis | 1990–2021 | most | some | **complete** |
| Schleswig-Holstein | Wahlkreis (35) | 1954,1958, 1979–2022 | 2000 (9 HTML tables), 2005–2022 | 1954–1996 | 2000 added (StatLA HTML); still missing 1947,1950,1962,1967,1971,1975 |
| Thüringen | Wahlkreis (44) | 1990–2024 | all (.xlsx) | — | **complete**, fully machine-readable |

**Total: 301 data files, ~2.3 GB, all 16 states** (281 first pass + 20 net from the June-2026 gap-fill).
All files pass format-integrity checks (valid PDF/XLSX/XLS/CSV/XML/HTML; no error pages).

## Format notes (state-specific raw formats — do NOT "fix" these)
- **NRW**: `.txt` semicolon-delimited (IT.NRW open-data format).
- **Niedersachsen**: `.tar.gz` votemanager archives + `.xml`.
- **Hessen**: some years are `.tif` scanned-page images.
- **Bremen**: 2023 is a `.js` (data embedded in the results-presentation JavaScript).
- **Bayern**: constituency = **Stimmkreis**, with Erst-/Zweitstimme.

## Gap-fill (June 2026, second workflow)

A targeted second pass (`statistischebibliothek.de` SOLR-proxy + state archives) closed the biggest holes:
- **Filled:** **Saarland** 1947/1952/1955/1960/1965 (Kreis-level scans) + the 1970–1990 Sonderheft-172
  compendium → Saarland now complete; **Bayern** 1946 & 1954 (Stat. Jahrbücher, Stimmkreis tables verified)
  → Bayern complete; **Sachsen** 1994 (49/60 WK via Internet Archive HTML; other 11 in the 1999-csv "1994"
  columns) & 1999 (.csv) → Sachsen complete; **Schleswig-Holstein** 2000 (9 StatLA HTML tables).
- **Hard limit — genuinely NOT available online at constituency level** (printed Hefte / archives only,
  or only Gemeinde/Kreis-level in online DBs): **NRW** 1947,1950,1966,1970,1975,1980,1985,1990,1995;
  **Rheinland-Pfalz** 1947→1991 (12 elections); **Niedersachsen** 1951→1986 + 1994; **Berlin** 1950→1985
  (West-Berlin); **SH** 1947,1950,1962,1967,1971,1975. The statistical offices' digitised Landtagswahl
  series for these states start only ~2000–2010; older constituency results exist only as printed
  Statistische Berichte (IT.NRW webshop, Einzelschriften, library/Staatsarchiv) requiring physical scanning.

**Realistic path for the remaining gaps:** GERDA already holds these states' historical results at
**Gemeinde level** (`../Landtagswahlen/`); the missing Wahlkreis figures can be **reconstructed** by
aggregating Gemeinde-level votes up to each year's Wahlkreis using a Gemeinde→Wahlkreis assignment —
a derivation, not a download.

## Known status (weakest → strongest)
- Still thin historically: **NRW** (no 1947–1995 WK files online), **RP** (pre-1996), **NI** (1951–86),
  **Berlin** (pre-1989), **SH** (several mid-century years).
- Strongest: complete **Brandenburg, MV, Sachsen-Anhalt, Thüringen, Sachsen** (1990→); deep historical
  **Hessen (1946–)**, **BW (1952–)**, **Bayern (1946–)**, **Saarland (1947–)**.
- Historical years are scans → need OCR (see the project's `ocr-extract` workflow) before a cleaning pipeline.

*Collected June 2026 via automated per-state discovery + download (every URL HTTP-verified before download).
See each state folder's `README.md` for exact source URLs and per-file provenance.*
