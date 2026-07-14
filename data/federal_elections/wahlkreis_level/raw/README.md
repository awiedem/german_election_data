# Bundestagswahlen — Wahlkreis (constituency) level — RAW

Federal election (Bundestagswahl) results broken down by **Wahlkreis** (the 299 single-member
constituencies) — a new geographic level for the federal tree, parallel to the existing
`municipality_level/` and `county_level/` splits, and analogous to
`data/state_elections/raw/Landtagswahlen_Wahlkreis/` for the state pipeline.

Raw files are stored **verbatim** as published; cleaning/standardisation happens in
`code/federal_elections_wahlkreis/`.

## Source

All files are official open data from **Die Bundeswahlleiterin** (Wiesbaden), under
*Datenlizenz Deutschland – Namensnennung – Version 2.0* (https://www.govdata.de/dl-de/by-2-0).
Downloaded by `code/federal_elections_wahlkreis/00_download_raw.R` (each URL documented there).

Two result-file formats appear:

- **`kerg2.csv`** — flat/long form (one row per `Gebiet × Gruppe × Stimme`, absolute + relative
  values + previous-period columns). Published for **2017, 2021, 2025**. Preferred by the parser.
- **`kerg.csv`** — classic wide form (multi-row header; party columns grouped by
  Erststimmen/Zweitstimmen × Endgültig/Vorperiode). Published for **2002–2025**.

Each file reports results at three `Gebietsart` levels — Bund (99), Land (1–16), and **Wahlkreis**
(1–299) — with Erststimme (`Stimme = 1`) and Zweitstimme (`Stimme = 2`), and marks the Direktmandat
winner per Wahlkreis.

## Files on disk

| Folder | File | Election | Format |
|---|---|---|---|
| `BTW02/` | `btw02_kerg.csv` | 2002 | classic |
| `BTW05/` | `btw05_kerg.csv` | 2005 | classic |
| `BTW09/` | `btw09_kerg.csv` | 2009 | classic |
| `BTW13/` | `btw13_kerg.csv` | 2013 | classic |
| `BTW17/` | `btw17_kerg.csv`, `btw17_kerg2.csv` | 2017 | classic + flat |
| `BTW21/` | `w-btw21_kerg.csv`, `w-btw21_kerg2.csv` | 2021 | classic + flat (incl. 2024 Berlin repeat-election correction) |
| `BTW25/` | `btw25_kerg.csv`, `btw25_kerg2.csv` | 2025 | classic + flat |

### 2025 reference tables (in `BTW25/`)

- `btw25_wahlkreisnamen_utf8.csv` — the 299 constituency numbers and names (2025 definition).
- `btw25_parteien.csv` — party group numbers and full/short designations.
- `btw25_wkr_gemeinden_20241130_utf8.csv` — constituency ↔ municipality assignment (2025).
- `btwkr25_umrechnung_btw21.csv` — **official recomputation of the 2021 result onto 2025 constituency
  boundaries**. This is the basis for the 2021→2025 Wahlkreis crosswalk (unchanged/redrawn/new), so no
  geometry guess-work is needed.

## Notes

- 2021 uses the **`w-` (Wiederholungswahl-corrected)** files, which fold in the February 2024 partial
  repeat election in Berlin — the definitive 2021 figures.
- `Gebietsart = "Einzelbewerber/Wählergruppe"` rows are Wahlkreis-specific independents with no national
  aggregate; only `Gruppenart = "Partei"` rows reconcile Wahlkreis → Bund (verified exact for 2025).
- Coverage is **2002–2025** (seven elections), the machine-readable open-data era. Pre-2002 constituency
  results exist only in older/scanned formats and are out of scope here (no OCR guess-work), matching the
  state pipeline's "machine-readable only" rule.
