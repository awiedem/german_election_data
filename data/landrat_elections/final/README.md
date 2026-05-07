# Landrat Elections Data

Direct-election results for heads of German Landkreise (rural counties) and equivalent administrative regions (e.g. Städteregion Aachen, Regionalverband Saarbrücken). Companion to the mayoral elections dataset; same schema, different geographic units.

## Scope

| State | Years | Distinct Kreise | Source / pipeline |
|---|---|---:|---|
| Bayern (BY) | 1945--2025 | 72 | from `Amtstitel = "Landrat/Landrätin"` in Bayerisches Landesamt Excel (mayoral pipeline) |
| Nordrhein-Westfalen (NRW) | 2009, 2014, 2015, 2020, 2025 | 31 + Städteregion Aachen | IT.NRW Excel files (mayoral pipeline) |
| Rheinland-Pfalz (RLP) | 1994--2024 | 24 | Landeswahlleiter sheet "Landräte" (mayoral pipeline) |
| Niedersachsen (NI) | 2006--2025 | 39 | PDF extraction (mayoral pipeline) |
| **Brandenburg (BB)** | 2018--2026 | 14 | scraped from `wahlen.brandenburg.de/.../landraetewahlen/` ([code/landrat_elections/00_bb_scrape.R](../../../code/landrat_elections/00_bb_scrape.R)) |
| **Sachsen (SN)** | 2002, 2008, 2015, 2020, 2022, 2025 | 13 | mixed sources ([code/landrat_elections/00_sn_scrape.R](../../../code/landrat_elections/00_sn_scrape.R)) — 2002 XLS winner-only; 2008 + 2015 per-Kreis HTML (wahlarchiv); 2020/2025 single-Kreis Excels; 2022 statewide Excel (9 Kreise, aggregated from Gemeinde rows) |
| **Sachsen-Anhalt (ST)** | 2007, 2014, 2015 | 11 | CSV downloads for 2007/2014 + 2015 per-Kreis HTML. The 2007 CSV has only Gemeinde-level rows (no Kreis summary), so the parser aggregates them by 5-digit Kreis prefix ([code/landrat_elections/00_st_scrape.R](../../../code/landrat_elections/00_st_scrape.R)) |
| **Thüringen (TH)** | 2006, 2012, 2014, 2015, 2018, 2020, 2021, 2023, 2024 | 17 | parsed from Thüringer Landesamt für Statistik LR/LSInfoG xlsx files ([code/landrat_elections/00_th_parse.R](../../../code/landrat_elections/00_th_parse.R)) |
| Saarland (SL) | 2011, 2012, 2014, 2024 | 5 + RVS | RVS via mayoral pipeline; 5 Kreise (NK 2024 from clean PDF, others Wikipedia % only) ([code/landrat_elections/00_sl_extra.R](../../../code/landrat_elections/00_sl_extra.R)) |

Schleswig-Holstein does not currently appear (direct elections only 1998–2009, would need per-Kreis archive scraping). Baden-Württemberg's Landräte are elected by Kreistag (no popular vote → out of scope). Hamburg, Bremen, and Berlin are city-states without Landkreise.

## Datasets

| File | Rows | Cols | Unit | Description |
|---|---|---|---|---|
| `landrat_unharm` | 1,659 | 16 | Election | One row per election-round (winner-level summary), original boundaries |
| `landrat_candidates` | 3,753 | 32 | Candidate | One row per candidate per election cycle (wide format) |

**Data quality note**: 8 SL rows (Merzig-Wadern 2011, Saarlouis 2012, Saarpfalz 2014/2024, St. Wendel 2024) have only `candidate_voteshare` populated — `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`, and absolute `candidate_votes` are NA. This is because Saarland Kreis-level absolute counts are scattered across per-Kreis websites in inconsistent formats. The Neunkirchen 2024 row is fully populated (parsed from a clean Bekanntmachung PDF). Identify these degraded rows by `is.na(eligible_voters)` filter.

`landrat_candidates` has 32 columns rather than 44 because the gender / migration-background enrichment in `04_candidate_characteristics.R` is currently only applied to `mayoral_candidates`. If you need those columns for Landrat candidates as well, the same enrichment can be added trivially — open an issue or PR.

All files are available as `.rds` and `.csv`.

## Schema

Identical to `mayoral_unharm` and `mayoral_candidates` respectively — see `../../mayoral_elections/final/README.md` for column definitions. The only difference is the `election_type` value, which is always `"Landratswahl"` here.

## Geographic identifiers

- `ags`: 8-digit Amtlicher Gemeindeschlüssel for the Landkreis or equivalent. Always ends in `"000"` (no sub-municipality component).
- Examples: `05154000` Kreis Kleve, `05334000` Städteregion Aachen, `07335000` Landkreis Mayen-Koblenz.

## No harmonization

This dataset is currently published only in unharmonized form (original boundaries at the time of each election). County boundaries since 1975 in the West and since the 1990s reforms in the East have been largely stable, so the gain from harmonization is limited. A future `02_landrat_harm.R` could be added using `data/crosswalks/final/cty_crosswalks.csv` if comparability across the few historical Kreisreformen becomes important.

## Known data-quality issues

- **NRW 2025 Stichwahl date typo (patched)**: Same upstream IT.NRW issue affecting OB rows also affects Landrat rows in the 2025 file. All 2025 SW dates are encoded as `2020-09-27` in the source XML. The pipeline patches them to `2025-09-28` automatically; see [`code/mayoral_elections/01_mayoral_unharm.R`](../../../code/mayoral_elections/01_mayoral_unharm.R) for the workaround. Will be removed once IT.NRW corrects the source.

## Provenance

Two pipeline branches feed this dataset:

**1. Mayoral pipeline** (in `code/mayoral_elections/`) — produces Landrat rows for BY, NRW, RLP, NI, SL because their raw files mix Landrat and Bürgermeister/Oberbürgermeister together. The split happens at the end of stages 01 and 01b.

- `code/mayoral_elections/01_mayoral_unharm.R` writes initial `landrat_unharm.{rds,csv}`
- `code/mayoral_elections/01b_mayoral_candidates.R` writes initial `landrat_candidates.{rds,csv}`

**2. Landrat-specific pipeline** (in `code/landrat_elections/`) — scrapes Landrat-only data from states whose Landrat results are not co-located with mayoral data:

- `code/landrat_elections/00_bb_scrape.R` — Brandenburg (HTML pages from `wahlen.brandenburg.de`)
- `code/landrat_elections/00_sn_scrape.R` — Sachsen (mixed: 2002 XLS, 2008 HTML, 2020/2022/2025 XLSX)
- `code/landrat_elections/00_st_scrape.R` — Sachsen-Anhalt (CSV downloads)
- `code/landrat_elections/00_th_parse.R` — Thüringen (parses pre-loaded xlsx files at `data/mayoral_elections/raw/thueringen/`)
- `code/landrat_elections/00_sl_extra.R` — Saarland additional Landrat data for the 5 Kreise not in the mayoral pipeline. Downloads NK 2024 PDF + hardcodes Wikipedia-sourced rows for Merzig-Wadern, Saarlouis, Saarpfalz, St. Wendel.
- `code/landrat_elections/01_landrat_combine.R` — parses all scraped/cached files, combines with mayoral-pipeline output, writes final `landrat_unharm.{rds,csv}` AND `landrat_candidates.{rds,csv}`

Run order: mayoral pipeline first → then `00_*_scrape.R` for each new state → then `01_landrat_combine.R`. Scrapers are idempotent (skip already-cached files).
