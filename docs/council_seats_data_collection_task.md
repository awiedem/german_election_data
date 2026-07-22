# Task: expand council-seat coverage in GERDA (for Codex)

## What this is and why

GERDA has two council-seat products (see `docs/council_seats_update_2026.md`): `seats_*` columns inside `municipal_unharm`, and a `county_council_seats` panel with 400 current-boundary counties from 2008 through 2025. WS1 below was completed in July 2026; WS2 and WS3 remain deferred.

1. The county panel stops at **2022** and misses the **June 2024 Kreistagswahlen** (held in most eastern states plus BW, RLP, SL — roughly nine states voted on 9 June 2024).
2. Reform-created counties have **no pre-reform data** (Mecklenburg-Vorpommern 2008–2010, merged Landkreis Göttingen before 2016, Städteregion Aachen before 2009), because the source only has current boundaries.

This document scopes a data-collection task to address these, and states plainly what is automatable and what is not.

## Feasibility: read this before starting

The existing `county_council_seats` file was compiled by hand from about 70 different websites — the Deutscher Landkreistag (`landkreistag.de`, ~2,200 rows), the state statistical offices, many PDF result reports (e.g. `statistik-nord.de`), and dozens of individual city and county sites. There is no single endpoint that reproduces it, and a fully automated re-scrape of all 70 sources is not realistic: too many are one-off PDFs and bespoke municipal pages.

The automatable path is the one GERDA already uses for state elections: the **Regionalstatistik GENESIS database** (`regionalstatistik.de`), queried through the `wiesbaden` R package with a DESTATIS GENESIS API account (the same account the state-election pipeline needs). Regionalstatistik publishes communal election results, and for several states includes seat distributions (Sitzverteilung / Mandate) for Kreistags- and Gemeinderatswahlen. **The first step of this task is to confirm which seat tables exist there and for which state-years**, because that determines how much can be pulled cleanly versus scraped page by page.

Realistic expectation: the 2024 Kreistagswahlen and recent cycles are well-structured on official portals and should be largely automatable. The pre-2011 county data is old, often PDF-only, and will be partly manual. Do not promise a fully automated solution for the historical gaps.

## Workstreams, in priority order

### WS1 — Extend `county_council_seats` to include the 2024 Kreistagswahlen (completed)

Completed with Schleswig-Holstein 2023 and the 2024 elections in Baden-Württemberg, Brandenburg, Mecklenburg-Vorpommern, Rheinland-Pfalz, Saarland, Sachsen, Sachsen-Anhalt, and Thüringen. See `docs/county_seats_coverage_2025.md` for the source audit and validation record.

Add county-council seat distributions for the elections held after the current panel ends (primarily 9 June 2024; also any 2023 county elections), on the same current-boundary county set.

- Primary source: Regionalstatistik GENESIS via `wiesbaden` — search for communal-election seat tables (Sitzverteilung nach Parteien, Kreisebene). Confirm table codes and coverage first.
- Fallback per state where GENESIS lacks seats: the state statistical office result portals already cited in the existing file's `source` column (e.g. `wahlen.thueringen.de`, `wahlergebnisse.sachsen-anhalt.de`, `wahlen.sachsen.de`, `statistik-nord.de`, `wahlen.statistik.niedersachsen.de`, `statistikdaten.bayern.de`, `wahlen.rlp.de`, `statistik-berlin-brandenburg.de`). Many expose CSV/Excel downloads for the 2024 cycle.
- Extend the panel years to 2023–2025 as data allows (composition is constant between elections, so a 2024 election fills 2024 and later years for that county).

### WS2 — Backfill pre-reform county-years (narrow, partly manual)

Recover the councils that existed before the in-window reforms, so those county-years are real data rather than `NA`:
- Mecklenburg-Vorpommern 2008–2010: the 18 pre-2011 Landkreise and 6 kreisfreie Städte (the 2009 Kreistagswahl). Sources: `laiv-mv.de` / `wahlen.mvnet.de` / `statistik-mv` — likely PDF result reports.
- Landkreis Göttingen + Landkreis Osterode am Harz before their 2016 merger. Source: `wahlen.statistik.niedersachsen.de` / `nls.niedersachsen.de` (the older NI portal has HTML result tables, e.g. `nls.niedersachsen.de/KW2006/…html`).
- City of Aachen and Landkreis Aachen before the 2009 Städteregion. Source: `wahlergebnisse.nrw` / `landesdatenbank.nrw.de`.
- Note: these enter the panel under **their own historical county codes**, not the current merged codes. This changes the "fixed current boundaries" design for these specific rows, so flag them and discuss the representation before merging (a `boundary_vintage` marker may be warranted). ~27 county-years are at stake — small; do this only if the research need justifies the manual effort.

### WS3 — Municipal seat gaps (optional, larger, separate pipeline)

`municipal_unharm` has no seat data for Bayern, Berlin, Saarland, Sachsen, Sachsen-Anhalt (and only partial/kreisfreie for Niedersachsen and RLP). Filling these means new parsing in `01_municipal_unharm.R`, not a self-contained scrape, and is a bigger effort. Treat as a follow-on only if requested.

## Output contract (match the existing schema exactly)

New/extended county rows must conform to `county_council_seats` (see `code/county_elections/03_county_seats.R` and the codebook section "County Council Seats"):

`county` (5-digit char), `county_name`, `county_type` (`"Landkreis"` / `"kreisfreie Stadt"`), `state`, `state_name`, `year` (int), `government_party`, `seats_total` (int, `NA` if source blank), `seats_spd, seats_cdu_csu, seats_fdp, seats_gruene, seats_freie_wahler, seats_linke_pds, seats_afd, seats_regional, seats_other` (int, blank→0), `flag_seats_total_incongruent` (logical), `comment`, `source` (the actual URL used), `last_checked` (Date).

Party mapping conventions: CDU→`seats_cdu_csu`, Linke/PDS→`seats_linke_pds`, GRÜNE→`seats_gruene`, Freie Wähler→`seats_freie_wahler`; regional parties (e.g. SSW) → `seats_regional`; everything else → `seats_other`. Keep names in GERDA convention, not raw source labels.

Do not overwrite the existing hand-compiled 2008–2022 rows. Add new rows (2023+ and any backfilled historical rows) and integrate in `03_county_seats.R` behind a clearly separated block, so the provenance of hand-compiled versus newly-scraped rows stays legible (consider a `source_batch` or keep the `source` URL faithful per row).

## Validation (required before reporting done)

1. **Overlap cross-check.** Wherever a newly-scraped county-year overlaps an existing hand-compiled row (e.g. a county's 2019 composition that persists into 2022), the seat vectors must match. Report any disagreement rather than silently replacing.
2. **Internal consistency.** `seats_total == sum(9 party columns)` on every non-flagged, non-`NA` row; set `flag_seats_total_incongruent` where the source itself disagrees.
3. **Key + type.** Unique `county × year`; all seat columns non-negative integers; `county` is 5-digit character.
4. **Official spot-checks.** For at least three new county-years, verify `seats_total` and the largest party against the official result page. Record the URLs.
5. **Coverage report.** State exactly which state-years were added, which were attempted and failed (and why), and what remains `NA`. No silent gaps.

## Environment notes for Codex

- GENESIS/Regionalstatistik access uses the `wiesbaden` R package and a DESTATIS API account — the same setup the state-election pipeline documents (see project `CLAUDE.md`, "State elections require a DESTATIS GENESIS API account"). Confirm credentials before starting WS1.
- Scraping etiquette: identify a User-Agent, rate-limit, and cache raw downloads (PDF/CSV) under `data/county_elections/raw/` so re-runs do not re-hit the servers.
- PDF extraction: `pdftools` / `tabulizer` in R, or a Python equivalent; several state reports are table-in-PDF and need OCR-free text extraction, not vision models, where the PDF has a text layer.
- Keep everything on a feature branch; do not touch the phantom-modified Kreistags xlsx files in the working tree.

## Glossary

- **Kreistagswahl**: county-council election. **Gemeinderatswahl**: municipal-council election.
- **Sitzverteilung / Mandate**: allocation of council seats to parties.
- **Regionalstatistik / GENESIS**: the joint online database of the German statistical offices (`regionalstatistik.de`), queryable via API; GERDA already uses it through the `wiesbaden` R package for state elections.
- **Kreisgebietsreform**: county-boundary reform (mergers/abolitions); the ones inside 2008–2022 are MV 2011, Göttingen 2016, Aachen 2009.
- **Current boundaries**: the ~400 counties as they exist post-reform (~2021); the existing panel is fixed at these.
