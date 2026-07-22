# Council seats added to GERDA — change record (July 2026)

## State and verdict

Two seat-count (Sitze) products were added and validated. Municipal council seats now ship inside `municipal_unharm`; a new stand-alone dataset `county_council_seats` holds county-council composition. Both are done, regenerated, and pass their validation checks. No existing dataset column or row changed value. Remaining work is the git commits, and a set of deferred items listed at the end (most importantly two municipal voter-group seat columns held back over a source bug, and one interpretation question for a coauthor).

"Seats" here means the number of council mandates a party won in a given council (Gemeinderat / Stadtrat at the municipal level, Kreistag at the county level), as distinct from the vote shares GERDA already carried.

## What changed

### 1. Municipal seats surfaced in `municipal_unharm`

The municipal pipeline (`code/municipality_elections/01_municipal_unharm.R`) already read seat data for several states and carried `sitze_*` columns most of the way through, but the final "reduce to prop_ only" `select()` (line ~20435) kept only vote-share and flag columns and silently dropped the seat columns. The dataset never exposed them.

Change: one line — add `starts_with("seats_")` to that `select()`. Nothing else in the municipal script was touched. The existing party-rename chain already converts the seat column names to the same ASCII snake_case as the vote columns (`seats_grüne` → `seats_gruene`, `seats_freiewähler` → `seats_freie_wahler`, etc.), so no rename edit was needed.

Result: `municipal_unharm` goes from 82,354 × 32 to 82,354 × 42. The ten added columns are `seats_cdu_csu, seats_spd, seats_linke_pds, seats_gruene, seats_afd, seats_piraten, seats_fdp, seats_die_partei, seats_freie_wahler, seats_bsw`. Seats are `NA` wherever the source has no seat data.

Seats were added to the unharmonized dataset only. Harmonizing to fixed boundaries aggregates municipalities that merged using population weights; a population-weighted sum of seats is not the seat count of any real council, so the harmonized datasets deliberately do not carry seats.

### 2. New dataset `county_council_seats`

New script `code/county_elections/03_county_seats.R` reads the hand-compiled file `Sitzverteilungen_der_Parteien_2008-2022_v1-0-0.csv` and writes `data/county_elections/final/county_council_seats.{rds,csv}`.

This is a yearly council-composition panel, not an election table: one row per county-year for 400 counties × 18 years (2008–2025 = 7,200 rows), where a council's composition is repeated every year until the next election changes it. It is published separately from `county_elec_unharm` (which holds election-level Kreistagswahl vote results) because the two have different row semantics: a panel of standing composition versus a table of election events.

The July 2026 extension preserves every 2008–2022 field and adds Schleswig-Holstein 2023, the 2024 elections in Baden-Württemberg, Brandenburg, Mecklenburg-Vorpommern, Rheinland-Pfalz, Saarland, Sachsen, Sachsen-Anhalt, and Thüringen, and the Nordrhein-Westfalen 2025 Kreistags-/Ratswahl (14 September 2025; 53 counties, parsed from the official `wahlergebnisse.nrw` per-county pages). Official result files and cached official HTML pages are parsed in a separate block in `03_county_seats.R`. Each election result is carried forward through 2025; counties without a new election retain their 2022 vector, including their 2022 `government_party`. Once a new election overlays a county, `government_party` becomes `NA` from that year on because the official seat sources do not identify the governing party of the new council (420 of the 1,200 rows in 2023–2025 are `NA` on this account, including the 53 NRW 2025 councils; the rest carry the 2022 value forward).

A review of the extension (see below) found that the split of non-major-party seats across `seats_freie_wahler` / `seats_regional` / `seats_other` uses a different convention in the parsed 2023–2025 rows than in the hand-compiled 2008–2022 rows (the old rows often folded Freie Wähler and local voter groups into `seats_regional`). Because the six major-party columns are unaffected and the old rows cannot be re-split, the panel now carries a derived column `seats_local_other = seats_freie_wahler + seats_regional + seats_other` that is comparable across all years and states. Use it, not the three components, for time series of non-establishment seats. Two smaller review fixes were also applied: `government_party` is now carried forward as described above (previously it was `NA` for all 2023–2025 rows), and the Sachsen Landkreis parser now assigns local voter associations to `seats_other` rather than `seats_regional`, matching the city block and the other new states.

**Boundaries — the reason a fixed county file is possible across a period with reforms.** Some counties did change inside 2008–2022 (Städteregion Aachen formed 2009; the Mecklenburg-Vorpommern Kreisgebietsreform cut 18 counties to 8 in 2011; Landkreis Göttingen merged with Osterode in 2016). The source uses one fixed set of ~400 current (post-reform, ~2021) county codes for all years and represents the change honestly rather than by fabrication: a reform-created county has `NA` total and zero party seats for the years before it existed (verified — no pre-existence row carries backfilled seats), and the abolished predecessor councils are simply not in the file. The consequence is a real coverage limit, not a data error: the panel holds no pre-reform county-council composition for reformed areas (Mecklenburg-Vorpommern 2008–2010 is entirely `NA`; merged Göttingen is `NA` before 2016). "Balanced 400 × 15" describes the row structure, which is balanced by construction, not full data coverage in every cell.

Twenty-two columns: `county` (5-digit, matches `county` in `county_elec_unharm`), `county_name`, `county_type`, `state`, `state_name`, `year`, `government_party`, `seats_total`, nine party seat columns (`seats_spd, seats_cdu_csu, seats_fdp, seats_gruene, seats_freie_wahler, seats_linke_pds, seats_afd, seats_regional, seats_other`), the derived `seats_local_other` (their non-major-party sum), `flag_seats_total_incongruent`, `comment`, `source`, `last_checked`.

Two treatments applied on top of the raw file:
- A blank `Sitze gesamt` (council size) cell becomes `NA`, not 0. Thirty-eight rows have a blank total while their party columns sum to a real number; coding those as 0 would assert a zero-seat council, which is wrong. Party-column blanks stay 0 (a party with no seats).
- `flag_seats_total_incongruent` (logical) marks the five rows where the source's own total does not equal the sum of the nine party columns (Groß-Gerau 2011, Landkreis Heilbronn 2008, Donau-Ries 2020–2022). These are discrepancies inside the hand-compiled source; the values are kept as recorded and flagged rather than altered.

The roughly 45 detailed `Sonstige: <party>` columns in the raw file (a breakdown of the `seats_other` aggregate) are not carried into the published panel; they remain in the raw CSV.

## Coverage

Municipal seats, by state (all `NA` outside these): Baden-Württemberg 1989–2024, Hessen 1993–2021, Thüringen 1994–2024, Nordrhein-Westfalen 1994–2025 (kreisfreie Städte only from 2025), Brandenburg 2003–2024, Rheinland-Pfalz 2004–2019 (excluding kreisfreie Städte), Schleswig-Holstein 2018 only, Mecklenburg-Vorpommern 2019 and 2024, Saarland 2019, Sachsen-Anhalt 1994–2019 (all six elections), Niedersachsen (2011 and 2016 ordinary Gemeinden; 2021 the eight kreisfreie Städte), Bremen and Hamburg (Bürgerschaft; Hamburg 2025 only). No seat data at all for Bayern, Berlin, Sachsen.

A July 2026 follow-up merged three states' already-on-disk seat sources: Saarland 2019 (`GRW_Sitzverteilung_2019.xlsx`, year-guarded join so 2019 seats are not stamped onto earlier years), Sachsen-Anhalt 1994–2019 (seat counts were already present in the vote workbook as unnamed columns two positions right of each party's vote column — extracted, no new file), and Niedersachsen 2011 and 2016 (seat objects were read but never merged). Niedersachsen's eight kreisfreie Städte enter through a separate block absent from the 2011/2016 seat files, so they stay `NA` those years (the 2021 source conversely covers only those cities).

Two coverage points differ from the original plan's expectation and were checked: Schleswig-Holstein 2023 has no seats because that block sets the seat columns to `NA_real_` on purpose (SH 2023 has no seat source), not through an error; Mecklenburg-Vorpommern seats are present, which the initial diagnostic had missed.

County seats: 400 counties in every year 2008–2025 by construction (fixed current boundaries), with pre-existence years of reform-created counties left `NA` (see the boundary note above). Mecklenburg-Vorpommern 2008–2010 is `NA` throughout. Pre-reform predecessor counties and municipal seat gaps remain deferred.

### Design decision: why one fixed-boundary file, not per-year files with then-current units

A per-year structure — one file per year holding that year's then-current set of counties — was considered and declined. It would be the more faithful representation of each year's reality, but it cannot be populated: the source `Sitzverteilungen` file contains only the ~400 current (post-reform) counties, and the county vote data `county_elec_unharm` likewise starts Mecklenburg-Vorpommern at 2014 on the eight current codes. The pre-reform councils (Mecklenburg-Vorpommern's 18 pre-2011 Landkreise, the separate pre-2016 Göttingen and Osterode, the separate pre-2009 city and Landkreis of Aachen) are not in the repository at all. A per-year file would therefore just relabel the same empty cells: for the ~27 reform-affected county-years the then-current units would carry no data, and we would drop the current-code `NA` rows and have nothing to put in their place. The single fixed-boundary file keeps identity aligned with `county_elec_unharm` and loses no information relative to the alternative. Revisit only if the pre-reform seat data is first collected from the state statistical offices cited in each row's `source` column; that is a separate data-collection task, not a restructuring.

## Validation

Municipal, regression against the committed dataset: 82,354 rows unchanged; all 32 pre-existing columns byte-identical after a common sort; the only difference is the 10 added seat columns. All seat values are non-negative whole numbers.

Municipal, does-it-look-right: seats track votes where present. Stuttgart 2019 Gemeinderat has GRÜNE largest at 16 seats (they led the vote); Köln 2025 Stadtrat has GRÜNE largest at 22. The major-party seat columns sum to less than council size in both, which is expected (see caveat below). Köln 2020 is `NA` because NRW kreisfreie Städte carry no seat data before 2025.

County: county × year key is unique; 7,200 rows, 400 × 18 balanced; all seat values are non-negative integers. Every newly parsed election row has `seats_total` equal to the sum of the nine party columns. The five historical incongruences are preserved, and the Donau-Ries 2022 vector carries its existing flag through 2025. The parser reproduces 33 of 36 Rheinland-Pfalz 2019 seat vectors exactly; the three disagreements (07140, 07235, and 07335) are recorded and the historical rows are not overwritten. Full source and spot-check results are in `docs/county_seats_coverage_2025.md`.

## Caveat that matters for use

Municipal party-seat columns do not sum to council size. Only ten major parties have seat columns. Local voter groups (Wählergruppen), joint nominations, independent candidates, and small parties hold a large share of German local council seats and are not represented, so the row sum of `seats_*` is a lower bound on the council, not its total. County seats do have a `seats_total` and a residual `seats_other`, so the county panel's party columns sum to the total except for the preserved historical source discrepancies.

## Deferred / open items

1. **Two municipal voter-group seat columns held back.** The source also carries seats for local voter groups (Wählergruppen) and joint nominations (Gemeinsame Wahlvorschläge). They were excluded from this release for two reasons: they are dropped earlier in the pipeline (a `-contains()` step around line 20205 removes them along with the vote-count versions), so recovering them needs an additional edit; and they carry a confirmed bug — Brandenburg 2003 and 2008 write the raw "Wählergruppen" seats into *different* output columns (2003 puts them in `sitze_Gemeinsame_Wahlvorschläge`, 2008 in `sitze_Wählergruppen`), so the two columns are swapped across years for at least Brandenburg. Fix requires auditing the Brandenburg 2003/2008/2014/2019/2024 mappings before these columns can be trusted. Voter-group seats are the single most valuable missing piece for local-council research, so this is the top follow-up.

2. **`government_party` interpretation.** The county file's `Regierungspartei` column is treated as the party of the county executive (Landrat / Oberbürgermeister). This reading is inferred; the source has no codebook. Confirm with Vincent before the dataset is described publicly.

3. **Sitzverteilungen provenance.** The county seat file is hand-compiled (v1.0.0, added by Vincent) with per-row source URLs but no methodology note. A one-line provenance/citation statement should be settled before redistribution.

4. **`gerda` R package.** The companion package (`hhilbig/gerda`) loads GERDA datasets from the web via `load_gerda_web()`. Adding `county_council_seats` to that package is a separate task in that repo, out of scope here.

## Files touched

- `code/municipality_elections/01_municipal_unharm.R` — one-line select edit.
- `code/county_elections/03_county_seats.R` — new script.
- `data/municipal_elections/final/municipal_unharm.{rds,csv}` — regenerated (LFS).
- `data/county_elections/final/county_council_seats.{rds,csv}` — new (LFS).
- `docs/codebook.md`, `README.md`, `docs/data_pipeline.md` — documentation.

## Glossary

- **AGS** (Amtlicher Gemeindeschlüssel): 8-digit municipality identifier; first 5 digits are the county.
- **Kreistag**: county council. **Gemeinderat / Stadtrat**: municipal council.
- **Kreisfreie Stadt**: a city that is its own county (no surrounding Landkreis).
- **Wählergruppen**: local voter associations that contest council elections without being a registered national party; large in local German politics.
- **Harmonized vs unharmonized**: unharmonized uses each election year's own municipal boundaries; harmonized remaps results onto a fixed set of boundaries (2021 or 2025) so they are comparable over time.
- **SSW** (Südschleswigscher Wählerverband): Danish-minority regional party in Schleswig-Holstein; in the county file its seats fall under `seats_regional`.
