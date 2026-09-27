# Landrat Elections Data

Direct-election results for heads of German Landkreise (rural counties) and equivalent administrative regions (e.g. Städteregion Aachen, Regionalverband Saarbrücken). Companion to the mayoral elections dataset; same schema, different geographic units.

## Scope

| State | Years | Distinct Kreise | Source / pipeline |
|---|---|---:|---|
| Bayern (BY) | 1945--2026 | 72 | from `Amtstitel = "Landrat/Landrätin"` in Bayerisches Landesamt Excel (mayoral pipeline) |
| Nordrhein-Westfalen (NRW) | 2009, 2014, 2015, 2020, 2025 | 31 + Städteregion Aachen | IT.NRW Excel files (mayoral pipeline) |
| Rheinland-Pfalz (RLP) | 1995--2025 | 24 | Landeswahlleiter sheet "Landräte" (mayoral pipeline) |
| Niedersachsen (NI) | 2006--2021 | 39 | PDF extraction (mayoral pipeline) |
| **Mecklenburg-Vorpommern (MV)** | 2000--2025 | 18 | PDF extraction from LAIV-MV Direktwahlen PDFs via [code/mayoral_elections/00_mv_parse.py](../../../code/mayoral_elections/00_mv_parse.py) (mayoral pipeline). Pre-/post-2011-reform Kreis codes are year-aware (e.g. Nordwestmecklenburg `13058000` pre-2011, `13074000` from 2011) |
| **Brandenburg (BB)** | 2010--2026 | 14 | scraped from `wahlen.brandenburg.de/.../landraetewahlen/` ([code/landrat_elections/00_bb_scrape.R](../../../code/landrat_elections/00_bb_scrape.R)). The portal shows only the latest cycle per Kreis under a fixed URL, so a later cycle is saved next to the cached one as `BB_<slug>_<date>.html` (Ostprignitz-Ruppin holds 2018 and 2026). **Every cycle since direct election began (Jan 2010)**: the 2010--2018 pages the portal had already overwritten are archived copies from the Wayback Machine, of the same portal URLs (2013--2018 cycles) or of the Landeswahlleiter's earlier site (2010, and two 2016 pages), all "Endgültiges Ergebnis"; provenance per file in [`raw/brandenburg/README.md`](../raw/brandenburg/README.md) |
| **Sachsen (SN)** | 2002, 2008, 2015, 2020, 2022, 2025 | 13 | mixed sources ([code/landrat_elections/00_sn_scrape.R](../../../code/landrat_elections/00_sn_scrape.R)) — 2002 XLS winner-only; 2008 + 2015 per-Kreis HTML (wahlarchiv); 2020/2025 single-Kreis Excels; 2022 statewide Excel (9 Kreise, aggregated from Gemeinde rows) |
| **Sachsen-Anhalt (ST)** | 2007--2026 | 11 | CSV downloads for 2007/2014 + 2015 per-Kreis HTML; 2019--2026 from the rolling `lr.csv`. The 2007 CSV has only Gemeinde-level rows (no Kreis summary), so the parser aggregates them by 5-digit Kreis prefix ([code/landrat_elections/00_st_scrape.R](../../../code/landrat_elections/00_st_scrape.R)) |
| **Thüringen (TH)** | 2006, 2012, 2014, 2015, 2018, 2020, 2021, 2023, 2024, 2026 | 17 | parsed from Thüringer Landesamt für Statistik LR/LSInfoG xlsx files ([code/landrat_elections/00_th_parse.R](../../../code/landrat_elections/00_th_parse.R)) |
| Hessen (HE) | 1993--2024 | 21 | HSL "Direktwahlen in Hessen seit 1993" workbook (mayoral pipeline, Landrat rows split off) |
| Saarland (SL) | 2011, 2012, 2014, 2024 | 5 + RVS | RVS via mayoral pipeline; 5 Kreise (NK 2024 from clean PDF, others Wikipedia % only) ([code/landrat_elections/00_sl_extra.R](../../../code/landrat_elections/00_sl_extra.R)) |

Schleswig-Holstein does not currently appear (direct elections only 1998–2009, would need per-Kreis archive scraping). Baden-Württemberg's Landräte are elected by Kreistag (no popular vote → out of scope). Hamburg, Bremen, and Berlin are city-states without Landkreise.

## Datasets

| File | Rows | Cols | Unit | Description |
|---|---|---|---|---|
| `landrat_unharm` | 2,166 | 17 | Election | One row per election-round (winner-level summary), original boundaries |
| `landrat_candidates` | 5,348 | 33 | Candidate | One row per candidate per election cycle (wide format) |

**Data quality note**: 8 SL rows (Merzig-Wadern 2011, Saarlouis 2012, Saarpfalz 2014/2024, St. Wendel 2024) have only `candidate_voteshare` populated — `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`, and absolute `candidate_votes` are NA. This is because Saarland Kreis-level absolute counts are scattered across per-Kreis websites in inconsistent formats. The Neunkirchen 2024 row is fully populated (parsed from a clean Bekanntmachung PDF). Identify these degraded rows by `is.na(eligible_voters)` filter.

**Source anomalies kept as published**: the Märkisch-Oderland Stichwahl of 17 October 2021 reports 60,866 Wählende but 60,632 valid + 254 invalid = 60,886 on the Landeswahlleiter's own page; the official results portal has no page for that election to check against. The Märkisch-Oderland Hauptwahl of 22 September 2013 reports 108,801 Wähler but 106,238 valid + 1,563 invalid = 107,801, identically on the 2015 page and its 2019 re-publication (the candidate votes sum to the valid count).

`landrat_candidates` has 33 columns rather than 44 because the gender / migration-background enrichment in `04_candidate_characteristics.R` is currently only applied to `mayoral_candidates`. If you need those columns for Landrat candidates as well, the same enrichment can be added trivially — open an issue or PR.

All files are available as `.rds` and `.csv`.

## Schema

Identical to `mayoral_unharm` and `mayoral_candidates` respectively — see `../../mayoral_elections/final/README.md` for column definitions. The differences are the `election_type` value, which is always `"Landratswahl"` here, and one extra column in both files, `flag_elected_by_council`.

### Brandenburg: Landräte elected by the Kreistag (`flag_elected_by_council`)

In Brandenburg the voters elect a Landrat only with **more than half of the valid votes, and that majority must be at least 15 % of the eligible voters** (§ 72 Abs. 2 BbgKWahlG, applied to the Landrat by § 83; unchanged since direct election began in 2010). The same test applies in the Stichwahl. If the runoff leader misses it, nobody has been elected by the voters and the **Kreistag elects the Landrat** (§ 72 Abs. 2 Satz 5, § 77 Abs. 4 BbgKWahlG). Every Landeswahlleiter result page prints both thresholds.

Ranking candidates by votes would crown the runoff leader of such a cycle as if the voters had elected them. Instead:

- `flag_elected_by_council = TRUE` on every row of the cycle, in both files (`FALSE` everywhere else, never `NA`);
- `landrat_candidates`: `is_winner = NA` for every candidate of the cycle — the ballot seated nobody;
- `landrat_unharm`: `winner_party`, `winner_votes`, `winner_voteshare` are `NA` on the failed Stichwahl row. The cycle's Hauptwahl row keeps its round leader, like every Hauptwahl that went to a runoff.

Twelve cycles are flagged. Their Stichwahl pages say "Gewählt: kein Bewerber" (Oberhavel 2021's carries no "Gewählt" footer), and the 15 % threshold below is the "Stimmenzahl, die 15 % der Wahlberechtigten umfasst" printed on it:

| Kreis | Stichwahl | Runoff leader | Votes | 15 % threshold | Share of electorate | Seated by the Kreistag |
|---|---|---|---:|---:|---:|---|
| Barnim | 24 Jan 2010 | Bodo Ihrke (SPD) | 18,048 | 22,737 | 11.9 % | Ihrke, by lot after two Kreistag ballots without a majority |
| Elbe-Elster | 24 Jan 2010 | Iris Schülzke (parteilos) | 12,602 | 14,765 | 12.8 % | **Christian Jaschinski (CDU), the runoff loser**, 29 Mar 2010 |
| Ostprignitz-Ruppin | 24 Jan 2010 | Ralf Reinhardt (SPD) | 11,580 | 13,321 | 13.0 % | Reinhardt, 20 May 2010 |
| Spree-Neiße | 24 Jan 2010 | Dieter Friese (SPD) | 14,784 | 16,600 | 13.4 % | **Harald Altekrüger (CDU), the runoff loser**, 19 Apr 2010 |
| Uckermark | 14 Mar 2010 | Klemens Schmitz (Einzelwahlvorschlag) | 16,254 | 16,655 | 14.6 % | **Dietmar Schulze (SPD), who had not stood**, 19 May 2010 |
| Teltow-Fläming | 14 Apr 2013 | Kornelia Wehlan (DIE LINKE) | 20,155 | 20,695 | 14.6 % | Wehlan, 9 Sep 2013 |
| Oberhavel | 8 Mar 2015 | Ludger Weskamp (SPD) | 21,288 | 26,267 | 12.2 % | Weskamp, 27 May 2015 |
| Havelland | 24 Apr 2016 | Roger Lewandowski (CDU) | 20,000 | 20,175 | 14.9 % | Lewandowski, 20 Jun 2016 |
| Oder-Spree | 11 Dec 2016 | Rolf Lindemann (SPD) | 17,819 | 23,068 | 11.6 % | Lindemann, 25 Jan 2017 |
| Barnim | 6 May 2018 | Daniel Kurth (SPD) | 17,470 | 23,358 | 11.2 % | Kurth, 4 Jul 2018 |
| Ostprignitz-Ruppin | 6 May 2018 | Ralf Reinhardt (SPD) | 12,222 | 12,844 | 14.3 % | Reinhardt, 6 Sep 2018: tie in the Kreistag against Egmont Hamelow (CDU), decided by lot |
| Oberhavel | 12 Dec 2021 | Volker-Alexander Tönnies (SPD) | 24,964 | 27,284 | 13.7 % | Tönnies, 6 Apr 2022: 37 of 56 votes in the first ballot (postal vote) |

Sources for the Kreistag column: the notes on the Landeswahlleiter's own Stichwahl pages in `raw/brandenburg/` (Elbe-Elster, Ostprignitz-Ruppin, Spree-Neiße and Uckermark 2010, Teltow-Fläming 2013, Oberhavel 2015, Oder-Spree 2016, Barnim 2018), the Kreistag dates in the Landeswahlleiter's archived results index (`raw/brandenburg/_master_index_wayback_2019-08-23.html`: Havelland, Oberhavel 2015, Oder-Spree, Teltow-Fläming, Barnim 2018, Ostprignitz-Ruppin 2018), [Wikipedia on Bodo Ihrke](https://de.wikipedia.org/wiki/Bodo_Ihrke) (Barnim 2010) and [on Roger Lewandowski](https://de.wikipedia.org/wiki/Roger_Lewandowski) (Havelland), [Tagesspiegel, 7 Sep 2018](https://www.tagesspiegel.de/potsdam/brandenburg/wenn-das-los-den-landrat-bestimmt-7836439.html), and the [Landkreis Oberhavel press release](https://www.oberhavel.de/Quicknavigation/Startseite/Alexander-T%C3%B6nnies-wird-neuer-Landrat-Oberhavels.php?object=tx,2244.1&ModID=7&FID=2244.64535.1). The Kreistag chose the runoff leader nine times, the runoff loser twice and a candidate who had not stood once. The Kreistag's choice is therefore not written into `is_winner`.

Of the 14 first direct elections (10 Jan 2010 to 11 Dec 2016), 9 are flagged, which is the count Gäbler & Rösel give ([ifo Dresden berichtet 4/2019](https://www.ifo.de/DocDL/ifoDD_19-04_03-07_Gaebler.pdf)).

Every other Brandenburg cycle clears the rule; the closest is Potsdam-Mittelmark 2022 at 16.1 % of the electorate. `01_landrat_combine.R` also **stops** if a Brandenburg Hauptwahl leader misses the rule and no Stichwahl page is on file (runoff not scraped yet), rather than seating the first-round leader. `99_audit.R` section 12 recomputes the rule from the published counts, pins all twelve cycles, and checks the 9-of-14 count.

## Geographic identifiers

- `ags`: 8-digit Amtlicher Gemeindeschlüssel for the Landkreis or equivalent. Always ends in `"000"` (no sub-municipality component).
- Examples: `05154000` Kreis Kleve, `05334000` Städteregion Aachen, `07335000` Landkreis Mayen-Koblenz.

## No harmonization

This dataset is currently published only in unharmonized form (original boundaries at the time of each election). County boundaries since 1975 in the West and since the 1990s reforms in the East have been largely stable, so the gain from harmonization is limited. A future `02_landrat_harm.R` could be added using `data/crosswalks/final/cty_crosswalks.csv` if comparability across the few historical Kreisreformen becomes important.

## Known data-quality issues

- **NRW 2025 Stichwahl date typo (patched)**: Same upstream IT.NRW issue affecting OB rows also affects Landrat rows in the 2025 file. All 2025 SW dates are encoded as `2020-09-27` in the source XML. The pipeline patches them to `2025-09-28` automatically; see [`code/mayoral_elections/01_mayoral_unharm.R`](../../../code/mayoral_elections/01_mayoral_unharm.R) for the workaround. Will be removed once IT.NRW corrects the source.

## Provenance

Two pipeline branches feed this dataset:

**1. Mayoral pipeline** (in `code/mayoral_elections/`) — produces Landrat rows for BY, NRW, RLP, NI, SL, and MV because their raw files mix Landrat and Bürgermeister/Oberbürgermeister together. The split happens at the end of stages 01 and 01b. MV additionally uses a Python Stage-0 ([`00_mv_parse.py`](../../../code/mayoral_elections/00_mv_parse.py)) that parses the 69 LAIV-MV Direktwahlen PDFs into a candidate-level intermediate which stages 01/01b read.

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
