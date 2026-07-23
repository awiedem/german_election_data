# Landrat Election Data — Source Inventory by State

Reference document inventorying *all* potential sources for direct Landrat (county head) elections across the 16 German states. Used to plan additions to `data/landrat_elections/final/`.

Last updated: May 2026.

## Constitutional context — which states elect Landräte directly?

| State | Direct election of Landrat? | Since | Notes |
|---|---|---|---|
| Bayern (BY) | **Yes** | always | All 71 Landkreise; current data covers 1945–2025 |
| Brandenburg (BB) | **Yes** | 1993 | All 14 Landkreise |
| Hessen (HE) | **Yes** | 1992 (first elections 1997) | All 21 Landkreise; 75.16% support in 1991 referendum |
| Mecklenburg-Vorpommern (MV) | **Yes** | ~2002 | All 6 Landkreise (post-2011 reform) |
| Niedersachsen (NI) | **Yes** | 1996 | Abolished the 2-track system in 1996 |
| Nordrhein-Westfalen (NRW) | **Yes** | 1999 | All 31 Landkreise + Städteregion Aachen |
| Rheinland-Pfalz (RLP) | **Yes** | 1994 | All 24 Landkreise |
| Saarland (SL) | **Yes** | 2008 (RVS) | Only Regionalverband Saarbrücken (SL has 6 Landkreise but only RVS has direct election; the rest elect via Kreistag) |
| Sachsen (SN) | **Yes** | 1994 | All 10 Landkreise (post-2008 reform) |
| Sachsen-Anhalt (ST) | **Yes** | 2007 | All 11 Landkreise (post-2007 reform); before 2007 indirect |
| Thüringen (TH) | **Yes** | 1994 | All 17 Landkreise |
| Schleswig-Holstein (SH) | ⚠️ Direct only 1998–2009 | abolished Sept 2009 | Reverted to Kreistag election |
| Baden-Württemberg (BW) | **No** (indirect) | n/a | Landrat elected by Kreistag for 8 years |
| Berlin (BE), Bremen (HB), Hamburg (HH) | **n/a** | n/a | City-states without Landkreise |

## Current GERDA coverage (1,469 rows in `landrat_unharm`)

| State | Years in dataset | Rows | Source file in repo |
|---|---|---:|---|
| Bayern | 1945–2025 | 1,098 | `data/mayoral_elections/raw/bayern/20251114_Wahlen_seit_1945.xlsx` (`Amtstitel == "Landrat/Landrätin"`) |
| NRW | 2009, 2014, 2015, 2020, 2025 | 151 | `data/mayoral_elections/raw/nrw/KW * Oberbürgermeister-Landratswahlen.xlsx` |
| RLP | 1994–2024 | 119 | `data/mayoral_elections/raw/rlp/20251219_..._Direktwahlen 1994-2024.xlsx` (sheet `Landräte`) |
| Niedersachsen | 2006–2025 | 99 | `data/mayoral_elections/raw/niedersachsen/Direktwahlen .../*.pdf` |
| Saarland | 2024 | 2 | `data/mayoral_elections/raw/saarland/Wahldaten_Bürgermeisterwahlen_2019-2025.xlsx` (RVS only) |

## Known gaps in current GERDA coverage

### Likely gaps in already-covered states

- **Niedersachsen 1996, 1999, 2001, 2004**: Direct election began 1996, so up to 4 election cycles before our 2006 start may exist. Source: `nls.niedersachsen.de` historical archives. *Investigate per-Kreis websites.*
- **NRW 1999, 2004**: First direct elections in NRW began 1999. Our data starts 2009. There were 1999 + 2004 election cycles we don't have yet. Source: IT.NRW historical archive (probably exists but not in the same Excel format we're using).
- **RLP**: 1994–2024 looks complete, but may not include all by-elections (Nachwahlen) outside the regular cycle.
- **Bayern**: 1945–2025 looks complete (1098 rows × 71 Landkreise ≈ 15 elections each on average). Should be comprehensive.

### States completely absent from our dataset

| State | Source portal | Format | Coverage estimate | Difficulty to integrate |
|---|---|---|---:|---|
| **Thüringen** (TH) | [wahlen.thueringen.de](https://wahlen.thueringen.de/kommunalwahlen/kw_wahlergebnisse_LR.asp) | Excel downloads per year | 11 election cycles 1994–2024 | **Easy** — direct downloads available |
| **Sachsen** (SN) | [wahlen.sachsen.de](https://wahlen.sachsen.de/landratswahlen-2022.html) | Excel | At least 2015, 2022, 2025; 1994–2008 unclear | **Easy** for recent; investigate older |
| **Brandenburg** (BB) | [wahlergebnisse.brandenburg.de](https://wahlergebnisse.brandenburg.de/) | Per-Kreis HTML pages | ~14 Kreise × ~5 cycles since 1993 ≈ 70 elections | **Medium** — scraping per-Kreis required |
| **Sachsen-Anhalt** (ST) | [wahlergebnisse.sachsen-anhalt.de](https://wahlergebnisse.sachsen-anhalt.de/) | Excel for recent; older unclear | 11 Kreise × ~3 cycles since 2007 ≈ 33 elections | **Medium** |
| **Hessen** (HE) | [wahlen.hessen.de](https://wahlen.hessen.de/kommunalwahlen/direktwahlen) (no centralized download); per-Kreis portals (e.g. `votemanager-gi.ekom21cdn.de`) | Decentralized — each of 21 Kreise has its own web portal | 21 Kreise × ~5 cycles since 1997 ≈ 100 elections | **Hard** — no central download |
| **Mecklenburg-Vorpommern** (MV) | Per-Kreis websites; [laiv-mv.de](https://www.laiv-mv.de/Wahlen/) does not centralize Landrat results | HTML/PDF per Kreis | 6 Kreise × ~3 cycles since 2002 ≈ 18 elections | **Hard** — fully decentralized |
| **Schleswig-Holstein** (SH) | Per-Kreis archives only (e.g. [Stormarn 1998, 2003, 2008](https://www.kreis-stormarn.de/kreis/wahlen/landratswahl.html)) | PDF | ~11 Kreise × 2–3 elections in 1998–2009 ≈ 25 elections | **Hard** — historical PDFs only |

### Out of scope

- **Baden-Württemberg**: Landrat is elected by Kreistag, not by voters. No popular-election data exists.
- **Berlin / Bremen / Hamburg**: City-states without Landkreise.

## Source URL patterns (for future scraping work)

### Thüringen — easiest to integrate
```
Per year (statewide):       https://wahlen.thueringen.de/kommunalwahlen/kw_wahlergebnisse_LR.asp
Year/cycle index file:      ../NeuLesen.asp?seite=LRInfoG{YYYY}
Stichwahl variant:          ../NeuLesen.asp?seite=LSInfoG{YYYY}
Years available:            1994, 2000, 2006, 2012, 2018 (statewide cycles)
                            + 2014, 2015, 2020, 2021, 2023, 2024 (mid-term Kreis-specific)
```

### Sachsen
```
Recent:   https://wahlen.sachsen.de/landratswahlen-{YYYY}.html
Excel:    https://wahlen.sachsen.de/download/Landrat/statistik-sachsen_landratswahl{YYYY}_ergebnisse_endgueltig.xlsx
2022 confirmed: https://wahlen.sachsen.de/download/Landrat/statistik-sachsen_landratswahl2022_ergebnisse_endgueltig.xlsx
```

### Brandenburg
```
Per Kreis: https://wahlergebnisse.brandenburg.de/{kreisid}/0/{date_yyyymmdd}/landratswahl_kreis/index.html
Stichwahl: https://wahlergebnisse.brandenburg.de/{kreisid}/{kreisid}/{date_yyyymmdd}/landratswahl_kreis/index.html
Kreis IDs: 60=Barnim, 62=Elbe-Elster, 71=Spree-Neiße, 73=Uckermark, ... (14 total)
```

### Sachsen-Anhalt
```
Portal:    https://wahlergebnisse.sachsen-anhalt.de/
Pattern:   /wahlen/{wahlart}{YY}/index.php — but Landratswahl pattern needs investigation
```

## Recommended additions, in order of cost-benefit

1. ~~**Thüringen**~~ — ✅ DONE May 2026: 17 Kreise × 9 cycles, 99 unharm rows, 232 candidate rows. See `code/landrat_elections/00_th_parse.R`.
2. ~~**Sachsen (incl. 2015)**~~ — ✅ DONE May 2026: 13 distinct Kreise across 6 cycles (2002, 2008, 2015, 2020, 2022, 2025), 38 unharm rows, 127 candidate rows. See `code/landrat_elections/00_sn_scrape.R`. The 2015 wahlarchiv HTML pages have the same layout as 2008.
3. ~~**Brandenburg 2018-2026**~~ — ✅ DONE May 2026: 14 Kreise, 24 unharm rows, 74 candidate rows. **Pre-2018 (1993-2010) NOT addressed**: BB only introduced direct Landrat elections in 2010 (5 Kreise tried, only 1 reached quorum). The official portal does not host these; would require per-Kreis archive scraping or hardcoding from press releases. Estimate: 2-3 days for the 5 attempted 2010 elections.
4. ~~**Sachsen-Anhalt (incl. 2015)**~~ — ✅ DONE May 2026: 11 Kreise + 2015 Altmarkkreis Salzwedel, 14 unharm rows, 43 candidate rows. **Still missing**: 2019+ — these are mid-cycle elections in individual Kreise; the LSA portal organizes them under `kw{YY}` (Kommunalwahlen) which mainly hosts Kreistag (parliament) elections, not Landrat. Per-Kreis search needed. Estimate: 3-5 days.
5. **NRW pre-2009** (1999 + 2004 cycles) — investigate IT.NRW historical archive. Estimate: 1 day.
6. **NI pre-2006** (1996, 1999, 2001, 2004 cycles) — extend the existing PDF-parsing pipeline. Estimate: 2 days.
7. **Hessen** — decentralized per-Kreis portals; harder. Estimate: 1 week+.
8. **MV** — same situation as HE; harder. Estimate: 1 week.
9. **SH 1998–2009** — historical PDFs per-Kreis; lowest priority because of small size and one-off nature. Estimate: 3 days.

## Implementation pattern (for whoever picks this up)

For each state added:
1. Create `data/landrat_elections/raw/{state}/` and place raw files there. Add to "Raw data is read-only" list in `CLAUDE.md`.
2. Create `code/landrat_elections/` directory with state-specific ingestion scripts (analogous to how the mayoral pipeline does it).
3. Output rows must have the same schema as existing `landrat_unharm` (16 cols) and `landrat_candidates` (32 cols). See `data/landrat_elections/final/README.md`.
4. Add the new state to the regression-test in `code/mayoral_elections/99_audit.R`.
5. Update this document and `data/landrat_elections/final/README.md` with the new coverage.

## How this analysis was conducted

Web research conducted May 2026 by checking the Landeswahlleiter / Statistisches Landesamt portal for each state, plus Wikipedia and individual Kreis websites. The constitutional context (which states have direct Landrat election) was cross-referenced with [Wikipedia's "Landrat (Deutschland)" article](https://de.wikipedia.org/wiki/Landrat_(Deutschland)) and the [Deutscher Landkreistag publication on direct election](https://www.landkreistag.de/images/stories/publikationen/band%2089%20gesamt%20online.pdf).
