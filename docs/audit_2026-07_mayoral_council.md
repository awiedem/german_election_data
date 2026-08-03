# Critical audit — mayoral, Landrat, municipal-council and county-council pipelines

**Date:** 2026-07-30
**Scope:** raw sources → Stage 0 parsers → Stage 1 (`*_unharm`) → Stage 2 (`*_harm`) → derived panels,
for `data/mayoral_elections/`, `data/landrat_elections/`, `data/municipal_elections/`,
`data/county_elections/`.
**Method:** 22 independent auditors swept the four pipelines (data integrity, code logic, raw
inventory), producing 257 candidate findings. These were deduplicated into 78 root-cause clusters,
and every cluster was then handed to an adversarial verifier tasked with *refuting* it by
re-deriving each number independently from the data and raw sources. This document records only
**verified** results; refuted and corrected claims are noted as such.

The three existing audit suites (`code/mayoral_elections/99_audit.R`, `98_full_audit.R`,
`code/landrat_elections/99_audit.R`) pass with 0 errors both before and after this audit. Every
defect below is invisible to them — which is itself a finding, addressed in §7.

---

## 1. Executive summary

| Severity | Clusters | What it means |
|---|---|---|
| **Critical** | 19 | Wrong values in published outputs, or silent loss of data that exists |
| **Major** | 36 | Likely-wrong values, silent-failure risk, or recoverable data not ingested |
| **Minor** | 22 | Inconsistency, fragility, documentation drift |

The single largest defect is the **HW/SW pairing engine in `01b_mayoral_candidates.R`**, which
assigns **104 of 501 Hessen runoff cycles the wrong winner** (including Frankfurt 2012 and the
Ahnatal 2020 Losentscheid) and smears runoff results across non-participating candidates in
Bayern, Thüringen and Sachsen.

Four defects each corrupt an entire state-year of council data:
Sachsen county 2019/2024 `valid_votes` holds the **invalid**-ballot count (40× understated);
Sachsen county drops all Große-Kreisstadt rows (**16.8 %** of the 2024 electorate);
Hessen county `eligible_voters` uses a sub-population in five years (turnout overstated 5–14 pp);
Hamburg 2025 municipal doubles the electorate and halves BSW's share.

Two harmonisation defects fabricate votes: `municipal_harm` (2021 boundaries) inflates
Thüringen 2024 by **+11.1 %** and Brandenburg 2024 by **+3.7 %** via an unnormalised crosswalk
inversion; a crosswalk defect splits Bad Neuenahr-Ahrweiler **50/50 into Dernau** in every
`municipal_harm_25` year.

---

## 2. Critical findings

### 2.1 Mayoral / Landrat

**C-1 · Hessen runoff winners are wrong in 104 of 501 cycles** (`01b_mayoral_candidates.R`)
The HE block converts `candidate_gender` with `nzchar` but never `candidate_name`, so `fread`'s
empty strings survive. `add_match_key` (:2422-2426) tests only `!is.na(candidate_name)`, giving every
unnamed HE candidate the identical key `""`. `distinct()` (:2449) then keeps one runoff row and
`left_join` (:2460-2463) fans it onto **all** Hauptwahl candidates; the winner override (:2580-2595)
picks `which.max` over the now-identical values and lands on the first-round leader.

Verified: 101 of 474 mayoral + 3 of 27 Landrat runoff cycles carry the wrong winner. 404 of 499
runoff loser results are absent. 1,996 HE winner rows have empty names; the panel collapses 1,995
elections into 426 pseudo-persons, corrupting every HE incumbency variable.
Fixtures: Frankfurt 2012 crowns CDU (true: Feldmann, SPD 57.4 %); Darmstadt 1993 gives all 7
candidates `votes_sw = 20,282`; Ahnatal 2020's Losentscheid is reversed.

**C-2 · The same pairing key smears runoffs across states**
Independently of the empty-name variant, `match_key` is the raw name string or `__party__<label>`,
neither unique within an election, and `date_pairs` is keyed on `ags` alone with `gap > 0 & gap < 60`.
Verified: 2,807 rows carry an identical `(rank_sw, votes_sw)` pair (HE 1,894, BY 676, TH 223, SN 14)
across 842 elections; 468 HE / 127 BY / 45 TH elections have more `votes_sw` carriers than actual
runoff candidates; 293 SW-only rows sit inside elections that have HW data, 167 of them flagged
winner. Bayreuth 2012 crowns the runoff *loser*.

**C-3 · Bayern Landrat elections contaminate the mayoral datasets and the panel**
The raw "Wahlen seit 1945" sheet leaves `Amtstitel` empty on non-decisive rounds; the classifier's
`case_when` default sends them to `Bürgermeisterwahl`. `03_mayor_panel.R:71-97` bypasses the
classifier entirely and has **no office filter at all**.
Verified: 1,222 Landkreis-AGS rows in raw = 1,097 titled + 125 NA; all 125 land in `mayoral_unharm`
as Bürgermeisterwahl and **none** in `landrat_unharm`, orphaning 119 of 148 Bayern Landrat runoffs.
`mayor_panel` contains 1,063 Landrat terms / 441 pseudo-mayors on 71 Landkreis AGS.
All 125 NA rows resolve to Landrat (121 via the decisive round, 4 orphaned annulled rounds);
**zero** Landkreis-AGS rows carry a mayoral title, so an AGS-pattern fix is clean.

**C-4 · Niedersachsen 2021 runoff results are ingested as a second Hauptwahl**
`DW2021/DW_Einzel.pdf` is the runoff file but is registered with `round = "hauptwahl"` and the
Hauptwahl's date. Stage 1's `distinct()` drops all 85 runoff rows (78 mayoral + 8 Landrat winners
left NA); Stage 1b's dedup instead *mixes* the rounds, so 33 elections carry two different
`valid_votes` and 32 have candidate sums exceeding `valid_votes`.
Fixture: Hildesheim publishes Lynack's runoff total 86,375 in the Hauptwahl record.
Verified correction: **2011 had no runoffs** (the discovery pass was wrong); 2014 and 2016 *do* have
them (46 and 14 in the source overviews). Total missing runoff outcomes: **172**, of which the 2021
tranche (86) already exists in raw.

**C-5 · The Niedersachsen candidate parser produces 31 wrong winners**
`01b:1046` uses `^\s+\d+\s{2,}(.+?)\s{3,}(.+?)\s*$`; because `.` matches a space, the lazy group can
match a single space when the "Beruf" column wraps, so the name lands in `candidate_profession` and
fixed `ci+1`/`ci+2` offsets then read the wrong lines. A second regex (:1069) requires a decimal
percentage, silently NA-ing votes on the 14 (2016) and 16 (2019) lines printed with integer percents.
Verified wrong winners: Schaumburg 2006 (true Schöttelndreier SPD 65.8 %), Leer 2016 (Groote SPD
56.8 %), Aurich 2019 (Meinen EB 53.5 % — the published winner placed *third* with 14.5 %),
Heidekreis 2021 (Grote EB 52.8 %), plus 27 further mayoral elections. 210 NI mayoral and 20 NI
Landrat candidate rows have blank names.

**C-6 · Thüringen Landrat parser silently drops candidates (R regex engine trap)**
`00_th_parse.R:129` uses `[\w\s.\-äöüÄÖÜß]` inside a bracket expression. R's default TRE engine does
**not** honour `\w`/`\s` there — the class parses as `{\, w, s, .}` plus the range `\`(0x5C)→`ä`(0xE4),
which excludes space, hyphen, digits and uppercase letters. Every hyphenated or title-prefixed
surname is skipped.
Verified over all 100 sheets: 1 sheet dropped entirely (Kyffhäuserkreis 2024 **Stichwahl** — the
whole runoff is absent), 10 partially, 12 candidates lost. **4 provably wrong winners**: Weimarer
Land 2018 (Schmidt-Rose CDU 60.4 % elected in round 1), Wartburgkreis 2024 (Brodführer CDU 63.0 %),
Weimarer Land 2024 (Schmidt-Rose 58.6 %), Kyffhäuserkreis 2024 (Hochwind-Schneider SPD 58.6 %).

**C-7 · TH + ST `candidate_voteshare` is the first candidate's share for everyone**
`00_th_parse.R:178-179` and `01_landrat_combine.R:318-319` call `ifelse()` with a length-1 `valid`,
so the result is length-1 and `mutate()` recycles it.
Verified: 99/99 TH rounds affected; 163/232 TH and 33/102 ST rows off by >0.005; propagates into
`winner_voteshare` via `max(candidate_voteshare)` for 50/99 TH and 7/23 ST unharm rows.
Fixture: Sonneberg 2023 publishes 0.0436 where the true winner share is 0.4667.

**C-8 · Schleswig-Holstein: 24 of 37 municipalities carry the wrong AGS**
The hardcoded `ags_map` in `00_sh_scrape.R:168-207` is wrong for 24 entries; **21 of them are valid
codes of other municipalities**, so `mayoral_harm` files those elections under the wrong Gemeinde
(Büsum's election appears as Buchholz's). The crosswalk lookup at :155-164 is dead code.
Collisions are chained (Oldenburg i.H.→Ratekau's code, Ratekau→Schönwalde's, Stockelsdorf→Scharbeutz's,
Scharbeutz→Süsel's), so the table must be replaced wholesale, not entry-by-entry.
Affects 24/45 unharm rows, 58/110 candidate rows, 22 panel rows.

**C-9 · Niedersachsen 2013: 24 of 37 rows carry the wrong AGS**
12 rows on 9 municipalities sit on codes owned by others (Katlenburg-Lindau on Northeim's,
Ronnenberg on Sehnde's, Seevetal on Rosengarten's …) and reach `mayoral_harm` under the wrong
municipality; 15 more use nonexistent codes and are silently dropped.
Verified corrections: Langelsheim 2021 (`03153019`) and Cappeln (`03453003`) are **correct** and must
not be "fixed".

**C-10 · Saarland winner detection uses a hardcoded party whitelist**
`01_mayoral_unharm.R:470-471` recognises 6 party labels; 12 candidate labels in the source fall
outside it. Four rounds publish the wrong winner — Völklingen 2024 HW and SW (true: WIR BÜRGER
Völklingen 5,644 / 6,245, published SPD 4,532 / 4,700) and Saarwellingen 2024 (true: FWG 3,787 =
51.0 %, published Einzelbewerber 2,220). `mayoral_harm` inherits them.
Verified correction: `01b` does **not** share the whitelist, so `mayoral_candidates` and the panel
hold the right winners — the defect is confined to `01`.

**C-11 · Sachsen-Anhalt shared-AGS pairs silently drop 3 elections**
`01_mayoral_unharm.R:1832-1835` groups by `(ags, election_date, round)` and slices the top row, so
where two Gemeinden share a source AGS the second election vanishes (Heideloh, Cosa, Dannefeld).
Worse, in `mayoral_candidates` the second Gemeinde's *elected mayor* is demoted to a losing
candidate of the other Gemeinde's election and then anonymised — three real mayors erased.
`flag_shared_ags` reaches no final dataset.

**C-12 · The mayor panel invents a year-2025 row for every 2026-elected mayor**
`03_mayor_panel.R:778` hardcodes `max_year <- 2025L` and :787-789 builds `seq(election_year, term_end_year)`
which descends for 2026 terms.
Verified: exactly 1,978 rows in each annual panel with `years_since_election = -1`; 888 of the
duplicated `(ags, 2025)` keys hold two different people.

### 2.2 County-council elections

**C-13 · Sachsen 2019/2024 `valid_votes` contains the *invalid*-ballot count**
`01_county_elec_unharm.R:1127` matches `"g.ltige Stimmzettel$"` unanchored, so it selects
`ungültige Stimmzettel` — the same column already used for `invalid_votes`.
Verified: 405/405 rows per year match the invalid column, 0/405 the valid one. Statewide
`valid_votes` 27,714 vs a true 1,134,326 (2019) and 26,833 vs 1,210,420 (2024) — **40.9× / 45.1×**
understated. Party *shares* are unaffected (they use `gültige Stimmen`), but every absolute count and
every population-weighted county aggregate is wrong; `harm_21_cty` shares become invalid-ballot
weighted (error ≤0.83 pp).

**C-14 · Sachsen drops every Große-Kreisstadt row, in all years**
City results are split into Wahlkreis parts with 9-digit codes; the `nchar(ags) == 8` filters at
:1073 / :1227 discard them and **no parent row ever exists**, so this is pure loss.
Verified 2024: 23 rows → 10 cities → 363,834 eligible = **16.8 %** of the Kreistag electorate
(Zwickau, Plauen, Görlitz, Freital, Pirna, Freiberg, Bautzen, Radebeul, Riesa, Meißen).
2019: 11 cities / 406,357 = 18.1 %. Legacy years lose up to 34 cities (1999).

**C-15 · Hessen county `eligible_voters` is the "ohne Sperrvermerk" sub-population**
The column heuristic at :2288-2294 picks the first column whose first value exceeds 100.
Verified per row: 421/421 rows match the sub-column and 0/421 match "Insgesamt" in 1981, 1985, 1997,
2011 and 2016 — **2,105 rows**. Statewide turnout is overstated by 5.5 to 8.3 pp (2016: published
58.37 %, true 50.10 %); 15 rows exceed 100 % turnout. 2006 has the same layout but is parsed
correctly, so the heuristic is unstable rather than uniformly wrong.

**C-16 · NRW 2014: 17 municipalities carry another municipality's results**
A source defect (both label columns wrong together), faithfully aggregated by the parser.
Verified: Kreis Viersen is a deterministic 5-element cyclic label shift; Coesfeld and Steinfurt are
**not** block swaps but six individual foreign precinct groups (identified by precinct name and
duplicate Stimmbezirk numbers). Applying both corrections reconciles all 44 municipalities to within
50 voters of the municipal pipeline. Requires a manual remap, not a parser change.

**C-17 · MV 2019/2024 Amt-level pooled postal votes are silently dropped**
The same defect class already fixed for Brandenburg. Pool rows carry `Wahlberechtigte = 0` and are
removed by the `eligible_voters > 0` filter.
Verified: 33,722 voters and 98,888 valid votes lost in 2024 (8,581 / 25,461 in 2019); statewide
turnout published 61.71 % vs a true 64.21 %. 183 Gemeinden inside the 24 pooled Ämter have a median
turnout of 0.563 vs 0.720 elsewhere; worst case Warnow-West 54.6 → 76.4 pp. Allocation is feasible:
joining on `(Kreisname, Amtsname)` resolves 24/24 pools.

### 2.3 Municipal-council elections and harmonisation

**C-18 · `municipal_harm` fabricates votes for post-2021 election years**
`02_municipal_harm.R:74-91` relabels *forward* crosswalk weights as backward weights without
renormalising, so a municipality that merged after 2021 is copied at **full value** onto each of its
2021 constituents.
Verified: TH 2024 +284,271 votes (**+11.09 %**, 28 phantom rows), BB 2024 +153,130 (**+3.73 %**),
SH 2023 +0.54 %, plus smaller effects in MV/SN/BY/HE. 27 `ags_25` codes have chain weights summing to
≠ 1 (max 4.0 for Schwedt/Oder). `municipal_harm_25` is unaffected. The documented Bromskirchen
"split-back" fixture passes on row count while duplicating Allendorf (Eder) at full value.

**C-19 · A crosswalk defect halves Bad Neuenahr-Ahrweiler into Dernau**
`ags_1990_to_2025_crosswalk.rds` maps `07131007` 50/50 onto itself and Dernau for every year
1990–2023. The cause is in `code/crosswalks/05_build_23_25_ags_crosswalks.R:129-153`: a partial
Umgliederung of 35 inhabitants is weighted `pop_old / pop_total` over transfers *into* the target,
giving the fragment weight 1 instead of ≈0.00125; the rescale at :426-433 then produces 0.5/0.5.
Verified in every `municipal_harm_25` RP year (2019: BNA 13,832 → 6,916, Dernau 1,038 → 7,954).
A global sweep found the same signature on Rodeberg `16064055` (split 50/50 between Dingelstädt and
Mühlhausen) and the Seebad/Ostseebad rename chains (12 duplicate half-value rows — the only
duplicate keys in any harm file).

**C-20 · Hamburg 2025 doubles the electorate and distorts every share**
`01_municipal_unharm.R:3700-3702` sums Landesliste + Wahlkreislisten, whose eligible/voter columns are
identical.
Verified: shipped eligible 2,626,086 and voters 1,775,484 are exactly 2× the true 1,313,043 / 887,742;
valid votes 8,696,985 vs 4,371,246. Shares become a blend (SPD 32.14 vs 33.51 official) and **BSW,
which ran no Wahlkreislisten, is halved: 0.89 vs 1.76 %**.

---

## 3. Major findings (condensed)

**Mayoral / Landrat.** All Thüringen kreisfreie-Stadt OB and Landrat dates are file-creation
timestamps, not election days (40 mayoral rows, **all 99** Landrat rows; the true date is not present
in the source files and must come from outside) · Eisenach 2018 is duplicated under `16056000` and
`16063105`, and harmonisation keeps the wrongly-dated copy · Aachen carries its defunct pre-2009 AGS
for 2014-2025, losing 2014 and 2020 from `mayoral_harm` · Region Hannover's Regionspräsident elections
are typed `Bürgermeisterwahl` on a Kreis AGS and reach the municipality-level harm file although
`03241000` has no crosswalk row · 364 RLP Verbandsgemeinde candidate rows carry a literal space in
`ags` (fixable in one line) · 8 RLP elections held on 2010-11-07 are swallowed by a `cumsum` grouping
on a blank Wahltag column · RLP Hauptwahl rows carry the *eventual* winner, which in Leiningerland 2025
publishes a flat wrong winner (Rüttger won outright with 53.4 %; the file says SPD 46.6 %) · Sachsen
mayoral hardcodes `Bürgermeisterwahl`, ignoring the source's own `KW_OB` column (273 rows / 96 cities,
4 of which change status over time) · a Saarland recall vote (Homburg 2021) is recorded in the panel as
a mayor winning a term with 75 % · Sachsen-Anhalt's StaLA gender (8,250 values) and birth years are
discarded and replaced by name-based predictions (8 measured disagreements, 307 winners left NA) ·
85 named non-winner rows in `landrat_candidates` violate the project's own ST anonymisation policy ·
the ST historical parser repairs mis-stamped AGS only within a date, leaving ~19 wrong-municipality
elections (Gardelegen's real 2008 winner is absent from the panel, replaced by five phantom village
elections) · BW Komm.ONE's keep-filter misses 4 cycles, one of which (Gütenbach 2025) is absent from
GERDA entirely · BY 2026 Kennwort matching breaks 12 mayoral + 2 Landrat winner pairings · SH is missing
3 decisive 2024 runoffs and 9 elections from 2026 · Sachsen-Anhalt's Landrat series ends in 2015
because the scraper's probe fails silently.

**County.** HE 2021 ships 417 rows with 11-character AGS · HE `invalid_votes` is NA in 6 years
although the source publishes it (2,516 rows) · NI county `number_voters` and `valid_votes` omit
SG-level postal districts for a subset of Samtgemeinden each year (worst SG gap 42.7 %) · SH 2023
drops all four kreisfreie Städte although the raw file contains them · kreisfreie city councils are
included inconsistently across states and years (630 rows, two unreported breaks) · post-2021
boundary changes are unhandled in county harmonisation (~34 municipalities missing, absorbers
inflated) · NA metadata becomes 0 in harmonised outputs across 21 state-years · municipality
covariates are NA for all 3,928 rows from 2023 on.

**Municipal.** Thüringen `ags_name` is the wrong municipality for 5,197 rows (vote data is correctly
keyed; only the name column is scrambled — the sitze file has the right names) · TH 2024 is missing
all 5 kreisfreie Städte, but the data is **absent from the raw file**, so this is a sourcing gap ·
3 Niedersachsen Samtgemeinde aggregates escape the name-based filter because the source name column
is width-truncated, double-counting 87,021 votes · Bayern's `DIELINKE2` column is unmapped in
2002-2020, dropping 114,115 Linke votes into `other` (München 2020 publishes `linke_pds = NA` for a
3.27 % result) · RP loses 11,474 pre-1990 rows from both harm files, and the RP crosswalk patch
present in `02` is missing from `03`.

**Un-ingested data that already exists in `raw/`.** MV county 1990-2011 (6 files, 5 open natively in
readxl) · TH county 1994/1999 (HTML tables mislabelled `.xls`; AGS construction verified) and 1990 ·
SN county 1994 (Kreis-level) and 1995 (Gemeinde-level) · **NRW Kreistag 2025 — already read into
memory by `01_municipal_unharm.R:7166` and discarded by a filter, while the seats file already
contains it** · NI municipal 1981/1986 (a working parser for the byte-identical sibling file already
exists in the county pipeline; one line differs).

---

## 4. Claims that were refuted or materially corrected

Recorded so they are not "fixed" into new defects:

- **NI 2011 runoffs are not missing.** Both 2011 source PDFs contain zero occurrences of
  "Stichwahl"; plurality leaders were declared elected. The gaps are 2014 and 2016.
- **Langelsheim 2021 (`03153019`) and Cappeln (`03453003`) have correct AGS.**
- **Schaumburg 2006's true winner is Schöttelndreier**, not Jörg Farr (who won in 2011).
- **The Lühe duplicate is a Hauptwahl/Stichwahl pair**, not a vorläufig/endgültig duplicate; the
  proposed name-normalising dedup would have destroyed the Hauptwahl.
- **The 39 SN-1994 codes missing from the crosswalks lose no votes** — a manual remap covers all 39
  exactly. Actual SN 1994 loss is 6,219 votes, entirely from the Tiefenbach cross-state typo.
- **BW's 21 non-Sunday dates are source misprints**, printed verbatim in the StaLA report; the parser
  is faithful. Do not "fix" the parser.
- **`01b` does not share Saarland's party whitelist** — candidates and panel are correct.
- **Sachsen mayoral OB status must come from the source's `KW_OB` column** (273 rows / 96 cities), not
  from an AGS-ends-in-`000` rule (31 rows, and wrong for 4 cities that change status).
- **Titles inside surnames are a pipeline-wide convention**, not an HE-only defect (RLP 107, SN 25,
  HE 23, MV 18 …); fixing HE alone would increase inconsistency.
- **Velten's crosswalk split sums to exactly 1.0** and is correct.
- **BY municipal 1996 is vacuously correct** for the `DIELINKE2` defect (no Linke data at all that
  year); 2008 *is* affected. Total loss is 114,115 votes, not ~356k.

---

## 5. Fix plan

Ordering is dictated by data flow: crosswalk artefacts and Stage-0 parsers first, then Stage 1,
then Stage 2 and the panel. Within a tier, items are independent.

### Tier 0 — crosswalk artefacts (block all harmonisation fixes)
1. Fix the partial-Umgliederung weighting in `code/crosswalks/05_build_23_25_ags_crosswalks.R`
   (weight by the donor's own population), rebuild `crosswalk_ags_2023_to_2025.rds` and
   `ags_1990_to_2025_crosswalk.rds`; this repairs Dernau and Rodeberg.
2. Deduplicate rename-chain rows on `(ags, year, ags_25)` (Seebad/Ostseebad class).
3. Add `stopifnot` on weight sums per `(ags, year)` to the builders.

### Tier 1 — Stage 0 parsers
4. `00_th_parse.R:129` — replace the broken bracket class with `perl = TRUE` (C-6), add the
   per-sheet vote-sum invariant.
5. `00_th_parse.R:178-179` + `01_landrat_combine.R:318-319` — vectorise the share computation (C-7).
6. `00_sh_scrape.R` — rebuild `ags_map` from VG250/`ags_crosswalks` with a name-match assertion (C-8);
   enumerate elections from the index page; fix the NA-unsafe filter.
7. `00_th_mayoral_parse.py` / `00_th_parse.R` — take election dates from the statewide Wahltag table
   instead of the file timestamp; assert Sunday.
8. `00_sl_extra.R` — drop the `%` capture, compute shares from votes (M36).
9. `00_bw_komm_one_scrape.py` — broaden **both** the termine filter and `MAYORAL_ROUND` (M23).
10. `00_st_hist_parse.py` — run the AGS recovery over all rows, not only same-date groups (M18).

### Tier 2 — Stage 1
11. **`01b_mayoral_candidates.R` pairing engine** (C-1 + C-2 + M04, must land together):
    `"" → NA` for name fields in `standardise_candidates`; make `match_key` unique (append
    within-round rank on collision) and join one-to-one so a fan-out errors instead of smearing;
    normalise titles/whitespace; take `election_type` and round-level metadata from the Hauptwahl;
    respect an unambiguous source `is_winner` instead of unconditional `which.max`.
12. Bayern office classifier in `01`, `01b` **and** `03_mayor_panel.R` (C-3), then re-run the Landrat
    combine to recover the 119 missing Hauptwahlen.
13. NI: register the 2021 runoff file as `stichwahl` (C-4); rewrite the candidate-block regexes to
    anchor on the Lfd.-Nr. line and scan forward (C-5); rebuild the 2013 AGS lookup (C-9); add the
    `Regionspräsident` branch (M33).
14. Saarland: replace the whitelist with the `summary_types` exclusion, honour `Wahlart...3`, and
    exclude/flag Abwahl rounds (C-10).
15. Sachsen-Anhalt: group by `(ags, ags_name, …)` and carry `flag_shared_ags` (C-11); stop discarding
    StaLA gender and birth year (M19).
16. RLP: strip whitespace from VG Schlüssel (M14); group on the Schlüssel column (8 lost elections);
    use the round leader for Hauptwahl `winner_*` (Leiningerland); patch Koblenz 2017 and Lahnstein 2021.
17. Sachsen mayoral: use `KW_OB` (M22).
18. County `01`: anchor the Sachsen `gültige Stimmzettel` match (C-13); aggregate Große-Kreisstadt
    parts (C-14); resolve the Hessen Wahlberechtigte sub-column by header text (C-15); add the NRW
    2014 remap (C-16); allocate MV pooled postal votes (C-17); fix the HE 2021 AGS construction;
    extract HE `invalid_votes`; allocate the NI SG residual; include SH 2023 cities.
19. Municipal `01`: Hamburg 2025 Landesliste only (C-20); map `DIELINKE2`; replace the NI SG
    name-filter with the AGS-suffix test; take TH `ags_name` from the sitze file.
20. Anonymise ST losers in `01_landrat_combine.R`, and rebuild scraper states from the parsed rds so
    a second run is idempotent (M35).
21. Normalise `candidate_gender` to `m`/`w` in `standardise_candidates` — **and flip
    `99_audit.R:287` in the same commit** (M32).

### Tier 3 — Stage 2 and panels
22. `02_municipal_harm.R` — compute backward weights properly (C-18); port the RP crosswalk patch into
    `03_municipal_harm_25.R`; fix the Tiefenbach cross-state typo in both; extend RP remaps to all years.
23. `02_mayoral_harm.R` — hard-stop on unmatched AGS instead of `cat()`; add `round` to the dedup key;
    carry `flag_superseded`; stop summing `winner_votes` across different winners.
24. `03_mayor_panel.R` — `pmax(max_year, election_year)`; `nzchar` in `name_key`; drop Landrat rows;
    validate `ags_21` against the 2021 universe.
25. County `02` — chain post-2021 crosswalks; preserve NA instead of 0; stop publishing all-NA rows as
    `valid_votes = 1`.

### Tier 4 — coverage and documentation
26. Ingest NRW Kreistag 2025 (one filter), NI municipal 1981/1986 (one line in a copied parser),
    TH county 1994/1999, MV county 1994-2011, SN county 1994/1995.
27. Update `README.md`, `docs/codebook.md`, `docs/data_pipeline.md`, `docs/mayoral_elections_known_issues.md`
    and the website update log; document every remaining source anomaly.

---

## 5b. Outcome — what the re-run produced

Every fix below was verified against the rebuilt outputs, not merely applied.

### Wrong values corrected

| Fixture | Before | After |
|---|---|---|
| Frankfurt 2012 mayoral winner | CDU (first-round leader) | **SPD**, Feldmann, 92,215 runoff votes |
| Runoff results smeared onto non-participants | 645 elections | **0** |
| Thüringen Landrat wrong winners | 4 | **0** (Schmidt-Rose 60.4 %, Brodführer 63.0 %, Schmidt-Rose 58.6 %, Hochwind-Schneider 58.6 %) |
| Kyffhäuserkreis 2024 runoff | absent | present, 20,951 votes |
| Leiningerland 2025 | SPD 46.6 % | **CDU 53.4 %** |
| Völklingen 2024 (both rounds) | SPD | **WIR BÜRGER Völklingen** |
| Neunkirchen 2024 winner share | 0.09 | **0.529** |
| Sachsen county valid/voters ratio | 0.02 | **0.976 / 0.979** |
| Hessen county 2016 turnout | 58.37 % | **50.10 %** |
| MV county 2024 turnout | 61.71 % | **64.21 %** |
| Hamburg 2025 electorate | 2,626,086 | **1,313,043** (BSW 0.89 % → 1.76 %) |
| Munich 2020 municipal Linke | NA | **3.27 %** |
| Bad Neuenahr-Ahrweiler 2019 (harm_25) | 6,916 (halved into Dernau) | **13,814** |
| RP Tiefenbach 1994 | 6,706 | **487** |

### Silent losses recovered

| Item | Before | After |
|---|---|---|
| Bayern Landrat runoffs without their Hauptwahl | 119 of 148 | **0 of 149** |
| `landrat_unharm` rows | 1,966 | **2,105** |
| Sachsen Große-Kreisstadt electorate (2024) | dropped | **363,834** recovered; state total 2,166,357 |
| MV pooled postal votes (2024) | dropped | **33,722 voters / 98,888 votes** allocated |
| RP pre-1990 rows in `municipal_harm_25` | 0 | **11,474** |
| RP elections swallowed by a blank Wahltag | 8 lost | recovered |
| SH 2024 Stichwahlen | 3 missing | present |
| Hessen county `invalid_votes` NA | 2,516 rows | **1** |

### Fabricated values removed

| Item | Before | After |
|---|---|---|
| `municipal_harm` deviation from unharmonised totals | TH 2024 **+11.09 %**, BB 2024 +3.73 % | **0.000 % in every state-year** |
| Crosswalk weight groups ≠ 1 | 101 across four artefacts | **0** |
| Duplicate crosswalk keys | 231 | **0** |
| Duplicate `(ags, year)` in `municipal_harm_25` | 12 | **0** |
| Annual-panel rows predating their own election | 1,978 | **0** |
| Duplicated municipality-years in the annual panel | 1,978 | **0** |
| Bayern Landrat terms in `mayor_panel` | 1,063 | **0** |
| Hessen pseudo-persons in the panel | 1,995 elections → 426 people | 435 rows → **429 people** |
| Elections given an arbitrary winner | 545 | **7** (all genuinely undeterminable, left NA) |
| Named Sachsen-Anhalt losers in `landrat_candidates` | 85 | **0** |
| Non-Sunday Thüringen dates | all 99 Landrat + 40 OB rows | **0** |

### Two regressions caught during verification, before they shipped

Both were introduced by fixes in this pass and found by re-running the suites:

1. Declining to guess a winner when no votes exist correctly stopped inventing
   one for multi-candidate elections — but also stripped the winner from **538
   uncontested single-candidate elections**, where the sole candidate is the
   winner by definition and `mayoral_unharm` still named one. A sole candidate
   is now flagged; the two files agree again.
2. The audit's BW elected-women fixture filtered `candidate_gender == "female"`,
   which only selected StaLA values because predictions used a different
   vocabulary. Unifying the vocabulary would have turned that 114 into 300
   silently; the check now keys on `candidate_gender_source == "raw"`.

### Audit-suite status

All three suites pass on the rebuilt outputs (0 errors). Five checks were
corrected rather than satisfied, because they asserted behaviour the audit
proved wrong:

- an exact tie is a real outcome, so two candidates may share runoff rank 1 —
  but only when they polled the same number of votes (Sulzemoos 1956 259:259,
  resolved by a repeat election; Würzburg 2008 673:673);
- three Sachsen-Anhalt elections are two different Gemeinden sharing one source
  AGS, so `ags_name` is part of the key there and the pooled candidate maximum
  is not the per-Gemeinde winner;
- an election with no votes, no shares and no source flag has no winner, rather
  than one assigned by row order;
- Sachsen-Anhalt losing candidates must be anonymised in `landrat_candidates`
  too, so a missing name there is required rather than a defect.

## 5c. Follow-up: the federal pipeline (2026-07-30)

The report originally said the harmonisation-inversion defect was "still live in the
federal/state pipelines". A follow-up measured that rather than assuming it, and the claim
was half wrong:

- **Federal, confirmed and fixed** (`02_federal_muni_harm_21.R`, commit `86ce618a`):
  BTW 2025 was inflated by **207,993 valid votes** (TH +8.95 %, BB +3.85 %), with 33
  `ags_25` codes whose chain weight exceeded 1 — worst Uder at 11.0. After the fix the
  whole file is **+2 votes**. The damage was smaller than the municipal pipeline's
  +11.1 % only because the crosswalk artefacts had already been normalised per donor.
- **State, refuted.** `03_federal_muni_harm_25.R` and all three state harm scripts read
  the same artefacts but only ever forwards, so they never inverted anything. No state-year
  deviated by more than 0.13 % before any change.
- All five harm scripts now stop if a source row's weights fail to hand out exactly 100 %
  of its votes — keyed on *votes* rather than weights, because rows with no electorate hit
  a unit-weight placeholder that made a weight-based guard report 148 false positives.
- Regenerating the crosswalks had left two `*_harm_25` outputs stale; both were rebuilt.
  Worth checking sibling outputs after any crosswalk rebuild.

### Found while verifying that fix, and still open

`federal_muni_harm_21` contains **19,704 rows with a 9-character AGS** — all
Schleswig-Holstein, exactly 2,463 per year across 1990-2017, `ags_name` NA. For SH 1990 the
8-character rows already sum to 1,624,679, exactly the unharmonised total, while the
9-character rows add a further **11,027,320 phantom votes**. The crosswalks are not the
cause (all uniformly 8-character), and `federal_muni_harm_25` is clean, so the defect sits
in this script's own path and almost certainly predates both fixes. Being handled
separately.

`ags_crosswalks` itself still carries **909 `(ags, year)` weight groups summing to as much
as 20** — Bavarian gemeindefreie Gebiete across every vintage 1990-2020. Harmless only
because those territories have no electorate; the audit's own sweep confirmed none appears
in `municipal_unharm`. It feeds the municipal, county, mayoral and federal pipelines.

## 6. Source anomalies to document, not fix

Bavaria 2026 XLSX turnout contradicts its own counts in 3 rows · Bavaria 2026 leaves `Stichwahl`
blank for Mehring's real runoff and sets it for two non-runoffs · Koblenz 2017's raw workbook swaps
the two share cells (proved by column arithmetic) · Lahnstein 2021's turnout is shifted one column
left · BW's 21 non-Sunday dates and the Neudenau `08.03.2023` misprint · Dillenburg 2025 differs by
one vote between the two HSL products · HE Cornberg/Hanau winner-name misspellings · SN 1994's
four Kreise that held no election · six Bavarian Gemeinden recorded as "zur Zeit ohne Bürgermeister".

---

## 7. Process finding: the audit suites cannot see these defects

All three suites pass. They check fixtures and cross-leakage but never the invariants that would have
caught these classes. Recommended additions:

- **Arithmetic invariants:** `valid_votes > invalid_votes`; `eligible_voters ≥ number_voters`;
  per-election `sum(candidate_votes) == valid_votes` (where absolute votes exist);
  `winner_votes == max(candidate_votes)`.
- **Pairing invariants:** no `(ags, election_date, election_type)` may contain more `votes_sw` carriers
  than `n_candidates_sw`; no duplicate `(rank_sw, votes_sw)` within a round.
- **Identifier invariants:** every AGS exists in the register for its election year, with a matching
  name; every `ags_21` is a valid 2021 municipality; `nchar(ags) == 8`.
- **Harmonisation invariants:** crosswalk weights sum to 1 per `(ags, year)`; per state-year
  `|Σ valid_votes(harm) − Σ valid_votes(unharm)| / Σ unharm < 1 %`; no duplicate `(ags, year)` keys.
- **Cross-pipeline invariants:** for coincident election days, county and municipal `eligible_voters`
  must agree per `(ags, year)` — this alone would have caught C-16 and C-17.
- **Coverage invariants:** each state's municipality count per election year must not drop by more
  than a threshold between consecutive elections; no silent `tryCatch` skip may reduce a state to zero
  rows (the Brandenburg-absence class).
- **Idempotency:** running `01_landrat_combine.R` twice must be a no-op.

---

## 8. Follow-up: ingesting the data that already existed in `raw/`

Tier-4 item 26 of the fix plan, worked one election at a time after the main branch settled. Each was
committed separately, with its own raw-to-final reconciliation; `git log` carries the detail.

| Ingestion | Rows added | Level | Notes |
|---|---:|---|---|
| NRW Kreistagswahl 2025 | 53 | Kreis | The file was already being read by `01_municipal_unharm.R`, which kept the `"Krfr."` rows and discarded the Kreise. Aachen appears both as a kreisfreie Stadt and inside the Städteregion, so the city row is dropped; the remainder reconciles to the file's own Land row. |
| NI Gemeindewahlen 1981/1986 | 2,058 | Gemeinde | Sat unreferenced inside a zip; the county pipeline already had a working parser for the byte-identical sibling file. |
| TH Kreistagswahlen 1994/1999 | 2,266 | Gemeinde | HTML tables mislabelled `.xls`. |
| MV Kreistagswahlen 1994–2011 | 4,589 | Gemeinde | Amt-level postal-vote pools are allocated rather than dropped. |
| SN Kreistagswahl 1994 | 19 | **Kreis** | Legacy BIFF; needs a Python Stage 0 (`00_sn_1994_parse.py`). |

**Sachsen 1994** is the only one that changed the schema, and is worth recording in full.

The report is Kreis-level — the Landesamt published no Gemeinde breakdown — so it is routed through
the county crosswalk by the same `county_level_years` mechanism NRW 2025 uses, and reaches
`county_elec_harm_21_cty` but not `_muni`. All nine party columns reconcile exactly to the printed
statewide totals (gültige Stimmen 4,073,279), and every non-1994 row of both outputs is bit-identical
to the previous build.

Two judgement calls:

- **`valid_votes` semantics.** Saxony stores valid *ballots* from 1999 onward, but 1994 prints that
  figure only as a percentage of Wähler, to one decimal. It is reconstructed as `pct/100 × Wähler`
  rather than substituting the three-vote total, which would have reproduced exactly the semantic
  flip this audit found in Sachsen 2019/2024 (C-13). Table 1 confirms the relation is exact —
  `1,559,982 × 94.22782 % = 1,469,937`, the printed statewide Gültige Stimmzettel — so the only error
  is the source's own rounding, about ±0.05 % of ballots. Party shares use the exact three-vote total
  and carry no such error.
- **`flag_partial_coverage` (new column, `_cty` only).** The Sächsisches Verfassungsgericht annulled
  the 1994 Kreistagswahl in Meißen, Kamenz, Dresden-Land and Hoyerswerda, and struck down the
  formation of Elstertalkreis and Göltzschtalkreis. Harmonisation is otherwise silent about donors
  that are simply absent: it weights whatever arrives and emits a row that looks complete, so Kreis
  Meißen 2021 would have been published with 98,003 electors against the ~200,000 it has in every
  neighbouring year, with nothing to mark it. The flag measures each target's covered population
  against the crosswalk's own figures and fires on the six affected counties. `NA` means not
  assessed. Part A (municipality level) has the same structural exposure, but no live instance
  is known: the Thüringen 2024 case cited when this was written is not real (see §12), and
  `ags_crosswalks` carries no population column, so that check needs a different population
  source and stays on the worklist.

**Sachsen 1995 was deliberately not ingested.** The re-run covers the four annulled Kreise at
Gemeinde level, but the source carries **no AGS at all** — only names, in Wahlkreis sections. Of 179
Gemeinden, 147 match a name in the 1995 crosswalk vintage (8 of them ambiguously) and 32 do not;
loosening the match converts most "unmatched" into "ambiguous" rather than resolving it
(`Reichenbach` alone offers 10 candidate codes). Larger towns are split across Wahlkreise
(`Auerbach-West` is an entire Wahlkreis, not a place), so the split rows must also be recombined.
Ingesting it therefore means ~32 per-name adjudications in which a wrong pick silently files a real
election under the wrong municipality — the same defect class as the 24 wrong AGS this audit found
in SH and the 24 in NI 2013. It needs the Landkreis/Wahlkreis section context worked through by hand
and is left open rather than guessed.

---

## 9. Follow-up: missing elections — Niedersachsen runoffs (2014, 2016, 2019)

Worklist item 2. The premise recorded after the audit was that ~86 NI runoffs were
"missing", with the *Stichwahl* mentions in the Übersicht PDFs cited as proof they exist.
That proof was misread: it shows the runoffs **happened**, not that we held their results.
Checking each file's page count against its own text settles it — for every year,
`pages == count("Stichwahl erforderlich") + count("kann als gewählt gelten")`, so every
`*_Einzel.pdf` in `raw/` is Hauptwahl-only. The two 2016 file pairs are text-identical
duplicates of the Hauptwahl, not a Hauptwahl/Stichwahl pair. Only 2021 ever had a runoff
file. **The runoff results were never downloaded**, so this was a sourcing task.

The Landeswahlleitung publishes them as one-page summary tables, now in `raw/`:

| Year | Runoff date | Rows | Content |
|---|---|---:|---|
| 2014 | 15 Jun 2014 | 46 | winner, party, votes, %, electorate, voters, valid, turnout |
| 2016 | 25 Sep 2016 | 14 | same |
| 2019 | 16 Jun 2019 | 23 | **winner and party only** — no counts are published |

2024 (2 runoffs) and 2025 (1) remain missing: those year pages carry only *Wahltermine*.
2015, 2018 and 2020 held off-cycle Direktwahlen that GERDA has no rows for at all, and
publish no results either — 2017 does (5 elections, rich format, downloaded but not yet
ingested). All are recorded on the worklist.

**These tables carry no Schlüssel, only a Kommune name** — the same shape as the Sachsen
1995 problem in §8. It is nonetheless safe here, because the runoffs are a *closed set*:
every row must correspond to a Hauptwahl page of the same year that says "Stichwahl
erforderlich". So the AGS is **inherited from the matched Hauptwahl** rather than derived
from the name, and the join refuses to proceed unless it is exactly one-to-one. It matched
46/46, 14/14 and 23/23 with no duplicate keys and four abbreviation aliases listed
explicitly. Two further invariants hold: every runoff date is a Sunday, and every runoff
winner's share exceeds 50 % (0.505–0.732), which is true by definition and would break
first if rows were mismatched.

Do not infer "needed a runoff" from a missing winner: in 2016 and 2019 two more rows lack
a winner than actually went to a runoff, so the parser now carries the source's own
`Stichwahl erforderlich` flag.

**What it changed.** 83 runoffs added (77 mayoral, 6 Landrat). Every NI election in those
years now has a winner: 2014 156→201, 2016 19→31, 2019 54→74. Non-NI rows are
bit-identical and the audit suites report the same six warnings as before.

**Eight seated mayors were wrong.** The panel had been seating the *Hauptwahl leader* —
every one of them below 50 % — where the runoff was actually won by someone else:

| Municipality | Year | Panel had | Actually elected |
|---|---|---|---|
| Langenhagen | 2014 | SPD, 35.0 % | Mirko Heuer, **CDU**, 58.8 % |
| Hann. Münden | 2014 | SPD, 47.9 % | WGR, 56.9 % |
| Gehrden | 2014 | CDU, 43.5 % | Cord Mittendorf, **SPD**, 50.9 % |
| Bad Fallingbostel | 2014 | CDU, 37.3 % | Karin Thorey, **EB**, 53.5 % |
| Leer (Ostfriesland) | 2014 | EB, 31.4 % | **CDU**, 54.7 % |
| Wildeshausen | 2014 | EB, 36.6 % | Jens Kuraschinski, **WGR**, 54.4 % |
| Celle | 2016 | SPD, 46.2 % | Dr. Jörg Nigge, **CDU**, 50.7 % |
| Essen (Oldenburg) | 2016 | CDU, 38.2 % | Heinrich Kreßmann, **EB**, 57.0 % |

A further 44 rows kept the right winner but carried the Hauptwahl share instead of the
decisive one. Langenhagen and Celle were verified against external sources: Heuer beat the
incumbent Friedhelm Fischer (SPD) with 58.8 %, and Nigge unseated Dirk-Ulrich Mende (SPD) —
in both cases the panel had been reporting the loser.

**Candidate file.** The tables name only the Wahlsieger, so one candidate row per runoff is
emitted. The runner-up is deliberately *not* reconstructed as `valid_votes − winner_votes`:
that is right only if the runoff ran with the Hauptwahl's top two, and a withdrawal
promotes the third, so it would publish a named candidate who may never have stood. For the
same reason `n_candidates` is NA rather than 1 for these rounds, and `is_winner` comes from
the source's own "Wahlsieger" column rather than vote rank — 2019 publishes no counts to
rank by.

---

## 10. Follow-up: missing elections — Schleswig-Holstein 2026

Nine 2026 mayoral elections were published on wahlen-sh.de and absent from GERDA
(`mayoral_unharm` held only Ratekau and Schwentinental). Root cause: the election
registry in `00_sh_scrape.R` is hand-maintained, so a newly published election is
invisible to the pipeline until somebody notices.

**The registry is not replaced by index enumeration — it is checked against it.** The
index omits pages in both directions: some Hauptwahl pages appear only when there was no
runoff, and the 2024 runoffs appear nowhere on it. So the index is used as a *lower bound*
the registry must cover. `check_sh_registry_complete()` now fetches
`andere_wahlen.html`, keeps the slugs that look mayoral (`bgm*`, `bm_*`, `ob_*` —
Bürgerentscheide `be_*`, Abstimmungen, Einwohnerbefragungen and Gemeindewahlen `gkw_*` are
not mayoral) and **stops** if any is missing from the registry. It currently reports the
registry covering all 45 mayoral elections linked from the index.

The nine: Ratekau and Schwentinental (8 Mar), Bönningstedt and Reinfeld (19 Apr), Reinbek
and Trittau (10 May), Ammersbek, Heide and Schwarzenbek (7 Jun). None went to a runoff —
both slug spellings (`bgm_stichwahl_2026_*`, `bgmstichwahl_2026_*`) 404 for every one, and
the scraper's own check confirms no sub-50 % Hauptwahl lacks a Stichwahl. Schwarzenbek is
still flagged *vorläufig* on the portal.

**A second defect surfaced while ingesting them.** In a single-candidate confirmation
election the portal puts the ballot option, not a party, in the party column — and spells
it three ways across pages: `Ja-Stimmen`, `Ja Stimmen` and plain `Ja` (likewise Nein).
Both stages compared against the literal `"Ja-Stimmen"` / `"Nein-Stimmen"`, so Bönningstedt
and Reinbek were published with `winner_party = "Ja Stimmen"` and `"Ja"`. Now matched with
`^ja[ -]?(stimmen)?$` / `^nein[ -]?(stimmen)?$`, case-insensitive, in both `01` and `01b`.

The more serious half of that defect is what the old code would have done to a *failed*
confirmation vote: dropping the Nein rows and taking the top row by votes seats the
rejection as a win — the same failure the Saarland `…abwahl` recall rounds produce. A guard
now stops the pipeline if any confirmation election has `nein >= ja`. Calibrating it caught
a mistake in the guard itself: a confirmation election must be identified by the presence of
a **Nein** row, because on some pages the yes-row carries the candidate's real party
instead (Lütjenburg 2024 is `EB`, not `Ja`), so scoring only `Ja`-labelled rows reported the
yes side as zero and called a 74.7 % win a defeat.

Net: SH goes from 50 to 57 rows in `mayoral_unharm` with 9 panel terms for 2026; rows
outside SH and NI are bit-identical and the audit suites report the same six warnings.

---

## 11. Follow-up: missing elections — Sachsen-Anhalt Landrat 2019-2026

The ST Landrat series stopped at 2015 while eleven Kreise voted between 2019 and 2026.
The worklist blamed `00_st_scrape.R:52-86` — `tryCatch(error = -1, warning = -1)` plus a
silent `file.remove` — and that is right, but the diagnosis it enabled was wrong. Probing
every `lr{YY}dat{N}.csv` for 2007-2026 by hand returns **404 for all but three files**:
`lr07dat3`, `lr14dat1` and `lr14dat3`. The per-year scheme was never the general case;
it only ever existed for those two years. Because the loop swallowed every error and then
deleted anything that was not a CSV, a 404, a moved URL and "no election that year" were
indistinguishable, so nothing ever suggested the scheme itself was the problem.

**Everything from 2019 on lives in one rolling file**, `/wahlen/lrlr/erg/csv/lr.csv`,
linked from the portal's own downloads page, in a completely different WIDE schema: 21
metadata columns then nine 7-column candidate blocks (`B{k}_TITEL`, `_NAME`, `_VORNAME`,
`_PARTABK`, `_GESCHL`, `_STI`, `_STI_SW`), carrying Hauptwahl and Stichwahl side by side —
the same shape as the ST mayoral StaLA extract. It holds 14 elections; the three with
`ART == "HaOB"` are the kreisfreie Städte (Dessau-Roßlau 2021, Halle 2025, Magdeburg 2022),
which are Oberbürgermeister elections already supplied by the StaLA mayoral source and are
skipped here rather than duplicated into the Landrat dataset.

Result: **ST Landrat goes from 23 to 39 rows**, adding 16 rounds across 11 Kreise —
Stendal 2019 (which is not even on the portal's current Terminübersicht), Harz 2020,
Salzlandkreis / Burgenlandkreis / Anhalt-Bitterfeld / Jerichower Land / Mansfeld-Südharz /
Wittenberg 2021, Altmarkkreis Salzwedel 2022, Börde 2025 and Saalekreis 2026.

The rounds validate each other exactly: **every** Hauptwahl winner below 50 % is followed
by a Stichwahl and every one at or above 50 % is not. Two runoffs reversed the first round —
Altmarkkreis Salzwedel 2022 (CDU led 38.0 %, SPD won 51.1 %) and Saalekreis 2026 (AfD led
43.3 %, CDU won 54.3 %) — which is why the winner of each round is taken as that round's
top-voted candidate rather than from the file's overall "elected person" column.

**`lr07dat1.csv` is not a missing file.** The 2007 downloads page lists only two, and
labels them differently from 2014: `dat2` is "Endergebnisse der Kreisfreien Städte und
Landkreise" and `dat3` "Endergebnisse von Gemeinden" — both Hauptwahl. No 2007 Stichwahl
results were ever published. That gap is now visible rather than assumed: six 2007
Landratswahlen sit in the data with a sub-50 % Hauptwahl winner and no runoff row
(Anhalt-Bitterfeld 45.6 %, Burgenlandkreis 13.8 %, Mansfeld-Südharz 30.7 %, Saalekreis
35.2 %, Salzlandkreis 32.0 %, Wittenberg 28.3 %), so `winner_party` on those rows is the
first-round leader, not necessarily the elected Landrat. Treat them accordingly.

Also fixed: the 2007 rows carried placeholder names like "Landkreis 15082" — `lr07dat3.csv`
has only Gemeinde rows, so the Kreis name had to be synthesised — now filled from the
register. And the probe reports its outcomes instead of hiding them (37 failures today),
with the rolling file's schema asserted so a portal change fails loudly.

Note on the audit's "unharm rows have no matching candidate row" warning, 394 → 399: all
five are Stichwahl rows, and `landrat_candidates` is wide (one row per candidate per
election, runoff in `election_date_sw`), so 336 of 347 runoff rows already behave this way.
All 11 ST runoff rows match correctly on `election_date_sw`.


---

## 12. Follow-up: Thüringen 2024 kreisfreie Städte — the gap does not exist

The worklist recorded ~714,000 votes missing from `county_elec_unharm`, on the grounds that
`Thüringen_2024_Kreistagswahl.xlsx` "has only Kreis 61-77 sheets". Checked directly, the
workbook has **23 sheets: Hinweise, Kreis 51-55 and Kreis 61-77** — the five kreisfreie
Städte are sheets 51-55 and are read like any other.

They are present and complete throughout: `county_elec_unharm` has all 22 Thüringen units
for 2024 (17 Landkreise + 5 kreisfreie Städte; Eisenach ceased to be kreisfrei in 2021, so
22 rather than the 23 of earlier years), with Erfurt at 169,887 electors and 59.2 % turnout,
Gera 75,091 / 58.6 %, Jena 82,605 / 63.0 %, Suhl 30,226 / 57.4 %, Weimar 50,968 / 60.4 %.
Both harmonised outputs carry them too — 22 counties in `_cty`, 5 rows in `_muni`. The five
account for 23.6 % of the state's 2024 electorate, and their 244,711 voters at three votes
each are the ~714,000 the note called missing.

No change was required. The entry is retracted, and the two places that had already repeated
it — §8 above and the comment in `02_county_elec_harm_21.R` — are corrected. The general
point they were illustrating still stands: Part A harmonisation cannot detect a source unit
that is simply absent. It just has no known live instance.

---

## 13. Follow-up: the Niedersachsen Samtgemeinde postal-vote residual

Worklist item 3, and the one open finding that was changing published numbers rather than
merely omitting rows. Niedersachsen counts Briefwahl in **Samtgemeinde-level districts**, so
a member Gemeinde's row holds only its Urnenwähler while the Samtgemeinde row holds the
full total. `ni_keep_entities()` discarded the Samtgemeinde row as an aggregate — which did
not merely drop a subtotal, it silently deleted every postal vote, leaving member turnout
and party shares computed on Urnenwahl alone.

The electorate was never affected: `eligible_voters` reconciles **exactly** (residual 0) in
every year, which is precisely what makes eligible voters the right weight for
redistributing the rest. Measured residual, and it matches the worklist to the vote:

| Year | voters | party votes | Samtgemeinden affected |
|---|---:|---:|---:|
| 2001 | 0 | 0 | 0 — a genuine control |
| 2006 | 17,915 | 52,643 | 27 |
| 2011 | 16,910 | 49,389 | 22 |
| 2016 | 41,781 | 122,773 | 40 |
| 2021 | 37,896 | 111,656 | 26 |

`ni_allocate_sg_residual()` now pushes the residual down onto the member Gemeinden by
eligible-voter share before the Samtgemeinde row is dropped, mirroring
`bb_allocate_postal()`. **Each party is allocated on its own residual**, not on the total,
because postal voters do not vote like Urnenwähler. A negative residual is refused rather
than "corrected": that would mean members already exceed their own Samtgemeinde, which is
not a Briefwahl split and must not be repaired by subtracting votes from real
municipalities.

Effect on published turnout: 2006 51.46 → 51.74 %, 2011 52.20 → 52.47 %, 2016 54.92 →
55.56 %, 2021 56.42 → 57.00 %. Rows carry `flag_sg_postal_allocated` (unharm only — see
below).

**A second defect surfaced while separating the cases.** Not every suffix ≥ 400 code is an
aggregate: the **gemeindefreie Bezirke Lohheide (03351501) and Osterheide (03358501)** have
that shape but no member Gemeinden. They are ordinary municipalities with their own
electorate, both are their own targets in `ags_crosswalks`, and the municipal pipeline
already keeps them — but the county pipeline dropped them **in every year**, discarding
about 600 voters per election. They are distinguished structurally (a suffix ≥ 400 row with
no indented members) and kept, adding 18 rows across 1981-2021. The blanket post-parse net
that removed them is retained as a safety catch but now exempts them and *names* what it
drops instead of only counting it — it currently reports one Samtgemeinde, `03357406`.

**1981-1996 needs no allocation.** The residual is zero in 2001, so the practice of counting
Briefwahl at Samtgemeinde level began between 2001 and 2006, after the compilation period;
and the compilation years' turnout reproduces the published Kommunalwahl figures closely
(76.23 / 72.07 / 68.25 / 64.51 % against roughly 76 / 72 / 68 / 64.5), which they would not
if postal votes were missing — the 2016 defect understated turnout by 0.64 points. The
allocator is therefore deliberately not wired into `ni_ktw_parse_compilation()`.

**Two schema notes.** `flag_sg_postal_allocated` marks only rows that actually received
votes, not every member of every Samtgemeinde — flagging 2001, whose residual is zero,
would have been a false statement. And it is published on `county_elec_unharm` only: it
describes how a *source* row was built and does not survive aggregation onto 2021
boundaries, where one output row can mix allocated and unallocated predecessors. That drop
is explicit in `02`, not incidental. Finally, `02` derives `party_vars` as "everything not
in `metadata_cols`", so a new non-party column would have been multiplied by `valid_votes`
and published as a party; the flag is registered there and a guard now stops the script if
any `flag_*` column ever reaches `party_vars`.

---

## 14. Follow-up: verifying the Niedersachsen candidate parser

Worklist item 4 — a verification task, not a fix. The rewrite touched regex logic across
nine PDF vintages and was the widest-blast-radius change on the branch.

**All four fixtures confirmed against the raw PDF text**, not merely against the parsed
output: Schaumburg 2006 Schöttelndreier SPD 44,045 (the page says "kann als gewählt gelten:
Herr Schöttelndreier, Heinz-Gerhard" — *not* Farr, who won in 2011); Leer 2016 Groote SPD
41,710; Aurich 2019 Meinen EB 48,887; Heidekreis 2021 Grote EB 33,085. Each reconciles
**exactly** to that page's printed Gültige Stimmen (66,984 / 73,449 / 91,404 / 62,653).

Two of the four are useful in a second way: Aurich 2019 and Heidekreis 2021 print the
*loser* as Lfd.Nr 1, so they demonstrate that the winner is taken by votes and not by row
order — the defect this audit found elsewhere.

**Verdict on the residual NA rows: genuinely absent from the source.** There are 83 NA-vote
and 61 NA-party rows in Niedersachsen. 72 of the 83 still carry a candidate name and 42 also
a profession, i.e. the row was read and only its numbers are missing; 48 are the last
candidate of their election. Region Hannover 2021 shows the mechanism word-by-word: page 8
ends with "7 Krause, Andrea Angestellte" and page 9 opens a different election, so her votes
(16,950, the exact shortfall) and party are nowhere in the file. The parser records what the
page shows and leaves the rest NA rather than inventing it, which is the correct behaviour.

**A larger finding the worklist did not know about.** Across 1,082 Niedersachsen elections
with candidate votes, **259 do not sum to `valid_votes`**. Splitting them by comparing the
parsed row count against the candidate rows physically present on the page:

* **162 — the source omits candidates.** The page carries exactly as many rows as were
  parsed. Celle 2021 is the archetype: the page lists one candidate (Flader, CDU, 50,198 =
  62.1 %) and then prints "Zusammen: 80883 100.0", with the other 30,685 votes belonging to
  candidates who appear nowhere on it. `pdf_data()` confirms the page holds only 83 words.
* **27 — the page-boundary truncation above**, which the parser already handles correctly by
  emitting a named row with NA votes.

This is a limitation of the Landeswahlleitung's *Einzelergebnisse*, not an extraction
failure, and `98_full_audit.R` was already right to exclude Niedersachsen from `cand.sum_hw`
("only top candidates listed"). But excluding it outright also hid how large the gap is, so
the check now prints an `[INFO]` line reporting the excluded population: **14,086 elections
in BY/NI/BW where the source lists only some candidates (BW 792, BY 13,061, NI 233)**.
Anyone summing candidate votes in those three states must expect them not to reach
`valid_votes`; the election-level `valid_votes` remains authoritative.

No parser change was needed. The rewrite is confirmed correct on every case examined.
