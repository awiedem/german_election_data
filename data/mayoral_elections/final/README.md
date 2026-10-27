# Mayoral Elections Data

Seven datasets covering mayoral elections in 13 German states (Baden-Württemberg, Bayern, Brandenburg, Hessen, Mecklenburg-Vorpommern, Niedersachsen, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland, Sachsen, Sachsen-Anhalt, Schleswig-Holstein, Thüringen), 1945--2026.

Scope: head-of-municipality elections (`Bürgermeisterwahl`, `Oberbürgermeisterwahl`, `VG-Bürgermeisterwahl`, `SG-Bürgermeisterwahl`). Head-of-county elections (`Landratswahl`) are published as a separate dataset — see [`../../landrat_elections/final/`](../../landrat_elections/final/).

## Datasets

| File | Rows | Cols | Unit | Description |
|---|---|---|---|---|
| `mayoral_unharm` | 59,700 | 19 | Election | One row per election-round (winner-level summary), original boundaries |
| `mayoral_harm` | 54,795 | 29 | Election | Same as above, mapped to 2021 municipal boundaries |
| `mayoral_candidates` | 118,417 | 49 | Candidate | One row per candidate per election cycle (wide format), original boundaries, incl. candidate characteristics |
| `mayor_panel` | 48,382 | 36 | Person-election | Within-mayor panel with person IDs and ID provenance, original boundaries |
| `mayor_panel_harm` | 48,350 | 37 | Person-election | Same as above, mapped to 2021 municipal boundaries |
| `mayor_panel_annual` | 248,974 | 32 | Person-year | Annual panel (forward-filled from elections), original boundaries |
| `mayor_panel_annual_harm` | 248,744 | 33 | Person-year | Same as above, mapped to 2021 municipal boundaries |

All files are available as `.rds` and `.csv`.

## Related datasets

Landrat (county head) elections live in their own dataset at `../../landrat_elections/final/landrat_unharm.{rds,csv}` and `landrat_candidates.{rds,csv}`. They share the same schema as the mayoral files but cover county-level units (8-digit AGS ending in `000`) and are not harmonized to municipal boundaries.

## Known data-quality issues

- **NRW 2025 Stichwahl date typo (patched)**: The IT.NRW raw file `KW 2025 Oberbürgermeister-Landratswahlen.xlsx` encodes every Stichwahl row's date as Excel serial `44101` (2020-09-27) instead of `45928` (2025-09-28). This was verified by direct XML inspection — it is a source-data error, not a coding error. Our pipeline patches the dates in [`code/mayoral_elections/01_mayoral_unharm.R`](../../../code/mayoral_elections/01_mayoral_unharm.R) and [`01b_mayoral_candidates.R`](../../../code/mayoral_elections/01b_mayoral_candidates.R) so that 2025 SW data is usable. The patch will be removed once IT.NRW corrects the source file.

- **Baden-Württemberg — hybrid (Komm.ONE + Statistisches Landesamt)**: BW (state `08`) covers all 1,101 Gemeinden, combining two sources. (a) **Komm.ONE** (`wahlergebnisse.komm.one`, the votemanager portal of BW's municipal IT provider) supplies **full candidate-level** results — all candidates, votes, full turnout, and both Hauptwahl and Stichwahl — for the municipalities/elections it publishes in its modern format (election dates ~2003–2025; **274 BW elections now have full candidate lists**). (b) The **Statistisches Landesamt** report B VII 3-j/25 (most-recent election per Gemeinde as of 31.12.2024) fills the rest — the ~217 Gemeinden whose election is only in Komm.ONE's older format, and elections predating the portal — as **winner-only** rows (no losing candidates, `n_candidates` `NA`). Komm.ONE supersedes the StaLA row for a cycle only when it captured the decisive round (a Stichwahl, or a Hauptwahl winner ≥50%); otherwise the StaLA winner is kept (e.g. the rare case where a runoff is absent from the portal). **No party affiliation either way** — BW mayoral candidates have no Wahlvorschlagsträger, so `winner_party`/`candidate_party` are always `NA`. The elected winner's **gender and birth year** come from the official register (StaLA), carried onto the Komm.ONE winner where available (else gender is name-predicted); the published count of 114 elected women is preserved. Komm.ONE candidates carry no birth year. Sources: Komm.ONE votemanager (`code/mayoral_elections/00_bw_komm_one_scrape.py`) and StaLA B VII 3-j/25 (`00_bw_parse.py`). A few per-Gemeinde StaLA fields not in the unified schema — `amtsperiode`, `hauptamtlich`, `eu_citizen`, `briefwaehler`, `wahlgrund` — are preserved in the raw intermediate `data/mayoral_elections/raw/baden_wuerttemberg/bw_parsed.csv`.

- **Brandenburg coverage (1993–2026)**: BB (state `12`) covers the **hauptamtliche** Bürgermeister-/OB-wahlen of the amtsfreie Gemeinden/Städte + the 4 kreisfreie Städte, 2010–2026, from a workbook the Geschäftsstelle des Landeswahlleiters supplied on request (Stand 31.07.2026); the older result-portal scrape supplies the 2 elections the workbook omits. Two limits stated by the source: digital transmission by the Gemeinden only became compulsory in **2014**, so 2010–2013 makes no claim to completeness; and results are back-cast onto the **current Gebietsstand (01.01.2026)**, so — as in Rheinland-Pfalz council data — BB `*_unharm` rows are already on current boundaries rather than election-year ones. Unlike Baden-Württemberg, BB publishes **party affiliation and all candidates** (Hauptwahl + Stichwahl), so it populates the full candidate schema; the elected person's gender is predicted from the name (no register data). Since 19.10.2026 losing candidates are not named in the public file (see the privacy bullet below). Single-candidate rounds are Ja/Nein votes (the candidate's votes are the Ja-Stimmen, so they sum to less than the valid votes). No separate Briefwahl results exist for BB. Two further source quirks: nine candidates are spelled differently in the two rounds of the same election (a dropped letter, a truncated surname, swapped name fields), and the published name is the externally verified one rather than either round's raw spelling; and the Wahlvorschlagsträger is free text that the source abbreviates differently per round, so for 20 winners `winner_party` in `mayoral_unharm` (which reports the decisive round) reads differently from `candidate_party` in `mayoral_candidates` (keyed on the first round) — the same nomination, written two ways. Source: Landeswahlleiter Brandenburg (workbook + `wahlen.brandenburg.de`).
- **Brandenburg 1993–2024 from the Statistische Berichte B VII 3-6 (October 2026)**: the LDS/AfS reports on the Bürgermeisterwahlen add (a) the hauptamtliche Bürgermeister and Oberbürgermeister of 1993–2008 and (b) the **ehrenamtliche Bürgermeister** of the amtsangehörige Gemeinden elected on the general Kommunalwahl days 1998, 2003, 2008, 2014, 2019 and 2024 (`office_type` "Ehrenamtliche(r) Bürgermeister*in"; 2,590 elections). Full results (all candidates with votes) exist only for the 22 towns of 05.12.1993 and the 76 runoffs of 19.12.1993 (both preliminary), the Potsdam OB of 1998, the three OB races of 2002 and Brandenburg an der Havel 2003; every other election from these reports is **winner-only** (name, Wahlvorschlag, date; `n_candidates` NA, no vote counts). A winner-only election decided in a runoff is filed under its first round, whose date the reports print but not its result: `mayoral_unharm` has a first-round row without counts or winner (222 cycles) and the runoff row with the winner. Limits: the hauptamtliche elections of 2004–2013 come from another source (next bullet); no source covers the by-elections of ehrenamtliche mayors between the general days; 36 elections seated nobody (`flag_decisive_round_missing`: two 1993 runoffs, 32 rejected single candidates and one runoff in 1998, Tschernitz 2024 where the elected person declined). The 26 mayors chosen by the Gemeindevertretung (5 after failed elections and 15 former Amtsdirektoren in 2003; 2 in 2008 and 4 in 2024, "Wahl durch Vertretung") are cycles in `mayoral_candidates` only, with `flag_elected_by_council` and no directly elected winner; they end the previous mayor's term in the panel. The 1993 results are the reports' preliminary figures (vorläufige Ergebnisse); the Amt für Statistik holds no final result (27.10.2026), and no column marks this. A 1993 runoff sheet printed as "Neuenhagen" is Neuenhagen b. Berlin, not the village of that name (an electorate/population check now guards the name lookup). Rows from these reports keep the AGS of their report year rather than the 2026 Gebietsstand. Losing candidates named in the reports (1993, OB races) are in the restricted twin only. Source: Landesbetrieb für Datenverarbeitung und Statistik Brandenburg (1993–2003) and Amt für Statistik Berlin-Brandenburg (2008–2024), Statistische Berichte B VII 3-6 (statistischebibliothek.de), used with the permission of the Amt für Statistik Berlin-Brandenburg (27.10.2026); the scanned issues 1993–2002 were transcribed by hand and checked against the reports' own totals.
- **Sachsen-Anhalt coverage (full historical series, 1994–2026)**: ST (state `15`) comes from the Statistisches Landesamt Sachsen-Anhalt file *"Bürgermeisterwahlen in Sachsen-Anhalt ab 1994"* (Stand 13.07.2026) — **4,611 round-results across 1994–2026**, with all candidates and their votes for both rounds, party (formal Wahlvorschlagsträger), and the elected mayor's gender, birth year and Amtsantrittsdatum. Because municipalities were repeatedly merged, `ags` is the AGS **at the time of the election** (2,057 distinct historical codes mapping onto today's 218 Gemeinden); the harmonised file maps these onto 2021 boundaries. Two caveats. **1994 is sparse by design** — the source states that for many 1994 elections only the winner is known, so vote counts and losing-candidate names are often missing (599 of 1,299 rows lack Gültige Stimmen); later years are essentially complete. **The elections that the older Dezernat-13 extract also covers (2019–2026) are taken from that extract**, which is the more accurate record for that window. A small number of rows carry documented source anomalies: three have Wähler > Wahlberechtigte (turnout > 1, flagged), two have candidate votes that do not sum to the Gültige Stimmen (Halle 2025, Leuna 1994), and the source files some elections of then-independent Gemeinden under the code of the town they later joined — on the same day as another Gemeinde (Heideloh/Großzöberitz 2001, Cosa/Diebzig, Dambeck/Dannefeld) or on another day (Liesten and Wieblitz-Eversdorf 2008 under Salzwedel). `00_st_hist_parse.py` re-codes every such election to the Gemeinde's own code of that year from the crosswalk (since October 2026 also the other-day cases, and the source's "AGS aktuell" is not trusted where it contradicts the crosswalk within one Kreis), so no AGS holds two Gemeinden on one day and `flag_shared_ags` is 0 everywhere. Sources: Statistisches Landesamt Sachsen-Anhalt (historical file + Dezernat-13 "Datensatzbeschreibung Bürgermeisterwahlen"), with the Landeswahlleiter portal (`wahlergebnisse.sachsen-anhalt.de`) retained as a fallback.

- **Sachsen-Anhalt candidate names are anonymised (data-licence restriction)**: the Statistisches Landesamt Sachsen-Anhalt supplies the ST source with full candidate names **for scientific use only**; only anonymised data may be redistributed, on the same basis as the Bavarian data. § 80 of the Kommunalwahlordnung für das Land Sachsen-Anhalt (KWO LSA) separately bars publishing Wahlvorschlag (candidate) data more than six months after the final result. Accordingly, in the published ST data **only the elected person is named** — exactly as in Bayern. Losing candidates keep their votes, vote shares, ranks and Wahlvorschlagsträger (party), but carry **no name, forename, title, gender, birth year or any name-derived attribute** (name origin, migration background, surname statistics). This is enforced in `01b_mayoral_candidates.R` and guarded by a `ST licence` check in `99_audit.R`. For the same reason the ST raw and intermediate files (`2026_0661_BM-Wahl_ab_1994.xlsx`, `bmbm.csv`, `st_*_parsed.csv`) are **not distributed with this repository**; obtain them directly from the Statistisches Landesamt Sachsen-Anhalt if you need to re-run the ST stage.

- **Brandenburg hauptamtliche 2004–2013 from the Landeswahlleiter's result reports (October 2026)**: the Geschäftsstelle des Landeswahlleiters sent scans of the result reports (*Ergebnismeldungen*) the Gemeinden had filed with it for their hauptamtliche Bürgermeister- and Oberbürgermeisterwahlen. They add 119 elections (164 rounds, 115 Gemeinden, 2004–2012) with full results: every Bewerber with Wahlvorschlagsträger and votes, and the counts of both rounds. GERDA now holds 85 hauptamtliche elections of 2004–2009 instead of 4 winner-only ones. 47 further elections on the scans (2010–2018) are already in the workbook and stay there; all their candidate votes agree exactly. One electorate differs: Liebenwalde 11.09.2011, 3,704 on the scan and 3,407 in the workbook, kept as published until the Landeswahlleiter confirms. Four 2008 elections the B VII 3-6 report held as winner-only now have full results. Rows keep the AGS of the election day. Source anomalies are kept or corrected only with evidence, and documented in `raw/brandenburg/lwl_ergebnismeldungen_2004_2013/README.md`. Belzig 28.09.2008 prints 5,779 Wahlberechtigte (a turnout of 97 %); GERDA publishes 9,779 from the Gemeindevertretung election of the same day and register. Schwarzheide 2008 is a runoff whose first round has no result (its field is unknown, so `n_candidates_hw` is NA). The first round of Schöneiche bei Berlin, 22.04.2012, missing from the scans, comes from the Landeswahlleiter's own result page as archived by the Internet Archive. In Eichwalde and Pritzwalk 2009 the two rounds print electorates that differ by about 10 %. Losing Bewerber are in the restricted twin only. Source: Landeswahlleiter Brandenburg, scans of 15.10.2026 (not distributed: they name losing candidates).
- **Brandenburg 2001–2017 from the Landeswahlleiter's archived result pages (October 2026)**: from 2008 to 2017 the Landeswahlleiter published each Gemeinde's latest Bürgermeisterwahl at `wahlen.brandenburg.de/bmwahlen/`; the Internet Archive kept 984 captures (414 Gemeinden). They replace **411 elections the B VII 3-6 reports held winner-only** — 2001–2003, 2008 and the ehrenamtliche of 25.05.2014 — with full results (counts and every Bewerber), and add 7 elections GERDA lacked (among them Forst 2015, Mühlberg/Elbe 2016, Guben 2016 and Falkenberg/Elster 2017, which the Landeswahlleiter's workbook omits). Brandenburg rounds without counts fall from 2,901 to 2,448 (2014: 281 to 13). Where the archive lacks one round of an election, the report's winner-only round is kept. The elected person always agrees with the report's. Lychen 2002, which B VII 3-6 files as one round on 05.05., gets its Hauptwahl of 21.04. Rosenau 2014 (single candidate rejected) and Pinnow 2008 are council choices after a failed vote. Five pages carry the site's wrong date and are left out. Raw folder `raw/brandenburg/lwl_bmwahlen_archive/` (README and manifest tracked, pages gitignored).
- **Brandenburg losing candidates not named (19 October 2026)**: as in Sachsen-Anhalt, Thüringen and Bayern, `mayoral_candidates` and `landrat_candidates` name only the elected person in Brandenburg; every other candidate keeps votes, shares, rank and Wahlvorschlagsträger, while the name and every field derived from it (gender, name origin, migration background) stay in the restricted twin. This now covers the Landeswahlleiter workbook 2010–2026 and its portal fallback (756 rows in 271 elections) and the Landrat portal pages (124 rows in 34 elections), as already the B VII 3-6 reports and the 2004–2013 result reports (user decisions 16 and 19 October 2026). The raw workbook, its parsed CSV, the portal scrape and the Landrat pages are no longer in the repository; tracked copies with only the elected persons named (`raw/brandenburg/bb_lwl_parsed_public.csv`, `landrat_elections/raw/brandenburg/bb_landrat_parsed_public.csv`) rebuild the public files exactly. A losing candidate's own-name Wahlvorschlag label ("EB Muster", "Einzelwahlvorschlag Max Muster"; invented) is the plain list type ("Einzelbewerber", "Einzelwahlvorschlag") in `candidate_party`, and in `mayoral_unharm`'s `winner_party` of a Hauptwahl whose leader lost the runoff: 228 mayoral and 6 Landrat labels, 7 unharm rounds (user decision 19.10.2026). A list whose name merely contains a surname is unchanged; the restricted twin keeps every label as printed.
- **Thüringen losing candidates are not named (October 2026)**: the Thüringer Landesamt für Statistik names every candidate of a recent election. After a period it withdraws the losers' names (§ 50 Abs. 2 ThürKWO) and names only the elected person. Since 2026-10-03 GERDA applies the Sachsen-Anhalt rule to Thüringen in `mayoral_candidates` and `landrat_candidates`: the elected person is named; losing candidates keep votes, shares, ranks and Wahlvorschlagsträger but carry no personal or name-derived data. Their names stay in the restricted twin only. Since 2026-10-12 this also applies to the **elected** persons named in the TLS Kommunalwahl reports 2000, 2004 and 2010: the TLS allows their results to be published only without any personal data on candidates and elected persons (see the Thüringen section). Guarded by the TH checks next to the ST licence check in `99_audit.R`. For the same reason the Thüringen raw and intermediate files that name candidates (the `LRInfo*`/`LSInfo*` workbooks, `th_ob_parsed.csv`, `th_bm_scraped.csv` and the Landrat `thueringen_parsed.rds`, and since October 2026 the TLS Kommunalwahl reports 2000, 2004 and 2010 with their transcriptions and `th_mayoral_tls_augmented.csv`) are **not distributed with this repository** since 2026-10-03; obtain them from the Thüringer Landesamt für Statistik (or re-run `00_th_scrape.py`) to re-run the TH stage.

- **Hessen coverage, names and person recovery**: HE (state `06`) uses the Hessisches Statistisches Landesamt historical workbook *Direktwahlen in Hessen seit 1993* for the full 1993--2026 series: 2,908 municipal round-results on 428 election-time AGS, mapping to 421 current municipalities. It contains every Wahlvorschlag, vote totals, the elected person's gender, and their official `Amtszeiten seit 1993` and `Wiederwahl` counters, but it redacts candidate names.
  - **Names 1993–2012** come from Hessami, Zohal (2018), "Accountability and Incentives of Appointed and Elected Public Officials", *Review of Economics and Statistics* 100(1): 51–64. Replication data: Harvard Dataverse, doi:[10.7910/DVN/FZWOMK](https://doi.org/10.7910/DVN/FZWOMK) (CC0). The data were obtained by the author from the same HSL division. **The elected person is named in the public files (all 1,506 elections of 1993–2012); losing candidates are named only in the restricted twin** (`../final_restricted/`), because they are private individuals.
  - The same source names the winners of 11 elections held in 2013.
  - 2017--2026 names come from the public B VII m snapshots and the 2026 hessenschau pages. 2013–2016 are otherwise unnamed.
  - In total 2,258 public HE candidate rows carry a name.
  - Every runoff result sits on the same person's first-round row. Where neither round names the candidates, the link was checked against named results from the municipalities' result pages and hessenschau.
  - The elected person's gender comes from HSL for every winner.
  - Two runoffs did not involve the first round's top two: Nauheim 2023 (a tie for second place, decided by lot) and Siegbach 1996 (the runner-up withdrew).
  - The official counters identify every decisive winner: `mayor_panel` contains 2,427 HE person-elections for 1,316 people; 2,415 rows are counter-linked and 12 recent rows are linked by a public name.
  - Three source-verified return spells are reconnected across an intervening mayor: Erhard Rohrbach in Maintal, Rüdiger Heß in Frankenberg (Eder), and Rudolf Dörr in Waldems. `consecutive_terms` resets when they return.
  - The 21 Landkreise are split to [`../../landrat_elections/final/`](../../landrat_elections/final/).
  - Single-candidate elections are Ja/Nein ballots, so the candidate's Ja votes may be below all valid votes. The two that failed (Driedorf 2016, Morschen 2022) have no winner; see `flag_decisive_round_missing`.

- **Thüringen party of named winners (fixed October 2026)**: Thüringen's database names elected persons as their Wahlvorschlag, "Nachname, Vorname (Träger)". Until October 2026 that whole string was stored as the party, so the Thüringen `winner_party` in `mayor_panel` was a person's name in 624 of 647 rows. Now the Träger is the party. Where the source gives no Träger, the party is `NA` (351 of 645 Thüringen panel rows). See [known issues §16](../../../docs/mayoral_elections_known_issues.md).

- **2026 Kommunalwahlen (Bayern + Hessen, newest cycle)**: The most recent local elections — **Bayern** (8 March 2026, Stichwahl 22 March) and **Hessen** (15 March 2026, Stichwahlen 29 March / 12 April, plus Jan/June by-elections) — are added from sources newer than the historical files above, each independently spot-verified against official municipality pages and local press (13/13 winners confirmed).
  - **Bayern 2026** (1,920 Gemeinden incl. 50 Oberbürgermeister; 62 Landrat split to the landrat dataset): from the Bayerisches Landesamt für Statistik *Mandatsträger* XLSX (official). Full candidate-level votes + turnout; the elected person's name/gender/birth year/first-Amtsantritt come from the Mandatsträger sheet and are attached to the winning Wahlvorschlag (the panel links re-elected incumbents by Amtsantritt — 1,075 of the 2026 winners were already in office). In the published file only the winner is named; losing candidates' names from the LfStat candidate lists are kept in the restricted twin.
  - **Hessen 2026** (28 Gemeinden incl. Hanau OB; 9 Stichwahlen) is a **hybrid**: the ~12 Gemeinden already in the May-2026 *B VII m* XLSX carry **full vote counts** (via the `hessen` source above); the remaining ~16 — the by-elections (Jan/June) and March results not yet in that report issue — are scraped from the hessenschau result pages as **percentage-only** (like RLP: candidate + party + vote share + turnout, but `valid_votes` / `candidate_votes` `NA`). The two sources are de-duplicated by `(ags, election_date)`. The winner is the decisive round's top result (Stichwahl if held); a built-in integrity check guarantees no runoff is missed (every recorded winner ≥ 50%), and the runoff date is read from the page text (not the URL). The %-only rows are superseded by full vote counts as later *B VII m* issues add them. All 13 sampled 2026 winners were independently re-verified (13/13).

---

## 1. `mayoral_unharm` -- Election-Level, Unharmonized

One row per (municipality, election date, election type, round). Reports the winner's party, votes, and vote share.

**Columns**: `ags`, `ags_name`, `state`, `state_name`, `election_year`, `election_date`, `election_type`, `round`, `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`, `winner_party`, `party_family`, `winner_votes`, `winner_voteshare`, `flag_superseded`, `flag_shared_ags`

**`party_family`**: the party family of `winner_party`, that is of the round's winner, or of the leader of a Hauptwahl that went to a runoff (see "Party families" under `mayor_panel`). It is added by `01b_mayoral_candidates.R`, which matches the label to the candidate of that round: an own-name label needs the person.

**`flag_superseded`** (logical): `TRUE` for a Bayern round that did **not** seat the mayor and is superseded by a later valid round — either (a) an annulled round (`Wahlart` "... ungültig"), or (b) a Hauptwahl that won **no absolute majority** (winner < 50%), was **not** resolved by a Stichwahl, and is followed by a repeat Hauptwahl (*Neuwahl*) for the same office within 250 days (a failed first attempt, or an early-postwar multi-ballot council election). The majority test deliberately spares Hauptwahlen that were duly won (≥50%) and merely preceded a later **by-election** — those seated a mayor and are not flagged. These rows are **kept, not dropped**, so no election is lost; filter `flag_superseded == FALSE` to restrict to decisive rounds. `FALSE` for every non-Bayern state — the Bayern "Wahlen seit 1945" source is the only one that records precursor rounds as separate dated rows (the `mayor_panel` already excludes them via its own `ungültig`-filter + Amtsantritt dedup). 87 rows flagged in `mayoral_unharm`.

**Election types**: `Buergermeisterwahl`, `Oberbuergermeisterwahl`, `Landratswahl`, `VG-Buergermeisterwahl` (RLP), `SG-Buergermeisterwahl` (NI)

**Round**: `"hauptwahl"` (first round) or `"stichwahl"` (runoff). All states have runoff data; for BW both the pre-2023 *Neuwahl* and the post-2023 *Stichwahl* second round are coded `"stichwahl"`.

---

## 2. `mayoral_harm` -- Election-Level, Harmonized to 2021 Boundaries

Maps all AGS codes to fixed 2021 municipal boundaries using population-weighted crosswalks (`data/crosswalks/final/ags_crosswalks.csv`). This allows consistent panel analysis across time despite municipal mergers and splits.

Same 17 core columns as `mayoral_unharm` (including `round` and `party_family`, which comes with `winner_party` from the dominant predecessor), plus 12 flag/metadata columns. `ags_name` is the 2021 municipality's name (from the crosswalk), not a predecessor's.

| Column | Description |
|---|---|
| `flag_unsuccessful_naive_merge` | Crosswalk year-shift fallback was needed |
| `flag_pre_1990` | Pre-1990 election using 1990 crosswalk as fallback |
| `flag_aggregated` | Row aggregated from multiple predecessor municipalities |
| `flag_turnout_above_1` | Raw turnout exceeded 1 before capping |
| `flag_voteshare_above_1` | Raw winner voteshare exceeded 1 before capping |
| `flag_pct_only` | Percentage-only data (all counts NA; RLP + some old Bayern) |
| `flag_superseded` | A predecessor's round was annulled or repeated (Bayern) |
| `flag_multi_winner` | More than one predecessor elected a mayor in this round (each predecessor is its own office): the winner columns describe the dominant (largest) predecessor only (945 rows) |
| `flag_partial_counts` | Only some predecessors report counts (the others are winner-only): the summed counts cover part of the 2021 municipality (58 rows) |
| `flag_runoff_other_predecessor` | A runoff row that is not the runoff of the dominant predecessor of the 2021 unit's preceding Hauptwahl row, e.g. a village runoff two weeks after the town decided its own election in the first round (Potsdam 1998, Groß Glienicke): it does not decide that cycle (113 rows) |
| `flag_shared_ags` | The source gives two Gemeinden the same AGS on one day and the parser could not recover their own codes (Sachsen-Anhalt; 0 rows since October 2026) |
| `n_predecessors` | Number of predecessor municipalities in aggregation group |

**Key design decisions**:

- **Grouping by `(ags_21, election_date, round)`** rather than `(ags_21, election_year)`, so runoff elections stay as separate rows and non-synchronized predecessor elections are not incorrectly merged.
- **Aggregation**: For N:1 mergers (964 rows), vote counts use `sum(x * pop_cw)` and the winner is taken from the largest predecessor by population. A mayoral election is not additive: when several predecessors elected their own mayors on one day (Brandenburg's 1998 ehrenamtliche elections fold up to 18 villages into one 2021 unit), the row describes the largest predecessor's election and `flag_multi_winner` is 1. For percentage-only data (RLP), turnout and vote share use weighted means. A 1:N split copies the result into every successor (the panel uses only the largest successor).
- **Pre-1990 fallback**: Bayern data from 1945--1989 (19,096 rows) uses the 1990 crosswalk since no earlier crosswalk exists.
- **Excluded types**: VG-Buergermeisterwahl, SG-Buergermeisterwahl, and Landratswahl are dropped because their pseudo-AGS codes cannot be mapped through the municipality crosswalk. Approximately 1,300 rows are excluded.

---

## 3. `mayoral_candidates` -- Candidate-Level (Wide Format)

One row per candidate per election cycle. Companion to `mayoral_unharm` -- same elections, but with all candidates and both Hauptwahl and Stichwahl results in columns.

**Election-level columns** (shared across candidates):

| Column | Description |
|---|---|
| `ags`, `ags_name`, `state`, `state_name` | Municipality identifiers |
| `election_year` | Year of the election cycle |
| `election_date` | Hauptwahl (first round) date |
| `election_date_sw` | Stichwahl (runoff) date, `NA` if no runoff |
| `election_type` | Type of election |
| `has_stichwahl` | `TRUE` if this election went to a runoff |
| `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes` | Hauptwahl vote metadata |
| `turnout` | Hauptwahl turnout as proportion (0--1) |
| `turnout_sw` | Stichwahl turnout (NA if no runoff) |

**Candidate-level columns**:

| Column | Description |
|---|---|
| `candidate_name` | Full name (available for NRW, RLP, NI, SL, SN partial, SH, BW) |
| `candidate_last_name` | Last name |
| `candidate_first_name` | First name |
| `candidate_gender` | `"m"` / `"w"` — raw (RLP, SL, BW from the official register) or predicted via `gender-guesser` (other named states) |
| `candidate_party` | Party or label (e.g., CSU, SPD, Parteilos, EB) |
| `candidate_party_family` | Party family of `candidate_party` (see "Party families" under `mayor_panel`): `cdu_csu`, `spd`, `gruene`, `linke_pds`, `fdp`, `afd`, `fw_wg`, `independent`, `other`, `joint_list`; `NA` without a label (Baden-Württemberg) |
| `candidate_votes_hw` | Hauptwahl vote count (NA for RLP) |
| `candidate_voteshare_hw` | Hauptwahl vote share (0--1) |
| `candidate_rank_hw` | Hauptwahl rank by votes (1 = most votes) |
| `n_candidates_hw` | Number of candidates in the Hauptwahl |
| `candidate_votes_sw` | Stichwahl vote count (NA if not in runoff) |
| `candidate_voteshare_sw` | Stichwahl vote share (NA if not in runoff) |
| `candidate_rank_sw` | Stichwahl rank (NA if not in runoff) |
| `n_candidates_sw` | Number of candidates in the Stichwahl |
| `is_winner` | Overall election winner (won HW outright OR won SW) |
| `flag_superseded` | `TRUE` for Bayern rounds that did not seat the mayor (annulled rounds, or a no-majority Hauptwahl repeated by a later *Neuwahl* within 250 days). Constant within an election. `FALSE` elsewhere. Filter `== FALSE` for decisive rounds only. 229 candidate rows flagged. |
| `flag_decisive_round_missing` | `TRUE` where the round that **decided** the cycle is not in the source, so the cycle has no winner and every candidate's `is_winner` is `NA`. Constant within an election. The one incomplete cycle it was built for (Hagenow 2015) was completed when the Landkreis supplied the missing Stichwahl. It also marks a round that the source records as seating nobody: two Hessen Hauptwahlen where the sole candidate failed the Ja/Nein vote (Driedorf 2016-03-06, Morschen 2022-03-06; a new election followed months later), one Thüringen runoff whose winner did not accept the election (16077047, Stichwahl 2026-03-15; "Wahl nicht angenommen; Neuwahlen erforderlich"), four Thüringen write-in elections of 2004-06-27 in which nobody won a majority (Andenhausen, Metebach, St. Bernhard, Kauern; "Wiederholungswahl erforderlich", TLS report 20421), 20 further Thüringen cycles of 1994–2010 (14 first rounds without a majority, runoff or seated winner, and 6 elections of the 2009–2010 window without runoffs whose winner never took office; see the Thüringen section), and 62 Brandenburg cycles from the B VII 3-6 reports (36 failed or declined elections and the 26 council choices of `flag_elected_by_council`): 89 cycles in all. Distinct from a source that is merely silent about the winner — there, the top-voted candidate is taken as the winner. |
| `flag_elected_by_council` | `TRUE` on the 27 Brandenburg cycles in which the Gemeindevertretung chose the mayor (after a direct election that seated nobody, or a former Amtsdirektor at the 2001–2003 Gemeindegebietsreform). The cycle has no directly elected winner and nobody named; the panel ends the previous mayor's term at it. Same column as in `landrat_candidates`. |
| `candidate_birth_year` | Birth year (NI; BW from the official register) |
| `candidate_profession` | Profession (NI only) |
| `office_type` | Office type (BY, SL, BW) |

**Candidate characteristics columns** (added by `04_candidate_characteristics.R`):

| Column | Description |
|---|---|
| `candidate_gender_source` | `"raw"` (from source data: RLP, SL) or `"predicted"` (from name classification) |
| `candidate_gender_method` | Classification method: `raw`, `full_de`, `full_global`, `hyphen_first_de`, `hyphen_first_global`, `accent_norm_global`, `manual` |
| `candidate_gender_prob` | Confidence score: 1.0 for raw, 0.99 for `full_de`/`manual`, 0.95 for `hyphen_first_de`, 0.90 for global matches |
| `candidate_name_origin` | Fine-grained name origin: `"german"`, `"turkish"`, `"arabic"`, `"eastern_european"`, `"southern_european"` |
| `candidate_name_origin_conf` | Confidence in origin classification (0.50--0.95) |
| `candidate_name_origin_method` | Detection method: `"combined"`, `"surname_match"`, `"firstname_match"`, `"surname_pattern"`, `"default"` |
| `candidate_migration_bg` | Binary migration background indicator: 0 = German-origin, 1 = likely non-German origin |
| `candidate_migration_bg_prob` | Probability of migration background (continuous, 0--1) |
| `candidate_local_surname` | Placeholder (NA) — awaiting telephone directory data |
| `candidate_surname_county_share` | Placeholder (NA) |
| `candidate_surname_n_counties` | Placeholder (NA) |
| `candidate_surname_overrep_ratio` | Placeholder (NA) |

**Field availability by state**:

| State | Names | Gender (raw) | Gender (predicted) | Migration bg | Birth Year | Profession | Candidate Votes |
|---|---|---|---|---|---|---|---|
| Baden-Württemberg | Yes | Yes (100%) | -- | Yes | Yes | -- | Winner only |
| Bayern | -- | -- | -- | -- | -- | -- | Yes |
| Brandenburg | Yes, the elected persons (since 19.10.2026 every losing candidate, from any source, in the restricted twin only) | -- | Yes | Yes | -- | -- | Yes, except the winner-only elections of the B VII 3-6 reports |
| Hessen | 1993–2012 elected persons (Hessami 2018) + 2013 winners + 2017–2026 public snapshots; losing candidates 1993–2012 in the restricted twin only | Yes (all winners) | Yes | Yes | -- | -- | Yes |
| Sachsen-Anhalt | Yes | -- | Yes | Yes | -- | -- | Yes |
| Thüringen | Elected persons only, from 2006 (OB) and 2020 (all Gemeinden); losing candidates and every name from the TLS reports 2000, 2004 and 2010 in the restricted twin | -- | Yes | Yes | -- | -- | Yes |
| Niedersachsen | Yes | -- | Yes (99.9%) | Yes | Yes | Yes | Yes |
| Nordrhein-Westfalen | Yes | -- | Yes (100%) | Yes | -- | -- | Yes |
| Rheinland-Pfalz | Yes | Yes | Yes (100%) | Yes | -- | -- | -- (% only) |
| Saarland | Yes | Yes | Yes (100%) | Yes | -- | -- | Yes |
| Sachsen | Partial | -- | Yes (98.6%) | Yes | -- | -- | Yes |
| Schleswig-Holstein | Yes | -- | Yes (100%) | Yes | -- | -- | Yes |

### Gender Classification Method

Gender is classified using the Python [`gender-guesser`](https://pypi.org/project/gender-guesser/) package (Jorg Michael's `nam_dict.txt`, ~70,000 names with country-specific gender codes). The lookup is pre-computed by `code/mayoral_elections/04a_build_gender_lookup.py` and merged in `04_candidate_characteristics.R`.

**Classification pipeline** (in priority order):
1. Raw gender from source data (RLP, SL) — takes precedence over predictions
2. Germany-specific match (`get_gender(name, country="germany")`) — confidence 0.99
3. Hyphenated name fallback (first component of hyphenated names) — confidence 0.95
4. Global match (no country restriction) — confidence 0.90
5. Accent-normalized fallback (strip diacritics) — confidence 0.90
6. Manual overrides (~90 entries for typos, rare names, foreign names) — confidence 0.99

**Coverage**: 48,418 candidate rows have gender assigned: 11,507 from raw data and 36,911 predicted. Bayern's historical source ("Wahlen seit 1945") has no candidate names. The printed *Bürgermeisterverzeichnisse* 1978–2014 and the candidate lists 2008–2026 (Statistische Berichte B VII 3-2 and 3-7) of the Bayerisches Landesamt für Statistik name the elected persons, but the Landesamt declined permission to redistribute their names, birth years and professions (16 October 2026: § 9 (3) of its terms bars electronic redistribution of its directories, and it sees no legal basis for publishing the personal data in other databases). GERDA therefore publishes no Bayern name, birth year, profession, name origin or migration background before 2026; the 2026 winners are named from the Landesamt's *Mandatsträger* file. The winners' gender, derived from the given name in the registers, is published (17,782 winner rows before 2026). The registers are otherwise used internally only, to correct the panel's person keys (see `mayor_panel` below).

**Validation**:
- Cross-validation against RLP raw data: Accuracy = 0.9979, F1 (female) = 0.9887, Precision = 0.9887, Recall = 0.9887. 4 mismatches are errors in the raw data (verified manually).
- Cross-validation against SL raw data: 171/171 (100%).
- Cross-election consistency: 1 person with inconsistent gender across elections (raw data entry error in RLP).

### Migration Background Classification

Migration background is classified using a rule-based name-origin approach applied to both first and last names. This is a probabilistic estimate based solely on name patterns; it should not be interpreted as verified migration status.

**Classification rules** (in priority order):
1. **Turkish**: ~87 common Turkish surnames + ~80 Turkish first names. Combined match = 0.95 confidence; surname-only = 0.80; firstname-only = 0.60.
2. **Arabic**: ~44 Arabic surname patterns (incl. `al-`, `el-`, `abd-` prefixes) + ~50 Arabic first names. Combined = 0.95; surname-only = 0.75; firstname-only = 0.55.
3. **Eastern European**: Surname-ending patterns (e.g., `-owski`, `-enko`, `-escu` = 0.85; `-ski`, `-ovic`, `-ek` = 0.60).
4. **Southern European**: Surname-ending patterns (e.g., `-opoulos`, `-elli`, `-etti` = 0.85).
5. **German** (default): German surname patterns (`Sch-`, `-mann`, `-berg`, etc.) = 0.90; unmatched = 0.50.

**Coverage**: all 48,221 candidate rows with a retained last name are classified. 633 (1.3%) are flagged as likely migration background. Most records receive the conservative `default` German-origin classification at confidence 0.50; use the method and confidence fields for sensitivity analysis rather than treating that default as verified ancestry.

**Important caveats**: This method cannot detect naturalized Germans with German-sounding names, nor can it distinguish between recent migration and families that have been in Germany for generations. Some German-origin names match non-German patterns (e.g., "Zein" is both a German and Arabic surname). The low base rate (~1.7%) means even small false-positive rates produce a non-trivial share of misclassifications. Use the confidence scores for sensitivity analysis.

---

## 4. `mayor_panel` / `mayor_panel_harm` -- Within-Mayor Panel

One row per person per election. Tracks individual mayors across multiple terms, enabling mayor fixed effects (person FE) estimation. The `_harm` version maps all AGS codes to 2021 municipal boundaries.

**Person identification**:

- **Bayern**: Groups consecutive terms via `Tag des ersten Amtsantritt` (same Amtsantritt date across elections = same person). The source workbook names nobody; the printed registers (used internally, their names are not published) correct the key where it holds two people (`first_amtsantritt_split`). Since October 2026 the reverse is corrected too (`first_amtsantritt_merge`, 13 keys, 19 rows): the 2026 Mandatsträger file dates a re-elected mayor's first Amtsantritt a few days off the workbook's (Rattiszell, Falter), which gave the incumbent a second id. A key joins the preceding key of its Gemeinde when the names agree (surname and first given name, birth years equal where both are known), or when the two dates lie within 120 days, the list is the same, the names do not contradict, and the later key's Amtsantritt lies more than a year before its own first election (a key that starts with its own election is a successor: an appointed 1945 mayor and the 1946 elected one stay two people). Rounds marked `flag_superseded` seated nobody and are not terms.
- **Hessen**: Uses the official person-level `Amtszeiten seit 1993` / `Wiederwahl` sequence, so all 2,427 decisive winner records receive IDs. Three documented non-consecutive return spells are anchored explicitly: Maintal, Frankenberg (Eder), and Waldems, whose annulled 1999 election was followed by the 1993 mayor's re-election in 2000. Only 12 recent elections not yet in the official sequence fall back to public-name matching. Hessen keeps two terms in one municipality-year where the official counters show two (since October 2026 every state keeps a second election cycle of a year): Alsbach-Hähnlein 2007 and Babenhausen 2002 (repeat elections), and Biebesheim am Rhein 1993, Bad Nauheim 1999 and Burghaun 2014 (by-elections). The earlier term of such a pair has no row in the annual panels.
- **Other named states**: Matches a normalized surname plus given-name initial within municipality. Accents, punctuation, academic titles, and source field-order changes are normalized first. A pinned conservative alias map reconnects 33 people (68 election rows) split by obvious transcription variants, added/dropped given names, or documented surname changes; ambiguous same-surname cases remain separate.
- **Renumbered and merged municipalities (October 2026)**: a Kreisreform that only renumbers a Gemeinde (Sachsen-Anhalt 2007, Sachsen 2008, the four MV cities 2011, Göttingen 2016) does not start a new municipality: its codes form one *lineage*, within which names are matched and terms counted (until October 2026 842 mayors had two ids). The name key holds the surname and the first initial, so a link also requires a compatible given name (equal, a prefix, or at most two edits apart): a father and son of one surname and initial stay two people. A mayor of a Gemeinde that merged into another and who wins there within 15 years keeps their `person_id` (`person_id_method = "merger_name_link"`, 112 people outside and 85 inside Brandenburg); their term counters restart in the new municipality and `was_predecessor_mayor` marks those terms. A merged-away person keeps the id of their earliest record, so no other id changes.
- **Two people under one name key (October 2026)**: a name key holds a surname and an initial, so one municipality can put two people under it. A record starts a new person (`person_id_method = "candidate_name_split"`) when its given name is incompatible with the person's previous record and gender or birth year (more than a year apart) contradict it — Hotopp Angelika (1994) / Andy (2001, born 1974), Hampel Ingo (1994) / Ingeburg (2001) — or when a reviewed split is pinned in `data/mayoral_elections/metadata/person_split_pins.csv` with its evidence (Espelkamp: Heinrich Vieker 1999-2020 and his son Henning from 2020). The alias that had joined Ellefeld's Heinrich Kerber to Jörg Heinrich Kerber was father and son and is gone. A review of all 37 people whose records carry incompatible given names found the other 33 to be one person each (short forms, added given names, abbreviations). Riethnordhausen's two Hampels are two people: the Statistisches Landesamt Sachsen-Anhalt confirmed this on 16 October 2026 after asking the Verbandsgemeinde, and the split is now pinned as well. It confirmed Eggenstedt's two Hotopps on 21 October 2026 after asking the Stadt Wanzleben-Börde (Angelika Hotopp mayor 1994-2001, Andy Hotopp from July 2001); that pin now cites the answer.
- **Stable ids (October 2026)**: ids are no longer positional. `data/mayoral_elections/metadata/person_id_registry.csv` records the id of every person-election (`ags`, `election_date`); it was seeded from the panel published in commit 176259e. Each run gives every person the registry id that covers most of their elections, issues new ids after the highest one ever issued in the state, and keeps ids that lost all their elections as retired rows (`active = FALSE`) so that they are never reissued. 97.7 % of the published person-elections kept their published id; the others belong to people the October 2026 links merged (one id of each pair retires) or split. The registry is rewritten by every run of `03_mayor_panel.R`; commit it with the panel.
- **One row per election cycle**: records of one municipality less than 60 days apart are one election cycle (Hauptwahl and Stichwahl filed separately); of two winners of one cycle the one with the higher vote share is kept. A later election in the same year — a by-election, a repeat after an annulled election — is its own row (116 municipality-years outside Hessen have two, e.g. Lützen 2010: Reichel in May, Könnecke in December); until October 2026 the panel kept one winner per municipality-year and dropped them. The same person elected twice in one year keeps one row, the later election.
- **Party families (October 2026)**: the Wahlvorschlag is free text (14,700 distinct labels). `party_family` groups the labels ([`code/shared/party_family.R`](../../../code/shared/party_family.R)); `winner_party` keeps them as printed. The same families are in every file: `candidate_party_family` in `mayoral_candidates` and `landrat_candidates`, `party_family` next to `winner_party` in `mayoral_unharm`, `mayoral_harm`, `landrat_unharm` and the four panels.
  - `cdu_csu`, `spd`, `gruene`, `linke_pds`, `fdp`, `afd`: the parties and their predecessors (PDS, Bündnis 90, GAL, FDP/DVP), and party organisations such as the Junge Union.
  - `fw_wg`: FREIE WÄHLER or any local Wählergruppe or Bürgerliste. This is not the council files' `freie_wahler`, which is the party alone: a mayoral label cannot tell the party from a local "Freie Wähler" list.
  - `independent`: an Einzelbewerber (EB, Einzelwahlvorschlag, parteilos, "kein Wahlvorschlag", Thüringen's "Weitere Personen", a candidate's own name as in Rheinland-Pfalz, Sachsen-Anhalt's blank label).
  - `other`: another party (BP, ÖDP, REP, NPD, KPD, GB/BHE, Die PARTEI, ...).
  - `joint_list`: a list of two or more of these ("CSU/Freie Wählergemeinschaft", "SPD/GRÜNE"). Supporters ("CSU und ihre Freunde"), an open list ("CDU - Offene Liste") and a party branch do not make a list joint.
  - `NA`: no label (Baden-Württemberg), or a source category that mixes families (Nordrhein-Westfalen's "Wählergr./Einzelbew.").
- **Party comparison**: since 19 October 2026 `party_switch` and `is_new_party_mayor` compare families, not labels. Two successive local lists, or two independents, are no switch, and a joint list continues each of its members (CSU → CSU/Freie Wähler is no switch; two minor parties compare as parties). Compared as labels, `party_switch` was 1 in 48.8 % of the comparable rows, mostly because successive or respelled local lists counted as switches; now 22.8 %. Compare `winner_party` to study the lists themselves.
- **Source-limited records**: An unnamed winner is not assigned a name-based pseudo-ID. Thüringen's winners of 1994--2019 remain outside the person panel because the database publishes names only for recent cycles and the names in the TLS reports 2000, 2004 and 2010 may not be published (see the Thüringen section); Bayern remains fully traceable through Amtsantritt despite lacking names.

**Columns**:

| Column | Description |
|---|---|
| `person_id` | Unique pseudonymous mayor identifier (for example `p_09_00001`) |
| `person_id_method` | Identification provenance: `first_amtsantritt`, `first_amtsantritt_split`, `first_amtsantritt_merge`, `hessen_official_term_sequence`, `candidate_name`, `candidate_name_variant_link`, `candidate_name_split`, or `merger_name_link` (linked across a merger, see above). `candidate_name_variant_link` marks one person whose name is spelled differently between sources or report years (a misprinted or transliterated surname such as Horts/Horst or Clauß/Claus, a married or dropped second surname, given names added or dropped, a 1993 record giving the surname only); each such link is pinned with its evidence in `data/mayoral_elections/metadata/person_name_variant_links.csv` or in the alias table of `03_mayor_panel.R` (69 people). `first_amtsantritt_split` (Bayern) marks a mayor split off a first-Amtsantritt key that the source shares with another person (e.g. Augsburg OB: the OB elected in 2002 under his predecessor's 1990 date); such rows have no source start date, so `term_start_date` is their first election. `first_amtsantritt_merge` (Bayern) marks the rows of a key merged into the preceding key of the same person; they take that key's start date. `candidate_name_split` marks a person split off a name key (see above). |
| `source_person_term_number` | Hessen's official number of terms since 1993; `NA` elsewhere |
| `source_person_reelections` | Hessen's official number of re-elections; `NA` elsewhere |
| `ags` | 8-digit municipality code (original boundaries) |
| `ags_21` | 8-digit municipality code mapped to 2021 boundaries (`_harm` only) |
| `state` | 2-digit state code |
| `election_year` | Year of the election |
| `election_date` | Date of the decisive round (Stichwahl date if applicable) |
| `term_number` | Sequential term count within (person, municipality), starting at 1; a renumbered municipality counts as the same, a merged one does not |
| `consecutive_terms` | Count within the current uninterrupted spell; resets after an intervening mayor or a gap over 10 years |
| `winner_party` | Party of the winning candidate |
| `party_family` | Party family of `winner_party` (see "Party families" above) |
| `winner_voteshare` | Vote share in the decisive round (0--1) |
| `winning_margin` | Vote share difference between winner and runner-up (0--1) |
| `n_candidates` | Number of candidates in the election |
| `is_incumbent` | 1 if `term_number >= 2`, else 0 |
| `was_predecessor_mayor` | 1 if this person was mayor of a Gemeinde that merged into this municipality (and is linked to it by name), else 0. Not counted in `term_number` / `is_incumbent` |
| `next_runs_again` | 1 if this person wins the next election in this municipality, 0 if a different person wins, NA if no subsequent election is observed or an election whose winner is not in the panel lies in a year in between (as for `party_switch` and `margin_change`) |
| `party_switch` | 1 if the party family differs from that of the previous election's winner in this municipality, 0 if the two share a family (a joint list shares each of its members' families), NA for the first election, an unknown family, or when an election whose winner is not in the panel lies in a year in between |
| `is_new_party_mayor` | 1 if none of the winner's party families has won in this municipality before (so 1 at its first panel election), else 0 |
| `margin_change` | Change in `winning_margin` from the previous election in this municipality (NA as for `party_switch`) |
| `tenure_start` | Year of the mayor's first election in this municipality |
| `years_in_office` | `election_year - tenure_start` |
| `term_start_date` | Date of first taking office (Bayern: Amtsantritt; others: first election date) |
| `n_terms` | Total number of terms observed for this person |
| `total_tenure_years` | Year span from first to last election |
| `has_margin_variation` | Whether winning margin varies across this person's terms (useful for FE feasibility) |
| `candidate_gender` | Mayor's gender: `"m"` / `"w"`. From raw data or predicted via `gender-guesser` (Bayern before 2026: from the given name in the LfStat registers, which itself is not published). |
| `candidate_gender_source` | `"raw"` or `"predicted"`. |
| `candidate_gender_prob` | Confidence score (0--1). See `mayoral_candidates` for details. |
| `candidate_gender_method` | Classification method. See `mayoral_candidates` for details. |
| `candidate_migration_bg` | Binary migration background (0/1). NA for Bayern before 2026. |
| `candidate_migration_bg_prob` | Probability of migration background (0--1). |
| `candidate_name_origin` | Fine-grained name origin category. |
| `candidate_name_origin_conf` | Confidence in origin classification (0.50--0.95). |
| `candidate_name_origin_method` | Detection method for origin classification. |

**Coverage**: 23,333 unique mayors (unharm) / 23,322 (harm), spanning 48,382 / 48,350 person-elections. Candidate gender is populated on 35,676 / 35,676 person-elections; for Bayern (19,688) it comes from the given names in the printed *Bürgermeisterverzeichnisse* and candidate lists, which are not published themselves, and from the 2026 *Mandatsträger* file. Name origin and migration background are NA for Bayern before 2026. All rows carry an explicit `person_id_method`.

**Bayern person keys.** Bayern mayors are linked across terms by the workbook's first-Amtsantritt date. Where that date is impossible for the round (more than 200 days after it) or the register names (read from the restricted register data; only the pseudonymous ids are published) show two different people under one date, `03_mayor_panel.R` splits the later or earlier mayor off (30 new person_ids; existing ids unchanged). Where the names, or without names the same list held continuously within one term, show it is the same person, the rounds stay together and only the wrong start date is dropped.

---

## 5. `mayor_panel_annual` / `mayor_panel_annual_harm` -- Annual Mayor Panel

One row per mayor per year. Forward-fills election-level data across the mayor's term, creating a balanced annual panel. The `_harm` version maps all AGS codes to 2021 boundaries.

Each mayor-election is expanded from `election_year` to the year before the next election in that municipality, whether or not that election's winner is named. A term also ends (since October 2026):

- when the municipality ceases to exist (merged away; a pure renumbering is followed to the new code, so the mayor stays in office) -- not where the election itself is filed under a code that had already ended (Brandenburg 1998 rows under 1997 codes, Dessau OB 2007): that term runs to the next election or the legal maximum;
- when the Gemeindevertretung chooses a new mayor (Brandenburg, `flag_elected_by_council`);
- at the longest term the law allows (with one year of slack), where the source shows no later election although one must have been held: e.g. 48 Hessen Gemeinden whose 2023-2025 election is missing from the HSL table, NRW elections outside the general dates, Sachsen-Anhalt 1994 terms without a 2001 election. Bayern before 1995 is not capped;
- in Thüringen, six years after a pre-2020 election when no later election of the Gemeinde is in any source.

Otherwise an open term runs to 2026. A year with no row means the mayor is unknown. Until October 2026 terms ran on to the next *named* winner or to 2026; the change removed 46,478 annual rows (net) and all but 6 of the 2,556 terms longer than 12 years, almost all in Sachsen-Anhalt and Sachsen.

**Columns**:

| Column | Description |
|---|---|
| `ags` | 8-digit municipality code (original boundaries) |
| `ags_21` | 8-digit municipality code mapped to 2021 boundaries (`_harm` only) |
| `year` | Calendar year |
| `person_id` | Unique mayor identifier |
| `person_id_method` | Person-identification provenance, carried from the election panel |
| `source_person_term_number` | Hessen source term counter, carried from the election panel |
| `source_person_reelections` | Hessen source re-election counter, carried from the election panel |
| `state` | 2-digit state code |
| `election_year` | Year of the election that started this term |
| `election_date` | Date of the decisive round |
| `term_number` | Term count within (person, municipality) |
| `winner_party` | Party of the mayor (constant within term) |
| `party_family` | Party family of `winner_party` (constant within term) |
| `winner_voteshare` | Vote share in the decisive round (constant within term) |
| `winning_margin` | Winner-runner-up margin (constant within term) |
| `n_candidates` | Number of candidates (constant within term) |
| `is_incumbent` | 1 if `term_number >= 2` |
| `was_predecessor_mayor` | 1 if this person was mayor of a Gemeinde merged into this municipality |
| `next_runs_again` | Whether this person wins the next election |
| `years_since_election` | `year - election_year` |
| `years_to_next_election` | Years until the next election in this municipality (NA if unknown) |
| `electoral_cycle_pos` | Position in the electoral cycle, 0 (election year) to <1 (year before next election) |
| `tenure_start` | Year of first election |
| `term_start_date` | Date of first taking office |
| `candidate_gender` | Mayor's gender (constant within term). |
| `candidate_gender_source` | `"raw"` or `"predicted"` |
| `candidate_gender_prob` | Confidence score (0--1) |
| `candidate_gender_method` | Classification method |
| `candidate_migration_bg` | Binary migration background (0/1, constant within term) |
| `candidate_migration_bg_prob` | Probability of migration background (0--1) |
| `candidate_name_origin` | Fine-grained name origin category |
| `candidate_name_origin_conf` | Confidence in origin classification |
| `candidate_name_origin_method` | Detection method for origin classification |

**Coverage**: 248,974 person-years (unharm) / 248,744 (harm), years 1945--2026. In the `_harm` versions several predecessor mayors can hold office in one 2021 unit in the same year (before a merger): the term counters follow the original municipality, so filter or aggregate by `ags` and `year` as the analysis needs. Candidate characteristics are forward-filled across the term (constant within each mayor-term).

---

## Stichwahl (Runoff) Coverage

| State | HW Elections | SW Elections | Detection Method |
|---|---|---|---|
| Bayern | 32,460 | 3,322 | `Wahlart` column in raw data (incl. 8/22 March 2026 Kommunalwahl) |
| Sachsen | 1,876 | 300 | `Status` VE/EE + date matching |
| Nordrhein-Westfalen | 1,522 | 332 | 60-day cycle detection |
| Baden-Württemberg | 1,240 | 161 | `Wahlart` H/N/S in the StaLA report (Neuwahl and Stichwahl) + Komm.ONE rounds |
| Niedersachsen | 1,003 | 240 | Separate Stichwahl PDFs (2006/2013) + round detection |
| Rheinland-Pfalz | 861 | 222 | `Stichwahltag` column + separate HW/SW results; the 2026 edition marks runoffs with "x" |
| Hessen | 2,429 | 479 | Historical workbook `Stichwahl` marker + paired HW/SW rows; 2026 hessenschau supplement is %-only |
| Brandenburg | 3,123 | 489 | `Wahlart` column in the Landeswahlleiter workbook; runoff dates and markers in the B VII 3-6 reports; Hauptwahl/Stichwahl on each result report 2004–2013 and on the archived result pages |
| Sachsen-Anhalt | 4,141 | 473 | Hauptwahl + Stichwahl columns on each source row |
| Saarland | 55 | 14 | `Wahlart...3` column in raw data |
| Schleswig-Holstein | 44 | 13 | Scraped with round info |
| Thüringen | 4,467 | 382 | Hauptwahl + optional "Stichwahl am" block per result page (database); runoff dates in the TLS reports 2000/2004; no runoffs in 2010 |
| Mecklenburg-Vorpommern | 33 | 19 | Separate Hauptwahl/Stichwahl sections in the LAIV PDFs; Wismar 2026 from the city's official result |

---

## Coverage by State

| State | Code | Year Range | Unharm Rows | Panel Persons | Panel Elections | Source |
|---|---|---|---|---|---|---|
| Baden-Württemberg | 08 | 2003--2025 (hybrid) | 1,401 | 1,167 | 1,241 | Komm.ONE votemanager (candidate-level) + Stat. Landesamt B VII 3-j/25 (winner-only) |
| Bayern | 09 | 1945--2026 | 35,782 | 12,702 | 32,336 | Excel (Bayerisches Landesamt historical series + 2026 Mandatsträger) |
| Brandenburg | 12 | 1993--2026 | 3,612 | 2,099 | 3,165 | Excel (Landeswahlleiter, on request) + wahlen.brandenburg.de + Statistische Berichte B VII 3-6 (1993-2024) + Gemeinden's result reports 2004-2013 (Landeswahlleiter archive, scans) + the Landeswahlleiter's archived result pages 2001-2017 (Internet Archive) |
| Hessen | 06 | 1993--2026 | 2,908 | 1,316 | 2,427 | HSL historical workbook + B VII m public-name snapshots + hessenschau 2026 |
| Sachsen | 14 | 2001--2024 | 2,176 | 1,112 | 1,876 | Excel (Bürgermeisteratlas) |
| Sachsen-Anhalt | 15 | 1994--2026 | 4,614 | 2,589 | 4,136 | Excel (Stat. Landesamt "BM-Wahl ab 1994") + Dezernat-13 extract |
| Nordrhein-Westfalen | 05 | 2009--2025 | 1,854 | 934 | 1,522 | Excel (IT.NRW) |
| Rheinland-Pfalz | 07 | 1994--2026 | 1,083 | 132 | 205 | Excel (Stat. Landesamt, editions of 19.12.2025 and 12.10.2026) |
| Niedersachsen | 03 | 2006--2025 | 1,243 | 533 | 698 | PDF extraction |
| Saarland | 10 | 2019--2025 | 69 | 55 | 55 | Excel |
| Schleswig-Holstein | 01 | 2023--2026 | 57 | 44 | 44 | Web scraping (wahlen-sh.de) |
| Mecklenburg-Vorpommern | 13 | 2001--2026 | 52 | 23 | 33 | PDF extraction (LAIV-MV Direktwahlen + Kreiswahlleitung Ludwigslust-Parchim) |
| Thüringen | 16 | 1994--2026 | 4,849 | 627 | 644 | DB scrape (wahlen.thueringen.de, all Gemeinden) + Info/Daten files (kreisfreie-Stadt OB) + TLS Kommunalwahl reports 2000, 2004, 2010 |

**Note**: Panel person/election counts are from the unharmonized `mayor_panel`. The panel only includes Buergermeisterwahl and Oberbuergermeisterwahl (not Landratswahl, VG/SG-Buergermeisterwahl). Stichwahl-only records are collapsed into the parent election cycle.

---

## Known Issues and Caveats

**Rheinland-Pfalz has no absolute vote counts.** All count columns are NA; only turnout and vote shares (as percentages) are available. In `mayoral_harm`, these use weighted-mean aggregation instead of weighted-sum.

**Rheinland-Pfalz: two editions of the source (October 2026).** The Statistisches Landesamt's workbook of 19.12.2025 (every candidate, elections to 29.06.2025) is the base. Its edition of 12.10.2026 names only the elected person but runs to 13.09.2026; it adds 35 elections (34 after 29.06.2025, among them Landrat Westerwaldkreis and Eifelkreis Bitburg-Prüm 2026 and Oberbürgermeister Koblenz and Ludwigshafen 2025, Zweibrücken, Pirmasens, Speyer and Worms 2026, plus Vordereifel 2024). These rows are winner-only: the winner's share, no other candidate, `n_candidates` NA; in a runoff cycle the Hauptwahl row carries only the turnout. Each edition has errors the other does not, so neither replaces the other: 17 cells of the 2025 edition are corrected in `data/mayoral_elections/metadata/rlp_source_corrections.csv`, each with its evidence (official notices, the Landeswahlleiter's list), and eight Verbandsgemeinde elections held with the Landtagswahl of 27.03.2011, whose dates the source types as text, are no longer dated 07.11.2010. See known issues §18.

**Bayern names only its 2026 winners.** The source data provides party and votes per candidate slot but not names. In `mayoral_candidates`, Bayern rows before 2026 have `candidate_name = NA`; the names in the Landesamt's printed registers and candidate lists may not be redistributed (see above).

**Niedersachsen data is PDF-extracted.** Parsing 3 different PDF layouts introduces minor issues: 91 elections where the sum of candidate votes falls below 50% of valid_votes (top candidates listed, not all), and 2 elections with a vote count discrepancy of 8 votes (source data rounding).

**Candidate-name repairs (October 2026).** Three parser defects in [`01b_mayoral_candidates.R`](../../../code/mayoral_elections/01b_mayoral_candidates.R), reported by a register-linkage project, are fixed. `mayoral_candidates` goes from 113,562 to 113,535 rows (Niedersachsen 2,617 → 2,590); no other state changes. Merged with the Hessen-names release (Hessami 2018), which folds 190 Hessen runoff-only rows into their first-round rows, the file has 113,345 rows.
- *Niedersachsen: phantom candidates named with a number (26 rows, 20 elections, 2006–2021).* These were not Ja/Nein ballots. A candidate with fewer than 100 votes has a votes line such as `75   4.4`, which the parser read as a new candidate line. The vote share became a "candidate" with a number as surname and the real candidate's party and birth year; the real candidate lost their votes, party and birth year. The phantom rows are gone and the 26 real candidates carry their votes. `n_candidates_hw` now gives the true field size, and so does `mayor_panel$n_candidates` (18 elections; the other two are Samtgemeinde elections, which are not in the panel). 18 of the 20 elections now sum exactly to `valid_votes`. The other two, 03155002 (2021-09-12) and 03358023 (2019-05-26), lose their last candidate's votes at a page break in the source PDF; that candidate keeps a name and NA votes. Winners, winner votes, margins and `person_id`s are unchanged.
- *Niedersachsen: candidates with no first name (5 rows).* All five first names are in the source. In the 2006 Hauptwahl file a long name wraps after the comma, with the given name on the line below the number (03154019, 03255023, 03404000, all 2006-09-10). In the 2006 runoff file (03241010) three spaces after the comma pushed the given name into `candidate_profession`. That also split the candidate into a Hauptwahl row and a runoff-only row, which are now one row. In 03157009 (2021-09-12) the table cell is clipped in the PDF itself, so the given name comes from the page header that names the elected person. Two winners now get a predicted gender in `mayor_panel`.
- *Bayern 2026: nobility names split after the title (5 rows).* The Mandatsträger file writes "Surname Firstname" with no comma, and the parser took the first word as the surname. A surname with a title and particle ("Graf von X") therefore came out as surname "Graf" and given name "von X Firstname". The surname now runs to the word after the last particle (09171133, 09274135, 09474135, 09475146, 09478116; all 2026-03-08). Gender comes from the source and is unchanged.
- `98_full_audit.R` section G7 now fails if any candidate name is a number, ends in a comma, or (Bayern) has a particle in the given name.
- Checked and correct: the 2026 winners in 09188144 and 09777165 have the same name and birth year but are different people, and they have different `person_id`s.
- *Second pass, same month: the other name splitters (213 mayoral and 274 Landrat rows).* Only the name parts and the characteristics derived from them change; row counts stay the same.
  - *Rheinland-Pfalz, Oberbürgermeister and Landrat.* These two source sheets have no Vorname column and write "Nachname, Vorname" in one cell. The whole cell went into `candidate_last_name` and `candidate_first_name` was NA (201 OB rows; 274 rows in `landrat_candidates`). The cell is now split at the comma, with academic titles removed from the two parts. 135 OB candidates get a predicted gender for the first time.
  - *Baden-Württemberg, Komm.ONE.* The portal writes "Vorname Nachname" and the parser took the last word as the surname. A surname now starts at its particle, together with a nobility title directly before it (08115028, 08216008, 08325011, 08426117). For elected candidates the StaLA register decides the surname wherever it differs from that rule. This covers a two-word surname without a particle (08119075), "Vorname A von B" (08327056) and one cell written "Nachname Vorname" (08127089).
  - *Mecklenburg-Vorpommern 2001 (13002000).* "Name, Vorname, zu" is now surname "zu Name", given name "Vorname".
  - *Bayern 2026.* The two winners with a two-word surname and no particle (09461000, 09778180) are pinned in `data/mayoral_elections/metadata/candidate_name_splits_verified.csv`, each checked against a public source. The run stops if a pinned name no longer appears in the source. A by-election entry written "Nachname, Vorname" (09171114, 2026-05-17) is split at the comma.
  - `mayor_panel`: `person_id`s unchanged; 4 RLP rows change their name-origin confidence.
  - `98_full_audit.R` section G7 now checks every state, and `landrat_candidates`, for a particle in the given name and a comma in the surname.
- Fixed later the same month: the 272 Thüringen given names that held the Wahlvorschlag in brackets ("Vorname (Wählergruppe X)"); see known issues §16.
- Still open: two Baden-Württemberg rows are the write-in total "Vom Wähler vorgeschlagene Person(en)", not a person. A losing Komm.ONE candidate with a two-word surname and no particle cannot be recognised, because the register names only the winner.

**Thüringen has two sources and largely redacted candidate names.** The Bürgermeisterwahlen of all ~596 Gemeinden (1994–2026, hauptamtlich + ehrenamtlich) are scraped from the Thüringer Landesamt für Statistik database (`wahlen.thueringen.de`) by [`code/mayoral_elections/00_th_scrape.py`](../../../code/mayoral_elections/00_th_scrape.py); the 6 kreisfreie-Stadt Oberbürgermeister elections (Erfurt/Gera/Jena/Suhl/Weimar/Eisenach) come from the raw Info/Daten files via [`00_th_mayoral_parse.py`](../../../code/mayoral_elections/00_th_mayoral_parse.py). Per § 50 Abs. 2 ThürKWO the database **redacts candidate personal data**, so for the Gemeinde Bürgermeisterwahlen `candidate_party` holds the *Wahlvorschlag* (party, or an Einzelbewerber/group label such as "Einzelbewerber", "Weitere Personen") and `candidate_name` is populated only where the Wahlvorschlag names a person. Such a Wahlvorschlag reads "Nachname, Vorname (Träger)". The name goes to the name columns and the Träger to `candidate_party`, which is `NA` when the source gives no Träger. Joint lists such as "SPD, CDU, UWS" are not names ([`th_wahlvorschlag.R`](../../../code/mayoral_elections/th_wahlvorschlag.R), known issues §16). Consequently the `mayor_panel` tracks only the named subset (1,542 persons, 1999--2004 and from 2020); the bulk of Gemeinde winners appear in the election-level datasets but not the person panel. Election dates are exact (Hauptwahl from the result page, Stichwahl from the "Stichwahl am …" block). The OB elections 1994/2000 (from the Daten HTML) are party-level only (no names). A stratified 57-election sample was independently re-verified against the live source (57/57 match).

**Thüringen: who was elected without a majority (October 2026).** A Thüringen mayor needs an absolute majority; without one, a list election goes to a runoff and a write-in election is repeated. The exception is the period without runoffs: on 6 June 2010 "genügte eine einfache Mehrheit" (TLS report 29926), and the database has no runoff for any election between April 2009 and August 2010. [`00_th_tls_reports_parse.py`](../../../code/mayoral_elections/00_th_tls_reports_parse.py) (`decide_majority_rule()`) applies this:
- **Outside 2009–2010:** 14 first rounds of 1994–2008 had no majority and no runoff. A repeat election followed in 13 of them, and Kauern 2002 had no elected mayor in 2004 either. They now seat nobody (`flag_decisive_round_missing`); until October 2026 GERDA seated their leader, in 6 of them the pooled write-ins.
- **Inside 2009–2010:** the candidate with the most votes was elected, which the report's list of mayors in office confirms for 2009 (e.g. Schmölln, 30.8 %). The winner is the top **named** candidate: the database pools write-ins as "Weitere Personen", and that pool can hold more votes than any one person (Oettern, Paska and Löberschütz 2010, Kauern 2009, and four of the 2010 elections added from the report). For 6 June 2010 the report names the elected person of every election, and the rule seats exactly that person in all 684 accepted ones.
- **Never accepted:** six window elections seat nobody. Five are marked "Neuwahl erforderlich" (Lehesten, Renthendorf, Mittelpöllnitz, and the added Neidhartshausen and Chursdorf, 2010), and Bruchstedt held a Neuwahl in August 2010 after its February election.
- **Kept as published:** 7 cycles of 1994–1999 without a runoff seat the pooled write-ins, which hold over 50 %. A write-in person was elected, but the database does not say who, so the winner row reads "Weitere Personen".

**Thüringen 1999--2010 from the TLS Kommunalwahl reports (October 2026).** The Thüringer Landesamt für Statistik sent its reports *Kommunalwahlen 2004, Endgültige Ergebnisse, Wahlen der Bürgermeister* (Bestell-Nr. 20 421) and *Kommunalwahlen 2000, Vorläufige Ergebnisse, Stichwahlen* (29 924); *Kommunalwahlen 2010, Endgültige Ergebnisse, Wahlen der Bürgermeister* (29 926) is in the public Statistische Bibliothek. [`00_th_tls_reports_parse.py`](../../../code/mayoral_elections/00_th_tls_reports_parse.py) reads them:
- **2004:** the report names every candidate of all 820 Bürgermeisterwahlen of 27.06.2004 (runoffs 11.07.2004). The database lists only today's Gemeinden and holds 439 of them; there both sources agree exactly on every count, and the report adds the names. The other 381 Gemeinden (most merged since) are added from the report, in the database's convention: in an election *ohne Bindung an Wahlvorschläge* the write-ins are pooled as "Weitere Personen". The report's figures reconcile exactly with its own Kreis and state totals.
- **Mayors elected on other dates (1999--2004):** the report's list of all mayors in office identifies the winners of 113 elections the database holds and adds 63 winner-only elections (party where printed, votes and share of the decisive round; no counts, `n_candidates` `NA`). For a runoff the list prints the first round's date; the TLS supplied the runoff dates of the three runoffs only this list records (Andenhausen 10.10.2004, Berga/Elster 26.01.2003, Teichwolframsdorf 27.06.1999), which are filed as cycles whose first round has no result.
- **2010:** the report names every candidate of all 689 Bürgermeisterwahlen of 06.06.2010 (no runoffs; see the majority rule above). The database holds 389; there both agree exactly on every count, and the report adds the names. The other 300 Gemeinden (merged since) are added from the report in the database's convention. The report's figures reproduce its own Kreis and state totals, including each party's votes and Mandate.
- **Mayors elected on other dates (2005--2010):** the same report's list of all mayors in office at the end of 2010 identifies the winners of 168 elections the database holds and adds 95 winner-only elections in no other source (Gemeinden merged since). The list does not mark runoffs; an added election is filed as a runoff (8) only where its date is a statewide runoff day in the database, and otherwise as a first round, so a few of the 87 may be runoff winners. The list also shows that Bad Berka's runoff of 21.05.2006, a 1,459 to 1,459 tie decided by lot, was won by the Einzelbewerber; until October 2026 GERDA seated the other candidate.
- **2000:** the runoffs of 28.05.2000 (25 Bürgermeister, 3 Oberbürgermeister, 3 Landräte) name both candidates. Five Bürgermeister runoffs and the three Landrat runoffs are added as runoff-only cycles (their first round is in no source we hold); these use the report's preliminary figures.
- **No name from these reports is published, the elected persons' included.** The TLS consented on 12.10.2026 to publishing the reports' results with a source citation, "unter Löschung sämtlicher personenbezogenen Daten zu Bewerbern und gewählten Kandidaten" (§ 50 Abs. 2 ThürKWO). The public files keep votes, shares, ranks and Wahlvorschlagsträger; names, titles and name-derived fields are in the restricted twin only. The winners named only in these reports (1994--2010) are therefore not in the person panel, which holds Thüringen's 644 named terms from 2006 on.
- Four 2004 write-in elections seated nobody (`flag_decisive_round_missing`). The TLS corrected two misprinted dates of the list (Königsee 07.09.2003, not 27.09.; Leutenberg 15.06.2003, not 17.06., which had made Leutenberg's election appear twice) and confirmed the spelling of Tautenburg's 2004 winner.

**Mecklenburg-Vorpommern is PDF-extracted** from 69 LAIV-MV "Direktwahlen" result PDFs (`data/mayoral_elections/raw/mecklenburg_vorpommern/`) by [`code/mayoral_elections/00_mv_parse.py`](../../../code/mayoral_elections/00_mv_parse.py), which writes a candidate-level intermediate (`mv_parsed.csv`) read by stages 01/01b. That central source covers only the kreisfreie / große Städte (Oberbürgermeister; Schwerin, Rostock, Stralsund, Greifswald, Neubrandenburg, Wismar) and the Landräte — the Landeswahlleitung does not publish the hauptamtliche Bürgermeister of the amtsfreien Gemeinden and refers to the **Kreiswahlleitungen** for them. Those are being collected Landkreis by Landkreis; the first delivery, from **Ludwigslust-Parchim** (2026-08-07, completed 2026-08-10), covers its five amtsfreie Gemeinden — Boizenburg/Elbe, Hagenow, Lübtheen, Ludwigslust and Parchim — with 9 rounds over 2014--2023, parsed by [`00_mv_lup_parse.py`](../../../code/mayoral_elections/00_mv_lup_parse.py) into `mv_lup_parsed.csv` (see `lup_amtsfrei/_source_manifest.csv` for per-document provenance). One limitation of that delivery is carried into the data rather than papered over: the **Parchim 2022** Bekanntmachung states no Wahlvorschlagsträger at all, so both candidates have an empty `candidate_party`. The **Hagenow 2015** Stichwahl was missing from the original zip and was supplied on request; both rounds are now present, and Thomas Möller (Die Linke) is linked across his 2015 and 2022 terms. The remaining five Landkreise are still outstanding. AGS assignment is year-aware: the 2011 Kreisgebietsreform merged four kreisfreie Städte into Landkreise, so e.g. the 2001/2008 Greifswald OB elections carry the then-kreisfreie code `13001000` (harmonized to the 2021 municipality `13075039`), while 2015+ elections carry `13075039` directly. One source quirk: in the **Greifswald 2015 Stichwahl** the PDF's reported Wähler (16,342) is internally inconsistent with gültige + ungültige Stimmen (16,432) by 90 votes — reproduced as printed. **Wismar 2026** (12.04. and runoff 26.04.2026), missing from the LAIV files, is transcribed from the city's official Bekanntmachungen (`wismar_2026/`, checked: votes sum to the valid votes, valid + invalid = voters). **Rügen Landrat, May 2001**: the sole candidate in the Ja/Nein runoff failed the quorum of 25 % of the electorate (§ 64 Abs. 3 KWG M-V) and a new election followed in September; the May cycle carries `flag_decisive_round_missing` and seats nobody (`is_winner = NA`, winner fields NA). Both came from the Landeswahlleitung's overview of 2026-10-12 (`landeswahlleitung_2026-10-12/`).

**Stichwahl completeness varies.** NRW and Bayern have both candidates in runoff elections. NS 2013 and RLP Stichwahl results list only the winner (1 candidate instead of 2). SH scrapes both Stichwahl candidates. NS only has separate Stichwahl PDFs for 2006 and 2013; other years have no candidate-level Stichwahl data.

**Sachsen year-boundary runoffs.** 2 elections where `election_year != year(election_date)` because the runoff crossed a calendar year (e.g., `election_year = 2008`, `election_date = 2009-01-11`).

**Sachsen "Stichwahl" is not a 2-person runoff.** In Sachsen, when no candidate wins >50% in the first round (VE), a full re-election (EE) is held with all candidates, not a 2-person runoff. This means `n_candidates_sw` can be 3--6 for Sachsen elections.

**Mayor panel: person identification is imperfect.** Bayern uses Amtsantritt dates, which correctly groups >99% of terms but cannot distinguish two different mayors with the same Amtsantritt date in the same municipality (extremely rare). Named states rely on name matching, which misses candidates without names in the data (1,384 elections, mostly Sachsen). 40 Bayern mayors show tenure spans >30 years, which are plausible given 6-year terms since 1945. One Bayern mayor has a 48-year span (10 terms).

**Mayor panel: duplicate winners of one election cycle.** Where two different persons are both marked as winners of one cycle (HW winner vs SW winner from separate data records, less than 60 days apart), the panel keeps the person with the higher vote share, which identifies the overall winner (6 records in the October 2026 build). Elections further apart are separate cycles and all are kept.

For the full list of known issues including fixed bugs, see `docs/mayoral_elections_known_issues.md`.
