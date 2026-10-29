# GERDA TODO / Status

Tracking outstanding work on the German Election Database. Last updated: 2026-09-28.

---

## How we work (since 2026-10-13)

- **One integrator for `main`.** Vincent's integrating Claude session (since 2026-10-27: the session preparing the mayoral/Landrat release; 2026-10-19: "Ingest Brandenburg mayoral scans 2004–2013 (Landeswahlleiter)", closed the same day; before that "Bavaria/Saxony-Anhalt GERDA updates", which no longer exists) is the only one that changes `main`'s working tree: it pulls new commits, applies changes, rebuilds, runs the audits and commits. Other sessions work in a scratch clone and send it patches.
- **Hanno: no direct commits or pushes for now.** A lot of mayoral/Landrat work is uncommitted on purpose while the statistics offices clarify what may be redistributed. Please report findings here or on Slack; they get integrated and credited.
- **Before starting an item**, put your name next to it in this file. The Rhein-Pfalz-Kreis 2017 code was fixed twice on 2026-10-12 because nobody had claimed it.
- **Permissions:** Brandenburg Statistische Berichte B VII 3-6 1993–2008: the AfS **consented** on 2026-10-27 (elected mayors: name, Wahlvorschlagsträger, Wahltag, votes, with a source citation), so the release is being prepared. Thüringen TLS reports 2000/2004/2010: may be published without any names (TLS, 2026-10-12). Bayern Bürgermeisterverzeichnisse and LfStat candidate lists: the LfStat **declined** on 2026-10-16 to let GERDA redistribute their names, birth years and professions, so they are now in the restricted twins only, winners included (applied in main the same day; known issues §19). The mayoral/Landrat work ships as one release.
- **Decided 2026-10-16 (Vincent):** the 2026 Bayern Mandatsträger names stay published (the refusal does not name them); Sachsen-Anhalt winners' names stay published; Bayern winners' gender stays published (names withheld). The integrator is now the session "Bavaria/Saxony-Anhalt GERDA updates".

---

## Open Issues

### 0. Data audit of all final datasets (2026-10-12)

A multi-agent audit found 13 high-severity and 32 medium-severity issues, plus labeling and documentation drift; see [audit report](docs/audit_2026-10-12.md) and `data/data_checks/audit_2026-10-12/`. Fixed on 2026-10-12: Greens merged with Die Grauen in Berlin 2021/2023, Hessen 1991 and BW 1992 (H1, M11); Rhein-Pfalz-Kreis Landrat 2017 AGS (H9); county covariates for Berlin 1991 and Thüringen 2016-2020 (H6, H7). Fixed on 2026-10-19, committed but not pushed (§0b): M12 (`far_right` without the Bavarian Republikaner, 0 instead of NA). Fixed upstream in the same period: `pop_density_ags` unit (H5), Schleswig-Holstein Kreiswahl rows (H10), Thüringen 2024 kreisfreie Städte and TH 2019/BB 2014 party columns (H11, H12). The other high-severity items are open and need decisions or new sources: Brandenburg 1999 state coverage (H2), postal votes missing in federal county 1957-65 (H3) and in eastern municipal federal data 1994-2002 (H4), city-state parliaments in `county_council_seats` (H8), and annual mayor panel forward-fill and duplicates (H13). Vincent: please pick up whichever of the open items (high or medium, see the report) you want to take on.

### 0b. Hanno's state-election notes (2026-10-16/18: `far_right` zeros, postal allocation)

**Claimed 2026-10-19 by the Claude session "Hanno's document issues". Applied in main and state files rebuilt on 2026-10-19 in two rounds (the patch, then the fixes from a five-reviewer audit: party-sum identity, Wahlkreis weights for ST pools, MV 1994/1998 pools by Wahlkreis, MV 2011 Amt column, Bremen 1999-2007 from the Wahlbereich totals); patches and HANDOVER.md in `review/state_farright_postal_2026-10-19/`. Rounds 1–4 committed together on 2026-10-19 (state paths and the state hunks of the shared docs only), not pushed. At the push: website update-log entry (draft in HANDOVER.md; add the round 3 and 4 items); render `codebook.pdf` at the next release (main's codebook also holds unreleased mayoral edits). Backups: `~/gerda_backups/2026-10-19_pre_state_farright_postal/` (before) and `..._round1_state_farright_postal/` (after round 1).** Scope: Bayern 1994-2013 REP/NPD under unmapped column names; SH 2000-2009 and HB 1999-2007 parties left in `other`; `far_right` NA where the source lumps parties into `other`; FDVP in `far_right`; MV postal allocation (flag, conservation); ST 2011 Brücken-Hackpfüffel postal district; ST 2011/2016 named postal pools; README allocation wording; crosswalk 2024 population note.

**Round 3 applied in main 2026-10-19 (13:45) by the integrator** (`review/state_farright_postal_2026-10-19/state_farright_postal_round3.patch`, Vincent's decisions of 2026-10-19: far_right counts pro NRW / pro Deutschland and the DLVH, not the DSU; far_left NA rule). The three harm files are rebuilt and all checks match the handover: far-right 50/50, accounting PASS 21/23/25, missingness 1 507 comparisons, `checks_state_harm.R` only the pre-existing turnout FAIL, Wahlkreis audit 0 FAIL, Excel exported and checked. Committed with rounds 1, 2 and 4. Backup: `~/gerda_backups/2026-10-19_pre_state_round3/`.

**Round 4 (Hanno's suggestions) applied in main 2026-10-19 by the session "Hanno's document issues"** (`review/state_farright_postal_2026-10-19/state_hanno_suggestions_round4.patch`): SH 2017/2022 and BB 2004/2014/2024 postal pools now go through `allocate_pooled_counts()` (flagged, exact; SH by Amt and Wahlkreis, A2), receivers capped at their electorate, new metadata `postal_allocation_rules.csv`, `state_postal_pools.csv`, `state_postal_receivers.csv`, `state_party_columns.csv`, crosswalk population notes in the codebook. Committed with rounds 1–3. Backup: `~/gerda_backups/2026-10-19_pre_state_round4/`. Not done from Hanno's list: per-row columns in the election files (the receivers table is the companion instead), capping in the Thüringen residual and the Sachsen lead pools.

**Round 5 (Hanno's unverified leads) applied in main 2026-10-19 by the session "Hanno's document issues", committed on top of rounds 1–4, not pushed** (verdicts in `review/state_party_leads_2026-10/report.md`; Vincent's decisions the same day). The leads were correct data (SN/ST 1990 Greens in `forum`/`grue_nf`; the western `linke_pds` gaps and explicit far-right zeros are parties that did not stand). Fixed: `far_left`/`far_left_w_linke` NA in Bremen 1991 and 1995 (no valid votes, so the NA rule now weights `other` by voters); `far_right` counts Hamburg's HLA and NL and Rheinland-Pfalz 1991's Deutsche Allianz – Heimatbündnis (RP block: "DA" → `deutsche_liga`). Backup: `~/gerda_backups/2026-10-19_pre_state_round5/`. Left for later (Vincent): WASG in `far_left_w_linke` (RP/BW/Berlin/MV 2006); NA instead of 0 for parties not on the ballot (RP, Hamburg, Saarland, Sachsen 2004); NI 2003 "–" read as NA.

### 0d. Hessen mayoral data: HSL October 2026 web table (2026-10-27)

**Owner: the integrator session (2026-10-27).**
- [x] `HE_Direktwahlen_BVII-m10-2026_StatHessen_tabelle_1.xlsx` (Stand 09.10.2026) parsed into `he_parsed_2026-10.csv`; `00_he_hist_parse.py` adds the 29 rounds the workbook lacks and corrects four workbook rounds (`HIST_FIXES`, votemanager-confirmed); six runoff pairings pinned; known issues §24.
- [x] Published `person_id`s kept: the registry rows of Wolfhagen 2023, Weinbach 2024 and Rödermark 2025 were moved to the corrected cycle dates.
- [x] **Votemanager/Komm.ONE integration completed by Codex on 2026-10-28; not committed or pushed.** Hessen: 75 verified rounds cover all 48 Gemeinden whose newer results HSL never received (60 new rounds) and replace all 15 percentage-only rounds of 2026. BW: 709 recovered rounds in complete cycles enter the pipeline; the net addition after correcting duplicate source dates is 18 rounds. Denkingen 2023 is corrected to 887 winner votes. Thirteen cycle-date migrations preserve published person IDs. All three audits pass (0 errors); outputs outside HE/BW are unchanged and Landrat files are byte-identical. Sources and exclusions: `raw/votemanager/README.md`, known issues §25.
- [x] **Final audit fixes (Claude, 2026-10-28; uncommitted).** Audit: `review/votemanager_2026-10-27/final_audit_claude_2026-10-28/` (logs of the rebuild in `fix_rebuild/`). Singen (Hohentwiel) OB 2021 stood twice (StaLA misdated 11.07.2023): eleventh date correction + thirteenth ID migration, and `01` now stops on identical counts on two dates in one BW Gemeinde (`99_audit.R` §37f). Walldorf 2021 runoff candidate split by "apl." and Scheer 2024 bulletin layout ("Surname Given, Residence") and a "Dr." in Heidelberg 2006 surnames fixed in `00_votemanager_supplement.py` (import-review replayed in a scratch copy, only those rows changed). Heidenrod 2025 winner linked to his 2019 term (`he_name_variant_links` in `03`; the unpublished id `p_06_01358` removed from the registry). Backup of the pre-fix state: `~/gerda_backups/2026-10-28_pre_audit_fixes/`.
- [x] **Bayern workbook rows that copy another round (Claude, 2026-10-29; uncommitted).** Notes: `review/by_st_same_counts_2026-10-28/NOTES.md` (rebuild logs in `apply_rebuild/`). Four rows of "Wahlen seit 1945" repeat another date's counts verbatim and are dropped by `by_source_duplicates.R` (pins in `metadata/by_source_duplicate_rows.csv`, read by `01`, `01b` and `03`): Gundelsheim runoff 1996 (seated the 1948 mayor), Babenhausen (Schwaben) 16.03.2015 (an extra term), Seukendorf runoff dated 10.07.2022, Wolfsegg 19.02.1978. `99_audit.R` §38 checks every state for rounds sharing all four counts with another date; ten source conventions and five unexplained groups are exempted by rule or pin (known issues §26). Checked 2026-10-29: Schwarzhofen 2014/2020 is genuine (Landratsamt Schwandorf figures); in Schwarzenbach and Eisenheim the 1996 row matches the same-day Gemeinderatswahl, the other row stays unverified; Jeggau 2008 is open (the StaLA holds only what the Gemeinde reported). The rebuild left one 16 KB block of NUL bytes inside `mayor_panel_annual_harm.csv` (caught by `99_audit.R` §33, as on 2026-10-13); the file was rewritten from its .rds with 03's `fwrite` call and is byte-identical to the damaged copy outside that block. Backup: `~/gerda_backups/2026-10-29_pre_by_source_duplicates/`.
- [x] **Final-audit decisions applied (Vincent, 2026-10-29; uncommitted).** Logs: `review/votemanager_2026-10-27/decisions_2026-10-29/rebuild4/`; backup `~/gerda_backups/2026-10-29_pre_decisions_1C_2B_3C_4B/`.
  - **1C, StaLA conflicts:** StaLA unless the Gemeinde's final notice confirms the municipal table. The notices found confirm the table for 7 of 16 rounds (Leibertingen, Bartholomä — result sheet + "Endergebnis" news, Bad Liebenzell, Löffingen — Mitteilungsblatt 17.12.2021, Weingarten, Durmersheim ×2). Küssaberg 2021 keeps the table as an exception: StaLA equals it minus postal district "Briefwahl 2" on all four fields. The other 8 are StaLA winner-only (`metadata/votemanager_stala_conflict_decisions.csv`).
  - **2B:** a losing candidate's name stays public only if public in every round of theirs (Dettenheim 2023 runoff row now withheld too; import-review replayed in scratch).
  - **3C, leader everywhere (Vincent's choice of direction):** first rounds that went to a runoff carry their leader in `winner_*` in all states. 1,112 rounds filled from `mayoral_candidates` (NI 231, HE 496, BW 91, SN 294 party) and 47 Landrat rounds from `landrat_candidates` (NI 19, HE 27, and Rügen May 2001, whose failed runoff still seats nobody; `99_audit.R` §30f now checks that runoff row).
  - **4B:** the register's fuller given name is kept where Komm.ONE prints only its first part (5 winners, incl. Beuren 2023 and Kirchardt 2024).
  - **Withheld names out of tracked files (3B):** the two losing candidates withheld under 2B (Dettenheim 2023, Lauterbach (Hessen) 2026) are blanked in `raw/baden_wuerttemberg/bw_komm_one_parsed.csv` and `raw/hessen/he2026_parsed.csv` by their scrapers (`withheld_losers.py`, pins keyed on counts in `metadata/withheld_loser_rows.csv`). Both reran from cache identical apart from those rows. Git history keeps the old files (no purge, as for Brandenburg).
- [ ] **Remaining municipal source gaps:** keep Schorndorf 07.11.2021 flagged (candidate sum 13,571 versus StaLA 13,570; no independent total). Fischerbach 13.10.2024 is partial; Schwäbisch Hall 04.07.2021 has a ten-vote accounting discrepancy; Eschbach 07.05.2023 lacks a recovered decisive round. Existing StaLA fallbacks remain. Archive follow-up: Aglasterhausen 31.01.2021, Höpfingen 13.06.2021, Nusplingen 05.06.2016 and the unconfirmed Hagnau 14.10.2001 register entry. Evidence and next source leads are in `review/votemanager_2026-10-27/remaining24/README.md`; no municipality has been contacted.

### 0c. Brandenburg mayoral and Landrat data (2026-10-19)

**Owner since 2026-10-27: the integrator session preparing the release (the AfS consented on 2026-10-27). Before: "Ingest Brandenburg mayoral scans 2004–2013 (Landeswahlleiter)", closed 2026-10-19. All of it is in main and rebuilt, but uncommitted: it ships with the mayoral/Landrat release. Re-verified 2026-10-27: a full rebuild reproduces every output byte for byte, all three audits pass, 47 workbooks PASS. Pick-up note with state, order of steps and next actions: [`review/bb_mayoral_2026-10-19/PICKUP.md`](review/bb_mayoral_2026-10-19/PICKUP.md).**

- [x] The Gemeinden's result reports 2004–2013 (Landeswahlleiter scans): 119 elections. See `raw/brandenburg/lwl_ergebnismeldungen_2004_2013/README.md` and known issues §20.
- [x] `fread()` doubled quotes in party labels (all states): 10 unharm and 13 candidate labels outside Brandenburg.
- [x] Every Brandenburg loser restricted, whatever the source (user decisions 2026-10-16/19). This covers 756 workbook and portal rows and 124 Landrat rows. The raw files are gitignored, with tracked name-stripped copies instead (known issues §21).
- [x] The Landeswahlleiter's archived result pages 2001–2017 (Internet Archive): 411 winner-only elections replaced, 7 added. See `raw/brandenburg/lwl_bmwahlen_archive/README.md` and known issues §22.
- [x] Losers' own-name Wahlvorschlag labels ("EB Muster") are the plain list type in the public files (user decision 2026-10-19): 228 mayoral and 6 Landrat labels, 7 unharm rounds; the twin keeps them.
- [x] Loser names removed from tracked code and docs (Landrat comments, the Landrat raw README, the workbook parser's name pins → gitignored `bb_lwl_name_fixes_restricted.csv`).
- [x] **Kreistag 2024 pages untracked (user decision 2026-10-19), done the same day.** The pages in `data/county_elections/raw/Kreistagswahlen/Brandenburg/2024_html/` list every candidate; only `03_county_seats.R` reads them, and only the table of elected members.
  - They are gitignored. `Brandenburg_2024_Sitze.csv` (tracked; Kreis, party, seats, total) holds what the script reads; its 'EB …' labels name elected members only. The script rewrites it whenever all 18 pages are present.
  - `county_council_seats` is `identical()` with and without the pages. Patch: `review/bb_mayoral_2026-10-19/bb_kreistag2024_untrack.patch`.
  - **At release:** `git rm --cached` the 18 pages and add the CSV.
  - A scan of every tracked council raw file (text, xlsx, xls, docx, pdf; umlaut paths included) found no other file that lists non-elected candidates. The MV statistical reports B731E 2014/2019 list elected Kreistag members only (office-holders).
- [x] **Sachsen-Anhalt Landrat raw files untracked (user decision 2026-10-27).** The five Landeswahlleiter files in `data/landrat_elections/raw/sachsen_anhalt/` name losing candidates; they are gitignored, and `01_landrat_combine.R` writes the tracked name-stripped copy `st_landrat_parsed_public.csv` (same code as Brandenburg's, now `write_landrat_public()`). The public files are `identical()` with and without the raw files; the Landrat audit checks the copy.
- [x] **At the release commit** (done: pushed 2026-10-27 as e541d25, ab84315, d6083a1; website 1d0d2fa):
  - `git rm --cached` the five Sachsen-Anhalt Landrat raw files and add `st_landrat_parsed_public.csv`;
  - `git rm --cached` the workbook xlsx, `bb_lwl_parsed.csv`, `bb_bm_parsed.csv` and `data/landrat_elections/raw/brandenburg/BB_*.html` (incl. the four `BB_pressemitteilung-*` pages, which name candidates too);
  - add `bb_lwl_parsed_public.csv`, `bb_landrat_parsed_public.csv` and the archive README, manifest and CDX listing;
  - note in the commit that the tracked `name_origin_lookup.rds` no longer holds the 642 name pairs of Brandenburg losers.
- [x] Git history: **kept, no purge** (user decision 2026-10-19). The untracked files' older versions stay in the repository history.
- [x] Questions to the Landeswahlleiter SENT 2026-10-19 (reply in Herr Sokol's thread "hh* 2025/1211 – Teil 4", cc the Landeswahlleiter): Schwarzheide 2008, Belzig, Liebenwalde, Eichwalde/Pritzwalk, and the four elections missing from the workbook. **Awaiting his answer.**
- [x] Website update-log lines at the release (2026-10-27, website 1d0d2fa):
  - Brandenburg mayoral 2004–2013 and 2001–2017;
  - Brandenburg losing candidates restricted;
  - the party-label quote fix.
- [x] **M22 party families: DONE 2026-10-19 in main (uncommitted; user decision 2026-10-19).**
  - `code/shared/party_family.R` sorts the labels into `cdu_csu`, `spd`, `gruene`, `linke_pds`, `fdp`, `afd`, `fw_wg`, `independent`, `other`, `joint_list` (NA: no label / mixed category).
  - New columns: `candidate_party_family` (mayoral + Landrat candidates, computed in 01b on the full frame; the combine fills only its own rows) and `party_family` (four panels, 03).
  - `party_switch` = no overlap of the family components of successive winners (joint lists count as each member; minor parties as parties); `is_new_party_mayor` = no component has won there before.
  - Effect: `party_switch` 48.8 % -> 22.8 % (Bayern 50.0 -> 21.3 %); every other column identical. `99_audit.R` §36, Landrat audit; known issues §23; READMEs; codebook (qmd + md by hand).
  - Election-level files too (user request 2026-10-19 evening): `party_family` next to `winner_party` in `mayoral_unharm` / `mayoral_harm` / `landrat_unharm`. 01b adds it to both unharm files (matches each round's label to its candidate: the own-name rule decides 235 + 20 rounds), 02 carries it with `winner_party`, the combine computes it in `build_unharm()` for its rows; 02 and the combine stop if 01b has not run after 01. Audit §36g + Landrat check. Backup `~/gerda_backups/2026-10-19_pre_m22b_election_families/`.
  - Noted on the way: 5 public labels are the candidate's own name while the name is withheld (3 Bayern winners whose LfStat names were withheld on 16.10, 2 Thüringen). Known issues §17.5: such labels are kept as published (decision 2026-10-12); the generic-label rule of 2026-10-19 is Brandenburg only. Unchanged; only Vincent can extend it.
  - Backup: `~/gerda_backups/2026-10-19_pre_m22_party_families/`.
- [x] The former Uckermark Landrat (chosen by the Kreistag 2010, lost 2018) **stays named** in the two comments and the codebook: they concern his office (user decision 2026-10-19).

### 1. Historical state-election errors (GitHub #7)

Brandenburg 1990 and 1994 now use verified official second-vote spreadsheets.
The 1990 reconstruction preserves election-day municipalities: 22 municipalities
that voted in Mecklenburg-Vorpommern are excluded, and Brunow/Dambeck are restored
from the original report. Both elections retain in-person coverage; pooled postal
votes are reconciled separately and are not allocated. The user approved these
geographic and postal-vote choices on 2026-09-28.

All 13 parties in 1990 and all 12 in 1994 reconcile with official statewide totals
when the separately retained postal counts are included. There are 1,792 municipal
records in 1990 (previously 1,793, including two false OCR identifiers) and 1,693 in
1994 (previously 1,644). See [repair notes](docs/sources/bb_1990_1994.md) and
`data/data_checks/bb_1990_1994/` for verification results and downstream coverage.

NRW 1966/1970 and SH 1983 were repaired in the September historical release.
SH 1983 still requires comparison with in-person totals, not totals including postal votes.

### 2. Follow-up on GitHub #7

- [x] Implement the `bundeslaendeR` comparison and move quality flags to the front.
- [x] Replace the Brandenburg OCR inputs and document the historical geography.
- [ ] Review the repaired exports and prepared reply in `docs/sources/bb_1990_1994.md`.
- [ ] Investigate the remaining Rheinland-Pfalz 2006 `linke_pds` comparison (2.557 percentage points) and the external BB 1994 BFWG/BürgerBündnis mapping; keep #7 open.
- [ ] Commit, push, and post the reply only after Hanno requests publication.

---

### 3. Other open / partially addressed enhancement requests

- [x] ✅ **`election_date` in federal municipality harmonized datasets** — added to `federal_muni_harm_21` and `federal_muni_harm_25`. Applied both as a post-hoc patch (`code/checks/add_missing_metadata.R`) and in the source scripts (`02_federal_muni_harm_21.R`, `03_federal_muni_harm_25.R`) so the change persists across pipeline runs.
- [x] ✅ **`state_name` and `ags_name` in federal municipality datasets** — added to `federal_muni_harm_21`, `federal_muni_harm_25`, `federal_muni_unharm`. Applied both as patch and in source scripts (`01_federal_muni_unharm.R`, `02_federal_muni_harm_21.R`, `03_federal_muni_harm_25.R`).
- [x] ✅ **`state_name` in municipal datasets** — added to `municipal_harm`, `municipal_unharm`, `municipal_harm_25`. Applied both as patch and in source scripts (`01_municipal_unharm.R`, `02_municipal_harm.R`, `03_municipal_harm_25.R`).

**Already done (confirmed):**
- ✅ `election_date` in `state_harm_21`, `state_harm_25`, `state_unharm`, `federal_muni_unharm`, `municipal_harm`, `municipal_unharm`
- ✅ `ags_name` and `state_name` in `state_harm_21`, `state_harm_25`
- ✅ `ags_name` in `municipal_harm`, `municipal_unharm`
- ✅ Codebook at `docs/codebook.md` (55KB, substantial)
- ✅ `population_ags` scale documented as thousands throughout the codebook (lines 86, 97, 159, 200, 277)

---

### 4. Pipeline completeness gaps

**Already done (since TODO was originally drafted):**
- ✅ **Mayoral harm**: `02_mayoral_harm.R` exists, produces `mayoral_harm.rds` plus `mayor_panel.rds` and annual panel
- ✅ **County elections (Kreistagswahlen)**: `01_county_elec_unharm.R` and `02_county_elec_harm_21.R` exist; `county_elec_unharm.rds`, `county_elec_harm_21.rds`, and muni/cty variants in final/
- ✅ **European elections**: `01_european_muni_unharm.R` and `02_european_muni_harm.R` exist; `european_muni_unharm.rds` and `european_muni_harm.rds` in final/

**Still outstanding:**
- [ ] **RLP mayoral elections**: only percentages (no absolute vote counts) due to source data limitations — would need original Excel/PDF sources with counts
- [x] ✅ **SH mayoral elections**: `00_sh_scrape.R` has been run; output files exist at `data/mayoral_elections/raw/sh/sh_mayoral_scraped.{rds,csv}`; SH mayoral data integrated into `mayoral_unharm` (45 rows, years 2023–2025). Coverage is limited to 2023+ because `wahlen-sh.de` does not publish historical bulk data.
- [ ] **Municipal 2024 missing states**: currently have BW, SL, BB, MV, SN, ST, TH. Missing **HE, NI, NRW, RLP, SH** for 2024. Some of these may not have held 2024 Gemeinderatswahlen.
  - **RLP 2024 specifically**: raw data not yet in repo. The Oct 2025 Gemeinderatswahlen zip covers BW, SL, SN, ST, TH but not RLP. Maurice Baudet von Gersdorff (email) was reaching out to the RLP statistical office. Blocked on data acquisition.
  - Note: RLP **mayoral** (Direktwahlen) 1994–2024 was delivered (Dec 2025 file) and is in `mayoral_unharm` (16 RLP mayoral elections in 2024).

### 9. Wahlkreis-level state elections (August 2026 extension)

**Done (branch `ltw-wkr-clean-extensions`):** 28 elections added to `ltw_wkr_unharm`
from clean text-layer PDFs already in the repo, no OCR (BE 1999–2021, BW 2001–2011,
BY 2008/2013, HB 2003–2023, HE 2009, NI 2008, SN 2004/2009, SL 1980–2017, SH 2005).
Coverage now 103 elections, 1980–2026. Per-election recoverability of everything
still missing: `docs/ltw_wkr_recoverability.md`. Audit: 0 FAIL.

**Follow-ups:**
- [ ] Re-download the two corrupt raw PDFs (BW 1996 `BVII2.pdf`, BE 1995
  `Ergebnisbericht.pdf` — broken xref, gs-unrecoverable) and re-classify them.
- [ ] Source the Bremen Hefte for 1991/1995/1999 (full party detail; the long-series
  PDF alone was judged insufficient — seat-winning parties hidden in Sonstige).
- [ ] Larger recoverability scan of the OTHER pipelines' raw holdings (state
  Gemeinde-level historical PDFs, county elections, mayoral), same method:
  empirical text-layer tests + per-file classification.
- [ ] Investigate the pre-existing BW 2026 CSV parser warning: statewide mismatch of
  5,108 erststimme votes for "Anderer Kreiswahlvorschlag" (warns, doesn't halt).
- [ ] The remaining Wahlkreis gaps are OCR-class (see the recoverability doc); they
  join the OCR problem in item 1 above rather than the clean pipeline.

---

### 5. User-reported bugs (status from emails)

| Reporter | Issue | Status |
|---|---|---|
| Pauly (Spiegel) | Kleinstgemeinden dropped in crosswalks | ❓ Needs verification — specific AGS list from Pauly required |
| Pauly | 0.1% missing votes in state 2016/2021 harmonized | ✅ Fixed — `total_vote_share ≈ 1.0000` across all RLP/BaWü rows |
| Pauly | RLP 2021 "Sonstige" double-counting (FW, ÖDP, Die Partei, Tierschutz, Piraten, Volt) | ✅ Fixed — each party has distinct non-overlapping values |
| Pauly | RLP 2021 missing `election_date` | ✅ Fixed — `election_date = 2021-03-14` |
| Pauly | Time-series loading support | ❓ Feature request, separate discussion |
| Oriola | RLP 1999 municipal missing vote shares | ✅ Fixed by Florian Sichart |
| Greinetz | 1949 municipality data | N/A — directed to Julian Voss's dataset |
| Lockwald | Shapefile location | N/A — answered directly |

---

### 6. Party coverage policy

**Decision:** we will not break out additional minor parties (e.g., Bündnis C, PfV, KlimalisteBW, and other small or single-election parties) as separate columns in GERDA. Minor parties change across elections and across states, and breaking them out would explode the column count and create many near-empty columns. They remain aggregated in the `other` column. Users needing specific minor parties should go to the raw source data.

---

### 7. Legacy items from internal doc (pre-2025)

Audited 2026-04-22 against current state of the data.

**Still outstanding:**

- [ ] **Small municipalities suppressed post-2021 in federal muni data.** Since BTW 2021, the Bundeswahlleiterin suppresses Zweitstimmen for some very small municipalities (e.g. Friedrichsgabekoog `01051033`, Zendscheid `07232331`) for Geheimhaltungsgründe, rolling their votes into a collective "einschl. Gemeinde X" row under the parent Gemeinde (Reinsbüttel, Sankt Thomas). These AGS are therefore missing from `federal_muni_unharm` and the harmonized 2021/2025 files. The votes are included in state totals under the parent Gemeinde. **Needs:** a note in the codebook / README documenting which AGS are affected and why.
- [x] ✅ **SH/ST municipal pre-2018 source verified** (Thomas's comment). Checked 2026-04-22:
  - SH raw files are named `sh_gemeindewahlen_YYYY.xlsx`; loaded via `01_municipal_unharm.R` as `sh_YYYY_gemeinderatswahlen_data`. Years 1994, 1998, 2003, 2008, 2013, 2018, 2023 match the SH Kommunalwahl schedule. Confirmed Gemeinderatswahlen.
  - ST raw file `sachsen-anhalt_1994_to_2019.xlsx` has sheets "1994", "1999", "2001-2004", "2005-2010", "2014-2015", "2019". Main Kommunalwahl years (1994, 1999, 2004, 2009, 2014, 2019, 2024) have normal row counts (218–1,289). Non-standard years (2005–2008, 2010) have only 2–18 rows each — these are special elections for individual newly-formed municipalities after the 2007–2010 Gemeindereform, not Kreistagswahlen. The drop from 1,121 rows (2004) to 190 (2009) to 218 (2014+) reflects the real municipal consolidation.
- [x] ✅ **Population scale.** Already documented as thousands in `docs/codebook.md` (Berlin `population_ags = 3,677.472` = 3.68M people). The Google Doc note "in 100s" was incorrect.
- [x] ✅ **Kumulieren/Panaschieren documentation gap** (Andreas). Added notes to `docs/codebook.md`: HH/HB 2011+ 5-vote state elections (near the BY Gesamtstimmen note) and a municipal-elections note covering BW/BY/HE/RP/MV/SH/SL and others — clarifying that `valid_votes` counts cast individual votes, party shares are proportions of these, and NRW is the single-vote exception.
- [ ] **Parquet export** (Sascha). Nice-to-have for smaller file sizes and faster downloads. No `.parquet` files in `data/` currently.
- [ ] **Link to external datasets.** Cornelius's dataset, Julian Voss's 1949 dataset — future integration opportunity.
- [ ] **Dissemination workflow.** Send a post via PolMeth / EPSA mailing lists when GERDA is updated with new elections.

---

### 8. Council seats follow-ups (merged to main 2026-07-23)

The council-seats branch (`feat/council-seats`, merge commit `8b20142`) added ten `seats_*` columns to `municipal_unharm` and the `county_council_seats` panel (400 counties x 2008-2025). Change record: `docs/council_seats_update_2026.md`. Remaining items:

- [ ] **Municipal voter-group seat columns (deferred).** Two columns (Wählergruppen, Gemeinsame Wahlvorschläge) are dropped by a `-contains()` step in `01_municipal_unharm.R` (~line 20205) AND are mislabeled at the source: Brandenburg 2003 vs 2008 write raw "Wählergruppen" seats into swapped output columns. Audit BB 2003/2008/2014/2019/2024 against official sources before exposing them.
- [ ] **Ask Vincent:** (a) is `government_party` in the hand-compiled county seats file the Landrat's party? (currently inferred, not confirmed); (b) provenance/citation line for `Sitzverteilungen_der_Parteien_2008-2022_v1-0-0` for the codebook.
- [ ] **Municipal seats coverage gaps** (optional extensions): RLP seats stop at 2019, SH at 2018 (newer elections need collection); no municipal seats at all for Bayern, Berlin, Sachsen.
- [ ] **County seats pre-reform backfill** (optional): the panel uses fixed current county codes, so abolished pre-reform councils are absent (all of MV 2008-2010 is NA; same for pre-2009 Städteregion Aachen and pre-2016 LK Göttingen predecessors). Backfilling would require mapping abolished county councils.
- [ ] **Refresh project `CLAUDE.md`.** Stale claims: "county elections: no processing code" (there are now unharm/harm_21/seats scripts and final datasets) and municipal coverage "1990-2021" (now 1984-2026 with seats columns).
- [ ] **Website/package release alignment.** gerda R package catalog updated 2026-07-23 (dev version: `county_council_seats` exposed, municipal ranges corrected; 47 datasets). When the next package version ships, mirror the NEWS entry in the website `update-log.md` / `r-package.md` (see gerda repo `CLAUDE.md` for the checklist) — 0.7.1 is still pending at CRAN.
- [ ] **Broken pointer file.** `meinungsbild/docs/methodology_notes.pdf` is a 130-byte Git-LFS pointer, not a real PDF (siblings `.qmd`/`.md`/`.html` are fine). Re-render from the `.qmd` or delete.

**Confirmed done (for traceability):**

- ✅ `incogruence` typo in column names (Vincent's fix). No longer present.
- ✅ `Inf` turnout values in `municipal_harm.rds` (Vincent's fix). `turnout` range now 0.27–1.0 with zero Inf/NaN.
- ✅ Federal muni county level 1953–1967 coverage. `federal_cty_unharm` covers 1953–2025.
- ✅ Berlin AGS `1100000` only-in-1990 issue. Now appears across multiple years.
- ✅ Sachsen GE/TG duplicates (fixed Feb 2026).
- ✅ BSW column in harmonized state datasets.
- ✅ AfD in harmonized data starting 2013 (per paper appendix).
- ✅ R package on CRAN (`install.packages("gerda")`).
- ✅ Website uses HTTPS.
- ✅ Paper revisions (economic voting removed, PDF software description added, electoral system overview, ReadMe, Party Manifesto / ParlGov crosswalk, etc.). Paper published in Nature Scientific Data 2025.
- ✅ `election_date`, `state_name`, `ags_name` added to federal muni datasets (2026-04-22 via `code/checks/add_missing_metadata.R`).
- ✅ `state_name` added to municipal datasets (same patch).
- ✅ `flag_*` columns moved to the front of `state_unharm` (same patch).
- ✅ `population_ags` unit (thousands) already documented in codebook.

---

## Reference: State Election Datasets — Known Issues

(Folded in from the previous `docs/state_elections_known_issues.md`, February 2026. Methodology and changelog: `docs/state_elections_update_2026.md`. Automated checks: `code/checks/checks_state_harm.R`.)

### Dataset overview

Four state election datasets are available:

| Dataset | File | Rows | Cols | Elections | States | Target Boundaries |
|---------|------|------|------|-----------|--------|-------------------|
| `state_harm_21` | `data/state_elections/final/state_harm_21.rds` | ~33,286 | 44 | 2006–2024 | 15 | 2021 |
| `state_harm_23` | `data/state_elections/final/state_harm_23.rds` | ~27,000 | 43 | 2006–2023 | 14 | 2023 |
| `state_harm_25` | `data/state_elections/final/state_harm_25.rds` | ~33,189 | 44 | 2006–2024 | 15 | 2025 |
| `state_2224_unharm` | `data/state_elections/final/state_2224_unharm.rds` | ~6,405 | 35 | 2022–2024 | 11 | Original |

All three harmonized datasets use the weighted-sum-of-counts aggregation method. An older `state_harm.rds` (2006–2019, weighted mean of shares) is retained for backward compatibility.

### Key differences between datasets

| Feature | `harm_21` | `harm_23` | `harm_25` | `2224_unharm` |
|---------|-----------|-----------|-----------|---------------|
| `bsw` column | Yes | **No** | Yes | Yes |
| Bremen included | Yes | **No** | Yes | Yes |
| 2024 elections | Yes | **No** | Yes | Yes |
| Greens column name | `gruene` | `gruene` | `gruene` | **`grune`** |
| `other` column | Yes | Yes | Yes | **No** |
| Quality flags | Yes | Yes | Yes | **No** |
| Covariates | Yes | Yes | Yes | **No** |
| `state_name` column | Yes | Yes | Yes | **No** |

### Coverage

No dataset includes **Hamburg** (state `02`) — Hamburg lacks municipality-level state election data in the DESTATIS pipeline.

`state_harm_23` excludes Bremen (state `04`) and all 2024 elections. The 11 state-year combinations in `state_2224_unharm` are: SL 2022, SH 2022, NRW 2022, NI 2022, HB 2023, HE 2023, BY 2023, BE 2023, BB 2024, SN 2024, TH 2024.

### Turnout issues

**NA turnout (all datasets).** Four Thuringia municipalities: `16063033`, `16074082`, `16075087`, `16075105`. Source data issue in Thuringia 2024.

**Turnout > 1 (`state_harm_21`, `state_harm_25`).** 11 rows slightly exceed 1.0 in SH 2022 and SN 2024, likely from mail-in vote allocation. Vincent (April 2026) added `flag_harm_turnout_above_1` and caps >1.5 to NA.

**Turnout = 0 (`state_harm_23` only).** ~26,858 rows with `turnout = 0`, concentrated in pre-2022 election years. Fix is in `state_harm_21` and `state_harm_25` but not in `state_harm_23`. **Prefer `state_harm_21` or `state_harm_25` for turnout analyses.**

**Additional NA turnout (`state_harm_23` only).** AGS `07336021` (RLP).

### Vote share issues

`total_vote_share` deviates from 1.0 by >5pp for ~2% of rows — tracked party columns don't cover every minor party. 4 rows across `state_harm_21`/`state_harm_25` have `total_vote_share < 0.7` (elections with large local-party shares).

### Party-specific

- **Bremen AfD (2023)**: coded as 0 in harmonized datasets, `NA` in `state_2224_unharm`. AfD did not contest.
- **BSW**: not in `state_harm_23` (BSW first contested in 2024, outside coverage).
- **`grune` vs `gruene`**: `state_2224_unharm` uses `grune`; harmonized datasets use `gruene`.

### Quality flags

`flag_unsuccessful_naive_merge`: rate ~19% in `state_harm_21` (driven by 100% flag rate for 2022–2024 elections from the different `03_state_2022-24.R` source pipeline — mechanical, not a data issue). Rate ~0.05% in `state_harm_23`/`state_harm_25`.

`flag_total_votes_incongruent`: rate ~11–13%, concentrated in 2008 (~40%) and 2018 (~67%). Source data issue from DESTATIS pipeline.

### Covariate gaps

Covariates (`area_ags`, `population_ags`, `employees_ags`, `pop_density_ags`) are present in all three harmonized datasets but systematically NA for recent elections:

| Dataset | Gap | Reason |
|---------|-----|--------|
| `state_harm_21` | 100% NA for 2022–2024 | `ags_area_pop_emp.rds` covers through ~2021 |
| `state_harm_23` | `employees_ags` 100% NA for 2022–2023 | Employment data ends earlier |
| `state_harm_25` | 100% NA for 2024 | Covariate source covers through ~2023 |

Population in thousands.

### Cross-dataset vote total differences

Up to ~5% difference across target years for Sachsen, ~2.4% for RLP, <2% for others. Expected: different crosswalk targets use different population-weighted mappings for municipal boundary changes. Not data errors.

### `state_harm_23` specific issues

`state_harm_23` (produced by `04_state_harm_23.R`) has:
1. Turnout = 0 for ~26,858 pre-2022 rows (unfixed `number_voters` bug)
2. No BSW column
3. No Bremen
4. No 2024 elections
5. Additional NA turnout for AGS `07336021`
6. 14 states, 40 state-year combos (vs. 15/48 in `harm_21`/`harm_25`)

**For most uses, prefer `state_harm_21` or `state_harm_25`.**
