# Mayoral Elections Data

Seven datasets covering mayoral elections in 13 German states (Baden-Württemberg, Bayern, Brandenburg, Hessen, Mecklenburg-Vorpommern, Niedersachsen, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland, Sachsen, Sachsen-Anhalt, Schleswig-Holstein, Thüringen), 1945--2026.

Scope: head-of-municipality elections (`Bürgermeisterwahl`, `Oberbürgermeisterwahl`, `VG-Bürgermeisterwahl`, `SG-Bürgermeisterwahl`). Head-of-county elections (`Landratswahl`) are published as a separate dataset — see [`../../landrat_elections/final/`](../../landrat_elections/final/).

## Datasets

| File | Rows | Cols | Unit | Description |
|---|---|---|---|---|
| `mayoral_unharm` | 48,306 | 17 | Election | One row per election-round (winner-level summary), original boundaries |
| `mayoral_harm` | 46,955 | 23 | Election | Same as above, mapped to 2021 municipal boundaries |
| `mayoral_candidates` | 99,013 | 45 | Candidate | One row per candidate per election cycle (wide format), original boundaries, incl. candidate characteristics |
| `mayor_panel` | 38,790 | 31 | Person-election | Within-mayor panel with person IDs, original boundaries |
| `mayor_panel_harm` | 37,701 | 32 | Person-election | Same as above, mapped to 2021 municipal boundaries |
| `mayor_panel_annual` | 199,076 | 27 | Person-year | Annual panel (forward-filled from elections), original boundaries |
| `mayor_panel_annual_harm` | 193,453 | 28 | Person-year | Same as above, mapped to 2021 municipal boundaries |

All files are available as `.rds` and `.csv`.

## Related datasets

Landrat (county head) elections live in their own dataset at `../../landrat_elections/final/landrat_unharm.{rds,csv}` and `landrat_candidates.{rds,csv}`. They share the same schema as the mayoral files but cover county-level units (8-digit AGS ending in `000`) and are not harmonized to municipal boundaries.

## Known data-quality issues

- **NRW 2025 Stichwahl date typo (patched)**: The IT.NRW raw file `KW 2025 Oberbürgermeister-Landratswahlen.xlsx` encodes every Stichwahl row's date as Excel serial `44101` (2020-09-27) instead of `45928` (2025-09-28). This was verified by direct XML inspection — it is a source-data error, not a coding error. Our pipeline patches the dates in [`code/mayoral_elections/01_mayoral_unharm.R`](../../../code/mayoral_elections/01_mayoral_unharm.R) and [`01b_mayoral_candidates.R`](../../../code/mayoral_elections/01b_mayoral_candidates.R) so that 2025 SW data is usable. The patch will be removed once IT.NRW corrects the source file.

- **Baden-Württemberg — hybrid (Komm.ONE + Statistisches Landesamt)**: BW (state `08`) covers all 1,101 Gemeinden, combining two sources. (a) **Komm.ONE** (`wahlergebnisse.komm.one`, the votemanager portal of BW's municipal IT provider) supplies **full candidate-level** results — all candidates, votes, full turnout, and both Hauptwahl and Stichwahl — for the municipalities/elections it publishes in its modern format (election dates ~2003–2025; **274 BW elections now have full candidate lists**). (b) The **Statistisches Landesamt** report B VII 3-j/25 (most-recent election per Gemeinde as of 31.12.2024) fills the rest — the ~217 Gemeinden whose election is only in Komm.ONE's older format, and elections predating the portal — as **winner-only** rows (no losing candidates, `n_candidates` `NA`). Komm.ONE supersedes the StaLA row for a cycle only when it captured the decisive round (a Stichwahl, or a Hauptwahl winner ≥50%); otherwise the StaLA winner is kept (e.g. the rare case where a runoff is absent from the portal). **No party affiliation either way** — BW mayoral candidates have no Wahlvorschlagsträger, so `winner_party`/`candidate_party` are always `NA`. The elected winner's **gender and birth year** come from the official register (StaLA), carried onto the Komm.ONE winner where available (else gender is name-predicted); the published count of 114 elected women is preserved. Komm.ONE candidates carry no birth year. Sources: Komm.ONE votemanager (`code/mayoral_elections/00_bw_komm_one_scrape.py`) and StaLA B VII 3-j/25 (`00_bw_parse.py`). A few per-Gemeinde StaLA fields not in the unified schema — `amtsperiode`, `hauptamtlich`, `eu_citizen`, `briefwaehler`, `wahlgrund` — are preserved in the raw intermediate `data/mayoral_elections/raw/baden_wuerttemberg/bw_parsed.csv`.

- **Brandenburg coverage (recent-cycle portal scrape)**: BB (state `12`) is scraped from the Landeswahlleiter result portal, which publishes only the **current cycle** — the most recent Bürgermeister-/OB-wahl of each amtsfreie Gemeinde/Stadt + the 4 kreisfreie Städte (~2018–2026). It is not a full historical series, and BB excludes the amtsangehörige Gemeinden (ehrenamtliche Bürgermeister), whose portal keys are 12-digit Amt+Gemeinde codes without a clean 8-digit AGS. Unlike Baden-Württemberg, BB publishes **party affiliation and all candidates** (Hauptwahl + Stichwahl), so it populates the full candidate schema; candidate gender is predicted from names (no register data). Single-candidate rounds are Ja/Nein votes (the candidate's votes are the Ja-Stimmen, so they sum to less than the valid votes). Source: Landeswahlleiter Brandenburg (`wahlen.brandenburg.de`).
- **Sachsen-Anhalt coverage (full historical series, 1994–2026)**: ST (state `15`) comes from the Statistisches Landesamt Sachsen-Anhalt file *"Bürgermeisterwahlen in Sachsen-Anhalt ab 1994"* (Stand 13.07.2026) — **4,611 round-results across 1994–2026**, with all candidates and their votes for both rounds, party (formal Wahlvorschlagsträger), and the elected mayor's gender, birth year and Amtsantrittsdatum. Because municipalities were repeatedly merged, `ags` is the AGS **at the time of the election** (2,057 distinct historical codes mapping onto today's 218 Gemeinden); the harmonised file maps these onto 2021 boundaries. Two caveats. **1994 is sparse by design** — the source states that for many 1994 elections only the winner is known, so vote counts and losing-candidate names are often missing (599 of 1,299 rows lack Gültige Stimmen); later years are essentially complete. **The elections that the older Dezernat-13 extract also covers (2019–2026) are taken from that extract**, which is the more accurate record for that window. A small number of rows carry documented source anomalies: three have Wähler > Wahlberechtigte (turnout > 1, flagged), two have candidate votes that do not sum to the Gültige Stimmen (Halle 2025, Leuna 1994), and six rows are flagged `flag_shared_ags` because the source gives two different Gemeinden the same AGS on the same day. Sources: Statistisches Landesamt Sachsen-Anhalt (historical file + Dezernat-13 "Datensatzbeschreibung Bürgermeisterwahlen"), with the Landeswahlleiter portal (`wahlergebnisse.sachsen-anhalt.de`) retained as a fallback.

- **Hessen coverage & limitations**: HE (state `06`) is parsed from the Hessisches Statistisches Landesamt report *B VII m – Direktwahlen*, a most-recent-per-Gemeinde/Landkreis snapshot (election dates ~2020–2026). The primary source is now the **May-2026 issue in XLSX form** (`00_he_parse_xlsx.py`), which lists **every Wahlvorschlag with its votes** (real `n_candidates`, up to 20) and **full vote counts** — a major upgrade over the older 2024 PDF (winner + first Wahlvorschlag only), which is retained only as a fallback (`he_pdf_parsed.csv`) for the ~57 units whose last election predates the XLSX's 2020 window. 421 Gemeinden (Bürgermeister + Oberbürgermeister: the 5 kreisfreie Städte + the 7 Sonderstatusstädte Hanau, Fulda, Gießen, Marburg, Rüsselsheim, Wetzlar, Bad Homburg) feed the mayoral dataset; the 21 Landkreise (Landrat) are split to [`../../landrat_elections/final/`](../../landrat_elections/final/). The winner is the **highest-voted Wahlvorschlag** of the decisive round (named, with party + gender); losing Wahlvorschläge carry party + votes but no candidate name. Single-candidate elections are Ja/Nein (winner votes = Ja-Stimmen < valid). Source: HSL report B VII m (statistischebibliothek.de); the parser is a byte-exact reproduction (0/463 party-vote mismatches vs the raw table), and a winner sample was independently re-verified externally.

- **2026 Kommunalwahlen (Bayern + Hessen, newest cycle)**: The most recent local elections — **Bayern** (8 March 2026, Stichwahl 22 March) and **Hessen** (15 March 2026, Stichwahlen 29 March / 12 April, plus Jan/June by-elections) — are added from sources newer than the historical files above, each independently spot-verified against official municipality pages and local press (13/13 winners confirmed).
  - **Bayern 2026** (1,920 Gemeinden incl. 50 Oberbürgermeister; 62 Landrat split to the landrat dataset): from the Bayerisches Landesamt für Statistik *Mandatsträger* XLSX (official). Full candidate-level votes + turnout; the elected person's name/gender/birth year/first-Amtsantritt come from the Mandatsträger sheet and are attached to the winning Wahlvorschlag (the panel links re-elected incumbents by Amtsantritt — 1,075 of the 2026 winners were already in office). Candidate names exist only for the winner (Bayern publishes no losing-candidate names).
  - **Hessen 2026** (28 Gemeinden incl. Hanau OB; 9 Stichwahlen) is a **hybrid**: the ~12 Gemeinden already in the May-2026 *B VII m* XLSX carry **full vote counts** (via the `hessen` source above); the remaining ~16 — the by-elections (Jan/June) and March results not yet in that report issue — are scraped from the hessenschau result pages as **percentage-only** (like RLP: candidate + party + vote share + turnout, but `valid_votes` / `candidate_votes` `NA`). The two sources are de-duplicated by `(ags, election_date)`. The winner is the decisive round's top result (Stichwahl if held); a built-in integrity check guarantees no runoff is missed (every recorded winner ≥ 50%), and the runoff date is read from the page text (not the URL). The %-only rows are superseded by full vote counts as later *B VII m* issues add them. All 13 sampled 2026 winners were independently re-verified (13/13).

---

## 1. `mayoral_unharm` -- Election-Level, Unharmonized

One row per (municipality, election date, election type, round). Reports the winner's party, votes, and vote share.

**Columns**: `ags`, `ags_name`, `state`, `state_name`, `election_year`, `election_date`, `election_type`, `round`, `eligible_voters`, `number_voters`, `valid_votes`, `invalid_votes`, `turnout`, `winner_party`, `winner_votes`, `winner_voteshare`, `flag_superseded`

**`flag_superseded`** (logical): `TRUE` for a Bayern round that did **not** seat the mayor and is superseded by a later valid round — either (a) an annulled round (`Wahlart` "... ungültig"), or (b) a Hauptwahl that won **no absolute majority** (winner < 50%), was **not** resolved by a Stichwahl, and is followed by a repeat Hauptwahl (*Neuwahl*) for the same office within 250 days (a failed first attempt, or an early-postwar multi-ballot council election). The majority test deliberately spares Hauptwahlen that were duly won (≥50%) and merely preceded a later **by-election** — those seated a mayor and are not flagged. These rows are **kept, not dropped**, so no election is lost; filter `flag_superseded == FALSE` to restrict to decisive rounds. `FALSE` for every non-Bayern state — the Bayern "Wahlen seit 1945" source is the only one that records precursor rounds as separate dated rows (the `mayor_panel` already excludes them via its own `ungültig`-filter + Amtsantritt dedup). 87 rows flagged in `mayoral_unharm`.

**Election types**: `Buergermeisterwahl`, `Oberbuergermeisterwahl`, `Landratswahl`, `VG-Buergermeisterwahl` (RLP), `SG-Buergermeisterwahl` (NI)

**Round**: `"hauptwahl"` (first round) or `"stichwahl"` (runoff). All states have runoff data; for BW both the pre-2023 *Neuwahl* and the post-2023 *Stichwahl* second round are coded `"stichwahl"`.

---

## 2. `mayoral_harm` -- Election-Level, Harmonized to 2021 Boundaries

Maps all AGS codes to fixed 2021 municipal boundaries using population-weighted crosswalks (`data/crosswalks/final/ags_crosswalks.csv`). This allows consistent panel analysis across time despite municipal mergers and splits.

Same 16 core columns as `mayoral_unharm` (including `round`), plus 7 flag/metadata columns:

| Column | Description |
|---|---|
| `flag_unsuccessful_naive_merge` | Crosswalk year-shift fallback was needed |
| `flag_pre_1990` | Pre-1990 election using 1990 crosswalk as fallback |
| `flag_aggregated` | Row aggregated from multiple predecessor municipalities |
| `flag_turnout_above_1` | Raw turnout exceeded 1 before capping |
| `flag_voteshare_above_1` | Raw winner voteshare exceeded 1 before capping |
| `flag_pct_only` | Percentage-only data (all counts NA; RLP + some old Bayern) |
| `n_predecessors` | Number of predecessor municipalities in aggregation group |

**Key design decisions**:

- **Grouping by `(ags_21, election_date)`** rather than `(ags_21, election_year)`, so runoff elections stay as separate rows and non-synchronized predecessor elections are not incorrectly merged.
- **Aggregation**: For N:1 mergers (103 rows), vote counts use `sum(x * pop_cw)` and the winner is taken from the largest predecessor by population. For percentage-only data (RLP), turnout and vote share use weighted means.
- **Pre-1990 fallback**: Bayern data from 1945--1989 (19,093 rows) uses the 1990 crosswalk since no earlier crosswalk exists.
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
| `candidate_votes_hw` | Hauptwahl vote count (NA for RLP) |
| `candidate_voteshare_hw` | Hauptwahl vote share (0--1) |
| `candidate_rank_hw` | Hauptwahl rank by votes (1 = most votes) |
| `n_candidates_hw` | Number of candidates in the Hauptwahl |
| `candidate_votes_sw` | Stichwahl vote count (NA if not in runoff) |
| `candidate_voteshare_sw` | Stichwahl vote share (NA if not in runoff) |
| `candidate_rank_sw` | Stichwahl rank (NA if not in runoff) |
| `n_candidates_sw` | Number of candidates in the Stichwahl |
| `is_winner` | Overall election winner (won HW outright OR won SW) |
| `flag_superseded` | `TRUE` for Bayern rounds that did not seat the mayor (annulled rounds, or a no-majority Hauptwahl repeated by a later *Neuwahl* within 250 days). Constant within an election. `FALSE` elsewhere. Filter `== FALSE` for decisive rounds only. 239 candidate rows flagged. |
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
| Brandenburg | Yes | -- | Yes | Yes | -- | -- | Yes |
| Hessen | Yes | Yes | Yes | Yes | -- | -- | Winner (if Wahlvorschlag 1) + WV1 |
| Sachsen-Anhalt | Yes | -- | Yes | Yes | -- | -- | Yes |
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

**Coverage**: 14,187 / 14,174 named candidates (100%) have gender assigned. 2,129 from raw data, 12,058 predicted. Bayern candidates have no names in the source data and thus no gender. 53 entries without gender are non-person records (party names, place names, initials).

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

**Coverage**: 14,859 candidates with last names classified. 255 (1.7%) flagged as migration background. 121 low-confidence classifications (conf < 0.80), predominantly Eastern European surname endings.

**Important caveats**: This method cannot detect naturalized Germans with German-sounding names, nor can it distinguish between recent migration and families that have been in Germany for generations. Some German-origin names match non-German patterns (e.g., "Zein" is both a German and Arabic surname). The low base rate (~1.7%) means even small false-positive rates produce a non-trivial share of misclassifications. Use the confidence scores for sensitivity analysis.

---

## 4. `mayor_panel` / `mayor_panel_harm` -- Within-Mayor Panel

One row per person per election. Tracks individual mayors across multiple terms, enabling mayor fixed effects (person FE) estimation. The `_harm` version maps all AGS codes to 2021 municipal boundaries.

**Person identification**:

- **Bayern**: Groups consecutive terms via `Tag des ersten Amtsantritt` (same Amtsantritt date across elections = same person). No candidate names are available.
- **Named states** (NRW, NI, RLP, SL, SH): Name-key matching using `tolower(last_name)_first_initial_state` within each municipality.
- **Sachsen**: Partial name matching (~49% coverage); remaining winners get sequential IDs.

**Columns**:

| Column | Description |
|---|---|
| `person_id` | Unique mayor identifier (e.g., `p_09_00001` for Bayern, `p_05_mueller_k_05` for NRW) |
| `ags` | 8-digit municipality code (original boundaries) |
| `ags_21` | 8-digit municipality code mapped to 2021 boundaries (`_harm` only) |
| `state` | 2-digit state code |
| `election_year` | Year of the election |
| `election_date` | Date of the decisive round (Stichwahl date if applicable) |
| `term_number` | Sequential term count within (person, municipality), starting at 1 |
| `winner_party` | Party of the winning candidate |
| `winner_voteshare` | Vote share in the decisive round (0--1) |
| `winning_margin` | Vote share difference between winner and runner-up (0--1) |
| `n_candidates` | Number of candidates in the election |
| `is_incumbent` | 1 if `term_number >= 2`, else 0 |
| `next_runs_again` | 1 if this person wins the next election in this municipality, 0 if a different person wins, NA if no subsequent election observed |
| `tenure_start` | Year of the mayor's first election in this municipality |
| `years_in_office` | `election_year - tenure_start` |
| `term_start_date` | Date of first taking office (Bayern: Amtsantritt; others: first election date) |
| `n_terms` | Total number of terms observed for this person |
| `total_tenure_years` | Year span from first to last election |
| `has_margin_variation` | Whether winning margin varies across this person's terms (useful for FE feasibility) |
| `candidate_gender` | Mayor's gender: `"m"` / `"w"`. From raw data or predicted via `gender-guesser`. NA for Bayern. |
| `candidate_gender_source` | `"raw"` or `"predicted"`. NA for Bayern. |
| `candidate_gender_prob` | Confidence score (0--1). See `mayoral_candidates` for details. |
| `candidate_gender_method` | Classification method. See `mayoral_candidates` for details. |
| `candidate_migration_bg` | Binary migration background (0/1). NA for Bayern. |
| `candidate_migration_bg_prob` | Probability of migration background (0--1). |
| `candidate_name_origin` | Fine-grained name origin category. |
| `candidate_name_origin_conf` | Confidence in origin classification (0.50--0.95). |
| `candidate_name_origin_method` | Detection method for origin classification. |

**Coverage**: 14,452 unique mayors (unharm) / 13,971 (harm), spanning 34,495 / 33,319 person-elections. Candidate characteristics available for non-Bayern states (3,089 / 34,495 person-elections have gender; Bayern has no candidate names in source data).

---

## 5. `mayor_panel_annual` / `mayor_panel_annual_harm` -- Annual Mayor Panel

One row per mayor per year. Forward-fills election-level data across the mayor's term, creating a balanced annual panel. The `_harm` version maps all AGS codes to 2021 boundaries.

Each mayor-election is expanded from `election_year` to the year before the next election in that municipality (or 2025 if no subsequent election is observed).

**Columns**:

| Column | Description |
|---|---|
| `ags` | 8-digit municipality code (original boundaries) |
| `ags_21` | 8-digit municipality code mapped to 2021 boundaries (`_harm` only) |
| `year` | Calendar year |
| `person_id` | Unique mayor identifier |
| `state` | 2-digit state code |
| `election_year` | Year of the election that started this term |
| `election_date` | Date of the decisive round |
| `term_number` | Term count within (person, municipality) |
| `winner_party` | Party of the mayor (constant within term) |
| `winner_voteshare` | Vote share in the decisive round (constant within term) |
| `winning_margin` | Winner-runner-up margin (constant within term) |
| `n_candidates` | Number of candidates (constant within term) |
| `is_incumbent` | 1 if `term_number >= 2` |
| `next_runs_again` | Whether this person wins the next election |
| `years_since_election` | `year - election_year` |
| `years_to_next_election` | Years until the next election in this municipality (NA if unknown) |
| `electoral_cycle_pos` | Position in the electoral cycle, 0 (election year) to <1 (year before next election) |
| `tenure_start` | Year of first election |
| `term_start_date` | Date of first taking office |
| `candidate_gender` | Mayor's gender (constant within term). NA for Bayern. |
| `candidate_gender_source` | `"raw"` or `"predicted"` |
| `candidate_gender_prob` | Confidence score (0--1) |
| `candidate_gender_method` | Classification method |
| `candidate_migration_bg` | Binary migration background (0/1, constant within term) |
| `candidate_migration_bg_prob` | Probability of migration background (0--1) |
| `candidate_name_origin` | Fine-grained name origin category |
| `candidate_name_origin_conf` | Confidence in origin classification |
| `candidate_name_origin_method` | Detection method for origin classification |

**Coverage**: 185,112 person-years (unharm) / 179,011 (harm), years 1945--2025. Candidate characteristics are forward-filled across the term (constant within each mayor-term).

---

## Stichwahl (Runoff) Coverage

| State | HW Elections | SW Elections | Detection Method |
|---|---|---|---|
| Bayern | 33,297 | 3,174 | `Wahlart` column in raw data (incl. 8/22 March 2026 Kommunalwahl) |
| Sachsen | 1,576 | 300 | `Status` VE/EE + date matching |
| Nordrhein-Westfalen | 1,341 | 331 | 60-day cycle detection |
| Baden-Württemberg | 1,101 | 143 | `Wahlart` H/N/S in the report (126 Neuwahl + 17 Stichwahl) |
| Niedersachsen | 1,000 | 93 | Separate Stichwahl PDFs (2006/2013) + round detection |
| Rheinland-Pfalz | 693 | 227 | `Stichwahltag` column + separate HW/SW results |
| Hessen | 449 | 97 | `Stichwahl` marker + paired HW/SW rows (report + 2026 hessenschau, %-only) |
| Brandenburg | 78 | 38 | Separate `~h_`/`~s_` result pages on the portal |
| Sachsen-Anhalt | 473 | 380 | Hauptwahl + Stichwahl columns on each source row |
| Saarland | 42 | 15 | `Wahlart...3` column in raw data |
| Schleswig-Holstein | 25 | 10 | Scraped with round info |

---

## Coverage by State

| State | Code | Year Range | Unharm Rows | Panel Persons | Panel Elections | Source |
|---|---|---|---|---|---|---|
| Baden-Württemberg | 08 | 2003--2025 (hybrid) | 1,401 | 1,167 | 1,241 | Komm.ONE votemanager (candidate-level) + Stat. Landesamt B VII 3-j/25 (winner-only) |
| Bayern | 09 | 1945--2025 | 34,824 | 12,246 | 31,383 | Excel (Bayerisches Landesamt) |
| Brandenburg | 12 | 2018--2026 (recent cycle) | 116 | 79 | 79 | Web scraping (wahlen.brandenburg.de) |
| Hessen | 06 | 2017--2024 (snapshot at 06.05.2024) | 509 | 421 | 421 | PDF (Stat. Landesamt B VII m Direktwahlen) |
| Sachsen | 14 | 2001--2024 | 2,176 | 447 | 524 | Excel (Buergermeisteratlas) |
| Sachsen-Anhalt | 15 | 1994--2026 | 4,611 | 3,181 | 4,112 | Excel (Stat. Landesamt "BM-Wahl ab 1994") + Dezernat-13 extract |
| Nordrhein-Westfalen | 05 | 2009--2025 | 1,986 | 1,007 | 1,639 | Excel (IT.NRW) |
| Rheinland-Pfalz | 07 | 1994--2025 | 1,147 | 128 | 193 | Excel (Stat. Landesamt) |
| Niedersachsen | 03 | 2006--2025 | 1,186 | 532 | 664 | PDF extraction |
| Saarland | 10 | 2019--2025 | 72 | 57 | 57 | Excel |
| Schleswig-Holstein | 01 | 2023--2025 | 45 | 35 | 35 | Web scraping (wahlen-sh.de) |
| Mecklenburg-Vorpommern | 13 | 2001--2026 | 41 | 21 | 25 | PDF extraction (LAIV-MV Direktwahlen) |
| Thüringen | 16 | 1994--2026 | 3,978 | 631 | 647 | DB scrape (wahlen.thueringen.de, all Gemeinden) + Info/Daten files (kreisfreie-Stadt OB) |

**Note**: Panel person/election counts are from the unharmonized `mayor_panel`. The panel only includes Buergermeisterwahl and Oberbuergermeisterwahl (not Landratswahl, VG/SG-Buergermeisterwahl). Stichwahl-only records are collapsed into the parent election cycle. Bayern dominates with 74.7% of multi-term mayors.

---

## Known Issues and Caveats

**Rheinland-Pfalz has no absolute vote counts.** All count columns are NA; only turnout and vote shares (as percentages) are available. In `mayoral_harm`, these use weighted-mean aggregation instead of weighted-sum.

**Bayern has no candidate names.** The source data provides party and votes per candidate slot but not names. In `mayoral_candidates`, Bayern rows have `candidate_name = NA`.

**Niedersachsen data is PDF-extracted.** Parsing 3 different PDF layouts introduces minor issues: 91 elections where the sum of candidate votes falls below 50% of valid_votes (top candidates listed, not all), and 2 elections with a vote count discrepancy of 8 votes (source data rounding).

**Thüringen has two sources and largely redacted candidate names.** The Bürgermeisterwahlen of all ~596 Gemeinden (1994–2026, hauptamtlich + ehrenamtlich) are scraped from the Thüringer Landesamt für Statistik database (`wahlen.thueringen.de`) by [`code/mayoral_elections/00_th_scrape.py`](../../../code/mayoral_elections/00_th_scrape.py); the 6 kreisfreie-Stadt Oberbürgermeister elections (Erfurt/Gera/Jena/Suhl/Weimar/Eisenach) come from the raw Info/Daten files via [`00_th_mayoral_parse.py`](../../../code/mayoral_elections/00_th_mayoral_parse.py). Per § 50 Abs. 2 ThürKWO the database **redacts candidate personal data**, so for the Gemeinde Bürgermeisterwahlen `candidate_party` holds the *Wahlvorschlag* (party, or an Einzelbewerber/group label such as "Einzelbewerber", "Weitere Personen") and `candidate_name` is populated only where the Wahlvorschlag is itself a person's name. Consequently the `mayor_panel` tracks only the named subset (631 persons); the bulk of Gemeinde winners appear in the election-level datasets but not the person panel. Election dates are exact (Hauptwahl from the result page, Stichwahl from the "Stichwahl am …" block). The OB elections 1994/2000 (from the Daten HTML) are party-level only (no names). A stratified 57-election sample was independently re-verified against the live source (57/57 match).

**Mecklenburg-Vorpommern is PDF-extracted** from 69 LAIV-MV "Direktwahlen" result PDFs (`data/mayoral_elections/raw/mecklenburg_vorpommern/`) by [`code/mayoral_elections/00_mv_parse.py`](../../../code/mayoral_elections/00_mv_parse.py), which writes a candidate-level intermediate (`mv_parsed.csv`) read by stages 01/01b. Coverage is limited to the kreisfreie / große Städte (Oberbürgermeister; Schwerin, Rostock, Stralsund, Greifswald, Neubrandenburg, Wismar) — smaller municipalities' hauptamtliche Bürgermeister are not published centrally by the Landeswahlleiter. AGS assignment is year-aware: the 2011 Kreisgebietsreform merged four kreisfreie Städte into Landkreise, so e.g. the 2001/2008 Greifswald OB elections carry the then-kreisfreie code `13001000` (harmonized to the 2021 municipality `13075039`), while 2015+ elections carry `13075039` directly. One source quirk: in the **Greifswald 2015 Stichwahl** the PDF's reported Wähler (16,342) is internally inconsistent with gültige + ungültige Stimmen (16,432) by 90 votes — reproduced as printed.

**Stichwahl completeness varies.** NRW and Bayern have both candidates in runoff elections. NS 2013 and RLP Stichwahl results list only the winner (1 candidate instead of 2). SH scrapes both Stichwahl candidates. NS only has separate Stichwahl PDFs for 2006 and 2013; other years have no candidate-level Stichwahl data.

**Sachsen year-boundary runoffs.** 2 elections where `election_year != year(election_date)` because the runoff crossed a calendar year (e.g., `election_year = 2008`, `election_date = 2009-01-11`).

**Sachsen "Stichwahl" is not a 2-person runoff.** In Sachsen, when no candidate wins >50% in the first round (VE), a full re-election (EE) is held with all candidates, not a 2-person runoff. This means `n_candidates_sw` can be 3--6 for Sachsen elections.

**Mayor panel: person identification is imperfect.** Bayern uses Amtsantritt dates, which correctly groups >99% of terms but cannot distinguish two different mayors with the same Amtsantritt date in the same municipality (extremely rare). Named states rely on name matching, which misses candidates without names in the data (1,384 elections, mostly Sachsen). 40 Bayern mayors show tenure spans >30 years, which are plausible given 6-year terms since 1945. One Bayern mayor has a 48-year span (10 terms).

**Mayor panel: 176 duplicate-winner elections resolved.** In 169 municipality-years, two different persons were both marked as election winners (HW winner vs SW winner from separate data records). The panel keeps the person with the highest vote share, which correctly identifies the overall winner.

For the full list of known issues including fixed bugs, see `docs/mayoral_elections_known_issues.md`.
