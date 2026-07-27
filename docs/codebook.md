# GERDA Codebook
2026-07-27

# About this codebook

This codebook documents every variable in every dataset published by the
GERDA project. It is generated from `docs/codebook.qmd`; the PDF
(`docs/codebook.pdf`) and the Markdown version (`docs/codebook.md`) are
both rendered from that single source, so they cannot drift apart.

Datasets are downloadable from
[german-elections.com/election-data](https://www.german-elections.com/election-data/).
Known data-quality caveats per election type are collected in the [usage
notes](https://www.german-elections.com/usage-notes/); this document
describes *what each column is*, while the usage notes describe *what to
watch out for*. The full processing pipeline is documented in
`docs/data_pipeline.md`.

Two conventions keep this document readable. Columns that recur across
many datasets — identifiers, turnout components, harmonization weights,
party columns — are defined once in “Shared conventions” and are not
repeated in every table. Dataset sections then list the columns specific
to that file, and state which shared blocks apply.

# Shared conventions

## Identifiers

| Variable                        | Type      | Description                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:--------------------------------|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ags`                           | character | Municipality identifier (Amtlicher Gemeindeschlüssel), 8-digit character string. Digits 1–2 are the state, 1–5 the county. Leading zeros are significant (Schleswig-Holstein `"01..."`, Hamburg `"02..."`), so the column must always be read as character — reading it as numeric silently breaks joins. In unharmonized files it is the code in force at the time of the election; in harmonized files it is the target-year code. |
| `ags_name`                      | character | Municipality name as carried by the source.                                                                                                                                                                                                                                                                                                                                                                                          |
| `ags_21`, `ags_25`              | character | Municipality identifier mapped to 2021 (or 2025) boundaries.                                                                                                                                                                                                                                                                                                                                                                         |
| `ags_name_21`, `ags_name_25`    | character | Municipality name under the target-year definition.                                                                                                                                                                                                                                                                                                                                                                                  |
| `county`                        | character | County identifier, the first 5 digits of `ags`.                                                                                                                                                                                                                                                                                                                                                                                      |
| `county_code`, `county_code_21` | character | County identifier in county-level files, optionally harmonized to 2021.                                                                                                                                                                                                                                                                                                                                                              |
| `county_name`                   | character | County name.                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `state`                         | character | State identifier, the first 2 digits of `ags` (`"01"` Schleswig-Holstein … `"16"` Thüringen).                                                                                                                                                                                                                                                                                                                                        |
| `state_name`                    | character | State name. English in the federal and European files, German in most others.                                                                                                                                                                                                                                                                                                                                                        |
| `wkr_nr`, `wkr_name`            | character | Constituency (Wahlkreis) number and name.                                                                                                                                                                                                                                                                                                                                                                                            |
| `election_year`                 | numeric   | Year of the election.                                                                                                                                                                                                                                                                                                                                                                                                                |
| `election_date`                 | Date      | Date of the election.                                                                                                                                                                                                                                                                                                                                                                                                                |

## Turnout block

These columns appear, with the same meaning, in every election-result
dataset.

| Variable          | Type    | Description                                                                                                                                                          |
|:------------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `eligible_voters` | numeric | Number of eligible voters (Wahlberechtigte).                                                                                                                         |
| `number_voters`   | numeric | Number of voters (Wähler), including those casting invalid ballots.                                                                                                  |
| `valid_votes`     | numeric | Number of valid votes (gültige Stimmen). In multi-vote systems this counts cast *votes*, not ballots — see “Multi-vote systems” below.                               |
| `invalid_votes`   | numeric | Number of invalid votes (ungültige Stimmen).                                                                                                                         |
| `turnout`         | numeric | `number_voters / eligible_voters`, a proportion in 0–1. Where mail-in allocation pushes the naive ratio above 1 it is capped, and a flag records that it was capped. |

Any of these may be `NA` where the source does not report them; the
dataset sections and the usage notes name the specific state-years
affected.

## Party columns

Party results are stored one column per party, named in snake_case
(`cdu`, `spd`, `gruene`, `linke_pds`, `afd`, `fdp`, `freie_waehler`, …).
Party labels are normalized across states and years by a shared
`normalise_party()` mapping so that the same political party carries the
same column name everywhere.

Values are **vote shares as proportions (0–1)**, not percentages. The
denominator differs by pipeline and is stated in each dataset section —
most use `valid_votes`, the federal and European municipality files use
`number_voters`.

Recurring aggregate columns:

| Variable           | Type    | Description                                                                                                                                               |
|:-------------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `other`            | numeric | Combined share of all parties not carried as their own column. Typically computed as a residual, floored at zero.                                         |
| `cdu_csu`          | numeric | Combined CDU/CSU share. CDU and CSU never compete in the same state, so this is the union of the two and is the column to use for cross-state comparison. |
| `far_right`        | numeric | Combined share of parties classified as far right.                                                                                                        |
| `far_left`         | numeric | Combined share of parties classified as far left, **excluding** Die Linke/PDS.                                                                            |
| `far_left_w_linke` | numeric | As `far_left`, but including Die Linke/PDS.                                                                                                               |
| `total_vote_share` | numeric | Sum of all party shares in the row. A diagnostic: it should be ~1.                                                                                        |
| `waehlergruppen`   | numeric | Combined share of local voter groups (municipal and county elections).                                                                                    |
| `einzelbewerber`   | numeric | Combined share of independent candidates (municipal and county elections).                                                                                |

**Zero versus missing.** How a zero is treated is *not* uniform across
GERDA, because the sources differ. Each dataset section states its rule.
The two patterns are:

- *Zero preserved.* A `0` means the source reported no votes for that
  party. It does not distinguish “stood and won nothing” from “was not
  on the ballot”.
- *Zero recoded to `NA`.* Municipal elections recode `0` to `NA` and
  record the fact in a `replaced_0_with_na_*` flag; state elections
  recode a party that polled zero across a whole state-year. See the
  municipal section for the full reasoning, which applies in spirit to
  both.

In all datasets, `NA` means “no result recorded for this party here” and
should **not** be replaced with 0 before averaging: doing so treats a
party that was never on the ballot as one that was rejected by voters,
biasing means and trends downward.

## Harmonization block

Harmonized datasets map results onto a fixed set of administrative
boundaries (2021, 2023, or 2025) so results are comparable over time,
using population-weighted crosswalks from `data/crosswalks/`.
Municipalities that merged are combined; municipalities that split have
their votes distributed across successors by population weight.

| Variable                        | Type    | Description                                                                                                                                                                                                 |
|:--------------------------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `pop_cw`                        | numeric | Population-based crosswalk weight: the share of the source municipality assigned to this target municipality. Sums to 1 within each source `(ags, year)`.                                                   |
| `area_cw`                       | numeric | Area-based crosswalk weight, defined analogously.                                                                                                                                                           |
| `emp_cw`                        | numeric | Employment-based crosswalk weight.                                                                                                                                                                          |
| `weights`                       | numeric | The weight actually applied to this row during aggregation.                                                                                                                                                 |
| `n_predecessors`                | integer | Number of source municipalities merged into this target boundary.                                                                                                                                           |
| `flag_unsuccessful_naive_merge` | int/num | 1 where the direct crosswalk merge failed and the row was resolved by a fallback (previous-year code, identity mapping, or a manual correction). Not an error marker — a record of how the row was matched. |
| `flag_aggregated`               | integer | 1 where the row is the result of merging several predecessor municipalities.                                                                                                                                |
| `area`, `population`            | numeric | Area (km²) and population (in thousands) of the municipality, from official Gemeindeverzeichnis registers, carried through the crosswalk.                                                                   |

The method used to harmonize votes differs by pipeline and is
deliberate: federal, state, county and European results convert shares
to counts, sum the counts with weights, then recompute shares; municipal
elections use a hybrid (weighted sums for voter counts, weighted means
for party shares) because their denominator is `valid_votes` under
cumulative voting. Percentage columns are never summed directly across
municipalities.

## Multi-vote systems

Several German elections give each voter more than one vote, which
changes what `valid_votes` counts:

- **Municipal and county councils** in most states allow Kumulieren and
  Panaschieren: each voter has as many votes as there are council seats,
  and may cumulate them on one candidate or split them across lists.
  `valid_votes` therefore counts cast individual votes, and
  `valid_votes / number_voters` reflects council size and cumulation
  behaviour, not a ballot count. Party shares remain proportions of cast
  votes and sum to 1 within a municipality; they are comparable across
  municipalities within a state, but not across states with different
  rules (up-to-3 cumulation in Baden-Württemberg versus up-to-5 in
  Hessen and Rheinland-Pfalz). Nordrhein-Westfalen is the main
  exception, with a single list vote.
- **Hamburg and Bremen since 2011** use a 5-vote personalized list
  system, so `valid_votes ~ 5 x number_voters` in state elections.
- **Bayern** state elections count Gesamtstimmen (Erst- plus
  Zweitstimmen), as both ballots count towards seat allocation.

## Flag columns

Flags are diagnostics, never silent corrections: the underlying value is
left as recorded and the flag tells you how to interpret it. Values are
1/0 or `TRUE`/`FALSE`.

| Variable                                           | Description                                                                                                                                             |
|:---------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------|
| `flag_naive_turnout_above_1`                       | The uncapped `number_voters / eligible_voters` exceeded 1, generally a mail-in allocation rounding artifact.                                            |
| `flag_harm_turnout_above_1`                        | As above, arising after harmonization.                                                                                                                  |
| `flag_turnout_above_1`                             | European-elections equivalent; turnout was capped at 1.                                                                                                 |
| `flag_no_valid_votes`                              | The row reports no valid votes.                                                                                                                         |
| `flag_briefwahl_only`                              | A “municipality” that is really a mail-in voting district: `eligible_voters == 0` but `valid_votes > 0`.                                                |
| `flag_briefwahl_agg`                               | A county-level mail-in aggregate row (`ags` ending `999`, `eligible_voters == 0`), present only in 1994 and 1998. Filter these out for balanced panels. |
| `flag_total_votes_incongruent`                     | The summed party votes do not match `valid_votes`.                                                                                                      |
| `flag_other_party_residual`                        | The `other` column was derived as a residual rather than reported.                                                                                      |
| `flag_unsuccessful_naive_merge`, `flag_aggregated` | See the harmonization block above.                                                                                                                      |

# Federal elections

Bundestag results. Municipality level 1980–2025, county level 1953–2025,
constituency level 1990–2025. Vote shares in the municipality- and
county-level files are proportions of **`number_voters`**, following the
original GERDA convention; the Wahlkreis files use `valid_votes`.

## Municipality level

**Files:** `federal_muni_raw`, `federal_muni_unharm`,
`federal_muni_harm_21`, `federal_muni_harm_25` in
`data/federal_elections/municipality_level/final/`.

`federal_muni_raw` (160,313 x 145) is the ingested source data before
standardization. `federal_muni_unharm` (151,793 x 141) is standardized
on each year’s own boundaries. `federal_muni_harm_21` (107,660 x 149)
and `federal_muni_harm_25` (107,295 x 147) are harmonized to 2021 and
2025 boundaries and start in 1990.

Shared blocks apply: identifiers, turnout, party columns (111–125 party
columns depending on file), harmonization, flags.

**Mail-in vote bookkeeping.** Municipalities in shared Briefwahl
districts do not report mail-in votes separately, so the pipeline
allocates them proportionally. These columns record the inputs and the
result of that allocation.

| Variable                                                            | Type      | Description                                                                                                            |
|:--------------------------------------------------------------------|:----------|:-----------------------------------------------------------------------------------------------------------------------|
| `voters_wo_blockingnotice`                                          | numeric   | Eligible voters without Sperrvermerk (source field A1).                                                                |
| `voters_blockingnotice`                                             | numeric   | Eligible voters with Sperrvermerk (A2) — those issued a polling card.                                                  |
| `voters_par25_2`                                                    | numeric   | Voters registered under § 25(2) BWO (A3).                                                                              |
| `voters_w_ballot`                                                   | numeric   | Voters with a Wahlschein, i.e. mail-in and out-of-district voters (B1).                                                |
| `unique_mailin`                                                     | numeric   | 1 where the municipality has its own mail-in district, 0 where it shares one with other municipalities.                |
| `unique_multi_mailin`                                               | numeric   | 1 where the municipality has several mail-in districts of its own.                                                     |
| `voters_weight`                                                     | numeric   | The municipality’s share of its county’s eligible voters, used to allocate shared mail-in votes.                       |
| `blocked_weight`                                                    | numeric   | The municipality’s share of its county’s Sperrvermerk voters, the weight actually applied to mail-in ballots.          |
| `eligible_voters_orig`, `number_voters_orig`, `blocked_voters_orig` | numeric   | The source values before mail-in allocation. Compare against the allocated columns to see what the allocation changed. |
| `turnout_wo_mailin`                                                 | numeric   | `number_voters_orig / eligible_voters_orig` — turnout computed from pre-allocation figures.                            |
| `bwbez`                                                             | character | Briefwahlbezirk identifier (in `federal_muni_raw`).                                                                    |

**Vote-total diagnostics** (harmonized files):

| Variable                       | Type    | Description                                                                                                                             |
|:-------------------------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------|
| `total_votes`                  | numeric | Row sum of all party vote counts.                                                                                                       |
| `total_votes_incogruence`      | numeric | `total_votes - valid_votes`. Note the misspelling of “incongruence” in the column name; it is retained to avoid breaking existing code. |
| `perc_total_votes_incogruence` | numeric | The same discrepancy as a share of `valid_votes`.                                                                                       |
| `flag_total_votes_incongruent` | integer | 1 where the discrepancy is non-zero.                                                                                                    |

`federal_muni_raw` additionally carries `gruene_comb` and
`linke_pds_comb`, which combine the separately reported predecessor
labels (`b90/gr` with `grüne`, and the PDS lineage) into single series.

## County level

**Files:** `federal_cty_unharm` (8,878 x 125), `federal_cty_harm` (4,000
x 127) in `data/federal_elections/county_level/final/`.

Shared blocks apply, with 111 party columns. County-level data reaches
back to 1953, further than the municipality files.

| Variable             | Type      | Description                                                                                                                       |
|:---------------------|:----------|:----------------------------------------------------------------------------------------------------------------------------------|
| `ags`                | character | In `federal_cty_unharm`, the 5-digit county code (the column name is retained for continuity).                                    |
| `county_code`        | character | County identifier in the harmonized file.                                                                                         |
| `year`               | numeric   | Election year in `federal_cty_unharm`, alongside `election_date`.                                                                 |
| `total_votes`        | numeric   | Row sum of party vote counts (harmonized file).                                                                                   |
| `flag_briefwahl_agg` | integer   | 1 for mail-in aggregate rows (`ags` ending `999`, no eligible voters), present in 1994 and 1998 only. Filter for balanced panels. |

## Constituency (Wahlkreis) level

**Files:** `federal_wkr_unharm` (4,186 x 109), `federal_wkr_unharm_long`
(114,702 x 15), `federal_wkr_2021_on_2025` (598 x 56),
`wkr_2021_to_2025_crosswalk` (299 x 11) in
`data/federal_elections/wahlkreis_level/final/`.

Unlike the other federal files, vote shares here are proportions of
`valid_votes`, and results are split by ballot.

| Variable                     | Type      | Description                                                                                                                                                   |
|:-----------------------------|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `stimme`                     | character | Which ballot the row describes: `"erststimme"` (candidate vote) or `"zweitstimme"` (party-list vote). Every constituency-year appears twice, once per ballot. |
| `elected_party`              | character | Party that won the constituency’s direct mandate (Direktmandat). Populated on `erststimme` rows only.                                                         |
| `other`                      | numeric   | Combined share of parties without their own column.                                                                                                           |
| `flag_no_valid_votes`        | integer   | 1 where the row reports no valid votes.                                                                                                                       |
| `flag_naive_turnout_above_1` | integer   | 1 where uncapped turnout exceeded 1.                                                                                                                          |

`federal_wkr_unharm_long` holds the same results in long format — one
row per constituency, ballot and party — which is usually easier to work
with than 93 party columns:

| Variable     | Type      | Description                |
|:-------------|:----------|:---------------------------|
| `party`      | character | Normalized party name.     |
| `votes`      | numeric   | Votes cast for that party. |
| `vote_share` | numeric   | `votes / valid_votes`.     |

`federal_wkr_2021_on_2025` recomputes the 2021 result on the 2025
constituency boundaries, so that the 2021 and 2025 elections can be
compared directly. It carries the identifier, turnout and party blocks
(47 party columns) plus `boundary_change`.

`wkr_2021_to_2025_crosswalk` documents the boundary reform itself:

| Variable                               | Type      | Description                                                                                   |
|:---------------------------------------|:----------|:----------------------------------------------------------------------------------------------|
| `boundary_change`                      | character | `"unchanged"` (283 constituencies), `"redrawn"`, or `"new"`.                                  |
| `renamed`                              | logical   | Whether the constituency name changed even if its boundary did not.                           |
| `prior_2021_wkr_nr`, `prior_2021_name` | character | The 2021 predecessor constituency. `NA` where `boundary_change == "new"`.                     |
| `recomputed_2021_eligible`             | numeric   | 2021 eligible voters recomputed on 2025 boundaries.                                           |
| `actual_2021_eligible`                 | numeric   | Eligible voters as actually reported in 2021.                                                 |
| `eligible_delta`                       | numeric   | Difference between the two. Zero for unchanged constituencies — a check on the recomputation. |

# State elections

Landtagswahlen. Municipality level 1946–2026, constituency level as a
separate set of files. Vote shares are proportions of `valid_votes`.
Every party that ever ran is preserved as its own column (352 party
columns).

## Municipality level

**Files:** `state_unharm` (149,353 x 365), `state_harm_21` (82,689 x
376), `state_harm_23` (82,596 x 376), `state_harm_25` (82,466 x 376) in
`data/state_elections/final/`.

Shared blocks apply. The three harmonized files differ only in target
boundary year and all begin in 1990.

| Variable                                                                         | Type    | Description                                                                                                                                                                                                                        |
|:---------------------------------------------------------------------------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `flag_briefwahl_only`                                                            | integer | 1 where `eligible_voters == 0` and `valid_votes > 0` — a mail-in district rather than a municipality. Chiefly Brandenburg 1990, Schleswig-Holstein 1983, and Nordrhein-Westfalen 1966 major cities.                                |
| `flag_no_valid_votes`                                                            | integer | 1 where the row reports no valid votes.                                                                                                                                                                                            |
| `flag_naive_turnout_above_1`                                                     | integer | 1 where uncapped turnout exceeded 1.                                                                                                                                                                                               |
| `flag_harm_turnout_above_1`                                                      | integer | As above, after harmonization (harmonized files).                                                                                                                                                                                  |
| `flag_other_party_residual`                                                      | integer | 1 where `other` was computed as a residual rather than reported by the source.                                                                                                                                                     |
| `total_vote_share`                                                               | numeric | Sum of all party shares; a diagnostic that should be ~1.                                                                                                                                                                           |
| `einzelbewerber`, `einzelbewerber_1`, `einzelbewerber_2`, `einzelbewerber_innen` | numeric | Independent candidates. The source lists them under several distinct labels which are deliberately not merged, because in some state-years they identify different individuals. Sum them if you want a single independents series. |
| `area_ags`, `population_ags`, `employees_ags`, `pop_density_ags`                 | numeric | Municipality covariates joined in from `ags_area_pop_emp` (harmonized files). See the covariates section.                                                                                                                          |

**Zero-vote recoding.** A party that received zero votes across *all*
municipalities in a state-year is recoded from 0 to `NA`, so that “did
not participate” is distinguishable from “ran and got no votes”. This is
a state-year-wide rule, unlike the per-municipality rule used for
municipal elections.

## Constituency level

**Files:** `ltw_wkr_unharm` (7,607 x 399), `ltw_wkr_unharm_long`
(128,023 x 15) in `data/state_elections/final/`.

Landtagswahl results at constituency level for all 16 states, in the
same wide and long shapes as the federal Wahlkreis files, with `stimme`,
`wkr_nr`, `wkr_name`, `state_abbr`, the turnout block, `other`,
`flag_no_valid_votes` and `flag_naive_turnout_above_1`. The long file
carries `party`, `votes` and `vote_share`.

# Municipal elections

Kommunalwahlen — municipal council (Gemeinderat / Stadtrat) elections.

**Files:** `municipal_unharm` (82,773 x 42, 1984–2026), `municipal_harm`
(71,482 x 40, 1990–2026, 2021 boundaries), `municipal_harm_25` (71,239 x
39, 2025 boundaries) in `data/municipal_elections/final/`.

Unlike the other pipelines, municipal elections carry a fixed set of ten
major parties rather than every party that ever ran. Vote shares are
proportions of `valid_votes`. Municipal elections are not synchronized
nationally — each state sets its own schedule.

Shared blocks apply: identifiers, turnout, harmonization.

| Variable                                                                                              | Type      | Description                                                                                                                                                          |
|:------------------------------------------------------------------------------------------------------|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `election_type`                                                                                       | character | Type of council election.                                                                                                                                            |
| `cdu_csu`, `spd`, `linke_pds`, `gruene`, `afd`, `piraten`, `fdp`, `die_partei`, `freie_wahler`, `bsw` | numeric   | Vote share for each of the ten major parties, as a proportion of `valid_votes`.                                                                                      |
| `other`                                                                                               | numeric   | Combined share of all remaining lists — local voter groups, joint nominations, independents, minor parties. In many small municipalities this is the largest column. |
| `seats_*`                                                                                             | numeric   | Council seats won, ten columns matching the party columns. `municipal_unharm` only — see below.                                                                      |
| `replaced_0_with_na_*`                                                                                | numeric   | Ten flags (1/0) recording zero-to-`NA` recoding — see below.                                                                                                         |

## Zero votes versus no list (`replaced_0_with_na_*`)

Where a source reports exactly 0 votes for one of the ten party columns,
`01_municipal_unharm.R` recodes both the vote count and the vote share
from 0 to `NA` and sets the matching `replaced_0_with_na_<party>` flag
to 1.

A reported 0 almost always means the party **fielded no list** in that
municipality, not that it ran and won no votes. A list on the ballot
virtually always attracts at least a few votes; the affected
municipalities are overwhelmingly small (median ~950 valid votes,
concentrated in Rheinland-Pfalz and Baden-Württemberg); and of the
~105,000 flagged cells in `municipal_unharm` only two record a council
seat for the flagged party. Leaving the 0 in place would bias averages
and time trends downward.

Three cases are therefore distinguishable:

- **non-`NA` value** — the party ran; the value is its vote share.
- **`NA` with flag = 1** — the source reported 0; in practice the party
  did not stand.
- **`NA` with flag = 0** — the party is not carried at all in that
  state-year’s source (for example AfD before 2013, BSW before 2024).

“Party X ran in municipality Y” is thus simply `!is.na(x)`. Do **not**
replace `NA` with 0 before averaging. Note that the underlying sources
do not themselves distinguish “ran and received 0 votes” from “did not
run”, so that distinction cannot be recovered with certainty. The flags
are present in all three municipal files and remain strictly 0/1 after
harmonization.

## Council seats (`seats_*`)

Seat counts are the number of council mandates a party won. They are
carried in `municipal_unharm` **only**: a population-weighted sum of
seats across merged municipalities is not a real council, so the
harmonized files omit them.

Coverage, `NA` elsewhere: Baden-Württemberg 1989–2024, Hessen 1993–2021,
Thüringen 1994–2024, Nordrhein-Westfalen 1994–2025 (kreisfreie Städte
only from 2025), Brandenburg 2003–2024, Rheinland-Pfalz 2004–2019
(excluding kreisfreie Städte), Sachsen-Anhalt 1994–2019,
Mecklenburg-Vorpommern 2019 and 2024, Saarland 2019, Niedersachsen
2011/2016 (ordinary Gemeinden) and 2021 (the eight kreisfreie Städte —
the two sources are complementary, never a complete year),
Schleswig-Holstein 2018, Bremen 1991–2023 and Hamburg 2025. No seat data
for Bayern, Berlin, or Sachsen.

**Party seats do not sum to council size.** Only the ten major parties
have seat columns, while local voter groups, joint nominations and
independents hold a substantial share of German local seats. The row sum
is a lower bound on council size, not the total.

# County elections

Kreistagswahlen — county council elections, 1948–2026, plus a separate
county-council composition panel.

**Files:** `county_elec_unharm` (41,370 x 410),
`county_elec_harm_21_muni` (28,135 x 421), `county_elec_harm_21_cty`
(2,087 x 418), `county_council_seats` (7,200 x 22) in
`data/county_elections/final/`.

Shared blocks apply, with 399 party columns. Results are reported at
municipality level in most states, so the harmonized data comes in two
shapes: `_muni` keeps the municipality as the unit, `_cty` aggregates to
the county. Baden-Württemberg and Bayern publish at county level and are
harmonized with county crosswalks; the other states use municipality
crosswalks.

| Variable                                                         | Type    | Description                                                                                            |
|:-----------------------------------------------------------------|:--------|:-------------------------------------------------------------------------------------------------------|
| `waehlergruppen`                                                 | numeric | Combined share of local voter groups (Wählergruppen), which win a large share of county council seats. |
| `einzelbewerber`                                                 | numeric | Combined share of independent candidates.                                                              |
| `flag_total_votes_incongruent`                                   | integer | 1 where summed party votes do not match `valid_votes`.                                                 |
| `perc_total_votes_incogruence`                                   | numeric | That discrepancy as a share of `valid_votes` (note the retained misspelling).                          |
| `area_ags`, `population_ags`, `employees_ags`, `pop_density_ags` | numeric | Municipality covariates joined in (`_muni` file).                                                      |

Niedersachsen’s three-vote system makes the standard formula invalid, so
`invalid_votes` is `NA` there. Hamburg is excluded (its
Bezirksversammlungswahlen are not comparable), as are pre-digital
PDF-only years (Nordrhein-Westfalen 1946–1994, Saarland 1974–1979).

## County council seats (`county_council_seats`)

Seat distributions in county councils (Kreistage) and the councils of
kreisfreie Städte, as a **yearly panel** covering 2008–2025 (400
counties x 18 years). This is a council-composition panel, not an
election table: a county’s seat distribution is repeated every year
until the next election changes it.

| Variable                                                                                                        | Type      | Description                                                                                                                                                                                                                                                     |
|:----------------------------------------------------------------------------------------------------------------|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `county`                                                                                                        | character | County identifier (5-digit), matching `county` in `county_elec_unharm`.                                                                                                                                                                                         |
| `county_name`                                                                                                   | character | Name of the county or kreisfreie Stadt.                                                                                                                                                                                                                         |
| `county_type`                                                                                                   | character | `"Landkreis"` or `"kreisfreie Stadt"`.                                                                                                                                                                                                                          |
| `state`, `state_name`                                                                                           | character | State identifier and name.                                                                                                                                                                                                                                      |
| `year`                                                                                                          | integer   | Calendar year (2008–2025).                                                                                                                                                                                                                                      |
| `government_party`                                                                                              | character | Party of the county executive (Landrat / Oberbürgermeister); `"parteilos"` = independent. Inferred from the source column `Regierungspartei`, whose exact interpretation is not documented upstream. `NA` for 2023–2025, as the newer sources do not report it. |
| `seats_total`                                                                                                   | integer   | Total council size. `NA` where the source left it blank (39 rows).                                                                                                                                                                                              |
| `seats_spd`, `seats_cdu_csu`, `seats_fdp`, `seats_gruene`, `seats_freie_wahler`, `seats_linke_pds`, `seats_afd` | integer   | Seats won by each major party. Blank in source = 0 seats.                                                                                                                                                                                                       |
| `seats_regional`                                                                                                | integer   | Seats won by regional parties (e.g. SSW). Not comparable across the 2022/2023 boundary — see below.                                                                                                                                                             |
| `seats_other`                                                                                                   | integer   | Seats won by all remaining parties combined. Not comparable across that boundary either.                                                                                                                                                                        |
| `seats_local_other`                                                                                             | integer   | `seats_freie_wahler + seats_regional + seats_other`: everything not held by the six major parties. Defined identically in all years, so this is the column to use for time series.                                                                              |
| `flag_seats_total_incongruent`                                                                                  | logical   | `TRUE` where `seats_total` does not equal the sum of the nine party columns (8 rows). Source discrepancies are kept as recorded.                                                                                                                                |
| `comment`                                                                                                       | character | Free-text note from the source.                                                                                                                                                                                                                                 |
| `source`                                                                                                        | character | Source URL(s) for the row.                                                                                                                                                                                                                                      |
| `last_checked`                                                                                                  | Date      | Date the source entry was last verified.                                                                                                                                                                                                                        |

**The three-way split of non-major-party seats is not comparable over
time.** The hand-compiled 2008–2022 rows often folded Freie Wähler and
local voter groups into `seats_regional`, whereas the parsed 2023–2025
rows assign Freie Wähler to `seats_freie_wahler`, local groups to
`seats_other`, and reserve `seats_regional` for genuine regional
parties. Landkreis Böblingen, for example, shows `seats_regional` 26 and
`seats_freie_wahler` 0 in 2019, then 0 and 24 in 2024, with no real
change in who won the seats. The six major-party columns are consistent
throughout; for anything involving the rest, use `seats_local_other`.

**Boundaries.** The panel uses one fixed set of ~400 current county
codes for every year. Counties created by a reform inside the window
(Städteregion Aachen 2009, the eight Mecklenburg-Vorpommern counties of
2011, merged Landkreis Göttingen 2016) are empty before they existed
rather than backfilled, and the councils those reforms abolished are not
included.

# European elections

European Parliament elections, 2009–2024, at municipality level. Vote
shares are proportions of **`number_voters`**, matching the federal
municipality convention, so party shares sum to roughly
`valid_votes / number_voters` rather than to 1.

**Files:** `european_muni_unharm` (44,722 x 87), `european_muni_harm`
(42,986 x 90) in `data/european_elections/final/`.

Shared blocks apply, with 71 party columns.

| Variable                            | Type    | Description                                                              |
|:------------------------------------|:--------|:-------------------------------------------------------------------------|
| `voters_wo_sperrvermerk`            | numeric | Eligible voters without Sperrvermerk (A1).                               |
| `voters_w_sperrvermerk`             | numeric | Eligible voters with Sperrvermerk (A2) — EU citizens.                    |
| `voters_par24_2`                    | numeric | Voters registered under § 24(2) EuWO (A3) — Germans abroad.              |
| `voters_w_wahlschein`               | numeric | Voters with a Wahlschein (absentee ballot certificate, B1).              |
| `flag_turnout_above_1`              | integer | 1 where turnout exceeded 1 before capping (mail-in allocation rounding). |
| `flag_aggregated`, `n_predecessors` | integer | Harmonization bookkeeping — see the shared block.                        |

Zero handling here follows the *zero preserved* pattern: a 0 means the
source reported no votes, and parties that did not run in a given year
are also 0.

Berlin appears as 14 Bezirke rows per year in the unharmonized file and
as a single row (AGS `11000000`) in the harmonized one. Mail-in votes
from shared districts are allocated proportionally by eligible voters
within each `(county, BWBez)` group. Crosswalk year mapping: 2009→2009,
2014→2014, 2019→2019, 2024→2020.

# Mayoral elections

Direct elections of municipal mayors (Bürgermeister /
Oberbürgermeister), 1945–2026, covering 13 states. Head-of-county
elections are published separately — see the Landrat section.

**Files:** `mayoral_unharm` (55,224 x 17), `mayoral_harm` (51,627 x 23),
`mayoral_candidates` (112,466 x 45), `mayor_panel` (44,822 x 31),
`mayor_panel_harm` (43,718 x 32), `mayor_panel_annual` (265,551 x 27),
`mayor_panel_annual_harm` (259,890 x 28) in
`data/mayoral_elections/final/`.

Hessen is a complete series from the introduction of direct mayoral
elections in 1993 (HSL historical file, obtained on request). Candidate
names are redacted by the statistical office there; names are present
only where the published B VII m snapshot or the 2026 hessenschau
results supply them (2017–2026 winners and 2026 candidates). Hessen
single-candidate elections are Ja/Nein votes: the candidate’s votes can
legitimately fall short of `valid_votes`, whose count includes the Nein
votes.

## Election level (`mayoral_unharm` / `mayoral_harm`)

One row per municipality, election and round.

| Variable           | Type      | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|:-------------------|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `election_type`    | character | `Bürgermeisterwahl`, `Oberbürgermeisterwahl`, `VG-Bürgermeisterwahl` (Verbandsgemeinde), or `SG-Bürgermeisterwahl` (Samtgemeinde).                                                                                                                                                                                                                                                                                                                                                    |
| `round`            | character | `"hauptwahl"` (first round) or `"stichwahl"` (runoff).                                                                                                                                                                                                                                                                                                                                                                                                                                |
| `winner_party`     | character | Party or nominating list of the winner. This is the formal Wahlvorschlagsträger, **not** the winner’s party membership: candidates affiliated with a party frequently run as Einzelbewerber in local elections and are recorded with a blank party. Do not “correct” these against secondary sources.                                                                                                                                                                                 |
| `winner_votes`     | numeric   | Votes for the winner. `NA` where the source reports shares only, or where the winner was not the first-listed Wahlvorschlag in a winner-only source.                                                                                                                                                                                                                                                                                                                                  |
| `winner_voteshare` | numeric   | Winner’s share of valid votes (0–1).                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| `flag_superseded`  | logical   | Bayern only. `TRUE` for a round that did not seat a mayor and was superseded by a later valid round: either annulled (`Wahlart` contains “ungültig”), or a Hauptwahl without an absolute majority that was not resolved by a runoff and was followed by a repeat Hauptwahl within 250 days. Duly-won Hauptwahlen that merely preceded a later by-election are **not** flagged. Rows are kept, not dropped — filter `== FALSE` for decisive rounds only. `FALSE` for all other states. |

`mayoral_harm` adds the harmonization block plus `flag_pre_1990`,
`flag_aggregated`, `flag_turnout_above_1`, `flag_voteshare_above_1` and
`flag_pct_only` (the last marking rows whose source gives percentages
only, so absolute counts are `NA`).

## Candidate level (`mayoral_candidates`)

One row per candidate per election cycle, wide across rounds: Hauptwahl
results carry the `_hw` suffix and Stichwahl results the `_sw` suffix.

| Variable                                                                               | Type      | Description                                                                                                                                                                |
|:---------------------------------------------------------------------------------------|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `election_date`, `election_date_sw`                                                    | Date      | Hauptwahl and Stichwahl dates. `election_date_sw` is `NA` without a runoff.                                                                                                |
| `has_stichwahl`                                                                        | logical   | Whether the cycle went to a runoff.                                                                                                                                        |
| `turnout`, `turnout_sw`                                                                | numeric   | Turnout in each round.                                                                                                                                                     |
| `candidate_name`, `candidate_last_name`, `candidate_first_name`                        | character | Candidate name. `NA` for Bayern (losing candidates are not named in the source) and for Thüringen, where §50 ThürKWO redacts them.                                         |
| `candidate_party`                                                                      | character | Nominating list — as with `winner_party`, the formal Wahlvorschlagsträger.                                                                                                 |
| `candidate_votes_hw`, `candidate_voteshare_hw`, `candidate_rank_hw`, `n_candidates_hw` | numeric   | Hauptwahl votes, share, rank (1 = most votes), and field size.                                                                                                             |
| `candidate_votes_sw`, `candidate_voteshare_sw`, `candidate_rank_sw`, `n_candidates_sw` | numeric   | The same for the Stichwahl; `NA` for candidates not in the runoff.                                                                                                         |
| `is_winner`                                                                            | logical   | Whether this candidate won the cycle (outright in the Hauptwahl or in the runoff). Use this rather than max votes — in winner-only sources the winner is flagged directly. |
| `flag_superseded`                                                                      | logical   | As in `mayoral_unharm`, made constant within each `(ags, election_date, election_type)`.                                                                                   |
| `candidate_birth_year`, `candidate_profession`                                         | num/chr   | Available for some states only.                                                                                                                                            |
| `office_type`                                                                          | character | Office type; Bayern and Saarland only.                                                                                                                                     |

**Predicted candidate characteristics.** Gender and name-origin fields
are *estimates from names*, not verified attributes, and should be
described as such in any published analysis.

| Variable                                                                                                                       | Type      | Description                                                                                                   |
|:-------------------------------------------------------------------------------------------------------------------------------|:----------|:--------------------------------------------------------------------------------------------------------------|
| `candidate_gender`                                                                                                             | character | `"m"` or `"w"`.                                                                                               |
| `candidate_gender_source`                                                                                                      | character | `"raw"` (from the election authority) or `"predicted"`.                                                       |
| `candidate_gender_method`                                                                                                      | character | `raw`, `full_de`, `full_global`, `hyphen_first_de`, `hyphen_first_global`, `accent_norm_global`, or `manual`. |
| `candidate_gender_prob`                                                                                                        | numeric   | Confidence, 0–1: 1.0 raw, 0.99 `full_de`/`manual`, 0.95 `hyphen_first_de`, 0.90 global.                       |
| `candidate_name_origin`                                                                                                        | character | `"german"`, `"turkish"`, `"arabic"`, `"eastern_european"`, `"southern_european"`.                             |
| `candidate_name_origin_conf`                                                                                                   | numeric   | Confidence, 0.50–0.95.                                                                                        |
| `candidate_name_origin_method`                                                                                                 | character | `"combined"`, `"surname_match"`, `"firstname_match"`, `"surname_pattern"`, `"default"`.                       |
| `candidate_migration_bg`                                                                                                       | integer   | 0 = German-origin name, 1 = likely non-German origin.                                                         |
| `candidate_migration_bg_prob`                                                                                                  | numeric   | Probability of migration background, 0–1.                                                                     |
| `candidate_local_surname`, `candidate_surname_county_share`, `candidate_surname_n_counties`, `candidate_surname_overrep_ratio` | num/int   | **Placeholders, all `NA`.** Surname-rootedness measures awaiting telephone directory data.                    |

Gender classification uses the Python `gender-guesser` package; raw
gender from Rheinland-Pfalz, Saarland and Baden-Württemberg takes
precedence over prediction. Cross-validation against raw data: 99.79%
accuracy for Rheinland-Pfalz, 100% for Saarland. Of 107,457 candidate
rows, 24,447 carry a name and 22,905 a gender — the remainder are states
that do not publish losing candidates’ names.

## Mayor panels

`mayor_panel` has one row per person per election (42,836 rows, 20,739
distinct mayors, 1945–2026); `mayor_panel_annual` expands this to one
row per person per year (254,045 rows), forward-filling the term. The
`_harm` variants add `ags_21` and map to 2021 boundaries (41,732 and
248,384 rows).

| Variable                           | Type      | Description                                                                                                  |
|:-----------------------------------|:----------|:-------------------------------------------------------------------------------------------------------------|
| `person_id`                        | character | Unique mayor identifier (e.g. `p_09_00001` for Bayern), linking a person’s terms.                            |
| `term_number`                      | integer   | Sequential term within (person, municipality), starting at 1.                                                |
| `consecutive_terms`                | integer   | Consecutive terms, resetting after a gap of more than one cycle.                                             |
| `winner_party`, `winner_voteshare` | chr/num   | Party and vote share in the decisive round.                                                                  |
| `winning_margin`                   | numeric   | Vote-share gap between winner and runner-up.                                                                 |
| `margin_change`                    | numeric   | Change in that margin since the previous election.                                                           |
| `n_candidates`                     | numeric   | Size of the candidate field.                                                                                 |
| `is_incumbent`                     | integer   | 1 where `term_number >= 2`.                                                                                  |
| `next_runs_again`                  | integer   | 1 if this person wins the next election, 0 if someone else does, `NA` if there is no subsequent election.    |
| `party_switch`                     | integer   | 1 where the winning party changed from the previous election.                                                |
| `is_new_party_mayor`               | integer   | 1 where this party wins in this municipality for the first time.                                             |
| `tenure_start`                     | numeric   | Year of the person’s first election in this municipality.                                                    |
| `years_in_office`                  | numeric   | `election_year - tenure_start`.                                                                              |
| `term_start_date`                  | Date      | Date of first taking office (Bayern: Amtsantritt; elsewhere the first election date).                        |
| `n_terms`, `total_tenure_years`    | int/num   | Total terms observed, and the year span from first to last election.                                         |
| `has_margin_variation`             | logical   | Whether the winning margin varies across this person’s terms — useful for judging fixed-effects feasibility. |

`mayor_panel_annual` replaces the term-summary columns with
position-in-cycle measures:

| Variable                 | Type    | Description                                                                                     |
|:-------------------------|:--------|:------------------------------------------------------------------------------------------------|
| `year`                   | integer | Calendar year.                                                                                  |
| `years_since_election`   | numeric | `year - election_year`.                                                                         |
| `years_to_next_election` | numeric | Years until the next election (`NA` if unknown).                                                |
| `electoral_cycle_pos`    | numeric | Position in the cycle, from 0 in the election year to just under 1 in the year before the next. |

Both panels carry the same predicted-characteristics columns as
`mayoral_candidates`, constant within a term.

# Landrat elections

Direct elections of county executives (Landrat / Landrätin, plus the
heads of Städteregion Aachen and Regionalverband Saarbrücken),
1945–2026, 11 states, 263 counties. Published separately from mayoral
elections.

**Files:** `landrat_unharm` (1,966 x 16), `landrat_candidates` (4,623 x
32) in `data/landrat_elections/final/`. Hessen covers all 21 Landkreise
from 1993 onward (HSL historical file).

Columns are identical to the corresponding mayoral files —
`landrat_unharm` matches `mayoral_unharm` minus `flag_superseded` (which
is Bayern-mayoral only), and `landrat_candidates` matches
`mayoral_candidates` minus `flag_superseded` and the
predicted-characteristics block. `election_type` is `Landratswahl`
throughout, and `ags` is the county’s 8-digit code.

Only Bayern reaches back to the 1950s. This is a matter of electoral law
rather than data availability: most states introduced direct Landrat
elections much later, and Baden-Württemberg has never elected Landräte
directly (its councils choose them).

# Crosswalks

Municipality and county boundary crosswalks, used by every harmonized
dataset and publishable in their own right.

**Files:** `ags_crosswalks` (405,993 x 11), `ags_1990_to_2025_crosswalk`
(451,772 x 9), `cty_crosswalks` (14,165 x 11) in
`data/crosswalks/final/`.

| Variable                                         | Type      | Description                                                                                                                                                                       |
|:-------------------------------------------------|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ags` / `county_code`                            | character | Source municipality or county code, as of `year`. In `ags_crosswalks` this column is stored as numeric; cast it with `as.character()` (and restore leading zeros) before joining. |
| `ags_name` / `county_name`                       | character | Source name.                                                                                                                                                                      |
| `year`                                           | integer   | Year whose boundaries the source code refers to.                                                                                                                                  |
| `ags_21` / `ags_25` / `county_code_21`           | character | Target code under the harmonized boundary definition.                                                                                                                             |
| `ags_name_21` / `ags_name_25` / `county_name_21` | character | Target name.                                                                                                                                                                      |
| `pop_cw`, `area_cw`, `emp_cw`                    | numeric   | Population-, area- and employment-based weights for the source→target mapping. Each sums to 1 within a source `(code, year)`.                                                     |
| `area`, `population`, `employees`                | numeric   | Area (km²), population (thousands) and employees (thousands) of the source unit.                                                                                                  |

A source unit that did not change appears once with weight 1. A
municipality that merged appears once, pointing at its successor with
weight 1. A municipality that split appears once per successor, with
weights summing to 1.

<div>

> **Note**
>
> When chaining crosswalks across several target years, verify that the
> resulting weights still sum to 1 per source `(code, year)`.
> Un-rescaled chained weights silently inflate or deflate harmonized
> vote counts.

</div>

# Covariates

Yearly municipality and county characteristics, harmonized to 2021
boundaries and generated alongside the crosswalks.

**Files:** `ags_area_pop_emp` (351,808 x 7) in
`data/covars_municipality/final/`, `cty_area_pop_emp` (12,800 x 7) in
`data/covars_county/final/`.

| Variable                              | Type      | Description                                                                                    |
|:--------------------------------------|:----------|:-----------------------------------------------------------------------------------------------|
| `ags_21` / `county_code_21`           | character | Municipality or county identifier at 2021 boundaries.                                          |
| `ags_name_21` / `county_name_21`      | character | Name under the 2021 definition.                                                                |
| `year`                                | numeric   | Year of observation.                                                                           |
| `area_ags` / `area_cty`               | numeric   | Area in km², from official Gemeindeverzeichnis files.                                          |
| `population_ags` / `population_cty`   | numeric   | Population in thousands.                                                                       |
| `employees_ags` / `employees_cty`     | numeric   | Employees subject to social-security contributions, in thousands. Available from 1997 onwards. |
| `pop_density_ags` / `pop_density_cty` | numeric   | Population density, derived from the population and area columns.                              |

Shapefiles (VG250 municipality and county boundaries for 2000 and 2021)
are published alongside these under `data/shapefiles/`.

# Known artifacts

Columns that exist in published files but carry no analytic meaning,
listed so that users are not left guessing:

- `total_votes_incogruence` and `perc_total_votes_incogruence` — the
  column names misspell “incongruence”. Retained so existing code keeps
  working.
- `candidate_local_surname`, `candidate_surname_county_share`,
  `candidate_surname_n_counties`, `candidate_surname_overrep_ratio` in
  `mayoral_candidates` — placeholders, uniformly `NA`.

# Work in progress

The database is work in progress. If you have suggestions, comments, or
issues, please email us or file an issue on
[GitHub](https://github.com/awiedem/german_election_data).

# Citation

Please cite the accompanying
[paper](https://www.nature.com/articles/s41597-025-04811-5) when using
this dataset:

Heddesheimer, Vincent, Hanno Hilbig, Florian Sichart, & Andreas
Wiedemann. 2025. *GERDA: German Election Database*. Nature: Scientific
Data, 12: 618.

    @article{Heddesheimer2025GERDA,
       author = {Vincent Heddesheimer and Hanno Hilbig and Florian Sichart and Andreas Wiedemann},
       doi = {10.1038/s41597-025-04811-5},
       issn = {2052-4463},
       issue = {1},
       journal = {Scientific Data},
       month = {4},
       pages = {618},
       title = {GERDA: The German Election Database},
       volume = {12},
       url = {https://www.nature.com/articles/s41597-025-04811-5},
       year = {2025}
    }
