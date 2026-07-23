# State elections at WAHLKREIS (constituency) level — pipeline

Landtagswahlen (Abgeordnetenhaus in Berlin; Bürgerschaft in Hamburg/Bremen) at the **constituency**
level — the geographic unit below state and below Kreis: **Wahlkreis** in most states, **Stimmkreis**
in Bayern, **Wahlbereich** in Bremen. This is a new geographic level distinct from the existing
Gemeinde-level state pipeline (`code/state_elections/`), analogous to the federal
`municipality_level` / `county_level` split.

## Pipeline

| Stage | Script | Purpose |
|---|---|---|
| Raw grab | (one-off, documented in the raw README) | Download official Wahlkreis result files → `data/state_elections/raw/Landtagswahlen_Wahlkreis/<State>/` |
| Stage 0 | `parsers/parse_<ABBR>.R` (16 scripts) | Per-state parser: read the machine-readable raw file(s), emit a tidy LONG intermediate `data/state_elections/processed/wahlkreis/<ABBR>_ltw_wkr_long.csv` |
| Stage 1 | `01_ltw_wkr_unharm.R` | Bind all states, normalise party labels, build the unharmonized outputs |
| helper | `_normalise_party.R` | Party-name → snake_case normaliser (copy of the municipality pipeline's, with extra rules: letter-spaced acronyms "C D U"→cdu; "Die Grauen"→graue fix; REP/Grüne-coalition folds) |

Run order: `Rscript code/state_elections_wahlkreis/parsers/parse_<ABBR>.R` (each, regenerates its
intermediate) → `Rscript code/state_elections_wahlkreis/01_ltw_wkr_unharm.R`.

## Outputs (`data/state_elections/final/`)

- **`ltw_wkr_unharm_long.{csv,rds}`** — tidy long, one row per `state × year × Wahlkreis × stimme × party`,
  with **absolute vote COUNTS** + `vote_share` + turnout. The count-level source of truth.
- **`ltw_wkr_unharm.{csv,rds}`** — wide, GERDA-style (mirrors `state_unharm`): one row per
  `state × year × Wahlkreis × stimme`; party columns are **vote SHARES** of `valid_votes`.

### Schema (wide)
`flag_no_valid_votes, flag_naive_turnout_above_1` (front) · meta: `state` (2-digit AGS code),
`election_year, election_date, wkr_nr` (character, leading zeros), `wkr_name, stimme,
eligible_voters, number_voters, valid_votes, invalid_votes, turnout` · then sorted party-share
columns · `other` · `cdu_csu`.

`stimme ∈ {erststimme, zweitstimme, einzelstimme}`. **einzelstimme** = single-vote systems:
Baden-Württemberg through 2021 and Saarland. The two-vote split begins when a state introduced a
second vote — note **NW from 2010** and **BW from 2026** (so NW 2000/2005 and BW 2016/2021 are
einzelstimme; later years are erst/zweit). Bayern's constituency is the **Stimmkreis** with both votes.

## Coverage (machine-readable elections; as built June 2026)

| State | Years (Wahlkreis level, machine-readable) |
|---|---|
| Brandenburg | 1990,1994,1999,2004,2009,2014,2019,2024 |
| Berlin | 2016,2023 |
| Baden-Württemberg | 2016,2021,2026 |
| Bayern (Stimmkreis) | 2018,2023 |
| Bremen (Wahlbereich) | 2015,2019 |
| Hessen | 2023 |
| Hamburg | 2008,2011,2015,2020,2025 |
| Mecklenburg-Vorpommern | 1994,1998,2002,2006,2011,2016,2021 |
| Niedersachsen | 1998,2003,2013,2017,2022 |
| Nordrhein-Westfalen | 2000,2005,2010,2012,2017,2022 |
| Rheinland-Pfalz | 2001,2006,2011,2016,2021,2026 |
| Schleswig-Holstein | 2000,2009,2017,2022 |
| Saarland | 2022 |
| Sachsen | 1994,1999,2014,2019,2024 |
| Sachsen-Anhalt | 1990,1994,1998,2002,2006,2011,2016,2021 |
| Thüringen | 1990,1994,1999,2004,2009,2014,2019,2024 |

7,607 wide rows · 128,020 long rows · 388 parties · 1990–2026.

## Validation (every parser + the final build)

- **Statewide reproduction**: summing each parser's Wahlkreise per party reproduces the source file's
  own statewide ("Land"/"000") total row exactly — the key correctness guarantee (all 16 states pass).
- **Per-row integrity**: in every `(state, year, Wahlkreis, stimme)`, Σ party votes = valid_votes
  (0 / 7,607 failures). Wide party shares sum to 1.0 in 7,607 / 7,607 rows.
- turnout ∈ [0.37, 0.84]; 0 duplicates; 0 missing valid_votes/eligible_voters.
- Each parser was checked by an independent verifier agent.

### Audit (raw → final), June 2026
- `99_audit.R` — 28 deterministic internal checks (schema, types, integrity, shares, turnout, coverage,
  normalization splits, long↔wide). **0 FAIL** (1 benign WARN: Berlin 2016 Spandau-2, a 1.06%
  blank-Erststimme gap). Re-run it after any pipeline change.
- An adversarial multi-agent audit independently re-derived every state from raw (not trusting the
  parsers) and compared aggregated statewide shares + Direktmandat winners against **official published
  results** (Landeswahlleiter / Wikipedia) — matched within ≤0.06 pp for all audited states. It found and
  fixed **two party-normalization collisions** caused by states using parties' full legal names:
  *Die PARTEI* ("…Tierschutz, Elitenförderung…") was collapsing into `tierschutz` (Saarland), and
  *Alternative für Deutschland* was falling out of `afd` into a fallback (Saarland) — both fixed in
  `_normalise_party.R` and re-verified against official results.

### Caveat: `cdu_csu` is an ALIAS, not an extra party
`cdu_csu` repeats the Union vote (= `cdu` + `csu`), exactly as in `state_unharm`. Do **not** include it
when summing all party columns (that double-counts the Union → ~1.3). Sum the individual party columns
(excluding `cdu_csu` and `other`); those sum to 1.0.

## Known limitations (clean-data scope)

- **Machine-readable only.** Historical years available only as scanned PDF/TIF are **deferred to a
  future OCR stage**, not parsed here (so no OCR guess-work enters the clean dataset). This is why
  pre-~2000 coverage is thin for several western states (see the raw README's coverage table).
- Specific PDF-only gaps with no clean machine-readable source: **SN** 2004/2009, **SH** 2005/2012,
  **BE** 2021 (CSV retired by the source), **HE** pre-2023, **NW/RP/NI/BW** older years.
- **HH 2015 erststimme** is percentage-only at the source → that one vote/year omitted (zweitstimme present).
- **City-states**: Bremen's unit is the Wahlbereich (Bremen/Bremerhaven), not single-member districts;
  Hamburg's Wahlkreise exist only from 2008.
- Boundaries are each election's **own** Wahlkreis definitions (unharmonized). Wahlkreise are redrawn
  between elections and are NOT comparable across time without harmonization (not attempted — see the
  project notes: clean cross-time harmonization is infeasible without per-year Wahlkreis geometries).
- A few genuinely distinct 1990-East coalition lists (e.g. `buendnis_90` standalone, `b_dkp_kpd`) are
  kept as their own party columns by design.
- **MV 1994–2011 `wkr_name` is empty** (5 elections × 36 Wahlkreise): the constituency is identified by
  `wkr_nr` and all vote/turnout values are validated, but the source XLS labels Wahlkreise by number only
  and MV's 2011 Kreisgebietsreform redrew them, so names cannot be cleanly backfilled from later years.
  Cosmetic (missing label), not a value error.
