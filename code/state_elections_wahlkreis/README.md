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
| Stage 0a | `parsers/00_he_pdf_parse.py` | **Hessen 2018 + 2013** out of the B VII 2-4 report PDF (no OCR needed — it has a text layer), cross-checked against the independent B VII 2-1 "Vergleichszahlen" report → `processed/wahlkreis/he_pdf/HE_2018_2013_pdf_long.csv` |
| Stage 0b | `parsers/00_bb_wkr_names.py`, `parsers/00_mv_wkr_names.py` | Wahlkreis **names** for Brandenburg (1990-2024) and Mecklenburg-Vorpommern (1994-2011), whose result workbooks carry only the constituency number → `processed/wahlkreis/wkr_names/<ABBR>_wkr_names.csv` |
| Stage 0 | `parsers/parse_<ABBR>.R` (16 scripts) | Per-state parser: read the machine-readable raw file(s) (plus the Stage-0a/0b intermediates for HE/BB/MV), emit a tidy LONG intermediate `data/state_elections/processed/wahlkreis/<ABBR>_ltw_wkr_long.csv` |
| Stage 1 | `01_ltw_wkr_unharm.R` | Bind all states, normalise party labels, build the unharmonized outputs |
| helper | `_normalise_party.R` | Party-name → snake_case normaliser (copy of the municipality pipeline's, with extra rules: letter-spaced acronyms "C D U"→cdu; "Die Grauen"→graue fix; REP/Grüne-coalition folds) |

Run order:

```bash
python3 code/state_elections_wahlkreis/parsers/00_he_pdf_parse.py     # before parse_HE.R
python3 code/state_elections_wahlkreis/parsers/00_bb_wkr_names.py     # before parse_BB.R
python3 code/state_elections_wahlkreis/parsers/00_mv_wkr_names.py     # before parse_MV.R
Rscript  code/state_elections_wahlkreis/parsers/parse_<ABBR>.R        # each state
Rscript  code/state_elections_wahlkreis/01_ltw_wkr_unharm.R
Rscript  code/state_elections_wahlkreis/99_audit.R
```

The three Stage-0 scripts each validate their own output and refuse to write on
failure, so a broken parse cannot reach the R stages.

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

`flag_wkr_boundaries_recomputed` = 1 where the constituency figures were back-cast onto a
**later** election's Wahlkreiseinteilung and so are not on the boundaries in force on
election day. Hessen 2013 only (106 of its 110 rows); 0 for every other state-year. See the
Hessen note below.

**`wkr_name` is per election year.** States renumber their Wahlkreise, so the same `wkr_nr`
can be a different constituency in a different year (Brandenburg WK 11 = Oranienburg I in
1990, Havelland I in 1994/1999, Uckermark I from 2004). Key on `(state, election_year,
wkr_nr)`; never join across years on the number alone.

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
| Hessen | 2013,2018,2023 |
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

7,827 wide rows · 132,478 long rows · 384 parties · 1990–2026.

### Hessen 2013 + 2018 (added August 2026)

The Hessisches Statistisches Landesamt publishes machine-readable constituency results only
for 2023. Everything earlier exists solely in the printed "Statistische Berichte", of which
exactly one issue is digitised: **B VII 2-4 – 5j/18** (3rd updated edition, August 2024).
That issue's Table 12 reports **both** 2018 and 2013 per Wahlkreis, for Wahlkreisstimmen
(12.1) and Landesstimmen (12.2), and it carries a text layer — so this is a PDF parse, not
an OCR job. Table 15 supplies the six parties that Table 12.2 lumps into "Sonstige" for
2018, so HE 2018 Landesstimmen has no residual at all.

*Parsing.* Coordinate-based, and it must stay that way: German thousands separators in this
report are **spaces**, so a text-order split cannot tell `3 818 88 122` (= 3818, 88122) from
`3, 81888, 122`. Tokens are merged on the horizontal gap (~2 pt inside a number, ≥ 8 pt
between columns) and every merged cell must then land on its column's right edge. Column
*identity* is pinned rather than derived from the stacked, hyphenated headers
("PIRA-/TEN", "MENSCH-/LICHE/WELT") — a naive read of the text layer puts ÖDP where
*Die PARTEI* belongs — and is re-verified on every run against Table 1, which names every
party: the 55 Wahlkreise must sum to Table 1's statewide count exactly, for every party in
all four series. 26 checks in total, all passing.

*Independent validation.* Every 2013 figure is also compared against
**B VII 2-1 – 5j/18 "Vergleichszahlen"**, published six years earlier. The two reports agree
exactly in 53 of 55 Wahlkreise on all shared quantities.

*2013 is on the 2018 boundaries.* The December 2017 LWG amendment re-cut some Wahlkreise
(ten whole municipalities plus Frankfurt's Stadtbezirk 531 Schwanheim moved), and both
reports back-cast 2013 onto the new Einteilung — the genuine 2013-boundary report
(B VII 2-4 – 5j/13) is not digitised anywhere. Hence `flag_wkr_boundaries_recomputed`.
The two reports differ **only** on Frankfurt am Main I (WK 34) and IV (WK 37), and by
exactly the Schwanheim transfer: the 2018 Vergleichszahlen applied it, the 2024 results
report did not. GERDA publishes the B VII 2-4 figures, so those two constituencies stand on
their own 2013 boundaries and are flagged 0; their combined totals are identical in both
documents, which the parser asserts.

### Brandenburg + Mecklenburg-Vorpommern constituency names (added August 2026)

Both states' result workbooks identify the Wahlkreis by number only, so BB was published
with the placeholder `"Landtagswahlkreis NN"` (704 rows) and MV 1994–2011 with a blank name
(360 rows). Names now come from each election's own Statistischer Bericht:

* **BB 2004–2024** — extracted from the report text layer on every run, one unambiguous
  candidate per Wahlkreis required. Two text-layer artefacts are pinned and repaired
  (2004 WK 35 `Frankfurt [Oder])`, 2009 WK 31 `Oder-Spree I V`), keyed on the broken string
  so the override fails loudly if the source is ever re-issued.
* **BB 1990, 1994, 1999** — the reports are image-only scans (1994, 1999) or carry unusable
  OCR (1990: "Baeskow-", "Liibben-", "Cottbus!"), so these lists were read visually off
  high-DPI renderings and are pinned with their source page.
* **MV 2002, 2006, 2011** — section 1.3 "Übersicht über die Wahlkreise" of each year's own
  report (coordinate-based; the *result* tables abbreviate and wrap the long names).
* **MV 1994, 1998** — no name is published anywhere. They take the 2002 names, but only
  after proving they are the same units: every Gemeinde appearing in both years' official
  "nach Gemeinden" workbook must sit in the same Wahlkreis. It does — 960/960, 961/961 and
  1068/1068 with zero disagreements — and the check re-runs on every invocation.

Names are stored **per election year**: BB renumbered with the 1993 Kreisgebietsreform and
again with the Wahlkreisänderungsgesetz of 23 October 1998, and MV renamed WK 21 between
2002 and 2006.

## Validation (every parser + the final build)

- **Statewide reproduction**: summing each parser's Wahlkreise per party reproduces the source file's
  own statewide ("Land"/"000") total row exactly — the key correctness guarantee (all 16 states pass).
- **Per-row integrity**: in every `(state, year, Wahlkreis, stimme)`, Σ party votes = valid_votes
  (0 / 7,607 failures). Wide party shares sum to 1.0 in 7,607 / 7,607 rows.
- turnout ∈ [0.37, 0.84]; 0 duplicates; 0 missing valid_votes/eligible_voters.
- Each parser was checked by an independent verifier agent.

### Audit (raw → final), June 2026
- `99_audit.R` — **59 deterministic internal checks in 21 sections** (schema, types, integrity,
  shares, turnout, coverage, normalization splits, long↔wide, plus §17-20 added August 2026:
  Wahlkreis-name completeness and per-year fixtures, the Hessen 2013/2018 statewide fixtures,
  `flag_wkr_boundaries_recomputed`, and §21 a cross-pipeline roll-up against the
  Gemeinde-level `state_unharm`). **0 FAIL**, 2 WARN, both benign and both explained in the
  script: Berlin 2016 Spandau-2 (a 1.06% blank-Erststimme gap), and §21's broad sweep, where
  118 of 440 state-year × party comparisons differ because the two datasets do not always
  measure the same thing — Bayern's Gemeinde-level files hold *Gesamtstimmen* (ratio ≈ 0.50),
  and BB/ST/NI omit pooled Briefwahl in some years. Hessen, the year added here, reproduces
  the Gemeinde-level pipeline **exactly** in all three years. Re-run after any pipeline change.

  §17-20 were calibrated against the pre-change published file: §17 fires on 1,064 rows
  (704 Brandenburg placeholders + 360 blank Mecklenburg-Vorpommern names), §19 on Hessen
  having only 2023, §20 on the flag column being absent — and all fall silent afterwards.
  A full old-vs-new diff confirmed the change is surgical: 0 rows lost, exactly 220 gained
  (Hessen 2013 + 2018, 55 Wahlkreise × 2 Stimmen each), and on the 7,607 pre-existing rows
  the only column that moved is `wkr_name`, only in Brandenburg and Mecklenburg-Vorpommern.
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
