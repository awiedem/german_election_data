# Harmonization audit — 16 September 2026

The maintained harmonizers now check their complete input before aggregation.
An unknown AGS cannot disappear through an unmatched-row filter: the run writes
a diagnostic and stops. Reviewed exclusions remain explicit, with bounded keys,
a reason and evidence. Successful runs write source and mapping ledgers.

This addresses the inconsistent municipal/state behaviour and related loss or
duplication risks. It does not establish that every historical boundary or every
source figure is correct. Annual crosswalks cannot resolve all changes within a
year, and several allocations still depend on population-based estimates.

## Why the old checks missed lost observations

The state scripts filtered out unmatched rows before checking weight sums.
Those checks could only examine survivors. Comparing the three harmonized
outputs also missed observations absent from all three. The municipal scripts
stopped on unresolved matches, but had less complete accounting for identity,
backward and fallback mappings. Mayoral scripts allowed exclusions for entire
AGS codes, and the person panel dropped missing targets outright.

The shared checks in `code/shared/harmonization_audit.R` now require:

- Every eligible source identity has a mapping; no invented source identities.
- Every target has the right code format and belongs to the target universe.
- No duplicated source-to-target edge, missing/negative/non-finite weight, or
  source weight sum different from one (tolerance `1e-7`). The federal pipeline's
  existing uninhabited-territory exception requires every available core count
  to be known zero; it never permits missing targets or lost source rows.
- Core source counts and their missingness survive the mapping join unchanged.
- State, European and mayoral aggregated counts conserve totals before rounding.
  State and European checks include the separately named party counts.
- Source keys are present before filters on year can discard missing-key rows.
- Postal allocation and the two Meinungsbild county-to-constituency joins have
  recipients for every source key. Missing counties also stop adjacency creation
  and spatial fitting rather than shrinking the geographic sample.

Fallbacks still operate only on unmatched rows. Equally close crosswalk years
must agree on their complete target/weight vectors; disagreement stops the run.
Distinct routes in composed crosswalks are combined before joining elections;
identical duplicate routes require review. Municipal share averaging is unchanged.

## Confirmed corrections

| Case | Evidence and treatment |
| --- | --- |
| Burg, Sachsen-Anhalt 2011 | The original workbook assigns postal district 994, `Burg, Stadt`, `Brief`, to malformed AGS `15080156`. Correct to `15086015` before district aggregation; guard the identifying fields and retain the raw workbook unchanged. This restores **1,217 valid postal votes** to each harmonized boundary version. |
| Obergeckler source code `07232503` | The RLP historical input uses a later merged code for 1991, 1996, 2001, 2006, 2011 and 2016. Back-allocate its **640 valid votes** to Niedergeckler/Obergeckler for 2021/2023 targets using the existing municipal/county 0.25/0.75 population allocation. This is an estimate, not recovered precinct geography. The 2025 target already handled it. |
| Bavaria 1990 extra-municipal records | Exclude 43 county-plus-`444` records only after verifying that all counts and party results are absent/zero. Previously the valid-vote placeholder manufactured votes that the crosswalk could multiply. Real municipalities with incomplete figures retain the existing imputation method. |
| Municipal names | Keep crosswalk targets as padded character AGS. This restores **5,055** previously missing municipality names in the 2021-boundary file; electoral values are unchanged. |
| Mayoral shared AGS | Preserve three distinct historical elections previously removed by AGS/date/type/round deduplication. The source's `flag_shared_ags` makes municipality name part of the source identity and now reaches the harmonized output. |
| Mayoral crosswalk gaps | Recover 15 Sachsen-Anhalt observations only where all available crosswalk years agree on one target with weight one; reuse the documented Merzien 1994 alias. The person panel uses the same recovery logic. |
| Eisenach 2018 | Two apparent unmatched elections are copies of rounds already recorded under historical AGS `16056000`. Before excluding the retrospective-code copies, assert matching date/round, core counts, winner party and winner votes. A first recovery attempt would have double-counted them; the before/after audit caught this and the regression test now rejects it. |
| European elections, Uder 2024 | Replace rounded handwritten weights summing to 1.0005 with normalized stored predecessor populations. Remove missing-weight-to-one replacement and correct `flag_unsuccessful_naive_merge` to identify an actual failed first lookup. |

## Exclusions that remain visible

- **33 pre-1990 Bavaria mayoral rounds:** exact AGS/date/round/type keys in
  `code/shared/mayoral_mapping_exclusions.csv`. They lack supported historical
  mappings. Later observations for the same AGS are not excluded by this list.
- **Six federal county postal totals:** `(12999,13999,14999,15999)/1994` and
  `(12999,13999)/1998`. They are already flagged `flag_briefwahl_agg` in the source
  and have no county allocation. They remain in the unharmonized file; the
  harmonizer records their counts and exclusion rather than inventing a map.
- **Two empty county-election records:** `03355049/1991` and `06433012/2021`.
  Exclusion requires that the record is empty in the current run. A repaired
  source row survives even if the other historical empty record is still empty.
- **Eisenach's two duplicate rounds:** excluded only after comparison with their
  retained equivalents, as above.
- Existing scope restrictions, including the state crosswalk's 1990 start and
  supra-municipal mayoral offices, remain in place. State and mayoral runs write
  separate scope reports.

## Measured published changes

`harmonization_changes_2026-09-16.csv` records dimensions and core-count totals
against the local files saved before this work. Totals sum known values only;
missing source values have not become observed zeros.

| Output | Rows before → after | Change in summed valid votes |
| --- | ---: | ---: |
| `state_unharm` | 149,353 → 149,352 | 0 |
| `state_harm_21` | 82,689 → 82,527 | +1,666 |
| `state_harm_23` | 82,596 → 82,436 | +1,845 |
| `state_harm_25` | 82,466 → 82,301 | +1,205 |
| `municipal_harm` | 73,535 → 73,535 | 0 |
| `european_muni_harm` | 42,986 → 42,986 | 0 |
| `mayoral_harm` | 52,083 → 52,088 | +6,333 |
| `mayor_panel_harm` | 45,316 → 45,331 | Not a count dataset |
| `mayor_panel_annual_harm` | 281,177 → 281,216 | Not a count dataset |

The state net changes combine recovered votes with removal of artificial
placeholder contributions; they therefore differ from the gross recovered
amounts. The unharmonized Burg row is combined with its real municipality rather
than added again. European valid-vote totals stay constant, while normalized
allocation and rounding change electorate by −1, voters by −2 and invalid votes
by −2 nationally. These are allocation/rounding changes, not dropped sources.

The federal county harmonizer also retains matched counties when turnout inputs
are missing or zero; those inputs cannot justify discarding known vote counts.
The postal-total flag stays in the unharmonized source rather than appearing as
an all-zero/missing incidental output column.

The federal municipality/county, municipal 2025 and county-election rebuilds
preserve their existing final values. All nine changed products have matching
CSV/RDS and regenerated XLSX files. Final object comparisons verify all nine
installed datasets against their latest staged builds and eight unchanged
outputs against the saved baseline, including their schemas. The website update
log has a local entry.

## Repository-wide review and validation

The initial static screen covered **185 tracked code files, 118,977 lines and
1,120 candidate mapping/filter/aggregation sites**. It included R, Python,
JavaScript/TypeScript, shell/config code and the separate Meinungsbild project.
The accompanying JSON inventory records files and hashes. Mapping and dropping
paths identified by the screen were reviewed in context; this is not a claim
that every line or every historical source parser was executed.

- Maintained federal, municipal, county, state, European and mayoral mapping
  paths were reviewed and exercised on local inputs. Full builds were staged
  before installing changed outputs. State ingestion and the separate recent
  state script were also exercised; original raw files were not edited.
- Legacy state and municipal entry points now stop before exporting incomplete
  mappings. An existing invalid R escape and a malformed negative `grep` row
  selector in an archived municipal check were corrected.
- Crosswalk-builder filters for worksheet headers/aggregate rows, constituency
  joins between tables derived from the same source, plotting comparisons,
  validation subsets and explicit analytical sample restrictions were reviewed
  separately. They were not replaced by generic no-filter rules.
- Meinungsbild's county-to-constituency inner joins now check geographic coverage.
  **Real-data verification remains unavailable:**
  `meinungsbild/data/raw/gles/crosswalk/btw_21_to_krs21.csv` is absent locally.
  Missing counties in adjacency construction/spatial fitting also stop the run.
  No model was refitted and no survey records were copied into diagnostics.
- **58 regression/adversarial checks pass:** lost source rows, lost split edges,
  duplicates, malformed/missing targets and keys, invalid weights, changed source
  counts, missingness, ambiguous years, bounded exclusions and raw Burg evidence.
- **Three independent state accounting checks pass:** every output target and
  each of its four core counts are recomputed from recorded contributions, then
  ledger totals are compared with the full eligible input. Deliberately removing
  Burg's 1,217 votes or deleting its output row makes this check fail.
- Existing state QA: **46 PASS, 25 WARNING, 0 FAIL**. Its obsolete coverage
  assumptions and false party-column classifications were corrected. Missing
  RLP electorate is not treated as zero; named minor parties count toward share
  totals; the original MV 1990 workbook's separate CSU label is retained; turnout
  over one requires the existing flag. A failed check now returns a failing exit
  status. Remaining warnings are reported, not treated as proof of correctness.
- Existing mayoral `99_audit.R`: all checks pass, zero warnings.
  `98_full_audit.R`: **0 ERROR, 7 WARN** concerning existing source/result issues.
- **126 R files parse; 46 Python files parse.** This does not test every parser's
  behaviour or execute the separate website/model projects.
- Excel exporter unit checks pass. **All nine changed workbooks pass** complete
  XML/row/cell/hash and sampled-value validation, covering **891,772 rows**.
  At the initial audit, the full-repository check found 31 pre-existing RDS hash
  mismatches following the earlier compression commits. The publication check
  on **17 September 2026** refreshed the full export collection and manifest:
  **all 47 workbooks pass**, covering **4,780,100 rows**, with no remaining hash
  discrepancies. Staged Git LFS objects also match the manifest.

## Reproduce the checks

From the repository root, after rebuilding the relevant harmonizers:

```sh
Rscript --vanilla code/checks/test_harmonization_audit.R
Rscript --vanilla code/checks/check_harmonization_accounting.R
Rscript --vanilla code/checks/checks_state_harm.R
Rscript --vanilla code/mayoral_elections/99_audit.R
Rscript --vanilla code/mayoral_elections/98_full_audit.R
python3 code/checks/test_excel_export.py
```

CSV diagnostics regenerate under `data/data_checks/harmonization/` and are
ignored by Git. Failure reports include original identifiers and counts where
available. State provenance records original AGS, lookup year/method and weights.
The checks run before final writes; an unresolved mapping stops the new build
and leaves the previous final files in place.

Annual nearest-year and identity fallbacks remain documented heuristics.
Unambiguous weight accounting prevents loss or multiplication of votes; it
cannot prove the historical geography was correct. Further improvements would
require dated boundary events or additional source evidence, especially for
multiple AGS changes within one year and pre-1990 municipalities.
