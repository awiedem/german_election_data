# State-election missingness and documentation update, 23 September 2026

This is a prepared, unpublished update. It preserves the existing file names,
column names, vote-share denominators, ballot units and result versions.

## Implemented data repair

`code/state_elections/01b_state_unharm_raw.R` now sums Hessen pre-reform
participation fields with an all-missing-preserving helper. The former final
zero-to-NA rule for HE voters is no longer needed. This preserves unknown
source values without classifying observed zero counts as missing.

The final negative-invalid-vote clamp omits `na.rm = TRUE`. The one known
negative rounding artifact is still clamped to zero; unknown counts stay NA.
The three existing harmonization scripts already preserve all-missing groups
and required no changes.

All four current municipality datasets were rebuilt in CSV, RDS and Excel:

| Dataset | Rows | Columns | Invalid counts restored from zero to NA | Remaining zero invalid counts |
|---|---:|---:|---:|---:|
| `state_unharm` | 149,352 | 365 | 22,295 | 3,134 |
| `state_harm_21` | 82,527 | 376 | 13,861 | 1,494 |
| `state_harm_23` | 82,436 | 376 | 13,861 | 1,485 |
| `state_harm_25` | 82,301 | 376 | 13,855 | 1,472 |

In `state_unharm`, the restored gaps include 20,670 RP 1979–2016 records and
835 HE records (417 in 1958; 418 in 1962). Removing the global clamp's missingness
conversion also restores 790 gaps already represented as NA elsewhere in the
parser: NI 7, HB 27, NRW 87, BW 9, BY 534, BB 89, MV 36 and ST 1.
This does not certify or re-extract those historical sources. The
[election-level change summary](../data/data_checks/state_missingness/change_summary.csv)
and [row-level changes](../data/data_checks/state_missingness/changed_invalid_votes.csv)
identify every affected observation.

All values outside `invalid_votes` match the baseline at numerical tolerance
1e-12; keys, schema and missingness match. Every changed invalid count was
previously zero and is now NA; every remaining known invalid count is unchanged.

## Source verification and regression checks

- Running the unchanged parser reproduced the baseline `state_unharm` exactly.
- HE 1958/1962 was checked by independently reading the original workbook,
  grouping its municipality rows by GKZ, and comparing each final municipality's
  voters and invalid counts. Both elections have nine known city observations.
  Known invalid counts sum to 11,352 and 11,256, respectively; these are partial
  sums, not statewide totals.
- RP 1979–2016 missing participation and invalid counts are preserved. Valid
  totals for all nine elections reconcile with the source's Landesstimmen
  `Gesamtsumme` column.
- MV 1990 workbook columns 13 and 15 explicitly read CSU and DSU. Municipality
  source counts and reconstructed counts agree at 9,663 and 6,499, respectively.
  The [complete official result](https://www.laiv-mv.de/static/LAIV/Wahlen/Dateien/Dokumente/Landtagswahlen/Ergebnisseite/LW%201990%20Erst-Zweitstimmen.pdf)
  also lists them separately. The [municipality source](https://www.laiv-mv.de/Wahlen/Landtagswahlen/1990/Ergebnisse/)
  is in-person only. No party labels or shares were changed.
- `check_state_missingness.R` passes on the rebuilt files and fails on the
  original RDS files at the HE missingness assertion. It also pins the documented
  SH 1983 unresolved counts and BY's complete voters/missing eligible counts.
- All 1,493 CSV/RDS column comparisons passed, including missingness and
  identifiers. The three harmonization scripts completed their existing strict
  crosswalk/reconciliation checks; their existing turnout/source warnings remain.
- Excel verification checked hashes, XML, row/populated-cell counts, styles and
  sampled values, plus **every invalid-vote value or blank** against CSV across
  all four workbooks. See [the Excel report](../data/data_checks/state_missingness/excel_checks.json).
- SHA-256 comparison confirms all 302 raw input files are unchanged after the
  rebuild. [Source hashes](../data/data_checks/state_missingness/source_hashes.csv)
  record that check. Git LFS pointers were hydrated from locally held objects;
  no raw extraction was regenerated and no source file was edited.

Rebuild and check from the repository root, with existing dependencies and
hydrated inputs:

```bash
Rscript --vanilla code/state_elections/01b_state_unharm_raw.R
Rscript --vanilla -e 'library(conflicted); source("code/state_elections/02b_state_harm_21.R")'
Rscript --vanilla -e 'library(conflicted); source("code/state_elections/04_state_harm_23.R")'
Rscript --vanilla -e 'library(conflicted); source("code/state_elections/05_state_harm_25.R")'
Rscript --vanilla code/checks/build_state_metadata.R
Rscript --vanilla code/checks/check_state_missingness.R
python3 code/export_excel.py --only state_unharm state_harm_21 state_harm_23 state_harm_25
python3 code/checks/check_state_excel_missingness.py
quarto render docs/codebook.qmd --to pdf
quarto render docs/codebook.qmd --to gfm
```

Optionally pass a directory of pre-change RDS files to
`check_state_missingness.R` to regenerate the baseline change tables. Baselines
must have the same elections and rows; this comparison is deliberately strict.

## Documentation and quality flags

The source codebook and generated Markdown/PDF, data README, top-level README,
and new metadata tables now distinguish denominator conventions, votes versus
voters, missing versus zero counts, partial aggregates, constituency ballot
selection, non-party/derived columns, party aliases and actual coverage.
The codebook was rendered and its changed pages visually reviewed.

The federal 2021 comparison preserves the original county result (SPD
11,955,434 with the voter denominator) and the constituency result incorporating
the Berlin 2024 repeat (11,901,558 with the valid-vote denominator), verified
against the [original certification](https://www.bundeswahlleiterin.de/en/dam/jcr/5d304be8-7412-4442-972a-e4dfd9e55ce9/20211020_niederschrift_3bwa.pdf)
and [updated official result](https://www.bundeswahlleiterin.de/bundestagswahlen/2021/ergebnisse/bund-99.html).

A new flag inside the wide election files would need coordinated changes to
multiple party-column selectors. Instead,
`data/state_elections/metadata/source_limitations.csv` supplies a narrow,
joinable `flag_source_unreliable` for NRW 1966/1970 and SH 1983. Absence from
this warning list does not imply validation. The legacy `flag_briefwahl_only`
name and values remain compatible, but comments and documentation now explain
that it is a zero-electorate/positive-vote diagnostic, not proof of postal
district status. The generated column schema explicitly separates party shares,
`other`, derived shares, counts, identifiers, diagnostics and covariates.

## Follow-up audit and corrections

The follow-up audit found one functional metadata bug: `ags_name` was classified
as `party_share` in the 2023/2025 schemas. Both rows now have the `identifier`
role and no denominator. The generator rejects nonnumeric share columns, and
the regression checker verifies column coverage, order and municipality-name
roles against each RDS. Election CSV/RDS/XLSX values were not changed by this
audit.

The documentation now also corrects:

- **Imputed denominators:** harmonization can store voter/electorate/unit weights
  in `valid_votes`. Bremen 1991/1995 therefore yields proxy party counts, not
  observed votes. Integer rounding changes its source percentages slightly;
  the former claim of exact preservation was incorrect. The codebook, data
  notes, website and package help now agree on this exception.
- **Completeness:** `_known` counts nonmissing stored values, including proxies.
  Unharmonized allocated counts can be fractional; harmonized counts are rounded.
- **Flags and columns:** state turnout is not capped at one, source flags are
  absent from the harmonized files, and `flag_other_party_residual` tests
  `total_vote_share` outside [0.999, 1.001]. That total excludes `other` and
  derived aggregates. Stale or nonexistent column names were removed.
- **Coverage:** top-level state coverage and final-file row counts now match
  the four current exports. The unsupported claim that several states have
  zero difference from the former API series was removed.

The source checks now require the complete HE municipality key sets (excluding
the two intentionally removed aggregate codes), preventing missing rows from
passing vacuously. All 1,080 MV 1990 invalid counts are compared individually
with the source workbook, including seven source-reported zeros. Isolated
mutation checks confirmed rejection of a zero replaced by NA, a dropped HE
municipality, and the former `ags_name` misclassification.

All 1,493 CSV/RDS column comparisons and the baseline comparison passed again.
A separate elementwise check found zero numeric differences outside
`invalid_votes` in all four datasets.
The Excel checks passed for all four workbooks, including all 396,616 invalid-vote
cells/blanks; all 302 raw input hashes remain unchanged. The codebook was
regenerated and its changed PDF pages visually reviewed. The website built
locally, its draft note remained unpublished, and the package loader's parsed
implementation is identical before and after these documentation edits.

## Cross-repository changes and integration

The website and R-package changes are in isolated local checkouts, on
`codex/missingness-documentation` in each repository. Shared working files were
not edited. Website commits: `dcf6f0d` and audit correction `fc577fb`; package
commits: `fada313` and audit correction `0e1fee6`.
The website changes cover `usage_notes.md`, `election-data.md` and
an update-log draft with `published: false`; its Jekyll build passed (existing
Sass deprecation warnings only), and the draft is absent from the built log.
The package changes cover the loader's roxygen help, regenerated `.Rd`, README,
vignette and a development NEWS entry. Loader implementation and package version
are unchanged. Roxygen/Rd checks pass. The follow-up audit ran 22 local
parameter-validation assertions successfully; three network-dependent tests
were skipped by the suite. The network-dependent schema suite could not download data in the
sandbox: three tests skipped and its rename-notice test failed two assertions
because no successful download occurred. No loader behavior was changed to
address that unrelated test limitation.

The integration coordinator must combine this work with the separately reviewed
NRW and BW changes, then rebuild the shared final files once:

1. **NRW:** retain the missingness helper/clamp while integrating the separate
   source repair. Remove or revise NRW entries in `source_limitations.csv` and
   update the codebook, README, website warnings and release draft only when the
   repair passes source reconciliation. The present warnings describe this
   branch's still-unrepaired values, not the other task's prospective result.
2. **BW 1952:** this branch correctly documents the current start as 1956.
   After validated inclusion, change that to 1952 in the README, codebook and
   website, with the constituent-assembly and Wahlschein denominator caveat
   established by that task. No BW 1952 observations were added here.
3. Regenerate `election_completeness.csv`, `column_schema.csv`, exports and
   codebook after combined data changes. Baseline comparison expectations must
   account explicitly for the new/repaired elections; do not weaken the
   missingness/source assertions. Keep SH 1983 marked unresolved.
4. Coalesce same-day website notes into one entry. Keep it unpublished until the
   combined data is published; only then refresh `_data/downloads.yml` with the
   website's `scripts/update_download_data.py`. Package NEWS can be folded into
   the upstream development section without a loader rewrite or version bump.

No release, merge, push, website deployment or external message was performed.
