# Generated harmonization diagnostics

The harmonizers regenerate CSV files in this directory. They are local build
outputs, excluded from Git; the review and measured changes are recorded in
`docs/harmonization_audit_2026-09-16.md`. Override the destination with R option
`gerda.audit_dir` when running tests or comparing builds.

- `*_source_ledger.csv`: source identity, electoral counts, number of mapping
  edges, total allocated weight and audit status.
- `*_mapping.csv` / `*_provenance.csv`: source-to-target edges and weights;
  state-election provenance also records the original AGS and lookup method/year.
- `*_exclusions.csv` / `*_out_of_scope.csv`: reviewed omissions and reasons.
- `*_failures.csv` / `*_conservation_failures.csv`: diagnostics for failed checks.
  A successful check writes an empty report. Other failure-specific reports can
  remain from older failed runs; use the current run's exit status and timestamps.

Run the relevant harmonizers before `check_harmonization_accounting.R`, which
compares their ledgers to the published state-election files and current input.
The diagnostics contain geographic identifiers and electoral counts, not
candidate names or survey records.
