# State-election metadata

These tables describe the four current municipality-series RDS exports. They
add no columns to the election files and are not a new package dataset.

- `column_schema.csv`: one row per dataset and column. `party_share` names
  individual source categories; `residual_share` is `other`. Exclude
  `derived_share`, identifiers, counts, turnout, diagnostics and covariates
  from party sums. This is a column schema, not a cross-file party identity map.
- `election_completeness.csv`: actual elections present, reporting geography,
  row counts, nonmissing/zero stored values and sums of nonmissing counts.
  The `_known` suffix means nonmissing in the export, not source-observed. An entirely
  missing field has an empty sum, not zero. A partial sum is not a state total.
  Even complete fields may contain extraction errors; completeness
  does not establish source validity. Unharmonized counts can be fractional
  after postal allocation; harmonized counts are rounded to integers. Missing
  `valid_votes` in harmonization is filled with positive voters, then positive
  electorate, then a unit weight. These proxies count as nonmissing here; in
  particular, Bremen 1991/1995 totals are voter-based weights, not valid votes.
- `source_limitations.csv`: a narrow, manually reviewed warning list. Join on
  `(dataset, state, election_year)` while reading `state` as character. The
  optional `flag_source_unreliable = 1` identifies the listed unresolved
  extractions; absence from this table is **not** a clean bill of health.
  NRW source repairs must update this list only after reconciliation.

Regenerate the first two tables with
`Rscript --vanilla code/checks/build_state_metadata.R`. All three tables refer
to available rows, not to complete historical coverage or complete postal-vote
coverage. See [the data README](../final/README.md) for source limitations.

Example for a downloaded/local `state_unharm`:

```r
schema <- read.csv("data/state_elections/metadata/column_schema.csv")
party_cols <- subset(schema, dataset == "state_unharm" &
                       role %in% c("party_share", "residual_share"))$column
party_data <- state_unharm[party_cols]
known_party_categories <- rowSums(!is.na(party_data))
share_sum_known <- rowSums(party_data, na.rm = TRUE)
share_sum_known[known_party_categories == 0] <- NA_real_
# This is a sum of recorded categories, not proof that every category is known.
# cdu_csu is excluded because it overlaps cdu and csu; never add flags or turnout.
```
