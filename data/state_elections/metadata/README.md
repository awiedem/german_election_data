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
  A listed value of 0 describes a coverage restriction without classifying the
  recovered counts as corrupt. NRW 1966/1970 was removed after source reconciliation.
  BW 1952 is labelled `municipality_1979_boundaries` in the completeness table.
- `rp_2026_pooled_municipalities.csv`: the 58 Rheinland-Pfalz Ortsgemeinden whose
  2026 Landtagswahl ballots were counted inside a neighbouring Gemeinde of the same
  Verbandsgemeinde (`donor_ags` → `receiver_ags`, with the legal ground as given by
  the source). Donor rows carry NA counts and shares; receiver rows hold the pooled
  unit, electorate included. Written by `01b_state_unharm_raw.R` from the source file.
  The rows involved carry `flag_pooled = 1` in all four state files.
- `st_postal_pool_members.csv`: the 162 pooled postal districts of the Sachsen-Anhalt
  Landtagswahlen 2006 (101), 2011 (27) and 2016 (34), keyed by Kreis code, Wahlbezirk and
  label as printed, with the Gemeinden each serves (`member_ags`). The label names them
  or their Verwaltungs- or Verbandsgemeinde (2011/2016: members then from the workbook's
  own VG column, restricted to the Landtagswahlkreis where the label says "WK n"; 2006:
  from the Gemeindeverzeichnis of 31.12.2005, whose Gemeindeverband key opens the
  Wahlbezirk number (Storkau (Elbe), in VG Tangermünde from 12.01.2006, as of election day), restricted to the pool's Landtagswahlkreis and without the
  Gemeinden that count their own postal votes, built by
  `review/st_postal_pools_2026-10-19/st_postal_pool_members_build_2006.R`); `basis`
  records how, and notes the special cases (Thale's OT Allrode, Petersberg's OT
  Brachstedt in the Landsberg pools, Apenburg-Winterfeld served by the Kalbe pool,
  Halberstadt 2006, which counts its own postal votes and has a pool of its own).
  `shared_ags` (2006 only) lists the 14 Gemeinde parts that no pool of their
  Wahlkreis names; they share every pool of their Wahlkreis and Kreis, their
  electorate split over those pools in proportion to the electorate each names.
  Each member is weighted by its electorate in the pool's Landtagswahlkreis, computed
  from the source. Read by `01b_state_unharm_raw.R`, which stops if a source pool has
  no entry here, an entry matches no pool, or a pool's voters or Wahlkreis differ
  from `pool_voters` / `landtagswahlkreis`. The 1990-2002 pools name only the Kreis
  or the Land and need no entry: 01b sends them to the Kreis's part of their
  Wahlkreis (the whole Wahlkreis where booked on the Land, or where a Kreis of it has
  no pool of its own).
- `postal_allocation_rules.csv`: one row per state and election of `state_unharm`
  saying whether postal votes were pooled in the source (`postal_votes`), and if so
  how GERDA distributed them: `allocation` (the pool level), `weight`, `counts`
  (integer and exact, or fractional), `n_pools`, `pool_voters`, `n_receivers` and
  `n_capped` (receivers whose share was cut to their electorate). Written by
  `01b_state_unharm_raw.R`. "Not in the data" marks the in-person-only elections
  (SH 1983, BB 1990/1994, MV 1990).
- `state_postal_pools.csv`: every pooled postal district distributed, with its
  `pool_id`, source totals (`pool_voters`, `pool_valid_votes`, `pool_invalid_votes`),
  `weight`, `receivers` (AGS, `;`-separated) and `n_capped`. Use it to apply other
  weights.
- `state_postal_receivers.csv`: one row per municipality and election that received
  pooled postal votes (exactly the rows with `flag_postal_allocated` = 1):
  `postal_voters_allocated`, `postal_valid_votes_allocated`, their shares of the
  row's voters and valid votes, and `postal_pool_ids`. Join to the election files on
  `state`, `election_year`, `ags`; the allocated counts are the modelled part of the
  row. Fractional in Thüringen and the Sachsen 2019/2024 lead-Gemeinde pools.
- `state_party_columns.csv`: one row per state, election and party column of
  `state_unharm` that holds a value there (`rows_with_value`, `rows_zero`, statewide
  share weighted by valid votes), plus `other`. A party without a row was not named
  by the source: it did not stand, or it sits in `other` (check `other`'s share).
  Written by `code/checks/build_state_metadata.R`.
- `ni_pooled_municipalities.csv`: the two Niedersachsen Gemeinden (Beierstedt,
  Gevensleben) that have no result of their own in the 2017 Landtagswahl source.
  The source does not name the receiver; Jerxheim is inferred because the
  Samtgemeinde Heeseberg's electorate equals Jerxheim's plus Söllingen's, and
  Jerxheim's electorate nearly doubles against 2013 and 2022. Same treatment and
  `flag_pooled` as the RP file. Written by `01b_state_unharm_raw.R`, which stops if
  the source's empty rows ever differ from these donors.

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
