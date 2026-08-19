source("code/county_elections/parsers/parse_mv_2011_muni.R")

raw_dir <- "data/county_elections/raw/Kreistagswahlen"
result <- suppressWarnings(parse_mv_2011_municipality_election(raw_dir))
pools <- attr(result, "postal_pools")
municipality_counts <- attr(result, "municipality_vote_counts")
reconciliation <- attr(result, "county_reconciliation")
diagnostics <- attr(result, "coverage_diagnostics")

stopifnot(
  nrow(result) == 804L,
  nrow(pools) == 78L,
  !anyDuplicated(result$ags),
  !anyDuplicated(pools$ags),
  !any(result$ags %in% pools$ags),
  identical(sort(unique(result$county)), paste0("1307", 1:6)),
  all(result$state == "13"),
  all(result$election_year == 2011L),
  all(result$result_level == "municipality"),
  all(result$contest_type == "kreistag"),
  all(result$event_scope == "split_reform"),
  all(result$source_limitation),
  all(pools$result_level == "administrative_office_postal_pool"),
  all(pools$source_limitation)
)

metadata <- c(
  "ags", "ags_name", "county", "state", "election_year",
  "result_level", "contest_type", "event_scope",
  "eligible_voters", "number_voters", "turnout",
  "invalid_votes", "valid_votes", "source_limitation", "source_note"
)
party_cols <- setdiff(names(result), metadata)
vote_cols <- paste0("vote_count_", party_cols)
stopifnot(
  length(party_cols) == 29L,
  all(vote_cols %in% names(municipality_counts)),
  all(vote_cols %in% names(pools)),
  all(c("cdu", "linke_pds", "spd", "fdp", "gruene") %in% party_cols)
)

# The source's C columns are candidate votes. Each voter may cast up to three,
# so the relevant ballot identity is C + D <= 3B, not C + D = B.
all_counts <- rbind(municipality_counts, pools[names(municipality_counts)])
stopifnot(
  all(municipality_counts$number_voters <=
        municipality_counts$eligible_voters),
  all(all_counts$valid_votes + all_counts$invalid_votes >=
        all_counts$number_voters),
  all(all_counts$valid_votes + all_counts$invalid_votes <=
        3 * all_counts$number_voters),
  all(abs(rowSums(all_counts[vote_cols], na.rm = TRUE) -
            all_counts$valid_votes) < 1e-7)
)

shares <- as.matrix(result[party_cols])
stopifnot(
  !any(shares < 0 | shares > 1, na.rm = TRUE),
  all(abs(rowSums(shares, na.rm = TRUE) - 1) < 1e-7)
)

# Compare municipality plus postal-pool counts against the county output from
# the existing historical parser, including every party and turnout component.
source("code/county_elections/parsers/parse_mv_history.R")
existing <- suppressWarnings(
  .mv_parse_later(
    file.path(
      raw_dir,
      "Mecklenburg-Vorpommern",
      "Mecklenburg-Vorpommern_2011_Kreistagswahl.xls"
    ),
    2011L
  )
)
existing <- existing[order(existing$county), ]
reconciliation <- reconciliation[order(reconciliation$county), ]
stopifnot(
  identical(existing$county, reconciliation$county),
  all(existing$eligible_voters == reconciliation$eligible_voters),
  all(existing$number_voters == reconciliation$number_voters),
  all(existing$invalid_votes == reconciliation$invalid_votes),
  all(existing$valid_votes == reconciliation$valid_votes)
)
for (party in party_cols) {
  expected_counts <- round(existing[[party]] * existing$valid_votes)
  observed_counts <- reconciliation[[paste0("vote_count_", party)]]
  same <- (is.na(expected_counts) & is.na(observed_counts)) |
    (!is.na(expected_counts) & !is.na(observed_counts) &
       expected_counts == observed_counts)
  stopifnot(all(same))
}

stopifnot(
  diagnostics$source_rows == 882L,
  diagnostics$municipality_rows == 804L,
  diagnostics$postal_pool_rows == 78L,
  diagnostics$municipality_valid_votes == 1524412,
  diagnostics$unallocated_postal_valid_votes == 136788,
  abs(diagnostics$municipality_valid_vote_share - 0.9176571) < 1e-7,
  diagnostics$excluded_row_share < 0.2,
  (1 - diagnostics$municipality_valid_vote_share) < 0.2
)

print(diagnostics)
print(reconciliation[c(
  "county", "eligible_voters", "number_voters",
  "invalid_votes", "valid_votes"
)])
cat("MV 2011 municipality recovery parser tests passed.\n")
