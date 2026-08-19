source("code/county_elections/parsers/parse_mv_history.R")

raw_dir <- "data/county_elections/raw/Kreistagswahlen"
result <- suppressWarnings(parse_mv_historical_county_elections(raw_dir))

expected_years <- c(1990L, 1994L, 1999L, 2004L, 2009L, 2011L)
stopifnot(identical(sort(unique(result$election_year)), expected_years))
stopifnot(!anyDuplicated(paste(result$ags, result$election_year, sep = "-")))
stopifnot(!anyNA(result[c(
  "ags", "county", "state", "election_year", "result_level",
  "contest_type", "event_scope", "eligible_voters", "number_voters",
  "invalid_votes", "valid_votes"
)]))
stopifnot(all(result$turnout >= 0))
# The official 1999 source has one internally inconsistent record:
# AGS 13059050 reports 159 voters and 158 eligible voters. Preserve and flag
# the published values rather than altering or silently dropping the record.
voter_overages <- result[result$number_voters > result$eligible_voters, ]
stopifnot(nrow(voter_overages) == 1L)
stopifnot(
  voter_overages$election_year == 1999L,
  voter_overages$ags == "13059050",
  voter_overages$number_voters == 159,
  voter_overages$eligible_voters == 158
)
stopifnot(all(result$turnout[result$ags != "13059050" | result$election_year != 1999L] <= 1))
stopifnot(all(result$result_level[result$election_year < 2011L] == "municipality"))
stopifnot(all(result$result_level[result$election_year == 2011L] == "county"))
stopifnot(all(result$event_scope[result$election_year == 2011L] == "split_reform"))
stopifnot(!anyNA(result$ags_name[result$election_year == 2011L]))

metadata <- c(
  "ags", "ags_name", "county", "state", "election_year", "result_level",
  "contest_type", "event_scope", "eligible_voters", "number_voters",
  "turnout", "invalid_votes", "valid_votes"
)
party_cols <- setdiff(names(result), metadata)
stopifnot(all(c("cdu", "spd", "fdp", "gruene", "linke_pds", "einzelbewerber") %in%
              party_cols))
shares <- as.matrix(result[party_cols])
stopifnot(!any(shares < 0 | shares > 1, na.rm = TRUE))
positive <- result$valid_votes > 0
stopifnot(all(abs(rowSums(shares, na.rm = TRUE)[positive] - 1) < 1e-7))

counts <- as.data.frame(table(result$election_year), stringsAsFactors = FALSE)
names(counts) <- c("election_year", "output_rows")
counts$election_year <- as.integer(as.character(counts$election_year))
stopifnot(all(counts$output_rows > 0))

diagnostics <- attr(result, "aggregation_diagnostics")
stopifnot(nrow(diagnostics) == length(expected_years))
stopifnot(all(diagnostics$source_rows >= diagnostics$output_rows))
stopifnot(
  diagnostics$unallocated_postal_votes[diagnostics$election_year == 2009L] == 864,
  sum(diagnostics$unallocated_postal_votes) == 864
)

print(counts, row.names = FALSE)
print(diagnostics, row.names = FALSE)
cat("MV historical parser tests passed.\n")
