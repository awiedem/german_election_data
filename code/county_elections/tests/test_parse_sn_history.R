set.seed(20260730)

source("code/county_elections/parsers/parse_sn_history.R")

raw_parent <- "data/county_elections/raw/Kreistagswahlen"
result <- parse_sn_historical_county_elections(raw_parent)

stopifnot(
  identical(sort(unique(result$election_year)), c(1994L, 1995L)),
  nrow(result) == 24L,
  sum(result$election_year == 1994L) == 19L,
  sum(result$election_year == 1995L) == 5L,
  !anyDuplicated(paste(result$ags, result$election_year, sep = "_")),
  all(result$state == "14"),
  all(result$contest_type == "kreistag"),
  all(result$event_scope == "split_reform"),
  all(result$result_level[result$election_year == 1994L] == "county"),
  sum(result$result_level[result$election_year == 1995L] == "county") == 3L,
  sum(result$result_level[result$election_year == 1995L] == "municipality") == 2L,
  all(nchar(result$ags[result$result_level == "county"]) == 8L),
  all(
    result$ags[result$result_level == "county"] ==
      paste0(result$county[result$result_level == "county"], "000")
  ),
  all(nchar(result$ags[result$result_level == "municipality"]) == 8L),
  !anyNA(result$eligible_voters),
  !anyNA(result$number_voters),
  all(is.na(result$valid_votes[result$election_year == 1994L])),
  all(is.na(result$invalid_votes[result$election_year == 1994L])),
  !anyNA(result$valid_votes[result$election_year == 1995L]),
  !anyNA(result$invalid_votes[result$election_year == 1995L]),
  all(
    result$valid_votes[result$election_year == 1995L] +
      result$invalid_votes[result$election_year == 1995L] ==
      result$number_voters[result$election_year == 1995L]
  ),
  all(result$turnout > 0 & result$turnout <= 1)
)

party_columns <- c(
  "cdu", "spd", "pds", "gruene", "fdp", "rep", "dsu",
  "andere_parteien", "waehlervereinigungen", "forum"
)
party_columns <- intersect(party_columns, names(result))
party_sums <- rowSums(result[party_columns], na.rm = TRUE)
stopifnot(all(abs(party_sums - 1) < 1e-10))

cat(
  "Saxony historical parser test passed:",
  nrow(result), "rows;",
  paste(names(table(result$election_year)), as.integer(table(result$election_year)),
        sep = "=", collapse = ", "),
  "\n"
)
