set.seed(20260730)

source("code/county_elections/parsers/parse_nrw_1994.R")

raw_dir <- "data/county_elections/raw/Kreistagswahlen"
result <- parse_nrw_1994_county_elections(raw_dir)

stopifnot(
  nrow(result) == 54L,
  nrow(unique(result[c("ags", "election_year")])) == 54L,
  identical(unique(result$state), "05"),
  identical(unique(result$election_year), 1994L),
  identical(unique(result$result_level), "county"),
  identical(unique(result$event_scope), "statewide"),
  all(nchar(result$ags) == 8L),
  all(result$ags == paste0(result$county, "000")),
  all(result$eligible_voters >= result$number_voters),
  all(result$number_voters == result$valid_votes + result$invalid_votes),
  all(result$turnout > 0 & result$turnout <= 1),
  all(result$source_limitation),
  length(unique(result$source_note)) == 1L
)

contest_counts <- table(result$contest_type)
stopifnot(
  unname(contest_counts[["kreistag"]]) == 31L,
  unname(contest_counts[["kreisfreie_city_council"]]) == 23L
)

metadata <- c(
  "ags", "ags_name", "county", "state", "election_year",
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
  "turnout", "result_level", "contest_type", "event_scope",
  "source_limitation", "source_note"
)
party_columns <- setdiff(names(result), metadata)
stopifnot(
  identical(
    sort(party_columns),
    sort(c(
      "spd", "cdu", "gruene", "fdp", "rep", "oedp", "statt_partei",
      "deut", "other"
    ))
  ),
  all(abs(rowSums(result[party_columns]) - 1) < 1e-12),
  sum(result$eligible_voters) == 12993928,
  sum(result$number_voters) == 10620423,
  sum(result$invalid_votes) == 161801,
  sum(result$valid_votes) == 10458622,
  abs(sum(result$spd * result$valid_votes) - 4423907) < 1e-8,
  abs(sum(result$cdu * result$valid_votes) - 4217391) < 1e-8
)

message(
  "NRW 1994 parser test passed: 54 unique units (31 Kreise and 23 city ",
  "councils), exact ballot identities, share sums, and published totals."
)
