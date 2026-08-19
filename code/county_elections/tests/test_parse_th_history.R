suppressPackageStartupMessages({
  library(dplyr)
})

source("code/county_elections/parsers/parse_th_history.R")

raw_dir <- "data/county_elections/raw/Kreistagswahlen"
result <- parse_th_historical_county_elections(raw_dir)

stopifnot(identical(sort(unique(result$election_year)), c(1990L, 1994L, 1999L)))
stopifnot(
  identical(
    as.integer(table(result$election_year)),
    c(1707L, 1247L, 1019L)
  )
)
stopifnot(!anyDuplicated(result[c("ags", "election_year")]))
stopifnot(all(grepl("^16[0-9]{6}$", result$ags)))
stopifnot(all(result$state == "16"))
stopifnot(all(result$result_level == "municipality"))
stopifnot(all(result$event_scope == "statewide"))
stopifnot(all(result$contest_type %in% c(
  "kreistag", "kreisfreie_city_council"
)))
stopifnot(all(result$source_limitation))

core <- c(
  "ags", "ags_name", "county", "state", "election_year",
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes", "turnout",
  "result_level", "contest_type", "event_scope", "source_limitation",
  "source_note"
)
stopifnot(!anyNA(result[c(
  "ags", "ags_name", "county", "state", "election_year",
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes", "turnout",
  "result_level", "contest_type", "event_scope"
)]))
stopifnot(all(result$number_voters <= result$eligible_voters))
stopifnot(all(result$valid_votes + result$invalid_votes == result$number_voters))
stopifnot(all(result$turnout >= 0 & result$turnout <= 1))

party_columns <- setdiff(names(result), core)
stopifnot(length(party_columns) > 5L)
stopifnot(all(vapply(result[party_columns], is.numeric, logical(1))))
stopifnot(all(vapply(
  result[party_columns],
  function(x) all(is.na(x) | (x >= 0 & x <= 1)),
  logical(1)
)))

year_counts <- result |>
  count(election_year, name = "municipalities") |>
  arrange(election_year)
largest_adjacent_drop <- max(
  -diff(year_counts$municipalities) / head(year_counts$municipalities, -1L)
)
if (largest_adjacent_drop > 0.20) {
  message(
    "Coverage warning: historical boundary consolidation reduces rows from ",
    year_counts$municipalities[which.max(
      -diff(year_counts$municipalities) /
        head(year_counts$municipalities, -1L)
    )],
    " to ",
    year_counts$municipalities[which.max(
      -diff(year_counts$municipalities) /
        head(year_counts$municipalities, -1L)
    ) + 1L],
    " (>20%). This is expected across the 1994 county/municipality reform."
  )
}

message(
  "Thuringia historical parser checks passed: ",
  paste(year_counts$election_year, year_counts$municipalities, sep = "=", collapse = ", ")
)
