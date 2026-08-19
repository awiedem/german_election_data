set.seed(20260730)

source("code/county_elections/parsers/parse_nrw_2025.R")

raw_dir <- "data/county_elections/raw/Kreistagswahlen"
result <- parse_nrw_2025_county_elections(raw_dir)
source_file <- paste0(
  "data/county_elections/raw/local_elections_nrw/",
  "KW25_Stimmbezirke.csv"
)
municipalities <- parse_nrw_2025_municipality_elections(source_file)

stopifnot(
  nrow(result) == 53L,
  nrow(unique(result[c("ags", "election_year")])) == 53L,
  identical(unique(result$state), "05"),
  identical(unique(result$election_year), 2025L),
  identical(unique(result$result_level), "county"),
  identical(unique(result$event_scope), "statewide"),
  all(nchar(result$ags) == 8L),
  all(result$ags == paste0(result$county, "000")),
  all(result$eligible_voters >= result$number_voters),
  all(result$number_voters == result$valid_votes + result$invalid_votes),
  all(result$turnout > 0 & result$turnout <= 1)
)

stopifnot(
  nrow(municipalities) == 374L,
  nrow(unique(municipalities[c("ags", "election_year")])) == 374L,
  length(unique(municipalities$county)) == 31L,
  identical(unique(municipalities$state), "05"),
  identical(unique(municipalities$election_year), 2025L),
  identical(unique(municipalities$result_level), "municipality"),
  identical(unique(municipalities$event_scope), "statewide"),
  sum(municipalities$contest_type == "kreistag") == 364L,
  sum(municipalities$contest_type == "other_county_equivalent") == 10L,
  all(nchar(municipalities$ags) == 8L),
  all(municipalities$county == substr(municipalities$ags, 1L, 5L)),
  all(municipalities$eligible_voters >= municipalities$number_voters),
  all(municipalities$number_voters ==
        municipalities$valid_votes + municipalities$invalid_votes),
  all(municipalities$turnout > 0 & municipalities$turnout <= 1),
  !any(municipalities$source_limitation)
)

contest_counts <- table(result$contest_type)
stopifnot(
  unname(contest_counts[["kreistag"]]) == 30L,
  unname(contest_counts[["kreisfreie_city_council"]]) == 22L,
  unname(contest_counts[["other_county_equivalent"]]) == 1L
)

metadata <- c(
  "ags", "ags_name", "eligible_voters", "number_voters", "invalid_votes",
  "valid_votes", "turnout", "county", "state", "election_year",
  "result_level", "contest_type", "event_scope"
)
party_columns <- setdiff(names(result), metadata)
municipality_metadata <- c(metadata, "source_limitation", "source_note")
municipality_party_columns <- setdiff(names(municipalities), municipality_metadata)
municipality_share_sums <- rowSums(
  municipalities[municipality_party_columns],
  na.rm = TRUE
)
stopifnot(
  length(municipality_party_columns) > 0L,
  all(abs(municipality_share_sums - 1) < 1e-12)
)

county_contests <- result |>
  dplyr::filter(.data$contest_type != "kreisfreie_city_council") |>
  dplyr::arrange(.data$county)
municipality_totals <- municipalities |>
  dplyr::group_by(.data$county) |>
  dplyr::summarise(
    dplyr::across(
      c("eligible_voters", "number_voters", "invalid_votes", "valid_votes"),
      sum
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$county)
stopifnot(
  identical(county_contests$county, municipality_totals$county),
  identical(
    as.data.frame(county_contests[c(
      "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
    )]),
    as.data.frame(municipality_totals[c(
      "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
    )])
  )
)

common_party_columns <- intersect(party_columns, municipality_party_columns)
for (party in common_party_columns) {
  county_counts <- county_contests[[party]] * county_contests$valid_votes
  county_counts[is.na(county_counts)] <- 0
  municipality_counts <- municipalities |>
    dplyr::mutate(count = .data[[party]] * .data$valid_votes) |>
    dplyr::group_by(.data$county) |>
    dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$county) |>
    dplyr::pull(.data$count)
  stopifnot(all(abs(county_counts - municipality_counts) < 1e-8))
}

share_sums <- rowSums(result[party_columns], na.rm = TRUE)
stopifnot(
  length(party_columns) > 0L,
  all(abs(share_sums - 1) < 1e-12)
)

message(
  "NRW 2025 parser test passed: 53 exact county/county-equivalent rows and ",
  "374 municipality contributions, with exact ballot, party, and county ",
  "aggregation checks."
)
