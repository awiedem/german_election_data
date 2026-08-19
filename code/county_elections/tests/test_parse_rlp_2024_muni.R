set.seed(20260730)

source("code/county_elections/parsers/parse_rlp_2024_muni.R")

source_dir <- paste0(
  "data/county_elections/raw/Kreistagswahlen/",
  "Rheinland-Pfalz/2024_municipality"
)
stopifnot(dir.exists(source_dir))

result <- parse_rlp_2024_municipality(source_dir)

stopifnot(nrow(result) == 2289L)
stopifnot(nrow(unique(result[c("ags", "election_year")])) == nrow(result))
stopifnot(length(unique(result$ags)) == nrow(result))
stopifnot(all(grepl("^07[0-9]{6}$", result$ags)))
stopifnot(all(result$county == substr(result$ags, 1L, 5L)))
stopifnot(length(unique(result$county)) == 24L)
stopifnot(all(result$state == "07"))
stopifnot(all(result$election_year == 2024L))
stopifnot(all(result$result_level == "municipality"))
stopifnot(all(result$contest_type == "kreistag"))
stopifnot(all(result$event_scope == "statewide"))
stopifnot(all(result$source_election_type == "LK_KREISTAGSWAHL"))
stopifnot(all(result$source_granularity == "municipality_partial_of_county_contest"))
stopifnot(!any(result$source_limitation))
stopifnot(!any(result$postal_vote_limitation))
stopifnot(all(result$municipality_coverage == 1))
stopifnot(all(result$eligible_voters > 0))
stopifnot(all(result$number_voters <= result$eligible_voters))
stopifnot(all(result$valid_votes + result$invalid_votes == result$number_voters))
stopifnot(all(result$turnout >= 0 & result$turnout <= 1))

stopifnot(sum(result$eligible_voters) == 2398971)
stopifnot(sum(result$number_voters) == 1565939)
stopifnot(sum(result$valid_votes) == 1525033)
stopifnot(sum(result$invalid_votes) == 40906)
stopifnot(sum(result$raw_valid_candidate_votes) == 66067259)

count_columns <- grep("^vote_count_", names(result), value = TRUE)
stopifnot(length(count_columns) == 12L)
for (count_column in count_columns) {
  party <- sub("^vote_count_", "", count_column)
  stopifnot(party %in% names(result))
  offered <- !is.na(result[[count_column]])
  stopifnot(all(result[[count_column]][offered] >= 0))
  expected_share <- result[[count_column]][offered] / result$valid_votes[offered]
  stopifnot(all(abs(result[[party]][offered] - expected_share) < 1e-12))
}
share_sum <- rowSums(
  result[sub("^vote_count_", "", count_columns)],
  na.rm = TRUE
)
stopifnot(all(abs(share_sum - 1) < 1e-12))

# The JSON source contains the county total, Verbandsgemeinde aggregates, and
# Gemeinde leaves. The parser performs an exact within-county reconciliation;
# repeat the statewide implication here for the published Kreistag-only totals.
stopifnot(sum(result$vote_count_spd, na.rm = TRUE) == 307372)
stopifnot(sum(result$vote_count_cdu, na.rm = TRUE) == 507509)
stopifnot(sum(result$vote_count_gruene, na.rm = TRUE) == 143568)
stopifnot(sum(result$vote_count_afd, na.rm = TRUE) == 215948)
stopifnot(sum(result$vote_count_fdp, na.rm = TRUE) == 69978)

description <- paste(
  readLines(
    file.path(source_dir, "official_results_description.html"),
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)
stopifnot(grepl(
  "Für die Wahlen auf Kreisebene werden auch Teilergebnisse",
  description,
  fixed = TRUE
))
stopifnot(grepl("Ortsgemeindeebene", description, fixed = TRUE))

cat(
  "RLP 2024 municipal Kreistag parser tests passed:",
  nrow(result), "municipalities in", length(unique(result$county)), "Landkreise\n"
)
