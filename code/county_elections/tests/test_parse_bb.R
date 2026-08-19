set.seed(20260730)

source("code/county_elections/parsers/parse_bb.R")

raw_dir <- Sys.getenv(
  "BB_RAW_DIR",
  unset = "data/county_elections/raw/Kreistagswahlen/Brandenburg"
)
bb <- parse_bb_county_elections(raw_dir)

metadata_cols <- c(
  "ags", "ags_name", "county", "election_year", "state",
  "eligible_voters", "number_voters", "invalid_votes", "valid_votes",
  "turnout", "result_level", "contest_type", "event_scope",
  "source_limitation", "source_note"
)
party_cols <- setdiff(names(bb), metadata_cols)
party_share_sum <- rowSums(bb[party_cols], na.rm = TRUE)

expected_rows <- c(
  `2003` = 436L,
  `2008` = 420L,
  `2014` = 418L,
  `2019` = 417L,
  `2024` = 413L
)
actual_rows <- table(bb$election_year)

stopifnot(
  identical(as.integer(actual_rows[names(expected_rows)]), unname(expected_rows)),
  anyDuplicated(bb[c("ags", "election_year")]) == 0L,
  all(grepl("^12[0-9]{6}$", bb$ags)),
  all(bb$county == substr(bb$ags, 1L, 5L)),
  all(bb$state == "12"),
  all(bb$result_level == "municipality"),
  all(bb$event_scope == "statewide"),
  all(bb$contest_type %in% c("kreistag", "kreisfreie_city_council")),
  all(bb$eligible_voters > 0),
  all(bb$number_voters >= 0),
  all(bb$valid_votes > 0),
  all(bb$turnout >= 0 & bb$turnout <= 1),
  all(abs(party_share_sum - 1) < 1e-10),
  all(bb$source_limitation[bb$election_year %in% c(2003L, 2008L, 2019L) &
    bb$contest_type == "kreistag"]),
  !any(bb$source_limitation[bb$election_year %in% c(2014L, 2024L)]),
  all(!is.na(bb$source_note[bb$source_limitation])),
  all(is.na(bb$source_note[!bb$source_limitation]))
)

cat("Brandenburg parser checks passed\n")
print(dplyr::count(bb, election_year, source_limitation))
cat(
  "Rows:", nrow(bb),
  "| Columns:", ncol(bb),
  "| Duplicate AGS-years:", anyDuplicated(bb[c("ags", "election_year")]),
  "| Maximum party-share error:", max(abs(party_share_sum - 1)),
  "\n"
)
