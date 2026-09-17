# Shared recovery for the two mayoral products. Only unresolved rows enter here.
# Accept a historical-code fallback only when every available crosswalk year
# agrees on one target and every weight is exactly one (no split or ambiguity).
gerda_recover_mayoral_mapping <- function(rows, cw) {
  if (!nrow(rows)) return(rows)
  if (!"pop_cw" %in% names(rows)) rows$pop_cw <- rep(NA_real_, nrow(rows))
  rows$crosswalk_year <- rep(NA_real_, nrow(rows))
  rows$mapping_method <- rep(NA_character_, nrow(rows))
  lookup <- rows$ags
  # Same source-code corrections already used by state/county elections.
  lookup[rows$ags == "15159029" & rows$election_year == 1994] <- "15026310" # Merzien
  # The 2018 Eisenach source uses its post-2021 county code retrospectively.
  lookup[rows$ags == "16063105" & rows$election_year == 2018] <- "16056000"
  candidates <- which(substr(rows$ags, 1, 2) == "15" |
                        (rows$ags == "16063105" & rows$election_year == 2018))
  for (i in candidates) {
    available <- cw[cw$ags == lookup[i], , drop = FALSE]
    if (!nrow(available) || anyNA(available$ags_21) ||
        length(unique(available$ags_21)) != 1L ||
        any(!is.finite(available$pop_cw) | abs(available$pop_cw - 1) > 1e-7)) next
    available <- available[order(abs(available$year - rows$election_year[i]), available$year), , drop = FALSE]
    rows$ags_21[i] <- available$ags_21[1]
    rows$pop_cw[i] <- 1
    if ("population" %in% names(rows)) rows$population[i] <- available$population[1]
    if ("ags_name_21" %in% names(rows)) rows$ags_name_21[i] <- available$ags_name_21[1]
    rows$crosswalk_year[i] <- available$year[1]
    rows$mapping_method[i] <- "stable_target_across_all_crosswalk_years"
  }
  rows
}

gerda_mayoral_exceptions <- function() {
  readr::read_csv("code/shared/mayoral_mapping_exclusions.csv",
                 col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
}

# The TH sources list the SAME 2018 Eisenach rounds twice, once as OB under
# the historical code and once as BM under the later code. Verify the complete
# electoral payload against the retained record before excluding either copy.
gerda_exclude_eisenach_duplicates <- function(df) {
  dates <- as.Date(c("2018-04-15", "2018-04-29"))
  duplicate <- which(df$ags == "16063105" & df$election_date %in% dates &
                       df$election_type == "Bürgermeisterwahl")
  fields <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes",
              "winner_party", "winner_votes")
  for (i in duplicate) {
    keep <- which(df$ags == "16056000" & df$election_date == df$election_date[i] &
                    df$round == df$round[i] & df$election_type == "Oberbürgermeisterwahl")
    stopifnot(length(keep) == 1L)
    for (col in fields) stopifnot(identical(df[[col]][i], df[[col]][keep]))
  }
  exceptions <- as.data.frame(df[duplicate, c("ags", "election_date", "round", "election_type")])
  exceptions$reason <- rep("Duplicate Eisenach 2018 round under a retrospective AGS", nrow(exceptions))
  exceptions$evidence <- rep("Identical counts, winner party and winner votes retained under 16056000 on the same date/round", nrow(exceptions))
  gerda_exclude_documented(df, exceptions, c("ags", "election_date", "round", "election_type"),
                         "mayoral_21_duplicates")
}
