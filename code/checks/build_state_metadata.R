# Generate coverage, completeness, and an explicit column schema from final RDS.
# Run after rebuilding the four current state municipality datasets.
source("code/shared/state_missingness.R")
out <- "data/state_elections/metadata"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stems <- c("state_unharm", "state_harm_21", "state_harm_23", "state_harm_25")
counts <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")
identifiers <- c("ags", "county", "state", "state_name", "election_year",
                 "election_date", "ags_name", "ags_name_21", "ags_name_23", "ags_name_25")
derived <- c("cdu_csu", "far_right", "far_left", "far_left_w_linke")
diagnostics <- c("total_vote_share", "perc_total_votes_incongruence",
                 "perc_total_votes_incogruence")
covariates <- c("area_ags", "population_ags", "employees_ags", "pop_density_ags")
schema <- completeness <- list()
for (nm in stems) {
  x <- as.data.frame(readRDS(paste0("data/state_elections/final/", nm, ".rds")))
  cols <- names(x)
  role <- ifelse(cols %in% identifiers, "identifier",
    ifelse(cols %in% counts, "count",
    ifelse(cols == "turnout", "turnout",
    ifelse(grepl("^flag_", cols) | cols %in% diagnostics, "diagnostic",
    ifelse(cols %in% covariates, "covariate",
    ifelse(cols %in% derived, "derived_share",
    ifelse(cols == "other", "residual_share", "party_share")))))))
  share_cols <- cols[role %in% c("party_share", "residual_share", "derived_share")]
  nonnumeric <- share_cols[!vapply(x[share_cols], is.numeric, logical(1))]
  if (length(nonnumeric)) stop("Unclassified nonnumeric columns in ", nm, ": ",
                               paste(nonnumeric, collapse = ", "))
  schema[[nm]] <- data.frame(dataset = nm, column = cols, role = role,
    denominator = ifelse(role %in% c("party_share", "residual_share", "derived_share"),
                         "valid_votes", ifelse(role == "turnout", "eligible_voters", "")))
  for (idx in split(seq_len(nrow(x)), interaction(x$state, x$election_date, drop = TRUE))) {
    d <- x[idx, ]
    z <- data.frame(dataset = nm, state = d$state[1], election_year = d$election_year[1],
                    election_date = d$election_date[1], rows = nrow(d))
    z$geography <- if (nm != "state_unharm") "municipality_harmonized" else
      if (d$state[1] == "05" && d$election_year[1] < 1975) "county_synthetic_ags" else
      if (d$state[1] %in% c("02", "04", "11")) "city_state_municipality" else "municipality"
    for (v in c(counts, "turnout")) {
      z[[paste0(v, "_known")]] <- sum(!is.na(d[[v]]))
      z[[paste0(v, "_zero")]] <- sum(d[[v]] == 0, na.rm = TRUE)
      if (v != "turnout") z[[paste0(v, "_sum_known")]] <- gerda_sum_known(d[[v]])
    }
    completeness[[length(completeness) + 1L]] <- z
  }
}
write.csv(do.call(rbind, schema), file.path(out, "column_schema.csv"),
          row.names = FALSE, na = "")
d <- do.call(rbind, completeness)
d <- d[order(d$dataset, d$state, d$election_date), ]
write.csv(d, file.path(out, "election_completeness.csv"), row.names = FALSE, na = "")
cat("Wrote", nrow(d), "dataset-election coverage/completeness rows and",
    sum(vapply(schema, nrow, integer(1))), "column definitions.\n")
