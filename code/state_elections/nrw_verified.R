# Source-verified county / county-free-city results, including postal votes.
# Synthetic IDs are year-specific row identifiers, not official municipal AGS.
gerda_nrw_verified_results <- function(path =
    "data/state_elections/derived/nrw_1966_1970/nrw_1966_1970_kreis.csv") {
  x <- read.csv(path, colClasses = "character", check.names = FALSE)
  parties <- c("cdu", "spd", "fdp", "zentrum", "uap", "fsu", "dkp", "npd")
  counts <- c("eligible_voters", "number_voters", "invalid_votes", "valid_votes", parties)
  stopifnot(nrow(x) == 185L, setequal(unique(x$election_year), c("1966", "1970")),
            all(x$type %in% c("krfr", "kreis")), all(x$source_row == "a"),
            !anyDuplicated(paste(x$election_year, x$ags)))
  for (field in counts) {
    stopifnot(all(grepl("^[0-9]+$", x[[field]])))
    x[[field]] <- as.numeric(x[[field]])
  }
  stopifnot(all(x$number_voters == x$valid_votes + x$invalid_votes),
            all(x$valid_votes == rowSums(x[parties])),
            all(x$eligible_voters >= x$number_voters), all(x$number_voters > 0))
  dates <- c(`1966` = "1966-07-10", `1970` = "1970-06-14")
  result <- lapply(names(dates), function(year) {
    d <- x[x$election_year == year, ]
    stopifnot(nrow(d) == if (year == "1966") 95L else 90L,
              identical(d$ags, paste0("050", sprintf("%02d", seq_len(nrow(d))), "000")))
    out <- data.frame(ags = d$ags, election_year = as.integer(year), state = "05",
                      election_date = as.Date(dates[[year]]),
                      d[c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")],
                      turnout = d$number_voters / d$eligible_voters)
    for (party in parties) out[[party]] <- d[[party]] / d$valid_votes
    # Keep the established pre-1975 schema for parties absent in these elections.
    for (party in c("dg", "dfu", "gdp", "parteilose", "bdd", "dp", "drp", "dsu",
                    "bhe", "kpd", "rsf", "srp", "csab", "unabhaengige", "rwvp")) {
      out[[party]] <- 0
    }
    out$other <- 0
    out$cdu_csu <- out$cdu
    out
  })
  setNames(result, names(dates))
}
