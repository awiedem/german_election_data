# Table 6 of the SH 1983 statistical report, pp. 18--117.
# See derived/sh_1983/README.md for transcription and source controls.
gerda_sh_1983_verified <- function(root = here::here()) {
  path <- file.path(root, "data/state_elections/derived/sh_1983/table6_transcription.tsv")
  stopifnot(requireNamespace("digest", quietly = TRUE))
  stopifnot(digest::digest(file = path, algo = "sha256", serialize = FALSE) ==
              "8cd6fe1ebf47a52686fce0f811e7abb5d6488bc3d8a187c9ebe58b2fc62665d2")
  d <- read.delim(path, colClasses = "character", check.names = FALSE)
  counts <- setdiff(names(d), c("ags", "name_reference", "source_page_left", "source_row"))
  stopifnot(nrow(d) == 1128L, !anyDuplicated(d$ags), all(grepl("^010[0-9]{5}$", d$ags)))
  for (column in counts) {
    stopifnot(all(grepl("^[0-9]+$", d[[column]])))
    d[[column]] <- as.numeric(d[[column]])
  }
  parties <- c("cdu", "spd", "fdp", "ssw", "dkp", "dgl", "gruene", "fp", "fsu")
  stopifnot(all(d$eligible_voters == d$without_certificate + d$with_certificate),
            all(d$number_voters == d$valid_votes + d$invalid_votes),
            all(d$valid_votes == rowSums(d[c(parties, "einzelbewerber_llsh")])),
            sum(d$eligible_voters) == 1965881, sum(d$number_voters) == 1506849,
            sum(d$valid_votes) == 1502477)
  # Source p. 4: the final column is independents in counties 01/54, LLSH elsewhere.
  independent <- substr(d$ags, 4, 5) %in% c("01", "54")
  d$einzelbewerber <- ifelse(independent, d$einzelbewerber_llsh, 0)
  d$llsh <- ifelse(independent, 0, d$einzelbewerber_llsh)
  stopifnot(sum(d$einzelbewerber) == 50, sum(d$llsh) == 94)
  result <- data.frame(
    ags = d$ags, election_year = 1983L, state = "01",
    election_date = as.Date("1983-03-13"), eligible_voters = d$eligible_voters,
    number_voters = d$number_voters, invalid_votes = d$invalid_votes,
    valid_votes = d$valid_votes, turnout = NA_real_
  )
  # Municipal counts exclude postal votes. NV/EV is not overall turnout.
  for (party in c(parties, "einzelbewerber", "llsh")) {
    result[[party]] <- d[[party]] / d$valid_votes
  }
  result$other <- 0
  result$cdu_csu <- result$cdu
  result
}
