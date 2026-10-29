# Rows of the Bayern "Wahlen seit 1945" workbook that repeat another row of the
# same Gemeinde under a different date: all four counts and the winner's votes
# are identical (Gundelsheim's 1996 runoff is its 1972 runoff, Babenhausen's
# 2015 row its 2014 election). Each is pinned with its values in
# metadata/by_source_duplicate_rows.csv and dropped right after reading, so
# 01, 01b and 03 see the same rows. The run stops if a pinned row or the row
# it copies is no longer in the workbook (a corrected source).
drop_by_source_duplicates <- function(raw) {
  pins <- data.table::fread("data/mayoral_elections/metadata/by_source_duplicate_rows.csv",
                            colClasses = "character", encoding = "UTF-8")
  num <- function(x) suppressWarnings(as.numeric(x))
  ags <- sprintf("09%06d", as.integer(raw$Gemeindeschlüssel))
  d <- format(as.Date(raw$`Tag der Wahl`))
  wahlart <- ifelse(is.na(raw$Wahlart), "", as.character(raw$Wahlart))
  counts <- paste(num(raw$Stimmberechtigte), num(raw$Wähler), num(raw$`Gültige Stimmen`),
                  num(raw$`ungültige Stimmen`), num(raw$`gültige Stimmen Wahlgewinner`))
  drop <- integer()
  for (i in seq_len(nrow(pins))) {
    p <- pins[i]
    pc <- paste(num(p$eligible_voters), num(p$number_voters), num(p$valid_votes),
                num(p$invalid_votes), num(p$winner_votes))
    hit <- which(ags == p$ags & d == p$election_date & wahlart == p$wahlart & counts == pc)
    orig <- which(ags == p$ags & d == p$copy_of & counts == pc)
    if (length(hit) != 1L || length(orig) != 1L) {
      stop("Bayern duplicate pin no longer matches the workbook: ", p$ags, " ", p$election_date)
    }
    drop <- c(drop, hit)
  }
  cat("Bayern: dropped", length(drop), "workbook rows that copy another row (by_source_duplicate_rows.csv)\n")
  raw[-drop, ]
}
