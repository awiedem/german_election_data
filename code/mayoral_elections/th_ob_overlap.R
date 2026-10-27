# Thüringen: database rounds that repeat an Oberbürgermeisterwahl
#
# Sourced by 01_mayoral_unharm.R and 01b_mayoral_candidates.R, so both build
# the same set of Thüringen rounds.
#
# The Thüringen database (th_bm_scraped.csv) lists elections under today's
# Gemeinden. Eisenach was kreisfrei until 30.06.2021 and is now a Gemeinde of
# the Wartburgkreis (16063105), so the database repeats its Oberbürgermeisterwahl
# of 15./29.04.2018 as a Bürgermeisterwahl of 16063105. th_ob_parsed.csv already
# holds that election under the code of election day (16056000) with its
# Amtstitel, and the duplicate stood in mayoral_unharm under both codes (data
# audit 2026-10-12, MAY-11).
#
# Drop every database round whose result -- date, round, valid votes and the
# multiset of candidate votes -- equals an OB-file round under another code.
# The rounds dropped are pinned, so a new overlap stops the run and gets looked
# at instead of disappearing unseen.

th_db_ob_repeats_expected <- c("16063105|2018-04-15|hauptwahl",
                               "16063105|2018-04-29|stichwahl")

drop_th_db_ob_repeats <- function(th_raw) {
  is_db <- th_raw$source %in% "wahlen.thueringen.de"
  is_ob <- th_raw$source %in% c("ThLfS Info xlsx", "ThLfS Daten HTML")
  round_key <- paste(th_raw$ags, th_raw$election_date, th_raw$round, sep = "|")

  # One fingerprint per round; NA when a count is missing, so that blank rounds
  # never match each other
  fingerprints <- function(rows) {
    idx <- split(rows, round_key[rows])
    fp <- vapply(idx, function(i) {
      v <- as.numeric(th_raw$candidate_votes[i])
      valid <- as.numeric(th_raw$valid_votes[i[1]])
      if (anyNA(v) || is.na(valid)) return(NA_character_)
      paste(th_raw$election_date[i[1]], th_raw$round[i[1]], valid,
            paste(sort(v), collapse = ","), sep = "|")
    }, character(1))
    data.frame(key = names(idx), ags = sub("\\|.*", "", names(idx)), fp = unname(fp))
  }
  db <- fingerprints(which(is_db))
  ob <- fingerprints(which(is_ob))
  hit <- merge(db[!is.na(db$fp), ], ob[!is.na(ob$fp), ], by = "fp",
               suffixes = c("", "_ob"))
  hit <- hit[hit$ags != hit$ags_ob, ]

  if (!setequal(hit$key, th_db_ob_repeats_expected)) {
    stop("Thüringen: database rounds repeating an OB-file round changed. Expected: ",
         paste(th_db_ob_repeats_expected, collapse = ", "),
         ". Found: ", paste(hit$key, collapse = ", "))
  }
  drop <- is_db & round_key %in% hit$key
  cat("Thüringen: dropped", sum(drop), "database rows that repeat an",
      "Oberbürgermeisterwahl of the OB file under another code:",
      paste(unique(round_key[drop]), collapse = ", "), "\n")
  th_raw[!drop, ]
}
