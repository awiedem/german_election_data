# Joint lists credited to a party in the harmonised municipal files only.
#
# municipal_unharm keeps every joint list (gemeinsamer Wahlvorschlag /
# Listenvereinigung) in `other`, as the source reports it. A few big-city cases
# where a party's own local list ran as a joint list are credited to that party
# in municipal_harm and municipal_harm_25, so the party's series has no hole
# (decision 2026-10-08). The cases are listed, with their source and a note, in
# data/municipal_elections/metadata/harm_joint_list_credits.csv.
#
# Sourced by 02_municipal_harm.R and 03_municipal_harm_25.R and applied to the
# unharmonised rows before the crosswalk merge, so the credit is harmonised like
# any other vote share.

apply_joint_list_credits <- function(
    df,
    path = "data/municipal_elections/metadata/harm_joint_list_credits.csv") {
  credits <- utils::read.csv(path, colClasses = c(ags = "character"),
                             stringsAsFactors = FALSE, encoding = "UTF-8")
  for (i in seq_len(nrow(credits))) {
    cr <- credits[i, ]
    id <- paste(cr$ags, cr$election_year)
    row <- which(df$ags == cr$ags & df$election_year == cr$election_year)
    if (length(row) != 1) {
      stop("joint-list credit ", id, ": expected one row, found ", length(row))
    }
    # pin: the credit was computed against this valid-vote total
    if (!isTRUE(all.equal(as.numeric(df$valid_votes[row]), cr$valid_votes))) {
      stop("joint-list credit ", id, ": valid_votes changed (", df$valid_votes[row],
           " vs ", cr$valid_votes, "); re-check the credit")
    }
    # a later source fix that fills the column itself must retire the credit
    if (!is.na(df[[cr$party]][row])) {
      stop("joint-list credit ", id, ": ", cr$party, " already has a value")
    }
    share <- cr$votes / cr$valid_votes
    if (df$other[row] < share - 1e-9) {
      stop("joint-list credit ", id, ": `other` is smaller than the credited share")
    }
    df[[cr$party]][row] <- share
    df$other[row] <- df$other[row] - share
    seat_col <- paste0("seats_", cr$party)
    if (seat_col %in% names(df) && !is.na(cr$seats)) df[[seat_col]][row] <- cr$seats
    message("joint-list credit: ", id, " ", cr$source_label, " -> ", cr$party,
            " (", round(100 * share, 2), " %)")
  }
  df
}
