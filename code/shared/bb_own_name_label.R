# Brandenburg: a losing candidate's own-name Wahlvorschlag label
#
# An independent candidate's Wahlvorschlag is often labelled with the
# candidate's own name: "EB Muster", "EWV Muster", "Einzelwahlvorschlag Max
# Muster", or just "Muster, Max" (invented examples). Every Brandenburg loser is
# restricted to the restricted twin (user decision 2026-10-19), and such a label
# would name them all the same, so in the PUBLIC files it becomes the plain
# list type (user decision 2026-10-19, superseding the "as printed" choice of
# 2026-10-16 for the 2004-2013 result reports). The restricted twin keeps the
# label as printed. Elected persons keep theirs.
#
# A label is replaced only if nothing but an independent-candidate prefix is left
# once the person's own surname, given names, initials and titles are removed:
# a list whose name merely contains the surname ("Unser Dorf ...") stays.
#
# Used by 01_mayoral_unharm.R, 01b_mayoral_candidates.R and
# code/landrat_elections/01_landrat_combine.R; 99_audit.R §27n and the Landrat
# audit check the result.

bb_eb_prefixes  <- c("", "eb", "ebw", "einzelbewerber", "einzelbewerberin")
bb_ewv_prefixes <- c("ew", "ewv", "ev", "einzelwahlvorschlag", "einwahlvorschlag", "einzelvorschlag")

bb_generic_own_label <- function(label, last, first) {
  out <- label
  ok <- which(!is.na(label) & !is.na(last) & nzchar(trimws(last)))
  for (k in ok) {
    if (!grepl(last[k], label[k], fixed = TRUE)) next
    rest <- gsub(last[k], " ", label[k], fixed = TRUE)
    given <- if (is.na(first[k])) character(0) else strsplit(first[k], "[[:space:]-]+")[[1]]
    for (t in given[nzchar(given)]) rest <- gsub(t, " ", rest, fixed = TRUE)
    rest <- gsub("\\b(Dr|Prof)\\b\\.?", " ", rest, perl = TRUE)     # titles
    rest <- gsub("\\b[[:upper:]]\\.", " ", rest, perl = TRUE)         # initials
    rest <- tolower(gsub("[^[:alpha:]]+", "", rest))
    if (rest %in% bb_eb_prefixes) {
      out[k] <- "Einzelbewerber"
    } else if (rest %in% bb_ewv_prefixes) {
      out[k] <- "Einzelwahlvorschlag"
    }
  }
  out
}
