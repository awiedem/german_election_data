### Plausibility checks for the municipal council data (municipal_unharm)
# October 2026, after the Hilbig et al. report: two rows had been published for
# years with impossible figures (Brandenburg an der Havel 1993 with 28 valid
# votes per voter, Mittenwalde 1998 with party shares summing to 1.148), and no
# existing check looked at either.
#
# 1. Party shares: the named parties may not add up to more than the valid
#    votes. `other` is the residual and clipped at 0, so an excess shows only
#    in the sum of the named columns.
# 2. Votes per voter: in states where every voter has a fixed number of
#    council votes, valid votes may not exceed that number times the voters.
#    States with cumulative voting (as many votes as seats: Bayern, Baden-
#    Württemberg, Hessen) or a variable number (Schleswig-Holstein) are
#    skipped; Rheinland-Pfalz reports weighted votes, i.e. valid ballots (1).
#    Majority-vote rows (flag_majority_vote == 1) are skipped too: there every
#    voter has as many votes as there are seats. No tolerance: in clean data
#    the cap holds exactly (e.g. Niedersachsen 1981-2021 tops out at 3.000
#    votes per valid ballot), so any excess is a defect.
#
# Calibration: both checks fire on the outputs from before the October 2026
# fixes (the two rows above) and are silent after them. Rows listed in `known`
# are reported as WARN: either verified SOURCE anomalies or rows this check
# found that have not been investigated yet (marked as such). Anything else is
# a FAIL and stops the script.
#
# Usage: Rscript code/checks/checks_muni_plausibility.R [path/to/municipal_unharm.rds]

pacman::p_load(data.table, here)

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) > 0) args[1] else
  here("data", "municipal_elections", "final", "municipal_unharm.rds")
d <- as.data.table(readRDS(path))
cat("checking", path, "-", nrow(d), "rows\n")

parties <- c(
  "cdu_csu", "spd", "linke_pds", "gruene", "afd", "piraten", "fdp",
  "die_partei", "freie_wahler", "bsw", "npd_heimat", "freie_sachsen", "iii_weg"
)
stopifnot(all(parties %in% names(d)))

# votes per voter by state code
max_votes <- c(
  "03" = 3, # Niedersachsen
  "05" = 1, # Nordrhein-Westfalen
  "07" = 1, # Rheinland-Pfalz (weighted votes = ballots)
  "10" = 1, # Saarland
  "11" = 1, # Berlin (Zweitstimmen)
  "12" = 3, # Brandenburg
  "13" = 3, # Mecklenburg-Vorpommern
  "14" = 3, # Sachsen
  "15" = 3, # Sachsen-Anhalt
  "16" = 3 # Thüringen
)

# "ags_year" = "reason" for rows that break a rule because the SOURCE says so.
# The 15 rows this check found on 2026-10-08 (voter counts too low, mostly
# postal voters left out) are corrected or withheld in 01_municipal_unharm.R
# via data/municipal_elections/metadata/voter_count_corrections.csv; a
# withheld count is NA, so those rows drop out of the votes-per-voter test.
known <- c(
  # LDS Brandenburg's own 2008 report prints valid votes at 100.2 % of the
  # maximum for both (pp. 35, 94): part of the postal voters left out of the
  # count. The true count is not in the record.
  "12062240_2008" = "source: postal voters partly missing (Hohenleipisch, 3.005)",
  "12067481_2008" = "source: postal voters partly missing (Storkow (Mark), 3.005)",
  # small excesses found 2026-10-08, not yet investigated
  "13072002_2024" = "not yet investigated (Alt Bukow, 3.004)",
  "15256027_1994" = "not yet investigated (Freyburg (Unstrut), 3.016)"
)

# outputs from before October 2026 have no flag column
if (!"flag_majority_vote" %in% names(d)) d[, flag_majority_vote := NA_integer_]
d[, key := paste(ags, election_year, sep = "_")]
d[, share_sum := rowSums(.SD, na.rm = TRUE), .SDcols = parties]
d[, max_v := max_votes[substr(ags, 1, 2)]]
d[, votes_per_voter := valid_votes / number_voters]

viol <- rbind(
  d[share_sum > 1 + 1e-6,
    .(check = "shares > 1", key, ags_name, value = round(share_sum, 3))],
  d[!is.na(max_v) & !is.na(votes_per_voter) & !(flag_majority_vote %in% 1L) &
      valid_votes > max_v * number_voters + 1e-9,
    .(check = "votes per voter", key, ags_name,
      value = round(votes_per_voter, 2))]
)
viol[, status := ifelse(key %in% names(known), "WARN", "FAIL")]

for (ck in c("shares > 1", "votes per voter")) {
  v <- viol[check == ck]
  n_fail <- sum(v$status == "FAIL")
  cat(sprintf("[%s] %s: %d FAIL, %d WARN\n",
              if (n_fail == 0) "PASS" else "FAIL", ck, n_fail, sum(v$status == "WARN")))
  if (nrow(v) > 0) print(v[order(-status, key)], nrows = 100)
}
if (nrow(viol[status == "WARN"]) > 0) {
  cat("\nWARN reasons:\n")
  for (k in unique(viol[status == "WARN", key])) cat(" ", k, ":", known[[k]], "\n")
}
if (any(viol$status == "FAIL")) stop("municipal plausibility checks failed")
cat("all municipal plausibility checks passed\n")
