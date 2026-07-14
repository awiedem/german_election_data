#### Bundestagswahlen at WAHLKREIS level — audit ####
## Deterministic checks over the four final outputs. Re-run after any pipeline
## change: Rscript code/federal_elections_wahlkreis/99_audit.R
## Exits non-zero on the first failure.

for (pkg in c("here", "data.table")) suppressMessages(library(pkg, character.only = TRUE))
here::i_am("code/federal_elections_wahlkreis/99_audit.R")
fin <- here("data", "federal_elections", "wahlkreis_level", "final")

pass <- 0L
check <- function(desc, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", desc)) }
  else stop(sprintf("FAIL: %s", desc), call. = FALSE)
}

wide <- as.data.table(readRDS(file.path(fin, "federal_wkr_unharm.rds")))
long <- as.data.table(readRDS(file.path(fin, "federal_wkr_unharm_long.rds")))
cw   <- as.data.table(readRDS(file.path(fin, "wkr_2021_to_2025_crosswalk.rds")))
rec  <- as.data.table(readRDS(file.path(fin, "federal_wkr_2021_on_2025.rds")))

years <- c(2002, 2005, 2009, 2013, 2017, 2021, 2025)
party_cols <- function(d) setdiff(names(d)[vapply(d, is.numeric, logical(1))],
  c("flag_no_valid_votes", "flag_naive_turnout_above_1", "election_year", "land_nr",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes", "turnout",
    "cdu_csu", "other"))

cat("federal_wkr_unharm (wide):\n")
check("7 elections 2002-2025", setequal(unique(wide$election_year), years))
check("299 Wahlkreise per election", all(wide[, uniqueN(wkr_nr), by = election_year]$V1 == 299))
check("two stimmen per Wahlkreis (4186 rows)", nrow(wide) == 299 * 2 * 7)
check("unique (year,wkr,stimme) key", !any(duplicated(wide[, .(election_year, wkr_nr, stimme)])))
pc <- party_cols(wide)
ss <- wide[valid_votes > 0, rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = c(pc, "other")]
check("party+other shares sum to 1.0", all(abs(ss - 1) < 1e-6))
check("no share outside [0,1]", all(wide[, as.matrix(.SD) >= -1e-9 & as.matrix(.SD) <= 1 + 1e-9,
                                         .SDcols = c(pc, "other")], na.rm = TRUE))
check("turnout in [0.4, 0.95]", all(wide$turnout > 0.4 & wide$turnout < 0.95, na.rm = TRUE))
check("no turnout>1 flagged", sum(wide$flag_naive_turnout_above_1) == 0)
check("every Direktmandat assigned", !any(is.na(wide[stimme == "erststimme"]$elected_party)))
cdu_v <- if ("cdu" %in% pc) wide$cdu else 0
csu_v <- if ("csu" %in% pc) wide$csu else 0
check("cdu_csu == cdu + csu", all(abs(wide$cdu_csu - (cdu_v + csu_v)) < 1e-9, na.rm = TRUE))

cat("\nfederal_wkr_unharm_long:\n")
check("long has 110518 rows", nrow(long) == 110518L)
check("long vote_share in [0,1]", all(long$vote_share >= -1e-9 & long$vote_share <= 1 + 1e-9, na.rm = TRUE))
check("long votes are non-negative integers", all(long$votes >= 0 & long$votes == round(long$votes), na.rm = TRUE))
# long per (year,wkr,stimme) named-party votes <= valid_votes
agg <- long[, .(named = sum(votes)), by = .(election_year, wkr_nr, stimme, valid_votes)]
check("named votes <= valid_votes everywhere", all(agg$named <= agg$valid_votes))

cat("\nwkr_2021_to_2025_crosswalk:\n")
check("299 Wahlkreise", nrow(cw) == 299L)
check("categories in {unchanged,redrawn,new}", all(cw$boundary_change %in% c("unchanged", "redrawn", "new")))
check("unchanged rows have zero eligible delta", all(cw[boundary_change == "unchanged"]$eligible_delta == 0))
check("new rows have no 2021 predecessor", all(is.na(cw[boundary_change == "new"]$prior_2021_name)))
check("matched rows have a 2021 predecessor", all(!is.na(cw[boundary_change != "new"]$prior_2021_name)))

cat("\nfederal_wkr_2021_on_2025 (recomputed):\n")
check("598 rows (299 x 2)", nrow(rec) == 598L)
rpc <- setdiff(names(rec)[vapply(rec, is.numeric, logical(1))], c("eligible_voters", "valid_votes", "cdu_csu"))
rss <- rec[valid_votes > 0, rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = rpc]
check("recomputed shares sum to 1.0", all(abs(rss - 1) < 1e-6))
check("boundary_change present for all", !any(is.na(rec$boundary_change)))

cat(sprintf("\nAll %d checks passed.\n", pass))
