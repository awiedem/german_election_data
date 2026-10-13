### Cross-dataset plausibility checks for the council-election datasets.
#
# Every per-pipeline audit tests a dataset against itself, so a file that holds
# the WRONG election, or a party column holding another party's votes, passes
# them all: its sums, shares and identifiers are internally consistent. Two such
# defects shipped until 2026-09 and were found only by comparing datasets:
#
#   * municipal_* Sachsen-Anhalt 2024 held the Kreistag results (the raw file
#     was the Kreistag file summed to Gemeinden): its party shares equalled
#     county_elec_unharm in every Gemeinde (correlation 1.000).
#   * county_elec_* Sachsen 2024 had AfD and CDU (and GRÜNE/SPD/FDP) swapped by
#     a mislabelled source sheet: the AfD share correlated -0.19, CDU -0.62,
#     with the Landtagswahl three months later.
#
# Checks (kreisfreie Städte are excluded from A, where Stadtrat and "Kreistag"
# are the same body and identical results are correct):
#   A. municipal vs county, same state-year: shares must not be identical.
#   B. county vs the nearest Landtagswahl (<= 3 years): no party may correlate
#      negatively across municipalities.
#   C. municipal vs the nearest Landtagswahl: same.
# Calibrated on the pre-fix data: A fires on ST 2024 (and on SH 1998-2013),
# B on SN 2024; neither fires elsewhere. Correlations below 0.2 are printed.
#
# Run: Rscript code/checks/checks_council_cross_dataset.R   (exit 1 on ERROR)

pacman::p_load(data.table)
setwd(here::here())

mu <- as.data.table(readRDS("data/municipal_elections/final/municipal_unharm.rds"))
ku <- as.data.table(readRDS("data/county_elections/final/county_elec_unharm.rds"))
su <- as.data.table(readRDS("data/state_elections/final/state_unharm.rds"))
for (d in list(mu, ku, su)) d[, ags := as.character(ags)]
ku[, cdu_csu := cdu]
su <- su[, .(ags, state, election_year, afd, cdu_csu, spd)]

parties <- c("afd", "cdu_csu", "spd")
n_err <- 0L
report <- function(sev, check, msg, det = NULL) {
  if (sev == "ERROR") n_err <<- n_err + 1L
  cat(sprintf("[%s] %-28s %s\n", sev, check, msg))
  if (!is.null(det) && nrow(det) > 0) print(det)
}

# --- A. municipal vs county: identical shares = one holds the other's data ----
not_krfr <- function(d) d[substr(ags, 6, 8) != "000"]
a <- rbindlist(lapply(parties, function(p) {
  j <- merge(
    not_krfr(mu)[, .(ags, election_year, st = substr(ags, 1, 2), m = get(p))],
    not_krfr(ku)[, .(ags, election_year, k = get(p))],
    by = c("ags", "election_year")
  )[!is.na(m) & !is.na(k)]
  j[, .(n = .N, share_identical = round(mean(abs(m - k) < 1e-4), 3),
        cor = round(cor(m, k), 4)), by = .(st, election_year)][, party := p]
}))
bad_a <- a[n >= 20 & share_identical > 0.5][order(st, election_year, party)]
if (nrow(bad_a) > 0) {
  report("ERROR", "muni_vs_county.identical",
         sprintf("%d state-year-party cells where municipal shares equal the Kreistag's",
                 nrow(bad_a)), bad_a)
} else {
  report("OK", "muni_vs_county.identical",
         sprintf("no municipal state-year reproduces the Kreistag (max identical share %.3f)",
                 max(a[n >= 20]$share_identical)))
}

# --- B/C. council vs nearest Landtagswahl: no negative correlations ------------
vs_state <- function(d, label) {
  out <- list()
  for (s in unique(substr(d$ags, 1, 2))) {
    ltw_years <- unique(su[state == s]$election_year)
    if (!length(ltw_years)) next
    for (y in unique(d[substr(ags, 1, 2) == s]$election_year)) {
      ltw <- ltw_years[which.min(abs(ltw_years - y))]
      if (abs(ltw - y) > 3) next
      for (p in parties) {
        j <- merge(d[substr(ags, 1, 2) == s & election_year == y, .(ags, x = get(p))],
                   su[state == s & election_year == ltw, .(ags, v = get(p))],
                   by = "ags")[!is.na(x) & !is.na(v)]
        if (nrow(j) < 20 || sd(j$x) == 0 || sd(j$v) == 0) next
        out[[length(out) + 1]] <- data.table(st = s, election_year = y, ltw = ltw,
                                             party = p, n = nrow(j),
                                             cor = round(cor(j$x, j$v), 3))
      }
    }
  }
  r <- rbindlist(out)
  neg <- r[cor < 0][order(cor)]
  if (nrow(neg) > 0) {
    report("ERROR", paste0(label, "_vs_state.negative"),
           sprintf("%d state-year-party cells correlate negatively with the Landtagswahl",
                   nrow(neg)), neg)
  } else {
    report("OK", paste0(label, "_vs_state.negative"),
           sprintf("all %d cells correlate positively (min %.3f)", nrow(r), min(r$cor)))
  }
  low <- r[cor >= 0 & cor < 0.2][order(cor)]
  if (nrow(low) > 0) {
    cat(sprintf("       %s: %d cells below 0.2 (informational)\n", label, nrow(low)))
    print(low)
  }
}
vs_state(ku, "county")
vs_state(mu, "muni")

cat(sprintf("\n%d ERROR(s)\n", n_err))
if (n_err > 0) quit(status = 1)
