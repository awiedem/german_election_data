### federal_muni_harm_21 audit
#
# Health check for data/federal_elections/municipality_level/final/federal_muni_harm_21.rds
# (Bundestagswahlen 1990-2025 on 2021 municipal boundaries). Run after
# code/federal_elections/municipality_level/02_federal_muni_harm_21.R completes.
#
# History, and why this file is now an audit rather than a notebook:
#   * It read `federal_muni_harm.rds`, a name retired when the dataset split into
#     harm_21 / harm_25, so it aborted on line 12 and had been dead ever since.
#   * Every "check" was a cat() or a print(), so even while running it could not
#     fail. It would not have caught the pad_zero_conditional defect that filed
#     every municipality of Saarland, Berlin, Brandenburg, Mecklenburg-Vorpommern,
#     Sachsen, Sachsen-Anhalt and Thüringen under 9-character Schleswig-Holstein
#     codes for 1990-2017 in this very dataset.
#
# Invariant classes follow docs/audit_2026-07_mayoral_council.md §7. The
# identifier and vote-conservation invariants also run as hard stops inside
# 02_federal_muni_harm_21.R before the save block, so a defective file is never
# written; they are repeated here so the published artefact can be re-verified
# on its own, and because this script covers ground the pipeline gate does not
# (share arithmetic, turnout arithmetic, Inf/NaN, party availability windows).
#
# Some quantities are legitimately non-zero and are reported, not asserted on:
# the year-on-year *_ratio columns hold Inf wherever a party went from zero
# votes to some, and harm_21 is knowingly not a balanced panel.
#
# Exit code is 0 iff all checks pass. Warnings do not fail the run.

suppressMessages({
  pacman::p_load(dplyr, readr, tidyr, conflicted)
  conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflict_prefer("lag", "dplyr", quiet = TRUE)
})

setwd(here::here())

failed <- 0
warned <- 0
fail <- function(msg) { cat("  x FAIL:", msg, "\n"); failed <<- failed + 1 }
pass <- function(msg) cat("  -", msg, "\n")
warn <- function(msg) { cat("  ! WARN:", msg, "\n"); warned <<- warned + 1 }
check <- function(ok, ok_msg, fail_msg) if (isTRUE(ok)) pass(ok_msg) else fail(fail_msg)

df     <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds")
raw    <- read_rds("data/federal_elections/municipality_level/final/federal_muni_raw.rds")
unharm <- read_rds("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")

parties <- df %>% dplyr::select(cdu:zentrum) %>% colnames()
STATES  <- sprintf("%02d", 1:16)
YEARS   <- c(1990, 1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)

cat("=====================================================================\n")
cat("federal_muni_harm_21 audit\n")
cat(sprintf("%s rows x %d cols | %s distinct AGS | %d party columns\n",
            format(nrow(df), big.mark = ","), ncol(df),
            format(n_distinct(df$ags), big.mark = ","), length(parties)))
cat("=====================================================================\n\n")


# 1. Identifier integrity -------------------------------------------------
# The AGS-padding class of defect. pad_zero_conditional(x, n) prepends a zero
# iff nchar(x) == n, i.e. the second argument is the length to MATCH, not the
# width to pad TO. Called with 8 it leaves 7-digit codes short and pushes
# 8-digit ones to 9 characters, where substr(ags, 1, 2) then reads the wrong
# state. Both the width and the state decomposition are asserted.

cat("1. Identifier integrity\n")

bad_ags <- sum(!grepl("^[0-9]{8}$", df$ags))
check(is.character(df$ags) && bad_ags == 0,
      sprintf("all %s AGS are 8-digit character codes", format(nrow(df), big.mark = ",")),
      sprintf("%d AGS are not 8-digit character strings (column class: %s)",
              bad_ags, class(df$ags)[1]))

n_state_mismatch <- sum(df$state != substr(df$ags, 1, 2))
check(n_state_mismatch == 0,
      "`state` agrees with substr(ags, 1, 2) in every row",
      sprintf("%d row(s) where `state` disagrees with the AGS prefix", n_state_mismatch))

n_dup <- sum(duplicated(df[c("ags", "election_year")]))
check(n_dup == 0, "(ags, election_year) is unique",
      sprintf("%d duplicate (ags, election_year) key(s)", n_dup))

check(setequal(df$election_year, YEARS),
      sprintf("election years are exactly %s", paste(YEARS, collapse = ", ")),
      sprintf("election years are %s", paste(sort(unique(df$election_year)), collapse = ", ")))

check(!any(is.na(df$ags_name)) && !any(is.na(df$state_name)) && !any(is.na(df$election_date)),
      "ags_name, state_name and election_date are complete",
      sprintf("missing metadata: %d ags_name, %d state_name, %d election_date",
              sum(is.na(df$ags_name)), sum(is.na(df$state_name)), sum(is.na(df$election_date))))

multi_date <- df %>% distinct(election_year, election_date) %>% count(election_year) %>% filter(n > 1)
check(nrow(multi_date) == 0, "exactly one election_date per election_year",
      sprintf("%d election year(s) carry more than one election_date", nrow(multi_date)))


# 2. State coverage -------------------------------------------------------
# The invariant the padding defect violated: with 9-character codes, states
# 10-16 held no rows at all in 1990-2017 and their municipalities re-surfaced
# under the "01" prefix.

cat("\n2. State coverage across election years\n")

cov <- df %>% mutate(st = substr(ags, 1, 2)) %>% count(st, election_year)

missing_st <- expand.grid(st = STATES, election_year = YEARS, stringsAsFactors = FALSE) %>%
  anti_join(cov, by = c("st", "election_year"))
check(nrow(missing_st) == 0,
      sprintf("all 16 states populated in all %d election years", length(YEARS)),
      sprintf("%d state-year cell(s) hold zero municipalities, e.g. %s",
              nrow(missing_st),
              paste(utils::head(paste0(missing_st$st, "/", missing_st$election_year), 8),
                    collapse = ", ")))

# On fixed 2021 boundaries a state's municipality count may only move by the
# handful of pre-2021 units the crosswalk cannot collapse. Worst observed
# 2026-07: Rheinland-Pfalz 2021 at 95.8 % of its own maximum.
cov <- cov %>% group_by(st) %>% mutate(frac = n / max(n)) %>% ungroup()
drop <- cov %>% filter(frac < 0.90)
check(nrow(drop) == 0,
      sprintf("no state-year below 90%% of that state's peak count (worst %.1f%%)",
              100 * min(cov$frac)),
      sprintf("%d state-year(s) below 90%% of the state peak: %s", nrow(drop),
              paste(sprintf("%s/%d=%.0f%%", drop$st, drop$election_year, 100 * drop$frac),
                    collapse = ", ")))


# 3. Vote-share arithmetic ------------------------------------------------

cat("\n3. Vote-share arithmetic\n")

pm  <- df %>% dplyr::select(all_of(parties)) %>% as.matrix()
oob <- sum(pm < -1e-12 | pm > 1 + 1e-12, na.rm = TRUE)
check(oob == 0,
      sprintf("all %s party vote shares lie in [0, 1]",
              format(sum(!is.na(pm)), big.mark = ",")),
      sprintf("%d party vote share cell(s) outside [0, 1]", oob))

# Shares must sum to exactly 1. The only exception is a municipality with a zero
# electorate, where the 0/0 division leaves NaN in every party column and the row
# sums to 0. Any other sum means votes were lost or double-counted, so the check
# is conditioned on valid_votes rather than waved through as "about 0.2 %".
s <- round(rowSums(pm, na.rm = TRUE), 8)
offenders <- which(s != 1 & df$valid_votes != 0)
check(length(offenders) == 0,
      sprintf("party shares sum to 1 in every row that has votes (%d zero-vote rows sum to 0)",
              sum(s != 1)),
      sprintf("%d row(s) with valid_votes > 0 whose shares do not sum to 1, e.g. %s",
              length(offenders),
              paste(utils::head(sprintf("%s/%d (sum %.6f)", df$ags[offenders],
                                        df$election_year[offenders], s[offenders]), 5),
                    collapse = ", ")))

dev_cdu <- max(abs(df$cdu_csu - (coalesce(df$cdu, 0) + coalesce(df$csu, 0))), na.rm = TRUE)
check(dev_cdu < 1e-9, "cdu_csu equals cdu + csu exactly",
      sprintf("cdu_csu deviates from cdu + csu by up to %.2e", dev_cdu))

n_fl <- sum(df$far_left > df$far_left_w_linke + 1e-12, na.rm = TRUE)
check(n_fl == 0, "far_left never exceeds far_left_w_linke",
      sprintf("%d row(s) where far_left > far_left_w_linke", n_fl))

for (v in c("far_right", "far_left", "far_left_w_linke", "turnout", "turnout_wo_mailin")) {
  x <- df[[v]]
  check(all(x >= -1e-12 & x <= 1 + 1e-12, na.rm = TRUE),
        sprintf("%s within [0, 1] (max %.4f)", v, max(x, na.rm = TRUE)),
        sprintf("%s leaves [0, 1]: min %.4f, max %.4f",
                v, min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
}


# 4. Turnout arithmetic ---------------------------------------------------

cat("\n4. Turnout arithmetic\n")

check(!any(is.na(df$valid_votes)) && !any(is.na(df$eligible_voters)) &&
        all(df$valid_votes >= 0) && all(df$eligible_voters >= 0),
      "valid_votes and eligible_voters are complete and non-negative",
      "valid_votes / eligible_voters contain NA or negative values")

n_vv <- sum(df$number_voters < df$valid_votes - 1e-6, na.rm = TRUE)
check(n_vv == 0, "number_voters >= valid_votes in every row",
      sprintf("%d row(s) record more valid votes than voters", n_vv))

# A handful of rows do have more voters than eligible voters, because mail-in
# districts are shared across municipalities. They are legitimate, but every one
# of them must carry the flag: an unflagged case means the flag logic stopped
# tracking the anomaly it exists for.
over <- df$eligible_voters < df$number_voters - 1e-6
unflagged <- sum(over & (is.na(df$flag_naive_turnout_above_1) | df$flag_naive_turnout_above_1 != 1))
check(unflagged == 0,
      sprintf("all %d rows with number_voters > eligible_voters carry flag_naive_turnout_above_1",
              sum(over)),
      sprintf("%d of %d rows with number_voters > eligible_voters are unflagged",
              unflagged, sum(over)))

# valid + invalid must reconstruct number_voters. Crosswalk-weighted allocation
# rounds, so the smallest municipalities drift by a vote or two; anything larger
# is a real allocation error. Worst observed 2026-07: 8 votes.
resid <- abs(df$number_voters - (df$valid_votes + df$invalid_votes))
check(max(resid, na.rm = TRUE) <= 50,
      sprintf("valid + invalid reconstructs number_voters (max residual %d votes; %d rows off by > 1)",
              as.integer(max(resid, na.rm = TRUE)), sum(resid > 1, na.rm = TRUE)),
      sprintf("max |number_voters - (valid + invalid)| is %.0f votes",
              max(resid, na.rm = TRUE)))


# 5. Harmonisation conserves votes ----------------------------------------
# Re-filing municipalities onto 2021 boundaries redistributes votes; it may
# never create or destroy them. A state that silently vanished shows up here as
# -100 %, one that absorbed another's rows as a large positive — so this check
# fails on the padding defect independently of section 1.
# Per state-year the tolerance is 0.01 %, except in 1990, where harmonising to
# 2021 borders genuinely moves territory across Land lines (Amt Neuhaus from
# Mecklenburg-Vorpommern to Niedersachsen, a few municipalities across the MV/BB
# and TH/SN lines). Those transfers cancel nationally, which is why the national
# tolerance stays tight in every year.

cat("\n5. Harmonisation against federal_muni_unharm\n")

u <- unharm %>% filter(election_year %in% YEARS)

recon <- function(hh, uu, value) {
  full_join(
    hh %>% mutate(st = substr(ags, 1, 2)) %>% group_by(st, election_year) %>%
      summarise(h = sum(.data[[value]], na.rm = TRUE), .groups = "drop"),
    uu %>% mutate(st = substr(ags, 1, 2)) %>% group_by(st, election_year) %>%
      summarise(un = sum(.data[[value]], na.rm = TRUE), .groups = "drop"),
    by = c("st", "election_year")) %>%
    mutate(
      h = coalesce(h, 0), un = coalesce(un, 0),
      # a state-year present downstream but absent upstream is fabrication and
      # has no denominator: report it as infinite rather than dividing by zero
      pct = case_when(un > 0 ~ 100 * (h - un) / un, h > 0 ~ Inf, TRUE ~ 0),
      tol = ifelse(election_year == 1990, 1.5, 0.01)
    )
}

nat <- full_join(
  df %>% group_by(election_year) %>% summarise(h = sum(valid_votes, na.rm = TRUE), .groups = "drop"),
  u  %>% group_by(election_year) %>% summarise(un = sum(valid_votes, na.rm = TRUE), .groups = "drop"),
  by = "election_year") %>%
  mutate(pct = 100 * (h - un) / un)
check(nrow(nat) == length(YEARS) && all(is.finite(nat$pct)) && max(abs(nat$pct)) < 0.01,
      sprintf("national valid_votes reconcile in all %d years (worst %+.1e %%)",
              nrow(nat), nat$pct[which.max(abs(nat$pct))]),
      sprintf("national valid_votes drift: %s",
              paste(sprintf("%d %+.4f%%", nat$election_year, nat$pct)[abs(nat$pct) >= 0.01],
                    collapse = ", ")))

for (value in c("valid_votes", "eligible_voters")) {
  r   <- recon(df, u, value)
  bad <- r %>% filter(!is.finite(pct) | abs(pct) > tol) %>% arrange(desc(abs(pct)))
  check(nrow(bad) == 0,
        sprintf("%s reconcile in all %d state-years (worst %+.2f%%, 1990 tolerance 1.5%%)",
                value, nrow(r), r$pct[which.max(abs(r$pct))]),
        sprintf("%d state-year(s) outside tolerance for %s: %s", nrow(bad), value,
                paste(utils::head(sprintf("%s/%d %+.2f%%", bad$st, bad$election_year, bad$pct), 8),
                      collapse = ", ")))
}


# 6. Inf / NaN in published columns ---------------------------------------
# NaN is the fingerprint of a 0/0 vote share and must stay confined to the
# municipalities that genuinely cast no votes. Inf must not occur at all: no
# published column divides by a quantity that can legitimately be zero.

cat("\n6. Inf / NaN in published columns\n")

numcols  <- names(df)[vapply(df, is.numeric, logical(1))]
inf_cols <- numcols[vapply(numcols, function(v) any(is.infinite(df[[v]])), logical(1))]
check(length(inf_cols) == 0, "no Inf in any published column",
      sprintf("Inf present in: %s", paste(inf_cols, collapse = ", ")))

nan_rows <- Reduce(`|`, lapply(numcols, function(v) is.nan(df[[v]])))
stray    <- sum(nan_rows & df$valid_votes != 0)
check(stray == 0,
      sprintf("NaN confined to the %d zero-vote municipalities", sum(nan_rows)),
      sprintf("%d row(s) carry NaN despite valid_votes > 0", stray))

raw_inf <- names(raw)[vapply(raw, function(x) is.numeric(x) && any(is.infinite(x)), logical(1))]
check(length(raw_inf) == 0, "no Inf in federal_muni_raw",
      sprintf("Inf in federal_muni_raw: %s", paste(raw_inf, collapse = ", ")))


# 7. Party availability over time -----------------------------------------
# A party must be absent before it existed and near-universal once it stood
# nationwide. Both directions catch a party column joined onto the wrong years.

cat("\n7. Party availability over time\n")

coverage_from <- function(col, first_year, min_cov = 0.97) {
  covg <- df %>% filter(election_year >= first_year) %>%
    group_by(election_year) %>%
    summarise(cov = mean(!is.na(.data[[col]])), .groups = "drop")
  check(all(covg$cov >= min_cov),
        sprintf("%s present in >= %.0f%% of municipalities from %d on (min %.1f%%)",
                col, 100 * min_cov, first_year, 100 * min(covg$cov)),
        sprintf("%s coverage falls below %.0f%%: %s", col, 100 * min_cov,
                paste(sprintf("%d %.1f%%", covg$election_year, 100 * covg$cov)[covg$cov < min_cov],
                      collapse = ", ")))
}

for (pw in list(list(col = "afd", from = 2013), list(col = "bsw", from = 2025))) {
  early <- df %>% filter(election_year < pw$from, !is.na(.data[[pw$col]]))
  check(nrow(early) == 0,
        sprintf("%s is NA in every election before %d", pw$col, pw$from),
        sprintf("%s holds %d non-NA value(s) before %d (earliest %s)",
                pw$col, nrow(early), pw$from,
                if (nrow(early)) min(early$election_year) else NA))
  coverage_from(pw$col, pw$from)
}

for (p in c("cdu_csu", "spd", "fdp", "gruene", "linke_pds")) coverage_from(p, min(YEARS))


# 8. Year-on-year plausibility --------------------------------------------
# Ratios against the previous election. Inf here is expected and is NOT an
# error: a party moving from zero votes to any share divides by zero, which is
# why linke_pds alone carries ~4,000 of them. Only the finite tail is asserted
# on, and generously — these bounds are set to catch a decimal-point or
# column-shift error, not ordinary political volatility.

cat("\n8. Year-on-year plausibility\n")

parties_main <- c("cdu_csu", "spd", "fdp", "gruene", "linke_pds", "afd", "turnout")

lagged <- df %>%
  arrange(ags, election_year) %>%
  group_by(ags) %>%
  mutate(across(all_of(parties_main), list(ratio = ~ . / lag(.)), .names = "{col}_ratio"),
         valid_votes_ratio = valid_votes / lag(valid_votes),
         unique_mailin_lag = lag(unique_mailin)) %>%
  ungroup()

cat("   finite-ratio quantiles      1%     50%     99%   99.9%     Inf\n")
for (p in parties_main) {
  x <- lagged[[paste0(p, "_ratio")]]
  q <- quantile(x[is.finite(x)], c(0.01, 0.5, 0.99, 0.999), na.rm = TRUE)
  cat(sprintf("     %-14s %8.3f %7.3f %7.3f %7.3f %7d\n",
              p, q[1], q[2], q[3], q[4], sum(is.infinite(x))))
}

# Turnout is the one ratio that can never be Inf: a municipality with no
# electorate has NA turnout in both years, not zero.
tr <- lagged$turnout_ratio
check(!any(is.infinite(tr)) && all(tr[is.finite(tr)] <= 3, na.rm = TRUE),
      sprintf("turnout never changes by more than 3x between elections (max %.2f)",
              max(tr[is.finite(tr)], na.rm = TRUE)),
      sprintf("turnout ratio reaches %.2f with %d Inf",
              max(tr[is.finite(tr)], na.rm = TRUE), sum(is.infinite(tr))))

# valid_votes cannot be held to a year-on-year bound in general, and the reason
# is `unique_mailin`: where a municipality shares a mail-in district (0), the
# district's pooled postal votes are split across its members, so each member's
# total moves with the split rather than with its own electorate. Every large
# jump in this file is of that kind — 139 of the 202 fall in 2025 alone, nearly
# all tiny Eifel municipalities, and Altenahr 2021 (x5.9) is the Ahrtal flood
# year, where the Verbandsgemeinde reported through one AGS. All of it is
# already present upstream in federal_muni_unharm, so none of it is a
# harmonisation defect and none of it is assertable.
#
# Restricting to municipalities that ran their OWN mail-in district in both
# elections removes the confound entirely: across 40,194 such consecutive pairs
# the ratio spans [0.61, 1.57]. On fixed 2021 boundaries that is what
# demographic change looks like, so a unit leaving (0.4, 2.5) there means the
# crosswalk mis-assigned it.
own_mailin <- lagged %>% filter(unique_mailin == 1, unique_mailin_lag == 1)
own_bad <- own_mailin %>% filter(valid_votes_ratio > 2.5 | valid_votes_ratio < 0.4)
cat(sprintf("   valid_votes jumps > 2x: %d overall, %d of them in shared mail-in districts\n",
            sum(lagged$valid_votes_ratio > 2, na.rm = TRUE),
            sum(lagged$valid_votes_ratio > 2 & lagged$unique_mailin == 0, na.rm = TRUE)))
check(nrow(own_bad) == 0,
      sprintf("valid_votes stay within (0.4, 2.5)x across %s own-mail-in pairs (observed %.2f-%.2f)",
              format(nrow(own_mailin), big.mark = ","),
              min(own_mailin$valid_votes_ratio, na.rm = TRUE),
              max(own_mailin$valid_votes_ratio, na.rm = TRUE)),
      sprintf("%d municipality-year(s) with their own mail-in district in both elections move outside (0.4, 2.5)x: %s",
              nrow(own_bad),
              paste(utils::head(sprintf("%s/%d x%.2f", own_bad$ags, own_bad$election_year,
                                        own_bad$valid_votes_ratio), 5), collapse = ", ")))


# 9. Panel shape (reported, not asserted) ---------------------------------
# harm_21 is known not to be a balanced panel over the 2021 municipality set;
# see checks_federal_muni_harmonization_stability.R. Reported so that a change
# in the imbalance is visible rather than silent.

cat("\n9. Panel shape\n")

appear <- table(table(df$ags))
cat("   appearances per AGS:",
    paste(sprintf("%sx: %d", names(appear), appear), collapse = ",  "), "\n")

extra <- setdiff(unique(df$ags), df$ags[df$election_year == 2021])
if (length(extra) == 0) {
  pass("every AGS in the panel is a 2021 municipality")
} else {
  warn(sprintf("%d AGS appear in the panel but not in 2021 (states %s) - known harm_21 stability issue",
               length(extra), paste(sort(unique(substr(extra, 1, 2))), collapse = "/")))
}


# 10. Dead columns --------------------------------------------------------
# All-NA party columns are correct: those parties only ever stood before 1990,
# and harm_21 begins in 1990. All-NA columns outside the party block are residue.

cat("\n10. Dead columns\n")

all_na     <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
dead_party <- intersect(all_na, parties)
dead_other <- setdiff(all_na, parties)
cat(sprintf("   %d all-NA party columns (parties that only stood pre-1990, expected): %s\n",
            length(dead_party), paste(dead_party, collapse = ", ")))
if (length(dead_other) == 0) {
  pass("no all-NA columns outside the party block")
} else {
  warn(sprintf("%d all-NA non-party column(s) published: %s",
               length(dead_other), paste(dead_other, collapse = ", ")))
}


cat("\n=====================================================================\n")
if (failed == 0) {
  cat(sprintf("All checks passed (%d warning%s)\n", warned, ifelse(warned == 1, "", "s")))
  quit(status = 0)
} else {
  cat(sprintf("%d check(s) FAILED (%d warning%s)\n", failed, warned, ifelse(warned == 1, "", "s")))
  quit(status = 1)
}
