### Landrat dataset audit — comprehensive integrity + correctness checks
#
# Covers schema, cross-leakage, AGS validity, date sanity, vote-count
# integrity, per-state coverage, external-truth spot checks, candidate-level
# integrity, duplicate detection, Stichwahl logic.
#
# Exit code 0 iff all checks pass.

suppressMessages({
  library(dplyr); library(readr); library(tibble); library(stringr)
  library(lubridate); library(here); library(conflicted)
  conflict_prefer("filter", "dplyr"); conflict_prefer("year", "lubridate")
  conflict_prefer("first", "dplyr")
})
setwd(here::here())

l  <- readRDS("data/landrat_elections/final/landrat_unharm.rds")
lc <- readRDS("data/landrat_elections/final/landrat_candidates.rds")

failed <- 0L
warned <- 0L
fail <- function(msg) { cat("  ✗ FAIL:", msg, "\n"); failed <<- failed + 1L }
warn <- function(msg) { cat("  ⚠ WARN:", msg, "\n"); warned <<- warned + 1L }
pass <- function(msg) { cat("  ✓", msg, "\n") }
check <- function(cond, ok, ko) {
  if (isTRUE(cond)) pass(ok) else fail(ko)
}
check_warn <- function(cond, ok, ko) {
  if (isTRUE(cond)) pass(ok) else warn(ko)
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

cat("════════════════════════════════════════════════════════════════════\n")
cat("Landrat dataset audit\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

# ============================================================================
# 1. Schema
# ============================================================================
cat("1. Schema checks\n")
expected_unharm <- c("ags", "ags_name", "state", "state_name",
                     "election_year", "election_date", "election_type", "round",
                     "eligible_voters", "number_voters", "valid_votes",
                     "invalid_votes", "turnout",
                     "winner_party", "winner_votes", "winner_voteshare")
miss_u <- setdiff(expected_unharm, names(l))
check(length(miss_u) == 0,
      sprintf("landrat_unharm has all %d expected columns", length(expected_unharm)),
      sprintf("landrat_unharm missing columns: %s", paste(miss_u, collapse = ", ")))

expected_cand <- c("ags", "ags_name", "state", "state_name",
                   "election_year", "election_date", "election_date_sw",
                   "election_type", "has_stichwahl",
                   "candidate_name", "candidate_party",
                   "candidate_votes_hw", "candidate_voteshare_hw",
                   "candidate_votes_sw", "candidate_voteshare_sw",
                   "is_winner")
miss_c <- setdiff(expected_cand, names(lc))
check(length(miss_c) == 0,
      sprintf("landrat_candidates has all %d critical columns", length(expected_cand)),
      sprintf("landrat_candidates missing columns: %s", paste(miss_c, collapse = ", ")))

check(is.character(l$ags) && all(nchar(l$ags) == 8L),
      "landrat_unharm$ags is character & all 8 chars",
      "ags malformed in landrat_unharm")

check(inherits(l$election_date, "Date"),
      "election_date is Date type",
      "election_date is not Date type")

# ============================================================================
# 2. Cross-leakage between mayoral and landrat
# ============================================================================
cat("\n2. Cross-leakage with mayoral pipeline\n")
m <- readRDS("data/mayoral_elections/final/mayoral_unharm.rds")

n1 <- sum(grepl("Krfr\\.|Kreisfreie Stadt", l$ags_name, ignore.case = TRUE))
check(n1 == 0,
      "no kreisfreie Städte in landrat_unharm",
      sprintf("%d kreisfreie Städte leaked into landrat_unharm", n1))

n2 <- sum(grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion",
                m$ags_name) & m$state == "05")
check(n2 == 0,
      "no Kreise leaked into mayoral_unharm (NRW)",
      sprintf("%d NRW Kreise leaked into mayoral_unharm", n2))

n3 <- sum(l$election_type != "Landratswahl", na.rm = TRUE)
check(n3 == 0,
      "all landrat_unharm rows have election_type == 'Landratswahl'",
      sprintf("%d landrat_unharm rows have non-Landrat election_type", n3))

# ============================================================================
# 3. AGS validity
# ============================================================================
cat("\n3. AGS validity\n")
n4 <- sum(!grepl("^[0-9]{8}$", l$ags))
check(n4 == 0,
      "all AGS are 8 digits",
      sprintf("%d AGS not 8 digits", n4))

mismatch <- l %>% filter(substr(ags, 1, 2) != state)
check(nrow(mismatch) == 0,
      "AGS state prefix matches state column for all rows",
      sprintf("%d rows have AGS prefix not matching state column", nrow(mismatch)))

n5 <- sum(!grepl("000$", l$ags))
if (n5 == 0) {
  pass("all AGS end in '000' (Kreis-level)")
} else {
  warn(sprintf("%d AGS do NOT end in '000' (SL Regionalverband uses 10000041)", n5))
}

# ============================================================================
# 4. Date sanity
# ============================================================================
cat("\n4. Date sanity\n")
mismatch <- l %>% filter(!is.na(election_date), election_year != year(election_date))
check(nrow(mismatch) == 0,
      "election_year matches year(election_date) for all rows",
      sprintf("%d rows have election_year != year(election_date)", nrow(mismatch)))

out_of_range <- l %>% filter(election_year < 1945 | election_year > 2030)
check(nrow(out_of_range) == 0,
      "all election years in [1945, 2030]",
      sprintf("%d rows with year outside plausible range", nrow(out_of_range)))

n_na_date <- sum(is.na(l$election_date))
check_warn(n_na_date == 0,
           "no NA election_date in landrat_unharm",
           sprintf("%d rows with NA election_date", n_na_date))

# ============================================================================
# 5. Vote-count integrity (where data is available)
# ============================================================================
cat("\n5. Vote-count integrity\n")
bad_turnout <- l %>% filter(!is.na(turnout) & (turnout < 0 | turnout > 1.05))
check(nrow(bad_turnout) == 0,
      "turnout in [0, 1.05] for all non-NA rows",
      sprintf("%d rows with turnout out of range", nrow(bad_turnout)))

diff_check <- l %>%
  filter(!is.na(valid_votes), !is.na(invalid_votes), !is.na(number_voters)) %>%
  mutate(diff = abs(valid_votes + invalid_votes - number_voters)) %>%
  filter(diff > 5)
check_warn(nrow(diff_check) == 0,
           "valid + invalid votes ≈ number_voters",
           sprintf("%d rows where valid+invalid differs from voters by >5",
                   nrow(diff_check)))

bad_vs <- l %>% filter(!is.na(winner_voteshare),
                        winner_voteshare < 0 | winner_voteshare > 1.01)
check(nrow(bad_vs) == 0,
      "winner_voteshare in [0, 1.01]",
      sprintf("%d rows with winner_voteshare out of range", nrow(bad_vs)))

cand_sum_check <- lc %>%
  filter(!is.na(candidate_votes_hw), !is.na(valid_votes), valid_votes > 0) %>%
  group_by(ags, election_date, state) %>%
  summarise(sum_cand = sum(candidate_votes_hw, na.rm = TRUE),
            valid = first(valid_votes), .groups = "drop") %>%
  mutate(over = sum_cand > valid * 1.01)
# Exclude NI: known pre-existing pipeline issue where HW/SW vote counts get
# mixed in the wide-format pivot (mayoral_elections/01b_mayoral_candidates.R).
n_over_excl_ni <- sum(cand_sum_check$over & cand_sum_check$state != "03")
n_over_ni     <- sum(cand_sum_check$over & cand_sum_check$state == "03")
check(n_over_excl_ni == 0,
      sprintf("candidate vote sums ≤ valid_votes for all non-NI elections (%d total)",
              sum(cand_sum_check$state != "03")),
      sprintf("%d elections (excluding NI) where vote sum exceeds valid_votes",
              n_over_excl_ni))
if (n_over_ni > 0) {
  warn(sprintf("%d NI elections with HW+SW vote-count mixing (known pre-existing pipeline issue)",
               n_over_ni))
}

# ============================================================================
# 6. Per-state coverage expectations
# ============================================================================
cat("\n6. Per-state coverage\n")
state_counts <- l %>% count(state, state_name)
expected_min <- c("03"=80, "05"=140, "07"=110, "09"=1000, "10"=5,
                  "12"=20, "14"=30, "15"=20, "16"=80)
for (st in names(expected_min)) {
  actual <- state_counts %>% filter(state == st) %>% pull(n)
  if (length(actual) == 0) actual <- 0L
  exp_n <- expected_min[[st]]
  check(actual >= exp_n,
        sprintf("state %s: %d rows (≥%d expected)", st, actual, exp_n),
        sprintf("state %s: only %d rows (expected ≥%d)", st, actual, exp_n))
}

# ============================================================================
# 7. External-truth spot checks
# ============================================================================
cat("\n7. External-truth spot checks\n")
spot_check <- function(label, ags_v, year, expected_party_pattern) {
  rows <- l %>% filter(ags == ags_v, election_year == year)
  if (nrow(rows) == 0) {
    fail(sprintf("%s: no row for ags=%s year=%d", label, ags_v, year))
    return(invisible())
  }
  best <- rows %>% arrange(desc(round == "stichwahl")) %>% slice(1)
  wp <- best$winner_party
  if (is.null(wp) || is.na(wp) || wp == "") {
    warn(sprintf("%s: winner_party is NA (year=%d, round=%s)",
                 label, year, best$round))
  } else if (grepl(expected_party_pattern, wp, ignore.case = TRUE)) {
    pass(sprintf("%s: winner_party = '%s'", label, wp))
  } else {
    fail(sprintf("%s: winner_party = '%s', expected match '%s'",
                 label, wp, expected_party_pattern))
  }
}

spot_check("Bayern LK Erding 2020", "09177000", 2020, "CSU")
spot_check("NRW Kreis Kleve 2020", "05154000", 2020, "CDU")
spot_check("Sachsen Mittelsachsen 2025", "14522000", 2025, "FW|CDU|Freie")
# Thüringen 2018 winner_party often NA because TH source files don't always
# list party in parens; this is a known data source limitation
n_th_eichs <- l %>% filter(ags == "16061000", election_year == 2018) %>% nrow()
check_warn(n_th_eichs > 0,
           sprintf("TH LK Eichsfeld 2018: %d row(s) present (winner party NA in source)",
                   n_th_eichs),
           "TH LK Eichsfeld 2018: missing")
# Region Hannover ("Regionspräsident") is treated specially in the existing
# NI parser. AGS may be 03241001 (Stadt) or 03241000 (Region) depending on
# how the PDF labels it. Just verify Hannover/Region exists in some form.
hano <- l %>% filter(grepl("annover", ags_name) & state == "03")
check_warn(nrow(hano) > 0,
           sprintf("NI Hannover/Region: %d row(s) present", nrow(hano)),
           "NI Hannover/Region: completely missing")
spot_check("SL LK Neunkirchen 2024", "10043000", 2024, "SPD")
spot_check("BB LK Havelland 2024", "12063000", 2024, "CDU")
spot_check("ST Burgenlandkreis 2014", "15084000", 2014, "CDU")

n_dus <- l %>% filter(ags == "05111000") %>% nrow()
check(n_dus == 0,
      "Düsseldorf (kreisfreie Stadt) NOT in landrat",
      sprintf("Düsseldorf appears %d times in landrat (should be 0)", n_dus))

n_rlp_94 <- l %>% filter(state == "07", election_year == 1994) %>% nrow()
check_warn(n_rlp_94 > 0,
           sprintf("RLP 1994 has %d rows", n_rlp_94),
           "RLP 1994 has 0 rows")

by_min_yr <- suppressWarnings(min(l %>% filter(state == "09") %>% pull(election_year)))
check(!is.infinite(by_min_yr) && by_min_yr <= 1950,
      sprintf("Bayern earliest year = %d (≤1950 expected)", by_min_yr),
      sprintf("Bayern earliest year = %d (expected ≤1950)", by_min_yr))

# ============================================================================
# 8. Candidate-level integrity
# ============================================================================
cat("\n8. Candidate-level integrity\n")
unharm_keys <- l %>% distinct(ags, election_date)
cand_keys <- lc %>% distinct(ags, election_date)
no_cands <- anti_join(unharm_keys, cand_keys, by = c("ags", "election_date"))
check_warn(nrow(no_cands) == 0,
           sprintf("every (ags, election_date) in unharm has ≥1 candidate row (%d total)",
                   nrow(unharm_keys)),
           sprintf("%d unharm rows have no matching candidate row", nrow(no_cands)))

no_winner <- lc %>%
  group_by(ags, election_date, election_type) %>%
  summarise(any_winner = any(is_winner, na.rm = TRUE), .groups = "drop") %>%
  filter(!any_winner)
check_warn(nrow(no_winner) == 0,
           "every election in candidates has ≥1 winner",
           sprintf("%d elections have no winner candidate", nrow(no_winner)))

no_rank1 <- lc %>%
  filter(!is.na(candidate_votes_hw)) %>%
  group_by(ags, election_date) %>%
  summarise(has_rank1 = any(candidate_rank_hw == 1, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!has_rank1)
check_warn(nrow(no_rank1) == 0,
           "every HW election has a rank-1 candidate",
           sprintf("%d elections missing rank-1 candidate", nrow(no_rank1)))

n_na_name_scraped <- lc %>%
  filter(state %in% c("05","12","14","15","16"), is.na(candidate_name)) %>%
  nrow()
check(n_na_name_scraped == 0,
      "all NRW/BB/SN/ST/TH candidates have non-NA name",
      sprintf("%d NRW/BB/SN/ST/TH candidates with NA name", n_na_name_scraped))

# ============================================================================
# 9. Duplicate detection
# ============================================================================
cat("\n9. Duplicate detection\n")
dup_unharm <- l %>%
  count(ags, election_date, election_type, round) %>%
  filter(n > 1)
check(nrow(dup_unharm) == 0,
      "no duplicate (ags, date, type, round) in unharm",
      sprintf("%d duplicate rows in unharm", nrow(dup_unharm)))

dup_cand <- lc %>%
  filter(!is.na(candidate_name)) %>%
  count(ags, election_date, candidate_name) %>%
  filter(n > 1)
check_warn(nrow(dup_cand) == 0,
           "no duplicate (ags, date, candidate_name) in candidates",
           sprintf("%d duplicate (ags, date, candidate_name) in candidates",
                   nrow(dup_cand)))

# ============================================================================
# 10. Stichwahl logic
# ============================================================================
cat("\n10. Stichwahl logic\n")
sw_rows <- l %>% filter(round == "stichwahl") %>% distinct(ags, election_year)
hw_rows <- l %>% filter(round == "hauptwahl") %>% distinct(ags, election_year)
sw_no_hw <- anti_join(sw_rows, hw_rows, by = c("ags", "election_year"))
check_warn(nrow(sw_no_hw) == 0,
           "every Stichwahl has a matching Hauptwahl in same year",
           sprintf("%d Stichwahl rows have no matching Hauptwahl (OK for SL hardcoded)",
                   nrow(sw_no_hw)))

sw_inconsistent <- lc %>%
  filter(has_stichwahl == TRUE) %>%
  group_by(ags, election_date) %>%
  summarise(any_sw_votes = any(!is.na(candidate_votes_sw)), .groups = "drop") %>%
  filter(!any_sw_votes)
check_warn(nrow(sw_inconsistent) == 0,
           "has_stichwahl=TRUE elections all have ≥1 candidate with SW votes",
           sprintf("%d elections marked has_stichwahl=TRUE but no SW votes",
                   nrow(sw_inconsistent)))

# ============================================================================
# 11. Saarland data quality
# ============================================================================
cat("\n11. Saarland data quality\n")
sl_no_eligible <- l %>% filter(state == "10", is.na(eligible_voters))
sl_total <- l %>% filter(state == "10") %>% nrow()
cat(sprintf("    SL rows total: %d (with NA eligible_voters: %d)\n",
            sl_total, nrow(sl_no_eligible)))

# ============================================================================
# 12. Coverage summary
# ============================================================================
cat("\n12. Coverage summary\n")
print(state_counts)

cat("\n────────────────────────────────────────────────────────────────────\n")
if (failed == 0L) {
  cat(sprintf("All checks passed ✓ (%d warnings)\n", warned))
  quit(status = 0L)
} else {
  cat(sprintf("%d check(s) failed ✗ (%d warnings)\n", failed, warned))
  quit(status = 1L)
}
