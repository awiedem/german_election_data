### Rigorous checks for all mayor panel datasets
# Date: March 2026
#
# Validates:
# - mayor_panel.rds             (unharmonized, election-level)
# - mayor_panel_annual.rds      (unharmonized, annual)
# - mayor_panel_harm.rds        (harmonized, election-level)
# - mayor_panel_annual_harm.rds (harmonized, annual)

rm(list = ls())

options(scipen = 999)

pacman::p_load(
  "tidyverse",
  "data.table",
  "lubridate",
  "conflicted"
)

conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")

pass_count <- 0
fail_count <- 0
warn_count <- 0

check_result <- function(name, passed, detail = "") {
  if (passed) {
    cat(sprintf("  PASS: %s\n", name))
    pass_count <<- pass_count + 1
  } else {
    cat(sprintf("  FAIL: %s %s\n", name, detail))
    fail_count <<- fail_count + 1
  }
}

check_warn <- function(name, ok, detail = "") {
  if (ok) {
    cat(sprintf("  OK:   %s\n", name))
  } else {
    cat(sprintf("  WARN: %s %s\n", name, detail))
    warn_count <<- warn_count + 1
  }
}


cat(strrep("=", 72), "\n")
cat("MAYOR PANEL: COMPREHENSIVE DATA QUALITY CHECKS\n")
cat(strrep("=", 72), "\n\n")


# ============================================================================
# LOAD ALL DATASETS
# ============================================================================

cat("Loading datasets...\n")

panel      <- read_rds("data/mayoral_elections/final/mayor_panel.rds") |> as_tibble()
annual     <- read_rds("data/mayoral_elections/final/mayor_panel_annual.rds") |> as_tibble()
panel_h    <- read_rds("data/mayoral_elections/final/mayor_panel_harm.rds") |> as_tibble()
annual_h   <- read_rds("data/mayoral_elections/final/mayor_panel_annual_harm.rds") |> as_tibble()

cat(sprintf("  Unharm election panel: %d rows x %d cols\n", nrow(panel), ncol(panel)))
cat(sprintf("  Unharm annual panel:   %d rows x %d cols\n", nrow(annual), ncol(annual)))
cat(sprintf("  Harm election panel:   %d rows x %d cols\n", nrow(panel_h), ncol(panel_h)))
cat(sprintf("  Harm annual panel:     %d rows x %d cols\n", nrow(annual_h), ncol(annual_h)))
cat("\n")


# ============================================================================
# PART A: SCHEMA CHECKS
# ============================================================================

cat(strrep("=", 72), "\n")
cat("PART A: SCHEMA CHECKS\n")
cat(strrep("=", 72), "\n\n")

# A1: Column presence — unharmonized should NOT have ags_21
check_result("Unharm panel does NOT have ags_21",
             !"ags_21" %in% names(panel))
check_result("Unharm annual does NOT have ags_21",
             !"ags_21" %in% names(annual))

# A2: Column presence — harmonized SHOULD have ags_21
check_result("Harm panel HAS ags_21",
             "ags_21" %in% names(panel_h))
check_result("Harm annual HAS ags_21",
             "ags_21" %in% names(annual_h))

# A3: Expected columns in election-level panel
expected_panel_cols <- c(
  "person_id", "ags", "state", "election_year", "election_date",
  "term_number", "winner_party", "winner_voteshare", "winning_margin",
  "n_candidates", "is_incumbent", "next_runs_again",
  "tenure_start", "years_in_office", "term_start_date",
  "n_terms", "total_tenure_years", "has_margin_variation"
)
missing_unharm <- setdiff(expected_panel_cols, names(panel))
missing_harm <- setdiff(expected_panel_cols, names(panel_h))
check_result("Unharm panel has all expected columns",
             length(missing_unharm) == 0,
             sprintf("— missing: %s", paste(missing_unharm, collapse = ", ")))
check_result("Harm panel has all expected columns",
             length(missing_harm) == 0,
             sprintf("— missing: %s", paste(missing_harm, collapse = ", ")))

# A4: Expected columns in annual panel
expected_annual_cols <- c(
  "ags", "year", "person_id", "state", "election_year", "election_date",
  "term_number", "winner_party", "winner_voteshare", "winning_margin",
  "n_candidates", "is_incumbent", "next_runs_again",
  "years_since_election", "years_to_next_election",
  "electoral_cycle_pos", "tenure_start", "term_start_date"
)
missing_a_unharm <- setdiff(expected_annual_cols, names(annual))
missing_a_harm <- setdiff(expected_annual_cols, names(annual_h))
check_result("Unharm annual has all expected columns",
             length(missing_a_unharm) == 0,
             sprintf("— missing: %s", paste(missing_a_unharm, collapse = ", ")))
check_result("Harm annual has all expected columns",
             length(missing_a_harm) == 0,
             sprintf("— missing: %s", paste(missing_a_harm, collapse = ", ")))

# A5: Row counts — harm should be <= unharm (crosswalk may drop rows)
check_result("Harm panel rows <= unharm panel rows",
             nrow(panel_h) <= nrow(panel),
             sprintf("— harm %d > unharm %d", nrow(panel_h), nrow(panel)))
check_result("Harm annual rows <= unharm annual rows",
             nrow(annual_h) <= nrow(annual),
             sprintf("— harm %d > unharm %d", nrow(annual_h), nrow(annual)))

cat("\n")


# ============================================================================
# PART B: ELECTION-LEVEL PANEL CHECKS (run on both versions)
# ============================================================================

run_panel_checks <- function(p, label) {
  cat(strrep("=", 72), "\n")
  cat(sprintf("PART B: ELECTION-LEVEL CHECKS [%s]\n", label))
  cat(strrep("=", 72), "\n\n")

  # B1: No duplicate (person_id, ags, election_year)
  cat("--- B1: Uniqueness ---\n")
  dupes <- p |>
    group_by(person_id, ags, election_year) |>
    filter(n() > 1) |>
    ungroup()
  check_result(sprintf("[%s] No duplicate (person_id, ags, election_year)", label),
               nrow(dupes) == 0,
               sprintf("— %d duplicates", nrow(dupes)))
  if (nrow(dupes) > 0) {
    print(dupes |> select(person_id, ags, election_year, winner_party) |> head(10))
  }

  # Also check just (person_id, election_year)
  dupes2 <- p |>
    group_by(person_id, election_year) |>
    filter(n() > 1) |>
    ungroup()
  check_result(sprintf("[%s] No duplicate (person_id, election_year)", label),
               nrow(dupes2) == 0,
               sprintf("— %d duplicates", nrow(dupes2)))

  # B2: term_number sequential
  cat("\n--- B2: Term number sequential ---\n")
  seq_check <- p |>
    arrange(person_id, ags, election_date) |>
    group_by(person_id, ags) |>
    mutate(expected_term = row_number()) |>
    ungroup() |>
    filter(term_number != expected_term)
  check_result(sprintf("[%s] term_number sequential within (person_id, ags)", label),
               nrow(seq_check) == 0,
               sprintf("— %d mismatches", nrow(seq_check)))

  # B3: person_id within single state
  cat("\n--- B3: Person within single state ---\n")
  multi_state <- p |>
    group_by(person_id) |>
    summarise(n_states = n_distinct(state), .groups = "drop") |>
    filter(n_states > 1)
  check_result(sprintf("[%s] No person_id spans multiple states", label),
               nrow(multi_state) == 0,
               sprintf("— %d persons", nrow(multi_state)))

  # B4: is_incumbent consistency (bidirectional)
  cat("\n--- B4: is_incumbent consistency ---\n")
  inc1 <- p |> filter(is_incumbent == 1, term_number < 2)
  check_result(sprintf("[%s] is_incumbent=1 implies term_number>=2", label),
               nrow(inc1) == 0,
               sprintf("— %d violations", nrow(inc1)))

  inc2 <- p |> filter(is_incumbent == 0, term_number >= 2)
  check_result(sprintf("[%s] term_number>=2 implies is_incumbent=1", label),
               nrow(inc2) == 0,
               sprintf("— %d violations", nrow(inc2)))

  # B5: winning_margin range
  cat("\n--- B5: Winning margin ---\n")
  margin_stats <- p |>
    filter(!is.na(winning_margin)) |>
    summarise(
      n = n(),
      min_m = min(winning_margin),
      max_m = max(winning_margin),
      mean_m = mean(winning_margin),
      n_neg = sum(winning_margin < 0),
      n_gt1 = sum(winning_margin > 1),
      n_zero = sum(winning_margin == 0),
      n_one = sum(winning_margin == 1)
    )
  cat(sprintf("  Range: [%.4f, %.4f], Mean: %.4f\n",
              margin_stats$min_m, margin_stats$max_m, margin_stats$mean_m))
  cat(sprintf("  Exactly 0: %d, Exactly 1: %d, NA: %d\n",
              margin_stats$n_zero, margin_stats$n_one,
              sum(is.na(p$winning_margin))))
  check_result(sprintf("[%s] No negative winning_margin", label),
               margin_stats$n_neg == 0,
               sprintf("— %d negative", margin_stats$n_neg))
  check_result(sprintf("[%s] No winning_margin > 1", label),
               margin_stats$n_gt1 == 0,
               sprintf("— %d > 1", margin_stats$n_gt1))

  # B6: winner_voteshare range [0, 1]
  cat("\n--- B6: Winner voteshare ---\n")
  vs_stats <- p |>
    filter(!is.na(winner_voteshare)) |>
    summarise(
      min_vs = min(winner_voteshare),
      max_vs = max(winner_voteshare),
      n_neg = sum(winner_voteshare < 0),
      n_gt1 = sum(winner_voteshare > 1)
    )
  cat(sprintf("  Range: [%.4f, %.4f]\n", vs_stats$min_vs, vs_stats$max_vs))
  check_result(sprintf("[%s] winner_voteshare in [0,1]", label),
               vs_stats$n_neg == 0 & vs_stats$n_gt1 == 0,
               sprintf("— %d <0, %d >1", vs_stats$n_neg, vs_stats$n_gt1))

  # B7: No missing critical columns
  cat("\n--- B7: Missing values in critical columns ---\n")
  critical_cols <- c("person_id", "ags", "state", "election_year",
                     "election_date", "term_number", "is_incumbent")
  for (col in critical_cols) {
    n_na <- sum(is.na(p[[col]]))
    check_result(sprintf("[%s] No NA in %s", label, col),
                 n_na == 0,
                 sprintf("— %d NAs", n_na))
  }

  # B8: AGS format — 8 digits, valid state prefix
  cat("\n--- B8: AGS format ---\n")
  valid_states <- c("01", "02", "03", "04", "05", "06", "07", "08",
                    "09", "10", "11", "12", "13", "14", "15", "16")
  ags_wrong_len <- sum(nchar(p$ags) != 8)
  check_result(sprintf("[%s] All AGS are 8 characters", label),
               ags_wrong_len == 0,
               sprintf("— %d wrong length", ags_wrong_len))

  ags_prefixes <- unique(substr(p$ags, 1, 2))
  bad_prefixes <- setdiff(ags_prefixes, valid_states)
  check_result(sprintf("[%s] All AGS have valid state prefix", label),
               length(bad_prefixes) == 0,
               sprintf("— bad: %s", paste(bad_prefixes, collapse = ", ")))

  # B9: State column matches AGS prefix
  cat("\n--- B9: State-AGS consistency ---\n")
  state_mismatch <- p |>
    mutate(ags_state = substr(ags, 1, 2)) |>
    filter(state != ags_state)
  check_result(sprintf("[%s] state matches AGS prefix", label),
               nrow(state_mismatch) == 0,
               sprintf("— %d mismatches", nrow(state_mismatch)))

  # B10: n_terms matches actual count
  cat("\n--- B10: n_terms consistency ---\n")
  actual_terms <- p |>
    group_by(person_id) |>
    summarise(actual_n = n(), recorded_n = first(n_terms), .groups = "drop") |>
    filter(actual_n != recorded_n)
  check_result(sprintf("[%s] n_terms matches actual count", label),
               nrow(actual_terms) == 0,
               sprintf("— %d mismatches", nrow(actual_terms)))

  # B11: total_tenure_years consistent
  cat("\n--- B11: total_tenure_years ---\n")
  tenure_check <- p |>
    group_by(person_id) |>
    summarise(
      actual_span = max(election_year) - min(election_year),
      recorded_span = first(total_tenure_years),
      .groups = "drop"
    ) |>
    filter(actual_span != recorded_span)
  check_result(sprintf("[%s] total_tenure_years matches year span", label),
               nrow(tenure_check) == 0,
               sprintf("— %d mismatches", nrow(tenure_check)))

  # B12: has_margin_variation consistent
  cat("\n--- B12: has_margin_variation ---\n")
  var_check <- p |>
    group_by(person_id) |>
    summarise(
      actual_var = n_distinct(round(winning_margin, 4), na.rm = TRUE) > 1,
      recorded_var = first(has_margin_variation),
      .groups = "drop"
    ) |>
    filter(actual_var != recorded_var)
  check_result(sprintf("[%s] has_margin_variation consistent", label),
               nrow(var_check) == 0,
               sprintf("— %d mismatches", nrow(var_check)))

  # B13: Election year range
  cat("\n--- B13: Election year range ---\n")
  yr_min <- min(p$election_year)
  yr_max <- max(p$election_year)
  cat(sprintf("  Year range: %d - %d\n", yr_min, yr_max))
  check_result(sprintf("[%s] Earliest election >= 1945", label),
               yr_min >= 1945,
               sprintf("— found %d", yr_min))
  check_result(sprintf("[%s] Latest election <= 2025", label),
               yr_max <= 2025,
               sprintf("— found %d", yr_max))

  # B14: next_runs_again values
  cat("\n--- B14: next_runs_again values ---\n")
  nra_vals <- sort(unique(p$next_runs_again[!is.na(p$next_runs_again)]))
  check_result(sprintf("[%s] next_runs_again in {0, 1, NA}", label),
               all(nra_vals %in% c(0, 1)),
               sprintf("— values: %s", paste(nra_vals, collapse = ", ")))

  # B15: years_in_office >= 0 and consistent
  cat("\n--- B15: years_in_office ---\n")
  yio_neg <- sum(p$years_in_office < 0, na.rm = TRUE)
  check_result(sprintf("[%s] years_in_office >= 0", label),
               yio_neg == 0,
               sprintf("— %d negative", yio_neg))
  yio_check <- p |>
    filter(years_in_office != (election_year - tenure_start))
  check_result(sprintf("[%s] years_in_office = election_year - tenure_start", label),
               nrow(yio_check) == 0,
               sprintf("— %d mismatches", nrow(yio_check)))

  # B16: State coverage
  cat("\n--- B16: State distribution ---\n")
  state_summary <- p |>
    group_by(state) |>
    summarise(
      n_elections = n(),
      n_persons = n_distinct(person_id),
      n_munis = n_distinct(ags),
      n_multi_term = n_distinct(person_id[term_number >= 2]),
      pct_multi = round(100 * n_multi_term / n_persons, 1),
      year_range = paste(min(election_year), "-", max(election_year)),
      .groups = "drop"
    )
  print(state_summary)

  cat("\n")
}

# Run on both versions
run_panel_checks(panel, "UNHARM")
run_panel_checks(panel_h, "HARM")

# B-HARM specific: ags_21 populated and valid
cat(strrep("=", 72), "\n")
cat("PART B-HARM: AGS_21 CHECKS\n")
cat(strrep("=", 72), "\n\n")

n_no_ags21 <- sum(is.na(panel_h$ags_21))
check_result("Harm panel: all rows have ags_21",
             n_no_ags21 == 0,
             sprintf("— %d missing", n_no_ags21))

ags21_wrong_len <- sum(nchar(panel_h$ags_21) != 8)
check_result("Harm panel: all ags_21 are 8 characters",
             ags21_wrong_len == 0,
             sprintf("— %d wrong length", ags21_wrong_len))

# Check that ags_21 state prefix matches state column
ags21_mismatch <- panel_h |>
  mutate(ags21_state = substr(ags_21, 1, 2)) |>
  filter(state != ags21_state)
check_result("Harm panel: ags_21 prefix matches state",
             nrow(ags21_mismatch) == 0,
             sprintf("— %d mismatches", nrow(ags21_mismatch)))

cat("\n")


# ============================================================================
# PART C: ANNUAL PANEL CHECKS (run on both versions)
# ============================================================================

run_annual_checks <- function(a, p, label) {
  cat(strrep("=", 72), "\n")
  cat(sprintf("PART C: ANNUAL PANEL CHECKS [%s]\n", label))
  cat(strrep("=", 72), "\n\n")

  # C1: Each person-election appears in the annual panel
  cat("--- C1: Election coverage ---\n")
  panel_keys <- p |> distinct(person_id, ags, election_year)
  annual_elec_keys <- a |>
    filter(year == election_year) |>
    distinct(person_id, ags, election_year)
  missing <- panel_keys |>
    anti_join(annual_elec_keys, by = c("person_id", "ags", "election_year"))
  check_result(sprintf("[%s] All elections appear in annual panel", label),
               nrow(missing) == 0,
               sprintf("— %d missing", nrow(missing)))

  # C2: No year gaps within (person_id, ags, election_year) groups
  cat("\n--- C2: Year continuity ---\n")
  year_gaps <- a |>
    arrange(person_id, ags, election_year, year) |>
    group_by(person_id, ags, election_year) |>
    mutate(year_diff = year - lag(year, default = year[1])) |>
    ungroup() |>
    filter(year_diff > 1)
  check_result(sprintf("[%s] No year gaps in annual panel", label),
               nrow(year_gaps) == 0,
               sprintf("— %d gaps", nrow(year_gaps)))

  # C3: No duplicate (ags, year, person_id)
  cat("\n--- C3: Uniqueness ---\n")
  a_dupes <- a |>
    group_by(ags, year, person_id) |>
    filter(n() > 1) |>
    ungroup()
  check_result(sprintf("[%s] No duplicate (ags, year, person_id)", label),
               nrow(a_dupes) == 0,
               sprintf("— %d duplicates", nrow(a_dupes)))

  # C4: years_since_election correct
  cat("\n--- C4: years_since_election ---\n")
  yse_check <- a |>
    filter(years_since_election != (year - election_year))
  check_result(sprintf("[%s] years_since_election = year - election_year", label),
               nrow(yse_check) == 0,
               sprintf("— %d mismatches", nrow(yse_check)))

  # C5: years_since_election >= 0
  yse_neg <- sum(a$years_since_election < 0, na.rm = TRUE)
  check_result(sprintf("[%s] years_since_election >= 0", label),
               yse_neg == 0,
               sprintf("— %d negative", yse_neg))

  # C6: years_to_next_election decreases by 1 each year
  cat("\n--- C6: years_to_next_election consistency ---\n")
  ytne_check <- a |>
    filter(!is.na(years_to_next_election)) |>
    arrange(person_id, ags, election_year, year) |>
    group_by(person_id, ags, election_year) |>
    mutate(ytne_diff = lag(years_to_next_election) - years_to_next_election) |>
    ungroup() |>
    filter(!is.na(ytne_diff), ytne_diff != 1)
  check_result(sprintf("[%s] years_to_next_election decreases by 1", label),
               nrow(ytne_check) == 0,
               sprintf("— %d violations", nrow(ytne_check)))

  # C7: electoral_cycle_pos in [0, 1]
  cat("\n--- C7: electoral_cycle_pos range ---\n")
  ecp_range <- a |>
    filter(!is.na(electoral_cycle_pos)) |>
    summarise(
      min_ecp = min(electoral_cycle_pos),
      max_ecp = max(electoral_cycle_pos),
      n_neg = sum(electoral_cycle_pos < 0),
      n_gt1 = sum(electoral_cycle_pos > 1)
    )
  cat(sprintf("  Range: [%.4f, %.4f]\n", ecp_range$min_ecp, ecp_range$max_ecp))
  check_result(sprintf("[%s] electoral_cycle_pos in [0, 1)", label),
               ecp_range$n_neg == 0 & ecp_range$n_gt1 == 0,
               sprintf("— %d <0, %d >1", ecp_range$n_neg, ecp_range$n_gt1))

  # C8: Year range
  cat("\n--- C8: Year range ---\n")
  cat(sprintf("  Year range: %d - %d\n", min(a$year), max(a$year)))
  check_result(sprintf("[%s] Annual panel years <= 2025", label),
               max(a$year) <= 2025,
               sprintf("— max year = %d", max(a$year)))

  # C9: No missing critical columns
  cat("\n--- C9: Missing values ---\n")
  annual_critical <- c("ags", "year", "person_id", "state",
                       "election_year", "election_date", "term_number")
  for (col in annual_critical) {
    n_na <- sum(is.na(a[[col]]))
    check_result(sprintf("[%s] No NA in annual.%s", label, col),
                 n_na == 0,
                 sprintf("— %d NAs", n_na))
  }

  # C10: Forward-fill consistency — static fields unchanged within term
  cat("\n--- C10: Static fields constant within term ---\n")
  static_check <- a |>
    group_by(person_id, ags, election_year) |>
    summarise(
      n_party = n_distinct(winner_party, na.rm = TRUE),
      n_vs = n_distinct(winner_voteshare, na.rm = TRUE),
      n_margin = n_distinct(winning_margin, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(n_party > 1 | n_vs > 1 | n_margin > 1)
  check_result(sprintf("[%s] Static fields constant within term", label),
               nrow(static_check) == 0,
               sprintf("— %d terms with variation", nrow(static_check)))

  cat("\n")
}

run_annual_checks(annual, panel, "UNHARM")
run_annual_checks(annual_h, panel_h, "HARM")

# C-HARM specific: ags_21 in annual
cat(strrep("=", 72), "\n")
cat("PART C-HARM: AGS_21 IN ANNUAL\n")
cat(strrep("=", 72), "\n\n")

n_no_ags21_annual <- sum(is.na(annual_h$ags_21))
check_result("Harm annual: all rows have ags_21",
             n_no_ags21_annual == 0,
             sprintf("— %d missing", n_no_ags21_annual))

cat("\n")


# ============================================================================
# PART D: CROSS-VERSION CONSISTENCY
# ============================================================================

cat(strrep("=", 72), "\n")
cat("PART D: CROSS-VERSION CONSISTENCY\n")
cat(strrep("=", 72), "\n\n")

# D1: Harmonized person_ids are a subset of unharmonized
harm_persons <- unique(panel_h$person_id)
unharm_persons <- unique(panel$person_id)
extra_harm <- setdiff(harm_persons, unharm_persons)
check_result("Harm person_ids subset of unharm",
             length(extra_harm) == 0,
             sprintf("— %d extra in harm", length(extra_harm)))

# D2: Number of dropped rows is reasonable
n_dropped <- nrow(panel) - nrow(panel_h)
pct_dropped <- 100 * n_dropped / nrow(panel)
cat(sprintf("  Rows dropped by crosswalk: %d (%.1f%%)\n", n_dropped, pct_dropped))
check_warn("Crosswalk drops < 10%",
           pct_dropped < 10,
           sprintf("— %.1f%% dropped", pct_dropped))

# D3: State distributions roughly match
cat("\n  State row counts (unharm vs harm):\n")
unharm_by_state <- panel |> count(state, name = "n_unharm")
harm_by_state <- panel_h |> count(state, name = "n_harm")
state_comp <- full_join(unharm_by_state, harm_by_state, by = "state") |>
  mutate(
    n_unharm = replace_na(n_unharm, 0),
    n_harm = replace_na(n_harm, 0),
    diff = n_unharm - n_harm,
    pct_diff = round(100 * diff / n_unharm, 1)
  )
print(state_comp)

# D4: Where the same (person_id, election_year) appears in both,
# election-level data should match
cat("\n")
common <- inner_join(
  panel |> select(person_id, ags, election_year,
                  winner_party_u = winner_party,
                  winner_voteshare_u = winner_voteshare),
  panel_h |> select(person_id, ags, election_year,
                    winner_party_h = winner_party,
                    winner_voteshare_h = winner_voteshare),
  by = c("person_id", "ags", "election_year")
)
party_mismatch <- common |>
  filter(winner_party_u != winner_party_h |
           abs(winner_voteshare_u - winner_voteshare_h) > 0.001)
check_result("Common rows: election data matches across versions",
             nrow(party_mismatch) == 0,
             sprintf("— %d mismatches", nrow(party_mismatch)))

cat("\n")


# ============================================================================
# PART E: BAYERN-SPECIFIC CHECKS
# ============================================================================

cat(strrep("=", 72), "\n")
cat("PART E: BAYERN-SPECIFIC CHECKS\n")
cat(strrep("=", 72), "\n\n")

bayern <- panel |> filter(state == "09")
bayern_persons <- bayern |>
  group_by(person_id) |>
  summarise(
    n_terms = n(),
    tenure_span = max(election_year) - min(election_year),
    .groups = "drop"
  )

cat(sprintf("  Unique persons: %d\n", n_distinct(bayern$person_id)))
cat(sprintf("  Person-elections: %d\n", nrow(bayern)))
cat(sprintf("  Mean terms: %.2f\n", mean(bayern_persons$n_terms)))
cat(sprintf("  Max terms: %d\n", max(bayern_persons$n_terms)))
cat(sprintf("  Max tenure span: %d years\n", max(bayern_persons$tenure_span)))
cat(sprintf("  Unique AGS: %d\n", n_distinct(bayern$ags)))

# Bayern has ~2056 municipalities
check_result("Bayern has >10,000 person-elections",
             nrow(bayern) > 10000,
             sprintf("— found %d", nrow(bayern)))

check_result("Bayern has >1500 unique AGS",
             n_distinct(bayern$ags) > 1500,
             sprintf("— found %d", n_distinct(bayern$ags)))

# No implausibly long tenures
n_very_long <- sum(bayern_persons$tenure_span > 42)
check_warn("No Bayern tenures > 42 years",
           n_very_long == 0,
           sprintf("— %d persons", n_very_long))

# Term distribution
cat("\n  Term distribution:\n")
term_dist <- bayern_persons |> count(n_terms) |> arrange(n_terms)
for (i in seq_len(nrow(term_dist))) {
  cat(sprintf("    %d terms: %d persons\n", term_dist$n_terms[i], term_dist$n[i]))
}

# Bayern election years should cluster around known cycles (6-year)
cat("\n  Election year distribution (first 10):\n")
print(bayern |> count(election_year) |> head(10))

cat("\n")


# ============================================================================
# PART F: NAMED-STATE CHECKS
# ============================================================================

cat(strrep("=", 72), "\n")
cat("PART F: NAMED-STATE CHECKS\n")
cat(strrep("=", 72), "\n\n")

named_states <- panel |> filter(state != "09")

# F1: Person ID format — named states should have name-based IDs
name_based <- named_states |>
  filter(str_detect(person_id, "^p_\\d{2}_[a-z]"))
noname_based <- named_states |>
  filter(!str_detect(person_id, "^p_\\d{2}_[a-z]"))

cat(sprintf("  Named-state elections with name-based ID: %d (%.1f%%)\n",
            nrow(name_based),
            100 * nrow(name_based) / nrow(named_states)))
cat(sprintf("  Named-state elections without name-based ID: %d\n",
            nrow(noname_based)))

# F2: Check major states have reasonable counts
for (s in c("03", "05", "07", "14")) {
  sub <- panel |> filter(state == s)
  state_name <- case_when(
    s == "03" ~ "NI", s == "05" ~ "NRW",
    s == "07" ~ "RLP", s == "14" ~ "SN"
  )
  check_result(sprintf("State %s (%s) has >50 elections", s, state_name),
               nrow(sub) > 50,
               sprintf("— found %d", nrow(sub)))
}

cat("\n")


# ============================================================================
# PART G: DATA INTEGRITY SPOT CHECKS
# ============================================================================

cat(strrep("=", 72), "\n")
cat("PART G: DATA INTEGRITY SPOT CHECKS\n")
cat(strrep("=", 72), "\n\n")

# G1: election_date is a valid Date
cat("--- G1: Date validity ---\n")
check_result("election_date is Date class",
             inherits(panel$election_date, "Date"))
check_result("election_date in annual is Date class",
             inherits(annual$election_date, "Date"))

# G2: election_year matches year of election_date
cat("\n--- G2: Year-date consistency ---\n")
yr_date_mismatch <- panel |>
  filter(!is.na(election_date)) |>
  filter(year(election_date) != election_year)
check_result("election_year matches year(election_date)",
             nrow(yr_date_mismatch) == 0,
             sprintf("— %d mismatches", nrow(yr_date_mismatch)))

# G3: Reasonable winner_party values (no NA for winners)
cat("\n--- G3: Winner party ---\n")
n_na_party <- sum(is.na(panel$winner_party))
cat(sprintf("  NA winner_party: %d of %d (%.1f%%)\n",
            n_na_party, nrow(panel), 100 * n_na_party / nrow(panel)))
# Check most common parties
top_parties <- panel |>
  filter(!is.na(winner_party)) |>
  count(winner_party, sort = TRUE) |>
  head(10)
cat("  Top winner parties:\n")
for (i in seq_len(nrow(top_parties))) {
  cat(sprintf("    %s: %d\n", top_parties$winner_party[i], top_parties$n[i]))
}

# G4: n_candidates >= 1
cat("\n--- G4: n_candidates ---\n")
n_cand_lt1 <- sum(panel$n_candidates < 1, na.rm = TRUE)
n_cand_na <- sum(is.na(panel$n_candidates))
check_result("n_candidates >= 1 (non-NA)",
             n_cand_lt1 == 0,
             sprintf("— %d < 1", n_cand_lt1))
cat(sprintf("  NA n_candidates: %d\n", n_cand_na))

cat("\n")


# ============================================================================
# SUMMARY
# ============================================================================

cat(strrep("=", 72), "\n")
cat(sprintf("FINAL SUMMARY: %d PASS, %d FAIL, %d WARNINGS\n",
            pass_count, fail_count, warn_count))
cat(strrep("=", 72), "\n")

if (fail_count > 0) {
  cat("\n*** ATTENTION: There are FAILing checks! ***\n")
} else {
  cat("\nAll checks passed.\n")
}
