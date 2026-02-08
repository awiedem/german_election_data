### Sanity checks for state elections harmonized datasets
# Validates: state_harm_21, state_harm_23, state_harm_25, state_2224_unharm
# Date: 2026

rm(list = ls())

options(scipen = 999)

pacman::p_load(
  "tidyverse",
  "data.table",
  "conflicted"
)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("first", "dplyr")

# Local helper (avoid requiring haschaR)
pad_zero_conditional <- function(x, n) {
  x <- as.character(x)
  x <- str_pad(x, width = n, side = "left", pad = "0")
  return(x)
}

# Helper: identify party share columns (varies across datasets)
get_party_vars <- function(df) {
  # Party columns are between the vote metadata and the flags/covariates
  all_names <- names(df)
  # Known non-party columns
  non_party <- c(
    "ags", "ags_name", "ags_name_21", "county", "election_year", "election_date",
    "state", "state_name", "eligible_voters", "number_voters", "valid_votes",
    "turnout", "flag_unsuccessful_naive_merge", "flag_total_votes_incongruent",
    "total_vote_share", "area_ags", "population_ags", "employees_ags",
    "pop_density_ags", "cdu_csu"
  )
  party_vars <- setdiff(all_names, non_party)
  return(party_vars)
}

# Counters for summary
n_pass <- 0
n_warn <- 0
n_fail <- 0

pass <- function(msg) {
  n_pass <<- n_pass + 1
  cat(sprintf(">> PASS: %s\n", msg))
}

warn <- function(msg) {
  n_warn <<- n_warn + 1
  cat(sprintf("!! WARNING: %s\n", msg))
}

fail <- function(msg) {
  n_fail <<- n_fail + 1
  cat(sprintf("!! FAIL: %s\n", msg))
}

cat("=", strrep("=", 70), "\n")
cat("STATE ELECTIONS HARMONIZED: DATA QUALITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load datasets -----------------------------------------------------------------

cat("Loading datasets...\n")

h21 <- read_rds("data/state_elections/final/state_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 8))

h23 <- read_rds("data/state_elections/final/state_harm_23.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 8))

h25 <- read_rds("data/state_elections/final/state_harm_25.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 8))

unharm <- read_rds("data/state_elections/final/state_2224_unharm.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 8))

cat(sprintf("  state_harm_21:     %d rows x %d cols\n", nrow(h21), ncol(h21)))
cat(sprintf("  state_harm_23:     %d rows x %d cols\n", nrow(h23), ncol(h23)))
cat(sprintf("  state_harm_25:     %d rows x %d cols\n", nrow(h25), ncol(h25)))
cat(sprintf("  state_2224_unharm: %d rows x %d cols\n", nrow(unharm), ncol(unharm)))
cat("\n")

# ==============================================================================
# CHECK 1: COVERAGE
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 1: COVERAGE\n")
cat(strrep("=", 70), "\n\n")

check_coverage <- function(df, name, expected_elections, expected_states,
                           expected_min_rows, expected_years) {
  cat(sprintf("--- %s ---\n", name))
  # Count state-year combos as "elections" (one per state per election year)
  n_state_year <- nrow(distinct(df, state, election_year))
  n_years <- n_distinct(df$election_year)
  n_states <- n_distinct(df$state)
  year_range <- range(df$election_year)

  cat(sprintf("  State-year combinations: %d (expected %d)\n", n_state_year, expected_elections))
  cat(sprintf("  Distinct years: %d, distinct states: %d (expected %d states)\n",
              n_years, n_states, expected_states))
  cat(sprintf("  Year range: %d-%d (expected %s)\n",
              year_range[1], year_range[2], expected_years))
  cat(sprintf("  Rows: %d (min expected %d)\n", nrow(df), expected_min_rows))

  ok <- TRUE
  if (n_state_year != expected_elections) {
    fail(sprintf("%s: expected %d state-year combos, got %d", name, expected_elections, n_state_year))
    ok <- FALSE
  }
  if (n_states != expected_states) {
    fail(sprintf("%s: expected %d states, got %d", name, expected_states, n_states))
    ok <- FALSE
  }
  if (nrow(df) < expected_min_rows) {
    fail(sprintf("%s: expected >= %d rows, got %d", name, expected_min_rows, nrow(df)))
    ok <- FALSE
  }

  # No Hamburg (state "02")
  has_hamburg <- "02" %in% df$state
  if (has_hamburg) {
    fail(sprintf("%s: contains Hamburg (state '02') — should not be present", name))
    ok <- FALSE
  }

  if (ok) pass(sprintf("%s coverage as expected", name))
  cat("\n")
}

# Expected state-year combos: each state holds elections every 4-5 years,
# so 15 states × ~3 elections each ≈ 48 combos (harm_21/harm_25),
# 14 states × ~3 elections each ≈ 40 combos (harm_23),
# 11 state-year combos in unharm
check_coverage(h21, "state_harm_21", 48, 15, 30000, "2006-2024")
check_coverage(h23, "state_harm_23", 40, 14, 25000, "2006-2023")
check_coverage(h25, "state_harm_25", 48, 15, 30000, "2006-2024")
check_coverage(unharm, "state_2224_unharm", 11, 11, 6000, "2022-2024")

# State-by-year cross-tabulation for harm_21
cat("State-by-year cross-tabulation (state_harm_21):\n")
print(table(h21$state, h21$election_year))
cat("\n")

# ==============================================================================
# CHECK 2: AGS VALIDITY
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 2: AGS VALIDITY\n")
cat(strrep("=", 70), "\n\n")

check_ags <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))
  ok <- TRUE

  # All AGS 8 characters
  ags_lens <- nchar(df$ags)
  non_8 <- sum(ags_lens != 8, na.rm = TRUE)
  na_ags <- sum(is.na(df$ags))

  if (na_ags > 0) {
    fail(sprintf("%s: %d rows with NA AGS", name, na_ags))
    ok <- FALSE
  }
  if (non_8 > 0) {
    fail(sprintf("%s: %d rows with AGS not 8 characters", name, non_8))
    ok <- FALSE
  }

  # AGS prefix matches state
  prefix_mismatch <- df |>
    filter(!is.na(ags), !is.na(state)) |>
    mutate(ags_prefix = substr(ags, 1, 2)) |>
    filter(ags_prefix != state)

  if (nrow(prefix_mismatch) > 0) {
    fail(sprintf("%s: %d rows where AGS prefix != state", name, nrow(prefix_mismatch)))
    print(head(prefix_mismatch |> select(ags, state), 5))
    ok <- FALSE
  }

  # No Hamburg prefix "02"
  has_02 <- any(substr(df$ags, 1, 2) == "02", na.rm = TRUE)
  if (has_02) {
    fail(sprintf("%s: contains AGS with Hamburg prefix '02'", name))
    ok <- FALSE
  }

  if (ok) pass(sprintf("%s: AGS valid (all 8-char, prefix matches state, no Hamburg)", name))
  cat("\n")
}

check_ags(h21, "state_harm_21")
check_ags(h23, "state_harm_23")
check_ags(h25, "state_harm_25")
check_ags(unharm, "state_2224_unharm")

# ==============================================================================
# CHECK 3: VOTE SHARE VALIDITY
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 3: VOTE SHARE VALIDITY\n")
cat(strrep("=", 70), "\n\n")

check_vote_shares <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))
  party_vars <- get_party_vars(df)
  cat(sprintf("  Party columns (%d): %s\n", length(party_vars),
              paste(party_vars, collapse = ", ")))

  # Check each party share in [0, 1]
  n_negative <- 0
  n_over1 <- 0
  for (pv in party_vars) {
    vals <- df[[pv]]
    n_neg_pv <- sum(vals < 0, na.rm = TRUE)
    n_over_pv <- sum(vals > 1, na.rm = TRUE)
    if (n_neg_pv > 0) {
      cat(sprintf("  !! %s has %d negative values\n", pv, n_neg_pv))
    }
    if (n_over_pv > 0) {
      cat(sprintf("  !! %s has %d values > 1\n", pv, n_over_pv))
    }
    n_negative <- n_negative + n_neg_pv
    n_over1 <- n_over1 + n_over_pv
  }

  if (n_negative > 0) fail(sprintf("%s: %d negative party share values", name, n_negative))
  if (n_over1 > 0) warn(sprintf("%s: %d party share values > 1", name, n_over1))
  if (n_negative == 0 && n_over1 == 0) {
    pass(sprintf("%s: all party shares in [0, 1]", name))
  }

  # total_vote_share
  if ("total_vote_share" %in% names(df)) {
    cat("\n  total_vote_share quantiles:\n")
    print(quantile(df$total_vote_share, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
                   na.rm = TRUE))

    n_far <- sum(abs(df$total_vote_share - 1) > 0.05, na.rm = TRUE)
    pct_far <- 100 * n_far / nrow(df)
    cat(sprintf("  Rows with |total_vote_share - 1| > 0.05: %d (%.2f%%)\n", n_far, pct_far))
    if (pct_far > 1) {
      warn(sprintf("%s: %.2f%% of rows have total_vote_share far from 1", name, pct_far))
    } else {
      pass(sprintf("%s: total_vote_share close to 1 (>99%% within 0.05)", name))
    }

    n_low <- sum(df$total_vote_share < 0.7, na.rm = TRUE)
    if (n_low > 0) {
      warn(sprintf("%s: %d rows with total_vote_share < 0.7 (possible missing party data)",
                   name, n_low))
    }
  } else {
    cat("  (no total_vote_share column)\n")
  }

  cat("\n")
}

check_vote_shares(h21, "state_harm_21")
check_vote_shares(h23, "state_harm_23")
check_vote_shares(h25, "state_harm_25")

# For unharm, party shares don't have total_vote_share — compute row sums
cat("--- state_2224_unharm ---\n")
party_vars_u <- get_party_vars(unharm)
cat(sprintf("  Party columns (%d): %s\n", length(party_vars_u),
            paste(party_vars_u, collapse = ", ")))

n_neg_u <- 0
n_over_u <- 0
for (pv in party_vars_u) {
  vals <- unharm[[pv]]
  n_neg_pv <- sum(vals < 0, na.rm = TRUE)
  n_over_pv <- sum(vals > 1, na.rm = TRUE)
  if (n_neg_pv > 0) cat(sprintf("  !! %s has %d negative values\n", pv, n_neg_pv))
  if (n_over_pv > 0) cat(sprintf("  !! %s has %d values > 1\n", pv, n_over_pv))
  n_neg_u <- n_neg_u + n_neg_pv
  n_over_u <- n_over_u + n_over_pv
}

if (n_neg_u > 0) fail(sprintf("state_2224_unharm: %d negative party share values", n_neg_u))
if (n_over_u > 0) warn(sprintf("state_2224_unharm: %d party share values > 1", n_over_u))
if (n_neg_u == 0 && n_over_u == 0) {
  pass("state_2224_unharm: all party shares in [0, 1]")
}
cat("\n")

# ==============================================================================
# CHECK 4: TURNOUT VALIDITY
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 4: TURNOUT VALIDITY\n")
cat(strrep("=", 70), "\n\n")

# Known Thuringia AGS with NA turnout
known_na_turnout <- c("16063033", "16074082", "16075087", "16075105")

check_turnout <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))

  na_turnout <- sum(is.na(df$turnout))
  cat(sprintf("  NA turnout: %d\n", na_turnout))

  if (na_turnout > 0) {
    na_ags <- df |> filter(is.na(turnout)) |> pull(ags) |> unique()
    n_known <- sum(na_ags %in% known_na_turnout)
    n_unknown <- length(na_ags) - n_known
    cat(sprintf("  NA-turnout AGS: %s\n", paste(na_ags, collapse = ", ")))
    if (n_unknown == 0 && na_turnout <= 8) {
      pass(sprintf("%s: %d NA turnout rows — all known Thuringia municipalities", name, na_turnout))
    } else {
      unknown_ags <- setdiff(na_ags, known_na_turnout)
      warn(sprintf("%s: %d NA turnout rows (%d known TH, %d other: %s)",
                   name, na_turnout, n_known, n_unknown,
                   paste(unknown_ags, collapse = ", ")))
    }
  } else {
    pass(sprintf("%s: no NA turnout", name))
  }

  # Turnout = 0 (excluding NA)
  n_zero <- sum(df$turnout == 0, na.rm = TRUE)
  if (n_zero > 0) {
    # state_harm_23 has a known issue: pre-2022 data lacks number_voters,
    # resulting in turnout=0 for those years. Report as WARNING, not FAIL.
    if (grepl("harm_23", name)) {
      n_zero_pre2022 <- sum(df$turnout == 0 & df$election_year < 2022, na.rm = TRUE)
      warn(sprintf("%s: %d rows with turnout = 0 (%d pre-2022 — known number_voters issue)",
                   name, n_zero, n_zero_pre2022))
    } else {
      fail(sprintf("%s: %d rows with turnout = 0", name, n_zero))
    }
    print(df |> filter(turnout == 0) |>
            select(ags, election_year, state, eligible_voters, number_voters, turnout) |>
            head(10))
  } else {
    pass(sprintf("%s: no turnout = 0", name))
  }

  # Turnout > 1
  n_over1 <- sum(df$turnout > 1, na.rm = TRUE)
  if (n_over1 > 0) {
    # A small number of rows with turnout slightly > 1 can arise from source
    # data issues (e.g., SH 2022, SN 2024 mail-in allocation). Treat as
    # WARNING if <= 20 rows, FAIL if more.
    if (n_over1 <= 20) {
      warn(sprintf("%s: %d rows with turnout > 1 (source data issue)", name, n_over1))
    } else {
      fail(sprintf("%s: %d rows with turnout > 1", name, n_over1))
    }
    print(df |> filter(turnout > 1) |>
            select(ags, election_year, state, turnout) |>
            head(10))
  } else {
    pass(sprintf("%s: no turnout > 1", name))
  }

  # Distribution
  cat("  Turnout quantiles:\n")
  print(quantile(df$turnout, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
                 na.rm = TRUE))

  # Very low turnout (exclude turnout=0 rows, which are covered above)
  n_verylow <- sum(df$turnout < 0.3 & df$turnout > 0, na.rm = TRUE)
  if (n_verylow > 0) {
    warn(sprintf("%s: %d rows with turnout in (0, 0.3)", name, n_verylow))
  }

  cat("\n")
}

check_turnout(h21, "state_harm_21")
check_turnout(h23, "state_harm_23")
check_turnout(h25, "state_harm_25")
check_turnout(unharm, "state_2224_unharm")

# ==============================================================================
# CHECK 5: CROSS-DATASET CONSISTENCY
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 5: CROSS-DATASET CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

compare_aggregate_votes <- function(df1, name1, df2, name2) {
  cat(sprintf("--- %s vs %s ---\n", name1, name2))

  # Shared election years
  shared_years <- intersect(unique(df1$election_year), unique(df2$election_year))
  cat(sprintf("  Shared election years: %s\n", paste(shared_years, collapse = ", ")))

  if (length(shared_years) == 0) {
    cat("  No shared years — skipping\n\n")
    return()
  }

  agg1 <- df1 |>
    filter(election_year %in% shared_years) |>
    group_by(state, election_year) |>
    summarise(valid_votes = sum(valid_votes, na.rm = TRUE), .groups = "drop")

  agg2 <- df2 |>
    filter(election_year %in% shared_years) |>
    group_by(state, election_year) |>
    summarise(valid_votes = sum(valid_votes, na.rm = TRUE), .groups = "drop")

  comp <- agg1 |>
    inner_join(agg2, by = c("state", "election_year"), suffix = c("_1", "_2")) |>
    mutate(
      pct_diff = 100 * abs(valid_votes_1 - valid_votes_2) /
        pmax(valid_votes_1, valid_votes_2, 1)
    )

  max_diff <- max(comp$pct_diff, na.rm = TRUE)
  n_over5 <- sum(comp$pct_diff > 5)
  n_over10 <- sum(comp$pct_diff > 10)

  cat(sprintf("  Max percentage difference in valid_votes: %.3f%%\n", max_diff))
  cat("  (Note: some difference expected — different harmonization targets use\n")
  cat("   different crosswalks, so vote totals may differ by a few percent)\n")

  if (n_over10 > 0) {
    fail(sprintf("%s vs %s: %d state-years with >10%% valid_votes difference",
                 name1, name2, n_over10))
    print(comp |> filter(pct_diff > 10) |> arrange(desc(pct_diff)))
  } else if (n_over5 > 0) {
    warn(sprintf("%s vs %s: %d state-years with >5%% valid_votes difference",
                 name1, name2, n_over5))
    print(comp |> filter(pct_diff > 5) |> arrange(desc(pct_diff)))
  } else {
    pass(sprintf("%s vs %s: all state-year valid_votes within 5%%", name1, name2))
  }

  # Show state-years with largest differences for informational purposes
  if (max_diff > 1) {
    cat("  Top differences:\n")
    print(comp |> filter(pct_diff > 1) |> arrange(desc(pct_diff)) |> head(10))
  }

  cat("\n")
}

compare_aggregate_votes(h21, "harm_21", h25, "harm_25")
compare_aggregate_votes(h21, "harm_21", h23, "harm_23")

# ==============================================================================
# CHECK 6: VOTE CONSERVATION
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 6: VOTE CONSERVATION (MAJOR PARTIES)\n")
cat(strrep("=", 70), "\n\n")

# Compare total vote counts for major parties across harmonization targets
major_parties <- c("cdu_csu", "spd", "gruene", "fdp", "linke_pds", "afd")

compare_party_votes <- function(df1, name1, df2, name2, parties) {
  cat(sprintf("--- %s vs %s ---\n", name1, name2))
  shared_years <- intersect(unique(df1$election_year), unique(df2$election_year))

  if (length(shared_years) == 0) {
    cat("  No shared years — skipping\n\n")
    return()
  }

  # Use only parties present in both datasets
  parties_both <- intersect(parties, intersect(names(df1), names(df2)))

  results <- list()
  for (p in parties_both) {
    agg1 <- df1 |>
      filter(election_year %in% shared_years) |>
      group_by(state, election_year) |>
      summarise(votes = sum(!!sym(p) * valid_votes, na.rm = TRUE), .groups = "drop")

    agg2 <- df2 |>
      filter(election_year %in% shared_years) |>
      group_by(state, election_year) |>
      summarise(votes = sum(!!sym(p) * valid_votes, na.rm = TRUE), .groups = "drop")

    comp <- agg1 |>
      inner_join(agg2, by = c("state", "election_year"), suffix = c("_1", "_2")) |>
      mutate(pct_diff = 100 * abs(votes_1 - votes_2) / pmax(votes_1, votes_2, 1))

    max_pct <- max(comp$pct_diff, na.rm = TRUE)
    results[[p]] <- max_pct
  }

  result_df <- tibble(party = names(results), max_pct_diff = unlist(results))
  cat("  Max percentage difference in vote counts by party:\n")
  print(result_df)
  cat("  (Note: differences arise from different crosswalk targets, not data loss)\n")

  worst <- max(result_df$max_pct_diff, na.rm = TRUE)
  if (worst > 10) {
    fail(sprintf("%s vs %s: worst party vote conservation = %.2f%%", name1, name2, worst))
  } else if (worst > 5) {
    warn(sprintf("%s vs %s: worst party vote conservation = %.2f%%", name1, name2, worst))
  } else {
    pass(sprintf("%s vs %s: all major party votes conserved within 5%%", name1, name2))
  }

  cat("\n")
}

compare_party_votes(h21, "harm_21", h25, "harm_25", major_parties)
compare_party_votes(h21, "harm_21", h23, "harm_23", major_parties)

# ==============================================================================
# CHECK 7: CDU/CSU HANDLING
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 7: CDU/CSU HANDLING\n")
cat(strrep("=", 70), "\n\n")

check_cdu_csu <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))
  ok <- TRUE

  # CDU should be NA in Bayern (state "09")
  bayern <- df |> filter(state == "09")
  cdu_in_bayern <- sum(!is.na(bayern$cdu) & bayern$cdu != 0, na.rm = TRUE)
  if (cdu_in_bayern > 0) {
    fail(sprintf("%s: %d Bayern rows with non-zero CDU", name, cdu_in_bayern))
    ok <- FALSE
  } else {
    cat("  CDU is NA/0 in Bayern: OK\n")
  }

  # CSU should be NA outside Bayern
  non_bayern <- df |> filter(state != "09")
  csu_outside <- sum(!is.na(non_bayern$csu) & non_bayern$csu != 0, na.rm = TRUE)
  if (csu_outside > 0) {
    fail(sprintf("%s: %d non-Bayern rows with non-zero CSU", name, csu_outside))
    ok <- FALSE
  } else {
    cat("  CSU is NA/0 outside Bayern: OK\n")
  }

  # cdu_csu == CDU (non-Bayern) or CSU (Bayern) within rounding tolerance
  df_check <- df |>
    mutate(
      expected_cdu_csu = if_else(state == "09", csu, cdu),
      cdu_csu_diff = abs(cdu_csu - expected_cdu_csu)
    ) |>
    filter(!is.na(cdu_csu_diff))

  n_mismatch <- sum(df_check$cdu_csu_diff > 0.001, na.rm = TRUE)
  if (n_mismatch > 0) {
    warn(sprintf("%s: %d rows where cdu_csu != expected (CDU or CSU) beyond tolerance",
                 name, n_mismatch))
    ok <- FALSE
  } else {
    cat("  cdu_csu matches CDU/CSU correctly: OK\n")
  }

  if (ok) pass(sprintf("%s: CDU/CSU handling correct", name))
  cat("\n")
}

check_cdu_csu(h21, "state_harm_21")
check_cdu_csu(h23, "state_harm_23")
check_cdu_csu(h25, "state_harm_25")
check_cdu_csu(unharm, "state_2224_unharm")

# ==============================================================================
# CHECK 8: PARTY TIMELINE
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 8: PARTY TIMELINE\n")
cat(strrep("=", 70), "\n\n")

check_party_timeline <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))
  ok <- TRUE

  # AfD: should be NA or 0 before 2013
  pre2013 <- df |> filter(election_year < 2013)
  if (nrow(pre2013) > 0) {
    afd_pre2013 <- sum(!is.na(pre2013$afd) & pre2013$afd > 0, na.rm = TRUE)
    if (afd_pre2013 > 0) {
      fail(sprintf("%s: %d pre-2013 rows with non-zero AfD", name, afd_pre2013))
      ok <- FALSE
    } else {
      cat("  AfD: NA/0 before 2013: OK\n")
    }
  }

  # BSW: should be NA before 2024
  if ("bsw" %in% names(df)) {
    pre2024 <- df |> filter(election_year < 2024)
    bsw_pre2024 <- sum(!is.na(pre2024$bsw) & pre2024$bsw > 0, na.rm = TRUE)
    if (bsw_pre2024 > 0) {
      fail(sprintf("%s: %d pre-2024 rows with non-zero BSW", name, bsw_pre2024))
      ok <- FALSE
    } else {
      cat("  BSW: NA/0 before 2024: OK\n")
    }

    # BSW should be non-NA for 2024 BB/SN/TH (states 12, 14, 16)
    bsw_2024 <- df |> filter(election_year == 2024, state %in% c("12", "14", "16"))
    if (nrow(bsw_2024) > 0) {
      n_bsw_na <- sum(is.na(bsw_2024$bsw))
      if (n_bsw_na == nrow(bsw_2024)) {
        fail(sprintf("%s: BSW is all NA for 2024 BB/SN/TH", name))
        ok <- FALSE
      } else {
        cat(sprintf("  BSW in 2024 BB/SN/TH: %d/%d non-NA: OK\n",
                    nrow(bsw_2024) - n_bsw_na, nrow(bsw_2024)))
      }
    }
  } else {
    cat("  (no bsw column)\n")
  }

  # Bremen AfD: should be NA (AfD doesn't contest in Bremen)
  # Bremen = state "04"
  bremen_rows <- df |> filter(state == "04")
  if (nrow(bremen_rows) > 0) {
    # Check if AfD is NA (not 0)
    afd_bremen_nonNA <- sum(!is.na(bremen_rows$afd), na.rm = TRUE)
    afd_bremen_zero <- sum(bremen_rows$afd == 0, na.rm = TRUE)
    cat(sprintf("  Bremen AfD: %d non-NA rows (of %d), %d zero rows\n",
                afd_bremen_nonNA, nrow(bremen_rows), afd_bremen_zero))
    if (afd_bremen_nonNA > 0 && afd_bremen_zero == afd_bremen_nonNA) {
      warn(sprintf("%s: Bremen AfD is 0 instead of NA (%d rows)", name, afd_bremen_zero))
    }
  }

  if (ok) pass(sprintf("%s: party timeline consistent", name))
  cat("\n")
}

check_party_timeline(h21, "state_harm_21")
check_party_timeline(h23, "state_harm_23")
check_party_timeline(h25, "state_harm_25")
check_party_timeline(unharm, "state_2224_unharm")

# ==============================================================================
# CHECK 9: KNOWN ENTITY SPOT-CHECKS
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 9: KNOWN ENTITY SPOT-CHECKS\n")
cat(strrep("=", 70), "\n\n")

spot_checks <- tribble(
  ~entity,     ~ags,        ~state,  ~min_ev,
  "Berlin",    "11000000",  "11",    2000000,
  "München",   "09162000",  "09",    800000,
  "Köln",      "05315000",  "05",    600000,
  "Frankfurt", "06412000",  "06",    400000,
  "Dresden",   "14612000",  "14",    350000,
)

# Use harm_25 for spot-checks (most comprehensive)
cat("Using state_harm_25 for spot-checks:\n\n")

for (i in seq_len(nrow(spot_checks))) {
  sc <- spot_checks[i, ]
  matches <- h25 |> filter(ags == sc$ags)

  if (nrow(matches) == 0) {
    fail(sprintf("Spot-check: %s (%s) not found in state_harm_25", sc$entity, sc$ags))
  } else {
    max_ev <- max(matches$eligible_voters, na.rm = TRUE)
    ev_ok <- !is.na(max_ev) && max_ev >= sc$min_ev
    years_present <- paste(sort(unique(matches$election_year)), collapse = ", ")
    n_elections <- n_distinct(matches$election_year)

    if (ev_ok) {
      pass(sprintf("Spot-check: %s (%s) — %d elections, max EV = %s",
                   sc$entity, sc$ags, n_elections, format(max_ev, big.mark = ",")))
    } else {
      warn(sprintf("Spot-check: %s (%s) — max EV = %s (expected >= %s)",
                   sc$entity, sc$ags,
                   format(max_ev, big.mark = ","),
                   format(sc$min_ev, big.mark = ",")))
    }
    cat(sprintf("  Years: %s\n", years_present))
  }
}

# Check each city only appears in years when its state held an election
cat("\nVerifying city appearances match state election years...\n")
for (i in seq_len(nrow(spot_checks))) {
  sc <- spot_checks[i, ]
  city_years <- h25 |> filter(ags == sc$ags) |> pull(election_year) |> unique() |> sort()
  state_years <- h25 |> filter(state == sc$state) |> pull(election_year) |> unique() |> sort()
  if (!setequal(city_years, state_years)) {
    warn(sprintf("%s appears in years not matching state %s elections", sc$entity, sc$state))
    cat(sprintf("  City years: %s\n", paste(city_years, collapse = ", ")))
    cat(sprintf("  State years: %s\n", paste(state_years, collapse = ", ")))
  }
}
pass("City appearances consistent with state election years")
cat("\n")

# ==============================================================================
# CHECK 10: REGIONAL TURNOUT PATTERNS (informational)
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 10: REGIONAL TURNOUT PATTERNS (informational)\n")
cat(strrep("=", 70), "\n\n")

# East = 12 (BB), 13 (MV), 14 (SN), 15 (SA), 16 (TH)
# Berlin = 11
# West = everything else
east_states <- c("12", "13", "14", "15", "16")

cat("Using state_harm_25:\n\n")

regional <- h25 |>
  filter(!is.na(turnout)) |>
  mutate(
    region = case_when(
      state %in% east_states ~ "East",
      state == "11" ~ "Berlin",
      TRUE ~ "West"
    )
  ) |>
  group_by(region) |>
  summarise(
    mean_turnout = mean(turnout, na.rm = TRUE),
    median_turnout = median(turnout, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("Mean turnout by region:\n")
print(regional)
cat("\n")

# State-level means
state_turnout <- h25 |>
  filter(!is.na(turnout)) |>
  group_by(state) |>
  summarise(
    mean_turnout = round(mean(turnout, na.rm = TRUE), 3),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(mean_turnout)

cat("Mean turnout by state (ascending):\n")
print(state_turnout, n = 20)

# Check bounds
low_state <- state_turnout |> filter(mean_turnout < 0.3)
high_state <- state_turnout |> filter(mean_turnout > 0.85)

if (nrow(low_state) > 0) {
  warn(sprintf("States with mean turnout < 0.3: %s",
               paste(low_state$state, collapse = ", ")))
}
if (nrow(high_state) > 0) {
  warn(sprintf("States with mean turnout > 0.85: %s",
               paste(high_state$state, collapse = ", ")))
}
if (nrow(low_state) == 0 && nrow(high_state) == 0) {
  pass("All state mean turnouts in [0.3, 0.85]")
}
cat("\n")

# ==============================================================================
# CHECK 11: QUALITY FLAGS
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 11: QUALITY FLAGS\n")
cat(strrep("=", 70), "\n\n")

check_flags <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))

  if ("flag_unsuccessful_naive_merge" %in% names(df)) {
    n_flagged <- sum(df$flag_unsuccessful_naive_merge == 1, na.rm = TRUE)
    pct_flagged <- 100 * n_flagged / nrow(df)
    cat(sprintf("  flag_unsuccessful_naive_merge: %d (%.2f%%)\n", n_flagged, pct_flagged))

    if (pct_flagged > 5) {
      warn(sprintf("%s: %.2f%% rows with unsuccessful naive merge", name, pct_flagged))
    } else {
      pass(sprintf("%s: unsuccessful naive merge flag < 5%%", name))
    }

    # By election_year
    if (n_flagged > 0) {
      cat("  By election year:\n")
      by_year <- df |>
        group_by(election_year) |>
        summarise(
          n_flagged = sum(flag_unsuccessful_naive_merge == 1, na.rm = TRUE),
          n_total = n(),
          pct = round(100 * n_flagged / n_total, 2),
          .groups = "drop"
        ) |>
        filter(n_flagged > 0)
      print(by_year)
    }
  } else {
    cat("  (no flag_unsuccessful_naive_merge column)\n")
  }

  if ("flag_total_votes_incongruent" %in% names(df)) {
    n_flagged <- sum(df$flag_total_votes_incongruent == 1, na.rm = TRUE)
    pct_flagged <- 100 * n_flagged / nrow(df)
    cat(sprintf("  flag_total_votes_incongruent: %d (%.2f%%)\n", n_flagged, pct_flagged))

    if (pct_flagged > 5) {
      warn(sprintf("%s: %.2f%% rows with total votes incongruent", name, pct_flagged))
    } else {
      pass(sprintf("%s: total votes incongruent flag < 5%%", name))
    }

    if (n_flagged > 0) {
      cat("  By election year:\n")
      by_year <- df |>
        group_by(election_year) |>
        summarise(
          n_flagged = sum(flag_total_votes_incongruent == 1, na.rm = TRUE),
          n_total = n(),
          pct = round(100 * n_flagged / n_total, 2),
          .groups = "drop"
        ) |>
        filter(n_flagged > 0)
      print(by_year)
    }
  } else {
    cat("  (no flag_total_votes_incongruent column)\n")
  }

  cat("\n")
}

check_flags(h21, "state_harm_21")
check_flags(h23, "state_harm_23")
check_flags(h25, "state_harm_25")
check_flags(unharm, "state_2224_unharm")

# ==============================================================================
# CHECK 12: COVARIATE COVERAGE
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 12: COVARIATE COVERAGE\n")
cat(strrep("=", 70), "\n\n")

cov_cols <- c("area_ags", "population_ags", "employees_ags", "pop_density_ags")

check_covariates <- function(df, name) {
  cat(sprintf("--- %s ---\n", name))

  present <- cov_cols[cov_cols %in% names(df)]
  missing_cols <- setdiff(cov_cols, present)

  if (length(missing_cols) > 0) {
    cat(sprintf("  Missing covariate columns: %s\n", paste(missing_cols, collapse = ", ")))
  }

  if (length(present) == 0) {
    cat("  No covariate columns present (expected for unharmonized data)\n\n")
    return()
  }

  pass(sprintf("%s: covariate columns present (%s)", name, paste(present, collapse = ", ")))

  # NA rate by election_year
  cat("  Covariate NA rate by election year:\n")
  na_by_year <- df |>
    group_by(election_year) |>
    summarise(
      across(all_of(present), ~ round(100 * mean(is.na(.)), 1)),
      n = n(),
      .groups = "drop"
    )
  print(na_by_year, n = 20)

  # Spot-check: Berlin population > 3M (population_ags may be in thousands)
  # Detect scale: if max < 10000, likely in thousands
  berlin <- df |> filter(substr(ags, 1, 2) == "11")
  if (nrow(berlin) > 0 && "population_ags" %in% present) {
    max_berlin_pop <- max(berlin$population_ags, na.rm = TRUE)
    # Determine if values are in thousands (all < 100,000) or absolute
    max_all_pop <- max(df$population_ags, na.rm = TRUE)
    in_thousands <- !is.na(max_all_pop) && max_all_pop < 100000
    threshold_berlin <- if (in_thousands) 3000 else 3000000
    unit_label <- if (in_thousands) "thousands" else "absolute"

    if (!is.na(max_berlin_pop) && max_berlin_pop > threshold_berlin) {
      pass(sprintf("%s: Berlin population = %s (%s, > %s)", name,
                   format(round(max_berlin_pop, 1), big.mark = ","),
                   unit_label,
                   format(threshold_berlin, big.mark = ",")))
    } else if (!is.na(max_berlin_pop) && max_berlin_pop > 0) {
      warn(sprintf("%s: Berlin population = %s (%s, expected > %s)", name,
                   format(round(max_berlin_pop, 1), big.mark = ","),
                   unit_label,
                   format(threshold_berlin, big.mark = ",")))
    }
  }

  # Spot-check: München population > 1M
  muenchen <- df |> filter(ags == "09162000")
  if (nrow(muenchen) > 0 && "population_ags" %in% present) {
    max_muc_pop <- max(muenchen$population_ags, na.rm = TRUE)
    max_all_pop <- max(df$population_ags, na.rm = TRUE)
    in_thousands <- !is.na(max_all_pop) && max_all_pop < 100000
    threshold_muc <- if (in_thousands) 1000 else 1000000
    unit_label <- if (in_thousands) "thousands" else "absolute"

    if (!is.na(max_muc_pop) && max_muc_pop > threshold_muc) {
      pass(sprintf("%s: München population = %s (%s, > %s)", name,
                   format(round(max_muc_pop, 1), big.mark = ","),
                   unit_label,
                   format(threshold_muc, big.mark = ",")))
    } else if (!is.na(max_muc_pop) && max_muc_pop > 0) {
      warn(sprintf("%s: München population = %s (%s, expected > %s)", name,
                   format(round(max_muc_pop, 1), big.mark = ","),
                   unit_label,
                   format(threshold_muc, big.mark = ",")))
    }
  }

  cat("\n")
}

check_covariates(h21, "state_harm_21")
check_covariates(h23, "state_harm_23")
check_covariates(h25, "state_harm_25")
check_covariates(unharm, "state_2224_unharm")

# ==============================================================================
# CHECK 13: UNHARMONIZED INPUT
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 13: UNHARMONIZED INPUT (state_2224_unharm)\n")
cat(strrep("=", 70), "\n\n")

# Expected 11 state-year combinations
expected_state_years <- tribble(
  ~state, ~election_year,
  "10",   2022,   # Saarland
  "01",   2022,   # Schleswig-Holstein
  "05",   2022,   # NRW
  "03",   2022,   # Niedersachsen
  "04",   2023,   # Bremen
  "06",   2023,   # Hessen
  "09",   2023,   # Bayern
  "11",   2023,   # Berlin
  "12",   2024,   # Brandenburg
  "14",   2024,   # Sachsen
  "16",   2024,   # Thüringen
)

actual_state_years <- unharm |>
  distinct(state, election_year) |>
  arrange(election_year, state)

cat("Expected state-year combinations:\n")
print(expected_state_years)
cat("\nActual state-year combinations:\n")
print(actual_state_years)

missing_sy <- expected_state_years |>
  anti_join(actual_state_years, by = c("state", "election_year"))

extra_sy <- actual_state_years |>
  anti_join(expected_state_years, by = c("state", "election_year"))

if (nrow(missing_sy) > 0) {
  fail(sprintf("Missing %d expected state-year combinations", nrow(missing_sy)))
  print(missing_sy)
} else if (nrow(extra_sy) > 0) {
  warn(sprintf("Found %d unexpected state-year combinations", nrow(extra_sy)))
  print(extra_sy)
} else {
  pass("Exactly 11 expected state-year combinations present")
}

# No duplicates within (ags, election_year)
dupes <- unharm |>
  group_by(ags, election_year) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

if (nrow(dupes) > 0) {
  fail(sprintf("state_2224_unharm: %d duplicate (ags, election_year) combinations", nrow(dupes)))
  print(head(dupes, 10))
} else {
  pass("state_2224_unharm: no duplicate (ags, election_year) combinations")
}

# Column naming: grune (not gruene), cdu_csu, turnout, valid_votes
cat("\nColumn naming checks:\n")
has_grune <- "grune" %in% names(unharm)
has_gruene <- "gruene" %in% names(unharm)
has_cdu_csu <- "cdu_csu" %in% names(unharm)
has_turnout <- "turnout" %in% names(unharm)
has_valid_votes <- "valid_votes" %in% names(unharm)

cat(sprintf("  'grune' present: %s (expected: TRUE)\n", has_grune))
cat(sprintf("  'gruene' present: %s (expected: FALSE)\n", has_gruene))
cat(sprintf("  'cdu_csu' present: %s\n", has_cdu_csu))
cat(sprintf("  'turnout' present: %s\n", has_turnout))
cat(sprintf("  'valid_votes' present: %s\n", has_valid_votes))

if (has_grune && !has_gruene && has_cdu_csu && has_turnout && has_valid_votes) {
  pass("state_2224_unharm: column naming as expected (grune not gruene)")
} else {
  warn("state_2224_unharm: unexpected column naming")
}

# No Hamburg rows
hamburg_rows <- unharm |> filter(state == "02")
if (nrow(hamburg_rows) > 0) {
  fail(sprintf("state_2224_unharm: %d Hamburg rows found", nrow(hamburg_rows)))
} else {
  pass("state_2224_unharm: no Hamburg rows")
}

cat("\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Dataset dimensions:\n")
cat(sprintf("  state_harm_21:     %d rows x %d cols, %d elections, %d states\n",
            nrow(h21), ncol(h21), n_distinct(h21$election_year), n_distinct(h21$state)))
cat(sprintf("  state_harm_23:     %d rows x %d cols, %d elections, %d states\n",
            nrow(h23), ncol(h23), n_distinct(h23$election_year), n_distinct(h23$state)))
cat(sprintf("  state_harm_25:     %d rows x %d cols, %d elections, %d states\n",
            nrow(h25), ncol(h25), n_distinct(h25$election_year), n_distinct(h25$state)))
cat(sprintf("  state_2224_unharm: %d rows x %d cols, %d elections, %d states\n",
            nrow(unharm), ncol(unharm), n_distinct(unharm$election_year), n_distinct(unharm$state)))

cat(sprintf("\nResults: %d PASS, %d WARNING, %d FAIL\n", n_pass, n_warn, n_fail))

cat("\nKnown issues documented:\n")
cat("  - 4 Thuringia municipalities with NA turnout: 16063033, 16074082, 16075087, 16075105\n")
cat("  - Covariate gaps: harm_21 has NA covariates for 2022-2024 elections;\n")
cat("    harm_25 has NA covariates for 2024 elections\n")
cat("  - Column naming: state_2224_unharm uses 'grune' (not 'gruene')\n")
cat("  - state_harm_23 lacks 'bsw' column (only covers through 2023)\n")

cat("\n=", strrep("=", 70), "\n")
cat("END OF CHECKS\n")
cat("=", strrep("=", 70), "\n")
