### Check quality of new state election data (2022-2023) and harmonization
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)

pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR"
)

conflict_prefer("filter", "dplyr")

cat("=", strrep("=", 70), "\n")
cat("STATE ELECTIONS 2022-2023: DATA QUALITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Helper function to pad AGS
pad_zero_conditional <- function(x, n) {
  x <- as.character(x)
  x <- str_pad(x, width = n, side = "left", pad = "0")
  return(x)
}

# Load datasets ----------------------------------------------------------------

cat("Loading datasets...\n")

# Unharmonized new data
df_new_unharm <- read_rds("data/state_elections/final/state_2223_unharm.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Harmonized datasets
df_harm_21 <- read_rds("data/state_elections/final/state_harm.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

df_harm_23 <- read_rds("data/state_elections/final/state_harm_23.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Old unharmonized data for comparison
df_old_unharm <- read_rds("data/state_elections/final/state_unharm.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

cat("Loaded datasets:\n")
cat("  - New unharmonized (2022-2023):", nrow(df_new_unharm), "rows\n")
cat("  - Harmonized to 2021:", nrow(df_harm_21), "rows\n")
cat("  - Harmonized to 2023:", nrow(df_harm_23), "rows\n")
cat("  - Old unharmonized:", nrow(df_old_unharm), "rows\n\n")

# Check 1: Coverage of new election years -------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 1: COVERAGE OF NEW ELECTION YEARS\n")
cat(strrep("=", 70), "\n\n")

# Check which states and years are in the new data
coverage_new <- df_new_unharm |>
  group_by(state, election_year) |>
  summarise(
    n_municipalities = n_distinct(ags),
    n_observations = n(),
    .groups = "drop"
  ) |>
  arrange(state, election_year)

cat("New data coverage by state and year:\n")
print(coverage_new)

# duplicates in new data?
df_new_unharm %>%
  group_by(ags, election_year) %>%
  summarise(n = n()) %>%
  filter(n > 1)

df_new_unharm %>%
  filter(ags == "09564000") |>
  glimpse()

# Expected: Niedersachsen (03) in 2022, Bayern (09) and Hessen (06) in 2023
expected_coverage <- tibble(
  state = c("03", "06", "09"),
  election_year = c(2022, 2023, 2023),
  state_name = c("Niedersachsen", "Hessen", "Bayern")
)

cat("\nExpected coverage:\n")
print(expected_coverage)

# Check if all expected states/years are present
coverage_check <- expected_coverage |>
  left_join(
    coverage_new,
    by = c("state", "election_year")
  ) |>
  mutate(
    is_present = !is.na(n_municipalities),
    has_data = n_municipalities > 0
  )

missing_coverage <- coverage_check |>
  filter(!is_present | !has_data)

if (nrow(missing_coverage) > 0) {
  cat("\n⚠️  WARNING: Missing expected coverage:\n")
  print(missing_coverage)
} else {
  cat("\n✓ All expected states and years are present\n")
}

# Check 2: Vote share validity ------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 2: VOTE SHARE VALIDITY\n")
cat(strrep("=", 70), "\n\n")

# Identify party variables (exclude metadata and aggregated variables)
get_party_vars <- function(df) {
  party_vars <- df |>
    select(cdu:last_col()) |>
    select(-any_of(c(
      "county", "election_year", "state", "state_name", "election_date",
      "eligible_voters", "number_voters", "valid_votes", "turnout",
      "flag_unsuccessful_naive_merge", "total_vote_share",
      "flag_total_votes_incongruent", "cdu_csu"
    ))) |> # Exclude cdu_csu to avoid double counting
    names()

  # Also exclude any variables that are clearly not party vote shares
  party_vars <- party_vars[!grepl("^(date|id|name|area|population|employees)", party_vars)]

  return(party_vars)
}

party_vars_new <- get_party_vars(df_new_unharm)
party_vars_harm_21 <- get_party_vars(df_harm_21)
party_vars_harm_23 <- get_party_vars(df_harm_23)

# Check sum of vote shares
check_vote_shares <- function(df, dataset_name, party_vars) {
  cat(sprintf("Checking %s...\n", dataset_name))

  # Calculate sum of vote shares
  party_data <- df |>
    select(any_of(party_vars))

  df_check <- df |>
    mutate(
      sum_vote_shares = rowSums(party_data, na.rm = TRUE),
      sum_vote_shares = round(sum_vote_shares, 8)
    )

  # Check if sum equals 1 (allow small tolerance for rounding and small parties)
  tolerance <- 0.05 # Allow 5% deviation
  n_not_one <- sum(abs(df_check$sum_vote_shares - 1) > tolerance, na.rm = TRUE)
  pct_not_one <- 100 * n_not_one / nrow(df_check)

  cat(sprintf(
    "  Rows where |vote share sum - 1| > %.2f: %d (%.2f%%)\n",
    tolerance, n_not_one, pct_not_one
  ))
  cat("    (Note: Small deviations expected due to rounding and parties not included)\n")

  # Check vote shares between 0 and 1
  invalid_shares <- df |>
    select(any_of(party_vars)) |>
    summarise_all(~ sum(. < 0 | . > 1, na.rm = TRUE)) |>
    pivot_longer(everything(), names_to = "party", values_to = "n_invalid") |>
    filter(n_invalid > 0)

  if (nrow(invalid_shares) > 0) {
    cat("  ⚠️  Vote shares outside [0,1]:\n")
    print(invalid_shares)
  } else {
    cat("  ✓ All vote shares are between 0 and 1\n")
  }

  # Show distribution of sum differences
  if (n_not_one > 0) {
    diff_from_one <- abs(df_check$sum_vote_shares - 1)
    cat("  Distribution of |sum - 1| (for rows with deviation > tolerance):\n")
    large_diff <- diff_from_one[diff_from_one > tolerance]
    if (length(large_diff) > 0) {
      print(quantile(large_diff,
        c(0.01, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
        na.rm = TRUE
      ))
    }

    # Show examples of largest deviations
    examples <- df_check |>
      filter(abs(sum_vote_shares - 1) > tolerance) |>
      arrange(desc(abs(sum_vote_shares - 1))) |>
      select(ags, election_year, state, sum_vote_shares) |>
      head(10)

    if (nrow(examples) > 0) {
      cat("  Examples of rows with largest deviation from 1:\n")
      print(examples)
    }

    # Flag very low sums (might indicate missing party data)
    very_low_sum <- df_check |>
      filter(sum_vote_shares < 0.7) |>
      nrow()

    if (very_low_sum > 0) {
      cat(sprintf("  ⚠️  Rows with vote share sum < 0.7: %d (may indicate missing party data)\n", very_low_sum))
    }
  }

  return(df_check)
}

# Check new unharmonized data
df_new_check <- check_vote_shares(df_new_unharm, "New unharmonized data", party_vars_new)


# Check harmonized to 2023 (only new years)
df_harm_23_new <- df_harm_23 |>
  filter(election_year %in% c(2022, 2023))

if (nrow(df_harm_23_new) > 0 && length(party_vars_harm_23) > 0) {
  df_harm_23_check <- check_vote_shares(df_harm_23_new, "Harmonized to 2023 (2022-2023)", party_vars_harm_23)
} else {
  cat("Harmonized to 2023: No data for 2022-2023\n")
}

# Check 3: Missing values ----------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 3: MISSING VALUES\n")
cat(strrep("=", 70), "\n\n")

check_missing <- function(df, dataset_name, key_vars = c("eligible_voters", "valid_votes", "turnout")) {
  cat(sprintf("Checking %s...\n", dataset_name))

  # Only check variables that exist
  key_vars <- key_vars[key_vars %in% names(df)]

  if (length(key_vars) > 0) {
    missing_summary <- df |>
      summarise(
        across(
          all_of(key_vars),
          ~ sum(is.na(.x))
        )
      ) |>
      pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
      mutate(pct_missing = 100 * n_missing / nrow(df))

    if (any(missing_summary$n_missing > 0)) {
      cat("  ⚠️  Missing values found:\n")
      print(missing_summary |> filter(n_missing > 0))
    } else {
      cat("  ✓ No missing values in key variables\n")
    }
  }

  # Check for rows with all party variables missing
  party_vars <- get_party_vars(df)
  if (length(party_vars) > 0) {
    all_party_missing <- df |>
      select(any_of(party_vars)) |>
      apply(1, function(x) all(is.na(x))) |>
      sum()

    if (all_party_missing > 0) {
      cat(sprintf("  ⚠️  Rows with all party variables missing: %d\n", all_party_missing))
    } else {
      cat("  ✓ No rows with all party variables missing\n")
    }
  }
}

check_missing(df_new_unharm, "New unharmonized data")
if (exists("df_harm_21_new") && nrow(df_harm_21_new) > 0) {
  check_missing(df_harm_21_new, "Harmonized to 2021 (2022-2023)")
}
if (exists("df_harm_23_new") && nrow(df_harm_23_new) > 0) {
  check_missing(df_harm_23_new, "Harmonized to 2023 (2022-2023)")
}

# Check 4: Municipality count stability --------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 4: MUNICIPALITY COUNT STABILITY\n")
cat(strrep("=", 70), "\n\n")

# For harm_21: should have same number of municipalities as reference year (2021 or closest)
# For harm_23: should have same number of municipalities as reference year (2023)

check_muni_stability <- function(df, dataset_name, reference_year = NULL) {
  cat(sprintf("Checking %s...\n", dataset_name))

  muni_count <- df |>
    group_by(election_year) |>
    summarise(
      n_municipalities = n_distinct(ags),
      n_observations = n(),
      .groups = "drop"
    ) |>
    arrange(election_year)

  print(muni_count)

  if (!is.null(reference_year)) {
    n_ref <- muni_count |>
      filter(election_year == reference_year) |>
      pull(n_municipalities)

    if (length(n_ref) > 0) {
      cat(sprintf("\nReference year %d: %d municipalities\n", reference_year, n_ref))

      deviations <- muni_count |>
        filter(election_year != reference_year) |>
        mutate(
          diff_from_ref = n_municipalities - n_ref,
          pct_diff = 100 * diff_from_ref / n_ref
        ) |>
        filter(diff_from_ref != 0)

      if (nrow(deviations) > 0) {
        cat("  ⚠️  Years with different municipality counts:\n")
        print(deviations)
      } else {
        cat("  ✓ All years have same municipality count as reference\n")
      }
    }
  }
}

# Check harm_21 - focus on new years
if (exists("df_harm_21_new") && nrow(df_harm_21_new) > 0) {
  # Get reference count from 2021 or closest year
  ref_count_21 <- df_harm_21 |>
    filter(election_year == 2021) |>
    summarise(n = n_distinct(ags)) |>
    pull(n)

  if (length(ref_count_21) > 0 && ref_count_21 > 0) {
    cat("Reference for harm_21: 2021 with", ref_count_21, "municipalities\n")
    check_muni_stability(df_harm_21_new, "Harmonized to 2021 (2022-2023)")
  }
}

# Check harm_23 - focus on new years
if (exists("df_harm_23_new") && nrow(df_harm_23_new) > 0) {
  # For 2023, we'd expect the count to match 2023 reference
  # But we don't have a 2023 reference year in the data, so just report counts
  check_muni_stability(df_harm_23_new, "Harmonized to 2023 (2022-2023)")
}

# Check 5: Consistency between harmonization versions ------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 5: CONSISTENCY BETWEEN HARMONIZATION VERSIONS\n")
cat(strrep("=", 70), "\n\n")

# Compare harm_21 and harm_23 for overlapping years (2022-2023)
has_harm_21_new <- exists("df_harm_21_new") && !is.null(df_harm_21_new) && nrow(df_harm_21_new) > 0
has_harm_23_new <- exists("df_harm_23_new") && !is.null(df_harm_23_new) && nrow(df_harm_23_new) > 0

if (has_harm_21_new && has_harm_23_new) {
  # Municipality counts by year
  muni_count_21 <- df_harm_21_new |>
    group_by(election_year) |>
    summarise(n_muni_21 = n_distinct(ags), .groups = "drop")

  muni_count_23 <- df_harm_23_new |>
    group_by(election_year) |>
    summarise(n_muni_23 = n_distinct(ags), .groups = "drop")

  comparison <- muni_count_21 |>
    full_join(muni_count_23, by = "election_year") |>
    mutate(diff = n_muni_23 - n_muni_21)

  cat("Municipality count comparison (harm_23 - harm_21):\n")
  print(comparison)

  # Check if same municipalities appear in both
  ags_21 <- df_harm_21_new |>
    distinct(ags, election_year)

  ags_23 <- df_harm_23_new |>
    distinct(ags, election_year)

  only_in_21 <- ags_21 |>
    anti_join(ags_23, by = c("ags", "election_year"))

  only_in_23 <- ags_23 |>
    anti_join(ags_21, by = c("ags", "election_year"))

  if (nrow(only_in_21) > 0) {
    cat("\n⚠️  Municipalities only in harm_21:", nrow(only_in_21), "\n")
    print(head(only_in_21, 20))
  }

  if (nrow(only_in_23) > 0) {
    cat("\n⚠️  Municipalities only in harm_23:", nrow(only_in_23), "\n")
    print(head(only_in_23, 20))
  }

  if (nrow(only_in_21) == 0 && nrow(only_in_23) == 0) {
    cat("\n✓ Same municipalities in both harmonization versions\n")
  }
} else {
  if (!has_harm_21_new) {
    cat("Note: harm_21 does not yet contain 2022-2023 data\n")
  }
  if (!has_harm_23_new) {
    cat("Note: harm_23 does not contain 2022-2023 data\n")
  }
  if (!has_harm_21_new && !has_harm_23_new) {
    cat("Cannot compare harmonization versions - no harmonized data for 2022-2023\n")
  }
}

# Check 6: Valid votes and turnout consistency --------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 6: VALID VOTES AND TURNOUT CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

check_votes_turnout <- function(df, dataset_name) {
  cat(sprintf("Checking %s...\n", dataset_name))

  # Check if required variables exist
  has_eligible <- "eligible_voters" %in% names(df)
  has_valid <- "valid_votes" %in% names(df)
  has_number <- "number_voters" %in% names(df)
  has_turnout <- "turnout" %in% names(df)

  if (has_eligible && has_valid) {
    df_check <- df |>
      mutate(
        valid_le_eligible = valid_votes <= eligible_voters
      )

    # Valid votes > eligible voters
    n_invalid_votes <- sum(!df_check$valid_le_eligible, na.rm = TRUE)
    if (n_invalid_votes > 0) {
      cat(sprintf("  ⚠️  Cases where valid_votes > eligible_voters: %d\n", n_invalid_votes))
      examples <- df_check |>
        filter(!valid_le_eligible) |>
        select(ags, election_year, state, eligible_voters, valid_votes) |>
        head(10)
      print(examples)
    } else {
      cat("  ✓ All valid_votes <= eligible_voters\n")
    }
  }

  # Turnout consistency
  if (has_turnout && has_number && has_eligible) {
    df_check <- df |>
      mutate(
        turnout_calc = ifelse(eligible_voters > 0,
          number_voters / eligible_voters,
          NA
        ),
        turnout_diff = ifelse(!is.na(turnout) & !is.na(turnout_calc),
          abs(turnout - turnout_calc),
          NA
        )
      )

    n_turnout_diff <- sum(df_check$turnout_diff > 0.001, na.rm = TRUE)
    if (n_turnout_diff > 0) {
      cat(sprintf("  ⚠️  Cases where turnout differs from calculated: %d\n", n_turnout_diff))
      # This might be expected if turnout is calculated differently
      cat("    (Note: May be expected due to different calculation methods)\n")
    } else {
      cat("  ✓ Turnout is consistent with number_voters/eligible_voters\n")
    }
  }

  # Turnout > 1
  if (has_turnout) {
    n_turnout_high <- sum(df$turnout > 1, na.rm = TRUE)
    if (n_turnout_high > 0) {
      cat(sprintf("  ⚠️  Cases where turnout > 1: %d\n", n_turnout_high))
    } else {
      cat("  ✓ No turnout > 1\n")
    }
  }
}

check_votes_turnout(df_new_unharm, "New unharmonized data")
if (exists("df_harm_21_new") && nrow(df_harm_21_new) > 0) {
  check_votes_turnout(df_harm_21_new, "Harmonized to 2021 (2022-2023)")
}
if (exists("df_harm_23_new") && nrow(df_harm_23_new) > 0) {
  check_votes_turnout(df_harm_23_new, "Harmonized to 2023 (2022-2023)")
}

# Check 7: Large changes in vote shares (compared to previous elections) ------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 7: LARGE CHANGES IN VOTE SHARES\n")
cat(strrep("=", 70), "\n\n")

# For new years, compare with previous election in same state
check_vote_changes <- function(df, dataset_name, comparison_df = NULL) {
  cat(sprintf("Checking %s...\n", dataset_name))

  # Get main parties
  main_parties <- c("cdu_csu", "spd", "afd", "gruene", "fdp", "linke_pds")
  main_parties <- main_parties[main_parties %in% names(df)]

  if (length(main_parties) == 0) {
    cat("  No main party variables found\n")
    return()
  }

  # Determine which comparison dataset to use
  # If comparison_df is provided, use it. Otherwise, detect based on dataset name.
  if (is.null(comparison_df)) {
    if (grepl("harmonized|harm", tolower(dataset_name), ignore.case = TRUE)) {
      # For harmonized data, use the full harmonized dataset
      # This ensures we're comparing apples to apples (both harmonized to same borders)
      if (grepl("2023|23", dataset_name)) {
        comparison_df <- df_harm_23  # Full harmonized to 2023
      } else if (grepl("2021|21", dataset_name)) {
        comparison_df <- df_harm_21  # Full harmonized to 2021
      } else {
        comparison_df <- df_old_unharm  # Fallback
      }
    } else {
      # For unharmonized data, use old unharmonized data
      comparison_df <- df_old_unharm
    }
  }

  # Use the full comparison dataset to calculate lag, then filter to new years
  # IMPORTANT: For harmonized data, we use the full harmonized dataset (e.g., df_harm_23)
  # which already contains all years harmonized to the same borders. We don't need to bind
  # old and new data - we just use the full dataset and filter to 2022-2023 AFTER calculating lag().
  # This ensures we're comparing within the same harmonization scheme.
  # For unharmonized data, we combine old and new unharmonized data.
  
  # Check if df is a subset of comparison_df (to avoid duplicates when binding)
  if (nrow(df) > 0 && nrow(comparison_df) > 0) {
    df_years <- unique(df$election_year)
    comp_years <- unique(comparison_df$election_year)
    
    # If df's years are all in comparison_df, just use comparison_df
    if (all(df_years %in% comp_years)) {
      df_all <- comparison_df |>
        arrange(ags, election_year) |>
        group_by(ags) |>
        mutate(
          across(
            all_of(main_parties),
            list(
              lag = ~ lag(.),
              diff = ~ . - lag(.)
            ),
            .names = "{col}_{fn}"
          )
        ) |>
        ungroup() |>
        filter(election_year %in% df_years)  # Filter to years in df
    } else {
      # Need to bind if df has years not in comparison_df
      df_all <- bind_rows(comparison_df, df) |>
        distinct(ags, election_year, .keep_all = TRUE) |>  # Remove any duplicates
        arrange(ags, election_year) |>
        group_by(ags) |>
        mutate(
          across(
            all_of(main_parties),
            list(
              lag = ~ lag(.),
              diff = ~ . - lag(.)
            ),
            .names = "{col}_{fn}"
          )
        ) |>
        ungroup() |>
        filter(election_year %in% df_years)
    }
  } else {
    df_all <- tibble()
  }

  # Find large changes (> 0.2 or > 50% change)
  large_changes <- df_all |>
    select(ags, election_year, state, contains("_diff")) |>
    pivot_longer(
      cols = contains("_diff"),
      names_to = "party",
      values_to = "change"
    ) |>
    mutate(
      party = str_remove(party, "_diff$"),
      abs_change = abs(change)
    ) |>
    filter(abs_change > 0.2, !is.na(change))

  if (nrow(large_changes) > 0) {
    cat(sprintf("  ⚠️  Found %d cases with vote share change > 0.2\n", nrow(large_changes)))
    cat("    (Note: Large changes may indicate real political shifts, not data errors)\n")

    # Show summary by party (count)
    large_changes_summary <- large_changes |>
      group_by(party, election_year) |>
      summarise(n_cases = n(), .groups = "drop") |>
      arrange(desc(n_cases))

    cat("\n  Summary by party:\n")
    print(large_changes_summary)

    # Show all municipalities with large changes, organized by party
    # Note: Changes are calculated by combining comparison data (all years) with new data (2022-2023),
    # then using lag() on the full time series before filtering to 2022-2023.
    # For harmonized data, we use the full harmonized dataset to ensure consistent borders.
    
    # Get previous election year and previous value
    # Extract lag values for each party from df_all (which already has lag values calculated)
    # We need to get the lag value for the specific party that changed
    prev_info <- df_all |>
      filter(ags %in% large_changes$ags) |>
      select(ags, election_year, contains("_lag")) |>
      pivot_longer(
        cols = contains("_lag"),
        names_to = "party_lag",
        values_to = "prev_value"
      ) |>
      mutate(party = str_remove(party_lag, "_lag$")) |>
      select(ags, election_year, party, prev_value) |>
      distinct()  # Ensure no duplicates
    
    # Get previous election year
    prev_years <- bind_rows(comparison_df, df) |>
      filter(ags %in% large_changes$ags) |>
      group_by(ags) |>
      arrange(election_year) |>
      mutate(prev_election = lag(election_year)) |>
      filter(election_year %in% c(2022, 2023)) |>
      select(ags, election_year, prev_election) |>
      ungroup()
    
    # Combine with previous values
    large_changes_with_context <- large_changes |>
      left_join(prev_years, by = c("ags", "election_year")) |>
      left_join(prev_info, by = c("ags", "election_year", "party")) |>
      mutate(
        comparison = ifelse(!is.na(prev_election), 
                           paste0("vs ", prev_election), 
                           "no previous data")
      ) |>
      select(ags, election_year, state, party, change, abs_change, comparison, prev_value) |>
      arrange(party, desc(abs_change))
    
    # Show all municipalities, grouped by party
    cat("\n  All municipalities with large changes (organized by party):\n")
    for (p in unique(large_changes_with_context$party)) {
      party_munis <- large_changes_with_context |>
        filter(party == p) |>
        select(-party)  # Remove party column since we're grouping by it
      
      cat(sprintf("\n  %s (%d municipalities):\n", toupper(p), nrow(party_munis)))
      print(party_munis)
    }
  } else {
    cat("  ✓ No unusually large vote share changes detected\n")
  }
}

# Check unharmonized new data
check_vote_changes(df_new_unharm, "New unharmonized data")

# Check harmonized to 2023
if (exists("df_harm_23_new") && nrow(df_harm_23_new) > 0) {
  check_vote_changes(df_harm_23_new, "Harmonized to 2023 (2022-2023)")
}

# Check 8: Flags and data quality indicators ---------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 8: DATA QUALITY FLAGS\n")
cat(strrep("=", 70), "\n\n")

check_flags <- function(df, dataset_name) {
  cat(sprintf("Checking %s...\n", dataset_name))

  if ("flag_unsuccessful_naive_merge" %in% names(df)) {
    n_flagged <- sum(df$flag_unsuccessful_naive_merge == 1, na.rm = TRUE)
    pct_flagged <- 100 * n_flagged / nrow(df)
    if (n_flagged > 0) {
      cat(sprintf(
        "  ⚠️  Rows with unsuccessful naive merge flag: %d (%.2f%%)\n",
        n_flagged, pct_flagged
      ))
      if (pct_flagged > 50) {
        cat("    (High percentage - may indicate crosswalk issues)\n")
      }
    } else {
      cat("  ✓ No unsuccessful naive merge flags\n")
    }
  }

  if ("flag_total_votes_incongruent" %in% names(df)) {
    n_flagged <- sum(df$flag_total_votes_incongruent == 1, na.rm = TRUE)
    pct_flagged <- 100 * n_flagged / nrow(df)
    if (n_flagged > 0) {
      cat(sprintf(
        "  ⚠️  Rows with total votes incongruent flag: %d (%.2f%%)\n",
        n_flagged, pct_flagged
      ))
      if (pct_flagged > 50) {
        cat("    (High percentage - may indicate vote share calculation issues)\n")
      }

      # Show examples
      examples <- df |>
        filter(flag_total_votes_incongruent == 1) |>
        select(ags, election_year, state, total_vote_share) |>
        head(10)

      if (nrow(examples) > 0) {
        cat("    Examples:\n")
        print(examples)
      }
    } else {
      cat("  ✓ No total votes incongruent flags\n")
    }
  }
}

if (exists("df_harm_21_new") && nrow(df_harm_21_new) > 0) {
  check_flags(df_harm_21_new, "Harmonized to 2021 (2022-2023)")
}
if (exists("df_harm_23_new") && nrow(df_harm_23_new) > 0) {
  check_flags(df_harm_23_new, "Harmonized to 2023 (2022-2023)")
}

# Check 9: Berlin, Hamburg, Bremen coverage ----------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 9: CITY-STATES COVERAGE (Berlin, Hamburg, Bremen)\n")
cat(strrep("=", 70), "\n\n")

city_states <- tibble(
  name = c("Berlin", "Hamburg", "Bremen (Nord)", "Bremen (Süd)"),
  ags = c("11000000", "02000000", "04011000", "04012000"),
  state = c("11", "02", "04", "04")
)

# Check if city-states appear in new data
for (i in 1:nrow(city_states)) {
  city_name <- city_states$name[i]
  ags_code <- city_states$ags[i]
  state_code <- city_states$state[i]

  in_new <- df_new_unharm |>
    filter(ags == ags_code | state == state_code) |>
    nrow()

  in_harm_21 <- if (exists("df_harm_21_new") && nrow(df_harm_21_new) > 0) {
    df_harm_21_new |>
      filter(ags == ags_code | state == state_code) |>
      nrow()
  } else {
    0
  }

  in_harm_23 <- if (exists("df_harm_23_new") && nrow(df_harm_23_new) > 0) {
    df_harm_23_new |>
      filter(ags == ags_code | state == state_code) |>
      nrow()
  } else {
    0
  }

  cat(sprintf("%s (AGS: %s):\n", city_name, ags_code))
  cat(sprintf("  In new unharmonized: %s\n", ifelse(in_new > 0, "Yes", "No")))
  cat(sprintf("  In harm_21: %s\n", ifelse(in_harm_21 > 0, "Yes", "No")))
  cat(sprintf("  In harm_23: %s\n", ifelse(in_harm_23 > 0, "Yes", "No")))
  cat("\n")
}

# Summary ---------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("New state election data (2022-2023) checks completed.\n")
cat("Key findings:\n")
cat("  - Coverage: Niedersachsen 2022, Hessen & Bayern 2023\n")
cat("  - Check output above for specific issues\n\n")

cat("=", strrep("=", 70), "\n")
cat("END OF CHECKS\n")
cat("=", strrep("=", 70), "\n")
