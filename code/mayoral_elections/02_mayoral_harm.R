### Harmonize mayoral election results to 2021 borders
# Vincent Heddesheimer
# First: March 02, 2026
#
# Mayoral elections differ fundamentally from party-vote elections (federal,
# state). Each row has ONE winner (party, votes, voteshare) rather than vote
# totals for multiple parties. This means:
#
# - Grouping by (ags_21, election_date) preserves runoff elections as separate
#   rows AND handles non-synchronized elections from different predecessor
#   municipalities naturally (they remain as separate rows).
# - For N:1 mergers with same-date elections (e.g. Bayern synchronized
#   elections): aggregate numeric counts via population-weighted sum, pick
#   winner info from the largest predecessor by population.
# - For 1:N splits: duplicate the election result to each successor
#   municipality, weight-split the counts.
# - Rheinland-Pfalz has percentage-only data (no absolute counts) — turnout
#   and winner_voteshare use weighted-mean fallback.
#
# Election types excluded from harmonization:
# - VG-Bürgermeisterwahl (RLP) — uses VG pseudo-AGS, not municipality AGS
# - SG-Bürgermeisterwahl (NI) — uses Samtgemeinde AGS, not in muni crosswalk
# - Landratswahl (NI/RLP) — uses county-level AGS, not in muni crosswalk
#
# Crosswalk coverage:
# - 1990-2020: full annual coverage via ags_crosswalks.csv
# - 2021+: identity mapping (already in 2021 boundaries)
# - Pre-1990: uses 1990 crosswalk as fallback (flagged)

rm(list = ls())
gc()

conflicts_prefer(dplyr::filter)

options(scipen = 999)


# 1. Load data -------------------------------------------------------------

df <- read_rds("data/mayoral_elections/final/mayoral_unharm.rds") |>
  as_tibble()

cat("Loaded mayoral_unharm:", nrow(df), "rows\n")
cat("States:", paste(sort(unique(df$state)), collapse = ", "), "\n")
cat("Year range:", min(df$election_year), "-", max(df$election_year), "\n")
table(df$election_type, useNA = "ifany")


# 2. Filter election types -------------------------------------------------

# Only Bürgermeisterwahl and Oberbürgermeisterwahl can be harmonized —
# VG/SG/Landrat elections use pseudo-AGS codes that cannot map through the
# municipality crosswalk.
harmonizable_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")

df_excluded <- df |> filter(!election_type %in% harmonizable_types)
df <- df |> filter(election_type %in% harmonizable_types)

cat("\nHarmonizable rows:", nrow(df), "\n")
cat("Excluded rows (VG/SG/Landrat):", nrow(df_excluded), "\n")
if (nrow(df_excluded) > 0) {
  cat("Excluded by type:\n")
  print(table(df_excluded$election_type))
}

# Deduplicate: remove any exact duplicates (e.g. Bayern 1948)
n_before <- nrow(df)
df <- df |> distinct(ags, election_date, election_type, .keep_all = TRUE)
if (nrow(df) < n_before) {
  cat("Removed", n_before - nrow(df), "duplicate rows\n")
}


# 3. Load crosswalks -------------------------------------------------------

cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# Crosswalk year range
cat("\nCrosswalk year range:", min(cw$year), "-", max(cw$year), "\n")

# Assign crosswalk lookup year
df <- df |>
  mutate(
    cw_year = case_when(
      election_year >= 2021 ~ NA_integer_,    # identity mapping
      election_year >= 1990 ~ election_year,   # direct crosswalk lookup
      election_year < 1990  ~ 1990L            # pre-1990 fallback
    ),
    flag_pre_1990 = as.integer(election_year < 1990)
  )

cat("Pre-1990 rows (using 1990 fallback):", sum(df$flag_pre_1990), "\n")
cat("Post-2020 rows (identity mapping):", sum(is.na(df$cw_year)), "\n")


# 4. Handle post-2020 data (identity mapping) ------------------------------

df_post2020 <- df |>
  filter(is.na(cw_year)) |>
  mutate(
    ags_21 = ags,
    pop_cw = 1,
    population = NA_real_,
    flag_unsuccessful_naive_merge = 0L
  )

df_pre2021 <- df |> filter(!is.na(cw_year))

cat("\nPost-2020:", nrow(df_post2020), "rows (identity)\n")
cat("Pre-2021:", nrow(df_pre2021), "rows (need crosswalk)\n")


# 5. Naive merge with crosswalk --------------------------------------------

df_cw_naive <- df_pre2021 |>
  left_join(
    cw |> select(ags, year, ags_21, ags_name_21, pop_cw, population),
    by = c("ags", "cw_year" = "year"),
    relationship = "many-to-many"
  )

not_merged_naive <- df_cw_naive |>
  filter(is.na(ags_21)) |>
  select(ags, election_year, cw_year) |>
  distinct() |>
  mutate(id = paste0(ags, "_", election_year))

cat("\nUnsuccessful naive merges:", nrow(not_merged_naive),
    "unique (ags, election_year) pairs\n")


# 6. Handle unsuccessful merges --------------------------------------------

# Strategy: try year - 1 as fallback, then year + 1
# (Some municipalities retain old AGS one year after boundary change)
if (nrow(not_merged_naive) > 0) {
  df_matched <- df_cw_naive |> filter(!is.na(ags_21))
  df_unmatched <- df_cw_naive |>
    filter(is.na(ags_21)) |>
    select(-ags_21, -ags_name_21, -pop_cw, -population)

  # Try year - 1
  df_try_minus1 <- df_unmatched |>
    mutate(cw_year_try = as.integer(pmax(cw_year - 1L, 1990L))) |>
    left_join(
      cw |> select(ags, year, ags_21, ags_name_21, pop_cw, population),
      by = c("ags", "cw_year_try" = "year")
    )

  df_fixed_minus1 <- df_try_minus1 |> filter(!is.na(ags_21))
  df_still_unmatched <- df_try_minus1 |>
    filter(is.na(ags_21)) |>
    select(-ags_21, -ags_name_21, -pop_cw, -population, -cw_year_try)

  # Try year + 1 for remaining
  if (nrow(df_still_unmatched) > 0) {
    df_try_plus1 <- df_still_unmatched |>
      mutate(cw_year_try = as.integer(pmin(cw_year + 1L, 2020L))) |>
      left_join(
        cw |> select(ags, year, ags_21, ags_name_21, pop_cw, population),
        by = c("ags", "cw_year_try" = "year")
      )
    df_fixed_plus1 <- df_try_plus1 |> filter(!is.na(ags_21))
    df_final_unmatched <- df_try_plus1 |> filter(is.na(ags_21))
  } else {
    df_fixed_plus1 <- df_still_unmatched[0, ]
    df_final_unmatched <- df_still_unmatched[0, ]
  }

  # Combine all matched rows
  df_cw <- bind_rows(
    df_matched,
    df_fixed_minus1 |> select(-cw_year_try),
    df_fixed_plus1 |> select(-cw_year_try)
  )

  cat("Fixed via year-1 fallback:", nrow(df_fixed_minus1), "rows\n")
  cat("Fixed via year+1 fallback:", nrow(df_fixed_plus1), "rows\n")
  cat("Still unmatched:", nrow(df_final_unmatched), "rows\n")

  if (nrow(df_final_unmatched) > 0) {
    still_unmatched_summary <- df_final_unmatched |>
      select(ags, ags_name, state, election_year) |>
      distinct()
    cat("\nUnmatched AGS codes (will be dropped):\n")
    print(still_unmatched_summary, n = min(50, nrow(still_unmatched_summary)))
  }
} else {
  df_cw <- df_cw_naive
  df_final_unmatched <- df_cw_naive[0, ]
}

# Flag rows that required a fallback
df_cw <- df_cw |>
  mutate(
    id = paste0(ags, "_", election_year),
    flag_unsuccessful_naive_merge = as.integer(id %in% not_merged_naive$id)
  ) |>
  select(-id)


# 7. Combine pre-2021 and post-2020 data -----------------------------------

# Ensure consistent columns before binding
common_cols <- c("ags", "ags_name", "state", "state_name",
                 "election_year", "election_date", "election_type", "round",
                 "eligible_voters", "number_voters", "valid_votes",
                 "invalid_votes", "turnout", "winner_party",
                 "winner_votes", "winner_voteshare",
                 "cw_year", "flag_pre_1990",
                 "ags_21", "pop_cw", "population",
                 "flag_unsuccessful_naive_merge")

# Add missing columns to post-2020 if needed
for (col in setdiff(common_cols, names(df_post2020))) {
  df_post2020[[col]] <- NA
}

df_all <- bind_rows(
  df_cw |> select(any_of(common_cols)),
  df_post2020 |> select(any_of(common_cols))
)

cat("\nTotal rows before aggregation:", nrow(df_all), "\n")

# Drop rows with no ags_21 mapping
n_dropped <- sum(is.na(df_all$ags_21))
cat("Dropping", n_dropped, "rows with no ags_21 mapping\n")
df_all <- df_all |> filter(!is.na(ags_21))

cat("Total rows after dropping unmatched:", nrow(df_all), "\n")


# 8. Aggregation -----------------------------------------------------------

# Weight for selecting the dominant predecessor
df_all <- df_all |>
  mutate(weight = pop_cw * coalesce(population, 1))

# 8a. Aggregate numeric count columns via weighted sum
count_cols <- c("eligible_voters", "number_voters", "valid_votes",
                "invalid_votes", "winner_votes")

df_counts <- df_all |>
  group_by(ags_21, election_date) |>
  summarise(
    across(
      all_of(count_cols),
      ~ if (all(is.na(.x))) NA_real_ else sum(.x * pop_cw, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(across(all_of(count_cols), ~ round(.x, digits = 0)))

# 8b. Pick categorical variables from the dominant predecessor
df_categorical <- df_all |>
  group_by(ags_21, election_date) |>
  slice_max(weight, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ags_21, election_date,
         winner_party, election_type, election_year,
         round, state, state_name, ags_name)

# 8c. Compute weighted means of share columns (fallback for RLP)
df_pct <- df_all |>
  group_by(ags_21, election_date) |>
  summarise(
    turnout_wmean = if (all(is.na(turnout))) NA_real_
                    else weighted.mean(turnout, w = weight, na.rm = TRUE),
    voteshare_wmean = if (all(is.na(winner_voteshare))) NA_real_
                      else weighted.mean(winner_voteshare, w = weight, na.rm = TRUE),
    .groups = "drop"
  )

# 8d. Track flags and predecessor count per group
df_flags <- df_all |>
  group_by(ags_21, election_date) |>
  summarise(
    flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE),
    flag_pre_1990 = max(flag_pre_1990, na.rm = TRUE),
    n_predecessors = n(),
    .groups = "drop"
  )

# 8e. Combine everything
df_harm <- df_counts |>
  left_join(df_categorical, by = c("ags_21", "election_date")) |>
  left_join(df_pct, by = c("ags_21", "election_date")) |>
  left_join(df_flags, by = c("ags_21", "election_date")) |>
  mutate(
    # Recompute turnout from aggregated counts; fall back to weighted mean
    turnout = case_when(
      !is.na(number_voters) & !is.na(eligible_voters) & eligible_voters > 0 ~
        number_voters / eligible_voters,
      TRUE ~ turnout_wmean
    ),
    # Recompute winner voteshare from aggregated counts; fall back to weighted mean
    winner_voteshare = case_when(
      !is.na(winner_votes) & !is.na(valid_votes) & valid_votes > 0 ~
        winner_votes / valid_votes,
      TRUE ~ voteshare_wmean
    )
  ) |>
  select(-turnout_wmean, -voteshare_wmean) |>
  rename(ags = ags_21)


# 9. Quality flags ---------------------------------------------------------

df_harm <- df_harm |>
  mutate(
    flag_aggregated = as.integer(n_predecessors > 1),
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    flag_voteshare_above_1 = as.integer(!is.na(winner_voteshare) & winner_voteshare > 1),
    flag_pct_only = as.integer(is.na(eligible_voters) & is.na(valid_votes)),
    # Cap turnout and voteshare at 1 (preserve NAs)
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout),
    winner_voteshare = ifelse(!is.na(winner_voteshare) & winner_voteshare > 1,
                              1, winner_voteshare)
  )


# 10. Final formatting and output ------------------------------------------

df_harm <- df_harm |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  ) |>
  select(
    ags, ags_name, state, state_name,
    election_year, election_date, election_type, round,
    eligible_voters, number_voters, valid_votes, invalid_votes,
    turnout, winner_party, winner_votes, winner_voteshare,
    flag_unsuccessful_naive_merge, flag_pre_1990, flag_aggregated,
    flag_turnout_above_1, flag_voteshare_above_1, flag_pct_only,
    n_predecessors
  ) |>
  arrange(ags, election_date)


# 11. Verification ---------------------------------------------------------

cat("\n=== Verification ===\n")
cat("Output rows:", nrow(df_harm), "\n")
cat("Unique AGS:", n_distinct(df_harm$ags), "\n")
cat("Year range:", min(df_harm$election_year), "-", max(df_harm$election_year), "\n")

# Check for duplicate (ags, election_date)
dupes <- df_harm |>
  group_by(ags, election_date) |>
  filter(n() > 1)
if (nrow(dupes) > 0) {
  cat("WARNING: duplicate (ags, election_date) rows:", nrow(dupes), "\n")
  print(dupes |> select(ags, ags_name, election_date, election_type) |> head(20))
} else {
  cat("No duplicate (ags, election_date) rows — OK\n")
}

# AGS length check
cat("AGS length distribution:\n")
print(table(nchar(df_harm$ags)))

# State distribution
cat("\nRows by state:\n")
print(df_harm |> count(state, state_name) |> arrange(state))

# Flag summary
cat("\nFlag summary:\n")
cat("  flag_unsuccessful_naive_merge:",
    sum(df_harm$flag_unsuccessful_naive_merge, na.rm = TRUE), "\n")
cat("  flag_pre_1990:",
    sum(df_harm$flag_pre_1990, na.rm = TRUE), "\n")
cat("  flag_aggregated:",
    sum(df_harm$flag_aggregated, na.rm = TRUE), "\n")
cat("  flag_pct_only:",
    sum(df_harm$flag_pct_only, na.rm = TRUE), "\n")
cat("  flag_turnout_above_1:",
    sum(df_harm$flag_turnout_above_1, na.rm = TRUE), "\n")
cat("  flag_voteshare_above_1:",
    sum(df_harm$flag_voteshare_above_1, na.rm = TRUE), "\n")

# Spot-check: München
muenchen <- df_harm |> filter(ags == "09162000")
cat("\nMünchen rows:", nrow(muenchen), "\n")
if (nrow(muenchen) > 0) {
  cat("  Year range:", min(muenchen$election_year), "-",
      max(muenchen$election_year), "\n")
  cat("  Max eligible_voters:", max(muenchen$eligible_voters, na.rm = TRUE), "\n")
}

# Spot-check: RLP data preserved
rlp <- df_harm |> filter(state == "07")
cat("\nRLP rows:", nrow(rlp), "\n")
cat("  Counts all NA:", all(is.na(rlp$eligible_voters)), "\n")
cat("  Turnout non-NA:", sum(!is.na(rlp$turnout)), "of", nrow(rlp), "\n")

# Sample aggregated rows
if (sum(df_harm$flag_aggregated, na.rm = TRUE) > 0) {
  cat("\nSample aggregated rows:\n")
  print(
    df_harm |>
      filter(flag_aggregated == 1) |>
      select(ags, ags_name, election_date, winner_party, n_predecessors) |>
      head(20)
  )
}

glimpse(df_harm)


# 12. Save -----------------------------------------------------------------

fwrite(df_harm, "data/mayoral_elections/final/mayoral_harm.csv")
write_rds(df_harm, "data/mayoral_elections/final/mayoral_harm.rds")

cat("\nSaved to data/mayoral_elections/final/mayoral_harm.{csv,rds}\n")
cat("Done.\n")

## END
