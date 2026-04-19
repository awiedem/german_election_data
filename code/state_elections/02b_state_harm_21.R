### Harmonize state election results to 2021 borders
# Based on 05_state_harm_25.R, adapted for 2021 crosswalk
# Uses weighted sum of counts method (not weighted mean of shares)
# Date: 2026

rm(list = ls())

conflict_prefer("filter", "dplyr")

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR"
)

# Load unharmonized data ----------------------------------------------------

cat("Loading unharmonized state election data...\n")

df <- read_rds("data/state_elections/final/state_unharm.rds") |>
  as_tibble() |>
  filter(election_year >= 1990) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    county = str_sub(ags, 1, 5),
    cdu = ifelse(state != "09" & (cdu == 0 | is.na(cdu)), cdu_csu, cdu),
    csu = ifelse(state == "09" & (csu == 0 | is.na(csu)), cdu_csu, csu)
  ) |>
  arrange(ags, election_year)

glimpse(df)
table(df$election_year)

# Convert vote shares to vote counts ----------------------------------------

# Identify party variables by excluding metadata columns
metadata_cols <- c("ags", "county", "election_year", "state", "election_date",
                   "eligible_voters", "number_voters", "valid_votes",
                   "invalid_votes", "turnout", "other", "cdu_csu",
                   "flag_naive_turnout_above_1", "flag_no_valid_votes",
                   "flag_briefwahl_only")
party_vars <- setdiff(names(df), metadata_cols)

cat("Party variables found:", paste(party_vars, collapse = ", "), "\n")

# For rows with NA valid_votes (e.g. HB pre-1999 percentage-only data),
# impute a weight from number_voters or eligible_voters so that the
# share→count→share round-trip preserves the original percentages.
n_na_vv <- sum(is.na(df$valid_votes))
if (n_na_vv > 0) {
  cat(sprintf("Imputing valid_votes weight for %d rows (using number_voters/eligible_voters)\n", n_na_vv))
  df <- df |>
    mutate(valid_votes = case_when(
      !is.na(valid_votes) ~ valid_votes,
      !is.na(number_voters) & number_voters > 0 ~ number_voters,
      !is.na(eligible_voters) & eligible_voters > 0 ~ eligible_voters,
      TRUE ~ 1  # fallback: unit weight (preserves shares in 1:1 mapping)
    ))
}

# Convert vote shares to vote counts
df <- df |>
  mutate(
    across(all_of(party_vars), ~ .x * valid_votes)
  )

# Handle Berlin, Hamburg districts and known problematic AGS codes -----------
# This should be done before crosswalk merge

df <- df |>
  mutate(
    ags = case_when(
      # Berlin: aggregate all districts to overall Berlin AGS
      str_sub(ags, 1, 2) == "11" ~ "11000000",
      # Hamburg: aggregate all districts to overall Hamburg AGS
      str_sub(ags, 1, 2) == "02" ~ "02000000",
      # Rhineland-Palatinate corrections (from 02_state_harm.R)
      ags == "07140502" & election_year == 2011 ~ "07135050", # Lahr
      ags == "07140503" & election_year == 2011 ~ "07135063", # Mörsdorf
      ags == "07140504" & election_year == 2011 ~ "07135094", # Zilshausen
      ags == "07232502" & election_year == 2011 ~ "07232021", # Brimingen
      ags == "07232502" & election_year == 2016 ~ "07232021", # Brimingen (2016!)
      ags == "07235207" & election_year == 2011 ~ "07231207", # Trittenheim
      # Eastern Germany AGS corrections (from federal harm scripts)
      # Sachsen-Anhalt 1990: wrong 3rd digit in DDR-era encoding
      ags == "15228170" & election_year == 1990 ~ "15028170", # Kleinheringen
      ags == "15228280" & election_year == 1990 ~ "15028280", # Neidschütz
      ags == "15228380" & election_year == 1990 ~ "15028380", # Wettaburg
      ags == "15320590" & election_year == 1990 ~ "15020590", # Wedringen
      ags == "15336010" & election_year == 1990 ~ "15036010", # Abbendorf
      ags == "15336290" & election_year == 1990 ~ "15036290", # Holzhausen
      ags == "15336660" & election_year == 1990 ~ "15036660", # Waddekath
      # Thuringia 1994: AGS renumbered during Kreisreform
      ags == "16063057" & election_year == 1994 ~ "16063094", # Moorgrund
      ags == "16063047" & election_year == 1994 ~ "16016410", # Kupfersuhl
      ags == "16063056" & election_year == 1994 ~ "16015420", # Möhra
      ags == "16069022" & election_year == 1994 ~ "16023360", # Heßberg
      ags == "16073098" & election_year == 1994 ~ "16033700", # Weißen
      # Saxony 1994: Kreis renumbered
      ags == "14082220" & election_year == 1994 ~ "14032270", # Krumbach
      ags == "14085170" & election_year == 1994 ~ "14031310", # Naunhof
      # Brandenburg 1990: kreisfreie Städte old encoding (060 suffix)
      ags == "12003060" & election_year == 1990 ~ "12003000", # Eisenhüttenstadt
      ags == "12006060" & election_year == 1990 ~ "12006000", # Schwedt/Oder
      # Sachsen-Anhalt 1994: Merzien post-Kreisreform code → pre-reform
      ags == "15159029" & election_year == 1994 ~ "15026310", # Merzien
      # Sachsen 1994: Cunsdorf dissolved 1995 into Elsterberg (not in CW)
      ags == "14045730" & election_year == 1994 ~ "14045610", # Cunsdorf → Elsterberg
      # Post-2021 mergers: map to main constituent (geographic approximation)
      ags == "14522275" & election_year == 2024 ~ "14522450", # Jahnatal → Ostrau (formed 2023)
      ags == "16061119" & election_year == 2024 ~ "16061097", # Uder (merged 2024) → old Uder
      ags == "16076094" & election_year == 2024 ~ "16076004", # Berga-Wünschendorf → Berga/Elster
      TRUE ~ ags
    )
  )

# NA-preserving sum: returns NA when ALL inputs are NA (sum(NA, na.rm=TRUE) → 0 in base R)
sum_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# After correcting AGS codes, aggregate any duplicates
df <- df |>
  group_by(ags, election_year, state) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum_na(.x)
    ),
    across(any_of(c("election_date", "county")), first),
    .groups = "drop"
  ) |>
  arrange(ags, election_year)

glimpse(df)

# Extract election_date lookup before harmonization (dates are lost in group_by)
date_lookup <- df |>
  distinct(state, election_year, election_date) |>
  filter(!is.na(election_date))

# Load crosswalks -----------------------------------------------------------

cat("Loading crosswalks...\n")

cw <- read_rds("data/crosswalks/final/ags_crosswalks.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  ) |>
  rename(election_year = year)

# Add Wasdow manually (not in crosswalk files)
# Wasdow (13072115) merged into Behren-Lübchin (13072010) in 2011
wasdow <- data.frame(
  ags = "13072115",
  ags_name = "Wasdow",
  election_year = 2011,
  area_cw = 1,
  pop_cw = 1,
  area = 26.20,
  population = 0.39,
  ags_21 = "13072010",
  ags_name_21 = "Behren-Lübchin",
  emp_cw = 1,
  employees = 0.2
)

# Ensure wasdow has all columns from cw
for (col in names(cw)) {
  if (!col %in% names(wasdow)) {
    wasdow[[col]] <- NA
  }
}

# Reorder to match cw column order
wasdow <- wasdow |>
  select(all_of(names(cw)))

# Bind to crosswalk
cw <- cw |>
  bind_rows(wasdow) |>
  arrange(ags, election_year)

glimpse(cw)
table(cw$election_year)

# Merge crosswalks with election data ---------------------------------------

cat("Merging crosswalks with election data...\n")

df_cw_naive <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year"))

# Check for unsuccessful merges
not_merged_naive <- df_cw_naive |>
  filter(is.na(ags_21)) |>
  select(ags, election_year) |>
  distinct() |>
  mutate(id = paste0(ags, "_", election_year))

if (nrow(not_merged_naive) > 0) {
  cat("WARNING: Unsuccessful merges found:", nrow(not_merged_naive), "\n")
  print(not_merged_naive)
}

# Handle special cases
df_cw <- df_cw_naive |>
  mutate(
    id = paste0(ags, "_", election_year),
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

# For observations that didn't merge, try using an adjusted year for crosswalk
# ags_crosswalks.rds only covers years 1990-2020, so for post-2020 elections
# we cap the fallback year at 2020
# NOTE: Only apply fallback join to unmatched rows to avoid duplicating
# multi-target AGS (which map to >1 ags_21 with pop_cw weights)
df_matched <- df_cw |> filter(!is.na(ags_21))
df_unmatched <- df_cw |> filter(is.na(ags_21))

if (nrow(df_unmatched) > 0) {
  df_unmatched <- df_unmatched |>
    mutate(
      year_cw = pmax(pmin(election_year - 1, 2020), 1990)
    ) |>
    select(-ags_21, -pop_cw, -area_cw) |>
    left_join(
      cw |> select(ags, election_year, ags_21, pop_cw, area_cw) |>
        rename(year_cw = election_year),
      by = c("ags", "year_cw")
    )
}

df_cw <- bind_rows(df_matched, df_unmatched)

## --- Fuzzy time matching: for still-unmatched AGS, find closest CW year ---
still_unmatched <- df_cw |> filter(is.na(ags_21))
if (nrow(still_unmatched) > 0) {
  n_unmatched_ags <- n_distinct(still_unmatched$ags)
  cat("Fuzzy time matching for", n_unmatched_ags, "unmatched AGS codes...\n")

  df_already_matched <- df_cw |> filter(!is.na(ags_21))

  unmatched_keys <- still_unmatched |> select(ags, election_year) |> distinct()
  cw_available <- cw |> select(ags, election_year) |> distinct() |>
    rename(cw_year = election_year)

  best_cw_year <- unmatched_keys |>
    left_join(cw_available, by = "ags", relationship = "many-to-many") |>
    filter(!is.na(cw_year)) |>
    mutate(year_dist = abs(cw_year - election_year) +
             ifelse(cw_year < election_year, 0.001, 0)) |>
    group_by(ags, election_year) |>
    slice_min(year_dist, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(ags, election_year, cw_year)

  still_unmatched <- still_unmatched |>
    select(-ags_21, -pop_cw, -area_cw) |>
    left_join(best_cw_year, by = c("ags", "election_year")) |>
    left_join(
      cw |> select(ags, election_year, ags_21, pop_cw, area_cw) |>
        rename(cw_year = election_year),
      by = c("ags", "cw_year")
    ) |>
    select(-cw_year)

  n_recovered <- sum(!is.na(still_unmatched$ags_21))
  cat("  Recovered", n_recovered, "of", nrow(still_unmatched),
      "rows via fuzzy time matching\n")
  df_cw <- bind_rows(df_already_matched, still_unmatched)
}

## --- Self-mapping: unmatched AGS that are already valid 2021 codes map to self ---
still_unmatched2 <- df_cw |> filter(is.na(ags_21))
if (nrow(still_unmatched2) > 0) {
  valid_targets <- unique(cw$ags_21)
  self_map <- still_unmatched2$ags %in% valid_targets
  if (any(self_map)) {
    cat("  Self-mapping", sum(self_map), "rows where AGS is already a valid ags_21\n")
    still_unmatched2$ags_21[self_map] <- still_unmatched2$ags[self_map]
    still_unmatched2$pop_cw[self_map] <- 1
    still_unmatched2$area_cw[self_map] <- 1
    df_cw <- bind_rows(df_cw |> filter(!is.na(ags_21)), still_unmatched2)
  }
}

glimpse(df_cw)

# Check remaining unsuccessful merges
not_merged_final <- df_cw |>
  filter(is.na(ags_21)) |>
  select(ags, election_year) |>
  distinct()

if (nrow(not_merged_final) > 0) {
  cat("WARNING: Still have unsuccessful merges:", nrow(not_merged_final), "\n")
  print(not_merged_final, n = Inf)
}


# Filter out unmatched rows before harmonization
df_cw <- df_cw |> filter(!is.na(ags_21))

# Harmonize ----------------------------------------------------------------

cat("Harmonizing to 2021 borders...\n")

# Weighted sum that preserves NA when ALL source values are NA
# (sum(NA * w, na.rm=TRUE) returns 0, but semantically it should be NA)
wsum_na <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  sum(x * w, na.rm = TRUE)
}

# Harmonize vote counts with weighted sum
votes <- df_cw |>
  group_by(ags_21, election_year) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ wsum_na(.x, pop_cw)
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ round(.x, digits = 0)
    )
  )

glimpse(votes)

# Convert vote counts back to vote shares
df_harm <- votes %>%
  mutate(
    across(all_of(party_vars), ~ ifelse(valid_votes > 0, .x / valid_votes, NA_real_)),
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    # Flag and cap harmonized turnout (mirrors unharm safety net)
    flag_harm_turnout_above_1 = ifelse(!is.na(turnout) & is.finite(turnout) & turnout > 1, 1L, 0L),
    turnout = ifelse(is.finite(turnout), turnout, NA_real_),
    turnout = ifelse(!is.na(turnout) & turnout > 1.5, NA_real_, turnout),
    # Recompute derived columns after harmonization
    other = pmax(1 - rowSums(across(all_of(party_vars)), na.rm = TRUE), 0),
    cdu_csu = coalesce(cdu, 0) + coalesce(csu, 0)
  ) |>
  rename(ags = ags_21) |>
  filter(!is.na(ags)) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  ) |>
  relocate(state, .after = election_year) |>
  relocate(state_name, .after = state) |>
  arrange(ags, election_year)

# Diagnostic: harmonized turnout flags
n_flagged <- sum(df_harm$flag_harm_turnout_above_1, na.rm = TRUE)
if (n_flagged > 0) {
  cat(sprintf("WARNING: %d rows with harmonized turnout > 1 (flagged)\n", n_flagged))
  n_capped <- sum(is.na(df_harm$turnout) & df_harm$flag_harm_turnout_above_1 == 1, na.rm = TRUE)
  if (n_capped > 0) cat(sprintf("  Of these, %d rows had turnout > 1.5 → set to NA\n", n_capped))
}

glimpse(df_harm)

# Add election_date from lookup (extracted before harmonization)
df_harm <- df_harm |>
  left_join(date_lookup, by = c("state", "election_year")) |>
  relocate(election_date, .after = election_year)

# Check for missing election dates
if (df_harm |> filter(is.na(election_date)) |> nrow() > 0) {
  cat("WARNING: Missing election dates found\n")
  df_harm |>
    filter(is.na(election_date)) |>
    select(state_name, election_year) |>
    distinct() |>
    print()
}

# Add flag for unsuccessful merges
df_harm <- df_harm |>
  mutate(
    id = paste0(ags, "_", election_year)
  ) |>
  left_join(
    df_cw |>
      mutate(id = paste0(ags_21, "_", election_year)) |>
      select(id, flag_unsuccessful_naive_merge) |>
      distinct() |>
      group_by(id) |>
      summarise(flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE)) |>
      ungroup(),
    by = "id"
  ) |>
  mutate(
    flag_unsuccessful_naive_merge = ifelse(is.na(flag_unsuccessful_naive_merge), 0, flag_unsuccessful_naive_merge)
  ) |>
  select(-id)

# Zero-vote party → NA recoding: parties that received zero votes in an entire
# state-year are recoded from 0 to NA (distinguishes "didn't participate" from
# "ran but got 0 votes"). Matches federal pipeline pattern.
derived_cols <- c("other", "cdu_csu", "far_right", "far_left", "far_left_w_linke")
share_cols <- setdiff(party_vars, derived_cols)
zero_party_lookup <- df_harm |>
  group_by(state, election_year) |>
  summarise(across(all_of(share_cols),
                   ~ all(. == 0 | is.na(.), na.rm = FALSE),
                   .names = "allzero__{.col}"),
            .groups = "drop") |>
  pivot_longer(starts_with("allzero__"),
               names_to = "party", values_to = "all_zero",
               names_prefix = "allzero__") |>
  filter(all_zero)

if (nrow(zero_party_lookup) > 0) {
  df_long <- df_harm |>
    mutate(.row_id = row_number()) |>
    pivot_longer(cols = all_of(share_cols), names_to = "party",
                 values_to = "vote_share") |>
    left_join(zero_party_lookup, by = c("state", "election_year", "party")) |>
    mutate(vote_share = if_else(!is.na(all_zero) & all_zero &
                                  (vote_share == 0 | is.na(vote_share)),
                                NA_real_, vote_share)) |>
    select(-all_zero) |>
    pivot_wider(names_from = "party", values_from = "vote_share")
  df_harm <- df_long |> select(-.row_id) |> arrange(ags, election_year)
  cat("Zero-vote → NA recoding applied\n")
}

# Pooled party columns
far_right_cols <- intersect(
  c("afd", "npd", "rep", "die_rechte", "dvu", "iii_weg", "fap", "ddd", "dsu"),
  names(df_harm))
far_left_cols <- intersect(
  c("dkp", "kpd", "mlpd", "sgp", "psg", "kbw"),
  names(df_harm))

cat("far_right cols:", paste(far_right_cols, collapse = ", "), "\n")
cat("far_left cols:", paste(far_left_cols, collapse = ", "), "\n")

df_harm <- df_harm |>
  mutate(
    far_right = rowSums(across(any_of(far_right_cols)), na.rm = TRUE),
    far_left = rowSums(across(any_of(far_left_cols)), na.rm = TRUE),
    far_left_w_linke = rowSums(across(any_of(c("linke_pds"))),
                                na.rm = TRUE) + far_left
  )

# Total vote share: sum ALL individual party columns (excludes "other")
# Note: total_vote_share < 1 means a non-trivial "other" residual, not data error
all_derived <- c("far_right", "far_left", "far_left_w_linke", "cdu_csu")
tvs_cols <- setdiff(party_vars, all_derived)
df_harm <- df_harm |>
  mutate(
    total_vote_share = round(rowSums(across(all_of(tvs_cols)), na.rm = TRUE), 8),
    flag_other_party_residual = ifelse(
      total_vote_share > 1.001 | total_vote_share < 0.999, 1, 0),
    perc_total_votes_incongruence = round(total_vote_share - 1, 6)
  )

cat("Duplicate ags x election_year rows:",
    sum(duplicated(df_harm[, c("ags", "election_year")])), "\n")

glimpse(df_harm)


# Add area and population --------------------------------------------------

cat("Adding covariates...\n")

# Load covariates (already in 2021 boundaries)
area_pop <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  rename(election_year = year)

glimpse(area_pop)

df_final <- df_harm |>
  left_join_check_obs(area_pop, by = c("ags", "election_year"))

glimpse(df_final)


# Save ----------------------------------------------------------------------

cat("Saving harmonized data...\n")

fwrite(df_final, "data/state_elections/final/state_harm_21.csv")
write_rds(df_final, "data/state_elections/final/state_harm_21.rds")

cat("Done!\n")
cat("Total observations:", nrow(df_final), "\n")
cat("Election years:", paste(sort(unique(df_final$election_year)), collapse = ", "), "\n")
cat("Number of unique municipalities:", n_distinct(df_final$ags), "\n")
cat("States:", paste(sort(unique(df_final$state)), collapse = ", "), "\n")

table(df_final$election_year)
table(df_final$state)

# Check BSW values present for 2024 elections
cat("\nBSW values for 2024 elections:\n")
df_final |>
  filter(election_year == 2024) |>
  group_by(state_name) |>
  summarize(
    n = n(),
    bsw_mean = mean(bsw, na.rm = TRUE),
    bsw_na_count = sum(is.na(bsw))
  ) |>
  print()

# Check CDU/CSU consistency
df_final %>%
  filter(election_year == 2024) %>%
  filter((is.na(csu) | csu == 0) & state == "09") |>
  glimpse()

df_final %>%
  filter(election_year == 2024) %>%
  filter((is.na(cdu) | cdu == 0) & state != "09") |>
  glimpse()

# Check no duplicate ags x election_year
dupes <- df_final |>
  group_by(ags, election_year) |>
  filter(n() > 1) |>
  nrow()
cat("\nDuplicate ags x election_year rows:", dupes, "\n")

# Check turnout > 0
zero_turnout <- df_final |>
  filter(turnout == 0 | is.na(turnout)) |>
  nrow()
cat("Rows with zero or NA turnout:", zero_turnout, "\n")
