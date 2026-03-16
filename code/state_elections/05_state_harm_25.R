### Harmonize state election results to 2025 borders
# Based on 04_state_harm_23.R
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
                   "invalid_votes", "turnout")
party_vars <- setdiff(names(df), metadata_cols)

cat("Party variables found:", paste(party_vars, collapse = ", "), "\n")

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
      TRUE ~ ags
    )
  )

# After correcting AGS codes, aggregate any duplicates
df <- df |>
  group_by(ags, election_year, state) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum(.x, na.rm = TRUE)
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

cw <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  ) |>
  rename(election_year = year)

# Add Wasdow manually (not in crosswalk files)
# Wasdow (13072115) merged into Behren-Lübchin (13072010) in 2011
# Check what the 2025 AGS is for Behren-Lübchin
cw_25_check <- cw |>
  filter(ags == "13072010" & election_year == 2011)

ags_25_wasdow <- if (nrow(cw_25_check) > 0) {
  first(cw_25_check$ags_25)
} else {
  "13072010" # Fallback
}

ags_name_25_wasdow <- if (nrow(cw_25_check) > 0) {
  first(cw_25_check$ags_name_25)
} else {
  "Behren-Lübchin"
}

# Create wasdow entry with columns matching cw
wasdow <- tibble(
  ags = "13072115",
  ags_name = "Wasdow",
  election_year = 2011,
  ags_25 = ags_25_wasdow,
  ags_name_25 = ags_name_25_wasdow,
  pop_cw = 1,
  area_cw = 1,
  population = 0.39,
  area = 26.20
) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
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
  filter(is.na(ags_25)) |>
  select(ags, election_year) |>
  distinct() |>
  mutate(id = paste0(ags, "_", election_year))

if (nrow(not_merged_naive) > 0) {
  cat("WARNING: Unsuccessful merges found:", nrow(not_merged_naive), "\n")
  print(not_merged_naive)
}

# Handle special cases (similar to federal elections script)
# For now, we'll flag them and continue
df_cw <- df_cw_naive |>
  mutate(
    id = paste0(ags, "_", election_year),
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

# For observations that didn't merge, try using year - 1 for crosswalk
# This handles cases where municipalities merged right after an election
# NOTE: Only apply fallback join to unmatched rows to avoid duplicating
# multi-target AGS (which map to >1 ags_25 with pop_cw weights)
df_matched <- df_cw |> filter(!is.na(ags_25))
df_unmatched <- df_cw |> filter(is.na(ags_25))

if (nrow(df_unmatched) > 0) {
  df_unmatched <- df_unmatched |>
    mutate(
      year_cw = pmax(election_year - 1, 1990)
    ) |>
    select(-ags_25, -pop_cw, -area_cw) |>
    left_join(
      cw |> select(ags, election_year, ags_25, pop_cw, area_cw) |>
        rename(year_cw = election_year),
      by = c("ags", "year_cw")
    )
}

df_cw <- bind_rows(df_matched, df_unmatched)

## --- Fuzzy time matching: for still-unmatched AGS, find closest CW year ---
still_unmatched <- df_cw |> filter(is.na(ags_25))
if (nrow(still_unmatched) > 0) {
  n_unmatched_ags <- n_distinct(still_unmatched$ags)
  cat("Fuzzy time matching for", n_unmatched_ags, "unmatched AGS codes...\n")

  df_already_matched <- df_cw |> filter(!is.na(ags_25))

  # Step 1: find the closest CW year for each (ags, election_year)
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

  # Step 2: join unmatched rows with the best CW year's crosswalk mappings
  still_unmatched <- still_unmatched |>
    select(-ags_25, -pop_cw, -area_cw) |>
    left_join(best_cw_year, by = c("ags", "election_year")) |>
    left_join(
      cw |> select(ags, election_year, ags_25, pop_cw, area_cw) |>
        rename(cw_year = election_year),
      by = c("ags", "cw_year")
    ) |>
    select(-cw_year)

  n_recovered <- sum(!is.na(still_unmatched$ags_25))
  cat("  Recovered", n_recovered, "of", nrow(still_unmatched),
      "rows via fuzzy time matching\n")
  df_cw <- bind_rows(df_already_matched, still_unmatched)
}

glimpse(df_cw)

# Check remaining unsuccessful merges
not_merged_final <- df_cw |>
  filter(is.na(ags_25)) |>
  select(ags, election_year) |>
  distinct()

if (nrow(not_merged_final) > 0) {
  cat("WARNING: Still have unsuccessful merges:", nrow(not_merged_final), "\n")
  print(not_merged_final, n = Inf)
}


# Harmonize ----------------------------------------------------------------

cat("Harmonizing to 2025 borders...\n")

# Harmonize vote counts with weighted sum
votes <- df_cw |>
  group_by(ags_25, election_year) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum(.x * pop_cw, na.rm = TRUE)
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
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_)
  ) |>
  rename(ags = ags_25) |>
  filter(!is.na(ags)) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  ) |>
  relocate(state, .after = election_year) |>
  relocate(state_name, .after = state) |>
  arrange(ags, election_year)

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
      mutate(id = paste0(ags_25, "_", election_year)) |>
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

# Calculate total vote share and flag
df_harm <- df_harm %>%
  mutate(
    total_vote_share = rowSums(
      across(any_of(c("cdu", "csu", "spd", "gruene", "fdp", "linke_pds", "afd", "other"))),
      na.rm = TRUE
    ),
    total_vote_share = round(total_vote_share, 8),
    flag_total_votes_incongruent = ifelse(total_vote_share > 1, 1, 0)
  )

glimpse(df_harm)


# Add area and population --------------------------------------------------

cat("Adding covariates...\n")

# Load 2023 covariates
area_pop_23 <- read_rds("data/covars_municipality/final/ags_area_pop_emp_2023.rds") |>
  mutate(ags_2023 = pad_zero_conditional(ags_2023, 7))

# Load 23->25 crosswalk to map covariates to 2025 boundaries
cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  filter(year == 2023) |>
  select(ags, ags_25, ags_name_25, pop_cw, area_cw) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

# Map covariates from 2023 to 2025 boundaries
area_pop <- area_pop_23 |>
  left_join(cw_23_25, by = c("ags_2023" = "ags")) |>
  mutate(
    ags_25 = coalesce(ags_25, ags_2023),
    ags_name_25 = coalesce(ags_name_25, ags_name_23)
  ) |>
  group_by(ags_25, year) |>
  summarize(
    ags_name_25 = first(ags_name_25),
    area_ags = sum(area_ags * coalesce(area_cw, 1), na.rm = TRUE),
    population_ags = sum(population_ags * coalesce(pop_cw, 1), na.rm = TRUE),
    employees_ags = sum(employees_ags * coalesce(pop_cw, 1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(pop_density_ags = population_ags / area_ags)

glimpse(area_pop)

df_final <- df_harm |>
  left_join_check_obs(area_pop, by = c("ags" = "ags_25", "election_year" = "year")) |>
  mutate(
    ags_name = ags_name_25
  ) |>
  relocate(ags_name, .after = ags) |>
  select(-ags_name_25)

glimpse(df_final)


# Save ----------------------------------------------------------------------

cat("Saving harmonized data...\n")

fwrite(df_final, "data/state_elections/final/state_harm_25.csv")
write_rds(df_final, "data/state_elections/final/state_harm_25.rds")

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
