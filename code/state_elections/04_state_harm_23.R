### Harmonize state election results to 2023 borders
# Vincent Heddesheimer
# Date: 2025

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

cw <- read_rds("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_23 = pad_zero_conditional(ags_2023, 7)
  ) |>
  rename(election_year = year)

# Add Wasdow manually (not in crosswalk files)
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Namens-Grenz-Aenderung/2011.xlsx?__blob=publicationFile
# Check what the 2023 AGS is for Behren-Lübchin (13072010 in 2021)
cw_21_23_check <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds") |>
  filter(ags_2021 == "13072010")

# Get the 2023 AGS (13072010 maps to itself in 2023)
ags_23_wasdow <- if (nrow(cw_21_23_check) > 0) {
  pad_zero_conditional(first(cw_21_23_check$ags_2023), 7)
} else {
  "13072010" # Fallback to 2021 AGS if not found
}

# Create wasdow entry with columns matching cw AFTER rename
wasdow <- tibble(
  ags = "13072115",
  ags_name = "Wasdow",
  election_year = 2011,
  ags_2023 = ags_23_wasdow, # Keep original column name
  ags_name_23 = "Behren-Lübchin",
  pop_cw = 1,
  area_cw = 1,
  population = 0.39,
  area = 26.20,
  employees = NA_real_
) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_23 = pad_zero_conditional(ags_2023, 7) # Add ags_23 column
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
  filter(is.na(ags_23)) |>
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
# multi-target AGS (which map to >1 ags_23 with pop_cw weights)
df_matched <- df_cw |> filter(!is.na(ags_23))
df_unmatched <- df_cw |> filter(is.na(ags_23))

if (nrow(df_unmatched) > 0) {
  df_unmatched <- df_unmatched |>
    mutate(
      year_cw = pmax(pmin(election_year - 1, 2022), 1990)
    ) |>
    select(-ags_23, -pop_cw, -area_cw) |>
    left_join(
      cw |> select(ags, election_year, ags_23, pop_cw, area_cw) |>
        rename(year_cw = election_year),
      by = c("ags", "year_cw")
    )
}

df_cw <- bind_rows(df_matched, df_unmatched)

glimpse(df_cw)

# Check remaining unsuccessful merges
not_merged_final <- df_cw |>
  filter(is.na(ags_23)) |>
  select(ags, election_year) |>
  distinct()

if (nrow(not_merged_final) > 0) {
  cat("WARNING: Still have unsuccessful merges:", nrow(not_merged_final), "\n")
  print(not_merged_final, n = Inf)
}


# Filter out unmatched rows before harmonization
df_cw <- df_cw |> filter(!is.na(ags_23))

# Harmonize ----------------------------------------------------------------

cat("Harmonizing to 2023 borders...\n")

# Harmonize vote counts with weighted sum
votes <- df_cw |>
  group_by(ags_23, election_year) |>
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
  rename(ags = ags_23) |>
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
      mutate(id = paste0(ags_23, "_", election_year)) |>
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


## Add area and population
area_pop <- read_rds("data/covars_municipality/final/ags_area_pop_emp_2023.rds")


glimpse(area_pop)

glimpse(df_harm)


df_final <- df_harm |>
  left_join_check_obs(area_pop, by = c("ags" = "ags_2023", "election_year" = "year")) |>
  mutate(
    ags_name = ags_name_23
  ) |>
  relocate(ags_name, .after = ags) |>
  select(-ags_name_23)

glimpse(df_final)


# Save ----------------------------------------------------------------------

cat("Saving harmonized data...\n")

fwrite(df_final, "data/state_elections/final/state_harm_23.csv")
write_rds(df_final, "data/state_elections/final/state_harm_23.rds")

cat("Done!\n")
cat("Total observations:", nrow(df_final), "\n")
cat("Election years:", paste(sort(unique(df_final$election_year)), collapse = ", "), "\n")
cat("Number of municipalities:", n_distinct(df_final$ags), "\n")

table(df_final$election_year)

# missing csu values in 2023?
df_final %>%
  filter(election_year == 2023) %>%
  filter((is.na(csu) | csu == 0) & state == "09") |>
  glimpse()
# none

df_final %>%
  filter(election_year == 2023) %>%
  filter((is.na(cdu) | cdu == 0) & state == "06") |>
  glimpse()
# none

