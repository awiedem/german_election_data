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

df_old <- read_rds("data/state_elections/final/state_unharm.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    county = str_sub(ags, 1, 5),
    cdu = ifelse(state != "09" & (cdu == 0 | is.na(cdu)), cdu_csu, cdu),
    csu = ifelse(state == "09" & (csu == 0 | is.na(csu)), cdu_csu, csu)
  ) |>
  rename(election_date = date)

df_new <- read_rds("data/state_elections/final/state_2223_unharm.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    county = str_sub(ags, 1, 5),
    cdu = ifelse(state != "09" & (cdu == 0 | is.na(cdu)), cdu_csu, cdu),
    csu = ifelse(state == "09" & (csu == 0 | is.na(csu)), cdu_csu, csu)
  ) |>
  rename(gruene = grune)

glimpse(df_old)
glimpse(df_new)

# Combine old and new data
df <- bind_rows(df_new, df_old) |>
  arrange(ags, election_year)

glimpse(df)
table(df$election_year)

# Convert vote shares to vote counts ----------------------------------------

# Identify party variables (exclude metadata and calculated variables)
party_vars <- df |>
  select(turnout:tierschutz, other) |>
  names()

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
      c(eligible_voters, number_voters, valid_votes, all_of(party_vars)),
      ~ sum(.x, na.rm = TRUE)
    ),
    across(any_of(c("election_date", "county", "date")), first),
    .groups = "drop"
  ) |>
  arrange(ags, election_year)

glimpse(df)

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
df_cw <- df_cw |>
  mutate(
    year_cw = case_when(
      is.na(ags_23) & election_year > 1990 ~ election_year - 1,
      TRUE ~ election_year
    )
  )

glimpse(df_cw)

# Try merging again with adjusted year
df_cw <- df_cw |>
  left_join(
    cw |>
      select(ags, election_year, ags_23, pop_cw, area_cw) |>
      rename(year_cw = election_year, pop_cw_alt = pop_cw, area_cw_alt = area_cw),
    by = c("ags", "year_cw")
  ) |>
  mutate(
    ags_23 = ifelse(is.na(ags_23.x), ags_23.y, ags_23.x),
    pop_cw = ifelse(is.na(pop_cw), pop_cw_alt, pop_cw),
    area_cw = ifelse(is.na(area_cw), area_cw_alt, area_cw)
  ) |>
  select(-ags_23.y, -pop_cw_alt, -area_cw_alt)

# Check remaining unsuccessful merges
not_merged_final <- df_cw |>
  filter(is.na(ags_23)) |>
  select(ags, election_year) |>
  distinct()

if (nrow(not_merged_final) > 0) {
  cat("WARNING: Still have unsuccessful merges:", nrow(not_merged_final), "\n")
  print(not_merged_final, n = Inf)
}


# Harmonize ----------------------------------------------------------------

cat("Harmonizing to 2023 borders...\n")

# Harmonize vote counts with weighted sum
votes <- df_cw |>
  group_by(ags_23, election_year) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, all_of(party_vars)),
      ~ sum(.x * pop_cw, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(eligible_voters, number_voters, valid_votes, all_of(party_vars)),
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
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  ) |>
  relocate(state, .after = election_year) |>
  relocate(state_name, .after = state) |>
  arrange(ags, election_year)

glimpse(df_harm)

# Add election_date
df_harm <- df_harm |>
  mutate(
    election_date = case_when(
      election_year == 2006 & state_name == "Berlin" ~ ymd("2006-09-17"),
      election_year == 2008 & state_name == "Bavaria" ~ ymd("2008-09-28"),
      election_year == 2008 & state_name == "Niedersachsen" ~ ymd("2008-01-27"),
      election_year == 2009 & state_name == "Brandenburg" ~ ymd("2009-09-27"),
      election_year == 2009 & state_name == "Hesse" ~ ymd("2009-01-18"),
      election_year == 2009 & state_name == "Saarland" ~ ymd("2009-08-30"),
      election_year == 2009 & state_name == "Saxony" ~ ymd("2009-08-30"),
      election_year == 2009 & state_name == "Schleswig-Holstein" ~ ymd("2009-09-27"),
      election_year == 2009 & state_name == "Thuringia" ~ ymd("2009-08-30"),
      election_year == 2010 & state_name == "North Rhine-Westphalia" ~ ymd("2010-05-09"),
      election_year == 2011 & state_name == "Baden-Württemberg" ~ ymd("2011-03-27"),
      election_year == 2011 & state_name == "Berlin" ~ ymd("2011-09-18"),
      election_year == 2011 & state_name == "Mecklenburg-Vorpommern" ~ ymd("2011-09-04"),
      election_year == 2011 & state_name == "Rhineland-Palatinate" ~ ymd("2011-03-27"),
      election_year == 2011 & state_name == "Saxony-Anhalt" ~ ymd("2011-03-20"),
      election_year == 2012 & state_name == "Saarland" ~ ymd("2012-03-25"),
      election_year == 2012 & state_name == "Schleswig-Holstein" ~ ymd("2012-05-06"),
      election_year == 2012 & state_name == "North Rhine-Westphalia" ~ ymd("2012-05-13"),
      election_year == 2013 & state_name == "Bavaria" ~ ymd("2013-09-15"),
      election_year == 2013 & state_name == "Hesse" ~ ymd("2013-09-22"),
      election_year == 2013 & state_name == "Niedersachsen" ~ ymd("2013-01-20"),
      election_year == 2014 & state_name == "Brandenburg" ~ ymd("2014-09-14"),
      election_year == 2014 & state_name == "Saxony" ~ ymd("2014-08-31"),
      election_year == 2014 & state_name == "Thuringia" ~ ymd("2014-09-14"),
      election_year == 2016 & state_name == "Baden-Württemberg" ~ ymd("2016-03-13"),
      election_year == 2016 & state_name == "Berlin" ~ ymd("2016-09-18"),
      election_year == 2016 & state_name == "Mecklenburg-Vorpommern" ~ ymd("2016-09-04"),
      election_year == 2016 & state_name == "Rhineland-Palatinate" ~ ymd("2016-03-13"),
      election_year == 2016 & state_name == "Saxony-Anhalt" ~ ymd("2016-03-13"),
      election_year == 2017 & state_name == "Niedersachsen" ~ ymd("2017-10-15"),
      election_year == 2017 & state_name == "North Rhine-Westphalia" ~ ymd("2017-05-14"),
      election_year == 2017 & state_name == "Saarland" ~ ymd("2017-03-26"),
      election_year == 2018 & state_name == "Bavaria" ~ ymd("2018-10-14"),
      election_year == 2018 & state_name == "Hesse" ~ ymd("2018-10-28"),
      election_year == 2019 & state_name == "Brandenburg" ~ ymd("2019-09-01"),
      election_year == 2019 & state_name == "Saxony" ~ ymd("2019-09-01"),
      election_year == 2019 & state_name == "Thuringia" ~ ymd("2019-10-27"),
      election_year == 2022 & state_name == "Niedersachsen" ~ ymd("2022-10-09"),
      election_year == 2023 & state_name == "Bavaria" ~ ymd("2023-10-08"),
      election_year == 2023 & state_name == "Hesse" ~ ymd("2023-10-08"),
      TRUE ~ NA_Date_
    )
  ) |>
  relocate(election_date, .after = election_year)

table(df_new$election_date)
df_new |>
  distinct(election_date, state)

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
      select(., cdu:other, -cdu_csu),
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

