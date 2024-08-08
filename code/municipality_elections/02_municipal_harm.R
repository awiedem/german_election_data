### Harmonize municipal electoral results to 2021 borders
# Vincent Heddesheimer
# First: Aug 08, 2024
# Last: Aug 08, 2024

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)


# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/ags_crosswalks.csv") |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    year = as.character(year),
    weights = pop_cw * population
    ) |>
  select(-ags_name)


# Merge with unharmonized election data -----------------------------------

df <- read_rds("output/municipal_unharm.rds") |>
  # filter years before 1990: no crosswalks available
  filter(election_year >= 1990)

glimpse(df)
glimpse(cw)
table(df$election_year, useNA = "ifany")
table(is.na(df$ags_name))


# inspect -----------------------------------------------------------------

# is there more than one election in one ags in one year?
dupl <-df |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  print(n=Inf) |>
  mutate(id = paste0(ags,"_",election_year))

inspect <- df |>
  mutate(id = paste0(ags,"_",election_year)) |>
  filter(id %in% dupl$id) |>
  arrange(ags, election_year)

fwrite(inspect, "data/municipal_elections/processed_data/duplicates.csv")


# Merge w/ cw -------------------------------------------------------------

# bind with crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year" = "year")) |>
  arrange(ags, election_year)
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)


# Harmonize ---------------------------------------------------------------

# We have four different outcomes per party that we want to harmonize:
# 1. Absolute votes: weighted sum
# 2. Weighted votes: weighted sum
# 3. Absolute seats: weighted sum
# 4. Vote share + turnout: weighted mean

# Weighted sum
sums <- df_cw |>
  group_by(ags_21, ags_name_21, election_year) |>
  summarize_at(
    # 1+2+3: Weighted sum
    vars(eligible_voters:seats_FREIEWÄHLER),
    ~ sum(.x * pop_cw, na.rm = TRUE)
  )

# Weighted mean
means <- df_cw |>
  group_by(ags_21, election_year) |>
  summarize_at(
    # 4: Weighted mean
    vars(prop_CDU:turnout),
    ~ weighted.mean(.x, w = pop_cw, na.rm = TRUE)
  )

  |>
  rename(
    ags = ags_21, year = election_year
  ) |>
  ungroup()
