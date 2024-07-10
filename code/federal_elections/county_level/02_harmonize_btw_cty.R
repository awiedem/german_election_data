### Harmonize BTW electoral results at couty level 1953-2021
# Vincent Heddesheimer
# Created: June, 18, 2024
# Last updated: 

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)


# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/county_crosswalks.csv")

glimpse(cw)

# how many county_code_21 for each year?
cw |>
  distinct(county_code_21, year) |>
  count(year)

## DF : in year 2021, was the muni ever part of a merger?

cw_info_ever_merged_cc_21 <- cw %>%
  group_by(county_code_21, year) %>%
  count() %>%
  ungroup() %>%
  group_by(county_code_21) %>%
  summarise(ever_merged = any(n > 1)) %>%
  ungroup()

# Merge with unharmonized election data -----------------------------------

df <- read_rds("data/federal_elections/county_level/processed_data/btw_cty_1953_2021_unharm.rds")

## Check means of some variables by year

agg_df <- df %>%
  group_by(election_year) %>%
  summarise(
    mean_valid_votes = mean(valid_votes, na.rm = TRUE),
    mean_number_voters = mean(number_voters, na.rm = TRUE),
    mean_population = mean(population, na.rm = TRUE),
    mean_cdu_csu = mean(cdu_csu, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = contains("mean"),
    names_to = "variable", values_to = "value"
  )

## Distribution

ggplot(agg_df, aes(x = election_year, y = value, color = variable)) +
  geom_point() +
  theme_hanno() +
  labs(
    title = "Descriptives of BTW data",
    x = "Year",
    y = "Mean value"
  ) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1", name = "") +
  facet_wrap(~variable, scales = "free_y")

# Harmonize ---------------------------------------------------------------

df$election_year

## Votes: weighted sum -----------------------------------------------------
votes <- df |>
  filter(election_year < 2021) |>
  group_by(ags_21, election_year) |>
  summarise(
    unique_mailin = max(unique_mailin),
    unique_multi_mailin = max(unique_multi_mailin),
    across(
      eligible_voters:left_wing_wLinke,
      ~ sum(.x * pop_cw, na.rm = TRUE)
    )
  ) |>
  # Round
  mutate(across(
    eligible_voters:left_wing_wLinke,
    ~ round(.x, digits = 0)
  )) |>
  ungroup()

## Population & area: weighted sum -----------------------------------------
area_pop <- df |>
  filter(election_year < 2021) |>
  group_by(ags_21, election_year) |>
  summarise(
    area = sum(area * area_cw, na.rm = TRUE),
    population = sum(population * pop_cw, na.rm = TRUE)
  ) |>
  # Round
  mutate(
    area = round(area, digits = 2),
    population = round(population, digits = 1)
  ) |>
  ungroup()

# Get population & area for 2021
ags21 <- read_excel(path = "data/crosswalks/31122021_Auszug_GV.xlsx", sheet = 2) |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    population = `...10`
  ) |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde),
    election_year = 2021
  ) |>
  slice(6:16065) |>
  filter(!is.na(Gemeinde)) |>
  select(ags, election_year, area, population)

# Create full df ----------------------------------------------------------

# Bind back to dataframe
df_harm <- votes |>
  # Rename ags
  left_join_check_obs(area_pop, by = c("ags_21", "election_year")) |>
  rename(ags = ags_21) |>
  # Bind 2021 data (that was unharmonized)
  bind_rows(df |>
              filter(election_year == 2021) |>
              select(-c(
                state, state_name, year,
                pop_cw, area_cw, ags_21
              ))) |>
  # Create state variable
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = str_sub(ags, end = -7),
    county = substr(ags, 1, 5)
  ) |>
  relocate(state, .after = election_year)

# Continue transformation
df_harm <- df_harm |>
  # Remove rows that have no voting data
  filter(eligible_voters != 0 & number_voters != 0) %>%
  left_join_check_obs(ags21, by = c("ags", "election_year")) |>
  mutate(
    area = ifelse(!is.na(area.x), area.x, area.y),
    population = ifelse(!is.na(population.x), population.x, population.y)
  ) |>
  select(-c(area.x, area.y, population.x, population.y))

# Calculate vote share & turnout ------------------------------------------

## First: row sum of votes for all parties

row_sums <- df_harm %>%
  select(-c(left_wing, left_wing_wLinke, right_wing, cdu_csu)) %>%
  select(cdu:zentrum) %>%
  rowSums(na.rm = TRUE)

summary(row_sums)

## Giant row sum?
## I checked and this seems to be units above the municipality level
## But we should maybe still investigate? 

## Merge to data

df_harm <- df_harm %>%
  mutate(
    total_votes = row_sums
  )

df_harm <- df_harm |>
  mutate(
    across(cdu:left_wing_wLinke, ~ .x / total_votes),
    turnout = total_votes / eligible_voters
  ) |>
  # Relocate columns
  relocate(turnout, .before = cdu) |>
  relocate(right_wing, .after = bsa) |>
  relocate(left_wing, .after = right_wing) |>
  relocate(left_wing_wLinke, .after = left_wing)

# AfD to NA for years prior to 2013

df_harm <- df_harm %>%
  mutate(
    afd = ifelse(election_year < 2013, NA, afd)
  )

## Save this now:

# Write .csv file
fwrite(df_harm, file = "data/federal_elections/municipality_level/processed_data/btw_1980_2021_harm21.csv")

### END