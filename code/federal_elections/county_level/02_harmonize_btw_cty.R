### Harmonize BTW electoral results at couty level 1953-2021
# Vincent Heddesheimer
# Created: June, 18, 2024
# Last updated: 

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)


# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/county_crosswalks.csv") |>
  mutate(county_code = pad_zero_conditional(county_code, 4))

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
cw_info_ever_merged_cc_21 |>
  print(n=Inf)

# Merge with unharmonized election data -----------------------------------

df <- read_rds("data/federal_elections/county_level/processed_data/btw_cty_1953_2021_unharm.rds") |>
  mutate(election_year = year) |>
  left_join_check_obs(cw, by = c("ags" = "county_code", "year")) |>
  arrange(ags, year)
# obs. increased, but this is wanted: means that we have changing counties


glimpse(df)
## Check means of some variables by year

agg_df <- df %>%
  group_by(year) %>%
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

ggplot(agg_df, aes(x = year, y = value, color = variable)) +
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

df$year

glimpse(df)

## Votes: weighted sum -----------------------------------------------------
votes <- df |>
  filter(year < 2021) |>
  group_by(county_code_21, election_year) |>
  summarise(
    across(
      eligible_voters:cdu_csu,
      ~ sum(.x * pop_cw, na.rm = TRUE)
    )
  ) |>
  # Round
  mutate(across(
    eligible_voters:cdu_csu,
    ~ round(.x, digits = 0)
  )) |>
  ungroup()

## Population & area: weighted sum -----------------------------------------
area_pop <- df |>
  filter(election_year < 2021) |>
  group_by(county_code_21, election_year) |>
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
ags21 <- read_excel(path = "data/crosswalks/04_KreiseVorjahr.xlsx", sheet = 2) |>
  select(
    ags = `Kreisfreie Städte und Landkreise nach Fläche, Bevölkerung und Bevölkerungsdichte`,
    area = `...5`,
    population = `...6`
  ) |>
  mutate(
    election_year = 2021
  ) |>
  slice(8:476) |>
  filter(!is.na(ags)) |>
  select(ags, election_year, area, population) |>
  # keep only if ags has 5 digits
  filter(nchar(ags) == 5) |>
  # round area to two digits
  mutate(area = as.numeric(area))

# Create full df ----------------------------------------------------------

# Bind back to dataframe
df_harm <- votes |>
  # Rename ags
  left_join_check_obs(area_pop, by = c("county_code_21", "election_year")) |>
  rename(county_code = county_code_21) |>
  mutate(county_code = pad_zero_conditional(county_code, 4)) |>
  # Bind 2021 data (that was unharmonized)
  bind_rows(df |>
              rename(county_code = ags) |>
              filter(election_year == 2021) |>
              select(-c(
                county_name, area, population,
                county_name_21, emp_cw, employees,
                area_cw, pop_cw, county_code_21
              ))) |>
  # Create state variable
  mutate(
    county_code = pad_zero_conditional(county_code, 4),
    state = str_sub(county_code, end = -4)
  ) |>
  relocate(state, .after = election_year)

glimpse(df_harm)

# Continue transformation
df_harm <- df_harm |>
  # Remove rows that have no voting data
  filter(eligible_voters != 0 & number_voters != 0) %>%
  left_join_check_obs(ags21, by = c("county_code" = "ags", "election_year")) |>
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
    across(cdu:cdu_csu, ~ .x / total_votes),
    turnout = total_votes / eligible_voters
  ) |>
  # Relocate columns
  relocate(turnout, .before = cdu) |>
  relocate(right_wing, .after = bsa) |>
  relocate(left_wing, .after = right_wing) |>
  relocate(left_wing_wLinke, .after = left_wing) |>
  relocate(cdu_csu, .after = csu)

# AfD to NA for years prior to 2013

df_harm <- df_harm %>%
  mutate(
    afd = ifelse(election_year < 2013, NA, afd)
  )

# inspect total votes vs valid votes
df_harm %>%
  select(valid_votes, total_votes)

# do they ever differ?
inspect <- df_harm %>%
  filter(valid_votes != total_votes) %>%
  select(county_code, election_year, eligible_voters, number_voters, valid_votes, total_votes)
# yes sometimes they do


## Save this now:

# Write .csv file
fwrite(df_harm, file = "data/federal_elections/county_level/processed_data/btw_1990_2021_harm21.csv")



# Inspect -----------------------------------------------------------------

df_harm <- fread("data/federal_elections/county_level/processed_data/btw_1990_2021_harm21.csv")
df_harm <- fread("~/Documents/GitHub/german_election_data/data/federal_elections/county_level/processed_data/btw_1990_2021_harm21.csv")


inspect <- df_harm |>
  filter(election_year >= 1998)


Hallo

### END