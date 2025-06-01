### Harmonize BTW electoral results at county level 1990-2021
# Vincent Heddesheimer
# Created: June, 18, 2024
# Last updated: August, 13, 2024

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# conflict: prefer filter from dplyr
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")


# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/final/cty_crosswalks.csv") |>
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


View(cw)

# Eisenach in cw?
cw |>
  filter(str_detect(county_name, "Eisenach"))

# Read unharmonized election data -----------------------------------------

df <- read_rds("data/federal_elections/county_level/final/federal_cty_unharm.rds") |>
  mutate(election_year = year) |>
  filter(election_year >= 1990)

 # Eisenach in df? 16016, 16056, 16063
df |>
  filter(ags == "16016" | ags == "16056" | ags == "16063")


# Vote shares to votes ----------------------------------------------------

df <- df |>
  mutate(
    across(cdu:far_left_w_linke, ~ .x * number_voters)
  )

# Naive merge with unharmonized election data -----------------------------------

df_naive_merge <- df |>
  left_join_check_obs(cw, by = c("ags" = "county_code", "year")) |>
  arrange(ags, year)
# obs. increased, but this is wanted: means that we have changing counties

# is there any ags that did not get merged to ags_21?
not_merged_naive <- df_naive_merge %>%
  filter(election_year < 2021) %>%
  filter(is.na(county_code_21)) %>%
  select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))
not_merged_naive
# If we do not follow the steps below, there are 50 cases.
# We found these by the below code.

glimpse(df)
## Check means of some variables by year

# Dealing with unsuccessful mergers ---------------------------------------

# apply the rules
df <- df |>
  mutate(
    id = paste0(ags, "_", election_year),
    year_cw = case_when(
      # 1. if in 1990, try 1991 as merge year
      id %in% not_merged_naive[not_merged_naive$election_year == 1990, ]$id ~ 1991,
      # 2. if not in 1990, try year - 1
      id %in% not_merged_naive[not_merged_naive$election_year > 1990, ]$id ~ election_year - 1,
      TRUE ~ election_year
    )
  )

# 3. The remaining ags that cannot be merged are districts in Berlin.
# We change their ags to the overall Berlin ags.
df <- df |>
  mutate(
    ags = case_when(
      # when ags starts with "11"
      str_sub(ags, 1, 2) == "11" ~ "11000",
      TRUE ~ ags
    )
  )

# Merge crosswalks with election data -------------------------------------

# Merge crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags" = "county_code", "year_cw" = "year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)

# is there any ags that did not get merged to ags_21?
not_merged <- df_cw %>%
  filter(election_year < 2021) %>%
  filter(is.na(county_code_21)) %>%
  select(ags, election_year) %>%
  distinct()
not_merged
# now, there is no unsuccessful merge.

# Flag the cases where we had to change the ags
df_cw <- df_cw |>
  mutate(
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

table(df_cw$flag_unsuccessful_naive_merge, useNA = "ifany")

glimpse(df_cw)

agg_df <- df_cw %>%
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

df_cw$year

glimpse(df_cw)

## Votes: weighted sum -----------------------------------------------------
votes <- df_cw |>
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
area_pop <- df_cw |>
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
ags21 <- read_excel(path = "data/crosswalks/raw/04_KreiseVorjahr.xlsx", sheet = 2) |>
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
  bind_rows(df_cw |>
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
names(df_harm)

row_sums <- df_harm %>%
  # select(-c(far_left, far_left_wLinke, far_right, cdu_csu)) %>%
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
    turnout = number_voters / eligible_voters
  ) |>
  # Relocate columns
  relocate(turnout, .before = cdu) |>
  relocate(cdu_csu, .after = total_votes)  |>
  relocate(far_right, .after = cdu_csu) |>
  relocate(far_left, .after = far_right) |>
  relocate(far_left_w_linke, .after = far_left) |>
  # relocate(county, .after = state) |>
  relocate(flag_unsuccessful_naive_merge, .after = population) |>
  select(-c(year, state_name, year_cw, id))

names(df_harm)

# Election date ---------------------------------------------------

df_harm <- df_harm |> mutate(election_date = case_when(
  election_year == "1953" ~ lubridate::ymd("1953-09-06"),
  election_year == "1957" ~ lubridate::ymd("1957-09-15"),
  election_year == "1961" ~ lubridate::ymd("1961-09-17"),
  election_year == "1965" ~ lubridate::ymd("1965-09-19"),
  election_year == "1969" ~ lubridate::ymd("1969-09-28"),
  election_year == "1972" ~ lubridate::ymd("1972-11-19"),
  election_year == "1976" ~ lubridate::ymd("1976-10-03"),
  election_year == "1980" ~ lubridate::ymd("1980-10-05"),
  election_year == "1983" ~ lubridate::ymd("1983-03-06"),
  election_year == "1987" ~ lubridate::ymd("1987-01-25"),
  election_year == "1990" ~ lubridate::ymd("1990-12-02"),
  election_year == "1994" ~ lubridate::ymd("1994-10-16"),
  election_year == "1998" ~ lubridate::ymd("1998-09-27"),
  election_year == "2002" ~ lubridate::ymd("2002-09-22"),
  election_year == "2005" ~ lubridate::ymd("2005-09-18"),
  election_year == "2009" ~ lubridate::ymd("2009-09-27"),
  election_year == "2013" ~ lubridate::ymd("2013-09-22"),
  election_year == "2017" ~ lubridate::ymd("2017-09-24"),
  election_year == "2021" ~ lubridate::ymd("2021-09-26"),
  .default = NA
), .after = election_year)

# check whether missing values for election_date
if (df_harm |> filter(is.na(election_date)) |> nrow() > 0) {
  message("Missing values for election_date")
} else {
  message("No missing values for election_date")
}
ga  
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
fwrite(df_harm, file = "data/federal_elections/county_level/final/federal_cty_harm.csv")
write_rds(df_harm, file = "data/federal_elections/county_level/final/federal_cty_harm.rds")



# Inspect -----------------------------------------------------------------

df_harm <- read_rds("data/federal_elections/county_level/final/federal_cty_harm.rds")
names(df_harm)


# Eisenach in df_harm? 16016, 16056, 16063
df_harm |>
  filter(county_code == "16016" | county_code == "16056" | county_code == "16063")



insp_harm <- df_harm |>
  filter(county_code == "03101" & election_year == 1994) |>
  mutate(across(cdu:zentrum, ~ .x * total_votes)) |>
  pivot_longer(
    cols = cdu:zentrum,
    names_to = "var",
    values_to = "value"
  ) |>
  filter(value != 0) |>
  select(var, value) |>
  filter(!(var %in% c("far_right", "far_left", "far_left_wLinke", "cdu_csu")))


inspect_unharm <- read_rds("data/federal_elections/county_level/final/federal_cty_unharm.rds") 

# inspect
names(inspect_unharm)
insp <- inspect_unharm |>
  filter(ags == "03101" & year == 1994) |>
  pivot_longer(
    cols = cdu:zentrum,
    names_to = "var",
    values_to = "value"
  ) |>
  filter(value != 0) |>
  select(var, value)


### END