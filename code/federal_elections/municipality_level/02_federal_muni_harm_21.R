### Harmonize BTW electoral results at muni level 1980-2025
# Harmonize to 2021 municipality borders
# Vincent Heddesheimer, Hanno Hilbig
# November 2025

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR"
)

conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")

# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

# how many ags_21 for each year?
cw |>
  distinct(ags_21, year) |>
  count(year) |>
  print(n = Inf)

## DF : in year 2021, was the muni ever part of a merger?

cw_info_ever_merged_ags_21 <- cw %>%
  group_by(ags_21, year) %>%
  count() %>%
  ungroup() %>%
  group_by(ags_21) %>%
  summarise(ever_merged = any(n > 1)) %>%
  ungroup()


# Read unharmonized election data -----------------------------------------

df <- read_rds("data/federal_elections/municipality_level/final/federal_muni_unharm.rds") |>
  as_tibble() |>
  # remove population & area that were used for weighting multi mail-in districts
  dplyr::select(-c(pop, area)) |>
  # dplyr::filter years before 1990: no crosswalks available
  dplyr::filter(election_year >= 1990) |>
  arrange(ags, election_year)

table(df$election_year)

# check Hamburg & Berlin
df |>
  dplyr::filter(state == "02" | state == "11") |>
  dplyr::filter(election_year < 2021) |>
  select(ags, election_year, state) |>
  as.data.frame()


# Vote shares to votes ----------------------------------------------------
names(df)

df <- df |>
  mutate(
    across(cdu:far_left_w_linke, ~ .x * number_voters)
  )

# Naive merge with unharmonized election data -----------------------------------

glimpse(df)
glimpse(cw)

# bind with crosswalks
df_naive_merge <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year" = "year")) |>
  arrange(ags, election_year)

# number of obs increases: but this is wanted, as we want to harmonize the data

# is there any ags that did not get merged to ags_21?
not_merged_naive <- df_naive_merge %>%
  dplyr::filter(election_year < 2021) %>%
  dplyr::filter(is.na(ags_21)) %>%
  dplyr::select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))

not_merged_naive
# If we do not follow the steps below, there are >1,600 cases.
# We found these by the below code.

# dplyr::filter out all observations for ags that were not merged
obs_not_merged_ags <- df_naive_merge %>%
  dplyr::filter(ags %in% not_merged_naive$ags) %>%
  dplyr::select(ags, ags_name, election_year)

# how often do these ags appear in the data?
table(obs_not_merged_ags$ags)
# some one time, some two, some three

# get all ags that appear one time in obs_not_merged_ags
one_time_ags <- obs_not_merged_ags %>%
  group_by(ags) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::select(ags, election_year) %>%
  mutate(id = paste0(ags, "_", election_year)) %>%
  distinct()

# the great majority of one-timers are from 1990
# 1. attempt: use 1991 as crosswalk year for these ags
# This worked very well! Reduced from 1656 cases to 266 cases!

# fill out the missing values in ags_name:employees with the values for which values are existing
cw_not_merged <- cw |>
  dplyr::filter(ags %in% not_merged_naive$ags)

# Dealing with unsuccessful mergers ---------------------------------------

# define cases where we want to use year - 1
ags_year_cw <- not_merged_naive %>%
  dplyr::filter(
    grepl("^031", id) | # id starts with 031
      grepl("^05", id) |
      id == "07143217_1994" | # id equals 07143217_1994
      grepl("^12", id) | # id starts with 120
      grepl("^14", id) | # id starts with 14
      grepl("^15", id) | # id starts with 15
      grepl("^16", id) # id starts with 16
  ) %>%
  pull(id)

# apply the rules
df <- df |>
  mutate(
    id = paste0(ags, "_", election_year),
    # 1. AGS where we have to change the ags for the respective year but where the year does not change
    ags = case_when(
      id == "16063057_1994" ~ "16063094", # Moorgrund
      TRUE ~ ags
    ),
    year_cw = case_when(
      # 2. Define year for the ones who only exist in 1990 (so that they don't get changes by the next rule)
      id == "15144280_1990" ~ 1990,
      id == "15228170_1990" ~ 1990,
      id == "15228220_1990" ~ 1990,
      id == "15228280_1990" ~ 1990,
      id == "15228380_1990" ~ 1990,
      id == "15320590_1990" ~ 1990,
      id == "15336010_1990" ~ 1990,
      id == "15336290_1990" ~ 1990,
      id == "15336660_1990" ~ 1990,
      id == "16022540_1990" ~ 1990,
      # 3. if in 1990, try 1991 as merge year
      id %in% not_merged_naive[not_merged_naive$election_year == 1990, ]$id ~ 1991,
      # 4. change year for remaining cases
      id == "15085255_2013" ~ 2010,
      id == "14082220_1994" ~ 1993, # Krumbach (but also have to change ags)
      id == "14085170_1994" ~ 1993, # Naunhof (but also have to change ags)
      id == "16063047_1994" ~ 1993, # Kupfersuhl (but also have to change ags)
      id == "16063056_1994" ~ 1993, # Möhra (but also have to change ags)
      id == "16063057_1994" ~ 1994, # Moorgrund (but also have to change ags)
      id == "16069022_1994" ~ 1993, # Heßberg (but also have to change ags)
      id == "16073098_1994" ~ 1993, # Weißen (but also have to change ags)
      id == "12071180_1998" ~ 1996, # Horno (but also have to change ags)
      # 5. if not in 1990, try year - 1
      id %in% not_merged_naive[not_merged_naive$election_year > 1990, ]$id ~ election_year - 1,
      TRUE ~ election_year
    ),
    # 6. wrong third digit: checked with election results Leitband
    # and manually matched ags names btw. election results & crosswalk files
    ags = case_when(
      id == "15144280_1990" ~ "15044280", # REINHARZ
      id == "15228170_1990" ~ "15028170", # KLEINHERINGEN
      id == "15228220_1990" ~ "15028220", # LISSDORF
      id == "15228280_1990" ~ "15028280", # NEIDSCHUETZ
      id == "15228380_1990" ~ "15028380", # WETTABURG
      id == "15320590_1990" ~ "15020590", # WEDRINGEN
      id == "15336010_1990" ~ "15036010", # ABBENDORF
      id == "15336290_1990" ~ "15036290", # HOLZHAUSEN
      id == "15336660_1990" ~ "15036660", # WADDEKATH
      id == "16022540_1990" ~ "15036710", # WOLMERSDORF
      id == "14082220_1994" ~ "14032270", # Krumbach (but also have to change year_cw)
      id == "14085170_1994" ~ "14031310", # Naunhof (but also have to change year_cw)
      id == "16063047_1994" ~ "16016410", # Kupfersuhl (but also have to change year_cw)
      id == "16063056_1994" ~ "16015420", # Möhra (but also have to change year_cw)
      id == "16069022_1994" ~ "16023360", # Heßberg (but also have to change year_cw)
      id == "16073098_1994" ~ "16033700", # Weißen (but also have to change year_cw)
      id == "12071180_1998" ~ "12071180", # Horno (but also have to change year_cw)
      TRUE ~ ags
    )
  )

# Streitholz non existent in crosswalks
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Namens-Grenz-Aenderung/1991.html
streitholz <- data.frame(
  ags = "16022540", ags_name = "Streitholz", year = 1990,
  area_cw = 1, pop_cw = 1,
  area = 1.59, population = 0.104,
  ags_21 = 16061049, ags_name_21 = "Hohes Kreuz",
  emp_cw = NA, employees = NA
)

names(cw)

# bind to cw
cw <- cw |>
  bind_rows(streitholz) |>
  arrange(ags, year)

# Merge crosswalks with election data -------------------------------------

# Merge crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags", "year_cw" = "year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)

# is there any ags that did not get merged to ags_21?
not_merged <- df_cw %>%
  dplyr::filter(election_year < 2021) %>%
  dplyr::filter(is.na(ags_21)) %>%
  dplyr::select(ags, election_year) %>%
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

## Check means of some variables by year

agg_df <- df_cw %>%
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

# ## Distribution

# ggplot(agg_df, aes(x = election_year, y = value, color = variable)) +
#   geom_point() +
#   theme_hanno() +
#   labs(
#     title = "Descriptives of BTW data",
#     x = "Year",
#     y = "Mean value"
#   ) +
#   theme(legend.position = "bottom") +
#   scale_color_brewer(palette = "Set1", name = "") +
#   facet_wrap(~variable, scales = "free_y")


# Inspect 2021 and 2025 election data -------------------------------------

# AGS in 2025 but not in 2021
df_25 <- df_cw %>%
  dplyr::filter(election_year == 2025) %>%
  dplyr::select(ags, election_year) %>%
  distinct()

df_21 <- df_cw %>%
  dplyr::filter(election_year == 2021) %>%
  dplyr::select(ags, election_year) %>%
  distinct()

df_25 %>%
  anti_join(df_21, by = "ags") %>%
  print(n = Inf)
# 91 munis in 2025 but not in 2021

# AGS in 2021 but not in 2025
df_21 %>%
  anti_join(df_25, by = "ags") %>%
  print(n = Inf)
# 56 munis in 2021 but not in 2025


# Crosswalk 2025 BTW data to 2021 level -----------------------------------

glimpse(df_cw)

# Load necessary crosswalks

full_cw <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds")
cw_2023_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds")


# Build 2025-->2023 dis-aggregation table
# (invert 2023-->2025 file)
cw_25_to_23 <- cw_2023_25 %>% # year == 2023 rows
  transmute(
    ags_25 = ags_25, # 2025 code  (was ‘target’)
    ags_23 = ags, # 2023 donor
    pop_w_25_23 = pop_cw, # population share (already sums to 1 within ags_25)
    area_w_25_23 = area_cw,
    population = population,
    area = area
  )

glimpse(cw_25_to_23)


# Map 2023-->2021 using old cross-walks
# (keep only one row per 2023 code and its weight toward a 2021 unit)
cw_23_to_21 <- cw_21_23 %>% # ags_crosswalks.csv  (1990–2021 → 2021)
  transmute(
    ags_23 = ags_2023, # source = 2023 code  (mostly unchanged since 2021)
    ags_21 = ags_2021,
    pop_w_23_21 = w_pop,
    area_w_23_21 = w_area
  )

glimpse(cw_23_to_21)

# For the handful of municipalities born between 2021-01-01 and 2023-12-31 (≈90 units)
# cw_23_to_21 is missing rows.  They all merge into 2021 units later, never split them, so we can safely set their 2021 weight to 1:
cw_23_to_21 <- bind_rows(
  cw_23_to_21,
  cw_25_to_23 %>%
    anti_join(cw_23_to_21, by = "ags_23") %>% # 2023 codes missing in 2021 map
    transmute(
      ags_23,
      ags_21        = ags_23, # identity
      pop_w_23_21   = 1,
      area_w_23_21  = 1
    )
)


# Create a fully-chained 2025 → 2021 cross-walk
cw_25_to_21 <- cw_25_to_23 %>%
  left_join(cw_23_to_21, by = "ags_23") %>%
  mutate(
    pop_w_25_21  = pop_w_25_23 * pop_w_23_21,
    area_w_25_21 = area_w_25_23 * area_w_23_21
  ) %>%
  group_by(ags_25, ags_21) %>% # collapse double paths (rare)
  summarise(
    pop_w_25_21 = sum(pop_w_25_21, na.rm = TRUE),
    area_w_25_21 = sum(area_w_25_21, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    area = sum(area, na.rm = TRUE),
    .groups = "drop"
  )
# Weights still sum to 1 within every 2025 source.

glimpse(cw_25_to_21)

# check
cw_25_to_21 %>%
  filter(pop_w_25_21 < 1) %>%
  print(n = Inf)

glimpse(df_cw)

# Distribute raw 2025 votes to 2021 borders
df25 <- df %>% # one row per 2025 AGS, votes already in counts
  filter(election_year == 2025) %>%
  left_join(cw_25_to_21, by = c("ags" = "ags_25")) %>%
  mutate(
    ags_21 = as.numeric(ags_21),
  ) %>%
  rename(
    pop_cw = pop_w_25_21,
    area_cw = area_w_25_21
  )

# check
names(df25)
glimpse(df25)



df25 %>%
  filter(pop_cw < 1) %>%
  select(ags, election_year, ags_21, population, pop_cw, area, area_cw) %>%
  print(n = Inf)

# check those that are not in 2021
names(df25)
df25 %>%
  anti_join(df |> filter(election_year == 2021) |> mutate(ags = as.numeric(ags)), by = c("ags_21" = "ags")) %>%
  select(ags, election_year, ags_21, population, pop_cw, area, area_cw) %>%
  print(n = Inf)

df25 %>%
  filter(ags_21 == "1001000") %>%
  select(ags_21, year = election_year, pop = population, p_w = pop_cw, area, aa_w = area_cw, cdu, voters = eligible_voters) %>%
  print(n = Inf)

glimpse(df25)


# Harmonize ---------------------------------------------------------------

df_cw$election_year

names(df_cw)

table(df_cw$election_year)

## Votes: weighted sum -----------------------------------------------------
votes <- df_cw |>
  dplyr::filter(election_year < 2021) |>
  bind_rows(df25) |>
  group_by(ags_21, election_year) |>
  summarise(
    unique_mailin = max(unique_mailin),
    unique_multi_mailin = max(unique_multi_mailin),
    across(
      eligible_voters_orig:far_left_w_linke,
      ~ sum(.x * pop_cw, na.rm = TRUE)
    )
  ) |>
  # Round
  mutate(across(
    eligible_voters_orig:far_left_w_linke,
    ~ round(.x, digits = 0)
  )) |>
  ungroup()

table(votes$election_year)

## Population & area: weighted sum -----------------------------------------
area_pop <- df_cw |>
  dplyr::filter(election_year < 2021) |>
  bind_rows(df25) |>
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
ags21 <- read_excel(path = "data/crosswalks/raw/31122021_Auszug_GV.xlsx", sheet = 2) |>
  dplyr::select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    ags_name = `...8`,
    area = `...9`,
    population = `...10`
  ) |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde),
    election_year = 2021,
    population = as.numeric(population) / 1000
  ) |>
  slice(6:16065) |>
  dplyr::filter(!is.na(Gemeinde)) |>
  dplyr::select(ags, ags_name, election_year, area, population)


# check which ags in unharm df in 2021 are not in ags21
df |>
  dplyr::filter(election_year == 2021) |>
  anti_join(ags21, by = "ags")
# they are all in ags21

# check which ags in ags21 are not in unharm df in 2021
ags21 |>
  anti_join(df |> dplyr::filter(election_year == 2021), by = "ags") |>
  select(ags, ags_name, area, population) |>
  arrange(population) |>
  print(n = Inf) 
# they just really don't exist in the 2021 raw data


ags_21 <- ags21 |> select(-ags_name)

# Get population & area for 2025 (map to 2021 boundaries via crosswalk)
ags25_raw <- read_excel(
  "data/covars_municipality/raw/municipality_sizes/AuszugGV4QAktuell_2024.xlsx",
  sheet = 2
) |>
  slice(9:16018) |>
  select(
    Land = `...3`,
    RB   = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    Gemeindename = `...8`,
    area = `...9`,
    population  = `...10`
  ) |>
  dplyr::filter(!is.na(Gemeinde)) |>
  mutate(
    Land     = pad_zero_conditional(Land, 1),
    Kreis    = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags_25   = paste0(Land, RB, Kreis, Gemeinde),
    population = as.numeric(population) / 1000,
    area = as.numeric(area)
  ) |>
  select(ags_25, area, population)

# Map 2025 data to 2021 boundaries using crosswalk
ags25 <- ags25_raw |>
  rename(area_25 = area, population_25 = population) |>
  left_join(cw_25_to_21, by = "ags_25") |>
  group_by(ags_21) |>
  summarise(
    area = sum(area_25 * area_w_25_21, na.rm = TRUE),
    population = sum(population_25 * pop_w_25_21, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    election_year = 2025,
    area = round(area, digits = 2),
    population = round(population, digits = 1),
    ags_21 = pad_zero_conditional(ags_21, 7)
  ) |>
  select(ags = ags_21, election_year, area, population)


# Create full df ----------------------------------------------------------

glimpse(votes)
table(votes$election_year)
names(df_cw)

# Bind back to dataframe
df_harm <- votes |>
  # Rename ags
  left_join_check_obs(area_pop, by = c("ags_21", "election_year")) |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  # Bind 2021 data (that was unharmonized)
  bind_rows(df_cw |>
    dplyr::filter(election_year == 2021) |>
    # if value is NA, fill in 0
    mutate(
      across(
        cdu:far_left_w_linke,
        ~ ifelse(is.na(.x), 0, .x)
      ),
      unique_multi_mailin = 0
    )) |>
  # Create state variable
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = str_sub(ags, end = -7),
    county = substr(ags, 1, 5)
  ) |>
  relocate(state, .after = election_year)

# Continue transformation
df_harm <- df_harm |>
  left_join_check_obs(ags21, by = c("ags", "election_year")) |>
  left_join_check_obs(ags25, by = c("ags", "election_year")) |>
  mutate(
    # Convert all area/population columns to numeric first
    area.x = as.numeric(area.x),
    area.y = as.numeric(area.y),
    area = as.numeric(area),
    population.x = as.numeric(population.x),
    population.y = as.numeric(population.y),
    population = as.numeric(population),
    # Use area/population from ags21 or ags25 if available and not NA/0, otherwise keep original
    # Priority: ags21 (area.x) > ags25 (area.y) > original (area)
    area = ifelse(!is.na(area.x) & area.x > 0, area.x,
           ifelse(!is.na(area.y) & area.y > 0, area.y, area)),
    population = ifelse(!is.na(population.x) & population.x > 0, population.x,
                 ifelse(!is.na(population.y) & population.y > 0, population.y, population))
  ) |>
  dplyr::select(-c(area.x, area.y, population.x, population.y))

# Calculate vote share & turnout ------------------------------------------

## First: row sum of votes for all parties

names(df_harm)

glimpse(df_harm)

row_sums <- df_harm %>%
  dplyr::select(-c(far_left, far_left_w_linke, far_right, cdu_csu)) %>%
  dplyr::select(cdu:zentrum) %>%
  rowSums(na.rm = TRUE)

summary(row_sums)

## Giant row sum?
## I checked and this seems to be units above the municipality level
## But we should maybe still investigate?

## Merge to data
df_harm <- df_harm %>%
  mutate(
    total_votes = row_sums,
    flag_total_votes_incongruent = ifelse(total_votes != valid_votes, 1, 0),
    total_votes_incogruence = round(total_votes - valid_votes),
    perc_total_votes_incogruence = total_votes_incogruence / valid_votes
  )

### inspect incongruence ----

table(df_harm$total_votes_incogruence, useNA = "ifany")
table(df_harm$flag_total_votes_incongruent, useNA = "ifany")
mean(df_harm$flag_total_votes_incongruent)

df_harm |>
  dplyr::filter(unique_mailin == 1 & flag_total_votes_incongruent == 1) |>
  select(ags, election_year, total_votes, valid_votes, unique_mailin, flag_total_votes_incongruent)
# no place that has unique_mailin == 1 has this problem



### plot incongruence -------------------------------------------------------

# histogram
df_harm %>%
  # filter(flag_total_votes_incongruent == 1) %>%
  ggplot(aes(x = total_votes_incogruence)) +
  geom_histogram(bins = 50) +
  # number of observations above each bar
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    vjust = -0.5,
    # angle = 90,
    size = 3
  ) +
  theme_hanno() +
  labs(
    x = "Difference between own calculation of total votes\nand total votes in data",
    y = "Count"
  ) +
  # increase max of y-axis to make room for text
  scale_y_continuous(limits = c(0, 65000)) +
  # x axis labels for all values
  scale_x_continuous(breaks = seq(-18, 6, by = 1)) +
  # axis labels a bit smaller
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )

ggsave("output/figures/total_votes_incongruence_hist.pdf", width = 7.5, height = 4)

# ts_to_overleaf("code")


# check total_votes vs. other vote variables
inspect <- df_harm |>
  dplyr::select(
    ags, election_year,
    eligible_voters, number_voters, valid_votes,
    total_votes,
    unique_mailin, flag_total_votes_incongruent, total_votes_incogruence, perc_total_votes_incogruence
  )

# inspect_98 <- df_harm |>
#   dplyr::filter(ags == "05111000" & election_year == 1998) |>
#   pivot_longer(
#     cols = c(cdu:zentrum),
#     names_to = "variable",
#     values_to = "value"
#   ) |>
#   dplyr::filter(value != 0) |>
#   dplyr::select(variable, value) |>
#   dplyr::filter(variable != "cdu_csu")

# inspect_90 <- df_harm |>
#   dplyr::filter(ags == "12053000" & election_year == 1990) |>
#   pivot_longer(
#     cols = c(cdu:zentrum),
#     names_to = "variable",
#     values_to = "value"
#   ) |>
#   dplyr::filter(value != 0) |>
#   dplyr::select(variable, value) |>
#   dplyr::filter(variable != "cdu_csu")


# inspect different vote counts
inspect <- df_harm |>
  dplyr::select(
    ags, election_year,
    eligible_voters, number_voters, valid_votes, total_votes
  )

# is total votes anywhere higher than number of voters?
inspect |>
  dplyr::filter(total_votes > number_voters) |>
  mutate(
    diff = total_votes - number_voters
  ) |>
  arrange(desc(diff))
# yes in 138 cases, but the highest difference is 5 votes
## We use the number of voters for calculating turnout

names(df_harm)

# calculate vote share & turnout
df_harm <- df_harm |>
  mutate(
    across(cdu:far_left_w_linke, ~ .x / total_votes),
    turnout = number_voters / eligible_voters_orig,
    flag_naive_turnout_above_1 = ifelse(turnout > 1, 1, 0),
    turnout = ifelse(turnout > 1, number_voters_orig / eligible_voters_orig, turnout),
    turnout_wo_mailin = number_voters_orig / eligible_voters_orig
  ) |>
  # Relocate columns
  relocate(cdu_csu, .after = perc_total_votes_incogruence) |>
  relocate(far_right, .after = cdu_csu) |>
  relocate(far_left, .after = far_right) |>
  relocate(far_left_w_linke, .after = far_left) |>
  relocate(county, .after = state) |>
  relocate(flag_unsuccessful_naive_merge, .after = population) |>
  relocate(flag_naive_turnout_above_1, .after = population) |>
  dplyr::select(-c(ags_name, ags_name_21, emp_cw, employees, year_cw, id)) |>
  # arrange
  arrange(ags, election_year)



# Election date ---------------------------------------------------

df <- df |> mutate(election_date = case_when(
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
  election_year == "2025" ~ lubridate::ymd("2025-02-23"),
  .default = NA
), .after = election_year)

# check whether missing values for election_date
if (df |> filter(is.na(election_date)) |> nrow() > 0) {
  message("Missing values for election_date")
} else {
  message("No missing values for election_date")
}

# Party votes to NA if no votes in year -----------------------------------

# Identify parties that did not receive any votes in a given election year
no_votes_parties <- df_harm %>%
  group_by(election_year) %>%
  summarise(across(cdu:zentrum, ~ all(. == 0), .names = "all_zero_{col}")) %>%
  pivot_longer(cols = starts_with("all_zero_"), names_to = "party", values_to = "all_zero") %>%
  mutate(party = sub("all_zero_", "", party)) %>%
  dplyr::select(election_year, party, all_zero)

# Recode 0 vote shares to NA for parties that did not receive any votes in an election year
df_harm <- df_harm %>%
  pivot_longer(cols = cdu:zentrum, names_to = "party", values_to = "vote_share") %>%
  left_join(no_votes_parties, by = c("election_year", "party")) %>%
  mutate(vote_share = if_else(all_zero == TRUE & vote_share == 0, NA_real_, vote_share)) %>%
  dplyr::select(-all_zero) %>%
  pivot_wider(names_from = "party", values_from = "vote_share")

glimpse(df_harm)

# Relocate
df_harm <- df_harm |>
  dplyr::select(
    ags:turnout_wo_mailin,
    cdu:zentrum,
    cdu_csu:far_left_w_linke,
    flag_naive_turnout_above_1:perc_total_votes_incogruence,
    area_cw:population
  )


# Save --------------------------------------------------------------------

## Save this now:

# Write .csv file
fwrite(df_harm, file = "data/federal_elections/municipality_level/final/federal_muni_harm_21.csv")
write_rds(df_harm, "data/federal_elections/municipality_level/final/federal_muni_harm_21.rds")



# Inspect -----------------------------------------------------------------

df_harm <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  arrange(ags, election_year)

table(df_harm$election_year)


## Inspect turnout ---------------------------------------------------------

insp <- df_harm |>
  # dplyr::filter(election_year == 2021) |>
  dplyr::filter(state == "07") |>
  arrange(ags, election_year) |>
  dplyr::select(ags, election_year, eligible_voters_orig:number_voters, turnout, turnout_wo_mailin, unique_mailin:unique_multi_mailin, )


## other -------------------------------------------------------------------


# count number of municipalities
df_harm |>
  distinct(ags) |>
  nrow()

# count number of election years
df_harm |>
  distinct(election_year) |>
  nrow()

# count number of ags in each election year
df_harm |>
  group_by(election_year) |>
  summarise(n = n_distinct(ags))

glimpse(df_harm)

# inspect 2021
inspect <- df_harm |>
  dplyr::filter(election_year == 2021)

# Berlin
inspect <- df_harm |>
  dplyr::filter(state == "11")


names(df_harm)

# Check for missing values in election_year
df_harm |>
  dplyr::filter(is.na(election_year))

# check turnout > 1
df_harm |>
  dplyr::filter(turnout > 1)


# Number of munis within shared mail in districts in 2021 ------------------

glimpse(df_harm)

# number of ags with unique_mailin == 0 in each election
n_joint <- df_harm |>
  group_by(election_year) |>
  summarise(
    n = n_distinct(ags),
    n_ags_joint_mailin = sum(unique_mailin == 0),
    ags_share = n_ags_joint_mailin / n * 100,
    n_voters = sum(eligible_voters),
    n_voters_joint_mailin = sum(eligible_voters[unique_mailin == 0]),
    n_voters_joint_mailin_share = n_voters_joint_mailin / n_voters * 100
  ) |>
  ungroup()

# View(n_joint)


mailin_df <- read_rds("data/federal_elections/municipality_level/additional/mailin_df.rds")


# merge
n_joint <- n_joint |>
  left_join_check_obs(mailin_df, by = "election_year")


# plot number of districts, number of munis per district und share of voters in these districts over time
n_joint |>
  pivot_longer(cols = c(mailin_join, n_ags_joint_mailin, n_voters_joint_mailin_share), names_to = "variable", values_to = "value") |>
  mutate(
    variable = case_when(
      variable == "mailin_join" ~ "Number of joint mail-in voting districts (count)",
      variable == "n_ags_joint_mailin" ~ "Number of municipalities in joint mail-in voting district (count)",
      variable == "n_voters_joint_mailin_share" ~ "Share of voters in joint mail-in voting district (%)"
    ),
    # round the share to 0 decimal places
    value = round(value, 0),
    # election year as factor
    election_year = factor(election_year)
  ) |>
  ggplot(aes(x = election_year, y = value)) +
  geom_col() +
  # value of mailin_join as text above each year
  geom_text(aes(label = value), vjust = -0.5) +
  theme_hanno() +
  labs(
    x = "Election",
    y = "" # remove y axis label since it's in the titles now
  ) +
  # increase max of y-axis to make room for text
  scale_y_continuous(limits = function(x) c(0, max(x) * 1.1)) +
  facet_wrap(~variable, scales = "free_y", nrow = 3) +
  scale_x_discrete(breaks = c(1990, 1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025))

ggsave("output/figures/n_mailin.pdf", width = 7, height = 7)

# move_plots_to_overleaf("code")




### END
