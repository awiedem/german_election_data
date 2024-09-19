### Harmonize BTW electoral results at muni level 1980-2021
# Vincent Heddesheimer, Hanno Hilbig
# May, 23, 2024

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)


# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  mutate(ags = pad_zero_conditional(ags, 7))

# how many ags_21 for each year?
cw |>
  distinct(ags_21, year) |>
  count(year)

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
  # remove population & area that were used for weighting multi mail-in districts
  dplyr::select(-c(pop, area)) |>
  # filter years before 1990: no crosswalks available
  filter(election_year >= 1990) |>
  arrange(ags, election_year)

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
  filter(election_year < 2021) %>%
  filter(is.na(ags_21)) %>%
  select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))
not_merged_naive
# If we do not follow the steps below, there are >1,600 cases.
# We found these by the below code.

# filter out all observations for ags that were not merged
obs_not_merged_ags <- df_naive_merge %>%
  filter(ags %in% not_merged_naive$ags) %>%
  select(ags, ags_name, election_year)

# how often do these ags appear in the data?
table(obs_not_merged_ags$ags)
# some one time, some two, some three

# get all ags that appear one time in obs_not_merged_ags
one_time_ags <- obs_not_merged_ags %>%
  group_by(ags) %>%
  filter(n() == 1) %>%
  select(ags, election_year) %>%
  mutate(id = paste0(ags, "_", election_year)) %>%
  distinct()
# the great majority of one-timers are from 1990
# 1. attempt: use 1991 as crosswalk year for these ags
# This worked very well! Reduced from 1656 cases to 266 cases!

# fill out the missing values in ags_name:employees with the values for which values are existing
cw_not_merged <- cw |>
  filter(ags %in% not_merged_naive$ags)


# Dealing with unsuccessful mergers ---------------------------------------

# define cases where we want to use year - 1
ags_year_cw <- not_merged_naive %>%
  filter(
    grepl("^031", id) |           # id starts with 031
      grepl("^05", id) |    
      id == "07143217_1994" |       # id equals 07143217_1994
      grepl("^12", id) |          # id starts with 120
      grepl("^14", id) |            # id starts with 14
      grepl("^15", id) |            # id starts with 15
      grepl("^16", id)              # id starts with 16
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
  filter(election_year < 2021) %>%
  filter(is.na(ags_21)) %>%
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

df_cw$election_year

names(df_cw) 

## Votes: weighted sum -----------------------------------------------------
votes <- df_cw |>
    filter(election_year < 2021) |>
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

## Population & area: weighted sum -----------------------------------------
area_pop <- df_cw |>
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
ags21 <- read_excel(path = "data/crosswalks/raw/31122021_Auszug_GV.xlsx", sheet = 2) |>
    dplyr::select(
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
        election_year = 2021,
        population = as.numeric(population) / 100
    ) |>
    slice(6:16065) |>
    filter(!is.na(Gemeinde)) |>
  dplyr::select(ags, election_year, area, population)
  

# Create full df ----------------------------------------------------------

glimpse(votes)
names(df_cw)

# Bind back to dataframe
df_harm <- votes |>
  # Rename ags
  left_join_check_obs(area_pop, by = c("ags_21", "election_year")) |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  # Bind 2021 data (that was unharmonized)
  bind_rows(df_cw |>
              filter(election_year == 2021) |>
              # if value is NA, fill in 0
              mutate(across(
                cdu:far_left_w_linke,
                ~ ifelse(is.na(.x), 0, .x)
              ),
              unique_multi_mailin = 0)
            ) |>
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

names(df_harm)

row_sums <- df_harm %>%
    select(-c(far_left, far_left_w_linke, far_right, cdu_csu)) %>%
    select(cdu:zentrum) %>%
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
    total_votes_incogruence = total_votes - valid_votes,
    perc_total_votes_incogruence = total_votes_incogruence / valid_votes
  )

### inspect incongruence ----

table(df_harm$total_votes_incogruence, useNA = "ifany")
table(df_harm$flag_total_votes_incongruent, useNA = "ifany")
mean(df_harm$flag_total_votes_incongruent)

df_harm |>
  filter(unique_mailin == 1 & flag_total_votes_incongruent == 1)
# no place that has unique_mailin == 1 has this problem

# check total_votes vs. other vote variables
inspect <- df_harm |>
  select(ags, election_year, 
         eligible_voters, number_voters, valid_votes, 
         total_votes, 
         unique_mailin, flag_total_votes_incongruent, total_votes_incogruence, perc_total_votes_incogruence)

# inspect_98 <- df_harm |>
#   filter(ags == "05111000" & election_year == 1998) |>
#   pivot_longer(
#     cols = c(cdu:zentrum),
#     names_to = "variable",
#     values_to = "value"
#   ) |>
#   filter(value != 0) |>
#   select(variable, value) |>
#   filter(variable != "cdu_csu")

# inspect_90 <- df_harm |>
#   filter(ags == "12053000" & election_year == 1990) |>
#   pivot_longer(
#     cols = c(cdu:zentrum),
#     names_to = "variable",
#     values_to = "value"
#   ) |>
#   filter(value != 0) |>
#   select(variable, value) |>
#   filter(variable != "cdu_csu")
  

# inspect different vote counts
inspect <- df_harm |>
  select(ags, election_year,
         eligible_voters, number_voters, valid_votes, total_votes)

# is total votes anywhere higher than number of voters?
inspect |>
  filter(total_votes > number_voters) |>
  mutate(
    diff = total_votes - number_voters
  ) |>
  arrange(desc(diff))
# yes in 151 cases, but the highest difference is 3 votes
## We use the number of voters for calculating turnout

names(df_harm)

# calculate vote share & turnout
df_harm <- df_harm |>
  mutate(
    across(cdu:far_left_w_linke, ~ .x / total_votes),
    turnout = number_voters / eligible_voters,
    flag_naive_turnout_above_1 = ifelse(turnout > 1, 1, 0),
    turnout = ifelse(turnout > 1, number_voters_orig / eligible_voters_orig, turnout),
    turnout_wo_mailin = number_voters_orig / eligible_voters_orig
    ) |>
  # Relocate columns
  relocate(cdu_csu, .after = perc_total_votes_incogruence)  |>
  relocate(far_right, .after = cdu_csu) |>
  relocate(far_left, .after = far_right) |>
  relocate(far_left_w_linke, .after = far_left) |>
  relocate(county, .after = state) |>
  relocate(flag_unsuccessful_naive_merge, .after = population) |>
  relocate(flag_naive_turnout_above_1, .after = population) |>
  select(-c(ags_name, ags_name_21, emp_cw, employees, year_cw, id)) |>
  # arrange
  arrange(ags, election_year)



# Party votes to NA if no votes in year -----------------------------------

# Identify parties that did not receive any votes in a given election year
no_votes_parties <- df_harm %>%
  group_by(election_year) %>%
  summarise(across(cdu:zentrum, ~ all(. == 0), .names = "all_zero_{col}")) %>%
  pivot_longer(cols = starts_with("all_zero_"), names_to = "party", values_to = "all_zero") %>%
  mutate(party = sub("all_zero_", "", party)) %>%
  select(election_year, party, all_zero)

# Recode 0 vote shares to NA for parties that did not receive any votes in an election year
df_harm <- df_harm %>%
  pivot_longer(cols = cdu:zentrum, names_to = "party", values_to = "vote_share") %>%
  left_join(no_votes_parties, by = c("election_year", "party")) %>%
  mutate(vote_share = if_else(all_zero == TRUE & vote_share == 0, NA_real_, vote_share)) %>%
  select(-all_zero) %>%
  pivot_wider(names_from = "party", values_from = "vote_share")

glimpse(df_harm)

# Relocate
df_harm <- df_harm |>
  select(ags:turnout_wo_mailin, 
         cdu:zentrum, 
         cdu_csu:far_left_w_linke,
         flag_naive_turnout_above_1:perc_total_votes_incogruence, 
         area_cw:population)


# Save --------------------------------------------------------------------

## Save this now:

# Write .csv file
fwrite(df_harm, file = "data/federal_elections/municipality_level/final/federal_muni_harm.csv")
write_rds(df_harm, "data/federal_elections/municipality_level/final/federal_muni_harm.rds")



# Inspect -----------------------------------------------------------------

df_harm <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm.rds") |>
  arrange(ags, election_year)

## Inspect turnout ---------------------------------------------------------

insp <- df_harm |> 
  # filter(election_year == 2021) |>
  filter(state == "07") |>
  arrange(ags, election_year) |>
  dplyr::select(ags, election_year, eligible_voters_orig:number_voters, turnout,turnout_wo_mailin, unique_mailin:unique_multi_mailin, )



glimpse(df_harm)

# inspect 2021
inspect <- df_harm |>
  filter(election_year == 2021)

# Berlin
inspect <- df_harm |>
  filter(state=="11")


names(df_harm)

# Check for missing values in election_year
df_harm |>
  filter(is.na(election_year))

# check turnout > 1
df_harm |>
  filter(turnout > 1)

### END