### Harmonize BTW electoral results at muni level 1980-2021
# Vincent Heddesheimer, Hanno Hilbig
# May, 23, 2024

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)


# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/ags_crosswalks.csv") |>
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

# Merge with unharmonized election data -----------------------------------

df <- read_rds("output/federal_muni_unharm.rds") |>
  # remove population & area that were used for weighting multi mail-in districts
  select(-c(pop, area)) |>
  # filter years before 1990: no crosswalks available
  filter(election_year >= 1990)

glimpse(df)
glimpse(cw)

# bind with crosswalks
df <- df |>
    left_join_check_obs(cw, by = c("ags", "election_year" = "year")) |>
    arrange(ags, election_year)
# number of obs increases: but this is wanted, as we want to harmonize the data

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

glimpse(votes)

# Bind back to dataframe
df_harm <- votes |>
  # Rename ags
  left_join_check_obs(area_pop, by = c("ags_21", "election_year")) |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  # Bind 2021 data (that was unharmonized)
  bind_rows(df |>
              filter(election_year == 2021) |>
              select(-c(
                state, state_name, election_year,
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
    total_votes = row_sums,
    flag_total_votes_incongruent = ifelse(total_votes != valid_votes, 1, 0),
    total_votes_incogruence = total_votes - valid_votes,
    perc_total_votes_incogruence = total_votes_incogruence / valid_votes
  )

### inspect incongruence ----

table(df_harm$total_votes_incogruence, useNA = "ifany")
table(df_harm$flag_total_votes_incongruent, useNA = "ifany")
mean(df_harm$flag_total_votes_incongruent)


# points
df_harm %>% 
  filter(flag_total_votes_incongruent == 1) %>% 
  ggplot(aes(x = log(total_votes), y = perc_total_votes_incogruence)) +
  geom_point()

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
  scale_x_continuous(breaks = seq(-17, 6, by = 1)) +
  # axis labels a bit smaller
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

ggsave("figures/total_votes_incongruence_hist.pdf", width = 7, height = 4)

move_plots_to_overleaf("code")

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



# calculate vote share & turnout
df_harm <- df_harm |>
  mutate(
    across(cdu:left_wing_wLinke, ~ .x / total_votes),
    # turnout = (valid + invalid) / eligible_voters
    turnout = number_voters / eligible_voters
  ) |>
  # Relocate columns
  relocate(turnout, .before = cdu) |>
  relocate(right_wing, .after = bsa) |>
  relocate(left_wing, .after = right_wing) |>
  relocate(left_wing_wLinke, .after = left_wing) |>
  relocate(county, .after = state) |>
  select(-c(ags_name, ags_name_21, emp_cw, employees))

# AfD to NA for years prior to 2013

df_harm <- df_harm %>%
    mutate(
        afd = ifelse(election_year < 2013, NA, afd)
    )

## Save this now:

# Write .csv file
fwrite(df_harm, file = "output/federal_muni_harm.csv")
write_rds(df_harm, "output/federal_muni_harm.rds")



# Inspect -----------------------------------------------------------------

df_harm <- read_rds("output/federal_muni_harm.rds")

# Berlin
inspect <- df_harm |>
  filter(state=="11")


names(df_harm)


### END