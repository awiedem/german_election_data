### Harmonize state election results to 2021 borders
# Vincent Heddesheimer
# First: March 05, 2024
# Last: Aug 13, 2024

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# load
df <- read_rds("data/state_elections/final/state_unharm.rds")
# df <- read_rds("01_Data/13_LTW/Elections_Clean.RDS")

glimpse(df)

table(df$election_year)



# Create crosswalks -------------------------------------------------------

# Load crosswalks
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  ) |>
  rename(election_year = year)


# Unsuccessful mergers ----------------------------------------------------

# Merge crosswalks
df_cw_naive <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw_naive)

# is there any ags that did not get merged to ags_21?
not_merged_naive <- df_cw_naive %>%
  filter(is.na(ags_21)) %>%
  select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))
not_merged_naive
# If we do not follow the steps below, there are 86 cases.
# We found these by the below code.

# filter out all observations for ags that were not merged
obs_not_merged_ags <- df_cw_naive %>%
  filter(ags %in% not_merged_naive$ags) %>%
  select(ags, ags_name, election_year)

# how often do these ags appear in the data?
table(obs_not_merged_ags$ags)
# some one time, some two, some three

# fill out the missing values in ags_name:employees with the values for which values are existing
cw_not_merged <- cw |>
  filter(ags %in% not_merged_naive$ags)


# Dealing with unsuccessful mergers ---------------------------------------

# When merging this file to state elections data, this results in some observations not being merged.
# This is because the state election data contains some observations with wrong ags identifiers.
# That is, some ags identifiers are not in the crosswalk files for that respective election_year.
# Therefore, we have to add these observations to the crosswalks.

# 1. One observation that is not included in crosswalk files at all: Wasdow
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Namens-Grenz-Aenderung/2011.xlsx?__blob=publicationFile
wasdow <- data.frame(
  ags = "13072115", ags_name = "Wasdow", election_year = 2011, 
  area_cw = 1, pop_cw = 1,
  area = 26.20, population = 3.9,
  ags_21 = "13072010", ags_name_21 = "Behren-Lübchin",
  emp_cw = 1, employees = 0.2
  )

names(cw)
names(wasdow)

# bind to cw
cw <- cw |> 
  bind_rows(wasdow) |>
  arrange(ags, election_year)

glimpse(cw)

# duplicates
df %>% 
  group_by(ags, election_year) %>% 
  filter(n() > 1) %>% 
  arrange(ags, election_year)
# none

# For the remaining observations we do two things:
# 2. Some ags retain the old ags one election_year after they already
# have been merged. We therefore create a new crosswalk election_year.

ags_election_year_cw <- 
  c("03156001_2013",
    "03156004_2013",
    "03156005_2013",
    "03156007_2013",
    "03156014_2013",
    "03158015_2013",
    "03158020_2013",
    "03158026_2013",
    "03158034_2013",
    "16063013_2019",
    "16063019_2019",
    "16063055_2019",
    "16064008_2019",
    "16064029_2019",
    "16064035_2019",
    "16064048_2019",
    "16064052_2019",
    "16064057_2019",
    "16065072_2019",
    "16066071_2019",
    "16067025_2019",
    "16067039_2019",
    "16067054_2019",
    "16067083_2019",
    "16068026_2019",
    "16070003_2019",
    "16070044_2019",
    "16071006_2019",
    "16071012_2019",
    "16071034_2019",
    "16071036_2019",
    "16071057_2019",
    "16071065_2019",
    "16071067_2019",
    "16071073_2019",
    "16071088_2019",
    "16071099_2019",
    "16072001_2019",
    "16075009_2019",
    "16075018_2019",
    "16075049_2019",
    "16075061_2019",
    "16076052_2019"
    )

df <- df |>
  mutate(
    id = paste0(ags, "_", election_year),
    election_year_cw = case_when(
      id %in% ags_election_year_cw ~ election_year - 1,
      TRUE ~ election_year
    )
  )


# 3. For the remaining ags we change the ags provided in the 
# election data to the correct ags as indicated by the cw for that election_year.
df <- df |>
  mutate(
    ags = case_when(
      ags == "07140502" & election_year == 2011 ~ "07135050", # Lahr
      ags == "07140503" & election_year == 2011 ~ "07135063", # Mörsdorf
      ags == "07140504" & election_year == 2011 ~ "07135094", # Zilshausen
      ags == "07232502" & election_year == 2011 ~ "07232021", # Brimingen
      ags == "07232502" & election_year == 2016 ~ "07232021", # Brimingen (2016!)
      ags == "07235207" & election_year == 2011 ~ "07231207", # Trittenheim
      TRUE ~ ags
    )
  )

# 4. The remaining ags that cannot be merged are districts in Berlin.
# We change their ags to the overall Berlin ags.
df <- df |>
  mutate(
    ags = case_when(
      # when ags starts with "11"
      str_sub(ags, 1, 2) == "11" ~ "11000000",
      TRUE ~ ags
    )
  )

# Merge crosswalks with election data -------------------------------------

# Merge crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year_cw" = "election_year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)

# is there any ags that did not get merged to ags_21?
not_merged <- df_cw %>%
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

glimpse(df_cw)

# Harmonize ---------------------------------------------------------------

table(df_cw$election_year)

# Harmonize with weighted_population average
df_harm <- df_cw |>
  mutate(weights = pop_cw * population) |>
  group_by(ags_21, election_year) |>
  summarise_at(
    vars(turnout:cdu_csu), ~ weighted.mean(.x, weights, na.rm = TRUE)
    ) |>
  rename(ags = ags_21) |>
  # state id (first two digits of ags)
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
    ) |>
  relocate(state, .after = election_year) |>
  relocate(state_name, .after = state) |>
  ungroup() |>
  arrange(ags, election_year)

# Add column to this indicating whether there was a flag
df_harm <- df_harm |>
  mutate(id = paste0(ags, "_", election_year)) |>
  left_join_check_obs(
    df_cw %>% 
      mutate(id = paste0(ags_21, "_", election_year)) %>%
      select(id, flag_unsuccessful_naive_merge) %>%
      distinct() %>%
      group_by(id) %>%
      summarise(flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE)
      ) %>%
      ungroup(), 
    by = "id") %>%
  select(-id)

glimpse(df_harm)
table(df_harm$state, useNA = "ifany")


# summarize eligible & valid voters
votes <- df_cw |>
  group_by(ags_21, election_year) |>
  summarise(
    across(
      eligible_voters:valid_votes,
      ~ sum(.x * pop_cw, na.rm = TRUE)
    )
  ) |>
  # Round
  mutate(across(
    eligible_voters:valid_votes,
    ~ round(.x, digits = 0)
  )) |>
  ungroup()

# left join
df_harm <- df_harm |>
  left_join_check_obs(votes, by = c("ags" = "ags_21", "election_year")) |>
  relocate(eligible_voters, .after = state_name) |>
  relocate(valid_votes, .after = eligible_voters)


# check where state is NA
df_harm %>%
  filter(is.na(ags)) %>%
  select(ags, election_year) %>%
  distinct()
# none



# Flag when total vote share > 1 ------------------------------------------

df_harm <- df_harm %>%
  mutate(total_vote_share = rowSums(select(., cdu:other), na.rm = TRUE),
         total_vote_share = round(total_vote_share, 8),
         flag_total_votes_incongruent = ifelse(total_vote_share > 1, 1, 0)
         )

table(df_harm$flag_total_votes_incongruent, useNA = "ifany")
# 29 observations

# is there any observations in which both CDU and CSU are not NA?
df_harm %>%
  filter(!is.na(cdu) & !is.na(csu)) %>%
  select(ags, election_year) %>%
  distinct()
# no


glimpse(df_harm)

# write
fwrite(df_harm, "data/state_elections/final/state_harm.csv")
write_rds(df_harm, "data/state_elections/final/state_harm.rds")



# Create plot -------------------------------------------------------------

# read
df_harm <- read_rds("data/state_elections/final/state_harm.rds")

# for which election_years do we have non-NA afd values?
df_harm %>%
  filter(!is.na(afd)) %>%
  select(election_year) %>%
  distinct()


# count number of municipalities
df_harm |>
  distinct(ags) |>
  nrow()

# count number of election years
df_harm |>
  distinct(election_year) |>
  nrow()

# Load municipality level data
muni <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  filter(year >= 2006 & year < 2020) |>
  rename(election_year = year)

# Merge
df_final <- muni |>
  left_join_check_obs(df_harm, by = c("ags", "election_year"))

# Create variable indicating whether there was election in given election_year
df_final <- df_final |>
  mutate(
    election_bin = ifelse(!is.na(turnout), 1, 0)
  )

glimpse(df_final)

# state variable
df_final <- df_final |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  )

table(df_harm$election_year)

plot_df <- df_final |>
  group_by(state_name, election_year) |>
  summarise(election_bin = max(election_bin, na.rm = TRUE)) |>
  ungroup()

# Add a special row for Schleswig-Holstein in 2017
plot_df <- plot_df |>
  add_row(state_name = "Schleswig-Holstein", election_year = 2017, election_bin = 2)

# Update the factor levels to include the special value
plot_df$election_bin <- factor(plot_df$election_bin, levels = c("0", "1", "2"))

plot_df |>
  ggplot(aes(x = as.numeric(election_year), 
             y = factor(state_name, 
                        levels = rev(levels(factor(state_name)))), 
             fill = election_bin)
  ) +
  geom_tile(color = "white") + # Add borders to the squares
  scale_fill_manual(
    values = c("1" = "grey20", "0" = "white", "2" = "darkgrey"), 
    name = "Election",
    labels = c("1" = "Election Held", "2" = "Data Unavailable")
  ) +
  labs(x = "Year", y = "State") +
  theme_hanno() +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2021, 5),
    expand = c(0.07, 0) # Add some padding to the right
    )  

ggsave("output/figures/state_elections.pdf", width = 7, height = 4)

move_plots_to_overleaf("code")

## END