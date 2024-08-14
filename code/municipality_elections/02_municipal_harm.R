### Harmonize municipal electoral results to 2021 borders
# Vincent Heddesheimer
# First: Aug 08, 2024
# Last: Aug 14, 2024

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
# no: none

# inspect <- df |>
#   mutate(id = paste0(ags,"_",election_year)) |>
#   filter(id %in% dupl$id) |>
#   arrange(ags, election_year)
# 
# fwrite(inspect, "data/municipal_elections/processed_data/duplicates.csv")


# Merge w/ cw -------------------------------------------------------------

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
# If we do not follow the steps below, there are >1,485 cases.
# We found these by the below code.

fwrite(not_merged_naive, "data/municipal_elections/processed_data/unsuccessful_mergers.csv")

glimpse(df_cw)8


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


# Create plot -------------------------------------------------------------

# Load municipality level data
muni <- read_rds("data/municipal_covars/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  filter(year >= 2006 & year < 2020)

# Merge
df_final <- muni |>
  left_join_check_obs(df_harm, by = c("ags", "year"))

# Create variable indicating whether there was election in given year
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

table(df_harm$year)

# create plot_df
plot_df <- df_final |>
  group_by(state_name, year) |>
  summarise(election_bin = max(election_bin, na.rm = TRUE)) |>
  ungroup()

plot_df |>
  ggplot(aes(x = as.factor(year), 
             y = factor(state_name, 
                        levels = rev(levels(factor(state_name)))), 
             fill = as.factor(election_bin))
  ) +
  geom_tile(color = "white") + # Add borders to the squares
  scale_fill_manual(values = c("1" = "darkgrey", "0" = "white"), name = "Election") +
  labs(x = "Year", y = "State") +
  theme_hanno() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave("output/figures/state_elections.pdf", width = 7, height = 4)

move_plots_to_overleaf("code")


### END
