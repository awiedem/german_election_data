# Code to build and clean municipality election data

rm(list = ls())

library(tidyverse)
library(haschaR)

# Load data
df <- read_rds("01_Data/11_Muni_Council_Elections/MuniElections_RAW.rds")

# Load crosswalks

cw <- fread("01_Data/02_umsteigeschlüssel/ags_crosswalks.csv") |>
  mutate(weights = pop_cw * population)


glimpse(cw)

## The weight we use is pop_cw*population
## The format is: each row is the muni in year t
## The weights are rel. to 2021

# Mutate df
df <- df |>
  mutate(
    ags = as.numeric(AGS_8dig),
    election_year = as.numeric(election_year)
  )

unique(df$ags) %>%
  nchar() %>%
  table()

# Why are the some 9-digit AGS in the raw data?

# Merge crosswalks
df_cw <- df |>
  left_join(cw, by = c("ags", "election_year" = "year"))

## Note that in the crosswalk data, the AGS are at most 8 digits
## Is this an issue since some AGS in the raw data are 9 digits?

glimpse(df_cw)
unique(cw$ags) %>% nchar() %>% table()

# Harmonize with weighted_population average
df_harm <- df_cw |>
  group_by(ags_21, election_year) |>
  mutate(Turnout = as.numeric(Turnout)) |>
  summarise_at(
    vars(Turnout:prop_Wählergruppen),
    ~ weighted.mean(.x,
      weights,
      na.rm = TRUE
    )
  ) |>
  rename(
    ags = ags_21, year = election_year,
    turnout = Turnout
  ) %>%
  ungroup()

# Define election vars

elec_vars <- c(
  "prop_CDU_CSU", "prop_SPD", "prop_DIELINKE", "prop_GRÜNE",
  "prop_AfD", "prop_FDP", "prop_FREIEWÄHLER",
  "prop_Wählergruppen"
)

# add all elec_vars up for each row
inspect <- df_harm %>%
  mutate(
    prop_all = rowSums(select(., all_of(elec_vars)),
      na.rm = TRUE
    )
  )

## Share of municipalities with prop_all > 1
## For the harmonized data

mean(inspect$prop_all > 1) * 100

## 2%

## Same for the raw results 
## Ie share of municipalities with prop_all > 1 in the raw data

df <- df %>%
  mutate(
    prop_all = rowSums(select(., all_of(elec_vars)), na.rm = TRUE)
  )

mean(df$prop_all > 1) * 100

## 0.5%

## Ags of places with prop_all > 1 in the raw data

df_raw_which_greater_1_total <- df %>%
  filter(prop_all > 1) %>%
  select(ags, election_year)

## Check if Ahrensfelde 2008 (12060005) is in the list

df_raw_which_greater_1_total %>%
  filter(ags == 12060005)

## Yes, it is

# write
fwrite(df_harm, "01_Data/11_Muni_Council_Elections/MuniElections_harmonized.csv")

# Merge with municipality level data --------------------------------------

# Load municipality level data
muni <- fread("01_Data/ags_covars_subsidy.csv")

## Check if ags / year combinations are unique in df_harm

df_harm %>%
  group_by(ags, year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  dplyr::filter(n > 1) %>%
  nrow() %>% 
  cat("There are", ., "duplicates in df_harm")

## Looks good, now same w/ muni

muni %>%
  group_by(ags, year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  dplyr::filter(n > 1) %>%
  nrow() %>% 
  cat("There are", ., "duplicates in muni")

  ## Also good

# Merge
df_final <- muni |>
  left_join(df_harm, by = c("ags", "year"))

# Create variable indicating whether there was election in given year
df_final <- df_final |>
  mutate(
    election_bin = ifelse(!is.na(turnout), 1, 0)
  )

## Check min and max for the prop vars again since we now save the final data

glimpse(df_final)

df_final %>%
  select(starts_with("prop")) %>%
  summarise(
    across(everything(), ~ c(min(.x, na.rm = TRUE), max(.x, na.rm = TRUE))
  )) %>%
  ungroup()

## check proportion

# elec vars
elec_vars <- c(
  "prop_CDU_CSU", "prop_SPD", "prop_DIELINKE", "prop_GRÜNE",
  "prop_AfD", "prop_FDP", "prop_FREIEWÄHLER",
  "prop_Wählergruppen"
)

inspect <- df_final |>
  dplyr::select(
    ags, year,
    election_bin,
    county, state,
    treat_no_well_perform_2014,
    all_of(elec_vars)
  ) %>%
  filter(election_bin == 1) %>%
  filter(!is.na(treat_no_well_perform_2014))

# add all elec_vars up for each row
inspect <- inspect %>%
  mutate(
    prop_all = rowSums(select(., all_of(elec_vars)), na.rm = TRUE)
  )

quantile(inspect$prop_all, probs = seq(0.95, 1, 0.01))

## Find Gemeinde for which this is the case
## Ahrensfelde as discussed

ags_check <- inspect %>%
  filter(prop_all > 1) %>%
  select(ags, year) %>%
  slice(1)

## Now, we subset CW to only Ahrensfelde in 2008

check_cw <- cw %>%
  filter(ags_21 == ags_check$ags & year == ags_check$year)

## We now merge the raw results (df) to CW, again only for Ahrensfelde in 2008
## Then check the sum of the prop vars

check_df <- df |>
  inner_join(check_cw, by = c("ags", "election_year" = "year")) %>%
  mutate(prop_sum = rowSums(select(., all_of(elec_vars)),
    na.rm = TRUE
  ))

check_df$prop_sum

# Harmonize with weighted_population average
df_harm <- df_cw |>
  group_by(ags_21, election_year) |>
  mutate(Turnout = as.numeric(Turnout)) |>
  summarise_at(
    vars(Turnout:prop_Wählergruppen),
    ~ weighted.mean(.x,
      weights,
      na.rm = TRUE
    )
  ) |>
  rename(
    ags = ags_21, year = election_year,
    turnout = Turnout
  ) %>%
  ungroup()

## Merge 

View(check_df)


# Write
fwrite(df_final, "01_Data/11_Muni_Council_Elections/MuniElections_final.csv")


# Inspect -----------------------------------------------------------------

# df_final <- fread("01_Data/11_Muni_Council_Elections/MuniElections_final.csv")

# years
table(df_final$year)

# reduce to analyzed munis
df_relevant <- df_final |>
  filter(!is.na(treat_no_well_perform_2014) & year > 2005)

# get n of municipalities without any election data (turnout always NA)
filtered_data <- df_relevant %>%
  group_by(ags) %>%
  filter(all(is.na(turnout))) %>%
  ungroup()

inspect <- df_relevant |>
  select(ags, year, turnout, prop_CDU_CSU)

table(!is.na(df_relevant$turnout))


# when are the elections in each state?
df_final %>%
  select(year, election_bin, states_east_west, state_name) %>%
  filter(states_east_west == "East") %>%
  ggplot(aes(x = as.character(year), y = state_name, fill = as.character(election_bin))) +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = c("1" = "black", "0" = "white")) +
  theme_hanno() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


ggsave("03_Plots/Descriptive/muni_elections.pdf", width = 7, height = 4)

ggsave("~/Dropbox (Princeton)/Apps/Overleaf/GRW/figures/muni_elections.pdf", width = 7, height = 4)


# check how often AfD is observed for each state
afdcheck <- df_final %>%
  select(year, election_bin, turnout, prop_AfD, states_east_west, state_name) %>%
  filter(states_east_west == "East") %>%
  filter(year %in% c(2014,2019)) %>%
  mutate(
    prop_AfD = ifelse(prop_AfD == 0, NA, prop_AfD),
    afd_bin = ifelse(!is.na(prop_AfD), 1, 0)
    ) %>%
  group_by(state_name, year) %>%
  summarise(
    AfD = sum(afd_bin, na.rm = TRUE),
    `Obs. Munis` = sum(election_bin, na.rm = TRUE),
    `Total Munis` = n(),
    Share = AfD/`Total Munis` * 100
    )

# save as table
afdcheck |>
  rename("Election" = year, "State" = state_name) |>
  kableExtra::kable(
    booktabs = TRUE,
    escape = FALSE,
    format = "latex",
    linesep = "",
    align = "llllll",
    digits = 2
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::save_kable(file = "04_Tables/afd_munis.tex", keep_tex = T)

# check how often AfD is observed for each year
afdcheck <- df_final %>%
  select(year, election_bin, prop_AfD, states_east_west, state_name) %>%
  filter(states_east_west == "East") %>%
  filter(year %in% c(2014,2019)) %>%
  mutate(
    prop_AfD = ifelse(prop_AfD == 0, NA, prop_AfD),
    afd_bin = ifelse(!is.na(prop_AfD), 1, 0)
  ) %>%
  group_by(year) %>%
  summarise(
    AfD = sum(afd_bin, na.rm = TRUE),
    Munis = n(),
    Share = AfD/Munis * 100
  ) 
  



# Inspect shares ----------------------------------------------------------

# elec vars
elec_vars <- c(
  "prop_CDU_CSU", "prop_SPD", "prop_DIELINKE", "prop_GRÜNE",
  "prop_AfD", "prop_FDP", "prop_FREIEWÄHLER",
  "prop_Wählergruppen"
)

df_muni <- "01_data/11_Muni_Council_Elections/MuniElections_final.csv" %>%
  fread() %>%
  dplyr::select(
    ags, year,
    election_bin,
    county, state,
    treat_no_well_perform_2014,
    all_of(elec_vars)
  ) %>%
  filter(election_bin == 1) %>%
  filter(!is.na(treat_no_well_perform_2014))

# ## Multiply by 100

df_muni <- df_muni %>%
  mutate(across(all_of(elec_vars), ~ . * 100))



# add all elec_vars up for each row
df_muni <- df_muni %>%
  mutate(
    prop_all = rowSums(select(., all_of(elec_vars)), na.rm = TRUE)
  )


## inspect raw data
# Load data
raw <- read_rds("01_Data/11_Muni_Council_Elections/MuniElections_RAW.rds")

raw <-  raw %>%
  mutate(
    prop_all = rowSums(select(., all_of(elec_vars)), na.rm = TRUE)
  ) |>
  select(AGS_8dig, Gebietsname, election_year, all_of(elec_vars), prop_all)


df_harm <-  df_harm %>%
  ungroup() |>
  mutate(
    prop_all = rowSums(select(., all_of(elec_vars)), na.rm = TRUE)
  )
