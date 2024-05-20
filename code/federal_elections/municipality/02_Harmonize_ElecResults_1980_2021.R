### Harmonize BTW electoral results 1980-2021

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# Load packages
pacman::p_load(tidyverse, data.table, readxl, kableExtra, haschaR)

conflicts_prefer(dplyr::filter)


# Read crosswalk files ----------------------------------------------------

cw90 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-1990-1999.xlsx", sheet = 1) |>
    select(
        ags = `Gemeinden\r\n 31.12.1996`,
        # note that excel data names are wrong for this sheet; I checked that this is the correct data
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.1996 in km²`,
        population = `Bevölkerung am 31.12.1996 in 100`
    ) |>
    mutate(year = 1990)

cw94 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-1990-1999.xlsx", sheet = 5) |>
    select(
        ags = `Gemeinden\r\n 31.12.1994`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.1994 in km²`,
        population = `Bevölkerung am 31.12.1994 in 100`
    ) |>
    mutate(year = 1994)
cw98 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-1990-1999.xlsx", sheet = 9) |>
    select(
        ags = `Gemeinden\r\n 31.12.1998`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.1998 in km²`,
        population = `Bevölkerung am 31.12.1998 in 100`
    ) |>
    mutate(year = 1998)
cw02 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-2000-2010.xlsx", sheet = 3) |>
    select(
        ags = `Gemeinden\r\n 31.12.2002`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.2002 in km²`,
        population = `Bevölkerung am 31.12.2002 in 100`
    ) |>
    mutate(year = 2002)
cw05 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-2000-2010.xlsx", sheet = 6) |>
    select(
        ags = `Gemeinden\r\n 31.12.2005`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.2005 in km²`,
        population = `Bevölkerung am 31.12.2005 in 100`
    ) |>
    mutate(year = 2005)
cw09 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-2000-2010.xlsx", sheet = 10) |>
    select(
        ags = `Gemeinden\r\n 31.12.2009`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.2009 in km²`,
        population = `Bevölkerung am 31.12.2009 in 100`
    ) |>
    mutate(year = 2009)
cw13 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-2011-2020.xlsx", sheet = 3) |>
    select(
        ags = `Gemeinden\r\n 31.12.2013`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.2013 in km²`,
        population = `Bevölkerung am 31.12.2013 in 100`
    ) |>
    mutate(year = 2013)
cw17 <- read_excel(path = "01_Data/02_umsteigeschlüssel/gmd_auf_2021/ref-gemeinden-umrech-2021-2011-2020.xlsx", sheet = 7) |>
    select(
        ags = `Gemeinden\r\n 31.12.2017`,
        pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
        area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
        ags_21 = `Gemeinden\r\n 31.12.2021`,
        area = `Fläche am 31.12.2017 in km²`,
        population = `Bevölkerung am 31.12.2017 in 100`
    ) |>
    mutate(year = 2017)

# Combine to one dataframe
cw <- Filter(function(x) is(x, "data.frame"), mget(ls())) |>
    reduce(bind_rows)

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

df <- fread("01_Data/01_BTW/btw_1980_2021_unharm.csv") |>
    # remove population & area that were used for weighting multi mail-in districts
    select(-c(pop, area)) |>
    # create year variable for merging
    mutate(year = ifelse(election_year < 1990, 1990, election_year)) |>
    left_join_check_obs(cw, by = c("ags", "year")) |>
    arrange(ags, election_year)

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
ags21 <- read_excel(path = "01_Data/02_umsteigeschlüssel/31122021_Auszug_GV.xlsx", sheet = 2) |>
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
fwrite(df_harm, file = "01_Data/01_BTW/btw_1980_2021_harm21.csv")

fwrite(df_harm, file = "~/Dropbox (Princeton)/german_energy_transition/data/09_bundestag_election_data/btw_1980_2021_harm21.csv")


fwrite(df_harm, file = "/Users/hanno/Library/CloudStorage/Dropbox/Research/03_German_Energy_Transition/data/09_bundestag_election_data/btw_1980_2021_harm21.csv")

# inspect ---------------------------------------------------------------- (hanno)
inspect <- df_harm %>%
    select(-c(left_wing, left_wing_wLinke, right_wing, cdu_csu)) %>%
    mutate(total_votes = rowSums(select(., cdu:zentrum), na.rm = TRUE))

mean(inspect$total_votes > 1.000001) * 100
quantile(inspect$total_votes, probs = seq(0.90, 1, 0.01))
quantile(inspect$total_votes, probs = seq(0.0, 0.1, 0.01))
nrow(inspect)

## Code below is based on previous procedure where we had places w/ sum > 1 -> this is not the case anymore now

# Sum greater than one ---------------------------------------------

df_harm_greater_one <- inspect %>%
    filter(election_year > 2004) %>%
    group_by(ags) %>%
    filter(any(total_votes > 1.07)) %>%
    distinct(ags, .keep_all = T) %>%
    ungroup()

## Compare this to the df that indicates whether there was ever merger

df_harm_greater_one <- df_harm_greater_one %>%
    left_join_check_obs(
        cw_info_ever_merged_ags_21 %>%
            mutate(ags_21 = as.character(ags_21)) %>%
            mutate(ags_21 = pad_zero_conditional(ags_21, 7)),
        by = c("ags" = "ags_21")
    )

## Pick a random AGs

ags_check <- df_harm_greater_one %>%
    filter(ever_merged == 0) %>%
    slice(1) %>%
    pull(ags)

## Check original data set

cw_check <- cw %>%
    filter(ags_21 == as.numeric(ags_check))

glimpse(cw_check)

## This is the original election df

df_check <- df %>%
    filter(ags == as.numeric(ags_check)) %>%
    dplyr::select(-right_wing, -left_wing, -left_wing_wLinke, -cdu_csu)

## Row sums of all parties as above:

df_check <- df_check %>%
    mutate(
        total_votes = rowSums(select(., cdu:zentrum), na.rm = TRUE)
    )

View(df_check)

## Compare to valid votes

data.frame(
    valid = df_check$valid_votes,
    total = df_check$total_votes
) %>%
    mutate(
        diff = valid - total,
        ratio = valid / total
    )

## Check the same place in the harmonized data

df_harm_check <- inspect %>%
    filter(ags == ags_check)

df_harm_check$total_votes

View(df_harm_check)

## Check distribution of population in inspect for total_votes > 1.0001

inspect %>%
    filter(total_votes > 1.002) %>%
    pull(valid_votes) %>%
    summary()

## all these places are pretty small




glimpse(df_check)

