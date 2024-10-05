#

pacman::p_load(tidyverse, haschaR)
conflict_prefer("filter", "dplyr")

# Get destatis table

df <- read_delim("data/data_checks/destatis_14111_0004.csv",
    delim = ";", skip = 3, locale = locale(encoding = "latin1")
) %>%
    dplyr::rename(
        state = 1,
        party = 2,
        candvote_2013 = 3,
        candvoteshare_2013 = 4,
        partyvote_2013 = 5,
        partyvoteshare_2013 = 6,
        candvote_2017 = 7,
        candvoteshare_2017 = 8,
        partyvote_2017 = 9,
        partyvoteshare_2017 = 10,
        candvote_2021 = 11,
        candvoteshare_2021 = 12,
        partyvote_2021 = 13,
        partyvoteshare_2021 = 14
    ) %>%
    slice(-1:-3) %>%
    dplyr::filter(!is.na(party)) %>%
    dplyr::select(-matches("partyvoteshare|candvote")) %>%
    filter(!partyvote_2013 == "-")

party_df <- data.frame(
    party = c(
        "Sozialdemokratische Partei Deutschlands", "Christlich Demokratische Union Deutschlands",
        "BÜNDNIS 90/DIE GRÜNEN", "Freie Demokratische Partei", "Alternative für Deutschland",
        "Christlich-Soziale Union in Bayern e.V.", "DIE LINKE"
    ),
    party_new = c(
        "spd", "cdu", "gruene", "fdp", "afd", "csu", "linke_pds"
    )
)

View(df)

df <- df %>%
    left_join(party_df, by = "party") %>%
    dplyr::select(-party) %>%
    dplyr::rename(party = party_new)

View(df)

haschaR::state_id_to_names

## Note that Greens in 2021 in Saarland is missing

# State names to state ags

dict_state_ags <- c(
    "Baden-Württemberg" = "08",
    "Bayern" = "09",
    "Berlin" = "11",
    "Brandenburg" = "12",
    "Bremen" = "04",
    "Hamburg" = "02",
    "Hessen" = "06",
    "Mecklenburg-Vorpommern" = "13",
    "Niedersachsen" = "03",
    "Nordrhein-Westfalen" = "05",
    "Rheinland-Pfalz" = "07",
    "Saarland" = "10",
    "Sachsen" = "14",
    "Sachsen-Anhalt" = "15",
    "Schleswig-Holstein" = "01",
    "Thüringen" = "16"
)

# To data frame

dict_state_ags <- data.frame(
    state = names(dict_state_ags),
    state_ags = dict_state_ags
)

# Merge the main df with the dict

df <- df %>%
    left_join(dict_state_ags, by = "state") %>%
    dplyr::select(-state)

# All vote cols to numeric

df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::matches("vote"), as.numeric)) %>%
    filter(!is.na(partyvote_2013))

View(df)

df <- df %>%
    mutate(party = case_when(
        party == "cdu" ~ "cdu_csu",
        party == "csu" ~ "cdu_csu",
        TRUE ~ party
    ))

View(df)

# Get the harmonzed federal election data

fed_elec_data <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm.rds") %>%
    dplyr::select(
        ags, election_year, state, cdu, csu, spd, gruene, fdp, linke_pds, afd,
        valid_votes
    ) %>%
    mutate(across(cdu:afd, ~ . * valid_votes)) %>%
    group_by(state, election_year) %>%
    summarise(across(cdu:afd, sum)) %>%
    filter(election_year %in% c(2013, 2017, 2021)) %>%
    pivot_longer(cols = cdu:afd, names_to = "party", values_to = "votes") %>%
    mutate(party2 = case_when(
        party == "cdu" ~ "cdu_csu",
        party == "csu" ~ "cdu_csu",
        TRUE ~ party
    )) %>%
    group_by(state, election_year, party2) %>%
    summarise(votes = sum(votes, na.rm = TRUE)) %>%
    rename(party = party2) %>%
    ungroup() %>%
    mutate(source = "destatis")

# Also convert the destatis data to long format

df_long <- df %>%
    pivot_longer(
        cols = partyvote_2013:partyvote_2021,
        names_to = c("variable", "year"),
        names_sep = "_",
        values_to = "votes"
    ) %>%
    mutate(year = as.numeric(year)) %>%
    dplyr::rename(state = state_ags, election_year = year) %>%
    dplyr::select(-variable) %>%
    mutate(source = "our_data") %>%
    filter(!is.na(votes))

## Note that the last command removes Greens in 2021 in Saarland


# Combine the two data sets

df_combined <- bind_rows(df_long, fed_elec_data)

# To wide, based on source
# Note that there seem to be some missing cells in destatis data
# Eg AfD is not consistently observed in destatis data

df_combined_wide <- df_combined %>%
    pivot_wider(
        names_from = source,
        values_from = votes
    ) %>%
    mutate(destatis = as.numeric(destatis)) %>%
    mutate(our_data = as.numeric(our_data)) %>%
    filter(!destatis == 0) %>%
    mutate(diff_abs = abs(our_data - destatis)) %>%
    mutate(diff_rel = ifelse(diff_abs == 0, 0, diff_abs * 100 / destatis))


ggplot(df_combined_wide, aes(x = diff_rel)) +
    geom_histogram() +
    facet_wrap(~election_year) +
    theme_minimal() +
    xlab("Difference in %") +
    ylab("Count") +
    ggtitle("Distribution of the absolute difference in total vote counts between our data and destatis data\nAs a % of total vote count from destatis per party and election year")

# Quantiles

cutoffs <- quantile(df_combined_wide$diff_rel, probs = seq(0.7, 1, 0.01))
cutoffs

cutoffs_abs <- quantile(df_combined_wide$diff_abs, probs = seq(0, 1, 0.1))
cutoffs_abs

# Check distribution of observations where we have differences

df_combined_wide %>%
    filter(diff_rel > 0) %>%
    count(party)

df_combined_wide %>%
    mutate(deviation = ifelse(diff_rel > 0, 1, 0)) %>%
    group_by(state, election_year) %>%
    summarise(share_of_parties_with_deviations = sum(deviation) / 6) %>%
    mutate(state = haschaR::state_id_to_names(state)) %>%
    print(n = 100)

mean(df_combined_wide$diff_abs == 0)
nrow(df_combined_wide)

16 * 6 * 3
