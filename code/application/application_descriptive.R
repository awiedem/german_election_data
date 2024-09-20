#

rm(list = ls())

pacman::p_load(tidyverse, haschaR, fixest, broom, modelsummary)

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::between)

# Load data

f_m_harm <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm.rds") %>%
    dplyr::select(turnout, cdu_csu, spd, ags, election_year, state, county, valid_votes) %>%
    mutate(ags = as.numeric(ags)) %>%
    mutate(level = "federal")
m_m_harm <- read_rds("data/municipal_elections/final/municipal_harm.rds") %>%
    dplyr::select(turnout, cdu_csu, spd, ags, year, state, county, valid_votes) %>%
    dplyr::rename(election_year = year) %>%
    mutate(ags = as.numeric(ags)) %>%
    mutate(level = "municipal")
s_m_harm <- read_rds("data/state_elections/final/state_harm.rds") %>%
    mutate(county = substr(ags, 1, 5)) %>%
    dplyr::select(turnout, cdu_csu, spd, ags, election_year, state, county, valid_votes) %>%
    mutate(ags = as.numeric(ags)) %>%
    mutate(level = "state")

# Multiply the DVs by 100

dv_list <- c("turnout", "cdu_csu", "spd")

f_m_harm <- f_m_harm %>%
    mutate(across(all_of(dv_list), ~ . * 100))

m_m_harm <- m_m_harm %>%
    mutate(across(all_of(dv_list), ~ . * 100))

s_m_harm <- s_m_harm %>%
    mutate(across(all_of(dv_list), ~ . * 100))

# Muni df flag: CDU and SPD never missing (across all elections)

m_m_harm <- m_m_harm %>%
    group_by(ags) %>%
    mutate(not_miss_cdu_csu = all(!is.na(cdu_csu))) %>%
    mutate(not_miss_spd = all(!is.na(spd))) %>%
    ungroup()

# Get ags

ags_use <- m_m_harm %>%
    dplyr::filter(not_miss_cdu_csu & not_miss_spd) %>%
    distinct(ags) %>%
    pull(ags)

length(ags_use)

# All data sets: 5k plus, states as listed above

add_decade_and_filter <- function(df) {
    df %>%
        dplyr::filter(ags %in% ags_use) %>%
        mutate(decade = case_when(
            between(election_year, 1990, 1999) ~ "1990-99",
            between(election_year, 2000, 2009) ~ "2000-09",
            between(election_year, 2010, 2030) ~ "2010+",
            TRUE ~ NA_character_
        ))
}

f_m_harm <- add_decade_and_filter(f_m_harm)
m_m_harm <- add_decade_and_filter(m_m_harm)
s_m_harm <- add_decade_and_filter(s_m_harm)

# Check total number of votes in 2021

f_m_harm %>%
    dplyr::filter(election_year == 2021) %>%
    summarise(sum(valid_votes, na.rm = TRUE)) %>%
    pull(1) %>%
    {
        . * 100 / (0.764 * 61172771)
    } %>%
    cat("Share of total valid votes in 2021: ", ., "\n")

# Descriptive -------------------------------------------

# By muni: avg vote share by party and decade

plist <- c("cdu_csu", "spd", "turnout")
f_agg <- f_m_harm %>%
    group_by(decade) %>%
    summarise(across(all_of(plist), list(mean = ~ weighted.mean(.x, valid_votes, na.rm = TRUE))), .groups = "drop") %>%
    pivot_longer(cols = -decade, names_to = c("variable", ".value"), names_pattern = "(.+)_(.+)") %>%
    mutate(level = "Federal")

s_agg <- s_m_harm %>%
    group_by(decade) %>%
    summarise(across(all_of(plist), list(mean = ~ weighted.mean(.x, valid_votes, na.rm = TRUE))), .groups = "drop") %>%
    pivot_longer(cols = -decade, names_to = c("variable", ".value"), names_pattern = "(.+)_(.+)") %>%
    mutate(level = "State")

m_agg <- m_m_harm %>%
    group_by(decade) %>%
    summarise(across(all_of(plist), list(mean = ~ weighted.mean(.x, valid_votes, na.rm = TRUE))), .groups = "drop") %>%
    pivot_longer(cols = -decade, names_to = c("variable", ".value"), names_pattern = "(.+)_(.+)") %>%
    mutate(level = "Municipal")

# Combine

d_agg <- bind_rows(f_agg, s_agg, m_agg) %>%
    mutate(variable = case_when(
        variable == "cdu_csu" ~ "CDU/CSU",
        variable == "spd" ~ "SPD",
        variable == "turnout" ~ "Turnout",
        TRUE ~ variable
    ))

# Plot (lines w/ CI as ribbon)

d_agg %>%
    ggplot(aes(x = decade, y = mean, color = level, fill = level, group = level)) +
    geom_point(size = 2) +
    geom_line() +
    theme_hanno() +
    theme(legend.position = "bottom") +
    labs(x = "Decade", y = "Mean vote share (%)\n(weighted by valid votes)") +
    facet_wrap(~variable, ncol = 3, scales = "free_y") +
    scale_color_brewer(palette = "Set2", name = element_blank()) +
    scale_fill_brewer(palette = "Set2", name = element_blank()) +
    haschaR::x_axis_90deg()

# Save

ggsave("output/figures/application/descr_sample2.pdf",
    width = 7, height = 4
)
ggsave("~/Library/CloudStorage/Dropbox/Apps/Overleaf/ElectionPaper/figures/descr_sample2.pdf",
    width = 7, height = 4
)

# Weighted correlation by decade

pacman::p_load(weights)

# By decade: muni level avg vote shares by party

f_agg_muni <- f_m_harm %>%
    group_by(ags, decade) %>%
    summarise(across(all_of(plist), ~ mean(.x, na.rm = TRUE)),
        valid_votes_federal = mean(valid_votes, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    dplyr::rename(
        cdu_csu_federal = cdu_csu,
        spd_federal = spd,
        turnout_federal = turnout
    )

m_agg_muni <- m_m_harm %>%
    group_by(ags, decade) %>%
    summarise(across(all_of(plist), ~ mean(.x, na.rm = TRUE)),
        valid_votes_muni = mean(valid_votes, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    dplyr::rename(
        cdu_csu_muni = cdu_csu,
        spd_muni = spd,
        turnout_muni = turnout
    )

# Merge

agg_cor <- f_agg_muni %>%
    left_join(m_agg_muni, by = c("ags", "decade"))

agg_cor_90s <- agg_cor %>%
    dplyr::filter(decade == "1990-99")
agg_cor_00s <- agg_cor %>%
    dplyr::filter(decade == "2000-09")
agg_cor_10s <- agg_cor %>%
    dplyr::filter(decade == "2010+")

# Initialize an empty list to store results
cor_results <- list()

# Define variables to loop over
variables <- c("cdu_csu", "spd", "turnout")
decades <- c("1990-99", "2000-09", "2010+")

# Loop over variables and decades
for (var in variables) {
    for (decade in decades) {
        # Select the correct dataset based on decade
        agg_cor_decade <- switch(decade,
            "1990-99" = agg_cor_90s,
            "2000-09" = agg_cor_00s,
            "2010+" = agg_cor_10s
        )

        # Calculate weighted correlation
        cor_result <- wtd.cor(
            x = agg_cor_decade[[paste0(var, "_federal")]],
            y = agg_cor_decade[[paste0(var, "_muni")]],
            weight = agg_cor_decade$valid_votes_federal
        ) %>%
            as_tibble() %>%
            mutate(
                decade = decade,
                party = ifelse(var == "turnout", "Turnout", toupper(var))
            )

        # Add result to the list
        cor_results <- c(cor_results, list(cor_result))
    }
}

# Combine all results into a single dataframe
cor_df <- bind_rows(cor_results) %>%
    mutate(conf.low = correlation - 1.96 * std.err) %>%
    mutate(conf.high = correlation + 1.96 * std.err)

# Plot (party on x, correlation on y, color by decade as above, errorbar CIs)

pd <- position_dodge(width = 0.4)

cor_df %>%
    ggplot(aes(x = party, y = correlation, color = decade, group = decade)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
    geom_point(position = pd, size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = pd, width = 0) +
    theme_hanno() +
    theme(
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm") # Increase the width of the legend key
    ) +
    labs(x = "Party", y = "Municipal-level correlation between\nlocal and federal elections\n(weighted by valid votes)") +
    scale_color_brewer(palette = "Set2", name = element_blank())

# Save

ggsave("output/figures/application/descr_cor_sample2.pdf",
    width = 7, height = 5
)
ggsave("~/Library/CloudStorage/Dropbox/Apps/Overleaf/ElectionPaper/figures/descr_cor_sample2.pdf",
    width = 6, height = 4.5
)
