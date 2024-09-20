#

rm(list = ls())

pacman::p_load(tidyverse, haschaR, fixest, broom, modelsummary)

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

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

s_m_harm$election_year %>% min()
m_m_harm$election_year %>% min()
f_m_harm$election_year %>% min()

# Get covars

df_m <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") %>%
    mutate(epop_ratio = employees_ags * 100 / population_ags) %>%
    dplyr::select(ags_21, year, epop_ratio, population_ags) %>%
    dplyr::rename(ags = ags_21, election_year = year) %>%
    mutate(ags = as.numeric(ags))

df_co_inkar <- read_csv("data/covars_county/cty_inkar_add.csv") %>%
    dplyr::select(1, 2, 3) %>%
    dplyr::rename(county = 1, election_year = 2, cty_unem = 3) %>%
    mutate(
        county = as.character(county),
        county = haschaR::pad_zero_conditional(county, 4)
    )

# Merge

glimpse(df_m)

f_m_harm <- f_m_harm %>%
    left_join_check_obs(df_m, by = c("ags", "election_year")) %>%
    left_join_check_obs(df_co_inkar, by = c("county", "election_year"))

m_m_harm <- m_m_harm %>%
    left_join_check_obs(df_m, by = c("ags", "election_year")) %>%
    left_join_check_obs(df_co_inkar, by = c("county", "election_year"))

s_m_harm <- s_m_harm %>%
    left_join_check_obs(df_m, by = c("ags", "election_year")) %>%
    left_join_check_obs(df_co_inkar, by = c("county", "election_year"))

# Calculate EPOP ratio change since prev election

f_m_harm <- f_m_harm %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(
        epop_ratio_change = epop_ratio - lag(epop_ratio),
        cty_unem_change = cty_unem - lag(cty_unem)
    ) %>%
    mutate(
        epop_ratio_decline = epop_ratio_change < 0,
        cty_unem_increase = cty_unem_change > 0
    ) %>%
    ungroup()


m_m_harm <- m_m_harm %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(
        epop_ratio_change = epop_ratio - lag(epop_ratio),
        cty_unem_change = cty_unem - lag(cty_unem)
    ) %>%
    mutate(
        epop_ratio_decline = epop_ratio_change < 0,
        cty_unem_increase = cty_unem_change > 0
    ) %>%
    ungroup()

# Multiply the DVs by 100

dv_list <- c("turnout", "cdu_csu", "spd")


f_m_harm <- f_m_harm %>%
    mutate(across(all_of(dv_list), ~ . * 100))

m_m_harm <- m_m_harm %>%
    mutate(across(all_of(dv_list), ~ . * 100))

# Get munis w/ CDU and SPD never missing

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

# Add decade and filter

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

f_m_harm <- f_m_harm %>%
    add_decade_and_filter()

m_m_harm <- m_m_harm %>%
    add_decade_and_filter()

# Population to categorical - deciles of the distribution
# Also standardize the EPOP ratio change

f_m_harm <- f_m_harm %>%
    mutate(pop_decile = ntile(population_ags, 10)) %>%
    mutate(epop_ratio_change_std = (epop_ratio_change - mean(epop_ratio_change, na.rm = TRUE)) / sd(epop_ratio_change, na.rm = TRUE))

m_m_harm <- m_m_harm %>%
    mutate(pop_decile = ntile(population_ags, 10)) %>%
    mutate(epop_ratio_change_std = (epop_ratio_change - mean(epop_ratio_change, na.rm = TRUE)) / sd(epop_ratio_change, na.rm = TRUE))

# Models

dv_list <- c("turnout", "cdu_csu", "spd")

m1 <- feols(.[dv_list] ~ epop_ratio_change_std | ags + election_year + pop_decile^election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election")

m2 <- feols(.[dv_list] ~ epop_ratio_change_std | ags + election_year + pop_decile^election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election")

m1_county <- feols(
    .[dv_list] ~ epop_ratio_change_std |
        ags + election_year^county + pop_decile^election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-county")

m2_county <- feols(.[dv_list] ~ epop_ratio_change_std | ags + election_year^county + pop_decile^election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-county")

m1_state <- feols(.[dv_list] ~ epop_ratio_change_std | ags + election_year^state + pop_decile^election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-state")

m2_state <- feols(.[dv_list] ~ epop_ratio_change_std | ags + election_year^state + pop_decile^election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-state")

# Combine all, rename the DVs

models <- bind_rows(m1, m2, m1_county, m2_county, m1_state, m2_state) %>%
    mutate(term = case_when(
        term == "epop_ratio_change_std" ~ "EPOP ratio change",
        TRUE ~ term
    )) %>%
    mutate(dv = case_when(
        dv == "turnout" ~ "Turnout",
        dv == "cdu_csu" ~ "CDU/CSU vote share",
        dv == "spd" ~ "SPD vote share",
        TRUE ~ dv
    )) %>%
    mutate(model = case_when(
        model == "federal" ~ "Federal election",
        model == "municipal" ~ "Municipal election",
        model == "state" ~ "State election",
        TRUE ~ model
    )) %>%
    mutate(fe = case_when(
        fe == "Election" ~ "Election FE",
        fe == "Election-county" ~ "Election-County FE",
        fe == "Election-state" ~ "Election-State FE",
        TRUE ~ fe
    ))

# Plot this as coefficient plots

pd <- position_dodge(width = 0.4)

models %>%
    ggplot(aes(x = dv, y = estimate, color = model, fill = model)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.0, position = pd) +
    geom_point(size = 2, position = pd) +
    facet_wrap(~fe, scales = "free_x") +
    theme_hanno() +
    theme(legend.position = "bottom") +
    labs(
        x = "Outcome", y = "Estimate", title = "Effect of EPOP ratio change on vote shares",
        subtitle = "Treatment: EPOP ratio change since last election (p.p.)"
    ) +
    scale_color_brewer(palette = "Set2", name = "") +
    scale_fill_brewer(palette = "Set2", name = "") +
    coord_flip()

ggsave("output/figures/application/epop_ratio_change_sample2.pdf",
    width = 9, height = 5
)

# Same analysis w/ EPOP ratio decline as the treatment -----------------

m1 <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year + pop_decile^election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election")

m2 <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year + pop_decile^election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election")

m1_county <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^county + pop_decile^election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-county")

m2_county <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^county,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-county")

m1_state <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^state + pop_decile^election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-state")

m2_state <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^state + pop_decile^election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-state")

models <- bind_rows(m1, m2, m1_county, m2_county, m1_state, m2_state) %>%
    mutate(term = case_when(
        term == "epop_ratio_decline" ~ "EPOP ratio decline",
        TRUE ~ term
    )) %>%
    mutate(dv = case_when(
        dv == "turnout" ~ "Turnout",
        dv == "cdu_csu" ~ "CDU/CSU vote share",
        dv == "spd" ~ "SPD vote share",
        TRUE ~ dv
    )) %>%
    mutate(model = case_when(
        model == "federal" ~ "Federal election",
        model == "municipal" ~ "Municipal election",
        model == "state" ~ "State election",
        TRUE ~ model
    )) %>%
    mutate(fe = case_when(
        fe == "Election" ~ "Election FE",
        fe == "Election-county" ~ "Election-County FE",
        fe == "Election-state" ~ "Election-State FE",
        TRUE ~ fe
    ))

# Plot this as coefficient plots

pd <- position_dodge(width = 0.4)

models %>%
    ggplot(aes(x = dv, y = estimate, color = model, fill = model)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.0, position = pd) +
    geom_point(size = 2, position = pd) +
    facet_wrap(~fe, scales = "free_x") +
    theme_hanno() +
    theme(legend.position = "bottom") +
    labs(
        x = NULL, y = "Estimate",
        title = "Effect of EPOP ratio decline on vote shares",
        subtitle = "Treatment: EPOP ratio change since last election (continuous)"
    ) +
    scale_color_brewer(palette = "Set2", name = "") +
    scale_fill_brewer(palette = "Set2", name = "") +
    coord_flip()

ggsave("output/figures/application/epop_ratio_decline_sample2.pdf", width = 9, height = 5)

# Unemployment as treatment ----------------------------------------------

# Check: which years are observed ?

f_m_harm %>%
    filter(!is.na(cty_unem_increase)) %>%
    distinct(ags, election_year) %>%
    group_by(ags) %>%
    summarise(first_year = min(election_year), last_year = max(election_year))

# starts in the 2000s

m1 <- feols(.[dv_list] ~ cty_unem_increase | ags + election_year,
    data = f_m_harm, cluster = ~county, fsplit = ~decade
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election") %>%
    mutate(decade = rep(c("Full sample", "2000-09", "2010+"), each = 3))

m2 <- feols(.[dv_list] ~ cty_unem_increase | ags + election_year,
    data = m_m_harm, cluster = ~ags, fsplit = ~decade
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election") %>%
    mutate(decade = rep(c("Full sample", "2000-09", "2010+"), each = 3))

m1_state <- feols(.[dv_list] ~ cty_unem_increase | ags + election_year^state,
    data = f_m_harm, cluster = ~ags, fsplit = ~decade
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-state") %>%
    mutate(decade = rep(c("Full sample", "2000-09", "2010+"), each = 3))

m2_state <- feols(.[dv_list] ~ cty_unem_increase | ags + election_year^state,
    data = m_m_harm, cluster = ~ags, fsplit = ~decade
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-state") %>%
    mutate(decade = rep(c("Full sample", "2000-09", "2010+"), each = 3))

models <- bind_rows(m1, m2, m1_state, m2_state) %>%
    mutate(term = case_when(
        term == "cty_unem_increase" ~ "Unemployment increase",
        TRUE ~ term
    )) %>%
    mutate(dv = case_when(
        dv == "turnout" ~ "Turnout",
        dv == "cdu_csu" ~ "CDU/CSU\nvote share",
        dv == "spd" ~ "SPD\nvote share",
        TRUE ~ dv
    )) %>%
    mutate(model = case_when(
        model == "federal" ~ "Federal election",
        model == "municipal" ~ "Municipal election",
        model == "state" ~ "State election",
        TRUE ~ model
    )) %>%
    mutate(fe = case_when(
        fe == "Election" ~ "Election FE",
        fe == "Election-county" ~ "Election-County FE",
        fe == "Election-state" ~ "Election-State FE",
        TRUE ~ fe
    )) %>%
    filter(fe == "Election-State FE")

# Levels: full, 2000-09, 2010+

models <- models %>%
    mutate(decade = factor(decade, levels = c("Full sample", "2000-09", "2010+")))

# Plot this as coefficient plots

pd <- position_dodge(width = 0.25)

models %>%
    ggplot(aes(x = dv, y = estimate, color = model, fill = model)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.0, position = pd) +
    geom_point(size = 2, position = pd) +
    facet_wrap(~decade, scales = "free_x") +
    ylim(c(-1.5, 1.6)) +
    theme_hanno() +
    theme(
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm") # Increase the width of the legend key
    ) +
    labs(
        x = NULL, y = "Estimate (p.p.)"
    ) +
    scale_color_brewer(palette = "Set2", name = "") +
    scale_fill_brewer(palette = "Set2", name = "") +
    coord_flip()

ggsave("output/figures/application/unemp_increase_sample2.pdf",
    width = 9, height = 5
)

ggsave("~/Library/CloudStorage/Dropbox/Apps/Overleaf/ElectionPaper/figures/unemp_increase_sample2.pdf",
    width = 7.5, height = 3.5
)

# Levels of EPOP as treatment ----------------------------------------------

f_m_harm$epop_ratio

m1 <- feols(.[dv_list] ~ epop_ratio | ags + election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election")

m2 <- feols(.[dv_list] ~ epop_ratio | ags + election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election")

m1_county <- feols(.[dv_list] ~ epop_ratio | ags + election_year^county,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-county")

m2_county <- feols(.[dv_list] ~ epop_ratio | ags + election_year^county,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-county")

m1_state <- feols(.[dv_list] ~ epop_ratio | ags + election_year^state,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-state")

m2_state <- feols(.[dv_list] ~ epop_ratio | ags + election_year^state,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-state")

models <- bind_rows(m1, m2, m1_county, m2_county, m1_state, m2_state) %>%
    mutate(term = case_when(
        term == "epop_ratio" ~ "EPOP ratio",
        TRUE ~ term
    )) %>%
    mutate(dv = case_when(
        dv == "turnout" ~ "Turnout",
        dv == "cdu_csu" ~ "CDU/CSU vote share",
        dv == "spd" ~ "SPD vote share",
        TRUE ~ dv
    )) %>%
    mutate(model = case_when(
        model == "federal" ~ "Federal election",
        model == "municipal" ~ "Municipal election",
        model == "state" ~ "State election",
        TRUE ~ model
    )) %>%
    mutate(fe = case_when(
        fe == "Election" ~ "Election FE",
        fe == "Election-county" ~ "Election-County FE",
        fe == "Election-state" ~ "Election-State FE",
        TRUE ~ fe
    ))

# Plot this as coefficient plots

pd <- position_dodge(width = 0.4)

models %>%
    ggplot(aes(x = dv, y = estimate, color = model, fill = model)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.0, position = pd) +
    geom_point(size = 2, position = pd) +
    facet_wrap(~fe, scales = "free_x") +
    theme_hanno() +
    theme(legend.position = "bottom") +
    labs(
        x = NULL, y = "Estimate",
        title = "Effect of EPOP ratio on vote shares",
        subtitle = "Treatment: EPOP ratio (continuous)"
    ) +
    scale_color_brewer(palette = "Set2", name = "") +
    scale_fill_brewer(palette = "Set2", name = "") +
    coord_flip()
