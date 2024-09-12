#

pacman::p_load(tidyverse, haschaR, fixest, broom, modelsummary)

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

# Load data

f_m_harm <- read_rds("output/federal_muni_harm.rds") %>%
    dplyr::select(turnout, cdu_csu, spd, ags, election_year, state, county, valid_votes) %>%
    mutate(ags = as.numeric(ags)) %>%
    mutate(level = "federal")
m_m_harm <- read_rds("output/municipal_harm.rds") %>%
    dplyr::select(turnout, cdu_csu, spd, ags, year, state, county, valid_votes) %>%
    dplyr::rename(election_year = year) %>%
    mutate(ags = as.numeric(ags)) %>%
    mutate(level = "municipal")
s_m_harm <- read_rds("output/state_harm.rds") %>%
    mutate(county = substr(ags, 1, 5)) %>%
    dplyr::select(turnout, cdu_csu, spd, ags, election_year, state, county, valid_votes) %>%
    mutate(ags = as.numeric(ags)) %>%
    mutate(level = "state")

s_m_harm$election_year %>% min()
m_m_harm$election_year %>% min()
f_m_harm$election_year %>% min()

# Get covars

df_m <- read_rds("data/municipal_covars/ags_area_pop_emp.rds") %>%
    mutate(epop_ratio = employees_ags * 100 / population_ags) %>%
    dplyr::select(ags_21, year, epop_ratio) %>%
    dplyr::rename(ags = ags_21, election_year = year) %>%
    mutate(ags = as.numeric(ags))

# Check: EPOP > 100?

mean(df_m$epop_ratio > 100, na.rm = TRUE)

# Merge

glimpse(df_m)

f_m_harm <- f_m_harm %>%
    left_join_check_obs(df_m, by = c("ags", "election_year"))

m_m_harm <- m_m_harm %>%
    left_join_check_obs(df_m, by = c("ags", "election_year"))

s_m_harm <- s_m_harm %>%
    left_join_check_obs(df_m, by = c("ags", "election_year"))

# Calculate EPOP ratio change since prev election

f_m_harm <- f_m_harm %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(epop_ratio_change = epop_ratio - lag(epop_ratio)) %>%
    mutate(epop_ratio_decline = epop_ratio_change < 0) %>%
    ungroup()

m_m_harm <- m_m_harm %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(epop_ratio_change = epop_ratio - lag(epop_ratio)) %>%
    mutate(epop_ratio_decline = epop_ratio_change < 0) %>%
    ungroup()

# Multiply the DVs by 100

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

# Subset each dataset to just the munis

f_m_harm <- f_m_harm %>%
    dplyr::filter(ags %in% ags_use)

m_m_harm <- m_m_harm %>%
    dplyr::filter(ags %in% ags_use)
# Models

dv_list <- c("turnout", "cdu_csu", "spd")

m1 <- feols(.[dv_list] ~ epop_ratio_change | ags + election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election")

m2 <- feols(.[dv_list] ~ epop_ratio_change | ags + election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election")

m1_county <- feols(.[dv_list] ~ epop_ratio_change | ags + election_year^county,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-county")

m2_county <- feols(.[dv_list] ~ epop_ratio_change | ags + election_year^county,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-county")

m1_state <- feols(.[dv_list] ~ epop_ratio_change | ags + election_year^state,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-state")

m2_state <- feols(.[dv_list] ~ epop_ratio_change | ags + election_year^state,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-state")

# Combine all, rename the DVs

models <- bind_rows(m1, m2, m1_county, m2_county, m1_state, m2_state) %>%
    mutate(term = case_when(
        term == "epop_ratio_change" ~ "EPOP ratio change",
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
        subtitle = "Treatment: EPOP ratio change since last election (p.p.)\nSample: munis w/ >5k votes in NRW, NDS, Saarland, Hessen, S-T"
    ) +
    scale_color_brewer(palette = "Set2", name = "") +
    scale_fill_brewer(palette = "Set2", name = "") +
    coord_flip()

ggsave("output/figures/application/epop_ratio_change_sample2.pdf",
    width = 9, height = 5
)

# Same analysis w/ EPOP ratio decline as the treatment -----------------

m1 <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election")

m2 <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election")

m1_county <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^county,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-county")

m2_county <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^county,
    data = m_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "municipal", fe = "Election-county")
m1_state <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^state,
    data = f_m_harm, cluster = ~ags
) %>%
    map_dfr(tidy_feols) %>%
    mutate(model = "federal", fe = "Election-state")

m2_state <- feols(.[dv_list] ~ epop_ratio_decline | ags + election_year^state,
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
        subtitle = "Treatment: EPOP ratio change since last election (continuous)\nSample: munis w/ >5k votes in NRW, NDS, Saarland, Hessen, S-T"
    ) +
    scale_color_brewer(palette = "Set2", name = "") +
    scale_fill_brewer(palette = "Set2", name = "") +
    coord_flip()

ggsave("output/figures/application/epop_ratio_decline_sample2.pdf", width = 9, height = 5)

# More models --------------------------------------------------------------

co_vars <- read_csv("data/county_covars/cty_area_pop_emp.csv") %>%
    mutate(epop_ratio = employees_cty * 100 / population_cty) %>%
    dplyr::filter(year %in% c(2008, 2009)) %>%
    arrange(county_code_21, year) %>%
    group_by(county_code_21) %>%
    mutate(
        shock = epop_ratio[year == 2009] - epop_ratio[year == 2008],
        population_cty = population_cty[year == 2008]
    ) %>%
    ungroup() %>%
    dplyr::filter(year == 2009) %>%
    dplyr::select(county_code_21, shock, population_cty)

quantile(co_vars$shock, na.rm = TRUE, probs = seq(0, 1, 0.1))

lmr_df <- read_excel("data/application/Zuordnung-Kreise-Arbeitsmarktregionen.xls") %>%
    dplyr::select(1, 3) %>%
    dplyr::rename(county_code_21 = 1, lmr = 2) %>%
    slice(-1:-6)

# Merge to county df

co_vars <- co_vars %>%
    left_join_check_obs(lmr_df) %>%
    group_by(lmr) %>%
    summarise(shock = -1 * weighted.mean(shock, w = population_cty, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(shock_binary = if_else(shock > 0, 1, 0))

# Merge lmr to federal data

glimpse(f_m_harm)

f_m_harm_lmr <- f_m_harm %>%
    left_join_check_obs(lmr_df %>% dplyr::rename(county = county_code_21), by = "county") %>%
    left_join_check_obs(co_vars, by = "lmr")

# Merge lmr to municipal data

m_m_harm_lmr <- m_m_harm %>%
    left_join_check_obs(lmr_df %>% dplyr::rename(county = county_code_21), by = "county") %>%
    left_join_check_obs(co_vars, by = "lmr")

# Models

m_f <- feols(.[dv_list] ~ i(election_year, shock, ref = 2005) | ags + election_year,
    data = f_m_harm_lmr, cluster = ~lmr, weights = ~valid_votes
)


m_f_state <- feols(
    .[dv_list] ~ i(election_year, shock, ref = 2005) |
        ags + election_year^state + election_year[ags],
    data = f_m_harm_lmr, cluster = ~lmr, weights = ~valid_votes
)


iplot(m_f[1])
iplot(m_f[2])
iplot(m_f[3])

iplot(m_f_state[1])
iplot(m_f_state[2])
iplot(m_f_state[3])

# Same w/ binary treatment

m_f_binary <- feols(.[dv_list] ~ i(election_year, shock_binary, ref = 2005) | ags + election_year,
    data = f_m_harm_lmr, cluster = ~lmr, weights = ~valid_votes
)

m_f_binary_state <- feols(
    .[dv_list] ~ i(election_year, shock_binary, ref = 2005) |
        ags + election_year^state + election_year[ags],
    data = f_m_harm_lmr, cluster = ~lmr, weights = ~valid_votes
)

iplot(m_f_binary[1])
iplot(m_f_binary[2])
iplot(m_f_binary[3])

iplot(m_f_binary_state[1])
iplot(m_f_binary_state[2])
iplot(m_f_binary_state[3])
