#

pacman::p_load(
    tidyverse
)

conflicts_prefer(dplyr::filter)

# Federal ---------------------------------------------------------------------

load("data/municipal_elections/final_output/muni_elec_FINAL.RData")
df <- kommunalwahlen_merge %>%
    mutate(state = substr(AGS_8dig, 1, 2))

# Voter-weighted voteshares by year

parties <- c("prop_CDU", "prop_SPD", "prop_FREIEWÄHLER", "prop_OTHER")


df_agg <- df %>%
    dplyr::filter(!is.na(election_year)) %>%
    group_by(election_year, state) %>%
    summarize(across(
        all_of(parties),
        ~ weighted.mean(.x, Wähler, na.rm = T)
    )) %>%
    pivot_longer(
        cols = -c("election_year", "state"),
        names_to = "party",
        values_to = "vote_share"
    ) %>%
    mutate(party = haschaR::german_party_convert(party)) %>%
    mutate(party = case_when(
        party == "prop_FREIEWÄHLER" ~ "FW",
        party == "prop_GRÜNE" ~ "Greens",
        party == "prop_OTHER" ~ "Other",
        TRUE ~ party
    )) %>%
    mutate(vote_share = ifelse(is.nan(vote_share), NA, vote_share)) %>%
    ungroup() %>%
    filter(!is.na(vote_share)) %>%
    filter(!vote_share == 0) %>%
    mutate(state = haschaR::state_id_to_names(state)) %>%
    ungroup() %>%
    filter(!state == "00")

# Plot

ggplot(
    df_agg %>% mutate(election_year = as.numeric(election_year)),
    aes(x = election_year, y = vote_share, party, color = party, fill = party)
) +
    geom_point() +
    geom_line(aes(group = party)) +
    labs(
        x = "Year",
        y = "Voter-weighted avg. vote share"
    ) +
    haschaR::theme_hanno() +
    theme(
        legend.position = "bottom"
    ) +
    facet_wrap(~state, ncol = 4) +
    scale_x_continuous(breaks = seq(1980, 2040, 4)) +
    haschaR::x_axis_90deg() +
    scale_color_brewer(type = "qual", palette = 2, name = "") +
    scale_fill_brewer(type = "qual", palette = 2, name = "")

# Save this

ggsave("output/figures/vote_share.pdf", width = 11, height = 10)
