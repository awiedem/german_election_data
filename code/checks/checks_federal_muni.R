#

pacman::p_load(
    tidyverse,
    purrr
)

conflicts_prefer(dplyr::lag)

# Federal ---------------------------------------------------------------------

df <- read_csv("data/federal_elections/municipality_level/processed_data/btw_1980_2021_harm21.csv")

glimpse(df)

parties <- df %>%
    dplyr::select(cdu_csu:zentrum) %>%
    dplyr::select(-left_wing, -left_wing_wLinke, -right_wing) %>%
    colnames()

# Checks

# 1. Sum of party vote shares = 1

s_votes <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, sum, na.rm = T) %>%
    round(8)

cat(
    "Share of rows with sum of party vote shares not equal to 1: ",
    mean(s_votes != 1), "\n"
)

df_check <- df %>%
    filter(s_votes < 1) %>%
    dplyr::select(ags, election_year, state, eligible_voters)

df_check$election_year %>% table()
df$election_year %>% table()

# Likely 2021, here party vote shares also do not sum to one
# Election year is completely missing

# 2. All party vote shares are between 0 and 1

s_votes <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, function(x) all(x >= 0 & x <= 1, na.rm = T)) %>%
    sum(na.rm = T)

cat(
    "Share rows with party vote shares between 0 and 1: ",
    (s_votes / nrow(df)), "\n"
)

# 3. Large changes in number of votes?

glimpse(df)

df <- df %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(
        lag_v = lag(valid_votes),
        diff_votes_pct = (valid_votes - lag_v) / lag_v
    ) %>%
    ungroup()

df %>%
    dplyr::select(
        ags, election_year,
        valid_votes, diff_votes_pct
    ) %>%
    head(9)

ggplot(df, aes(x = 100 * abs(diff_votes_pct))) +
    geom_histogram() +
    scale_x_log10()

df$diff_votes_pct %>%
    quantile(c(0.01, 0.5, 0.99, 0.999, 0.9999),
        na.rm = T
    )

# There is one place - check

df_check <- df %>%
    group_by(ags) %>%
    filter(any(diff_votes_pct > 2)) %>%
    dplyr::select(ags, election_year, valid_votes, diff_votes_pct)
df_check %>%
    print(n = 50)

df_check %>%
    pull(ags) %>%
    unique() %>%
    dput()

# Same w/ declines

df_check <- df %>%
    group_by(ags) %>%
    filter(any(diff_votes_pct < -0.5)) %>%
    dplyr::select(ags, election_year, valid_votes, diff_votes_pct)
df_check %>%
    print(n = 50)

df_check %>%
    pull(ags) %>%
    unique() %>%
    dput()

# 4. Large changes in eligible_voters?

df <- df %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(
        lag_ev = lag(eligible_voters),
        diff_ev_pct = (eligible_voters - lag_ev) / lag_ev
    ) %>%
    ungroup()

df %>%
    dplyr::select(ags, election_year, eligible_voters, diff_ev_pct) %>%
    head(9)

ggplot(df, aes(x = 100 * abs(diff_ev_pct))) +
    geom_histogram() +
    scale_x_log10()

df$diff_ev_pct %>%
    quantile(c(0.01, 0.5, 0.99, 0.999, 0.9999), na.rm = T)

# Check

df_check <- df %>%
    group_by(ags) %>%
    filter(any(diff_ev_pct < -0.5)) %>%
    dplyr::select(ags, election_year, eligible_voters, diff_ev_pct)

df_check %>%
    head(10)

# Large changes in % party vote shares between elections?

parties_main <- c("cdu_csu", "spd", "fdp", "grüne", "linke_pds", "afd")

# Calculate the lagged vote shares and their percentage changes
df <- df %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(across(all_of(parties_main),
        list(
            lag = ~ lag(.),
            diff_pct = ~ (. - lag(.)) / lag(.)
        ),
        .names = "{col}_{fn}"
    )) %>%
    ungroup()

# Select and display the calculated changes for verification
df %>%
    dplyr::select(ags, election_year, all_of(parties_main), ends_with("diff_pct")) %>%
    head(10)

# Reshape the data to long format for easier faceting
df_long <- df %>%
    pivot_longer(
        cols = ends_with("diff_pct"),
        names_to = "party",
        values_to = "diff_pct"
    ) %>%
    mutate(party = gsub("_diff_pct", "", party))

# Plot the histograms with facet wrap
ggplot(df_long, aes(x = abs(diff_pct))) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(~party, scales = "free_y") +
    ggtitle("Distribution of Percentage Changes in Party Vote Shares") +
    xlab("Vote Share Change (%)") +
    ylab("Frequency") +
    theme_minimal()

# Calculate quantiles for each party's percentage changes
party_quantiles <- lapply(parties_main, function(party) {
    df[[paste0(party, "_diff_pct")]] %>%
        quantile(c(0.01, 0.5, 0.99, 0.999, 0.9999), na.rm = TRUE)
})

names(party_quantiles) <- parties_main
party_quantiles

# Mostly small parties that see large swings

# Linke:
df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(linke_pds_diff_pct > 2)) %>%
    dplyr::select(ags, election_year, linke_pds, linke_pds_diff_pct)

df_check_large_changes %>%
    head(10)

# AfD:.names

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(afd_diff_pct > 1.5)) %>%
    dplyr::select(ags, election_year, afd, afd_diff_pct)

df_check_large_changes %>%
    head(10)

# FDP

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(fdp_diff_pct > 1.5)) %>%
    dplyr::select(ags, election_year, fdp, fdp_diff_pct)

df_check_large_changes %>%
    head(10)

# spd

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(spd_diff_pct > 1.5)) %>%
    dplyr::select(ags, election_year, spd, spd_diff_pct)

df_check_large_changes %>%
    head(10)

# Looks reasonable
