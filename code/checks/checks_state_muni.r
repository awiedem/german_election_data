#

pacman::p_load(
    tidyverse,
    purrr
)

conflicts_prefer(dplyr::lag)

# Federal ---------------------------------------------------------------------

df <- read_rds("output/state_harm.rds")

glimpse(df)

parties <- df %>%
    dplyr::select(afd:spd) %>%
    colnames()

# Checks

# 1. Sum of party vote shares = 1

s_votes <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, sum, na.rm = T) %>%
    round(8)

cat(
    "Share of rows with sum of party vote shares not equal to 1: ",
    mean(s_votes != 100), "\n"
)

df_check <- df %>%
    filter(s_votes < 100) %>%
    dplyr::select(ags, year, all_of(parties))

df_check$year %>% table()

# Likely 2021, here party vote shares also do not sum to one
# Election year is completely missing

# 2. All party vote shares are between 0 and 100

s_votes <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, function(x) all(x >= 0 & x <= 100, na.rm = T)) %>%
    sum(na.rm = T)

cat(
    "Share rows with party vote shares between 0 and 100: ",
    (s_votes / nrow(df)), "\n"
)

parties
# Large changes in % party vote shares between elections?

parties_main <- c("cdu_csu", "spd", "fdp", "greens", "left", "afd")

# Calculate the lagged vote shares and their percentage changes
df <- df %>%
    arrange(ags, year) %>%
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
    dplyr::select(ags, year, all_of(parties_main), ends_with("diff_pct")) %>%
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
    filter(any(left_diff_pct > 2)) %>%
    dplyr::select(ags, year, left, left_diff_pct)

df_check_large_changes %>%
    head(10)

# AfD:

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(afd_diff_pct > 1.5)) %>%
    dplyr::select(ags, year, afd, afd_diff_pct)

df_check_large_changes %>%
    head(10)

# FDP

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(fdp_diff_pct > 1.5)) %>%
    dplyr::select(ags, year, fdp, fdp_diff_pct)

df_check_large_changes %>%
    head(10)

# spd

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(spd_diff_pct > 1.5)) %>%
    dplyr::select(ags, year, spd, spd_diff_pct)

df_check_large_changes %>%
    head(10)

# Looks reasonable
