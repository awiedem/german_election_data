# Clean environment
rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, purrr, conflicted)

# Resolve conflicts
conflicts_prefer(dplyr::lag)
conflicts_prefer(lubridate::year)

# Load data
df <- read_rds("data/state_elections/final/state_harm.rds")
df_raw <- read_rds("data/state_elections/final/state_unharm.rds")


# how many ags?
df %>%
  select(ags) %>%
  distinct() %>%
  nrow()

nrow(df)

# Identify party columns
parties <- df %>%
    dplyr::select(cdu:other) %>%
    colnames()

# Check 1: Sum of party vote shares
s_votes <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, sum, na.rm = TRUE) %>%
    round(8)

cat("Share of rows with sum of party vote shares not equal to 1:", mean(s_votes != 1), "\n")

# Distribution of difference from 1
abs(1 - s_votes) %>%
    quantile(c(0.01, 0.5, 0.8, 0.9, 0.95, 1))

# Investigate cases where sum > 1 (excluding Bavaria)
df_check <- df %>%
    filter(s_votes > 1, state_name != "Bavaria")

df_check_ags <- df %>%
  mutate(total_votes = rowSums(select(., cdu:other), na.rm = TRUE),
         total_votes = round(total_votes, 8)) %>%
  filter(total_votes > 1) 

df_check_ags_raw <- df_raw %>%
  mutate(total_votes = rowSums(select(., cdu:other), na.rm = TRUE),
         total_votes = round(total_votes, 8)) %>%
  filter(total_votes > 1)


print(table(df_check$election_year))
print(table(df_check$state_name))

# Check 2: Party vote shares between 0 and 1
s_votes_valid <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, function(x) all(x >= 0 & x <= 1, na.rm = TRUE)) %>%
    sum(na.rm = TRUE)

cat("Share of rows with all party vote shares between 0 and 1:", s_votes_valid / nrow(df), "\n")

# Check 3: Large changes in party vote shares between elections
parties_main <- c(
    "cdu_csu", "spd", "fdp", "gruene",
    "linke_pds", "afd", "turnout"
)

df <- df %>%
    arrange(ags, election_year) %>%
    group_by(ags) %>%
    mutate(across(all_of(parties_main),
        list(
            lag = ~ lag(.),
            diff_pct = ~ (. / lag(.))
        ),
        .names = "{col}_{fn}"
    )) %>%
    ungroup()

# Reshape data for visualization
df_long <- df %>%
    pivot_longer(
        cols = ends_with("diff_pct"),
        names_to = "party",
        values_to = "diff_pct"
    ) %>%
    mutate(party = gsub("_diff_pct", "", party))

# Plot distribution of vote share changes
ggplot(df_long %>% filter(abs(diff_pct) >= 0.5), aes(x = abs(diff_pct))) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(~party, scales = "free") +
    ggtitle("Distribution of Ratio between current and lagged vote shares") +
    xlab("Ratio of vote share in t to vote share in t-1") +
    ylab("Frequency") +
    theme_minimal()

# Calculate quantiles for each party's percentage changes
party_quantiles <- lapply(parties_main, function(party) {
    df[[paste0(party, "_diff_pct")]] %>%
        quantile(c(0.01, 0.5, 0.99, 0.999, 0.9999), na.rm = TRUE)
})
names(party_quantiles) <- parties_main
print(party_quantiles)

# Check 4: Large changes in valid votes
df <- df %>%
    group_by(ags) %>%
    mutate(
        valid_votes_lag = lag(valid_votes),
        valid_votes_ratio = valid_votes / valid_votes_lag
    ) %>%
    ungroup()

df_check_large_changes <- df %>%
    group_by(ags) %>%
    filter(any(valid_votes_ratio > 1.5)) %>%
    dplyr::select(ags, election_year, valid_votes, valid_votes_ratio)

print(nrow(df_check_large_changes))

df_check_large_changes %>%
    filter(valid_votes > 1000) %>%
    print(n = 100)
