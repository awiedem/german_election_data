# Clean environment
rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, purrr, conflicted)

# Resolve conflicts
conflicts_prefer(dplyr::lag)
conflicts_prefer(lubridate::year)

# Load data
df <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm.rds")

# Load raw data
df_raw <- read_rds("data/federal_elections/municipality_level/final/federal_muni_raw.rds")

glimpse(df)

# how many ags?
df %>%
  select(ags) %>%
  distinct() %>%
  nrow()

nrow(df)

# Identify party columns
parties <- df %>%
    dplyr::select(cdu:zentrum) %>%
    colnames()

# Check 1: Sum of party vote shares
s_votes <- df %>%
    dplyr::select(all_of(parties)) %>%
    apply(1, sum, na.rm = TRUE) %>%
    round(8)

cat("Share of rows with sum of party vote shares not equal to 1 (in pct):", 100 * mean(s_votes != 1), "\n")

# 0.001 %

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
            ratio = ~ (. / lag(.))
        ),
        .names = "{col}_{fn}"
    )) %>%
    ungroup()

# Reshape data for visualization
df_long <- df %>%
    pivot_longer(
        cols = ends_with("ratio"),
        names_to = "party",
        values_to = "ratio"
    ) %>%
    mutate(party = gsub("_ratio", "", party))

# Plot distribution of vote share changes
ggplot(df_long %>% filter(abs(ratio) >= 0.5), aes(x = abs(ratio))) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(~party, scales = "free") +
    ggtitle("Distribution of Ratio between current and lagged vote shares") +
    xlab("Ratio of vote share in t to vote share in t-1") +
    ylab("Frequency") +
    theme_minimal()

# Calculate quantiles for each party's percentage changes
party_quantiles <- lapply(parties_main, function(party) {
    df[[paste0(party, "_ratio")]] %>%
        quantile(c(0.01, 0.5, 0.99, 0.999, 0.9999), na.rm = TRUE)
})
names(party_quantiles) <- parties_main
print(party_quantiles)

# Check linke_pds

df %>%
    arrange(ags, election_year) %>%
    dplyr::select(ags, election_year, linke_pds, linke_pds_ratio, valid_votes) %>%
    group_by(ags) %>%
    filter(any(abs(linke_pds_ratio) >= 50) & !any(is.infinite(linke_pds_ratio))) %>%
    print(n = 100)
## Fine: Small towns and Linke becomes suddenly more feasible (from PDA to Linke)



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
    filter(any(valid_votes_ratio > 2)) %>%
    dplyr::select(ags, election_year, valid_votes, valid_votes_ratio)

print(nrow(df_check_large_changes))

df_check_large_changes %>%
    filter(valid_votes > 1000) %>%
    print(n = 500)

## oftentimes created by mergers; but show up like this in raw (unharmonized data)

# Check 5: How many times does a municipality appear in the data?

df %>%
    group_by(ags) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    pull(n) %>%
    table()

# 6     7     8     9
# 1     2   326 10459

# does each municipality appear exactly 9 times?
# No; 10459 municipalities appear 9 times, 326 municipalities appear 8 times, 2 municipalities appear 7 times, and 1 municipality appears 6 times.

df %>%
    group_by(ags) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    arrange(n)
