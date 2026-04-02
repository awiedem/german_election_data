### Harmonize county election (Kreistagswahl) results to 2021 borders
# Based on 02b_state_harm_21.R
# Uses weighted sum of counts method (not weighted mean of shares)
#
# BW (08) and BY (09) have county-level data (5-digit AGS + "000") —
# these use the county crosswalk (cty_crosswalks.csv).
# All other states have municipality-level data (8-digit AGS) —
# these use the municipality crosswalk (ags_crosswalks.rds).
#
# Date: March 2026

rm(list = ls())

conflict_prefer("filter", "dplyr")

options(scipen = 999)

pacman::p_load(
  "tidyverse",
  "data.table",
  "readxl",
  "haschaR"
)

# Load unharmonized data ----------------------------------------------------

cat("Loading unharmonized county election data...\n")

df <- read_rds("data/county_elections/final/county_elec_unharm.rds") |>
  as_tibble() |>
  filter(election_year >= 1990) |>
  mutate(
    # Fix 11-digit AGS from HE 2021 → truncate to 8
    ags = ifelse(nchar(ags) > 8, substr(ags, 1, 8), ags),
    ags = pad_zero_conditional(ags, 7),
    county = str_sub(ags, 1, 5)
  ) |>
  arrange(ags, election_year)

cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")
cat("States:", paste(sort(unique(df$state)), collapse = ", "), "\n")
cat("Years:", paste(sort(unique(df$election_year)), collapse = ", "), "\n")

# Convert vote shares to vote counts ----------------------------------------

metadata_cols <- c("ags", "ags_name", "county", "state", "election_year",
                   "eligible_voters", "number_voters", "valid_votes",
                   "invalid_votes", "turnout")
party_vars <- setdiff(names(df), metadata_cols)

cat("Party variables found:", length(party_vars), "\n")

# For rows with NA valid_votes, impute a weight so that the
# share->count->share round-trip preserves the original percentages.
n_na_vv <- sum(is.na(df$valid_votes))
if (n_na_vv > 0) {
  cat(sprintf("Imputing valid_votes weight for %d rows\n", n_na_vv))
  df <- df |>
    mutate(valid_votes = case_when(
      !is.na(valid_votes) ~ valid_votes,
      !is.na(number_voters) & number_voters > 0 ~ number_voters,
      !is.na(eligible_voters) & eligible_voters > 0 ~ eligible_voters,
      TRUE ~ 1
    ))
}

# Convert vote shares to vote counts
df <- df |>
  mutate(
    across(all_of(party_vars), ~ .x * valid_votes)
  )

# Handle known AGS corrections ------------------------------------------------

df <- df |>
  mutate(
    ags = case_when(
      # Thuringia post-2021 mergers
      ags == "16061119" & election_year == 2024 ~ "16061097",  # Uder
      ags == "16076094" & election_year == 2024 ~ "16076004",  # Berga-Wünschendorf → Berga/Elster
      # Sachsen post-2021 merger
      ags == "14522275" & election_year == 2024 ~ "14522450",  # Jahnatal → Ostrau
      # Sachsen 2008: Tiefenbach dissolved into Döbeln (14522540)
      ags == "14522560" & election_year == 2008 ~ "14522540",  # Tiefenbach → Döbeln
      # Sachsen-Anhalt 1994: Merzien post-Kreisreform code
      ags == "15159029" & election_year == 1994 ~ "15026310",  # Merzien
      # Sachsen-Anhalt 2007: Zeppernick → Raguhn (15086140) via earlier merger
      ags == "15086270" & election_year == 2007 ~ "15086140",  # Zeppernick → Raguhn
      # Sachsen-Anhalt 2007: 7 municipalities merged into Bördeland (15089042)
      ags == "15089040" & election_year == 2007 ~ "15089042",  # Biere → Bördeland
      ags == "15089080" & election_year == 2007 ~ "15089042",  # Eggersdorf → Bördeland
      ags == "15089085" & election_year == 2007 ~ "15089042",  # Eickendorf → Bördeland
      ags == "15089160" & election_year == 2007 ~ "15089042",  # Großmühlingen → Bördeland
      ags == "15089190" & election_year == 2007 ~ "15089042",  # Kleinmühlingen → Bördeland
      ags == "15089335" & election_year == 2007 ~ "15089042",  # Welsleben → Bördeland
      ags == "15089370" & election_year == 2007 ~ "15089042",  # Zens → Bördeland
      TRUE ~ ags
    )
  )

# After correcting AGS codes, aggregate any duplicates
df <- df |>
  group_by(ags, election_year, state) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum(.x, na.rm = TRUE)
    ),
    across(any_of(c("ags_name", "county")), first),
    .groups = "drop"
  ) |>
  arrange(ags, election_year)

cat("After AGS corrections:", nrow(df), "rows\n")

# ==========================================================================
# Split into county-level (BW, BY) and municipality-level (all others)
# ==========================================================================

county_level_states <- c("08", "09")  # BW and BY
df_cty <- df |> filter(state %in% county_level_states)
df_muni <- df |> filter(!state %in% county_level_states)

cat("\nCounty-level data (BW, BY):", nrow(df_cty), "rows\n")
cat("Municipality-level data:", nrow(df_muni), "rows\n")

# ==========================================================================
# PART A: Municipality-level harmonization (all states except BW, BY)
# ==========================================================================

cat("\n--- Part A: Municipality-level harmonization ---\n")

# Load municipality crosswalk
cw_muni <- read_rds("data/crosswalks/final/ags_crosswalks.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  ) |>
  rename(election_year = year)

# Add Wasdow manually (not in crosswalk files)
wasdow <- data.frame(
  ags = "13072115", ags_name = "Wasdow", election_year = 2011,
  area_cw = 1, pop_cw = 1, area = 26.20, population = 0.39,
  ags_21 = "13072010", ags_name_21 = "Behren-Lübchin",
  emp_cw = 1, employees = 0.2
)
for (col in names(cw_muni)) {
  if (!col %in% names(wasdow)) wasdow[[col]] <- NA
}
wasdow <- wasdow |> select(all_of(names(cw_muni)))
cw_muni <- cw_muni |> bind_rows(wasdow) |> arrange(ags, election_year)

# Naive merge
df_cw_naive <- df_muni |>
  left_join_check_obs(cw_muni, by = c("ags", "election_year"))

not_merged_naive <- df_cw_naive |>
  filter(is.na(ags_21)) |>
  select(ags, election_year) |>
  distinct() |>
  mutate(id = paste0(ags, "_", election_year))

cat("Unsuccessful naive merges:", nrow(not_merged_naive), "\n")

df_cw <- df_cw_naive |>
  mutate(
    id = paste0(ags, "_", election_year),
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

# Year-1 fallback (only for unmatched rows)
df_matched <- df_cw |> filter(!is.na(ags_21))
df_unmatched <- df_cw |> filter(is.na(ags_21))

if (nrow(df_unmatched) > 0) {
  cat("Applying year-1 fallback to", nrow(df_unmatched), "unmatched rows...\n")
  df_unmatched <- df_unmatched |>
    mutate(year_cw = pmax(pmin(election_year - 1, 2020), 1990)) |>
    select(-ags_21, -pop_cw, -area_cw) |>
    left_join(
      cw_muni |> select(ags, election_year, ags_21, pop_cw, area_cw) |>
        rename(year_cw = election_year),
      by = c("ags", "year_cw")
    )
}
df_cw <- bind_rows(df_matched, df_unmatched)

# Fuzzy time matching for still-unmatched AGS
still_unmatched <- df_cw |> filter(is.na(ags_21))
if (nrow(still_unmatched) > 0) {
  n_unmatched_ags <- n_distinct(still_unmatched$ags)
  cat("Fuzzy time matching for", n_unmatched_ags, "unmatched AGS codes...\n")

  df_already_matched <- df_cw |> filter(!is.na(ags_21))
  unmatched_keys <- still_unmatched |> select(ags, election_year) |> distinct()
  cw_available <- cw_muni |> select(ags, election_year) |> distinct() |>
    rename(cw_year = election_year)

  best_cw_year <- unmatched_keys |>
    left_join(cw_available, by = "ags", relationship = "many-to-many") |>
    filter(!is.na(cw_year)) |>
    mutate(year_dist = abs(cw_year - election_year) +
             ifelse(cw_year < election_year, 0.001, 0)) |>
    group_by(ags, election_year) |>
    slice_min(year_dist, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(ags, election_year, cw_year)

  still_unmatched <- still_unmatched |>
    select(-ags_21, -pop_cw, -area_cw) |>
    left_join(best_cw_year, by = c("ags", "election_year")) |>
    left_join(
      cw_muni |> select(ags, election_year, ags_21, pop_cw, area_cw) |>
        rename(cw_year = election_year),
      by = c("ags", "cw_year")
    ) |>
    select(-cw_year)

  n_recovered <- sum(!is.na(still_unmatched$ags_21))
  cat("  Recovered", n_recovered, "of", nrow(still_unmatched),
      "rows via fuzzy time matching\n")
  df_cw <- bind_rows(df_already_matched, still_unmatched)
}

# Self-mapping: unmatched AGS that are already valid 2021 codes
still_unmatched2 <- df_cw |> filter(is.na(ags_21))
if (nrow(still_unmatched2) > 0) {
  valid_targets <- unique(cw_muni$ags_21)
  self_map <- still_unmatched2$ags %in% valid_targets
  if (any(self_map)) {
    cat("  Self-mapping", sum(self_map), "rows where AGS is already a valid ags_21\n")
    still_unmatched2$ags_21[self_map] <- still_unmatched2$ags[self_map]
    still_unmatched2$pop_cw[self_map] <- 1
    still_unmatched2$area_cw[self_map] <- 1
    df_cw <- bind_rows(df_cw |> filter(!is.na(ags_21)), still_unmatched2)
  }
}

# Report remaining failures — these should NOT exist after all corrections
not_merged_final_muni <- df_cw |>
  filter(is.na(ags_21)) |>
  select(ags, election_year, state) |>
  distinct()
if (nrow(not_merged_final_muni) > 0) {
  cat("ERROR: Still have", nrow(not_merged_final_muni),
      "unsuccessful municipality merges:\n")
  print(not_merged_final_muni, n = Inf)
  stop("Unmatched AGS found. Add AGS corrections or crosswalk entries before proceeding.")
}

df_cw <- df_cw |> filter(!is.na(ags_21))
cat("Municipality rows entering harmonization:", nrow(df_cw), "\n")

# Harmonize municipality-level data
votes_muni <- df_cw |>
  group_by(ags_21, election_year) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum(.x * pop_cw, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ round(.x, digits = 0)
    )
  ) |>
  rename(ags = ags_21) |>
  mutate(
    flag_unsuccessful_naive_merge = 0  # placeholder, updated below
  )

# Carry forward the flag
flag_lookup_muni <- df_cw |>
  mutate(id = paste0(ags_21, "_", election_year)) |>
  group_by(id) |>
  summarise(flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE)) |>
  ungroup()

votes_muni <- votes_muni |>
  mutate(id = paste0(ags, "_", election_year)) |>
  select(-flag_unsuccessful_naive_merge) |>
  left_join(flag_lookup_muni, by = "id") |>
  mutate(flag_unsuccessful_naive_merge = ifelse(
    is.na(flag_unsuccessful_naive_merge), 0, flag_unsuccessful_naive_merge)) |>
  select(-id)

cat("Municipality harmonization:", nrow(votes_muni), "rows\n")

# ==========================================================================
# PART B: County-level harmonization (BW, BY)
# ==========================================================================

cat("\n--- Part B: County-level harmonization (BW, BY) ---\n")

cw_cty <- fread("data/crosswalks/final/cty_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    county_code = pad_zero_conditional(county_code, 4),
    county_code_21 = pad_zero_conditional(county_code_21, 4)
  )

# Extract 5-digit county code from 8-digit AGS
df_cty <- df_cty |>
  mutate(county_code = substr(ags, 1, 5))

# Naive merge on county_code and election_year
df_cty_cw <- df_cty |>
  left_join(
    cw_cty |> select(county_code, year, county_code_21, pop_cw, area_cw),
    by = c("county_code", "election_year" = "year")
  )

not_merged_cty <- df_cty_cw |>
  filter(is.na(county_code_21)) |>
  select(county_code, election_year) |>
  distinct()
cat("Unsuccessful naive county merges:", nrow(not_merged_cty), "\n")

# Year-1 fallback for unmatched county rows
df_cty_matched <- df_cty_cw |> filter(!is.na(county_code_21))
df_cty_unmatched <- df_cty_cw |> filter(is.na(county_code_21))

if (nrow(df_cty_unmatched) > 0) {
  cat("Applying year-1 fallback to", nrow(df_cty_unmatched), "unmatched county rows...\n")
  df_cty_unmatched <- df_cty_unmatched |>
    mutate(year_cw = pmax(pmin(election_year - 1, 2020), 1990)) |>
    select(-county_code_21, -pop_cw, -area_cw) |>
    left_join(
      cw_cty |> select(county_code, year, county_code_21, pop_cw, area_cw) |>
        rename(year_cw = year),
      by = c("county_code", "year_cw")
    )
  df_cty_cw <- bind_rows(df_cty_matched, df_cty_unmatched)
}

# Self-mapping for county codes already in 2021 boundaries
still_unmatched_cty <- df_cty_cw |> filter(is.na(county_code_21))
if (nrow(still_unmatched_cty) > 0) {
  valid_cty_targets <- unique(cw_cty$county_code_21)
  self_map_cty <- still_unmatched_cty$county_code %in% valid_cty_targets
  if (any(self_map_cty)) {
    cat("  Self-mapping", sum(self_map_cty), "county rows\n")
    still_unmatched_cty$county_code_21[self_map_cty] <- still_unmatched_cty$county_code[self_map_cty]
    still_unmatched_cty$pop_cw[self_map_cty] <- 1
    still_unmatched_cty$area_cw[self_map_cty] <- 1
    df_cty_cw <- bind_rows(df_cty_cw |> filter(!is.na(county_code_21)), still_unmatched_cty)
  }
}

not_merged_cty_final <- df_cty_cw |> filter(is.na(county_code_21))
if (nrow(not_merged_cty_final) > 0) {
  cat("ERROR: Still have", nrow(not_merged_cty_final), "unmatched county rows:\n")
  print(not_merged_cty_final |> select(county_code, election_year, state) |> distinct(), n = Inf)
  stop("Unmatched county codes found. Add corrections before proceeding.")
}

df_cty_cw <- df_cty_cw |> filter(!is.na(county_code_21))
cat("County rows entering harmonization:", nrow(df_cty_cw), "\n")

# Harmonize county-level data
votes_cty <- df_cty_cw |>
  group_by(county_code_21, election_year) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum(.x * pop_cw, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ round(.x, digits = 0)
    )
  ) |>
  # Convert 5-digit county code to 8-digit AGS (county + "000")
  mutate(
    ags = paste0(pad_zero_conditional(county_code_21, 4), "000"),
    flag_unsuccessful_naive_merge = 0
  ) |>
  select(-county_code_21)

cat("County harmonization:", nrow(votes_cty), "rows\n")

# ==========================================================================
# Combine and finalize
# ==========================================================================

cat("\n--- Combining municipality and county results ---\n")

votes <- bind_rows(votes_muni, votes_cty)
cat("Combined:", nrow(votes), "rows\n")

# Convert vote counts back to vote shares
df_harm <- votes |>
  mutate(
    across(all_of(party_vars), ~ ifelse(valid_votes > 0, .x / valid_votes, NA_real_)),
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_)
  ) |>
  filter(!is.na(ags)) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = substr(ags, 1, 2),
    county = substr(ags, 1, 5)
  ) |>
  relocate(state, .after = election_year) |>
  arrange(ags, election_year)

# Zero-vote party -> NA recoding -------------------------------------------

derived_cols <- c("other", "cdu_csu", "far_right", "far_left", "far_left_w_linke")
share_cols <- setdiff(party_vars, derived_cols)

zero_party_lookup <- df_harm |>
  group_by(state, election_year) |>
  summarise(across(all_of(share_cols),
                   ~ all(. == 0 | is.na(.), na.rm = FALSE),
                   .names = "allzero__{.col}"),
            .groups = "drop") |>
  pivot_longer(starts_with("allzero__"),
               names_to = "party", values_to = "all_zero",
               names_prefix = "allzero__") |>
  filter(all_zero)

if (nrow(zero_party_lookup) > 0) {
  cat("Applying zero-vote -> NA recoding for", nrow(zero_party_lookup),
      "state-year-party combos\n")
  df_long <- df_harm |>
    mutate(.row_id = row_number()) |>
    pivot_longer(cols = all_of(share_cols), names_to = "party",
                 values_to = "vote_share") |>
    left_join(zero_party_lookup, by = c("state", "election_year", "party")) |>
    mutate(vote_share = if_else(!is.na(all_zero) & all_zero &
                                  (vote_share == 0 | is.na(vote_share)),
                                NA_real_, vote_share)) |>
    select(-all_zero) |>
    pivot_wider(names_from = "party", values_from = "vote_share")
  df_harm <- df_long |> select(-.row_id) |> arrange(ags, election_year)
  cat("Zero-vote -> NA recoding applied\n")
}

# Derived party columns ----------------------------------------------------

far_right_cols <- intersect(
  c("afd", "npd", "rep", "die_rechte", "dvu", "iii_weg", "fap", "ddd", "dsu",
    "die_heimat_heimat", "die_republikaner_rep"),
  names(df_harm))
far_left_cols <- intersect(
  c("dkp", "kpd", "mlpd", "sgp", "psg", "kbw"),
  names(df_harm))

cat("far_right cols:", paste(far_right_cols, collapse = ", "), "\n")
cat("far_left cols:", paste(far_left_cols, collapse = ", "), "\n")

df_harm <- df_harm |>
  mutate(
    far_right = rowSums(across(any_of(far_right_cols)), na.rm = TRUE),
    far_left = rowSums(across(any_of(far_left_cols)), na.rm = TRUE),
    far_left_w_linke = rowSums(across(any_of(c("linke_pds", "pds"))),
                                na.rm = TRUE) + far_left
  )

# Total vote share check ---------------------------------------------------

all_derived <- c("far_right", "far_left", "far_left_w_linke", "cdu_csu")
tvs_cols <- setdiff(party_vars, all_derived)
df_harm <- df_harm |>
  mutate(
    total_vote_share = round(rowSums(across(all_of(tvs_cols)), na.rm = TRUE), 8),
    flag_total_votes_incongruent = ifelse(
      total_vote_share > 1.001 | total_vote_share < 0.999, 1, 0),
    perc_total_votes_incogruence = round(total_vote_share - 1, 6)
  )

cat("\nDuplicate ags x election_year rows:",
    sum(duplicated(df_harm[, c("ags", "election_year")])), "\n")
cat("Rows with incongruent total vote share:",
    sum(df_harm$flag_total_votes_incongruent, na.rm = TRUE), "\n")

# ==========================================================================
# OUTPUT 1: Municipality-level only (exclude BW/BY county-level data)
# ==========================================================================

cat("\n--- Output 1: Municipality-level ---\n")

df_muni_out <- df_harm |> filter(!state %in% county_level_states)

# Add municipality-level covariates
area_pop <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  rename(election_year = year)

df_muni_out <- df_muni_out |>
  left_join_check_obs(area_pop, by = c("ags", "election_year"))

cat("Municipality-level:", nrow(df_muni_out), "rows,",
    n_distinct(df_muni_out$ags), "unique AGS,",
    length(unique(df_muni_out$state)), "states\n")

fwrite(df_muni_out, "data/county_elections/final/county_elec_harm_21_muni.csv")
write_rds(df_muni_out, "data/county_elections/final/county_elec_harm_21_muni.rds")

# ==========================================================================
# OUTPUT 2: County-level (aggregate municipalities to county, + BW/BY)
# ==========================================================================

cat("\n--- Output 2: County-level (aggregated) ---\n")

# Start from the harmonized vote COUNTS (before share conversion).
# We need the count-space data to aggregate municipalities → county.
# Re-derive from df_harm: multiply shares back by valid_votes.
df_harm_counts <- df_harm |>
  mutate(
    across(all_of(party_vars), ~ ifelse(!is.na(.x), .x * valid_votes, NA_real_))
  )

# Aggregate municipality-level rows to county level
# (BW/BY rows already are county-level — county == substr(ags, 1, 5))
df_cty_agg <- df_harm_counts |>
  group_by(county, election_year, state) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum(.x, na.rm = TRUE)
    ),
    flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ round(.x, digits = 0)
    )
  )

# Convert counts back to shares
df_cty_out <- df_cty_agg |>
  mutate(
    across(all_of(party_vars), ~ ifelse(valid_votes > 0, .x / valid_votes, NA_real_)),
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    # Use county code as the identifier (5-digit)
    ags = paste0(county, "000")
  ) |>
  rename(county_code = county) |>
  arrange(county_code, election_year)

# Recompute derived columns and total_vote_share for county-level
df_cty_out <- df_cty_out |>
  mutate(
    far_right = rowSums(across(any_of(far_right_cols)), na.rm = TRUE),
    far_left = rowSums(across(any_of(far_left_cols)), na.rm = TRUE),
    far_left_w_linke = rowSums(across(any_of(c("linke_pds", "pds"))),
                                na.rm = TRUE) + far_left,
    total_vote_share = round(rowSums(across(all_of(tvs_cols)), na.rm = TRUE), 8),
    flag_total_votes_incongruent = ifelse(
      total_vote_share > 1.001 | total_vote_share < 0.999, 1, 0),
    perc_total_votes_incogruence = round(total_vote_share - 1, 6)
  )

# Add county-level covariates
cty_area_pop <- read_excel(
  path = "data/crosswalks/raw/04_KreiseVorjahr.xlsx", sheet = 2
) |>
  select(
    county_code = `Kreisfreie Städte und Landkreise nach Fläche, Bevölkerung und Bevölkerungsdichte`,
    area = `...5`,
    population = `...6`
  ) |>
  slice(8:476) |>
  filter(!is.na(county_code) & nchar(county_code) == 5) |>
  mutate(area = as.numeric(area), population = as.numeric(population))

df_cty_out <- df_cty_out |>
  left_join(cty_area_pop, by = "county_code")

cat("County-level:", nrow(df_cty_out), "rows,",
    n_distinct(df_cty_out$county_code), "unique counties,",
    length(unique(df_cty_out$state)), "states\n")

fwrite(df_cty_out, "data/county_elections/final/county_elec_harm_21_cty.csv")
write_rds(df_cty_out, "data/county_elections/final/county_elec_harm_21_cty.rds")

# ==========================================================================
# Summary
# ==========================================================================

cat("\n=== DONE ===\n")

cat("\nMunicipality-level output:\n")
cat("  File: county_elec_harm_21_muni.{rds,csv}\n")
cat("  Rows:", nrow(df_muni_out), "\n")
cat("  States:", paste(sort(unique(df_muni_out$state)), collapse = ", "), "\n")
cat("  Duplicates:", sum(duplicated(df_muni_out[, c("ags", "election_year")])), "\n")

cat("\nCounty-level output:\n")
cat("  File: county_elec_harm_21_cty.{rds,csv}\n")
cat("  Rows:", nrow(df_cty_out), "\n")
cat("  States:", paste(sort(unique(df_cty_out$state)), collapse = ", "), "\n")
cat("  Duplicates:", sum(duplicated(df_cty_out[, c("county_code", "election_year")])), "\n")

cat("\nCounty-level sanity checks:\n")
cat("  Turnout range:", round(range(df_cty_out$turnout, na.rm = TRUE), 4), "\n")
cat("  Rows with total_vote_share > 1.05:",
    sum(df_cty_out$total_vote_share > 1.05, na.rm = TRUE), "\n")
cat("  Rows with total_vote_share < 0.95:",
    sum(df_cty_out$total_vote_share < 0.95, na.rm = TRUE), "\n")

cat("\nCounty-level rows per state:\n")
print(table(df_cty_out$state))

cat("\nCounty-level rows per election year:\n")
print(table(df_cty_out$election_year))
