### European Elections 2009-2024: Harmonize to 2021 Municipality Boundaries
# Maps EW municipality-level results onto fixed 2021 boundaries
# using population-weighted crosswalks.
# Vincent Heddesheimer
# April 2026

rm(list = ls())
source("code/shared/harmonization_audit.R")

options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "haschaR")
conflict_prefer("filter", "dplyr")


# --- 1. Read data -----------------------------------------------------------

df <- gerda_read_election_source("data/european_elections/final/european_muni_unharm.rds") |>
  as_tibble()

cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

cat("Unharm rows:", nrow(df), "\n")
cat("Unharm cols:", ncol(df), "\n")
cat("Years:", paste(sort(unique(df$election_year)), collapse = ", "), "\n")


# --- 2. Identify party columns and convert shares to counts -----------------

party_cols <- sort(setdiff(
  names(df),
  c("ags", "county", "state", "state_name", "election_year", "election_date",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
    "voters_wo_sperrvermerk", "voters_w_sperrvermerk", "voters_par24_2",
    "voters_w_wahlschein", "turnout", "flag_turnout_above_1", "flag_pooled")
))

all_numeric_cols <- c(
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
  "voters_wo_sperrvermerk", "voters_w_sperrvermerk", "voters_par24_2",
  "voters_w_wahlschein",
  party_cols
)

cat("Party columns:", length(party_cols), "\n")

# Convert party vote shares to absolute counts. Shares are shares of valid
# votes (01_european_muni_unharm.R), so the same denominator must be used here
# and in section 7.
# (NA only in the Gemeinden the 2024 source counts inside a neighbour)
stopifnot(all(df$flag_pooled[is.na(df$valid_votes)] == 1))
df <- df |>
  mutate(across(all_of(party_cols), ~ .x * valid_votes))


# --- 3. Aggregate Berlin Bezirke per year -----------------------------------

# Berlin has 12-14 Bezirke rows; aggregate to single city per year
berlin_rows <- df |> filter(state == "11")
cat("Berlin Bezirke rows:", nrow(berlin_rows), "\n")

df <- df |>
  mutate(ags = ifelse(state == "11", "11000000", ags)) |>
  group_by(ags, election_year) |>
  summarise(
    county = first(county),
    state = first(state),
    state_name = first(state_name),
    election_date = first(election_date),
    across(all_of(all_numeric_cols), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    flag_pooled = max(flag_pooled),
    .groups = "drop"
  )

cat("After Berlin aggregation:", nrow(df), "rows\n")


# --- 4. Map crosswalk years -------------------------------------------------

# Each election year maps to the crosswalk year that best represents boundaries
# (2024 is mapped separately in 6e: the 1990-2020 crosswalk ends before it)
cw_year_map <- c(
  "2009" = 2009,
  "2014" = 2014,
  "2019" = 2019
)

df <- df |>
  mutate(year_cw = cw_year_map[as.character(election_year)])


# --- 5. Naive merge with crosswalk ------------------------------------------

df_merged <- df |>
  filter(election_year < 2024) |>
  left_join(
    cw |> select(ags, year, ags_21, pop_cw, area_cw),
    by = c("ags", "year_cw" = "year")
  )

n_matched <- sum(!is.na(df_merged$ags_21))
n_unmatched <- sum(is.na(df_merged$ags_21))
cat("Naive merge: matched =", n_matched, "| unmatched =", n_unmatched, "\n")

# Inspect unmatched
unmatched <- df_merged |>
  filter(is.na(ags_21)) |>
  select(ags, election_year, state, eligible_voters) |>
  distinct()

cat("\nUnmatched AGS codes:\n")
unmatched |>
  arrange(election_year, ags) |>
  print(n = Inf)


# --- 6. Handle unmatched AGS ------------------------------------------------

# 6a: Try year-1 fallback (split matched/unmatched first, per CLAUDE.md)
df_ok <- df_merged |> filter(!is.na(ags_21))
df_fail <- df_merged |>
  filter(is.na(ags_21)) |>
  select(-ags_21, -pop_cw, -area_cw)

cat("\nAttempting year-1 fallback for", nrow(df_fail), "rows...\n")

df_fallback <- df_fail |>
  mutate(year_cw_fb = year_cw - 1) |>
  left_join(
    cw |> select(ags, year, ags_21, pop_cw, area_cw),
    by = c("ags", "year_cw_fb" = "year")
  ) |>
  select(-year_cw_fb)

n_fb_ok <- sum(!is.na(df_fallback$ags_21))
n_fb_fail <- sum(is.na(df_fallback$ags_21))
cat("  Fallback matched:", n_fb_ok, "| still unmatched:", n_fb_fail, "\n")

# Combine successfully matched fallback rows
df_ok <- bind_rows(
  df_ok,
  df_fallback |> filter(!is.na(ags_21))
)

# 6b: Still unmatched — inspect
still_unmatched <- df_fallback |>
  filter(is.na(ags_21)) |>
  select(ags, election_year, state, eligible_voters) |>
  distinct()

if (nrow(still_unmatched) > 0) {
  cat("\nStill unmatched after fallback:\n")
  still_unmatched |> arrange(election_year, ags) |> print(n = Inf)

  # 6c: Identity codes (AGS exists in 2021 boundaries as-is)
  # These are AGS codes that exist in 2021 but aren't in the crosswalk
  # because they didn't change boundaries
  identity_codes <- still_unmatched$ags

  # Check which of these exist as ags_21 in the crosswalk
  valid_ags_21 <- unique(cw$ags_21)
  identity_ok <- identity_codes[identity_codes %in% valid_ags_21]
  identity_fail <- identity_codes[!identity_codes %in% valid_ags_21]

  cat("\nIdentity codes (exist as ags_21):", length(identity_ok), "\n")
  if (length(identity_ok) > 0) cat("  ", paste(identity_ok, collapse = ", "), "\n")

  if (length(identity_fail) > 0) {
    cat("Cannot resolve:", length(identity_fail), "\n")
    cat("  ", paste(identity_fail, collapse = ", "), "\n")
  }

  # Apply identity mapping
  df_identity <- df_fallback |>
    filter(is.na(ags_21) & ags %in% identity_ok) |>
    select(-ags_21, -pop_cw, -area_cw) |>
    mutate(ags_21 = ags, pop_cw = 1, area_cw = 1)

  df_ok <- bind_rows(df_ok, df_identity)

  # 6d: Anything still unmatched cannot be placed
  final_unmatched <- df_fallback |>
    filter(is.na(ags_21) & !ags %in% identity_ok)
  if (nrow(final_unmatched) > 0) {
    cat("\n!!! UNRESOLVED AGS codes:\n")
    final_unmatched |>
      select(ags, election_year, state) |>
      print(n = Inf)
    stop("Cannot resolve all AGS codes. Fix before proceeding.")
  }
}

# 6e: 2024 back onto 2021 boundaries -------------------------------------------
# Until 2026-09 the 2024 results went through the 2020 crosswalk year. A code
# that already existed in 2021 therefore mapped to itself even where it had
# absorbed neighbours by June 2024, so those neighbours vanished from 2024 with
# their votes filed under the survivor (25 Gemeinden: BB 3, TH 15, RP 2, SH 2,
# MV 2, HE 1); only Jahnatal, Berga-Wünschendorf and Uder were split back by
# hand. Routing through the 2025 codes instead would smear votes between
# Gemeinden that were still separate on election day and merged later.
# So the source unit is the Gemeinde as it stood on 9 June 2024 -- the codes
# of the 2024 file itself. Each 2021 Gemeinde is assigned to the one that
# contained it: itself if it still existed, else its 2023 successor, else its
# 2025 successor (the first of these present in the 2024 file). A 2024 result
# is then split over its 2021 members by 2021 population (the correct
# inversion: pop_constituent / pop_unit, never relabelled forward weights).
df_24 <- df |> filter(election_year == 2024)
codes_24 <- df_24$ags
cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds") |>
  as_tibble()
cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  as_tibble() |> filter(year == 2023)
pop_21 <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  filter(year == 2021) |>
  transmute(ags_21, pop_21 = population_ags)

members_24 <- tibble(ags_21 = sort(unique(cw$ags_21))) |>
  left_join(cw_21_23 |> select(ags_21 = ags_2021, ags_23 = ags_2023, w_23 = w_pop),
            by = "ags_21", relationship = "one-to-many") |>
  mutate(ags_23 = coalesce(ags_23, ags_21), w_23 = coalesce(w_23, 1)) |>
  left_join(cw_23_25 |> select(ags_23 = ags, ags_25, w_25 = pop_cw),
            by = "ags_23", relationship = "many-to-many") |>
  mutate(
    ags_25 = coalesce(ags_25, ags_23), w_25 = coalesce(w_25, 1),
    ags = case_when(
      ags_21 %in% codes_24 ~ ags_21,
      ags_23 %in% codes_24 ~ ags_23,
      ags_25 %in% codes_24 ~ ags_25
    ),
    w = case_when(
      ags_21 %in% codes_24 ~ 1,
      ags_23 %in% codes_24 ~ w_23,
      TRUE                 ~ w_23 * w_25
    )
  ) |>
  # a 2021 Gemeinde that still existed keeps weight 1 whatever its later
  # history; otherwise weights reaching the same 2024 code by two routes add up
  group_by(ags_21, ags) |>
  summarise(w = if (first(ags_21) %in% codes_24) 1 else sum(w), .groups = "drop") |>
  left_join(pop_21, by = "ags_21")

# 2021 Gemeinden without a 2024 home must be uninhabited (the source drops
# those rows); anything populated is an error -- except Dierfeld (9
# inhabitants), which the 2024 source lists with an electorate of zero.
lost_21 <- members_24 |> filter(is.na(ags), !ags_21 %in% "07231021")
stopifnot(all(coalesce(lost_21$pop_21, 0) == 0))
members_24 <- members_24 |> filter(!is.na(ags))

map_24 <- members_24 |>
  mutate(mass = coalesce(pop_21, 0) * w) |>
  group_by(ags) |>
  mutate(pop_cw = if (sum(mass) > 0) mass / sum(mass) else w / sum(w)) |>
  ungroup() |>
  filter(pop_cw > 0) |>
  transmute(ags, ags_21, pop_cw, area_cw = pop_cw)
stopifnot(all(codes_24 %in% map_24$ags))
w_chk <- map_24 |> group_by(ags) |> summarise(w = sum(pop_cw)) |> filter(abs(w - 1) > 1e-9)
stopifnot(nrow(w_chk) == 0)

df_24_ok <- df_24 |>
  mutate(year_cw = NA_real_) |>
  left_join(map_24, by = "ags", relationship = "one-to-many")
cat("2024:", nrow(df_24), "Gemeinden ->", n_distinct(df_24_ok$ags_21), "2021 Gemeinden;",
    sum(map_24$ags != map_24$ags_21 | map_24$pop_cw < 1), "split / merged edges\n")
df_ok <- bind_rows(df_ok, df_24_ok)

# The 2024 flag keeps its old meaning: the code is unknown to the 2020 crosswalk.
unmatched <- bind_rows(
  unmatched,
  df_24 |> filter(!ags %in% cw$ags[cw$year == 2020]) |>
    select(ags, election_year, state, eligible_voters)
)

cat("\nAfter all corrections:", nrow(df_ok), "rows\n")

# The flag describes the first lookup, not whether the municipality merged.
failed_keys <- gerda_key(unmatched, c("ags", "election_year"))
df_ok <- df_ok |>
  mutate(flag_unsuccessful_naive_merge = as.integer(
    gerda_key(df_ok, c("ags", "election_year")) %in% failed_keys))

# Identity rows already have an explicit weight of one. Missing crosswalk
# weights are errors, never evidence that the municipality was unchanged.
gerda_audit_mapping(df, df_ok, c("ags", "election_year"), "ags_21",
                    "european_muni_21", target_codes = cw$ags_21,
                    counts = all_numeric_cols)


# --- 7. Weighted aggregation by (ags_21, election_year) ---------------------

df_harm <- df_ok |>
  group_by(ags_21, election_year) |>
  summarise(
    across(
      all_of(all_numeric_cols),
      ~ if (all(is.na(.x))) NA_real_ else sum(.x * pop_cw, na.rm = TRUE)
    ),
    flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE),
    flag_pooled = max(flag_pooled),
    n_predecessors = n(),
    .groups = "drop"
  )

gerda_audit_totals(df, df_harm, "election_year", all_numeric_cols, "european_muni_21")

# Vote shares come from the unrounded counts; rounding each party's weighted
# votes separately made them miss valid_votes, so shares did not sum to 1.
# Only the voter counts are rounded to whole numbers.
count_cols <- setdiff(all_numeric_cols, party_cols)
df_harm <- df_harm |>
  mutate(
    across(all_of(party_cols),
           ~ ifelse(round(valid_votes) > 0, .x / valid_votes, NA_real_)),
    across(all_of(count_cols), round)
  )
share_sum <- rowSums(select(df_harm, all_of(party_cols)))
stopifnot(all(abs(share_sum[which(df_harm$valid_votes > 0)] - 1) < 1e-9))

cat("Harmonized rows:", nrow(df_harm), "\n")


# --- 8. Compute vote shares and turnout -------------------------------------

df_harm <- df_harm |>
  rename(ags = ags_21) |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state),
    county = substr(ags, 1, 5)
  )

# Restore election_date from election_year
date_map <- c(
  "2009" = "2009-06-07",
  "2014" = "2014-05-25",
  "2019" = "2019-05-26",
  "2024" = "2024-06-09"
)
df_harm <- df_harm |>
  mutate(election_date = lubridate::ymd(date_map[as.character(election_year)]))

# Compute turnout (vote shares: section 7)
df_harm <- df_harm |>
  mutate(
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout),
    flag_aggregated = as.integer(n_predecessors > 1)
  )


# --- 9. Reorder and write ---------------------------------------------------

df_harm <- df_harm |>
  select(
    ags, county, state, state_name,
    election_year, election_date,
    eligible_voters, number_voters, valid_votes, invalid_votes,
    voters_wo_sperrvermerk, voters_w_sperrvermerk, voters_par24_2, voters_w_wahlschein,
    turnout,
    all_of(party_cols),
    flag_turnout_above_1,
    flag_unsuccessful_naive_merge,
    flag_pooled,
    flag_aggregated,
    n_predecessors
  ) |>
  arrange(election_year, ags)

glimpse(df_harm)

write_rds(df_harm, "data/european_elections/final/european_muni_harm.rds", compress = "gz")
fwrite(df_harm, "data/european_elections/final/european_muni_harm.csv")

cat("\nWritten:", nrow(df_harm), "rows x", ncol(df_harm), "columns\n")


# --- 10. Sanity checks -------------------------------------------------------

# Rows per year
cat("\nRows per year:\n")
df_harm |> count(election_year) |> print()

# State distribution per year
cat("\nState distribution:\n")
df_harm |>
  group_by(election_year, state, state_name) |>
  summarise(n = n(), mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop") |>
  arrange(election_year, state) |>
  print(n = 70)

# National totals per year
cat("\nNational totals per year:\n")
df_harm |>
  group_by(election_year) |>
  summarise(
    n_muni = n(),
    eligible = sum(eligible_voters, na.rm = TRUE),
    voters = sum(number_voters, na.rm = TRUE),
    valid = sum(valid_votes, na.rm = TRUE),
    turnout = sum(number_voters, na.rm = TRUE) / sum(eligible_voters, na.rm = TRUE)
  ) |>
  print()

# Major party shares per year (weighted)
cat("\nNational party shares per year (weighted):\n")
major_parties <- c("cdu", "csu", "spd", "gruene", "afd", "die_linke", "fdp", "bsw")
for (yr in c(2009, 2014, 2019, 2024)) {
  cat(sprintf("\n  %d:\n", yr))
  sub <- df_harm |> filter(election_year == yr)
  total_v <- sum(sub$valid_votes, na.rm = TRUE)
  for (p in major_parties) {
    if (p %in% names(sub)) {
      s <- sum(sub[[p]] * sub$valid_votes, na.rm = TRUE) / total_v
      if (s > 0.001) cat(sprintf("    %s: %.4f\n", p, s))
    }
  }
}

# Flags per year
cat("\nFlags per year:\n")
df_harm |>
  group_by(election_year) |>
  summarise(
    n_turnout_above_1 = sum(flag_turnout_above_1, na.rm = TRUE),
    n_unsuccessful_merge = sum(flag_unsuccessful_naive_merge, na.rm = TRUE),
    n_aggregated = sum(flag_aggregated, na.rm = TRUE)
  ) |>
  print()

# Check duplicates
cat("\nDuplicate (ags, year) pairs:\n")
dupl <- df_harm |> count(ags, election_year) |> filter(n > 1)
cat("  Count:", nrow(dupl), "\n")


### END
