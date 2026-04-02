### European Elections 2009-2024: Harmonize to 2021 Municipality Boundaries
# Maps EW municipality-level results onto fixed 2021 boundaries
# using population-weighted crosswalks.
# Vincent Heddesheimer
# April 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "haschaR")
conflict_prefer("filter", "dplyr")


# --- 1. Read data -----------------------------------------------------------

df <- read_rds("data/european_elections/final/european_muni_unharm.rds") |>
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
    "voters_w_wahlschein", "turnout", "flag_turnout_above_1")
))

all_numeric_cols <- c(
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
  "voters_wo_sperrvermerk", "voters_w_sperrvermerk", "voters_par24_2",
  "voters_w_wahlschein",
  party_cols
)

cat("Party columns:", length(party_cols), "\n")

# Convert party vote shares to absolute counts
df <- df |>
  mutate(across(all_of(party_cols), ~ .x * number_voters))


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
    across(all_of(all_numeric_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

cat("After Berlin aggregation:", nrow(df), "rows\n")


# --- 4. Map crosswalk years -------------------------------------------------

# Each election year maps to the crosswalk year that best represents boundaries
cw_year_map <- c(
  "2009" = 2009,
  "2014" = 2014,
  "2019" = 2019,
  "2024" = 2020  # Latest available crosswalk year
)

df <- df |>
  mutate(year_cw = cw_year_map[as.character(election_year)])


# --- 5. Naive merge with crosswalk ------------------------------------------

df_merged <- df |>
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

  # 6d: Manual merger codes for remaining unmatched
  # These are municipalities that merged AFTER the crosswalk period (post-2020)
  # Population weights from crosswalk analysis
  merger_cw <- tribble(
    ~ags,        ~ags_21,    ~pop_cw,
    # Jahnatal (SN): 2 predecessors
    "14522275", "14522450", 3.52 / (3.52 + 1.31),
    "14522275", "14522620", 1.31 / (3.52 + 1.31),
    # Uder (TH): 11 predecessors (pop_cw from 2023->2025 crosswalk)
    "16061119", "16061007", 0.0853,
    "16061119", "16061028", 0.0134,
    "16061119", "16061065", 0.0503,
    "16061119", "16061067", 0.116,
    "16061119", "16061068", 0.0492,
    "16061119", "16061077", 0.0356,
    "16061119", "16061084", 0.0231,
    "16061119", "16061091", 0.0473,
    "16061119", "16061096", 0.0552,
    "16061119", "16061097", 0.434,
    "16061119", "16061111", 0.0911,
    # Berga-Wuenschendorf (TH): 2 predecessors
    "16076094", "16076004", 0.537,
    "16076094", "16076084", 0.463
  )

  # Apply merger mapping for any remaining unmatched that are in merger_cw
  df_still_fail <- df_fallback |>
    filter(is.na(ags_21) & !ags %in% identity_ok) |>
    select(-ags_21, -pop_cw, -area_cw)

  if (nrow(df_still_fail) > 0) {
    df_mergers <- df_still_fail |>
      filter(ags %in% merger_cw$ags) |>
      left_join(merger_cw, by = "ags") |>
      mutate(area_cw = pop_cw)

    df_ok <- bind_rows(df_ok, df_mergers)

    # Check if anything still unresolved
    final_unmatched <- df_still_fail |>
      filter(!ags %in% merger_cw$ags)
    if (nrow(final_unmatched) > 0) {
      cat("\n!!! UNRESOLVED AGS codes:\n")
      final_unmatched |>
        select(ags, election_year, state) |>
        print(n = Inf)
      stop("Cannot resolve all AGS codes. Fix before proceeding.")
    }
  }
}

cat("\nAfter all corrections:", nrow(df_ok), "rows\n")

# Flag unsuccessful naive merges
df_ok <- df_ok |>
  mutate(
    flag_unsuccessful_naive_merge = as.integer(
      is.na(pop_cw) | (ags != ags_21 & !(state == "11"))
    )
  )

# For identity codes, pop_cw should already be 1. Double check:
df_ok <- df_ok |>
  mutate(pop_cw = ifelse(is.na(pop_cw), 1, pop_cw))


# --- 7. Weighted aggregation by (ags_21, election_year) ---------------------

df_harm <- df_ok |>
  group_by(ags_21, election_year) |>
  summarise(
    across(
      all_of(all_numeric_cols),
      ~ round(sum(.x * pop_cw, na.rm = TRUE), 0)
    ),
    flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE),
    n_predecessors = n(),
    .groups = "drop"
  )

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

# Compute vote shares and turnout
df_harm <- df_harm |>
  mutate(
    across(
      all_of(party_cols),
      ~ ifelse(number_voters > 0, .x / number_voters, NA_real_)
    ),
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
    flag_aggregated,
    n_predecessors
  ) |>
  arrange(election_year, ags)

glimpse(df_harm)

write_rds(df_harm, "data/european_elections/final/european_muni_harm.rds")
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
  total_v <- sum(sub$number_voters, na.rm = TRUE)
  for (p in major_parties) {
    if (p %in% names(sub)) {
      s <- sum(sub[[p]] * sub$number_voters, na.rm = TRUE) / total_v
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
