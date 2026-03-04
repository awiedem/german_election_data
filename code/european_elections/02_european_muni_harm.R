### European Elections 2024: Harmonize to 2021 Municipality Boundaries
# Maps EW24 municipality-level results onto fixed 2021 boundaries
# using population-weighted crosswalks.
# Vincent Heddesheimer
# March 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "haschaR")
conflict_prefer("filter", "dplyr")


# --- 1. Read data -----------------------------------------------------------

df <- read_rds("data/european_elections/final/european_muni_unharm.rds") |>
  as_tibble()

cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

cat("Unharm rows:", nrow(df), "\n")
cat("Unharm cols:", ncol(df), "\n")


# --- 2. Identify party columns and convert shares to counts -----------------

party_cols <- names(df)[which(names(df) == "cdu"):which(names(df) == "v_partei3")]
all_numeric_cols <- c(
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
  "voters_wo_sperrvermerk", "voters_w_sperrvermerk", "voters_par24_2",
  "voters_w_wahlschein",
  party_cols
)

# Convert party vote shares to absolute counts
df <- df |>
  mutate(across(all_of(party_cols), ~ .x * number_voters))


# --- 3. Aggregate Berlin Bezirke -------------------------------------------

# Berlin has 14 Bezirke rows (11101000-11212000); aggregate to single city
berlin_ags <- df |> filter(state == "11") |> pull(ags) |> unique()
cat("Berlin Bezirke:", length(berlin_ags), "\n")

df <- df |>
  mutate(ags = ifelse(state == "11", "11000000", ags)) |>
  group_by(ags) |>
  summarise(
    county = first(county),
    state = first(state),
    state_name = first(state_name),
    election_year = first(election_year),
    election_date = first(election_date),
    across(all_of(all_numeric_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

cat("After Berlin aggregation:", nrow(df), "rows\n")


# --- 4. Build crosswalk for EW24 AGS -> ags_21 -----------------------------

# 4a: Direct match with year=2020 crosswalk
cw_2020 <- cw |>
  filter(year == 2020) |>
  select(ags, ags_21, pop_cw, area_cw)

df_matched <- df |>
  left_join(cw_2020, by = "ags") |>
  mutate(ags_21 = pad_zero_conditional(ags_21, 7))

n_matched <- sum(!is.na(df_matched$ags_21))
n_unmatched <- sum(is.na(df_matched$ags_21))
cat("Direct match:", n_matched, "| Unmatched:", n_unmatched, "\n")

# 4b: Build manual crosswalk for unmatched codes
# Identity codes (exist in 2021 boundaries with same AGS)
identity_codes <- c("03153019", "07132502", "11000000", "16063105", "16065089")

# Merger codes: EW24 merged municipality -> split back to 2021 predecessors
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
) |>
  mutate(ags_21 = pad_zero_conditional(as.numeric(ags_21), 7))

# 4c: Apply manual mappings for unmatched codes
df_unmatched <- df_matched |>
  filter(is.na(ags_21)) |>
  select(-ags_21, -pop_cw, -area_cw)

# Identity codes: ags_21 = ags, pop_cw = 1
df_identity <- df_unmatched |>
  filter(ags %in% identity_codes) |>
  mutate(ags_21 = ags, pop_cw = 1, area_cw = 1)

# Merger codes: join with manual crosswalk
df_mergers <- df_unmatched |>
  filter(ags %in% merger_cw$ags) |>
  left_join(merger_cw, by = "ags") |>
  mutate(area_cw = pop_cw)  # Use same weight for area

# Combine
df_all <- bind_rows(
  df_matched |> filter(!is.na(ags_21)),
  df_identity,
  df_mergers
)

cat("After crosswalk mapping:", nrow(df_all), "rows\n")
cat("  (expanded by", nrow(df_all) - nrow(df), "rows from merger splits)\n")

# Check for remaining unmatched
still_unmatched <- df_matched |>
  filter(is.na(ags_21) & !(ags %in% identity_codes) & !(ags %in% merger_cw$ags))
if (nrow(still_unmatched) > 0) {
  warning("Still unmatched AGS: ", paste(still_unmatched$ags, collapse = ", "))
} else {
  cat("All AGS codes successfully mapped to ags_21\n")
}

# Flag unsuccessful naive merges
df_all <- df_all |>
  mutate(
    flag_unsuccessful_naive_merge = as.integer(
      ags %in% c(identity_codes, merger_cw$ags) & ags != "11000000"
    )
  )


# --- 5. Weighted aggregation by ags_21 --------------------------------------

df_harm <- df_all |>
  group_by(ags_21) |>
  summarise(
    across(
      all_of(all_numeric_cols),
      ~ round(sum(.x * pop_cw, na.rm = TRUE), 0)
    ),
    flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge),
    n_predecessors = n(),
    .groups = "drop"
  )

cat("Harmonized rows:", nrow(df_harm), "\n")


# --- 6. Compute vote shares and turnout -------------------------------------

df_harm <- df_harm |>
  rename(ags = ags_21) |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state),
    county = substr(ags, 1, 5),
    election_year = 2024L,
    election_date = lubridate::ymd("2024-06-09")
  )

# Compute total party votes (for vote share denominator)
df_harm <- df_harm |>
  mutate(
    total_party_votes = rowSums(pick(all_of(party_cols)), na.rm = TRUE),
    # Vote shares: party / number_voters (following federal pipeline convention)
    across(
      all_of(party_cols),
      ~ ifelse(number_voters > 0, .x / number_voters, NA_real_)
    ),
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout),
    flag_aggregated = as.integer(n_predecessors > 1)
  )

cat("Rows with turnout > 1:", sum(df_harm$flag_turnout_above_1, na.rm = TRUE), "\n")


# --- 7. Reorder and write ---------------------------------------------------

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
  arrange(ags)

glimpse(df_harm)

write_rds(df_harm, "data/european_elections/final/european_muni_harm.rds")
fwrite(df_harm, "data/european_elections/final/european_muni_harm.csv")

cat("Written:", nrow(df_harm), "rows x", ncol(df_harm), "columns\n")


# --- 8. Sanity checks -------------------------------------------------------

# State distribution
df_harm |>
  group_by(state, state_name) |>
  summarise(
    n = n(),
    mean_turnout = mean(turnout, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(state) |>
  print(n = 16)

# National totals
cat("\nNational totals:\n")
national <- df_harm |>
  summarise(
    eligible = sum(eligible_voters),
    voters = sum(number_voters),
    valid = sum(valid_votes),
    turnout = sum(number_voters) / sum(eligible_voters)
  )
print(national)

# Major party shares
cat("\nNational party shares (weighted):\n")
total_v <- sum(df_harm$number_voters, na.rm = TRUE)
for (p in c("cdu", "csu", "spd", "gruene", "afd", "bsw", "fdp", "die_linke")) {
  s <- sum(df_harm[[p]] * df_harm$number_voters, na.rm = TRUE) / total_v
  cat(sprintf("  %s: %.4f\n", p, s))
}

# Flags
cat("\nFlags:\n")
cat("  flag_turnout_above_1:", sum(df_harm$flag_turnout_above_1), "\n")
cat("  flag_unsuccessful_naive_merge:", sum(df_harm$flag_unsuccessful_naive_merge), "\n")
cat("  flag_aggregated:", sum(df_harm$flag_aggregated), "\n")

# Check duplicates
dupl <- df_harm |> count(ags) |> filter(n > 1)
cat("  Duplicate AGS:", nrow(dupl), "\n")


### END
