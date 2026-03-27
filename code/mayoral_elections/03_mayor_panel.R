### Build within-mayor panel from GERDA mayoral elections
# Vincent Heddesheimer
# March 2026
#
# Creates a mayor-level panel with unique person IDs, enabling mayor fixed
# effects (person FE) estimation.
#
# Person identification:
# - Bayern: "Tag des ersten Amtsantritt" groups consecutive terms
# - Named states (NRW, RLP, NI, SL, SH): name-key matching
# - Sachsen: partial name matching (~49% coverage)
#
# Outputs (both unharmonized + harmonized to 2021 boundaries):
# - mayor_panel.rds             — one row per person-election (original AGS)
# - mayor_panel_annual.rds      — one row per person-year (original AGS)
# - mayor_panel_harm.rds        — one row per person-election (AGS 2021)
# - mayor_panel_annual_harm.rds — one row per person-year (AGS 2021)

rm(list = ls())
gc()

# Load libraries
pacman::p_load(
  tidyverse,
  readxl,
  data.table,
  lubridate,
  conflicted,
  here
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("lag", "dplyr")
conflict_prefer("first", "dplyr")

setwd(here::here())
options(scipen = 999)

# Helper function to pad AGS codes
pad_zero_conditional <- function(x, n, pad = "0") {
  x <- as.character(x)
  x[nchar(x) < n] <- paste0(pad, x[nchar(x) < n])
  return(x)
}


# ============================================================================
# 1. LOAD DATA
# ============================================================================

cat("\n=== Loading data ===\n")

cand <- read_rds("data/mayoral_elections/final/mayoral_candidates.rds") |>
  as_tibble()
cat("Loaded mayoral_candidates:", nrow(cand), "rows\n")

# Filter to municipal mayor elections only
valid_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")
cand <- cand |> filter(election_type %in% valid_types)
cat("After filtering to BM/OBM:", nrow(cand), "rows\n")


# ============================================================================
# 2. BAYERN — Person IDs from "Tag des ersten Amtsantritt"
# ============================================================================

cat("\n=== Processing Bayern: person IDs from Amtsantritt ===\n")

bayern_raw <- read_excel(
  "data/mayoral_elections/raw/bayern/20251114_Wahlen_seit_1945.xlsx",
  sheet = "20251114_bewerberRBZ1-7"
) |> as.data.table()

# Build AGS (matching 01_mayoral_unharm.R exactly)
bayern <- bayern_raw |>
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) |>
  mutate(
    gemeindeschluessel_padded = str_pad(as.character(Gemeindeschlüssel),
                                         width = 6, side = "left", pad = "0"),
    ags = paste0("09", gemeindeschluessel_padded),
    election_date = as.Date(`Tag der Wahl`),
    election_year = year(election_date),
    wahlart = as.character(Wahlart),
    amtsantritt = as.Date(`Tag des ersten Amtsantritt`),
    winner_party = `Wahlvorschlag Wahlgewinner`,
    winner_votes = as.numeric(`gültige Stimmen Wahlgewinner`),
    valid_votes = as.numeric(`Gültige Stimmen`),
    eligible_voters = as.numeric(Stimmberechtigte),
    number_voters = as.numeric(Wähler)
  ) |>
  filter(nchar(ags) == 8)

# Exclude "ungültig" rows
bayern <- bayern |>
  filter(is.na(wahlart) | !grepl("ungültig", wahlart, ignore.case = TRUE))

cat("Bayern after excluding ungültig:", nrow(bayern), "rows\n")

# Build a row-level index to retrieve Bewerber columns later
# First, replicate the same filter pipeline on raw data to get aligned rows
bayern_filtered_raw <- bayern_raw |>
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) |>
  mutate(
    gemeindeschluessel_padded = str_pad(as.character(Gemeindeschlüssel),
                                         width = 6, side = "left", pad = "0"),
    ags_tmp = paste0("09", gemeindeschluessel_padded),
    wahlart_tmp = as.character(Wahlart)
  ) |>
  filter(nchar(ags_tmp) == 8) |>
  filter(is.na(wahlart_tmp) | !grepl("ungültig", wahlart_tmp, ignore.case = TRUE))

# Count candidates per row (winner + non-NA Bewerber 2-14)
bewerber_vote_cols <- paste0("gültige Stimmen Bewerber ", 2:14)
existing_bv_cols <- bewerber_vote_cols[bewerber_vote_cols %in% names(bayern_filtered_raw)]

bayern$runner_up_votes_clean <- as.numeric(bayern_filtered_raw$`gültige Stimmen Bewerber 2`)
bayern$n_candidates_raw <- 1L + rowSums(!is.na(
  as.data.frame(bayern_filtered_raw)[, existing_bv_cols, drop = FALSE]
))

# Deduplicate election cycles: keep Stichwahl where available
# Group elections in the same ags within 60 days
bayern <- bayern |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    date_gap = as.numeric(election_date - lag(election_date, default = election_date[1])),
    cycle = cumsum(date_gap > 60)
  ) |>
  ungroup()

# For each cycle: if Stichwahl exists, keep it; else keep the first round
# But carry forward n_candidates from first round
bayern_ncand <- bayern |>
  group_by(ags, cycle) |>
  summarise(
    n_candidates_first = max(n_candidates_raw[wahlart != "Stichwahl" |
                                                is.na(wahlart)],
                              na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(n_candidates_first = ifelse(is.infinite(n_candidates_first),
                                      NA_integer_,
                                      as.integer(n_candidates_first)))

bayern <- bayern |>
  group_by(ags, cycle) |>
  mutate(
    has_stichwahl = any(wahlart == "Stichwahl", na.rm = TRUE),
    is_final = case_when(
      has_stichwahl & wahlart == "Stichwahl" ~ TRUE,
      has_stichwahl & (wahlart != "Stichwahl" | is.na(wahlart)) ~ FALSE,
      !has_stichwahl ~ TRUE
    )
  ) |>
  ungroup() |>
  filter(is_final) |>
  left_join(bayern_ncand, by = c("ags", "cycle"))

# Compute voteshare and margin
bayern <- bayern |>
  mutate(
    winner_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                !is.na(winner_votes),
                              winner_votes / valid_votes, NA_real_),
    runner_up_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                   !is.na(runner_up_votes_clean),
                                 runner_up_votes_clean / valid_votes, NA_real_),
    winning_margin = winner_voteshare - coalesce(runner_up_voteshare, 0),
    n_candidates = coalesce(n_candidates_first, n_candidates_raw)
  )

cat("Bayern after Stichwahl dedup:", nrow(bayern), "rows\n")

# Assign person_id from Amtsantritt
# Same (ags, amtsantritt) = same person
bayern_persons <- bayern |>
  filter(!is.na(amtsantritt)) |>
  distinct(ags, amtsantritt) |>
  arrange(ags, amtsantritt) |>
  group_by(ags) |>
  mutate(person_seq = row_number()) |>
  ungroup() |>
  mutate(person_id = sprintf("p_09_%05d", row_number()))

bayern_panel <- bayern |>
  left_join(bayern_persons, by = c("ags", "amtsantritt")) |>
  arrange(ags, election_date) |>
  mutate(
    state = "09",
    term_start_date = amtsantritt
  ) |>
  select(
    person_id, ags, state,
    election_year, election_date,
    winner_party, winner_voteshare, winning_margin,
    n_candidates, term_start_date
  )

cat("Bayern person-elections:", nrow(bayern_panel), "\n")
cat("Bayern unique persons:", n_distinct(bayern_panel$person_id, na.rm = TRUE), "\n")
cat("Bayern rows without person_id:", sum(is.na(bayern_panel$person_id)), "\n")


# ============================================================================
# 3. NAMED STATES — Person IDs from candidate names
# ============================================================================

cat("\n=== Processing named states: person IDs from names ===\n")

# States with candidate names (excluding Bayern)
named_states <- c("01", "03", "05", "07", "10", "14")

winners_named <- cand |>
  filter(state %in% named_states, is_winner == TRUE)

cat("Winners in named states:", nrow(winners_named), "\n")

# Compute winning margin from candidate data
# Step 1: Get winner voteshare per election
winner_vs_data <- cand |>
  filter(state %in% named_states, is_winner == TRUE) |>
  mutate(
    winner_vs = case_when(
      has_stichwahl & !is.na(candidate_voteshare_sw) ~ candidate_voteshare_sw,
      TRUE ~ candidate_voteshare_hw
    )
  ) |>
  select(ags, election_date, winner_vs, n_candidates_hw) |>
  distinct(ags, election_date, .keep_all = TRUE)

# Step 2: Get runner-up voteshare
# For Stichwahl elections: runner-up is rank_sw == 2
# For non-Stichwahl: runner-up is rank_hw == 2
runner_up_sw <- cand |>
  filter(state %in% named_states, has_stichwahl == TRUE,
         !is.na(candidate_rank_sw), candidate_rank_sw == 2) |>
  select(ags, election_date, runner_up_vs = candidate_voteshare_sw) |>
  distinct(ags, election_date, .keep_all = TRUE)

runner_up_hw <- cand |>
  filter(state %in% named_states,
         (has_stichwahl == FALSE | is.na(has_stichwahl)),
         candidate_rank_hw == 2) |>
  select(ags, election_date, runner_up_vs = candidate_voteshare_hw) |>
  distinct(ags, election_date, .keep_all = TRUE)

runner_up_data <- bind_rows(runner_up_sw, runner_up_hw) |>
  distinct(ags, election_date, .keep_all = TRUE)

# Combine
margin_data <- winner_vs_data |>
  left_join(runner_up_data, by = c("ags", "election_date")) |>
  mutate(
    winning_margin = pmax(winner_vs - coalesce(runner_up_vs, 0), 0),
    n_candidates = as.integer(n_candidates_hw)
  ) |>
  select(ags, election_date, winning_margin, n_candidates)

# Join margin and n_candidates to winners
winners_named <- winners_named |>
  left_join(
    margin_data |> select(ags, election_date, winning_margin, n_candidates),
    by = c("ags", "election_date")
  ) |>
  mutate(
    # Effective winner voteshare (final round)
    winner_voteshare = case_when(
      has_stichwahl & !is.na(candidate_voteshare_sw) ~ candidate_voteshare_sw,
      TRUE ~ candidate_voteshare_hw
    )
  )

# Build name key for person matching
winners_named <- winners_named |>
  mutate(
    name_key = case_when(
      !is.na(candidate_last_name) & !is.na(candidate_first_name) ~
        paste0(tolower(candidate_last_name), "_",
               tolower(substr(candidate_first_name, 1, 1)), "_", state),
      !is.na(candidate_last_name) ~
        paste0(tolower(candidate_last_name), "__", state),
      TRUE ~ NA_character_
    )
  )

cat("Winners with name_key:", sum(!is.na(winners_named$name_key)), "\n")
cat("Winners without name_key:", sum(is.na(winners_named$name_key)), "\n")

# Assign person_id per (name_key, ags) combination
named_persons <- winners_named |>
  filter(!is.na(name_key)) |>
  distinct(name_key, ags) |>
  arrange(ags, name_key) |>
  mutate(person_id = sprintf("p_%s_%05d",
                              substr(ags, 1, 2),
                              row_number()))

# But we need state-specific numbering
named_persons <- winners_named |>
  filter(!is.na(name_key)) |>
  distinct(name_key, ags, state) |>
  arrange(state, ags, name_key) |>
  group_by(state) |>
  mutate(person_id = sprintf("p_%s_%05d", state[1], row_number())) |>
  ungroup()

winners_named <- winners_named |>
  left_join(named_persons |> select(name_key, ags, person_id),
            by = c("name_key", "ags"))

# Build panel rows for named states
named_panel <- winners_named |>
  select(
    person_id, ags, state, election_year, election_date,
    winner_party = candidate_party,
    winner_voteshare, winning_margin, n_candidates,
    # Candidate characteristics (from 04_candidate_characteristics.R)
    candidate_gender, candidate_gender_source, candidate_gender_prob,
    candidate_gender_method,
    candidate_migration_bg, candidate_migration_bg_prob,
    candidate_name_origin, candidate_name_origin_conf,
    candidate_name_origin_method
  ) |>
  mutate(term_start_date = NA_Date_)

cat("\nNamed states person-elections:", nrow(named_panel), "\n")
cat("Named states unique persons:", n_distinct(named_panel$person_id, na.rm = TRUE), "\n")

# By state:
for (s in sort(unique(named_panel$state))) {
  sub <- named_panel |> filter(state == s)
  cat(sprintf("  State %s: %d elections, %d persons\n",
              s, nrow(sub), n_distinct(sub$person_id, na.rm = TRUE)))
}


# ============================================================================
# 4. COMBINE ALL STATES
# ============================================================================

cat("\n=== Combining all states ===\n")

panel <- bind_rows(bayern_panel, named_panel) |>
  arrange(state, ags, election_date)

cat("Combined panel:", nrow(panel), "rows\n")
cat("Unique persons:", n_distinct(panel$person_id, na.rm = TRUE), "\n")
cat("Rows without person_id:", sum(is.na(panel$person_id)), "\n")


# ============================================================================
# 5. BUILD PANEL COLUMNS
# ============================================================================

cat("\n=== Building panel columns ===\n")

# Remove rows without person_id and deduplicate
# When the same person has multiple elections in the same ags+year (HW + SW),
# keep only the latest date (the final/decisive round).
panel <- panel |>
  filter(!is.na(person_id)) |>
  distinct(person_id, ags, election_date, .keep_all = TRUE) |>
  arrange(person_id, ags, election_year, desc(election_date)) |>
  distinct(person_id, ags, election_year, .keep_all = TRUE)

cat("After person-level dedup:", nrow(panel), "rows\n")

# When multiple persons are both marked as winners in the same ags+year
# (HW winner vs SW winner from separate records), keep only the one with
# the highest voteshare — this correctly identifies the overall winner in
# both cases: (a) HW-dated rows with SW data, (b) standalone SW records.
n_before <- nrow(panel)
panel <- panel |>
  arrange(ags, election_year, desc(winner_voteshare)) |>
  distinct(ags, election_year, .keep_all = TRUE)
cat("After ags-year dedup:", nrow(panel), "(dropped", n_before - nrow(panel), "dup winners)\n")

# Term number within each person in each municipality
panel <- panel |>
  arrange(person_id, ags, election_date) |>
  group_by(person_id, ags) |>
  mutate(
    term_number = row_number(),
    tenure_start = min(election_year),
    years_in_office = election_year - tenure_start
  ) |>
  ungroup()

# is_incumbent: term_number >= 2
panel <- panel |>
  mutate(is_incumbent = as.integer(term_number >= 2))

# next_runs_again: does this person win the NEXT election in this ags?
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    next_person_id = lead(person_id),
    next_runs_again = case_when(
      is.na(next_person_id) ~ NA_integer_,
      next_person_id == person_id ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  ungroup() |>
  select(-next_person_id)

# For Bayern: term_start_date is amtsantritt; for others: election_date of first term
panel <- panel |>
  group_by(person_id, ags) |>
  mutate(
    term_start_date = case_when(
      !is.na(term_start_date) ~ term_start_date,  # Bayern: already set
      TRUE ~ min(election_date)  # Others: first election date
    )
  ) |>
  ungroup()

# --- Incumbency enrichment variables ---
cat("\n  Adding incumbency enrichment variables...\n")

# party_switch: did the winning party change from the previous election in this ags?
# is_new_party_mayor: first time this party wins in this municipality (ever in data)
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    prev_party = lag(winner_party),
    party_switch = case_when(
      is.na(prev_party) ~ NA_integer_,
      prev_party == winner_party ~ 0L,
      TRUE ~ 1L
    )
  ) |>
  ungroup()

# is_new_party_mayor: has this party EVER won before in this ags (within data)?
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    is_new_party_mayor = as.integer(
      !duplicated(paste(ags, winner_party)) &
        !is.na(winner_party)
    )
  ) |>
  ungroup()

# margin_change: change in winning margin from previous election
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    prev_margin = lag(winning_margin),
    margin_change = case_when(
      is.na(prev_margin) | is.na(winning_margin) ~ NA_real_,
      TRUE ~ winning_margin - prev_margin
    )
  ) |>
  ungroup() |>
  select(-prev_party, -prev_margin)

# consecutive_terms: number of consecutive terms by same person (resets if gap)
panel <- panel |>
  arrange(person_id, ags, election_date) |>
  group_by(person_id, ags) |>
  mutate(
    prev_election_year = lag(election_year),
    gap = case_when(
      is.na(prev_election_year) ~ FALSE,
      # Allow gaps up to max term length + 2 (e.g., 8 years for 6-year terms)
      election_year - prev_election_year > 10 ~ TRUE,
      TRUE ~ FALSE
    ),
    consec_group = cumsum(gap),
    consecutive_terms = row_number()
  ) |>
  ungroup() |>
  select(-prev_election_year, -gap, -consec_group)

cat(sprintf("  party_switch: %d switches out of %d with data (%.1f%%)\n",
            sum(panel$party_switch == 1L, na.rm = TRUE),
            sum(!is.na(panel$party_switch)),
            100 * mean(panel$party_switch == 1L, na.rm = TRUE)))
cat(sprintf("  is_new_party_mayor: %d first-time party wins\n",
            sum(panel$is_new_party_mayor == 1L, na.rm = TRUE)))


# ============================================================================
# 6. VALIDATE PERSON IDS
# ============================================================================

cat("\n=== Validation ===\n")

# Check for implausibly long tenures (>30 years)
long_tenures <- panel |>
  group_by(person_id) |>
  summarise(
    tenure_span = max(election_year) - min(election_year),
    n_terms = n(),
    state = first(state),
    ags = first(ags),
    .groups = "drop"
  ) |>
  filter(tenure_span > 30)

if (nrow(long_tenures) > 0) {
  cat("WARNING: Implausibly long tenures (>30 years):", nrow(long_tenures), "\n")
  print(long_tenures |> head(20))
} else {
  cat("No implausibly long tenures (>30 years) — OK\n")
}

# Check no person spans multiple states
multi_state <- panel |>
  group_by(person_id) |>
  summarise(n_states = n_distinct(state), .groups = "drop") |>
  filter(n_states > 1)

if (nrow(multi_state) > 0) {
  cat("WARNING: person_id spanning multiple states:", nrow(multi_state), "\n")
} else {
  cat("No person_id spans multiple states — OK\n")
}


# Snapshot before crosswalk for unharmonized version
panel_unharm <- panel


# ============================================================================
# 7. HARMONIZE TO 2021 BOUNDARIES
# ============================================================================

cat("\n=== Harmonizing to 2021 boundaries ===\n")

# Crosswalk AGS stored as integers (leading zeros dropped). Pad to 8 digits
# to match the 8-digit string AGS used in the mayoral pipeline.
# For 1:N splits, keep only the dominant successor (highest pop_cw) so we
# don't duplicate person-election rows.
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = str_pad(as.character(ags), width = 8, side = "left", pad = "0"),
    ags_21 = str_pad(as.character(ags_21), width = 8, side = "left", pad = "0")
  ) |>
  group_by(ags, year) |>
  slice_max(pop_cw, n = 1, with_ties = FALSE) |>
  ungroup()

# Assign crosswalk lookup year
panel <- panel |>
  mutate(
    cw_year = case_when(
      election_year >= 2021 ~ NA_integer_,
      election_year >= 1990 ~ as.integer(election_year),
      election_year < 1990  ~ 1990L
    )
  )

# Post-2020: identity mapping
panel_post2020 <- panel |>
  filter(is.na(cw_year)) |>
  mutate(ags_21 = ags)

# Pre-2021: crosswalk lookup
panel_pre2021 <- panel |>
  filter(!is.na(cw_year)) |>
  left_join(
    cw |> select(ags, year, ags_21),
    by = c("ags", "cw_year" = "year")
  )

# Handle unmatched: try year-1
unmatched <- panel_pre2021 |> filter(is.na(ags_21))
if (nrow(unmatched) > 0) {
  matched <- panel_pre2021 |> filter(!is.na(ags_21))
  unmatched_try <- unmatched |>
    select(-ags_21) |>
    mutate(cw_year_try = pmax(cw_year - 1L, 1990L)) |>
    left_join(
      cw |> select(ags, year, ags_21),
      by = c("ags", "cw_year_try" = "year")
    ) |>
    select(-cw_year_try)

  # Try year+1 for still unmatched
  still_unmatched <- unmatched_try |> filter(is.na(ags_21))
  if (nrow(still_unmatched) > 0) {
    fixed_minus1 <- unmatched_try |> filter(!is.na(ags_21))
    still_unmatched_try <- still_unmatched |>
      select(-ags_21) |>
      mutate(cw_year_try = pmin(cw_year + 1L, 2020L)) |>
      left_join(
        cw |> select(ags, year, ags_21),
        by = c("ags", "cw_year_try" = "year")
      ) |>
      select(-cw_year_try)
    panel_pre2021 <- bind_rows(matched, fixed_minus1, still_unmatched_try)
  } else {
    panel_pre2021 <- bind_rows(matched, unmatched_try)
  }
} # else all matched

panel <- bind_rows(panel_pre2021, panel_post2020) |>
  select(-cw_year)

n_no_ags21 <- sum(is.na(panel$ags_21))
cat("Rows without ags_21:", n_no_ags21, "\n")
if (n_no_ags21 > 0) {
  cat("Dropping", n_no_ags21, "rows without ags_21 mapping\n")
  panel <- panel |> filter(!is.na(ags_21))
}


# Recompute term_number after crosswalk may have dropped some rows
panel <- panel |>
  arrange(person_id, ags, election_date) |>
  group_by(person_id, ags) |>
  mutate(
    term_number = row_number(),
    tenure_start = min(election_year),
    years_in_office = election_year - tenure_start,
    is_incumbent = as.integer(term_number >= 2)
  ) |>
  ungroup()

# Recompute next_runs_again
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    next_person_id = lead(person_id),
    next_runs_again = case_when(
      is.na(next_person_id) ~ NA_integer_,
      next_person_id == person_id ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  ungroup() |>
  select(-next_person_id)

# Recompute enrichment variables after harmonization
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    prev_party = lag(winner_party),
    party_switch = case_when(
      is.na(prev_party) ~ NA_integer_,
      prev_party == winner_party ~ 0L,
      TRUE ~ 1L
    ),
    is_new_party_mayor = as.integer(
      !duplicated(paste(ags, winner_party)) & !is.na(winner_party)
    ),
    prev_margin = lag(winning_margin),
    margin_change = case_when(
      is.na(prev_margin) | is.na(winning_margin) ~ NA_real_,
      TRUE ~ winning_margin - prev_margin
    )
  ) |>
  ungroup() |>
  select(-prev_party, -prev_margin)

panel <- panel |>
  arrange(person_id, ags, election_date) |>
  group_by(person_id, ags) |>
  mutate(
    prev_ey = lag(election_year),
    gap = case_when(
      is.na(prev_ey) ~ FALSE,
      election_year - prev_ey > 10 ~ TRUE,
      TRUE ~ FALSE
    ),
    consec_group = cumsum(gap),
    consecutive_terms = row_number()
  ) |>
  ungroup() |>
  select(-prev_ey, -gap, -consec_group)


# ============================================================================
# 8. FINALIZE BOTH VERSIONS (UNHARMONIZED + HARMONIZED)
# ============================================================================

# Helper: compute within-mayor stats, format, and build annual panel
finalize_panel <- function(p, version = "harm") {
  has_ags_21 <- "ags_21" %in% names(p)

  cat(sprintf("\n--- Finalizing %s panel ---\n", version))

  # Within-mayor statistics
  person_stats <- p |>
    group_by(person_id) |>
    summarise(
      n_terms = n(),
      total_tenure_years = max(election_year) - min(election_year),
      has_margin_variation = n_distinct(round(winning_margin, 4), na.rm = TRUE) > 1,
      .groups = "drop"
    )
  p <- p |> left_join(person_stats, by = "person_id")

  # Final column selection
  panel_cols <- c(
    "person_id", "ags",
    if (has_ags_21) "ags_21",
    "state", "election_year", "election_date",
    "term_number", "consecutive_terms", "winner_party", "winner_voteshare",
    "winning_margin", "margin_change",
    "n_candidates", "is_incumbent", "next_runs_again",
    "party_switch", "is_new_party_mayor",
    "tenure_start", "years_in_office", "term_start_date",
    "n_terms", "total_tenure_years", "has_margin_variation",
    # Candidate characteristics
    "candidate_gender", "candidate_gender_source", "candidate_gender_prob",
    "candidate_gender_method",
    "candidate_migration_bg", "candidate_migration_bg_prob",
    "candidate_name_origin", "candidate_name_origin_conf",
    "candidate_name_origin_method"
  )
  p <- p |>
    select(all_of(panel_cols)) |>
    arrange(state, ags, election_date, person_id)

  # Build annual panel
  cat("  Building annual panel...\n")

  p <- p |>
    arrange(ags, election_date) |>
    group_by(ags) |>
    mutate(next_election_year = lead(election_year)) |>
    ungroup()

  max_year <- 2025L

  p_annual <- p |>
    mutate(
      term_end_year = case_when(
        !is.na(next_election_year) ~ pmax(next_election_year - 1L, election_year),
        TRUE ~ max_year
      )
    ) |>
    rowwise() |>
    mutate(year = list(seq(election_year, term_end_year))) |>
    unnest(year) |>
    ungroup() |>
    mutate(
      years_since_election = year - election_year,
      years_to_next_election = case_when(
        !is.na(next_election_year) ~ next_election_year - year,
        TRUE ~ NA_integer_
      ),
      term_length = case_when(
        !is.na(next_election_year) ~ next_election_year - election_year,
        TRUE ~ NA_integer_
      ),
      electoral_cycle_pos = case_when(
        !is.na(term_length) & term_length > 0 ~
          years_since_election / term_length,
        TRUE ~ NA_real_
      )
    )

  annual_cols <- c(
    "ags",
    if (has_ags_21) "ags_21",
    "year", "person_id", "state", "election_year", "election_date",
    "term_number", "winner_party", "winner_voteshare", "winning_margin",
    "n_candidates", "is_incumbent", "next_runs_again",
    "years_since_election", "years_to_next_election",
    "electoral_cycle_pos", "tenure_start", "term_start_date",
    # Candidate characteristics (constant within term)
    "candidate_gender", "candidate_gender_source", "candidate_gender_prob",
    "candidate_gender_method",
    "candidate_migration_bg", "candidate_migration_bg_prob",
    "candidate_name_origin", "candidate_name_origin_conf",
    "candidate_name_origin_method"
  )
  p_annual <- p_annual |>
    select(all_of(annual_cols)) |>
    arrange(ags, year, person_id)

  # Remove temp column from election-level panel
  p <- p |> select(-next_election_year)

  cat(sprintf("  Election panel: %d rows, %d persons\n",
              nrow(p), n_distinct(p$person_id)))
  cat(sprintf("  Annual panel: %d rows, years %d-%d\n",
              nrow(p_annual), min(p_annual$year), max(p_annual$year)))

  list(panel = p, annual = p_annual)
}

cat("\n=== Finalizing both versions ===\n")

unharm <- finalize_panel(panel_unharm, version = "unharm")
harm   <- finalize_panel(panel, version = "harm")


# ============================================================================
# 9. SUMMARY
# ============================================================================

cat("\n=== Mayor Panel Summary ===\n")
cat("Unharmonized (original boundaries):\n")
cat(sprintf("  Election panel: %d rows, %d persons\n",
            nrow(unharm$panel), n_distinct(unharm$panel$person_id)))
cat(sprintf("  Annual panel: %d rows\n", nrow(unharm$annual)))
cat("Harmonized (2021 boundaries):\n")
cat(sprintf("  Election panel: %d rows, %d persons\n",
            nrow(harm$panel), n_distinct(harm$panel$person_id)))
cat(sprintf("  Annual panel: %d rows\n", nrow(harm$annual)))

cat("\nState breakdown (harmonized):\n")
for (s in sort(unique(harm$panel$state))) {
  sub <- harm$panel |> filter(state == s)
  n_mayors <- n_distinct(sub$person_id)
  n_multi <- sub |>
    group_by(person_id) |>
    filter(n() >= 2) |>
    ungroup() |>
    pull(person_id) |>
    n_distinct()
  state_name <- case_when(
    s == "01" ~ "Schleswig-Holstein",
    s == "03" ~ "Niedersachsen",
    s == "05" ~ "NRW",
    s == "07" ~ "Rheinland-Pfalz",
    s == "09" ~ "Bayern",
    s == "10" ~ "Saarland",
    s == "14" ~ "Sachsen",
    TRUE ~ s
  )
  cat(sprintf("  %s (%s): %d mayors (%d with 2+ terms)\n",
              state_name, s, n_mayors, n_multi))
}


# ============================================================================
# 10. SAVE
# ============================================================================

cat("\n=== Saving ===\n")

# Unharmonized (original boundaries)
write_rds(unharm$panel, "data/mayoral_elections/final/mayor_panel.rds")
fwrite(unharm$panel, "data/mayoral_elections/final/mayor_panel.csv")
cat("Saved mayor_panel.{rds,csv} (unharmonized)\n")

write_rds(unharm$annual, "data/mayoral_elections/final/mayor_panel_annual.rds")
fwrite(unharm$annual, "data/mayoral_elections/final/mayor_panel_annual.csv")
cat("Saved mayor_panel_annual.{rds,csv} (unharmonized)\n")

# Harmonized (2021 boundaries)
write_rds(harm$panel, "data/mayoral_elections/final/mayor_panel_harm.rds")
fwrite(harm$panel, "data/mayoral_elections/final/mayor_panel_harm.csv")
cat("Saved mayor_panel_harm.{rds,csv} (harmonized)\n")

write_rds(harm$annual, "data/mayoral_elections/final/mayor_panel_annual_harm.rds")
fwrite(harm$annual, "data/mayoral_elections/final/mayor_panel_annual_harm.csv")
cat("Saved mayor_panel_annual_harm.{rds,csv} (harmonized)\n")

cat("\n=== Done ===\n")
