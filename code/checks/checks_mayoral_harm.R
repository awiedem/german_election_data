### Sanity checks for mayoral elections harmonized data
# Date: March 2026

rm(list = ls())

options(scipen = 999)

pacman::p_load(
  "tidyverse",
  "data.table",
  "lubridate"
)

conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")

cat("=", strrep("=", 70), "\n")
cat("MAYORAL ELECTIONS HARMONIZED: DATA QUALITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load datasets ----------------------------------------------------------------

cat("Loading datasets...\n")

df <- read_rds("data/mayoral_elections/final/mayoral_harm.rds") |>
  as_tibble()
df_unharm <- read_rds("data/mayoral_elections/final/mayoral_unharm.rds") |>
  as_tibble()

cat("Loaded mayoral_harm:", nrow(df), "rows x", ncol(df), "cols\n")
cat("Loaded mayoral_unharm:", nrow(df_unharm), "rows x", ncol(df_unharm), "cols\n")
cat("Harm columns:", paste(names(df), collapse = ", "), "\n\n")


# Check 1: Row accounting vs unharm -------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 1: ROW ACCOUNTING (HARM vs UNHARM)\n")
cat(strrep("=", 70), "\n\n")

# Expected losses:
# - VG/SG/Landrat exclusion
# - Unmatched AGS codes (county-level, pre-1972 Bayern)
# - Aggregation of N:1 mergers
# - 4 Bayern duplicates removed

n_vg_sg_landrat <- sum(!df_unharm$election_type %in%
                         c("Bürgermeisterwahl", "Oberbürgermeisterwahl"))
cat(sprintf("Unharm total:       %d\n", nrow(df_unharm)))
cat(sprintf("  - VG/SG/Landrat:  %d\n", n_vg_sg_landrat))
cat(sprintf("  = Harmonizable:   %d\n", nrow(df_unharm) - n_vg_sg_landrat))
cat(sprintf("Harm total:         %d\n", nrow(df)))
cat(sprintf("  Difference:       %d (unmatched AGS + duplicates + aggregated mergers)\n",
            nrow(df_unharm) - n_vg_sg_landrat - nrow(df)))

# Harm should have fewer rows than harmonizable unharm
if (nrow(df) < nrow(df_unharm) - n_vg_sg_landrat) {
  cat(">> PASS: Harm has fewer rows than harmonizable unharm (expected)\n")
} else {
  cat("!! FAIL: Harm has more rows than harmonizable unharm (unexpected)\n")
}

# Only BM and OB types should be present
harm_types <- unique(df$election_type)
if (all(harm_types %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl"))) {
  cat(">> PASS: Only Bürgermeisterwahl and Oberbürgermeisterwahl in harm data\n")
} else {
  cat("!! FAIL: Unexpected election types in harm data:",
      paste(setdiff(harm_types, c("Bürgermeisterwahl", "Oberbürgermeisterwahl")),
            collapse = ", "), "\n")
}


# Check 2: Coverage by state and year ------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 2: COVERAGE BY STATE AND YEAR\n")
cat(strrep("=", 70), "\n\n")

coverage <- df |>
  group_by(state_name, state) |>
  summarise(
    n_rows = n(),
    n_ags = n_distinct(ags),
    min_year = min(election_year, na.rm = TRUE),
    max_year = max(election_year, na.rm = TRUE),
    n_years = n_distinct(election_year),
    .groups = "drop"
  ) |>
  arrange(desc(n_rows))

cat("Coverage by state:\n")
print(coverage)

expected <- tibble(
  state_name = c("Bavaria", "Saxony", "Rhineland-Palatinate",
                 "North Rhine-Westphalia", "Niedersachsen", "Saarland",
                 "Schleswig-Holstein"),
  expected_min_rows = c(28000, 1800, 200, 1500, 50, 50, 30),
  expected_min_year = c(1945, 2001, 1994, 2009, 2006, 2019, 2023),
  expected_max_year = c(2025, 2024, 2025, 2025, 2025, 2025, 2025)
)

coverage_check <- coverage |>
  left_join(expected, by = "state_name") |>
  mutate(
    rows_ok = n_rows >= expected_min_rows,
    min_year_ok = min_year <= expected_min_year,
    max_year_ok = max_year >= expected_max_year
  )

issues <- coverage_check |>
  filter(!rows_ok | !min_year_ok | !max_year_ok)

if (nrow(issues) > 0) {
  cat("\n!! FAIL: Coverage issues:\n")
  print(issues |> select(state_name, n_rows, expected_min_rows, min_year,
                          expected_min_year, max_year, expected_max_year))
} else {
  cat("\n>> PASS: All states have expected coverage\n")
}


# Check 3: AGS validity (2021 boundaries) -------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 3: AGS VALIDITY (2021 BOUNDARIES)\n")
cat(strrep("=", 70), "\n\n")

# All AGS should be 8 characters
ags_lengths <- nchar(df$ags)
non_8 <- sum(ags_lengths != 8, na.rm = TRUE)
na_ags <- sum(is.na(df$ags))

if (na_ags > 0) {
  cat(sprintf("!! FAIL: %d rows with NA AGS\n", na_ags))
} else {
  cat(">> PASS: No NA AGS values\n")
}

if (non_8 > 0) {
  cat(sprintf("!! FAIL: %d rows with AGS not 8 digits\n", non_8))
} else {
  cat(">> PASS: All AGS are 8 characters\n")
}

# AGS prefix matches state code
prefix_mismatch <- df |>
  mutate(ags_prefix = substr(ags, 1, 2)) |>
  filter(ags_prefix != state)

if (nrow(prefix_mismatch) > 0) {
  cat(sprintf("!! FAIL: %d rows where AGS prefix != state code\n",
              nrow(prefix_mismatch)))
} else {
  cat(">> PASS: All AGS prefixes match state codes\n")
}

# Cross-check harm AGS against the crosswalk
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(ags_21 = as.character(ags_21))

# Pad ags_21 to 8 digits
cw$ags_21 <- ifelse(nchar(cw$ags_21) == 7, paste0("0", cw$ags_21), cw$ags_21)
cw_ags21 <- unique(cw$ags_21)

# Check how many harm AGS are in the crosswalk's ags_21 values
# (post-2020 identity-mapped AGS may not appear as ags_21 targets)
df_pre2021 <- df |> filter(election_year < 2021)
n_in_cw <- sum(unique(df_pre2021$ags) %in% cw_ags21)
n_not_in_cw <- sum(!unique(df_pre2021$ags) %in% cw_ags21)

cat(sprintf("\nPre-2021 harm AGS codes in crosswalk ags_21: %d of %d (%.1f%%)\n",
            n_in_cw, n_in_cw + n_not_in_cw,
            100 * n_in_cw / (n_in_cw + n_not_in_cw)))

if (n_not_in_cw > 0) {
  cat(sprintf("!! WARNING: %d pre-2021 harm AGS not found in crosswalk ags_21\n",
              n_not_in_cw))
  not_in_cw <- df_pre2021 |>
    filter(!ags %in% cw_ags21) |>
    distinct(ags, ags_name, state_name)
  print(head(not_in_cw, 20))
} else {
  cat(">> PASS: All pre-2021 harm AGS are valid ags_21 codes\n")
}


# Check 4: Round distribution ---------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 4: ROUND DISTRIBUTION (HAUPTWAHL vs STICHWAHL)\n")
cat(strrep("=", 70), "\n\n")

round_by_state <- df |>
  group_by(state_name, round) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(state_name, round)

cat("Round distribution by state:\n")
print(round_by_state, n = 30)

# Check that round column only has valid values
invalid_round <- df |> filter(!round %in% c("hauptwahl", "stichwahl"))
if (nrow(invalid_round) > 0) {
  cat(sprintf("\n!! FAIL: %d rows with invalid round value\n", nrow(invalid_round)))
} else {
  cat("\n>> PASS: All round values are 'hauptwahl' or 'stichwahl'\n")
}


# Check 5: No duplicate (ags, election_date, round) ---------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 5: DUPLICATE DETECTION\n")
cat(strrep("=", 70), "\n\n")

dupes <- df |>
  group_by(ags, election_date, round) |>
  filter(n() > 1)

if (nrow(dupes) > 0) {
  cat(sprintf("!! FAIL: %d rows in duplicate (ags, election_date, round) groups\n",
              nrow(dupes)))
  cat("Examples:\n")
  print(head(dupes |>
               select(ags, ags_name, election_date, election_type, round, winner_party) |>
               arrange(ags, election_date), 20))
} else {
  cat(">> PASS: No duplicate (ags, election_date, round) rows\n")
}


# Check 6: Vote count consistency ----------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 6: VOTE COUNT CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

# valid_votes + invalid_votes ≈ number_voters
df_vote_check <- df |>
  filter(!is.na(valid_votes) & !is.na(invalid_votes) & !is.na(number_voters)) |>
  mutate(
    vote_sum = valid_votes + invalid_votes,
    vote_diff = abs(vote_sum - number_voters)
  )

cat(sprintf("Rows with all vote counts non-NA: %d\n", nrow(df_vote_check)))

# Allow tolerance of 2 for rounding in harmonization
n_vote_inconsistent <- sum(df_vote_check$vote_diff > 2)

if (n_vote_inconsistent > 0) {
  cat(sprintf("!! WARNING: %d rows where |valid + invalid - number_voters| > 2\n",
              n_vote_inconsistent))
  cat("Distribution of differences:\n")
  print(quantile(df_vote_check$vote_diff[df_vote_check$vote_diff > 2],
                 c(0.5, 0.9, 0.99, 1), na.rm = TRUE))
  cat("Top 10 by difference:\n")
  print(head(df_vote_check |>
               filter(vote_diff > 2) |>
               select(ags, ags_name, election_date, valid_votes, invalid_votes,
                      number_voters, vote_sum, vote_diff, n_predecessors) |>
               arrange(desc(vote_diff)), 10))
} else {
  cat(">> PASS: valid_votes + invalid_votes ≈ number_voters (within tolerance 2)\n")
}

# winner_votes <= valid_votes
df_winner_check <- df |>
  filter(!is.na(winner_votes) & !is.na(valid_votes))

n_winner_exceeds <- sum(df_winner_check$winner_votes > df_winner_check$valid_votes + 1)

if (n_winner_exceeds > 0) {
  cat(sprintf("\n!! WARNING: %d rows where winner_votes > valid_votes + 1\n",
              n_winner_exceeds))
  print(head(df_winner_check |>
               filter(winner_votes > valid_votes + 1) |>
               select(ags, ags_name, election_date, winner_votes, valid_votes,
                      n_predecessors), 10))
} else {
  cat(">> PASS: winner_votes <= valid_votes (within tolerance 1)\n")
}

# number_voters <= eligible_voters
df_voters_check <- df |>
  filter(!is.na(number_voters) & !is.na(eligible_voters))

n_voters_exceeds <- sum(df_voters_check$number_voters > df_voters_check$eligible_voters + 1)

if (n_voters_exceeds > 0) {
  cat(sprintf("!! WARNING: %d rows where number_voters > eligible_voters + 1\n",
              n_voters_exceeds))
  print(head(df_voters_check |>
               filter(number_voters > eligible_voters + 1) |>
               select(ags, ags_name, election_date, number_voters, eligible_voters,
                      n_predecessors) |>
               arrange(desc(number_voters - eligible_voters)), 10))
} else {
  cat(">> PASS: number_voters <= eligible_voters (within tolerance 1)\n")
}


# Check 7: Turnout and voteshare range ----------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 7: TURNOUT AND VOTESHARE RANGE\n")
cat(strrep("=", 70), "\n\n")

df_turnout <- df |> filter(!is.na(turnout))

cat(sprintf("Rows with non-NA turnout: %d (%.1f%% of total)\n",
            nrow(df_turnout), 100 * nrow(df_turnout) / nrow(df)))
cat("Turnout distribution:\n")
print(quantile(df_turnout$turnout, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

# Turnout > 1 (should be 0 after capping)
n_over1 <- sum(df_turnout$turnout > 1)
if (n_over1 > 0) {
  cat(sprintf("!! FAIL: %d rows with turnout > 1 (should have been capped)\n", n_over1))
} else {
  cat(">> PASS: No turnout > 1\n")
}

# Turnout = 0
n_zero <- sum(df_turnout$turnout == 0)
if (n_zero > 0) {
  cat(sprintf("!! WARNING: %d rows with turnout = 0\n", n_zero))
} else {
  cat(">> PASS: No turnout = 0\n")
}

# Winner voteshare range
df_ws <- df |> filter(!is.na(winner_voteshare))
cat(sprintf("\nRows with non-NA winner_voteshare: %d (%.1f%% of total)\n",
            nrow(df_ws), 100 * nrow(df_ws) / nrow(df)))
cat("Distribution:\n")
print(quantile(df_ws$winner_voteshare, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

n_ws_over1 <- sum(df_ws$winner_voteshare > 1)
if (n_ws_over1 > 0) {
  cat(sprintf("!! FAIL: %d rows with winner_voteshare > 1\n", n_ws_over1))
} else {
  cat(">> PASS: All winner_voteshare <= 1\n")
}

n_ws_zero <- sum(df_ws$winner_voteshare <= 0)
if (n_ws_zero > 0) {
  cat(sprintf("!! WARNING: %d rows with winner_voteshare <= 0\n", n_ws_zero))
} else {
  cat(">> PASS: All winner_voteshare > 0\n")
}


# Check 8: Flag consistency ----------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 8: FLAG CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

flag_cols <- c("flag_unsuccessful_naive_merge", "flag_pre_1990",
               "flag_aggregated", "flag_turnout_above_1",
               "flag_voteshare_above_1", "flag_pct_only")

for (fc in flag_cols) {
  vals <- df[[fc]]
  n_1 <- sum(vals == 1, na.rm = TRUE)
  n_0 <- sum(vals == 0, na.rm = TRUE)
  n_na <- sum(is.na(vals))
  cat(sprintf("  %s: %d flagged, %d clean, %d NA\n", fc, n_1, n_0, n_na))
}

# flag_pre_1990: should only apply to Bayern
pre90_non_by <- df |> filter(flag_pre_1990 == 1, state != "09")
if (nrow(pre90_non_by) > 0) {
  cat("\n!! FAIL: flag_pre_1990 = 1 for non-Bayern rows\n")
  print(pre90_non_by |> count(state_name))
} else {
  cat("\n>> PASS: flag_pre_1990 only applies to Bayern\n")
}

# flag_pct_only: should mainly apply to RLP; also applies to old Bayern
# records from 1940s-50s where count data was not recorded
pct_by_state <- df |> filter(flag_pct_only == 1) |> count(state_name)
cat("   flag_pct_only by state:\n")
print(pct_by_state)

# All RLP rows should be flagged
rlp_all_pct <- all(df |> filter(state == "07") |> pull(flag_pct_only) == 1)
if (rlp_all_pct) {
  cat(">> PASS: All RLP rows are flag_pct_only = 1\n")
} else {
  cat("!! FAIL: Not all RLP rows have flag_pct_only = 1\n")
}

# Non-RLP/non-Bayern should not be flagged
pct_other <- df |> filter(flag_pct_only == 1, !state %in% c("07", "09"))
if (nrow(pct_other) > 0) {
  cat("!! FAIL: flag_pct_only = 1 for states other than RLP/Bayern\n")
  print(pct_other |> count(state_name))
} else {
  cat(">> PASS: flag_pct_only only in RLP (structural) and Bayern (old records)\n")
}

# flag_aggregated: n_predecessors should be > 1
agg_check <- df |> filter(flag_aggregated == 1)
if (all(agg_check$n_predecessors > 1)) {
  cat(">> PASS: All flag_aggregated=1 rows have n_predecessors > 1\n")
} else {
  cat("!! FAIL: Some flag_aggregated=1 rows have n_predecessors <= 1\n")
}

# Non-aggregated rows should have n_predecessors == 1
non_agg_check <- df |> filter(flag_aggregated == 0)
if (all(non_agg_check$n_predecessors == 1)) {
  cat(">> PASS: All flag_aggregated=0 rows have n_predecessors == 1\n")
} else {
  cat("!! WARNING: Some flag_aggregated=0 rows have n_predecessors != 1\n")
}

# flag_turnout_above_1 and flag_voteshare_above_1 should be 0
# (values are capped at 1 in the script)
if (all(df$flag_turnout_above_1 == 0, na.rm = TRUE)) {
  cat(">> PASS: No flag_turnout_above_1 triggered (clean data)\n")
} else {
  n_t1 <- sum(df$flag_turnout_above_1 == 1, na.rm = TRUE)
  cat(sprintf("   INFO: %d rows had turnout > 1 before capping\n", n_t1))
}


# Check 9: Missing data patterns by state --------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 9: MISSING DATA PATTERNS BY STATE\n")
cat(strrep("=", 70), "\n\n")

vote_cols <- c("eligible_voters", "number_voters", "valid_votes",
               "invalid_votes", "turnout", "winner_votes", "winner_voteshare",
               "winner_party")

for (st in sort(unique(df$state_name))) {
  df_st <- df |> filter(state_name == st)
  cat(sprintf("--- %s (%d rows) ---\n", st, nrow(df_st)))

  na_counts <- df_st |>
    summarise(across(all_of(vote_cols), ~ sum(is.na(.)))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "n_na") |>
    mutate(pct_na = round(100 * n_na / nrow(df_st), 1)) |>
    filter(n_na > 0)

  if (nrow(na_counts) > 0) {
    print(na_counts)
  } else {
    cat("  No missing values in vote columns\n")
  }
  cat("\n")
}

# RLP-specific: all count columns should be NA
df_rlp <- df |> filter(state == "07")
rlp_count_cols <- c("eligible_voters", "number_voters", "valid_votes",
                     "invalid_votes", "winner_votes")
rlp_has_counts <- df_rlp |>
  summarise(across(all_of(rlp_count_cols), ~ sum(!is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_nonNA") |>
  filter(n_nonNA > 0)

if (nrow(rlp_has_counts) > 0) {
  cat("!! FAIL: RLP has non-NA count columns (expected all NA):\n")
  print(rlp_has_counts)
} else {
  cat(">> PASS: RLP has all count columns as NA (percentages only)\n")
}


# Check 10: Aggregation quality -------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 10: AGGREGATION QUALITY\n")
cat(strrep("=", 70), "\n\n")

agg <- df |> filter(flag_aggregated == 1)
cat(sprintf("Total aggregated rows: %d\n", nrow(agg)))
cat("By state:\n")
print(agg |> count(state_name) |> arrange(desc(n)))

cat("\nPredecessor count distribution:\n")
print(table(agg$n_predecessors))

# Spot-check: aggregated rows should have reasonable eligible_voters
# (should be sum of predecessors, not a single municipality's value)
cat("\nLargest aggregated municipalities:\n")
print(agg |>
        filter(!is.na(eligible_voters)) |>
        arrange(desc(eligible_voters)) |>
        select(ags, ags_name, election_date, eligible_voters,
               winner_party, n_predecessors) |>
        head(10))

# Check that turnout was recomputed properly for aggregated rows
agg_turnout_check <- agg |>
  filter(!is.na(number_voters) & !is.na(eligible_voters) & eligible_voters > 0) |>
  mutate(
    expected_turnout = number_voters / eligible_voters,
    turnout_diff = abs(turnout - expected_turnout)
  )

if (nrow(agg_turnout_check) > 0) {
  n_turnout_mismatch <- sum(agg_turnout_check$turnout_diff > 0.001, na.rm = TRUE)
  if (n_turnout_mismatch > 0) {
    cat(sprintf("\n!! WARNING: %d aggregated rows where turnout != number_voters/eligible_voters\n",
                n_turnout_mismatch))
  } else {
    cat("\n>> PASS: All aggregated row turnouts match number_voters/eligible_voters\n")
  }
}

# Check that voteshare was recomputed properly
agg_vs_check <- agg |>
  filter(!is.na(winner_votes) & !is.na(valid_votes) & valid_votes > 0) |>
  mutate(
    expected_vs = winner_votes / valid_votes,
    vs_diff = abs(winner_voteshare - expected_vs)
  )

if (nrow(agg_vs_check) > 0) {
  n_vs_mismatch <- sum(agg_vs_check$vs_diff > 0.001, na.rm = TRUE)
  if (n_vs_mismatch > 0) {
    cat(sprintf("!! WARNING: %d aggregated rows where voteshare != winner_votes/valid_votes\n",
                n_vs_mismatch))
  } else {
    cat(">> PASS: All aggregated row voteshares match winner_votes/valid_votes\n")
  }
}


# Check 11: Known entity spot-checks ------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 11: KNOWN ENTITY SPOT-CHECKS\n")
cat(strrep("=", 70), "\n\n")

spot_checks <- tribble(
  ~entity,      ~ags,        ~min_ev,  ~min_rows,
  "München",    "09162000",  500000,   5,
  "Nürnberg",   "09564000",  200000,   5,
  "Köln",       "05315000",  500000,   2,
  "Düsseldorf", "05111000",  300000,   2,
  "Dresden",    "14612000",  300000,   2,
  "Leipzig",    "14713000",  300000,   2,
  "Chemnitz",   "14511000",  100000,   2,
  "Kiel",       "01002000",  100000,   1,
)

for (i in seq_len(nrow(spot_checks))) {
  sc <- spot_checks[i, ]
  matches <- df |> filter(ags == sc$ags)

  if (nrow(matches) == 0) {
    cat(sprintf("!! WARNING: %s (%s) not found in harm data\n", sc$entity, sc$ags))
  } else {
    max_ev <- max(matches$eligible_voters, na.rm = TRUE)
    ev_ok <- !is.infinite(max_ev) && max_ev >= sc$min_ev
    rows_ok <- nrow(matches) >= sc$min_rows
    cat(sprintf(">> %s: %s — %d rows, max EV = %s %s\n",
                ifelse(ev_ok & rows_ok, "PASS", "WARN"),
                sc$entity, nrow(matches),
                ifelse(is.infinite(max_ev), "NA", format(round(max_ev), big.mark = ",")),
                ifelse(!ev_ok, sprintf("(expected >= %s)",
                                        format(sc$min_ev, big.mark = ",")),
                       ifelse(!rows_ok, sprintf("(expected >= %d rows)", sc$min_rows),
                              ""))))
  }
}


# Check 12: Comparison with unharm data ----------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 12: HARM vs UNHARM COMPARISON\n")
cat(strrep("=", 70), "\n\n")

# For non-aggregated, non-pre-1990, non-split rows, the data should be identical
# to unharm (after filtering to BM/OB types and post-2020 rows)
df_simple <- df |>
  filter(flag_aggregated == 0,
         flag_pre_1990 == 0,
         n_predecessors == 1,
         election_year >= 1990,
         election_year >= 2021)

df_unharm_post20 <- df_unharm |>
  filter(election_type %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl"),
         election_year >= 2021)

# Join on (ags, election_date) and compare
if (nrow(df_simple) > 0 & nrow(df_unharm_post20) > 0) {
  # Deduplicate unharm: keep row with highest winner_voteshare per (ags, election_date)
  # (handles rare cases like Seukendorf where unharm has both winner + runner-up rows)
  unharm_dedup <- df_unharm_post20 |>
    group_by(ags, election_date) |>
    slice_max(winner_voteshare, n = 1, with_ties = FALSE) |>
    ungroup()

  comparison <- df_simple |>
    inner_join(unharm_dedup |>
                 select(ags, election_date, eligible_voters, winner_party,
                        winner_voteshare, turnout) |>
                 rename(ev_unharm = eligible_voters, wp_unharm = winner_party,
                        ws_unharm = winner_voteshare, to_unharm = turnout),
               by = c("ags", "election_date"))

  cat(sprintf("Matched post-2020 identity-mapped rows: %d\n", nrow(comparison)))

  # Check eligible_voters match
  ev_mismatch <- comparison |>
    filter(!is.na(eligible_voters) & !is.na(ev_unharm)) |>
    filter(abs(eligible_voters - ev_unharm) > 0)

  if (nrow(ev_mismatch) > 0) {
    cat(sprintf("!! FAIL: %d rows where harm eligible_voters != unharm eligible_voters\n",
                nrow(ev_mismatch)))
  } else {
    cat(">> PASS: eligible_voters match for identity-mapped rows\n")
  }

  # Check winner_party match
  wp_mismatch <- comparison |>
    filter(!is.na(winner_party) & !is.na(wp_unharm)) |>
    filter(winner_party != wp_unharm)

  if (nrow(wp_mismatch) > 0) {
    cat(sprintf("!! FAIL: %d rows where harm winner_party != unharm winner_party\n",
                nrow(wp_mismatch)))
    print(head(wp_mismatch |> select(ags, election_date, winner_party, wp_unharm), 10))
  } else {
    cat(">> PASS: winner_party matches for identity-mapped rows\n")
  }

  # Check winner_voteshare match (within tolerance for recomputed)
  ws_mismatch <- comparison |>
    filter(!is.na(winner_voteshare) & !is.na(ws_unharm)) |>
    filter(abs(winner_voteshare - ws_unharm) > 0.001)

  if (nrow(ws_mismatch) > 0) {
    cat(sprintf("!! WARNING: %d rows where harm winner_voteshare != unharm (diff > 0.001)\n",
                nrow(ws_mismatch)))
  } else {
    cat(">> PASS: winner_voteshare matches for identity-mapped rows (within 0.001)\n")
  }
} else {
  cat("   SKIP: Not enough rows for comparison\n")
}

# Check that all 7 states are represented (compare by state code, not name,
# since unharm uses German names and harm uses English names via state_id_to_names)
harm_states <- sort(unique(df$state))
unharm_states <- sort(unique(df_unharm |>
  filter(election_type %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl")) |>
  pull(state)))

missing_states <- setdiff(unharm_states, harm_states)
if (length(missing_states) > 0) {
  cat(sprintf("\n!! FAIL: State codes in unharm but not in harm: %s\n",
              paste(missing_states, collapse = ", ")))
} else {
  cat(sprintf("\n>> PASS: All %d state codes in unharm are represented in harm\n",
              length(unharm_states)))
}


# Check 13: Election date integrity --------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 13: ELECTION DATE INTEGRITY\n")
cat(strrep("=", 70), "\n\n")

n_na_date <- sum(is.na(df$election_date))
if (n_na_date > 0) {
  cat(sprintf("!! FAIL: %d rows with NA election_date\n", n_na_date))
} else {
  cat(">> PASS: No NA election_date values\n")
}

# Year match
df_year_check <- df |>
  filter(!is.na(election_date)) |>
  mutate(date_year = year(election_date),
         year_match = election_year == date_year)

n_year_mismatch <- sum(!df_year_check$year_match)

if (n_year_mismatch > 0) {
  cat(sprintf("   INFO: %d rows where election_year != year(election_date)\n",
              n_year_mismatch))
  cat("   (Expected for Sachsen year-boundary runoffs)\n")
  cat("   By state:\n")
  print(df_year_check |> filter(!year_match) |> count(state_name))
} else {
  cat(">> PASS: election_year matches year(election_date)\n")
}

# Date range plausibility
cat(sprintf("Date range: %s to %s\n", min(df$election_date, na.rm = TRUE),
            max(df$election_date, na.rm = TRUE)))

min_year <- year(min(df$election_date, na.rm = TRUE))
max_year <- year(max(df$election_date, na.rm = TRUE))
if (min_year < 1945 | max_year > 2026) {
  cat(sprintf("!! WARNING: Dates outside 1945-2026 range\n"))
} else {
  cat(">> PASS: All dates in plausible range\n")
}


# Summary ----------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat(sprintf("Harmonized mayoral elections dataset: %d rows, %d cols, %d states\n",
            nrow(df), ncol(df), n_distinct(df$state_name)))
cat(sprintf("Unique AGS (2021 boundaries): %d\n", n_distinct(df$ags)))
cat(sprintf("States: %s\n", paste(sort(unique(df$state_name)), collapse = ", ")))
cat(sprintf("Year range: %d to %d\n",
            min(df$election_year), max(df$election_year)))
cat(sprintf("\nFlags: %d pre-1990, %d aggregated, %d pct-only, %d unsuccessful merge\n",
            sum(df$flag_pre_1990), sum(df$flag_aggregated),
            sum(df$flag_pct_only), sum(df$flag_unsuccessful_naive_merge)))

cat("\nReview check output above for specific PASS/WARNING/FAIL results.\n\n")

cat("=", strrep("=", 70), "\n")
cat("END OF CHECKS\n")
cat("=", strrep("=", 70), "\n")
