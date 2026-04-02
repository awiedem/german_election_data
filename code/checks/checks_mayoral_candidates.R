### Sanity checks for mayoral elections candidate-level data
# Date: 2026
# Companion to checks_mayoral_unharm.R — validates the candidate-level dataset
# produced by 01b_mayoral_candidates.R
# NOTE: Dataset is in wide format — one row per candidate per election cycle,
# with _hw and _sw column suffixes for Hauptwahl and Stichwahl results.

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
cat("MAYORAL ELECTIONS CANDIDATE-LEVEL: DATA QUALITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load datasets ----------------------------------------------------------------

cat("Loading datasets...\n")

df <- read_rds("data/mayoral_elections/final/mayoral_candidates.rds") |>
  as_tibble()

cat("Loaded mayoral_candidates:", nrow(df), "rows x", ncol(df), "cols\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n\n")

# Also load unharm for cross-validation
unharm <- read_rds("data/mayoral_elections/final/mayoral_unharm.rds") |>
  as_tibble()
cat("Loaded mayoral_unharm:", nrow(unharm), "rows for cross-validation\n\n")

# ==============================================================================
# CHECK 1: COVERAGE BY STATE AND YEAR
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("CHECK 1: COVERAGE BY STATE AND YEAR\n")
cat(strrep("=", 70), "\n\n")

coverage <- df |>
  group_by(state_name, state) |>
  summarise(
    n_rows = n(),
    n_elections = n_distinct(paste(ags, election_date)),
    min_year = min(election_year, na.rm = TRUE),
    max_year = max(election_year, na.rm = TRUE),
    n_years = n_distinct(election_year),
    avg_candidates = round(n() / n_distinct(paste(ags, election_date)), 1),
    n_with_sw = sum(has_stichwahl),
    pct_with_sw = round(100 * sum(has_stichwahl) / n(), 1),
    .groups = "drop"
  ) |>
  arrange(desc(n_rows))

cat("Coverage by state:\n")
print(coverage, n = 20)

# Expected row counts (approximate — candidate-level)
expected <- tibble(
  state_name = c("Bayern", "Sachsen", "Rheinland-Pfalz",
                 "Nordrhein-Westfalen", "Niedersachsen", "Saarland",
                 "Schleswig-Holstein"),
  expected_min_rows = c(50000, 5000, 2000, 4000, 2000, 100, 50),
  expected_min_year = c(1945, 2001, 1994, 2009, 2006, 2019, 2023),
  expected_max_year = c(2025, 2024, 2024, 2025, 2025, 2025, 2025)
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
  cat("\n!! FAIL: Coverage issues detected:\n")
  print(issues |> select(state_name, n_rows, expected_min_rows, min_year,
                          expected_min_year, max_year, expected_max_year))
} else {
  cat("\n>> PASS: All states have expected coverage\n")
}

# ==============================================================================
# CHECK 2: AGS VALIDITY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 2: AGS VALIDITY\n")
cat(strrep("=", 70), "\n\n")

# All AGS should be 8 characters
ags_lengths <- nchar(df$ags)
non_8 <- sum(ags_lengths != 8, na.rm = TRUE)
na_ags <- sum(is.na(df$ags))

cat("AGS length distribution:\n")
print(table(ags_lengths, useNA = "ifany"))

if (na_ags > 0) {
  cat(sprintf("\n!! FAIL: %d rows with NA AGS\n", na_ags))
  cat("   By state:\n")
  print(df |> filter(is.na(ags)) |> count(state_name) |> arrange(desc(n)))
} else {
  cat("\n>> PASS: No NA AGS values\n")
}

if (non_8 > 0) {
  cat(sprintf("!! FAIL: %d rows with AGS not 8 digits\n", non_8))
  print(head(df |> filter(nchar(ags) != 8) |>
               select(ags, state_name, ags_name), 10))
} else {
  cat(">> PASS: All AGS are 8 characters\n")
}

# State prefix matches state_name
state_prefix_map <- tibble(
  state = c("01", "03", "05", "07", "09", "10", "14"),
  state_name = c("Schleswig-Holstein", "Niedersachsen", "Nordrhein-Westfalen",
                 "Rheinland-Pfalz", "Bayern", "Saarland", "Sachsen")
)

df_prefix <- df |>
  mutate(ags_prefix = substr(ags, 1, 2))

prefix_mismatch <- df_prefix |>
  filter(ags_prefix != state)

if (nrow(prefix_mismatch) > 0) {
  cat(sprintf("\n!! FAIL: %d rows where AGS prefix != state code\n",
              nrow(prefix_mismatch)))
  print(head(prefix_mismatch |> select(ags, state, state_name, ags_prefix), 10))
} else {
  cat(">> PASS: All AGS prefixes match state codes\n")
}

# ==============================================================================
# CHECK 3: DUPLICATE DETECTION
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 3: DUPLICATE DETECTION\n")
cat(strrep("=", 70), "\n\n")

# Exact duplicates within (ags, election_date, candidate_party, candidate_votes_hw)
dupes_exact <- df |>
  group_by(ags, election_date, candidate_party, candidate_votes_hw) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

if (nrow(dupes_exact) > 0) {
  n_duped_rows <- sum(dupes_exact$n) - nrow(dupes_exact)
  dupes_detail <- dupes_exact |>
    left_join(df |> distinct(ags, state_name), by = "ags")
  # Bayern has no candidate names, so multiple candidates from the same party
  # (e.g. "Parteilos") with the same vote count appear as "duplicates" here.
  n_bayern <- dupes_detail |> filter(state_name == "Bayern") |> nrow()
  n_other <- nrow(dupes_detail) - n_bayern
  cat(sprintf("   INFO: %d duplicate groups by (ags, date, party, votes_hw) — %d extra rows\n",
              nrow(dupes_exact), n_duped_rows))
  cat(sprintf("   Bayern (expected — no names, same-party same-vote ties): %d groups\n", n_bayern))
  if (n_other > 0) {
    cat(sprintf("!! WARNING: %d non-Bayern duplicate groups:\n", n_other))
    print(dupes_detail |> filter(state_name != "Bayern") |> count(state_name) |> arrange(desc(n)))
  } else {
    cat(">> PASS: All duplicates are expected Bayern same-party ties\n")
  }
} else {
  cat(">> PASS: No exact duplicates by (ags, date, party, votes_hw)\n")
}

# Duplicates within (ags, election_date, candidate_name) for states with names
df_named <- df |> filter(!is.na(candidate_name))
dupes_name <- df_named |>
  group_by(ags, election_date, candidate_name) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

if (nrow(dupes_name) > 0) {
  cat(sprintf("\n!! WARNING: %d duplicate groups by (ags, date, name)\n",
              nrow(dupes_name)))
  dupes_name_detail <- dupes_name |>
    left_join(df |> distinct(ags, state_name), by = "ags")
  cat("   By state:\n")
  print(dupes_name_detail |> count(state_name) |> arrange(desc(n)))
  cat("   Examples:\n")
  print(head(dupes_name, 10))
} else {
  cat(">> PASS: No duplicate candidate names within same election\n")
}

# ==============================================================================
# CHECK 4: VOTE COUNT CONSISTENCY (HAUPTWAHL)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 4: VOTE COUNT CONSISTENCY (HAUPTWAHL)\n")
cat(strrep("=", 70), "\n\n")

# valid_votes + invalid_votes ≈ number_voters (where all non-NA)
df_vote_check <- df |>
  filter(!is.na(valid_votes) & !is.na(invalid_votes) & !is.na(number_voters)) |>
  # Use one row per election (all candidates share the same stats)
  distinct(ags, election_date, valid_votes, invalid_votes, number_voters) |>
  mutate(
    vote_sum = valid_votes + invalid_votes,
    vote_diff = abs(vote_sum - number_voters)
  )

n_vote_inconsistent <- sum(df_vote_check$vote_diff > 1)

cat(sprintf("Elections with all three vote counts non-NA: %d\n", nrow(df_vote_check)))

if (n_vote_inconsistent > 0) {
  cat(sprintf("!! WARNING: %d elections where |valid + invalid - number_voters| > 1\n",
              n_vote_inconsistent))
  cat("   Distribution of differences:\n")
  print(quantile(df_vote_check$vote_diff[df_vote_check$vote_diff > 1],
                 c(0.5, 0.9, 0.99, 1), na.rm = TRUE))
} else {
  cat(">> PASS: valid_votes + invalid_votes = number_voters (within tolerance)\n")
}

# number_voters <= eligible_voters
df_voters_check <- df |>
  filter(!is.na(number_voters) & !is.na(eligible_voters)) |>
  distinct(ags, election_date, number_voters, eligible_voters)

n_voters_exceeds <- sum(df_voters_check$number_voters > df_voters_check$eligible_voters)

if (n_voters_exceeds > 0) {
  cat(sprintf("\n!! WARNING: %d elections where number_voters > eligible_voters\n",
              n_voters_exceeds))
  print(head(df_voters_check |>
               filter(number_voters > eligible_voters) |>
               arrange(desc(number_voters - eligible_voters)), 10))
} else {
  cat(">> PASS: number_voters <= eligible_voters for all elections\n")
}

# ==============================================================================
# CHECK 5: CANDIDATE VOTE AGGREGATION vs VALID VOTES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 5: CANDIDATE VOTE AGGREGATION vs VALID VOTES (HAUPTWAHL)\n")
cat(strrep("=", 70), "\n\n")

# Sum candidate_votes_hw per election and compare to valid_votes
vote_agg <- df |>
  filter(!is.na(candidate_votes_hw) & !is.na(valid_votes)) |>
  group_by(ags, election_date, state_name, valid_votes) |>
  summarise(
    sum_candidate_votes = sum(candidate_votes_hw, na.rm = TRUE),
    n_candidates = n(),
    .groups = "drop"
  ) |>
  mutate(
    ratio = ifelse(valid_votes > 0, sum_candidate_votes / valid_votes, NA)
  )

cat(sprintf("Elections with candidate votes and valid_votes: %d\n", nrow(vote_agg)))
cat("Ratio distribution (sum_candidate_votes_hw / valid_votes):\n")
print(quantile(vote_agg$ratio, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
               na.rm = TRUE))

# Flag if > 105% or < 50%
n_over <- sum(vote_agg$ratio > 1.05, na.rm = TRUE)
n_under <- sum(vote_agg$ratio < 0.50, na.rm = TRUE)

if (n_over > 0) {
  cat(sprintf("\n!! WARNING: %d elections where sum(candidate_votes_hw) > 105%% of valid_votes\n",
              n_over))
  cat("   By state:\n")
  print(vote_agg |> filter(ratio > 1.05) |> count(state_name) |> arrange(desc(n)))
  cat("   Worst examples:\n")
  print(head(vote_agg |> filter(ratio > 1.05) |>
               select(ags, state_name, election_date, valid_votes,
                      sum_candidate_votes, ratio) |>
               arrange(desc(ratio)), 10))
} else {
  cat("\n>> PASS: No elections with candidate votes > 105% of valid_votes\n")
}

if (n_under > 0) {
  under_by_state <- vote_agg |> filter(ratio < 0.50) |> count(state_name) |> arrange(desc(n))
  cat(sprintf("\n   INFO: %d elections where sum(candidate_votes_hw) < 50%% of valid_votes\n",
              n_under))
  cat("   (Expected for NS/BY: PDFs may list only top candidates, not all)\n")
  cat("   By state:\n")
  print(under_by_state)
} else {
  cat(">> PASS: No elections with candidate votes < 50% of valid_votes\n")
}

# ==============================================================================
# CHECK 6: TURNOUT RANGE
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 6: TURNOUT RANGE\n")
cat(strrep("=", 70), "\n\n")

df_turnout <- df |>
  filter(!is.na(turnout)) |>
  distinct(ags, election_date, turnout, state_name)

cat(sprintf("Elections with non-NA turnout: %d\n", nrow(df_turnout)))
cat("Turnout distribution (Hauptwahl):\n")
print(quantile(df_turnout$turnout, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

# Flag turnout = 0
n_zero <- sum(df_turnout$turnout == 0)
if (n_zero > 0) {
  cat(sprintf("\n!! WARNING: %d elections with turnout = 0\n", n_zero))
} else {
  cat("\n>> PASS: No turnout = 0\n")
}

# Flag turnout > 1
n_over1 <- sum(df_turnout$turnout > 1)
if (n_over1 > 0) {
  cat(sprintf("!! WARNING: %d elections with turnout > 1\n", n_over1))
  cat("   By state:\n")
  print(df_turnout |> filter(turnout > 1) |> count(state_name) |> arrange(desc(n)))
} else {
  cat(">> PASS: No turnout > 1\n")
}

# Stichwahl turnout
df_turnout_sw <- df |>
  filter(!is.na(turnout_sw)) |>
  distinct(ags, election_date, turnout_sw, state_name)

cat(sprintf("\nElections with non-NA turnout_sw: %d\n", nrow(df_turnout_sw)))
if (nrow(df_turnout_sw) > 0) {
  cat("Turnout distribution (Stichwahl):\n")
  print(quantile(df_turnout_sw$turnout_sw, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

  n_sw_over1 <- sum(df_turnout_sw$turnout_sw > 1)
  if (n_sw_over1 > 0) {
    cat(sprintf("!! WARNING: %d elections with turnout_sw > 1\n", n_sw_over1))
  } else {
    cat(">> PASS: No turnout_sw > 1\n")
  }
}

# Very low turnout
n_verylow <- sum(df_turnout$turnout < 0.15 & df_turnout$turnout > 0)
if (n_verylow > 0) {
  cat(sprintf("\n   INFO: %d elections with turnout in (0, 0.15) — unusual but possible\n",
              n_verylow))
}

# ==============================================================================
# CHECK 7: CANDIDATE VOTESHARE VALIDITY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 7: CANDIDATE VOTESHARE VALIDITY\n")
cat(strrep("=", 70), "\n\n")

# Hauptwahl voteshare
df_vs_hw <- df |> filter(!is.na(candidate_voteshare_hw))

cat(sprintf("Rows with non-NA candidate_voteshare_hw: %d (%.1f%% of total)\n",
            nrow(df_vs_hw), 100 * nrow(df_vs_hw) / nrow(df)))
cat("Distribution (HW):\n")
print(quantile(df_vs_hw$candidate_voteshare_hw,
               c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

n_neg <- sum(df_vs_hw$candidate_voteshare_hw < 0)
if (n_neg > 0) {
  cat(sprintf("\n!! FAIL: %d rows with candidate_voteshare_hw < 0\n", n_neg))
} else {
  cat("\n>> PASS: All candidate_voteshare_hw >= 0\n")
}

n_gt1 <- sum(df_vs_hw$candidate_voteshare_hw > 1)
if (n_gt1 > 0) {
  cat(sprintf("!! WARNING: %d rows with candidate_voteshare_hw > 1\n", n_gt1))
  cat("   By state:\n")
  print(df_vs_hw |> filter(candidate_voteshare_hw > 1) |>
          count(state_name) |> arrange(desc(n)))
} else {
  cat(">> PASS: All candidate_voteshare_hw <= 1\n")
}

# Stichwahl voteshare
df_vs_sw <- df |> filter(!is.na(candidate_voteshare_sw))
cat(sprintf("\nRows with non-NA candidate_voteshare_sw: %d (%.1f%% of total)\n",
            nrow(df_vs_sw), 100 * nrow(df_vs_sw) / nrow(df)))
if (nrow(df_vs_sw) > 0) {
  cat("Distribution (SW):\n")
  print(quantile(df_vs_sw$candidate_voteshare_sw,
                 c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

  n_sw_gt1 <- sum(df_vs_sw$candidate_voteshare_sw > 1)
  if (n_sw_gt1 > 0) {
    cat(sprintf("!! WARNING: %d rows with candidate_voteshare_sw > 1\n", n_sw_gt1))
  } else {
    cat(">> PASS: All candidate_voteshare_sw <= 1\n")
  }
}

# ==============================================================================
# CHECK 8: WINNER CONSISTENCY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 8: WINNER CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

# Exactly 1 is_winner = TRUE per election
winner_counts <- df |>
  group_by(ags, election_date) |>
  summarise(
    n_winners = sum(is_winner, na.rm = TRUE),
    n_candidates = n(),
    .groups = "drop"
  )

n_no_winner <- sum(winner_counts$n_winners == 0)
n_multi_winner <- sum(winner_counts$n_winners > 1)

cat(sprintf("Elections checked: %d\n", nrow(winner_counts)))

# Check no-winner elections
no_winner_detail <- winner_counts |>
  filter(n_winners == 0) |>
  left_join(
    df |>
      group_by(ags, election_date) |>
      summarise(all_votes_na = all(is.na(candidate_votes_hw)), .groups = "drop"),
    by = c("ags", "election_date")
  ) |>
  left_join(df |> distinct(ags, state_name), by = "ags")

n_no_winner_expected <- sum(no_winner_detail$all_votes_na, na.rm = TRUE)
n_no_winner_unexpected <- n_no_winner - n_no_winner_expected

cat(sprintf("No-winner elections: %d total\n", n_no_winner))
cat(sprintf("  Expected (all candidate_votes_hw NA — older records): %d\n",
            n_no_winner_expected))
if (n_no_winner_unexpected > 0) {
  cat(sprintf("!! WARNING: %d unexpected no-winner elections (with non-NA votes):\n",
              n_no_winner_unexpected))
  print(no_winner_detail |> filter(!all_votes_na) |>
          count(state_name) |> arrange(desc(n)))
} else {
  cat(">> PASS: All no-winner elections explained by NA vote data\n")
}

# Multi-winner: ties in votes
if (n_multi_winner > 0) {
  cat(sprintf("\n   INFO: %d elections with multiple winners (vote ties)\n", n_multi_winner))
  multi_detail <- winner_counts |>
    filter(n_winners > 1) |>
    left_join(df |> distinct(ags, state_name), by = "ags")
  cat("   By state:\n")
  print(multi_detail |> count(state_name) |> arrange(desc(n)))
} else {
  cat(">> PASS: No election has multiple winners\n")
}

# candidate_rank_hw == 1 ↔ is_winner (for elections WITHOUT Stichwahl)
rank_winner_mismatch <- df |>
  filter(!is.na(candidate_rank_hw), !has_stichwahl) |>
  mutate(
    rank1 = candidate_rank_hw == 1,
    mismatch = rank1 != is_winner
  )

n_mismatch <- sum(rank_winner_mismatch$mismatch, na.rm = TRUE)
if (n_mismatch > 0) {
  cat(sprintf("\n!! WARNING: %d rows where candidate_rank_hw == 1 ↔ is_winner mismatch (no-SW elections)\n",
              n_mismatch))
  print(head(rank_winner_mismatch |> filter(mismatch) |>
               select(ags, state_name, election_date, candidate_name,
                      candidate_rank_hw, is_winner, candidate_votes_hw), 10))
} else {
  cat(">> PASS: candidate_rank_hw == 1 ↔ is_winner consistent (for non-Stichwahl elections)\n")
}

# For Stichwahl elections: winner should be candidate_rank_sw == 1
rank_sw_winner <- df |>
  filter(!is.na(candidate_rank_sw), has_stichwahl) |>
  mutate(
    rank1_sw = candidate_rank_sw == 1,
    mismatch = rank1_sw != is_winner
  )

n_sw_mismatch <- sum(rank_sw_winner$mismatch, na.rm = TRUE)
if (n_sw_mismatch > 0) {
  cat(sprintf("!! WARNING: %d rows where candidate_rank_sw == 1 ↔ is_winner mismatch (SW elections)\n",
              n_sw_mismatch))
  print(head(rank_sw_winner |> filter(mismatch) |>
               select(ags, state_name, election_date, candidate_name,
                      candidate_rank_sw, is_winner, candidate_votes_sw), 10))
} else {
  cat(">> PASS: candidate_rank_sw == 1 ↔ is_winner consistent (for Stichwahl elections)\n")
}

# ==============================================================================
# CHECK 9: CROSS-VALIDATION WITH MAYORAL_UNHARM
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 9: CROSS-VALIDATION WITH MAYORAL_UNHARM\n")
cat(strrep("=", 70), "\n\n")

# Get winner rows from candidates dataset
cand_winners <- df |>
  filter(is_winner) |>
  select(ags, election_date, state_name,
         cand_winner_party = candidate_party,
         cand_winner_votes_hw = candidate_votes_hw)

# Match with unharm hauptwahl rows
unharm_hw <- unharm |>
  filter(round == "hauptwahl") |>
  select(ags, election_date,
         unharm_winner_party = winner_party,
         unharm_winner_votes = winner_votes)

# Count elections in each dataset
n_cand_elections <- n_distinct(paste(df$ags, df$election_date))
n_unharm_elections <- nrow(unharm)

cat(sprintf("Candidate dataset: %d unique elections\n", n_cand_elections))
cat(sprintf("Unharm dataset: %d rows\n", n_unharm_elections))

# Join
cross_val <- cand_winners |>
  left_join(unharm_hw, by = c("ags", "election_date"))

n_matched <- sum(!is.na(cross_val$unharm_winner_party) | !is.na(cross_val$unharm_winner_votes))
n_unmatched <- sum(is.na(cross_val$unharm_winner_party) & is.na(cross_val$unharm_winner_votes))

cat(sprintf("\nWinners matched to unharm (HW): %d\n", n_matched))
cat(sprintf("Winners not in unharm: %d\n", n_unmatched))

if (n_unmatched > 0) {
  cat("   Unmatched by state:\n")
  print(cross_val |> filter(is.na(unharm_winner_party) & is.na(unharm_winner_votes)) |>
          count(state_name) |> arrange(desc(n)))
}

# Winner party agreement
party_match <- cross_val |>
  filter(!is.na(unharm_winner_party) & !is.na(cand_winner_party)) |>
  mutate(party_agree = cand_winner_party == unharm_winner_party)

n_party_agree <- sum(party_match$party_agree)
n_party_disagree <- sum(!party_match$party_agree)

cat(sprintf("\nWinner party comparison (where both non-NA):\n"))
cat(sprintf("  Agree: %d (%.1f%%)\n", n_party_agree,
            100 * n_party_agree / nrow(party_match)))
cat(sprintf("  Disagree: %d (%.1f%%)\n", n_party_disagree,
            100 * n_party_disagree / nrow(party_match)))

if (n_party_disagree > 0) {
  cat("  Disagreement examples:\n")
  print(head(party_match |>
               filter(!party_agree) |>
               select(ags, state_name, election_date,
                      cand_winner_party, unharm_winner_party), 10))
}

# Winner votes agreement
vote_match <- cross_val |>
  filter(!is.na(unharm_winner_votes) & !is.na(cand_winner_votes_hw)) |>
  mutate(vote_diff = abs(cand_winner_votes_hw - unharm_winner_votes))

n_vote_agree <- sum(vote_match$vote_diff == 0)
n_vote_disagree <- sum(vote_match$vote_diff > 0)

cat(sprintf("\nWinner votes comparison (where both non-NA):\n"))
cat(sprintf("  Exact match: %d (%.1f%%)\n", n_vote_agree,
            100 * n_vote_agree / nrow(vote_match)))
cat(sprintf("  Differ: %d (%.1f%%)\n", n_vote_disagree,
            100 * n_vote_disagree / nrow(vote_match)))

if (n_vote_disagree > 0) {
  cat("  Distribution of differences:\n")
  print(quantile(vote_match$vote_diff[vote_match$vote_diff > 0],
                 c(0.5, 0.9, 0.99, 1), na.rm = TRUE))
}

# Check unharm elections missing from candidates
cand_all_elections <- df |>
  distinct(ags, election_date) |>
  mutate(in_candidates = TRUE)

# Also include SW dates from candidates
cand_sw_dates <- df |>
  filter(!is.na(election_date_sw)) |>
  distinct(ags, election_date = election_date_sw) |>
  mutate(in_candidates = TRUE)

cand_all_dates <- bind_rows(cand_all_elections, cand_sw_dates) |>
  distinct(ags, election_date, .keep_all = TRUE)

unharm_elections <- unharm |>
  select(ags, election_date, state_name, election_year) |>
  left_join(cand_all_dates, by = c("ags", "election_date"))

missing_from_cand <- unharm_elections |>
  filter(is.na(in_candidates))

cat(sprintf("\nUnharm elections missing from candidates dataset: %d\n",
            nrow(missing_from_cand)))
if (nrow(missing_from_cand) > 0) {
  cat("   By state:\n")
  print(missing_from_cand |> count(state_name) |> arrange(desc(n)))

  # Expected: pre-1970 Bayern elections without candidate data
  n_pre1970_bayern <- missing_from_cand |>
    filter(state_name == "Bayern" & election_year < 1970) |> nrow()
  cat(sprintf("   Pre-1970 Bayern (expected — no candidate data in source): %d\n",
              n_pre1970_bayern))

  n_truly_missing <- nrow(missing_from_cand) - n_pre1970_bayern
  if (n_truly_missing > 0) {
    cat(sprintf("   Other missing: %d\n", n_truly_missing))
    print(missing_from_cand |>
            filter(!(state_name == "Bayern" & election_year < 1970)) |>
            count(state_name) |> arrange(desc(n)))
  } else {
    cat(">> PASS: All missing elections explained by pre-1970 Bayern\n")
  }
}

# ==============================================================================
# CHECK 10: NAME QUALITY PER STATE
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 10: NAME QUALITY PER STATE\n")
cat(strrep("=", 70), "\n\n")

# Bayern: no names expected
df_by <- df |> filter(state_name == "Bayern")
n_by_names <- sum(!is.na(df_by$candidate_name))
cat(sprintf("Bayern: %d rows with candidate_name (expected: 0) — %s\n",
            n_by_names, ifelse(n_by_names == 0, "PASS", "UNEXPECTED")))

# NRW: should have names with comma (last, first format)
df_nrw <- df |> filter(state_name == "Nordrhein-Westfalen")
n_nrw_name <- sum(!is.na(df_nrw$candidate_name))
n_nrw_last <- sum(!is.na(df_nrw$candidate_last_name))
n_nrw_first <- sum(!is.na(df_nrw$candidate_first_name))
cat(sprintf("NRW: %d/%d with name, %d with last, %d with first\n",
            n_nrw_name, nrow(df_nrw), n_nrw_last, n_nrw_first))
nrw_comma <- sum(grepl(",", df_nrw$candidate_name[!is.na(df_nrw$candidate_name)]))
cat(sprintf("  Names containing comma: %d — parsed into last/first: %s\n",
            nrw_comma, ifelse(n_nrw_last > 0, "PASS", "FAIL")))

# Saarland: should have Vorname + Nachname + gender
df_sl <- df |> filter(state_name == "Saarland")
n_sl_name <- sum(!is.na(df_sl$candidate_name))
n_sl_gender <- sum(!is.na(df_sl$candidate_gender))
n_sl_last <- sum(!is.na(df_sl$candidate_last_name))
n_sl_first <- sum(!is.na(df_sl$candidate_first_name))
cat(sprintf("Saarland: %d/%d with name, %d last, %d first, %d gender\n",
            n_sl_name, nrow(df_sl), n_sl_last, n_sl_first, n_sl_gender))
cat(sprintf("  Gender coverage: %.1f%% — %s\n",
            100 * n_sl_gender / nrow(df_sl),
            ifelse(n_sl_gender / nrow(df_sl) > 0.9, "PASS", "WARNING")))

# Sachsen: EB/EV have names, party candidates don't
df_sn <- df |> filter(state_name == "Sachsen")
n_sn_name <- sum(!is.na(df_sn$candidate_name))
n_sn_party <- sum(!is.na(df_sn$candidate_party))
cat(sprintf("Sachsen: %d/%d with name (EB/EV only), %d with party\n",
            n_sn_name, nrow(df_sn), n_sn_party))
cat(sprintf("  Name rate: %.1f%% (expected ~40-50%% for independent candidates)\n",
            100 * n_sn_name / nrow(df_sn)))

# RLP: should have names + voteshare but candidate_votes = NA
df_rlp <- df |> filter(state_name == "Rheinland-Pfalz")
n_rlp_name <- sum(!is.na(df_rlp$candidate_name))
n_rlp_vs <- sum(!is.na(df_rlp$candidate_voteshare_hw))
n_rlp_votes <- sum(!is.na(df_rlp$candidate_votes_hw))
n_rlp_gender <- sum(!is.na(df_rlp$candidate_gender))
cat(sprintf("RLP: %d/%d with name, %d with voteshare_hw, %d with votes_hw, %d with gender\n",
            n_rlp_name, nrow(df_rlp), n_rlp_vs, n_rlp_votes, n_rlp_gender))
cat(sprintf("  candidate_votes_hw = NA expected for RLP (percentages only): %s\n",
            ifelse(n_rlp_votes == 0, "PASS", sprintf("WARNING — %d have votes", n_rlp_votes))))

# Niedersachsen: names + profession + birth year
df_ns <- df |> filter(state_name == "Niedersachsen")
n_ns_name <- sum(!is.na(df_ns$candidate_name))
n_ns_prof <- sum(!is.na(df_ns$candidate_profession))
n_ns_birth <- sum(!is.na(df_ns$candidate_birth_year))
cat(sprintf("NS: %d/%d with name, %d with profession, %d with birth_year\n",
            n_ns_name, nrow(df_ns), n_ns_prof, n_ns_birth))
cat(sprintf("  Name rate: %.1f%%, Profession rate: %.1f%%, Birth year rate: %.1f%%\n",
            100 * n_ns_name / nrow(df_ns),
            100 * n_ns_prof / nrow(df_ns),
            100 * n_ns_birth / nrow(df_ns)))

# ==============================================================================
# CHECK 11: BIRTH YEAR RANGE
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 11: BIRTH YEAR RANGE\n")
cat(strrep("=", 70), "\n\n")

df_birth <- df |> filter(!is.na(candidate_birth_year))

cat(sprintf("Rows with birth year: %d\n", nrow(df_birth)))
cat("Distribution:\n")
print(quantile(df_birth$candidate_birth_year, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

# Flag implausible
n_too_old <- sum(df_birth$candidate_birth_year < 1880)
n_too_young <- sum(df_birth$candidate_birth_year > 2010)

if (n_too_old > 0 | n_too_young > 0) {
  cat(sprintf("\n!! WARNING: %d birth years < 1880, %d > 2010 (implausible)\n",
              n_too_old, n_too_young))
  cat("   Examples:\n")
  print(head(df_birth |>
               filter(candidate_birth_year < 1880 | candidate_birth_year > 2010) |>
               select(ags, state_name, election_year, candidate_name,
                      candidate_birth_year), 10))
} else {
  cat("\n>> PASS: All birth years in plausible range (1880-2010)\n")
}

# ==============================================================================
# CHECK 12: ZERO-VOTE CANDIDATES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 12: ZERO-VOTE CANDIDATES\n")
cat(strrep("=", 70), "\n\n")

df_zero <- df |> filter(!is.na(candidate_votes_hw) & candidate_votes_hw == 0)

cat(sprintf("Candidates with 0 HW votes: %d (%.2f%% of rows with votes)\n",
            nrow(df_zero),
            100 * nrow(df_zero) / sum(!is.na(df$candidate_votes_hw))))
cat("By state:\n")
print(df_zero |> count(state_name) |> arrange(desc(n)))

# Flag if suspicious (> 5% of rows in a state)
zero_by_state <- df |>
  filter(!is.na(candidate_votes_hw)) |>
  group_by(state_name) |>
  summarise(
    n_total = n(),
    n_zero = sum(candidate_votes_hw == 0),
    pct_zero = round(100 * n_zero / n_total, 2),
    .groups = "drop"
  )

high_zero <- zero_by_state |> filter(pct_zero > 5)
if (nrow(high_zero) > 0) {
  cat("\n!! WARNING: States with > 5% zero-vote candidates:\n")
  print(high_zero)
} else {
  cat("\n>> PASS: No state has > 5% zero-vote candidates\n")
}

# ==============================================================================
# CHECK 13: SINGLE-CANDIDATE ELECTIONS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 13: SINGLE-CANDIDATE ELECTIONS\n")
cat(strrep("=", 70), "\n\n")

single_cand <- df |>
  group_by(ags, election_date, state_name) |>
  summarise(n_cand = n(), .groups = "drop") |>
  filter(n_cand == 1)

cat(sprintf("Total single-candidate elections: %d\n", nrow(single_cand)))
cat("By state:\n")
print(single_cand |> count(state_name) |> arrange(desc(n)))

# Bayern expected to dominate
n_by_single <- sum(single_cand$state_name == "Bayern")
cat(sprintf("\nBayern: %d single-candidate elections (%.1f%% of Bayern elections)\n",
            n_by_single,
            100 * n_by_single / n_distinct(paste(df_by$ags, df_by$election_date))))

# ==============================================================================
# CHECK 14: ELECTION TYPE DISTRIBUTION
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 14: ELECTION TYPE DISTRIBUTION\n")
cat(strrep("=", 70), "\n\n")

type_by_state <- df |>
  distinct(ags, election_date, state_name, election_type) |>
  group_by(state_name, election_type) |>
  summarise(n_elections = n(), .groups = "drop") |>
  arrange(state_name, desc(n_elections))

cat("Election types by state:\n")
print(type_by_state, n = 50)

# RLP should have 4 types
rlp_types <- type_by_state |> filter(state_name == "Rheinland-Pfalz") |>
  pull(election_type)
expected_rlp <- c("Bürgermeisterwahl", "Landratswahl", "Oberbürgermeisterwahl",
                  "VG-Bürgermeisterwahl")
rlp_ok <- all(expected_rlp %in% rlp_types)
cat(sprintf("\nRLP types: %s — %s\n",
            paste(rlp_types, collapse = ", "),
            ifelse(rlp_ok, "PASS", "WARNING")))

# NS should have multiple types
ns_types <- type_by_state |> filter(state_name == "Niedersachsen") |>
  pull(election_type)
expected_ns <- c("Bürgermeisterwahl", "Landratswahl", "SG-Bürgermeisterwahl")
ns_ok <- all(expected_ns %in% ns_types)
cat(sprintf("NS types: %s — %s\n",
            paste(ns_types, collapse = ", "),
            ifelse(ns_ok, "PASS", "WARNING")))

# NRW should have BM + OB
nrw_types <- type_by_state |> filter(state_name == "Nordrhein-Westfalen") |>
  pull(election_type)
expected_nrw <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")
nrw_ok <- all(expected_nrw %in% nrw_types)
cat(sprintf("NRW types: %s — %s\n",
            paste(nrw_types, collapse = ", "),
            ifelse(nrw_ok, "PASS", "WARNING")))

# ==============================================================================
# CHECK 15: STICHWAHL DISTRIBUTION AND CONSISTENCY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 15: STICHWAHL DISTRIBUTION AND CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

# has_stichwahl distribution by state
sw_by_state <- df |>
  distinct(ags, election_date, state_name, has_stichwahl) |>
  group_by(state_name, has_stichwahl) |>
  summarise(n_elections = n(), .groups = "drop") |>
  arrange(state_name, has_stichwahl)

cat("has_stichwahl by state:\n")
print(sw_by_state, n = 30)

# Elections with has_stichwahl should have election_date_sw
sw_elections <- df |>
  filter(has_stichwahl) |>
  distinct(ags, election_date, election_date_sw, state_name)

n_sw_no_date <- sum(is.na(sw_elections$election_date_sw))
cat(sprintf("\nStichwahl elections: %d total\n", nrow(sw_elections)))
cat(sprintf("  Without election_date_sw: %d\n", n_sw_no_date))

if (n_sw_no_date > 0) {
  cat("   Missing SW date by state:\n")
  print(sw_elections |> filter(is.na(election_date_sw)) |>
          count(state_name) |> arrange(desc(n)))
} else {
  cat(">> PASS: All Stichwahl elections have election_date_sw\n")
}

# election_date_sw should be after election_date
sw_date_check <- sw_elections |>
  filter(!is.na(election_date_sw)) |>
  mutate(gap_days = as.numeric(election_date_sw - election_date))

cat(sprintf("\nSW-HW date gap distribution (days):\n"))
print(quantile(sw_date_check$gap_days, c(0, 0.25, 0.5, 0.75, 1)))

n_neg_gap <- sum(sw_date_check$gap_days <= 0)
if (n_neg_gap > 0) {
  cat(sprintf("!! FAIL: %d elections where election_date_sw <= election_date\n", n_neg_gap))
} else {
  cat(">> PASS: All election_date_sw > election_date\n")
}

# Candidates with SW data should be in has_stichwahl elections
cand_with_sw <- df |>
  filter(!is.na(candidate_votes_sw) | !is.na(candidate_voteshare_sw)) |>
  filter(!has_stichwahl)

if (nrow(cand_with_sw) > 0) {
  cat(sprintf("\n!! FAIL: %d rows with SW data but has_stichwahl = FALSE\n",
              nrow(cand_with_sw)))
} else {
  cat(">> PASS: All SW data rows have has_stichwahl = TRUE\n")
}

# Stichwahl elections should have n_candidates_sw <= n_candidates_hw
sw_ncand <- df |>
  filter(has_stichwahl, !is.na(n_candidates_sw), !is.na(n_candidates_hw)) |>
  distinct(ags, election_date, n_candidates_hw, n_candidates_sw) |>
  filter(n_candidates_sw > n_candidates_hw)

if (nrow(sw_ncand) > 0) {
  cat(sprintf("\n!! WARNING: %d elections where n_candidates_sw > n_candidates_hw\n",
              nrow(sw_ncand)))
} else {
  cat(">> PASS: n_candidates_sw <= n_candidates_hw for all Stichwahl elections\n")
}

# Stichwahl should typically have 2 candidates
sw_ncand_dist <- df |>
  filter(has_stichwahl, !is.na(n_candidates_sw)) |>
  distinct(ags, election_date, n_candidates_sw, state_name)

cat(sprintf("\nCandidates per Stichwahl:\n"))
print(table(sw_ncand_dist$n_candidates_sw, useNA = "ifany"))

n_not_2 <- sum(sw_ncand_dist$n_candidates_sw != 2)
if (n_not_2 > 0) {
  cat(sprintf("   INFO: %d Stichwahl elections without exactly 2 candidates\n", n_not_2))
  cat("   By state:\n")
  print(sw_ncand_dist |> filter(n_candidates_sw != 2) |>
          count(state_name, n_candidates_sw) |> arrange(desc(n)))
} else {
  cat(">> PASS: All Stichwahl elections have exactly 2 candidates\n")
}

# ==============================================================================
# CHECK 16: SPOT CHECKS — KNOWN CITIES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 16: SPOT CHECKS — KNOWN CITIES\n")
cat(strrep("=", 70), "\n\n")

spot_checks <- tribble(
  ~entity,      ~ags,       ~state_name,            ~min_ev,  ~expect_name,
  "München",    "09162000", "Bayern",                500000,   FALSE,
  "Nürnberg",   "09564000", "Bayern",                200000,   FALSE,
  "Köln",       "05315000", "Nordrhein-Westfalen",   500000,   TRUE,
  "Düsseldorf", "05111000", "Nordrhein-Westfalen",   300000,   TRUE,
  "Dresden",    "14612000", "Sachsen",               300000,   FALSE,
  "Leipzig",    "14713000", "Sachsen",               300000,   FALSE,
  "Chemnitz",   "14511000", "Sachsen",               100000,   FALSE,
)

for (i in seq_len(nrow(spot_checks))) {
  sc <- spot_checks[i, ]
  matches <- df |> filter(ags == sc$ags)

  if (nrow(matches) == 0) {
    cat(sprintf("!! WARNING: %s (%s) not found in data\n", sc$entity, sc$ags))
    next
  }

  n_elections <- n_distinct(matches$election_date)
  max_ev <- max(matches$eligible_voters, na.rm = TRUE)
  ev_ok <- !is.na(max_ev) && is.finite(max_ev) && max_ev >= sc$min_ev
  has_name <- any(!is.na(matches$candidate_name))

  cat(sprintf(">> %s: %s — %d candidate rows, %d elections, max EV = %s %s\n",
              ifelse(ev_ok | is.na(sc$min_ev), "PASS", "WARN"),
              sc$entity, nrow(matches), n_elections,
              ifelse(is.finite(max_ev), format(max_ev, big.mark = ","), "NA"),
              ifelse(!ev_ok & !is.na(sc$min_ev),
                     sprintf("(expected >= %s)", format(sc$min_ev, big.mark = ",")),
                     "")))

  if (sc$expect_name & !has_name) {
    cat(sprintf("   !! Expected candidate names for %s but none found\n", sc$entity))
  }

  # Show a sample election
  latest <- matches |>
    filter(election_date == max(election_date)) |>
    arrange(candidate_rank_hw)
  if (nrow(latest) > 0) {
    cat(sprintf("   Latest election (%s): %d candidates, has_stichwahl = %s",
                latest$election_date[1], nrow(latest),
                ifelse(latest$has_stichwahl[1], "TRUE", "FALSE")))
    if (any(!is.na(latest$candidate_name))) {
      winner <- latest |> filter(is_winner)
      if (nrow(winner) > 0) {
        cat(sprintf(", winner: %s (%s)",
                    ifelse(!is.na(winner$candidate_name[1]),
                           winner$candidate_name[1], "no name"),
                    ifelse(!is.na(winner$candidate_party[1]),
                           winner$candidate_party[1], "no party")))
      }
    } else {
      winner <- latest |> filter(is_winner)
      if (nrow(winner) > 0) {
        cat(sprintf(", winner party: %s",
                    ifelse(!is.na(winner$candidate_party[1]),
                           winner$candidate_party[1], "NA")))
      }
    }
    cat("\n")
  }
}

# ==============================================================================
# CHECK 17: VOTESHARE SUM PER ELECTION (HAUPTWAHL)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 17: VOTESHARE SUM PER ELECTION (HAUPTWAHL)\n")
cat(strrep("=", 70), "\n\n")

vs_sum <- df |>
  filter(!is.na(candidate_voteshare_hw)) |>
  group_by(ags, election_date, state_name) |>
  summarise(vs_sum = sum(candidate_voteshare_hw), n = n(), .groups = "drop")

cat("Voteshare HW sum distribution:\n")
print(quantile(vs_sum$vs_sum, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

n_over <- sum(vs_sum$vs_sum > 1.05)
n_under <- sum(vs_sum$vs_sum < 0.5)

if (n_over > 0) {
  over_detail <- vs_sum |> filter(vs_sum > 1.05)
  cat(sprintf("\n   INFO: %d elections where sum(candidate_voteshare_hw) > 1.05\n", n_over))
  cat("   (Expected: RLP VG elections with per-municipality voteshares)\n")
  cat("   By state:\n")
  print(over_detail |> count(state_name) |> arrange(desc(n)))
} else {
  cat("\n>> PASS: No elections with voteshare HW sum > 1.05\n")
}

if (n_under > 0) {
  cat(sprintf("   INFO: %d elections where sum(candidate_voteshare_hw) < 0.5\n", n_under))
  cat("   (Expected for NS/BY: PDFs may list only top candidates)\n")
}

# Stichwahl voteshare sum — should be ~1.0 for elections with 2 SW candidates
vs_sum_sw <- df |>
  filter(!is.na(candidate_voteshare_sw)) |>
  group_by(ags, election_date, state_name) |>
  summarise(vs_sum = sum(candidate_voteshare_sw), n = n(), .groups = "drop")

if (nrow(vs_sum_sw) > 0) {
  cat(sprintf("\nStichwahl voteshare sum: %d elections\n", nrow(vs_sum_sw)))
  cat("Distribution:\n")
  print(quantile(vs_sum_sw$vs_sum, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))
}

# ==============================================================================
# CHECK 18: ELECTION YEAR vs DATE CONSISTENCY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 18: ELECTION YEAR vs DATE CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

yr_mismatch <- df |>
  filter(!is.na(election_date)) |>
  mutate(date_year = year(election_date)) |>
  filter(election_year != date_year)

n_yr_mismatch <- n_distinct(paste(yr_mismatch$ags, yr_mismatch$election_date))

if (n_yr_mismatch > 0) {
  cat(sprintf("   INFO: %d elections where election_year != year(election_date) (%d rows)\n",
              n_yr_mismatch, nrow(yr_mismatch)))
  cat("   (Expected: Sachsen year-boundary runoffs where election_year = cycle year)\n")
  print(yr_mismatch |> distinct(ags, state_name, election_year, election_date) |> head(10))
} else {
  cat(">> PASS: election_year matches year(election_date) for all rows\n")
}

# ==============================================================================
# CHECK 19: N_CANDIDATES AND CANDIDATE_RANK CONSISTENCY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 19: N_CANDIDATES AND CANDIDATE_RANK CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

# Count of HW candidates (non-NA rank) should match n_candidates_hw
# Note: total row count may differ because SW-only candidates have candidate_rank_hw = NA
nc_check <- df |>
  filter(!is.na(candidate_rank_hw)) |>
  group_by(ags, election_date) |>
  summarise(actual_n = n(), stored_n = first(n_candidates_hw), .groups = "drop") |>
  filter(actual_n != stored_n)

if (nrow(nc_check) > 0) {
  nc_detail <- nc_check |>
    left_join(df |> distinct(ags, state_name), by = "ags")
  cat(sprintf("   INFO: %d elections where HW candidate count != n_candidates_hw\n", nrow(nc_check)))
  cat("   (Expected: SW-only candidates share n_candidates_hw from the election group)\n")
  cat("   By state:\n")
  print(nc_detail |> count(state_name) |> arrange(desc(n)))
} else {
  cat(">> PASS: HW candidate count matches n_candidates_hw for all elections\n")
}

# Check total rows vs n_candidates_hw — difference = SW-only candidates
sw_only_check <- df |>
  group_by(ags, election_date) |>
  summarise(
    total_rows = n(),
    hw_candidates = sum(!is.na(candidate_rank_hw)),
    sw_only = sum(is.na(candidate_rank_hw)),
    .groups = "drop"
  ) |>
  filter(sw_only > 0)

cat(sprintf("   INFO: %d elections with SW-only candidates (%d extra rows)\n",
            nrow(sw_only_check), sum(sw_only_check$sw_only)))

# candidate_rank_hw should have no gaps (max_rank should equal n_candidates_hw)
rank_check <- df |>
  filter(!is.na(candidate_rank_hw)) |>
  group_by(ags, election_date) |>
  summarise(max_rank = max(candidate_rank_hw), n_cand = n(), .groups = "drop") |>
  filter(max_rank > n_cand)

if (nrow(rank_check) > 0) {
  cat(sprintf("!! WARNING: %d elections with HW rank gaps (max_rank > n_candidates)\n",
              nrow(rank_check)))
} else {
  cat(">> PASS: No HW rank gaps (max_rank == n_candidates for all elections)\n")
}

# ==============================================================================
# CHECK 20: CANDIDATE GENDER CONSISTENCY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CHECK 20: CANDIDATE GENDER CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

gender_vals <- df |> filter(!is.na(candidate_gender)) |> pull(candidate_gender)
cat("Gender values:\n")
print(table(gender_vals, useNA = "ifany"))

valid_genders <- c("m", "w")
invalid <- setdiff(unique(gender_vals), valid_genders)
if (length(invalid) > 0) {
  cat(sprintf("!! WARNING: Non-standard gender values found: %s\n",
              paste(invalid, collapse = ", ")))
  print(df |> filter(candidate_gender %in% invalid) |>
          select(ags, state_name, candidate_name, candidate_gender) |> head(10))
} else {
  cat(">> PASS: All gender values are 'm' or 'w'\n")
}

# Gender by state
cat("\nGender coverage by state:\n")
print(df |>
        filter(!is.na(candidate_gender)) |>
        count(state_name, candidate_gender) |>
        pivot_wider(names_from = candidate_gender, values_from = n, values_fill = 0))

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Mayoral elections candidate-level data checks completed.\n")
cat(sprintf("Dataset: %d rows, %d cols, %d states\n",
            nrow(df), ncol(df), n_distinct(df$state_name)))
cat("Format: WIDE — one row per candidate per election cycle\n")
cat("States:", paste(sort(unique(df$state_name)), collapse = ", "), "\n")
cat("Year range:", min(df$election_year, na.rm = TRUE), "to",
    max(df$election_year, na.rm = TRUE), "\n")
cat(sprintf("Total elections: %d\n",
            n_distinct(paste(df$ags, df$election_date))))
cat(sprintf("Elections with Stichwahl: %d (%.1f%%)\n",
            n_distinct(paste(df$ags[df$has_stichwahl], df$election_date[df$has_stichwahl])),
            100 * n_distinct(paste(df$ags[df$has_stichwahl], df$election_date[df$has_stichwahl])) /
              n_distinct(paste(df$ags, df$election_date))))
cat(sprintf("Candidates with names: %d (%.1f%%)\n",
            sum(!is.na(df$candidate_name)),
            100 * sum(!is.na(df$candidate_name)) / nrow(df)))

cat("\nReview check output above for specific PASS/WARNING/FAIL results.\n")
cat("\n")

cat("=", strrep("=", 70), "\n")
cat("END OF CHECKS\n")
cat("=", strrep("=", 70), "\n")
