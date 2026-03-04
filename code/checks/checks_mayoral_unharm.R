### Sanity checks for mayoral elections unharmonized data
# Date: 2026

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
cat("MAYORAL ELECTIONS UNHARMONIZED: DATA QUALITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load dataset -----------------------------------------------------------------

cat("Loading dataset...\n")

df <- read_rds("data/mayoral_elections/final/mayoral_unharm.rds") |>
  as_tibble()

cat("Loaded mayoral_unharm:", nrow(df), "rows x", ncol(df), "cols\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n\n")

# Check 1: Coverage by state and year -----------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 1: COVERAGE BY STATE AND YEAR\n")
cat(strrep("=", 70), "\n\n")

coverage <- df |>
  group_by(state_name, state) |>
  summarise(
    n_rows = n(),
    min_year = min(election_year, na.rm = TRUE),
    max_year = max(election_year, na.rm = TRUE),
    n_years = n_distinct(election_year),
    .groups = "drop"
  ) |>
  arrange(desc(n_rows))

cat("Coverage by state:\n")
print(coverage)

# Expected row counts (approximate)
expected <- tibble(
  state_name = c("Bayern", "Sachsen", "Rheinland-Pfalz",
                 "Nordrhein-Westfalen", "Niedersachsen", "Saarland",
                 "Schleswig-Holstein"),
  expected_min_rows = c(30000, 2000, 1000, 1000, 50, 50, 30),
  expected_min_year = c(1945, 2001, 1994, 2009, 2019, 2019, 2023),
  expected_max_year = c(2025, 2024, 2025, 2025, 2019, 2025, 2025)
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

# Check 2: AGS validity -------------------------------------------------------

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
} else {
  cat("\n>> PASS: No NA AGS values\n")
}

if (non_8 > 0) {
  cat(sprintf("!! FAIL: %d rows with AGS not 8 digits\n", non_8))
  cat("Examples:\n")
  print(head(df |> filter(nchar(ags) != 8) |> select(ags, state_name, ags_name), 10))
} else {
  cat(">> PASS: All AGS are 8 characters\n")
}

# Check state prefix matches state_name
state_prefix_map <- tibble(
  state = c("09", "05", "10", "14", "07", "03"),
  state_name = c("Bayern", "Nordrhein-Westfalen", "Saarland",
                 "Sachsen", "Rheinland-Pfalz", "Niedersachsen")
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

# Check 3: Vote count consistency ----------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 3: VOTE COUNT CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

# valid_votes + invalid_votes ≈ number_voters (where all non-NA)
df_vote_check <- df |>
  filter(!is.na(valid_votes) & !is.na(invalid_votes) & !is.na(number_voters)) |>
  mutate(
    vote_sum = valid_votes + invalid_votes,
    vote_diff = abs(vote_sum - number_voters)
  )

n_vote_inconsistent <- sum(df_vote_check$vote_diff > 1)

cat(sprintf("Rows with all three vote counts non-NA: %d\n", nrow(df_vote_check)))

if (n_vote_inconsistent > 0) {
  cat(sprintf("!! WARNING: %d rows where |valid + invalid - number_voters| > 1\n",
              n_vote_inconsistent))
  cat("Distribution of differences:\n")
  print(quantile(df_vote_check$vote_diff[df_vote_check$vote_diff > 1],
                 c(0.5, 0.9, 0.99, 1), na.rm = TRUE))
  cat("Examples:\n")
  print(head(df_vote_check |>
               filter(vote_diff > 1) |>
               select(ags, state_name, election_year, valid_votes, invalid_votes,
                      number_voters, vote_sum, vote_diff) |>
               arrange(desc(vote_diff)), 10))
} else {
  cat(">> PASS: valid_votes + invalid_votes = number_voters (within tolerance)\n")
}

# winner_votes <= valid_votes
df_winner_check <- df |>
  filter(!is.na(winner_votes) & !is.na(valid_votes))

n_winner_exceeds <- sum(df_winner_check$winner_votes > df_winner_check$valid_votes)

if (n_winner_exceeds > 0) {
  cat(sprintf("\n!! WARNING: %d rows where winner_votes > valid_votes\n",
              n_winner_exceeds))
  print(head(df_winner_check |>
               filter(winner_votes > valid_votes) |>
               select(ags, state_name, election_year, winner_votes, valid_votes), 10))
} else {
  cat(">> PASS: winner_votes <= valid_votes for all rows\n")
}

# number_voters <= eligible_voters
df_voters_check <- df |>
  filter(!is.na(number_voters) & !is.na(eligible_voters))

n_voters_exceeds <- sum(df_voters_check$number_voters > df_voters_check$eligible_voters)

if (n_voters_exceeds > 0) {
  cat(sprintf("\n!! WARNING: %d rows where number_voters > eligible_voters\n",
              n_voters_exceeds))
  print(head(df_voters_check |>
               filter(number_voters > eligible_voters) |>
               select(ags, state_name, election_year, number_voters, eligible_voters) |>
               arrange(desc(number_voters - eligible_voters)), 10))
} else {
  cat(">> PASS: number_voters <= eligible_voters for all rows\n")
}

# Check 4: Turnout realistic range --------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 4: TURNOUT REALISTIC RANGE\n")
cat(strrep("=", 70), "\n\n")

df_turnout <- df |> filter(!is.na(turnout))

cat(sprintf("Rows with non-NA turnout: %d (%.1f%% of total)\n",
            nrow(df_turnout), 100 * nrow(df_turnout) / nrow(df)))
cat("Turnout distribution:\n")
print(quantile(df_turnout$turnout, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

# Flag turnout = 0
n_zero <- sum(df_turnout$turnout == 0)
if (n_zero > 0) {
  cat(sprintf("\n!! WARNING: %d rows with turnout = 0\n", n_zero))
  print(df_turnout |> filter(turnout == 0) |>
          select(ags, state_name, election_year, eligible_voters, number_voters, turnout) |>
          head(10))
} else {
  cat("\n>> PASS: No turnout = 0\n")
}

# Flag turnout > 1
n_over1 <- sum(df_turnout$turnout > 1)
if (n_over1 > 0) {
  cat(sprintf("!! WARNING: %d rows with turnout > 1\n", n_over1))
  print(df_turnout |> filter(turnout > 1) |>
          select(ags, state_name, election_year, eligible_voters, number_voters, turnout) |>
          head(10))
} else {
  cat(">> PASS: No turnout > 1\n")
}

# Flag very low turnout < 0.15
n_verylow <- sum(df_turnout$turnout < 0.15 & df_turnout$turnout > 0)
if (n_verylow > 0) {
  cat(sprintf("\n   INFO: %d rows with turnout in (0, 0.15) — unusual but possible\n",
              n_verylow))
  cat("   By state:\n")
  print(df_turnout |> filter(turnout < 0.15 & turnout > 0) |>
          count(state_name) |> arrange(desc(n)))
}

# Check 5: Winner voteshare validity ------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 5: WINNER VOTESHARE VALIDITY\n")
cat(strrep("=", 70), "\n\n")

df_ws <- df |> filter(!is.na(winner_voteshare))

cat(sprintf("Rows with non-NA winner_voteshare: %d (%.1f%% of total)\n",
            nrow(df_ws), 100 * nrow(df_ws) / nrow(df)))
cat("Distribution:\n")
print(quantile(df_ws$winner_voteshare, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))

# Voteshare <= 0
n_le0 <- sum(df_ws$winner_voteshare <= 0)
if (n_le0 > 0) {
  cat(sprintf("\n!! WARNING: %d rows with winner_voteshare <= 0\n", n_le0))
  print(head(df_ws |> filter(winner_voteshare <= 0) |>
               select(ags, state_name, election_year, winner_voteshare, winner_votes, valid_votes), 10))
} else {
  cat("\n>> PASS: All winner_voteshare > 0\n")
}

# Voteshare > 1
n_gt1 <- sum(df_ws$winner_voteshare > 1)
if (n_gt1 > 0) {
  cat(sprintf("!! WARNING: %d rows with winner_voteshare > 1\n", n_gt1))
  cat("   By state:\n")
  print(df_ws |> filter(winner_voteshare > 1) |>
          count(state_name) |> arrange(desc(n)))
  cat("   Examples:\n")
  print(head(df_ws |> filter(winner_voteshare > 1) |>
               select(ags, state_name, election_year, winner_voteshare, winner_votes, valid_votes), 10))
} else {
  cat(">> PASS: All winner_voteshare <= 1\n")
}

# Check 6: Missing data patterns -----------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 6: MISSING DATA PATTERNS BY STATE\n")
cat(strrep("=", 70), "\n\n")

vote_cols <- c("eligible_voters", "number_voters", "valid_votes",
               "invalid_votes", "turnout", "winner_votes", "winner_voteshare",
               "winner_party")

for (st in sort(unique(df$state_name))) {
  df_st <- df |> filter(state_name == st)
  cat(sprintf("--- %s (%d rows) ---\n", st, nrow(df_st)))

  na_counts <- df_st |>
    summarise(across(all_of(vote_cols[vote_cols %in% names(df_st)]),
                     ~ sum(is.na(.)))) |>
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

# RLP-specific check: should have NA for count columns (percentages only)
df_rlp <- df |> filter(state_name == "Rheinland-Pfalz")
rlp_count_cols <- c("eligible_voters", "number_voters", "valid_votes",
                     "invalid_votes", "winner_votes")
rlp_has_counts <- df_rlp |>
  summarise(across(all_of(rlp_count_cols), ~ sum(!is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_nonNA") |>
  filter(n_nonNA > 0)

if (nrow(rlp_has_counts) > 0) {
  cat("RLP non-NA count columns (expected: mostly NA since RLP has percentages only):\n")
  print(rlp_has_counts)
} else {
  cat(">> PASS: RLP has all count columns as NA (as expected — percentages only)\n")
}

# Sachsen-specific: winner_party NA pattern
df_sn <- df |> filter(state_name == "Sachsen")
n_sn_no_party <- sum(is.na(df_sn$winner_party))
cat(sprintf("\nSachsen: %d rows with NA winner_party (%.1f%% — expected for VE/first-round rows)\n",
            n_sn_no_party, 100 * n_sn_no_party / nrow(df_sn)))

# Check 7: Election date parsing -----------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 7: ELECTION DATE VALIDITY\n")
cat(strrep("=", 70), "\n\n")

n_na_date <- sum(is.na(df$election_date))
cat(sprintf("Rows with NA election_date: %d\n", n_na_date))

if (n_na_date > 0) {
  cat("   By state:\n")
  print(df |> filter(is.na(election_date)) |> count(state_name) |> arrange(desc(n)))
}

# Date range
df_dated <- df |> filter(!is.na(election_date))
cat(sprintf("Date range: %s to %s\n", min(df_dated$election_date), max(df_dated$election_date)))

# Year range check
min_date_year <- year(min(df_dated$election_date))
max_date_year <- year(max(df_dated$election_date))

if (min_date_year < 1945 | max_date_year > 2026) {
  cat(sprintf("!! WARNING: Date years outside 1945-2026 range: %d to %d\n",
              min_date_year, max_date_year))
} else {
  cat(">> PASS: All dates in plausible range (1945-2026)\n")
}

# election_year == year(election_date)?
df_year_check <- df |>
  filter(!is.na(election_date) & !is.na(election_year)) |>
  mutate(date_year = year(election_date),
         year_match = election_year == date_year)

n_year_mismatch <- sum(!df_year_check$year_match)

if (n_year_mismatch > 0) {
  cat(sprintf("!! WARNING: %d rows where election_year != year(election_date)\n",
              n_year_mismatch))
  print(head(df_year_check |> filter(!year_match) |>
               select(ags, state_name, election_year, election_date, date_year), 10))
} else {
  cat(">> PASS: election_year matches year(election_date) for all rows\n")
}

# Check 8: Known entity spot-checks -------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 8: KNOWN ENTITY SPOT-CHECKS\n")
cat(strrep("=", 70), "\n\n")

spot_checks <- tribble(
  ~entity,     ~ags,        ~state_name,         ~min_ev,
  "Chemnitz",  "14511000",  "Sachsen",            100000,
  "Dresden",   "14612000",  "Sachsen",            300000,
  "Leipzig",   "14713000",  "Sachsen",            300000,
  "München",   "09162000",  "Bayern",             500000,
  "Nürnberg",  "09564000",  "Bayern",             200000,
  "Köln",      "05315000",  "Nordrhein-Westfalen", 500000,
  "Düsseldorf","05111000",  "Nordrhein-Westfalen", 300000,
)

for (i in seq_len(nrow(spot_checks))) {
  sc <- spot_checks[i, ]
  matches <- df |> filter(ags == sc$ags)

  if (nrow(matches) == 0) {
    cat(sprintf("!! WARNING: %s (%s) not found in data\n", sc$entity, sc$ags))
  } else {
    max_ev <- max(matches$eligible_voters, na.rm = TRUE)
    ev_ok <- !is.na(max_ev) && max_ev >= sc$min_ev
    cat(sprintf(">> %s: %s (%s) — %d rows, max eligible_voters = %s %s\n",
                ifelse(ev_ok, "PASS", "WARN"),
                sc$entity, sc$ags, nrow(matches),
                format(max_ev, big.mark = ","),
                ifelse(ev_ok, "", sprintf("(expected >= %s)",
                                          format(sc$min_ev, big.mark = ",")))))
  }
}

# RLP spot-check: check by state since RLP uses non-standard AGS for VG
df_rlp_ob <- df |> filter(state_name == "Rheinland-Pfalz",
                           election_type == "Oberbürgermeisterwahl")
cat(sprintf("\nRLP Oberbürgermeisterwahl: %d rows\n", nrow(df_rlp_ob)))
if (nrow(df_rlp_ob) > 0) {
  cat("  Municipalities:\n")
  print(df_rlp_ob |> distinct(ags, ags_name) |> arrange(ags))
}

# Check 9: Round distribution --------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 9: ROUND DISTRIBUTION (HAUPTWAHL vs STICHWAHL)\n")
cat(strrep("=", 70), "\n\n")

round_by_state <- df |>
  group_by(state_name, round) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(state_name, round)

cat("Round distribution by state:\n")
print(round_by_state, n = 30)

# Expected Stichwahl counts (approximate)
expected_sw <- tribble(
  ~state_name,              ~expected_min_sw,
  "Bayern",                 3000,
  "Sachsen",                250,
  "Nordrhein-Westfalen",    100,
  "Rheinland-Pfalz",        30,
  "Saarland",               10,
  "Niedersachsen",          5,
  "Schleswig-Holstein",     1,
)

sw_counts <- round_by_state |>
  filter(round == "stichwahl") |>
  left_join(expected_sw, by = "state_name") |>
  mutate(ok = n >= expected_min_sw)

cat("\nStichwahl counts vs expected minimums:\n")
print(sw_counts)

sw_issues <- sw_counts |> filter(!ok)
if (nrow(sw_issues) > 0) {
  cat("\n!! WARNING: Some states below expected Stichwahl minimum:\n")
  print(sw_issues |> select(state_name, n, expected_min_sw))
} else {
  cat("\n>> PASS: All states meet expected Stichwahl minimums\n")
}

# Check that round column only has valid values
invalid_round <- df |> filter(!round %in% c("hauptwahl", "stichwahl"))
if (nrow(invalid_round) > 0) {
  cat(sprintf("!! FAIL: %d rows with invalid round value\n", nrow(invalid_round)))
} else {
  cat(">> PASS: All round values are 'hauptwahl' or 'stichwahl'\n")
}

# Check 10: Duplicate detection ------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 10: DUPLICATE DETECTION\n")
cat(strrep("=", 70), "\n\n")

# Check for duplicate (ags, election_date, election_type, round)
dupes <- df |>
  group_by(ags, election_date, election_type, round) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

if (nrow(dupes) > 0) {
  cat(sprintf("!! WARNING: %d (ags, date, type, round) combinations with >1 row\n",
              nrow(dupes)))
  cat("   By state:\n")
  dupes_detail <- dupes |>
    left_join(df |> distinct(ags, state_name), by = "ags")
  print(dupes_detail |> count(state_name) |> arrange(desc(n)))
  cat("   Examples:\n")
  print(head(dupes_detail |> arrange(desc(n)), 20))
} else {
  cat(">> PASS: No duplicate (ags, date, type, round) rows\n")
}

# Also check without election_type (looser check)
dupes_loose <- df |>
  group_by(ags, election_date) |>
  summarise(n = n(), n_types = n_distinct(election_type), .groups = "drop") |>
  filter(n > 1)

if (nrow(dupes_loose) > 0) {
  cat(sprintf("\n   INFO: %d (ags, election_date) with >1 row (across election types)\n",
              nrow(dupes_loose)))
  # This is expected if multiple election types on same date
  n_same_type <- sum(dupes_loose$n_types == 1)
  cat(sprintf("   Of these, %d have same election_type (true duplicates)\n", n_same_type))
  cat(sprintf("   and %d have different election_types (expected, e.g. OB + Landrat)\n",
              nrow(dupes_loose) - n_same_type))
}

# Check 11: Election type distribution ----------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECK 11: ELECTION TYPE DISTRIBUTION\n")
cat(strrep("=", 70), "\n\n")

type_by_state <- df |>
  group_by(state_name, election_type) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(state_name, desc(n))

cat("Election types by state:\n")
print(type_by_state, n = 50)

# RLP should have 4 types
rlp_types <- type_by_state |> filter(state_name == "Rheinland-Pfalz") |> pull(election_type)
expected_rlp <- c("Bürgermeisterwahl", "Landratswahl", "Oberbürgermeisterwahl",
                  "VG-Bürgermeisterwahl")
rlp_ok <- all(expected_rlp %in% rlp_types)
cat(sprintf("\nRLP election types: %s (%s)\n",
            paste(rlp_types, collapse = ", "),
            ifelse(rlp_ok, "PASS — all 4 expected types present",
                   sprintf("WARN — missing: %s",
                           paste(setdiff(expected_rlp, rlp_types), collapse = ", ")))))

# NS should have 3 types
ns_types <- type_by_state |> filter(state_name == "Niedersachsen") |> pull(election_type)
expected_ns <- c("Bürgermeisterwahl", "Landratswahl", "SG-Bürgermeisterwahl")
ns_ok <- all(expected_ns %in% ns_types)
cat(sprintf("NS election types: %s (%s)\n",
            paste(ns_types, collapse = ", "),
            ifelse(ns_ok, "PASS — all 3 expected types present",
                   sprintf("WARN — missing: %s",
                           paste(setdiff(expected_ns, ns_types), collapse = ", ")))))

# NRW should have 2 types (BM + OB)
nrw_types <- type_by_state |> filter(state_name == "Nordrhein-Westfalen") |> pull(election_type)
expected_nrw <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")
nrw_ok <- all(expected_nrw %in% nrw_types)
cat(sprintf("NRW election types: %s (%s)\n",
            paste(nrw_types, collapse = ", "),
            ifelse(nrw_ok, "PASS — both BM and OB types present",
                   sprintf("WARN — missing: %s",
                           paste(setdiff(expected_nrw, nrw_types), collapse = ", ")))))

# Other states should only have Bürgermeisterwahl
other_states <- c("Bayern", "Saarland", "Sachsen")
for (st in other_states) {
  st_types <- type_by_state |> filter(state_name == st) |> pull(election_type)
  if (length(st_types) == 1 && st_types == "Bürgermeisterwahl") {
    cat(sprintf("%s: PASS — only Bürgermeisterwahl\n", st))
  } else {
    cat(sprintf("%s: INFO — types: %s\n", st, paste(st_types, collapse = ", ")))
  }
}

# Summary ----------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Mayoral elections unharmonized data checks completed.\n")
cat(sprintf("Dataset: %d rows, %d cols, %d states\n",
            nrow(df), ncol(df), n_distinct(df$state_name)))
cat("States:", paste(sort(unique(df$state_name)), collapse = ", "), "\n")
cat("Year range:", min(df$election_year, na.rm = TRUE), "to",
    max(df$election_year, na.rm = TRUE), "\n")
cat("\nReview check output above for specific PASS/WARNING/FAIL results.\n")
cat("\n")

cat("=", strrep("=", 70), "\n")
cat("END OF CHECKS\n")
cat("=", strrep("=", 70), "\n")
