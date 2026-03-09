### Sanity checks for mayor panel data
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
cat("MAYOR PANEL: DATA QUALITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load datasets ----------------------------------------------------------------

cat("Loading datasets...\n")

panel <- read_rds("data/mayoral_elections/final/mayor_panel.rds") |>
  as_tibble()
annual <- read_rds("data/mayoral_elections/final/mayor_panel_annual.rds") |>
  as_tibble()

cat("Mayor panel:", nrow(panel), "rows x", ncol(panel), "cols\n")
cat("Annual panel:", nrow(annual), "rows x", ncol(annual), "cols\n\n")

pass_count <- 0
fail_count <- 0
warn_count <- 0

check_result <- function(name, passed, detail = "") {
  if (passed) {
    cat(sprintf("  PASS: %s\n", name))
    pass_count <<- pass_count + 1
  } else {
    cat(sprintf("  FAIL: %s %s\n", name, detail))
    fail_count <<- fail_count + 1
  }
}


# Check 1: No duplicate (person_id, election_year) ----------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 1: NO DUPLICATE (person_id, election_year)\n")
cat(strrep("=", 70), "\n\n")

dupes <- panel |>
  group_by(person_id, election_year) |>
  filter(n() > 1) |>
  ungroup()

check_result("No duplicate (person_id, election_year)",
             nrow(dupes) == 0,
             sprintf("— found %d duplicates", nrow(dupes)))

if (nrow(dupes) > 0) {
  print(dupes |> select(person_id, ags, election_year, winner_party) |> head(20))
}
cat("\n")


# Check 2: term_number is sequential within person_id -------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 2: TERM_NUMBER IS SEQUENTIAL\n")
cat(strrep("=", 70), "\n\n")

seq_check <- panel |>
  arrange(person_id, ags, election_date) |>
  group_by(person_id, ags) |>
  mutate(expected_term = row_number()) |>
  ungroup() |>
  filter(term_number != expected_term)

check_result("term_number is sequential within (person_id, ags)",
             nrow(seq_check) == 0,
             sprintf("— %d mismatches", nrow(seq_check)))
cat("\n")


# Check 3: person_id doesn't span multiple states -----------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 3: PERSON_ID WITHIN SINGLE STATE\n")
cat(strrep("=", 70), "\n\n")

multi_state <- panel |>
  group_by(person_id) |>
  summarise(n_states = n_distinct(state), .groups = "drop") |>
  filter(n_states > 1)

check_result("No person_id spans multiple states",
             nrow(multi_state) == 0,
             sprintf("— %d persons in >1 state", nrow(multi_state)))
cat("\n")


# Check 4: Bayern Amtsantritt grouping produces sensible tenures ---------------

cat(strrep("=", 70), "\n")
cat("CHECK 4: BAYERN TENURE LENGTHS\n")
cat(strrep("=", 70), "\n\n")

bayern <- panel |> filter(state == "09")
bayern_persons <- bayern |>
  group_by(person_id) |>
  summarise(
    n_terms = n(),
    tenure_span = max(election_year) - min(election_year),
    .groups = "drop"
  )

cat("  Bayern unique persons:", n_distinct(bayern$person_id), "\n")
cat("  Bayern person-elections:", nrow(bayern), "\n")
cat("  Mean terms:", round(mean(bayern_persons$n_terms), 2), "\n")
cat("  Max terms:", max(bayern_persons$n_terms), "\n")
cat("  Max tenure span:", max(bayern_persons$tenure_span), "years\n")

# Bayern has ~2000 municipalities, elections every 6 years since 1945
# Expect ~10,000+ person-elections
check_result("Bayern has >10,000 person-elections",
             nrow(bayern) > 10000,
             sprintf("— found %d", nrow(bayern)))

# No unreasonably long tenures (>40 years is suspicious)
n_very_long <- sum(bayern_persons$tenure_span > 40)
if (n_very_long > 0) {
  cat(sprintf("  WARNING: %d Bayern persons with tenure span >40 years\n", n_very_long))
  warn_count <- warn_count + 1
} else {
  cat("  No Bayern persons with tenure span >40 years — OK\n")
}
cat("\n")


# Check 5: Distribution by state ----------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 5: STATE DISTRIBUTION\n")
cat(strrep("=", 70), "\n\n")

state_summary <- panel |>
  group_by(state) |>
  summarise(
    n_elections = n(),
    n_persons = n_distinct(person_id),
    n_multi_term = n_distinct(person_id[term_number >= 2]),
    pct_multi = round(100 * n_multi_term / n_persons, 1),
    year_range = paste(min(election_year), "-", max(election_year)),
    .groups = "drop"
  )

print(state_summary)
cat("\n")


# Check 6: is_incumbent consistency -------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 6: IS_INCUMBENT CONSISTENCY\n")
cat(strrep("=", 70), "\n\n")

inc_check <- panel |>
  filter(is_incumbent == 1, term_number < 2)

check_result("is_incumbent=1 implies term_number>=2",
             nrow(inc_check) == 0,
             sprintf("— %d violations", nrow(inc_check)))

inc_check2 <- panel |>
  filter(is_incumbent == 0, term_number >= 2)

check_result("term_number>=2 implies is_incumbent=1",
             nrow(inc_check2) == 0,
             sprintf("— %d violations", nrow(inc_check2)))
cat("\n")


# Check 7: winning_margin in [0, 1] -------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 7: WINNING_MARGIN RANGE\n")
cat(strrep("=", 70), "\n\n")

margin_range <- panel |>
  filter(!is.na(winning_margin)) |>
  summarise(
    min_margin = min(winning_margin),
    max_margin = max(winning_margin),
    mean_margin = mean(winning_margin),
    n_negative = sum(winning_margin < 0),
    n_above_1 = sum(winning_margin > 1)
  )

cat(sprintf("  Range: [%.4f, %.4f]\n", margin_range$min_margin, margin_range$max_margin))
cat(sprintf("  Mean: %.4f\n", margin_range$mean_margin))

check_result("No negative winning_margin",
             margin_range$n_negative == 0,
             sprintf("— %d negative values", margin_range$n_negative))

check_result("No winning_margin > 1",
             margin_range$n_above_1 == 0,
             sprintf("— %d values > 1", margin_range$n_above_1))
cat("\n")


# Check 8: Annual panel coverage -----------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 8: ANNUAL PANEL COVERAGE\n")
cat(strrep("=", 70), "\n\n")

# Each person-election should appear at least once
panel_keys <- panel |> distinct(person_id, election_year)
annual_election_keys <- annual |>
  filter(year == election_year) |>
  distinct(person_id, election_year)

missing <- panel_keys |>
  anti_join(annual_election_keys, by = c("person_id", "election_year"))

check_result("All panel elections appear in annual panel",
             nrow(missing) == 0,
             sprintf("— %d missing", nrow(missing)))

# No gaps in year sequence within person
year_gaps <- annual |>
  arrange(person_id, ags, year) |>
  group_by(person_id, ags, election_year) |>
  mutate(year_diff = year - lag(year, default = year[1])) |>
  ungroup() |>
  filter(year_diff > 1)

check_result("No year gaps in annual panel",
             nrow(year_gaps) == 0,
             sprintf("— %d gaps", nrow(year_gaps)))
cat("\n")


# Check 9: ags_21 populated ---------------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECK 9: AGS_21 COVERAGE\n")
cat(strrep("=", 70), "\n\n")

n_no_ags21 <- sum(is.na(panel$ags_21))
cat(sprintf("  Panel rows without ags_21: %d of %d (%.1f%%)\n",
            n_no_ags21, nrow(panel), 100 * n_no_ags21 / nrow(panel)))
check_result("All panel rows have ags_21",
             n_no_ags21 == 0,
             sprintf("— %d missing", n_no_ags21))

n_no_ags21_annual <- sum(is.na(annual$ags_21))
check_result("All annual panel rows have ags_21",
             n_no_ags21_annual == 0,
             sprintf("— %d missing", n_no_ags21_annual))
cat("\n")


# Summary ----------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(sprintf("SUMMARY: %d PASS, %d FAIL, %d WARNINGS\n",
            pass_count, fail_count, warn_count))
cat(strrep("=", 70), "\n")
