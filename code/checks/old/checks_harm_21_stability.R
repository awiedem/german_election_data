### Check why the number of municipalities in harm_21 is not stable across years
# Should be exactly the same as 2021 (10641) for all years
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table"
)

conflict_prefer("filter", "dplyr")

# Helper function to pad AGS
pad_zero_conditional <- function(x, n) {
  x <- as.character(x)
  x <- str_pad(x, width = n, side = "left", pad = "0")
  return(x)
}

cat("=", strrep("=", 70), "\n")
cat("CHECKING STABILITY OF MUNICIPALITY COUNT IN harm_21\n")
cat("=", strrep("=", 70), "\n\n")

# Load harmonized dataset ---------------------------------------------------

cat("Loading harmonized dataset...\n")
harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

cat("Municipality count by election year:\n")
muni_count <- harm_21 |>
  count(election_year, name = "n_municipalities") |>
  arrange(election_year)

print(muni_count)

# Expected: all years should have the same number as 2021
n_expected <- muni_count |> filter(election_year == 2021) |> pull(n_municipalities)
cat("\nExpected number of municipalities (from 2021):", n_expected, "\n\n")

# Find which municipalities are missing in each year ------------------------

cat(strrep("=", 70), "\n")
cat("FINDING MISSING MUNICIPALITIES BY YEAR\n")
cat(strrep("=", 70), "\n\n")

# Get all municipalities that exist in 2021
ags_2021 <- harm_21 |>
  filter(election_year == 2021) |>
  distinct(ags) |>
  pull(ags)

cat("Total municipalities in 2021:", length(ags_2021), "\n\n")

# For each election year, find which municipalities from 2021 are missing
missing_by_year <- tibble()

for (yr in sort(unique(harm_21$election_year))) {
  if (yr == 2021) next
  
  ags_yr <- harm_21 |>
    filter(election_year == yr) |>
    distinct(ags) |>
    pull(ags)
  
  missing <- setdiff(ags_2021, ags_yr)
  extra <- setdiff(ags_yr, ags_2021)
  
  missing_by_year <- missing_by_year |>
    bind_rows(tibble(
      election_year = yr,
      n_municipalities = length(ags_yr),
      n_missing_from_2021 = length(missing),
      n_extra_not_in_2021 = length(extra),
      missing_ags = list(missing),
      extra_ags = list(extra)
    ))
  
  cat(sprintf("Year %d:\n", yr))
  cat(sprintf("  Municipalities: %d (expected: %d)\n", length(ags_yr), n_expected))
  cat(sprintf("  Missing from 2021: %d\n", length(missing)))
  cat(sprintf("  Extra (not in 2021): %d\n", length(extra)))
  
  if (length(missing) > 0 && length(missing) <= 20) {
    cat("  Missing AGS:", paste(missing, collapse = ", "), "\n")
  } else if (length(missing) > 20) {
    cat("  First 20 missing AGS:", paste(missing[1:20], collapse = ", "), "\n")
  }
  
  if (length(extra) > 0 && length(extra) <= 20) {
    cat("  Extra AGS:", paste(extra, collapse = ", "), "\n")
  } else if (length(extra) > 20) {
    cat("  First 20 extra AGS:", paste(extra[1:20], collapse = ", "), "\n")
  }
  cat("\n")
}

# Analyze missing municipalities --------------------------------------------

cat(strrep("=", 70), "\n")
cat("ANALYZING MISSING MUNICIPALITIES\n")
cat(strrep("=", 70), "\n\n")

# Unnest missing AGS
missing_ags_all <- missing_by_year |>
  select(election_year, missing_ags) |>
  unnest(missing_ags) |>
  rename(ags = missing_ags)

# Count how often each AGS is missing
missing_frequency <- missing_ags_all |>
  count(ags, sort = TRUE)

cat("Municipalities that are missing in some years:\n")
cat("Total unique missing AGS:", nrow(missing_frequency), "\n\n")

cat("Top 20 most frequently missing AGS:\n")
print(missing_frequency |> head(20))

# Check if these AGS appear in any year
cat("\nChecking if missing AGS appear in other years:\n")
for (ags_check in missing_frequency$ags[1:min(10, nrow(missing_frequency))]) {
  years_present <- harm_21 |>
    filter(ags == ags_check) |>
    distinct(election_year) |>
    pull(election_year)
  
  cat(sprintf("AGS %s: appears in years %s\n", 
              ags_check, 
              paste(sort(years_present), collapse = ", ")))
}

# Analyze extra municipalities ----------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("ANALYZING EXTRA MUNICIPALITIES (not in 2021)\n")
cat(strrep("=", 70), "\n\n")

# Unnest extra AGS
extra_ags_all <- missing_by_year |>
  select(election_year, extra_ags) |>
  unnest(extra_ags) |>
  rename(ags = extra_ags)

if (nrow(extra_ags_all) > 0) {
  cat("Municipalities that appear in some years but NOT in 2021:\n")
  cat("Total unique extra AGS:", length(unique(extra_ags_all$ags)), "\n\n")
  
  extra_frequency <- extra_ags_all |>
    count(ags, sort = TRUE)
  
  cat("Top 20 most frequently appearing extra AGS:\n")
  print(extra_frequency |> head(20))
  
  # Check what years these appear in
  cat("\nChecking years where extra AGS appear:\n")
  for (ags_check in extra_frequency$ags[1:min(10, nrow(extra_frequency))]) {
    years_present <- harm_21 |>
      filter(ags == ags_check) |>
      distinct(election_year) |>
      pull(election_year)
    
    cat(sprintf("AGS %s: appears in years %s\n", 
                ags_check, 
                paste(sort(years_present), collapse = ", ")))
  }
} else {
  cat("No extra municipalities found (all municipalities in other years also exist in 2021).\n")
}

# Check crosswalk coverage --------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING CROSSWALK COVERAGE\n")
cat(strrep("=", 70), "\n\n")

# Load crosswalk
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# For each year, check which ags_21 should exist based on crosswalk
for (yr in sort(unique(harm_21$election_year))) {
  if (yr == 2021) {
    # 2021: all AGS are ags_21
    expected_ags <- ags_2021
  } else if (yr <= 2020) {
    # Use crosswalk
    expected_ags <- cw_21 |>
      filter(year == yr) |>
      distinct(ags_21) |>
      pull(ags_21)
  } else if (yr == 2025) {
    # For 2025, need to use the 2025->2021 crosswalk
    # This is complex, so we'll skip for now and focus on the issue
    expected_ags <- NULL
  } else {
    expected_ags <- NULL
  }
  
  if (!is.null(expected_ags)) {
    actual_ags <- harm_21 |>
      filter(election_year == yr) |>
      distinct(ags) |>
      pull(ags)
    
    missing_in_cw <- setdiff(ags_2021, expected_ags)
    extra_in_cw <- setdiff(expected_ags, ags_2021)
    
    cat(sprintf("Year %d:\n", yr))
    cat(sprintf("  Expected ags_21 from crosswalk: %d\n", length(expected_ags)))
    cat(sprintf("  Actual ags in harm_21: %d\n", length(actual_ags)))
    cat(sprintf("  ags_2021 missing in crosswalk: %d\n", length(missing_in_cw)))
    cat(sprintf("  Extra ags in crosswalk (not in 2021): %d\n", length(extra_in_cw)))
    
    if (length(missing_in_cw) > 0 && length(missing_in_cw) <= 10) {
      cat("  Missing in crosswalk:", paste(missing_in_cw, collapse = ", "), "\n")
    }
    cat("\n")
  }
}

# Summary -------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("The harmonized dataset should have exactly", n_expected, "municipalities for all years.\n")
cat("However, the actual counts vary:\n\n")

print(muni_count |>
  mutate(
    difference = n_municipalities - n_expected,
    pct_diff = round(100 * difference / n_expected, 2)
  ))

cat("\nPossible causes:\n")
cat("1. Some municipalities from 2021 are missing in earlier years (crosswalk issue)\n")
cat("2. Some municipalities appear in earlier years but not in 2021 (should not happen)\n")
cat("3. The harmonization process is not correctly aggregating all source municipalities\n\n")

# Save results --------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat("SAVING RESULTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("data/data_checks")) {
  dir.create("data/data_checks", recursive = TRUE)
}

write_csv(missing_by_year |> select(-missing_ags, -extra_ags), 
          "data/data_checks/harm_21_stability_summary.csv")

write_csv(missing_frequency, 
          "data/data_checks/harm_21_missing_frequency.csv")

if (nrow(extra_ags_all) > 0) {
  write_csv(extra_frequency, 
            "data/data_checks/harm_21_extra_frequency.csv")
}

cat("Results saved.\n")
cat("\nScript completed!\n")

