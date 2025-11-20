### Check for missing values in harmonized federal election data
# Diagnose missing values in federal_muni_harm_21 vs federal_muni_harm_25
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table"
)

# Load data.table functions
library(data.table)

conflict_prefer("filter", "dplyr")

# Helper function to pad AGS
pad_zero_conditional <- function(x, n) {
  x <- as.character(x)
  x <- str_pad(x, width = n, side = "left", pad = "0")
  return(x)
}

# Load harmonized datasets ------------------------------------------------

cat("Loading harmonized datasets...\n")

harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

harm_25 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_25.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

table(harm_21$election_year)
table(harm_25$election_year)

harm_21 |>
  filter(ags == "14626110" & election_year >= 2021) |>
  glimpse()

harm_25 |>
  filter(ags == "14626110" & election_year >= 2021) |>
  glimpse()

# Load crosswalks for context ----------------------------------------------

cat("Loading crosswalk files...\n")

cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds")
cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
cw_25_to_21 <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  filter(year == 2025) |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Define key variables to check for missing values ------------------------

vote_vars <- c(
  "eligible_voters", "number_voters", "valid_votes",
  "cdu", "spd", "afd", "fdp", "gruene", "linke_pds", "cdu_csu"
)

# Function to check missing values for a municipality ---------------------

check_muni_missing <- function(ags_code, verbose = TRUE) {
  ags_code <- pad_zero_conditional(ags_code, 7)
  
  if (verbose) {
    cat("\n", strrep("=", 70), "\n")
    cat("Checking municipality:", ags_code, "\n")
    cat(strrep("=", 70), "\n")
  }
  
  # Get data for this municipality
  muni_21 <- harm_21 |>
    filter(ags == ags_code) |>
    arrange(election_year)
  
  muni_25 <- harm_25 |>
    filter(ags == ags_code) |>
    arrange(election_year)
  
  # Check if municipality exists in both datasets
  if (nrow(muni_21) == 0 && nrow(muni_25) == 0) {
    cat("WARNING: Municipality", ags_code, "not found in either dataset!\n")
    return(NULL)
  }
  
  if (nrow(muni_21) == 0) {
    cat("WARNING: Municipality", ags_code, "not found in harm_21 dataset!\n")
    cat("Found in harm_25 with", nrow(muni_25), "election years.\n")
    return(list(ags = ags_code, issue = "missing_in_21", data_25 = muni_25))
  }
  
  if (nrow(muni_25) == 0) {
    cat("WARNING: Municipality", ags_code, "not found in harm_25 dataset!\n")
    cat("Found in harm_21 with", nrow(muni_21), "election years.\n")
    return(list(ags = ags_code, issue = "missing_in_25", data_21 = muni_21))
  }
  
  # Get all election years
  all_years <- unique(c(muni_21$election_year, muni_25$election_year))
  
  # Check for missing values in each election year
  missing_summary <- tibble()
  
  for (yr in sort(all_years)) {
    row_21 <- muni_21 |> filter(election_year == yr)
    row_25 <- muni_25 |> filter(election_year == yr)
    
    # Check if year exists in both
    exists_21 <- nrow(row_21) > 0
    exists_25 <- nrow(row_25) > 0
    
    if (!exists_21 && !exists_25) next
    
    # Count missing values in vote variables
    if (exists_21) {
      missing_21 <- row_21 |>
        select(all_of(vote_vars)) |>
        summarise_all(~ sum(is.na(.))) |>
        rowSums()
    } else {
      missing_21 <- NA
    }
    
    if (exists_25) {
      missing_25 <- row_25 |>
        select(all_of(vote_vars)) |>
        summarise_all(~ sum(is.na(.))) |>
        rowSums()
    } else {
      missing_25 <- NA
    }
    
    # Check if any key variables are completely missing
    if (exists_21) {
      all_missing_21 <- row_21 |>
        select(all_of(vote_vars)) |>
        summarise_all(~ all(is.na(.))) |>
        rowSums() > 0
    } else {
      all_missing_21 <- TRUE
    }
    
    if (exists_25) {
      all_missing_25 <- row_25 |>
        select(all_of(vote_vars)) |>
        summarise_all(~ all(is.na(.))) |>
        rowSums() > 0
    } else {
      all_missing_25 <- TRUE
    }
    
    missing_summary <- missing_summary |>
      bind_rows(tibble(
        election_year = yr,
        exists_21 = exists_21,
        exists_25 = exists_25,
        missing_count_21 = ifelse(exists_21, missing_21, NA),
        missing_count_25 = ifelse(exists_25, missing_25, NA),
        all_missing_21 = all_missing_21,
        all_missing_25 = all_missing_25
      ))
  }
  
  # Print summary
  if (verbose) {
    cat("\nElection Year Coverage:\n")
    print(missing_summary)
    
    # Identify problematic years
    problems <- missing_summary |>
      filter(
        (!exists_21 & exists_25) | (exists_21 & !exists_25) |
        (exists_21 & exists_25 & (all_missing_21 != all_missing_25))
      )
    
    if (nrow(problems) > 0) {
      cat("\n⚠️  PROBLEMATIC YEARS:\n")
      print(problems)
    } else {
      cat("\n✓ No obvious issues found for this municipality.\n")
    }
  }
  
  return(list(
    ags = ags_code,
    summary = missing_summary,
    data_21 = muni_21,
    data_25 = muni_25
  ))
}

# Systematic check: Find municipalities with missing values --------------

cat("\n", strrep("=", 70), "\n")
cat("SYSTEMATIC CHECK: Finding municipalities with missing values\n")
cat(strrep("=", 70), "\n\n")

# Check for missing values in each dataset
check_missing_21 <- harm_21 |>
  group_by(ags, election_year) |>
  summarise(
    n_missing = sum(is.na(eligible_voters), is.na(number_voters), is.na(valid_votes)),
    all_missing = all(is.na(eligible_voters)) & all(is.na(number_voters)) & all(is.na(valid_votes)),
    .groups = "drop"
  ) |>
  filter(all_missing | n_missing > 0)

check_missing_25 <- harm_25 |>
  group_by(ags, election_year) |>
  summarise(
    n_missing = sum(is.na(eligible_voters), is.na(number_voters), is.na(valid_votes)),
    all_missing = all(is.na(eligible_voters)) & all(is.na(number_voters)) & all(is.na(valid_votes)),
    .groups = "drop"
  ) |>
  filter(all_missing | n_missing > 0)

# Find municipalities that appear in one but not the other
ags_21 <- unique(harm_21$ags)
ags_25 <- unique(harm_25$ags)

only_in_21 <- setdiff(ags_21, ags_25)
only_in_25 <- setdiff(ags_25, ags_21)

cat("Municipalities only in harm_21:", length(only_in_21), "\n")
cat("Municipalities only in harm_25:", length(only_in_25), "\n\n")

# Check for municipalities with missing 2025 data in harm_21
missing_2025_in_21 <- harm_21 |>
  filter(election_year == 2025) |>
  filter(is.na(eligible_voters) | is.na(number_voters) | is.na(valid_votes)) |>
  select(ags, election_year) |>
  distinct()

cat("Municipalities with missing 2025 data in harm_21:", nrow(missing_2025_in_21), "\n")

# Check for municipalities with missing pre-2025 data in harm_25
missing_pre_2025_in_25 <- harm_25 |>
  filter(election_year < 2025) |>
  filter(is.na(eligible_voters) | is.na(number_voters) | is.na(valid_votes)) |>
  select(ags, election_year) |>
  distinct()

cat("Municipalities with missing pre-2025 data in harm_25:", nrow(missing_pre_2025_in_25), "\n\n")

# Find municipalities that have the pattern described by user
# (missing 2025 in harm_21, but missing pre-2025 in harm_25)
problematic_ags <- intersect(
  missing_2025_in_21$ags,
  missing_pre_2025_in_25$ags
)

cat("Municipalities with BOTH issues (missing 2025 in harm_21 AND missing pre-2025 in harm_25):", 
    length(problematic_ags), "\n\n")

# Check specific example: Görlitz (14626110) -------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING SPECIFIC EXAMPLE: Görlitz (AGS: 14626110)\n")
cat(strrep("=", 70), "\n")

goerlitz_check <- check_muni_missing("14626110", verbose = TRUE)

# Check crosswalk information for Görlitz
cat("\nCrosswalk Information for Görlitz:\n")
cat("AGS 2021:", "14626110", "\n")

# Check if Görlitz exists in crosswalks
cw_goerlitz_21_23 <- cw_21_23 |>
  filter(ags_2021 == 14626110 | ags_2023 == 14626110)

cw_goerlitz_23_25 <- cw_23_25 |>
  filter(ags == 14626110 | ags_25 == 14626110)

if (nrow(cw_goerlitz_21_23) > 0) {
  cat("\n2021-2023 Crosswalk:\n")
  print(cw_goerlitz_21_23)
}

if (nrow(cw_goerlitz_23_25) > 0) {
  cat("\n2023-2025 Crosswalk:\n")
  print(cw_goerlitz_23_25)
}

# Check city-states: Berlin, Hamburg, Bremen -----------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING CITY-STATES: Berlin, Hamburg, Bremen\n")
cat(strrep("=", 70), "\n\n")

# Define city-state AGS codes
city_states <- tibble(
  name = c("Berlin", "Hamburg", "Bremen (Nord)", "Bremen (Süd)"),
  ags = c("11000000", "02000000", "04011000", "04012000"),
  state = c("11", "02", "04", "04")
)

# Check each city-state
for (i in 1:nrow(city_states)) {
  city_name <- city_states$name[i]
  ags_code <- city_states$ags[i]
  
  cat("\n", strrep("-", 70), "\n")
  cat(sprintf("Checking %s (AGS: %s)\n", city_name, ags_code))
  cat(strrep("-", 70), "\n")
  
  # Get data for this city-state
  city_21 <- harm_21 |>
    filter(ags == ags_code) |>
    arrange(election_year)
  
  city_25 <- harm_25 |>
    filter(ags == ags_code) |>
    arrange(election_year)
  
  # Check coverage
  years_21 <- sort(unique(city_21$election_year))
  years_25 <- sort(unique(city_25$election_year))
  all_years <- sort(unique(c(harm_21$election_year, harm_25$election_year)))
  
  cat(sprintf("\nElection years in harm_21: %s\n", paste(years_21, collapse = ", ")))
  cat(sprintf("Election years in harm_25: %s\n", paste(years_25, collapse = ", ")))
  cat(sprintf("Expected years: %s\n", paste(all_years, collapse = ", ")))
  
  # Check for missing years
  missing_in_21 <- setdiff(all_years, years_21)
  missing_in_25 <- setdiff(all_years, years_25)
  
  if (length(missing_in_21) > 0) {
    cat(sprintf("⚠️  WARNING: Missing years in harm_21: %s\n", paste(missing_in_21, collapse = ", ")))
  } else {
    cat("✓ All expected years present in harm_21\n")
  }
  
  if (length(missing_in_25) > 0) {
    cat(sprintf("⚠️  WARNING: Missing years in harm_25: %s\n", paste(missing_in_25, collapse = ", ")))
  } else {
    cat("✓ All expected years present in harm_25\n")
  }
  
  # Check for missing vote variables
  if (nrow(city_21) > 0) {
    missing_vars_21 <- city_21 |>
      select(all_of(vote_vars)) |>
      summarise_all(~sum(is.na(.))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
      filter(n_missing > 0)
    
    if (nrow(missing_vars_21) > 0) {
      cat("\n⚠️  Missing vote variables in harm_21:\n")
      print(missing_vars_21)
    } else {
      cat("\n✓ No missing vote variables in harm_21\n")
    }
  }
  
  if (nrow(city_25) > 0) {
    missing_vars_25 <- city_25 |>
      select(all_of(vote_vars)) |>
      summarise_all(~sum(is.na(.))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
      filter(n_missing > 0)
    
    if (nrow(missing_vars_25) > 0) {
      cat("\n⚠️  Missing vote variables in harm_25:\n")
      print(missing_vars_25)
    } else {
      cat("\n✓ No missing vote variables in harm_25\n")
    }
  }
  
  # Summary table for this city-state
  if (nrow(city_21) > 0 && nrow(city_25) > 0) {
    cat("\nSummary by election year:\n")
    summary_city <- city_21 |>
      select(ags, election_year, eligible_voters, valid_votes, cdu, spd, afd) |>
      left_join(
        city_25 |>
          select(ags, election_year, eligible_voters, valid_votes, cdu, spd, afd),
        by = c("ags", "election_year"),
        suffix = c("_21", "_25")
      ) |>
      mutate(
        has_data_21 = !is.na(eligible_voters_21) & !is.na(valid_votes_21),
        has_data_25 = !is.na(eligible_voters_25) & !is.na(valid_votes_25)
      ) |>
      select(ags, election_year, has_data_21, has_data_25)
    
    print(summary_city)
  }
}

cat("\n", strrep("=", 70), "\n")
cat("CITY-STATES CHECK COMPLETE\n")
cat(strrep("=", 70), "\n\n")

# Detailed analysis of problematic municipalities ------------------------

if (length(problematic_ags) > 0) {
  cat("\n", strrep("=", 70), "\n")
  cat("DETAILED ANALYSIS OF PROBLEMATIC MUNICIPALITIES\n")
  cat(strrep("=", 70), "\n\n")
  
  # Analyze first 10 problematic municipalities
  n_check <- min(10, length(problematic_ags))
  
  for (i in 1:n_check) {
    ags_check <- problematic_ags[i]
    result <- check_muni_missing(ags_check, verbose = TRUE)
    
    # Check crosswalk chain
    cat("\nCrosswalk Chain Analysis:\n")
    
    # Check 2021-2023
    cw_21_23_check <- cw_21_23 |>
      filter(ags_2021 == as.numeric(ags_check) | ags_2023 == as.numeric(ags_check))
    
    # Check 2023-2025
    cw_23_25_check <- cw_23_25 |>
      filter(ags == as.numeric(ags_check) | ags_25 == as.numeric(ags_check))
    
    if (nrow(cw_21_23_check) > 0) {
      cat("2021-2023 mapping found:\n")
      print(cw_21_23_check |> head())
    } else {
      cat("⚠️  No 2021-2023 mapping found!\n")
    }
    
    if (nrow(cw_23_25_check) > 0) {
      cat("2023-2025 mapping found:\n")
      print(cw_23_25_check |> head())
    } else {
      cat("⚠️  No 2023-2025 mapping found!\n")
    }
    
    cat("\n")
  }
}

# Analyze municipality count stability --------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("ANALYZING MUNICIPALITY COUNT STABILITY\n")
cat(strrep("=", 70), "\n\n")

# Count municipalities per year
muni_count_21 <- harm_21 |>
  count(election_year, name = "n_municipalities") |>
  arrange(election_year)

muni_count_25 <- harm_25 |>
  count(election_year, name = "n_municipalities") |>
  arrange(election_year)

cat("Municipality count per year in harm_21:\n")
print(muni_count_21)

cat("\nMunicipality count per year in harm_25:\n")
print(muni_count_25)

# Find municipalities that appear/disappear across years
cat("\n", strrep("-", 70), "\n")
cat("Finding municipalities that appear or disappear across years...\n")
cat(strrep("-", 70), "\n\n")

# For harm_21: Find municipalities missing in specific years
muni_by_year_21 <- harm_21 |>
  select(ags, election_year) |>
  distinct() |>
  group_by(ags) |>
  summarise(
    years_present = paste(sort(election_year), collapse = ", "),
    n_years = n(),
    .groups = "drop"
  )

# Expected years (all years from 1990 to 2025)
expected_years <- sort(unique(harm_21$election_year))
expected_n_years <- length(expected_years)

# Find municipalities with incomplete coverage
incomplete_21 <- muni_by_year_21 |>
  filter(n_years < expected_n_years) |>
  arrange(desc(n_years))

cat("Municipalities with incomplete year coverage in harm_21:", nrow(incomplete_21), "\n")
cat("Expected years:", paste(expected_years, collapse = ", "), "\n")
cat("Expected number of years:", expected_n_years, "\n\n")

if (nrow(incomplete_21) > 0) {
  cat("Top 20 municipalities with most missing years:\n")
  print(incomplete_21 |> head(20))
  
  # Analyze which years are most commonly missing
  # Create full grid of all municipalities and all years
  all_ags_21 <- unique(harm_21$ags)
  full_grid_21 <- expand_grid(ags = all_ags_21, election_year = expected_years)
  
  # Find which combinations are missing
  actual_combinations_21 <- harm_21 |>
    select(ags, election_year) |>
    distinct()
  
  missing_combinations_21 <- full_grid_21 |>
    anti_join(actual_combinations_21, by = c("ags", "election_year"))
  
  missing_years_analysis_21 <- missing_combinations_21 |>
    count(election_year, name = "n_missing") |>
    arrange(desc(n_missing))
  
  cat("\nYears with most missing municipalities in harm_21:\n")
  print(missing_years_analysis_21)
}

# Same for harm_25
muni_by_year_25 <- harm_25 |>
  select(ags, election_year) |>
  distinct() |>
  group_by(ags) |>
  summarise(
    years_present = paste(sort(election_year), collapse = ", "),
    n_years = n(),
    .groups = "drop"
  )

expected_years_25 <- sort(unique(harm_25$election_year))
expected_n_years_25 <- length(expected_years_25)

incomplete_25 <- muni_by_year_25 |>
  filter(n_years < expected_n_years_25) |>
  arrange(desc(n_years))

cat("\nMunicipalities with incomplete year coverage in harm_25:", nrow(incomplete_25), "\n")
cat("Expected years:", paste(expected_years_25, collapse = ", "), "\n")
cat("Expected number of years:", expected_n_years_25, "\n\n")

if (nrow(incomplete_25) > 0) {
  cat("Top 20 municipalities with most missing years:\n")
  print(incomplete_25 |> head(20))
  
  # Analyze which years are most commonly missing
  # Create full grid of all municipalities and all years
  all_ags_25 <- unique(harm_25$ags)
  full_grid_25 <- expand_grid(ags = all_ags_25, election_year = expected_years_25)
  
  # Find which combinations are missing
  actual_combinations_25 <- harm_25 |>
    select(ags, election_year) |>
    distinct()
  
  missing_combinations_25 <- full_grid_25 |>
    anti_join(actual_combinations_25, by = c("ags", "election_year"))
  
  missing_years_analysis_25 <- missing_combinations_25 |>
    count(election_year, name = "n_missing") |>
    arrange(desc(n_missing))
  
  cat("\nYears with most missing municipalities in harm_25:\n")
  print(missing_years_analysis_25)
}

# Check if the variation is due to crosswalk issues
cat("\n", strrep("-", 70), "\n")
cat("Checking if variation is due to crosswalk coverage...\n")
cat(strrep("-", 70), "\n\n")

# Load crosswalks to see what municipalities should exist
# For harm_21: use ags_crosswalks.csv which has ags_21
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# For harm_25: use ags_1990_to_2025_crosswalk.rds which has ags_25
cw_25 <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

# For harm_21: Check which municipalities should exist based on crosswalk
expected_ags_21 <- cw_21 |>
  filter(year %in% expected_years) |>
  distinct(ags_21, year) |>
  mutate(ags = ags_21) |>
  distinct(ags, year) |>
  count(year, name = "n_expected")

cat("Expected municipality count per year (based on crosswalk) for harm_21:\n")
print(expected_ags_21)

# Compare with actual counts
comparison_21 <- muni_count_21 |>
  left_join(expected_ags_21, by = c("election_year" = "year")) |>
  mutate(
    difference = n_municipalities - n_expected,
    pct_diff = round(100 * difference / n_expected, 2)
  )

cat("\nComparison: Actual vs Expected (harm_21):\n")
print(comparison_21)

# For harm_25: Check which municipalities should exist
expected_ags_25 <- cw_25 |>
  filter(year %in% expected_years_25) |>
  distinct(ags_25, year) |>
  mutate(ags = ags_25) |>
  distinct(ags, year) |>
  count(year, name = "n_expected")

cat("\nExpected municipality count per year (based on crosswalk) for harm_25:\n")
print(expected_ags_25)

comparison_25 <- muni_count_25 |>
  left_join(expected_ags_25, by = c("election_year" = "year")) |>
  mutate(
    difference = n_municipalities - n_expected,
    pct_diff = round(100 * difference / n_expected, 2)
  )

cat("\nComparison: Actual vs Expected (harm_25):\n")
print(comparison_25)

# Find municipalities that are in crosswalk but missing in harmonized data
cat("\n", strrep("-", 70), "\n")
cat("Finding municipalities in crosswalk but missing in harmonized data...\n")
cat(strrep("-", 70), "\n\n")

# For a specific year (e.g., 2025) check what's missing
# Note: cw_21 only goes up to 2020, so we need to use the 2025 crosswalk chain
year_to_check <- 2025

# For harm_21: Need to use the chain 2025 -> 2023 -> 2021
# Load the 2023-2025 crosswalk and map to 2021
cw_23_25_check <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  as_tibble() |>
  filter(year == 2023) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

cw_21_23_check <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds") |>
  as_tibble() |>
  mutate(
    ags_2021 = pad_zero_conditional(ags_2021, 7),
    ags_2023 = pad_zero_conditional(ags_2023, 7)
  )

# Chain: 2025 -> 2023 -> 2021
cw_25_to_21_check <- cw_23_25_check |>
  left_join(cw_21_23_check, by = c("ags" = "ags_2023")) |>
  distinct(ags_2021) |>
  pull(ags_2021)

expected_ags_year_21 <- cw_25_to_21_check

actual_ags_year_21 <- harm_21 |>
  filter(election_year == year_to_check) |>
  distinct(ags) |>
  pull(ags)

missing_in_harm_21 <- setdiff(expected_ags_year_21, actual_ags_year_21)

cat("Year", year_to_check, "- harm_21:\n")
cat("Expected municipalities (from crosswalk):", length(expected_ags_year_21), "\n")
cat("Actual municipalities in harmonized data:", length(actual_ags_year_21), "\n")
cat("Missing municipalities:", length(missing_in_harm_21), "\n")

if (length(missing_in_harm_21) > 0 && length(missing_in_harm_21) <= 20) {
  cat("\nMissing municipalities:\n")
  print(missing_in_harm_21)
} else if (length(missing_in_harm_21) > 20) {
  cat("\nFirst 20 missing municipalities:\n")
  print(missing_in_harm_21[1:20])
}

expected_ags_year_25 <- cw_25 |>
  filter(year == year_to_check) |>
  distinct(ags_25) |>
  mutate(ags = pad_zero_conditional(ags_25, 7)) |>
  pull(ags)

actual_ags_year_25 <- harm_25 |>
  filter(election_year == year_to_check) |>
  distinct(ags) |>
  pull(ags)

missing_in_harm_25 <- setdiff(expected_ags_year_25, actual_ags_year_25)

cat("\nYear", year_to_check, "- harm_25:\n")
cat("Expected municipalities (from crosswalk):", length(expected_ags_year_25), "\n")
cat("Actual municipalities in harmonized data:", length(actual_ags_year_25), "\n")
cat("Missing municipalities:", length(missing_in_harm_25), "\n")

if (length(missing_in_harm_25) > 0 && length(missing_in_harm_25) <= 20) {
  cat("\nMissing municipalities:\n")
  print(missing_in_harm_25)
} else if (length(missing_in_harm_25) > 20) {
  cat("\nFirst 20 missing municipalities:\n")
  print(missing_in_harm_25[1:20])
}

# Summary report ----------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY REPORT\n")
cat(strrep("=", 70), "\n\n")

cat("Total municipalities in harm_21:", length(ags_21), "\n")
cat("Total municipalities in harm_25:", length(ags_25), "\n")
cat("Municipalities in both:", length(intersect(ags_21, ags_25)), "\n\n")

cat("Missing 2025 data in harm_21:", nrow(missing_2025_in_21), "municipalities\n")
cat("Missing pre-2025 data in harm_25:", nrow(missing_pre_2025_in_25), "municipalities\n")
cat("Municipalities with both issues:", length(problematic_ags), "\n\n")

# Election year coverage
cat("Election year coverage in harm_21:\n")
print(harm_21 |> count(election_year) |> arrange(election_year))

cat("\nElection year coverage in harm_25:\n")
print(harm_25 |> count(election_year) |> arrange(election_year))

# Save results ------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("Saving diagnostic results...\n")
cat(strrep("=", 70), "\n\n")

# Create summary dataframe
diagnostic_summary <- tibble(
  ags = problematic_ags,
  issue_type = "missing_2025_in_21_and_missing_pre_2025_in_25"
) |>
  left_join(
    missing_2025_in_21 |> 
      rename(missing_2025_21 = election_year) |>
      select(ags),
    by = "ags"
  ) |>
  left_join(
    missing_pre_2025_in_25 |>
      group_by(ags) |>
      summarise(missing_years_25 = paste(sort(election_year), collapse = ", ")),
    by = "ags"
  )

write_csv(diagnostic_summary, "data/data_checks/federal_muni_missing_diagnostic.csv")

cat("Diagnostic summary saved to: data/data_checks/federal_muni_missing_diagnostic.csv\n")
cat("\nScript completed!\n")

