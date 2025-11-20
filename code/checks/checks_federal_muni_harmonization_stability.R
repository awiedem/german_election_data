### Check stability and discrepancies in harmonized federal election data
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR",
  "readxl"
)

conflict_prefer("filter", "dplyr")

cat("=", strrep("=", 70), "\n")
cat("FEDERAL ELECTIONS: HARMONIZATION STABILITY CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load harmonized dataset ---------------------------------------------------

cat("Loading harmonized dataset...\n")
harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = haschaR::pad_zero_conditional(ags, 7))

cat("Municipality count by election year:\n")
muni_count <- harm_21 |>
  count(election_year, name = "n_municipalities") |>
  arrange(election_year)

print(muni_count)

# Expected: all years should have the same number as 2021
n_expected <- muni_count |> filter(election_year == 2021) |> pull(n_municipalities)
cat("\nExpected number of municipalities (from 2021):", n_expected, "\n\n")

# Get municipalities in 2021
ags_2021 <- harm_21 |>
  filter(election_year == 2021) |>
  distinct(ags) |>
  pull(ags)

cat("Total municipalities in 2021:", length(ags_2021), "\n\n")

# Find missing and extra municipalities by year -----------------------------

cat(strrep("=", 70), "\n")
cat("FINDING MISSING AND EXTRA MUNICIPALITIES BY YEAR\n")
cat(strrep("=", 70), "\n\n")

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
      n_extra_not_in_2021 = length(extra)
    ))
  
  cat(sprintf("Year %d:\n", yr))
  cat(sprintf("  Municipalities: %d (expected: %d)\n", length(ags_yr), n_expected))
  cat(sprintf("  Missing from 2021: %d\n", length(missing)))
  cat(sprintf("  Extra (not in 2021): %d\n", length(extra)))
  cat("\n")
}

print(missing_by_year)

# Check crosswalk for extra municipalities -----------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING CROSSWALK FOR EXTRA MUNICIPALITIES\n")
cat(strrep("=", 70), "\n\n")

# Load 2021 actual data
ags21_actual <- read_excel(
  path = "data/crosswalks/raw/31122021_Auszug_GV.xlsx", 
  sheet = 2
) |>
  dplyr::select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`
  ) |>
  mutate(
    Land = haschaR::pad_zero_conditional(Land, 1),
    Kreis = haschaR::pad_zero_conditional(Kreis, 1),
    Gemeinde = haschaR::pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = haschaR::pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  slice(6:16065) |>
  dplyr::filter(!is.na(Gemeinde)) |>
  distinct(ags) |>
  pull(ags)

cat("Municipalities in actual 2021 data:", length(ags21_actual), "\n\n")

# Load crosswalk
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = haschaR::pad_zero_conditional(ags, 7),
    ags_21 = haschaR::pad_zero_conditional(ags_21, 7)
  )

# Get all unique ags_21 from crosswalk
ags21_in_cw <- cw_21 |>
  distinct(ags_21) |>
  pull(ags_21)

cat("Unique ags_21 in crosswalk:", length(ags21_in_cw), "\n")

# Find ags_21 in crosswalk but not in actual 2021 data
ags21_extra_in_cw <- setdiff(ags21_in_cw, ags21_actual)
ags21_missing_in_cw <- setdiff(ags21_actual, ags21_in_cw)

cat("ags_21 in crosswalk but NOT in 2021 data:", length(ags21_extra_in_cw), "\n")
cat("ags_21 in 2021 data but NOT in crosswalk:", length(ags21_missing_in_cw), "\n\n")

if (length(ags21_extra_in_cw) > 0) {
  cat("⚠️  PROBLEM: Crosswalk contains ags_21 that don't exist in 2021 data!\n")
  cat("These cause extra municipalities to appear in harmonized data.\n\n")
  
  if (length(ags21_extra_in_cw) <= 50) {
    cat("Extra ags_21 in crosswalk:\n")
    print(sort(ags21_extra_in_cw))
  } else {
    cat("First 50 extra ags_21 in crosswalk:\n")
    print(sort(ags21_extra_in_cw)[1:50])
  }
  
  # Check which years these appear in
  cat("\nChecking which years these extra ags_21 appear in crosswalk:\n")
  extra_by_year_cw <- cw_21 |>
    filter(ags_21 %in% ags21_extra_in_cw) |>
    distinct(ags_21, year) |>
    count(ags_21, sort = TRUE) |>
    head(20)
  
  print(extra_by_year_cw)
  
  # Check what source AGS map to these extra ags_21 (example for 1990)
  cat("\nSource AGS that map to these extra ags_21 (example for 1990):\n")
  source_mapping <- cw_21 |>
    filter(year == 1990, ags_21 %in% ags21_extra_in_cw) |>
    select(ags, ags_21) |>
    distinct() |>
    arrange(ags_21) |>
    head(30)
  
  print(source_mapping)
}

# Check if extra municipalities appear as source AGS ------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF EXTRA MUNICIPALITIES APPEAR AS SOURCE AGS\n")
cat(strrep("=", 70), "\n\n")

# Get extra municipalities for 1990
ags_1990 <- harm_21 |>
  filter(election_year == 1990) |>
  distinct(ags) |>
  pull(ags)

extra_1990 <- setdiff(ags_1990, ags_2021)

cat("Extra municipalities in 1990:", length(extra_1990), "\n")

# Check crosswalk for 1990
cw_1990 <- cw_21 |>
  filter(year == 1990)

# Check if these extra AGS appear as source AGS
extra_as_source <- cw_1990 |>
  filter(ags %in% extra_1990) |>
  distinct(ags, ags_21) |>
  arrange(ags)

if (nrow(extra_as_source) > 0) {
  cat(sprintf("Extra AGS that appear as source AGS in crosswalk: %d\n", nrow(extra_as_source)))
  cat("These source AGS map to the following ags_21:\n")
  print(extra_as_source |> head(30))
  
  # Check if these mapped ags_21 exist in 2021
  mapped_ags_21 <- unique(extra_as_source$ags_21)
  missing_mapped <- setdiff(mapped_ags_21, ags_2021)
  
  if (length(missing_mapped) > 0) {
    cat(sprintf("\n⚠️  PROBLEM: %d of these mapped ags_21 do NOT exist in 2021!\n", length(missing_mapped)))
    cat("These are the root cause of extra municipalities.\n")
    cat("First 20:", paste(missing_mapped[1:min(20, length(missing_mapped))], collapse = ", "), "\n")
  }
}

# Check when extra ags_21 disappear ------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING WHEN EXTRA ags_21 DISAPPEAR FROM CROSSWALK\n")
cat(strrep("=", 70), "\n\n")

if (length(ags21_extra_in_cw) > 0) {
  # Check in which years these extra ags_21 appear
  example_extra <- ags21_extra_in_cw[1:min(5, length(ags21_extra_in_cw))]
  
  for (ags_check in example_extra) {
    cat(sprintf("\nAGS %s:\n", ags_check))
    
    # Check as source AGS
    as_source <- cw_21 |>
      filter(ags == ags_check) |>
      select(year, ags, ags_21) |>
      distinct() |>
      arrange(year)
    
    if (nrow(as_source) > 0) {
      cat("  Appears as source AGS, maps to ags_21:\n")
      print(as_source |> head(10))
    }
    
    # Check as ags_21
    as_ags21 <- cw_21 |>
      filter(ags_21 == ags_check) |>
      select(year, ags, ags_21) |>
      distinct() |>
      arrange(year)
    
    if (nrow(as_ags21) > 0) {
      cat("  Appears as ags_21 (target):\n")
      print(as_ags21 |> head(10))
    }
    
    # Check if it exists in 2021
    exists_2021 <- ags_check %in% ags_2021
    cat(sprintf("  Exists in 2021 data: %s\n", exists_2021))
  }
}

# Summary -------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Root cause of discrepancies:\n")
cat("1. The crosswalk contains", length(ags21_extra_in_cw), "ags_21 that don't exist in actual 2021 data\n")
cat("2. These ags_21 appear when harmonizing earlier years\n")
cat("3. This causes extra municipalities to appear in harm_21\n")
cat("4. The crosswalk was likely created before some municipalities were dissolved/merged\n\n")

cat("Impact:\n")
print(missing_by_year)

cat("\nSolution:\n")
cat("The crosswalk needs to be updated to only include ags_21 that actually exist in 2021.\n")
cat("OR the harmonization process should filter out ags_21 that don't exist in 2021.\n")
cat("(See code/federal_elections/municipality_level/02_federal_muni_harm_21.R, line 207)\n\n")

# Save results --------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat("SAVING RESULTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("data/data_checks")) {
  dir.create("data/data_checks", recursive = TRUE)
}

write_csv(missing_by_year, "data/data_checks/harm_21_stability_summary.csv")
cat("Stability summary saved.\n")

if (length(ags21_extra_in_cw) > 0) {
  write_csv(
    tibble(ags_21 = ags21_extra_in_cw),
    "data/data_checks/ags21_extra_in_crosswalk.csv"
  )
  cat("Extra ags_21 in crosswalk saved.\n")
}

if (length(ags21_missing_in_cw) > 0) {
  write_csv(
    tibble(ags_21 = ags21_missing_in_cw),
    "data/data_checks/ags21_missing_in_crosswalk.csv"
  )
  cat("Missing ags_21 in crosswalk saved.\n")
}

if (nrow(extra_as_source) > 0) {
  write_csv(extra_as_source, "data/data_checks/extra_ags_as_source_mapping.csv")
  cat("Extra AGS as source mapping saved.\n")
}

cat("\nScript completed!\n")

