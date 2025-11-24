### Check which ags_21 in the crosswalk don't exist in actual 2021 data
# This explains why there are extra municipalities in harmonized data
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR"
)

conflict_prefer("filter", "dplyr")

cat("=", strrep("=", 70), "\n")
cat("CHECKING WHICH ags_21 IN CROSSWALK DON'T EXIST IN 2021 DATA\n")
cat("=", strrep("=", 70), "\n\n")

# Load 2021 data ------------------------------------------------------------

cat("Loading 2021 municipality data...\n")
ags21_actual <- read_excel(
  path = "data/crosswalks/raw/31122021_Auszug_GV.xlsx", 
  sheet = 2
) |>
  dplyr::select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    population = `...10`
  ) |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  slice(6:16065) |>
  dplyr::filter(!is.na(Gemeinde)) |>
  distinct(ags) |>
  pull(ags)

cat("Municipalities in actual 2021 data:", length(ags21_actual), "\n\n")

# Load crosswalk ------------------------------------------------------------

cat("Loading crosswalk...\n")
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# Get all unique ags_21 from crosswalk
ags21_in_cw <- cw_21 |>
  distinct(ags_21) |>
  pull(ags_21)

cat("Unique ags_21 in crosswalk:", length(ags21_in_cw), "\n\n")

# Find ags_21 in crosswalk but not in actual 2021 data ---------------------

cat(strrep("=", 70), "\n")
cat("FINDING ags_21 IN CROSSWALK BUT NOT IN 2021 DATA\n")
cat(strrep("=", 70), "\n\n")

ags21_extra_in_cw <- setdiff(ags21_in_cw, ags21_actual)
ags21_missing_in_cw <- setdiff(ags21_actual, ags21_in_cw)

cat("ags_21 in crosswalk but NOT in 2021 data:", length(ags21_extra_in_cw), "\n")
cat("ags_21 in 2021 data but NOT in crosswalk:", length(ags21_missing_in_cw), "\n\n")

if (length(ags21_extra_in_cw) > 0) {
  cat("These are the problematic ags_21 that cause extra municipalities:\n")
  if (length(ags21_extra_in_cw) <= 50) {
    print(sort(ags21_extra_in_cw))
  } else {
    cat("First 50:", paste(sort(ags21_extra_in_cw)[1:50], collapse = ", "), "\n")
  }
  
  # Check which years these appear in
  cat("\nChecking which years these extra ags_21 appear in crosswalk:\n")
  extra_by_year <- cw_21 |>
    filter(ags_21 %in% ags21_extra_in_cw) |>
    distinct(ags_21, year) |>
    count(ags_21, sort = TRUE) |>
    head(20)
  
  print(extra_by_year)
  
  # Check what source AGS map to these extra ags_21
  cat("\nSource AGS that map to these extra ags_21 (example for 1990):\n")
  source_mapping <- cw_21 |>
    filter(year == 1990, ags_21 %in% ags21_extra_in_cw) |>
    select(ags, ags_21) |>
    distinct() |>
    arrange(ags_21) |>
    head(30)
  
  print(source_mapping)
}

# Check if these extra ags_21 appear in harmonized data ---------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF EXTRA ags_21 APPEAR IN HARMONIZED DATA\n")
cat(strrep("=", 70), "\n\n")

harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Check for year 1990
ags_1990_harm <- harm_21 |>
  filter(election_year == 1990) |>
  distinct(ags) |>
  pull(ags)

extra_in_harm_1990 <- intersect(ags_1990_harm, ags21_extra_in_cw)

cat("Extra ags_21 that appear in harm_21 for 1990:", length(extra_in_harm_1990), "\n")

if (length(extra_in_harm_1990) > 0) {
  cat("These extra ags_21 from crosswalk appear in harmonized data:\n")
  if (length(extra_in_harm_1990) <= 30) {
    print(sort(extra_in_harm_1990))
  } else {
    cat("First 30:", paste(sort(extra_in_harm_1990)[1:30], collapse = ", "), "\n")
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
cat("- For 1990:", length(extra_in_harm_1990), "extra municipalities appear\n")
cat("- These are municipalities that existed in the crosswalk but were later dissolved/merged\n\n")

cat("Solution:\n")
cat("The crosswalk needs to be updated to only include ags_21 that actually exist in 2021.\n")
cat("OR the harmonization process should filter out ags_21 that don't exist in 2021.\n\n")

# Save results --------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat("SAVING RESULTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("data/data_checks")) {
  dir.create("data/data_checks", recursive = TRUE)
}

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

cat("\nScript completed!\n")

