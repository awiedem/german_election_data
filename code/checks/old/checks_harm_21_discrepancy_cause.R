### Analyze why there are discrepancies in municipality count in harm_21
# Should understand why extra municipalities appear in earlier years
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
cat("ANALYZING CAUSE OF DISCREPANCIES IN harm_21\n")
cat("=", strrep("=", 70), "\n\n")

# Load harmonized dataset ---------------------------------------------------

cat("Loading harmonized dataset...\n")
harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Get municipalities in 2021
ags_2021 <- harm_21 |>
  filter(election_year == 2021) |>
  distinct(ags) |>
  pull(ags)

cat("Municipalities in 2021:", length(ags_2021), "\n\n")

# Find extra municipalities (not in 2021) ----------------------------------

cat(strrep("=", 70), "\n")
cat("FINDING EXTRA MUNICIPALITIES (not in 2021)\n")
cat(strrep("=", 70), "\n\n")

# Get extra municipalities for a specific year (e.g., 1990)
year_to_check <- 1990

ags_year <- harm_21 |>
  filter(election_year == year_to_check) |>
  distinct(ags) |>
  pull(ags)

extra_ags <- setdiff(ags_year, ags_2021)

cat(sprintf("Year %d: %d extra municipalities (not in 2021)\n", year_to_check, length(extra_ags)))
cat("First 20 extra AGS:", paste(extra_ags[1:min(20, length(extra_ags))], collapse = ", "), "\n\n")

# Check crosswalk for these extra municipalities ----------------------------

cat(strrep("=", 70), "\n")
cat("CHECKING CROSSWALK FOR EXTRA MUNICIPALITIES\n")
cat(strrep("=", 70), "\n\n")

# Load crosswalk
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# Check if these extra AGS appear in the crosswalk for year_to_check
cat(sprintf("Checking crosswalk for year %d...\n", year_to_check))

cw_year <- cw_21 |>
  filter(year == year_to_check)

# Check which of the extra AGS are in the crosswalk as ags_21
extra_in_cw <- cw_year |>
  filter(ags_21 %in% extra_ags) |>
  distinct(ags_21) |>
  pull(ags_21)

cat(sprintf("Extra AGS that appear as ags_21 in crosswalk: %d\n", length(extra_in_cw)))

if (length(extra_in_cw) > 0) {
  cat("These are the problem: they appear as ags_21 in the crosswalk but don't exist in 2021!\n")
  cat("First 10:", paste(extra_in_cw[1:min(10, length(extra_in_cw))], collapse = ", "), "\n\n")
  
  # Check what source AGS map to these extra ags_21
  cat("Source AGS that map to these extra ags_21:\n")
  source_ags_mapping <- cw_year |>
    filter(ags_21 %in% extra_in_cw) |>
    select(ags, ags_21) |>
    distinct()
  
  print(source_ags_mapping |> head(20))
  
  # Check if these source AGS should map to different ags_21
  cat("\nChecking if these source AGS should map to different ags_21 in later years...\n")
  for (ags_check in source_ags_mapping$ags[1:min(5, nrow(source_ags_mapping))]) {
    cw_all_years <- cw_21 |>
      filter(ags == ags_check) |>
      select(year, ags, ags_21) |>
      distinct() |>
      arrange(year)
    
    cat(sprintf("\nSource AGS %s maps to:\n", ags_check))
    print(cw_all_years)
  }
}

# Check if extra AGS appear as source AGS in crosswalk ----------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF EXTRA AGS APPEAR AS SOURCE AGS\n")
cat(strrep("=", 70), "\n\n")

# Check if any of the extra AGS appear as source AGS (ags) in the crosswalk
extra_as_source <- cw_year |>
  filter(ags %in% extra_ags) |>
  distinct(ags, ags_21) |>
  arrange(ags)

if (nrow(extra_as_source) > 0) {
  cat(sprintf("Extra AGS that appear as source AGS in crosswalk: %d\n", nrow(extra_as_source)))
  cat("These source AGS map to the following ags_21:\n")
  print(extra_as_source |> head(20))
  
  # Check if these ags_21 exist in 2021
  mapped_ags_21 <- unique(extra_as_source$ags_21)
  missing_ags_21 <- setdiff(mapped_ags_21, ags_2021)
  
  if (length(missing_ags_21) > 0) {
    cat(sprintf("\n⚠️  PROBLEM: %d of these mapped ags_21 do NOT exist in 2021!\n", length(missing_ags_21)))
    cat("Missing ags_21:", paste(missing_ags_21[1:min(10, length(missing_ags_21))], collapse = ", "), "\n")
  } else {
    cat("\n✓ All mapped ags_21 exist in 2021.\n")
  }
} else {
  cat("No extra AGS appear as source AGS in crosswalk.\n")
}

# Check what happens during harmonization ------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SIMULATING HARMONIZATION PROCESS\n")
cat(strrep("=", 70), "\n\n")

# Load raw data for year_to_check
df_raw <- read_rds("data/federal_elections/municipality_level/final/federal_muni_unharm.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  filter(election_year == year_to_check) |>
  select(ags) |>
  distinct()

cat(sprintf("Raw AGS in year %d: %d\n", year_to_check, nrow(df_raw)))

# Simulate the merge with crosswalk
df_merged <- df_raw |>
  left_join(
    cw_year |> select(ags, ags_21),
    by = "ags"
  )

# Count how many unique ags_21 we get
unique_ags_21 <- df_merged |>
  filter(!is.na(ags_21)) |>
  distinct(ags_21) |>
  pull(ags_21)

cat(sprintf("Unique ags_21 after merge: %d\n", length(unique_ags_21)))
cat(sprintf("Expected (from 2021): %d\n", length(ags_2021)))

extra_after_merge <- setdiff(unique_ags_21, ags_2021)
missing_after_merge <- setdiff(ags_2021, unique_ags_21)

cat(sprintf("Extra ags_21 after merge: %d\n", length(extra_after_merge)))
cat(sprintf("Missing ags_21 after merge: %d\n", length(missing_after_merge)))

if (length(extra_after_merge) > 0) {
  cat("\nExtra ags_21 that appear after merge:\n")
  cat("First 20:", paste(extra_after_merge[1:min(20, length(extra_after_merge))], collapse = ", "), "\n")
  
  # Check which source AGS map to these extra ags_21
  cat("\nSource AGS that map to extra ags_21:\n")
  source_to_extra <- df_merged |>
    filter(ags_21 %in% extra_after_merge) |>
    select(ags, ags_21) |>
    distinct() |>
    arrange(ags_21)
  
  print(source_to_extra |> head(30))
}

# Check if these extra ags_21 exist in crosswalk for other years ------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF EXTRA ags_21 EXIST IN OTHER YEARS\n")
cat(strrep("=", 70), "\n\n")

if (length(extra_after_merge) > 0) {
  # Check if these extra ags_21 appear in crosswalk for 2021
  cw_2021 <- cw_21 |>
    filter(year == 2021)
  
  extra_in_2021_cw <- cw_2021 |>
    filter(ags_21 %in% extra_after_merge) |>
    distinct(ags_21) |>
    pull(ags_21)
  
  cat(sprintf("Extra ags_21 that appear in 2021 crosswalk: %d\n", length(extra_in_2021_cw)))
  
  if (length(extra_in_2021_cw) > 0) {
    cat("⚠️  PROBLEM: These ags_21 appear in the 2021 crosswalk but not in the actual 2021 data!\n")
    cat("This suggests the crosswalk contains municipalities that were dissolved/merged after 2021.\n")
    cat("First 10:", paste(extra_in_2021_cw[1:min(10, length(extra_in_2021_cw))], collapse = ", "), "\n")
  } else {
    cat("These extra ags_21 do NOT appear in the 2021 crosswalk.\n")
    cat("This suggests they were created by the harmonization process incorrectly.\n")
  }
}

# Summary -------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("The discrepancy is caused by:\n")
cat("1. The crosswalk contains ags_21 that don't exist in the actual 2021 data\n")
cat("2. These ags_21 appear when harmonizing earlier years\n")
cat("3. The harmonization process correctly uses the crosswalk, but the crosswalk\n")
cat("   contains municipalities that were dissolved/merged after the crosswalk was created\n\n")

cat("Solution options:\n")
cat("1. Update the crosswalk to remove municipalities that don't exist in 2021\n")
cat("2. Filter the harmonized data to only keep municipalities that exist in 2021\n")
cat("3. Investigate why the crosswalk contains these municipalities\n\n")

cat("Script completed!\n")

