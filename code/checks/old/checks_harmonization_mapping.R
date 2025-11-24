### Check if all municipalities from raw data are successfully mapped in harmonized datasets
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
cat("CHECKING MAPPING IN HARMONIZED DATASETS\n")
cat("=", strrep("=", 70), "\n\n")

# Load raw unharmonized data ------------------------------------------------

cat("Loading raw unharmonized election data...\n")
df_raw <- read_rds("data/federal_elections/municipality_level/final/federal_muni_unharm.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  filter(election_year >= 1990) |>
  select(ags, election_year) |>
  distinct()

cat("Total unique AGS-year combinations in raw data:", nrow(df_raw), "\n")
cat("Election years:", paste(sort(unique(df_raw$election_year)), collapse = ", "), "\n\n")

# Load harmonized datasets --------------------------------------------------

cat("Loading harmonized datasets...\n")
harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  select(ags, election_year) |>
  distinct()

harm_25 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_25.rds") |>
  as_tibble() |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  select(ags, election_year) |>
  distinct()

cat("Harmonized datasets loaded.\n\n")

# Check mapping for harm_21 -------------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECKING MAPPING IN harm_21\n")
cat(strrep("=", 70), "\n\n")

# For harm_21, we need to check if all raw AGS map to some ags_21
# The raw AGS should map to ags_21 through the crosswalk process

# Load crosswalks to understand the mapping
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# For 2025, need to use the 2025->2021 crosswalk
cw_25_to_21 <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  as_tibble() |>
  filter(year == 2025) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

# Load the 2025->2021 crosswalk that was built in the harmonization script
cw_2023_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds")

# Build 2025-->2023 dis-aggregation table (same as in harmonization script)
cw_25_to_23 <- cw_2023_25 %>%
  filter(year == 2023) %>%
  transmute(
    ags_25 = pad_zero_conditional(ags_25, 7),
    ags_23 = pad_zero_conditional(ags, 7),
    pop_w_25_23 = pop_cw,
    area_w_25_23 = area_cw
  )

# Map 2023-->2021
cw_23_to_21 <- cw_21_23 %>%
  transmute(
    ags_23 = pad_zero_conditional(ags_2023, 7),
    ags_21 = pad_zero_conditional(ags_2021, 7),
    pop_w_23_21 = w_pop,
    area_w_23_21 = w_area
  )

# For municipalities born between 2021-2023
cw_23_to_21 <- bind_rows(
  cw_23_to_21,
  cw_25_to_23 %>%
    anti_join(cw_23_to_21, by = "ags_23") %>%
    transmute(
      ags_23,
      ags_21 = ags_23,
      pop_w_23_21 = 1,
      area_w_23_21 = 1
    )
)

# Create fully-chained 2025 → 2021 crosswalk
cw_25_to_21_full <- cw_25_to_23 %>%
  left_join(cw_23_to_21, by = "ags_23") %>%
  mutate(
    pop_w_25_21 = pop_w_25_23 * pop_w_23_21,
    area_w_25_21 = area_w_25_23 * area_w_23_21
  ) %>%
  group_by(ags_25, ags_21) %>%
  summarise(
    pop_w_25_21 = sum(pop_w_25_21, na.rm = TRUE),
    area_w_25_21 = sum(area_w_25_21, na.rm = TRUE),
    .groups = "drop"
  )

# Check mapping for each election year
cat("Checking mapping by election year for harm_21:\n\n")

for (yr in sort(unique(df_raw$election_year))) {
  raw_ags_yr <- df_raw |>
    filter(election_year == yr) |>
    pull(ags)
  
  harm_ags_yr <- harm_21 |>
    filter(election_year == yr) |>
    pull(ags)
  
  if (yr <= 2020) {
    # Use cw_21 for years 1990-2020
    mapped_ags <- cw_21 |>
      filter(year == yr) |>
      distinct(ags_21) |>
      mutate(ags_21 = pad_zero_conditional(ags_21, 7)) |>
      pull(ags_21)
    
    # Check if all raw AGS map to some ags_21
    raw_in_cw <- cw_21 |>
      filter(year == yr) |>
      distinct(ags) |>
      mutate(ags = pad_zero_conditional(ags, 7)) |>
      pull(ags)
    
    unmapped <- setdiff(raw_ags_yr, raw_in_cw)
    
  } else if (yr == 2021) {
    # 2021: AGS are already ags_21
    mapped_ags <- harm_ags_yr
    unmapped <- setdiff(raw_ags_yr, mapped_ags)
    
  } else if (yr == 2025) {
    # 2025: Use cw_25_to_21_full
    raw_in_cw <- cw_25_to_21_full |>
      distinct(ags_25) |>
      pull(ags_25)
    
    unmapped <- setdiff(raw_ags_yr, raw_in_cw)
    mapped_ags <- cw_25_to_21_full |>
      distinct(ags_21) |>
      mutate(ags_21 = pad_zero_conditional(ags_21, 7)) |>
      pull(ags_21)
  }
  
  cat(sprintf("Year %d:\n", yr))
  cat(sprintf("  Raw AGS: %d\n", length(raw_ags_yr)))
  cat(sprintf("  Harm AGS: %d\n", length(harm_ags_yr)))
  cat(sprintf("  Mapped AGS: %d\n", length(mapped_ags)))
  cat(sprintf("  Unmapped in crosswalk: %d\n", length(unmapped)))
  
  if (length(unmapped) > 0) {
    cat(sprintf("  ⚠️  WARNING: %d AGS not found in crosswalk!\n", length(unmapped)))
    if (length(unmapped) <= 10) {
      cat("  Unmapped AGS:", paste(unmapped, collapse = ", "), "\n")
    } else {
      cat("  First 10 unmapped AGS:", paste(unmapped[1:10], collapse = ", "), "\n")
    }
  } else {
    cat("  ✓ All raw AGS found in crosswalk\n")
  }
  cat("\n")
}

# Check mapping for harm_25 -------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING MAPPING IN harm_25\n")
cat(strrep("=", 70), "\n\n")

# Load crosswalk for harm_25
cw_25 <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

cat("Checking mapping by election year for harm_25:\n\n")

for (yr in sort(unique(df_raw$election_year))) {
  raw_ags_yr <- df_raw |>
    filter(election_year == yr) |>
    pull(ags)
  
  harm_ags_yr <- harm_25 |>
    filter(election_year == yr) |>
    pull(ags)
  
  if (yr < 2025) {
    # Use cw_25 for years < 2025
    raw_in_cw <- cw_25 |>
      filter(year == yr) |>
      distinct(ags) |>
      pull(ags)
    
    unmapped <- setdiff(raw_ags_yr, raw_in_cw)
    mapped_ags <- cw_25 |>
      filter(year == yr) |>
      distinct(ags_25) |>
      pull(ags_25)
    
  } else if (yr == 2025) {
    # 2025: AGS are already ags_25
    mapped_ags <- harm_ags_yr
    unmapped <- setdiff(raw_ags_yr, mapped_ags)
  }
  
  cat(sprintf("Year %d:\n", yr))
  cat(sprintf("  Raw AGS: %d\n", length(raw_ags_yr)))
  cat(sprintf("  Harm AGS: %d\n", length(harm_ags_yr)))
  cat(sprintf("  Mapped AGS: %d\n", length(mapped_ags)))
  cat(sprintf("  Unmapped in crosswalk: %d\n", length(unmapped)))
  
  if (length(unmapped) > 0) {
    cat(sprintf("  ⚠️  WARNING: %d AGS not found in crosswalk!\n", length(unmapped)))
    if (length(unmapped) <= 10) {
      cat("  Unmapped AGS:", paste(unmapped, collapse = ", "), "\n")
    } else {
      cat("  First 10 unmapped AGS:", paste(unmapped[1:10], collapse = ", "), "\n")
    }
  } else {
    cat("  ✓ All raw AGS found in crosswalk\n")
  }
  cat("\n")
}

# Summary -------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("This check verifies that all AGS from raw data can be mapped through crosswalks.\n")
cat("Note: This does NOT verify that the harmonization scripts actually use all raw data.\n")
cat("For that, you would need to check if the harmonization scripts filter out any AGS.\n\n")

cat("Script completed!\n")

