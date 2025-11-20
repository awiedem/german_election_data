### Check how extra municipalities in harm_21 are created
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR"
)

conflict_prefer("filter", "dplyr")

cat("=", strrep("=", 70), "\n")
cat("CHECKING SOURCE OF EXTRA MUNICIPALITIES\n")
cat("=", strrep("=", 70), "\n\n")

# Load data
harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = haschaR::pad_zero_conditional(ags, 7))

ags_2021 <- harm_21 |>
  filter(election_year == 2021) |>
  distinct(ags) |>
  pull(ags)

ags_1990 <- harm_21 |>
  filter(election_year == 1990) |>
  distinct(ags) |>
  pull(ags)

extra <- setdiff(ags_1990, ags_2021)

cat("Extra municipalities in 1990:", length(extra), "\n")
cat("First 20:", paste(extra[1:20], collapse = ", "), "\n\n")

# Check crosswalk
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = haschaR::pad_zero_conditional(ags, 7),
    ags_21 = haschaR::pad_zero_conditional(ags_21, 7)
  ) |>
  filter(year == 1990)

# Check if extra AGS appear as source AGS
extra_as_source <- cw |>
  filter(ags %in% extra) |>
  distinct(ags, ags_21)

cat("Extra AGS that appear as source AGS in crosswalk:", nrow(extra_as_source), "\n")

if (nrow(extra_as_source) > 0) {
  cat("\nThese source AGS map to:\n")
  print(extra_as_source |> head(30))
  
  # Check if mapped ags_21 exist in 2021
  mapped_ags_21 <- unique(extra_as_source$ags_21)
  missing_mapped <- setdiff(mapped_ags_21, ags_2021)
  
  cat("\nMapped ags_21 that don't exist in 2021:", length(missing_mapped), "\n")
  
  if (length(missing_mapped) > 0) {
    cat("These are problematic!\n")
    print(missing_mapped)
  } else {
    cat("All mapped ags_21 exist in 2021.\n")
    cat("So the problem is: these source AGS should map to ags_21, but they appear as ags in harm_21!\n")
    cat("This suggests the harmonization process is not correctly mapping them.\n")
  }
}

# Check if extra AGS appear as ags_21 in crosswalk
extra_as_ags21 <- cw |>
  filter(ags_21 %in% extra) |>
  distinct(ags_21)

cat("\nExtra AGS that appear as ags_21 in crosswalk:", nrow(extra_as_ags21), "\n")

if (nrow(extra_as_ags21) > 0) {
  cat("These ags_21 appear in crosswalk but not in 2021 data:\n")
  print(extra_as_ags21)
}

cat("\nScript completed!\n")

