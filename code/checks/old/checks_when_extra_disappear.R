### Check when extra municipalities disappear from crosswalk
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

cat("Extra municipalities in 1990:", length(extra), "\n\n")

# Load crosswalk
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = haschaR::pad_zero_conditional(ags, 7),
    ags_21 = haschaR::pad_zero_conditional(ags_21, 7)
  )

# Check in which years these extra ags_21 appear
cat("Checking when extra ags_21 appear/disappear in crosswalk:\n\n")

for (yr in sort(unique(cw$year))) {
  cw_yr <- cw |> filter(year == yr)
  ags21_yr <- cw_yr |> distinct(ags_21) |> pull(ags_21)
  
  extra_in_yr <- intersect(extra, ags21_yr)
  
  if (length(extra_in_yr) > 0) {
    cat(sprintf("Year %d: %d extra ags_21 appear\n", yr, length(extra_in_yr)))
  }
}

# Check specific examples
cat("\n", strrep("=", 70), "\n")
cat("DETAILED CHECK FOR SPECIFIC EXAMPLES\n")
cat(strrep("=", 70), "\n\n")

example_ags <- extra[1:5]

for (ags_check in example_ags) {
  cat(sprintf("\nAGS %s:\n", ags_check))
  
  # Check as source AGS
  as_source <- cw |>
    filter(ags == ags_check) |>
    select(year, ags, ags_21) |>
    distinct() |>
    arrange(year)
  
  if (nrow(as_source) > 0) {
    cat("  Appears as source AGS, maps to ags_21:\n")
    print(as_source)
  }
  
  # Check as ags_21
  as_ags21 <- cw |>
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

cat("\nScript completed!\n")

