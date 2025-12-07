### Check if all municipalities from raw data appear in harmonized datasets
# This checks if the harmonization scripts successfully map all raw data
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
cat("CHECKING IF ALL RAW DATA APPEARS IN HARMONIZED DATASETS\n")
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

# For harm_21: Check if all raw AGS map to some ags_21 ----------------------

cat(strrep("=", 70), "\n")
cat("CHECKING COVERAGE IN harm_21\n")
cat(strrep("=", 70), "\n\n")

# Load crosswalks to map raw AGS to ags_21
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# For 2025, build the 2025->2021 crosswalk (same as in harmonization script)
cw_2023_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds")

cw_25_to_23 <- cw_2023_25 %>%
  filter(year == 2023) %>%
  transmute(
    ags_25 = pad_zero_conditional(ags_25, 7),
    ags_23 = pad_zero_conditional(ags, 7),
    pop_w_25_23 = pop_cw,
    area_w_25_23 = area_cw
  )

cw_23_to_21 <- cw_21_23 %>%
  transmute(
    ags_23 = pad_zero_conditional(ags_2023, 7),
    ags_21 = pad_zero_conditional(ags_2021, 7),
    pop_w_23_21 = w_pop,
    area_w_23_21 = w_area
  )

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
  ) |>
  mutate(
    ags_25 = pad_zero_conditional(ags_25, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# Map raw AGS to ags_21 for each year
df_raw_mapped_21 <- df_raw |>
  left_join(
    cw_21 |> select(ags, year, ags_21),
    by = c("ags", "election_year" = "year")
  ) |>
  # For 2021, AGS are already ags_21
  mutate(
    ags_21 = ifelse(election_year == 2021, ags, ags_21)
  ) |>
  # For 2025, use cw_25_to_21_full
  left_join(
    cw_25_to_21_full |> select(ags_25, ags_21),
    by = c("ags" = "ags_25")
  ) |>
  mutate(
    ags_21 = ifelse(election_year == 2025, coalesce(ags_21.y, ags), ags_21.x)
  ) |>
  select(ags, election_year, ags_21)

# Check if all raw AGS map to some ags_21
unmapped_21 <- df_raw_mapped_21 |>
  filter(is.na(ags_21)) |>
  select(ags, election_year) |>
  distinct()

cat("Raw AGS-year combinations:", nrow(df_raw_mapped_21), "\n")
cat("Successfully mapped to ags_21:", sum(!is.na(df_raw_mapped_21$ags_21)), "\n")
cat("NOT mapped to ags_21:", nrow(unmapped_21), "\n\n")

if (nrow(unmapped_21) > 0) {
  cat("⚠️  WARNING: Some raw AGS are NOT mapped to ags_21!\n\n")
  cat("Unmapped by election year:\n")
  print(unmapped_21 |> count(election_year, sort = TRUE))
} else {
  cat("✓ All raw AGS successfully mapped to ags_21!\n\n")
}

# Check if all mapped ags_21 appear in harm_21
mapped_ags_21 <- df_raw_mapped_21 |>
  filter(!is.na(ags_21)) |>
  select(ags_21, election_year) |>
  distinct()

missing_in_harm_21 <- mapped_ags_21 |>
  anti_join(harm_21, by = c("ags_21" = "ags", "election_year"))

cat("Mapped ags_21-year combinations:", nrow(mapped_ags_21), "\n")
cat("Missing in harm_21:", nrow(missing_in_harm_21), "\n\n")

if (nrow(missing_in_harm_21) > 0) {
  cat("⚠️  WARNING: Some mapped ags_21 are NOT in harm_21!\n\n")
  cat("Missing by election year:\n")
  print(missing_in_harm_21 |> count(election_year, sort = TRUE))
} else {
  cat("✓ All mapped ags_21 appear in harm_21!\n\n")
}

# For harm_25: Check if all raw AGS map to some ags_25 ----------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING COVERAGE IN harm_25\n")
cat(strrep("=", 70), "\n\n")

# Load crosswalk for harm_25
cw_25 <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

# Map raw AGS to ags_25 for each year
df_raw_mapped_25 <- df_raw |>
  left_join(
    cw_25 |> select(ags, year, ags_25),
    by = c("ags", "election_year" = "year")
  ) |>
  # For 2025, AGS are already ags_25
  mutate(
    ags_25 = ifelse(election_year == 2025, ags, ags_25)
  ) |>
  select(ags, election_year, ags_25)

# Check if all raw AGS map to some ags_25
unmapped_25 <- df_raw_mapped_25 |>
  filter(is.na(ags_25)) |>
  select(ags, election_year) |>
  distinct()

cat("Raw AGS-year combinations:", nrow(df_raw_mapped_25), "\n")
cat("Successfully mapped to ags_25:", sum(!is.na(df_raw_mapped_25$ags_25)), "\n")
cat("NOT mapped to ags_25:", nrow(unmapped_25), "\n\n")

if (nrow(unmapped_25) > 0) {
  cat("⚠️  WARNING: Some raw AGS are NOT mapped to ags_25!\n\n")
  cat("Unmapped by election year:\n")
  print(unmapped_25 |> count(election_year, sort = TRUE))
} else {
  cat("✓ All raw AGS successfully mapped to ags_25!\n\n")
}

# Check if all mapped ags_25 appear in harm_25
mapped_ags_25 <- df_raw_mapped_25 |>
  filter(!is.na(ags_25)) |>
  select(ags_25, election_year) |>
  distinct()

missing_in_harm_25 <- mapped_ags_25 |>
  anti_join(harm_25, by = c("ags_25" = "ags", "election_year"))

cat("Mapped ags_25-year combinations:", nrow(mapped_ags_25), "\n")
cat("Missing in harm_25:", nrow(missing_in_harm_25), "\n\n")

if (nrow(missing_in_harm_25) > 0) {
  cat("⚠️  WARNING: Some mapped ags_25 are NOT in harm_25!\n\n")
  cat("Missing by election year:\n")
  print(missing_in_harm_25 |> count(election_year, sort = TRUE))
} else {
  cat("✓ All mapped ags_25 appear in harm_25!\n\n")
}

# Summary by election year --------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY BY ELECTION YEAR\n")
cat(strrep("=", 70), "\n\n")

summary_by_year <- df_raw |>
  group_by(election_year) |>
  summarise(
    n_raw = n(),
    .groups = "drop"
  ) |>
  left_join(
    df_raw_mapped_21 |>
      group_by(election_year) |>
      summarise(
        n_mapped_21 = sum(!is.na(ags_21)),
        n_unmapped_21 = sum(is.na(ags_21)),
        .groups = "drop"
      ),
    by = "election_year"
  ) |>
  left_join(
    harm_21 |>
      group_by(election_year) |>
      summarise(n_harm_21 = n(), .groups = "drop"),
    by = "election_year"
  ) |>
  left_join(
    df_raw_mapped_25 |>
      group_by(election_year) |>
      summarise(
        n_mapped_25 = sum(!is.na(ags_25)),
        n_unmapped_25 = sum(is.na(ags_25)),
        .groups = "drop"
      ),
    by = "election_year"
  ) |>
  left_join(
    harm_25 |>
      group_by(election_year) |>
      summarise(n_harm_25 = n(), .groups = "drop"),
    by = "election_year"
  )

cat("Coverage summary:\n")
print(summary_by_year)

# Save results --------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SAVING RESULTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("data/data_checks")) {
  dir.create("data/data_checks", recursive = TRUE)
}

if (nrow(unmapped_21) > 0) {
  write_csv(unmapped_21, "data/data_checks/unmapped_raw_to_ags_21.csv")
  cat("Unmapped raw AGS for ags_21 saved.\n")
}

if (nrow(missing_in_harm_21) > 0) {
  write_csv(missing_in_harm_21, "data/data_checks/missing_ags_21_in_harm_21.csv")
  cat("Missing ags_21 in harm_21 saved.\n")
}

if (nrow(unmapped_25) > 0) {
  write_csv(unmapped_25, "data/data_checks/unmapped_raw_to_ags_25.csv")
  cat("Unmapped raw AGS for ags_25 saved.\n")
}

if (nrow(missing_in_harm_25) > 0) {
  write_csv(missing_in_harm_25, "data/data_checks/missing_ags_25_in_harm_25.csv")
  cat("Missing ags_25 in harm_25 saved.\n")
}

write_csv(summary_by_year, "data/data_checks/harmonization_coverage_summary.csv")
cat("Summary saved.\n")

cat("\nScript completed!\n")

