### Check if all municipalities from raw data are successfully mapped
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
cat("FEDERAL ELECTIONS: RAW DATA MAPPING CHECKS\n")
cat("=", strrep("=", 70), "\n\n")

# Load raw unharmonized data ------------------------------------------------

cat("Loading raw unharmonized election data...\n")
df_raw <- read_rds("data/federal_elections/municipality_level/final/federal_muni_unharm.rds") |>
  as_tibble() |>
  mutate(ags = haschaR::pad_zero_conditional(ags, 7)) |>
  filter(election_year >= 1990) |>
  select(ags, election_year) |>
  distinct()

cat("Total unique AGS-year combinations in raw data:", nrow(df_raw), "\n")
cat("Unique AGS codes in raw data:", length(unique(df_raw$ags)), "\n")
cat("Election years:", paste(sort(unique(df_raw$election_year)), collapse = ", "), "\n\n")

# Load crosswalks -----------------------------------------------------------

cat("Loading crosswalks...\n")

# For ags_21: use ags_crosswalks.csv (covers 1990-2020)
cw_21 <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = haschaR::pad_zero_conditional(ags, 7),
    ags_21 = haschaR::pad_zero_conditional(ags_21, 7)
  )

# For ags_25: need to chain crosswalks
cw_1990_23 <- read_rds("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = haschaR::pad_zero_conditional(ags, 7),
    ags_2023 = haschaR::pad_zero_conditional(ags_2023, 7)
  )

cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  as_tibble() |>
  filter(year == 2023) |>
  mutate(
    ags = haschaR::pad_zero_conditional(ags, 7),
    ags_25 = haschaR::pad_zero_conditional(ags_25, 7)
  )

# Chain them together for ags_25
cw_25 <- cw_1990_23 |>
  left_join(
    cw_23_25 |> select(ags, ags_25),
    by = c("ags_2023" = "ags")
  ) |>
  mutate(
    ags_25 = ifelse(is.na(ags_25), ags_2023, ags_25)
  ) |>
  select(ags, year, ags_25)

cat("Crosswalks loaded.\n\n")

# Check mapping to ags_21 ---------------------------------------------------

cat(strrep("=", 70), "\n")
cat("CHECKING MAPPING TO ags_21\n")
cat(strrep("=", 70), "\n\n")

# For years 1990-2020, use cw_21
df_raw_90_20 <- df_raw |>
  filter(election_year <= 2020)

df_merged_21_90_20 <- df_raw_90_20 |>
  left_join(
    cw_21 |> select(ags, year, ags_21),
    by = c("ags", "election_year" = "year")
  )

# For years 2021-2025, need to use chain: ags -> ags_2023 -> ags_21
cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds") |>
  as_tibble() |>
  mutate(
    ags_2021 = haschaR::pad_zero_conditional(ags_2021, 7),
    ags_2023 = haschaR::pad_zero_conditional(ags_2023, 7)
  )

# For 2021-2023: use cw_1990_23 directly
df_raw_21_23 <- df_raw |>
  filter(election_year >= 2021, election_year <= 2023)

df_temp_23_21_23 <- df_raw_21_23 |>
  left_join(
    cw_1990_23 |>
      filter(year %in% c(2021, 2022, 2023)) |>
      select(ags, year, ags_2023),
    by = c("ags", "election_year" = "year")
  ) |>
  left_join(
    cw_21_23 |> select(ags_2023, ags_2021),
    by = "ags_2023"
  ) |>
  rename(ags_21 = ags_2021)

# For 2025: need to map 2025 -> 2023 -> 2021
cw_25_23_reverse <- cw_23_25 |>
  filter(year == 2023) |>
  select(ags_25 = ags_25, ags_2023 = ags) |>
  distinct()

df_raw_25 <- df_raw |>
  filter(election_year == 2025)

df_temp_23_25 <- df_raw_25 |>
  left_join(
    cw_25_23_reverse,
    by = c("ags" = "ags_25")
  ) |>
  left_join(
    cw_21_23 |> select(ags_2023, ags_2021),
    by = "ags_2023"
  ) |>
  rename(ags_21 = ags_2021)

# Combine 2021-2023 and 2025
df_merged_21_21_25 <- bind_rows(
  df_temp_23_21_23 |> select(ags, election_year, ags_21),
  df_temp_23_25 |> select(ags, election_year, ags_21)
)

# Combine both periods
df_merged_21 <- bind_rows(
  df_merged_21_90_20,
  df_merged_21_21_25 |> select(ags, election_year, ags_21)
)

# Check for unmapped AGS
unmapped_21 <- df_merged_21 |>
  filter(is.na(ags_21)) |>
  select(ags, election_year) |>
  distinct()

cat("Total AGS-year combinations in raw data:", nrow(df_merged_21), "\n")
cat("Successfully mapped to ags_21:", sum(!is.na(df_merged_21$ags_21)), "\n")
cat("NOT mapped to ags_21:", nrow(unmapped_21), "\n\n")

if (nrow(unmapped_21) > 0) {
  cat("⚠️  WARNING: Some AGS-year combinations are NOT mapped to ags_21!\n\n")
  cat("Unmapped AGS by election year:\n")
  print(unmapped_21 |> count(election_year, sort = TRUE))
} else {
  cat("✓ All AGS-year combinations successfully mapped to ags_21!\n\n")
}

# Check mapping to ags_25 ---------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING MAPPING TO ags_25\n")
cat(strrep("=", 70), "\n\n")

# For years 1990-2023: use cw_25
df_raw_90_23 <- df_raw |>
  filter(election_year <= 2023)

df_merged_25_90_23 <- df_raw_90_23 |>
  left_join(
    cw_25,
    by = c("ags", "election_year" = "year")
  )

# For 2025: AGS are already ags_25 (identity mapping)
df_raw_25_only <- df_raw |>
  filter(election_year == 2025) |>
  mutate(ags_25 = ags)

# Combine both periods
df_merged_25 <- bind_rows(
  df_merged_25_90_23 |> select(ags, election_year, ags_25),
  df_raw_25_only |> select(ags, election_year, ags_25)
)

# Check for unmapped AGS
unmapped_25 <- df_merged_25 |>
  filter(is.na(ags_25)) |>
  select(ags, election_year) |>
  distinct()

cat("Total AGS-year combinations in raw data:", nrow(df_merged_25), "\n")
cat("Successfully mapped to ags_25:", sum(!is.na(df_merged_25$ags_25)), "\n")
cat("NOT mapped to ags_25:", nrow(unmapped_25), "\n\n")

if (nrow(unmapped_25) > 0) {
  cat("⚠️  WARNING: Some AGS-year combinations are NOT mapped to ags_25!\n\n")
  cat("Unmapped AGS by election year:\n")
  print(unmapped_25 |> count(election_year, sort = TRUE))
} else {
  cat("✓ All AGS-year combinations successfully mapped to ags_25!\n\n")
}

# Check if mapped AGS appear in harmonized datasets --------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF MAPPED AGS APPEAR IN HARMONIZED DATASETS\n")
cat(strrep("=", 70), "\n\n")

harm_21 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds") |>
  as_tibble() |>
  mutate(ags = haschaR::pad_zero_conditional(ags, 7))

harm_25 <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm_25.rds") |>
  as_tibble() |>
  mutate(ags = haschaR::pad_zero_conditional(ags, 7))

# Check for harm_21
mapped_ags_21 <- df_merged_21 |>
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

# Check for harm_25
mapped_ags_25 <- df_merged_25 |>
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
    df_merged_21 |>
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
    df_merged_25 |>
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

cat("Mapping and coverage summary:\n")
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

if (nrow(unmapped_25) > 0) {
  write_csv(unmapped_25, "data/data_checks/unmapped_raw_to_ags_25.csv")
  cat("Unmapped raw AGS for ags_25 saved.\n")
}

if (nrow(missing_in_harm_21) > 0) {
  write_csv(missing_in_harm_21, "data/data_checks/missing_ags_21_in_harm_21.csv")
  cat("Missing ags_21 in harm_21 saved.\n")
}

if (nrow(missing_in_harm_25) > 0) {
  write_csv(missing_in_harm_25, "data/data_checks/missing_ags_25_in_harm_25.csv")
  cat("Missing ags_25 in harm_25 saved.\n")
}

write_csv(summary_by_year, "data/data_checks/raw_data_mapping_summary.csv")
cat("Summary saved.\n")

cat("\nScript completed!\n")

