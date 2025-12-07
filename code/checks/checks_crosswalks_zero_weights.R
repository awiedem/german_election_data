### Check crosswalks for zero weights (pop_cw = 0 or area_cw = 0)
# Diagnose why some municipalities have zero weights in crosswalks
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "data.table",
  "readxl"
)

conflict_prefer("filter", "dplyr")

# Helper function to pad AGS
pad_zero_conditional <- function(x, n) {
  x <- as.character(x)
  x <- str_pad(x, width = n, side = "left", pad = "0")
  return(x)
}

cat(strrep("=", 70), "\n")
cat("CROSSWALK ZERO WEIGHTS DIAGNOSTIC\n")
cat(strrep("=", 70), "\n\n")

# Load crosswalk files ------------------------------------------------

cat("Loading crosswalk files...\n")

cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds")
cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
cw_1990_25 <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds")

# Check for zero weights in 2023-2025 crosswalk ----------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING 2023-2025 CROSSWALK FOR ZERO WEIGHTS\n")
cat(strrep("=", 70), "\n\n")

# Find entries with zero weights
zero_weights_23_25 <- cw_23_25 |>
  filter(pop_cw == 0 | area_cw == 0 | is.na(pop_cw) | is.na(area_cw)) |>
  arrange(ags)

cat("Entries with zero or missing weights in 2023-2025 crosswalk:", nrow(zero_weights_23_25), "\n\n")

if (nrow(zero_weights_23_25) > 0) {
  cat("Sample of entries with zero weights:\n")
  print(zero_weights_23_25 |> head(20))
  
  # Check if these are mergers
  cat("\nChecking if these are mergers (multiple ags mapping to same ags_25):\n")
  mergers_with_zero <- zero_weights_23_25 |>
    group_by(ags_25) |>
    summarise(
      n_donors = n(),
      donors = paste(ags, collapse = ", "),
      all_zero_pop = all(pop_cw == 0 | is.na(pop_cw)),
      all_zero_area = all(area_cw == 0 | is.na(area_cw)),
      .groups = "drop"
    ) |>
    filter(n_donors > 1)
  
  cat("Mergers with zero weights:", nrow(mergers_with_zero), "\n")
  if (nrow(mergers_with_zero) > 0) {
    print(mergers_with_zero |> head(20))
  }
}

# Check specific case: Görlitz ----------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING SPECIFIC CASE: Görlitz (14626110)\n")
cat(strrep("=", 70), "\n\n")

goerlitz_cw <- cw_23_25 |>
  filter(ags == "14626110" | ags_25 == "14626110") |>
  arrange(ags, year)

cat("Görlitz in 2023-2025 crosswalk:\n")
print(goerlitz_cw)

# Check if Görlitz appears in raw change files
cat("\nChecking raw change files for Görlitz...\n")

# Try to read raw change files
path_24 <- "data/crosswalks/raw/2024.xlsx"
path_25 <- "data/crosswalks/raw/2025-02.xlsx"

if (file.exists(path_24) && file.exists(path_25)) {
  std_names <- c(
    "event_id", "entity", "ars_old", "ags_old",
    "name_old", "law_short",
    "area_old", "pop_old",
    "ars_new", "ags_new",
    "name_new", "effective", "stat_effective"
  )
  
  read_changes <- function(path) {
    read_excel(path,
      sheet = "Gebietsaenderungen",
      col_names = FALSE
    ) %>%
      slice(-(1:4)) %>%
      set_names(std_names[seq_len(ncol(.))]) %>%
      filter(!is.na(ags_old))
  }
  
  chg_2024 <- read_changes(path_24)
  chg_2025 <- read_changes(path_25)
  changes <- bind_rows(chg_2024, chg_2025)
  
  goerlitz_changes <- changes |>
    filter(ags_old == "14626110" | ags_new == "14626110") |>
    mutate(
      pop_old_num = as.numeric(gsub("[^0-9]", "", pop_old)),
      area_old_num = as.numeric(gsub("[^0-9,.]", "", area_old))
    )
  
  cat("Görlitz in raw change files:\n")
  print(goerlitz_changes)
  
  if (nrow(goerlitz_changes) > 0) {
    cat("\nPopulation values in raw data:\n")
    print(goerlitz_changes |> select(ags_old, name_old, pop_old, pop_old_num, area_old, area_old_num))
  }
} else {
  cat("Raw change files not found. Skipping raw data check.\n")
}

# Check 2021-2023 crosswalk for Görlitz ------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING 2021-2023 CROSSWALK FOR GÖRLITZ\n")
cat(strrep("=", 70), "\n\n")

goerlitz_21_23 <- cw_21_23 |>
  filter(ags_2021 == "14626110" | ags_2023 == "14626110")

cat("Görlitz in 2021-2023 crosswalk:\n")
print(goerlitz_21_23)

# Check full 1990-2025 crosswalk for Görlitz --------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING FULL 1990-2025 CROSSWALK FOR GÖRLITZ\n")
cat(strrep("=", 70), "\n\n")

goerlitz_full <- cw_1990_25 |>
  filter(ags == "14626110" | ags_25 == "14626110") |>
  arrange(year, ags)

cat("Görlitz in full 1990-2025 crosswalk:\n")
print(goerlitz_full |> head(30))

# Systematic check: Find all municipalities with zero weights --------

cat("\n", strrep("=", 70), "\n")
cat("SYSTEMATIC CHECK: ALL MUNICIPALITIES WITH ZERO WEIGHTS\n")
cat(strrep("=", 70), "\n\n")

# In 2023-2025 crosswalk
zero_pop_23_25 <- cw_23_25 |>
  filter(pop_cw == 0 | is.na(pop_cw)) |>
  distinct(ags, ags_25) |>
  mutate(issue = "zero_pop_weight")

zero_area_23_25 <- cw_23_25 |>
  filter(area_cw == 0 | is.na(area_cw)) |>
  distinct(ags, ags_25) |>
  mutate(issue = "zero_area_weight")

both_zero_23_25 <- intersect(
  zero_pop_23_25$ags,
  zero_area_23_25$ags
)

cat("Municipalities with zero pop_cw in 2023-2025:", nrow(zero_pop_23_25), "\n")
cat("Municipalities with zero area_cw in 2023-2025:", nrow(zero_area_23_25), "\n")
cat("Municipalities with BOTH zero:", length(both_zero_23_25), "\n\n")

if (length(both_zero_23_25) > 0) {
  cat("Municipalities with both zero weights:\n")
  print(
    cw_23_25 |>
      filter(ags %in% both_zero_23_25) |>
      select(ags, ags_name, ags_25, ags_name_25, pop_cw, area_cw, area, population) |>
      arrange(ags) |>
      head(30)
  )
}

# Check if zero weights are due to missing population/area data ------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF ZERO WEIGHTS ARE DUE TO MISSING DATA\n")
cat(strrep("=", 70), "\n\n")

zero_weights_detail <- cw_23_25 |>
  filter(pop_cw == 0 | area_cw == 0 | is.na(pop_cw) | is.na(area_cw)) |>
  mutate(
    missing_pop = is.na(population) | population == 0,
    missing_area = is.na(area) | area == 0,
    both_missing = missing_pop & missing_area
  )

cat("Entries with zero weights:\n")
cat("  Missing population:", sum(zero_weights_detail$missing_pop, na.rm = TRUE), "\n")
cat("  Missing area:", sum(zero_weights_detail$missing_area, na.rm = TRUE), "\n")
cat("  Both missing:", sum(zero_weights_detail$both_missing, na.rm = TRUE), "\n\n")

# Check for municipalities that appear in mergers but have zero weights
cat("Checking mergers with zero weights:\n")
mergers_zero <- cw_23_25 |>
  filter(pop_cw == 0 | area_cw == 0) |>
  group_by(ags_25) |>
  summarise(
    n_donors = n(),
    donors = paste(ags, collapse = ", "),
    donor_names = paste(ags_name, collapse = "; "),
    total_pop = sum(population, na.rm = TRUE),
    total_area = sum(area, na.rm = TRUE),
    all_zero_pop = all(pop_cw == 0 | is.na(pop_cw)),
    all_zero_area = all(area_cw == 0 | is.na(area_cw)),
    .groups = "drop"
  ) |>
  filter(n_donors > 1)

cat("Mergers where all donors have zero weights:", nrow(mergers_zero), "\n")
if (nrow(mergers_zero) > 0) {
  print(mergers_zero |> head(20))
}

# Summary report ------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY REPORT\n")
cat(strrep("=", 70), "\n\n")

cat("Total entries in 2023-2025 crosswalk:", nrow(cw_23_25), "\n")
cat("Entries with zero pop_cw:", nrow(zero_pop_23_25), "\n")
cat("Entries with zero area_cw:", nrow(zero_area_23_25), "\n")
cat("Entries with both zero:", length(both_zero_23_25), "\n\n")

# Save results --------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("Saving diagnostic results...\n")
cat(strrep("=", 70), "\n\n")

# Create summary dataframe
diagnostic_summary <- cw_23_25 |>
  filter(pop_cw == 0 | area_cw == 0 | is.na(pop_cw) | is.na(area_cw)) |>
  select(ags, ags_name, ags_25, ags_name_25, year, pop_cw, area_cw, area, population) |>
  mutate(
    issue_type = case_when(
      (pop_cw == 0 | is.na(pop_cw)) & (area_cw == 0 | is.na(area_cw)) ~ "both_zero",
      pop_cw == 0 | is.na(pop_cw) ~ "zero_pop",
      area_cw == 0 | is.na(area_cw) ~ "zero_area",
      TRUE ~ "other"
    ),
    missing_data = is.na(population) | is.na(area) | population == 0 | area == 0
  ) |>
  arrange(ags, year)

write_csv(diagnostic_summary, "data/data_checks/crosswalk_zero_weights_diagnostic.csv")

cat("Diagnostic summary saved to: data/data_checks/crosswalk_zero_weights_diagnostic.csv\n")
cat("\nScript completed!\n")

