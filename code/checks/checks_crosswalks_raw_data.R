### Check raw crosswalk data files for missing population/area data
# Diagnose why some municipalities have missing pop/area in raw change files
# Vincent Heddesheimer
# Date: 2025

rm(list = ls())

# Disallow scientific notation
options(scipen = 999)
pacman::p_load(
  "tidyverse",
  "readxl",
  "data.table"
)

conflict_prefer("filter", "dplyr")

cat(strrep("=", 70), "\n")
cat("RAW CROSSWALK DATA DIAGNOSTIC\n")
cat(strrep("=", 70), "\n\n")

# Paths to raw data files ---------------------------------------------

path_24 <- "data/crosswalks/raw/2024.xlsx"
path_25 <- "data/crosswalks/raw/2025-02.xlsx"

# Check if files exist
if (!file.exists(path_24)) {
  stop("File not found: ", path_24)
}
if (!file.exists(path_25)) {
  stop("File not found: ", path_25)
}

cat("Raw data files found.\n")
cat("  2024:", path_24, "\n")
cat("  2025:", path_25, "\n\n")

# Helper function to read changes --------------------------------------

std_names <- c(
  "event_id", "entity", "ars_old", "ags_old",
  "name_old", "law_short",
  "area_old", "pop_old",
  "ars_new", "ags_new",
  "name_new", "effective", "stat_effective"
)

read_changes <- function(path) {
  cat("Reading:", path, "\n")
  
  df <- read_excel(
    path,
    sheet = "Gebietsaenderungen",
    col_names = FALSE
  ) %>%
    slice(-(1:4)) %>% # drop title rows
    set_names(std_names[seq_len(ncol(.))]) %>%
    filter(!is.na(ags_old)) # filter out empty rows
  
  cat("  Rows read:", nrow(df), "\n")
  return(df)
}

# Read raw data --------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("READING RAW DATA FILES\n")
cat(strrep("=", 70), "\n\n")

chg_2024 <- read_changes(path_24)
chg_2025 <- read_changes(path_25)
changes <- bind_rows(chg_2024, chg_2025)

cat("\nTotal changes:", nrow(changes), "\n")
cat("  From 2024 file:", nrow(chg_2024), "\n")
cat("  From 2025 file:", nrow(chg_2025), "\n\n")

# Check for missing population/area data ------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING FOR MISSING POPULATION/AREA DATA\n")
cat(strrep("=", 70), "\n\n")

# Convert to numeric and check for missing
changes_clean <- changes |>
  mutate(
    pop_old_num = as.numeric(gsub("[^0-9]", "", pop_old)),
    area_old_num = as.numeric(gsub("[^0-9,.]", "", area_old)),
    pop_missing = is.na(pop_old) | pop_old == "" | is.na(pop_old_num),
    area_missing = is.na(area_old) | area_old == "" | is.na(area_old_num),
    both_missing = pop_missing & area_missing
  )

# Summary
cat("Missing data summary:\n")
cat("  Missing pop_old:", sum(changes_clean$pop_missing, na.rm = TRUE), "\n")
cat("  Missing area_old:", sum(changes_clean$area_missing, na.rm = TRUE), "\n")
cat("  Both missing:", sum(changes_clean$both_missing, na.rm = TRUE), "\n\n")

# Show entries with missing data
missing_data <- changes_clean |>
  filter(pop_missing | area_missing) |>
  select(
    event_id, ags_old, name_old, ags_new, name_new,
    pop_old, pop_old_num, area_old, area_old_num,
    pop_missing, area_missing, both_missing
  ) |>
  arrange(ags_old)

cat("Entries with missing data:\n")
print(missing_data |> head(50))

# Check specific case: Görlitz ----------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING SPECIFIC CASE: Görlitz (14626110)\n")
cat(strrep("=", 70), "\n\n")

goerlitz_raw <- changes_clean |>
  filter(ags_old == "14626110" | ags_new == "14626110")

cat("Görlitz in raw data:\n")
print(goerlitz_raw |> 
  select(event_id, ags_old, name_old, ags_new, name_new, 
         pop_old, pop_old_num, area_old, area_old_num,
         pop_missing, area_missing))

# Check what type of change Görlitz is -------------------------------

cat("\n", strrep("=", 70), "\n")
cat("ANALYZING CHANGE TYPES\n")
cat(strrep("=", 70), "\n\n")

changes_clean <- changes_clean |>
  mutate(
    type = case_when(
      ags_new == ags_old ~ "boundary_shift",
      ags_new != ags_old & !is.na(ags_new) ~ "merge_or_split",
      is.na(ags_new) ~ "dissolution"
    )
  )

cat("Change types:\n")
print(changes_clean |> count(type))

cat("\nMissing data by change type:\n")
print(
  changes_clean |>
    group_by(type) |>
    summarise(
      n = n(),
      missing_pop = sum(pop_missing, na.rm = TRUE),
      missing_area = sum(area_missing, na.rm = TRUE),
      missing_both = sum(both_missing, na.rm = TRUE),
      .groups = "drop"
    )
)

# Check if missing data is specific to certain change types ----------

cat("\n", strrep("=", 70), "\n")
cat("MISSING DATA BY CHANGE TYPE\n")
cat(strrep("=", 70), "\n\n")

missing_by_type <- changes_clean |>
  filter(pop_missing | area_missing) |>
  count(type, pop_missing, area_missing, both_missing) |>
  arrange(type, desc(n))

print(missing_by_type)

# Check if Görlitz is a boundary shift -------------------------------

goerlitz_type <- goerlitz_raw |>
  mutate(
    type = case_when(
      ags_new == ags_old ~ "boundary_shift",
      ags_new != ags_old & !is.na(ags_new) ~ "merge_or_split",
      is.na(ags_new) ~ "dissolution"
    )
  )

cat("\nGörlitz change type:\n")
print(goerlitz_type |> select(ags_old, name_old, ags_new, name_new, type, 
                               pop_old, area_old, pop_missing, area_missing))

# Check all boundary shifts with missing data -----------------------

cat("\n", strrep("=", 70), "\n")
cat("BOUNDARY SHIFTS WITH MISSING DATA\n")
cat(strrep("=", 70), "\n\n")

boundary_shifts_missing <- changes_clean |>
  filter(type == "boundary_shift" & (pop_missing | area_missing)) |>
  select(event_id, ags_old, name_old, ags_new, name_new,
         pop_old, pop_old_num, area_old, area_old_num,
         pop_missing, area_missing) |>
  arrange(ags_old)

cat("Boundary shifts with missing data:", nrow(boundary_shifts_missing), "\n\n")
if (nrow(boundary_shifts_missing) > 0) {
  print(boundary_shifts_missing |> head(30))
}

# Check if these are identity mappings (ags_old == ags_new) ----------

cat("\n", strrep("=", 70), "\n")
cat("IDENTITY MAPPINGS (ags_old == ags_new) WITH MISSING DATA\n")
cat(strrep("=", 70), "\n\n")

identity_missing <- changes_clean |>
  filter(ags_old == ags_new & (pop_missing | area_missing)) |>
  select(event_id, ags_old, name_old, ags_new, name_new,
         pop_old, area_old, pop_missing, area_missing) |>
  arrange(ags_old)

cat("Identity mappings with missing data:", nrow(identity_missing), "\n\n")
if (nrow(identity_missing) > 0) {
  print(identity_missing)
  
  cat("\nUnique AGS with identity mappings and missing data:\n")
  print(identity_missing |> distinct(ags_old) |> arrange(ags_old))
}

# Check for name changes (law_short == 4) ------------------------------

cat("\n", strrep("=", 70), "\n")
cat("NAME CHANGES (law_short == 4) WITH MISSING DATA\n")
cat(strrep("=", 70), "\n\n")

name_changes_missing <- changes_clean |>
  filter(law_short == "4" & (pop_missing | area_missing)) |>
  select(event_id, ags_old, name_old, ags_new, name_new, law_short,
         pop_old, area_old, pop_missing, area_missing) |>
  arrange(ags_old)

cat("Name changes (law_short == 4) with missing data:", nrow(name_changes_missing), "\n\n")
if (nrow(name_changes_missing) > 0) {
  print(name_changes_missing)
  
  cat("\nUnique AGS with name changes and missing data:\n")
  print(name_changes_missing |> distinct(ags_old) |> arrange(ags_old))
}

# Check overlap: identity mappings that are also name changes ----------

cat("\n", strrep("=", 70), "\n")
cat("OVERLAP: IDENTITY MAPPINGS THAT ARE NAME CHANGES\n")
cat(strrep("=", 70), "\n\n")

identity_and_name_change <- changes_clean |>
  filter(ags_old == ags_new & law_short == "4" & (pop_missing | area_missing)) |>
  select(event_id, ags_old, name_old, ags_new, name_new, law_short,
         pop_old, area_old, pop_missing, area_missing) |>
  arrange(ags_old)

cat("Identity mappings that are also name changes with missing data:", 
    nrow(identity_and_name_change), "\n\n")
if (nrow(identity_and_name_change) > 0) {
  print(identity_and_name_change)
}

# Summary: All change types with missing data -------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY: MISSING DATA BY law_short\n")
cat(strrep("=", 70), "\n\n")

missing_by_law <- changes_clean |>
  filter(pop_missing | area_missing) |>
  count(law_short, pop_missing, area_missing) |>
  arrange(law_short, desc(n))

cat("Missing data by law_short:\n")
print(missing_by_law)

cat("\nAll entries with missing data, showing law_short:\n")
print(
  changes_clean |>
    filter(pop_missing | area_missing) |>
    select(event_id, ags_old, name_old, ags_new, name_new, law_short,
           pop_old, area_old, pop_missing, area_missing) |>
    arrange(law_short, ags_old)
)

# Check raw Excel structure for Görlitz ------------------------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING RAW EXCEL STRUCTURE\n")
cat(strrep("=", 70), "\n\n")

# Read raw Excel without processing to see actual values
cat("Reading raw Excel for Görlitz (first 20 rows around Görlitz)...\n")

# Try to find Görlitz in the raw file
raw_2024 <- read_excel(path_24, sheet = "Gebietsaenderungen", col_names = FALSE)

# Find row with Görlitz
goerlitz_row <- raw_2024 |>
  mutate(row_num = row_number()) |>
  filter(if_any(everything(), ~ str_detect(as.character(.x), "14626110|Görlitz"))) |>
  head(5)

if (nrow(goerlitz_row) > 0) {
  cat("\nFound Görlitz in raw Excel (2024 file):\n")
  print(goerlitz_row)
  
  # Show surrounding rows for context
  if (nrow(goerlitz_row) > 0) {
    row_idx <- goerlitz_row$row_num[1]
    context_rows <- raw_2024 |>
      slice(max(1, row_idx - 2):min(nrow(raw_2024), row_idx + 2)) |>
      mutate(row_num = row_number() + max(1, row_idx - 3))
    
    cat("\nContext (rows around Görlitz):\n")
    print(context_rows)
  }
}

# Check 2025 file for Görlitz
raw_2025 <- read_excel(path_25, sheet = "Gebietsaenderungen", col_names = FALSE)

goerlitz_row_25 <- raw_2025 |>
  mutate(row_num = row_number()) |>
  filter(if_any(everything(), ~ str_detect(as.character(.x), "14626110|Görlitz"))) |>
  head(5)

if (nrow(goerlitz_row_25) > 0) {
  cat("\nFound Görlitz in raw Excel (2025 file):\n")
  print(goerlitz_row_25)
}

# Summary of all affected municipalities ------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY: ALL AFFECTED MUNICIPALITIES\n")
cat(strrep("=", 70), "\n\n")

affected_ags <- changes_clean |>
  filter(pop_missing | area_missing) |>
  distinct(ags_old, name_old) |>
  arrange(ags_old)

cat("Total unique municipalities with missing data:", nrow(affected_ags), "\n\n")
print(affected_ags)

# Check if these municipalities appear in other years ----------------

cat("\n", strrep("=", 70), "\n")
cat("CHECKING IF AFFECTED MUNICIPALITIES HAVE DATA IN OTHER YEARS\n")
cat(strrep("=", 70), "\n\n")

# Check if these AGS appear with data in the same file
for (ags_check in head(affected_ags$ags_old, 10)) {
  ags_data <- changes_clean |>
    filter(ags_old == ags_check | ags_new == ags_check) |>
    select(event_id, ags_old, name_old, ags_new, name_new,
           pop_old, pop_old_num, area_old, area_old_num,
           pop_missing, area_missing)
  
  if (nrow(ags_data) > 0) {
    cat("\nAGS:", ags_check, "\n")
    print(ags_data)
  }
}

# Save results --------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("Saving diagnostic results...\n")
cat(strrep("=", 70), "\n\n")

# Create summary
diagnostic_summary <- changes_clean |>
  filter(pop_missing | area_missing) |>
  select(
    event_id, ags_old, name_old, ags_new, name_new,
    pop_old, pop_old_num, area_old, area_old_num,
    pop_missing, area_missing, both_missing, type
  ) |>
  arrange(ags_old, event_id)

write_csv(diagnostic_summary, "data/data_checks/crosswalk_raw_data_missing_diagnostic.csv")

cat("Diagnostic summary saved to: data/data_checks/crosswalk_raw_data_missing_diagnostic.csv\n")
cat("\nScript completed!\n")

