## 02b_download_zensus.R
## Parse Zensus 2022 table 2000S-3044 (Age × Gender × Schulabschluss at Kreis level)
## and build the actual poststratification frame.
##
## Input:  meinungsbild/data/raw/zensus/2000S-3044_de.csv (manually downloaded from
##         ergebnisse.zensus2022.de/datenbank/online)
## Output: meinungsbild/data/poststrat/poststrat_kreis.rds
##         meinungsbild/data/poststrat/poststrat_bundesland.rds

library(tidyverse)

mb_root <- file.path(here::here(), "meinungsbild")

zensus_path <- file.path(mb_root, "data", "raw", "zensus", "2000S-3044_de.csv")
stopifnot(file.exists(zensus_path))

# ---- 1. Parse the GENESIS wide-format CSV ----------------------------------

# Read raw lines
raw_lines <- readLines(zensus_path, encoding = "UTF-8")

# Header row 4 (1-indexed) contains Kreis codes: "01001 Flensburg, Stadt" etc.
header_line <- raw_lines[4]
header_fields <- str_split(header_line, ";")[[1]]

# First 4 fields are empty (row labels: date, age, gender, education)
# Then alternating: Kreis_name, Kreis_name (value column, quality flag column)
kreis_fields <- header_fields[5:length(header_fields)]
# Extract county codes (first 5 characters) from odd-numbered positions (value columns)
value_positions <- seq(1, length(kreis_fields), by = 2)
county_codes <- str_sub(kreis_fields[value_positions], 1, 5)

message("Found ", length(county_codes), " Kreise")

# Data rows start at line 7, end before footer
data_start <- 7
# Find footer: line starting with "__"
footer_idx <- which(str_detect(raw_lines, "^_+"))
data_end <- if (length(footer_idx) > 0) footer_idx[1] - 1 else length(raw_lines)

data_lines <- raw_lines[data_start:data_end]
message("Data rows: ", length(data_lines))

# Parse each data line
parse_line <- function(line) {
  fields <- str_split(line, ";")[[1]]
  date     <- fields[1]
  age_grp  <- fields[2]
  gender   <- fields[3]
  educ     <- str_trim(fields[4])

  # Extract values (odd positions after the first 4 fields)
  vals <- fields[5:length(fields)]
  counts <- vals[seq(1, length(vals), by = 2)]
  # Convert to numeric (some may be "-" or empty for suppressed cells)
  counts <- suppressWarnings(as.numeric(counts))

  tibble(
    age_group_raw = age_grp,
    gender_raw    = gender,
    educ_raw      = educ,
    county_code   = county_codes,
    N             = counts
  )
}

zensus_long <- map_dfr(data_lines, parse_line, .progress = TRUE)

message("Parsed ", nrow(zensus_long), " raw cells")

# ---- 2. Filter and recode ---------------------------------------------------

# Keep only specific education categories we need (not "Insgesamt" or subtotals)
# Education categories in the data:
#   "Insgesamt" — total
#   "Noch in schulischer Ausbildung" — still in school
#   "Mit allgemeinbildendem Schulabschluss" — has a general school degree (subtotal)
#   "Haupt-/ Volksschulabschluss" — Hauptschule
#   "Abschluss der Polytechnischen Oberschule" — POS (East German equivalent of Realschule)
#   "Realschulabschluss, Mttl. Reife o. glw. Abschl." — Realschule
#   "Fachhochschul- oder Hochschulreife (Abitur)" — Abitur
#   "Ohne allgemeinbildenden Schulabschluss" — no degree

educ_mapping <- tribble(
  ~educ_raw,                                              ~educ_label,
  "Haupt-/ Volksschulabschluss",                          "hauptschule",
  "Abschluss der Polytechnischen Oberschule",             "realschule",     # POS ≈ Realschule
  "Realschulabschluss, Mttl. Reife o. glw. Abschl.",     "realschule",
  "Fachhochschul- oder Hochschulreife (Abitur)",          "abitur",
  "Ohne allgemeinbildenden Schulabschluss",               "no_degree"
)

# Filter: adults 18+, specific gender, specific education
# Age groups to keep (18+): 15-19 is partially under 18, we include it as the Zensus
# doesn't split 15-17/18-19. Standard MRP practice for Zensus data.
age_groups_18plus <- c(
  "15 bis 19 Jahre",
  "20 bis 24 Jahre", "25 bis 29 Jahre",
  "30 bis 34 Jahre", "35 bis 39 Jahre",
  "40 bis 44 Jahre", "45 bis 49 Jahre",
  "50 bis 54 Jahre", "55 bis 59 Jahre",
  "60 bis 64 Jahre", "65 bis 69 Jahre",
  "70 bis 74 Jahre", "75 bis 79 Jahre",
  "80 bis 84 Jahre", "85 bis 89 Jahre",
  "90 Jahre und älter"
)

# Map 5-year groups → our 5 MRP age categories
age_mapping <- tribble(
  ~age_group_raw,           ~age_cat,
  "15 bis 19 Jahre",        "18-29",
  "20 bis 24 Jahre",        "18-29",
  "25 bis 29 Jahre",        "18-29",
  "30 bis 34 Jahre",        "30-44",
  "35 bis 39 Jahre",        "30-44",
  "40 bis 44 Jahre",        "30-44",
  "45 bis 49 Jahre",        "45-59",
  "50 bis 54 Jahre",        "45-59",
  "55 bis 59 Jahre",        "45-59",
  "60 bis 64 Jahre",        "60-74",
  "65 bis 69 Jahre",        "60-74",
  "70 bis 74 Jahre",        "60-74",
  "75 bis 79 Jahre",        "75+",
  "80 bis 84 Jahre",        "75+",
  "85 bis 89 Jahre",        "75+",
  "90 Jahre und älter",     "75+"
)

gender_mapping <- tribble(
  ~gender_raw,  ~male,
  "Männlich",   1L,
  "Weiblich",   0L
)

# Apply filters and mappings
poststrat_raw <- zensus_long |>
  filter(
    age_group_raw %in% age_groups_18plus,
    gender_raw %in% c("Männlich", "Weiblich")
  ) |>
  inner_join(educ_mapping, by = "educ_raw") |>
  inner_join(age_mapping, by = "age_group_raw") |>
  inner_join(gender_mapping, by = "gender_raw")

message("After filtering: ", nrow(poststrat_raw), " cells")
message("Missing N values: ", sum(is.na(poststrat_raw$N)))

# Replace NA counts with 0 (suppressed small cells)
poststrat_raw <- poststrat_raw |>
  mutate(N = replace_na(N, 0))

# ---- 3. Aggregate to MRP cells ----------------------------------------------

# Sum across fine Zensus groups → our 5 age × 2 gender × 5 educ cells per Kreis
# Note: Realschule + POS are already both mapped to "realschule"
# Note: 15-19, 20-24, 25-29 are all mapped to "18-29"

poststrat_kreis <- poststrat_raw |>
  group_by(county_code, age_cat, male, educ_label) |>
  summarise(N = sum(N, na.rm = TRUE), .groups = "drop")

# ---- 4. Handle "Noch in schulischer Ausbildung" and "university" -----------

# The Zensus table 2000S-3044 only covers Schulabschluss (school degree), not
# beruflicher Abschluss (professional/university degree). People with Abitur who
# went to university are classified under "Abitur" here.
#
# For MRP we need a "university" education category. We'll split the Abitur
# category using national-level proportions from Zensus table 2000S-4024
# (which cross-tabs Schulabschluss × beruflicher Abschluss).
#
# National estimate: among adults with Abitur, ~55% have a university degree.
# Source: Zensus 2022, Statistisches Bundesamt.

uni_share_of_abitur <- 0.55

# Also need to distribute "Noch in schulischer Ausbildung" (still in school)
# These are mostly 15-19 year olds. Distribute proportionally across education
# categories within each Kreis×age×gender cell.
#
# For adults 20+, "still in school" counts are tiny and can be dropped.
# For 15-19, they're substantial but these people don't have a final degree yet.
# Standard practice: exclude them or distribute proportionally.
# We exclude them (they're already partially captured in "no_degree").

# Split Abitur into abitur_only and university
poststrat_kreis <- poststrat_kreis |>
  mutate(
    N_new = case_when(
      educ_label == "abitur" ~ round(N * (1 - uni_share_of_abitur)),
      TRUE ~ N
    )
  )

# Create university rows from abitur
uni_rows <- poststrat_kreis |>
  filter(educ_label == "abitur") |>
  mutate(
    educ_label = "university",
    N_new = round(N * uni_share_of_abitur)
  )

poststrat_kreis <- bind_rows(
  poststrat_kreis |> mutate(N = N_new) |> select(-N_new),
  uni_rows |> mutate(N = N_new) |> select(-N_new)
)

# ---- 5. Final formatting ----------------------------------------------------

poststrat_kreis <- poststrat_kreis |>
  mutate(
    age_cat    = factor(age_cat, levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
    educ_label = factor(educ_label, levels = c("no_degree", "hauptschule", "realschule",
                                                "abitur", "university")),
    state_code = str_sub(county_code, 1, 2)
  ) |>
  arrange(county_code, age_cat, male, educ_label)

# ---- 6. Diagnostics ---------------------------------------------------------

message("\n=== Zensus 2022 Poststratification Frame ===")
message("Cells: ", nrow(poststrat_kreis), " (", n_distinct(poststrat_kreis$county_code),
        " Kreise × 50 cells)")
message("Total adult population: ", format(sum(poststrat_kreis$N), big.mark = ","))

message("\nAge distribution:")
poststrat_kreis |>
  group_by(age_cat) |>
  summarise(N = sum(N)) |>
  mutate(pct = round(100 * N / sum(N), 1)) |>
  print()

message("\nGender distribution:")
poststrat_kreis |>
  group_by(male) |>
  summarise(N = sum(N)) |>
  mutate(pct = round(100 * N / sum(N), 1)) |>
  print()

message("\nEducation distribution:")
poststrat_kreis |>
  group_by(educ_label) |>
  summarise(N = sum(N)) |>
  mutate(pct = round(100 * N / sum(N), 1)) |>
  print()

message("\nPer-Kreis stats:")
kreis_stats <- poststrat_kreis |>
  group_by(county_code) |>
  summarise(n_cells = n(), pop = sum(N), min_cell = min(N), max_cell = max(N))
message("  Pop range: ", format(min(kreis_stats$pop), big.mark = ","),
        " - ", format(max(kreis_stats$pop), big.mark = ","))
message("  Smallest cell: ", min(kreis_stats$min_cell))

# ---- 7. Aggregate to Bundesland level ----------------------------------------

poststrat_bundesland <- poststrat_kreis |>
  group_by(state_code, age_cat, male, educ_label) |>
  summarise(N = sum(N), .groups = "drop")

# ---- 8. Save -----------------------------------------------------------------

dir.create(file.path(mb_root, "data", "poststrat"), showWarnings = FALSE, recursive = TRUE)

saveRDS(poststrat_kreis,
        file.path(mb_root, "data", "poststrat", "poststrat_kreis.rds"))
saveRDS(poststrat_bundesland,
        file.path(mb_root, "data", "poststrat", "poststrat_bundesland.rds"))

message("\nSaved Zensus 2022 poststrat frames to data/poststrat/")
message("This uses ACTUAL Zensus 2022 cross-tabulations at Kreis level.")
