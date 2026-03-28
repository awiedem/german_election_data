## 02_build_poststrat_frame.R
## Build poststratification frame from Zensus 2022
##
## Target cells: age_group x gender x education at Kreis level
## Source: Zensus 2022 via restatis R package or direct download
##
## Outputs: meinungsbild/data/poststrat/poststrat_kreis.rds
##          meinungsbild/data/poststrat/poststrat_bundesland.rds

library(tidyverse)

mb_root <- file.path(here::here(), "meinungsbild")
dir.create(file.path(mb_root, "data", "poststrat"), showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# OPTION A: Download via restatis (GENESIS API)
# Requires one-time registration at www-genesis.destatis.de
# ============================================================================

build_poststrat_restatis <- function() {
  if (!requireNamespace("restatis", quietly = TRUE)) {
    stop("Install restatis: install.packages('restatis')")
  }
  library(restatis)

  # Table 12211-03-02-4: Population by age groups, gender, education at Kreis level
  # Adjust table code as needed — check GENESIS catalog
  message("Downloading Zensus 2022 cross-tabulation from GENESIS...")
  message("Note: requires GENESIS API credentials. Register at www-genesis.destatis.de")
  message("Then run: restatis::gen_auth(username = 'YOUR_USER', password = 'YOUR_PASS')")

  # TODO: Identify exact table code for Zensus 2022 age x gender x education at Kreis

  # Candidate tables:
  #   - 12211-03-02-4 (Bevoelkerung nach Altersgruppen, Geschlecht, Kreise)
  #   - Check ergebnisse.zensus2022.de for available cross-tabs
  #
  # raw <- restatis::gen_table("12211-03-02-4")
  stop("Table code needs to be verified. See comments above.")
}

# ============================================================================
# OPTION B: Build from manually downloaded Zensus 2022 CSV
# Download from: https://ergebnisse.zensus2022.de
# ============================================================================

build_poststrat_manual <- function(zensus_csv_path) {
  message("Reading Zensus 2022 data from: ", zensus_csv_path)
  raw <- read_csv(zensus_csv_path, show_col_types = FALSE)
  # Harmonization depends on the exact file format — adjust below
  raw
}

# ============================================================================
# OPTION C: Build synthetic frame from marginal distributions (MrsP)
# If joint age x gender x education not available at Kreis level,
# construct synthetic joint distribution from marginals using
# iterative proportional fitting (raking)
# ============================================================================

build_poststrat_synthetic <- function(age_gender_path, education_path) {
  message("Building synthetic poststratification frame via IPF...")

  age_gender <- read_csv(age_gender_path, show_col_types = FALSE)
  education  <- read_csv(education_path, show_col_types = FALSE)

  # Iterative proportional fitting to construct joint distribution
  # from marginal distributions (Leemann & Wasserfallen 2017)
  # TODO: Implement IPF once marginal data format is known
  stop("IPF implementation pending — need marginal data files first.")
}

# ============================================================================
# Define cell structure
# ============================================================================

# Age groups (matching typical survey categories and Zensus reporting)
age_groups <- tibble(
  age_group = c("18-29", "30-44", "45-59", "60-74", "75+"),
  age_group_id = 1:5
)

# Gender
genders <- tibble(
  gender = c("male", "female"),
  gender_id = 1:2
)

# Education levels (mapped to German system)
education_levels <- tibble(
  education = c(
    "no_degree",          # Ohne Abschluss
    "hauptschule",        # Haupt-/Volksschulabschluss
    "realschule",         # Realschulabschluss / Mittlere Reife
    "abitur",             # (Fach-)Hochschulreife
    "university"          # (Fach-)Hochschulabschluss
  ),
  education_id = 1:5
)

# Full cell grid (50 cells per Kreis)
cell_grid <- expand_grid(age_groups, genders, education_levels) |>
  mutate(cell_id = row_number())

message("Poststratification cell grid: ", nrow(cell_grid), " cells per Kreis")
message("(", nrow(age_groups), " age groups x ",
        nrow(genders), " genders x ",
        nrow(education_levels), " education levels)")

# ============================================================================
# Placeholder: once data source is determined, run one of the options above,
# then cross the population counts with the cell grid
# ============================================================================

message("\n--- Next steps ---")
message("1. Register at www-genesis.destatis.de for API access")
message("2. OR download Zensus 2022 cross-tabs manually from ergebnisse.zensus2022.de")
message("3. Identify which tables provide age x gender x education at Kreis level")
message("4. Run the appropriate build function above")
message("5. Output: one row per cell per Kreis with population count (N)")

# Save cell grid definition for reference
saveRDS(cell_grid, file.path(mb_root, "data", "poststrat", "cell_grid.rds"))
message("Saved cell grid definition to data/poststrat/cell_grid.rds")
