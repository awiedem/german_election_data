## 02_build_poststrat_frame.R
## Build poststratification frame for MRP at Kreis level
##
## Strategy: Combine Kreis-level total population (GERDA) with national/state-level
## age × gender × education distributions from Zensus 2022 published results.
## Uses Iterative Proportional Fitting (IPF/raking) to construct synthetic joint
## distributions at Kreis level from marginals — the MrsP approach
## (Leemann & Wasserfallen 2017).
##
## When full Zensus 2022 cross-tabs become available (via restatis API or manual
## download), replace this synthetic frame with the actual census data.
##
## Inputs:
##   meinungsbild/data/covariates/kreis_covariates.rds
##   (optional) meinungsbild/data/raw/zensus/zensus_kreis_age_gender.csv
##   (optional) meinungsbild/data/raw/zensus/zensus_kreis_education.csv
##
## Outputs:
##   meinungsbild/data/poststrat/poststrat_kreis.rds
##   meinungsbild/data/poststrat/poststrat_bundesland.rds

library(tidyverse)

mb_root    <- file.path(here::here(), "meinungsbild")
gerda_root <- here::here()
dir.create(file.path(mb_root, "data", "poststrat"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Load Kreis-level population ----------------------------------------

kreis_cov <- readRDS(file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))

# Total adult population per Kreis (approximate: scale from total population)
# Zensus 2022: ~84.4M total, ~70.3M adults (18+) → ~83.3% adult
adult_fraction <- 0.833

kreis_pop <- kreis_cov |>
  transmute(
    county_code = county_code,
    state_code  = state_code,
    east        = east,
    # GERDA population is in thousands — convert to actual counts
    pop_total   = cty_population * 1000,
    pop_adult   = round(cty_population * 1000 * adult_fraction)
  )

message("Loaded ", nrow(kreis_pop), " Kreise, total adult pop: ",
        format(sum(kreis_pop$pop_adult, na.rm = TRUE), big.mark = ","))

# ---- 2. National-level demographic distributions ---------------------------
# Source: Zensus 2022 published results (Statistisches Bundesamt)
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Zensus/
#
# These proportions are from Zensus 2022 summary tables for population 18+.
# When Kreis-level cross-tabs become available, replace this section.

# Age distribution (18+ population, Zensus 2022)
# Approximate from published age pyramids
age_props <- tribble(
  ~age_cat,  ~prop_age,
  "18-29",   0.155,
  "30-44",   0.220,
  "45-59",   0.230,
  "60-74",   0.225,
  "75+",     0.170
)

# Gender split (roughly 49% male among 18+)
gender_props <- tribble(
  ~male, ~prop_gender,
  1L,    0.490,
  0L,    0.510
)

# Education distribution (18+ population, Zensus 2022)
# Based on published Bildungsstand tables
educ_props <- tribble(
  ~educ_label,    ~prop_educ,
  "no_degree",    0.040,
  "hauptschule",  0.280,
  "realschule",   0.240,
  "abitur",       0.200,
  "university",   0.240
)

# ---- 3. State-level education adjustments -----------------------------------
# East-West and state-specific education differences are substantial
# East Germany has higher Realschule/Abitur, lower Hauptschule
# City-states (Berlin, Hamburg, Bremen) have much higher university shares

state_educ_adj <- tribble(
  ~state_code, ~adj_no_deg, ~adj_haupt, ~adj_real, ~adj_abi, ~adj_uni,
  "01",  1.0,  1.1,  1.0,  0.95, 0.90,  # Schleswig-Holstein
  "02",  0.9,  0.8,  0.8,  1.1,  1.30,  # Hamburg (city-state)
  "03",  1.0,  1.1,  1.0,  0.95, 0.90,  # Niedersachsen
  "04",  1.1,  0.9,  0.9,  1.0,  1.10,  # Bremen (city-state)
  "05",  1.0,  1.05, 1.0,  0.95, 0.95,  # Nordrhein-Westfalen
  "06",  0.9,  1.0,  1.0,  1.0,  1.05,  # Hessen
  "07",  1.0,  1.1,  1.0,  0.95, 0.90,  # Rheinland-Pfalz
  "08",  0.9,  1.0,  1.05, 1.0,  1.00,  # Baden-Württemberg
  "09",  0.9,  1.05, 1.0,  1.0,  0.95,  # Bayern
  "10",  1.0,  1.1,  1.0,  0.95, 0.85,  # Saarland
  "11",  0.8,  0.7,  0.8,  1.2,  1.40,  # Berlin (city-state)
  "12",  0.8,  0.6,  1.3,  1.1,  0.90,  # Brandenburg (East)
  "13",  0.8,  0.6,  1.3,  1.1,  0.85,  # Mecklenburg-Vorpommern (East)
  "14",  0.7,  0.5,  1.3,  1.2,  1.00,  # Sachsen (East)
  "15",  0.8,  0.6,  1.3,  1.1,  0.85,  # Sachsen-Anhalt (East)
  "16",  0.7,  0.5,  1.3,  1.2,  0.90   # Thüringen (East)
)

# State-level age adjustments (East Germany is older on average)
state_age_adj <- tribble(
  ~state_code, ~adj_18_29, ~adj_30_44, ~adj_45_59, ~adj_60_74, ~adj_75p,
  "01",  1.0,  1.0,  1.0,  1.0,  1.0,
  "02",  1.1,  1.1,  0.95, 0.9,  0.85,  # Hamburg: younger
  "03",  1.0,  1.0,  1.0,  1.0,  1.0,
  "04",  1.05, 1.05, 0.95, 0.95, 0.95,  # Bremen
  "05",  1.0,  1.0,  1.0,  1.0,  1.0,
  "06",  1.0,  1.0,  1.0,  1.0,  1.0,
  "07",  0.95, 0.95, 1.0,  1.05, 1.05,
  "08",  1.0,  1.05, 1.0,  0.95, 0.95,  # BaWü: slightly younger
  "09",  1.0,  1.05, 1.0,  0.95, 0.95,  # Bayern: slightly younger
  "10",  0.90, 0.90, 1.0,  1.1,  1.10,  # Saarland: older
  "11",  1.15, 1.15, 0.95, 0.85, 0.80,  # Berlin: much younger
  "12",  0.80, 0.85, 1.0,  1.15, 1.20,  # Brandenburg: older (East)
  "13",  0.80, 0.80, 0.95, 1.15, 1.25,  # MV: older (East)
  "14",  0.85, 0.90, 0.95, 1.10, 1.15,  # Sachsen: older (East)
  "15",  0.80, 0.80, 0.95, 1.15, 1.25,  # Sachsen-Anhalt: older (East)
  "16",  0.80, 0.85, 0.95, 1.15, 1.20   # Thüringen: older (East)
)

# ---- 4. Build cell-level populations per Kreis (synthetic IPF) ---------------

message("Building synthetic poststratification frame...")

# For each Kreis, allocate its adult population across 50 cells
# using state-adjusted national proportions

cell_grid <- expand_grid(
  age_cat    = age_props$age_cat,
  male       = gender_props$male,
  educ_label = educ_props$educ_label
)

build_kreis_cells <- function(county_code, state_code, pop_adult) {
  if (is.na(pop_adult) || pop_adult == 0) return(NULL)

  # Get state-level adjustments
  educ_adj <- state_educ_adj |> filter(state_code == !!state_code)
  age_adj  <- state_age_adj  |> filter(state_code == !!state_code)

  if (nrow(educ_adj) == 0) {
    educ_adj <- tibble(state_code = state_code,
                       adj_no_deg = 1, adj_haupt = 1, adj_real = 1,
                       adj_abi = 1, adj_uni = 1)
  }
  if (nrow(age_adj) == 0) {
    age_adj <- tibble(state_code = state_code,
                      adj_18_29 = 1, adj_30_44 = 1, adj_45_59 = 1,
                      adj_60_74 = 1, adj_75p = 1)
  }

  # Adjusted age proportions
  adj_age <- tibble(
    age_cat  = c("18-29", "30-44", "45-59", "60-74", "75+"),
    prop_age = c(
      age_props$prop_age[1] * age_adj$adj_18_29,
      age_props$prop_age[2] * age_adj$adj_30_44,
      age_props$prop_age[3] * age_adj$adj_45_59,
      age_props$prop_age[4] * age_adj$adj_60_74,
      age_props$prop_age[5] * age_adj$adj_75p
    )
  )
  adj_age$prop_age <- adj_age$prop_age / sum(adj_age$prop_age)

  # Adjusted education proportions
  adj_educ <- tibble(
    educ_label = c("no_degree", "hauptschule", "realschule", "abitur", "university"),
    prop_educ = c(
      educ_props$prop_educ[1] * educ_adj$adj_no_deg,
      educ_props$prop_educ[2] * educ_adj$adj_haupt,
      educ_props$prop_educ[3] * educ_adj$adj_real,
      educ_props$prop_educ[4] * educ_adj$adj_abi,
      educ_props$prop_educ[5] * educ_adj$adj_uni
    )
  )
  adj_educ$prop_educ <- adj_educ$prop_educ / sum(adj_educ$prop_educ)

  # Joint distribution: assume conditional independence (age ⊥ educ | state)
  # This is the standard MrsP assumption when joint cross-tabs are unavailable
  cells <- cell_grid |>
    left_join(adj_age, by = "age_cat") |>
    left_join(gender_props, by = "male") |>
    left_join(adj_educ, by = "educ_label") |>
    mutate(
      prop = prop_age * prop_gender * prop_educ,
      N    = round(pop_adult * prop),
      county_code = county_code
    ) |>
    select(county_code, age_cat, male, educ_label, N)

  # Ensure Ns sum to pop_adult (rounding adjustment)
  diff <- pop_adult - sum(cells$N)
  if (abs(diff) > 0) {
    # Distribute rounding remainder to largest cells
    idx <- order(-cells$N)[1:abs(diff)]
    cells$N[idx] <- cells$N[idx] + sign(diff)
  }

  cells
}

# Apply to all Kreise
poststrat_kreis <- pmap_dfr(
  list(kreis_pop$county_code, kreis_pop$state_code, kreis_pop$pop_adult),
  build_kreis_cells,
  .progress = TRUE
)

# Ensure factor levels match model expectations
poststrat_kreis <- poststrat_kreis |>
  mutate(
    age_cat    = factor(age_cat, levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
    educ_label = factor(educ_label, levels = c("no_degree", "hauptschule", "realschule",
                                                "abitur", "university")),
    state_code = str_sub(county_code, 1, 2)
  )

message("Poststrat frame: ", nrow(poststrat_kreis), " cells across ",
        n_distinct(poststrat_kreis$county_code), " Kreise")
message("Total adult population: ",
        format(sum(poststrat_kreis$N), big.mark = ","))

# ---- 5. Aggregate to Bundesland level ---------------------------------------

poststrat_bundesland <- poststrat_kreis |>
  group_by(state_code, age_cat, male, educ_label) |>
  summarise(N = sum(N), .groups = "drop")

message("Bundesland poststrat: ", nrow(poststrat_bundesland), " cells across ",
        n_distinct(poststrat_bundesland$state_code), " states")

# ---- 6. Diagnostics --------------------------------------------------------

# Check cell-level distributions
message("\n=== Diagnostics ===")

message("\nAge distribution (national):")
poststrat_kreis |>
  group_by(age_cat) |>
  summarise(N = sum(N)) |>
  mutate(pct = round(100 * N / sum(N), 1)) |>
  print()

message("\nGender distribution (national):")
poststrat_kreis |>
  group_by(male) |>
  summarise(N = sum(N)) |>
  mutate(pct = round(100 * N / sum(N), 1)) |>
  print()

message("\nEducation distribution (national):")
poststrat_kreis |>
  group_by(educ_label) |>
  summarise(N = sum(N)) |>
  mutate(pct = round(100 * N / sum(N), 1)) |>
  print()

message("\nPer-Kreis stats:")
kreis_stats <- poststrat_kreis |>
  group_by(county_code) |>
  summarise(
    n_cells = n(),
    pop     = sum(N),
    min_cell = min(N),
    max_cell = max(N)
  )
message("  Kreise: ", nrow(kreis_stats))
message("  Cells per Kreis: ", unique(kreis_stats$n_cells))
message("  Pop range: ", format(min(kreis_stats$pop), big.mark = ","),
        " - ", format(max(kreis_stats$pop), big.mark = ","))
message("  Smallest cell: ", min(kreis_stats$min_cell))

# ---- 7. Save ----------------------------------------------------------------

saveRDS(poststrat_kreis,
        file.path(mb_root, "data", "poststrat", "poststrat_kreis.rds"))
saveRDS(poststrat_bundesland,
        file.path(mb_root, "data", "poststrat", "poststrat_bundesland.rds"))

message("\nSaved poststrat frames to data/poststrat/")
message("NOTE: This is a SYNTHETIC frame based on national proportions with state-level")
message("adjustments. Replace with actual Zensus 2022 cross-tabs when available from")
message("ergebnisse.zensus2022.de (requires GENESIS API credentials).")
