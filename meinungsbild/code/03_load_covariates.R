## 03_load_covariates.R
## Load geographic-level covariates for MRP from GERDA and regional statistics
##
## Outputs: meinungsbild/data/covariates/kreis_covariates.rds
##          meinungsbild/data/covariates/bundesland_covariates.rds

library(tidyverse)
library(sf)

# ---- Paths ----------------------------------------------------------------
gerda_root <- here::here()  # german_election_data root
mb_root    <- file.path(gerda_root, "meinungsbild")

dir.create(file.path(mb_root, "data", "covariates"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Federal election results (county level) ---------------------------
# Party vote shares are the single strongest geographic predictor in MRP
fed_cty <- readRDS(file.path(
  gerda_root, "data", "federal_elections", "county_level", "final",
  "federal_cty_harm.rds"
))

# Keep most recent election and key party shares
fed_cty_latest <- fed_cty |>
  filter(election_year == max(election_year)) |>
  transmute(
    county_code = county_code,
    fed_year     = election_year,
    fed_turnout  = turnout,
    fed_cdu_csu  = cdu_csu,
    fed_spd      = spd,
    fed_gruene   = gruene,
    fed_fdp      = fdp,
    fed_linke    = linke_pds,
    fed_afd      = afd
  )

# ---- 2. County-level covariates (population, area, employment) -------------
cty_covars <- readRDS(file.path(
  gerda_root, "data", "covars_county", "final",
  "cty_area_pop_emp.rds"
))

# Most recent year
cty_covars_latest <- cty_covars |>
  filter(year == max(year)) |>
  transmute(
    county_code    = county_code_21,
    county_name    = county_name_21,
    cty_population = population_cty,
    cty_area       = area_cty,
    cty_pop_density = pop_density_cty,
    cty_employees  = employees_cty
  )

# ---- 3. INKAR socioeconomic indicators (county level) ----------------------
inkar_path <- file.path(
  gerda_root, "data", "covars_county", "cty_inkar_harm2021.csv"
)

if (file.exists(inkar_path)) {
  inkar <- read_csv(inkar_path, show_col_types = FALSE)

  # Select key MRP-relevant indicators (most recent year per indicator)
  inkar_vars <- c(
    "county_code_21",
    "year",
    "Arbeitslosenquote_inkar",
    "Ausländeranteil_inkar",
    "Bruttoinlandsprodukt_je_Einwohner_inkar"
  )
  available_vars <- intersect(inkar_vars, names(inkar))

  # INKAR uses 4-digit county codes; pad to 5-digit to match GERDA
  inkar <- inkar |>
    mutate(county_code = str_pad(as.character(county), 5, pad = "0"))

  available_vars <- c("county_code", "year",
                       intersect(inkar_vars[!inkar_vars %in% c("county_code_21", "year")],
                                 names(inkar)))

  inkar_latest <- inkar |>
    select(all_of(available_vars)) |>
    group_by(county_code) |>
    filter(year == max(year)) |>
    ungroup() |>
    select(-year)
} else {
  message("INKAR file not found at: ", inkar_path)
  inkar_latest <- NULL
}

# ---- 4. Derive East/West and state identifiers ----------------------------
# State = first two digits of county_code
derive_state_vars <- function(df) {
  df |>
    mutate(
      state_code = str_sub(county_code, 1, 2),
      east = state_code %in% c("12", "13", "14", "15", "16"),
      # Berlin (11) coded separately
      berlin = state_code == "11"
    )
}

# ---- 5. Merge all county-level covariates ---------------------------------
kreis_covariates <- cty_covars_latest |>
  left_join(fed_cty_latest, by = "county_code") |>
  derive_state_vars()

if (!is.null(inkar_latest)) {
  kreis_covariates <- kreis_covariates |>
    left_join(inkar_latest, by = "county_code")
}

# ---- 6. Aggregate to Bundesland level -------------------------------------
bundesland_covariates <- kreis_covariates |>
  group_by(state_code) |>
  summarise(
    n_kreise        = n(),
    bl_population   = sum(cty_population, na.rm = TRUE),
    bl_pop_density  = sum(cty_population, na.rm = TRUE) / sum(cty_area, na.rm = TRUE),
    # Population-weighted vote shares
    bl_fed_turnout  = weighted.mean(fed_turnout, cty_population, na.rm = TRUE),
    bl_fed_cdu_csu  = weighted.mean(fed_cdu_csu, cty_population, na.rm = TRUE),
    bl_fed_spd      = weighted.mean(fed_spd, cty_population, na.rm = TRUE),
    bl_fed_gruene   = weighted.mean(fed_gruene, cty_population, na.rm = TRUE),
    bl_fed_fdp      = weighted.mean(fed_fdp, cty_population, na.rm = TRUE),
    bl_fed_linke    = weighted.mean(fed_linke, cty_population, na.rm = TRUE),
    bl_fed_afd      = weighted.mean(fed_afd, cty_population, na.rm = TRUE),
    east            = first(east),
    .groups = "drop"
  )

# ---- 7. Save ---------------------------------------------------------------
saveRDS(kreis_covariates,
        file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))
saveRDS(bundesland_covariates,
        file.path(mb_root, "data", "covariates", "bundesland_covariates.rds"))

message("Saved ", nrow(kreis_covariates), " Kreis-level covariates")
message("Saved ", nrow(bundesland_covariates), " Bundesland-level covariates")
