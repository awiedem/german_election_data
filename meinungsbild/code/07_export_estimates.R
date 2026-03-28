## 07_export_estimates.R
## Export MRP estimates to JSON (for website) and CSV/Excel (for download)
##
## Inputs:  meinungsbild/data/estimates/estimates_kreis.rds
##          meinungsbild/data/estimates/estimates_bundesland.rds
## Outputs: meinungsbild/web/public/data/estimates_kreis.json
##          meinungsbild/web/public/data/estimates_bundesland.json
##          meinungsbild/output/tables/estimates_kreis.csv
##          meinungsbild/output/tables/estimates_bundesland.csv

library(tidyverse)
library(jsonlite)

mb_root <- file.path(here::here(), "meinungsbild")

dir.create(file.path(mb_root, "web", "public", "data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(mb_root, "output", "tables"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Load estimates -----------------------------------------------------

estimates_kreis <- readRDS(file.path(mb_root, "data", "estimates", "estimates_kreis.rds"))
estimates_bl    <- readRDS(file.path(mb_root, "data", "estimates", "estimates_bundesland.rds"))

# ---- 2. Load geography names -----------------------------------------------

kreis_cov <- readRDS(file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))

bundesland_names <- tibble(
  state_code = c("01", "02", "03", "04", "05", "06", "07", "08",
                 "09", "10", "11", "12", "13", "14", "15", "16"),
  state_name = c(
    "Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen",
    "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Baden-Wuerttemberg",
    "Bayern", "Saarland", "Berlin", "Brandenburg",
    "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thueringen"
  )
)

# ---- 3. Enrich with names and round ----------------------------------------

estimates_kreis_out <- estimates_kreis |>
  left_join(
    kreis_cov |> select(county_code, county_name, state_code),
    by = "county_code"
  ) |>
  left_join(bundesland_names, by = "state_code") |>
  mutate(across(where(is.numeric) & !matches("pop"), ~ round(.x, 4)))

estimates_bl_out <- estimates_bl |>
  left_join(bundesland_names, by = "state_code") |>
  mutate(across(where(is.numeric) & !matches("pop"), ~ round(.x, 4)))

# ---- 4. Export JSON for website --------------------------------------------

# Nested structure: { issue_id: { county_code: { estimate, sd, ... } } }
kreis_nested <- estimates_kreis_out |>
  group_by(issue_id) |>
  group_split() |>
  set_names(map_chr(., ~ first(.x$issue_id))) |>
  map(function(df) {
    df |>
      select(county_code, county_name, state_code, state_name,
             estimate, sd, q025, q10, q90, q975, pop) |>
      as.data.frame()
  })

bl_nested <- estimates_bl_out |>
  group_by(issue_id) |>
  group_split() |>
  set_names(map_chr(., ~ first(.x$issue_id))) |>
  map(function(df) {
    df |>
      select(state_code, state_name, estimate, sd, q025, q10, q90, q975, pop) |>
      as.data.frame()
  })

write_json(kreis_nested,
           file.path(mb_root, "web", "public", "data", "estimates_kreis.json"),
           pretty = TRUE, auto_unbox = TRUE)

write_json(bl_nested,
           file.path(mb_root, "web", "public", "data", "estimates_bundesland.json"),
           pretty = TRUE, auto_unbox = TRUE)

# ---- 5. Export CSV for download --------------------------------------------

write_csv(estimates_kreis_out,
          file.path(mb_root, "output", "tables", "estimates_kreis.csv"))
write_csv(estimates_bl_out,
          file.path(mb_root, "output", "tables", "estimates_bundesland.csv"))

# ---- 6. Issue metadata JSON (for website dropdowns) ------------------------

issue_meta_path <- file.path(mb_root, "harmonization")
yaml_files <- list.files(issue_meta_path, pattern = "\\.yaml$", full.names = TRUE)

if (length(yaml_files) > 0 && requireNamespace("yaml", quietly = TRUE)) {
  issue_metadata <- map(yaml_files, yaml::read_yaml)
  names(issue_metadata) <- tools::file_path_sans_ext(basename(yaml_files))
  write_json(issue_metadata,
             file.path(mb_root, "web", "public", "data", "issues.json"),
             pretty = TRUE, auto_unbox = TRUE)
  message("Exported issue metadata for ", length(issue_metadata), " issues")
} else {
  message("No YAML harmonization files found or yaml package not installed.")
}

message("Export complete:")
message("  JSON: web/public/data/estimates_*.json")
message("  CSV:  output/tables/estimates_*.csv")
