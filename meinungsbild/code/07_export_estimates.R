## 07_export_estimates.R
## Export MRP estimates to JSON (for website) and CSV/Excel (for download)
##
## Inputs:  meinungsbild/data/estimates/estimates_kreis.rds
##          meinungsbild/data/estimates/estimates_bundesland.rds
##          meinungsbild/data/estimates/estimates_wkr.rds
##          meinungsbild/data/issue_concordance.csv
## Outputs: meinungsbild/web/public/data/estimates_kreis.json
##          meinungsbild/web/public/data/estimates_bundesland.json
##          meinungsbild/web/public/data/estimates_wkr.json
##          meinungsbild/web/public/data/issues.json
##          meinungsbild/output/tables/estimates_*.csv

library(tidyverse)
library(jsonlite)

mb_root <- file.path(here::here(), "meinungsbild")

dir.create(file.path(mb_root, "web", "public", "data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(mb_root, "output", "tables"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Load estimates -----------------------------------------------------

estimates_kreis <- readRDS(file.path(mb_root, "data", "estimates", "estimates_kreis.rds"))
estimates_bl    <- readRDS(file.path(mb_root, "data", "estimates", "estimates_bundesland.rds"))
estimates_wkr   <- readRDS(file.path(mb_root, "data", "estimates", "estimates_wkr.rds"))

# ---- 2. Load geography names -----------------------------------------------

kreis_cov <- readRDS(file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))

bundesland_names <- tibble(
  state_code = c("01", "02", "03", "04", "05", "06", "07", "08",
                 "09", "10", "11", "12", "13", "14", "15", "16"),
  state_name = c(
    "Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen",
    "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Baden-Württemberg",
    "Bayern", "Saarland", "Berlin", "Brandenburg",
    "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"
  )
)

# ---- 3. Load issue metadata from concordance --------------------------------

conc <- read_csv(file.path(mb_root, "data", "issue_concordance.csv"),
                 show_col_types = FALSE)

issue_meta <- conc |>
  group_by(issue_id) |>
  summarise(
    label = first(na.omit(label_en)),
    label_de = first(na.omit(label_de)),
    category = first(na.omit(category)),
    question_de = first(na.omit(question_de)),
    question_en = first(na.omit(question_en)),
    response_type = first(na.omit(response_type)),
    binary_rule = first(na.omit(binary_rule)),
    direction = first(na.omit(direction)),
    .groups = "drop"
  )

# ---- 4. Enrich with names and round ----------------------------------------

# Ensure estimate column exists (lme4 output has 'estimate' and 'pop')
est_cols <- intersect(
  c("estimate", "sd", "q025", "q10", "q90", "q975"),
  names(estimates_kreis)
)

estimates_kreis_out <- estimates_kreis |>
  left_join(
    kreis_cov |> select(county_code, county_name, state_code),
    by = "county_code"
  ) |>
  left_join(bundesland_names, by = "state_code") |>
  mutate(estimate = round(estimate, 4))

estimates_bl_out <- estimates_bl |>
  left_join(bundesland_names, by = "state_code") |>
  mutate(estimate = round(estimate, 4))

estimates_wkr_out <- estimates_wkr |>
  mutate(
    wkr_nr = as.integer(wkr_nr),
    estimate = round(estimate, 4)
  )

# ---- 5. Export JSON for website --------------------------------------------

# Helper: nest estimates by issue_id
nest_by_issue <- function(df, geo_cols) {
  splits <- df |>
    group_by(issue_id) |>
    group_split()
  names(splits) <- map_chr(splits, ~ first(.x$issue_id))
  map(splits, function(d) {
    cols <- intersect(c(geo_cols, "estimate", "pop"), names(d))
    d |> select(all_of(cols)) |> as.data.frame()
  })
}

kreis_nested <- nest_by_issue(
  estimates_kreis_out,
  c("county_code", "county_name", "state_code", "state_name")
)

bl_nested <- nest_by_issue(
  estimates_bl_out,
  c("state_code", "state_name")
)

wkr_nested <- nest_by_issue(
  estimates_wkr_out,
  c("wkr_nr")
)

write_json(kreis_nested,
           file.path(mb_root, "web", "public", "data", "estimates_kreis.json"),
           pretty = TRUE, auto_unbox = TRUE)

write_json(bl_nested,
           file.path(mb_root, "web", "public", "data", "estimates_bundesland.json"),
           pretty = TRUE, auto_unbox = TRUE)

write_json(wkr_nested,
           file.path(mb_root, "web", "public", "data", "estimates_wkr.json"),
           pretty = TRUE, auto_unbox = TRUE)

# ---- 6. Export issue metadata JSON ------------------------------------------

issue_list <- issue_meta |>
  filter(issue_id %in% unique(estimates_kreis$issue_id)) |>
  as.data.frame()

write_json(issue_list,
           file.path(mb_root, "web", "public", "data", "issues.json"),
           pretty = TRUE, auto_unbox = TRUE)

message("Exported metadata for ", nrow(issue_list), " issues")

# ---- 7. Export CSV for download --------------------------------------------

write_csv(estimates_kreis_out,
          file.path(mb_root, "output", "tables", "estimates_kreis.csv"))
write_csv(estimates_bl_out,
          file.path(mb_root, "output", "tables", "estimates_bundesland.csv"))
write_csv(estimates_wkr_out,
          file.path(mb_root, "output", "tables", "estimates_wkr.csv"))

message("\nExport complete:")
message("  JSON: web/public/data/estimates_{kreis,bundesland,wkr}.json")
message("  JSON: web/public/data/issues.json")
message("  CSV:  output/tables/estimates_{kreis,bundesland,wkr}.csv")
message("  Issues: ", n_distinct(estimates_kreis$issue_id))
message("  Kreise: ", n_distinct(estimates_kreis$county_code))
message("  Bundesländer: ", n_distinct(estimates_bl$state_code))
message("  Wahlkreise: ", n_distinct(estimates_wkr$wkr_nr))
