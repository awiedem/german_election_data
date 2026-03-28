## 05_poststratify.R
## Poststratify MRP model predictions to produce geographic estimates
##
## theta_g = sum_j (N_jg / N_g) * Pr(y=1 | demographics_j, geography_g)
##
## Inputs:  meinungsbild/data/estimates/models/fit_[issue_id].rds
##          meinungsbild/data/poststrat/poststrat_kreis.rds
##          meinungsbild/data/covariates/kreis_covariates.rds
## Outputs: meinungsbild/data/estimates/estimates_kreis.rds
##          meinungsbild/data/estimates/estimates_bundesland.rds

library(tidyverse)
library(brms)

mb_root <- file.path(here::here(), "meinungsbild")

# ---- 1. Load poststratification frame and covariates -----------------------

poststrat <- readRDS(file.path(mb_root, "data", "poststrat", "poststrat_kreis.rds"))
kreis_cov <- readRDS(file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))

# Expected poststrat columns:
#   county_code, age_group, gender, education, N (population count per cell)

# ---- 2. Poststratification function ----------------------------------------

poststratify_issue <- function(issue_id, poststrat_df, kreis_cov_df,
                               use_brms = TRUE) {
  model_path <- file.path(mb_root, "data", "estimates", "models",
                          paste0("fit_", issue_id, ".rds"))
  if (!file.exists(model_path)) {
    warning("Model not found for ", issue_id, " — skipping.")
    return(NULL)
  }

  fit <- readRDS(model_path)

  # Build prediction grid: one row per poststrat cell per Kreis
  pred_data <- poststrat_df |>
    left_join(kreis_cov_df, by = "county_code") |>
    mutate(
      survey_source = "politbarometer",  # reference level for prediction
      year = max(year, na.rm = TRUE)      # most recent period
    )

  if (use_brms) {
    # Posterior predictions (full uncertainty)
    pp <- posterior_epred(fit, newdata = pred_data, allow_new_levels = TRUE,
                          sample_new_levels = "gaussian")
    # pp is draws x observations matrix

    # Poststratify: population-weighted average per geography
    estimates_kreis <- poststratify_draws(pp, pred_data, group_var = "county_code")
    estimates_land  <- poststratify_draws(pp, pred_data, group_var = "state_code")
  } else {
    # Point predictions from lme4
    pred_data$.pred <- predict(fit, newdata = pred_data, type = "response",
                               allow.new.levels = TRUE)

    estimates_kreis <- pred_data |>
      group_by(county_code) |>
      summarise(
        estimate = weighted.mean(.pred, N, na.rm = TRUE),
        n_cells  = n(),
        pop      = sum(N, na.rm = TRUE),
        .groups  = "drop"
      )

    estimates_land <- pred_data |>
      group_by(state_code) |>
      summarise(
        estimate = weighted.mean(.pred, N, na.rm = TRUE),
        n_cells  = n(),
        pop      = sum(N, na.rm = TRUE),
        .groups  = "drop"
      )
  }

  list(kreis = estimates_kreis, bundesland = estimates_land)
}

# ---- 3. Poststratify from posterior draws (Bayesian) -----------------------

poststratify_draws <- function(pp_matrix, pred_data, group_var) {
  # pp_matrix: n_draws x n_obs
  # For each draw, compute population-weighted mean by group

  groups <- pred_data[[group_var]]
  weights <- pred_data$N
  unique_groups <- unique(groups)

  # Pre-compute group membership and weights
  result <- matrix(NA_real_, nrow = nrow(pp_matrix), ncol = length(unique_groups))
  colnames(result) <- unique_groups

  for (g in seq_along(unique_groups)) {
    idx <- which(groups == unique_groups[g])
    w   <- weights[idx]
    w   <- w / sum(w, na.rm = TRUE)
    result[, g] <- pp_matrix[, idx, drop = FALSE] %*% w
  }

  # Summarise posterior
  tibble(
    !!group_var := unique_groups,
    estimate    = colMeans(result),
    sd          = apply(result, 2, sd),
    q025        = apply(result, 2, quantile, 0.025),
    q10         = apply(result, 2, quantile, 0.10),
    q90         = apply(result, 2, quantile, 0.90),
    q975        = apply(result, 2, quantile, 0.975),
    pop         = map_dbl(unique_groups, ~ sum(weights[groups == .x], na.rm = TRUE))
  )
}

# ---- 4. Run for all issues -------------------------------------------------

model_files <- list.files(
  file.path(mb_root, "data", "estimates", "models"),
  pattern = "^fit_.*\\.rds$", full.names = FALSE
)

issue_ids <- str_remove(str_remove(model_files, "^fit_"), "\\.rds$")

if (length(issue_ids) > 0) {
  all_estimates <- map(issue_ids, function(id) {
    message("Poststratifying: ", id)
    poststratify_issue(id, poststrat, kreis_cov)
  }, .progress = TRUE)
  names(all_estimates) <- issue_ids

  # Stack all Kreis estimates
  estimates_kreis <- map_dfr(issue_ids, function(id) {
    est <- all_estimates[[id]]$kreis
    if (!is.null(est)) mutate(est, issue_id = id)
  })

  # Stack all Bundesland estimates
  estimates_bl <- map_dfr(issue_ids, function(id) {
    est <- all_estimates[[id]]$bundesland
    if (!is.null(est)) mutate(est, issue_id = id)
  })

  saveRDS(estimates_kreis,
          file.path(mb_root, "data", "estimates", "estimates_kreis.rds"))
  saveRDS(estimates_bl,
          file.path(mb_root, "data", "estimates", "estimates_bundesland.rds"))

  message("Saved estimates for ", length(issue_ids), " issues")
} else {
  message("No fitted models found. Run 04_fit_mrp.R first.")
}
