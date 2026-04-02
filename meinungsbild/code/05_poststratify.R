## 05_poststratify.R
## Poststratify MRP model predictions to produce geographic estimates
## at three levels: Bundesland (16), Kreis (~401), and Wahlkreis (299).
##
## theta_g = sum_j (N_jg / N_g) * Pr(y=1 | demographics_j, geography_g)
##
## Because Wahlkreise don't nest in Kreise, we build separate poststratification
## tables for each target geography:
##   - Kreis poststrat: Zensus cells at Kreis level → aggregate to Bundesland
##   - WKR poststrat:   Kreis cells re-weighted via WKR-to-Kreis crosswalk
##
## Inputs:  meinungsbild/data/estimates/models/fit_[issue_id].rds
##          meinungsbild/data/poststrat/poststrat_kreis.rds
##          meinungsbild/data/covariates/kreis_covariates.rds
##          meinungsbild/data/raw/gles/crosswalk/btw_21_to_krs21.csv
## Outputs: meinungsbild/data/estimates/estimates_kreis.rds
##          meinungsbild/data/estimates/estimates_bundesland.rds
##          meinungsbild/data/estimates/estimates_wkr.rds

library(tidyverse)
library(brms)

mb_root <- file.path(here::here(), "meinungsbild")

dir.create(file.path(mb_root, "data", "estimates"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Load poststratification frame and covariates -----------------------

poststrat <- readRDS(file.path(mb_root, "data", "poststrat", "poststrat_kreis.rds"))
kreis_cov <- readRDS(file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))

# Expected poststrat columns:
#   county_code, age_cat, gender (or male), educ_label, N (population count per cell)
# Ensure naming matches model expectations
if ("gender" %in% names(poststrat) && !"male" %in% names(poststrat)) {
  poststrat$male <- as.integer(poststrat$gender == "male")
}

# ---- 2. WKR-to-Kreis crosswalk for WKR-level poststratification -----------

# Use the most recent crosswalk (2021 boundaries, which BTW 2025 also uses)
wkr_cw <- read_csv(
  file.path(mb_root, "data", "raw", "gles", "crosswalk", "btw_21_to_krs21.csv"),
  show_col_types = FALSE
) |>
  transmute(
    wkr_nr = as.integer(WKR_NR),
    county_code = str_pad(as.character(AGS), 5, pad = "0"),
    pct_area = PERCENTAGE / 100  # convert percentage to proportion
  )

# Build WKR-level poststrat table:
# For each WKR × demographic cell, weight = Kreis cell N × area share
poststrat_wkr <- poststrat |>
  inner_join(wkr_cw, by = "county_code", relationship = "many-to-many") |>
  mutate(N_wkr = N * pct_area) |>
  group_by(wkr_nr, age_cat, male, educ_label) |>
  summarise(
    N = sum(N_wkr, na.rm = TRUE),
    # For covariates: take population-weighted average across constituent Kreise
    .groups = "drop"
  )

message("Poststrat tables: ",
        nrow(poststrat), " Kreis cells, ",
        nrow(poststrat_wkr), " WKR cells")

# ---- 3. Poststratification from posterior draws (Bayesian) -----------------

poststratify_draws <- function(pp_matrix, pred_data, group_var) {
  # pp_matrix: n_draws x n_obs
  # For each draw, compute population-weighted mean by group

  groups <- pred_data[[group_var]]
  weights <- pred_data$N
  unique_groups <- sort(unique(groups))

  # Pre-allocate

  result <- matrix(NA_real_, nrow = nrow(pp_matrix), ncol = length(unique_groups))
  colnames(result) <- unique_groups

  for (g in seq_along(unique_groups)) {
    idx <- which(groups == unique_groups[g])
    w   <- weights[idx]
    w   <- w / sum(w, na.rm = TRUE)
    result[, g] <- pp_matrix[, idx, drop = FALSE] %*% w
  }

  # Summarise posterior distribution
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

# ---- 4. Poststratify one issue at all geographic levels --------------------

poststratify_issue <- function(issue_id, poststrat_kreis, poststrat_wkr_df,
                               kreis_cov_df, use_brms = TRUE) {
  model_path <- file.path(mb_root, "data", "estimates", "models",
                          paste0("fit_", issue_id, ".rds"))
  if (!file.exists(model_path)) {
    warning("Model not found for ", issue_id, " — skipping.")
    return(NULL)
  }

  fit <- readRDS(model_path)

  # ---- Kreis-level predictions ----
  pred_kreis <- poststrat_kreis |>
    left_join(kreis_cov_df, by = "county_code") |>
    mutate(
      survey_source = levels(fit$data$survey_source)[1],  # reference level
      legperiod     = sort(unique(fit$data$legperiod))[
        length(unique(fit$data$legperiod))],  # most recent period
      age_cat       = factor(age_cat,
                             levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
      educ_label    = factor(educ_label,
                             levels = c("no_degree", "hauptschule", "realschule",
                                        "abitur", "university")),
      state_code    = factor(str_sub(county_code, 1, 2)),
      county_code   = factor(county_code),
      wkr_nr        = NA  # not used for Kreis-level prediction
    )

  if (use_brms) {
    message("  Posterior predictions for Kreis poststrat cells (n=", nrow(pred_kreis), ")...")
    pp_kreis <- posterior_epred(fit, newdata = pred_kreis,
                                allow_new_levels = TRUE,
                                sample_new_levels = "gaussian")

    est_kreis <- poststratify_draws(pp_kreis, pred_kreis, "county_code")
    est_land  <- poststratify_draws(pp_kreis, pred_kreis, "state_code")

    # ---- WKR-level predictions ----
    # Need to generate predictions for WKR poststrat cells
    # Map WKR to constituent Kreise for covariates
    pred_wkr <- poststrat_wkr_df |>
      # Assign covariates: use population-weighted average across constituent Kreise
      left_join(
        wkr_cw |>
          left_join(kreis_cov_df, by = "county_code") |>
          group_by(wkr_nr) |>
          summarise(across(where(is.numeric), ~ weighted.mean(.x, pct_area, na.rm = TRUE)),
                    .groups = "drop") |>
          select(-pct_area),
        by = "wkr_nr"
      ) |>
      mutate(
        survey_source = levels(fit$data$survey_source)[1],
        legperiod     = sort(unique(fit$data$legperiod))[
          length(unique(fit$data$legperiod))],
        age_cat       = factor(age_cat,
                               levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
        educ_label    = factor(educ_label,
                               levels = c("no_degree", "hauptschule", "realschule",
                                          "abitur", "university")),
        state_code    = NA_character_,  # WKR doesn't nest in states cleanly
        county_code   = NA_character_,  # cross-classified
        wkr_nr        = factor(wkr_nr)
      )

    message("  Posterior predictions for WKR poststrat cells (n=", nrow(pred_wkr), ")...")
    pp_wkr <- posterior_epred(fit, newdata = pred_wkr,
                               allow_new_levels = TRUE,
                               sample_new_levels = "gaussian")

    est_wkr <- poststratify_draws(pp_wkr, pred_wkr, "wkr_nr")

  } else {
    # ---- lme4 point predictions ----
    pred_kreis$.pred <- predict(fit, newdata = pred_kreis, type = "response",
                                allow.new.levels = TRUE)

    est_kreis <- pred_kreis |>
      group_by(county_code) |>
      summarise(estimate = weighted.mean(.pred, N, na.rm = TRUE),
                pop = sum(N, na.rm = TRUE), .groups = "drop")

    est_land <- pred_kreis |>
      group_by(state_code) |>
      summarise(estimate = weighted.mean(.pred, N, na.rm = TRUE),
                pop = sum(N, na.rm = TRUE), .groups = "drop")

    pred_wkr <- poststrat_wkr_df |>
      mutate(
        survey_source = levels(fit@frame$survey_source)[1],
        legperiod     = sort(unique(fit@frame$legperiod))[
          length(unique(fit@frame$legperiod))],
        county_code   = NA_character_,
        state_code    = NA_character_
      )
    pred_wkr$.pred <- predict(fit, newdata = pred_wkr, type = "response",
                              allow.new.levels = TRUE)

    est_wkr <- pred_wkr |>
      group_by(wkr_nr) |>
      summarise(estimate = weighted.mean(.pred, N, na.rm = TRUE),
                pop = sum(N, na.rm = TRUE), .groups = "drop")
  }

  list(kreis = est_kreis, bundesland = est_land, wahlkreis = est_wkr)
}

# ---- 5. Run for all issues -------------------------------------------------

model_files <- list.files(
  file.path(mb_root, "data", "estimates", "models"),
  pattern = "^fit_.*\\.rds$", full.names = FALSE
)

issue_ids <- str_remove(str_remove(model_files, "^fit_"), "\\.rds$")

if (length(issue_ids) > 0) {
  # Prepare standardized covariates (same as in 04_fit_mrp.R)
  kreis_cov_std <- kreis_cov |>
    mutate(
      fed_afd_share_z   = scale(fed_afd)[, 1],
      fed_cdu_share_z   = scale(fed_cdu_csu)[, 1],
      fed_spd_share_z   = scale(fed_spd)[, 1],
      fed_turnout_z     = scale(fed_turnout)[, 1],
      log_pop_density_z = scale(log(cty_pop_density + 1))[, 1],
      east              = as.integer(str_sub(county_code, 1, 2) %in%
                                       c("12", "13", "14", "15", "16"))
    )

  if ("Arbeitslosenquote_inkar" %in% names(kreis_cov_std)) {
    kreis_cov_std$unemployment_z <- scale(kreis_cov_std$Arbeitslosenquote_inkar)[, 1]
  } else {
    kreis_cov_std$unemployment_z <- 0
  }

  all_estimates <- map(issue_ids, function(id) {
    message("\n=== Poststratifying: ", id, " ===")
    poststratify_issue(id, poststrat, poststrat_wkr, kreis_cov_std)
  }, .progress = TRUE)
  names(all_estimates) <- issue_ids

  # Stack Kreis estimates
  estimates_kreis <- map_dfr(issue_ids, function(id) {
    est <- all_estimates[[id]]$kreis
    if (!is.null(est)) mutate(est, issue_id = id)
  })

  # Stack Bundesland estimates
  estimates_bl <- map_dfr(issue_ids, function(id) {
    est <- all_estimates[[id]]$bundesland
    if (!is.null(est)) mutate(est, issue_id = id)
  })

  # Stack Wahlkreis estimates
  estimates_wkr <- map_dfr(issue_ids, function(id) {
    est <- all_estimates[[id]]$wahlkreis
    if (!is.null(est)) mutate(est, issue_id = id)
  })

  # Save
  saveRDS(estimates_kreis,
          file.path(mb_root, "data", "estimates", "estimates_kreis.rds"))
  saveRDS(estimates_bl,
          file.path(mb_root, "data", "estimates", "estimates_bundesland.rds"))
  saveRDS(estimates_wkr,
          file.path(mb_root, "data", "estimates", "estimates_wkr.rds"))

  message("\nSaved estimates for ", length(issue_ids), " issues across 3 geographic levels")
  message("  Kreis:      ", nrow(estimates_kreis), " rows")
  message("  Bundesland: ", nrow(estimates_bl), " rows")
  message("  Wahlkreis:  ", nrow(estimates_wkr), " rows")

} else {
  message("No fitted models found. Run 04_fit_mrp.R first.")
}
