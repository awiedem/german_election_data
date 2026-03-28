## 04_fit_mrp.R
## Fit multilevel regression models for MRP
##
## For each binary policy issue:
##   y_i ~ Bernoulli(logit^-1(
##       beta_age[age_i] + beta_gender[gender_i] + beta_edu[edu_i]
##     + alpha_land[land_i] + alpha_kreis[kreis_i]
##     + beta_survey[source_i] + beta_year * year_i
##   ))
##   alpha_kreis ~ N(gamma * X_kreis, sigma_kreis)
##
## Inputs:  meinungsbild/data/harmonized/survey_pooled.rds
##          meinungsbild/data/covariates/kreis_covariates.rds
## Outputs: meinungsbild/data/estimates/models/fit_[issue_id].rds

library(tidyverse)
library(brms)

mb_root <- file.path(here::here(), "meinungsbild")
dir.create(file.path(mb_root, "data", "estimates", "models"),
           showWarnings = FALSE, recursive = TRUE)

# ---- 1. Load data ----------------------------------------------------------

survey <- readRDS(file.path(mb_root, "data", "harmonized", "survey_pooled.rds"))
kreis_cov <- readRDS(file.path(mb_root, "data", "covariates", "kreis_covariates.rds"))

# Standardize Kreis-level covariates for model stability
kreis_cov_std <- kreis_cov |>
  mutate(across(
    c(fed_cdu_csu, fed_spd, fed_gruene, fed_afd, fed_linke, fed_turnout,
      cty_pop_density),
    ~ scale(.x)[, 1],
    .names = "{.col}_z"
  ))

# Merge covariates into survey data
survey <- survey |>
  left_join(kreis_cov_std, by = "county_code")

# ---- 2. Model specification ------------------------------------------------

# Expected columns in survey_pooled.rds:
#   - y:            binary outcome (1 = support, 0 = oppose)
#   - issue_id:     character, identifies the policy issue
#   - age_group:    factor (18-29, 30-44, 45-59, 60-74, 75+)
#   - gender:       factor (male, female)
#   - education:    factor (no_degree, hauptschule, realschule, abitur, university)
#   - state_code:   character, 2-digit Bundesland code
#   - county_code:  character, 5-digit Kreis code (if available)
#   - survey_source: character (politbarometer, gles, allbus, etc.)
#   - year:         numeric

build_formula <- function(has_kreis = TRUE) {
  if (has_kreis) {
    bf(
      y ~ age_group + gender + education + survey_source +
        (1 | state_code) +
        (1 | county_code) +
        year,
      family = bernoulli(link = "logit")
    )
  } else {
    # Fallback: no Kreis random effect (Bundesland-only geographic variation)
    bf(
      y ~ age_group + gender + education + survey_source +
        (1 | state_code) +
        year,
      family = bernoulli(link = "logit")
    )
  }
}

# Priors: weakly informative
mrp_priors <- c(
  prior(normal(0, 2), class = "b"),
  prior(normal(0, 2), class = "Intercept"),
  prior(normal(0, 1), class = "sd")
)

# ---- 3. Fit function -------------------------------------------------------

fit_mrp_issue <- function(issue_id, data, has_kreis = TRUE,
                          chains = 4, iter = 2000, cores = 4) {
  message("Fitting MRP for issue: ", issue_id)

  issue_data <- data |>
    filter(issue_id == !!issue_id) |>
    drop_na(y, age_group, gender, education, state_code)

  if (nrow(issue_data) < 100) {
    warning("Fewer than 100 observations for ", issue_id, " — skipping.")
    return(NULL)
  }

  # Check if Kreis-level data is available
  has_kreis_data <- has_kreis && !all(is.na(issue_data$county_code))
  formula <- build_formula(has_kreis = has_kreis_data)

  message("  N = ", nrow(issue_data),
          " | Bundeslaender = ", n_distinct(issue_data$state_code),
          if (has_kreis_data) paste0(" | Kreise = ", n_distinct(issue_data$county_code)),
          " | Survey sources = ", n_distinct(issue_data$survey_source))

  fit <- brm(
    formula   = formula,
    data      = issue_data,
    prior     = mrp_priors,
    chains    = chains,
    iter      = iter,
    cores     = cores,
    backend   = "cmdstanr",
    threads   = threading(2),
    seed      = 42,
    file      = file.path(mb_root, "data", "estimates", "models",
                          paste0("fit_", issue_id))
  )

  fit
}

# ---- 4. Prototype: fit with lme4 for rapid iteration ----------------------

fit_mrp_issue_freq <- function(issue_id, data, has_kreis = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Install lme4: install.packages('lme4')")
  }

  message("Fitting frequentist MRP (lme4) for issue: ", issue_id)

  issue_data <- data |>
    filter(issue_id == !!issue_id) |>
    drop_na(y, age_group, gender, education, state_code)

  if (nrow(issue_data) < 100) {
    warning("Fewer than 100 observations for ", issue_id, " — skipping.")
    return(NULL)
  }

  has_kreis_data <- has_kreis && !all(is.na(issue_data$county_code))

  if (has_kreis_data) {
    fit <- lme4::glmer(
      y ~ age_group + gender + education + survey_source + year +
        (1 | state_code) + (1 | county_code),
      data = issue_data,
      family = binomial(link = "logit"),
      control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
  } else {
    fit <- lme4::glmer(
      y ~ age_group + gender + education + survey_source + year +
        (1 | state_code),
      data = issue_data,
      family = binomial(link = "logit"),
      control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
  }

  fit
}

# ---- 5. Run all issues -----------------------------------------------------

# List of issue IDs to estimate (from harmonization configs)
issue_ids <- survey |> distinct(issue_id) |> pull()

message("Issues to estimate: ", paste(issue_ids, collapse = ", "))

# Uncomment to run:
# fits <- map(issue_ids, ~ fit_mrp_issue(.x, survey), .progress = TRUE)
# names(fits) <- issue_ids

# For rapid prototyping, use lme4:
# fits_freq <- map(issue_ids, ~ fit_mrp_issue_freq(.x, survey), .progress = TRUE)
# names(fits_freq) <- issue_ids
