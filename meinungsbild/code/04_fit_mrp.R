## 04_fit_mrp.R
## [EXPERIMENTAL] Full Bayesian MRP with structured priors (brms/cmdstanr)
## Not used for production estimates — see 04b_fit_all_lme4.R instead.
## This script is ready but requires substantial compute (hours per issue).
##
## References:
##   - Gao et al. (2021): RW1 priors for ordinal demographics, BYM2 spatial
##   - Ghitza & Gelman (2013): Deep two-way demographic interactions
##   - Goplerud (2024): Deep hierarchical models match ML ensembles
##   - Caughey & Warshaw (2015): Temporal dynamics via RW1
##   - Selb & Munzert (2011): MRP for German Wahlkreise
##
## Model:
##   y ~ 1 +
##       male +                                # gender FE
##       (1 | gr(age_cat, cov = RW1)) +        # RW1 prior for ordinal age
##       (1 | gr(educ_label, cov = RW1)) +     # RW1 prior for ordinal education
##       (1 | male:age_cat) +                  # deep interaction: gender × age
##       (1 | educ_label:age_cat) +            # deep interaction: education × age
##       (1 | male:educ_label) +               # deep interaction: gender × education
##       (1 | survey_source) +                 # survey house effects RE
##       (1 | legperiod) +                     # legislative period RE
##       (1 | state_code) +                    # Bundesland RE
##       (1 | county_code) +                   # Kreis RE (BYM2 if adjacency matrix available)
##       (1 | wkr_nr) +                        # Wahlkreis RE (cross-classified, not nested in Kreis)
##       fed_afd_share_z + fed_cdu_share_z +   # area-level covariates
##       fed_turnout_z + log_pop_density_z +
##       unemployment_z
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

# BYM2 adjacency matrix (built by 03b_build_adjacency.R)
adj_path <- file.path(mb_root, "data", "covariates", "kreis_adjacency.rds")
if (file.exists(adj_path)) {
  W_kreis <- readRDS(adj_path)
  message("Loaded adjacency matrix: ", nrow(W_kreis), " Kreise, ",
          "mean neighbors = ", round(mean(rowSums(W_kreis)), 1))
  use_bym2 <- TRUE
} else {
  message("No adjacency matrix found — using IID county RE instead of BYM2")
  W_kreis <- NULL
  use_bym2 <- FALSE
}

# ---- 2. Prepare Kreis-level covariates -------------------------------------

# Standardize covariates for model stability
kreis_cov_std <- kreis_cov |>
  mutate(
    fed_afd_share_z    = scale(fed_afd)[, 1],
    fed_cdu_share_z    = scale(fed_cdu_csu)[, 1],
    fed_spd_share_z    = scale(fed_spd)[, 1],
    fed_turnout_z      = scale(fed_turnout)[, 1],
    log_pop_density_z  = scale(log(cty_pop_density + 1))[, 1],
    state_code         = str_sub(county_code, 1, 2)
  )

# Add unemployment if available from INKAR
if ("Arbeitslosenquote_inkar" %in% names(kreis_cov_std)) {
  kreis_cov_std$unemployment_z <- scale(kreis_cov_std$Arbeitslosenquote_inkar)[, 1]
} else {
  kreis_cov_std$unemployment_z <- 0
}

# Merge covariates into survey data
survey <- survey |>
  left_join(
    kreis_cov_std |> select(county_code, fed_afd_share_z, fed_cdu_share_z,
                            fed_spd_share_z, fed_turnout_z, log_pop_density_z,
                            unemployment_z),
    by = "county_code"
  )

# For respondents without county_code, use state-level means
state_means <- kreis_cov_std |>
  group_by(state_code) |>
  summarise(across(c(fed_afd_share_z, fed_cdu_share_z, fed_spd_share_z,
                     fed_turnout_z, log_pop_density_z, unemployment_z),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

for (v in c("fed_afd_share_z", "fed_cdu_share_z", "fed_spd_share_z",
            "fed_turnout_z", "log_pop_density_z", "unemployment_z")) {
  miss <- is.na(survey[[v]])
  if (any(miss)) {
    lookup <- state_means |> select(state_code, !!v)
    survey <- rows_patch(survey, lookup, by = "state_code", unmatched = "ignore")
  }
}

# ---- 3. Structured priors: RW1 covariance matrices -------------------------

# RW1 (random walk order 1) penalizes differences between adjacent categories.
# This is the key innovation from Gao et al. (2021) for ordinal demographics.

# Age categories: 18-29, 30-44, 45-59, 60-74, 75+ (5 levels)
make_rw1_cov <- function(n) {
  # Precision matrix for RW1
  Q <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    Q[i, i]     <- Q[i, i] + 1
    Q[i, i+1]   <- -1
    Q[i+1, i]   <- -1
    Q[i+1, i+1] <- Q[i+1, i+1] + 1
  }
  # Convert to covariance (generalized inverse of Q)
  # For brms gr() we need the covariance matrix
  # Use eigendecomposition, dropping the zero eigenvalue (sum-to-zero constraint)
  eig <- eigen(Q, symmetric = TRUE)
  # The last eigenvalue is ~0 (the constant vector)
  k <- n - 1
  Sigma <- eig$vectors[, 1:k] %*% diag(1/eig$values[1:k]) %*% t(eig$vectors[, 1:k])
  # Scale to have unit diagonal on average
  Sigma <- Sigma / mean(diag(Sigma))
  # Ensure positive definiteness (add small nugget)
  Sigma <- Sigma + diag(1e-6, n)
  # Force exact symmetry
  Sigma <- (Sigma + t(Sigma)) / 2
  Sigma
}

cov_age5  <- make_rw1_cov(5)
cov_educ5 <- make_rw1_cov(5)

rownames(cov_age5)  <- colnames(cov_age5)  <- c("18-29", "30-44", "45-59", "60-74", "75+")
rownames(cov_educ5) <- colnames(cov_educ5) <- c("no_degree", "hauptschule", "realschule",
                                                  "abitur", "university")

# ---- 4. Model formula builder -----------------------------------------------

build_deep_formula <- function(has_kreis = TRUE, has_wkr = TRUE,
                               use_rw1 = TRUE, use_bym2 = FALSE) {
  # Build formula components
  terms <- c("1", "male")

  # Demographic random effects
  if (use_rw1) {
    terms <- c(terms,
               "(1 | gr(age_cat, cov = cov_age5))",
               "(1 | gr(educ_label, cov = cov_educ5))")
  } else {
    terms <- c(terms, "(1 | age_cat)", "(1 | educ_label)")
  }

  # Deep interactions (Ghitza & Gelman 2013)
  terms <- c(terms,
             "(1 | male:age_cat)",
             "(1 | educ_label:age_cat)",
             "(1 | male:educ_label)")

  # Survey source RE (absorbs house/mode effects)
  terms <- c(terms, "(1 | survey_source)")

  # Temporal: legislative period RE
  terms <- c(terms, "(1 | legperiod)")

  # Geographic hierarchy
  terms <- c(terms, "(1 | state_code)")

  if (has_kreis) {
    if (use_bym2) {
      # BYM2: Besag-York-Mollié type 2 spatial random effect
      # Decomposes into structured (spatial) + unstructured (IID) components
      terms <- c(terms, "car(W_kreis, gr = county_code, type = \"bym2\")")
    } else {
      terms <- c(terms, "(1 | county_code)")
    }
  }

  if (has_wkr) {
    # Cross-classified: WKR does not nest in Kreis
    terms <- c(terms, "(1 | wkr_nr)")
  }

  # Area-level covariates (strongest geographic predictors per Warshaw & Rodden 2012)
  # Note: east dropped — redundant with AfD share (r > 0.8) and causes reification
  terms <- c(terms,
             "fed_afd_share_z", "fed_cdu_share_z",
             "fed_turnout_z", "log_pop_density_z",
             "unemployment_z")

  formula_str <- paste("y ~", paste(terms, collapse = " + "))

  bf(as.formula(formula_str), family = bernoulli(link = "logit"))
}

# ---- 5. Priors ---------------------------------------------------------------

# Following Gao et al. (2021) and Goplerud (2024)
build_priors <- function(use_bym2 = FALSE) {
  p <- c(
    # Fixed effects: weakly informative
    prior(normal(0, 2), class = "b"),
    prior(normal(0, 2), class = "Intercept"),
    # Random effect SDs: half-normal (regularizing but not too tight)
    prior(normal(0, 1), class = "sd")
  )
  if (use_bym2) {
    # BYM2 spatial SD prior
    p <- c(p, prior(normal(0, 1), class = "sdcar"))
  }
  p
}

# ---- 6. Fit function --------------------------------------------------------

fit_mrp_issue <- function(issue, data,
                          chains = 4, iter = 2000, warmup = 1000,
                          cores = 4, threads_per = 2,
                          use_rw1 = TRUE) {
  message("===== Fitting deep MRP for issue: ", issue, " =====")

  issue_data <- data |>
    filter(issue_id == !!issue) |>
    drop_na(y, age_cat, male, educ_label, state_code)

  if (nrow(issue_data) < 200) {
    warning("Fewer than 200 observations for ", issue, " — skipping.")
    return(NULL)
  }

  # Determine available geographic levels
  has_kreis <- sum(!is.na(issue_data$county_code)) > 50
  has_wkr   <- sum(!is.na(issue_data$wkr_nr)) > 50

  # Ensure factors
  issue_data <- issue_data |>
    mutate(
      age_cat    = factor(age_cat, levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
      educ_label = factor(educ_label, levels = c("no_degree", "hauptschule", "realschule",
                                                  "abitur", "university")),
      male       = as.integer(male),
      state_code = factor(state_code),
      survey_source = factor(survey_source),
      legperiod  = factor(legperiod)
    )

  if (has_kreis) issue_data$county_code <- factor(issue_data$county_code)
  if (has_wkr)   issue_data$wkr_nr      <- factor(issue_data$wkr_nr)

  # BYM2 requires that all county_codes in the data match adjacency matrix

  use_bym2_issue <- use_bym2 && has_kreis && !is.null(W_kreis)
  if (use_bym2_issue) {
    # Filter to counties present in adjacency matrix
    issue_data <- issue_data |>
      filter(county_code %in% rownames(W_kreis))
    if (n_distinct(issue_data$county_code) < 50) {
      message("  Too few counties in adjacency matrix — falling back to IID")
      use_bym2_issue <- FALSE
    }
  }

  # Build formula
  formula <- build_deep_formula(has_kreis = has_kreis, has_wkr = has_wkr,
                                use_rw1 = use_rw1, use_bym2 = use_bym2_issue)

  message("  N = ", nrow(issue_data),
          " | States = ", n_distinct(issue_data$state_code),
          if (has_kreis) paste0(" | Kreise = ", n_distinct(issue_data$county_code)),
          if (has_wkr) paste0(" | WKR = ", n_distinct(issue_data$wkr_nr)),
          " | Sources = ", n_distinct(issue_data$survey_source),
          " | Periods = ", n_distinct(issue_data$legperiod))

  # Build priors (adds sdcar prior if BYM2)
  mrp_priors <- build_priors(use_bym2 = use_bym2_issue)

  message("  BYM2: ", use_bym2_issue)

  # Build data2 list for brms (RW1 covariance matrices + BYM2 adjacency)
  data2_list <- list()
  if (use_rw1) {
    data2_list$cov_age5 <- cov_age5
    data2_list$cov_educ5 <- cov_educ5
  }
  if (use_bym2_issue) {
    # Subset adjacency matrix to counties in data
    counties_in_data <- sort(unique(as.character(issue_data$county_code)))
    W_sub <- W_kreis[counties_in_data, counties_in_data, drop = FALSE]
    data2_list$W_kreis <- W_sub
  }
  if (length(data2_list) == 0) data2_list <- NULL

  fit <- brm(
    formula   = formula,
    data      = issue_data,
    data2     = data2_list,
    prior     = mrp_priors,
    chains    = chains,
    iter      = iter,
    warmup    = warmup,
    cores     = cores,
    backend   = "cmdstanr",
    threads   = threading(threads_per),
    seed      = 42,
    control   = list(adapt_delta = 0.95, max_treedepth = 12),
    file      = file.path(mb_root, "data", "estimates", "models",
                          paste0("fit_", issue)),
    file_refit = "on_change"
  )

  # Diagnostics
  message("  Rhat max: ", round(max(rhat(fit), na.rm = TRUE), 4))
  message("  Bulk ESS min: ",
          round(min(neff_ratio(fit) * (iter - warmup) * chains, na.rm = TRUE)))

  fit
}

# ---- 7. Frequentist prototype (lme4) ----------------------------------------

fit_mrp_issue_freq <- function(issue, data) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Install lme4: install.packages('lme4')")
  }

  message("Fitting frequentist MRP (lme4) for issue: ", issue)

  issue_data <- data |>
    filter(issue_id == !!issue) |>
    drop_na(y, age_cat, male, educ_label, state_code) |>
    mutate(
      age_cat    = factor(age_cat),
      educ_label = factor(educ_label),
      state_code = factor(state_code),
      survey_source = factor(survey_source)
    )

  if (nrow(issue_data) < 200) {
    warning("Fewer than 200 observations for ", issue, " — skipping.")
    return(NULL)
  }

  has_kreis <- sum(!is.na(issue_data$county_code)) > 50
  has_wkr   <- sum(!is.na(issue_data$wkr_nr)) > 50

  # Build lme4 formula (no RW1 priors, but deep interactions + cross-classification)
  fe <- "y ~ male + fed_afd_share_z + fed_cdu_share_z + fed_turnout_z + log_pop_density_z + unemployment_z"
  re <- "(1 | age_cat) + (1 | educ_label) + (1 | male:age_cat) + (1 | educ_label:age_cat) + (1 | male:educ_label) + (1 | survey_source) + (1 | legperiod) + (1 | state_code)"

  if (has_kreis) {
    re <- paste0(re, " + (1 | county_code)")
    issue_data$county_code <- factor(issue_data$county_code)
  }
  if (has_wkr) {
    re <- paste0(re, " + (1 | wkr_nr)")
    issue_data$wkr_nr <- factor(issue_data$wkr_nr)
  }

  formula_str <- paste(fe, "+", re)

  message("  N = ", nrow(issue_data))

  fit <- lme4::glmer(
    as.formula(formula_str),
    data = issue_data,
    family = binomial(link = "logit"),
    control = lme4::glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )

  fit
}

# ---- 8. Run all issues -------------------------------------------------------

# Discover available issues
issue_ids <- survey |> distinct(issue_id) |> pull()

message("\n", length(issue_ids), " issues to estimate: ",
        paste(issue_ids, collapse = ", "))

# ---- 9. Run 5 pilot issues with brms ----------------------------------------

pilot_issues <- c(
  "immigration_restrict",   # immigration: strong east/west gradient
  "climate_ego",            # climate: urban/rural gradient
  "same_sex_marriage",      # social: education gradient
  "buergergeld_cut",        # welfare: economic gradient
  "defense_spending"        # defense: salient, broad coverage
)

# Filter to issues actually in data
pilot_issues <- intersect(pilot_issues, issue_ids)
message("\n=== Running brms pilot for ", length(pilot_issues), " issues ===")
message("Issues: ", paste(pilot_issues, collapse = ", "))
message("BYM2: ", use_bym2, "\n")

pilot_fits <- list()
for (issue in pilot_issues) {
  message("\n[", which(pilot_issues == issue), "/", length(pilot_issues), "] ", issue)
  pilot_fits[[issue]] <- tryCatch(
    fit_mrp_issue(issue, survey, use_rw1 = TRUE),
    error = function(e) {
      message("  ERROR: ", e$message)
      NULL
    }
  )
}

successful <- names(compact(pilot_fits))
message("\n=== ", length(successful), "/", length(pilot_issues),
        " pilot issues fitted successfully ===")

# Print convergence diagnostics
for (issue in successful) {
  fit <- pilot_fits[[issue]]
  rhat_max <- max(rhat(fit), na.rm = TRUE)
  message(issue, ": Rhat_max=", round(rhat_max, 4))
}

# To run ALL issues with brms (long!):
# fits <- map(issue_ids, ~ fit_mrp_issue(.x, survey), .progress = TRUE)
# names(fits) <- issue_ids
# conditional_effects(test_fit)
