## 08_check_pipeline.R
## Comprehensive validation pipeline for the Meinungsbild MRP analysis.
##
## Checks every stage: harmonization → covariates → poststrat frame →
## model fitting → poststratification → export → website data.
##
## Usage: source("08_check_pipeline.R")
##        Results printed to console and saved to output/checks/

library(tidyverse)
library(jsonlite)
library(lme4)

mb_root    <- file.path(here::here(), "meinungsbild")
gerda_root <- here::here()

dir.create(file.path(mb_root, "output", "checks"), showWarnings = FALSE, recursive = TRUE)

# ---- Helpers ---------------------------------------------------------------

pass  <- function(msg) message("  \u2705 PASS: ", msg)
fail  <- function(msg) message("  \u274c FAIL: ", msg)
warn  <- function(msg) message("  \u26a0\ufe0f  WARN: ", msg)
info  <- function(msg) message("  \u2139\ufe0f  INFO: ", msg)

check <- function(cond, pass_msg, fail_msg) {
 if (isTRUE(cond)) pass(pass_msg) else fail(fail_msg)
  invisible(cond)
}

n_pass <- 0L
n_fail <- 0L
n_warn <- 0L

check_t <- function(cond, pass_msg, fail_msg) {
  if (isTRUE(cond)) {
    pass(pass_msg)
    n_pass <<- n_pass + 1L
  } else {
    fail(fail_msg)
    n_fail <<- n_fail + 1L
  }
  invisible(cond)
}

warn_t <- function(cond, ok_msg, warn_msg) {
  if (isTRUE(cond)) {
    pass(ok_msg)
    n_pass <<- n_pass + 1L
  } else {
    warn(warn_msg)
    n_warn <<- n_warn + 1L
  }
  invisible(cond)
}

# ============================================================================
# STAGE 0: FILE EXISTENCE
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 0: FILE EXISTENCE")
message(strrep("=", 70))

required_files <- list(
  concordance    = file.path(mb_root, "data", "issue_concordance.csv"),
  survey_pooled  = file.path(mb_root, "data", "harmonized", "survey_pooled.rds"),
  survey_wide    = file.path(mb_root, "data", "harmonized", "survey_pooled_wide.rds"),
  kreis_cov      = file.path(mb_root, "data", "covariates", "kreis_covariates.rds"),
  bl_cov         = file.path(mb_root, "data", "covariates", "bundesland_covariates.rds"),
  poststrat_kr   = file.path(mb_root, "data", "poststrat", "poststrat_kreis.rds"),
  poststrat_bl   = file.path(mb_root, "data", "poststrat", "poststrat_bundesland.rds"),
  estimates_kr   = file.path(mb_root, "data", "estimates", "estimates_kreis.rds"),
  estimates_bl   = file.path(mb_root, "data", "estimates", "estimates_bundesland.rds"),
  estimates_wkr  = file.path(mb_root, "data", "estimates", "estimates_wkr.rds")
)

for (nm in names(required_files)) {
  check_t(file.exists(required_files[[nm]]),
          paste0(nm, " exists"),
          paste0(nm, " MISSING: ", required_files[[nm]]))
}

# Stop early if critical files missing
critical <- c("survey_pooled", "kreis_cov", "poststrat_kr",
              "estimates_kr", "estimates_bl", "estimates_wkr")
missing_critical <- critical[!sapply(required_files[critical], file.exists)]
if (length(missing_critical) > 0) {
  stop("Critical files missing: ", paste(missing_critical, collapse = ", "),
       "\nRun the pipeline first before checking.")
}

# ============================================================================
# STAGE 1: HARMONIZED SURVEY DATA
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 1: HARMONIZED SURVEY DATA (01_harmonize_all.R)")
message(strrep("=", 70))

survey <- readRDS(required_files$survey_pooled)
conc   <- read_csv(required_files$concordance, show_col_types = FALSE)

# --- 1.1 Basic structure ---
message("\n--- 1.1 Structure checks ---")

expected_cols <- c("respondent_id", "survey_source", "year", "state_code",
                   "male", "age_cat", "educ_label", "weight",
                   "issue_id", "y")
for (col in expected_cols) {
  check_t(col %in% names(survey),
          paste0("Column '", col, "' present"),
          paste0("Column '", col, "' MISSING from survey_pooled"))
}

check_t(nrow(survey) > 100000,
        paste0("Survey has ", format(nrow(survey), big.mark = ","), " rows (> 100k)"),
        paste0("Survey only has ", nrow(survey), " rows — too few"))

# --- 1.2 Respondent counts by source ---
message("\n--- 1.2 Survey sources ---")

source_counts <- survey |>
  distinct(respondent_id, survey_source) |>
  count(survey_source)

info(paste0("Survey sources: ", paste(source_counts$survey_source, collapse = ", ")))
info(paste0("Total respondents: ", format(sum(source_counts$n), big.mark = ",")))

expected_sources <- c("gles_tracking", "gles_cross2025", "gles_rcs2025",
                      "gles_cross_cum", "allbus")
for (src in expected_sources) {
  warn_t(src %in% source_counts$survey_source,
         paste0("Source '", src, "' present (N=",
                source_counts$n[source_counts$survey_source == src], ")"),
         paste0("Source '", src, "' missing — may reduce estimate quality"))
}

check_t(sum(source_counts$n) >= 50000,
        paste0("Total respondents = ", sum(source_counts$n), " (>= 50k target)"),
        paste0("Only ", sum(source_counts$n), " respondents — below 50k target"))

# --- 1.3 Demographics ---
message("\n--- 1.3 Demographic variables ---")

# Age categories
age_levels <- c("18-29", "30-44", "45-59", "60-74", "75+")
observed_ages <- sort(unique(as.character(survey$age_cat)))
check_t(all(age_levels %in% observed_ages),
        "All 5 age categories present",
        paste0("Missing age categories: ",
               paste(setdiff(age_levels, observed_ages), collapse = ", ")))

# Education levels
educ_levels <- c("no_degree", "hauptschule", "realschule", "abitur", "university")
observed_educ <- sort(unique(as.character(survey$educ_label)))
check_t(all(educ_levels %in% observed_educ),
        "All 5 education levels present",
        paste0("Missing education levels: ",
               paste(setdiff(educ_levels, observed_educ), collapse = ", ")))

# Gender (binary)
check_t(all(sort(unique(survey$male)) == c(0L, 1L)),
        "Gender coded as 0/1",
        paste0("Unexpected gender values: ", paste(unique(survey$male), collapse = ", ")))

# Age distribution: check no single category dominates excessively
age_dist <- survey |>
  distinct(respondent_id, age_cat) |>
  count(age_cat) |>
  mutate(pct = n / sum(n))
max_age_pct <- max(age_dist$pct)
check_t(max_age_pct < 0.40,
        paste0("Age distribution balanced (max category = ",
               round(100 * max_age_pct, 1), "%)"),
        paste0("Age distribution skewed — largest category = ",
               round(100 * max_age_pct, 1), "%"))

# Education distribution
educ_dist <- survey |>
  distinct(respondent_id, educ_label) |>
  count(educ_label) |>
  mutate(pct = n / sum(n))
min_educ_pct <- min(educ_dist$pct)
warn_t(min_educ_pct > 0.01,
       paste0("All education levels have > 1% (min = ",
              round(100 * min_educ_pct, 1), "%)"),
       paste0("Sparse education level — smallest = ",
              round(100 * min_educ_pct, 1), "% — model may struggle"))

# --- 1.4 Geographic coverage ---
message("\n--- 1.4 Geographic coverage ---")

n_states <- n_distinct(survey$state_code, na.rm = TRUE)
check_t(n_states == 16,
        paste0("All 16 Bundesländer represented"),
        paste0("Only ", n_states, " / 16 Bundesländer"))

n_kreise <- n_distinct(survey$county_code, na.rm = TRUE)
info(paste0("Kreise with respondents: ", n_kreise, " / ~400"))
warn_t(n_kreise >= 200,
       paste0(n_kreise, " Kreise have respondents (>= 200)"),
       paste0("Only ", n_kreise, " Kreise — many counties rely on model-only predictions"))

n_wkr <- n_distinct(survey$wkr_nr, na.rm = TRUE)
info(paste0("Wahlkreise with respondents: ", n_wkr, " / 299"))

# --- 1.5 Response variable ---
message("\n--- 1.5 Binary response variable ---")

# Exclude continuous issues (e.g., lr_self) for binary check
continuous_issues <- conc |>
  filter(binary_rule == "continuous") |>
  pull(issue_id) |>
  unique()
survey_binary <- survey |> filter(!issue_id %in% continuous_issues)
check_t(all(survey_binary$y %in% c(0L, 1L, NA)),
        "Response y is binary (0/1/NA) for all non-continuous issues",
        paste0("Non-binary y values found in binary issues: ",
               paste(head(setdiff(unique(survey_binary$y), c(0, 1, NA)), 5), collapse = ", ")))

na_rate <- mean(is.na(survey$y))
check_t(na_rate < 0.5,
        paste0("NA rate in y = ", round(100 * na_rate, 1), "% (< 50%)"),
        paste0("High NA rate: ", round(100 * na_rate, 1), "%"))

# --- 1.6 Issue coverage ---
message("\n--- 1.6 Issue coverage ---")

n_issues <- n_distinct(survey$issue_id)
info(paste0("Issues in survey: ", n_issues))

issue_summary <- survey |>
  filter(!is.na(y)) |>
  group_by(issue_id) |>
  summarise(
    n_obs     = n(),
    pct_y1    = mean(y),
    n_states  = n_distinct(state_code, na.rm = TRUE),
    n_kreise  = n_distinct(county_code, na.rm = TRUE),
    n_sources = n_distinct(survey_source),
    .groups = "drop"
  ) |>
  arrange(n_obs)

# Check no issues have degenerate y (all 0 or all 1)
degen <- issue_summary |> filter(pct_y1 == 0 | pct_y1 == 1)
check_t(nrow(degen) == 0,
        "No degenerate issues (all 0 or all 1)",
        paste0(nrow(degen), " degenerate issues: ",
               paste(degen$issue_id, collapse = ", ")))

# Check issues with very low N
low_n <- issue_summary |> filter(n_obs < 300)
warn_t(nrow(low_n) == 0,
       "All issues have >= 300 valid observations",
       paste0(nrow(low_n), " issues have < 300 obs: ",
              paste(low_n$issue_id, collapse = ", ")))

# --- 1.7 Concordance consistency ---
message("\n--- 1.7 Concordance ↔ Survey consistency ---")

conc_issues <- unique(conc$issue_id)
survey_issues <- unique(survey$issue_id)

in_conc_not_survey <- setdiff(conc_issues, survey_issues)
in_survey_not_conc <- setdiff(survey_issues, conc_issues)

warn_t(length(in_conc_not_survey) == 0,
       "All concordance issues found in survey",
       paste0(length(in_conc_not_survey), " concordance issues missing from survey: ",
              paste(head(in_conc_not_survey, 5), collapse = ", ")))

warn_t(length(in_survey_not_conc) == 0,
       "All survey issues found in concordance",
       paste0(length(in_survey_not_conc), " survey issues not in concordance: ",
              paste(head(in_survey_not_conc, 5), collapse = ", ")))

# --- 1.8 Year range ---
message("\n--- 1.8 Year range ---")

year_range <- range(survey$year, na.rm = TRUE)
info(paste0("Year range: ", year_range[1], " – ", year_range[2]))
check_t(year_range[1] <= 2013,
        "Earliest year <= 2013 (good temporal depth)",
        "Data starts after 2013 — limited temporal depth")
check_t(year_range[2] >= 2024,
        paste0("Latest year = ", year_range[2], " (recent data)"),
        paste0("Latest year = ", year_range[2], " — may be outdated"))


# ============================================================================
# STAGE 2: COVARIATES
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 2: COVARIATES (03_load_covariates.R)")
message(strrep("=", 70))

kreis_cov <- readRDS(required_files$kreis_cov)
bl_cov    <- readRDS(required_files$bl_cov)

# --- 2.1 Structure ---
message("\n--- 2.1 Structure ---")

cov_expected <- c("county_code", "county_name", "state_code",
                  "cty_population", "cty_pop_density",
                  "fed_afd", "fed_cdu_csu", "fed_turnout", "east")
for (col in cov_expected) {
  check_t(col %in% names(kreis_cov),
          paste0("Covariate '", col, "' present"),
          paste0("Covariate '", col, "' MISSING"))
}

n_kreise_cov <- nrow(kreis_cov)
check_t(n_kreise_cov >= 395 & n_kreise_cov <= 410,
        paste0(n_kreise_cov, " Kreise in covariates (expected ~400)"),
        paste0(n_kreise_cov, " Kreise — unexpected count"))

check_t(nrow(bl_cov) == 16,
        "16 Bundesländer in covariates",
        paste0(nrow(bl_cov), " Bundesländer — expected 16"))

# --- 2.2 Value ranges ---
message("\n--- 2.2 Value ranges ---")

# Party vote shares should be in [0, 1] or [0, 100] — check which
afd_max <- max(kreis_cov$fed_afd, na.rm = TRUE)
if (afd_max > 1) {
  info("Party shares appear to be in percentage format (0–100)")
  share_upper <- 100
} else {
  info("Party shares appear to be in proportion format (0–1)")
  share_upper <- 1
}

check_t(all(kreis_cov$fed_afd >= 0 & kreis_cov$fed_afd <= share_upper, na.rm = TRUE),
        paste0("AfD share in valid range [0, ", share_upper, "]"),
        "AfD share has out-of-range values")

check_t(all(kreis_cov$fed_cdu_csu >= 0 & kreis_cov$fed_cdu_csu <= share_upper, na.rm = TRUE),
        paste0("CDU/CSU share in valid range [0, ", share_upper, "]"),
        "CDU/CSU share has out-of-range values")

check_t(all(kreis_cov$cty_pop_density > 0, na.rm = TRUE),
        "Population density all positive",
        "Some Kreise have zero or negative population density")

# --- 2.3 Missing values ---
message("\n--- 2.3 Missing values ---")

for (col in c("fed_afd", "fed_cdu_csu", "fed_turnout", "cty_pop_density")) {
  n_na <- sum(is.na(kreis_cov[[col]]))
  warn_t(n_na == 0,
         paste0("No NAs in ", col),
         paste0(n_na, " NAs in ", col))
}

# --- 2.4 East/West coding ---
message("\n--- 2.4 East/West coding ---")

east_states <- c("12", "13", "14", "15", "16")
east_actual <- kreis_cov |>
  filter(east == TRUE | east == 1) |>
  pull(state_code) |>
  unique() |>
  sort()

check_t(all(east_actual %in% east_states) && all(east_states %in% east_actual),
        "East/West coding correct (12-16 = East)",
        paste0("East/West mismatch — coded as East: ", paste(east_actual, collapse = ", ")))

# --- 2.5 County codes format ---
message("\n--- 2.5 County code format ---")

check_t(all(nchar(kreis_cov$county_code) == 5),
        "All county codes are 5 digits",
        "Some county codes are not 5 digits")

check_t(!any(duplicated(kreis_cov$county_code)),
        "No duplicate county codes",
        paste0(sum(duplicated(kreis_cov$county_code)), " duplicate county codes"))


# ============================================================================
# STAGE 3: POSTSTRATIFICATION FRAME
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 3: POSTSTRATIFICATION FRAME (02b_download_zensus.R)")
message(strrep("=", 70))

poststrat <- readRDS(required_files$poststrat_kr)
poststrat_bl <- readRDS(required_files$poststrat_bl)

# --- 3.1 Structure ---
message("\n--- 3.1 Structure ---")

ps_expected <- c("county_code", "age_cat", "male", "educ_label", "N")
for (col in ps_expected) {
  check_t(col %in% names(poststrat),
          paste0("Poststrat column '", col, "' present"),
          paste0("Poststrat column '", col, "' MISSING"))
}

# --- 3.2 Cell dimensions ---
message("\n--- 3.2 Cell dimensions ---")

n_kreise_ps <- n_distinct(poststrat$county_code)
check_t(n_kreise_ps >= 395 & n_kreise_ps <= 410,
        paste0(n_kreise_ps, " Kreise in poststrat frame (~400 expected)"),
        paste0(n_kreise_ps, " Kreise — unexpected"))

cells_per_kreis <- poststrat |>
  count(county_code) |>
  pull(n)
check_t(all(cells_per_kreis == 50),
        "All Kreise have exactly 50 cells (5 age × 2 gender × 5 educ)",
        paste0("Cell counts per Kreis vary: ",
               min(cells_per_kreis), "–", max(cells_per_kreis)))

expected_nrow <- n_kreise_ps * 50
check_t(nrow(poststrat) == expected_nrow,
        paste0("Poststrat has ", nrow(poststrat), " rows (", n_kreise_ps, " × 50)"),
        paste0("Expected ", expected_nrow, " rows, got ", nrow(poststrat)))

# --- 3.3 Population totals ---
message("\n--- 3.3 Population totals ---")

total_pop <- sum(poststrat$N)
info(paste0("Total adult population: ", format(total_pop, big.mark = ",")))

# Zensus 2022: ~70.3 million adults (18+)
check_t(total_pop > 50e6 & total_pop < 90e6,
        paste0("Total population plausible (",
               round(total_pop / 1e6, 1), "M, expected ~70M)"),
        paste0("Total population implausible: ",
               format(total_pop, big.mark = ",")))

# No zero-population Kreise
kreis_pop <- poststrat |>
  group_by(county_code) |>
  summarise(pop = sum(N))
check_t(all(kreis_pop$pop > 0),
        "No zero-population Kreise",
        paste0(sum(kreis_pop$pop == 0), " Kreise with zero population"))

# No negative N values
check_t(all(poststrat$N >= 0),
        "No negative cell counts",
        "Negative cell counts found")

# --- 3.4 Demographic distributions ---
message("\n--- 3.4 Demographic distributions ---")

age_ps <- poststrat |>
  group_by(age_cat) |>
  summarise(N = sum(N)) |>
  mutate(pct = N / sum(N))

for (i in seq_len(nrow(age_ps))) {
  info(paste0("Age ", age_ps$age_cat[i], ": ",
              round(100 * age_ps$pct[i], 1), "%"))
}

# Check no age group < 5% or > 40%
check_t(all(age_ps$pct > 0.05) & all(age_ps$pct < 0.40),
        "Age distribution within plausible range (5-40% each)",
        "Age distribution has implausible concentrations")

gender_ps <- poststrat |>
  group_by(male) |>
  summarise(N = sum(N)) |>
  mutate(pct = N / sum(N))

male_pct <- gender_ps$pct[gender_ps$male == 1]
check_t(male_pct > 0.45 & male_pct < 0.55,
        paste0("Male proportion = ", round(100 * male_pct, 1), "% (expected ~49%)"),
        paste0("Male proportion = ", round(100 * male_pct, 1), "% — implausible"))

educ_ps <- poststrat |>
  group_by(educ_label) |>
  summarise(N = sum(N)) |>
  mutate(pct = N / sum(N))

for (i in seq_len(nrow(educ_ps))) {
  info(paste0("Educ ", educ_ps$educ_label[i], ": ",
              round(100 * educ_ps$pct[i], 1), "%"))
}

# University should be ~20-30%, no_degree should be small
uni_pct <- educ_ps$pct[educ_ps$educ_label == "university"]
warn_t(uni_pct > 0.10 & uni_pct < 0.40,
       paste0("University share = ", round(100 * uni_pct, 1), "% (plausible)"),
       paste0("University share = ", round(100 * uni_pct, 1), "% — check Abitur→university split"))

# --- 3.5 Bundesland aggregation consistency ---
message("\n--- 3.5 Bundesland aggregation ---")

bl_from_kreis <- poststrat |>
  mutate(state_code = str_sub(county_code, 1, 2)) |>
  group_by(state_code, age_cat, male, educ_label) |>
  summarise(N_agg = sum(N), .groups = "drop")

bl_check <- poststrat_bl |>
  left_join(bl_from_kreis,
            by = c("state_code", "age_cat", "male", "educ_label"))

if ("N_agg" %in% names(bl_check)) {
  max_diff <- max(abs(bl_check$N - bl_check$N_agg), na.rm = TRUE)
  check_t(max_diff <= 1,
          "Bundesland frame matches Kreis aggregation (max diff <= 1)",
          paste0("Bundesland ≠ aggregated Kreis — max diff = ", max_diff))
} else {
  warn("Could not check Bundesland consistency (column mismatch)")
}

# --- 3.6 Poststrat ↔ Covariates alignment ---
message("\n--- 3.6 Poststrat ↔ Covariates alignment ---")

ps_codes <- unique(poststrat$county_code)
cov_codes <- unique(kreis_cov$county_code)

in_ps_not_cov <- setdiff(ps_codes, cov_codes)
in_cov_not_ps <- setdiff(cov_codes, ps_codes)

check_t(length(in_ps_not_cov) == 0,
        "All poststrat Kreise have covariates",
        paste0(length(in_ps_not_cov), " poststrat Kreise missing from covariates"))

warn_t(length(in_cov_not_ps) <= 5,
       paste0(length(in_cov_not_ps), " covariate Kreise not in poststrat (acceptable)"),
       paste0(length(in_cov_not_ps), " covariate Kreise missing from poststrat — check merges"))


# ============================================================================
# STAGE 4: MODEL ESTIMATES
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 4: MODEL ESTIMATES (04b_fit_all_lme4.R)")
message(strrep("=", 70))

est_kr  <- readRDS(required_files$estimates_kr)
est_bl  <- readRDS(required_files$estimates_bl)
est_wkr <- readRDS(required_files$estimates_wkr)

# --- 4.1 Basic structure ---
message("\n--- 4.1 Structure ---")

check_t("issue_id" %in% names(est_kr) & "estimate" %in% names(est_kr),
        "Kreis estimates have issue_id and estimate columns",
        "Kreis estimates missing key columns")

check_t("issue_id" %in% names(est_bl) & "estimate" %in% names(est_bl),
        "Bundesland estimates have issue_id and estimate columns",
        "Bundesland estimates missing key columns")

check_t("issue_id" %in% names(est_wkr) & "estimate" %in% names(est_wkr),
        "WKR estimates have issue_id and estimate columns",
        "WKR estimates missing key columns")

# --- 4.2 Issue coverage ---
message("\n--- 4.2 Issue coverage ---")

est_issues_kr  <- sort(unique(est_kr$issue_id))
est_issues_bl  <- sort(unique(est_bl$issue_id))
est_issues_wkr <- sort(unique(est_wkr$issue_id))

n_est_issues <- length(est_issues_kr)
info(paste0("Issues estimated: ", n_est_issues))
check_t(n_est_issues >= 30,
        paste0(n_est_issues, " issues estimated (>= 30 target)"),
        paste0("Only ", n_est_issues, " issues estimated — below 30 target"))

# Same issues across all 3 geographies
check_t(identical(est_issues_kr, est_issues_bl),
        "Same issues in Kreis and Bundesland estimates",
        paste0("Issue mismatch: Kreis has ", length(est_issues_kr),
               ", Bundesland has ", length(est_issues_bl)))

check_t(identical(est_issues_kr, est_issues_wkr),
        "Same issues in Kreis and WKR estimates",
        paste0("Issue mismatch: Kreis has ", length(est_issues_kr),
               ", WKR has ", length(est_issues_wkr)))

# --- 4.3 Geographic coverage per issue ---
message("\n--- 4.3 Geographic coverage ---")

kreise_per_issue <- est_kr |>
  group_by(issue_id) |>
  summarise(n = n_distinct(county_code))

check_t(all(kreise_per_issue$n >= 395),
        paste0("All issues have >= 395 Kreise (min = ", min(kreise_per_issue$n), ")"),
        paste0("Some issues have < 395 Kreise (min = ", min(kreise_per_issue$n), ")"))

bl_per_issue <- est_bl |>
  group_by(issue_id) |>
  summarise(n = n_distinct(state_code))

check_t(all(bl_per_issue$n == 16),
        "All issues have all 16 Bundesländer",
        paste0("Some issues missing Bundesländer (min = ", min(bl_per_issue$n), ")"))

wkr_per_issue <- est_wkr |>
  group_by(issue_id) |>
  summarise(n = n_distinct(wkr_nr))

check_t(all(wkr_per_issue$n >= 295),
        paste0("All issues have >= 295 WKR (min = ", min(wkr_per_issue$n), ")"),
        paste0("Some issues have < 295 WKR (min = ", min(wkr_per_issue$n), ")"))

# --- 4.4 Estimate range (must be valid probabilities) ---
message("\n--- 4.4 Estimate ranges ---")

check_t(all(est_kr$estimate >= 0 & est_kr$estimate <= 1),
        "All Kreis estimates in [0, 1]",
        paste0("Kreis estimates out of range: min=",
               round(min(est_kr$estimate), 4), " max=",
               round(max(est_kr$estimate), 4)))

check_t(all(est_bl$estimate >= 0 & est_bl$estimate <= 1),
        "All Bundesland estimates in [0, 1]",
        paste0("Bundesland estimates out of range: min=",
               round(min(est_bl$estimate), 4), " max=",
               round(max(est_bl$estimate), 4)))

check_t(all(est_wkr$estimate >= 0 & est_wkr$estimate <= 1),
        "All WKR estimates in [0, 1]",
        paste0("WKR estimates out of range: min=",
               round(min(est_wkr$estimate), 4), " max=",
               round(max(est_wkr$estimate), 4)))

# No degenerate estimates (all exactly 0 or 1)
degen_issues <- est_kr |>
  group_by(issue_id) |>
  summarise(all_zero = all(estimate == 0), all_one = all(estimate == 1)) |>
  filter(all_zero | all_one)

check_t(nrow(degen_issues) == 0,
        "No issues have degenerate estimates (all 0 or all 1)",
        paste0("Degenerate issues: ", paste(degen_issues$issue_id, collapse = ", ")))

# --- 4.5 Within-issue variation ---
message("\n--- 4.5 Within-issue variation ---")

issue_variation <- est_kr |>
  group_by(issue_id) |>
  summarise(
    mean_est = mean(estimate),
    sd_est   = sd(estimate),
    min_est  = min(estimate),
    max_est  = max(estimate),
    range_pp = 100 * (max(estimate) - min(estimate)),
    .groups  = "drop"
  )

low_var <- issue_variation |> filter(sd_est < 0.01)
warn_t(nrow(low_var) == 0,
       "All issues have sufficient cross-county variation (SD > 1pp)",
       paste0(nrow(low_var), " issues with very low variation (SD < 1pp): ",
              paste(low_var$issue_id, collapse = ", "),
              " — may reflect genuinely uniform attitudes or insufficient geographic covariates"))

high_range <- issue_variation |> filter(range_pp > 60)
warn_t(nrow(high_range) == 0,
       "No issues with implausibly wide ranges (> 60pp)",
       paste0(nrow(high_range), " issues with > 60pp range: ",
              paste(high_range$issue_id, collapse = ", ")))

info("Issue variation summary:")
print(issue_variation |>
        mutate(across(c(mean_est, sd_est, min_est, max_est), ~ round(.x, 3)),
               range_pp = round(range_pp, 1)) |>
        arrange(desc(sd_est)),
      n = 50)

# --- 4.6 Cross-level consistency ---
message("\n--- 4.6 Cross-level consistency (Kreis → Bundesland aggregation) ---")

# Aggregate Kreis estimates to Bundesland level and compare
est_kr_with_state <- est_kr |>
  left_join(kreis_cov |> select(county_code, state_code), by = "county_code")

agg_bl <- est_kr_with_state |>
  group_by(issue_id, state_code) |>
  summarise(
    agg_estimate = weighted.mean(estimate, pop, na.rm = TRUE),
    .groups = "drop"
  )

cross_check <- est_bl |>
  left_join(agg_bl, by = c("issue_id", "state_code"))

if (nrow(cross_check) > 0 && all(!is.na(cross_check$agg_estimate))) {
  # These should be very close (not identical due to poststratification details)
  diff_pp <- 100 * abs(cross_check$estimate - cross_check$agg_estimate)
  max_diff <- max(diff_pp, na.rm = TRUE)
  median_diff <- median(diff_pp, na.rm = TRUE)
  info(paste0("Kreis→Bundesland aggregation vs. direct estimate: ",
              "median diff = ", round(median_diff, 2), "pp, ",
              "max diff = ", round(max_diff, 2), "pp"))
  warn_t(max_diff < 10,
         paste0("Cross-level consistency OK (max diff = ", round(max_diff, 2), "pp)"),
         paste0("Large cross-level discrepancy: max diff = ", round(max_diff, 2), "pp"))
} else {
  warn("Could not compute cross-level consistency (missing state_code)")
}


# ============================================================================
# STAGE 5: VALIDATION — MRP vs. DIRECT ESTIMATES
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 5: VALIDATION — MRP vs. DIRECT ESTIMATES")
message(strrep("=", 70))

# Direct Bundesland estimates from raw survey data
direct_bl <- survey |>
  filter(!is.na(y), !is.na(state_code)) |>
  group_by(issue_id, state_code) |>
  summarise(
    direct_est = mean(y, na.rm = TRUE),
    n_obs      = n(),
    .groups    = "drop"
  )

val <- est_bl |>
  inner_join(direct_bl, by = c("issue_id", "state_code")) |>
  filter(n_obs >= 30)  # only compare where we have decent N

if (nrow(val) > 0) {
  # --- 5.1 Correlation ---
  message("\n--- 5.1 Bundesland-level correlations ---")

  val_by_issue <- val |>
    group_by(issue_id) |>
    summarise(
      r      = cor(estimate, direct_est),
      rmse   = sqrt(mean((estimate - direct_est)^2)),
      bias   = mean(estimate - direct_est),
      n_bl   = n(),
      .groups = "drop"
    ) |>
    arrange(desc(r))

  info("Issue-level validation:")
  print(val_by_issue |>
          mutate(across(c(r, rmse, bias), ~ round(.x, 3))),
        n = 50)

  median_r    <- median(val_by_issue$r, na.rm = TRUE)
  median_rmse <- median(val_by_issue$rmse, na.rm = TRUE)

  info(paste0("Median correlation: ", round(median_r, 3)))
  info(paste0("Median RMSE: ", round(100 * median_rmse, 1), " pp"))

  check_t(median_r >= 0.7,
          paste0("Median Bundesland correlation = ", round(median_r, 3), " (>= 0.7)"),
          paste0("Median correlation = ", round(median_r, 3), " — below 0.7 threshold"))

  check_t(median_rmse <= 0.10,
          paste0("Median RMSE = ", round(100 * median_rmse, 1), "pp (<= 10pp)"),
          paste0("Median RMSE = ", round(100 * median_rmse, 1), "pp — exceeds 10pp"))

  # --- 5.2 Poorly-fitting issues ---
  message("\n--- 5.2 Poorly-fitting issues ---")

  poor_issues <- val_by_issue |> filter(r < 0.5)
  warn_t(nrow(poor_issues) == 0,
         "No issues with r < 0.5",
         paste0(nrow(poor_issues), " issues with r < 0.5: ",
                paste(poor_issues$issue_id, collapse = ", "),
                " — consider excluding from website or flagging as low confidence"))

  # --- 5.3 Systematic bias ---
  message("\n--- 5.3 Systematic bias ---")

  overall_bias <- mean(val$estimate - val$direct_est, na.rm = TRUE)
  info(paste0("Overall mean bias: ", round(100 * overall_bias, 2), " pp"))
  warn_t(abs(overall_bias) < 0.03,
         paste0("No systematic bias (|bias| = ", round(100 * abs(overall_bias), 2), "pp < 3pp)"),
         paste0("Systematic bias detected: ", round(100 * overall_bias, 2), "pp"))

  # --- 5.4 East/West pattern preservation ---
  message("\n--- 5.4 East/West pattern preservation ---")

  east_codes <- c("12", "13", "14", "15", "16")

  ew_check <- val |>
    mutate(region = ifelse(state_code %in% east_codes, "East", "West")) |>
    group_by(issue_id, region) |>
    summarise(
      mrp_mean    = mean(estimate),
      direct_mean = mean(direct_est),
      .groups     = "drop"
    ) |>
    pivot_wider(names_from = region,
                values_from = c(mrp_mean, direct_mean)) |>
    mutate(
      mrp_diff    = mrp_mean_East - mrp_mean_West,
      direct_diff = direct_mean_East - direct_mean_West,
      sign_match  = sign(mrp_diff) == sign(direct_diff)
    )

  if (nrow(ew_check) > 0 && "sign_match" %in% names(ew_check)) {
    sign_rate <- mean(ew_check$sign_match, na.rm = TRUE)
    check_t(sign_rate >= 0.80,
            paste0("E/W direction match = ", round(100 * sign_rate), "% (>= 80%)"),
            paste0("E/W direction match = ", round(100 * sign_rate), "% — model misses regional pattern"))
  }

  # --- 5.5 Save validation table ---
  write_csv(val_by_issue,
            file.path(mb_root, "output", "checks", "validation_bundesland.csv"))
  info("Saved validation table to output/checks/validation_bundesland.csv")

} else {
  warn("No overlapping Bundesland estimates for validation")
}


# ============================================================================
# STAGE 6: FACE VALIDITY — KNOWN CORRELATIONS
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 6: FACE VALIDITY — KNOWN CORRELATIONS")
message(strrep("=", 70))

# Check that MRP estimates correlate with Kreis-level covariates in expected directions

est_kr_cov <- est_kr |>
  left_join(kreis_cov |> select(county_code, fed_afd, fed_cdu_csu, fed_gruene,
                                 cty_pop_density, east),
            by = "county_code") |>
  filter(!is.na(fed_afd))

face_checks <- tibble(
  issue_id     = character(),
  covariate    = character(),
  expected_dir = character(),
  actual_r     = numeric(),
  correct      = logical()
)

run_face_check <- function(issue, covar, expected_sign, label) {
  d <- est_kr_cov |> filter(issue_id == !!issue)
  if (nrow(d) < 50) return(invisible())
  r <- cor(d$estimate, d[[covar]], use = "complete.obs")
  actual_sign <- ifelse(r > 0, "+", "-")
  ok <- actual_sign == expected_sign
  status <- if (ok) pass else warn
  status(paste0(label, ": r = ", round(r, 3),
                " (expected ", expected_sign, ", got ", actual_sign, ")"))
  face_checks <<- bind_rows(face_checks,
                             tibble(issue_id = issue, covariate = covar,
                                    expected_dir = expected_sign,
                                    actual_r = round(r, 3), correct = ok))
}

# Immigration restrict ↔ AfD share should be positive
if ("immigration_restrict" %in% est_issues_kr) {
  run_face_check("immigration_restrict", "fed_afd", "+",
                 "Immigration restriction ↔ AfD share")
}

# Climate priority ↔ Grüne share should be positive
if ("climate_ego" %in% est_issues_kr & "fed_gruene" %in% names(est_kr_cov)) {
  run_face_check("climate_ego", "fed_gruene", "+",
                 "Climate priority ↔ Grüne share")
}

# Fear immigration ↔ AfD share: y=1 if low fear (<=2 on 1-7),
# so high AfD areas should have LESS "no fear" → negative correlation
if ("fear_immigration" %in% est_issues_kr) {
  run_face_check("fear_immigration", "fed_afd", "-",
                 "Low fear of immigration ↔ AfD share")
}

# Political interest ↔ population density (urbanites more interested)
if ("political_interest" %in% est_issues_kr) {
  run_face_check("political_interest", "cty_pop_density", "+",
                 "Political interest ↔ population density")
}

# Same-sex marriage ↔ Grüne share: no clear expected direction
# Support is high everywhere; Grüne share correlates with urbanity/west but
# same-sex marriage support is also high in urban East. Skip this check.
# if ("same_sex_marriage" %in% est_issues_kr & "fed_gruene" %in% names(est_kr_cov)) {
#   run_face_check("same_sex_marriage", "fed_gruene", "+",
#                  "Same-sex marriage ↔ Grüne share")
# }

# Democracy satisfaction ↔ East should be negative
if ("democracy_satisfaction" %in% est_issues_kr) {
  d <- est_kr_cov |> filter(issue_id == "democracy_satisfaction")
  if (nrow(d) > 50) {
    east_mean <- mean(d$estimate[d$east == TRUE | d$east == 1])
    west_mean <- mean(d$estimate[d$east == FALSE | d$east == 0])
    ok <- east_mean < west_mean
    if (ok) pass(paste0("Democracy satisfaction: East (", round(100*east_mean,1),
                        "%) < West (", round(100*west_mean,1), "%) — correct"))
    else warn(paste0("Democracy satisfaction: East (", round(100*east_mean,1),
                     "%) >= West (", round(100*west_mean,1), "%) — unexpected"))
  }
}

if (nrow(face_checks) > 0) {
  write_csv(face_checks,
            file.path(mb_root, "output", "checks", "face_validity.csv"))
  n_correct <- sum(face_checks$correct)
  info(paste0("Face validity: ", n_correct, "/", nrow(face_checks),
              " checks match expected direction"))
}


# ============================================================================
# STAGE 7: WEBSITE DATA EXPORT
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 7: WEBSITE DATA EXPORT (07_export_estimates.R)")
message(strrep("=", 70))

web_data <- file.path(mb_root, "web", "public", "data")

# --- 7.1 JSON files exist ---
message("\n--- 7.1 JSON file existence ---")

json_files <- c(
  "estimates_kreis.json",
  "estimates_bundesland.json",
  "estimates_wkr.json",
  "issues.json"
)

for (f in json_files) {
  fp <- file.path(web_data, f)
  check_t(file.exists(fp),
          paste0(f, " exists (", round(file.size(fp) / 1024), " KB)"),
          paste0(f, " MISSING"))
}

# --- 7.2 JSON is valid and parseable ---
message("\n--- 7.2 JSON validity ---")

for (f in json_files) {
  fp <- file.path(web_data, f)
  if (!file.exists(fp)) next
  parsed <- tryCatch(
    fromJSON(fp),
    error = function(e) NULL
  )
  check_t(!is.null(parsed),
          paste0(f, " is valid JSON"),
          paste0(f, " FAILED to parse as JSON"))
}

# --- 7.3 Issues metadata ---
message("\n--- 7.3 Issues metadata ---")

issues_json <- fromJSON(file.path(web_data, "issues.json"))

expected_meta_cols <- c("issue_id", "label", "label_de", "category",
                        "question_de", "question_en", "response_type",
                        "binary_rule", "direction")
for (col in expected_meta_cols) {
  check_t(col %in% names(issues_json),
          paste0("Issues JSON has '", col, "' field"),
          paste0("Issues JSON missing '", col, "' field"))
}

# Check question_de has actual question text (not codebook labels)
codebook_patterns <- c("Issuebatterie:", "Positionsissue:", "Psychologische Konzepte:",
                       "Einstellungen:", "Abgehaengtheit:")
has_codebook <- issues_json$question_de |>
  str_detect(paste(codebook_patterns, collapse = "|"))
check_t(!any(has_codebook),
        "question_de contains actual question text (not codebook labels)",
        paste0(sum(has_codebook), " issues still have codebook labels instead of question text"))

# Check question_en exists and is not empty
if ("question_en" %in% names(issues_json)) {
  empty_en <- is.na(issues_json$question_en) | issues_json$question_en == ""
  check_t(!any(empty_en),
          "All issues have English question text",
          paste0(sum(empty_en), " issues missing English question text"))
}

# --- 7.4 Estimates JSON structure ---
message("\n--- 7.4 Estimates JSON structure ---")

est_kr_json <- fromJSON(file.path(web_data, "estimates_kreis.json"))

n_issues_json <- length(est_kr_json)
info(paste0("Issues in Kreis JSON: ", n_issues_json))

# Compare with metadata
meta_issues <- sort(issues_json$issue_id)
json_issues <- sort(names(est_kr_json))

check_t(identical(meta_issues, json_issues),
        "Issue IDs match between metadata and estimates JSON",
        paste0("Mismatch: ", length(setdiff(meta_issues, json_issues)),
               " in metadata not in estimates, ",
               length(setdiff(json_issues, meta_issues)), " vice versa"))

# Check first issue's structure
if (length(est_kr_json) > 0) {
  first_issue <- est_kr_json[[1]]
  json_est_cols <- names(first_issue)
  check_t("county_code" %in% json_est_cols & "estimate" %in% json_est_cols,
          "Kreis JSON has county_code and estimate fields",
          paste0("Kreis JSON has: ", paste(json_est_cols, collapse = ", ")))
}

# --- 7.5 Estimates are rounded and in range ---
message("\n--- 7.5 Estimate values in JSON ---")

# Sample check: verify all estimates in [0, 1]
all_json_estimates <- unlist(lapply(est_kr_json, function(x) x$estimate))
check_t(all(all_json_estimates >= 0 & all_json_estimates <= 1),
        paste0("All JSON estimates in [0, 1] (n=",
               length(all_json_estimates), ")"),
        "Some JSON estimates out of [0, 1] range")

# Check rounding (should be 4 decimal places)
max_decimals <- max(nchar(sub("^-?\\d+\\.?", "", as.character(all_json_estimates))))
info(paste0("Max decimal places in JSON: ", max_decimals))


# ============================================================================
# STAGE 8: END-TO-END TRACEABILITY
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 8: END-TO-END TRACEABILITY")
message(strrep("=", 70))

# Pick a specific issue and trace through all stages
trace_issue <- if ("immigration_restrict" %in% est_issues_kr) {
  "immigration_restrict"
} else {
  est_issues_kr[1]
}

message(paste0("\n--- Tracing issue: ", trace_issue, " ---"))

# Survey
trace_survey <- survey |>
  filter(issue_id == trace_issue, !is.na(y))
info(paste0("Survey: N=", nrow(trace_survey),
            ", mean(y)=", round(mean(trace_survey$y), 3),
            ", sources=", n_distinct(trace_survey$survey_source),
            ", states=", n_distinct(trace_survey$state_code, na.rm = TRUE),
            ", kreise=", n_distinct(trace_survey$county_code, na.rm = TRUE)))

# Concordance
trace_conc <- conc |> filter(issue_id == trace_issue)
info(paste0("Concordance: ", nrow(trace_conc), " entries, ",
            "datasets: ", paste(unique(trace_conc$dataset), collapse = ", ")))

# Estimates
trace_kr <- est_kr |> filter(issue_id == trace_issue)
trace_bl <- est_bl |> filter(issue_id == trace_issue)
trace_wkr <- est_wkr |> filter(issue_id == trace_issue)
info(paste0("Estimates: Kreis=", nrow(trace_kr),
            ", Bundesland=", nrow(trace_bl),
            ", WKR=", nrow(trace_wkr)))
info(paste0("Kreis range: ", round(min(trace_kr$estimate), 3),
            " – ", round(max(trace_kr$estimate), 3)))
info(paste0("Bundesland range: ", round(min(trace_bl$estimate), 3),
            " – ", round(max(trace_bl$estimate), 3)))

# JSON
if (trace_issue %in% names(est_kr_json)) {
  trace_json <- est_kr_json[[trace_issue]]
  info(paste0("JSON: ", nrow(trace_json), " Kreise"))
  check_t(nrow(trace_json) == nrow(trace_kr),
          "JSON row count matches RDS",
          paste0("JSON has ", nrow(trace_json), " rows vs RDS ", nrow(trace_kr)))
}


# ============================================================================
# STAGE 9: SAVED MODEL CHECK (if models exist)
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 9: SAVED MODELS")
message(strrep("=", 70))

model_dir <- file.path(mb_root, "data", "estimates", "models")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)

info(paste0("Saved model files: ", length(model_files)))

if (length(model_files) > 0) {
  for (mf in model_files) {
    mname <- basename(mf)
    mod <- tryCatch(readRDS(mf), error = function(e) NULL)
    if (is.null(mod)) {
      fail(paste0(mname, " failed to load"))
      next
    }
    # Check if it's a glmer or brms model
    if (inherits(mod, "glmerMod")) {
      conv <- mod@optinfo$conv$lme4
      if (is.null(conv) || length(conv) == 0) {
        pass(paste0(mname, ": lme4 model, converged"))
      } else {
        warn(paste0(mname, ": lme4 model, convergence warnings"))
      }
      n_obs <- nrow(mod@frame)
      n_fe  <- length(fixef(mod))
      n_re  <- length(ranef(mod))
      info(paste0("  N=", n_obs, ", fixed effects=", n_fe, ", RE groups=", n_re))
    } else if (inherits(mod, "brmsfit")) {
      pass(paste0(mname, ": brms model loaded"))
    } else {
      info(paste0(mname, ": unknown model class (", class(mod)[1], ")"))
    }
  }
} else {
  info("No saved model files found (models may be fitted in-memory)")
}


# ============================================================================
# STAGE 10: CONCORDANCE INTERNAL CONSISTENCY
# ============================================================================

message("\n", strrep("=", 70))
message("STAGE 10: CONCORDANCE INTERNAL CONSISTENCY")
message(strrep("=", 70))

# --- 10.1 Required columns ---
message("\n--- 10.1 Concordance columns ---")

conc_expected <- c("issue_id", "dataset", "variable", "binary_rule",
                   "response_type", "direction", "category")
for (col in conc_expected) {
  check_t(col %in% names(conc),
          paste0("Concordance has '", col, "'"),
          paste0("Concordance missing '", col, "'"))
}

# --- 10.2 Consistent metadata per issue ---
message("\n--- 10.2 Metadata consistency within issues ---")

meta_consistency <- conc |>
  group_by(issue_id) |>
  summarise(
    n_binary_rules = n_distinct(binary_rule, na.rm = TRUE),
    n_response_types = n_distinct(response_type, na.rm = TRUE),
    n_directions = n_distinct(direction, na.rm = TRUE),
    n_categories = n_distinct(category, na.rm = TRUE),
    .groups = "drop"
  )

# Direction must be consistent (hard fail) — different scales/rules across datasets are OK
dir_inconsistent <- meta_consistency |>
  filter(n_directions > 1)

check_t(nrow(dir_inconsistent) == 0,
        "All issues have consistent direction across datasets",
        paste0(nrow(dir_inconsistent), " issues have inconsistent direction: ",
               paste(dir_inconsistent$issue_id, collapse = ", ")))

# Different response_types/binary_rules across datasets is expected (ALLBUS binary vs GLES scales)
scale_inconsistent <- meta_consistency |>
  filter(n_binary_rules > 1 | n_response_types > 1)
warn_t(nrow(scale_inconsistent) == 0,
       "All issues have same response_type/binary_rule across datasets",
       paste0(nrow(scale_inconsistent), " issues have varying scales across datasets (expected for multi-source pooling): ",
              paste(scale_inconsistent$issue_id, collapse = ", ")))

# --- 10.3 Valid binary rules ---
message("\n--- 10.3 Binary rule format ---")

valid_rule_pattern <- "^y=1 if [<>]=? \\d+$|^continuous$"
invalid_rules <- conc |>
  filter(!is.na(binary_rule),
         !str_detect(binary_rule, valid_rule_pattern))

check_t(nrow(invalid_rules) == 0,
        "All binary rules follow expected format",
        paste0(nrow(invalid_rules), " invalid binary rules: ",
               paste(unique(invalid_rules$binary_rule), collapse = ", ")))

# --- 10.4 Binary rule ↔ response type consistency ---
message("\n--- 10.4 Rule ↔ scale consistency ---")

rule_scale_check <- conc |>
  filter(!is.na(binary_rule), binary_rule != "continuous") |>
  mutate(threshold = as.numeric(str_extract(binary_rule, "\\d+"))) |>
  mutate(scale_max = case_when(
    response_type == "scale_1_5"  ~ 5,
    response_type == "scale_1_7"  ~ 7,
    response_type == "scale_1_11" ~ 11,
    response_type == "binary_yn"  ~ 1,
    TRUE ~ NA_real_
  )) |>
  filter(!is.na(scale_max), !is.na(threshold))

out_of_scale <- rule_scale_check |>
  filter(threshold > scale_max)

check_t(nrow(out_of_scale) == 0,
        "All binary thresholds within scale range",
        paste0(nrow(out_of_scale), " rules have threshold > scale max"))


# ============================================================================
# SUMMARY
# ============================================================================

message("\n", strrep("=", 70))
message("PIPELINE CHECK SUMMARY")
message(strrep("=", 70))

message(paste0("\n  \u2705 PASSED: ", n_pass))
message(paste0("  \u274c FAILED: ", n_fail))
message(paste0("  \u26a0\ufe0f  WARNINGS: ", n_warn))
message(paste0("  TOTAL: ", n_pass + n_fail + n_warn, " checks\n"))

if (n_fail == 0) {
  message("\u2705 ALL CHECKS PASSED — pipeline is healthy\n")
} else {
  message(paste0("\u274c ", n_fail, " CHECKS FAILED — investigate above\n"))
}

# Save summary
summary_df <- tibble(
  pass = n_pass,
  fail = n_fail,
  warn = n_warn,
  timestamp = Sys.time()
)
write_csv(summary_df,
          file.path(mb_root, "output", "checks", "check_summary.csv"))
message("Detailed results saved to output/checks/")
