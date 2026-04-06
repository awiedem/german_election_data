## 01_harmonize_all.R
## Master harmonization script: reads all survey datasets, extracts and
## harmonizes demographics and policy/attitude variables, applies binary
## coding rules from the issue concordance, and produces a single pooled
## survey file for MRP estimation.
##
## Inputs:
##   meinungsbild/data/raw/gles/ZA6832_v2-0-0.dta       (Tracking 2009-2023)
##   meinungsbild/data/raw/gles/ZA10100_v2-0-0.dta       (Cross-Section 2025)
##   meinungsbild/data/raw/gles/ZA10101_v3-0-0.dta       (RCS 2025)
##   meinungsbild/data/raw/gles/ZA6835_v2-0-0.dta        (Cross-Section Cum. 2009-2021)
##   meinungsbild/data/raw/gles/ZA6834_v1-0-0.dta        (Panel 2016-2021)
##   meinungsbild/data/raw/allbus/ZA8974_v1-0-0.dta      (ALLBUS 2023-2024)
##   meinungsbild/data/issue_concordance.csv
##   meinungsbild/data/raw/gles/crosswalk/btw_*_to_krs21.csv
##
## Outputs:
##   meinungsbild/data/harmonized/survey_pooled.rds       (long format: 1 row per respondent × issue)
##   meinungsbild/data/harmonized/survey_pooled_wide.rds  (wide format: 1 row per respondent, issues as columns)

library(tidyverse)
library(haven)

# ---- Paths ------------------------------------------------------------------
gerda_root <- here::here()
mb_root    <- file.path(gerda_root, "meinungsbild")
raw_gles   <- file.path(mb_root, "data", "raw", "gles")
raw_allbus <- file.path(mb_root, "data", "raw", "allbus")
out_dir    <- file.path(mb_root, "data", "harmonized")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load concordance -------------------------------------------------------
concordance <- read_csv(
  file.path(mb_root, "data", "issue_concordance.csv"),
  show_col_types = FALSE
)

# ---- Load WKR-to-Kreis crosswalks -------------------------------------------
# One crosswalk per WKR boundary phase (5 phases = 5 Bundestag elections)
cw_dir <- file.path(raw_gles, "crosswalk")
cw_files <- list.files(cw_dir, pattern = "btw_.*_to_krs21\\.csv$", full.names = TRUE)

wkr_to_kreis <- map_dfr(cw_files, function(f) {
  yr_label <- str_extract(basename(f), "\\d+")
  # Expand 2-digit year to 4-digit
  yr4 <- ifelse(as.numeric(yr_label) < 50,
                paste0("20", str_pad(yr_label, 2, pad = "0")),
                paste0("19", yr_label))
  read_csv(f, show_col_types = FALSE) |>
    transmute(
      wkr_phase_year = as.integer(yr4),
      wkr_nr   = as.integer(WKR_NR),
      ags      = str_pad(as.character(AGS), 5, pad = "0"),
      pct_area = PERCENTAGE  # area-based percentage of WKR in this Kreis
    )
})

# For each WKR × phase, keep only the Kreis with the largest area overlap
# (assigns each respondent to one Kreis for the cross-classified model)
wkr_primary_kreis <- wkr_to_kreis |>
  group_by(wkr_phase_year, wkr_nr) |>
  slice_max(pct_area, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(wkr_phase_year, wkr_nr, county_code = ags)

# ============================================================================
# DATASET-SPECIFIC EXTRACTION FUNCTIONS
# Each returns a tibble with harmonized columns:
#   respondent_id, survey_source, year, state_code, wkr_nr, county_code,
#   male, age, age_cat, educ, weight, + raw issue variable columns
# ============================================================================

# ---- Helper: age → 5-category factor ----------------------------------------
cut_age <- function(age) {
  cut(age,
      breaks = c(17, 29, 44, 59, 74, Inf),
      labels = c("18-29", "30-44", "45-59", "60-74", "75+"),
      right = TRUE)
}

# ---- Helper: clean haven-labelled to numeric ---------------------------------
as_num <- function(x) {
  # Converts haven_labelled values to numeric, setting negative codes to NA
  x <- as.numeric(x)
  ifelse(x < 0, NA_real_, x)
}

# ---- Helper: education harmonization ----------------------------------------
# Maps diverse German education codings to 5 levels:
#   1 = no_degree, 2 = hauptschule, 3 = realschule, 4 = abitur, 5 = university
#
# GLES standard school coding (d7/d5/s0003_1/s0003_3/pre003):
#   1=ohne Abschluss, 2=Hauptschule, 3=Realschule/Mittlere Reife,
#   4=Fachhochschulreife, 5=Abitur/allg. Hochschulreife, 6=anderer Abschluss,
#   9=noch Schüler
harmonize_school_educ_gles <- function(school_code) {
  case_when(
    school_code == 1            ~ 1L,  # no degree
    school_code == 2            ~ 2L,  # Hauptschule
    school_code == 3            ~ 3L,  # Realschule / Mittlere Reife
    school_code %in% c(4, 5)   ~ 4L,  # FH-Reife or Abitur
    school_code == 6            ~ 3L,  # other → realschule equivalent
    school_code == 9            ~ NA_integer_,  # still in school
    TRUE                        ~ NA_integer_
  )
}

# Tracking s0003_2 uses a 9-category combined school+vocational scheme:
#   1=ohne Abschluss, 2=Hauptschule ohne Lehre, 3=Hauptschule mit Lehre,
#   4=Realschule ohne Lehre, 5=Realschule mit Lehre, 6=Realschule mit Lehre (variant),
#   7=Fachhochschulreife, 8=Abitur, 9=noch Schüler
harmonize_school_educ_tracking_v2 <- function(school_code) {
  case_when(
    school_code == 1            ~ 1L,  # no degree
    school_code %in% c(2, 3)   ~ 2L,  # Hauptschule (with/without vocational)
    school_code %in% c(4, 5, 6) ~ 3L, # Realschule (with/without vocational)
    school_code %in% c(7, 8)   ~ 4L,  # FH-Reife or Abitur
    school_code == 9            ~ NA_integer_,  # still in school
    TRUE                        ~ NA_integer_
  )
}

# Upgrade to university if vocational education indicates tertiary degree
# GLES d8/d6/s0012: multiple binary flags for vocational qualifications
# We check Fachhochschulabschluss (i) and Hochschulabschluss (j)
upgrade_to_university <- function(educ, has_fh, has_uni) {
  has_fh  <- as_num(has_fh)
  has_uni <- as_num(has_uni)
  ifelse(!is.na(has_fh) & has_fh == 1 | !is.na(has_uni) & has_uni == 1,
         5L, educ)
}

# ---- Helper: legislative period from year ------------------------------------
assign_legperiod <- function(year) {
  case_when(
    year <= 2013 ~ "17BT",
    year <= 2017 ~ "18BT",
    year <= 2021 ~ "19BT",
    year <= 2025 ~ "20BT",
    year >  2025 ~ "21BT",
    TRUE         ~ NA_character_
  )
}

# ---- Helper: Bundesland code from numeric ------------------------------------
# GLES bula: 1=SH, 2=HH, 3=NI, 4=HB, 5=NRW, 6=HE, 7=RP, 8=BW, 9=BY,
#            10=SL, 11=BE, 12=BB, 13=MV, 14=SN, 15=ST, 16=TH
bula_to_state_code <- function(bula) {
  bula <- as_num(bula)
  str_pad(as.character(as.integer(bula)), 2, pad = "0")
}

# ============================================================================
# 1. ZA6832: GLES Tracking 2009-2023 (N ≈ 52,341)
# ============================================================================
extract_tracking <- function() {
  message("Loading ZA6832 (GLES Tracking 2009-2023)...")
  d <- read_dta(file.path(raw_gles, "ZA6832_v2-0-0.dta"))

  # Demographics
  out <- tibble(
    respondent_id = paste0("T_", seq_len(nrow(d))),
    survey_source = "gles_tracking",
    # Derive year from sample variable (wave indicator)
    # sample encodes waves; we extract interview year from it
    # The tracking study runs continuously — use the za_nr_od (original study number)
    # to infer the approximate year, or use sample directly
    sample_raw    = as_num(d$sample),
    state_code    = bula_to_state_code(d$s0004),
    east          = as.integer(as_num(d$t0002) == 0),  # t0002: 0=Ost, 1=West
    male          = as.integer(as_num(d$s0001) == 1),  # 1=male, 2=female
    birth_year    = as_num(d$s0002)  # s0002 is year of birth, NOT age
  )

  # Infer year: the tracking study waves map to specific time periods.
  # za_nr_od contains the original sub-study number which maps to election cycles.
  # We'll use the Wahlkreis phase assignments as a year proxy:
  # Phase 1 = 2009 boundaries → ~2009-2012
  # Phase 2 = 2013 boundaries → ~2013-2016
  # Phase 3 = 2017 boundaries → ~2017
  # Phase 4 = 2017 boundaries (redistricting) → ~2018-2020

  # Phase 5 = 2021 boundaries → ~2021-2023
  # More precisely: use the original study number to infer election year
  za_nr_od <- as_num(d$za_nr_od)

  # Map original study numbers to approximate years
  # ZA5700-5757 are pre-election through post-election studies for different BTW cycles
  # We'll derive from phase assignments which is more reliable
  wkr_phase <- case_when(
    !is.na(as_num(d$t0001_1)) ~ 1L,
    !is.na(as_num(d$t0001_2)) ~ 2L,
    !is.na(as_num(d$t0001_3)) ~ 3L,
    !is.na(as_num(d$t0001_4)) ~ 4L,
    !is.na(as_num(d$t0001_5)) ~ 5L,
    TRUE                      ~ NA_integer_
  )

  # Phase → approximate midpoint year
  out$year <- case_when(
    wkr_phase == 1 ~ 2010L,
    wkr_phase == 2 ~ 2014L,
    wkr_phase == 3 ~ 2017L,
    wkr_phase == 4 ~ 2019L,
    wkr_phase == 5 ~ 2022L,
    TRUE           ~ NA_integer_
  )

  # Wahlkreis: coalesce across phases
  out$wkr_nr <- coalesce(
    as_num(d$t0001_1), as_num(d$t0001_2), as_num(d$t0001_3),
    as_num(d$t0001_4), as_num(d$t0001_5)
  ) |> as.integer()

  # Phase year for WKR-to-Kreis crosswalk matching
  out$wkr_phase_year <- case_when(
    wkr_phase == 1 ~ 2005L,
    wkr_phase == 2 ~ 2009L,
    wkr_phase == 3 ~ 2013L,
    wkr_phase == 4 ~ 2017L,
    wkr_phase == 5 ~ 2021L,
    TRUE           ~ NA_integer_
  )

  # Education: school + vocational → 5-level
  # s0003 is split into 3 versions across waves with DIFFERENT coding schemes:
  #   s0003_1 and s0003_3: standard 6-category (1=ohne, 2=Haupt, 3=Real, 4=FH, 5=Abi, 6=anderer)
  #   s0003_2: 9-category combined school+vocational scheme
  educ_v1 <- harmonize_school_educ_gles(as_num(d$s0003_1))
  educ_v2 <- harmonize_school_educ_tracking_v2(as_num(d$s0003_2))
  educ_v3 <- harmonize_school_educ_gles(as_num(d$s0003_3))
  out$educ <- coalesce(educ_v1, educ_v2, educ_v3)
  out$educ <- upgrade_to_university(out$educ, d$s0012i, d$s0012j)

  # Convert birth year to age (approximate: interview year - birth year)
  out$age <- out$year - out$birth_year
  out$birth_year <- NULL
  out$age_cat <- cut_age(out$age)

  # Weights: use the MZ-based weight with time-droppers (most inclusive)
  out$weight <- coalesce(as_num(d$t0003_1), as_num(d$t0003_2))

  # --- Extract issue variables ---
  conc_t <- concordance |> filter(dataset == "ZA6832_tracking0923")
  for (i in seq_len(nrow(conc_t))) {
    var <- conc_t$variable[i]
    if (var %in% names(d)) {
      out[[paste0("raw_", conc_t$issue_id[i], "__", var)]] <- as_num(d[[var]])
    }
  }

  out |> select(-sample_raw)
}

# ============================================================================
# 2. ZA10100: GLES Cross-Section 2025 (N ≈ 7,337)
# ============================================================================
extract_cross2025 <- function() {
  message("Loading ZA10100 (GLES Cross-Section 2025)...")
  d <- read_dta(file.path(raw_gles, "ZA10100_v2-0-0.dta"))

  out <- tibble(
    respondent_id = paste0("C25_", seq_len(nrow(d))),
    survey_source = "gles_cross2025",
    year          = as_num(d$intyear) |> as.integer(),
    state_code    = bula_to_state_code(d$bula),
    east          = as.integer(as_num(d$ostwest) == 0),  # ostwest: 0=Ost, 1=West
    male          = as.integer(as_num(d$d1) == 1),
    age           = {
      yob <- as_num(d$d2a)
      yr  <- as_num(d$intyear)
      ifelse(!is.na(yob) & !is.na(yr), yr - yob, NA_real_)
    },
    wkr_nr        = as_num(d$wknr) |> as.integer(),
    wkr_phase_year = 2021L,  # 2025 election uses 2021-based WKR boundaries (BTW25)
    weight        = as_num(d$w_ipfges)
  )

  # Education
  school <- as_num(d$d7)
  out$educ <- harmonize_school_educ_gles(school)
  # d8a-d8q: multiple binary flags for vocational qualifications
  # d8i = FH-Abschluss, d8o = Bachelor, d8p = Master/Diplom/Magister, d8q = Promotion
  has_fh <- as_num(d$d8i)
  has_ba <- if ("d8o" %in% names(d)) as_num(d$d8o) else rep(NA_real_, nrow(d))
  has_ma <- if ("d8p" %in% names(d)) as_num(d$d8p) else rep(NA_real_, nrow(d))
  has_phd <- if ("d8q" %in% names(d)) as_num(d$d8q) else rep(NA_real_, nrow(d))
  is_uni <- (!is.na(has_fh) & has_fh == 1) |
            (!is.na(has_ba) & has_ba == 1) |
            (!is.na(has_ma) & has_ma == 1) |
            (!is.na(has_phd) & has_phd == 1)
  out$educ <- ifelse(is_uni, 5L, out$educ)

  out$age_cat <- cut_age(out$age)

  # Issue variables
  conc_c <- concordance |> filter(dataset == "ZA10100_cross2025")
  for (i in seq_len(nrow(conc_c))) {
    var <- conc_c$variable[i]
    if (var %in% names(d)) {
      out[[paste0("raw_", conc_c$issue_id[i], "__", var)]] <- as_num(d[[var]])
    }
  }

  out
}

# ============================================================================
# 3. ZA10101: GLES Rolling Cross-Section 2025 (N ≈ 8,562)
# ============================================================================
extract_rcs2025 <- function() {
  message("Loading ZA10101 (GLES RCS 2025)...")
  d <- read_dta(file.path(raw_gles, "ZA10101_v3-0-0.dta"))

  out <- tibble(
    respondent_id = paste0("R25_", seq_len(nrow(d))),
    survey_source = "gles_rcs2025",
    year          = 2025L,
    state_code    = bula_to_state_code(coalesce(
      if ("bula" %in% names(d)) d$bula else NULL,
      if ("sup004" %in% names(d)) d$sup004 else NULL
    )),
    east          = as.integer(as_num(d$ostwest) == 0),  # ostwest: 0=Ost, 1=West
    male          = as.integer(as_num(d$pre001) == 1),
    age           = {
      yob <- as_num(d$pre002a)
      ifelse(!is.na(yob), 2025L - yob, NA_real_)
    },
    wkr_nr        = as_num(d$elecdist25) |> as.integer(),
    wkr_phase_year = 2021L,  # BTW 2025 uses WKR boundaries based on 2021 redistricting
    weight        = if ("w_pre_ipf" %in% names(d)) as_num(d$w_pre_ipf) else 1
  )

  # Education
  school <- as_num(d$pre003)
  out$educ <- harmonize_school_educ_gles(school)
  # pre004a-pre004m: vocational quals. Check for FH/Uni flags
  if ("pre004i" %in% names(d) && "pre004j" %in% names(d)) {
    out$educ <- upgrade_to_university(out$educ, d$pre004i, d$pre004j)
  } else if ("pre004l" %in% names(d) && "pre004m" %in% names(d)) {
    out$educ <- upgrade_to_university(out$educ, d$pre004l, d$pre004m)
  }

  out$age_cat <- cut_age(out$age)

  # Issue variables
  conc_r <- concordance |> filter(dataset == "ZA10101_rcs2025")
  for (i in seq_len(nrow(conc_r))) {
    var <- conc_r$variable[i]
    if (var %in% names(d)) {
      out[[paste0("raw_", conc_r$issue_id[i], "__", var)]] <- as_num(d[[var]])
    }
  }

  out
}

# ============================================================================
# 4. ZA6835: GLES Cross-Section Cumulation 2009-2021 (N ≈ 21,040)
# ============================================================================
extract_cross_cum <- function() {
  message("Loading ZA6835 (GLES Cross-Section Cumulation 2009-2021)...")
  d <- read_dta(file.path(raw_gles, "ZA6835_v2-0-0.dta"))

  out <- tibble(
    respondent_id = paste0("CC_", seq_len(nrow(d))),
    survey_source = "gles_cross_cum",
    year          = as_num(d$intyear) |> as.integer(),
    state_code    = bula_to_state_code(d$bula),
    east          = as.integer(as_num(d$ostwest) == 0),  # ostwest: 0=Ost, 1=West
    male          = as.integer(as_num(d$d1) == 1),
    age           = as_num(d$d2),  # direct age variable
    weight        = if ("w_ipfges_1" %in% names(d)) as_num(d$w_ipfges_1)
                    else if ("w_ow" %in% names(d)) as_num(d$w_ow)
                    else 1
  )

  # Wahlkreis: year-specific columns (wknr_09, wknr_13, wknr_17, wknr_21)
  out$wkr_nr <- coalesce(
    as_num(d$wknr_09), as_num(d$wknr_13),
    as_num(d$wknr_17), as_num(d$wknr_21)
  ) |> as.integer()

  # Map year to WKR boundary phase for crosswalk matching
  out$wkr_phase_year <- case_when(
    out$year <= 2012 ~ 2009L,
    out$year <= 2016 ~ 2013L,
    out$year <= 2020 ~ 2017L,
    out$year >= 2021 ~ 2021L,
    TRUE             ~ NA_integer_
  )

  # Education
  school <- as_num(d$d5)
  out$educ <- harmonize_school_educ_gles(school)
  # d6i = Fachhochschulabschluss, d6j = Hochschulabschluss (if present)
  if ("d6i" %in% names(d) && "d6j" %in% names(d)) {
    out$educ <- upgrade_to_university(out$educ, d$d6i, d$d6j)
  }

  out$age_cat <- cut_age(out$age)

  # Issue variables
  conc_cc <- concordance |> filter(dataset == "ZA6835_cross_cum0921")
  for (i in seq_len(nrow(conc_cc))) {
    var <- conc_cc$variable[i]
    if (var %in% names(d)) {
      out[[paste0("raw_", conc_cc$issue_id[i], "__", var)]] <- as_num(d[[var]])
    }
  }

  out
}

# ============================================================================
# 5. ZA8974: ALLBUS 2023-2024 (N ≈ 29,117)
# ============================================================================
extract_allbus <- function() {
  message("Loading ZA8974 (ALLBUS 2023-2024)...")
  d <- read_dta(file.path(raw_allbus, "ZA8974_v1-0-0.dta"))

  out <- tibble(
    respondent_id = paste0("A_", seq_len(nrow(d))),
    survey_source = "allbus",
    # ZA8974 is a cumulation of ALLBUS 2023-2024. The `sample` variable encodes
    # sampling mode (1=Festnetz, 2=Mobilfunk, 3=SMS/Online), NOT the year.
    # No year variable exists — both years fall in 20BT legislative period.
    # Use 2024 as default (midpoint of fieldwork).
    year          = 2024L,
    # ALLBUS v1 uses non-standard state codes: Berlin-West=11, Berlin-Ost=12,
    # Brandenburg=13, MV=14, Sachsen=15, Sachsen-Anhalt=16, Thüringen=17
    # Map to standard AGS: Berlin=11, Brandenburg=12, MV=13, Sachsen=14, SA=15, TH=16
    state_code    = {
      bula <- as_num(d$v1)
      dplyr::case_when(
        bula >= 1 & bula <= 10 ~ str_pad(as.integer(bula), 2, pad = "0"),
        bula %in% c(11, 12)    ~ "11",  # Berlin-West + Berlin-Ost → Berlin
        bula == 13             ~ "12",  # Brandenburg
        bula == 14             ~ "13",  # Mecklenburg-Vorpommern
        bula == 15             ~ "14",  # Sachsen
        bula == 16             ~ "15",  # Sachsen-Anhalt
        bula == 17             ~ "16",  # Thüringen
        TRUE                   ~ NA_character_
      )
    },
    east          = as.integer(as_num(d$v2) == 1),  # v2: 1=neue Bundesländer (Ost), 2=alte Bundesländer (West)
    male          = as.integer(as_num(d$v175) == 1),
    wkr_nr        = NA_integer_,
    wkr_phase_year = NA_integer_,
    weight        = as_num(d$dwght)
  )

  # Age: v176 = "Alter, kategorisiert" — categorical age groups
  # 1=18-20, 2=21-24, 3=25-29, 4=30-34, 5=35-39, 6=40-44,
  # 7=45-49, 8=50-59, 9=60-69, 10=70-79, 11=80+
  age_raw <- as_num(d$v176)
  out$age <- case_when(
    age_raw == 1  ~ 19,   # 18-20 midpoint
    age_raw == 2  ~ 22.5, # 21-24
    age_raw == 3  ~ 27,   # 25-29
    age_raw == 4  ~ 32,   # 30-34
    age_raw == 5  ~ 37,   # 35-39
    age_raw == 6  ~ 42,   # 40-44
    age_raw == 7  ~ 47,   # 45-49
    age_raw == 8  ~ 54.5, # 50-59
    age_raw == 9  ~ 64.5, # 60-69
    age_raw == 10 ~ 74.5, # 70-79
    age_raw == 11 ~ 85,   # 80+
    TRUE          ~ NA_real_
  )

  # Education: v177 = "Schulabschluss, 5 Kat."
  # 1=Hauptschulabschluss, 2=Mittlere Reife/Realschule, 3=Abitur/FH-Reife,
  # 4=kein Schulabschluss, 5=noch in Schule
  school_raw <- as_num(d$v177)
  out$educ <- case_when(
    school_raw == 1 ~ 2L,  # Hauptschule
    school_raw == 2 ~ 3L,  # Realschule / Mittlere Reife
    school_raw == 3 ~ 4L,  # Abitur / FH-Reife
    school_raw == 4 ~ 1L,  # kein Schulabschluss
    school_raw == 5 ~ NA_integer_,  # noch in Schule → exclude
    TRUE            ~ NA_integer_
  )

  # Upgrade to university if v179 (Abgeschlossenes Studium) = 1 (ja)
  uni <- as_num(d$v179)
  out$educ <- ifelse(!is.na(uni) & uni == 1, 5L, out$educ)

  out$age_cat <- cut_age(out$age)

  # Issue variables
  conc_a <- concordance |> filter(dataset == "ZA8974_allbus")
  for (i in seq_len(nrow(conc_a))) {
    var <- conc_a$variable[i]
    if (var %in% names(d)) {
      out[[paste0("raw_", conc_a$issue_id[i], "__", var)]] <- as_num(d[[var]])
    }
  }

  out
}

# ============================================================================
# 6. EXTRACT ALL DATASETS
# ============================================================================
message("\n=== Extracting and harmonizing survey data ===\n")

datasets <- list()

# Wrap each in tryCatch so one failure doesn't block everything
datasets$tracking   <- tryCatch(extract_tracking(),  error = function(e) {
  warning("Failed to load Tracking: ", e$message); NULL })
datasets$cross2025  <- tryCatch(extract_cross2025(), error = function(e) {
  warning("Failed to load Cross 2025: ", e$message); NULL })
datasets$rcs2025    <- tryCatch(extract_rcs2025(),   error = function(e) {
  warning("Failed to load RCS 2025: ", e$message); NULL })
datasets$cross_cum  <- tryCatch(extract_cross_cum(), error = function(e) {
  warning("Failed to load Cross Cum: ", e$message); NULL })
datasets$allbus     <- tryCatch(extract_allbus(),    error = function(e) {
  warning("Failed to load ALLBUS: ", e$message); NULL })

# Remove NULLs
datasets <- compact(datasets)

message("\nLoaded ", length(datasets), " datasets:")
for (nm in names(datasets)) {
  message("  ", nm, ": ", nrow(datasets[[nm]]), " respondents")
}

# ============================================================================
# 7. HARMONIZE COMMON COLUMNS & BIND
# ============================================================================

# Ensure all datasets have the same core columns before binding
core_cols <- c("respondent_id", "survey_source", "year", "state_code", "east",
               "male", "age", "age_cat", "educ", "wkr_nr", "wkr_phase_year",
               "weight")

ensure_cols <- function(df) {
  for (col in core_cols) {
    if (!col %in% names(df)) {
      if (col %in% c("wkr_nr", "wkr_phase_year")) {
        df[[col]] <- NA_integer_
      } else if (col %in% c("weight")) {
        df[[col]] <- 1
      } else {
        df[[col]] <- NA
      }
    }
  }
  df
}

datasets <- map(datasets, ensure_cols)

# Bind all datasets (raw issue columns will be filled with NA where not present)
survey_wide <- bind_rows(datasets, .id = "dataset_name")

message("\nPooled dataset: ", nrow(survey_wide), " respondents")

# ============================================================================
# 8. ASSIGN KREIS (COUNTY) VIA WKR-TO-KREIS CROSSWALK
# ============================================================================

# Match WKR to primary Kreis
survey_wide <- survey_wide |>
  left_join(wkr_primary_kreis,
            by = c("wkr_phase_year", "wkr_nr"),
            relationship = "many-to-one")

# If county_code.x already existed (shouldn't for our data), coalesce
if ("county_code.x" %in% names(survey_wide)) {
  survey_wide <- survey_wide |>
    mutate(county_code = coalesce(county_code.x, county_code.y)) |>
    select(-county_code.x, -county_code.y)
}

message("Respondents with Kreis assignment: ",
        sum(!is.na(survey_wide$county_code)), " / ", nrow(survey_wide))

# ============================================================================
# 9. LEGISLATIVE PERIOD
# ============================================================================

survey_wide$legperiod <- assign_legperiod(survey_wide$year)

# ============================================================================
# 10. EDUCATION FACTOR LABELS
# ============================================================================

survey_wide$educ_label <- factor(
  survey_wide$educ,
  levels = 1:5,
  labels = c("no_degree", "hauptschule", "realschule", "abitur", "university")
)

# ============================================================================
# 11. APPLY BINARY CODING RULES → LONG FORMAT
# ============================================================================
# For each issue in the concordance, find the corresponding raw column(s),
# apply the binary_rule, and produce a long-format outcome.

# Identify all raw issue columns
raw_issue_cols <- names(survey_wide)[str_starts(names(survey_wide), "raw_")]

# Parse: raw_{issue_id}__{variable}
issue_var_map <- tibble(col = raw_issue_cols) |>
  mutate(
    issue_id = str_replace(col, "^raw_(.+)__.*$", "\\1"),
    variable = str_replace(col, "^raw_.+__(.+)$", "\\1")
  )

# Get unique issues and their binary rules from concordance
issue_rules <- concordance |>
  select(issue_id, variable, binary_rule, response_type, direction) |>
  distinct()

# Function to apply a binary rule to a numeric vector
apply_binary_rule <- function(x, rule) {
  if (is.na(rule) || rule == "continuous") return(x)

  # Parse rules like "y=1 if >= 7", "y=1 if <= 2", "y=1 if == 322",
  # "y=1 if agree", "y=1 if support", "y=1 if oppose (recode)",
  # "y=1 if cannot cope", "y=1 if good"
  if (str_detect(rule, "==\\s*\\d+")) {
    val <- as.numeric(str_extract(rule, "\\d+$"))
    return(as.integer(x == val))
  }
  if (str_detect(rule, ">=\\s*\\d+")) {
    threshold <- as.numeric(str_extract(rule, "\\d+$"))
    return(as.integer(x >= threshold))
  }
  if (str_detect(rule, "<=\\s*\\d+")) {
    threshold <- as.numeric(str_extract(rule, "\\d+$"))
    return(as.integer(x <= threshold))
  }
  # Binary yes/no questions: typically 1=yes, 2=no or 1=agree, 2=disagree
  if (str_detect(rule, "y=1 if agree|y=1 if support|y=1 if good")) {
    return(as.integer(x == 1))
  }
  if (str_detect(rule, "y=1 if oppose|y=1 if cannot cope")) {
    return(as.integer(x == 2))  # 2=no/disagree → support for the "oppose" direction
  }

  # Fallback: treat as binary (1 = support)
  as.integer(x == 1)
}

# Build long-format outcome data
message("\nApplying binary coding rules...")

long_pieces <- list()

for (i in seq_len(nrow(issue_var_map))) {
  iid  <- issue_var_map$issue_id[i]
  var  <- issue_var_map$variable[i]
  col  <- issue_var_map$col[i]

  # Find the matching rule
  rule_row <- issue_rules |>
    filter(issue_id == iid, variable == var)

  if (nrow(rule_row) == 0) next

  rule <- rule_row$binary_rule[1]
  resp_type <- rule_row$response_type[1]

  raw_vals <- survey_wide[[col]]

  # Only keep non-NA responses
  valid_idx <- which(!is.na(raw_vals))
  if (length(valid_idx) == 0) next

  y_coded <- apply_binary_rule(raw_vals[valid_idx], rule)

  long_pieces[[length(long_pieces) + 1]] <- tibble(
    row_idx   = valid_idx,
    issue_id  = iid,
    y         = y_coded,
    raw_value = raw_vals[valid_idx],
    response_type = resp_type
  )
}

long_outcomes <- bind_rows(long_pieces)

message("Long outcomes: ", nrow(long_outcomes), " issue-responses across ",
        n_distinct(long_outcomes$issue_id), " issues")

# ============================================================================
# 12. JOIN LONG OUTCOMES WITH RESPONDENT DATA
# ============================================================================

# Core respondent-level data (without raw issue columns)
respondent_data <- survey_wide |>
  mutate(row_idx = row_number()) |>
  select(row_idx, respondent_id, survey_source, dataset_name,
         year, legperiod, state_code, east, county_code, wkr_nr,
         male, age, age_cat, educ, educ_label, weight)

survey_long <- long_outcomes |>
  left_join(respondent_data, by = "row_idx") |>
  select(-row_idx)

# ============================================================================
# 13. SUMMARY STATISTICS
# ============================================================================

message("\n=== Final pooled dataset summary ===")
message("Total issue-responses: ", nrow(survey_long))
message("Unique respondents:    ", n_distinct(survey_long$respondent_id))
message("Unique issues:         ", n_distinct(survey_long$issue_id))
message("Year range:            ", min(survey_long$year, na.rm = TRUE),
        " - ", max(survey_long$year, na.rm = TRUE))
message("")

# Per-issue counts
issue_summary <- survey_long |>
  group_by(issue_id) |>
  summarise(
    n        = n(),
    n_y1     = sum(y == 1, na.rm = TRUE),
    pct_y1   = round(100 * mean(y, na.rm = TRUE), 1),
    n_states = n_distinct(state_code, na.rm = TRUE),
    n_kreise = n_distinct(county_code, na.rm = TRUE),
    n_wkr    = n_distinct(wkr_nr, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  arrange(desc(n))

message("Issue-level summary:")
print(as.data.frame(issue_summary), row.names = FALSE)

# Per-source counts
source_summary <- survey_long |>
  group_by(survey_source) |>
  summarise(
    n_responses  = n(),
    n_respondents = n_distinct(respondent_id),
    n_issues     = n_distinct(issue_id),
    .groups = "drop"
  )

message("\nSource-level summary:")
print(as.data.frame(source_summary), row.names = FALSE)

# ============================================================================
# 14. SAVE
# ============================================================================

# Long format (for MRP: one row per respondent × issue)
saveRDS(survey_long,
        file.path(out_dir, "survey_pooled.rds"))
message("\nSaved: ", file.path(out_dir, "survey_pooled.rds"))

# Wide format (one row per respondent, issues as columns — useful for exploration)
# Keep only the binary-coded outcomes as wide columns
wide_outcomes <- long_outcomes |>
  select(row_idx, issue_id, y) |>
  # If a respondent has multiple variables for the same issue (from same dataset),

  # keep the first non-NA
  group_by(row_idx, issue_id) |>
  slice(1) |>
  ungroup() |>
  pivot_wider(names_from = issue_id, values_from = y, names_prefix = "y_")

survey_pooled_wide <- respondent_data |>
  left_join(wide_outcomes, by = "row_idx") |>
  select(-row_idx)

saveRDS(survey_pooled_wide,
        file.path(out_dir, "survey_pooled_wide.rds"))
message("Saved: ", file.path(out_dir, "survey_pooled_wide.rds"))

message("\n=== Harmonization complete ===")
