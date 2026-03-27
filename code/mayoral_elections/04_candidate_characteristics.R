### Candidate characteristics: gender, migration background, local surname
# Vincent Heddesheimer
# March 2026
#
# Adds individual-level characteristics to the mayoral candidates dataset:
# 1. Gender — via gender-guesser (Python, Jörg Michael's name database) with
#    manual overrides for rare/typo names. Lookup is pre-computed and stored as
#    CSV; this script only reads and merges it.
# 2. Migration background — name-origin classification (binary + fine-grained)
# 3. Local surname rootedness — telephone directory method (placeholder)
#
# Gender lookup regeneration:
#   The gender lookup CSV is built by the companion Python script
#   code/mayoral_elections/04a_build_gender_lookup.py
#   which uses the `gender-guesser` package (Jörg Michael's nam_dict.txt,
#   ~70k names with country-specific gender codes). Run that script first
#   if candidate_first_name values have changed.
#
# Reads: data/mayoral_elections/final/mayoral_candidates.rds
#        data/mayoral_elections/processed/gender_guesser_lookup.csv
# Writes: data/mayoral_elections/final/mayoral_candidates.rds (updated)
#         data/mayoral_elections/final/mayoral_candidates.csv (updated)
#         data/mayoral_elections/processed/name_origin_lookup.rds

rm(list = ls())
gc()

pacman::p_load(
  tidyverse,
  data.table,
  conflicted,
  here
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("first", "dplyr")

setwd(here::here())
options(scipen = 999)

dir.create("data/mayoral_elections/processed", showWarnings = FALSE, recursive = TRUE)


# ============================================================================
# 1. LOAD DATA
# ============================================================================

cat("=== Loading candidate data ===\n")
cand <- read_rds("data/mayoral_elections/final/mayoral_candidates.rds")
cat(sprintf("Loaded %d candidates\n", nrow(cand)))

# Strip any previously added enrichment columns (idempotent re-runs)
enrichment_cols <- c(
  "candidate_gender_source", "candidate_gender_prob", "candidate_gender_method",
  "candidate_name_origin", "candidate_name_origin_conf", "candidate_name_origin_method",
  "candidate_migration_bg", "candidate_migration_bg_prob",
  "candidate_local_surname", "candidate_surname_county_share",
  "candidate_surname_n_counties", "candidate_surname_overrep_ratio"
)
cols_to_drop <- intersect(enrichment_cols, names(cand))
if (length(cols_to_drop) > 0) {
  cat(sprintf("Stripping %d enrichment columns from prior run\n", length(cols_to_drop)))
  cand <- cand |> select(-all_of(cols_to_drop))
  # Reset predicted genders back to NA (keep only raw genders from source data)
  if ("candidate_gender_source" %in% cols_to_drop) {
    cand_orig <- read_rds("data/mayoral_elections/final/mayoral_candidates.rds")
    cand$candidate_gender <- cand_orig$candidate_gender
  }
}

cat(sprintf("Existing raw gender coverage: %d / %d (%.1f%%)\n",
            sum(!is.na(cand$candidate_gender)), nrow(cand),
            100 * mean(!is.na(cand$candidate_gender))))


# ============================================================================
# 2. GENDER CLASSIFICATION (gender-guesser lookup)
# ============================================================================

cat("\n=== Gender classification ===\n")

# Read pre-computed gender lookup from Python gender-guesser
# Built by: code/mayoral_elections/04a_build_gender_lookup.py
lookup_path <- "data/mayoral_elections/processed/gender_guesser_lookup.csv"
if (!file.exists(lookup_path)) {
  stop("Gender lookup not found at ", lookup_path,
       "\nRun code/mayoral_elections/04a_build_gender_lookup.py first.")
}
gender_lookup <- fread(lookup_path, colClasses = "character")
cat(sprintf("Loaded gender lookup: %d unique first names\n", nrow(gender_lookup)))
cat(sprintf("  Classified: %d, Not classifiable: %d\n",
            sum(gender_lookup$gender %in% c("m", "w")),
            sum(!gender_lookup$gender %in% c("m", "w"))))

# Merge gender predictions into candidates
cand <- cand |>
  left_join(
    gender_lookup |>
      filter(gender %in% c("m", "w")) |>
      select(candidate_first_name, gender_predicted = gender, gender_method = method),
    by = "candidate_first_name"
  ) |>
  mutate(
    # Source tracking: raw data takes precedence
    candidate_gender_source = case_when(
      !is.na(candidate_gender) ~ "raw",
      !is.na(gender_predicted) ~ "predicted",
      TRUE ~ NA_character_
    ),
    # Method: how was gender determined?
    candidate_gender_method = case_when(
      !is.na(candidate_gender) ~ "raw",
      !is.na(gender_method) ~ gender_method,
      TRUE ~ NA_character_
    ),
    # Confidence: 1.0 for raw data, method-based for predictions
    candidate_gender_prob = case_when(
      !is.na(candidate_gender) ~ 1.0,
      # Manual overrides and German-specific matches are high confidence
      gender_method %in% c("manual", "full_de") ~ 0.99,
      # Hyphenated name first-part matches
      gender_method %in% c("hyphen_first_de") ~ 0.95,
      # Global (non-country-specific) matches
      gender_method %in% c("full_global", "hyphen_first_global", "accent_norm_global") ~ 0.90,
      !is.na(gender_predicted) ~ 0.85,
      TRUE ~ NA_real_
    ),
    # Fill in gender where missing (raw data takes precedence)
    candidate_gender = case_when(
      !is.na(candidate_gender) ~ candidate_gender,
      TRUE ~ gender_predicted
    )
  ) |>
  select(-gender_predicted, -gender_method)

# Gender coverage after classification
cat("\nGender coverage after classification:\n")
coverage <- cand |>
  group_by(state) |>
  summarise(
    n = n(),
    has_name = sum(!is.na(candidate_first_name)),
    n_gender = sum(!is.na(candidate_gender)),
    pct_total = round(100 * mean(!is.na(candidate_gender)), 1),
    pct_named = round(100 * sum(!is.na(candidate_gender)) / pmax(sum(!is.na(candidate_first_name)), 1), 1),
    n_raw = sum(candidate_gender_source == "raw", na.rm = TRUE),
    n_pred = sum(candidate_gender_source == "predicted", na.rm = TRUE),
    .groups = "drop"
  )
print(coverage)

cat(sprintf("\nOverall: %d / %d (%.1f%%) have gender\n",
            sum(!is.na(cand$candidate_gender)), nrow(cand),
            100 * mean(!is.na(cand$candidate_gender))))
cat(sprintf("Among named candidates: %d / %d (%.1f%%)\n",
            sum(!is.na(cand$candidate_gender)),
            sum(!is.na(cand$candidate_first_name)),
            100 * sum(!is.na(cand$candidate_gender)) / sum(!is.na(cand$candidate_first_name))))


# ============================================================================
# 3. MIGRATION BACKGROUND — NAME-ORIGIN CLASSIFICATION
# ============================================================================

cat("\n=== Migration background classification ===\n")

# --- Turkish name lists ---
turkish_surnames <- tolower(c(
  "yilmaz", "kaya", "demir", "celik", "sahin", "yildiz", "yildirim",
  "ozturk", "aydin", "ozdemir", "arslan", "dogan", "kilic", "aslan",
  "cetin", "kara", "koc", "kurt", "ozkan", "simsek", "polat", "korkmaz",
  "turk", "erdogan", "aksoy", "bayrak", "bulut", "coskun", "gul", "gunes",
  "kaplan", "karatas", "keskin", "kurtulus", "oral", "ozer", "peker",
  "sari", "sen", "tas", "tekin", "toprak", "tosun", "turan", "ucar",
  "uzun", "yalcin", "yasar", "yavuz", "yildiran", "akbas", "aktas",
  "akyol", "akyuz", "altun", "ari", "ates", "avci", "baran", "bas",
  "basaran", "bayram", "bicer", "bilgin", "bozkurt", "can", "cinar",
  "coban", "dag", "demirel", "demirtas", "deniz", "dursun", "elmas",
  "erdem", "ercan", "eryilmaz", "genc", "gok", "gokce", "guler",
  "gumus", "gunay", "gunduz", "guner", "gurbuz", "ince", "isik",
  "kahraman", "karagoz", "karaman", "keles", "kocer", "kose",
  "oguz", "ozbek", "ozcelik", "ozen", "parlak", "solmaz", "sonmez",
  "soylu", "tanriverdi", "temel", "tunc", "tuncer", "turhan",
  "turkmen", "unal", "usta", "vardar", "yaman", "yavas"
))

turkish_firstnames <- tolower(c(
  "mehmet", "mustafa", "ahmet", "ali", "hasan", "huseyin", "ibrahim",
  "ismail", "osman", "murat", "yusuf", "omer", "halil", "abdullah",
  "mahmut", "recep", "ramazan", "suleyman", "kemal", "cemal",
  "hakan", "serkan", "burak", "emre", "cem", "fatih", "selim",
  "baris", "erkan", "volkan", "kadir", "orhan", "metin", "erdal",
  "ilhan", "levent", "turgut", "burhan", "cengiz", "dogan",
  "fatma", "ayse", "emine", "hatice", "zeynep", "elif", "havva",
  "sultan", "meryem", "zehra", "hacer", "naciye", "gulsen",
  "nurten", "sevim", "fikriye", "fadime", "gulseren", "nesrin",
  "dilek", "aysel", "serap", "sibel", "filiz", "hulya", "tulay",
  "derya", "ebru", "esra", "betul", "ozlem", "sevgi", "songul",
  "aysegul", "pinar", "gamze", "tugba", "buse", "selin"
))

# --- Arabic name lists ---
arabic_surnames <- tolower(c(
  "mohammed", "mohamed", "ahmad", "ahmed", "hassan", "hussein", "ali",
  "omar", "abdel", "abd", "abdallah", "abdullah", "al-", "el-",
  "nasser", "nassar", "khalil", "khalid", "karim", "rashid", "said",
  "hamid", "habib", "amin", "saleh", "mansour", "haddad", "khoury",
  "ibrahim", "ismail", "moussa", "youssef", "kassem", "farah",
  "bakr", "jaber", "najjar", "sabbagh", "shahin", "taha", "zein"
))

arabic_firstnames <- tolower(c(
  "mohammed", "mohamed", "ahmad", "ahmed", "omar", "youssef", "hassan",
  "hussein", "mustafa", "khalid", "karim", "nabil", "rami", "tariq",
  "samir", "faisal", "adnan", "bassam", "imad", "jihad", "maher",
  "munir", "nasser", "rashid", "salim", "walid", "ziad",
  "fatima", "aisha", "amina", "layla", "maryam", "noura", "rania",
  "samira", "yasmin", "zahra", "huda", "iman", "khadija", "leila",
  "nadia", "nour", "salma", "sana", "sawsan", "suha"
))

# --- Pattern-based detection ---
eastern_european_high_conf <- c(
  "owski$", "ewski$", "inski$", "wski$", "czyk$", "czak$",
  "owicz$", "ewicz$", "ević$", "evic$",
  "enko$", "chuk$", "escu$"
)
eastern_european_patterns <- c(
  eastern_european_high_conf,
  "ynski$", "ski$", "sky$",
  "ovic$", "ić$", "ic$",
  "yshyn$", "iuk$",
  "eanu$", "oiu$",
  "ová$", "ova$", "ek$"
)
southern_european_high_conf <- c(
  "opoulos$", "opoulou$", "idis$", "akis$", "oudis$",
  "elli$", "etti$", "ucci$", "acci$", "azzo$",
  "oglou$", "oglu$"
)

# Classification function
classify_name_origin <- function(first_name, last_name) {
  fn <- tolower(trimws(first_name))
  ln <- tolower(trimws(last_name))

  origin <- "german"
  confidence <- 0.5
  method <- NA_character_

  # --- Turkish ---
  fn_turkish <- fn %in% turkish_firstnames
  ln_turkish <- ln %in% turkish_surnames
  if (fn_turkish && ln_turkish) {
    return(list(origin = "turkish", conf = 0.95, method = "combined"))
  } else if (ln_turkish && !fn_turkish) {
    return(list(origin = "turkish", conf = 0.80, method = "surname_match"))
  } else if (fn_turkish && !ln_turkish) {
    return(list(origin = "turkish", conf = 0.60, method = "firstname_match"))
  }

  # --- Arabic ---
  ln_arabic <- ln %in% arabic_surnames ||
    grepl("^al-", ln) || grepl("^el-", ln) || grepl("^abd", ln)
  fn_arabic <- fn %in% arabic_firstnames
  if (fn_arabic && ln_arabic) {
    return(list(origin = "arabic", conf = 0.95, method = "combined"))
  } else if (ln_arabic && !fn_arabic) {
    return(list(origin = "arabic", conf = 0.75, method = "surname_match"))
  } else if (fn_arabic && !ln_arabic) {
    return(list(origin = "arabic", conf = 0.55, method = "firstname_match"))
  }

  # --- Eastern European (pattern-based) ---
  if (any(grepl(paste(eastern_european_high_conf, collapse = "|"), ln))) {
    return(list(origin = "eastern_european", conf = 0.85, method = "surname_pattern"))
  } else if (any(grepl(paste(eastern_european_patterns, collapse = "|"), ln))) {
    return(list(origin = "eastern_european", conf = 0.60, method = "surname_pattern"))
  }

  # --- Southern European (pattern-based) ---
  if (any(grepl(paste(southern_european_high_conf, collapse = "|"), ln))) {
    return(list(origin = "southern_european", conf = 0.85, method = "surname_pattern"))
  }

  # Default: German origin
  if (grepl("(^sch|^str|^kr|^br|^gr|^pf|^kn)", ln) ||
      grepl("(mann$|berg$|burg$|stein$|feld$|bach$|wald$|hof$|dorf$)", ln)) {
    confidence <- 0.90
    method <- "surname_pattern"
  } else {
    confidence <- 0.50
    method <- "default"
  }

  return(list(origin = origin, conf = confidence, method = method))
}

# Apply classification to all candidates with last names
cat("Classifying name origins...\n")

unique_names <- cand |>
  filter(!is.na(candidate_last_name)) |>
  mutate(
    fn_clean = tolower(trimws(coalesce(candidate_first_name, ""))),
    ln_clean = tolower(trimws(candidate_last_name))
  ) |>
  distinct(fn_clean, ln_clean)

cat(sprintf("  Unique name combinations: %d\n", nrow(unique_names)))

classifications <- unique_names |>
  rowwise() |>
  mutate(
    result = list(classify_name_origin(fn_clean, ln_clean)),
    candidate_name_origin = result$origin,
    candidate_name_origin_conf = result$conf,
    candidate_name_origin_method = result$method
  ) |>
  ungroup() |>
  select(-result) |>
  mutate(
    candidate_migration_bg = as.integer(candidate_name_origin != "german"),
    candidate_migration_bg_prob = case_when(
      candidate_name_origin != "german" ~ candidate_name_origin_conf,
      TRUE ~ 1 - candidate_name_origin_conf  # P(migration) = 1 - P(german)
    )
  )

write_rds(classifications, "data/mayoral_elections/processed/name_origin_lookup.rds")

cat("\nName origin classification summary:\n")
classifications |>
  count(candidate_name_origin) |>
  mutate(pct = round(100 * n / sum(n), 2)) |>
  print()

cat("\nMigration background:\n")
cat(sprintf("  German-origin: %d (%.1f%%)\n",
            sum(classifications$candidate_migration_bg == 0),
            100 * mean(classifications$candidate_migration_bg == 0)))
cat(sprintf("  Migration background: %d (%.1f%%)\n",
            sum(classifications$candidate_migration_bg == 1),
            100 * mean(classifications$candidate_migration_bg == 1)))

# Merge back into candidates
cand <- cand |>
  mutate(
    fn_clean = tolower(trimws(coalesce(candidate_first_name, ""))),
    ln_clean = tolower(trimws(candidate_last_name))
  ) |>
  left_join(
    classifications |> select(fn_clean, ln_clean, candidate_name_origin,
                              candidate_name_origin_conf, candidate_name_origin_method,
                              candidate_migration_bg, candidate_migration_bg_prob),
    by = c("fn_clean", "ln_clean")
  ) |>
  select(-fn_clean, -ln_clean)


# ============================================================================
# 4. LOCAL SURNAME ROOTEDNESS (PLACEHOLDER)
# ============================================================================

cat("\n=== Local surname rootedness ===\n")
cat("BLOCKED: Waiting for telephone directory data from Thomas Tichelbaecker.\n")
cat("Adding placeholder columns with NA values.\n")

cand <- cand |>
  mutate(
    candidate_local_surname = NA_integer_,
    candidate_surname_county_share = NA_real_,
    candidate_surname_n_counties = NA_integer_,
    candidate_surname_overrep_ratio = NA_real_
  )


# ============================================================================
# 5. FINAL SUMMARY & SAVE
# ============================================================================

cat("\n=== Final summary ===\n")
cat(sprintf("Total candidates: %d\n", nrow(cand)))
cat(sprintf("Gender coverage: %d (%.1f%%)\n",
            sum(!is.na(cand$candidate_gender)),
            100 * mean(!is.na(cand$candidate_gender))))
cat(sprintf("  - Raw: %d\n", sum(cand$candidate_gender_source == "raw", na.rm = TRUE)))
cat(sprintf("  - Predicted: %d\n", sum(cand$candidate_gender_source == "predicted", na.rm = TRUE)))
cat(sprintf("  - Among named: %.1f%%\n",
            100 * sum(!is.na(cand$candidate_gender)) / sum(!is.na(cand$candidate_first_name))))
cat(sprintf("Migration bg coverage: %d (%.1f%%)\n",
            sum(!is.na(cand$candidate_migration_bg)),
            100 * mean(!is.na(cand$candidate_migration_bg))))
cat(sprintf("  - German-origin: %d\n", sum(cand$candidate_migration_bg == 0, na.rm = TRUE)))
cat(sprintf("  - Migration bg: %d\n", sum(cand$candidate_migration_bg == 1, na.rm = TRUE)))

cat("\nNew columns:\n")
new_cols <- c(
  "candidate_gender_source", "candidate_gender_prob", "candidate_gender_method",
  "candidate_name_origin", "candidate_name_origin_conf", "candidate_name_origin_method",
  "candidate_migration_bg", "candidate_migration_bg_prob",
  "candidate_local_surname", "candidate_surname_county_share",
  "candidate_surname_n_counties", "candidate_surname_overrep_ratio"
)
for (col in new_cols) {
  n_filled <- sum(!is.na(cand[[col]]))
  cat(sprintf("  %s: %d non-NA (%.1f%%)\n", col, n_filled, 100 * n_filled / nrow(cand)))
}

cat("\n=== Saving ===\n")
write_rds(cand, "data/mayoral_elections/final/mayoral_candidates.rds")
fwrite(cand, "data/mayoral_elections/final/mayoral_candidates.csv")
cat("Saved mayoral_candidates.{rds,csv} with characteristics columns\n")

cat("\n=== Done ===\n")
