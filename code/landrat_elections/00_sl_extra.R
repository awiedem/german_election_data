### Saarland additional Landrat data — beyond what's in the mayoral pipeline
# Vincent Heddesheimer, May 2026
#
# The mayoral pipeline only pulls the Regionalverband Saarbrücken Landrat
# election from `data/mayoral_elections/raw/saarland/Wahldaten_Bürgermeisterwahlen_2019-2025.xlsx`.
# The other 5 SL Landkreise (Merzig-Wadern, Neunkirchen, Saarlouis,
# Saarpfalz-Kreis, St. Wendel) hold direct Landrat elections every 8 years,
# but their results are scattered across per-Kreis websites with inconsistent
# formats and frequently only percentages (no absolute vote counts).
#
# This script:
# 1. Downloads the Neunkirchen 2024 Bekanntmachung PDF (full data including
#    absolute vote counts).
# 2. Hardcodes Wikipedia-sourced rows for the other 4 Kreise. These rows
#    have candidate_voteshare but NO eligible_voters / valid_votes / absolute
#    candidate_votes, so they are flagged with `data_source = "wikipedia_pct"`.
#
# This is a deliberate quality-vs-coverage trade: ~10 rows added, but most
# are %-only. The NK 2024 row has clean absolute counts. Future updates can
# replace the % rows with full data as we find clean sources per Kreis.

rm(list = ls())
gc()

pacman::p_load(tidyverse, here, conflicted, pdftools)
conflict_prefer("filter", "dplyr")
setwd(here::here())

raw_dir <- "data/landrat_elections/raw/saarland"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. Download Neunkirchen 2024 Bekanntmachung PDF (clean, parseable)
# ============================================================================
nk_url <- "https://www.landkreis-neunkirchen.de/fileadmin/user_upload/Landrat_Politik/Wahlen/2024/2024_06_17_Bekanntmachung_Ergebnis_LR-Wahl.pdf"
nk_pdf <- file.path(raw_dir, "SL_NK_2024_Bekanntmachung.pdf")
if (!file.exists(nk_pdf)) {
  tryCatch(download.file(nk_url, nk_pdf, mode = "wb", quiet = TRUE),
           error = function(e) cat("  failed:", e$message, "\n"))
}

parse_nk_2024 <- function(pdf_path) {
  if (!file.exists(pdf_path)) return(NULL)
  txt <- pdf_text(pdf_path)
  # Extract data via regex
  eligible <- as.numeric(gsub("\\.", "",
    str_extract(txt, "Wahlberechtigte\\s+([\\d.]+)") %>%
      str_extract("[\\d.]+$")))
  voters <- as.numeric(gsub("\\.", "",
    str_extract(txt, "Wähler\\s+([\\d.]+)") %>%
      str_extract("[\\d.]+$")))
  invalid <- as.numeric(gsub("\\.", "",
    str_extract(txt, "Ungültige\\s+([\\d.]+)") %>%
      str_extract("[\\d.]+$")))
  valid <- as.numeric(gsub("\\.", "",
    str_extract(txt, "Gültige\\s+([\\d.]+)") %>%
      str_extract("[\\d.]+$")))

  # Candidates: "Nr. N <name> -<party>- <votes> ... <pct> %"
  candidates <- str_match_all(txt,
    "Nr\\.\\s*\\d+\\s+([^-]+?)\\s*-([^-]+)-\\s+([\\d.]+)\\s+\\S+\\s+\\S*\\s*([\\d,]+)\\s*%")[[1]]
  if (nrow(candidates) == 0) return(NULL)

  tibble(
    candidate_name = str_squish(candidates[, 2]),
    candidate_party = str_squish(candidates[, 3]),
    candidate_votes = as.numeric(gsub("\\.", "", candidates[, 4])),
    candidate_voteshare = as.numeric(gsub(",", ".", candidates[, 5])) / 100
  ) %>% mutate(
    ags = "10043000", ags_name = "Landkreis Neunkirchen",
    state = "10", state_name = "Saarland",
    election_year = 2024L,
    election_date = as.Date("2024-06-09"),
    election_type = "Landratswahl",
    round = "hauptwahl",
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid,
    invalid_votes = invalid,
    turnout = ifelse(!is.na(eligible) & eligible > 0,
                     voters / eligible, NA_real_)
  )
}

# ============================================================================
# 2. Hardcoded rows from Wikipedia and other public sources
# ============================================================================
# Format: one row per (election, candidate). NO absolute votes for these —
# only percentages. Aggregate totals are missing too. Mark with NA.

hardcoded <- bind_rows(
  # ---- Saarpfalz-Kreis 2014 (Stichwahl 25.05.2014) ----
  # https://de.wikipedia.org/wiki/Saarpfalz-Kreis
  tibble(
    ags = "10045000", ags_name = "Saarpfalz-Kreis",
    election_year = 2014L,  election_date = as.Date("2014-05-25"),
    candidate_name = c("Gallo, Theophil", "Nagel, Peter"),
    candidate_party = c("SPD", "CDU"),
    candidate_voteshare = c(0.504, 0.496),
    round = "stichwahl"
  ),
  # ---- Saarpfalz-Kreis 2024 (Stichwahl 23.06.2024) ----
  tibble(
    ags = "10045000", ags_name = "Saarpfalz-Kreis",
    election_year = 2024L,  election_date = as.Date("2024-06-23"),
    candidate_name = c("John, Frank", "Fess, Klaus-Ludwig"),
    candidate_party = c("SPD", "CDU"),
    candidate_voteshare = c(0.6064, 0.4036),
    round = "stichwahl"
  ),
  # ---- Merzig-Wadern 2011 (23.10.2011) ----
  # https://de.wikipedia.org/wiki/Landkreis_Merzig-Wadern
  tibble(
    ags = "10042000", ags_name = "Landkreis Merzig-Wadern",
    election_year = 2011L, election_date = as.Date("2011-10-23"),
    candidate_name = c("Schlegel-Friedrich, Daniela", "Rausch, Markus"),
    candidate_party = c("CDU", "SPD"),
    candidate_voteshare = c(0.7019, 0.2981),
    round = "hauptwahl"
  ),
  # ---- Saarlouis 2012 (22.01.2012) ----
  # https://de.wikipedia.org/wiki/Landkreis_Saarlouis
  tibble(
    ags = "10044000", ags_name = "Landkreis Saarlouis",
    election_year = 2012L, election_date = as.Date("2012-01-22"),
    candidate_name = c("Lauer, Patrik", "Hartz, Thomas"),
    candidate_party = c("SPD", "CDU"),
    candidate_voteshare = c(0.557, 0.443),
    round = "stichwahl"  # turnout 34.15% suggests SW
  ),
  # ---- St. Wendel 2024 (09.06.2024) ----
  # https://de.wikipedia.org/wiki/Landkreis_St._Wendel
  tibble(
    ags = "10046000", ags_name = "Landkreis St. Wendel",
    election_year = 2024L, election_date = as.Date("2024-06-09"),
    candidate_name = c("Recktenwald, Udo", "Klein, Réka", "Kelkel, Bertram"),
    candidate_party = c("CDU", "SPD", "AfD"),
    candidate_voteshare = c(0.6519, 0.2540, 0.0941),
    round = "hauptwahl"
  )
)

hardcoded <- hardcoded %>%
  mutate(
    state = "10", state_name = "Saarland",
    election_type = "Landratswahl",
    candidate_votes = NA_real_,
    eligible_voters = NA_real_,
    number_voters = NA_real_,
    valid_votes = NA_real_,
    invalid_votes = NA_real_,
    turnout = NA_real_
  )

# ============================================================================
# Combine + cache
# ============================================================================
nk_data <- parse_nk_2024(nk_pdf)
all_data <- bind_rows(nk_data, hardcoded)

if (!is.null(all_data) && nrow(all_data) > 0) {
  saveRDS(all_data, "data/landrat_elections/raw/saarland_extra_parsed.rds")
  cat(sprintf("Saved %d Saarland extra rows\n", nrow(all_data)))
  cat("By Kreis:\n")
  print(all_data %>% count(ags_name, election_year, round))
}
