### Candidate-level mayoral election dataset
# Vincent Heddesheimer
# March 2026
#
# Creates a candidate-level dataset from mayoral election raw files.
# One row = one candidate in one election (long format).
# Companion to 01_mayoral_unharm.R which produces election-level summaries.
#
# Coverage:
# - Bayern (Bavaria) — 1945-2025 (party + votes only, no candidate names)
# - NRW (North Rhine-Westphalia) — 2009-2025 (full candidate names)
# - Saarland — 2019-2025 (names, gender, office type)
# - Sachsen (Saxony) — 2001-2024 (last names only)
# - Rheinland-Pfalz — 1994-2024 (names, percentages only)
# - Niedersachsen (Lower Saxony) — 2006-2025 (names, profession, birth year)

rm(list = ls())
gc()

# Load libraries
pacman::p_load(
  tidyverse,
  readxl,
  data.table,
  lubridate,
  conflicted,
  here,
  pdftools
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")

# Set working directory
setwd(here::here())

# Disallow scientific notation
options(scipen = 999)

# Helper function to pad AGS codes
pad_zero_conditional <- function(x, n, pad = "0") {
  x <- as.character(x)
  x[nchar(x) < n] <- paste0(pad, x[nchar(x) < n])
  return(x)
}

# Flag non-decisive Bayern rounds — i.e. rounds that did NOT seat the mayor and
# are superseded by a later valid round (mirrors compute_flag_superseded in
# 01_mayoral_unharm.R; KEEP THE TWO DEFINITIONS IN SYNC). Two cases:
#   (A) annulled rounds: Wahlart contains "ungültig" (Hauptwahl/Stichwahl ungültig).
#   (B) a Hauptwahl that did NOT seat a mayor — no absolute majority
#       (winner_voteshare < 0.5), NOT resolved by a Stichwahl within 60 days, and
#       followed by a repeat Hauptwahl (Neuwahl) for the same office within 250 days.
# The majority test separates a FAILED first round (<50%, repeated) from a by-election
# whose predecessor was duly ELECTED (>=50%, seated, then left office); the "completed
# by Stichwahl" test spares the normal first round of a runoff cycle.
# Rows are KEPT (never dropped); the flag just lets users exclude precursor rounds.
# Returns a logical vector aligned to the inputs; non-Bayern states default to FALSE.
compute_flag_superseded <- function(ags, election_type, election_date, round, wahlart,
                                    winner_voteshare) {
  d <- data.table::data.table(
    .i = seq_along(ags),
    ags = as.character(ags),
    etype = as.character(election_type),
    dt = as.Date(election_date),
    rnd = as.character(round),
    wa = as.character(wahlart),
    wv = as.numeric(winner_voteshare)
  )
  d[, annulled := !is.na(wa) & grepl("ungültig", wa, ignore.case = TRUE)]
  d[, `:=`(has_later_hw = FALSE, completed_by_sw = FALSE)]
  d[, `:=`(
    has_later_hw = vapply(seq_len(.N),
      function(k) any(rnd == "hauptwahl" & dt > dt[k] & dt <= dt[k] + 250, na.rm = TRUE),
      logical(1)),
    completed_by_sw = vapply(seq_len(.N),
      function(k) any(rnd == "stichwahl" & dt > dt[k] & dt - dt[k] < 60, na.rm = TRUE),
      logical(1))
  ), by = .(ags, etype)]
  d[, superseded_hw := rnd == "hauptwahl" & !is.na(wv) & wv < 0.5 &
                       has_later_hw & !completed_by_sw]
  d[order(.i), annulled | superseded_hw]
}

# Standardise output columns — ensures all expected columns exist with correct types
standardise_candidates <- function(dt) {
  output_cols <- c(
    "ags", "ags_name", "state", "state_name", "election_year", "election_date",
    "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
    "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
    "candidate_first_name", "candidate_gender", "candidate_party",
    "candidate_votes", "candidate_voteshare", "candidate_birth_year",
    "candidate_profession", "office_type",
    "n_candidates", "candidate_rank", "is_winner", "flag_superseded"
  )

  # Add missing columns as NA
  for (col in output_cols) {
    if (!col %in% names(dt)) dt[[col]] <- NA
  }

  dt <- dt %>%
    mutate(
      ags = as.character(ags),
      ags_name = as.character(ags_name),
      state = as.character(state),
      state_name = as.character(state_name),
      election_year = as.numeric(election_year),
      election_date = as.Date(election_date),
      election_type = as.character(election_type),
      round = as.character(round),
      eligible_voters = as.numeric(eligible_voters),
      number_voters = as.numeric(number_voters),
      valid_votes = as.numeric(valid_votes),
      invalid_votes = as.numeric(invalid_votes),
      turnout = as.numeric(turnout),
      candidate_name = as.character(candidate_name),
      candidate_last_name = as.character(candidate_last_name),
      candidate_first_name = as.character(candidate_first_name),
      candidate_gender = tolower(as.character(candidate_gender)),
      candidate_party = as.character(candidate_party),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      candidate_birth_year = as.numeric(candidate_birth_year),
      candidate_profession = as.character(candidate_profession),
      office_type = as.character(office_type),
      n_candidates = as.integer(n_candidates),
      candidate_rank = as.integer(candidate_rank),
      is_winner = as.logical(is_winner),
      flag_superseded = as.logical(flag_superseded)
    ) %>%
    select(all_of(output_cols))

  return(dt)
}

# Initialize list to store all state data
all_candidate_data <- list()

# ============================================================================
# BAYERN (Bavaria)
# ============================================================================

cat("\n=== Processing Bayern mayoral elections ===\n")

bayern_file <- "data/mayoral_elections/raw/bayern/20251114_Wahlen_seit_1945.xlsx"

bayern_raw <- read_excel(bayern_file, sheet = "20251114_bewerberRBZ1-7") %>%
  as.data.table()

# Build base election-level info
bayern_base <- bayern_raw %>%
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) %>%
  mutate(
    gemeindeschluessel_char = as.character(Gemeindeschlüssel),
    gemeindeschluessel_padded = str_pad(gemeindeschluessel_char, width = 6,
                                         side = "left", pad = "0"),
    ags = paste0("09", gemeindeschluessel_padded),
    election_date = as.Date(`Tag der Wahl`),
    election_year = lubridate::year(election_date),
    state = "09",
    state_name = "Bayern",
    # Classify by the source's own Amtstitel column. Without this, Bayern
    # Landrat elections (1098 rows) and Oberbürgermeister elections (557 rows)
    # were silently labeled "Bürgermeisterwahl" — see
    # docs/mayoral_elections_known_issues.md §14.
    election_type = case_when(
      grepl("^Landrat|^Landrät", Amtstitel) ~ "Landratswahl",
      grepl("^Oberbürgermeister", Amtstitel) ~ "Oberbürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    ),
    # Round: Wahlart distinguishes "erster Wahlgang" from "Stichwahl"
    round = case_when(
      Wahlart %in% c("Stichwahl", "Stichwahl ungültig", "Losentscheid") ~ "stichwahl",
      TRUE ~ "hauptwahl"  # "erster Wahlgang", "Hauptwahl ungültig", NA
    ),
    eligible_voters = as.numeric(Stimmberechtigte),
    number_voters = as.numeric(Wähler),
    valid_votes = as.numeric(`Gültige Stimmen`),
    invalid_votes = as.numeric(`ungültige Stimmen`),
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                     number_voters / eligible_voters, NA),
    ags_name = Gemeindename,
    office_type = as.character(Amtstitel)
  ) %>%
  # Flag annulled / superseded precursor rounds (see compute_flag_superseded).
  # winner_voteshare = the round winner's share of valid votes (same formula as
  # 01_mayoral_unharm.R) — used by the majority test inside the function.
  mutate(
    flag_superseded = compute_flag_superseded(
      ags, election_type, election_date, round, Wahlart,
      ifelse(!is.na(valid_votes) & valid_votes > 0,
             as.numeric(`gültige Stimmen Wahlgewinner`) / valid_votes, NA_real_))
  ) %>%
  filter(!is.na(ags), nchar(ags) == 8)

# Reshape candidates: winner + Bewerber 2-14 → long format
# Winner columns: Wahlvorschlag Wahlgewinner, gültige Stimmen Wahlgewinner
# Bewerber X columns: Wahlvorschlag Bewerber X, gültige Stimmen Bewerber X

# Build candidate rows for winner (rank 1)
bayern_winner <- bayern_base %>%
  mutate(
    candidate_party = `Wahlvorschlag Wahlgewinner`,
    candidate_votes = as.numeric(`gültige Stimmen Wahlgewinner`)
  ) %>%
  filter(!is.na(candidate_party) | !is.na(candidate_votes)) %>%
  select(ags, ags_name, state, state_name, election_year, election_date,
         election_type, round, eligible_voters, number_voters, valid_votes,
         invalid_votes, turnout, office_type, candidate_party, candidate_votes,
         flag_superseded)

# Build candidate rows for Bewerber 2-14
bayern_bewerber_list <- list()
for (i in 2:14) {
  party_col <- paste0("Wahlvorschlag Bewerber ", i)
  votes_col <- paste0("gültige Stimmen Bewerber ", i)

  if (!party_col %in% names(bayern_base) || !votes_col %in% names(bayern_base)) next

  bewerber_i <- bayern_base %>%
    mutate(
      candidate_party = .data[[party_col]],
      candidate_votes = as.numeric(.data[[votes_col]])
    ) %>%
    filter(!is.na(candidate_party) | !is.na(candidate_votes)) %>%
    select(ags, ags_name, state, state_name, election_year, election_date,
           election_type, round, eligible_voters, number_voters, valid_votes,
           invalid_votes, turnout, office_type, candidate_party, candidate_votes,
           flag_superseded)

  if (nrow(bewerber_i) > 0) {
    bayern_bewerber_list[[i]] <- bewerber_i
  }
}

bayern_candidates <- bind_rows(bayern_winner, bind_rows(bayern_bewerber_list)) %>%
  mutate(
    candidate_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                   !is.na(candidate_votes),
                                 candidate_votes / valid_votes, NA)
  ) %>%
  # Compute rank and n_candidates per election
  group_by(ags, election_date) %>%
  mutate(
    candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup()

bayern_clean <- standardise_candidates(bayern_candidates)

cat("Bayern: Processed", nrow(bayern_clean), "candidate rows from",
    n_distinct(paste(bayern_clean$ags, bayern_clean$election_date)), "elections\n")
all_candidate_data[["bayern"]] <- bayern_clean

# ============================================================================
# NRW (North Rhine-Westphalia)
# ============================================================================

cat("\n=== Processing NRW mayoral elections ===\n")

nrw_all_files <- list.files("data/mayoral_elections/raw/nrw/",
                             pattern = "\\.xlsx$",
                             full.names = TRUE,
                             ignore.case = TRUE)
nrw_bm_files <- nrw_all_files[grepl("B.*rgermeisterwahlen", basename(nrw_all_files),
                                      ignore.case = TRUE) &
                                 !grepl("Oberb.*rgermeister", basename(nrw_all_files),
                                        ignore.case = TRUE)]
nrw_ob_files <- nrw_all_files[grepl("Oberb.*rgermeister", basename(nrw_all_files),
                                      ignore.case = TRUE)]

cat("Found", length(nrw_bm_files), "NRW BM files,", length(nrw_ob_files), "NRW OB files\n")

# Process a single NRW Excel file — returns candidate-level long format
process_nrw_candidates <- function(file, skip_rows, default_election_type) {
  election_year <- as.numeric(str_extract(basename(file), "\\d{4}"))

  nrw_raw <- read_excel(file, sheet = 1, skip = skip_rows, col_names = FALSE) %>%
    as.data.table()

  nrw_raw <- nrw_raw %>%
    filter(!grepl("GKZ|Gemeinde|Kreis", as.character(.[[1]]), ignore.case = TRUE))

  if (ncol(nrw_raw) < 10) return(NULL)

  # Reshape: each candidate is a 3-column block starting at column 8
  n_cand_cols <- ncol(nrw_raw) - 7
  n_candidates <- n_cand_cols %/% 3

  cand_list <- list()
  for (i in seq_len(n_candidates)) {
    start_col <- 7 + (i - 1) * 3 + 1
    if (start_col + 2 > ncol(nrw_raw)) break
    cand_df <- nrw_raw[, c(1:7, start_col, start_col + 1, start_col + 2), with = FALSE]
    names(cand_df) <- c("gkz", "gemeinde", "datum", "wahlberechtigte", "waehler",
                         "ungueltige", "gueltige", "name", "wahlvorschlag", "stimmen")
    cand_list[[i]] <- cand_df
  }
  nrw_long <- bind_rows(cand_list)

  nrw_clean <- nrw_long %>%
    mutate(
      gkz = as.character(gkz),
      gemeinde = as.character(gemeinde),
      datum = as.character(datum),
      wahlberechtigte = as.numeric(wahlberechtigte),
      waehler = as.numeric(waehler),
      ungueltige = as.numeric(ungueltige),
      gueltige = as.numeric(gueltige),
      name = as.character(name),
      wahlvorschlag = as.character(wahlvorschlag),
      stimmen = as.numeric(stimmen)
    ) %>%
    filter(!is.na(gkz),
           !grepl("GKZ|Gemeinde|Kreis", gkz, ignore.case = TRUE),
           grepl("^[0-9]+$", gsub("[^0-9]", "", gkz)),
           nchar(gsub("[^0-9]", "", gkz)) >= 4,
           !is.na(stimmen),
           # Remove empty candidate slots (votes=0, no name, no party)
           !(stimmen == 0 & is.na(name) & is.na(wahlvorschlag)))

  if (nrow(nrw_clean) == 0) return(NULL)

  date_from_title <- str_extract(basename(file), "\\d{2}\\.\\d{2}\\.\\d{4}")
  default_date <- if (!is.na(date_from_title)) {
    as.Date(date_from_title, format = "%d.%m.%Y")
  } else {
    as.Date(paste0(election_year, "-01-01"))
  }

  nrw_clean <- nrw_clean %>%
    mutate(
      gkz_clean = str_pad(gsub("[^0-9]", "", gkz), width = 6, side = "left", pad = "0"),
      ags = paste0("05", gkz_clean),
      state = "05",
      state_name = "Nordrhein-Westfalen",
      ags_name = gemeinde,
      # Determine election type from the gemeinde (name) column. The AGS suffix
      # alone is ambiguous in NRW — both kreisfreie Städte and Landkreise have
      # AGS ending in "000". Use the source's own naming convention:
      #   "Krfr. Stadt X" / "Kreisfreie Stadt X" → Oberbürgermeisterwahl
      #   "Kreis X" / "Rhein-Erft-Kreis" / "Hochsauerlandkreis" / "Städteregion X" → Landratswahl
      election_type = case_when(
        default_election_type == "Bürgermeisterwahl" ~ "Bürgermeisterwahl",
        grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion", gemeinde) ~ "Landratswahl",
        grepl("Stadt", gemeinde) ~ "Oberbürgermeisterwahl",
        TRUE ~ "Oberbürgermeisterwahl"
      ),
      datum_num = suppressWarnings(as.numeric(datum)),
      datum_text = suppressWarnings(as.Date(datum, format = "%d.%m.%Y")),
      election_date = as.Date(case_when(
        !is.na(datum_num) & datum_num > 30000 & datum_num < 60000 ~
          as.character(as.Date(datum_num, origin = "1899-12-30")),
        !is.na(datum_text) ~ as.character(datum_text),
        TRUE ~ as.character(default_date)
      )),
      election_year = year(election_date),
      eligible_voters = wahlberechtigte,
      number_voters = waehler,
      valid_votes = gueltige,
      invalid_votes = ungueltige,
      turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0 &
                         !is.na(number_voters),
                       number_voters / eligible_voters, NA),
      # Parse candidate names
      candidate_name = str_trim(name),
      candidate_last_name = str_trim(str_extract(name, "^[^,]+")),
      candidate_first_name = str_trim(str_extract(name, ",\\s*(.+)$", group = 1)),
      candidate_party = str_trim(wahlvorschlag),
      candidate_votes = stimmen,
      candidate_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                     !is.na(stimmen),
                                   stimmen / valid_votes, NA)
    )

  # IT.NRW data error: in `KW 2025 Oberbürgermeister-Landratswahlen.xlsx` every
  # Stichwahl row has cell value 44101 (= 2020-09-27) hardcoded in column C
  # instead of 45928 (= 2025-09-28). Verified via direct XML inspection of the
  # source file — the row's vote totals match the 2025 SW (e.g. Düsseldorf
  # Keller 120430 ≠ 2020 SW 118308; Bonn row pairs Déus + Dörner, but Déus
  # only ran in 2025). Patch the dates so the 2025 SW data is usable. Remove
  # this once IT.NRW corrects the source file.
  if (election_year == 2025 && grepl("Oberb", basename(file))) {
    n_patch <- sum(nrw_clean$election_date == as.Date("2020-09-27"), na.rm = TRUE)
    if (n_patch > 0) {
      cat("  IT.NRW 2025 patch:", n_patch, "rows 2020-09-27 → 2025-09-28\n")
      nrw_clean <- nrw_clean %>%
        mutate(
          election_date = if_else(election_date == as.Date("2020-09-27"),
                                  as.Date("2025-09-28"),
                                  election_date),
          election_year = year(election_date)
        )
    }
  }

  nrw_clean <- nrw_clean %>%
    # Detect Stichwahl: NRW Excel files list HW and SW as separate rows
    # with different dates. Within each ags+election_type, duplicate GKZ rows
    # with a date within 60 days of each other form an election cycle.
    # The earlier date is hauptwahl, the later is stichwahl.
    # We must handle this per-file since files can span multiple election cycles.
    group_by(ags, election_type) %>%
    arrange(election_date) %>%
    mutate(
      # Assign election cycle: dates more than 60 days apart = different cycle
      date_gap = as.numeric(election_date - lag(election_date, default = election_date[1])),
      cycle = cumsum(date_gap > 60),
      .groups = "drop"
    ) %>%
    group_by(ags, election_type, cycle) %>%
    mutate(
      min_date = min(election_date),
      round = ifelse(election_date == min_date, "hauptwahl", "stichwahl")
    ) %>%
    ungroup() %>%
    select(-date_gap, -cycle, -min_date) %>%
    # Compute rank and n_candidates per election-round
    group_by(ags, election_date, election_type, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    select(ags, ags_name, state, state_name, election_year, election_date,
           election_type, round, eligible_voters, number_voters, valid_votes,
           invalid_votes, turnout, candidate_name, candidate_last_name,
           candidate_first_name, candidate_party, candidate_votes,
           candidate_voteshare, candidate_rank, n_candidates, is_winner)

  return(nrw_clean)
}

nrw_list <- list()
for (file in nrw_bm_files) {
  cat("Processing BM:", basename(file), "\n")
  result <- process_nrw_candidates(file, skip_rows = 2,
                                    default_election_type = "Bürgermeisterwahl")
  if (!is.null(result) && nrow(result) > 0) nrw_list[[basename(file)]] <- result
}
for (file in nrw_ob_files) {
  cat("Processing OB:", basename(file), "\n")
  result <- process_nrw_candidates(file, skip_rows = 3,
                                    default_election_type = "Oberbürgermeisterwahl")
  if (!is.null(result) && nrow(result) > 0) nrw_list[[basename(file)]] <- result
}

if (length(nrw_list) > 0 && any(sapply(nrw_list, nrow) > 0)) {
  nrw_combined <- bind_rows(nrw_list) %>%
    # Deduplicate: same candidate in same election-round can appear in multiple files
    # (e.g. 2020 Stichwahl results appear in both 2020 and 2025 OB files)
    distinct(ags, election_date, election_type, round, candidate_name, .keep_all = TRUE) %>%
    # Recompute rank after dedup
    group_by(ags, election_date, election_type, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    arrange(ags, election_year, election_date, candidate_rank)
} else {
  nrw_combined <- data.frame()
}

nrw_clean <- standardise_candidates(nrw_combined)

cat("NRW: Processed", nrow(nrw_clean), "candidate rows from",
    n_distinct(paste(nrw_clean$ags, nrw_clean$election_date)), "elections\n")
all_candidate_data[["nrw"]] <- nrw_clean

# ============================================================================
# SAARLAND
# ============================================================================

cat("\n=== Processing Saarland mayoral elections ===\n")

saarland_file <- "data/mayoral_elections/raw/saarland/Wahldaten_Bürgermeisterwahlen_2019-2025.xlsx"

saarland_raw <- read_excel(saarland_file, sheet = "Erfassung") %>%
  as.data.table()

# Column: Wahlberechtigte/Wähler/Gültige/Ungültige/Partei/Einzelbewerber
# Summary rows have info_type in {Wahlberechtigte, Wähler/-innen, Wähler, Gültige, Ungültige}
# Candidate rows have info_type = party name or "Einzelbewerber"
summary_types <- c("Wahlberechtigte", "Wähler/-innen", "Wähler", "Gültige", "Ungültige")

saarland_candidates <- saarland_raw %>%
  mutate(
    # The source AGS column omits the state prefix. Municipalities carry a
    # 5-digit code ("41511" -> 10041511), but the Kreis-level Regionalverband
    # Saarbrücken carries only its 2-digit Kreis code ("41"). Left-padding that
    # to six digits produced 10000041, which is not a valid AGS: for a
    # Kreis-level unit the Kreis code belongs in positions 3-5 with "000" as the
    # municipality part, i.e. 10041000 (matching county_elec_unharm's 10041100
    # and the sibling Landkreise 10042000, 10043000, ...).
    ags = ifelse(
      nchar(as.character(AGS)) <= 3,
      paste0("10", str_pad(as.character(AGS), width = 3, side = "left", pad = "0"), "000"),
      paste0("10", str_pad(as.character(AGS), width = 6, side = "left", pad = "0"))
    ),
    state = "10",
    state_name = "Saarland",
    election_year = Wahljahr,
    # Classify Regionalverband Saarbrücken (the SL equivalent of a Landkreis;
    # head election is "Regionalverbandsdirektor") as Landratswahl. Without
    # this the 15 RVS rows were silently labeled "Bürgermeisterwahl" — see
    # docs/mayoral_elections_known_issues.md §14.
    election_type = case_when(
      grepl("Regionalverband", `Gemeinde/Kreis`) ~ "Landratswahl",
      TRUE ~ "Bürgermeisterwahl"
    ),
    election_date = as.Date(paste(Wahljahr, Monat, Tag, sep = "-")),
    ags_name = `Gemeinde/Kreis`,
    info_type = `Wahlberechtigte/Wähler/Gültige/Ungültige/Partei/Einzelbewerber`,
    value = as.numeric(`Absolute Stimmen`),
    pct = as.numeric(`Stimmen in Prozent`)
  ) %>%
  filter(!is.na(ags), nchar(ags) == 8)

# Extract election-level summary stats
saarland_election_stats <- saarland_candidates %>%
  filter(info_type %in% summary_types) %>%
  group_by(ags, ags_name, election_date) %>%
  summarise(
    eligible_voters = value[info_type == "Wahlberechtigte"][1],
    number_voters = value[info_type %in% c("Wähler/-innen", "Wähler")][1],
    invalid_votes = value[info_type == "Ungültige"][1],
    valid_votes = value[info_type == "Gültige"][1],
    .groups = "drop"
  ) %>%
  mutate(
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0 &
                       !is.na(number_voters),
                     number_voters / eligible_voters, NA)
  )

# Extract candidate rows
saarland_cand_rows <- saarland_candidates %>%
  filter(!info_type %in% summary_types) %>%
  mutate(
    candidate_first_name = as.character(Vorname),
    candidate_last_name = as.character(Nachname),
    candidate_name = ifelse(!is.na(candidate_last_name),
                            paste0(candidate_last_name,
                                   ifelse(!is.na(candidate_first_name),
                                          paste0(", ", candidate_first_name), "")),
                            NA_character_),
    candidate_gender = as.character(Geschlecht),
    candidate_party = as.character(info_type),
    candidate_votes = value,
    candidate_voteshare = pct / 100,
    office_type = as.character(`Amt (Ehrenamt/Hauptamt)`),
    # Round: Wahlart...3 distinguishes "Bürgermeisterwahl" from "Stichwahl"
    round = case_when(
      `Wahlart...3` == "Stichwahl" ~ "stichwahl",
      TRUE ~ "hauptwahl"
    )
  ) %>%
  select(ags, ags_name, state, state_name, election_year, election_date,
         election_type, round, candidate_name, candidate_last_name,
         candidate_first_name, candidate_gender, candidate_party,
         candidate_votes, candidate_voteshare, office_type)

# Merge election stats with candidate rows
saarland_merged <- saarland_cand_rows %>%
  left_join(saarland_election_stats,
            by = c("ags", "ags_name", "election_date")) %>%
  group_by(ags, election_date, round) %>%
  mutate(
    candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup()

cat("  Saarland round distribution:\n")
print(table(saarland_merged$round))
saarland_clean <- standardise_candidates(saarland_merged)

cat("Saarland: Processed", nrow(saarland_clean), "candidate rows from",
    n_distinct(paste(saarland_clean$ags, saarland_clean$election_date)), "elections\n")
all_candidate_data[["saarland"]] <- saarland_clean

# ============================================================================
# SACHSEN (Saxony)
# ============================================================================

cat("\n=== Processing Sachsen mayoral elections ===\n")

sachsen_file <- "data/mayoral_elections/raw/sachsen/Bürgermeisterlatlas2001bis2024.xlsx"

sachsen_raw <- read_excel(sachsen_file, sheet = "Bürgermeisteratlas", col_names = TRUE) %>%
  as.data.table()

# Build election-level base
sachsen_base <- sachsen_raw %>%
  filter(!is.na(ORTNR)) %>%
  mutate(
    ags = as.character(ORTNR),
    ags_name = ORTNAME,
    state = "14",
    state_name = "Sachsen",
    election_year = as.numeric(Jahr),
    election_date = dmy(KW_TERMIN),
    election_type = "Bürgermeisterwahl",
    status_raw = Status,
    eligible_voters = as.numeric(Wahlberechtigte),
    number_voters = as.numeric(`Wähler`),
    valid_votes = as.numeric(`gültige Stimmen`),
    invalid_votes = as.numeric(`ungültige Stimmen`),
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                     number_voters / eligible_voters, NA)
  )

# Detect round from Status: VE = first round (runoff needed), EE = final result
# EE rows with a matching VE row (same municipality, 7-35 days earlier) = Stichwahl
sn_ve_dates <- sachsen_base %>%
  filter(status_raw == "VE") %>%
  select(ags, ve_date = election_date) %>%
  distinct()

sn_ee_has_ve <- sachsen_base %>%
  filter(status_raw == "EE") %>%
  select(ags, election_date) %>%
  left_join(sn_ve_dates, by = "ags", relationship = "many-to-many") %>%
  mutate(gap = as.numeric(election_date - ve_date)) %>%
  filter(gap > 0, gap < 60) %>%
  distinct(ags, election_date) %>%
  mutate(is_stichwahl_ee = TRUE)

sachsen_base <- sachsen_base %>%
  left_join(sn_ee_has_ve, by = c("ags", "election_date")) %>%
  mutate(
    round = case_when(
      status_raw == "VE" ~ "hauptwahl",
      is_stichwahl_ee == TRUE ~ "stichwahl",
      TRUE ~ "hauptwahl"  # Standalone EE = won outright in first round
    )
  ) %>%
  select(-status_raw, -is_stichwahl_ee)

cat("  Sachsen round distribution:\n")
print(table(sachsen_base$round))

# Reshape candidate columns 1-13 to long format
# Columns: X_WVT (party), X_LNAME (last name), X_Stimmen (votes), X_% (pct)
sachsen_cand_list <- list()
for (i in 1:13) {
  wvt_col <- paste0(i, "_WVT")
  lname_col <- paste0(i, "_LNAME")
  stimmen_col <- paste0(i, "_Stimmen")
  pct_col <- paste0(i, "_%")

  # Check if columns exist
  cols_exist <- c(stimmen_col) %in% names(sachsen_base)
  if (!all(cols_exist)) next

  cand_i <- sachsen_base %>%
    mutate(
      candidate_party = if (wvt_col %in% names(.)) as.character(.data[[wvt_col]]) else NA_character_,
      candidate_last_name = if (lname_col %in% names(.)) as.character(.data[[lname_col]]) else NA_character_,
      candidate_votes = as.numeric(.data[[stimmen_col]]),
      candidate_voteshare_pct = if (pct_col %in% names(.)) as.numeric(.data[[pct_col]]) else NA_real_
    ) %>%
    # Only keep rows where this candidate slot has real data:
    # require votes > 0 OR a non-NA party/name (filters out empty Excel columns parsed as 0)
    filter((!is.na(candidate_votes) & candidate_votes > 0) |
             !is.na(candidate_party) | !is.na(candidate_last_name)) %>%
    mutate(
      # _LNAME contains actual name (with comma) for EB/EV candidates,
      # or full party name for party candidates. Detect via comma presence.
      is_person_name = grepl(",", candidate_last_name),
      candidate_name = ifelse(is_person_name, candidate_last_name, NA_character_),
      # For person names: parse into last/first
      candidate_last_name = ifelse(is_person_name,
                                    str_trim(str_extract(candidate_last_name, "^[^,]+")),
                                    NA_character_),
      candidate_first_name = ifelse(is_person_name,
                                     str_trim(str_extract(candidate_name, ",\\s*(.+)$", group = 1)),
                                     NA_character_),
      candidate_voteshare = ifelse(!is.na(candidate_voteshare_pct),
                                    candidate_voteshare_pct / 100,
                                    ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                             !is.na(candidate_votes),
                                           candidate_votes / valid_votes, NA))
    ) %>%
    select(ags, ags_name, state, state_name, election_year, election_date,
           election_type, round, eligible_voters, number_voters, valid_votes,
           invalid_votes, turnout, candidate_name, candidate_last_name,
           candidate_first_name, candidate_party, candidate_votes,
           candidate_voteshare)

  if (nrow(cand_i) > 0) sachsen_cand_list[[i]] <- cand_i
}

sachsen_candidates <- bind_rows(sachsen_cand_list) %>%
  group_by(ags, election_date) %>%
  mutate(
    candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup() %>%
  arrange(ags, election_date, candidate_rank)

sachsen_clean <- standardise_candidates(sachsen_candidates)

cat("Sachsen: Processed", nrow(sachsen_clean), "candidate rows from",
    n_distinct(paste(sachsen_clean$ags, sachsen_clean$election_date)), "elections\n")
all_candidate_data[["sachsen"]] <- sachsen_clean

# ============================================================================
# RHEINLAND-PFALZ (Rhineland-Palatinate)
# ============================================================================

cat("\n=== Processing Rheinland-Pfalz mayoral elections ===\n")

rlp_file <- "data/mayoral_elections/raw/rlp/20251219_Heddesheimer_Direktwahlen 1994-2024.xlsx"

# RLP has 4 sheets with different municipality types and code formats.
# Multi-row-per-election format: row 1 = election info + first/winner candidate,
# subsequent rows = other candidates (same election, empty Wahltag).
# Data has ONLY percentages (no absolute vote counts).

process_rlp_candidates <- function(rlp_file, sheet_name, sheet_type) {

  raw <- read_excel(rlp_file, sheet = sheet_name, skip = 4, col_names = FALSE)

  # Column mapping depends on sheet type
  if (sheet_type == "landraete") {
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_geschlecht <- NA
    col_ergebnis_hw <- 7; col_ergebnis_sw <- 8
    col_beteiligung_hw <- 9; col_beteiligung_sw <- 10
    has_vorname <- FALSE
  } else if (sheet_type == "ob") {
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_geschlecht <- 7
    col_ergebnis_hw <- 8; col_ergebnis_sw <- 9
    col_beteiligung_hw <- 10; col_beteiligung_sw <- 11
    has_vorname <- FALSE
  } else {
    # VG and Vfr.Gemeinden: separate Nachname (6) and Vorname (7)
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_geschlecht <- 8
    col_ergebnis_hw <- 9; col_ergebnis_sw <- 10
    col_beteiligung_hw <- 11; col_beteiligung_sw <- 12
    has_vorname <- TRUE
    col_vorname <- 7
  }

  # Remove completely blank rows
  raw <- raw[!apply(raw, 1, function(x) all(is.na(x))), ]
  if (nrow(raw) == 0) return(NULL)

  # Assign group ID: each row where col_wahltag is non-NA starts a new election
  raw$group_id <- cumsum(!is.na(raw[[col_wahltag]]))

  results_list <- list()

  for (gid in unique(raw$group_id)) {
    group <- raw[raw$group_id == gid, ]
    first_row <- group[1, ]

    wahltag <- as.Date(first_row[[col_wahltag]])
    stichwahltag <- as.Date(first_row[[col_stichwahltag]])
    schluessel <- as.character(first_row[[col_schluessel]])
    ort_name <- as.character(first_row[[col_name]])

    if (is.na(schluessel)) next

    beteiligung_hw <- as.numeric(first_row[[col_beteiligung_hw]])
    beteiligung_sw <- as.numeric(first_row[[col_beteiligung_sw]])

    # Process ALL candidates in this group (all rows)
    for (row_idx in seq_len(nrow(group))) {
      row <- group[row_idx, ]

      partei <- as.character(row[[col_partei]])

      if (has_vorname) {
        nachname <- as.character(row[[col_kandidat]])
        vorname <- as.character(row[[col_vorname]])
        kandidat_name <- ifelse(!is.na(nachname),
                                paste0(nachname,
                                       ifelse(!is.na(vorname),
                                              paste0(", ", vorname), "")),
                                NA_character_)
      } else {
        kandidat_name <- as.character(row[[col_kandidat]])
        nachname <- kandidat_name
        vorname <- NA_character_
      }

      geschlecht <- if (!is.na(col_geschlecht) && col_geschlecht <= ncol(row)) {
        as.character(row[[col_geschlecht]])
      } else {
        NA_character_
      }

      ergebnis_hw <- as.numeric(row[[col_ergebnis_hw]])
      ergebnis_sw <- as.numeric(row[[col_ergebnis_sw]])

      # Skip rows with no candidate info at all
      if (is.na(partei) && is.na(kandidat_name) && is.na(ergebnis_hw)) next

      # Hauptwahl row for this candidate
      if (!is.na(ergebnis_hw)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          wahltag = wahltag,
          schluessel = schluessel,
          ort_name = ort_name,
          candidate_name = kandidat_name,
          candidate_last_name = nachname,
          candidate_first_name = vorname,
          candidate_gender = geschlecht,
          candidate_party = partei,
          candidate_voteshare = ergebnis_hw / 100,
          turnout = beteiligung_hw / 100,
          round = "hauptwahl",
          stringsAsFactors = FALSE
        )
      }

      # Stichwahl row for this candidate (if Stichwahl data exists)
      if (!is.na(ergebnis_sw) && !is.na(stichwahltag)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          wahltag = stichwahltag,
          schluessel = schluessel,
          ort_name = ort_name,
          candidate_name = kandidat_name,
          candidate_last_name = nachname,
          candidate_first_name = vorname,
          candidate_gender = geschlecht,
          candidate_party = partei,
          candidate_voteshare = ergebnis_sw / 100,
          turnout = beteiligung_sw / 100,
          round = "stichwahl",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(results_list) == 0) return(NULL)

  result <- bind_rows(results_list) %>%
    mutate(
      election_date = wahltag,
      election_year = lubridate::year(election_date)
    )

  return(result)
}

# Process each sheet
rlp_landraete <- process_rlp_candidates(rlp_file, "Landräte", "landraete")
rlp_ob <- process_rlp_candidates(rlp_file, "Oberbürgermeister Krfr.Städte", "ob")
rlp_vg <- process_rlp_candidates(rlp_file, "Bürgermeister Verbandsgemeinden", "vg")
rlp_vfr <- process_rlp_candidates(rlp_file, "Bürgermeister Vfr.Gemeinden", "vfr")

cat("  Landräte:", if(!is.null(rlp_landraete)) nrow(rlp_landraete) else 0, "candidate rows\n")
cat("  OB Krfr.Städte:", if(!is.null(rlp_ob)) nrow(rlp_ob) else 0, "candidate rows\n")
cat("  VG:", if(!is.null(rlp_vg)) nrow(rlp_vg) else 0, "candidate rows\n")
cat("  Vfr.Gemeinden:", if(!is.null(rlp_vfr)) nrow(rlp_vfr) else 0, "candidate rows\n")

# Map Schlüsselnummer to AGS for each sheet type
if (!is.null(rlp_landraete)) {
  rlp_landraete <- rlp_landraete %>%
    mutate(
      ags = paste0("07", str_pad(schluessel, width = 3, side = "left", pad = "0"), "000"),
      election_type = "Landratswahl"
    )
}

if (!is.null(rlp_ob)) {
  rlp_ob <- rlp_ob %>%
    mutate(
      ags = paste0("07", substr(schluessel, 1, 3), "000"),
      election_type = "Oberbürgermeisterwahl"
    )
}

if (!is.null(rlp_vg)) {
  rlp_vg <- rlp_vg %>%
    mutate(
      ags = paste0("07", str_pad(schluessel, width = 6, side = "left", pad = "0")),
      election_type = "VG-Bürgermeisterwahl"
    )
}

if (!is.null(rlp_vfr)) {
  rlp_vfr <- rlp_vfr %>%
    mutate(
      schluessel_clean = gsub("\\s", "", schluessel),
      ags = paste0("07",
                   substr(schluessel_clean, 1, 3),
                   substr(schluessel_clean, nchar(schluessel_clean) - 2,
                          nchar(schluessel_clean))),
      election_type = "Bürgermeisterwahl"
    ) %>%
    select(-schluessel_clean)
}

# Combine all RLP sheets
rlp_combined <- bind_rows(rlp_landraete, rlp_ob, rlp_vg, rlp_vfr) %>%
  mutate(
    state = "07",
    state_name = "Rheinland-Pfalz",
    ags_name = ort_name,
    # RLP only has percentages, no absolute counts
    eligible_voters = NA_real_,
    number_voters = NA_real_,
    valid_votes = NA_real_,
    invalid_votes = NA_real_,
    candidate_votes = NA_real_
  ) %>%
  # Compute rank by voteshare within each election-round
  group_by(ags, election_date, round) %>%
  mutate(
    candidate_rank = rank(-candidate_voteshare, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup() %>%
  select(ags, ags_name, state, state_name, election_year, election_date,
         election_type, round, eligible_voters, number_voters, valid_votes,
         invalid_votes, turnout, candidate_name, candidate_last_name,
         candidate_first_name, candidate_gender, candidate_party,
         candidate_votes, candidate_voteshare, candidate_rank, n_candidates,
         is_winner)

rlp_clean <- standardise_candidates(rlp_combined)

cat("RLP: Processed", nrow(rlp_clean), "candidate rows from",
    n_distinct(paste(rlp_clean$ags, rlp_clean$election_date, rlp_clean$round)),
    "election-rounds\n")
all_candidate_data[["rlp"]] <- rlp_clean

# ============================================================================
# NIEDERSACHSEN (Lower Saxony) - All years (2006-2025)
# ============================================================================

cat("\n=== Processing Niedersachsen mayoral elections (2006-2025) ===\n")

ns_base <- "data/mayoral_elections/raw/niedersachsen/Direktwahlen in Niedersachsen 2006-2025"

if (requireNamespace("pdftools", quietly = TRUE)) {

  # Party name mapping for 2006 (full names → abbreviations)
  ns_party_map_2006 <- c(
    "Christlich Demokratische Union Deutschlands" = "CDU",
    "Sozialdemokratische Partei Deutschlands" = "SPD",
    "BÜNDNIS 90/DIE GRÜNEN" = "GRÜNE",
    "B\u00dcNDNIS 90/DIE GR\u00dcNEN" = "GRÜNE",
    "Die Linkspartei." = "LINKE",
    "Freie Demokratische Partei" = "FDP",
    "Einzelwahlvorschlag" = "EB"
  )

  # --------------------------------------------------------------------------
  # Standard page parser — returns one row per CANDIDATE
  # --------------------------------------------------------------------------
  parse_ns_candidates_standard <- function(page, election_year, election_date,
                                           german_nums = FALSE) {

    schluessel <- str_extract(page, "Schlüssel:\\s*(\\d+)", group = 1)
    if (is.na(schluessel)) return(NULL)

    ort_name <- str_extract(page,
      "in (?:der |dem )?(?:Stadt |Landkreis |SG |Samtgemeinde |Gemeinde |Flecken )?(.+?)\\n\\s*Schlüssel",
      group = 1)
    if (is.na(ort_name)) {
      ort_name <- str_extract(page, "in\\s+(.+?)\\n", group = 1)
    }
    ort_name <- str_trim(ort_name)

    election_type_raw <- case_when(
      grepl("Landrat/Landrätin|Landrätin/Landrat|Landrätin / des Landrates", page) ~ "Landratswahl",
      grepl("Oberbürgermeister", page) ~ "Oberbürgermeisterwahl",
      grepl("Samtgemeindebürgermeister", page) ~ "SG-Bürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    )

    is_stichwahl_required <- grepl("Stichwahl erforderlich", page)

    page_lines <- str_split(page, "\n")[[1]]

    # Extract election-level stats
    extract_last_num <- function(lines, pattern) {
      for (l in lines) {
        if (grepl(pattern, l)) {
          l_clean <- if (german_nums) gsub("\\.", "", l) else l
          nums <- str_extract_all(l_clean, "\\d+")[[1]]
          if (length(nums) > 0) return(as.numeric(tail(nums, 1)))
        }
      }
      return(NA_real_)
    }

    wahlberechtigte <- extract_last_num(page_lines, "Wahlberechtigte")
    waehler <- extract_last_num(page_lines, "Wählerinnen")
    ungueltige <- extract_last_num(page_lines, "Ungültige")
    gueltige <- extract_last_num(page_lines, "Gültige Stimmen")

    # AGS construction
    ags <- case_when(
      nchar(schluessel) <= 3 ~ paste0("03", str_pad(schluessel, 3, "left", "0"), "000"),
      nchar(schluessel) <= 6 ~ paste0("03", str_pad(schluessel, 6, "left", "0")),
      TRUE ~ paste0("03", schluessel)
    )
    ags <- str_pad(ags, width = 8, side = "right", pad = "0")

    # Extract individual candidate blocks
    # Both 2006 and post-2006 share a 3-line structure per candidate:
    # Line 1: "     N            Name, First                 Profession"
    # Line 2: "                                                            VOTES    PCT"
    # Line 3: "   BIRTHYEAR                   Party"
    # In 2006: votes use dots as thousands sep, pct uses comma; party = full name
    # Post-2006: votes are plain numbers, pct uses dot; party = abbreviation
    candidates <- list()

    # Find candidate block start lines: "^\s+\d+\s+\S" (number followed by name)
    # Exclude lines that start with year-like 4-digit numbers (birth year lines)
    # and lines containing "Wahlberechtigte", "Wähler", etc.
    cand_start_indices <- c()
    for (idx in seq_along(page_lines)) {
      l <- page_lines[idx]
      # Match: leading spaces, then a 1-2 digit number, then spaces, then text (name)
      if (grepl("^\\s{2,}\\d{1,2}\\s{2,}\\S", l) &&
          !grepl("Wahlberechtigte|Wähler|Ungültige|Gültige|Wahlbeteiligung|Zusammen|Bewerberin|Lfd|Geburtsjahr|Partei", l)) {
        cand_start_indices <- c(cand_start_indices, idx)
      }
    }

    for (ci in cand_start_indices) {
      name_line <- page_lines[ci]
      # Parse name and profession from line 1
      # Format: "     1            Bockhop, Cord                          Landrat"
      nm <- str_match(name_line, "^\\s+\\d+\\s{2,}(.+?)\\s{3,}(.+?)\\s*$")
      if (is.na(nm[1])) next

      cand_name <- str_trim(nm[2])
      profession <- str_trim(nm[3])

      # Parse name into last, first
      name_parts <- str_split(cand_name, ",\\s*")[[1]]
      last_name <- str_trim(name_parts[1])
      first_name <- if (length(name_parts) > 1) str_trim(paste(name_parts[-1], collapse = ", ")) else NA_character_

      # Line 2: votes and percentage
      votes <- NA_real_
      if (ci + 1 <= length(page_lines)) {
        vote_line <- page_lines[ci + 1]
        if (german_nums) {
          # 2006: "                                                                 55.300   58,0"
          vm <- str_match(vote_line, "([\\d.]+)\\s+\\d+[,.]\\d+\\s*$")
          if (!is.na(vm[1])) {
            votes <- as.numeric(gsub("\\.", "", vm[2]))
          }
        } else {
          # Post-2006: "                                                              93148        89.8"
          vm <- str_match(vote_line, "(\\d+)\\s+\\d+[.]\\d+\\s*$")
          if (!is.na(vm[1])) {
            votes <- as.numeric(vm[2])
          }
        }
      }

      # Line 3: birth year and party
      birth_year <- NA_real_
      party <- NA_character_
      if (ci + 2 <= length(page_lines)) {
        party_line <- page_lines[ci + 2]
        # Format: "   1967                                     EB"
        pm <- str_match(party_line, "^\\s+(\\d{4})\\s+(.+?)\\s*$")
        if (!is.na(pm[1])) {
          birth_year <- as.numeric(pm[2])
          party_raw <- str_trim(pm[3])
          # Map full party name to abbreviation for 2006
          if (german_nums && party_raw %in% names(ns_party_map_2006)) {
            party <- unname(ns_party_map_2006[party_raw])
          } else {
            party <- party_raw
          }
        }
      }

      candidates[[length(candidates) + 1]] <- data.frame(
        candidate_name = cand_name,
        candidate_last_name = last_name,
        candidate_first_name = first_name,
        candidate_party = party,
        candidate_votes = votes,
        candidate_profession = profession,
        candidate_birth_year = birth_year,
        stringsAsFactors = FALSE
      )
    }

    if (length(candidates) == 0) return(NULL)

    cand_df <- bind_rows(candidates) %>%
      mutate(
        ags = ags,
        ags_name = ort_name,
        state = "03",
        state_name = "Niedersachsen",
        election_year = election_year,
        election_date = as.Date(election_date),
        election_type = election_type_raw,
        round = if (is_stichwahl_required) "hauptwahl" else "hauptwahl",
        eligible_voters = wahlberechtigte,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = ungueltige,
        turnout = ifelse(!is.na(wahlberechtigte) & wahlberechtigte > 0 &
                           !is.na(waehler),
                         waehler / wahlberechtigte, NA),
        candidate_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 &
                                       !is.na(candidate_votes),
                                     candidate_votes / gueltige, NA)
      )

    return(cand_df)
  }

  # --------------------------------------------------------------------------
  # 2006 Stichwahl parser — returns candidate rows
  # --------------------------------------------------------------------------
  parse_ns_candidates_2006_stichwahl <- function(page, election_year, election_date) {

    schluessel <- str_extract(page, "Schlüssel:\\s*(\\d+)", group = 1)
    if (is.na(schluessel)) return(NULL)

    ort_name <- str_extract(page,
      "in (?:der |dem )?(?:Stadt |Landkreis |SG |Samtgemeinde |Gemeinde |Flecken )?(.+?)\\n\\s*Schlüssel",
      group = 1)
    if (is.na(ort_name)) {
      ort_name <- str_extract(page, "in\\s+(.+?)\\n", group = 1)
    }
    ort_name <- str_trim(ort_name)

    election_type_raw <- case_when(
      grepl("Landrätin / des Landrates|Landrat", page) ~ "Landratswahl",
      grepl("Oberbürgermeister", page) ~ "Oberbürgermeisterwahl",
      grepl("Samtgemeindebürgermeister", page) ~ "SG-Bürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    )

    page_lines <- str_split(page, "\n")[[1]]

    # Extract Stichwahl election stats (rightmost values in two-column layout)
    extract_rightmost_num <- function(lines, pattern) {
      for (l in lines) {
        if (grepl(pattern, l)) {
          l_clean <- gsub("\\.", "", l)
          nums <- str_extract_all(l_clean, "\\d+")[[1]]
          if (length(nums) >= 2) return(as.numeric(tail(nums, 1)))
          if (length(nums) == 1) return(as.numeric(nums[1]))
        }
      }
      return(NA_real_)
    }

    wahlberechtigte <- extract_rightmost_num(page_lines, "Wahlberechtigte")
    waehler <- extract_rightmost_num(page_lines, "Wählerinnen")
    ungueltige <- extract_rightmost_num(page_lines, "Ungültige")
    gueltige <- extract_rightmost_num(page_lines, "Gültige Stimmen")

    ags <- case_when(
      nchar(schluessel) <= 3 ~ paste0("03", str_pad(schluessel, 3, "left", "0"), "000"),
      nchar(schluessel) <= 6 ~ paste0("03", str_pad(schluessel, 6, "left", "0")),
      TRUE ~ paste0("03", schluessel)
    )
    ags <- str_pad(ags, width = 8, side = "right", pad = "0")

    # Extract candidates from Stichwahl two-column layout
    # Line 1: "   1         Name, First               Profession          VOTES   PCT%"
    # Line 2: " BIRTHYEAR        Party                                    FIRSTROUND_VOTES  FIRSTROUND_%"
    # We want the Stichwahl numbers (first pair on the candidate line)
    candidates <- list()

    cand_start_indices <- c()
    for (idx in seq_along(page_lines)) {
      l <- page_lines[idx]
      if (grepl("^\\s{2,}\\d{1,2}\\s{2,}\\S", l) &&
          !grepl("Wahlberechtigte|Wähler|Ungültige|Gültige|Wahlbeteiligung|Zusammen|Bewerberin|Lfd|Geburtsjahr|Partei|Ergebnis", l)) {
        cand_start_indices <- c(cand_start_indices, idx)
      }
    }

    for (ci in cand_start_indices) {
      name_line <- page_lines[ci]
      # Format: "   1         Proch, Heinz-Jürgen               Bürgermeister                 2.287           52,3%"
      # Extract: number, name, profession, votes, pct
      nm <- str_match(name_line,
        "^\\s+\\d+\\s{2,}(.+?)\\s{3,}(.+?)\\s+([\\d.]+)\\s+\\d+[,.]\\d+%?\\s*$")
      if (is.na(nm[1])) next

      cand_name <- str_trim(nm[2])
      profession <- str_trim(nm[3])
      vote_str <- gsub("\\.", "", nm[4])
      votes <- as.numeric(vote_str)

      name_parts <- str_split(cand_name, ",\\s*")[[1]]
      last_name <- str_trim(name_parts[1])
      first_name <- if (length(name_parts) > 1) str_trim(paste(name_parts[-1], collapse = ", ")) else NA_character_

      # Line 2: birth year and party
      birth_year <- NA_real_
      party <- NA_character_
      if (ci + 1 <= length(page_lines)) {
        party_line <- page_lines[ci + 1]
        pm <- str_match(party_line, "^\\s+(\\d{4})\\s+(.+?)\\s+[\\d.]+\\s+\\d+[,.]\\d+")
        if (!is.na(pm[1])) {
          birth_year <- as.numeric(pm[2])
          party_raw <- str_trim(pm[3])
          if (party_raw %in% names(ns_party_map_2006)) {
            party <- unname(ns_party_map_2006[party_raw])
          } else {
            party <- party_raw
          }
        }
      }

      candidates[[length(candidates) + 1]] <- data.frame(
        candidate_name = cand_name,
        candidate_last_name = last_name,
        candidate_first_name = first_name,
        candidate_party = party,
        candidate_votes = votes,
        candidate_profession = profession,
        candidate_birth_year = birth_year,
        stringsAsFactors = FALSE
      )
    }

    if (length(candidates) == 0) return(NULL)

    cand_df <- bind_rows(candidates) %>%
      mutate(
        ags = ags,
        ags_name = ort_name,
        state = "03",
        state_name = "Niedersachsen",
        election_year = election_year,
        election_date = as.Date(election_date),
        election_type = election_type_raw,
        round = "stichwahl",
        eligible_voters = wahlberechtigte,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = ungueltige,
        turnout = ifelse(!is.na(wahlberechtigte) & wahlberechtigte > 0 &
                           !is.na(waehler),
                         waehler / wahlberechtigte, NA),
        candidate_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 &
                                       !is.na(candidate_votes),
                                     candidate_votes / gueltige, NA)
      )

    return(cand_df)
  }

  # --------------------------------------------------------------------------
  # 2013 tabular parser — winner-only rows (no individual candidate detail)
  # --------------------------------------------------------------------------
  parse_ns_candidates_2013_tabular <- function(pdf_file, election_date,
                                                is_stichwahl = FALSE) {
    pages <- pdftools::pdf_text(pdf_file)
    results <- list()

    page <- pages[1]
    page_lines <- str_split(page, "\n")[[1]]

    for (l in page_lines) {
      l_trimmed <- str_trim(l)
      if (l_trimmed == "" || !grepl("^\\d+\\s", l_trimmed)) next
      if (grepl("Gesamt:", l_trimmed)) next

      all_tokens <- str_extract_all(l_trimmed, "[\\d.,]+%?")[[1]]
      nr <- all_tokens[1]
      n <- length(all_tokens)

      is_stichwahl_required <- !is_stichwahl &&
        grepl("Stichwahl", l_trimmed) && !grepl("Wahlsieg", l_trimmed)

      if (is_stichwahl_required) {
        wahlber <- as.numeric(gsub("\\.", "", all_tokens[n-3]))
        waehler <- as.numeric(gsub("\\.", "", all_tokens[n-2]))
        gueltige <- as.numeric(gsub("\\.", "", all_tokens[n-1]))

        bez_match <- str_match(l_trimmed,
          "^\\d+\\s+(.+?)\\s+(LK|LHH|Stadt|Gem\\.|SG|Flecken)")
        kommune <- if (!is.na(bez_match[1])) str_trim(bez_match[2]) else NA_character_
        bezeichnung <- if (!is.na(bez_match[1])) bez_match[3] else NA_character_

        el_type <- case_when(
          bezeichnung %in% c("LK", "LHH") ~ "Landratswahl",
          TRUE ~ "Bürgermeisterwahl"
        )

        # No winner in Stichwahl-required first round — skip
        next
      }

      # Winner row
      stimmen <- as.numeric(gsub("\\.", "", all_tokens[n-5]))
      wahlber <- as.numeric(gsub("\\.", "", all_tokens[n-3]))
      waehler <- as.numeric(gsub("\\.", "", all_tokens[n-2]))
      gueltige <- as.numeric(gsub("\\.", "", all_tokens[n-1]))

      stimmen_str <- all_tokens[n-5]
      stimmen_pos <- regexpr(stimmen_str, l_trimmed, fixed = TRUE)[1]
      text_part <- str_trim(substr(l_trimmed, nchar(nr) + 1, stimmen_pos - 1))

      text_part <- sub("\\s+Wahlsieg\\s+", " ", text_part)

      winner_party_val <- str_extract(text_part, "\\S+$")
      text_rest <- str_trim(sub("\\S+\\s*$", "", text_part))

      bez_match <- str_match(text_rest,
        "^(.+?)\\s+(LK|LHH|Stadt|Gem\\.|SG|Flecken)\\s*(.*)")
      if (!is.na(bez_match[1])) {
        kommune <- str_trim(bez_match[2])
        bezeichnung <- bez_match[3]
      } else {
        kommune <- NA_character_
        bezeichnung <- NA_character_
      }

      el_type <- case_when(
        bezeichnung %in% c("LK", "LHH") ~ "Landratswahl",
        TRUE ~ "Bürgermeisterwahl"
      )

      results[[length(results) + 1]] <- data.frame(
        ags = NA_character_,
        ags_name = kommune,
        state = "03",
        state_name = "Niedersachsen",
        election_year = 2013L,
        election_date = as.Date(election_date),
        election_type = el_type,
        round = if (is_stichwahl) "stichwahl" else "hauptwahl",
        eligible_voters = wahlber,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = NA_real_,
        turnout = ifelse(!is.na(wahlber) & wahlber > 0,
                         waehler / wahlber, NA),
        candidate_party = winner_party_val,
        candidate_votes = stimmen,
        candidate_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 & !is.na(stimmen),
                                     stimmen / gueltige, NA),
        stringsAsFactors = FALSE
      )
    }

    if (length(results) > 0) bind_rows(results) else NULL
  }

  # --------------------------------------------------------------------------
  # AGS lookup for 2013
  # --------------------------------------------------------------------------
  ns_2013_ags_lookup <- c(
    "Adelebsen" = "03152001",
    "Bakum" = "03460002",
    "Belm" = "03459002",
    "Bückeburg" = "03257007",
    "Cappeln" = "03453003",
    "Dahlenburg" = "03355401",
    "Faßberg" = "03350007",
    "Goslar" = "03153017",
    "Hameln-Pyrmont" = "03252000",
    "Hannover" = "03241001",
    "Hildesheim" = "03254021",
    "Hollenstedt" = "03353403",
    "Jever" = "03455008",
    "Katlenburg-Lindau" = "03155011",
    "Langelsheim" = "03153022",
    "Liebenburg" = "03153023",
    "Marklohe" = "03256403",
    "Neuenkirchen" = "03461401",
    "Northeim LK" = "03155000",
    "Northeim Stadt" = "03155024",
    "Osnabrück" = "03404000",
    "Osterholz" = "03356000",
    "Ronnenberg" = "03241016",
    "Schladen-Werla" = "03158039",
    "Schüttorf" = "03456404",
    "Seevetal" = "03353029",
    "Steyerberg" = "03256025",
    "Sulingen" = "03251037",
    "Syke" = "03251038",
    "Velpke" = "03154403",
    "Wesermarsch" = "03461000",
    "Wiefelstede" = "03451020",
    "Wolfenbüttel" = "03158000"
  )

  # --------------------------------------------------------------------------
  # Year configuration
  # --------------------------------------------------------------------------
  ns_year_config <- list(
    list(year = 2006L, date = "2006-09-10",
         files = list(
           list(path = "DW2006/KW 2006 - Vorläufiges Ergebnis Direktwahlen.pdf",
                parser = "standard", german_nums = TRUE),
           list(path = "DW2006/Stichwahlen 2006.pdf",
                parser = "stichwahl_2006", date = "2006-09-24")
         )),
    list(year = 2011L, date = "2011-09-11",
         files = list(
           list(path = "DW2011/Einzelergebnisse.pdf", parser = "standard")
         )),
    list(year = 2013L, date = "2013-09-22",
         files = list(
           list(path = "DW2013/20130923_Direktwahlen_am_22-09-2013_Ergebnisse.pdf",
                parser = "tabular_2013"),
           list(path = "DW2013/20131007_Stichwahlen_am_06-10-2013_Ergebnisse.pdf",
                parser = "tabular_2013_sw", date = "2013-10-06")
         )),
    list(year = 2014L, date = "2014-05-25",
         files = list(
           list(path = "DW2014/einzelergebnisse-direktwahlen14.pdf", parser = "standard")
         )),
    list(year = 2016L, date = "2016-09-11",
         files = list(
           list(path = "DW2016/DW_Einzel.pdf", parser = "standard")
         )),
    list(year = 2019L, date = "2019-05-26",
         files = list(
           list(path = "DW2019/DW_Einzel.pdf", parser = "standard")
         )),
    list(year = 2021L, date = "2021-09-12",
         files = list(
           list(path = "DW2021/20210912_DW_Einzel.pdf", parser = "standard"),
           list(path = "DW2021/DW_Einzel.pdf", parser = "standard")
         )),
    list(year = 2024L, date = "2024-06-09",
         files = list(
           list(path = "DW2024/DW_Einzel.pdf", parser = "standard")
         )),
    list(year = 2025L, date = "2025-02-23",
         files = list(
           list(path = "DW2025/DW_Einzel.pdf", parser = "standard")
         ))
  )

  # --------------------------------------------------------------------------
  # Process all years
  # --------------------------------------------------------------------------
  ns_all_results <- list()

  for (yc in ns_year_config) {
    cat("  Processing NS", yc$year, "...\n")

    for (fc in yc$files) {
      pdf_path <- file.path(ns_base, fc$path)
      if (!file.exists(pdf_path)) {
        cat("    WARNING: File not found:", fc$path, "\n")
        next
      }

      file_date <- if (!is.null(fc$date)) fc$date else yc$date
      file_year <- yc$year

      if (fc$parser == "standard") {
        pages <- pdftools::pdf_text(pdf_path)
        german_nums <- isTRUE(fc$german_nums)
        file_results <- list()

        for (i in seq_along(pages)) {
          row <- parse_ns_candidates_standard(pages[i], file_year, file_date,
                                              german_nums = german_nums)
          if (!is.null(row)) {
            file_results[[length(file_results) + 1]] <- row
          }
        }

        if (length(file_results) > 0) {
          file_df <- bind_rows(file_results)
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser == "stichwahl_2006") {
        pages <- pdftools::pdf_text(pdf_path)
        file_results <- list()

        for (i in seq_along(pages)) {
          row <- parse_ns_candidates_2006_stichwahl(pages[i], file_year, file_date)
          if (!is.null(row)) {
            file_results[[length(file_results) + 1]] <- row
          }
        }

        if (length(file_results) > 0) {
          file_df <- bind_rows(file_results)
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser %in% c("tabular_2013", "tabular_2013_sw")) {
        is_sw <- fc$parser == "tabular_2013_sw"
        file_df <- parse_ns_candidates_2013_tabular(pdf_path, file_date,
                                                     is_stichwahl = is_sw)

        if (!is.null(file_df) && nrow(file_df) > 0) {
          # Apply AGS lookup
          file_df$ags <- ns_2013_ags_lookup[file_df$ags_name]
          na_mask <- is.na(file_df$ags)
          if (any(na_mask)) {
            type_suffix <- case_when(
              file_df$election_type == "Landratswahl" ~ "LK",
              file_df$election_type == "Oberbürgermeisterwahl" ~ "Stadt",
              TRUE ~ "Stadt"
            )
            compound_key <- paste(file_df$ags_name, type_suffix)
            file_df$ags[na_mask] <- ns_2013_ags_lookup[compound_key[na_mask]]
          }
          na_ags <- sum(is.na(file_df$ags))
          if (na_ags > 0) {
            cat("    WARNING:", na_ags, "municipalities without AGS mapping:",
                paste(file_df$ags_name[is.na(file_df$ags)], collapse = ", "), "\n")
          }
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }
      }
    }
  }

  # Combine all NS results
  if (length(ns_all_results) > 0) {
    ns_combined <- bind_rows(ns_all_results) %>%
      # Deduplicate: 2021 has two PDFs (Endergebnis + vorläufiges Ergebnis)
      # with same candidates but different vote counts.
      # Keep only the Endergebnis = the row with the highest eligible_voters.
      group_by(ags, election_date, round, candidate_name) %>%
      arrange(desc(eligible_voters)) %>%
      slice(1) %>%
      ungroup() %>%
      # Fix implausible birth years from OCR errors (e.g. 1076, 2539, 6935)
      mutate(candidate_birth_year = ifelse(
        !is.na(candidate_birth_year) &
          (candidate_birth_year < 1880 | candidate_birth_year > 2010),
        NA_real_, candidate_birth_year)) %>%
      # Remove rows where ALL candidate data is NA (parsing failures)
      filter(!is.na(candidate_votes) | !is.na(candidate_name)) %>%
      # Compute rank and n_candidates per election-round
      group_by(ags, election_date, round) %>%
      mutate(
        candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
        n_candidates = n(),
        is_winner = candidate_rank == 1
      ) %>%
      ungroup()

    ns_clean <- standardise_candidates(ns_combined)

    cat("Niedersachsen: Processed", nrow(ns_clean), "candidate rows across",
        length(unique(ns_clean$election_year)), "years\n")
    all_candidate_data[["niedersachsen"]] <- ns_clean
  } else {
    cat("WARNING: No Niedersachsen data processed\n")
  }

} else {
  cat("Note: pdftools package not available, skipping Niedersachsen PDF extraction\n")
}

# ============================================================================
# SCHLESWIG-HOLSTEIN
# ============================================================================
# Data is scraped from wahlen-sh.de by 00_sh_scrape.R.
# The scraped data is already in candidate-level format.
# Special cases:
#   - 4 confirmation elections (Ja/Nein) from 2024: candidate appears twice
#     (once as "EB"/"Ja-Stimmen" for yes-votes, once as "Nein-Stimmen" for no-votes).
#     We keep only the Ja row and set candidate_party = "EB".
#   - Party names already standardised by the scraper (standardise_sh_party).
#   - All elections have candidate names (Name, Last format).

cat("\n=== Processing Schleswig-Holstein mayoral elections ===\n")

sh_file <- "data/mayoral_elections/raw/sh/sh_mayoral_scraped.rds"

if (file.exists(sh_file)) {
  sh_raw <- readRDS(sh_file)

  sh_candidates <- sh_raw %>%
    # For Ja/Nein confirmation elections: drop "Nein-Stimmen" rows and fix party
    filter(is.na(candidate_party) | candidate_party != "Nein-Stimmen") %>%
    mutate(
      # Fix Ja-Stimmen party: these are EB candidates in confirmation elections
      candidate_party = ifelse(candidate_party == "Ja-Stimmen", "EB", candidate_party),
      # candidate_name is already in "Last, First" format from the scraper
      election_year = year(election_date),
      # Compute turnout
      turnout = ifelse(!is.na(number_voters) & !is.na(eligible_voters) &
                         eligible_voters > 0,
                       number_voters / eligible_voters, NA_real_)
    ) %>%
    # Compute rank and n_candidates per election-round
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    # Ensure we have all required columns
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = NA_character_
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank,
      is_winner
    )

  sh_clean <- standardise_candidates(sh_candidates)

  cat("Schleswig-Holstein: Processed", nrow(sh_clean), "candidate rows across",
      length(unique(sh_clean$election_year)), "years\n")
  all_candidate_data[["schleswig_holstein"]] <- sh_clean
} else {
  cat("Note: SH scraped data not found at", sh_file, "\n")
  cat("  Run 00_sh_scrape.R first to generate the data.\n")
}

# ============================================================================
# MECKLENBURG-VORPOMMERN
# ============================================================================
# Candidate-level data for MV Oberbürgermeister + Landrat direct elections,
# 2000-2026, parsed from 69 LAIV-MV PDFs by 00_mv_parse.py into
# data/mayoral_elections/raw/mecklenburg_vorpommern/mv_parsed.csv.
# The intermediate is already candidate-level long (one row per candidate per
# round); we only need to add rank / n_candidates / is_winner, like SH.
# Special cases (parsed & flagged in mv_parsed.csv `notes`):
#   - Ja/Nein single-candidate Stichwahl (Rügen 2001-05): only the "Ja" candidate
#     is recorded; its voteshare is NA. That Stichwahl failed -> a Neuwahl
#     (Rügen 2001-09) followed, recorded as a separate cycle.

cat("\n=== Processing Mecklenburg-Vorpommern mayoral/Landrat elections ===\n")

mv_file <- "data/mayoral_elections/raw/mecklenburg_vorpommern/mv_parsed.csv"

if (file.exists(mv_file)) {
  mv_raw <- fread(mv_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name", "candidate_first_name")))

  mv_candidates <- mv_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout)
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = NA_character_
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  mv_clean <- standardise_candidates(mv_candidates)

  cat("Mecklenburg-Vorpommern: Processed", nrow(mv_clean), "candidate rows across",
      length(unique(mv_clean$election_year)), "years\n")
  all_candidate_data[["mecklenburg_vorpommern"]] <- mv_clean
} else {
  cat("Note: MV parsed data not found at", mv_file, "\n")
  cat("  Run 00_mv_parse.py first to generate the data.\n")
}

# ============================================================================
# THÜRINGEN
# ============================================================================
# Candidate-level: th_bm_scraped.csv (all-Gemeinde Bürgermeisterwahlen, scraped
# from wahlen.thueringen.de) + th_ob_parsed.csv (kreisfreie-Stadt OB from raw
# files). Candidate names are mostly redacted (§50 ThürKWO) so candidate_party
# carries the Wahlvorschlag (party / Einzelbewerber label); candidate_name is
# populated only where the Wahlvorschlag is itself a person name (and for the
# OB Info files 2006-2024, which list named candidates).

cat("\n=== Processing Thüringen mayoral elections (BM + OB) ===\n")

th_files <- c("data/mayoral_elections/raw/thueringen_bm/th_bm_scraped.csv",
              "data/mayoral_elections/raw/thueringen/th_ob_parsed.csv")
th_have <- th_files[file.exists(th_files)]

if (length(th_have) > 0) {
  th_raw <- bind_rows(lapply(th_have, function(f)
    fread(f, encoding = "UTF-8",
          colClasses = list(character = c("ags", "ags_name", "state", "state_name",
            "election_date", "candidate_party", "candidate_name")))))

  th_candidates <- th_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      state = "16", state_name = "Thüringen",
      candidate_name = ifelse(candidate_name == "", NA_character_, candidate_name)
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    mutate(
      # Names are usually "Last, First" (OB Info files; Einzelbewerber Wahlvorschläge).
      # Most BM candidate_name is NA (redacted by §50 ThürKWO) -> no person tracking.
      candidate_last_name = ifelse(grepl(",", candidate_name),
                                   trimws(sub(",.*$", "", candidate_name)), NA_character_),
      candidate_first_name = ifelse(grepl(",", candidate_name),
                                    trimws(sub("^[^,]*,\\s*", "", candidate_name)), NA_character_),
      candidate_gender = NA_character_, candidate_birth_year = NA_real_,
      candidate_profession = NA_character_, office_type = NA_character_
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  th_clean <- standardise_candidates(th_candidates)

  cat("Thüringen: Processed", nrow(th_clean), "candidate rows across",
      length(unique(th_clean$election_year)), "years\n")
  all_candidate_data[["thueringen"]] <- th_clean
} else {
  cat("Note: Thüringen parsed data not found.\n")
  cat("  Run 00_th_scrape.py and 00_th_mayoral_parse.py first.\n")
}

# ============================================================================
# BADEN-WÜRTTEMBERG  (hybrid: Komm.ONE candidate-level + StaLA winner-only fallback)
# ============================================================================
# Two BW sources, merged:
#  (1) StaLA report (Tables 13+14) -> bw_parsed.csv (00_bw_parse.py): the *winner*
#      only, but with official-register gender + birth year; "letzte Wahl je
#      Gemeinde" @31.12.2024. candidate_party is always NA (BW has no party).
#  (2) Komm.ONE votemanager -> bw_komm_one_parsed.csv (00_bw_komm_one_scrape.py):
#      ALL candidates + votes + full turnout + Hauptwahl & Stichwahl, for the
#      municipalities whose mayoral election is published in Komm.ONE's modern
#      JSON format (~2021-2026). No party, no birth year.
# Komm.ONE supersedes StaLA where it covers an election (same ags + date): we use
# its full candidate list and drop the StaLA winner-only row for that election.
# To stay strictly additive we graft StaLA's register gender + birth year onto the
# matching Komm.ONE candidate (by ags + date + last name) so the winner keeps those
# attributes; losing candidates get gender predicted downstream by 04. The winner is
# attached to every round so the HW shell pairs with the SW row by name in the wide
# reshaping; is_winner flags the decisive round.

cat("\n=== Processing Baden-Württemberg mayoral elections ===\n")

bw_file <- "data/mayoral_elections/raw/baden_wuerttemberg/bw_parsed.csv"
bw_komm_file <- "data/mayoral_elections/raw/baden_wuerttemberg/bw_komm_one_parsed.csv"

if (file.exists(bw_file)) {
  bw_raw <- fread(bw_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name",
                    "candidate_first_name", "candidate_gender", "round")))

  bw_stala <- bw_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      candidate_birth_year = as.numeric(candidate_birth_year),
      turnout = as.numeric(turnout),
      is_decisive = as.logical(is_decisive)
    ) %>%
    mutate(
      candidate_rank = 1L,           # winner is the only reported candidate
      n_candidates = NA_integer_,    # losing candidates not published by StaLA
      is_winner = is_decisive,       # winner counts as elected in decisive round
      candidate_party = NA_character_,  # BW mayoral candidates have no party
      candidate_profession = NA_character_,
      office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                           "Oberbürgermeister*in", "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  if (file.exists(bw_komm_file)) {
    bk <- fread(bw_komm_file, encoding = "UTF-8",
                colClasses = list(character = c("ags", "ags_name", "state",
                  "state_name", "election_date", "candidate_party", "candidate_name",
                  "candidate_last_name", "candidate_first_name", "round"))) %>%
      mutate(
        election_date = as.Date(election_date),
        election_year = as.integer(election_year),
        eligible_voters = as.numeric(eligible_voters),
        number_voters = as.numeric(number_voters),
        valid_votes = as.numeric(valid_votes),
        invalid_votes = as.numeric(invalid_votes),
        turnout = as.numeric(turnout),
        candidate_votes = as.numeric(candidate_votes),
        candidate_voteshare = as.numeric(candidate_voteshare),
        candidate_rank = as.integer(candidate_rank),
        n_candidates = as.integer(n_candidates),
        is_winner = as.logical(is_winner)
      )
    # Komm.ONE coverage can be PARTIAL: for some runoff elections only the
    # Hauptwahl is published (the Stichwahl is absent from the registry), which
    # would mark the HW leader as the "winner" even though a runoff decided
    # otherwise and would leave the StaLA decisive row un-superseded (a duplicate).
    # So Komm.ONE supersedes StaLA for a cycle ONLY when it captured the DECISIVE
    # round — it has a Stichwahl, or a Hauptwahl winner with an absolute majority
    # (>=50%). Otherwise we fall back to StaLA (which records the decisive winner)
    # for that whole (ags, election_year) cycle and drop the incomplete Komm.ONE rows.
    komm_complete <- bk %>%
      group_by(ags, election_year) %>%
      summarise(decisive = any(round == "stichwahl") ||
                  any(round == "hauptwahl" & is_winner &
                      !is.na(candidate_voteshare) & candidate_voteshare >= 0.5),
                .groups = "drop") %>%
      filter(decisive) %>% select(ags, election_year)
    n_komm_cycles <- nrow(distinct(bk, ags, election_year))
    bk <- bk %>% semi_join(komm_complete, by = c("ags", "election_year"))

    # Graft StaLA register gender + birth year onto the matching Komm.ONE candidate.
    # StaLA has one winner per (ags, election_date); match the Komm.ONE candidate
    # whose name tokens include ALL of the StaLA winner's last-name tokens AND the
    # first token of the StaLA first name. Whole-token (not substring) avoids "Rist"
    # matching inside "Christof"; multi-token surnames ("Franco Olias") still match;
    # and requiring the FIRST name disambiguates two same-surname candidates so the
    # winner's attributes are not copied onto a same-surname loser (e.g. winner
    # "<X> Seithel" must not graft onto the losing "Jana Seithel").
    norm_tok <- function(x) tolower(gsub("\\s+", " ", trimws(gsub("[-,.]", " ", x))))
    tok_match <- function(sl, sf, cn) {
      slt <- strsplit(sl, " "); cnt <- strsplit(cn, " ")
      mapply(function(a, fst, b) length(a) > 0 && all(a %in% b) &&
                                 (is.na(fst) || fst == "" || fst %in% b),
             slt, sf, cnt)
    }
    stala_reg <- bw_stala %>%
      filter(!is.na(candidate_gender) | !is.na(candidate_birth_year)) %>%
      transmute(ags, election_date,
                .sl = norm_tok(candidate_last_name),
                .sf = sub(" .*", "", norm_tok(candidate_first_name)),
                .graft_gender = candidate_gender, .graft_birth = candidate_birth_year) %>%
      filter(.sl != "" & !is.na(.sl)) %>%
      distinct(ags, election_date, .sl, .sf, .keep_all = TRUE)
    graft <- bk %>%
      transmute(ags, election_date, candidate_name, .cn = norm_tok(candidate_name)) %>%
      inner_join(stala_reg, by = c("ags", "election_date"),
                 relationship = "many-to-many") %>%
      filter(tok_match(.sl, .sf, .cn)) %>%
      distinct(ags, election_date, candidate_name, .keep_all = TRUE) %>%
      select(ags, election_date, candidate_name, .graft_gender, .graft_birth)
    bk <- bk %>%
      left_join(graft, by = c("ags", "election_date", "candidate_name")) %>%
      mutate(
        candidate_gender = .graft_gender,           # NA where unmatched -> 04 predicts
        candidate_birth_year = .graft_birth,
        candidate_party = NA_character_,
        candidate_profession = NA_character_,
        office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                             "Oberbürgermeister*in", "Bürgermeister*in")
      ) %>%
      select(
        ags, ags_name, state, state_name, election_year, election_date,
        election_type, round, eligible_voters, number_voters, valid_votes,
        invalid_votes, turnout, candidate_name, candidate_last_name,
        candidate_first_name, candidate_gender, candidate_party,
        candidate_votes, candidate_voteshare, candidate_birth_year,
        candidate_profession, office_type, n_candidates, candidate_rank, is_winner
      )
    # Drop StaLA rows for cycles superseded by a COMPLETE Komm.ONE cycle.
    bw_stala_keep <- bw_stala %>% anti_join(komm_complete, by = c("ags", "election_year"))
    bw_candidates <- bind_rows(bk, bw_stala_keep)
    cat(sprintf("  Komm.ONE: %d candidate rows, %d complete cycles used (%d incomplete -> StaLA fallback); ",
                nrow(bk), nrow(komm_complete), n_komm_cycles - nrow(komm_complete)))
    cat(sprintf("StaLA winner-only kept for %d elections\n",
                nrow(distinct(bw_stala_keep, ags, election_date))))
    cat(sprintf("  register gender grafted onto %d Komm.ONE candidate rows\n",
                sum(!is.na(bk$candidate_gender))))
  } else {
    bw_candidates <- bw_stala
    cat("  Note: Komm.ONE file not found; using StaLA winner-only data only.\n")
  }

  bw_clean <- standardise_candidates(bw_candidates)

  cat("Baden-Württemberg: Processed", nrow(bw_clean), "candidate rows across",
      length(unique(bw_clean$election_year)), "years\n")
  all_candidate_data[["baden_wuerttemberg"]] <- bw_clean
} else {
  cat("Note: BW parsed data not found at", bw_file, "\n")
  cat("  Run 00_bw_parse.py first to generate the data.\n")
}

# ============================================================================
# BRANDENBURG
# ============================================================================
# Candidate-level data for BB Bürgermeister-/Oberbürgermeisterwahlen of the
# amtsfreie Gemeinden/Städte + 4 kreisfreie Städte, scraped from the
# Landeswahlleiter portal by 00_bb_scrape.py into bb_bm_parsed.csv. The
# intermediate is already candidate-level long (one row per candidate per round,
# with full names + party); we add rank / n_candidates / is_winner like MV.
# Single-candidate rounds are Ja/Nein votes (candidate_votes = Ja-Stimmen).

cat("\n=== Processing Brandenburg mayoral elections ===\n")

bb_file <- "data/mayoral_elections/raw/brandenburg/bb_bm_parsed.csv"

if (file.exists(bb_file)) {
  bb_raw <- fread(bb_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name",
                    "candidate_first_name", "round")))

  bb_candidates <- bb_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout)
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                           "Oberbürgermeister*in", "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  bb_clean <- standardise_candidates(bb_candidates)

  cat("Brandenburg: Processed", nrow(bb_clean), "candidate rows across",
      length(unique(bb_clean$election_year)), "years\n")
  all_candidate_data[["brandenburg"]] <- bb_clean
} else {
  cat("Note: BB parsed data not found at", bb_file, "\n")
  cat("  Run 00_bb_scrape.py first to generate the data.\n")
}

# ============================================================================
# SACHSEN-ANHALT
# ============================================================================
# HYBRID: StaLA primary (00_st_stala_parse.py → st_stala_parsed.csv, 218
# elections 2019-2026, all candidates, both rounds) + Landeswahlleiter portal
# supplements (00_st_scrape.py → st_bm_parsed.csv) for the 5 (ags, date)
# pairs post-2026-02-15 not yet in the StaLA extract, plus the 1 candidate
# (the 8th candidate, Genthin 2024) missing from StaLA due to a corrupted B08/B09
# record in bmbm.csv row 118.
#
# The merge rule mirrors 01_mayoral_unharm.R exactly (see there for the
# rationale). Winner identification: StaLA emits an `is_winner` flag from the
# row-level Elected record, which supersedes the rank-based inference for
# candidates whose surname carries a Dr. title. For portal-supplement rows
# (no is_winner column), we fall back to rank(-votes)=1.

cat("\n=== Processing Sachsen-Anhalt mayoral elections ===\n")

st_stala_file  <- "data/mayoral_elections/raw/sachsen_anhalt/st_stala_parsed.csv"
st_portal_file <- "data/mayoral_elections/raw/sachsen_anhalt/st_bm_parsed.csv"

st_col_classes <- list(character = c("ags", "ags_name", "state",
    "state_name", "election_date", "candidate_party",
    "candidate_name", "candidate_last_name", "candidate_first_name",
    "round"))

if (file.exists(st_stala_file)) {
  st_stala <- fread(st_stala_file, encoding = "UTF-8", colClasses = st_col_classes)

  if (file.exists(st_portal_file)) {
    st_portal <- fread(st_portal_file, encoding = "UTF-8", colClasses = st_col_classes)
    if (!"candidate_title" %in% names(st_portal)) st_portal[, candidate_title := NA_character_]
    if (!"is_winner"       %in% names(st_portal)) st_portal[, is_winner       := NA]
  } else {
    st_portal <- st_stala[0]
  }

  stala_pairs <- unique(st_stala[, .(ags, election_date)])
  st_portal[, candidate_votes := as.numeric(candidate_votes)]
  st_stala[, candidate_votes := as.numeric(candidate_votes)]

  portal_extras <- st_portal[!stala_pairs, on = c("ags", "election_date")]

  # Drop portal "extras" that are really a StaLA election under a wrong date
  # (mirrors 01_mayoral_unharm.R). Fingerprint = round + Gültige + the ordered
  # multiset of candidate votes; the same fingerprint for one AGS at two dates
  # means one election. StaLA dates Zerbst/Anhalt 2026 as 2026-02-08, the portal
  # carries the byte-identical result as 2026-04-12.
  round_fp <- function(dt) {
    x <- dt[!is.na(candidate_votes)][order(ags, election_date, round, candidate_votes)]
    if (!nrow(x)) return(x[, .(ags, election_date, round, fp = character(0))])
    x[, .(fp = paste0(round[1], "~", valid_votes[1], "~",
                      paste(candidate_votes, collapse = "|"))),
      by = .(ags, election_date, round)]
  }
  if (nrow(portal_extras)) {
    dup_keys <- round_fp(portal_extras)[round_fp(st_stala), on = c("ags", "fp"),
                                        nomatch = 0][
      election_date != i.election_date, .(ags, election_date, round)]
    if (nrow(dup_keys)) {
      cat("  Dropping ", nrow(dup_keys),
          " portal round(s) duplicating a StaLA round at another date\n", sep = "")
      portal_extras <- portal_extras[!dup_keys, on = c("ags", "election_date", "round")]
    }
  }

  # Robust portal-supplement rule (mirrors 01_mayoral_unharm.R): a portal row
  # is dropped as a dup of StaLA if StaLA has ANY row in the same
  # (ags, election_date, round) whose last_name OR first_name OR votes match
  # (case-insensitive). Catches a Genthin surname typo (same votes) and a
  # Barleben off-by-one scrape (same name); keeps genuinely missing
  # candidates (the Genthin record with 96 votes).
  st_portal[, .row_id := .I]
  portal_shared <- st_portal[stala_pairs, on = c("ags", "election_date"), nomatch = 0]

  claim_set <- function(dt, col, id_col = NULL) {
    vals <- as.character(.subset2(dt, col))
    keep <- !is.na(vals) & nzchar(trimws(vals))
    if (col == "candidate_votes") {
      vals[keep] <- paste0("V:", vals[keep])
    } else {
      vals[keep] <- paste0("N:", tolower(vals[keep]))
    }
    out <- data.table(ags = dt$ags[keep], election_date = dt$election_date[keep],
                      round = dt$round[keep], claim_key = vals[keep])
    if (!is.null(id_col)) out[, (id_col) := dt[[id_col]][keep]]
    out
  }
  stala_claims <- unique(rbindlist(list(
    claim_set(st_stala, "candidate_last_name"),
    claim_set(st_stala, "candidate_first_name"),
    claim_set(st_stala, "candidate_votes")
  )))
  portal_claims <- rbindlist(list(
    claim_set(portal_shared, "candidate_last_name", ".row_id"),
    claim_set(portal_shared, "candidate_first_name", ".row_id"),
    claim_set(portal_shared, "candidate_votes", ".row_id")
  ))

  matched_ids <- unique(portal_claims[stala_claims,
                                      on = c("ags", "election_date", "round", "claim_key"),
                                      nomatch = 0]$.row_id)
  portal_supp <- portal_shared[!.row_id %in% matched_ids]
  st_portal[, .row_id := NULL]
  portal_supp[, .row_id := NULL]

  st_raw <- rbindlist(list(st_stala, portal_extras, portal_supp),
                      use.names = TRUE, fill = TRUE)

  cat("  Sources merged: StaLA=", nrow(st_stala),
      " rows, portal-only elections=", nrow(portal_extras),
      " rows, portal supplements=", nrow(portal_supp), " rows\n", sep = "")

  st_candidates <- st_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout)
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      # Prefer StaLA's explicit winner flag; else rank-based (portal-supp rows).
      is_winner = dplyr::coalesce(as.logical(is_winner), candidate_rank == 1)
    ) %>%
    ungroup() %>%
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                           "Oberbürgermeister*in", "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  st_clean <- standardise_candidates(st_candidates)

  cat("Sachsen-Anhalt: Processed", nrow(st_clean), "candidate rows across",
      length(unique(st_clean$election_year)), "years\n")
  all_candidate_data[["sachsen_anhalt"]] <- st_clean
} else {
  cat("Note: ST StaLA data not found at", st_stala_file, "\n")
  cat("  Run 00_st_stala_parse.py first to generate the data.\n")
}

# ============================================================================
# HESSEN
# ============================================================================
# Candidate-level data for HE Bürgermeister-/Oberbürgermeister-/Landratswahlen,
# parsed from the StaLA "B VII m Direktwahlen" report by 00_he_parse.py into
# he_parsed.csv. The report names the elected winner (party + gender) and the
# first Wahlvorschlag only, so each (ags, round) contributes the winner plus —
# when the winner is not Wahlvorschlag 1 — that first candidate as a loser.
# is_winner is taken EXPLICITLY from the source (the winner's vote count is NA
# when the winner is not Wahlvorschlag 1), so we do NOT recompute it from ranks.

cat("\n=== Processing Hessen mayoral/Landrat elections ===\n")

he_file <- "data/mayoral_elections/raw/hessen/he_parsed.csv"

if (file.exists(he_file)) {
  he_raw <- fread(he_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name",
                    "candidate_first_name", "candidate_gender", "round")))

  he_candidates <- he_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      is_winner = as.logical(is_winner),
      candidate_rank = as.integer(candidate_rank),
      # XLSX (B VII m 2026) lists every Wahlvorschlag -> real count; the 2024-PDF
      # fallback rows (~57 units the XLSX omits) have no n_candidates (NA).
      n_candidates = suppressWarnings(as.integer(n_candidates)),
      candidate_gender = if_else(nzchar(candidate_gender), candidate_gender, NA_character_),
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        election_type == "Landratswahl" ~ "Landrat/Landrätin",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  he_clean <- standardise_candidates(he_candidates)

  cat("Hessen: Processed", nrow(he_clean), "candidate rows across",
      length(unique(he_clean$election_year)), "years\n")
  all_candidate_data[["hessen"]] <- he_clean
} else {
  cat("Note: HE parsed data not found at", he_file, "\n")
  cat("  Run 00_he_parse.py first to generate the data.\n")
}

# ============================================================================
# BAYERN — Kommunalwahl 2026
# ============================================================================
# Candidate-level data for the 8/22 March 2026 Bayern Kommunalwahl, parsed from
# the Landesamt Mandatsträger XLSX by 00_by_kommunalwahl2026_parse.py into
# by2026_parsed.csv (one row per Wahlvorschlag per round; Kennwort = party, with
# the elected person's name/gender/birth year attached to the winning slot).
# is_winner / candidate_rank / n_candidates come from the parser. As in the
# historical Bayern data, only the winner has a name (HW/SW pair by Kennwort).

cat("\n=== Processing Bayern Kommunalwahl 2026 ===\n")

by26_file <- "data/mayoral_elections/raw/bayern/by2026_parsed.csv"

if (file.exists(by26_file)) {
  by26_raw <- fread(by26_file, encoding = "UTF-8",
                    colClasses = list(character = c("ags", "ags_name", "state",
                      "state_name", "election_date", "candidate_party",
                      "candidate_name", "round")))

  by26_candidates <- by26_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      candidate_birth_year = as.numeric(candidate_birth_year),
      turnout = as.numeric(turnout),
      is_winner = as.logical(is_winner),
      candidate_rank = as.integer(candidate_rank),
      n_candidates = as.integer(n_candidates),
      candidate_name = if_else(nzchar(candidate_name), candidate_name, NA_character_),
      # Bayern Mandatsträger names are "Lastname Firstname" (no comma).
      candidate_last_name = if_else(!is.na(candidate_name),
                                    sub("^(\\S+).*$", "\\1", candidate_name), NA_character_),
      candidate_first_name = if_else(!is.na(candidate_name),
                                     trimws(sub("^\\S+\\s*", "", candidate_name)), NA_character_),
      candidate_gender = if_else(nzchar(candidate_gender), candidate_gender, NA_character_),
      candidate_party = if_else(nzchar(candidate_party), candidate_party, NA_character_),
      candidate_profession = NA_character_,
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        election_type == "Landratswahl" ~ "Landrat/Landrätin",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  by26_clean <- standardise_candidates(by26_candidates)

  cat("Bayern 2026: Processed", nrow(by26_clean), "candidate rows\n")
  all_candidate_data[["bayern_2026"]] <- by26_clean
} else {
  cat("Note: BY 2026 parsed data not found at", by26_file, "\n")
  cat("  Run 00_by_kommunalwahl2026_parse.py first to generate the data.\n")
}

# ============================================================================
# HESSEN — Kommunalwahl 2026 (15 March, Stichwahl 29 March / 12 April)
# ============================================================================
# Candidate-level data for the 2026 HE direct elections, scraped from the
# hessenschau result pages by 00_he_kommunalwahl2026_scrape.py into
# he2026_parsed.csv. PERCENTAGE-ONLY (like RLP): candidate_voteshare + turnout
# known, no absolute votes (candidate_votes / valid_votes NA). Candidate name +
# party + rank + n_candidates come from the scraper; gender is assigned later in
# 04_candidate_characteristics.R. is_winner is taken EXPLICITLY (decisive round).
# Both Hauptwahl and Stichwahl rounds are kept (paired wide in the next step).
# No Landrat elections in this source (mayoral only).

cat("\n=== Processing Hessen Kommunalwahl 2026 (%-only) ===\n")

he26_file <- "data/mayoral_elections/raw/hessen/he2026_parsed.csv"

if (file.exists(he26_file)) {
  he26_raw <- fread(he26_file, encoding = "UTF-8",
                    colClasses = list(character = c("ags", "ags_name", "state",
                      "state_name", "election_date", "candidate_party",
                      "candidate_name", "candidate_last_name",
                      "candidate_first_name", "round")))

  he26_candidates <- he26_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      eligible_voters = NA_real_, number_voters = NA_real_,
      valid_votes = NA_real_, invalid_votes = NA_real_,
      candidate_votes = NA_real_,                    # %-only source
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      is_winner = as.logical(is_winner),
      candidate_rank = as.integer(candidate_rank),
      n_candidates = as.integer(n_candidates),
      candidate_gender = NA_character_,              # assigned in 04
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      candidate_party = if_else(nzchar(candidate_party), candidate_party, NA_character_),
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  he26_clean <- standardise_candidates(he26_candidates)

  # The official XLSX (he_parsed, `hessen` block above) now carries 2026 with FULL
  # votes for the Gemeinden it covers. Drop those (ags, election_date) from the
  # hessenschau %-only scrape so each 2026 round comes from a single source; only
  # the rounds the XLSX lacks (the ~16 not yet in the May-2026 report, incl. the
  # Eschwege Stichwahl) remain %-only.
  if (!is.null(all_candidate_data[["hessen"]])) {
    xlsx26 <- all_candidate_data[["hessen"]] %>%
      filter(election_year == 2026) %>% distinct(ags, election_date)
    n_before <- nrow(he26_clean)
    he26_clean <- he26_clean %>% anti_join(xlsx26, by = c("ags", "election_date"))
    cat("Hessen 2026: dropped", n_before - nrow(he26_clean),
        "%-only rows now covered with full votes by the XLSX\n")
  }

  cat("Hessen 2026: Processed", nrow(he26_clean), "candidate rows across",
      n_distinct(he26_clean$ags), "Gemeinden\n")
  all_candidate_data[["hessen_2026"]] <- he26_clean
} else {
  cat("Note: HE 2026 parsed data not found at", he26_file, "\n")
  cat("  Run 00_he_kommunalwahl2026_scrape.py first to generate the data.\n")
}

# ============================================================================
# COMBINE ALL DATA
# ============================================================================

cat("\n=== Combining all candidate data ===\n")

mayoral_candidates <- bind_rows(all_candidate_data) %>%
  arrange(state, ags, election_year, election_date, round, candidate_rank) %>%
  # Ensure consistent column types (standardise_candidates already called per state)
  mutate(
    ags = as.character(ags),
    state = as.character(state),
    election_year = as.numeric(election_year),
    election_date = as.Date(election_date),
    # Only Bayern computes this; default every other state to FALSE (not superseded)
    flag_superseded = dplyr::coalesce(as.logical(flag_superseded), FALSE)
  )

# ============================================================================
# WIDE FORMAT: One row per candidate per election (HW + SW columns)
# ============================================================================

cat("\n=== Restructuring to wide format (HW + SW columns per candidate) ===\n")

hw_rows <- mayoral_candidates %>% filter(round == "hauptwahl")
sw_rows <- mayoral_candidates %>% filter(round == "stichwahl")

cat("  HW candidate rows:", nrow(hw_rows), "\n")
cat("  SW candidate rows:", nrow(sw_rows), "\n")

# --- Step 1: Find HW-SW date pairs ---
# For each (ags, SW date), find the closest preceding HW date within 60 days
sw_election_dates <- sw_rows %>% distinct(ags, sw_date = election_date)
hw_election_dates <- hw_rows %>% distinct(ags, hw_date = election_date)

date_pairs <- sw_election_dates %>%
  inner_join(hw_election_dates, by = "ags", relationship = "many-to-many") %>%
  mutate(gap = as.numeric(sw_date - hw_date)) %>%
  filter(gap > 0, gap < 60) %>%
  group_by(ags, sw_date) %>%
  slice_min(gap, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ags, hw_date, sw_date)

cat("  Election cycles with Stichwahl:", nrow(date_pairs), "\n")

# --- Step 2: Create candidate match key ---
# Use candidate_name where available, candidate_party otherwise
add_match_key <- function(df) {
  df %>%
    mutate(
      match_key = case_when(
        !is.na(candidate_name) ~ candidate_name,
        !is.na(candidate_party) ~ paste0("__party__", candidate_party),
        TRUE ~ paste0("__rank__", candidate_rank)
      )
    )
}

hw_keyed <- add_match_key(hw_rows)
sw_keyed <- add_match_key(sw_rows)

# --- Step 3: Prepare SW columns for joining ---
# Tag each SW row with its matching HW date
sw_to_join <- sw_keyed %>%
  inner_join(date_pairs, by = c("ags", "election_date" = "sw_date")) %>%
  transmute(
    ags, hw_date, match_key,
    election_date_sw = election_date,
    candidate_votes_sw = candidate_votes,
    candidate_voteshare_sw = candidate_voteshare,
    candidate_rank_sw = candidate_rank,
    is_winner_sw = is_winner,
    n_candidates_sw = n_candidates,
    turnout_sw = turnout,
    flag_superseded_sw = flag_superseded   # so an annulled Stichwahl flags the cycle
  ) %>%
  # Deduplicate in case of multiple matches (same key in same election)
  distinct(ags, hw_date, match_key, .keep_all = TRUE)

# --- Step 4: Build wide dataset ---
# Base = HW candidates, with SW columns joined where available
wide <- hw_keyed %>%
  # Add election_date_sw for elections that have a Stichwahl
  left_join(
    date_pairs %>% select(ags, hw_date, election_date_sw = sw_date),
    by = c("ags", "election_date" = "hw_date")
  ) %>%
  # Join SW candidate results
  left_join(
    sw_to_join,
    by = c("ags", "election_date" = "hw_date", "match_key")
  ) %>%
  # Resolve election_date_sw (from date_pairs or sw_to_join)
  mutate(
    election_date_sw = coalesce(election_date_sw.x, election_date_sw.y),
    has_stichwahl = !is.na(election_date_sw)
  ) %>%
  select(-election_date_sw.x, -election_date_sw.y) %>%
  # Rename HW-specific columns
  rename(
    candidate_votes_hw = candidate_votes,
    candidate_voteshare_hw = candidate_voteshare,
    candidate_rank_hw = candidate_rank,
    n_candidates_hw = n_candidates
  ) %>%
  # Recompute is_winner: if Stichwahl occurred, winner is SW winner
  mutate(
    is_winner = case_when(
      has_stichwahl & !is.na(is_winner_sw) ~ is_winner_sw,
      TRUE ~ is_winner
    ),
    # A cycle is superseded if EITHER its Hauptwahl (HW side) or its (annulled)
    # Stichwahl is flagged. The wide row is keyed on the HW, so without this an
    # annulled Stichwahl folded onto a valid HW would lose the flag.
    flag_superseded = flag_superseded | dplyr::coalesce(flag_superseded_sw, FALSE)
  ) %>%
  select(-match_key, -is_winner_sw, -round, -flag_superseded_sw)

# --- Step 5: Handle SW-only candidates (appear in SW but not HW) ---
sw_only <- sw_keyed %>%
  inner_join(date_pairs, by = c("ags", "election_date" = "sw_date")) %>%
  anti_join(
    hw_keyed,
    by = c("ags", "hw_date" = "election_date", "match_key")
  )

if (nrow(sw_only) > 0) {
  cat("  SW-only candidates (no matching HW row):", nrow(sw_only), "\n")
  # Add these with HW columns = NA
  sw_only_wide <- sw_only %>%
    mutate(
      election_date_sw = election_date,
      election_date = hw_date,
      has_stichwahl = TRUE,
      candidate_votes_sw = candidate_votes,
      candidate_voteshare_sw = candidate_voteshare,
      candidate_rank_sw = candidate_rank,
      n_candidates_sw = n_candidates,
      turnout_sw = turnout,
      candidate_votes_hw = NA_real_,
      candidate_voteshare_hw = NA_real_,
      candidate_rank_hw = NA_integer_,
      n_candidates_hw = NA_integer_,
      is_winner = is_winner  # SW winner
    ) %>%
    select(-match_key, -round, -hw_date,
           -candidate_votes, -candidate_voteshare,
           -candidate_rank, -n_candidates)
  wide <- bind_rows(wide, sw_only_wide)
} else {
  cat("  No SW-only candidates\n")
}

# --- Step 6: Handle SW rows without matching HW date (orphaned SW) ---
sw_orphaned <- sw_keyed %>%
  anti_join(date_pairs, by = c("ags", "election_date" = "sw_date"))

if (nrow(sw_orphaned) > 0) {
  cat("  WARNING: Orphaned SW rows (no matching HW date):", nrow(sw_orphaned), "\n")
  # These are Stichwahl elections where no first-round data exists.
  # Add them with HW columns = NA, using the SW date as the primary date.
  orphaned_wide <- sw_orphaned %>%
    mutate(
      election_date_sw = election_date,
      has_stichwahl = TRUE,
      candidate_votes_sw = candidate_votes,
      candidate_voteshare_sw = candidate_voteshare,
      candidate_rank_sw = candidate_rank,
      n_candidates_sw = n_candidates,
      turnout_sw = turnout,
      candidate_votes_hw = NA_real_,
      candidate_voteshare_hw = NA_real_,
      candidate_rank_hw = NA_integer_,
      n_candidates_hw = NA_integer_,
      is_winner = is_winner
    ) %>%
    select(-match_key, -round,
           -candidate_votes, -candidate_voteshare,
           -candidate_rank, -n_candidates)
  wide <- bind_rows(wide, orphaned_wide)
} else {
  cat("  No orphaned SW rows\n")
}

mayoral_candidates <- wide %>%
  arrange(state, ags, election_year, election_date, candidate_rank_hw) %>%
  # Final column order
  select(
    ags, ags_name, state, state_name, election_year, election_date,
    election_date_sw, election_type, has_stichwahl,
    eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
    turnout_sw,
    candidate_name, candidate_last_name, candidate_first_name,
    candidate_gender, candidate_party,
    candidate_votes_hw, candidate_voteshare_hw, candidate_rank_hw,
    n_candidates_hw,
    candidate_votes_sw, candidate_voteshare_sw, candidate_rank_sw,
    n_candidates_sw,
    is_winner, flag_superseded,
    candidate_birth_year, candidate_profession, office_type
  )

# Enforce exactly ONE is_winner per election. The HW<->SW candidate matching is
# unreliable for name-redacted candidates that share a party label (e.g. multiple
# "Einzelbewerber"/"Parteilos"), which previously produced several is_winner=TRUE
# per election. Mark the single winner = the candidate with the most votes in the
# DECIDING round (Stichwahl if the election had one, else Hauptwahl); vote-share
# fallback for percentage-only data (RLP). Ties -> first row.
mayoral_candidates <- as_tibble(mayoral_candidates) %>%
  mutate(.rid = dplyr::row_number()) %>%                       # global unique row id
  # group by election_type too: an AGS+date may host BOTH a Bürgermeister and a
  # Landrat election (combined election day) — they are separate elections.
  group_by(ags, election_date, election_type) %>%
  mutate(
    .any_sw = any(!is.na(candidate_votes_sw)) | any(!is.na(candidate_voteshare_sw)),
    .metric = dplyr::coalesce(
      ifelse(.any_sw, candidate_votes_sw, candidate_votes_hw),
      ifelse(.any_sw, candidate_voteshare_sw, candidate_voteshare_hw)
    ),
    .win_rid = .rid[which.max(ifelse(is.na(.metric), -Inf, .metric))]  # global id of the group max
  ) %>%
  ungroup() %>%
  mutate(is_winner = .rid == .win_rid) %>%                     # exactly one TRUE per election
  select(-.rid, -.any_sw, -.metric, -.win_rid)

# Safety net: guarantee at least one winner per election (covers rare edge cases
# where the deciding-round metric is degenerate). Flags the highest-vote row.
mayoral_candidates <- mayoral_candidates %>%
  group_by(ags, election_date, election_type) %>%
  mutate(
    .nw = sum(is_winner, na.rm = TRUE),
    .topvotes = dplyr::coalesce(candidate_votes_sw, candidate_votes_hw,
                                candidate_voteshare_sw, candidate_voteshare_hw, -Inf),
    is_winner = ifelse(.nw == 0 & dplyr::row_number() == which.max(.topvotes),
                       TRUE, is_winner)
  ) %>%
  ungroup() %>%
  select(-.nw, -.topvotes)

# flag_superseded is an ELECTION-level attribute, so make it constant within each
# (ags, election_date, election_type): a candidate that appears only in a folded
# Stichwahl (sw_only / orphaned-SW branch above) would otherwise carry the SW row's
# flag instead of the election's. any() also folds in the annulled-Stichwahl flag.
mayoral_candidates <- mayoral_candidates %>%
  group_by(ags, election_date, election_type) %>%
  mutate(flag_superseded = any(flag_superseded, na.rm = TRUE)) %>%
  ungroup()

cat("\n  Wide format: ", nrow(mayoral_candidates), "rows ×",
    ncol(mayoral_candidates), "columns\n")
cat("  flag_superseded TRUE rows:", sum(mayoral_candidates$flag_superseded), "\n")


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== Summary ===\n")
cat("Total candidate rows:", nrow(mayoral_candidates), "\n")
cat("Total elections:", n_distinct(paste(mayoral_candidates$ags,
                                          mayoral_candidates$election_date)), "\n")

cat("\nBy state:\n")
mayoral_candidates %>%
  group_by(state_name) %>%
  summarise(
    candidate_rows = n(),
    elections = n_distinct(paste(ags, election_date)),
    with_stichwahl = sum(has_stichwahl & !is.na(candidate_votes_sw)),
    has_name = sum(!is.na(candidate_name)),
    has_gender = sum(!is.na(candidate_gender)),
    .groups = "drop"
  ) %>%
  print(n = 20)

cat("\nStichwahl coverage:\n")
cat("  Elections with Stichwahl:",
    n_distinct(paste(mayoral_candidates$ags[mayoral_candidates$has_stichwahl],
                     mayoral_candidates$election_date[mayoral_candidates$has_stichwahl])), "\n")
cat("  Candidates with SW data:", sum(!is.na(mayoral_candidates$candidate_votes_sw)), "\n")

cat("\nCandidate name availability:\n")
cat("  With name:", sum(!is.na(mayoral_candidates$candidate_name)), "\n")
cat("  Without name:", sum(is.na(mayoral_candidates$candidate_name)), "\n")

# ============================================================================
# SPLIT MAYORAL VS LANDRAT
# ============================================================================
# Landrat (county/Städteregion head) elections live in their own dataset.

mayoral_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl",
                   "VG-Bürgermeisterwahl", "SG-Bürgermeisterwahl")

# A Landratswahl is a Kreis-level office, so its AGS must end in "000". Rows
# typed Landratswahl on a municipal AGS are source mislabels (Hannover 2013 is
# the Landeshauptstadt's Oberbürgermeisterwahl; Fahrenzhausen 1945 carries a
# stray "Landrat/Landrätin" Amtstitel in the Bayern file). Mirrors the guard in
# 01_mayoral_unharm.R — see the fuller note there.
lr_misfiled <- mayoral_candidates$election_type == "Landratswahl" &
  !grepl("000$", mayoral_candidates$ags)
if (any(lr_misfiled)) {
  muni_office <- mayoral_candidates %>%
    filter(election_type %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl")) %>%
    distinct(ags, election_type) %>%
    group_by(ags) %>% slice(1) %>% ungroup() %>%
    rename(inferred_type = election_type)
  fixed <- mayoral_candidates[lr_misfiled, ] %>%
    left_join(muni_office, by = "ags") %>%
    mutate(election_type = coalesce(inferred_type, "Bürgermeisterwahl")) %>%
    select(-inferred_type)
  cat("\nReclassified", sum(lr_misfiled),
      "Landratswahl candidate row(s) on a municipal AGS\n")
  mayoral_candidates <- bind_rows(mayoral_candidates[!lr_misfiled, ], fixed)
}

# flag_superseded is scoped to the mayoral datasets; the Landrat dataset has its
# own downstream combine pipeline, so drop the column from the Landrat split to
# keep landrat_candidates byte-identical to before.
landrat_candidates <- mayoral_candidates %>% filter(election_type == "Landratswahl") %>%
  select(-flag_superseded)
mayoral_candidates <- mayoral_candidates %>% filter(election_type %in% mayoral_types)

cat("\nDataset split:\n")
cat("  mayoral_candidates:", nrow(mayoral_candidates), "rows\n")
cat("  landrat_candidates:", nrow(landrat_candidates), "rows\n")

# ============================================================================
# ANONYMISATION — Sachsen-Anhalt (state 15)
# ============================================================================
# The Statistisches Landesamt Sachsen-Anhalt supplies its Bürgermeisterwahlen
# data with full candidate names for SCIENTIFIC USE ONLY. Only anonymised data
# may be redistributed or published, "analog zu den bayerischen Daten" — i.e.
# the Bayern model, where the elected person is named and losing candidates
# carry no personal data whatsoever. § 80 KWO LSA independently bars publishing
# Wahlvorschlag (candidate) data more than six months after the final result.
#
# We therefore strip every personal / name-derived field from ST NON-WINNER
# rows. Vote counts, shares, ranks and the Wahlvorschlagsträger (party) are NOT
# personal data and are kept, so the electoral analysis is unaffected. Winners
# are public office-holders and keep their names, exactly as in Bayern.
#
# This must run AFTER the wide HW/SW pivot, which pairs the two rounds by
# candidate name. Downstream, 04_candidate_characteristics.R derives gender and
# name-origin FROM the name, so stripping here also keeps those NA for losers
# (matching Bayern, whose losers are NA throughout).
st_personal_cols <- c("candidate_name", "candidate_last_name",
                      "candidate_first_name", "candidate_title",
                      "candidate_gender", "candidate_birth_year",
                      "candidate_profession")

anonymise_st_losers <- function(df, label) {
  if (!all(c("ags", "is_winner") %in% names(df)) || !nrow(df)) return(df)
  idx <- substr(as.character(df$ags), 1, 2) == "15" & !(df$is_winner %in% TRUE)
  n_before <- sum(idx & !is.na(df$candidate_last_name) &
                    nzchar(trimws(as.character(df$candidate_last_name))))
  for (cl in intersect(st_personal_cols, names(df))) {
    df[[cl]][idx] <- NA
  }
  cat("  ", label, ": anonymised ", sum(idx), " ST non-winner rows (",
      n_before, " carried a name)\n", sep = "")
  df
}

cat("\n=== Anonymising Sachsen-Anhalt losing candidates (StaLA licence) ===\n")
mayoral_candidates <- anonymise_st_losers(mayoral_candidates, "mayoral_candidates")
landrat_candidates <- anonymise_st_losers(landrat_candidates, "landrat_candidates")

# ============================================================================
# SAVE DATA
# ============================================================================

cat("\n=== Saving data ===\n")

# Mayoral output
write_rds(mayoral_candidates, "data/mayoral_elections/final/mayoral_candidates.rds")
fwrite(mayoral_candidates, "data/mayoral_elections/final/mayoral_candidates.csv")

# Landrat output (new standalone dataset)
dir.create("data/landrat_elections/final", recursive = TRUE, showWarnings = FALSE)
write_rds(landrat_candidates, "data/landrat_elections/final/landrat_candidates.rds")
fwrite(landrat_candidates, "data/landrat_elections/final/landrat_candidates.csv")

cat("Data saved to:\n")
cat("  - data/mayoral_elections/final/mayoral_candidates.rds\n")
cat("  - data/mayoral_elections/final/mayoral_candidates.csv\n")
cat("  - data/landrat_elections/final/landrat_candidates.rds\n")
cat("  - data/landrat_elections/final/landrat_candidates.csv\n")

cat("\n=== Done ===\n")
