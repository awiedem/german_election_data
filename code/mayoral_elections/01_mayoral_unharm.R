### Clean and combine mayoral election data
# Vincent Heddesheimer
# November 2025
#
# Note: Mayoral elections are available for:
# - Bayern (Bavaria) — 1945-2024
# - NRW (North Rhine-Westphalia) — various years
# - Saarland — 2019-2025
# - Sachsen (Saxony) — 2001-2024
# - Rheinland-Pfalz — 1994-2024 (percentages only, no absolute counts)
# - Niedersachsen (Lower Saxony) — 2019 pilot (PDF extraction)
#
# Data covers different time periods for each state

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

# Initialize list to store all state data
all_mayoral_data <- list()

# ============================================================================
# BAYERN (Bavaria)
# ============================================================================

cat("\n=== Processing Bayern mayoral elections ===\n")

bayern_file <- "data/mayoral_elections/raw/bayern/20251114_Wahlen_seit_1945.xlsx"

# Read Bayern data
bayern_raw <- read_excel(bayern_file, sheet = "20251114_bewerberRBZ1-7") %>%
  as.data.table()

# Clean Bayern data
bayern_clean <- bayern_raw %>%
  # Filter out rows with missing key information
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) %>%
  mutate(
    # Create AGS (8 digits) - Gemeindeschlüssel is typically 6 digits for cities
    # Bayern state code is 09, so AGS = 09 + Gemeindeschlüssel (padded to 6 digits) + 00
    gemeindeschluessel_char = as.character(Gemeindeschlüssel),
    gemeindeschluessel_padded = str_pad(gemeindeschluessel_char, width = 6, side = "left", pad = "0"),
    ags = paste0("09", gemeindeschluessel_padded),
    # Extract election date
    election_date = as.Date(`Tag der Wahl`),
    election_year = lubridate::year(election_date),
    # Basic election info
    state = "09",  # Bayern state code
    state_name = "Bayern",
    election_type = "Bürgermeisterwahl",
    # Voter information
    eligible_voters = as.numeric(Stimmberechtigte),
    number_voters = as.numeric(Wähler),
    valid_votes = as.numeric(`Gültige Stimmen`),
    invalid_votes = as.numeric(`ungültige Stimmen`),
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                    number_voters / eligible_voters, NA),
    # Round: Wahlart distinguishes "erster Wahlgang" from "Stichwahl"
    round = case_when(
      Wahlart %in% c("Stichwahl", "Stichwahl ungültig", "Losentscheid") ~ "stichwahl",
      TRUE ~ "hauptwahl"  # "erster Wahlgang", "Hauptwahl ungültig", NA
    )
  ) %>%
  # Extract winner information
  mutate(
    winner_party = `Wahlvorschlag Wahlgewinner`,
    winner_votes = as.numeric(`gültige Stimmen Wahlgewinner`),
    winner_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0,
                             winner_votes / valid_votes, NA)
  ) %>%
  # Extract candidate information (up to 14 candidates)
  mutate(
    candidate_2_party = `Wahlvorschlag Bewerber 2`,
    candidate_2_votes = as.numeric(`gültige Stimmen Bewerber 2`),
    candidate_3_party = `Wahlvorschlag Bewerber 3`,
    candidate_3_votes = as.numeric(`gültige Stimmen Bewerber 3`),
    candidate_4_party = `Wahlvorschlag Bewerber 4`,
    candidate_4_votes = as.numeric(`gültige Stimmen Bewerber 4`),
    candidate_5_party = `Wahlvorschlag Bewerber 5`,
    candidate_5_votes = as.numeric(`gültige Stimmen Bewerber 5`)
  ) %>%
  # Select relevant columns
  select(
    ags, ags_name = Gemeindename, state, state_name, election_year, election_date,
    election_type, round, eligible_voters, number_voters, valid_votes, invalid_votes,
    turnout, winner_party, winner_votes, winner_voteshare
  ) %>%
  # Filter out rows with missing AGS
  filter(!is.na(ags), nchar(ags) == 8)

cat("Bayern: Processed", nrow(bayern_clean), "elections\n")
all_mayoral_data[["bayern"]] <- bayern_clean

# ============================================================================
# NRW (North Rhine-Westphalia)
# ============================================================================

cat("\n=== Processing NRW mayoral elections ===\n")

# Find NRW files - handle special characters in filename
nrw_all_files <- list.files("data/mayoral_elections/raw/nrw/",
                             pattern = "\\.xlsx$",
                             full.names = TRUE,
                             ignore.case = TRUE)
# BM files = Bürgermeisterwahlen (kreisangehörige Gemeinden), skip 2 header rows
nrw_bm_files <- nrw_all_files[grepl("B.*rgermeisterwahlen", basename(nrw_all_files), ignore.case = TRUE) &
                                !grepl("Oberb.*rgermeister", basename(nrw_all_files), ignore.case = TRUE)]
# OB files = Oberbürgermeister-Landratswahlen (kreisfreie Städte + Kreise), skip 3 header rows
nrw_ob_files <- nrw_all_files[grepl("Oberb.*rgermeister", basename(nrw_all_files), ignore.case = TRUE)]

cat("Found", length(nrw_bm_files), "NRW BM files,", length(nrw_ob_files), "NRW OB files\n")
for (f in c(nrw_bm_files, nrw_ob_files)) cat("  -", basename(f), "\n")

# Helper function to process a single NRW Excel file (works for both BM and OB)
# Both file types have the same column structure:
# col1=GKZ, col2=Gemeinde, col3=Datum, col4=Wahlberechtigte, col5=Wähler,
# col6=Ungültige, col7=Gültige, then repeating 3-col candidate blocks (Name, Wahlvorschlag, Stimmen)
process_nrw_file <- function(file, skip_rows, default_election_type) {
  election_year <- as.numeric(str_extract(basename(file), "\\d{4}"))

  nrw_raw <- read_excel(file, sheet = 1, skip = skip_rows, col_names = FALSE) %>%
    as.data.table()

  # Filter out any remaining header rows
  nrw_raw <- nrw_raw %>%
    filter(!grepl("GKZ|Gemeinde|Kreis", as.character(.[[1]]), ignore.case = TRUE))

  if (ncol(nrw_raw) < 10) return(NULL)

  # Reshape: each candidate is a 3-column block starting at column 8
  # Fixed columns: 1-7 (GKZ, Gemeinde, Datum, Wahlberechtigte, Wähler, Ungültige, Gültige)
  # Candidate blocks: cols 8-10, 11-13, 14-16, ... (Name, Wahlvorschlag, Stimmen)
  n_cand_cols <- ncol(nrw_raw) - 7
  n_candidates <- n_cand_cols %/% 3

  # Build long-format: one row per municipality-candidate
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
           !is.na(stimmen))

  if (nrow(nrw_clean) == 0) return(NULL)

  # Default date from filename
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
      # Determine election type from GKZ: kreisfreie Städte have GKZ ending in "000"
      # (5-digit county code with 000 municipality), Kreise have non-000 suffix
      election_type = case_when(
        default_election_type == "Bürgermeisterwahl" ~ "Bürgermeisterwahl",
        grepl("000$", gkz_clean) ~ "Oberbürgermeisterwahl",
        TRUE ~ "Landratswahl"
      ),
      # Parse date — datum may be Excel numeric serial or text
      datum_num = suppressWarnings(as.numeric(datum)),
      datum_text = suppressWarnings(as.Date(datum, format = "%d.%m.%Y")),
      election_date = as.Date(case_when(
        !is.na(datum_num) & datum_num > 30000 & datum_num < 60000 ~
          as.character(as.Date(datum_num, origin = "1899-12-30")),
        !is.na(datum_text) ~ as.character(datum_text),
        TRUE ~ as.character(default_date)
      )),
      # Derive election_year from parsed date (not filename) — files may contain
      # rows from multiple years (e.g. 2025 OB file includes 2020 runoff rows)
      election_year = year(election_date)
    ) %>%
    group_by(ags, gemeinde, state, state_name, election_year, election_date, election_type) %>%
    summarise(
      eligible_voters = first(wahlberechtigte),
      valid_votes = first(gueltige),
      invalid_votes = first(ungueltige),
      number_voters = first(waehler),
      turnout = if (!is.na(eligible_voters) && eligible_voters > 0 && !is.na(number_voters)) {
        number_voters / eligible_voters
      } else NA,
      winner_party = wahlvorschlag[which.max(stimmen)],
      winner_votes = max(stimmen, na.rm = TRUE),
      winner_voteshare = if (!is.na(valid_votes) && valid_votes > 0) {
        winner_votes / valid_votes
      } else NA,
      .groups = "drop"
    ) %>%
    mutate(ags_name = gemeinde) %>%
    select(ags, ags_name, state, state_name, election_year, election_date, election_type,
           eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
           winner_party, winner_votes, winner_voteshare)

  return(nrw_clean)
}

nrw_list <- list()

# Process Bürgermeisterwahl files (skip = 2)
for (file in nrw_bm_files) {
  cat("Processing BM:", basename(file), "\n")
  result <- process_nrw_file(file, skip_rows = 2, default_election_type = "Bürgermeisterwahl")
  if (!is.null(result) && nrow(result) > 0) {
    nrw_list[[basename(file)]] <- result
  }
}

# Process Oberbürgermeister-Landratswahl files (skip = 3)
for (file in nrw_ob_files) {
  cat("Processing OB:", basename(file), "\n")
  result <- process_nrw_file(file, skip_rows = 3, default_election_type = "Oberbürgermeisterwahl")
  if (!is.null(result) && nrow(result) > 0) {
    nrw_list[[basename(file)]] <- result
  }
}

# Combine NRW data only if we have data
if (length(nrw_list) > 0 && any(sapply(nrw_list, nrow) > 0)) {
  nrw_combined <- bind_rows(nrw_list) %>%
    # Deduplicate: same election can appear in multiple files (e.g. 2020 runoff
    # rows appear in both the KW 2020 and KW 2025 OB files)
    distinct(ags, election_date, election_type, .keep_all = TRUE) %>%
    # Detect Stichwahl: elections within 60 days for same ags+type form a cycle
    group_by(ags, election_type) %>%
    arrange(election_date) %>%
    mutate(
      date_gap = as.numeric(election_date - lag(election_date)),
      cycle_id = cumsum(is.na(date_gap) | date_gap > 60)
    ) %>%
    group_by(ags, election_type, cycle_id) %>%
    mutate(
      round = ifelse(election_date == min(election_date), "hauptwahl", "stichwahl")
    ) %>%
    ungroup() %>%
    select(-date_gap, -cycle_id) %>%
    arrange(ags, election_year, election_date)
} else {
  nrw_combined <- data.frame(
    ags = character(0), ags_name = character(0), state = character(0),
    state_name = character(0), election_year = numeric(0),
    election_date = as.Date(character(0)), election_type = character(0),
    round = character(0),
    eligible_voters = numeric(0), number_voters = numeric(0),
    valid_votes = numeric(0), invalid_votes = numeric(0), turnout = numeric(0),
    winner_party = character(0), winner_votes = numeric(0),
    winner_voteshare = numeric(0), stringsAsFactors = FALSE
  ) %>% as.data.table()
}

cat("NRW: Processed", nrow(nrw_combined), "elections\n")
all_mayoral_data[["nrw"]] <- nrw_combined

# ============================================================================
# SAARLAND
# ============================================================================

cat("\n=== Processing Saarland mayoral elections ===\n")

saarland_file <- "data/mayoral_elections/raw/saarland/Wahldaten_Bürgermeisterwahlen_2019-2025.xlsx"

saarland_raw <- read_excel(saarland_file, sheet = "Erfassung") %>%
  as.data.table()

# Clean Saarland data
# The data is in long format: each row is either a summary stat or a party/candidate
saarland_clean <- saarland_raw %>%
  mutate(
    # Create AGS (8 digits)
    ags = paste0("10", str_pad(as.character(AGS), width = 6, side = "left", pad = "0")),
    state = "10",  # Saarland state code
    state_name = "Saarland",
    election_year = Wahljahr,
    election_type = "Bürgermeisterwahl",
    # Round: Wahlart...3 distinguishes "Bürgermeisterwahl" from "Stichwahl"
    round = case_when(
      `Wahlart...3` == "Stichwahl" ~ "stichwahl",
      TRUE ~ "hauptwahl"
    ),
    # Create election date
    election_date = as.Date(paste(Wahljahr, Monat, Tag, sep = "-")),
    ags_name = `Gemeinde/Kreis`,
    info_type = `Wahlberechtigte/Wähler/Gültige/Ungültige/Partei/Einzelbewerber`,
    value = as.numeric(`Absolute Stimmen`)
  ) %>%
  # Filter out rows with missing AGS
  filter(!is.na(ags), nchar(ags) == 8) %>%
  # Group by municipality and election
  group_by(ags, ags_name, state, state_name, election_year, election_date, election_type, round) %>%
  summarise(
    # Extract summary statistics
    eligible_voters = value[info_type == "Wahlberechtigte"][1],
    number_voters = value[info_type %in% c("Wähler/-innen", "Wähler")][1],
    invalid_votes = value[info_type == "Ungültige"][1],
    valid_votes = value[info_type == "Gültige"][1],
    # Calculate turnout
    turnout = if(!is.na(eligible_voters) && eligible_voters > 0 && !is.na(number_voters)) {
      number_voters / eligible_voters
    } else NA,
    # Get winner (party/candidate with most votes, excluding summary rows)
    party_votes = list(value[info_type %in% c("CDU", "SPD", "GRÜNE", "FDP", "AfD", "Einzelbewerber")]),
    party_names = list(info_type[info_type %in% c("CDU", "SPD", "GRÜNE", "FDP", "AfD", "Einzelbewerber")]),
    .groups = "drop"
  ) %>%
  # Extract winner information
  rowwise() %>%
  mutate(
    winner_idx = if(length(party_votes) > 0 && !all(is.na(party_votes))) {
      which.max(party_votes)
    } else NA,
    winner_party = if(!is.na(winner_idx) && length(party_names) > 0) {
      party_names[winner_idx]
    } else NA,
    winner_votes = if(!is.na(winner_idx) && length(party_votes) > 0) {
      party_votes[winner_idx]
    } else NA,
    winner_voteshare = if(!is.na(winner_votes) && !is.na(valid_votes) && valid_votes > 0) {
      winner_votes / valid_votes
    } else NA
  ) %>%
  ungroup() %>%
  select(ags, ags_name, state, state_name, election_year, election_date, election_type,
         round, eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
         winner_party, winner_votes, winner_voteshare) %>%
  # Remove rows where we don't have valid vote information
  filter(!is.na(valid_votes) | !is.na(eligible_voters))

cat("Saarland: Processed", nrow(saarland_clean), "elections\n")
all_mayoral_data[["saarland"]] <- saarland_clean

# ============================================================================
# SACHSEN (Saxony)
# ============================================================================

cat("\n=== Processing Sachsen mayoral elections ===\n")

sachsen_file <- "data/mayoral_elections/raw/sachsen/Bürgermeisterlatlas2001bis2024.xlsx"

# Read Sachsen data - wide format with up to 13 candidates per row
# Columns: Jahr, WG, ORTNR (8-digit AGS), ORTNAME, KW_TERMIN (dd.mm.yyyy),
#   Status (EE=final, VE=runoff required), SIEGER (winner with party in parens),
#   Wahlberechtigte, Wähler, gültige/ungültige Stimmen, 1_Stimmen..13_Stimmen
sachsen_raw <- read_excel(sachsen_file, sheet = "Bürgermeisteratlas", col_names = TRUE) %>%
  as.data.table()

sachsen_clean <- sachsen_raw %>%
  filter(!is.na(ORTNR)) %>%
  mutate(
    ags = as.character(ORTNR),  # Already 8-digit with state prefix 14
    ags_name = ORTNAME,
    state = "14",
    state_name = "Sachsen",
    election_year = as.numeric(Jahr),
    election_date = dmy(KW_TERMIN),
    election_type = "Bürgermeisterwahl",
    eligible_voters = as.numeric(Wahlberechtigte),
    number_voters = as.numeric(`Wähler`),
    valid_votes = as.numeric(`gültige Stimmen`),
    invalid_votes = as.numeric(`ungültige Stimmen`),
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                     number_voters / eligible_voters, NA),
    # Parse winner party from SIEGER column: "Name (Party)" -> "Party"
    # For VE rows (runoff required), SIEGER = "2. Wahlgang erforderlich"
    winner_party = case_when(
      Status == "VE" ~ NA_character_,  # First round requiring runoff — no winner yet
      TRUE ~ str_extract(SIEGER, "\\(([^)]+)\\)", group = 1)
    ),
    # Winner votes: candidate 1 is the winner when Status=EE
    # For VE rows, use the candidate with most votes
    winner_votes = as.numeric(`1_Stimmen`),
    winner_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 & !is.na(winner_votes),
                              winner_votes / valid_votes, NA),
    # Keep Status for round detection
    status_raw = Status
  ) %>%
  select(
    ags, ags_name, state, state_name, election_year, election_date,
    election_type, eligible_voters, number_voters, valid_votes, invalid_votes,
    turnout, winner_party, winner_votes, winner_voteshare, status_raw
  )

# Detect round from Status: VE = first round (runoff needed), EE = final result
# Every VE row has a matching EE row 14–28 days later for the same municipality
ve_dates <- sachsen_clean %>%
  filter(status_raw == "VE") %>%
  select(ags, ve_date = election_date)

# For each EE row, check if there's a VE row for same municipality within 7–35 days before
ee_has_ve <- sachsen_clean %>%
  filter(status_raw == "EE") %>%
  select(ags, election_date) %>%
  left_join(ve_dates, by = "ags", relationship = "many-to-many") %>%
  mutate(gap = as.numeric(election_date - ve_date)) %>%
  filter(gap > 0, gap < 60) %>%
  distinct(ags, election_date) %>%
  mutate(is_stichwahl_ee = TRUE)

sachsen_clean <- sachsen_clean %>%
  left_join(ee_has_ve, by = c("ags", "election_date")) %>%
  mutate(
    round = case_when(
      status_raw == "VE" ~ "hauptwahl",
      is_stichwahl_ee == TRUE ~ "stichwahl",
      TRUE ~ "hauptwahl"  # Standalone EE = won outright in first round
    )
  ) %>%
  select(-status_raw, -is_stichwahl_ee)

cat("Sachsen: Processed", nrow(sachsen_clean), "elections\n")
cat("  Years:", paste(range(sachsen_clean$election_year, na.rm = TRUE), collapse = "-"), "\n")
cat("  Status: EE =", sum(sachsen_raw$Status == "EE"), ", VE =", sum(sachsen_raw$Status == "VE"), "\n")
all_mayoral_data[["sachsen"]] <- sachsen_clean

# ============================================================================
# RHEINLAND-PFALZ (Rhineland-Palatinate)
# ============================================================================

cat("\n=== Processing Rheinland-Pfalz mayoral elections ===\n")

rlp_file <- "data/mayoral_elections/raw/rlp/20251219_Heddesheimer_Direktwahlen 1994-2024.xlsx"

# RLP has 4 sheets with different municipality types and code formats.
# All sheets share a multi-row-per-election format: row 1 = election info + first/winner
# candidate, subsequent rows = other candidates (same election, empty Wahltag).
# Data has ONLY percentages (no absolute vote counts).
# Headers are in rows 1-4; data starts at row 5 (skip = 4).

# Helper function to process one RLP sheet
# Column positions vary slightly by sheet type (see mappings below).
process_rlp_sheet <- function(rlp_file, sheet_name, sheet_type) {

  raw <- read_excel(rlp_file, sheet = sheet_name, skip = 4, col_names = FALSE)

  # Column mapping depends on sheet type:
  # Landräte (12 relevant cols): 1=Wahltag, 2=Stichwahltag, 3=Schlüssel(numeric),
  #   4=Name, 5=Partei, 6=Kandidatname, 7=Ergebnis%HW, 8=Ergebnis%SW,
  #   9=Wahlbeteiligung%HW, 10=Wahlbeteiligung%SW
  # OB Krfr.Städte (13 cols): 1=Wahltag, 2=Stichwahltag, 3=Schlüssel(char 8-digit),
  #   4=Stadt, 5=Partei, 6=Kandidatname, 7=Geschlecht, 8=Ergebnis%HW,
  #   9=Ergebnis%SW, 10=Wahlbeteiligung%HW, 11=Wahlbeteiligung%SW
  # VG + Vfr.Gemeinden (14 cols): 1=Wahltag, 2=Stichwahltag, 3=Schlüssel(char),
  #   4=Name, 5=WVT/Partei, 6=Nachname, 7=Vorname, 8=Geschlecht,
  #   9=Ergebnis%HW, 10=Ergebnis%SW, 11=Wahlbeteiligung%HW, 12=Wahlbeteiligung%SW

  if (sheet_type == "landraete") {
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_ergebnis_hw <- 7; col_ergebnis_sw <- 8
    col_beteiligung_hw <- 9; col_beteiligung_sw <- 10
  } else if (sheet_type == "ob") {
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_ergebnis_hw <- 8; col_ergebnis_sw <- 9
    col_beteiligung_hw <- 10; col_beteiligung_sw <- 11
  } else {
    # VG and Vfr.Gemeinden
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_ergebnis_hw <- 9; col_ergebnis_sw <- 10
    col_beteiligung_hw <- 11; col_beteiligung_sw <- 12
  }

  # Remove completely blank rows
  raw <- raw[!apply(raw, 1, function(x) all(is.na(x))), ]

  if (nrow(raw) == 0) return(NULL)

  # Assign a group ID: each row where col_wahltag is non-NA starts a new election group
  raw$group_id <- cumsum(!is.na(raw[[col_wahltag]]))

  # For each group, the first row is the winner (or leading candidate for Hauptwahl)
  results_list <- list()

  for (gid in unique(raw$group_id)) {
    group <- raw[raw$group_id == gid, ]
    first_row <- group[1, ]

    wahltag <- as.Date(first_row[[col_wahltag]])
    stichwahltag <- as.Date(first_row[[col_stichwahltag]])
    schluessel <- as.character(first_row[[col_schluessel]])
    ort_name <- as.character(first_row[[col_name]])

    if (is.na(schluessel)) next

    # First row candidate = winner of Hauptwahl (highest vote share)
    winner_partei_hw <- as.character(first_row[[col_partei]])
    winner_name_hw <- as.character(first_row[[col_kandidat]])
    winner_ergebnis_hw <- as.numeric(first_row[[col_ergebnis_hw]])
    beteiligung_hw <- as.numeric(first_row[[col_beteiligung_hw]])

    # Hauptwahl row
    results_list[[length(results_list) + 1]] <- data.frame(
      wahltag = wahltag,
      schluessel = schluessel,
      ort_name = ort_name,
      winner_party = winner_partei_hw,
      winner_voteshare = winner_ergebnis_hw / 100,  # Convert % to proportion
      turnout = beteiligung_hw / 100,
      is_stichwahl = FALSE,
      stringsAsFactors = FALSE
    )

    # Stichwahl row (if Stichwahl columns have data for first row)
    winner_ergebnis_sw <- as.numeric(first_row[[col_ergebnis_sw]])
    beteiligung_sw <- as.numeric(first_row[[col_beteiligung_sw]])

    if (!is.na(winner_ergebnis_sw) & !is.na(stichwahltag)) {
      results_list[[length(results_list) + 1]] <- data.frame(
        wahltag = stichwahltag,
        schluessel = schluessel,
        ort_name = ort_name,
        winner_party = winner_partei_hw,  # Same winner for Stichwahl
        winner_voteshare = winner_ergebnis_sw / 100,
        turnout = beteiligung_sw / 100,
        is_stichwahl = TRUE,
        stringsAsFactors = FALSE
      )
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
rlp_landraete <- process_rlp_sheet(rlp_file, "Landräte", "landraete")
rlp_ob <- process_rlp_sheet(rlp_file, "Oberbürgermeister Krfr.Städte", "ob")
rlp_vg <- process_rlp_sheet(rlp_file, "Bürgermeister Verbandsgemeinden", "vg")
rlp_vfr <- process_rlp_sheet(rlp_file, "Bürgermeister Vfr.Gemeinden", "vfr")

cat("  Landräte:", nrow(rlp_landraete), "rows\n")
cat("  OB Krfr.Städte:", nrow(rlp_ob), "rows\n")
cat("  VG:", nrow(rlp_vg), "rows\n")
cat("  Vfr.Gemeinden:", nrow(rlp_vfr), "rows\n")

# Map Schlüsselnummer to AGS for each sheet type
# Landräte: 3-digit county code → "07" + pad(code, 3) + "000"
if (!is.null(rlp_landraete)) {
  rlp_landraete <- rlp_landraete %>%
    mutate(
      ags = paste0("07", str_pad(schluessel, width = 3, side = "left", pad = "0"), "000"),
      election_type = "Landratswahl"
    )
}

# OB Krfr.Städte: 8-digit code (e.g., 11100000 = Koblenz) → "07" + substr(1,3) + "000"
if (!is.null(rlp_ob)) {
  rlp_ob <- rlp_ob %>%
    mutate(
      ags = paste0("07", substr(schluessel, 1, 3), "000"),
      election_type = "Oberbürgermeisterwahl"
    )
}

# VG: 5-6 digit VG-level code — not a municipality, store as VG election type
# VG codes (e.g. 13203) are Verbandsgemeinde identifiers, not AGS
# Map to 8-digit pseudo-AGS: "07" + pad left to 6 digits
if (!is.null(rlp_vg)) {
  rlp_vg <- rlp_vg %>%
    mutate(
      schluessel_clean = gsub("\\s", "", schluessel),
      ags = paste0("07", str_pad(schluessel_clean, width = 6, side = "left", pad = "0")),
      election_type = "VG-Bürgermeisterwahl"
    ) %>%
    select(-schluessel_clean)
}

# Vfr.Gemeinden: codes may have spaces (e.g. "231 00 134"), 8-10 chars
# Pattern: first 3 digits = county, last 3 digits = municipality
# Strip spaces first, then: "07" + first 3 digits + last 3 digits
if (!is.null(rlp_vfr)) {
  rlp_vfr <- rlp_vfr %>%
    mutate(
      schluessel_clean = gsub("\\s", "", schluessel),
      ags = paste0("07",
                   substr(schluessel_clean, 1, 3),
                   substr(schluessel_clean, nchar(schluessel_clean) - 2, nchar(schluessel_clean))),
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
    # Map is_stichwahl flag to round column
    round = ifelse(is_stichwahl, "stichwahl", "hauptwahl"),
    # RLP only has percentages, no absolute counts
    eligible_voters = NA_real_,
    number_voters = NA_real_,
    valid_votes = NA_real_,
    invalid_votes = NA_real_,
    winner_votes = NA_real_
  ) %>%
  select(
    ags, ags_name, state, state_name, election_year, election_date,
    election_type, round, eligible_voters, number_voters, valid_votes, invalid_votes,
    turnout, winner_party, winner_votes, winner_voteshare
  )

cat("RLP: Processed", nrow(rlp_combined), "elections total\n")
cat("  Years:", paste(range(rlp_combined$election_year, na.rm = TRUE), collapse = "-"), "\n")
cat("  By type:\n")
print(table(rlp_combined$election_type))
all_mayoral_data[["rlp"]] <- rlp_combined

# ============================================================================
# NIEDERSACHSEN (Lower Saxony) - All years (2006-2025)
# ============================================================================

cat("\n=== Processing Niedersachsen mayoral elections (2006-2025) ===\n")

ns_base <- "data/mayoral_elections/raw/niedersachsen/Direktwahlen in Niedersachsen 2006-2025"

if (requireNamespace("pdftools", quietly = TRUE)) {

  # --------------------------------------------------------------------------
  # Party name mapping for 2006 (full names → abbreviations)
  # --------------------------------------------------------------------------
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
  # Standard page parser (works for 2011, 2014, 2016, 2019, 2021, 2024, 2025
  # and 2006 with german_nums=TRUE)
  # --------------------------------------------------------------------------
  parse_ns_standard_page <- function(page, election_year, election_date,
                                     german_nums = FALSE) {

    # Extract Schlüssel (key identifier)
    schluessel <- str_extract(page, "Schlüssel:\\s*(\\d+)", group = 1)
    if (is.na(schluessel)) return(NULL)

    # Extract municipality name from "in <Name>" line
    # 2006 uses "in der Stadt X" / "in dem Landkreis X"; later years use "in X"
    ort_name <- str_extract(page,
      "in (?:der |dem )?(?:Stadt |Landkreis |SG |Samtgemeinde |Gemeinde |Flecken )?(.+?)\\n\\s*Schlüssel",
      group = 1)
    if (is.na(ort_name)) {
      # Fallback: try simpler pattern
      ort_name <- str_extract(page, "in\\s+(.+?)\\n", group = 1)
    }
    ort_name <- str_trim(ort_name)

    # Determine election type from header
    election_type_raw <- case_when(
      grepl("Landrat/Landrätin|Landrätin/Landrat|Landrätin / des Landrates", page) ~ "Landratswahl",
      grepl("Oberbürgermeister", page) ~ "Oberbürgermeisterwahl",
      grepl("Samtgemeindebürgermeister", page) ~ "SG-Bürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    )

    # Detect if Stichwahl (runoff required)
    is_stichwahl_required <- grepl("Stichwahl erforderlich", page)

    # Parse line-by-line: str_extract with $ fails on multiline PDF text
    page_lines <- str_split(page, "\n")[[1]]

    # Helper: extract last number from the first line matching a pattern
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

    # Extract winner info
    winner_party <- NA_character_

    if (!is_stichwahl_required) {
      if (german_nums) {
        # 2006 format: "kann als gewählt gelten:" on one line,
        # then "Herr/Frau  Name" on the next line,
        # then party name on the line after that
        winner_block <- str_extract(page,
          "kann als gewählt gelten:\\s*\n\\s*(?:Herr|Frau)\\s+(.+?)\\n\\s*(.+?)\\n",
          group = c(1, 2))
        if (!is.na(winner_block[1])) {
          winner_name <- str_trim(winner_block[1])
          party_line <- str_trim(winner_block[2])
          # Map full party name to abbreviation
          if (party_line %in% names(ns_party_map_2006)) {
            winner_party <- unname(ns_party_map_2006[party_line])
          } else {
            # Keep as-is for local parties / Wählergruppen
            winner_party <- party_line
          }
        }
      } else {
        # Post-2006 format: "kann als gewählt gelten:" then "Name, PARTY" on next/same line
        winner_line <- str_extract(page,
          "kann als gewählt gelten:\\s*\n\\s*(.+?)\\n", group = 1)
        if (is.na(winner_line)) {
          winner_line <- str_extract(page,
            "kann als gewählt gelten:\\s*(.+?)\\n", group = 1)
        }
        if (!is.na(winner_line)) {
          winner_line <- str_trim(winner_line)
          winner_party <- str_extract(winner_line, ",\\s*([^,]+)$", group = 1)
          winner_party <- str_trim(winner_party)
        }
      }
    }

    # Extract candidate votes
    candidate_votes <- c()
    for (l in page_lines) {
      if (german_nums) {
        # 2006: "    55.300   58,0" — numbers with dots, percentage with comma
        m <- str_match(l, "^\\s{30,}([\\d.]+)\\s+\\d+[,.]\\d+")
        if (!is.na(m[1])) {
          vote_str <- gsub("\\.", "", m[2])  # Remove thousands separator
          candidate_votes <- c(candidate_votes, as.numeric(vote_str))
        }
      } else {
        # Post-2006: "    93148        89.8"
        m <- str_match(l, "^\\s{30,}(\\d+)\\s+\\d+\\.\\d+")
        if (!is.na(m[1])) {
          candidate_votes <- c(candidate_votes, as.numeric(m[2]))
        }
      }
    }

    winner_votes <- if (length(candidate_votes) > 0 && !is_stichwahl_required) {
      max(candidate_votes, na.rm = TRUE)
    } else {
      NA_real_
    }

    # Map Schlüssel to AGS
    # 3-digit = Landkreise → "03" + pad(code, 3) + "000"
    # 6-digit = Gemeinden → "03" + pad(code, 6)
    ags <- case_when(
      nchar(schluessel) <= 3 ~ paste0("03", str_pad(schluessel, 3, "left", "0"), "000"),
      nchar(schluessel) <= 6 ~ paste0("03", str_pad(schluessel, 6, "left", "0")),
      TRUE ~ paste0("03", schluessel)
    )
    ags <- str_pad(ags, width = 8, side = "right", pad = "0")

    data.frame(
      ags = ags,
      ags_name = ort_name,
      state = "03",
      state_name = "Niedersachsen",
      election_year = election_year,
      election_date = as.Date(election_date),
      election_type = election_type_raw,
      eligible_voters = wahlberechtigte,
      number_voters = waehler,
      valid_votes = gueltige,
      invalid_votes = ungueltige,
      turnout = ifelse(!is.na(wahlberechtigte) & wahlberechtigte > 0 & !is.na(waehler),
                       waehler / wahlberechtigte, NA),
      winner_party = winner_party,
      winner_votes = winner_votes,
      winner_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 & !is.na(winner_votes),
                                winner_votes / gueltige, NA),
      stringsAsFactors = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # 2006 Stichwahl parser (two-column layout: first round + runoff)
  # --------------------------------------------------------------------------
  parse_ns_2006_stichwahl_page <- function(page, election_year, election_date) {

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

    # For Stichwahl two-column layout, the A/B/C/D lines have two values:
    # first-round value (left) and Stichwahl value (right)
    # We want the rightmost (Stichwahl) value
    extract_rightmost_num <- function(lines, pattern) {
      for (l in lines) {
        if (grepl(pattern, l)) {
          l_clean <- gsub("\\.", "", l)  # Remove thousands separators
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

    # Winner info
    winner_party <- NA_character_
    winner_block <- str_extract(page,
      "kann als gewählt gelten:\\s*\n\\s*(?:Herr|Frau)\\s+(.+?)\\n\\s*(.+?)\\n",
      group = c(1, 2))
    if (!is.na(winner_block[1])) {
      party_line <- str_trim(winner_block[2])
      if (party_line %in% names(ns_party_map_2006)) {
        winner_party <- unname(ns_party_map_2006[party_line])
      } else {
        winner_party <- party_line
      }
    }

    # Candidate votes in the Stichwahl column
    # The format has: "Name  Beruf  STICHWAHL_VOTES  STICHWAHL_%"
    # followed by: "Party  FIRSTROUND_VOTES  FIRSTROUND_%"
    # We want the Stichwahl votes from the candidate name lines
    candidate_votes <- c()
    for (l in page_lines) {
      # Match the candidate name line with Stichwahl results
      # Pattern: "1   Name   Beruf   16.212   48,3%"
      m <- str_match(l, "^\\s+\\d+\\s+\\S.*\\s+([\\d.]+)\\s+\\d+[,.]\\d+%?\\s*$")
      if (!is.na(m[1])) {
        vote_str <- gsub("\\.", "", m[2])
        candidate_votes <- c(candidate_votes, as.numeric(vote_str))
      }
    }

    winner_votes <- if (length(candidate_votes) > 0) {
      max(candidate_votes, na.rm = TRUE)
    } else {
      NA_real_
    }

    ags <- case_when(
      nchar(schluessel) <= 3 ~ paste0("03", str_pad(schluessel, 3, "left", "0"), "000"),
      nchar(schluessel) <= 6 ~ paste0("03", str_pad(schluessel, 6, "left", "0")),
      TRUE ~ paste0("03", schluessel)
    )
    ags <- str_pad(ags, width = 8, side = "right", pad = "0")

    data.frame(
      ags = ags,
      ags_name = ort_name,
      state = "03",
      state_name = "Niedersachsen",
      election_year = election_year,
      election_date = as.Date(election_date),
      election_type = election_type_raw,
      eligible_voters = wahlberechtigte,
      number_voters = waehler,
      valid_votes = gueltige,
      invalid_votes = ungueltige,
      turnout = ifelse(!is.na(wahlberechtigte) & wahlberechtigte > 0 & !is.na(waehler),
                       waehler / wahlberechtigte, NA),
      winner_party = winner_party,
      winner_votes = winner_votes,
      winner_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 & !is.na(winner_votes),
                                winner_votes / gueltige, NA),
      stringsAsFactors = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # 2013 tabular parser (one-page summary table, not page-per-election)
  # Uses token-based parsing: extract numeric columns from right side first,
  # then parse text portion (kommune, bezeichnung, party) from what remains.
  # --------------------------------------------------------------------------
  parse_ns_2013_tabular <- function(pdf_file, election_date, is_stichwahl = FALSE) {
    pages <- pdftools::pdf_text(pdf_file)
    results <- list()

    page <- pages[1]
    page_lines <- str_split(page, "\n")[[1]]

    for (l in page_lines) {
      l_trimmed <- str_trim(l)
      if (l_trimmed == "" || !grepl("^\\d+\\s", l_trimmed)) next
      # Skip the "Gesamt:" summary row
      if (grepl("Gesamt:", l_trimmed)) next

      # Extract all numeric/percentage tokens
      all_tokens <- str_extract_all(l_trimmed, "[\\d.,]+%?")[[1]]
      nr <- all_tokens[1]
      n <- length(all_tokens)

      # Detect Stichwahl-required row (main file only): has "Stichwahl" but not "Wahlsieg"
      is_stichwahl_required <- !is_stichwahl &&
        grepl("Stichwahl", l_trimmed) && !grepl("Wahlsieg", l_trimmed)

      if (is_stichwahl_required) {
        # Only 4 numeric tokens after Nr: Wahlber, Wähler, Gültige, Wahlbet%
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

        results[[length(results) + 1]] <- data.frame(
          ags = NA_character_,
          ags_name = kommune,
          state = "03",
          state_name = "Niedersachsen",
          election_year = 2013L,
          election_date = as.Date(election_date),
          election_type = el_type,
          eligible_voters = wahlber,
          number_voters = waehler,
          valid_votes = gueltige,
          invalid_votes = NA_real_,
          turnout = ifelse(!is.na(wahlber) & wahlber > 0,
                           waehler / wahlber, NA),
          winner_party = NA_character_,
          winner_votes = NA_real_,
          winner_voteshare = NA_real_,
          stringsAsFactors = FALSE
        )
        next
      }

      # Winner row: last 6 tokens = Stimmen, %, Wahlber, Wähler, Gültige, Wahlbet%
      stimmen <- as.numeric(gsub("\\.", "", all_tokens[n-5]))
      wahlber <- as.numeric(gsub("\\.", "", all_tokens[n-3]))
      waehler <- as.numeric(gsub("\\.", "", all_tokens[n-2]))
      gueltige <- as.numeric(gsub("\\.", "", all_tokens[n-1]))

      # Get text portion: everything between lfd.Nr and first vote number (stimmen)
      stimmen_str <- all_tokens[n-5]
      stimmen_pos <- regexpr(stimmen_str, l_trimmed, fixed = TRUE)[1]
      text_part <- str_trim(substr(l_trimmed, nchar(nr) + 1, stimmen_pos - 1))

      # For main file: strip "Wahlsieg" keyword
      text_part <- sub("\\s+Wahlsieg\\s+", " ", text_part)

      # Extract party (last word in text portion)
      winner_party_val <- str_extract(text_part, "\\S+$")
      text_rest <- str_trim(sub("\\S+\\s*$", "", text_part))

      # Extract Bezeichnung and Kommune
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
        eligible_voters = wahlber,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = NA_real_,
        turnout = ifelse(!is.na(wahlber) & wahlber > 0,
                         waehler / wahlber, NA),
        winner_party = winner_party_val,
        winner_votes = stimmen,
        winner_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 & !is.na(stimmen),
                                  stimmen / gueltige, NA),
        stringsAsFactors = FALSE
      )
    }

    if (length(results) > 0) bind_rows(results) else NULL
  }

  # --------------------------------------------------------------------------
  # AGS lookup for 2013 (name-based matching)
  # --------------------------------------------------------------------------
  # Keys use "Kommune Bezeichnung" format when disambiguation is needed
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
          row <- parse_ns_standard_page(pages[i], file_year, file_date,
                                        german_nums = german_nums)
          if (!is.null(row)) {
            file_results[[length(file_results) + 1]] <- row
          }
        }

        if (length(file_results) > 0) {
          file_df <- bind_rows(file_results)
          file_df$round <- "hauptwahl"
          cat("    ", fc$path, ":", nrow(file_df), "elections\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser == "stichwahl_2006") {
        pages <- pdftools::pdf_text(pdf_path)
        file_results <- list()

        for (i in seq_along(pages)) {
          row <- parse_ns_2006_stichwahl_page(pages[i], file_year, file_date)
          if (!is.null(row)) {
            file_results[[length(file_results) + 1]] <- row
          }
        }

        if (length(file_results) > 0) {
          file_df <- bind_rows(file_results)
          file_df$round <- "stichwahl"
          cat("    ", fc$path, ":", nrow(file_df), "elections\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser %in% c("tabular_2013", "tabular_2013_sw")) {
        is_sw <- fc$parser == "tabular_2013_sw"
        file_df <- parse_ns_2013_tabular(pdf_path, file_date, is_stichwahl = is_sw)

        if (!is.null(file_df) && nrow(file_df) > 0) {
          file_df$round <- if (is_sw) "stichwahl" else "hauptwahl"
          # Apply AGS lookup: try name first, then name + type suffix for disambiguation
          file_df$ags <- ns_2013_ags_lookup[file_df$ags_name]
          # For rows where simple name didn't match, try "Name Bezeichnung" key
          na_mask <- is.na(file_df$ags)
          if (any(na_mask)) {
            # Build "Name Typ" key using election_type → Bezeichnung mapping
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
          cat("    ", fc$path, ":", nrow(file_df), "elections\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }
      }
    }
  }

  # Combine all NS results
  if (length(ns_all_results) > 0) {
    ns_combined <- bind_rows(ns_all_results)

    # Deduplicate within same round: handles 2021 where two identical PDFs exist
    # Keep both hauptwahl and stichwahl rows (different dates/rounds)
    ns_combined <- ns_combined %>%
      distinct(ags, election_date, round, .keep_all = TRUE)

    cat("Niedersachsen: Processed", nrow(ns_combined), "elections across",
        length(unique(ns_combined$election_year)), "years\n")
    cat("  By year:\n")
    print(table(ns_combined$election_year))
    cat("  By type:\n")
    print(table(ns_combined$election_type))
    all_mayoral_data[["niedersachsen"]] <- ns_combined
  } else {
    cat("WARNING: No Niedersachsen data processed\n")
  }

} else {
  cat("Note: pdftools package not available, skipping Niedersachsen PDF extraction\n")
}

# ============================================================================
# SCHLESWIG-HOLSTEIN
# ============================================================================
# Data is scraped from wahlen-sh.de by 00_sh_scrape.R and saved as
# data/mayoral_elections/raw/sh/sh_mayoral_scraped.rds
# Coverage: 2023–2025, 35 municipalities, 45 elections
# Special cases:
#   - 4 confirmation elections (Ja/Nein votes for a single candidate)
#   - Candidate-level data is aggregated to winner-level for unharm

cat("\n=== Processing Schleswig-Holstein mayoral elections ===\n")

sh_file <- "data/mayoral_elections/raw/sh/sh_mayoral_scraped.rds"

if (file.exists(sh_file)) {
  sh_raw <- readRDS(sh_file)

  sh_clean <- sh_raw %>%
    # For Ja/Nein confirmation elections, keep the "Ja" row as the winner
    # and treat "Nein-Stimmen" rows as irrelevant for the winner-level dataset
    filter(is.na(candidate_party) | candidate_party != "Nein-Stimmen") %>%
    # For each election-round, identify the winner (highest votes)
    group_by(ags, election_date, round) %>%
    arrange(desc(candidate_votes)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      state = "01",
      state_name = "Schleswig-Holstein",
      election_year = year(election_date),
      # For Ja/Nein elections, the party is from the candidate, not "Ja-Stimmen"
      winner_party = ifelse(candidate_party == "Ja-Stimmen", "EB", candidate_party),
      winner_votes = candidate_votes,
      winner_voteshare = candidate_voteshare,
      turnout = ifelse(!is.na(number_voters) & !is.na(eligible_voters) &
                         eligible_voters > 0,
                       number_voters / eligible_voters, NA_real_)
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, winner_party, winner_votes, winner_voteshare
    )

  cat("Schleswig-Holstein: Processed", nrow(sh_clean), "elections across",
      length(unique(sh_clean$election_year)), "years\n")
  cat("  By year:\n")
  print(table(sh_clean$election_year))
  cat("  By type:\n")
  print(table(sh_clean$election_type))
  all_mayoral_data[["schleswig_holstein"]] <- sh_clean
} else {
  cat("Note: SH scraped data not found at", sh_file, "\n")
  cat("  Run 00_sh_scrape.R first to generate the data.\n")
}

# ============================================================================
# COMBINE ALL DATA
# ============================================================================

cat("\n=== Combining all mayoral election data ===\n")

# Combine all states
mayoral_unharm <- bind_rows(all_mayoral_data) %>%
  arrange(state, ags, election_year, election_date) %>%
  # Ensure consistent column types
  mutate(
    ags = as.character(ags),
    state = as.character(state),
    election_year = as.numeric(election_year),
    election_date = as.Date(election_date),
    round = as.character(round),
    eligible_voters = as.numeric(eligible_voters),
    number_voters = as.numeric(number_voters),
    valid_votes = as.numeric(valid_votes),
    invalid_votes = as.numeric(invalid_votes),
    turnout = as.numeric(turnout),
    winner_votes = as.numeric(winner_votes),
    winner_voteshare = as.numeric(winner_voteshare)
  ) %>%
  # Ensure all required columns are present
  select(ags, ags_name, state, state_name, election_year, election_date, election_type,
         round, eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
         winner_party, winner_votes, winner_voteshare)

# Summary
cat("\n=== Summary ===\n")
cat("Total elections:", nrow(mayoral_unharm), "\n")
cat("By state:\n")
print(table(mayoral_unharm$state_name, mayoral_unharm$election_year))

# Check for duplicates (may be due to multiple rounds - first round vs. runoff)
duplicates <- mayoral_unharm %>%
  count(ags, election_year, election_date) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  cat("\nWarning: Found", nrow(duplicates), "duplicate election records\n")
  cat("These may be multiple rounds (first round vs. runoff) - keeping all records\n")
  print(duplicates)
  
  # For now, we keep all records as they may represent different election rounds
  # If needed, we can add a round indicator later
}

# ============================================================================
# SAVE DATA
# ============================================================================

cat("\n=== Saving data ===\n")

# Save as RDS and CSV
write_rds(mayoral_unharm, "data/mayoral_elections/final/mayoral_unharm.rds")
fwrite(mayoral_unharm, "data/mayoral_elections/final/mayoral_unharm.csv")

cat("Data saved to:\n")
cat("  - data/mayoral_elections/final/mayoral_unharm.rds\n")
cat("  - data/mayoral_elections/final/mayoral_unharm.csv\n")

cat("\n=== Done ===\n")

