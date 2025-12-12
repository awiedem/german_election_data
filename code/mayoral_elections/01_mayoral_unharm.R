### Clean and combine mayoral election data
# Vincent Heddesheimer
# November 2025
#
# Note: Mayoral elections are only available for:
# - Bayern (Bavaria)
# - Niedersachsen (Lower Saxony) 
# - NRW (North Rhine-Westphalia)
# - Saarland
# - Sachsen (Saxony)
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
  here
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
                    number_voters / eligible_voters, NA)
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
    election_type, Wahlart, eligible_voters, number_voters, valid_votes, invalid_votes,
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
# Get all xlsx files and filter for Bürgermeisterwahlen (not Oberbürgermeister)
nrw_all_files <- list.files("data/mayoral_elections/raw/nrw/", 
                             pattern = "\\.xlsx$",
                             full.names = TRUE,
                             ignore.case = TRUE)
# Filter for files containing "Bürgermeisterwahlen" but not "Oberbürgermeister"
nrw_files <- nrw_all_files[grepl("B.*rgermeisterwahlen", basename(nrw_all_files), ignore.case = TRUE) &
                            !grepl("Oberb.*rgermeister", basename(nrw_all_files), ignore.case = TRUE)]

cat("Found", length(nrw_files), "NRW files\n")
if (length(nrw_files) > 0) {
  for (f in nrw_files) cat("  -", basename(f), "\n")
}

nrw_list <- list()

for (file in nrw_files) {
  cat("Processing:", basename(file), "\n")
  
  # Extract year from filename
  year_match <- str_extract(basename(file), "\\d{4}")
  election_year <- as.numeric(year_match)
  
  # Read the file - row 1 is title, row 2 is headers, data starts at row 3
  # So we skip 2 rows, but the first row after skip is still headers, so skip one more
  nrw_raw <- read_excel(file, sheet = 1, skip = 2, col_names = FALSE) %>%
    as.data.table()
  
  # Filter out the header row (first row usually contains "GKZ" or similar)
  nrw_raw <- nrw_raw %>%
    filter(!grepl("GKZ|Gemeinde|Kreis", as.character(.[[1]]), ignore.case = TRUE))
  
  # Manually set column names based on typical structure
  # Usually: col1=GKZ, col2=Gemeinde, col3=Datum, col4=Wähler, col5=Ungültige, col6=Gültige, col7=Bewerber, col8=Name, col9=Wahlvorschlag, col10=Stimmen
  if (ncol(nrw_raw) >= 10) {
    names(nrw_raw)[1:10] <- c("gkz", "gemeinde", "datum", "waehler", "ungueltige", 
                               "gueltige", "bewerber", "name", "wahlvorschlag", "stimmen")
  }
  
  # The structure is: GKZ, Gemeinde, Datum, Wähler, Ungültige, Gültige, Bewerber, Name, Wahlvorschlag, Stimmen, ...
  nrw_clean <- nrw_raw %>%
    # Convert to proper types
    mutate(
      gkz = as.character(gkz),
      gemeinde = as.character(gemeinde),
      datum = as.character(datum),
      waehler = as.numeric(waehler),
      ungueltige = as.numeric(ungueltige),
      gueltige = as.numeric(gueltige),
      name = as.character(name),
      wahlvorschlag = as.character(wahlvorschlag),
      stimmen = as.numeric(stimmen)
    ) %>%
    # Filter out header rows and empty rows
    filter(!is.na(gkz), 
           !grepl("GKZ|Gemeinde|Kreis", gkz, ignore.case = TRUE),
           grepl("^[0-9]+$", gsub("[^0-9]", "", gkz)),  # GKZ should be numeric
           nchar(gsub("[^0-9]", "", gkz)) >= 4)
  
  if (nrow(nrw_clean) > 0) {
    # Calculate default date from filename before mutate
    date_from_title <- str_extract(basename(file), "\\d{2}\\.\\d{2}\\.\\d{4}")
    default_date <- if (!is.na(date_from_title)) {
      as.Date(date_from_title, format = "%d.%m.%Y")
    } else {
      as.Date(paste0(election_year, "-01-01"))
    }
    
    nrw_clean <- nrw_clean %>%
      # Create AGS (8 digits from GKZ)
      # AGS format: state (2) + county+municipality (6) = 8 digits total
      # GKZ is typically 5-6 digits, so we pad it to 6 digits and prepend state code
      mutate(
        gkz_clean = str_pad(gsub("[^0-9]", "", gkz), width = 6, side = "left", pad = "0"),
        ags = paste0("05", gkz_clean),  # NRW state code is 05, then 6 digits for county+municipality
        state = "05",
        state_name = "Nordrhein-Westfalen",
        election_year = election_year,
        election_type = "Bürgermeisterwahl",
        # Parse date - try multiple formats
        election_date = as.Date(ifelse(
          !is.na(datum) & nchar(datum) >= 8,
          # Try parsing as date
          suppressWarnings({
            date_parsed <- as.Date(datum, format = "%d.%m.%Y")
            ifelse(is.na(date_parsed), as.character(default_date), as.character(date_parsed))
          }),
          as.character(default_date)
        ))
      ) %>%
    # Group by municipality and election to aggregate candidate data
    group_by(ags, gemeinde, state, state_name, election_year, election_date, election_type) %>%
    summarise(
      eligible_voters = first(waehler),
      valid_votes = first(gueltige),
      invalid_votes = first(ungueltige),
      number_voters = first(waehler),  # Assuming this is the same as eligible for now
      turnout = if(!is.na(eligible_voters) && eligible_voters > 0) {
        number_voters / eligible_voters
      } else NA,
      # Get winner (candidate with most votes)
      winner_party = wahlvorschlag[which.max(stimmen)],
      winner_votes = max(stimmen, na.rm = TRUE),
      winner_voteshare = if(!is.na(valid_votes) && valid_votes > 0) {
        winner_votes / valid_votes
      } else NA,
      .groups = "drop"
    ) %>%
    mutate(
      ags_name = gemeinde
    ) %>%
    select(ags, ags_name, state, state_name, election_year, election_date, election_type,
           eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
           winner_party, winner_votes, winner_voteshare)
  
    nrw_list[[basename(file)]] <- nrw_clean
  }
}

# Combine NRW data only if we have data
if (length(nrw_list) > 0 && any(sapply(nrw_list, nrow) > 0)) {
  nrw_combined <- bind_rows(nrw_list) %>%
    arrange(ags, election_year, election_date)
} else {
  # Create empty structure if no data
  nrw_combined <- data.frame(
    ags = character(0),
    ags_name = character(0),
    state = character(0),
    state_name = character(0),
    election_year = numeric(0),
    election_date = as.Date(character(0)),
    election_type = character(0),
    eligible_voters = numeric(0),
    number_voters = numeric(0),
    valid_votes = numeric(0),
    invalid_votes = numeric(0),
    turnout = numeric(0),
    winner_party = character(0),
    winner_votes = numeric(0),
    winner_voteshare = numeric(0),
    stringsAsFactors = FALSE
  ) %>%
    as.data.table()
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
    ags = str_pad(as.character(AGS), width = 8, side = "left", pad = "0"),
    state = "10",  # Saarland state code
    state_name = "Saarland",
    election_year = Wahljahr,
    election_type = "Bürgermeisterwahl",
    # Create election date
    election_date = as.Date(paste(Wahljahr, Monat, Tag, sep = "-")),
    ags_name = `Gemeinde/Kreis`,
    info_type = `Wahlberechtigte/Wähler/Gültige/Ungültige/Partei/Einzelbewerber`,
    value = as.numeric(`Absolute Stimmen`)
  ) %>%
  # Filter out rows with missing AGS
  filter(!is.na(ags), nchar(ags) == 8) %>%
  # Group by municipality and election
  group_by(ags, ags_name, state, state_name, election_year, election_date, election_type) %>%
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
         eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
         winner_party, winner_votes, winner_voteshare) %>%
  # Remove rows where we don't have valid vote information
  filter(!is.na(valid_votes) | !is.na(eligible_voters))

cat("Saarland: Processed", nrow(saarland_clean), "elections\n")
all_mayoral_data[["saarland"]] <- saarland_clean

# ============================================================================
# SACHSEN (Saxony)
# ============================================================================

cat("\n=== Processing Sachsen mayoral elections ===\n")
cat("Note: Sachsen data structure is complex (wide format with years as columns)\n")
cat("This requires more detailed parsing - creating placeholder structure for now\n")

sachsen_file <- "data/mayoral_elections/raw/sachsen/Bürgermeisterlatlas2001bis2024.xlsx"

# Read Sachsen data - the structure is complex with years as columns
# The file has a complex wide format that would require extensive parsing
# For now, we'll create a note that this needs additional work

# Try to read and extract basic information
tryCatch({
  sachsen_raw <- read_excel(sachsen_file, sheet = "Bürgermeisteratlas", skip = 5, n_max = 100) %>%
    as.data.table()
  
  # The structure appears to have: year, WG, BZID, EBENE, ORTNR (AGS), ORTNAME, and then candidate data
  # This is a wide format that needs to be reshaped
  # For now, create an empty structure with proper columns
  
  sachsen_clean <- data.frame(
    ags = character(0),
    ags_name = character(0),
    state = character(0),
    state_name = character(0),
    election_year = numeric(0),
    election_date = as.Date(character(0)),
    election_type = character(0),
    eligible_voters = numeric(0),
    number_voters = numeric(0),
    valid_votes = numeric(0),
    invalid_votes = numeric(0),
    turnout = numeric(0),
    winner_party = character(0),
    winner_votes = numeric(0),
    winner_voteshare = numeric(0),
    stringsAsFactors = FALSE
  ) %>%
    as.data.table()
  
  cat("Sachsen: Placeholder structure created (0 elections)\n")
  cat("Note: Sachsen data requires additional parsing due to complex wide format\n")
  all_mayoral_data[["sachsen"]] <- sachsen_clean
  
}, error = function(e) {
  cat("Error reading Sachsen file:", e$message, "\n")
  # Create empty structure
  sachsen_clean <- data.frame(
    ags = character(0),
    ags_name = character(0),
    state = character(0),
    state_name = character(0),
    election_year = numeric(0),
    election_date = as.Date(character(0)),
    election_type = character(0),
    eligible_voters = numeric(0),
    number_voters = numeric(0),
    valid_votes = numeric(0),
    invalid_votes = numeric(0),
    turnout = numeric(0),
    winner_party = character(0),
    winner_votes = numeric(0),
    winner_voteshare = numeric(0),
    stringsAsFactors = FALSE
  ) %>%
    as.data.table()
  all_mayoral_data[["sachsen"]] <- sachsen_clean
})

# ============================================================================
# NIEDERSACHSEN (Lower Saxony)
# ============================================================================

cat("\n=== Processing Niedersachsen mayoral elections ===\n")
cat("Note: Niedersachsen data is in PDF format and requires special processing\n")
cat("Skipping for now - PDF extraction would need additional tools\n")

# Niedersachsen has PDF files which require special handling
# This would typically require PDF extraction libraries like pdftools
# For now, we'll skip this and note it in the output

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
         eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
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

