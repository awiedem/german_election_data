### County Elections (Kreistagswahlen): Municipality-Level Results
# Processes raw county council election data for ST, TH, BW
# Aggregates ballot-district data to municipality level where available
# Vincent Heddesheimer
# March 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "readxl", "haschaR")
conflict_prefer("filter", "dplyr")

raw_dir <- "data/county_elections/raw/Kreistagswahlen/Kreistagswahlen"


# --- Helper functions -------------------------------------------------------

#' Clean word-wrap hyphens from Excel column headers
clean_header <- function(x) {
  x <- gsub("-\\s*\r?\n\\s*", "", x)   # word-wrap hyphens
  x <- gsub("\r?\n", " ", x)           # remaining newlines
  x <- gsub("\\s+", " ", x)            # collapse multiple spaces
  x <- trimws(x)
  x
}

#' Normalise party names to snake_case (county elections version)
normalise_party_cty <- function(x) {
  x <- tolower(x)
  x <- trimws(x)

  mapping <- c(
    # Major parties
    "cdu"          = "cdu",
    "spd"          = "spd",
    "fdp"          = "fdp",
    "f.d.p."       = "fdp",
    "f.d.p"        = "fdp",
    "fdp/dvp"      = "fdp",
    "csu"          = "csu",

    # Greens
    "grune"        = "gruene",
    "gr\u00fcne"   = "gruene",
    "gr\u00dcne"   = "gruene",
    "b\u00fc90/gr\u00fcne" = "gruene",
    "bu90/grune"   = "gruene",
    "b\u00fc90/ gr\u00fcne" = "gruene",
    "bu90/ grune"   = "gruene",
    "b\u00fcndnis 90/die gr\u00fcnen" = "gruene",
    "die gr\u00fcnen" = "gruene",

    # Left
    "pds"          = "linke_pds",
    "die linke"    = "linke_pds",
    "die linke."   = "linke_pds",
    "pds/die linke." = "linke_pds",
    "pds/ die linke." = "linke_pds",

    # AfD
    "afd"          = "afd",

    # Far right
    "npd"          = "npd",
    "rep"          = "rep",
    "die republikaner" = "rep",
    "dvu"          = "dvu",
    "heimat"       = "heimat",
    "iii. weg"     = "iii_weg",

    # Other parties
    "piraten"      = "piraten",
    "die partei"   = "die_partei",
    "tierschutzpartei" = "tierschutz",
    "die tierschutzpartei" = "tierschutz",
    "tierschutzallianz" = "tierschutzallianz",
    "freie w\u00e4hler" = "freie_waehler",
    "freie waehler" = "freie_waehler",
    "fw"           = "freie_waehler",
    "volt"         = "volt",
    "diebasis"     = "die_basis",
    "die basis"    = "die_basis",
    "oedp"         = "oedp",
    "\u00f6dp"     = "oedp",
    "odp"          = "oedp",
    "oedp/familie" = "oedp",
    "\u00d6dp"     = "oedp",
    "piraten-\u00f6dp" = "piraten_oedp",
    "piraten-odp"  = "piraten_oedp",
    "familie"      = "familie",
    "dsu"          = "dsu",
    "dp"           = "dp",
    "dkp"          = "dkp",
    "mlpd"         = "mlpd",
    "kpd"          = "kpd",
    "graue"        = "graue",
    "zentrum"       = "zentrum",
    "forum"         = "forum",
    "neues forum"   = "neues_forum",
    "pbc"           = "pbc",
    "statt partei"  = "statt_partei",
    "statt"         = "statt_partei",
    "future!"       = "future",
    "md-p"          = "md_p",
    "de"            = "de",
    "wasg"          = "wasg",
    "offensive d"   = "offensive_d",
    "spasspartei"   = "spasspartei",
    "mg"            = "mg",
    "fbm"           = "fbm",
    "gartenpartei"  = "gartenpartei",
    "lkr"           = "lkr",
    "nein!"         = "nein",
    "dlvh"          = "dlvh",
    "pro deutschland" = "pro_deutschland",
    "mehrwertstadt"   = "mehrwertstadt",

    # Wählergruppen and Einzelbewerber
    "wahlergruppen [summe]"         = "waehlergruppen",
    "w\u00e4hlergruppen [summe]"    = "waehlergruppen",
    "wg (summe)"                    = "waehlergruppen",
    "einzelbewerber/-innen [summe]" = "einzelbewerber",
    "einzelbewerber/innen [summe]"  = "einzelbewerber",
    "einzelbewerber [summe]"        = "einzelbewerber",
    "eb (summe)"                    = "einzelbewerber",

    # BW-specific
    "wahlervereinigungen"         = "waehlervereinigungen",
    "w\u00e4hlervereinigungen"    = "waehlervereinigungen",
    "gemeinsame wahlvorschlage"   = "gemeinsame_wv",
    "gemeinsame wahlvorschl\u00e4ge" = "gemeinsame_wv",
    "gemeinsame"                  = "gemeinsame_wv",
    "fwv"                         = "fwv",
    "wv"                          = "wv",
    "grune listen"                = "gruene_listen",
    "gr\u00fcne listen"           = "gruene_listen",
    "frauenlisten"                = "frauenlisten",
    "einzelbewerber"              = "einzelbewerber",
    "cduuwv"                      = "cdu_uwv",
    "fdpuwv"                      = "fdp_uwv",
    "grueneuwv"                   = "gruene_uwv",
    "gr\u00fcneuwv"               = "gruene_uwv",
    "oedpuwv"                     = "oedp_uwv",
    "\u00f6dpuwv"                 = "oedp_uwv",
    "pdsuwv"                      = "pds_uwv"
  )

  # Try exact match first
  result <- mapping[x]
  if (!is.na(result)) return(unname(result))

  # Fallback: convert to snake_case
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


# =============================================================================
# SACHSEN-ANHALT (ST)
# =============================================================================

cat("\n===== SACHSEN-ANHALT =====\n")

st_dir <- file.path(raw_dir, "Sachsen-Anhalt")

#' Parse a single ST XLSX file
parse_st_xlsx <- function(filepath, year) {
  cat("  Reading ST", year, "...\n")

  # Read raw
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Year-specific header layout
  if (year == 2004) {
    header_row <- 5
    data_start <- 8
  } else {
    header_row <- 4
    data_start <- 7
  }

  headers <- clean_header(unlist(raw[header_row, ]))

  # Identify key column positions via regex
  wahlber_pos <- which(grepl("Wahlberechtigte", headers, ignore.case = TRUE))[1]
  stimmen_pos <- which(grepl("Stimmen$", headers))[1]  # "Gültige Stimmen"
  wg_pos <- which(grepl("hlergruppen", headers))[1]
  eb_pos <- which(grepl("Einzelbew", headers, ignore.case = TRUE))[1]

  # Party columns: between Gültige Stimmen and Wählergruppen
  party_start <- stimmen_pos + 1
  party_end <- wg_pos - 1

  # Get party names from header
  party_raw_names <- headers[party_start:party_end]
  party_norm_names <- sapply(party_raw_names, normalise_party_cty, USE.NAMES = FALSE)

  # Meta columns: ags is always in column 3 (or 4 for 2004 with Wahldatum)
  if (year == 2004) {
    ags_col <- 4
    kreis_col <- 2
    kreis_name_col <- 3
    gemeinde_name_col <- 5
  } else {
    ags_col <- 3
    kreis_col <- 1
    kreis_name_col <- 2
    gemeinde_name_col <- 4
  }

  # Build column map: name -> column position
  col_map <- c(
    kreisschluessel = kreis_col,
    kreis_name = kreis_name_col,
    ags = ags_col,
    gemeinde_name = gemeinde_name_col,
    wahlberechtigte = wahlber_pos,
    waehler = wahlber_pos + 1,
    ungueltige_stimmzettel = wahlber_pos + 2,
    gueltige_stimmzettel = wahlber_pos + 3,
    gueltige_stimmen = stimmen_pos
  )
  # Add party positions
  party_positions <- setNames(party_start:(party_start + length(party_norm_names) - 1), party_norm_names)
  col_map <- c(col_map, party_positions, waehlergruppen = wg_pos, einzelbewerber = eb_pos)

  # Assign proper column names to raw data and select only needed columns
  all_col_names <- names(col_map)
  raw_selected <- raw[data_start:nrow(raw), col_map]
  names(raw_selected) <- all_col_names

  # Convert to data.frame to avoid tibble/across metadata issues
  df <- as.data.frame(raw_selected, stringsAsFactors = FALSE)

  # Define column groups
  vote_cols <- c("wahlberechtigte", "waehler", "ungueltige_stimmzettel",
                 "gueltige_stimmzettel", "gueltige_stimmen")
  party_cols <- c(party_norm_names, "waehlergruppen", "einzelbewerber")
  all_numeric <- c(vote_cols, party_cols)

  # Convert x/- to NA and make numeric
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Fill down kreisschluessel and ags
  df <- tidyr::fill(df, kreisschluessel, kreis_name, ags, gemeinde_name, .direction = "down")

  # Filter out empty rows
  df <- df[!is.na(df$wahlberechtigte) | !is.na(df$waehler), ]

  # Track which AGS had all-NA for each party (x = no candidacy, not 0 votes)
  na_tracker <- list()
  for (pc in party_cols) {
    na_tracker[[pc]] <- tapply(df[[pc]], df$ags, function(x) all(is.na(x)))
  }

  # Aggregate Wahlbezirke to Gemeinde level using data.table for reliability
  dt <- as.data.table(df)
  group_cols <- c("ags", "gemeinde_name", "kreisschluessel", "kreis_name")
  df_muni <- dt[, lapply(.SD, sum, na.rm = TRUE), by = group_cols, .SDcols = all_numeric]
  df_muni <- as.data.frame(df_muni)

  # Restore NA for parties with no candidacy (all-x in raw data became 0 after sum)
  for (pc in party_cols) {
    no_cand_ags <- names(na_tracker[[pc]])[na_tracker[[pc]] == TRUE]
    if (length(no_cand_ags) > 0) {
      df_muni[[pc]][df_muni$ags %in% no_cand_ags] <- NA_real_
    }
  }

  # Add metadata and rename
  df_muni$election_year <- as.integer(year)
  df_muni$state <- "15"
  df_muni$county <- substr(df_muni$ags, 1, 5)
  names(df_muni)[names(df_muni) == "gemeinde_name"] <- "ags_name"
  names(df_muni)[names(df_muni) == "wahlberechtigte"] <- "eligible_voters"
  names(df_muni)[names(df_muni) == "waehler"] <- "number_voters"
  names(df_muni)[names(df_muni) == "ungueltige_stimmzettel"] <- "invalid_votes"
  names(df_muni)[names(df_muni) == "gueltige_stimmzettel"] <- "valid_votes"

  # Compute vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_cols) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }
  df_muni$gueltige_stimmen <- NULL

  cat("    ->", nrow(df_muni), "municipalities,",
      sum(!is.na(df_muni$turnout)), "with turnout\n")

  as_tibble(df_muni)
}

# Parse 2024 CSV separately
parse_st_csv_2024 <- function(filepath) {
  cat("  Reading ST 2024 (CSV)...\n")

  df <- fread(filepath, encoding = "UTF-8")

  # Rename meta columns (CSV uses ";" separator, column names have " - " pattern)
  meta_rename <- c(
    "Kreisschl\u00fcssel" = "kreisschluessel",
    "Kreisfreie Stadt/Landkreis" = "kreis_name",
    "Gemeindeschl\u00fcssel" = "ags",
    "Gemeindename" = "ags_name",
    "A - Wahlberechtigte" = "eligible_voters",
    "B - W\u00e4hler/-innen" = "number_voters",
    "C1 - Ung\u00fcltige Stimmzettel" = "invalid_votes",
    "C2 - G\u00fcltige Stimmzettel" = "valid_votes",
    "D - G\u00fcltige Stimmen" = "gueltige_stimmen"
  )
  for (old_name in names(meta_rename)) {
    idx <- which(names(df) == old_name)
    if (length(idx) > 0) names(df)[idx[1]] <- meta_rename[old_name]
  }

  # Party columns: D01-D15
  party_cols <- c()
  for (i in seq_along(names(df))) {
    col <- names(df)[i]
    if (grepl("^D\\d{2} - ", col)) {
      party_name <- sub("^D\\d{2} - ", "", col)
      norm_name <- normalise_party_cty(party_name)
      names(df)[i] <- norm_name
      party_cols <- c(party_cols, norm_name)
    }
  }

  # WG and EB columns
  wg_idx <- which(grepl("^WG", names(df)))
  eb_idx <- which(grepl("^EB", names(df)))
  if (length(wg_idx) > 0) { names(df)[wg_idx[1]] <- "waehlergruppen"; party_cols <- c(party_cols, "waehlergruppen") }
  if (length(eb_idx) > 0) { names(df)[eb_idx[1]] <- "einzelbewerber"; party_cols <- c(party_cols, "einzelbewerber") }

  # Pad AGS
  df$ags <- pad_zero_conditional(df$ags, 7)
  df$kreisschluessel <- pad_zero_conditional(df$kreisschluessel, 4)

  # Ensure numeric (spaces in CSV = 0)
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "valid_votes", "gueltige_stimmen", party_cols)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v <- trimws(v)
    v[v == ""] <- "0"
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Aggregate to municipality using data.table
  group_cols <- c("ags", "ags_name", "kreisschluessel", "kreis_name")
  df_muni <- as.data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                                 by = group_cols, .SDcols = all_numeric]
  df_muni <- as.data.frame(df_muni)

  # Add metadata
  df_muni$election_year <- 2024L
  df_muni$state <- "15"
  df_muni$county <- substr(df_muni$ags, 1, 5)

  # Vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_cols) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }
  df_muni$gueltige_stimmen <- NULL

  cat("    ->", nrow(df_muni), "municipalities\n")
  as_tibble(df_muni)
}


# Process all ST years
st_files <- list(
  list(year = 1994, file = "Sachsen-Anhalt_1994_Kreistagswahl.xlsx"),
  list(year = 1999, file = "Sachsen-Anhalt_1999_Kreistagswahl.xlsx"),
  list(year = 2004, file = "Sachsen-Anhalt_2004_Kreistagswahl.xlsx"),
  list(year = 2007, file = "Sachsen-Anhalt_2007_Kreistagswahl.xlsx"),
  list(year = 2009, file = "Sachsen-Anhalt_2009_Kreistagswahl.xlsx"),
  list(year = 2014, file = "Sachsen-Anhalt_2014_Kreistagswahl.xlsx"),
  list(year = 2019, file = "Sachsen-Anhalt_2019_Kreistagswahl.xlsx")
)

st_results <- list()
for (f in st_files) {
  st_results[[as.character(f$year)]] <- parse_st_xlsx(
    file.path(st_dir, f$file), f$year
  )
}
st_results[["2024"]] <- parse_st_csv_2024(
  file.path(st_dir, "Sachsen-Anhalt_2024_Kreistagswahl.csv")
)

df_st <- bind_rows(st_results)

# Pad AGS to 8 digits
df_st <- df_st |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Remove non-municipality rows (county aggregates, VerbGem rows)
# These have eligible_voters = 0 or non-8-digit AGS
n_before <- nrow(df_st)
df_st <- df_st |> filter(nchar(ags) == 8 & eligible_voters > 0)
cat("  Removed", n_before - nrow(df_st), "non-municipality rows\n")

cat("ST total:", nrow(df_st), "rows x", ncol(df_st), "cols\n")
cat("ST years:", paste(sort(unique(df_st$election_year)), collapse = ", "), "\n")
cat("ST municipalities per year:\n")
df_st |> count(election_year) |> print()

# Drop internal columns
df_st <- df_st |> select(-kreisschluessel, -kreis_name)


# =============================================================================
# THÜRINGEN (TH)
# =============================================================================

cat("\n===== THÜRINGEN =====\n")

th_dir <- file.path(raw_dir, "Th++ringen")

#' Parse a single TH XLSX sheet (one Kreis)
#' Format: row 6 has party names; G rows = Gemeinde level
#' 2004: parties sequential, Einheit column; 2009+: parties paired (Anzahl/%)
parse_th_sheet <- function(raw, year) {
  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))
  r7 <- as.character(unlist(raw[7, ]))

  # Detect format: 2004 has "Einheit" in row 6 col 7
  has_einheit <- !is.na(r6[7]) && grepl("Einheit", r6[7], ignore.case = TRUE)

  # Find party columns from row 6
  # Parties start at position 15 (always)
  party_positions <- c()
  party_names <- c()
  for (i in 15:ncol(raw)) {
    name <- r6[i]
    if (!is.na(name) && nchar(trimws(name)) > 0) {
      # For 2009+: only take "Anzahl" columns (row 7)
      if (!has_einheit) {
        unit <- r7[i]
        if (!is.na(unit) && unit == "%") next
      }
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, normalise_party_cty(trimws(name)))
    }
  }

  # Meta column positions
  if (has_einheit) {
    # 2004: extra Einheit column at 7
    einheit_col <- 7
    wahlber_col <- 10
    waehler_col <- 11
    ungueltig_col <- 12
    gueltig_sz_col <- 13
    gueltig_st_col <- 14
  } else {
    einheit_col <- NA
    wahlber_col <- 9
    waehler_col <- 10
    ungueltig_col <- 12
    gueltig_sz_col <- 13
    gueltig_st_col <- 14
  }

  # Convert to matrix and strip names
  mat <- as.matrix(raw)
  colnames(mat) <- NULL

  # Filter to G rows
  satzart <- mat[, 2]
  g_mask <- !is.na(satzart) & satzart == "G"

  # For 2004: also filter to Einheit = "Anzahl"
  if (!is.na(einheit_col)) {
    einheit <- mat[, einheit_col]
    g_mask <- g_mask & !is.na(einheit) & grepl("Anza", einheit)
  }

  g_rows <- which(g_mask)
  if (length(g_rows) == 0) return(NULL)

  g_mat <- mat[g_rows, , drop = FALSE]

  # Build data frame
  df <- data.frame(
    kreis_nr = as.character(g_mat[, 3]),
    gemeinde_nr = as.character(g_mat[, 4]),
    ags_name = as.character(g_mat[, 6]),
    eligible_voters = suppressWarnings(as.numeric(g_mat[, wahlber_col])),
    number_voters = suppressWarnings(as.numeric(g_mat[, waehler_col])),
    invalid_votes = suppressWarnings(as.numeric(g_mat[, ungueltig_col])),
    valid_votes = suppressWarnings(as.numeric(g_mat[, gueltig_sz_col])),
    gueltige_stimmen = suppressWarnings(as.numeric(g_mat[, gueltig_st_col])),
    stringsAsFactors = FALSE
  )

  # Add party columns
  for (j in seq_along(party_names)) {
    v <- g_mat[, party_positions[j]]
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[party_names[j]]] <- suppressWarnings(as.numeric(v))
  }

  # Construct AGS: "16" + pad(kreis_nr, 3) + pad(gemeinde_nr, 3)
  df$kreis_nr <- str_pad(df$kreis_nr, 3, pad = "0")
  df$gemeinde_nr <- str_pad(df$gemeinde_nr, 3, pad = "0")
  df$ags <- paste0("16", df$kreis_nr, df$gemeinde_nr)
  df$county <- paste0("16", df$kreis_nr)

  df
}

#' Parse all sheets in a TH XLSX file
parse_th_xlsx <- function(filepath, year) {
  cat("  Reading TH", year, "...\n")
  sheets <- excel_sheets(filepath)
  # Skip "Hinweise" sheet
  data_sheets <- sheets[grepl("^(Kreis|Wahlkreis)", sheets)]

  all_results <- list()
  for (sh in data_sheets) {
    suppressMessages(
      raw <- read_excel(filepath, sheet = sh, col_names = FALSE, col_types = "text")
    )
    result <- parse_th_sheet(raw, year)
    if (!is.null(result)) {
      all_results[[sh]] <- result
    }
  }

  df <- bind_rows(all_results)

  # Remove internal columns
  df$kreis_nr <- NULL
  df$gemeinde_nr <- NULL

  # Add metadata
  df$election_year <- as.integer(year)
  df$state <- "16"

  # Compute vote shares and turnout
  df$turnout <- ifelse(df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  party_cols <- setdiff(names(df), c("ags", "ags_name", "county", "state",
                                      "election_year", "eligible_voters",
                                      "number_voters", "valid_votes",
                                      "invalid_votes", "gueltige_stimmen", "turnout"))
  for (pc in party_cols) {
    df[[pc]] <- ifelse(df$gueltige_stimmen > 0,
                       df[[pc]] / df$gueltige_stimmen, NA_real_)
  }
  df$gueltige_stimmen <- NULL

  cat("    ->", nrow(df), "municipalities,",
      sum(!is.na(df$turnout)), "with turnout\n")

  as_tibble(df)
}

# Process TH years (skip 1990/1994/1999 .xls for now)
th_files <- list(
  list(year = 2004, file = "Th++ringen_2004_Kreistagswahl.xlsx"),
  list(year = 2009, file = "Th++ringen_2009_Kreistagswahl.xlsx"),
  list(year = 2014, file = "Th++ringen_2014_Kreistagswahl.xlsx"),
  list(year = 2019, file = "Th++ringen_2019_Kreistagswahl.xlsx"),
  list(year = 2021, file = "Th++ringen_2021_Kreistagswahl.xlsx"),
  list(year = 2024, file = "Th++ringen_2024_Kreistagswahl.xlsx")
)

th_results <- list()
for (f in th_files) {
  th_results[[as.character(f$year)]] <- parse_th_xlsx(
    file.path(th_dir, f$file), f$year
  )
}

df_th <- bind_rows(th_results)
df_th <- df_th |> mutate(ags = pad_zero_conditional(ags, 7))

cat("TH total:", nrow(df_th), "rows x", ncol(df_th), "cols\n")
cat("TH years:", paste(sort(unique(df_th$election_year)), collapse = ", "), "\n")
df_th |> count(election_year) |> print()


# =============================================================================
# MECKLENBURG-VORPOMMERN (MV)
# =============================================================================

cat("\n===== MECKLENBURG-VORPOMMERN =====\n")

mv_dir <- file.path(raw_dir, "Mecklenburg-Vorpommern")

#' Parse MV CSV files (2019, 2024)
#' Format: semicolon-delimited, Latin-1, skip 5 header rows
#' Ausgabe "A" = absolute votes, "P" = percentages
parse_mv_csv <- function(filepath, year) {
  cat("  Reading MV", year, "(CSV)...\n")

  df <- fread(filepath, skip = 5, sep = ";", encoding = "Latin-1")

  # Filter to absolute vote rows
  df <- df[Ausgabe == "A"]

  # Rename meta columns
  meta_rename <- c(
    "Gemeinde"         = "ags",
    "Gemeindename"     = "ags_name",
    "Wahlberechtigte"  = "eligible_voters",
    "W\u00e4hler"      = "number_voters",
    "Ung\u00fcltige Stimmen" = "invalid_votes",
    "G\u00fcltige Stimmen"   = "gueltige_stimmen"
  )
  for (old_name in names(meta_rename)) {
    idx <- which(names(df) == old_name)
    if (length(idx) > 0) names(df)[idx[1]] <- meta_rename[old_name]
  }

  # Identify party columns: everything after "Gültige Stimmen" except meta cols
  gueltige_idx <- which(names(df) == "gueltige_stimmen")
  party_cols <- c()
  for (i in (gueltige_idx + 1):ncol(df)) {
    col <- names(df)[i]
    if (col %in% c("Ausgabe", "Berechnungsdatum", "Kreis", "Kreisname",
                    "Amt", "Amtsname", "Wahlbezirke insg.", "Erf. Wahlbezirke",
                    "Wahlbeteiligung", "ags", "ags_name", "eligible_voters",
                    "number_voters", "invalid_votes", "gueltige_stimmen")) next

    # Check for Einzelbewerber
    if (grepl("Einzelbewerber", col, ignore.case = TRUE)) {
      names(df)[i] <- "einzelbewerber"
      party_cols <- c(party_cols, "einzelbewerber")
    } else {
      norm_name <- normalise_party_cty(col)
      names(df)[i] <- norm_name
      party_cols <- c(party_cols, norm_name)
    }
  }

  # Ensure AGS is character and padded
  df$ags <- as.character(df$ags)
  df$ags <- pad_zero_conditional(df$ags, 7)

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "gueltige_stimmen", party_cols)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Track all-NA parties per AGS (no candidacy)
  na_tracker <- list()
  for (pc in party_cols) {
    na_tracker[[pc]] <- tapply(df[[pc]], df$ags, function(x) all(is.na(x)))
  }

  # Aggregate to municipality level (some may have sub-rows)
  group_cols <- c("ags", "ags_name")
  dt <- as.data.table(df)
  df_muni <- dt[, lapply(.SD, sum, na.rm = TRUE), by = group_cols, .SDcols = all_numeric]
  df_muni <- as.data.frame(df_muni)

  # Restore NA for no-candidacy parties
  for (pc in party_cols) {
    no_cand_ags <- names(na_tracker[[pc]])[na_tracker[[pc]] == TRUE]
    if (length(no_cand_ags) > 0) {
      df_muni[[pc]][df_muni$ags %in% no_cand_ags] <- NA_real_
    }
  }

  # Add metadata
  df_muni$election_year <- as.integer(year)
  df_muni$state <- "13"
  df_muni$county <- substr(df_muni$ags, 1, 5)

  # Compute vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_cols) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }

  # Rename gueltige_stimmen to valid_votes for consistency
  names(df_muni)[names(df_muni) == "gueltige_stimmen"] <- "valid_votes"

  cat("    ->", nrow(df_muni), "municipalities\n")
  as_tibble(df_muni)
}

#' Parse MV 2014 XLSX
#' Format: "nach Gemeinden" sheet, multi-row header
#' Col 3 = AGS, Col 4 = name, Col 9 = Wahlberechtigte (A),
#' Col 10 = Wähler (B), Col 12 = ungültige Stimmen (D),
#' Col 13 = gültige Stimmen (C), Cols 14+ = party votes (C1+)
parse_mv_xlsx_2014 <- function(filepath) {
  cat("  Reading MV 2014 (XLSX)...\n")

  suppressMessages(
    raw <- read_excel(filepath, sheet = "nach Gemeinden",
                      col_names = FALSE, col_types = "text")
  )

  # Party names from row 6, cols 14 onward
  r6 <- clean_header(unlist(raw[6, ]))
  party_positions <- c()
  party_names <- c()
  for (i in 14:ncol(raw)) {
    name <- r6[i]
    if (!is.na(name) && nchar(trimws(name)) > 0) {
      clean_name <- trimws(name)
      if (grepl("Einzelbewerber", clean_name, ignore.case = TRUE)) {
        norm <- "einzelbewerber"
      } else {
        norm <- normalise_party_cty(clean_name)
      }
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, norm)
    }
  }

  # Build column map
  col_map <- c(
    ags = 3, ags_name = 4,
    eligible_voters = 9, number_voters = 10,
    invalid_votes = 12, gueltige_stimmen = 13
  )
  party_map <- setNames(party_positions, party_names)
  col_map <- c(col_map, party_map)

  # Extract data rows (row 12 onward)
  raw_sel <- raw[12:nrow(raw), col_map]
  names(raw_sel) <- names(col_map)
  df <- as.data.frame(raw_sel, stringsAsFactors = FALSE)

  # Remove rows without AGS
  df <- df[!is.na(df$ags) & nchar(df$ags) > 0, ]

  # Convert numeric columns
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "gueltige_stimmen", party_names)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Add metadata
  df$election_year <- 2014L
  df$state <- "13"
  df$ags <- pad_zero_conditional(df$ags, 7)
  df$county <- substr(df$ags, 1, 5)

  # Compute vote shares and turnout
  df$turnout <- ifelse(df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  for (pc in party_names) {
    df[[pc]] <- ifelse(df$gueltige_stimmen > 0,
                       df[[pc]] / df$gueltige_stimmen, NA_real_)
  }
  names(df)[names(df) == "gueltige_stimmen"] <- "valid_votes"

  cat("    ->", nrow(df), "municipalities\n")
  as_tibble(df)
}

# Process MV years
mv_results <- list()
mv_results[["2014"]] <- parse_mv_xlsx_2014(
  file.path(mv_dir, "Mecklenburg-Vorpommern_2014_Kreistagswahl.xlsx")
)
mv_results[["2019"]] <- parse_mv_csv(
  file.path(mv_dir, "Mecklenburg-Vorpommern_2019_Kreistagswahl.csv"), 2019
)
mv_results[["2024"]] <- parse_mv_csv(
  file.path(mv_dir, "Mecklenburg-Vorpommern_2024_Kreistagswahl.csv"), 2024
)

df_mv <- bind_rows(mv_results)
df_mv <- df_mv |> mutate(ags = pad_zero_conditional(ags, 7))

# Filter to valid municipality rows (8-digit AGS, positive eligible_voters)
n_before <- nrow(df_mv)
df_mv <- df_mv |> filter(nchar(ags) == 8 & eligible_voters > 0)
cat("  Removed", n_before - nrow(df_mv), "non-municipality rows\n")

cat("MV total:", nrow(df_mv), "rows x", ncol(df_mv), "cols\n")
cat("MV years:", paste(sort(unique(df_mv$election_year)), collapse = ", "), "\n")
df_mv |> count(election_year) |> print()


# =============================================================================
# SACHSEN (SN)
# =============================================================================

cat("\n===== SACHSEN =====\n")

sn_dir <- file.path(raw_dir, "Sachsen")

#' Parse SN legacy XLSX files (1999-2014)
#' Single sheet, rows 4-6 = header, row 7 = state total, row 8+ = data
#' Col 1 = AGS (mixed 2/5/7/8-digit), Col 2 = name
#' Col 3 = Wahlberechtigte, Col 4 = Wähler, Col 5 = Ungültige,
#' Col 6 = Gültige Stimmzettel, Col 7 = Gültige Stimmen (Insgesamt),
#' Cols 8+ = party vote counts
parse_sn_legacy <- function(filepath, year) {
  cat("  Reading SN", year, "(legacy XLSX)...\n")

  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Party names: usually row 4 cols 8+, but 2014 has them in row 5
  r4 <- clean_header(unlist(raw[4, ]))
  r5 <- clean_header(unlist(raw[5, ]))
  party_start <- 8
  party_end <- ncol(raw)
  # If row 4 col 8 is NA, use row 5 (2014 format)
  if (is.na(r4[party_start]) || nchar(trimws(r4[party_start])) == 0) {
    party_raw <- r5[party_start:party_end]
  } else {
    party_raw <- r4[party_start:party_end]
  }

  # Last column is typically "Wählervereinigungen..." — normalise it
  party_norm <- c()
  party_positions <- c()
  for (i in seq_along(party_raw)) {
    name <- party_raw[i]
    if (is.na(name) || nchar(trimws(name)) == 0) next
    pos <- party_start + i - 1
    # Handle Wählervereinigungen
    if (grepl("hlervereinigungen", name, ignore.case = TRUE)) {
      party_norm <- c(party_norm, "waehlervereinigungen")
    } else {
      party_norm <- c(party_norm, normalise_party_cty(trimws(name)))
    }
    party_positions <- c(party_positions, pos)
  }

  # Build column map
  col_map <- c(
    ags = 1, ags_name = 2,
    eligible_voters = 3, number_voters = 4,
    invalid_votes = 5, valid_votes = 6,
    gueltige_stimmen = 7
  )
  party_map <- setNames(party_positions, party_norm)
  col_map <- c(col_map, party_map)

  # Extract data rows (skip header rows and state total at row 7+)
  raw_sel <- raw[8:nrow(raw), col_map]
  names(raw_sel) <- names(col_map)
  df <- as.data.frame(raw_sel, stringsAsFactors = FALSE)

  # Filter to 8-digit AGS (municipality level)
  df <- df[!is.na(df$ags) & nchar(df$ags) == 8, ]

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "valid_votes", "gueltige_stimmen", party_norm)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Remove rows with all-NA vote data
  df <- df[!is.na(df$eligible_voters) | !is.na(df$number_voters), ]

  # Add metadata
  df$election_year <- as.integer(year)
  df$state <- "14"
  df$ags <- pad_zero_conditional(df$ags, 7)
  df$county <- substr(df$ags, 1, 5)

  # Compute vote shares and turnout
  df$turnout <- ifelse(df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  for (pc in party_norm) {
    df[[pc]] <- ifelse(df$gueltige_stimmen > 0,
                       df[[pc]] / df$gueltige_stimmen, NA_real_)
  }
  df$gueltige_stimmen <- NULL

  cat("    ->", nrow(df), "municipalities\n")
  as_tibble(df)
}

#' Parse SN modern XLSX files (2019, 2024)
#' GE_TG sheet with row 1 = headers, row 2+ = data
#' Col 9 = Ortnummer (AGS), Col 10 = Ortname
#' Col 14 = Wahlberechtigte, Col 20 = gültige Stimmen
#' Party cols between gültige Stimmen and NA separator column
parse_sn_modern <- function(filepath, year) {
  cat("  Reading SN", year, "(modern XLSX)...\n")

  suppressMessages(
    raw <- read_excel(filepath, sheet = "GE_TG", col_names = FALSE, col_types = "text")
  )

  # Row 1 has column names
  headers <- as.character(unlist(raw[1, ]))

  # Find key columns
  ags_col <- which(headers == "Ortnummer")[1]
  name_col <- which(headers == "Ortname")[1]
  wahlber_col <- which(headers == "Wahlberechtigte")[1]
  waehler_col <- which(grepl("^W.hler$", headers))[1]
  ungueltig_col <- which(grepl("ung.ltige Stimmzettel$", headers))[1]
  gueltig_sz_col <- which(grepl("g.ltige Stimmzettel$", headers))[1]
  gueltig_st_col <- which(grepl("g.ltige Stimmen$", headers))[1]

  # Party columns: after gültige Stimmen up to first NA column
  party_positions <- c()
  party_names <- c()
  for (i in (gueltig_st_col + 1):ncol(raw)) {
    name <- headers[i]
    if (is.na(name)) break  # NA separator = end of vote count cols
    clean_name <- trimws(name)
    if (nchar(clean_name) == 0) break
    # Skip percentage columns
    if (grepl("in %$", clean_name)) break

    if (grepl("hlervereinigungen", clean_name, ignore.case = TRUE)) {
      party_names <- c(party_names, "waehlervereinigungen")
    } else {
      party_names <- c(party_names, normalise_party_cty(clean_name))
    }
    party_positions <- c(party_positions, i)
  }

  # Build column map
  col_map <- c(
    ags = ags_col, ags_name = name_col,
    eligible_voters = wahlber_col, number_voters = waehler_col,
    invalid_votes = ungueltig_col, valid_votes = gueltig_sz_col,
    gueltige_stimmen = gueltig_st_col
  )
  party_map <- setNames(party_positions, party_names)
  col_map <- c(col_map, party_map)

  # Extract data (row 2 onward)
  raw_sel <- raw[2:nrow(raw), col_map]
  names(raw_sel) <- names(col_map)
  df <- as.data.frame(raw_sel, stringsAsFactors = FALSE)

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "valid_votes", "gueltige_stimmen", party_names)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Remove rows with no vote data
  df <- df[!is.na(df$eligible_voters), ]

  # Add metadata
  df$election_year <- as.integer(year)
  df$state <- "14"
  df$ags <- pad_zero_conditional(df$ags, 7)
  df$county <- substr(df$ags, 1, 5)

  # Compute vote shares and turnout
  df$turnout <- ifelse(df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  for (pc in party_names) {
    df[[pc]] <- ifelse(df$gueltige_stimmen > 0,
                       df[[pc]] / df$gueltige_stimmen, NA_real_)
  }
  df$gueltige_stimmen <- NULL

  cat("    ->", nrow(df), "municipalities\n")
  as_tibble(df)
}

# Process SN years
sn_results <- list()

# Legacy format (1999-2014)
sn_legacy_files <- list(
  list(year = 1999, file = "Sachsen_1999_Kreistagswahl.xlsx"),
  list(year = 2004, file = "Sachsen_2004_Kreistagswahl.xlsx"),
  list(year = 2008, file = "Sachsen_2008_Kreistagswahl.xlsx"),
  list(year = 2014, file = "Sachsen_2014_Kreistagswahl.xlsx")
)
for (f in sn_legacy_files) {
  sn_results[[as.character(f$year)]] <- parse_sn_legacy(
    file.path(sn_dir, f$file), f$year
  )
}

# Modern format (2019, 2024)
sn_modern_files <- list(
  list(year = 2019, file = "Sachsen_2019_Kreistagswahl.xlsx"),
  list(year = 2024, file = "Sachsen_2024_Kreistagswahl.xlsx")
)
for (f in sn_modern_files) {
  sn_results[[as.character(f$year)]] <- parse_sn_modern(
    file.path(sn_dir, f$file), f$year
  )
}

df_sn <- bind_rows(sn_results)
df_sn <- df_sn |> mutate(ags = pad_zero_conditional(ags, 7))

# Filter to valid 8-digit AGS with positive eligible voters
n_before <- nrow(df_sn)
df_sn <- df_sn |> filter(nchar(ags) == 8 & eligible_voters > 0)
cat("  Removed", n_before - nrow(df_sn), "non-municipality rows\n")

cat("SN total:", nrow(df_sn), "rows x", ncol(df_sn), "cols\n")
cat("SN years:", paste(sort(unique(df_sn$election_year)), collapse = ", "), "\n")
df_sn |> count(election_year) |> print()


# =============================================================================
# Combine all states and write output
# =============================================================================

cat("\n===== COMBINING =====\n")

df_all <- bind_rows(df_st, df_th, df_mv, df_sn)

# Ensure AGS is padded
df_all <- df_all |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Reorder: meta cols first, then party cols sorted, then flags
meta_cols <- c("ags", "ags_name", "county", "state", "election_year",
               "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
               "turnout")
party_cols_all <- setdiff(names(df_all), c(meta_cols, "waehlergruppen", "einzelbewerber"))

df_all <- df_all |>
  select(all_of(meta_cols), sort(party_cols_all),
         any_of(c("waehlergruppen", "einzelbewerber"))) |>
  arrange(state, election_year, ags)

cat("Final:", nrow(df_all), "rows x", ncol(df_all), "cols\n")
cat("States:", paste(sort(unique(df_all$state)), collapse = ", "), "\n")

glimpse(df_all)

# Write
write_rds(df_all, "data/county_elections/final/county_elec_unharm.rds")
fwrite(df_all, "data/county_elections/final/county_elec_unharm.csv")
cat("Written to data/county_elections/final/\n")


# --- Sanity checks -----------------------------------------------------------

cat("\n===== SANITY CHECKS =====\n")

# Municipalities per state per year
df_all |>
  count(state, election_year) |>
  print(n = 50)

# Turnout range
cat("\nTurnout range:", range(df_all$turnout, na.rm = TRUE), "\n")
bad_turnout <- df_all |> filter(!is.na(turnout) & (turnout > 1 | turnout < 0))
cat("Bad turnout rows:", nrow(bad_turnout), "\n")

# Vote share check: sum of all party cols should be ~1
party_cols_final <- setdiff(names(df_all), meta_cols)
share_sums <- df_all |>
  rowwise() |>
  mutate(share_sum = sum(c_across(all_of(party_cols_final)), na.rm = TRUE)) |>
  ungroup() |>
  pull(share_sum)
cat("\nVote share sum range:", range(share_sums, na.rm = TRUE), "\n")
cat("Rows with sum > 1.05:", sum(share_sums > 1.05, na.rm = TRUE), "\n")
cat("Rows with sum < 0.95:", sum(share_sums < 0.95 & share_sums > 0, na.rm = TRUE), "\n")

# Duplicate check
dupl <- df_all |> count(ags, election_year) |> filter(n > 1)
cat("\nDuplicate (ags, year):", nrow(dupl), "\n")

# Major party shares (weighted by gueltige_stimmen proxy)
cat("\nNational-level party shares by year:\n")
for (yr in sort(unique(df_all$election_year))) {
  d <- df_all |> filter(election_year == yr)
  cat(yr, ": ")
  for (p in c("cdu", "spd", "linke_pds", "afd", "gruene", "fdp")) {
    if (p %in% names(d)) {
      s <- weighted.mean(d[[p]], d$valid_votes, na.rm = TRUE)
      cat(sprintf("%s=%.1f%% ", p, s * 100))
    }
  }
  cat("\n")
}


### END
