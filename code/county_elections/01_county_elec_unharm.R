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

    # BB-specific (aggregated lists)
    "cdu und andere"                    = "cdu",
    "gr\u00fcne/b90 und andere"         = "gruene",
    "gr\u00fcne/b 90 und andere"        = "gruene",
    "gr\u00fcne/b90"                    = "gruene",
    "gr\u00fcne/b 90"                   = "gruene",
    "bvb/freie w\u00e4hler und andere"  = "bvb_fw",
    "bvb / freie w\u00e4hler und andere" = "bvb_fw",
    "bvb/50plus"                        = "bvb_fw",
    "bv-bb (zusammenfassung)"           = "bvb_fw",
    "bv/bbs/fb"                         = "bv_bbs_fb",
    "bauern und andere"                 = "bauern",
    "bauern"                            = "bauern",
    "weitere w\u00e4hlergruppen"        = "weitere_wg",
    "weitere listenvereinigungen"       = "weitere_lv",
    "weitere politische vereinigungen"  = "weitere_pv",
    "schill"                            = "schill",
    "b\u00fcrger (zusammenfassung)"     = "buerger",
    "andere"                            = "andere",
    "freie sachsen"                     = "freie_sachsen",
    "bsw"                               = "bsw",
    "lausitzer allianz"                 = "lausitzer_allianz",
    "aufbruch deutscher patrioten"      = "aufbruch_dt_patrioten",

    # BY-specific
    "bp"                          = "bp",
    "bayernpartei"                = "bp",
    "freie w\u00e4hler bayern"    = "freie_waehler",
    "partei freie w\u00e4hler"    = "freie_waehler",
    "die franken"                 = "die_franken",
    "bsp"                         = "bsp",
    "eap"                         = "eap",
    "vsbd"                        = "vsbd",
    "kbw"                         = "kbw",
    "c.b.v."                      = "cbv",
    "cwu"                         = "cwu",
    "aud"                         = "aud",
    "iwp"                         = "iwp",
    "ds"                          = "ds",
    "ld"                          = "ld",
    "du"                          = "du",
    "hp"                          = "hp",
    "bbp"                         = "bbp",
    "bund freier b\u00fcrger - offensive f\u00fcr deutschland, die freiheitlichen" = "bfb",
    "fbu"                         = "fbu",
    "statt partei"                = "statt_partei",
    "asp"                         = "asp",
    "dacg"                        = "dacg",
    "gemeinsame wahlvorschl\u00e4ge" = "gemeinsame_wv",
    "w\u00e4hlergruppen"          = "waehlergruppen",
    "wahlergruppen"               = "waehlergruppen",
    "mut"                         = "mut",
    "v-partei"                    = "v_partei",
    "die freiheit"                = "die_freiheit",

    # SL-specific
    "die linke"                   = "linke_pds",
    "w\u00e4hlergr./listen"       = "waehlergruppen",
    "wahlergr./listen"            = "waehlergruppen",
    "sonstige"                    = "other",

    # HE-specific
    "die linke."                  = "linke_pds",
    "die tier- schutz"            = "tierschutz",
    "die tierschutz"              = "tierschutz",
    "tierschutzpartei"            = "tierschutz",
    "tierschutz- partei"          = "tierschutz",
    "gb/bhe"                      = "gb_bhe",
    "gpd-bhe"                     = "gb_bhe",
    "drp"                         = "drp",
    "dfu"                         = "dfu",
    "dl"                          = "dl",
    "sdo"                         = "sdo",
    "fw u. sdo"                   = "fw_sdo",
    "ldp"                         = "ldp",
    "ndp"                         = "ndp_he",
    "ap"                          = "ap",
    "evd"                         = "evd",
    "srp"                         = "srp",
    "appd"                        = "appd",
    "alfa"                        = "alfa",
    "einheit"                     = "einheit",
    "wg insgesamt"                = "waehlergruppen",
    "wg ins- gesamt"              = "waehlergruppen",
    "wg ins-gesamt"               = "waehlergruppen",
    "wg insg."                    = "waehlergruppen",
    "wg insg"                     = "waehlergruppen",
    "w\u00e4hler- gruppen"        = "waehlergruppen",
    "w\u00e4hler-gruppen"         = "waehlergruppen",
    "natur-gesetz"                = "naturgesetz",
    "dhp"                         = "dhp",
    "cm"                          = "cm",
    "liga"                        = "liga",
    "die blauen"                  = "die_blauen",

    # BW-specific
    "andere parteien"             = "other",
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
    "pdsuwv"                      = "pds_uwv",

    # SH-specific
    "ssw"                         = "ssw",
    "s\u00fcdschleswigscher w\u00e4hlerverband" = "ssw",
    "schill"                      = "schill",
    "die b\u00fcrgerpartei"       = "die_buergerpartei",
    "die b\u00fcrgerpartei e.v."  = "die_buergerpartei",
    "demokraten"                  = "demokraten",
    "verbraucherschutzpartei"     = "verbraucherschutz",
    "naturgesetz"                 = "naturgesetz",
    "flensburg w\u00e4hlen!"      = "flensburg_waehlen",
    "flensburg wahlen!"           = "flensburg_waehlen",
    "forum21"                     = "forum21",
    "ezb"                         = "einzelbewerber"
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
# BRANDENBURG (BB)
# =============================================================================

cat("\n===== BRANDENBURG =====\n")

bb_dir <- file.path(raw_dir, "Brandenburg")

#' Parse BB XLSX files (2003-2019)
#' All share: Stimmart (col 1), AGS (col 2), ballot-district level
#' Column names used for matching (positions vary slightly between years)
parse_bb_xlsx <- function(filepath, year) {
  cat("  Reading BB", year, "...\n")

  # Detect sheet name
  sheets <- excel_sheets(filepath)
  data_sheet <- if ("Ergebnis_1" %in% sheets) "Ergebnis_1" else "Ergebnis"

  suppressMessages(
    raw <- read_excel(filepath, sheet = data_sheet, col_names = FALSE, col_types = "text")
  )

  # Row 1 = headers
  headers <- clean_header(unlist(raw[1, ]))

  # Key column positions by name
  stimmart_col <- which(headers == "Stimmart")[1]
  ags_col <- which(headers == "AGS")[1]
  name_col <- which(headers == "Gemeindename")[1]
  wahlber_col <- which(grepl("Wahlberechtigte insgesamt", headers))[1]
  waehler_col <- which(grepl("^W.hler$", headers))[1]
  ungueltig_col <- which(grepl("Ung.ltige Stimmzettel", headers))[1]
  gueltig_col <- which(grepl("G.ltige Stimmen", headers))[1]

  # Party columns: everything after Gültige Stimmen
  party_positions <- c()
  party_names <- c()
  for (i in (gueltig_col + 1):ncol(raw)) {
    name <- headers[i]
    if (is.na(name) || nchar(trimws(name)) == 0) next

    clean_name <- trimws(name)

    # Skip "in Prozent" columns
    if (grepl("in Prozent", clean_name, ignore.case = TRUE)) next
    if (grepl("^Stimmen nach", clean_name, ignore.case = TRUE)) next

    # Handle EB (Einzelbewerber) — aggregate all individual EBs
    if (grepl("^EB\\b|^Einzelbew", clean_name)) {
      # Check if this is a summary column
      if (grepl("Zusammenfassung|Einzelbewerbende|^Einzelbewerber$", clean_name)) {
        party_positions <- c(party_positions, i)
        party_names <- c(party_names, "einzelbewerber")
      }
      # Skip individual EB columns (EB Name)
      next
    }

    norm <- normalise_party_cty(clean_name)
    # Deduplicate: if name already exists, append suffix
    if (norm %in% party_names) {
      norm <- paste0(norm, "_2")
    }
    party_positions <- c(party_positions, i)
    party_names <- c(party_names, norm)
  }

  # Build column map
  col_map <- c(
    stimmart = stimmart_col, ags = ags_col, ags_name = name_col,
    eligible_voters = wahlber_col, number_voters = waehler_col,
    invalid_votes = ungueltig_col, gueltige_stimmen = gueltig_col
  )
  party_map <- setNames(party_positions, party_names)
  col_map <- c(col_map, party_map)

  # Extract data (row 2 onward) — build df column by column to avoid name mangling
  n_data <- nrow(raw) - 1L
  df <- data.frame(row.names = seq_len(n_data), check.names = FALSE)
  for (k in seq_along(col_map)) {
    df[[names(col_map)[k]]] <- as.character(raw[[col_map[k]]])[2:nrow(raw)]
  }
  cat("    names[1:5]:", paste(names(df)[1:5], collapse=", "), "\n")

  # Filter to Kreistag rows only
  df <- df[!is.na(df[["stimmart"]]) & df[["stimmart"]] == "Kreistag", ]
  df[["stimmart"]] <- NULL
  cat("    after filter names[1:5]:", paste(names(df)[1:5], collapse=", "), "'ags'=", "ags" %in% names(df), "\n")

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "gueltige_stimmen", party_names)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }
  cat("    after numeric names[1:5]:", paste(names(df)[1:5], collapse=", "), "'ags'=", "ags" %in% names(df), "\n")

  # Track all-NA parties per AGS
  na_tracker <- list()
  for (pc in party_names) {
    na_tracker[[pc]] <- tapply(df[[pc]], df[["ags"]], function(x) all(is.na(x)))
  }
  cat("    after tapply OK\n")

  # Aggregate ballot districts to municipality level
  group_cols <- c("ags", "ags_name")
  dt <- as.data.table(df)
  cat("    dt names[1:5]:", paste(names(dt)[1:5], collapse=", "), "\n")
  df_muni <- dt[, lapply(.SD, sum, na.rm = TRUE), by = group_cols, .SDcols = all_numeric]
  df_muni <- as.data.frame(df_muni)

  # Restore NA for no-candidacy parties
  for (pc in party_names) {
    no_cand_ags <- names(na_tracker[[pc]])[na_tracker[[pc]] == TRUE]
    if (length(no_cand_ags) > 0) {
      df_muni[[pc]][df_muni[["ags"]] %in% no_cand_ags] <- NA_real_
    }
  }

  # Add metadata
  df_muni$election_year <- as.integer(year)
  df_muni$state <- "12"
  df_muni$ags <- pad_zero_conditional(df_muni$ags, 7)
  df_muni$county <- substr(df_muni$ags, 1, 5)

  # Compute vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_names) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }
  names(df_muni)[names(df_muni) == "gueltige_stimmen"] <- "valid_votes"

  cat("    ->", nrow(df_muni), "municipalities\n")
  as_tibble(df_muni)
}

#' Parse BB 2024 XLSX (different format: ARS instead of AGS, aggregated party section)
parse_bb_2024 <- function(filepath) {
  cat("  Reading BB 2024...\n")

  suppressMessages(
    raw <- read_excel(filepath, sheet = "Brandenburg_KW_W",
                      col_names = FALSE, col_types = "text")
  )

  # Row 1 = headers
  headers <- clean_header(unlist(raw[1, ]))

  # Key columns
  ars_col <- which(headers == "ARS")[1]
  name_col <- which(headers == "Gemeindename")[1]
  wahlber_col <- which(headers == "Wahlberechtigte insgesamt")[1]
  waehler_col <- which(grepl("^W.hlende$", headers))[1]
  ungueltig_col <- which(headers == "Ungültige Stimmzettel")[1] # nolint
  if (is.na(ungueltig_col)) ungueltig_col <- which(grepl("Ung.ltige Stimmzettel$", headers))[1]

  # "Gültige Stimmen" — first occurrence is the absolute count
  gueltig_col <- which(grepl("G.ltige Stimmen$", headers))[1]

  # "Stimmen nach aggregierten Wahlvorschlägen" marker at col 29
  # Aggregated party cols: 30-63 (every other = votes, skipping "in Prozent")
  agg_marker <- which(grepl("aggregierten", headers))[1]
  # Individual marker
  indiv_marker <- which(grepl("Stimmen nach Wahlvorschl.gen$", headers))[1]

  # Party cols: between agg_marker+1 and indiv_marker-1, skip "in Prozent"
  party_positions <- c()
  party_names <- c()
  end_col <- if (!is.na(indiv_marker)) indiv_marker - 1 else ncol(raw)
  for (i in (agg_marker + 1):end_col) {
    name <- headers[i]
    if (is.na(name) || nchar(trimws(name)) == 0) next
    if (grepl("in Prozent", name, ignore.case = TRUE)) next

    clean_name <- trimws(name)
    if (grepl("Einzelbewerbende|^Einzelbewerber", clean_name)) {
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, "einzelbewerber")
    } else {
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, normalise_party_cty(clean_name))
    }
  }

  # Build column map
  col_map <- c(
    ars = ars_col, ags_name = name_col,
    eligible_voters = wahlber_col, number_voters = waehler_col,
    invalid_votes = ungueltig_col, gueltige_stimmen = gueltig_col
  )
  party_map <- setNames(party_positions, party_names)
  col_map <- c(col_map, party_map)

  # Extract data — build df column by column to avoid name mangling
  n_data <- nrow(raw) - 1L
  df <- data.frame(row.names = seq_len(n_data), check.names = FALSE)
  for (k in seq_along(col_map)) {
    df[[names(col_map)[k]]] <- as.character(raw[[col_map[k]]])[2:nrow(raw)]
  }

  # Construct AGS from first 8 chars of ARS
  df$ags <- substr(df[["ars"]], 1, 8)
  df$ars <- NULL

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "gueltige_stimmen", party_names)
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Track all-NA parties per AGS
  na_tracker <- list()
  for (pc in party_names) {
    na_tracker[[pc]] <- tapply(df[[pc]], df$ags, function(x) all(is.na(x)))
  }

  # Aggregate ballot districts to municipality level
  group_cols <- c("ags", "ags_name")
  dt <- as.data.table(df)
  df_muni <- dt[, lapply(.SD, sum, na.rm = TRUE), by = group_cols, .SDcols = all_numeric]
  df_muni <- as.data.frame(df_muni)

  # Restore NA for no-candidacy parties
  for (pc in party_names) {
    no_cand_ags <- names(na_tracker[[pc]])[na_tracker[[pc]] == TRUE]
    if (length(no_cand_ags) > 0) {
      df_muni[[pc]][df_muni$ags %in% no_cand_ags] <- NA_real_
    }
  }

  # Add metadata
  df_muni$election_year <- 2024L
  df_muni$state <- "12"
  df_muni$ags <- pad_zero_conditional(df_muni$ags, 7)
  df_muni$county <- substr(df_muni$ags, 1, 5)

  # Compute vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_names) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }
  names(df_muni)[names(df_muni) == "gueltige_stimmen"] <- "valid_votes"

  cat("    ->", nrow(df_muni), "municipalities\n")
  as_tibble(df_muni)
}

# Process BB years — wrapped in tryCatch due to known tibble name-mangling bug
df_bb <- tryCatch({
  bb_results <- list()

  bb_xlsx_files <- list(
    list(year = 2003, file = "Brandenburg_2003_KTW.xlsx"),
    list(year = 2008, file = "Brandenburg_2008_KTW.xlsx"),
    list(year = 2014, file = "Brandenburg_2014_KTW.xlsx"),
    list(year = 2019, file = "Brandenburg_2019_KTW.xlsx")
  )
  for (f in bb_xlsx_files) {
    bb_results[[as.character(f$year)]] <- parse_bb_xlsx(
      file.path(bb_dir, f$file), f$year
    )
  }
  bb_results[["2024"]] <- parse_bb_2024(
    file.path(bb_dir, "Brandenburg_2024_KTW.xlsx")
  )

  df_bb <- bind_rows(bb_results)
  df_bb <- df_bb |> mutate(ags = pad_zero_conditional(ags, 7))

  # Filter to valid municipality rows
  n_before <- nrow(df_bb)
  df_bb <- df_bb |> filter(nchar(ags) == 8 & eligible_voters > 0)
  cat("  Removed", n_before - nrow(df_bb), "non-municipality rows\n")

  cat("BB total:", nrow(df_bb), "rows x", ncol(df_bb), "cols\n")
  cat("BB years:", paste(sort(unique(df_bb$election_year)), collapse = ", "), "\n")
  df_bb |> count(election_year) |> print()
  df_bb
}, error = function(e) {
  cat("  BB FAILED:", conditionMessage(e), "\n")
  cat("  Skipping BB — known tibble name-mangling issue\n")
  NULL
})


# =============================================================================
# BAYERN (BY) — Kreis-level, 1984–2020
# =============================================================================

cat("\n===== BAYERN =====\n")

by_dir <- file.path(raw_dir, "Bayern")

#' Parse a single BY Ergebnis + Wahlbeteiligung file pair
parse_by_year <- function(ergebnis_path, wahlbet_path, year) {
  cat("  Reading BY", year, "...\n")

  # --- Ergebnis file: party votes ---
  suppressMessages(
    raw_e <- read_excel(ergebnis_path, col_names = FALSE, col_types = "text")
  )

  # Row 6 has party names; data starts at row 7
  party_row <- 6
  party_names_raw <- as.character(raw_e[party_row, ])

  # Col 1 = AGS code, col 2 = name, col 3 = "Insgesamt" (total valid votes)
  # Cols 4+ = individual party votes
  # Identify party columns (cols 4 onwards, excluding NA)
  party_cols_idx <- which(!is.na(party_names_raw) & seq_along(party_names_raw) >= 4)
  party_labels <- party_names_raw[party_cols_idx]

  # Extract short party name from full name: "... (ABBREV)" pattern
  extract_short <- function(x) {
    m <- regmatches(x, regexpr("\\(([^)]+)\\)\\s*$", x))
    if (length(m) == 1 && nchar(m) > 0) {
      return(gsub("[()]", "", m))
    }
    x  # fallback: use full name
  }
  party_short <- sapply(party_labels, extract_short)

  # Normalise
  party_norm <- sapply(party_short, normalise_party_cty)

  # Data rows: rows 7 onwards
  data_rows <- 7:nrow(raw_e)
  codes <- as.character(raw_e[[1]][data_rows])
  names_col <- as.character(raw_e[[2]][data_rows])

  # Filter to 5-digit Kreis codes only (skip state/Regbez aggregates, footnotes)
  is_kreis <- !is.na(codes) & grepl("^\\d{5}$", codes)
  kreis_idx <- data_rows[is_kreis]

  # Build data frame — convert 5-digit county code to 8-digit AGS (append 000)
  df <- data.frame(
    ags = paste0(codes[is_kreis], "000"),
    ags_name = names_col[is_kreis],
    stringsAsFactors = FALSE
  )

  # Total valid votes (col 3 = "Insgesamt")
  df$valid_votes <- as.numeric(as.character(raw_e[[3]][kreis_idx]))

  # Party vote counts
  for (k in seq_along(party_cols_idx)) {
    vals <- as.character(raw_e[[party_cols_idx[k]]][kreis_idx])
    vals[vals == "-"] <- NA_character_
    df[[party_norm[k]]] <- as.numeric(vals)
  }

  # Handle duplicate normalised names (e.g., if two raw names map to same)
  dupl_names <- names(which(table(party_norm) > 1))
  for (dn in dupl_names) {
    dn_cols <- which(party_norm == dn)
    # Sum duplicates
    combined <- rowSums(sapply(party_cols_idx[dn_cols], function(ci) {
      v <- as.character(raw_e[[ci]][kreis_idx])
      v[v == "-"] <- NA_character_
      as.numeric(v)
    }), na.rm = TRUE)
    # Restore NA where ALL sources are NA
    all_na <- rowSums(!is.na(sapply(party_cols_idx[dn_cols], function(ci) {
      v <- as.character(raw_e[[ci]][kreis_idx])
      v[v == "-"] <- NA_character_
      as.numeric(v)
    }))) == 0
    combined[all_na] <- NA_real_
    df[[dn]] <- combined
  }

  # --- Wahlbeteiligung file: turnout data ---
  suppressMessages(
    raw_w <- read_excel(wahlbet_path, col_names = FALSE, col_types = "text")
  )

  # Row 4 has headers, data from row 6
  w_data_rows <- 6:nrow(raw_w)
  w_codes <- as.character(raw_w[[1]][w_data_rows])
  w_is_kreis <- !is.na(w_codes) & grepl("^\\d{5}$", w_codes)

  w_elig <- as.numeric(as.character(raw_w[[3]][w_data_rows[w_is_kreis]]))
  w_voters <- as.numeric(as.character(raw_w[[4]][w_data_rows[w_is_kreis]]))
  w_codes_kreis <- paste0(w_codes[w_is_kreis], "000")

  turnout_df <- data.frame(
    ags = w_codes_kreis,
    eligible_voters = w_elig,
    number_voters = w_voters,
    stringsAsFactors = FALSE
  )

  # Merge
  df <- merge(df, turnout_df, by = "ags", all.x = TRUE)

  # Compute invalid_votes = number_voters - valid_votes (approximation)
  # BY Kreistagswahlen use Gewichtete Stimmen — invalid is at Stimmzettel level
  # We don't have explicit invalid, so set NA
  df$invalid_votes <- NA_real_

  # Compute vote shares
  party_col_names <- unique(party_norm)
  for (pc in party_col_names) {
    if (pc %in% names(df)) {
      df[[pc]] <- ifelse(df$valid_votes > 0, df[[pc]] / df$valid_votes, NA_real_)
    }
  }

  # Compute turnout
  df$turnout <- ifelse(df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)

  # Add metadata
  df$county <- substr(df$ags, 1, 5)  # 5-digit county code
  df$state <- "09"
  df$election_year <- year

  # Separate waehlergruppen / gemeinsame_wv
  # Keep these as special columns like other states

  cat("    ->", nrow(df), "Kreise\n")
  as_tibble(df)
}

# Process all years with Ergebnis files (1984-2020)
by_years <- c(1984, 1990, 1996, 2002, 2008, 2014, 2020)
by_results <- list()

for (yr in by_years) {
  ergebnis_file <- file.path(by_dir, paste0("Bayern_", yr, "_KTW_Ergebnis.xlsx"))
  wahlbet_file <- file.path(by_dir, paste0("Bayern_", yr, "_KTW_Wahlbeteiligung.xlsx"))
  if (file.exists(ergebnis_file) && file.exists(wahlbet_file)) {
    by_results[[as.character(yr)]] <- parse_by_year(ergebnis_file, wahlbet_file, yr)
  } else {
    cat("  Skipping BY", yr, "- file not found\n")
  }
}

df_by <- bind_rows(by_results)
cat("BY total:", nrow(df_by), "rows x", ncol(df_by), "cols\n")
cat("BY years:", paste(sort(unique(df_by$election_year)), collapse = ", "), "\n")
df_by |> count(election_year) |> print()


# =============================================================================
# SAARLAND (SL) — Gemeinde-level, 1984–2024
# =============================================================================

cat("\n===== SAARLAND =====\n")

sl_dir <- file.path(raw_dir, "Saarland")

# Main multi-year file: fixed column layout
# Col layout (from inspection):
#   1=Kreis name, 2=Regionalschlüssel, 3=Jahr,
#   4=Wahlberechtigte, 5=Wähler, 6=Wähler%,
#   7=Ungültige(abs), 8=Ungültige%, 9=Gültige(abs), 10=Gültige%,
#   11=SPD(abs), 12=SPD%, 13=CDU(abs), 14=CDU%,
#   15=GRÜNE(abs), 16=GRÜNE%, 17=FDP(abs), 18=FDP%,
#   19=LINKE(abs), 20=LINKE%, 21=AfD(abs), 22=AfD%?,
#   23=FAMILIE(abs), 24=FAMILIE%, 25=REP(abs), 26=REP%,
#   27=NPD(abs), 28=NPD%, 29=DKP(abs), 30=DKP%,
#   31=Wählergr./Listen(abs), 32=Wählergr.%, 33=Sonstige(abs), 34=Sonstige%
suppressMessages(
  sl_raw <- read_excel(
    file.path(sl_dir, "Saarland_1984-2024_Kreistagswahl.xlsx"),
    col_names = FALSE, col_types = "text"
  )
)

# Data starts at row 5
sl_data <- sl_raw[5:nrow(sl_raw), ]

# Build data frame with explicit column mapping
df_sl <- data.frame(
  ags_name = as.character(sl_data[[1]]),
  ags = as.character(sl_data[[2]]),
  election_year = as.integer(as.character(sl_data[[3]])),
  eligible_voters = as.numeric(as.character(sl_data[[4]])),
  number_voters = as.numeric(as.character(sl_data[[5]])),
  invalid_votes = as.numeric(as.character(sl_data[[7]])),
  valid_votes = as.numeric(as.character(sl_data[[9]])),
  stringsAsFactors = FALSE
)

# Party columns: absolute values in odd-numbered columns
sl_party_map <- list(
  spd = 11, cdu = 13, gruene = 15, fdp = 17,
  linke_pds = 19, afd = 21, familie = 23,
  rep = 25, npd = 27, dkp = 29,
  waehlergruppen = 31, other = 33
)

for (pname in names(sl_party_map)) {
  col_idx <- sl_party_map[[pname]]
  vals <- as.numeric(as.character(sl_data[[col_idx]]))
  df_sl[[pname]] <- vals
}

# Filter out rows with missing ags or year
df_sl <- df_sl |> filter(!is.na(ags) & !is.na(election_year))

# Convert party vote counts to shares
sl_party_cols <- names(sl_party_map)

for (pc in sl_party_cols) {
  df_sl[[pc]] <- ifelse(df_sl$valid_votes > 0,
                        df_sl[[pc]] / df_sl$valid_votes, NA_real_)
}

# Compute turnout
df_sl$turnout <- ifelse(df_sl$eligible_voters > 0,
                        df_sl$number_voters / df_sl$eligible_voters, NA_real_)

# AGS: Saarland Regionalschlüssel is 8-digit, already correct
# County = first 5 digits
df_sl$county <- substr(df_sl$ags, 1, 5)
df_sl$state <- "10"

cat("SL total:", nrow(df_sl), "rows x", ncol(df_sl), "cols\n")
cat("SL years:", paste(sort(unique(df_sl$election_year)), collapse = ", "), "\n")
df_sl |> count(election_year) |> print()


# =============================================================================
# HESSEN (HE) — Gemeinde-level, 1948–2016 (+ 2021 Kreis-level)
# =============================================================================

cat("\n===== HESSEN =====\n")

he_dir <- file.path(raw_dir, "Hessen")

#' Parse a single HE sheet from the multi-year XLSX file
#' Handles three format eras:
#'   A) Pre-reform (1948–1972): ~2700 rows, GKZ in col 2, party names in row 3-4
#'   B) Post-reform single-col (1977–2001): ~425 rows, varying GKZ/party positions
#'   C) Modern multi-block (2006–2016): ~454 rows, 4-5 col blocks per party
parse_he_sheet <- function(filepath, sheet_name) {
  year <- as.integer(sheet_name)
  cat("  Reading HE", year, "...\n")

  suppressMessages(
    raw <- read_excel(filepath, sheet = sheet_name, col_names = FALSE, col_types = "text")
  )

  # --- Step 1: Find party name row (scan rows 2-10 for CDU or SPD) ---
  party_row <- NA
  for (r in 2:min(10, nrow(raw))) {
    vals <- as.character(raw[r, ])
    if (any(grepl("^CDU$|^SPD$", trimws(vals)), na.rm = TRUE)) {
      party_row <- r
      break
    }
  }
  if (is.na(party_row)) {
    cat("    WARNING: Could not find party row for", year, "- skipping\n")
    return(NULL)
  }

  # --- Step 2: Detect format (multi-block vs single-col) ---
  r6 <- as.character(raw[6, ])
  is_multi_block <- any(grepl("^absolut$", trimws(r6)), na.rm = TRUE)

  # --- Step 3: Find GKZ column and data start row ---
  # GKZ is 3-digit (Kreis) or 6-digit (Gemeinde), could be in col 1, 2, or 3
  gkz_col <- NA
  data_start <- NA
  gkz_pattern <- "^\\d{3,6}$"  # accept 3-6 digit codes
  for (gc in c(2, 1, 3)) {  # prefer col 2 (most common)
    for (dr in (party_row + 1):min(party_row + 15, nrow(raw))) {
      val <- as.character(raw[[gc]][dr])
      if (!is.na(val) && grepl(gkz_pattern, val) && !grepl("^\\d{1,2}$", val)) {
        gkz_col <- gc
        data_start <- dr
        break
      }
    }
    if (!is.na(gkz_col)) break
  }
  if (is.na(gkz_col)) {
    cat("    WARNING: Could not find GKZ column for", year, "- skipping\n")
    return(NULL)
  }

  # --- Step 4: Extract party columns ---
  party_vals <- as.character(raw[party_row, ])

  if (is_multi_block) {
    # Modern format: party names at positions 15/17, 19/22, 23/27, etc.
    # "absolut" sub-column is in row 6 at positions within each block
    # Party name positions: where row 3 has a party name AND it's after the meta cols
    # Find the first party column (CDU/SPD position)
    skip_labels <- c("Laut W", "nach §", "darunter", "ohne", "mit ")
    party_positions <- c()
    party_names <- c()
    for (i in seq_along(party_vals)) {
      pv <- trimws(party_vals[i])
      if (is.na(pv)) next
      if (any(sapply(skip_labels, function(s) grepl(s, pv, fixed = TRUE)))) next
      cpv <- clean_header(pv)
      # Skip standalone "Insgesamt", "insgesamt", "Ins-gesamt" but NOT "WG insgesamt"
      if (grepl("^[Ii]ns", cpv) && !grepl("^WG|^wg", cpv)) next
      # Skip "Auf die einzelnen Wählergruppen" aggregate headers
      if (grepl("Auf die einzelnen", cpv)) next
      if (i < 10) next  # meta columns are in first 10 cols
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, clean_header(pv))
    }

    # For each party, the "absolut" sub-column is at the party position itself
    # (row 6 confirms: absolut, gew., %, Sitze, dar.Frauen)
    # Vote count = data at the party position column

    # Build data frame
    all_data <- raw[data_start:nrow(raw), ]
    gkz_vals <- as.character(all_data[[gkz_col]])
    valid_rows <- !is.na(gkz_vals) & grepl("^\\d{3,6}$", gkz_vals) & nchar(gkz_vals) >= 3

    df <- data.frame(
      gkz = gkz_vals[valid_rows],
      stringsAsFactors = FALSE
    )

    # Name column: usually gkz_col + 1
    name_col <- gkz_col + 1
    df$ags_name <- as.character(all_data[[name_col]][valid_rows])

    # Meta cols: eligible_voters, voters, valid_votes
    # 2006/2016: col 7 = Insgesamt (eligible), col 8 = Wähler, col 14 = Gültige Stimmen
    # 2011: col 7 = Insgesamt (eligible), col 8 = Wähler, col 14 = Gültige Stimmen (check)
    # Find "Gültige Stimmen" in row 2
    r2 <- clean_header(as.character(raw[2, ]))
    valid_votes_col <- which(grepl("G.ltige.*Stimmen", r2))[1]
    voters_col <- which(grepl("^W.hler$", r2))[1]
    eligible_col <- which(grepl("Wahlberechtigte", r2))[1]

    # Eligible voters "Insgesamt" is typically 2-3 cols after the Wahlberechtigte label
    # Find the "Ins-gesamt" in row 3 near the eligible_col
    if (!is.na(eligible_col)) {
      # Look for first data column after eligible_col that has large numbers
      for (ec in (eligible_col):(eligible_col + 4)) {
        test_val <- as.numeric(as.character(all_data[[ec]][which(valid_rows)[1]]))
        if (!is.na(test_val) && test_val > 100) { eligible_col <- ec; break }
      }
    }

    df$eligible_voters <- if (!is.na(eligible_col)) as.numeric(as.character(all_data[[eligible_col]][valid_rows])) else NA_real_
    df$number_voters <- if (!is.na(voters_col)) as.numeric(as.character(all_data[[voters_col]][valid_rows])) else NA_real_
    df$valid_votes <- if (!is.na(valid_votes_col)) as.numeric(as.character(all_data[[valid_votes_col]][valid_rows])) else NA_real_

    # Party vote counts
    for (k in seq_along(party_positions)) {
      pname <- normalise_party_cty(tolower(trimws(party_names[k])))
      # Skip aggregate labels
      if (pname %in% c("wg_insgesamt", "wg_ins_gesamt", "wg_insg", "insgesamt",
                        "darunter", "auf_die_einzelnen_w_hlergruppen",
                        "auf_die_einzelnen_wahlergruppen")) next
      vals <- as.character(all_data[[party_positions[k]]][valid_rows])
      vals[vals == "x" | vals == "-"] <- NA_character_
      vote_counts <- as.numeric(vals)
      if (pname %in% names(df)) {
        # Duplicate: sum
        df[[pname]] <- rowSums(cbind(df[[pname]], vote_counts), na.rm = TRUE)
        df[[pname]][is.na(df[[pname]]) & is.na(vote_counts)] <- NA_real_
      } else {
        df[[pname]] <- vote_counts
      }
    }

  } else {
    # Single-column format: each party is one column
    # Party names at party_row, party columns identified by position
    # Need to find which columns are parties vs meta

    # Identify all non-NA values in party row
    all_positions <- which(!is.na(party_vals))
    all_labels <- clean_header(party_vals[all_positions])

    # Meta column detection: find positions of key meta labels in rows 2-party_row
    meta_labels <- c()
    for (r in 2:party_row) {
      rv <- clean_header(as.character(raw[r, ]))
      for (i in seq_along(rv)) {
        if (!is.na(rv[i]) && grepl("Wahlbe|W.hler|Ung.ltig|G.ltig|Stimmen", rv[i])) {
          meta_labels <- c(meta_labels, i)
        }
      }
    }

    # Data extraction
    all_data <- raw[data_start:nrow(raw), ]
    gkz_vals <- as.character(all_data[[gkz_col]])
    valid_rows <- !is.na(gkz_vals) & grepl("^\\d{3,6}$", gkz_vals) & nchar(gkz_vals) >= 3

    df <- data.frame(gkz = gkz_vals[valid_rows], stringsAsFactors = FALSE)

    # Name: next column after GKZ
    name_col <- gkz_col + 1
    # But for 1993 format (GKZ in col 1), name is in col 2
    df$ags_name <- as.character(all_data[[name_col]][valid_rows])

    # For single-col formats, find meta and party columns by scanning headers
    # Key columns vary by year. Use a heuristic: scan row above party_row for meta labels
    # Then all columns in party_row that are parties (not meta) are vote count columns

    # Find eligible_voters, voters, valid_votes by scanning header rows
    # Look in ALL header rows for keywords
    elig_col <- NA; voter_col <- NA; valid_col <- NA; invalid_col <- NA
    for (r in 2:party_row) {
      rv <- clean_header(as.character(raw[r, ]))
      for (i in seq_along(rv)) {
        if (is.na(rv[i])) next
        lv <- tolower(rv[i])
        if (grepl("wahlberechtigte|wahlbe rechtigte", lv) && is.na(elig_col)) {
          # Find the "insgesamt" sub-col near this
          for (ec in i:(i + 4)) {
            tv <- as.numeric(as.character(all_data[[ec]][which(valid_rows)[1]]))
            if (!is.na(tv) && tv > 100) { elig_col <- ec; break }
          }
        }
        if (grepl("^w.hler$|^w.hlerinnen", lv) && is.na(voter_col)) {
          for (vc in i:(i + 3)) {
            tv <- as.numeric(as.character(all_data[[vc]][which(valid_rows)[1]]))
            if (!is.na(tv) && tv > 50) { voter_col <- vc; break }
          }
        }
        if (grepl("g.ltige.*stimmen|g.ltig$", lv) && !grepl("ung.ltig", lv) && is.na(valid_col)) {
          valid_col <- i
        }
        if (grepl("ung.ltig", lv) && is.na(invalid_col)) {
          invalid_col <- i
        }
      }
    }

    # Fallback for valid_col: if not found, try column before first party position
    if (is.na(valid_col)) {
      first_party_pos <- NA
      for (i in all_positions) {
        rn <- trimws(party_vals[i])
        cn <- clean_header(rn)
        if (grepl("^CDU$|^SPD$|^CSU$", cn)) { first_party_pos <- i; break }
      }
      if (!is.na(first_party_pos)) {
        # Valid votes is typically 1 column before first party
        for (vc in (first_party_pos - 1):max(1, first_party_pos - 3)) {
          tv <- as.numeric(as.character(all_data[[vc]][which(valid_rows)[1]]))
          if (!is.na(tv) && tv > 100) { valid_col <- vc; break }
        }
      }
    }

    df$eligible_voters <- if (!is.na(elig_col)) as.numeric(as.character(all_data[[elig_col]][valid_rows])) else NA_real_
    df$number_voters <- if (!is.na(voter_col)) as.numeric(as.character(all_data[[voter_col]][valid_rows])) else NA_real_
    df$valid_votes <- if (!is.na(valid_col)) as.numeric(as.character(all_data[[valid_col]][valid_rows])) else NA_real_
    df$invalid_votes <- if (!is.na(invalid_col)) as.numeric(as.character(all_data[[invalid_col]][valid_rows])) else NA_real_

    # Party columns: all positions in party_row that are known party names
    known_parties <- c("CDU", "SPD", "FDP", "F.D.P.", "GRÜNE", "Grüne",
                       "NPD", "REP", "AfD", "PDS", "DIE LINKE", "Die Linke.",
                       "DKP", "KPD", "KBW", "LDP", "NDP", "AP", "EVD",
                       "GB/BHE", "DRP", "GDP", "BHE", "GPD-BHE",
                       "DP", "DFU", "DL", "SDO", "SRP",
                       "WASG", "PIRATEN", "Tierschutzpartei", "Die Tierschutz",
                       "ÖDP", "PBC", "ALFA", "EINHEIT", "FREIE WÄHLER",
                       "Wählergruppen", "Wähler-gruppen", "WG insgesamt",
                       "WG ins-gesamt", "FW u. SDO",
                       "ZENTRUM", "Zentrum", "LIGA", "DIE BLAUEN",
                       "NATUR-GESETZ", "DHP", "CM", "APPD")

    # Check row below party_row for WG sub-header indicators
    sub_row <- if (party_row + 1 <= nrow(raw)) as.character(raw[party_row + 1, ]) else rep(NA, ncol(raw))

    for (i in all_positions) {
      raw_name <- trimws(party_vals[i])
      cleaned_name <- clean_header(raw_name)
      # Skip meta labels and aggregates (check both raw and cleaned versions)
      # Note: W.hler must be anchored to avoid matching "Wählergruppen" etc.
      if (grepl("Wahlbe|W.hlerinnen|Ung.ltig|G.ltig|Stimmen|darunter|Merkmal|Bemerkungen|Kenn|Laut|Lt\\.|ohne|mit|GKZ|Gebiets|Aufteilung|Auf die|Von den|W.hlerverz", raw_name, ignore.case = TRUE)) next
      if (grepl("^W.hler$", cleaned_name, ignore.case = TRUE)) next
      # Skip "Insgesamt", "ins-gesamt" etc. but NOT "WG insgesamt"
      if (grepl("^insgesamt$|^ins[- ]?gesamt$|^son[- ]?stige$", cleaned_name, ignore.case = TRUE)) next
      # Skip WG breakdown sub-headers: if row below has "WG \d" at this column
      if (i <= length(sub_row) && !is.na(sub_row[i]) && grepl("^WG \\d|^WG\\d", trimws(sub_row[i]))) next
      if (grepl("^WG \\d|^WG\\d|^WG$", cleaned_name)) next  # Skip individual WG1, WG2, etc.
      if (i <= max(gkz_col + 1, 3)) next  # Skip early meta columns

      pname <- normalise_party_cty(tolower(clean_header(raw_name)))
      vals <- as.character(all_data[[i]][valid_rows])
      vals[vals == "x" | vals == "-"] <- NA_character_
      vote_counts <- as.numeric(vals)

      if (pname %in% names(df)) {
        old <- df[[pname]]
        df[[pname]] <- ifelse(is.na(old) & is.na(vote_counts), NA_real_,
                              ifelse(is.na(old), 0, old) + ifelse(is.na(vote_counts), 0, vote_counts))
      } else {
        df[[pname]] <- vote_counts
      }
    }
  }

  # --- Step 5: Compute shares and metadata ---
  # AGS: HE GKZ format
  # 6-digit GKZ (e.g., 431001) → AGS = "06" + GKZ = "06431001" (8-digit)
  # 3-digit GKZ (e.g., 431) → Kreis-level = "06" + GKZ + "000" = "06431000"
  df$ags <- ifelse(nchar(df$gkz) == 6, paste0("06", df$gkz),
            ifelse(nchar(df$gkz) == 3, paste0("06", df$gkz, "000"),
                   paste0("06", sprintf("%-6s", df$gkz))))

  df$county <- substr(df$ags, 1, 5)
  df$state <- "06"
  df$election_year <- year

  # If both 3-digit and 6-digit GKZ exist, keep only 6-digit (Gemeinde-level)
  has_gemeinde <- any(nchar(df$gkz) == 6)
  has_kreis <- any(nchar(df$gkz) == 3)
  if (has_gemeinde && has_kreis) {
    n_before <- nrow(df)
    df <- df[nchar(df$gkz) == 6, ]
    cat("    Filtered out", n_before - nrow(df), "Kreis aggregate rows\n")
  }

  # Aggregate Wahlbezirk → Gemeinde if multiple rows per GKZ
  if (anyDuplicated(df$ags)) {
    n_before <- nrow(df)
    # Columns to sum
    num_cols <- setdiff(names(df), c("gkz", "ags", "ags_name", "county", "state", "election_year"))
    # Keep first name per ags
    name_map <- df[!duplicated(df$ags), c("ags", "ags_name")]
    agg <- df |>
      dplyr::group_by(ags) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(num_cols),
                                     ~ sum(as.numeric(.), na.rm = TRUE)), .groups = "drop")
    # Restore NA where all source values were NA
    for (nc in num_cols) {
      all_na_ags <- df |>
        dplyr::group_by(ags) |>
        dplyr::summarise(all_na = all(is.na(.data[[nc]])), .groups = "drop") |>
        dplyr::filter(all_na) |>
        dplyr::pull(ags)
      agg[[nc]][agg$ags %in% all_na_ags] <- NA_real_
    }
    agg <- merge(name_map, agg, by = "ags")
    agg$county <- substr(agg$ags, 1, 5)
    agg$state <- "06"
    agg$election_year <- year
    agg$gkz <- substr(agg$ags, 3, 8)
    df <- agg
    cat("    Aggregated", n_before, "WBZ rows ->", nrow(df), "Gemeinden\n")
  }

  # Compute vote shares
  party_cols <- setdiff(names(df), c("gkz", "ags", "ags_name", "county", "state",
                                      "election_year", "eligible_voters",
                                      "number_voters", "valid_votes", "invalid_votes"))
  for (pc in party_cols) {
    df[[pc]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                       df[[pc]] / df$valid_votes, NA_real_)
  }

  # Compute turnout
  df$turnout <- ifelse(!is.na(df$eligible_voters) & df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)

  # Remove temporary gkz column
  df$gkz <- NULL

  cat("    ->", nrow(df), "Gemeinden,", length(party_cols), "party cols\n")
  as_tibble(df)
}

# Process all sheets
he_xlsx <- file.path(he_dir, "Hessen_1948-2016_Kreistagswahl_Gemeinden.xlsx")
he_sheets <- excel_sheets(he_xlsx)
he_results <- list()
for (sh in he_sheets) {
  result <- tryCatch(
    parse_he_sheet(he_xlsx, sh),
    error = function(e) {
      cat("    ERROR in HE", sh, ":", conditionMessage(e), "\n")
      NULL
    }
  )
  if (!is.null(result) && nrow(result) > 0) {
    he_results[[sh]] <- result
  }
}

# Process 2021 CSV (Kreis-level only, Latin-1 encoded)
he_2021_csv <- file.path(he_dir, "Hessen_2021_Kreistagswahl_Gemeinden.csv")
if (file.exists(he_2021_csv)) {
  cat("  Reading HE 2021 CSV...\n")
  tryCatch({
    # Read with Latin-1 encoding; row 1 = title, row 2 = headers, row 3 = position numbers
    he21_lines <- readLines(he_2021_csv, encoding = "latin1", warn = FALSE)
    he21_headers <- strsplit(he21_lines[2], ";")[[1]]
    he21_data_lines <- he21_lines[4:length(he21_lines)]
    he21_data_lines <- he21_data_lines[nchar(trimws(he21_data_lines)) > 0]

    he21_mat <- do.call(rbind, strsplit(he21_data_lines, ";"))
    he21_df <- as.data.frame(he21_mat, stringsAsFactors = FALSE)

    # Find key columns
    gkz_col_21 <- which(grepl("GKZ", he21_headers))[1]
    name_col_21 <- which(grepl("Gemeinde", he21_headers))[1]
    elig_col_21 <- which(grepl("insgesamt", he21_headers) & grepl("Wahlberechtigte", he21_headers))[1]
    if (is.na(elig_col_21)) elig_col_21 <- which(grepl("Wahlberechtigte insgesamt", he21_headers))[1]
    voter_col_21 <- which(grepl("hlerinnen", he21_headers))[1]  # Wählerinnen und Wähler
    valid_col_21 <- which(grepl("ltige Stimmen", he21_headers))[1]

    gkz_vals <- trimws(he21_df[[gkz_col_21]])
    valid_21 <- grepl("^\\d{3,6}$", gkz_vals)
    he21_data <- he21_df[valid_21, ]

    if (nrow(he21_data) > 0) {
      df_21 <- data.frame(
        ags = paste0("06", sprintf("%03s", trimws(he21_data[[gkz_col_21]])), "000"),
        ags_name = if (!is.na(name_col_21)) trimws(he21_data[[name_col_21]]) else NA_character_,
        state = "06",
        election_year = 2021L,
        stringsAsFactors = FALSE
      )
      df_21$eligible_voters <- if (!is.na(elig_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[elig_col_21]])) else NA_real_
      df_21$number_voters <- if (!is.na(voter_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[voter_col_21]])) else NA_real_
      df_21$valid_votes <- if (!is.na(valid_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[valid_col_21]])) else NA_real_
      df_21$county <- substr(df_21$ags, 1, 5)

      # Party columns: skip *-Sitze, WG names, position-only cols
      # Collect WG1/WG2/WG3 separately to sum into waehlergruppen
      wg_sum <- rep(0, nrow(he21_data))
      wg_any <- rep(FALSE, nrow(he21_data))
      for (i in seq_along(he21_headers)) {
        hdr <- trimws(he21_headers[i])
        if (is.na(hdr) || hdr == "") next
        if (grepl("Sitze|Lfd|GKZ|Gemeinde|Wahllokal|Wahlberechtigte|hler|darunter|ltige|Stimmzettel", hdr)) next
        if (i <= max(c(valid_col_21), na.rm = TRUE)) next
        vals <- as.numeric(gsub("[^0-9]", "", he21_data[[i]]))
        if (all(is.na(vals) | vals == 0)) next
        # Sum WG1/WG2/WG3 into waehlergruppen
        if (grepl("^WG\\d", hdr)) {
          wg_sum <- wg_sum + ifelse(is.na(vals), 0, vals)
          wg_any <- wg_any | !is.na(vals)
          next
        }
        pname <- normalise_party_cty(tolower(clean_header(hdr)))
        df_21[[pname]] <- ifelse(!is.na(df_21$valid_votes) & df_21$valid_votes > 0,
                                 vals / df_21$valid_votes, NA_real_)
      }
      # Add aggregated Wählergruppen
      if (any(wg_any)) {
        wg_sum[!wg_any] <- NA_real_
        df_21[["waehlergruppen"]] <- ifelse(!is.na(df_21$valid_votes) & df_21$valid_votes > 0,
                                            wg_sum / df_21$valid_votes, NA_real_)
      }
      df_21$turnout <- ifelse(!is.na(df_21$eligible_voters) & df_21$eligible_voters > 0,
                              df_21$number_voters / df_21$eligible_voters, NA_real_)
      cat("    -> 2021:", nrow(df_21), "Kreise\n")
      he_results[["2021"]] <- as_tibble(df_21)
    }
  }, error = function(e) cat("    ERROR in HE 2021 CSV:", conditionMessage(e), "\n"))
}

df_he <- bind_rows(he_results)
cat("HE total:", nrow(df_he), "rows x", ncol(df_he), "cols\n")
cat("HE years:", paste(sort(unique(df_he$election_year)), collapse = ", "), "\n")
df_he |> count(election_year) |> print()


# =============================================================================
# BADEN-WÜRTTEMBERG (BW) — Kreis-level, 1994–2019
# =============================================================================

cat("\n===== BADEN-WÜRTTEMBERG =====\n")

bw_dir <- "data/county_elections/raw/local_elections_bw"

#' Parse BW Format B (1994, 2004-2019): multi-row per Kreis
parse_bw_format_b <- function(filepath, year) {
  cat("  Reading BW", year, "(format B)...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Row 3 has party names (from col 10 onwards typically)
  r3 <- clean_header(as.character(raw[3, ]))

  # Find party positions: non-NA values in row 3 that are party names (after col 5)
  party_positions <- c()
  party_names <- c()
  for (i in 5:length(r3)) {
    if (!is.na(r3[i]) && !grepl("Wahlberechtigte|W.hler|Stimmen|Einheit|Schl.ssel|Landkreis|Lfd|insgesamt|Ins-", r3[i])) {
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, r3[i])
    }
  }

  # Data rows: filter to Einheit == "Anzahl"
  # Find Einheit column
  r2 <- clean_header(as.character(raw[2, ]))
  einheit_col <- which(grepl("Einheit", r2))[1]
  if (is.na(einheit_col)) einheit_col <- 4  # typical position

  schluessel_col <- which(grepl("Schl.ssel", r2))[1]
  if (is.na(schluessel_col)) schluessel_col <- 2
  name_col <- which(grepl("Landkreis|Kreis", r2))[1]
  if (is.na(name_col)) name_col <- 3

  # Find Bezeichnung column (tells us what metric: "Gültige Stimmen", "Gewählte", etc.)
  bez_col <- which(grepl("Bezeichnung", r2))[1]

  # Filter to Gültige Stimmen + Anzahl rows with valid Schlüssel
  data_start <- 5  # typical
  for (ds in 5:min(10, nrow(raw))) {
    sv <- as.character(raw[[schluessel_col]][ds])
    if (!is.na(sv) && grepl("^\\d{3}$", sv)) { data_start <- ds; break }
  }
  all_data <- raw[data_start:nrow(raw), ]
  einheit_vals <- tolower(trimws(as.character(all_data[[einheit_col]])))
  schluessel_vals <- as.character(all_data[[schluessel_col]])
  is_anzahl <- !is.na(einheit_vals) & grepl("^anz", einheit_vals)  # matches "anzahl" and "anz."
  is_valid_code <- !is.na(schluessel_vals) & grepl("^\\d{3}$", schluessel_vals)

  # Filter to "Gültige Stimmen" rows if Bezeichnung column exists
  if (!is.na(bez_col)) {
    bez_vals <- tolower(trimws(as.character(all_data[[bez_col]])))
    is_gultig <- !is.na(bez_vals) & grepl("^g.ltige stimmen$", bez_vals)
    keep <- is_anzahl & is_valid_code & is_gultig
  } else {
    keep <- is_anzahl & is_valid_code
  }

  df <- data.frame(
    ags = paste0("08", schluessel_vals[keep], "000"),
    ags_name = as.character(all_data[[name_col]][keep]),
    stringsAsFactors = FALSE
  )

  # Find valid_votes, eligible_voters columns in row 2-3
  elig_col <- which(grepl("Wahlberechtigte", r2))[1]
  voter_col <- which(grepl("^W.hler$", r2))[1]
  # Valid votes: look for "Gültige Stimmen" or just "Stimmen" after invalid
  stimmen_cols <- which(grepl("Stimmen", r2))
  valid_col <- NA
  invalid_col <- NA
  for (sc in stimmen_cols) {
    lbl <- tolower(r2[sc])
    if (grepl("ung.ltig", lbl)) invalid_col <- sc
    else if (grepl("g.ltig", lbl) && is.na(valid_col)) valid_col <- sc
  }

  df$eligible_voters <- if (!is.na(elig_col)) as.numeric(as.character(all_data[[elig_col]][keep])) else NA_real_
  df$number_voters <- if (!is.na(voter_col)) as.numeric(as.character(all_data[[voter_col]][keep])) else NA_real_
  df$valid_votes <- if (!is.na(valid_col)) as.numeric(as.character(all_data[[valid_col]][keep])) else NA_real_
  df$invalid_votes <- if (!is.na(invalid_col)) as.numeric(as.character(all_data[[invalid_col]][keep])) else NA_real_

  # Party vote counts
  for (k in seq_along(party_positions)) {
    pname <- normalise_party_cty(tolower(trimws(party_names[k])))
    vals <- as.character(all_data[[party_positions[k]]][keep])
    vals[vals == "x" | vals == "-" | vals == "."] <- NA_character_
    df[[pname]] <- as.numeric(vals)
  }

  # Compute shares
  bw_party_cols <- setdiff(names(df), c("ags", "ags_name", "eligible_voters",
                                         "number_voters", "valid_votes", "invalid_votes"))
  for (pc in bw_party_cols) {
    df[[pc]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                       df[[pc]] / df$valid_votes, NA_real_)
  }

  df$turnout <- ifelse(!is.na(df$eligible_voters) & df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  df$county <- substr(df$ags, 1, 5)
  df$state <- "08"
  df$election_year <- year

  cat("    ->", nrow(df), "Kreise\n")
  as_tibble(df)
}

#' Parse BW 1999 (Format A): wide format, one row per Kreis
parse_bw_1999 <- function(filepath) {
  cat("  Reading BW 1999 (format A)...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = TRUE, col_types = "text")
  )

  # Row 1 = headers: KKZ, Landkreis, Wahlberechtigung, ..., CDU_%, CDU_Anz, ...
  headers <- names(raw)

  # Find KKZ column
  kkz_col <- which(grepl("KKZ|Schl", headers, ignore.case = TRUE))[1]
  if (is.na(kkz_col)) kkz_col <- 1
  name_col_99 <- which(grepl("Landkreis|Kreis", headers, ignore.case = TRUE))[1]
  if (is.na(name_col_99)) name_col_99 <- 2

  # Valid rows: 3-digit KKZ
  kkz_vals <- as.character(raw[[kkz_col]])
  valid <- !is.na(kkz_vals) & grepl("^\\d{3}$", kkz_vals)
  data_99 <- raw[valid, ]

  df <- data.frame(
    ags = paste0("08", as.character(data_99[[kkz_col]]), "000"),
    ags_name = as.character(data_99[[name_col_99]]),
    stringsAsFactors = FALSE
  )

  # Find meta columns
  elig_col <- which(grepl("Wahlberechtigung|Wahlberechtigte", headers, ignore.case = TRUE))[1]
  voter_col <- which(grepl("^W.hler$|Wahlbeteiligung", headers, ignore.case = TRUE))[1]
  valid_col <- which(grepl("G.ltige.*Stimmen|Gultige_Stimmen", headers, ignore.case = TRUE))[1]

  df$eligible_voters <- if (!is.na(elig_col)) as.numeric(as.character(data_99[[elig_col]])) else NA_real_
  df$number_voters <- if (!is.na(voter_col)) as.numeric(as.character(data_99[[voter_col]])) else NA_real_
  df$valid_votes <- if (!is.na(valid_col)) as.numeric(as.character(data_99[[valid_col]])) else NA_real_

  # Party columns: find *_Gültige_Stimmen columns (vote counts, not %)
  # Headers use umlaut: "CDU_Gültige_Stimmen", "CDU_Gültige_Stimmen_Prozent", etc.
  # Keep only the absolute count columns (ending in _Gültige_Stimmen, not _Prozent)
  anz_cols <- grep("_G.ltige_Stimmen$", headers)
  for (ac in anz_cols) {
    # Extract party name from header
    party_raw <- sub("_G.ltige_Stimmen$", "", headers[ac])
    pname <- normalise_party_cty(tolower(trimws(party_raw)))
    vals <- as.numeric(as.character(data_99[[ac]]))
    df[[pname]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                          vals / df$valid_votes, NA_real_)
  }

  df$turnout <- ifelse(!is.na(df$eligible_voters) & df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  df$county <- substr(df$ags, 1, 5)
  df$state <- "08"
  df$election_year <- 1999L

  cat("    ->", nrow(df), "Kreise\n")
  as_tibble(df)
}

#' Parse BW 1994: unique format with Anz./% alternating rows, seat data in cols 18+
parse_bw_1994 <- function(filepath) {
  cat("  Reading BW 1994...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Row 3: party names at cols 10-17 (vote data); cols 18+ are seat allocations
  r3 <- clean_header(as.character(raw[3, ]))

  # Filter to data rows: col 1 has 3-digit code AND col 3 = "Anz."
  schluessel_vals <- as.character(raw[[1]])
  einheit_vals <- tolower(trimws(as.character(raw[[3]])))
  keep <- !is.na(schluessel_vals) & grepl("^\\d{3}$", schluessel_vals) &
          !is.na(einheit_vals) & grepl("^anz", einheit_vals)
  data_rows <- raw[keep, ]

  df <- data.frame(
    ags = paste0("08", as.character(data_rows[[1]]), "000"),
    ags_name = as.character(data_rows[[2]]),
    eligible_voters = as.numeric(as.character(data_rows[[4]])),
    number_voters = as.numeric(as.character(data_rows[[5]])),
    invalid_votes = as.numeric(as.character(data_rows[[6]])),
    valid_votes = as.numeric(as.character(data_rows[[7]])),
    stringsAsFactors = FALSE
  )

  # Party cols 10-17 only (skip cols 7-9 meta, cols 18+ seats)
  party_col_range <- 10:17
  for (pc in party_col_range) {
    pname_raw <- r3[pc]
    if (is.na(pname_raw) || pname_raw == "") next
    pname <- normalise_party_cty(tolower(trimws(pname_raw)))
    if (pname %in% c("insgesamt", "ins_gesamt")) next
    vals <- as.numeric(as.character(data_rows[[pc]]))
    df[[pname]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                          vals / df$valid_votes, NA_real_)
  }

  df$turnout <- ifelse(!is.na(df$eligible_voters) & df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  df$county <- substr(df$ags, 1, 5)
  df$state <- "08"
  df$election_year <- 1994L

  cat("    ->", nrow(df), "Kreise\n")
  as_tibble(df)
}

# Process BW files
bw_results <- list()
bw_results[["1999"]] <- tryCatch(
  parse_bw_1999(file.path(bw_dir, "Kreisergebnisse_KW_1999.xlsx")),
  error = function(e) { cat("  BW 1999 ERROR:", conditionMessage(e), "\n"); NULL }
)
bw_results[["1994"]] <- tryCatch(
  parse_bw_1994(file.path(bw_dir, "Kreisergebnisse_KW_1994.xlsx")),
  error = function(e) { cat("  BW 1994 ERROR:", conditionMessage(e), "\n"); NULL }
)
for (yr in c(2004, 2009, 2014, 2019)) {
  bw_results[[as.character(yr)]] <- tryCatch(
    parse_bw_format_b(file.path(bw_dir, paste0("Kreisergebnisse_KW_", yr, ".xlsx")), yr),
    error = function(e) { cat("  BW", yr, "ERROR:", conditionMessage(e), "\n"); NULL }
  )
}
bw_results <- bw_results[!sapply(bw_results, is.null)]
df_bw <- bind_rows(bw_results)
cat("BW total:", nrow(df_bw), "rows x", ncol(df_bw), "cols\n")
cat("BW years:", paste(sort(unique(df_bw$election_year)), collapse = ", "), "\n")
df_bw |> count(election_year) |> print()


# =============================================================================
# SCHLESWIG-HOLSTEIN (SH) — Wahlbezirk→Gemeinde, 1998–2023
# =============================================================================

cat("\n===== SCHLESWIG-HOLSTEIN =====\n")

sh_dir <- file.path(raw_dir, "Schleswig-Holstein")

# AGS conversion: stat code positions 1-5 → AGS = "010" + stat[1:5]
sh_stat_to_ags <- function(stat_code) {
  stat_code <- as.character(stat_code)
  # 2003 has 7-digit codes (missing leading zero)
  stat_code <- ifelse(nchar(stat_code) == 7, paste0("0", stat_code), stat_code)
  paste0("010", substr(stat_code, 1, 5))
}

#' Aggregate SH Wahlbezirk data to municipality level
sh_aggregate <- function(df, vote_cols) {
  ags_list <- unique(df$ags)
  result <- data.frame(row.names = seq_along(ags_list))
  result$ags <- ags_list
  result$ags_name <- df$ags_name[match(ags_list, df$ags)]

  num_cols <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes", vote_cols)
  num_cols <- intersect(num_cols, names(df))
  for (nc in num_cols) {
    vals <- tapply(df[[nc]], df$ags, function(x) {
      if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
    })
    result[[nc]] <- as.numeric(vals[result$ags])
  }
  result
}

#' Parse SH 1998/2003/2008 (no headers, fixed column positions)
parse_sh_early <- function(filepath, year) {
  cat("  Reading SH", year, "...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Party mappings (from Infodat documentation)
  party_maps <- list(
    "1998" = c("spd", "cdu", "gruene", "fdp", "ssw",
               "oedp", "naturgesetz", "statt_partei", "waehlergruppen", "einzelbewerber"),
    "2003" = c("spd", "cdu", "fdp", "gruene", "ssw",
               "linke_pds", "die_buergerpartei", "schill", "waehlergruppen", "einzelbewerber"),
    "2008" = c("cdu", "spd", "fdp", "gruene", "ssw", "npd",
               "linke_pds", "demokraten", "verbraucherschutz", "zentrum",
               "waehlergruppen", "einzelbewerber")
  )
  pmap <- party_maps[[as.character(year)]]

  # Fixed column positions: cols 1-14 = metadata, 15-18 = EV, 19-22 = voters,
  # 23 = invalid, 24 = valid, 25+ = party votes
  stat_col <- 1
  name_col <- 3  # Gemeinde name
  ev_col <- 18   # Wahlberechtigte insgesamt (A)
  voter_col <- 22  # Wähler insgesamt (B)
  invalid_col <- 23  # ungültige Stimmen (C)
  valid_col <- 24  # gültige Stimmen (D)
  party_start <- 25

  stat_codes <- as.character(raw[[stat_col]])
  # Filter to valid data rows (8-digit or 7-digit stat codes)
  valid_rows <- !is.na(stat_codes) & grepl("^\\d{7,8}$", stat_codes)
  data <- raw[valid_rows, ]

  df <- data.frame(
    ags = sh_stat_to_ags(data[[stat_col]]),
    ags_name = as.character(data[[name_col]]),
    eligible_voters = as.numeric(as.character(data[[ev_col]])),
    number_voters = as.numeric(as.character(data[[voter_col]])),
    invalid_votes = as.numeric(as.character(data[[invalid_col]])),
    valid_votes = as.numeric(as.character(data[[valid_col]])),
    stringsAsFactors = FALSE
  )

  # Party votes
  for (k in seq_along(pmap)) {
    col_idx <- party_start + k - 1
    if (col_idx > ncol(data)) break
    vals <- as.character(data[[col_idx]])
    vals[vals == "x" | vals == "-"] <- NA_character_
    df[[pmap[k]]] <- as.numeric(vals)
  }

  # Aggregate to municipality
  vote_cols <- pmap[pmap %in% names(df)]
  agg <- sh_aggregate(df, vote_cols)

  # Compute shares
  for (pc in vote_cols) {
    agg[[pc]] <- ifelse(!is.na(agg$valid_votes) & agg$valid_votes > 0,
                        agg[[pc]] / agg$valid_votes, NA_real_)
  }
  agg$turnout <- ifelse(!is.na(agg$eligible_voters) & agg$eligible_voters > 0,
                        agg$number_voters / agg$eligible_voters, NA_real_)
  agg$county <- substr(agg$ags, 1, 5)
  agg$state <- "01"
  agg$election_year <- year

  cat("    ->", nrow(agg), "Gemeinden\n")
  as_tibble(agg)
}

#' Parse SH 2013 (XLSX with headers in row 1)
parse_sh_2013 <- function(filepath) {
  cat("  Reading SH 2013...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Row 1 = headers, row 2 = field codes, row 3+ = data
  headers <- clean_header(as.character(raw[1, ]))
  codes <- as.character(raw[2, ])
  data <- raw[3:nrow(raw), ]

  # Find key columns by field code
  stat_col <- which(codes == "A1")[1] - 3  # stat code is a few cols before A1
  # Actually, col 1 is stat. Kennziffer based on structure
  stat_col <- 1
  name_col <- 3  # Gemeinde

  ev_col <- which(codes == "A")[1]
  voter_col <- which(codes == "B")[1]
  invalid_col <- which(codes == "C")[1]
  valid_col <- which(codes == "D")[1]

  # Party columns: D1, D2, ... from the field codes
  d_cols <- grep("^D\\d+$", codes)
  party_names_raw <- headers[d_cols]

  stat_codes <- as.character(data[[stat_col]])
  valid_rows <- !is.na(stat_codes) & grepl("^\\d{7,8}$", stat_codes)
  data <- data[valid_rows, ]

  df <- data.frame(
    ags = sh_stat_to_ags(data[[stat_col]]),
    ags_name = as.character(data[[name_col]]),
    eligible_voters = as.numeric(as.character(data[[ev_col]])),
    number_voters = as.numeric(as.character(data[[voter_col]])),
    invalid_votes = as.numeric(as.character(data[[invalid_col]])),
    valid_votes = as.numeric(as.character(data[[valid_col]])),
    stringsAsFactors = FALSE
  )

  # Party votes
  vote_col_names <- c()
  for (k in seq_along(d_cols)) {
    pname_raw <- party_names_raw[k]
    if (is.na(pname_raw) || pname_raw == "") next
    pname <- normalise_party_cty(tolower(pname_raw))
    vals <- as.character(data[[d_cols[k]]])
    vals[vals == "x" | vals == "-"] <- NA_character_
    if (pname %in% names(df)) {
      # Duplicate: individual candidate/group → sum into existing
      old <- df[[pname]]
      new <- as.numeric(vals)
      df[[pname]] <- ifelse(is.na(old) & is.na(new), NA_real_,
                            ifelse(is.na(old), 0, old) + ifelse(is.na(new), 0, new))
    } else {
      df[[pname]] <- as.numeric(vals)
      vote_col_names <- c(vote_col_names, pname)
    }
  }

  # Aggregate to municipality
  agg <- sh_aggregate(df, vote_col_names)

  for (pc in vote_col_names) {
    agg[[pc]] <- ifelse(!is.na(agg$valid_votes) & agg$valid_votes > 0,
                        agg[[pc]] / agg$valid_votes, NA_real_)
  }
  agg$turnout <- ifelse(!is.na(agg$eligible_voters) & agg$eligible_voters > 0,
                        agg$number_voters / agg$eligible_voters, NA_real_)
  agg$county <- substr(agg$ags, 1, 5)
  agg$state <- "01"
  agg$election_year <- 2013L

  cat("    ->", nrow(agg), "Gemeinden\n")
  as_tibble(agg)
}

#' Parse SH 2018 (XLSX with headers in row 4, data from row 7)
parse_sh_2018 <- function(filepath) {
  cat("  Reading SH 2018...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Row 4 = party names, row 6 = field codes, row 7+ = data
  headers <- clean_header(as.character(raw[4, ]))
  codes <- as.character(raw[6, ])
  data <- raw[7:nrow(raw), ]

  stat_col <- 1
  name_col <- 3

  ev_col <- which(codes == "A")[1]
  voter_col <- which(codes == "B")[1]
  invalid_col <- which(codes == "C")[1]
  valid_col <- which(codes == "D")[1]

  d_cols <- grep("^D\\d+$", codes)
  party_names_raw <- headers[d_cols]

  stat_codes <- as.character(data[[stat_col]])
  valid_rows <- !is.na(stat_codes) & grepl("^\\d{7,8}$", stat_codes)
  data <- data[valid_rows, ]

  df <- data.frame(
    ags = sh_stat_to_ags(data[[stat_col]]),
    ags_name = as.character(data[[name_col]]),
    eligible_voters = as.numeric(as.character(data[[ev_col]])),
    number_voters = as.numeric(as.character(data[[voter_col]])),
    invalid_votes = as.numeric(as.character(data[[invalid_col]])),
    valid_votes = as.numeric(as.character(data[[valid_col]])),
    stringsAsFactors = FALSE
  )

  vote_col_names <- c()
  for (k in seq_along(d_cols)) {
    pname_raw <- party_names_raw[k]
    if (is.na(pname_raw) || pname_raw == "") next
    pname <- normalise_party_cty(tolower(pname_raw))
    vals <- as.character(data[[d_cols[k]]])
    vals[vals == "x" | vals == "-"] <- NA_character_
    if (pname %in% names(df)) {
      old <- df[[pname]]
      new <- as.numeric(vals)
      df[[pname]] <- ifelse(is.na(old) & is.na(new), NA_real_,
                            ifelse(is.na(old), 0, old) + ifelse(is.na(new), 0, new))
    } else {
      df[[pname]] <- as.numeric(vals)
      vote_col_names <- c(vote_col_names, pname)
    }
  }

  agg <- sh_aggregate(df, vote_col_names)

  for (pc in vote_col_names) {
    agg[[pc]] <- ifelse(!is.na(agg$valid_votes) & agg$valid_votes > 0,
                        agg[[pc]] / agg$valid_votes, NA_real_)
  }
  agg$turnout <- ifelse(!is.na(agg$eligible_voters) & agg$eligible_voters > 0,
                        agg$number_voters / agg$eligible_voters, NA_real_)
  agg$county <- substr(agg$ags, 1, 5)
  agg$state <- "01"
  agg$election_year <- 2018L

  cat("    ->", nrow(agg), "Gemeinden\n")
  as_tibble(agg)
}

#' Parse SH 2023 (CSV with per-Kreis party mapping from Feldbezeichner)
parse_sh_2023 <- function(csv_path, fb_path) {
  cat("  Reading SH 2023...\n")

  # Read Feldbezeichner: maps D-field codes to party names per Kreis
  suppressMessages(
    fb <- read_excel(fb_path, sheet = "Kreise")
  )
  # Forward-fill Kreis name and Regionalschlüssel
  for (i in 2:nrow(fb)) {
    if (is.na(fb$Kreis[i])) {
      fb$Kreis[i] <- fb$Kreis[i - 1]
      fb$Regionalschlüssel[i] <- fb$Regionalschlüssel[i - 1]
    }
  }
  # Build per-Kreis mapping: list of (D-code → party_name)
  fb$kreis_code <- sprintf("%05d", as.integer(fb$Regionalschlüssel))
  fb$pname <- normalise_party_cty(tolower(trimws(fb$`Wahlvorschlag Kurzbezeichnung`)))

  # Read CSV (Latin-1 encoding, semicolon-delimited)
  csv_df <- read.csv2(csv_path, header = TRUE, stringsAsFactors = FALSE,
                       fileEncoding = "latin1", na.strings = c("", "NA"))
  headers <- names(csv_df)

  # Key columns
  stat_col <- which(headers == "Erfassungsgebietsnummer")[1]
  ev_col <- which(grepl("Wahlberechtigte.gesamt", headers))[1]
  voter_col <- which(grepl("Waehlende.gesamt", headers))[1]
  invalid_col <- which(grepl("ungueltige", headers))[1]
  valid_col <- which(grepl("gueltige", headers) & !grepl("ungueltige", headers))[1]

  # D-field columns
  d_col_indices <- grep("^D\\d+$", headers)
  d_field_names <- headers[d_col_indices]

  # Build base data frame
  stat_codes <- as.character(csv_df[[stat_col]])
  valid_rows <- !is.na(stat_codes) & grepl("^\\d{7,8}$", stat_codes)
  data <- csv_df[valid_rows, ]

  ags_vals <- sh_stat_to_ags(data[[stat_col]])
  kreis_codes <- substr(ags_vals, 1, 5)
  # Only keep Landkreise (01051-01062), not kreisfreie Städte (01001-01004)
  is_landkreis <- kreis_codes %in% unique(fb$kreis_code)
  data <- data[is_landkreis, ]
  ags_vals <- ags_vals[is_landkreis]
  kreis_codes <- kreis_codes[is_landkreis]

  df <- data.frame(
    ags = ags_vals,
    ags_name = NA_character_,
    kreis_code = kreis_codes,
    eligible_voters = as.numeric(as.character(data[[ev_col]])),
    number_voters = as.numeric(as.character(data[[voter_col]])),
    invalid_votes = as.numeric(as.character(data[[invalid_col]])),
    valid_votes = as.numeric(as.character(data[[valid_col]])),
    stringsAsFactors = FALSE
  )

  # For each D-field, look up party name per Kreis and assign votes
  all_pnames <- c()
  for (dc in seq_along(d_col_indices)) {
    d_field <- d_field_names[dc]
    vals <- as.numeric(as.character(data[[d_col_indices[dc]]]))

    # Look up party name for each row based on its Kreis
    for (kr in unique(df$kreis_code)) {
      kr_mask <- df$kreis_code == kr
      fb_row <- fb[fb$kreis_code == kr & fb$Feld == d_field, ]
      if (nrow(fb_row) == 0) next  # this D-field not used in this Kreis

      pname <- fb_row$pname[1]
      if (pname %in% names(df)) {
        df[[pname]][kr_mask] <- ifelse(
          is.na(df[[pname]][kr_mask]) & is.na(vals[kr_mask]), NA_real_,
          ifelse(is.na(df[[pname]][kr_mask]), 0, df[[pname]][kr_mask]) +
            ifelse(is.na(vals[kr_mask]), 0, vals[kr_mask])
        )
      } else {
        df[[pname]] <- NA_real_
        df[[pname]][kr_mask] <- vals[kr_mask]
        all_pnames <- c(all_pnames, pname)
      }
    }
  }
  all_pnames <- unique(all_pnames)

  # Aggregate to municipality
  df$kreis_code <- NULL
  agg <- sh_aggregate(df, all_pnames)

  for (pc in all_pnames) {
    if (pc %in% names(agg)) {
      agg[[pc]] <- ifelse(!is.na(agg$valid_votes) & agg$valid_votes > 0,
                          agg[[pc]] / agg$valid_votes, NA_real_)
    }
  }
  agg$turnout <- ifelse(!is.na(agg$eligible_voters) & agg$eligible_voters > 0,
                        agg$number_voters / agg$eligible_voters, NA_real_)
  agg$county <- substr(agg$ags, 1, 5)
  agg$state <- "01"
  agg$election_year <- 2023L

  cat("    ->", nrow(agg), "Gemeinden\n")
  as_tibble(agg)
}

# Process SH files
sh_results <- list()
for (yr in c(1998, 2003)) {
  sh_results[[as.character(yr)]] <- tryCatch(
    parse_sh_early(file.path(sh_dir, paste0("Schleswig-Holstein_", yr, "_Kreistagswahl.xls")), yr),
    error = function(e) { cat("  SH", yr, "ERROR:", conditionMessage(e), "\n"); NULL }
  )
}
sh_results[["2008"]] <- tryCatch(
  parse_sh_early(file.path(sh_dir, "Schleswig-Holstein_2008_Kreiswahl_Gemeindewahl.xls"), 2008),
  error = function(e) { cat("  SH 2008 ERROR:", conditionMessage(e), "\n"); NULL }
)
sh_results[["2013"]] <- tryCatch(
  parse_sh_2013(file.path(sh_dir, "Schleswig-Holstein_2013_Kreistagswahl.xlsx")),
  error = function(e) { cat("  SH 2013 ERROR:", conditionMessage(e), "\n"); NULL }
)
sh_results[["2018"]] <- tryCatch(
  parse_sh_2018(file.path(sh_dir, "Schleswig-Holstein_2018_Kreiswahl_Gemeindewahl.xlsx")),
  error = function(e) { cat("  SH 2018 ERROR:", conditionMessage(e), "\n"); NULL }
)
sh_results[["2023"]] <- tryCatch(
  parse_sh_2023(
    file.path(sh_dir, "Schleswig-Holstein_2023_Kreiswahl_Gemeindewahl.csv"),
    file.path(sh_dir, "Schleswig-Holstein_2023_Kreiswahl_Feldbezeichner.xlsx")
  ),
  error = function(e) { cat("  SH 2023 ERROR:", conditionMessage(e), "\n"); NULL }
)

sh_results <- sh_results[!sapply(sh_results, is.null)]
df_sh <- bind_rows(sh_results)
cat("SH total:", nrow(df_sh), "rows x", ncol(df_sh), "cols\n")
cat("SH years:", paste(sort(unique(df_sh$election_year)), collapse = ", "), "\n")
df_sh |> count(election_year) |> print()


# =============================================================================
# Combine all states and write output
# =============================================================================

cat("\n===== COMBINING =====\n")

all_dfs <- list(df_st, df_th, df_mv, df_sn)
if (!is.null(df_bb)) all_dfs <- c(all_dfs, list(df_bb))
if (exists("df_by") && nrow(df_by) > 0) all_dfs <- c(all_dfs, list(df_by))
if (exists("df_sl") && nrow(df_sl) > 0) all_dfs <- c(all_dfs, list(df_sl))
if (exists("df_he") && nrow(df_he) > 0) all_dfs <- c(all_dfs, list(df_he))
if (exists("df_bw") && nrow(df_bw) > 0) all_dfs <- c(all_dfs, list(df_bw))
if (exists("df_sh") && nrow(df_sh) > 0) all_dfs <- c(all_dfs, list(df_sh))
df_all <- bind_rows(all_dfs)

# Ensure AGS, county, state are character with proper zero-padding
df_all <- df_all |>
  mutate(
    ags = sprintf("%08s", as.character(ags)),
    county = sprintf("%05s", as.character(county)),
    state = sprintf("%02s", as.character(state))
  )

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
