### County Elections (Kreistagswahlen): Municipality-Level Results
# Processes raw county council election data for ST, TH, BW
# Aggregates ballot-district data to municipality level where available
# Vincent Heddesheimer
# March 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "readxl", "haschaR")
conflict_prefer("filter", "dplyr")

# NB: this used to point at a nested `Kreistagswahlen/Kreistagswahlen/` copy that
# duplicated its parent. That copy is now marked com.dropbox.ignored (and
# gitignored) so it no longer syncs, leaving it empty — the raw files live in the
# directory below.
raw_dir <- "data/county_elections/raw/Kreistagswahlen"


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
    "wahlervereinigungen soweit nicht nebenstehend genannt" = "waehlervereinigungen",
    "w\u00e4hlervereinigungen soweit nicht nebenstehend genannt" = "waehlervereinigungen",
    "gemeinsame wahlvorschlage"   = "gemeinsame_wv",
    "gemeinsame wahlvorschl\u00e4ge" = "gemeinsame_wv",
    "gemeinsame wahlvorschlage1)" = "gemeinsame_wv",
    "gemeinsame wahlvorschl\u00e4ge1)" = "gemeinsame_wv",
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

    # BW Kreistagswahl 2024 (GENESIS 14411): joint-list Wahlvorschl\u00e4ge of the
    # form "<party> und W\u00e4hlervereinigungen" fold into the base party (matching
    # the parse_bw_format_b own-list + joint-list summing convention); the
    # residual local-list bloc maps to the long-running waehlervereinigungen col.
    "sonstige w\u00e4hlervereinigungen"       = "waehlervereinigungen",
    "cdu und w\u00e4hlervereinigungen"        = "cdu",
    "fdp und w\u00e4hlervereinigungen"        = "fdp",
    "gr\u00fcne und w\u00e4hlervereinigungen" = "gruene",
    "die linke und w\u00e4hlervereinigungen"  = "linke_pds",
    "\u00f6dp und w\u00e4hlervereinigungen"   = "oedp",
    "dkp und w\u00e4hlervereinigungen"        = "dkp",
    "linksorientierte listen"                 = "linksorientierte_listen",
    "klimaliste"                              = "klimaliste",
    "volt - \u00f6dp"                         = "volt_oedp",
    "liste der jungen union"                  = "junge_union",
    "statt partei die unabh\u00e4ngigen"      = "statt_partei",
    "die grauen - graue panther"              = "graue",
    "die gerechtigkeitspartei - team todenh\u00f6fer" = "team_todenhofer",

    # NI-specific
    "b\u00fcndnis c"              = "buendnis_c",
    "bundnis c"                   = "buendnis_c",
    "die basis lv"                = "die_basis",
    "tierschutz-partei"           = "tierschutz",
    "tierschutz- partei"          = "tierschutz",
    "partei d. vernunft"          = "partei_vernunft",
    "pogo"                        = "pogo",
    "sfp"                         = "sfp",
    "nlp"                         = "nlp",
    "agp"                         = "agp",
    "mdu"                         = "mdu",
    "dmd"                         = "dmd",
    "diegede"                     = "diegede",
    "deut"                        = "deut",
    "die friesen"                 = "die_friesen",
    "die haie"                    = "die_haie",
    "die demokraten"              = "die_demokraten",
    "dib"                         = "dib",
    "big"                         = "big",
    "eine welt"                   = "eine_welt",
    "neue liberale"               = "neue_liberale",
    "du."                         = "du",

    # NRW-specific
    "pro nrw"                     = "pro_nrw",
    "pro deutschland"             = "pro_deutschland",
    "die rechte"                  = "die_rechte",
    "die violetten"               = "die_violetten",
    "tierschutz hier!"            = "tierschutz_hier",
    "aufbruch c"                  = "aufbruch_c",
    "b\u00fcndnis c"              = "buendnis_c",
    "basisdemokratie jetzt"       = "basisdemokratie_jetzt",
    "v-partei\u00b3"              = "v_partei3",
    "v-partei3"                   = "v_partei3",
    "so!"                         = "so",
    "volksabstimmung"             = "volksabstimmung",
    "unregierbare"                = "unregierbare",
    "deutschland"                 = "deutschland",
    "nichtwahler"                 = "nichtwaehler",
    "nichtw\u00e4hler"            = "nichtwaehler",
    "deutsch-land"                = "deutschland",
    "einzel-bewerber"             = "einzelbewerber",
    "einzelbewerber/-innen"       = "einzelbewerber",
    "die tierschutz-partei"       = "tierschutz",
    "die tierschutzpartei"        = "tierschutz",
    "\u00f6koli"                  = "oekoli",
    "okoli"                       = "oekoli",
    "offensived"                  = "offensive_d",
    "freie union"                 = "freie_union",
    "dmp"                         = "dmp",
    "bfb"                         = "bfb",
    "dsp"                         = "dsp",
    "dos"                         = "dos",
    "msp"                         = "msp",
    "pdf"                         = "pdf",
    "arminius"                    = "arminius",
    "duw"                         = "duw",
    "amp"                         = "amp",
    "pbp"                         = "pbp",
    "fakt"                        = "fakt",
    "uap"                         = "uap",
    "demokratie"                  = "demokratie",
    "einheit"                     = "einheit",

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

th_dir <- file.path(raw_dir, "Thüringen")

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
  list(year = 2004, file = "Thüringen_2004_Kreistagswahl.xlsx"),
  list(year = 2009, file = "Thüringen_2009_Kreistagswahl.xlsx"),
  list(year = 2014, file = "Thüringen_2014_Kreistagswahl.xlsx"),
  list(year = 2019, file = "Thüringen_2019_Kreistagswahl.xlsx"),
  list(year = 2021, file = "Thüringen_2021_Kreistagswahl.xlsx"),
  list(year = 2024, file = "Thüringen_2024_Kreistagswahl.xlsx")
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

# --- Amt-level pooled postal votes -------------------------------------------
# Line 3 of the MV CSVs states: "Bei Feststellung des Briefwahlergebnisses auf
# Amtsebene wird der Gemeindeschlüssel ab Stelle 6 geändert und um 7 und die
# letzten beiden Ziffern des Amtsschlüssels ergänzt." Those pool rows carry
# Wahlberechtigte == 0 and a Gemeindename of "Briefwahl <Amt>", so the
# `eligible_voters > 0` municipality filter used to delete them outright:
# 33,722 voters / 98,888 valid votes in 2024 (statewide turnout 61.71% shipped
# vs 64.21% true) and 8,581 / 25,461 in 2019, concentrated in the 24 (resp. 8)
# affected Ämter — Warnow-West showed 54.6% turnout instead of 76.4%
# (audit fix C-17, 2026-07). MV 2014 has no pool rows.
#
# Allocation mirrors bb_allocate_postal: split each pool over its Amt's member
# Gemeinden by eligible-voter share (MV publishes no Wahlschein counts), with
# largest-remainder rounding so sum(parties) == gueltige_stimmen still holds.
# The pool must be matched on the (Kreisname, Amtsname) pair — the Amt CODE
# cannot be used, because the pool rows carry a 4-digit Amtsschlüssel while the
# member rows carry the short form.
#
# The allocation is an estimate, so a Gemeinde whose urn turnout is already very
# high can be pushed past 100%: in 2024 exactly two do (Schossin 13076121 at
# 105.0% and Zülow 13076163 at 100.9%, both < 200 eligible voters and already
# at 86% / 82% before allocation). They are left as they are — capping would
# break sum(parties) == valid_votes and the Amt and Kreis totals. Amt-level
# aggregates are exact: Warnow-West lands on 76.4%, matching the raw file.

#' Distribute Amt-level pooled postal votes over the pool's member Gemeinden.
#' @param df_muni municipality-level data with ABSOLUTE counts
#' @param pool_map data.frame(pool, ags) linking each pool AGS to its members
mv_allocate_postal <- function(df_muni, pool_map, dist_cols, party_names, year) {
  if (nrow(pool_map) == 0) return(df_muni)

  pools <- df_muni[df_muni$ags %in% unique(pool_map$pool), , drop = FALSE]
  members <- df_muni[!(df_muni$ags %in% unique(pool_map$pool)), , drop = FALSE]
  pool_map <- pool_map[pool_map$ags %in% members$ags, , drop = FALSE]

  missing <- setdiff(unique(pools$ags), unique(pool_map$pool))
  if (length(missing) > 0) {
    stop("MV ", year, ": postal pool(s) with no member Gemeinde: ",
         paste(missing, collapse = ", "))
  }

  pool_map$w <- members$eligible_voters[match(pool_map$ags, members$ags)]
  pool_map$w[is.na(pool_map$w)] <- 0
  wsum <- tapply(pool_map$w, pool_map$pool, sum, na.rm = TRUE)
  if (any(wsum <= 0)) {
    stop("MV ", year, ": postal pool(s) with zero total eligible voters: ",
         paste(names(wsum)[wsum <= 0], collapse = ", "))
  }
  pool_map$share <- pool_map$w / unname(wsum[pool_map$pool])

  targets <- sort(unique(pool_map$ags))
  addm <- vapply(dist_cols, function(cl) {
    pv <- pools[[cl]][match(pool_map$pool, pools$ags)]
    a <- tapply(pv * pool_map$share, pool_map$ags, sum, na.rm = TRUE)
    unname(a[targets])
  }, numeric(length(targets)))
  addm <- matrix(addm, nrow = length(targets), dimnames = list(targets, dist_cols))
  addm[is.na(addm)] <- 0

  # Largest-remainder rounding of the party additions, so they still sum to the
  # municipality's added valid votes (see bb_allocate_postal for the rationale).
  pcols <- intersect(party_names, dist_cols)
  for (i in seq_len(nrow(addm))) {
    tgt <- round(addm[i, "gueltige_stimmen"])
    v <- addm[i, pcols]
    fl <- floor(v)
    need <- tgt - sum(fl)
    if (need > 0) {
      ord <- order(v - fl, decreasing = TRUE)
      fl[ord[seq_len(min(need, length(fl)))]] <- fl[ord[seq_len(min(need, length(fl)))]] + 1
    } else if (need < 0) {
      ord <- order(v - fl, decreasing = FALSE)
      take <- which(fl[ord] > 0)[seq_len(min(-need, sum(fl > 0)))]
      fl[ord[take]] <- fl[ord[take]] - 1
    }
    addm[i, pcols] <- fl
    addm[i, "gueltige_stimmen"] <- tgt
  }
  for (cl in setdiff(dist_cols, c(pcols, "gueltige_stimmen"))) {
    addm[, cl] <- round(addm[, cl])
  }

  idx <- match(targets, members$ags)
  for (cl in dist_cols) {
    base <- members[[cl]][idx]
    add <- addm[, cl]
    # NA means "this party did not stand here": keep NA only when the pool adds
    # nothing either, otherwise the pooled votes become its count.
    members[[cl]][idx] <- ifelse(is.na(base) & add == 0, NA_real_,
                                 ifelse(is.na(base), add, base + add))
  }

  cat(sprintf("    allocated %d Amt-level postal pools (%d voters) over %d municipalities\n",
              nrow(pools), round(sum(pools$number_voters, na.rm = TRUE)),
              length(targets)))
  members
}

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

  # Build the postal-pool -> member map while Kreisname/Amtsname are still
  # available (the aggregation below drops them). See mv_allocate_postal.
  amt_key <- paste(trimws(as.character(df$Kreisname)),
                   trimws(as.character(df$Amtsname)), sep = "|")
  is_pool <- substr(df$ags, 6, 6) == "7" &
    !is.na(df$eligible_voters) & df$eligible_voters == 0
  if (any(is_pool) && !all(grepl("^Briefwahl", trimws(as.character(df$ags_name[is_pool]))))) {
    stop("MV ", year, ": rows keyed as Amt-level postal pools that are not ",
         "named 'Briefwahl …' — check the Gemeindeschlüssel convention")
  }
  is_member <- !is_pool & !is.na(df$eligible_voters) & df$eligible_voters > 0 &
    nchar(df$ags) == 8
  pool_map <- do.call(rbind, lapply(which(is_pool), function(i) {
    m <- which(is_member & amt_key == amt_key[i])
    if (length(m) == 0) {
      stop("MV ", year, ": postal pool ", df$ags[i], " ('", df$ags_name[i],
           "') matches no Gemeinde in Amt '", amt_key[i], "'")
    }
    data.frame(pool = df$ags[i], ags = unique(df$ags[m]),
               stringsAsFactors = FALSE)
  }))
  if (is.null(pool_map)) {
    pool_map <- data.frame(pool = character(0), ags = character(0),
                           stringsAsFactors = FALSE)
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

  # Spread the Amt-level postal pools over their member Gemeinden and drop the
  # pool rows, BEFORE turnout and vote shares are derived.
  df_muni <- mv_allocate_postal(
    df_muni, pool_map,
    dist_cols = c("number_voters", "invalid_votes", "gueltige_stimmen", party_cols),
    party_names = party_cols, year = year
  )

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

# No Amt-level postal pool may survive as its own row, and none may have been
# silently dropped: after allocation the statewide voter total must match the
# raw file including the pools (audit fix C-17).
if (any(substr(df_mv$ags, 6, 6) == "7")) {
  stop("MV: postal pool rows reached the output: ",
       paste(unique(df_mv$ags[substr(df_mv$ags, 6, 6) == "7"]), collapse = ", "))
}
mv_expected_voters <- c("2019" = 779741, "2024" = 867871)
for (yr in names(mv_expected_voters)) {
  got <- sum(df_mv$number_voters[df_mv$election_year == as.integer(yr)], na.rm = TRUE)
  if (abs(got - mv_expected_voters[[yr]]) > 25) {
    stop("MV ", yr, ": statewide number_voters = ", got, ", expected ",
         mv_expected_voters[[yr]], " (pooled postal votes lost or double-counted)")
  }
}

cat("MV total:", nrow(df_mv), "rows x", ncol(df_mv), "cols\n")
cat("MV years:", paste(sort(unique(df_mv$election_year)), collapse = ", "), "\n")
df_mv |> count(election_year) |> print()


# =============================================================================
# SACHSEN (SN)
# =============================================================================

cat("\n===== SACHSEN =====\n")

sn_dir <- file.path(raw_dir, "Sachsen")

# --- Große Kreisstädte split across Wahlkreise --------------------------------
# Sachsen publishes its Große Kreisstädte only as per-Wahlkreis part rows -- in
# the legacy files as "<AGS>-<n>" ("14524330-1" = Zwickau 1), in the 2019/2024
# GE_TG sheet as a 9-digit code ("145243301"). A parent 8-digit row NEVER
# exists, so the `nchar(ags) == 8` municipality filter used to drop the entire
# city: 10 cities / 363,834 eligible voters in 2024 (16.8% of the Kreistag
# electorate) and 11 / 406,357 in 2019 (audit fix C-14, 2026-07).
# `sn_mark_parts()` rewrites the part AGS to the parent and flags the row;
# `sn_sum_parts()` then sums the count columns per parent. Both must run BEFORE
# vote shares / turnout are computed, so the shares follow from the summed
# counts rather than being averaged.

#' Rewrite Wahlkreis part codes to the parent AGS and flag them.
#' @param ags character vector of raw Ortnummern
#' @param pattern anchored regex matching a whole part code, with the parent
#'   AGS as capture group 1 ("^(\\d{8})-\\d+$" legacy, "^(\\d{8})\\d$" modern)
sn_mark_parts <- function(ags, pattern) {
  ags <- as.character(ags)
  is_part <- !is.na(ags) & grepl(pattern, ags)
  parent <- ags
  parent[is_part] <- sub(pattern, "\\1", ags[is_part])
  list(ags = parent, is_part = is_part)
}

#' Strip the Wahlkreis decoration off a part name to recover the city name.
#' "Freiberg 1" / "Plauen, Stadt (WK 10)" / "Bautzen 1 (WK 3)" /
#' "Meißen (linkselbisch)" / "Radebeul (West)" / "Riesa II" -> the bare name.
sn_parent_name <- function(x) {
  nm <- trimws(as.character(x[1]))
  for (i in 1:2) {
    nm <- sub("\\s*\\(WK\\s*[0-9]+\\)$", "", nm)
    nm <- sub("\\s*\\((West|Ost|Nord|Süd|Sud|linkselbisch|rechtselbisch)\\)$",
              "", nm)
    nm <- sub("\\s+([0-9]+|I{1,3}|IV|VI{0,3})$", "", nm)
  }
  trimws(nm)
}

#' Sum the Wahlkreis part rows of one municipality back onto a single row.
sn_sum_parts <- function(df, count_cols) {
  if (!any(df$is_wk_part)) {
    df$is_wk_part <- NULL
    return(df)
  }
  parts <- df[df$is_wk_part, , drop = FALSE]
  rest <- df[!df$is_wk_part, , drop = FALSE]

  # A parent that also appears as a stand-alone row would mean double counting.
  dbl <- intersect(unique(parts$ags), unique(rest$ags))
  if (length(dbl) > 0) {
    stop("SN: Wahlkreis parts and a parent row for the same AGS: ",
         paste(dbl, collapse = ", "))
  }

  agg <- do.call(rbind, lapply(split(parts, parts$ags), function(g) {
    out <- g[1, , drop = FALSE]
    for (cc in count_cols) {
      out[[cc]] <- if (all(is.na(g[[cc]]))) NA_real_ else sum(g[[cc]], na.rm = TRUE)
    }
    out$ags_name <- sn_parent_name(g$ags_name)
    out
  }))

  cat("    aggregated", nrow(parts), "Wahlkreis part rows ->",
      nrow(agg), "Große Kreisstädte\n")
  out <- rbind(rest, agg)
  out$is_wk_part <- NULL
  out[order(out$ags), , drop = FALSE]
}

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

  # Große Kreisstädte come as "<AGS>-<n>" Wahlkreis parts (see sn_mark_parts)
  marked <- sn_mark_parts(df$ags, "^(\\d{8})-\\d+$")
  df$ags <- marked$ags
  df$is_wk_part <- marked$is_part

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

  # Sum the Wahlkreis parts onto their parent BEFORE shares are computed
  df <- sn_sum_parts(df, all_numeric)

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
  ungueltig_col <- which(grepl("^ung.ltige Stimmzettel$", headers))[1]
  # NOTE: these patterns MUST be anchored at both ends. An unanchored
  # "g.ltige Stimmzettel$" also matches "ungültige Stimmzettel", which sits in
  # the earlier column, so which(...)[1] silently returned the INVALID-ballot
  # column and valid_votes came out ~40x too small (audit fix C-13, 2026-07).
  gueltig_sz_col <- which(grepl("^g.ltige Stimmzettel$", headers))[1]
  gueltig_st_col <- which(grepl("^g.ltige Stimmen$", headers))[1]
  stopifnot(
    !is.na(ungueltig_col), !is.na(gueltig_sz_col), !is.na(gueltig_st_col),
    gueltig_sz_col != ungueltig_col
  )

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

  # Große Kreisstädte come as 9-digit Wahlkreis parts (see sn_mark_parts)
  marked <- sn_mark_parts(df$ags, "^(\\d{8})\\d$")
  df$ags <- marked$ags
  df$is_wk_part <- marked$is_part

  # Sum the Wahlkreis parts onto their parent BEFORE shares are computed
  df <- sn_sum_parts(df, all_numeric)

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

# Guard against the anchored-header regression (C-13): valid_votes used to be
# read from the "ungültige Stimmzettel" column, making valid < invalid.
sn_bad <- df_sn |>
  filter(!is.na(valid_votes), !is.na(invalid_votes), valid_votes <= invalid_votes)
if (nrow(sn_bad) > 0) {
  print(head(sn_bad[, c("ags", "ags_name", "election_year",
                        "number_voters", "invalid_votes", "valid_votes")], 10))
  stop("SN: ", nrow(sn_bad), " rows with valid_votes <= invalid_votes — ",
       "the gültige/ungültige Stimmzettel columns are mixed up")
}
# Große Kreisstädte must survive the 8-digit filter (C-14)
sn_gks <- c("14524330", "14523320", "14626110", "14628110", "14628270",
            "14522180", "14625020", "14627210", "14627230", "14627140")
for (yr in c(2019, 2024)) {
  miss <- setdiff(sn_gks, df_sn$ags[df_sn$election_year == yr])
  if (length(miss) > 0) {
    stop("SN ", yr, ": Große Kreisstädte missing from output: ",
         paste(miss, collapse = ", "))
  }
}

cat("SN total:", nrow(df_sn), "rows x", ncol(df_sn), "cols\n")
cat("SN years:", paste(sort(unique(df_sn$election_year)), collapse = ", "), "\n")
df_sn |> count(election_year) |> print()


# =============================================================================
# BRANDENBURG (BB)
# =============================================================================

cat("\n===== BRANDENBURG =====\n")

bb_dir <- file.path(raw_dir, "Brandenburg")

#' Give every ballot district of one AGS the same municipality name, so that
#' aggregating by (ags, ags_name) cannot split a municipality across groups.
#' Postal-vote rows labelled "Briefwahl" never win the vote for the name.
canonical_ags_name <- function(ags, ags_name) {
  lookup <- tapply(ags_name, ags, function(x) {
    real <- x[!is.na(x) & x != "Briefwahl"]
    if (length(real) > 0) real[1] else NA_character_
  })
  unname(lookup[ags])
}

# --- Postal-vote allocation --------------------------------------------------
# Brandenburg pools postal ballots into districts that carry no municipality
# AGS, so they cannot be aggregated to a Gemeinde as they stand. This setting
# controls what happens to them. See docs/data_pipeline.md §8 for the evidence
# behind each option and for how much of the vote each one recovers.
#
#   "all"  (default) allocate every pool back to its member municipalities.
#          2014/2019/2024 use an Amt-level pool and the Wahlschein count
#          (Wahlberechtigte A2) as the weight — the same key the federal
#          pipeline uses. 2003/2008 have no Wahlschein counts and pool
#          Kreis-wide, so they fall back to eligible-voter share, which
#          assumes uniform postal propensity AND uniform postal vote choice
#          across a whole Landkreis. That is a strong assumption: it is the
#          part of this setting to reconsider first.
#   "amt"  allocate only the Amt-level pools (2014/2019/2024); leave 2003/2008
#          Urnenwahl-only. Choose this if the Kreis-wide assumption is too
#          strong for your application.
#   "none" drop all pooled postal votes (behaviour before 2026-07-28).
#
# To change it, edit this one line and re-run 01 then 02.
bb_postal_allocation <- "all"
stopifnot(bb_postal_allocation %in% c("all", "amt", "none"))

#' Read the Gemeindeverzeichnis for one reference date and return, for
#' Brandenburg, both the Gemeindeverband ("Amt") names and each Gemeinde's
#' membership. Used to resolve 2014/2019 postal pools, whose rows name their
#' Amt but carry no municipality code.
bb_gv_verbaende <- function(gv_stem) {
  path <- file.path("data", "covars_municipality", "raw", "municipality_sizes",
                    paste0(gv_stem, "_Auszug_GV.xlsx"))
  suppressMessages(
    r <- read_excel(path, sheet = 2, col_names = FALSE, col_types = "text")
  )
  d <- data.frame(
    sa = unlist(r[[1]]), land = unlist(r[[3]]), rb = unlist(r[[4]]),
    kreis = unlist(r[[5]]), vb = unlist(r[[6]]), gem = unlist(r[[7]]),
    nm = unlist(r[[8]]), stringsAsFactors = FALSE
  )
  d <- d[!is.na(d$land) & d$land == "12", ]
  list(
    # Satzart 50 = the Gemeindeverband itself (name we match the pool against)
    verband = data.frame(kreis = d$kreis[d$sa == "50"], vb = d$vb[d$sa == "50"],
                         nm = d$nm[d$sa == "50"], stringsAsFactors = FALSE),
    # Satzart 60 = the Gemeinden, each carrying its Verband code
    gemeinde = data.frame(
      ags = paste0(d$land, d$rb, d$kreis, d$gem)[d$sa == "60"],
      kreis = d$kreis[d$sa == "60"], vb = d$vb[d$sa == "60"],
      stringsAsFactors = FALSE
    )
  )
}

#' Normalise an Amt / Gemeindeverband name for matching
bb_norm_name <- function(x) {
  x <- tolower(trimws(x))
  gsub("\\s+", " ", x)
}

# The 2014 file has three pools that span SEVERAL Verbände at once (their names
# list the members, partly abbreviated: "NWU" = Nordwestuckermark,
# "Boitz.L." = Boitzenburger Land). Name matching cannot resolve these, and
# matching on the numeric code would silently misassign them — e.g. pool
# 12073904 would land on Amt Gartz. They are pinned explicitly by Verband code
# within Kreis 12073 (Uckermark); codes verified against GV 31.12.2013.
bb_multi_pools_2014 <- list(
  "12073901" = c("5310", "5304"),                 # Oder-Welse, Gartz (Oder)
  "12073902" = c("0429", "0579", "5303", "5306"), # Nordwestuckermark, Uckerland,
                                                  #   Brüssow, Gramzow
  "12073904" = c("0384", "0069", "5305")          # Lychen, Boitzenburger Land,
                                                  #   Gerswalde
)

#' Map each postal pool to its member municipalities.
#' Returns a data.frame(pool, ags); one row per (pool, member Gemeinde).
#' Hard-fails if any pool cannot be resolved — a silently misassigned pool
#' would be worse than a dropped one.
bb_pool_members <- function(pool_ags, pool_names, muni_ags, year,
                            amt_by_ags = NULL) {
  # 2024: every row carries an Amtsnummer, so membership is read straight off
  # the file — no Gemeindeverzeichnis and no name matching needed.
  if (year == 2024) {
    out <- do.call(rbind, lapply(seq_along(pool_ags), function(i) {
      p <- pool_ags[i]
      kreis <- substr(p, 1, 5)
      members <- muni_ags[substr(muni_ags, 1, 5) == kreis &
                            !is.na(amt_by_ags[muni_ags]) &
                            amt_by_ags[muni_ags] == amt_by_ags[[p]]]
      if (length(members) == 0) {
        stop("BB 2024: pool ", p, " ('", pool_names[i],
             "') has no member municipalities with Amtsnummer ", amt_by_ags[[p]])
      }
      data.frame(pool = p, ags = members, stringsAsFactors = FALSE)
    }))
    return(out)
  }

  # 2003/2008: pools are Kreis-wide and anonymous ("Briefwahl", Gemeinde 900).
  # The only grouping the source identifies is the Landkreis.
  if (year %in% c(2003, 2008)) {
    out <- do.call(rbind, lapply(seq_along(pool_ags), function(i) {
      kreis <- substr(pool_ags[i], 1, 5)
      members <- muni_ags[substr(muni_ags, 1, 5) == kreis]
      if (length(members) == 0) {
        stop("BB ", year, ": no member municipalities for pool ", pool_ags[i])
      }
      data.frame(pool = pool_ags[i], ags = members, stringsAsFactors = FALSE)
    }))
    return(out)
  }

  # 2014/2019: pools name their Amt. Match by NAME, not by code — Amt
  # Unterspreewald changed its Verband code (5112 -> 5114) between GV vintages,
  # so code matching is not stable across the election/GV gap.
  gv_stem <- if (year == 2014) "31122013" else "31122017"
  gv <- bb_gv_verbaende(gv_stem)
  gv$verband$key <- paste0(gv$verband$kreis, "|", bb_norm_name(gv$verband$nm))

  out <- do.call(rbind, lapply(seq_along(pool_ags), function(i) {
    p <- pool_ags[i]
    kreis2 <- substr(p, 4, 5)   # 2-digit Kreis, as used in the GV
    vbs <- NULL

    if (!is.null(bb_multi_pools_2014[[p]]) && year == 2014) {
      vbs <- bb_multi_pools_2014[[p]]
    } else {
      amt <- sub("^\\s*(BW\\s+im\\s+)?Amt\\s+", "", pool_names[i])
      hit <- gv$verband$key == paste0(kreis2, "|", bb_norm_name(amt))
      if (!any(hit)) {
        stop("BB ", year, ": cannot resolve postal pool ", p,
             " ('", pool_names[i], "') to a Gemeindeverband in GV ", gv_stem)
      }
      vbs <- gv$verband$vb[hit]
    }

    members <- gv$gemeinde$ags[gv$gemeinde$kreis == kreis2 &
                                 gv$gemeinde$vb %in% vbs]
    members <- intersect(members, muni_ags)
    if (length(members) == 0) {
      stop("BB ", year, ": pool ", p, " ('", pool_names[i],
           "') resolved to no municipality present in the election data")
    }
    data.frame(pool = p, ags = members, stringsAsFactors = FALSE)
  }))
  out
}

#' Distribute pooled postal votes over the member municipalities of each pool.
#' `df_muni` is municipality-level with absolute counts; pooled rows are those
#' whose AGS is not a real municipality (they carry eligible_voters == 0).
#' Weight is the Wahlschein count (a2) where the source reports it, otherwise
#' eligible voters. Counts stay unrounded so that the vote shares derived
#' downstream still sum to exactly 1; the caller rounds the published totals.
bb_allocate_postal <- function(df_muni, year, party_names, dist_cols,
                               amt_by_ags = NULL) {
  if (bb_postal_allocation == "none") return(df_muni)
  if (bb_postal_allocation == "amt" && year %in% c(2003, 2008)) return(df_muni)

  is_muni <- nchar(df_muni$ags) == 8 & !is.na(df_muni$eligible_voters) &
    df_muni$eligible_voters > 0
  if (!any(!is_muni)) return(df_muni)

  members <- df_muni[is_muni, ]
  pools   <- df_muni[!is_muni, ]
  map <- bb_pool_members(pools$ags, pools$ags_name, members$ags, year,
                         amt_by_ags = amt_by_ags)

  # Weight: Wahlschein holders where reported (2014+), else eligible voters
  wcol <- if ("a2" %in% names(members) && sum(members$a2, na.rm = TRUE) > 0) {
    "a2"
  } else {
    "eligible_voters"
  }
  map$w <- members[[wcol]][match(map$ags, members$ags)]
  map$w[is.na(map$w)] <- 0
  wsum <- tapply(map$w, map$pool, sum, na.rm = TRUE)
  # A pool whose members report no weight at all cannot be split meaningfully
  if (any(wsum <= 0)) {
    stop("BB ", year, ": postal pool(s) with zero total weight: ",
         paste(names(wsum)[wsum <= 0], collapse = ", "))
  }
  map$share <- map$w / unname(wsum[map$pool])

  # Additions per municipality, one column per distributed variable
  targets <- sort(unique(map$ags))
  addm <- vapply(dist_cols, function(cl) {
    pv <- pools[[cl]][match(map$pool, pools$ags)]
    a <- tapply(pv * map$share, map$ags, sum, na.rm = TRUE)
    unname(a[targets])
  }, numeric(length(targets)))
  addm <- matrix(addm, nrow = length(targets), dimnames = list(targets, dist_cols))
  addm[is.na(addm)] <- 0

  # Integerise. Party additions are rounded by largest remainder so that they
  # still sum exactly to the municipality's added valid votes — otherwise
  # rounding each party independently breaks the sum(parties) == valid_votes
  # identity and every allocated municipality would trip
  # flag_total_votes_incongruent downstream.
  pcols <- intersect(party_names, dist_cols)
  for (i in seq_len(nrow(addm))) {
    tgt <- round(addm[i, "gueltige_stimmen"])
    v <- addm[i, pcols]
    fl <- floor(v)
    need <- tgt - sum(fl)
    if (need > 0) {
      ord <- order(v - fl, decreasing = TRUE)
      fl[ord[seq_len(min(need, length(fl)))]] <- fl[ord[seq_len(min(need, length(fl)))]] + 1
    } else if (need < 0) {
      ord <- order(v - fl, decreasing = FALSE)
      take <- which(fl[ord] > 0)[seq_len(min(-need, sum(fl > 0)))]
      fl[ord[take]] <- fl[ord[take]] - 1
    }
    addm[i, pcols] <- fl
    addm[i, "gueltige_stimmen"] <- tgt
  }
  for (cl in setdiff(dist_cols, c(pcols, "gueltige_stimmen"))) {
    addm[, cl] <- round(addm[, cl])
  }

  idx <- match(targets, members$ags)
  for (cl in dist_cols) {
    base <- members[[cl]][idx]
    add <- addm[, cl]
    # NA means "this party did not stand here". Keep it NA when the pool
    # contributes nothing either; otherwise the pooled votes become its count.
    members[[cl]][idx] <- ifelse(is.na(base) & add == 0, NA_real_,
                                 ifelse(is.na(base), add, base + add))
  }

  cat(sprintf("    allocated %d pooled postal districts over %d municipalities (weight: %s)\n",
              nrow(pools), length(targets), wcol))
  members
}

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

  # Row 1 = headers. unname() is essential: unlist() on a read_excel(col_names =
  # FALSE) tibble keeps the "...N" placeholder names, which which() then carries
  # into the column map, so c(ags = <named 2>) would become "ags....2".
  headers <- unname(clean_header(unlist(raw[1, ])))

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
  eb_positions <- c()   # individual "EB <Name>" columns (no summary column in 2008)
  for (i in (gueltig_col + 1):ncol(raw)) {
    name <- headers[i]
    if (is.na(name) || nchar(trimws(name)) == 0) next

    clean_name <- trimws(name)

    # Skip "in Prozent" columns
    if (grepl("in Prozent", clean_name, ignore.case = TRUE)) next
    if (grepl("^Stimmen nach", clean_name, ignore.case = TRUE)) next

    # Handle EB (Einzelbewerber) — aggregate all individual EBs
    if (grepl("^EB\\b|^Einzelbew", clean_name)) {
      if (grepl("Zusammenfassung|Einzelbewerbende|^Einzelbewerber$", clean_name)) {
        # Summary column (2003, 2014, 2019) — use directly
        party_positions <- c(party_positions, i)
        party_names <- c(party_names, "einzelbewerber")
      } else {
        # Individual "EB <Name>" column (2008) — summed in below
        eb_positions <- c(eb_positions, i)
      }
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
  # Wahlberechtigte A2 = eligible voters holding a Wahlschein. Present and
  # populated from 2014 on; it is the weight for allocating pooled postal
  # votes (the same key the federal pipeline uses). Empty in 2003/2008.
  a2_col <- which(headers == "Wahlberechtigte A2")[1]
  if (!is.na(a2_col)) col_map <- c(col_map, c(a2 = unname(a2_col)))
  party_map <- setNames(party_positions, party_names)
  col_map <- c(col_map, party_map)

  # Extract data (row 2 onward) — build df column by column to avoid name mangling
  n_data <- nrow(raw) - 1L
  df <- data.frame(row.names = seq_len(n_data), check.names = FALSE)
  for (k in seq_along(col_map)) {
    df[[names(col_map)[k]]] <- as.character(raw[[col_map[k]]])[2:nrow(raw)]
  }

  # 2008 has no EB summary column, only one column per individual candidate —
  # sum them into einzelbewerber so those votes are not silently dropped.
  # NA is preserved where no EB stood at all (needed by the na_tracker below).
  if (length(eb_positions) > 0 && !("einzelbewerber" %in% party_names)) {
    eb_mat <- vapply(eb_positions, function(p) {
      v <- as.character(raw[[p]])[2:nrow(raw)]
      v[v %in% c("x", "-", "")] <- NA_character_
      suppressWarnings(as.numeric(v))
    }, numeric(n_data))
    eb_mat <- matrix(eb_mat, nrow = n_data)
    eb_sum <- rowSums(eb_mat, na.rm = TRUE)
    eb_sum[rowSums(!is.na(eb_mat)) == 0] <- NA_real_
    df[["einzelbewerber"]] <- eb_sum
    party_names <- c(party_names, "einzelbewerber")
    cat("    summed", length(eb_positions), "individual EB columns\n")
  }

  # Filter to Kreistag rows only
  df <- df[!is.na(df[["stimmart"]]) & df[["stimmart"]] == "Kreistag", ]
  df[["stimmart"]] <- NULL

  # Cottbus 2019 is reported as two city-council Wahlkreise, each carrying its
  # Ortsteil list after a colon. Strip that suffix (and collapse whitespace) so
  # the municipality aggregates into a single row rather than two.
  df[["ags_name"]] <- trimws(gsub("\\s+", " ", sub(":.*$", "", df[["ags_name"]])))

  # Pin one canonical name per AGS before aggregating. In 2003/2008 the postal
  # ballot districts are named "Briefwahl" rather than after their municipality;
  # grouping on (ags, ags_name) would put them in their own group, which the
  # later eligible_voters > 0 filter then drops — silently discarding every
  # postal vote (228k in 2003, 355k in 2008).
  df[["ags_name"]] <- canonical_ags_name(df[["ags"]], df[["ags_name"]])

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "gueltige_stimmen", party_names,
                    if ("a2" %in% names(df)) "a2")
  for (col_name in all_numeric) {
    v <- as.character(df[[col_name]])
    v[v %in% c("x", "-", "")] <- NA_character_
    df[[col_name]] <- suppressWarnings(as.numeric(v))
  }

  # Track all-NA parties per AGS
  na_tracker <- list()
  for (pc in party_names) {
    na_tracker[[pc]] <- tapply(df[[pc]], df[["ags"]], function(x) all(is.na(x)))
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
      df_muni[[pc]][df_muni[["ags"]] %in% no_cand_ags] <- NA_real_
    }
  }

  # Add metadata
  df_muni$election_year <- as.integer(year)
  df_muni$state <- "12"
  df_muni$ags <- pad_zero_conditional(df_muni$ags, 7)

  # Distribute the pooled postal districts over their member municipalities
  # (leaves df_muni with real municipalities only when allocation is on)
  df_muni <- bb_allocate_postal(
    df_muni, year, party_names,
    dist_cols = c("number_voters", "invalid_votes", "gueltige_stimmen",
                  party_names)
  )
  df_muni$a2 <- NULL
  df_muni$county <- substr(df_muni$ags, 1, 5)

  # Compute vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_names) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }
  names(df_muni)[names(df_muni) == "gueltige_stimmen"] <- "valid_votes"
  # Allocation leaves fractional counts; shares above were computed from the
  # unrounded values so they still sum to 1. Round the published totals.
  for (cl in c("number_voters", "invalid_votes", "valid_votes")) {
    df_muni[[cl]] <- round(df_muni[[cl]])
  }

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

  # Row 1 = headers (see parse_bb_xlsx: unname() prevents "...N" name mangling)
  headers <- unname(clean_header(unlist(raw[1, ])))

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
  # Amtsnummer identifies which Amt a Gemeinde (or a pooled postal district)
  # belongs to — the linkage used to allocate the pooled postal votes.
  # A2 = Wahlberechtigte mit Wahlschein is the allocation weight.
  amt_col <- which(headers == "Amtsnummer/Verbandsgemeinde")[1]
  a2_col <- which(headers == "Wahlberechtigte A2")[1]
  col_map <- c(col_map, c(amt = unname(amt_col), a2 = unname(a2_col)))
  party_map <- setNames(party_positions, party_names)
  col_map <- c(col_map, party_map)

  # Extract data — build df column by column to avoid name mangling
  n_data <- nrow(raw) - 1L
  df <- data.frame(row.names = seq_len(n_data), check.names = FALSE)
  for (k in seq_along(col_map)) {
    df[[names(col_map)[k]]] <- as.character(raw[[col_map[k]]])[2:nrow(raw)]
  }

  # Construct AGS from the 12-digit ARS. Layout is Land(2) + RB(1) + Kreis(2) +
  # Amt/Gemeindeverband(4) + Gemeinde(3), so the AGS is the first five digits
  # plus the last three — NOT substr(1, 8), which would splice in the Amt code
  # and collapse distinct Gemeinden of the same Amt onto one key.
  # Amt-wide postal districts carry a SHORT (9-char) ARS with no Gemeinde
  # segment; keep it verbatim so each pool stays a distinct key (several
  # Ämter of one Kreis would otherwise collapse onto the same code).
  df$ags <- ifelse(nchar(df[["ars"]]) == 12,
                   paste0(substr(df[["ars"]], 1, 5), substr(df[["ars"]], 10, 12)),
                   df[["ars"]])
  df$ars <- NULL
  df[["ags_name"]] <- trimws(gsub("\\s+", " ", sub(":.*$", "", df[["ags_name"]])))
  df[["ags_name"]] <- canonical_ags_name(df[["ags"]], df[["ags_name"]])
  amt_by_ags <- tapply(df[["amt"]], df[["ags"]], function(x) x[1])
  df$amt <- NULL

  # Convert to numeric
  all_numeric <- c("eligible_voters", "number_voters", "invalid_votes",
                    "gueltige_stimmen", party_names, "a2")
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

  # Distribute the Amt-wide postal districts over their member municipalities
  df_muni <- bb_allocate_postal(
    df_muni, 2024, party_names,
    dist_cols = c("number_voters", "invalid_votes", "gueltige_stimmen",
                  party_names),
    amt_by_ags = amt_by_ags
  )
  df_muni$a2 <- NULL
  df_muni$county <- substr(df_muni$ags, 1, 5)

  # Compute vote shares and turnout
  df_muni$turnout <- ifelse(df_muni$eligible_voters > 0,
                            df_muni$number_voters / df_muni$eligible_voters, NA_real_)
  for (pc in party_names) {
    df_muni[[pc]] <- ifelse(df_muni$gueltige_stimmen > 0,
                            df_muni[[pc]] / df_muni$gueltige_stimmen, NA_real_)
  }
  names(df_muni)[names(df_muni) == "gueltige_stimmen"] <- "valid_votes"
  # Shares were computed from unrounded counts, so they still sum to 1;
  # round the published totals after allocation.
  for (cl in c("number_voters", "invalid_votes", "valid_votes")) {
    df_muni[[cl]] <- round(df_muni[[cl]])
  }

  cat("    ->", nrow(df_muni), "municipalities\n")
  as_tibble(df_muni)
}

# Process BB years
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

# -----------------------------------------------------------------------------
# BY 2026 — Kommunalwahl of 8 March 2026, from the Landesamt results portal
# -----------------------------------------------------------------------------
# The 1984-2020 series comes from GENESIS table 14411-001r as XLSX pairs. For
# 2026 the Landesamt publishes machine-readable XML instead, at
# kommunalwahl2026.bayern.de/downloads.html (with XSD schemas). The Gremienwahl
# file covers exactly the 96 units this dataset already holds: 71 Kreistage plus
# the 25 Stadträte of the kreisfreie Städte. (Gemeinderatswahlen of the ~2,000
# kreisangehörige Gemeinden are NOT in the bulk download — only browsable per
# municipality — so municipal_elections cannot be extended from this source.)
#
# Bavarian council elections are cumulative (Kumulieren/Panaschieren), so the
# file reports both raw cumulative votes (Stimmen_absolut) and ballot-equivalent
# weighted votes (Gewichtete_Stimmen). The 1984-2020 series uses the weighted
# figures, and so do we: Gewichtete_Stimmen + Ungueltige_Stimmzettel == Waehler
# holds exactly for all 96 units.
#
# Unlike the XLSX series, the XML lists every individual Wahlvorschlag rather
# than a pre-aggregated "Wählergruppen" bucket. To keep Bayern internally
# consistent, recognised parties keep their own column and every local list
# folds into `waehlergruppen`, mirroring the earlier years. Coalition labels
# ("FREIE WÄHLER/Freie Wähler Ingolstadt", "ÖDP/Parteifreie") are attributed to
# their leading party.
by_xml_file <- file.path(by_dir, "Bayern_2026_Gremien_Komplett.xml")

if (file.exists(by_xml_file)) {
  cat("  Reading BY 2026 (XML) ...\n")
  suppressMessages(library(xml2))

  by26_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
  # Parties that keep their own column in Bayern; everything else is a local
  # list and folds into waehlergruppen.
  by26_keep <- c("csu", "spd", "afd", "gruene", "linke_pds", "fdp", "oedp",
                 "freie_waehler", "bp", "volt", "die_partei", "rep", "bsw",
                 "mut", "v_partei3", "piraten", "die_franken", "tierschutz")

  by26_col <- function(raw) {
    n <- normalise_party_cty(raw)
    if (n %in% by26_keep) return(n)
    # coalition label: attribute to the leading party if recognised
    lead <- normalise_party_cty(trimws(strsplit(raw, "/", fixed = TRUE)[[1]][1]))
    if (lead %in% by26_keep) return(lead)
    "waehlergruppen"
  }

  by26_doc <- read_xml(by_xml_file)
  by26_units <- xml_find_all(by26_doc, ".//Regionaleinheit")

  by26_rows <- lapply(by26_units, function(u) {
    key <- xml_attr(u, "Schluesselnummer")
    w   <- xml_find_first(u, "Wahl")
    ag  <- xml_find_first(w, "Allgemeine_Angaben")
    se  <- xml_find_first(w, "Stimmenergebnis")
    zus <- xml_find_first(se, "Wahlvorschlaege_zusammen")
    ung <- xml_find_first(se, "Ungueltige_Stimmzettel")
    wv  <- xml_find_all(se, "Wahlvorschlag")

    shares <- by26_num(xml_text(xml_find_first(wv, "Gewichtete_Stimmen_Anteil"))) / 100
    cols   <- vapply(xml_text(xml_find_first(wv, "Bezeichnung")), by26_col,
                     character(1), USE.NAMES = FALSE)
    agg <- tapply(shares, cols, sum, na.rm = TRUE)

    base <- data.frame(
      ags = paste0("09", formatC(as.integer(key), width = 3, flag = "0"), "000"),
      ags_name = xml_text(xml_find_first(ag, "Name_der_Regionaleinheit")),
      eligible_voters = by26_num(xml_text(xml_find_first(ag, "Stimmberechtigte"))),
      number_voters   = by26_num(xml_text(xml_find_first(ag, "Waehler"))),
      valid_votes     = by26_num(xml_text(xml_find_first(zus, "Gewichtete_Stimmen"))),
      invalid_votes   = by26_num(xml_text(xml_find_first(ung, "Anzahl"))),
      stringsAsFactors = FALSE
    )
    for (nm in names(agg)) base[[nm]] <- unname(agg[[nm]])
    base
  })

  df_by26 <- bind_rows(by26_rows)
  df_by26$turnout <- ifelse(df_by26$eligible_voters > 0,
                            df_by26$number_voters / df_by26$eligible_voters, NA_real_)
  df_by26$county <- substr(df_by26$ags, 1, 5)
  df_by26$state <- "09"
  df_by26$election_year <- 2026

  cat("    ->", nrow(df_by26), "units (Kreistage + Stadträte)\n")
  by_results[["2026"]] <- as_tibble(df_by26)
} else {
  cat("  Skipping BY 2026 - XML not found\n")
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

# --- Header-block sub-column resolution --------------------------------------
# Several HE sheets split a meta block into sub-columns and print the total in
# the LAST of them: "Wahlberechtigte" = ohne Sperrvermerk "W" | mit Sperrvermerk
# "W" | nach § 16a Abs. 2 KWO | Ins-gesamt, and "Ungültige Stimmzettel" =
# ins-gesamt | in %. The old magnitude heuristic ("first column in
# label_col..label_col+4 whose first data value exceeds 100") therefore latched
# onto the "ohne Sperrvermerk" column in 1981/1985/1997/2011/2016, understating
# eligible_voters by 6.5-14.2% and pushing 15 rows above 100% turnout
# (audit fix C-15, 2026-07). Resolve the sub-column by header text instead.

#' Locate the "Insgesamt" sub-column of a header block.
#'
#' The block runs from the label column up to (but excluding) the next filled
#' cell in the SAME header row — bounding it matters, because in 2001 the
#' Wahlberechtigte block is a single column and the neighbouring "insgesamt"
#' belongs to Wähler.
#'
#' Two rules are tried, on each of the `n_scan` rows below the label:
#'   1. a sub-header reading "Insgesamt" / "Ins-gesamt" (case-insensitive);
#'   2. the source's own Feldbezeichner row — 1997 labels the block A1, A2, A3,
#'      A, so the bare letter among lettered-and-numbered siblings is the total.
#' Returns NA when the block has no sub-columns (1948-1956, 2001), leaving the
#' caller on its original heuristic.
he_block_subcol <- function(raw, label_row, label_col, n_scan = 3L) {
  if (is.na(label_row) || is.na(label_col)) return(NA_integer_)
  hdr <- clean_header(as.character(raw[label_row, ]))
  filled <- which(!is.na(hdr) & nzchar(hdr))
  nxt <- filled[filled > label_col]
  block_end <- if (length(nxt) > 0) nxt[1] - 1L else min(label_col + 4L, ncol(raw))
  if (block_end < label_col) block_end <- label_col
  cols <- label_col:block_end
  last <- min(label_row + n_scan, nrow(raw))
  if (last <= label_row) return(NA_integer_)

  for (sr in (label_row + 1L):last) {
    sv <- clean_header(as.character(raw[sr, ]))
    cc <- cols[cols <= length(sv)]
    if (length(cc) == 0) next
    v <- tolower(trimws(sv[cc]))
    v[is.na(v)] <- ""
    hit <- which(grepl("^ins-?\\s*gesamt$", v))
    if (length(hit) > 0) return(cc[hit[1]])
    # Feldbezeichner row (1997): "A1" "A2" "A3" "A" -> the bare "A" is the total
    u <- toupper(v)
    bare <- which(grepl("^[A-Z]$", u))
    subs <- which(grepl("^[A-Z][0-9]+$", u))
    if (length(bare) == 1 && length(subs) > 0 &&
        all(substr(u[subs], 1, 1) == u[bare])) {
      return(cc[bare])
    }
  }
  NA_integer_
}

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

    # Eligible voters: take the block's "Ins-gesamt" sub-column by header text
    # (col 7 in 2006/2011/2016), NOT the first column carrying big numbers —
    # that is "ohne Sperrvermerk W" (col 4). See he_block_subcol.
    if (!is.na(eligible_col)) {
      ec_hdr <- he_block_subcol(raw, 2, eligible_col)
      if (!is.na(ec_hdr)) {
        eligible_col <- ec_hdr
      } else {
        # Fallback: first data column after the label that has large numbers
        for (ec in (eligible_col):(eligible_col + 4)) {
          test_val <- as.numeric(as.character(all_data[[ec]][which(valid_rows)[1]]))
          if (!is.na(test_val) && test_val > 100) { eligible_col <- ec; break }
        }
      }
    }

    # Invalid ballots: the "Ungültige Stimmzettel" block is ins-gesamt | in %,
    # so resolve its total sub-column too (col 11, not the percentage col 12).
    # This branch used to leave invalid_votes empty for 2006/2011/2016
    # (audit fix C-13, 2026-07).
    invalid_label_col <- which(grepl("Ung.ltige", r2))[1]
    invalid_col <- he_block_subcol(raw, 2, invalid_label_col)
    if (is.na(invalid_col)) invalid_col <- invalid_label_col

    df$eligible_voters <- if (!is.na(eligible_col)) as.numeric(as.character(all_data[[eligible_col]][valid_rows])) else NA_real_
    df$number_voters <- if (!is.na(voters_col)) as.numeric(as.character(all_data[[voters_col]][valid_rows])) else NA_real_
    df$valid_votes <- if (!is.na(valid_votes_col)) as.numeric(as.character(all_data[[valid_votes_col]][valid_rows])) else NA_real_
    df$invalid_votes <- if (!is.na(invalid_col)) as.numeric(as.character(all_data[[invalid_col]][valid_rows])) else NA_real_

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
          # Prefer the block's "Insgesamt" sub-column resolved by header text
          # (1981 col 9, 1985/1997 col 7, 1993 col 5); the magnitude heuristic
          # below picks "ohne Sperrvermerk W" instead. See he_block_subcol.
          elig_col <- he_block_subcol(raw, r, i)
          if (is.na(elig_col)) {
            for (ec in i:(i + 4)) {
              tv <- as.numeric(as.character(all_data[[ec]][which(valid_rows)[1]]))
              if (!is.na(tv) && tv > 100) { elig_col <- ec; break }
            }
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

    # 1997 only: the sheet carries the invalid-ballot counts (statewide 67,767)
    # but prints NO text header for them anywhere in rows 1-13 — the column is
    # identified solely by the Feldbezeichner "C" in row 9 — so the keyword scan
    # above legitimately finds nothing. It sits immediately left of the valid
    # column, exactly as in every other single-column sheet (audit fix C-13).
    if (is.na(invalid_col) && !is.na(valid_col) && year == 1997 && valid_col > 1) {
      invalid_col <- valid_col - 1L
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
    invalid_col_21 <- which(grepl("^ung.ltige Stimmzettel$", trimws(he21_headers),
                                  ignore.case = TRUE))[1]

    gkz_vals <- trimws(he21_df[[gkz_col_21]])
    valid_21 <- grepl("^\\d{3,6}$", gkz_vals)
    he21_data <- he21_df[valid_21, ]

    if (nrow(he21_data) > 0) {
      # AGS: the 2021 CSV already gives Gemeinden a 6-digit GKZ (431001) and
      # only the 21 Kreis aggregates a 3-digit one (431). sprintf("%03s", …)
      # leaves a 6-digit string untouched, so "06" + GKZ + "000" produced
      # 11-character AGS for all 417 Gemeinde rows (audit fix C-04, 2026-07;
      # 02_county_elec_harm_21.R patched this downstream — that patch is now
      # redundant). Mirrors the Gemeinde/Kreis construction used for the XLSX
      # sheets above.
      gkz_21 <- trimws(he21_data[[gkz_col_21]])
      ags_21 <- ifelse(nchar(gkz_21) == 6,
                       paste0("06", gkz_21),
                       paste0("06", sprintf("%03s", gkz_21), "000"))

      df_21 <- data.frame(
        ags = ags_21,
        ags_name = if (!is.na(name_col_21)) trimws(he21_data[[name_col_21]]) else NA_character_,
        state = "06",
        election_year = 2021L,
        stringsAsFactors = FALSE
      )
      df_21$eligible_voters <- if (!is.na(elig_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[elig_col_21]])) else NA_real_
      df_21$number_voters <- if (!is.na(voter_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[voter_col_21]])) else NA_real_
      df_21$valid_votes <- if (!is.na(valid_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[valid_col_21]])) else NA_real_
      df_21$invalid_votes <- if (!is.na(invalid_col_21)) as.numeric(gsub("[^0-9]", "", he21_data[[invalid_col_21]])) else NA_real_
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

# --- HE 2026 Kreistagswahl (15 March 2026), Landeswahlleiter / 23degrees portal ---
# Clean per-Gemeinde CSV (row 1 title, row 2 headers, row 3 position numbers, row 4+
# data). Party vote counts are in "<party> absolut" columns; local Wählergruppen
# (WG1, WG2, …) are position-indexed per Gemeinde and summed into `waehlergruppen`.
# Kept rows = individual Gemeinden (Gebietstyp "VF"); the LK aggregate rows
# (Gebietstyp "LK") are dropped, matching the existing Gemeinde-level HE structure.
he_2026_csv <- file.path(he_dir, "Hessen_2026_Kreiswahl_portal.csv")
if (file.exists(he_2026_csv)) {
  cat("  Reading HE 2026 Kreiswahl CSV...\n")
  tryCatch({
    he26_lines <- readLines(he_2026_csv, encoding = "UTF-8", warn = FALSE)
    he26_headers <- gsub('^"|"$', '', trimws(strsplit(he26_lines[2], ";")[[1]]))
    # line 1 = title, line 2 = column headers, data from line 3 (NO position-number
    # row); keep only rows whose Gebietsschlüssel is numeric.
    he26_rows <- lapply(strsplit(he26_lines[3:length(he26_lines)], ";"),
                        function(r) { r <- gsub('^"|"$', '', r); length(r) <- length(he26_headers); r })
    he26_rows <- he26_rows[vapply(he26_rows,
                        function(r) grepl("^[0-9]{6,}$", gsub("[^0-9]", "", r[1])), logical(1))]
    he26_df <- as.data.frame(do.call(rbind, he26_rows), stringsAsFactors = FALSE)
    names(he26_df) <- he26_headers

    # individual Gemeinden only (drop Landkreis "LK" / Land aggregates)
    he26_df <- he26_df[trimws(he26_df[["Gebietstyp"]]) == "VF", ]
    gs <- gsub("[^0-9]", "", he26_df[["Gebietsschlüssel"]])           # 9-digit
    ags26 <- paste0("06", substr(gs, nchar(gs) - 5L, nchar(gs)))      # 06 + last 6
    numcol <- function(nm) suppressWarnings(as.numeric(gsub("[^0-9]", "", he26_df[[nm]])))

    df_26 <- data.frame(
      ags = ags26, ags_name = trimws(he26_df[["Gebietsbezeichnung"]]),
      state = "06", election_year = 2026L, county = substr(ags26, 1, 5),
      stringsAsFactors = FALSE)
    df_26$eligible_voters <- numcol("Wahlberechtigte")
    df_26$number_voters   <- numcol("Wählerinnen und Wähler")
    df_26$valid_votes     <- numcol("Gültige Stimmen")               # cast votes (Kumulieren)
    # Invalid ballots: the exact column, NOT the "… (%)" sibling (C-13)
    if ("Ungültige Stimmzettel" %in% he26_headers) {
      df_26$invalid_votes <- numcol("Ungültige Stimmzettel")
    }
    df_26$turnout <- ifelse(!is.na(df_26$eligible_voters) & df_26$eligible_voters > 0,
                            df_26$number_voters / df_26$eligible_voters, NA_real_)

    wg_sum <- rep(0, nrow(he26_df)); wg_any <- rep(FALSE, nrow(he26_df))
    for (h in he26_headers) {
      if (!grepl(" absolut$", h)) next
      pn <- sub(" absolut$", "", h)
      vals <- suppressWarnings(as.numeric(gsub("[^0-9]", "", he26_df[[h]])))
      if (all(is.na(vals) | vals == 0)) next
      if (grepl("^WG\\d+$", pn)) {                                    # local Wählergruppe
        wg_sum <- wg_sum + ifelse(is.na(vals), 0, vals); wg_any <- wg_any | !is.na(vals); next
      }
      pname <- normalise_party_cty(tolower(clean_header(pn)))
      sh <- ifelse(!is.na(df_26$valid_votes) & df_26$valid_votes > 0, vals / df_26$valid_votes, NA_real_)
      df_26[[pname]] <- if (!is.null(df_26[[pname]]))                 # combine on rare name collision
        rowSums(cbind(df_26[[pname]], sh), na.rm = TRUE) else sh
    }
    if (any(wg_any)) {
      wg_sum[!wg_any] <- NA_real_
      df_26[["waehlergruppen"]] <- ifelse(!is.na(df_26$valid_votes) & df_26$valid_votes > 0,
                                          wg_sum / df_26$valid_votes, NA_real_)
    }
    cat("    -> 2026:", nrow(df_26), "Gemeinden\n")
    he_results[["2026"]] <- as_tibble(df_26)
  }, error = function(e) cat("    ERROR in HE 2026 CSV:", conditionMessage(e), "\n"))
}

df_he <- bind_rows(he_results)

# Remove Landkreis aggregate rows (AGS ending in "000") where municipality-level
# rows also exist for the same county. These are redundant sums.
# kreisfreie Städte (no sub-municipality rows) are kept.
he_county_codes <- substr(df_he$ags, 1, 5)
he_is_agg <- substr(df_he$ags, 6, nchar(df_he$ags)) == "000" |
             substr(df_he$ags, 6, nchar(df_he$ags)) == "000000"
he_has_munis <- he_county_codes %in% unique(he_county_codes[!he_is_agg])
n_he_agg <- sum(he_is_agg & he_has_munis)
if (n_he_agg > 0) {
  cat("  Removing", n_he_agg, "Landkreis aggregate rows (duplicates of municipality data)\n")
  df_he <- df_he[!(he_is_agg & he_has_munis), ]
}

# --- HE integrity assertions -------------------------------------------------
# (1) AGS must be 8 characters. The 2021 CSV used to emit 11-character codes
#     for all 417 Gemeinde rows (audit fix C-04).
he_bad_ags <- unique(df_he$ags[nchar(df_he$ags) != 8])
if (length(he_bad_ags) > 0) {
  stop("HE: ", length(he_bad_ags), " malformed AGS (expected 8 characters): ",
       paste(head(he_bad_ags, 5), collapse = ", "))
}

# (2) eligible_voters must not fall below number_voters. This used to fail on
#     2,105 rows because eligible_voters was read from the "ohne Sperrvermerk W"
#     sub-column instead of the block total (audit fix C-15).
#     KNOWN SOURCE ERROR, not a parsing fault: 06440022 in 1956 aggregates the
#     raw Rockenberg row, which itself reports 1,032 eligible against 1,187
#     voters — the only such row among 2,696 raw 1956 records.
he_turnout_bad <- df_he |>
  filter(!is.na(eligible_voters), !is.na(number_voters),
         eligible_voters < number_voters,
         !(ags == "06440022" & election_year == 1956))
if (nrow(he_turnout_bad) > 0) {
  print(head(he_turnout_bad[, c("ags", "ags_name", "election_year",
                                "eligible_voters", "number_voters")], 15))
  stop("HE: ", nrow(he_turnout_bad),
       " rows with eligible_voters < number_voters — the Wahlberechtigte ",
       "'Insgesamt' sub-column is probably mis-resolved")
}

cat("HE total:", nrow(df_he), "rows x", ncol(df_he), "cols\n")
cat("HE years:", paste(sort(unique(df_he$election_year)), collapse = ", "), "\n")
df_he |> count(election_year) |> print()


# =============================================================================
# BADEN-WÜRTTEMBERG (BW) — Kreis-level, 1994–2019
# =============================================================================

cat("\n===== BADEN-WÜRTTEMBERG =====\n")

bw_dir <- "data/county_elections/raw/local_elections_bw"

#' BW Kreistag pre-2024: assign the uncaptured residual (the local
#' Wählervereinigungen bloc, ~25% in BW Kreistag) to the waehlervereinigungen
#' SHARE column. The 2004-2019/1994/1999 source tables break out only named
#' parties, so per-Kreis shares otherwise sum to ~0.75; this makes them sum to
#' ~1.0 and yields a local-list series consistent with the 2024 GENESIS data,
#' which lists "Sonstige Wählervereinigungen" explicitly. Operates on already-
#' computed share columns; named-party shares are unchanged. (Not applied to the
#' 2024 parser, which captures the full Wahlvorschlag set → shares already = 1.0.)
bw_add_wv_residual <- function(df) {
  meta <- c("ags", "ags_name", "eligible_voters", "number_voters", "valid_votes",
            "invalid_votes", "turnout", "county", "state", "election_year",
            "waehlervereinigungen")
  pcols <- setdiff(names(df), meta)
  captured <- rowSums(df[, pcols, drop = FALSE], na.rm = TRUE)
  df$waehlervereinigungen <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                                    pmax(1 - captured, 0), NA_real_)
  df
}

#' Parse BW Format B (1994, 2004-2019): multi-row per Kreis
parse_bw_format_b <- function(filepath, year) {
  cat("  Reading BW", year, "(format B)...\n")
  suppressMessages(
    raw <- read_excel(filepath, col_names = FALSE, col_types = "text")
  )

  # Party names: row 3 for 2014+, row 5 for 2004/2009
  r3 <- clean_header(as.character(raw[3, ]))
  excl_pat <- "Wahlberechtigte|W.hler|Stimmen|Einheit|Schl.ssel|Landkreis|Lfd|insgesamt|Ins-|zusammen|davon|Verteilung|Mehrheit|Verh.ltnis|^Parteien$|^Gemein"

  party_positions <- c()
  party_names <- c()
  for (i in 5:length(r3)) {
    if (!is.na(r3[i]) && !grepl(excl_pat, r3[i])) {
      party_positions <- c(party_positions, i)
      party_names <- c(party_names, r3[i])
    }
  }

  # Fall back to row 5 if row 3 had no parties (2004/2009 format)
  if (length(party_positions) == 0) {
    r5 <- clean_header(as.character(raw[5, ]))
    for (i in 5:length(r5)) {
      if (!is.na(r5[i]) && !grepl(excl_pat, r5[i])) {
        party_positions <- c(party_positions, i)
        party_names <- c(party_names, r5[i])
      }
    }
  }
  cat("    Found", length(party_positions), "party columns\n")

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

  # Eligible voters and number of voters from row 2 headers
  elig_col <- which(grepl("Wahlberechtigte", r2))[1]
  voter_col <- which(grepl("^W.hler$", r2))[1]
  # Valid votes is always col 9 ("insgesamt") — already filtered to "Gültige Stimmen" + "Anzahl" rows
  # Invalid votes is always col 7 ("Ungültige Stimmzettel")
  valid_col <- 9
  invalid_col <- 7

  df$eligible_voters <- if (!is.na(elig_col)) as.numeric(as.character(all_data[[elig_col]][keep])) else NA_real_
  df$number_voters <- if (!is.na(voter_col)) as.numeric(as.character(all_data[[voter_col]][keep])) else NA_real_
  df$valid_votes <- as.numeric(as.character(all_data[[valid_col]][keep]))
  df$invalid_votes <- as.numeric(as.character(all_data[[invalid_col]][keep]))

  # Party vote counts — sum duplicates (own-list + joint-list columns in 2004/2009)
  for (k in seq_along(party_positions)) {
    pname <- normalise_party_cty(tolower(trimws(party_names[k])))
    vals <- as.character(all_data[[party_positions[k]]][keep])
    vals[vals == "x" | vals == "-" | vals == "."] <- NA_character_
    new_vals <- as.numeric(vals)
    if (pname %in% names(df)) {
      # Sum with existing column (party appears under both Parteien and Gemeinsame Wahlvorschläge)
      df[[pname]] <- ifelse(is.na(df[[pname]]), new_vals,
                            ifelse(is.na(new_vals), df[[pname]], df[[pname]] + new_vals))
    } else {
      df[[pname]] <- new_vals
    }
  }

  # Compute shares
  bw_party_cols <- setdiff(names(df), c("ags", "ags_name", "eligible_voters",
                                         "number_voters", "valid_votes", "invalid_votes"))
  for (pc in bw_party_cols) {
    df[[pc]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                       df[[pc]] / df$valid_votes, NA_real_)
  }

  df <- bw_add_wv_residual(df)
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

  df <- bw_add_wv_residual(df)
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

  df <- bw_add_wv_residual(df)
  df$turnout <- ifelse(!is.na(df$eligible_voters) & df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  df$county <- substr(df$ags, 1, 5)
  df$state <- "08"
  df$election_year <- 1994L

  cat("    ->", nrow(df), "Kreise\n")
  as_tibble(df)
}

#' Parse BW Kreistagswahl 2024 from GENESIS Flachdatei (14411_0002)
#' Kreis-level long format (Latin-1, ";"-sep). Only the 35 Landkreise hold a
#' Kreistagswahl; the 9 Stadtkreise appear with all-"-" rows and are dropped.
#' Party vote = "Gültige Stimmen bei Verhältniswahl" (raw cumulative votes) and
#' valid_votes = their per-Kreis sum, matching the share basis of the prior
#' XLSX years (parse_bw_format_b). This table carries no Wahlberechtigte /
#' Wähler / ungültige, so eligible_voters / number_voters / invalid_votes /
#' turnout are NA. Party label → token uses the trailing-"(...)" Kurzbezeichnung.
parse_bw_kt_genesis <- function(filepath, year = 2024L) {
  cat("  Reading BW", year, "Kreistag (GENESIS flat)...\n")
  d <- fread(filepath, sep = ";", header = TRUE, encoding = "Latin-1",
             colClasses = "character", quote = "")
  setnames(d, c("1_variable_attribute_code", "2_variable_attribute_label",
                "value_variable_label"), c("geo", "party_raw", "vvar"))
  d <- d[grepl("KRSI$", geo)]                       # Kreise only (drop "08LA" Land)
  d <- d[vvar == "Gültige Stimmen bei Verhältniswahl" & value_unit == "Anzahl"]
  d[, party_raw := enc2utf8(party_raw)]
  d[, ags := paste0(substr(geo, 1, 5), "000")]
  g_num <- function(v) {
    v <- trimws(v); v[v %in% c("-", ".", "x", "/", "...", "")] <- NA_character_
    as.numeric(gsub(",", ".", gsub("\\.", "", v)))
  }
  d[, votes := g_num(value)]
  d <- d[!is.na(votes)]                              # "-" = list not on this Kreis ballot
  g_kurz <- function(label) {
    m <- regmatches(label, regexpr("\\(([^()]+)\\)\\s*$", label))
    if (length(m) == 0L || !nzchar(m)) return(label)
    sub("^\\((.*)\\)\\s*$", "\\1", m)
  }
  toks <- vapply(unique(d$party_raw),
                 function(l) normalise_party_cty(tolower(trimws(g_kurz(l)))),
                 character(1))
  d[, token := toks[party_raw]]

  # per-Kreis valid total = sum of all list votes (mutually exclusive Wahlvorschläge)
  tot <- d[, .(valid_votes = sum(votes, na.rm = TRUE)), by = ags]
  tot <- tot[valid_votes > 0]                        # Stadtkreise have no Kreistag
  pc  <- dcast(d[ags %in% tot$ags], ags ~ token, value.var = "votes",
               fun.aggregate = function(x) sum(x, na.rm = TRUE), fill = NA_real_)
  df  <- merge(tot, pc, by = "ags")
  party_cols <- setdiff(names(df), c("ags", "valid_votes"))

  out <- data.frame(ags = df$ags, ags_name = NA_character_,
                    eligible_voters = NA_real_, number_voters = NA_real_,
                    valid_votes = df$valid_votes, invalid_votes = NA_real_,
                    stringsAsFactors = FALSE, check.names = FALSE)
  for (p in party_cols) {
    out[[p]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                       df[[p]] / df$valid_votes, NA_real_)
  }
  out$turnout       <- NA_real_
  out$county        <- substr(out$ags, 1, 5)
  out$state         <- "08"
  out$election_year <- as.integer(year)
  cat("    ->", nrow(out), "Kreise\n")
  as_tibble(out)
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
# 2024 Kreistagswahl from GENESIS flat file (StaLA table 14411_0002)
bw_kt24_file <- file.path(bw_dir, "KTW2024_14411_0002_kreis_stimmen_flat.csv")
if (file.exists(bw_kt24_file)) {
  bw_results[["2024"]] <- tryCatch(
    parse_bw_kt_genesis(bw_kt24_file, 2024L),
    error = function(e) { cat("  BW 2024 ERROR:", conditionMessage(e), "\n"); NULL }
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

  # Party columns: D1, D2, ... from the field codes, or all columns after valid_votes
  d_cols <- grep("^D\\d+$", codes)
  if (length(d_cols) == 0 && !is.na(valid_col)) {
    # No D-coded party columns — fall back to all columns after the valid_votes column
    d_cols <- (valid_col + 1):ncol(raw)
    d_cols <- d_cols[!is.na(headers[d_cols]) & headers[d_cols] != ""]
    cat("    No D-coded party columns; using", length(d_cols), "columns after valid_votes\n")
  }
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
  fb$pname <- sapply(tolower(trimws(fb$`Wahlvorschlag Kurzbezeichnung`)), normalise_party_cty, USE.NAMES = FALSE)
  # Einzelbewerber rows have NA party name — map to "einzelbewerber"
  fb$pname[is.na(fb$pname)] <- "einzelbewerber"

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
      fb_row <- fb[which(fb$kreis_code == kr & fb$Feld == d_field), ]
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
# NIEDERSACHSEN (NI)
# =============================================================================

cat("\n===== NIEDERSACHSEN =====\n")
ni_dir <- file.path(raw_dir, "Niedersachsen")

# Helper: classify NI geographic entities — keep municipalities, skip aggregates
# Keeps: Mitgliedsgemeinde (indented 6d), Einheitsgemeinde (non-indented 6d, suffix<400),
#        kreisfreie Städte (3d, no 6d sub-entries)
# Skips: Samtgemeinde (6d, suffix>=400), Kreise (3d with sub-entries), state/region (1d)
ni_keep_entities <- function(entities, codes, code_lengths, is_indented) {
  codes_6d <- codes[code_lengths == 6]
  prefixes_3d <- unique(substr(codes_6d, 1, 3))
  codes_3d <- codes[code_lengths == 3]
  kreisfrei_3d <- codes_3d[!codes_3d %in% prefixes_3d]

  keep <- rep(FALSE, length(entities))
  for (i in seq_along(entities)) {
    cl <- code_lengths[i]
    cc <- codes[i]
    if (cl == 6) {
      suffix <- as.numeric(substr(cc, 4, 6))
      if (is_indented[i]) keep[i] <- TRUE
      else if (suffix < 400) keep[i] <- TRUE
    } else if (cl == 3 && cc %in% kreisfrei_3d) {
      keep[i] <- TRUE
    }
  }
  keep
}

# Helper: construct 8-digit AGS from NI internal code
ni_make_ags <- function(code) {
  if (nchar(code) == 3) paste0("03", code, "000") else paste0("03", code)
}

# Party map for individual files (2001-2021): column position → party name
# All individual XML files share the same 51-column layout
ni_ktw_party_map <- c(
  "4" = "CDU", "5" = "SPD", "6" = "GRÜNE", "7" = "FDP",
  # Col 8 = Sonstige aggregate — skip
  "9" = "AFD", "10" = "AGP", "11" = "ALFA", "12" = "BIG",
  "13" = "Bündnis C", "14" = "CM", "15" = "DiB",
  "16" = "die Basis LV", "17" = "Die Demokraten",
  "18" = "Die Friesen", "19" = "DIE HAIE", "20" = "DMD",
  "21" = "DieGeDe", "22" = "Die Linke", "23" = "Die Partei",
  "24" = "DEUT", "25" = "DKP", "26" = "DP", "27" = "du.",
  "28" = "EINE WELT", "29" = "Einzelbewerber", "30" = "FAMILIE",
  "31" = "FREIE WÄHLER", "32" = "GRAUE", "33" = "LKR",
  "34" = "MDU", "35" = "NLP", "36" = "NPD",
  "37" = "Neue Liberale", "38" = "ödp",
  "39" = "Partei d. Vernunft", "40" = "PBC", "41" = "Piraten",
  "42" = "POGO", "43" = "REP", "44" = "Schill", "45" = "SFP",
  "46" = "STATT Partei", "47" = "Tierschutz-Partei", "48" = "Volt",
  "49" = "WASG", "50" = "Wählergruppen", "51" = "ZENTRUM"
)

# Parse individual NI KTW XML file (2001-2021)
ni_ktw_parse_individual <- function(filepath, year) {
  cat("  NI", year, "...")
  lines <- readLines(filepath, warn = FALSE)

  entity_idx <- grep('MergeAcross="51"', lines)

  entities <- sapply(entity_idx, function(i) {
    m <- regmatches(lines[i], gregexpr("<Data[^>]*>([^<]*)</Data>", lines[i]))[[1]]
    if (length(m) == 0) return("")
    sub("<Data[^>]*>", "", sub("</Data>", "", m[1]))
  })

  codes <- sub("\\s+.*", "", trimws(entities))
  code_lengths <- nchar(codes)
  is_indented <- grepl("^\\s", entities)
  keep <- ni_keep_entities(entities, codes, code_lengths, is_indented)

  extract_row_values <- function(eidx) {
    search_end <- min(eidx + 10, length(lines))
    hit <- grep("Anzahl</Data>", lines[(eidx + 1):search_end])
    if (length(hit) == 0) return(rep(NA_real_, 51))
    anzahl_line <- eidx + hit[1]
    row_end <- anzahl_line - 1 +
      grep("</Row>", lines[anzahl_line:min(anzahl_line + 60, length(lines))])[1]
    row_block <- paste(lines[anzahl_line:row_end], collapse = "")
    m <- regmatches(row_block, gregexpr("<Data[^>]*>([^<]*)</Data>", row_block))[[1]]
    vals <- sub("<Data[^>]*>", "", sub("</Data>", "", m))
    vals <- vals[-1]  # drop "Anzahl" label
    suppressWarnings(as.numeric(vals))
  }

  results <- vector("list", sum(keep))
  j <- 0
  for (i in which(keep)) {
    j <- j + 1
    vals <- extract_row_values(entity_idx[i])
    ags <- ni_make_ags(codes[i])
    row_data <- data.frame(
      ags = ags,
      eligible_voters = vals[1], number_voters = vals[2], valid_votes = vals[3],
      stringsAsFactors = FALSE
    )
    for (vi in names(ni_ktw_party_map)) {
      idx <- as.integer(vi)
      pname_raw <- ni_ktw_party_map[vi]
      pname <- normalise_party_cty(tolower(pname_raw))
      v <- if (idx <= length(vals)) vals[idx] else NA_real_
      if (pname %in% names(row_data)) {
        row_data[[pname]] <- row_data[[pname]] + ifelse(is.na(v), 0, v)
      } else {
        row_data[[pname]] <- v
      }
    }
    results[[j]] <- row_data
  }
  df <- bind_rows(results)

  # NI uses a 3-vote system ("Bis zu 3 Stimmen je Wähler"), so valid_votes ≈ 3× number_voters.
  # invalid_votes = number_voters - valid_votes would be negative; set to NA.
  df$invalid_votes <- NA_real_

  # Compute "other" as residual
  party_cols <- intersect(names(df), unique(sapply(ni_ktw_party_map, function(p) normalise_party_cty(tolower(p)))))
  party_cols <- setdiff(party_cols, c("other"))
  mapped_sum <- rowSums(df[party_cols], na.rm = TRUE)
  df$other <- pmax(df$valid_votes - mapped_sum, 0, na.rm = TRUE)

  # Convert absolute votes to shares
  all_pcols <- c(party_cols, "other")
  for (pc in all_pcols) {
    df[[pc]] <- ifelse(!is.na(df$valid_votes) & df$valid_votes > 0,
                       df[[pc]] / df$valid_votes, NA_real_)
  }

  df$turnout <- ifelse(!is.na(df$eligible_voters) & df$eligible_voters > 0,
                       df$number_voters / df$eligible_voters, NA_real_)
  df$ags_name <- NA_character_
  df$county <- substr(df$ags, 1, 5)
  df$state <- "03"
  df$election_year <- as.integer(year)

  cat(sum(keep), "municipalities\n")
  as_tibble(df)
}

# Parse NI compilation file (1981-1996, 4 elections)
ni_ktw_parse_compilation <- function(filepath) {
  cat("  NI compilation (1981-1996)...\n")
  lines <- readLines(filepath, warn = FALSE)

  merge_idx <- grep('MergeAcross="10"', lines)
  merge_text <- sapply(merge_idx, function(i) {
    m <- regmatches(lines[i], gregexpr("<Data[^>]*>([^<]*)</Data>", lines[i]))[[1]]
    if (length(m) == 0) return("")
    sub("<Data[^>]*>", "", sub("</Data>", "", m[1]))
  })

  is_geo <- !grepl("Stimmen|Anteile", merge_text)
  geo_idx <- merge_idx[is_geo]
  geo_names <- merge_text[is_geo]
  stim_idx <- merge_idx[grepl("Stimmen", merge_text)]
  antl_idx <- merge_idx[grepl("Anteile", merge_text)]

  codes <- sub("\\s+.*", "", trimws(geo_names))
  code_lengths <- nchar(codes)
  is_indented <- grepl("^\\s", geo_names)
  keep <- ni_keep_entities(geo_names, codes, code_lengths, is_indented)

  cat("    Keeping", sum(keep), "of", length(geo_names), "entities\n")

  all_results <- list()
  for (ki in which(keep)) {
    ags <- ni_make_ags(codes[ki])
    stim_line <- stim_idx[ki]
    antl_line <- antl_idx[ki]

    block <- lines[(stim_line + 1):(antl_line - 1)]
    row_starts <- grep("<Row>", block)

    for (rs in row_starts) {
      data_line <- stim_line + rs
      row_end <- data_line - 1 +
        grep("</Row>", lines[data_line:min(data_line + 20, length(lines))])[1]
      if (is.na(row_end)) next
      row_block <- paste(lines[data_line:row_end], collapse = "")
      m <- regmatches(row_block,
                      gregexpr("<Data[^>]*>([^<]*)</Data>", row_block))[[1]]
      vals_raw <- sub("<Data[^>]*>", "", sub("</Data>", "", m))
      label <- trimws(vals_raw[1])
      vals_raw <- vals_raw[-1]
      suppressWarnings(vals <- as.numeric(vals_raw))

      if (!grepl("^KW", label)) next
      date_part <- sub("^KW\\s+", "", label)
      date_parts <- strsplit(trimws(date_part), "\\.")[[1]]
      if (length(date_parts) != 3) next
      year <- as.integer(date_parts[3])

      ev <- vals[1]; nv <- vals[2]; vv <- vals[3]
      cdu_v <- vals[4]; spd_v <- vals[5]; fdp_v <- vals[6]; gruene_v <- vals[7]
      # vals[8] = Sonstige aggregate (skip — we compute other as residual)
      # vals[9], vals[10] = ABG subcategories (almost always NA)
      abg1_v <- if (length(vals) >= 9) vals[9] else NA_real_
      abg2_v <- if (length(vals) >= 10) vals[10] else NA_real_

      named_sum <- sum(c(cdu_v, spd_v, fdp_v, gruene_v, abg1_v, abg2_v), na.rm = TRUE)
      other_v <- max(vv - named_sum, 0, na.rm = TRUE)

      result <- data.frame(
        ags = ags, ags_name = NA_character_,
        eligible_voters = ev, number_voters = nv,
        valid_votes = vv, invalid_votes = NA_real_,
        cdu = cdu_v / vv, spd = spd_v / vv, fdp = fdp_v / vv,
        gruene = gruene_v / vv,
        waehlergruppen = ifelse(is.na(abg1_v), NA_real_, abg1_v / vv),
        einzelbewerber = ifelse(is.na(abg2_v), NA_real_, abg2_v / vv),
        other = ifelse(vv > 0, other_v / vv, NA_real_),
        turnout = ifelse(!is.na(ev) & ev > 0, nv / ev, NA_real_),
        county = substr(ags, 1, 5),
        state = "03",
        election_year = year,
        stringsAsFactors = FALSE
      )
      all_results[[length(all_results) + 1]] <- result
    }
  }
  df <- bind_rows(all_results)
  for (yr in sort(unique(df$election_year))) {
    cat("    ", yr, ":", sum(df$election_year == yr), "municipalities\n")
  }
  as_tibble(df)
}

# Process NI files
ni_results <- list()

# Individual files (2001-2021)
for (yr in c(2001, 2006, 2011, 2016, 2021)) {
  f <- file.path(ni_dir, paste0("Niedersachsen_", yr, "_Kreistagswahl.xml"))
  ni_results[[as.character(yr)]] <- tryCatch(
    ni_ktw_parse_individual(f, yr),
    error = function(e) { cat("  NI", yr, "ERROR:", conditionMessage(e), "\n"); NULL }
  )
}

# Compilation (1981-1996)
comp_file <- file.path(ni_dir, "Niedersachsen_1981-1996_Kreistagswahl.xml")
ni_comp <- tryCatch(
  ni_ktw_parse_compilation(comp_file),
  error = function(e) { cat("  NI compilation ERROR:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(ni_comp)) {
  for (yr in unique(ni_comp$election_year)) {
    ni_results[[as.character(yr)]] <- ni_comp |> filter(election_year == yr)
  }
}

ni_results <- ni_results[!sapply(ni_results, is.null)]
df_ni <- bind_rows(ni_results)

# Remove any Samtgemeinde aggregate rows that slipped through entity filtering
# (suffix >= 400 in positions 6-8 of 8-digit AGS, e.g. 03357406)
ni_suffix <- as.numeric(substr(df_ni$ags, 6, 8))
ni_is_sg <- !is.na(ni_suffix) & ni_suffix >= 400 & nchar(df_ni$ags) == 8
if (any(ni_is_sg)) {
  cat("  Removing", sum(ni_is_sg), "Samtgemeinde aggregate rows\n")
  df_ni <- df_ni[!ni_is_sg, ]
}

cat("NI total:", nrow(df_ni), "rows x", ncol(df_ni), "cols\n")
cat("NI years:", paste(sort(unique(df_ni$election_year)), collapse = ", "), "\n")
df_ni |> count(election_year) |> print()


# =============================================================================
# NORDRHEIN-WESTFALEN (NRW)
# =============================================================================

cat("\n===== NORDRHEIN-WESTFALEN =====\n")
nrw_dir <- file.path(raw_dir, "Nordrhein-Wetfalen")  # note: typo in directory name

# Sheet name mapping
nrw_sheet_map <- c(
  "1999" = "KW_99",
  "2004" = "Stimmbez_KW04",
  "2009" = "Stimmbez_KW09",
  "2014" = "Stimmbezirk KW14",
  "2020" = "Stimmbez.KW20"
)

# Known non-party columns to skip when detecting party columns
nrw_meta_labels <- c(
  "gkz", "kw99", "stimmbez.", "stimmbezirk", "name", "briefwahl",
  "wahlberechtigte", "wahler/-innen", "w\u00e4hler/-innen",
  "darunter", "ungultige", "ung\u00fcltige", "gultige", "g\u00fcltige",
  "verwaltungsbezirk", "krs", "gkz_gemeinde", "gemeinde-name",
  "gemeindenname", "nr.", "a1", "a2", "a3", "a", "b", "b1", "b2",
  "c", "d"
)

# Parse NRW early format (1999, 2004, 2009): headers in row 1, data from row 2
parse_nrw_early <- function(path, year) {
  cat("  NRW", year, "...")
  sheet <- nrw_sheet_map[[as.character(year)]]
  raw <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE))
  headers <- tolower(trimws(as.character(raw[1, ])))

  # Find key columns
  gkz_col <- which(headers %in% c("kw99", "gkz"))[1]
  ev_col <- which(grepl("wahlberechtigte insgesamt|wahlberechtigte.insgesamt", headers, ignore.case = TRUE))[1]
  voter_col <- which(grepl("^w.hler", headers))[1]
  invalid_col <- which(grepl("ung.ltige", headers))[1]
  valid_col <- which(grepl("g.ltige stimmen", headers) & !grepl("ung.ltige", headers))[1]

  # Party columns: everything after valid_col that's not metadata
  party_start <- valid_col + 1
  party_cols_idx <- party_start:ncol(raw)

  # Data rows (skip header)
  data <- raw[2:nrow(raw), ]

  # Extract GKZ and filter valid rows
  gkz_raw <- as.numeric(as.character(data[[gkz_col]]))
  gkz <- sprintf("%06d", as.integer(round(gkz_raw)))
  valid_rows <- !is.na(gkz_raw) & nchar(gkz) == 6

  data <- data[valid_rows, ]
  gkz <- gkz[valid_rows]
  ags <- paste0("05", gkz)

  df <- data.frame(
    ags = ags,
    ags_name = NA_character_,
    eligible_voters = as.numeric(as.character(data[[ev_col]])),
    number_voters = as.numeric(as.character(data[[voter_col]])),
    invalid_votes = as.numeric(as.character(data[[invalid_col]])),
    valid_votes = as.numeric(as.character(data[[valid_col]])),
    stringsAsFactors = FALSE
  )

  # Party columns
  wgr_cols <- c()
  party_names_map <- list()
  for (ci in party_cols_idx) {
    pname_raw <- headers[ci]
    if (is.na(pname_raw) || pname_raw == "" || pname_raw == "na") next
    # Clean word-wrap hyphens
    pname_raw <- gsub("-\\s*\r?\n\\s*", "", pname_raw)
    pname_raw <- gsub("\\s+", " ", trimws(pname_raw))

    if (grepl("^wgr", pname_raw)) {
      wgr_cols <- c(wgr_cols, ci)
      next
    }
    pname <- normalise_party_cty(pname_raw)
    vals <- as.numeric(as.character(data[[ci]]))
    if (pname %in% names(df)) {
      df[[pname]] <- df[[pname]] + ifelse(is.na(vals), 0, vals)
    } else {
      df[[pname]] <- vals
    }
    party_names_map[[pname]] <- TRUE
  }

  # Sum WGR columns into waehlergruppen
  if (length(wgr_cols) > 0) {
    wgr_sum <- rowSums(sapply(wgr_cols, function(ci) {
      as.numeric(as.character(data[[ci]]))
    }), na.rm = TRUE)
    # Track NA: if all WGR are NA for a row, keep NA
    wgr_all_na <- rowSums(!is.na(sapply(wgr_cols, function(ci) {
      as.numeric(as.character(data[[ci]]))
    }))) == 0
    wgr_sum[wgr_all_na] <- NA_real_
    df$waehlergruppen <- wgr_sum
    party_names_map[["waehlergruppen"]] <- TRUE
  }

  # Aggregate Stimmbezirk to municipality
  vote_cols <- names(party_names_map)
  meta_num <- c("eligible_voters", "number_voters", "invalid_votes", "valid_votes")
  agg <- df |>
    group_by(ags) |>
    summarise(
      ags_name = first(ags_name),
      across(all_of(meta_num), ~sum(.x, na.rm = TRUE)),
      across(any_of(vote_cols), ~{
        if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
      }),
      .groups = "drop"
    )

  # Convert to shares
  for (pc in vote_cols) {
    if (pc %in% names(agg)) {
      agg[[pc]] <- ifelse(!is.na(agg$valid_votes) & agg$valid_votes > 0,
                          agg[[pc]] / agg$valid_votes, NA_real_)
    }
  }

  # Compute "other" as residual
  all_pcols <- intersect(vote_cols, names(agg))
  share_sum <- rowSums(agg[all_pcols], na.rm = TRUE)
  agg$other <- pmax(1 - share_sum, 0)
  agg$other[is.na(agg$valid_votes) | agg$valid_votes == 0] <- NA_real_

  agg$turnout <- ifelse(!is.na(agg$eligible_voters) & agg$eligible_voters > 0,
                        agg$number_voters / agg$eligible_voters, NA_real_)
  agg$county <- substr(agg$ags, 1, 5)
  agg$state <- "05"
  agg$election_year <- as.integer(year)

  cat(nrow(agg), "municipalities\n")
  as_tibble(agg)
}

# --- Known IT.NRW data error: 2014 Kreistagswahl municipality labels ---------
# In `Nordrhein-Westfalen_2014_Kreistagswahl.xlsx` (sheet "Stimmbezirk KW14")
# 17 municipalities carry another municipality's Stimmbezirke. This is a SOURCE
# defect, not a parsing fault: BOTH label columns (col 2 GKZ_Gemeinde and col 3
# Gemeinde-Name) are populated and wrong together, so the parser aggregates them
# faithfully into the wrong Gemeinde. Same shape as the IT.NRW Stichwahl-date
# error patched in the mayoral pipeline — remove this block once IT.NRW
# corrects the file (audit fix C-16, 2026-07).
#
# Two distinct mechanisms, both verified against the 2020 file and against
# municipal_unharm for the same election day:
#
#  (a) Kreis Viersen 05166 — a deterministic ONE-POSITION CYCLIC SHIFT of five
#      municipality labels. The Stimmbezirk-number prefix is stable between
#      2014 and 2020, and in 2014 the prefix-8 block (76 precincts, 62,182
#      eligible voters) is labelled 05166028 Tönisvorst although 2020 shows
#      prefix 8 = Viersen (63,189). Brüggen/Grefrath/Kempen/Nettetal
#      (prefixes 1-4) are correct. After the shift all 9 Viersen municipalities
#      reconcile to within 0-21 voters of the municipal pipeline.
#
#  (b) Kreis Coesfeld 05558 and Kreis Steinfurt 05566 — not block swaps but six
#      individual foreign precinct groups, recognisable because they duplicate a
#      Stimmbezirk number already used inside the block and because the precinct
#      NAME belongs to the neighbouring municipality (Capelle is a Nordkirchen
#      Ortsteil, Darfeld a Rosendahl one, …). After these 12 rows are moved all
#      35 Coesfeld+Steinfurt municipalities land within 50 voters of the
#      municipal pipeline.

# ags -> corrected ags, applied in ONE vectorised pass (the map is cyclic)
nrw2014_viersen_shift <- c(
  "05166036" = "05166020",  # labelled Willich       -> Niederkrüchten
  "05166020" = "05166024",  # labelled Niederkrüchten -> Schwalmtal
  "05166024" = "05166028",  # labelled Schwalmtal     -> Tönisvorst
  "05166028" = "05166032",  # labelled Tönisvorst     -> Viersen
  "05166032" = "05166036"   # labelled Viersen        -> Willich
)

# Each entry: the wrong ags, the Stimmbezirk numbers, a name pattern unique to
# the group, the correct ags, and the exact number of rows it must match.
nrw2014_precinct_moves <- list(
  list(ags = "05558004", sb = 12:14, pattern = "Capelle",
       new_ags = "05558028", n_rows = 3L),   # Ascheberg   -> Nordkirchen
  list(ags = "05558008", sb = 1:4, pattern = "Darfeld",
       new_ags = "05558040", n_rows = 4L),   # Billerbeck  -> Rosendahl
  list(ags = "05558020", sb = 16, pattern = "Am Detterbach",
       new_ags = "05558032", n_rows = 1L),   # Havixbeck   -> Nottuln
  list(ags = "05558036", sb = 10, pattern = "Gemeinschaftshauptschule",
       new_ags = "05558024", n_rows = 1L),   # Olfen       -> Lüdinghausen
  list(ags = "05566064", sb = 4, pattern = "Regenbogenschule",
       new_ags = "05566084", n_rows = 1L),   # Nordwalde   -> Steinfurt
  list(ags = "05566096", sb = 3, pattern = "Kinderkiste",
       new_ags = "05566068", n_rows = 1L),   # Wettringen  -> Ochtrup
  list(ags = "05566096", sb = 9, pattern = "Laurenz Genusswerk",
       new_ags = "05566068", n_rows = 1L)    # Wettringen  -> Ochtrup
)

#' Re-label the mislabelled 2014 Stimmbezirk rows. Fails loudly if the raw file
#' no longer matches, so a corrected IT.NRW release cannot be silently patched.
nrw2014_fix_labels <- function(ags, sb_no, sb_name) {
  sb_no <- suppressWarnings(as.numeric(sb_no))
  sb_name <- trimws(as.character(sb_name))

  for (r in nrw2014_precinct_moves) {
    hit <- ags == r$ags & sb_no %in% r$sb &
      grepl(r$pattern, sb_name, fixed = TRUE)
    if (sum(hit) != r$n_rows) {
      stop("NRW 2014: precinct-move rule ", r$ags, " '", r$pattern, "' matched ",
           sum(hit), " rows, expected ", r$n_rows,
           " — the raw file changed, re-verify the remap")
    }
    ags[hit] <- r$new_ags
  }

  vhit <- ags %in% names(nrw2014_viersen_shift)
  if (!all(names(nrw2014_viersen_shift) %in% ags)) {
    stop("NRW 2014: Kreis Viersen label shift does not apply — ",
         "the raw file changed, re-verify the remap")
  }
  ags[vhit] <- unname(nrw2014_viersen_shift[ags[vhit]])

  ags
}

# Parse NRW late format (2014, 2020): two header rows, GKZ_Gemeinde in col 2
parse_nrw_late <- function(path, year) {
  cat("  NRW", year, "...")
  sheet <- nrw_sheet_map[[as.character(year)]]
  raw <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE))
  headers <- tolower(trimws(as.character(raw[1, ])))

  # Find key columns from row 1 headers
  gkz_col <- 2  # GKZ_Gemeinde is always col 2 in late format
  name_col <- 3
  ev_col <- which(grepl("wahlberechtigte insgesamt", headers))[1]
  voter_col <- which(grepl("^w.hler", headers))[1]
  invalid_col <- which(grepl("ung.ltige", headers))[1]
  valid_col <- which(grepl("g.ltige stimmen", headers) & !grepl("ung.ltige", headers))[1]

  # Party columns start after valid
  party_start <- valid_col + 1
  party_cols_idx <- party_start:ncol(raw)

  # Data starts from row 3 (skip two header rows)
  data <- raw[3:nrow(raw), ]

  # Extract GKZ
  gkz_raw <- as.numeric(as.character(data[[gkz_col]]))
  gkz <- sprintf("%06d", as.integer(round(gkz_raw)))
  valid_rows <- !is.na(gkz_raw) & nchar(gkz) == 6

  data <- data[valid_rows, ]
  gkz <- gkz[valid_rows]
  ags <- paste0("05", gkz)

  ags_name <- as.character(data[[name_col]])

  # Repair the 2014 municipality mislabelling BEFORE the Stimmbezirke are
  # aggregated by AGS (see nrw2014_fix_labels). Col 4 = Stimmbezirk number,
  # col 5 = Stimmbezirk name. The Gemeinde-Name column is wrong in exactly the
  # same rows, so re-derive it from the pre-remap ags -> name lookup (every
  # target AGS also appears with its own, correct, name in the file).
  if (year == 2014) {
    name_lookup <- tapply(ags_name, ags, function(x) x[1])
    ags <- nrw2014_fix_labels(ags, data[[4]], data[[5]])
    ags_name <- unname(name_lookup[ags])
  }

  df <- data.frame(
    ags = ags,
    ags_name = ags_name,
    eligible_voters = as.numeric(as.character(data[[ev_col]])),
    number_voters = as.numeric(as.character(data[[voter_col]])),
    invalid_votes = as.numeric(as.character(data[[invalid_col]])),
    valid_votes = as.numeric(as.character(data[[valid_col]])),
    stringsAsFactors = FALSE
  )

  # Party columns
  wgr_cols <- c()
  party_names_map <- list()
  for (ci in party_cols_idx) {
    pname_raw <- headers[ci]
    if (is.na(pname_raw) || pname_raw == "" || pname_raw == "na") next
    pname_raw <- gsub("-\\s*\r?\n\\s*", "", pname_raw)
    pname_raw <- gsub("\\s+", " ", trimws(pname_raw))

    if (grepl("^wgr", pname_raw)) {
      wgr_cols <- c(wgr_cols, ci)
      next
    }
    pname <- normalise_party_cty(pname_raw)
    vals <- as.numeric(as.character(data[[ci]]))
    if (pname %in% names(df)) {
      df[[pname]] <- df[[pname]] + ifelse(is.na(vals), 0, vals)
    } else {
      df[[pname]] <- vals
    }
    party_names_map[[pname]] <- TRUE
  }

  # Sum WGR columns
  if (length(wgr_cols) > 0) {
    wgr_sum <- rowSums(sapply(wgr_cols, function(ci) {
      as.numeric(as.character(data[[ci]]))
    }), na.rm = TRUE)
    wgr_all_na <- rowSums(!is.na(sapply(wgr_cols, function(ci) {
      as.numeric(as.character(data[[ci]]))
    }))) == 0
    wgr_sum[wgr_all_na] <- NA_real_
    df$waehlergruppen <- wgr_sum
    party_names_map[["waehlergruppen"]] <- TRUE
  }

  # Aggregate Stimmbezirk to municipality
  vote_cols <- names(party_names_map)
  meta_num <- c("eligible_voters", "number_voters", "invalid_votes", "valid_votes")
  agg <- df |>
    group_by(ags) |>
    summarise(
      ags_name = first(ags_name),
      across(all_of(meta_num), ~sum(.x, na.rm = TRUE)),
      across(any_of(vote_cols), ~{
        if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
      }),
      .groups = "drop"
    )

  # Convert to shares
  for (pc in vote_cols) {
    if (pc %in% names(agg)) {
      agg[[pc]] <- ifelse(!is.na(agg$valid_votes) & agg$valid_votes > 0,
                          agg[[pc]] / agg$valid_votes, NA_real_)
    }
  }

  # Compute "other" as residual
  all_pcols <- intersect(vote_cols, names(agg))
  share_sum <- rowSums(agg[all_pcols], na.rm = TRUE)
  agg$other <- pmax(1 - share_sum, 0)
  agg$other[is.na(agg$valid_votes) | agg$valid_votes == 0] <- NA_real_

  agg$turnout <- ifelse(!is.na(agg$eligible_voters) & agg$eligible_voters > 0,
                        agg$number_voters / agg$eligible_voters, NA_real_)
  agg$county <- substr(agg$ags, 1, 5)
  agg$state <- "05"
  agg$election_year <- as.integer(year)

  cat(nrow(agg), "municipalities\n")
  as_tibble(agg)
}

# Process NRW files
nrw_results <- list()
for (yr in c(1999, 2004, 2009)) {
  ext <- ".xls"
  f <- file.path(nrw_dir, paste0("Nordrhein-Westfalen_", yr, "_Kreistagswahl", ext))
  nrw_results[[as.character(yr)]] <- tryCatch(
    parse_nrw_early(f, yr),
    error = function(e) { cat("  NRW", yr, "ERROR:", conditionMessage(e), "\n"); NULL }
  )
}
for (yr in c(2014, 2020)) {
  ext <- ".xlsx"
  f <- file.path(nrw_dir, paste0("Nordrhein-Westfalen_", yr, "_Kreistagswahl", ext))
  nrw_results[[as.character(yr)]] <- tryCatch(
    parse_nrw_late(f, yr),
    error = function(e) { cat("  NRW", yr, "ERROR:", conditionMessage(e), "\n"); NULL }
  )
}

nrw_results <- nrw_results[!sapply(nrw_results, is.null)]
df_nrw <- bind_rows(nrw_results)
cat("NRW total:", nrow(df_nrw), "rows x", ncol(df_nrw), "cols\n")
cat("NRW years:", paste(sort(unique(df_nrw$election_year)), collapse = ", "), "\n")
df_nrw |> count(election_year) |> print()


# =============================================================================
# RHEINLAND-PFALZ (RP) — Gemeinde-level, 1964–2019
# =============================================================================
# Source: Statistisches Landesamt Rheinland-Pfalz, Sonderauswertung 111-AD26-0373
#   "Kreistagswahlen sowie Stadtratswahlen kreisfreier Städte in Rheinland-Pfalz
#   1964-2019" (received July 2026). Twelve elections, ~2,295 Gemeinden each.
#
# NOTE: the StaLA reports every election on the CURRENT (2025) municipal
#   boundaries, i.e. this series is already boundary-harmonised at source.
#
# Layout (sheet KT_Gemeindeebene_Parteistimmen): four header rows, then one row
#   per (Gemeinde, Wahltag).
#   col 1 Schlüssel (printed once per Gemeinde block -> fill down), col 2 name,
#   col 3 Stichtag, cols 4-29 "Gültige Stimmen" (25 parties + Gesamtsumme),
#   cols 30-55 "Ungewichtete Stimmen" (same columns, populated from 1994).
#   Sheet KT_Gemeindeebene_Wahlbet carries Wahlberechtigte / Wähler.
#
# RLP uses Kumulieren/Panaschieren: the party figures are the *gewichteten*
#   Stimmen, rescaled so that they sum to the number of valid ballots, so
#   Gesamtsumme is directly comparable to Wähler and the standard
#   invalid = Wähler - gültig formula applies.
#
# Schlüssel = Kreis(3) + Verbandsgemeinde(2) + Gemeinde(3);
#   AGS = "07" + Kreis + Gemeinde (the VG digits are not part of the AGS).

cat("\n===== RHEINLAND-PFALZ =====\n")

rp_file <- file.path(raw_dir, "Rheinland-Pfalz",
                     "Rheinland-Pfalz_1964-2019_Kreistagswahlen_StaLA.xlsx")

df_rp <- NULL
if (file.exists(rp_file)) {
  tryCatch({
    rp_raw <- read_excel(rp_file, sheet = "KT_Gemeindeebene_Parteistimmen",
                         col_names = FALSE, col_types = "text")
    rp_wb_raw <- read_excel(rp_file, sheet = "KT_Gemeindeebene_Wahlbet",
                            col_names = FALSE, col_types = "text")

    # Party names sit in header row 4 and repeat identically in both blocks
    rp_parties <- as.character(unlist(rp_raw[4, 4:29], use.names = FALSE))
    stopifnot(length(rp_parties) == 26,
              rp_parties[26] == "Gesamtsumme",
              identical(as.character(unlist(rp_raw[4, 30:55], use.names = FALSE)),
                        rp_parties))

    # Build column by column: read_excel(col_names = FALSE) tibbles mangle names
    # when assigned via names<-/setNames.
    rp_rows <- 5:nrow(rp_raw)
    rp <- data.frame(row.names = seq_along(rp_rows), check.names = FALSE)
    rp[["key"]]      <- as.character(rp_raw[[1]])[rp_rows]
    rp[["ags_name"]] <- as.character(rp_raw[[2]])[rp_rows]
    rp[["stichtag"]] <- as.character(rp_raw[[3]])[rp_rows]
    for (i in seq_along(rp_parties)) {
      rp[[paste0("v__", rp_parties[i])]] <-
        as.numeric(as.character(rp_raw[[3 + i]])[rp_rows])
    }

    rp_wb_rows <- 5:nrow(rp_wb_raw)
    rp_wb <- data.frame(row.names = seq_along(rp_wb_rows), check.names = FALSE)
    rp_wb[["key"]]             <- as.character(rp_wb_raw[[1]])[rp_wb_rows]
    rp_wb[["stichtag"]]        <- as.character(rp_wb_raw[[3]])[rp_wb_rows]
    rp_wb[["eligible_voters"]] <- as.numeric(as.character(rp_wb_raw[[4]])[rp_wb_rows])
    rp_wb[["number_voters"]]   <- as.numeric(as.character(rp_wb_raw[[5]])[rp_wb_rows])

    # Schlüssel/name are printed only once per Gemeinde block -> fill down
    rp    <- rp    |> tidyr::fill(key, ags_name, .direction = "down")
    rp_wb <- rp_wb |> tidyr::fill(key, .direction = "down")

    rp <- rp |> left_join(rp_wb, by = c("key", "stichtag"))
    stopifnot(!anyNA(rp$eligible_voters))

    rp_ags <- paste0("07", substr(rp$key, 1, 3), substr(rp$key, 6, 8))
    stopifnot(all(nchar(rp_ags) == 8))

    df_rp <- data.frame(
      ags           = rp_ags,
      ags_name      = trimws(rp$ags_name),
      county        = substr(rp_ags, 1, 5),
      state         = "07",
      election_year = as.integer(substr(rp$stichtag, 7, 10)),
      stringsAsFactors = FALSE
    )
    df_rp$eligible_voters <- rp$eligible_voters
    df_rp$number_voters   <- rp$number_voters
    df_rp$valid_votes     <- rp[["v__Gesamtsumme"]]
    df_rp$invalid_votes   <- df_rp$number_voters - df_rp$valid_votes
    # Four 2009 Gemeinden in Kreis Trier-Saarburg report 1-9 more valid ballots
    # than Wähler (Hinzert-Pölert, Longen, Naurath (Eifel), Thörnich) — a
    # Briefwahl allocation artefact in the source. Counts are kept as reported;
    # only the derived (negative) invalid_votes is blanked.
    n_rp_neg <- sum(df_rp$invalid_votes < 0, na.rm = TRUE)
    if (n_rp_neg > 0) {
      cat("  valid_votes > number_voters in", n_rp_neg,
          "rows (source artefact) -> invalid_votes set to NA\n")
      df_rp$invalid_votes[df_rp$invalid_votes < 0] <- NA_real_
    }
    df_rp$turnout <- ifelse(!is.na(df_rp$eligible_voters) & df_rp$eligible_voters > 0,
                            df_rp$number_voters / df_rp$eligible_voters, NA_real_)

    for (p in setdiff(rp_parties, "Gesamtsumme")) {
      pname <- normalise_party_cty(tolower(clean_header(p)))
      sh <- ifelse(!is.na(df_rp$valid_votes) & df_rp$valid_votes > 0,
                   rp[[paste0("v__", p)]] / df_rp$valid_votes, NA_real_)
      df_rp[[pname]] <- if (!is.null(df_rp[[pname]]))
        rowSums(cbind(df_rp[[pname]], sh), na.rm = TRUE) else sh
    }

    # Prefer the (unabbreviated) crosswalk names over the truncated StaLA column
    rp_names <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
      mutate(ags = sprintf("%08d", as.integer(ags))) |>
      filter(year == 2020, substr(ags, 1, 2) == "07") |>
      distinct(ags, cw_name = ags_name)
    df_rp <- df_rp |>
      left_join(rp_names, by = "ags") |>
      mutate(ags_name = coalesce(cw_name, ags_name)) |>
      select(-cw_name)

    stopifnot(
      nrow(df_rp) == nrow(unique(df_rp[, c("ags", "election_year")])),
      length(unique(df_rp$election_year)) == 12
    )

    # Two known source anomalies with Wähler > Wahlberechtigte (kept as reported;
    # both are internally consistent, i.e. gültig < Wähler):
    #   Börfink 1969 (07134011, 146/140) and Urbar 1989 (07137224, 3197/2197 —
    #   the Gemeinderat sheet reports 1668 voters for the same Gemeinde/year).
    n_rp_bad <- sum(df_rp$turnout > 1, na.rm = TRUE)
    cat("  turnout > 1 (known source anomalies):", n_rp_bad, "\n")

    cat("RP total:", nrow(df_rp), "rows x", ncol(df_rp), "cols\n")
    cat("RP years:", paste(sort(unique(df_rp$election_year)), collapse = ", "), "\n")
    df_rp |> count(election_year) |> as_tibble() |> print(n = 20)
  }, error = function(e) cat("  ERROR in RP:", conditionMessage(e), "\n"))
} else {
  cat("  RP file not found:", rp_file, "\n")
}

# =============================================================================
# Combine all states and write output
# =============================================================================

cat("\n===== COMBINING =====\n")

all_dfs <- list(df_st, df_th, df_mv, df_sn, df_bb)
if (exists("df_by") && nrow(df_by) > 0) all_dfs <- c(all_dfs, list(df_by))
if (exists("df_sl") && nrow(df_sl) > 0) all_dfs <- c(all_dfs, list(df_sl))
if (exists("df_he") && nrow(df_he) > 0) all_dfs <- c(all_dfs, list(df_he))
if (exists("df_bw") && nrow(df_bw) > 0) all_dfs <- c(all_dfs, list(df_bw))
if (exists("df_sh") && nrow(df_sh) > 0) all_dfs <- c(all_dfs, list(df_sh))
if (exists("df_ni") && nrow(df_ni) > 0) all_dfs <- c(all_dfs, list(df_ni))
if (exists("df_nrw") && nrow(df_nrw) > 0) all_dfs <- c(all_dfs, list(df_nrw))
if (exists("df_rp") && !is.null(df_rp) && nrow(df_rp) > 0) all_dfs <- c(all_dfs, list(df_rp))
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
