### European Elections 2009-2024: Municipality-Level Results
# Aggregates ballot-district data to municipality level
# Handles Amt/VG-level mail-in (Briefwahl) vote allocation
# Processes all 4 European Parliament elections: 2009, 2014, 2019, 2024
# Vincent Heddesheimer
# April 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "haschaR")
conflict_prefer("filter", "dplyr")


# --- 0. Party name normalisation ---------------------------------------------

normalise_party_eu <- function(pname) {
  mapping <- c(
    # Major parties
    "CDU"                     = "cdu",
    "SPD"                     = "spd",
    "CSU"                     = "csu",
    "FDP"                     = "fdp",
    "AfD"                     = "afd",
    "BSW"                     = "bsw",

    # Greens
    "GR\u00dcNE"              = "gruene",
    "GRÜNE"                   = "gruene",

    # Left
    "DIE LINKE"               = "die_linke",

    # Tierschutz variants
    "Tierschutzpartei"        = "tierschutz",
    "Die Tierschutzpartei"    = "tierschutz",
    "TIERSCHUTZ hier!"        = "tierschutz_hier",
    "Tierschutzallianz"       = "tierschutzallianz",
    "PARTEI F\u00dcR DIE TIERE" = "partei_fuer_die_tiere",
    "PARTEI FÜR DIE TIERE"   = "partei_fuer_die_tiere",

    # Free voters
    "FREIE W\u00c4HLER"      = "freie_waehler",
    "FREIE WÄHLER"            = "freie_waehler",
    "FW FREIE W\u00c4HLER"   = "freie_waehler",
    "FW FREIE WÄHLER"         = "freie_waehler",

    # ÖDP
    "\u00d6DP"                = "oedp",
    "ÖDP"                     = "oedp",
    "\u00f6dp"                = "oedp",
    "ödp"                     = "oedp",

    # Die PARTEI
    "Die PARTEI"              = "die_partei",
    "DIE PARTEI"              = "die_partei",

    # PIRATEN
    "PIRATEN"                 = "piraten",

    # FAMILIE
    "FAMILIE"                 = "familie",

    # Volt
    "Volt"                    = "volt",

    # NPD / HEIMAT
    "NPD"                     = "npd",
    "HEIMAT"                  = "heimat",

    # REP
    "REP"                     = "rep",

    # DVU
    "DVU"                     = "dvu",

    # BIG
    "BIG"                     = "big",

    # Bündnis C
    "B\u00fcndnis C"          = "buendnis_c",
    "Bündnis C"               = "buendnis_c",

    # BÜNDNIS DEUTSCHLAND
    "B\u00dcNDNIS DEUTSCHLAND" = "buendnis_deutschland",
    "BÜNDNIS DEUTSCHLAND"     = "buendnis_deutschland",

    # BP
    "BP"                      = "bp",

    # DKP
    "DKP"                     = "dkp",

    # MLPD
    "MLPD"                    = "mlpd",

    # PSG / SGP
    "PSG"                     = "psg",
    "SGP"                     = "sgp",

    # BüSo
    "B\u00fcSo"               = "bueso",
    "BüSo"                    = "bueso",

    # Volksabstimmung
    "Volksabstimmung"         = "volksabstimmung",

    # PBC
    "PBC"                     = "pbc",

    # CM
    "CM"                      = "cm",

    # DIE FRAUEN
    "DIE FRAUEN"              = "die_frauen",

    # MENSCHLICHE WELT
    "MENSCHLICHE WELT"        = "menschliche_welt",

    # V-Partei³
    "V-Partei\u00b3"          = "v_partei3",
    "V-Partei³"               = "v_partei3",

    # dieBasis
    "dieBasis"                = "die_basis",

    # DAVA
    "DAVA"                    = "dava",

    # KLIMALISTE
    "KLIMALISTE"              = "klimaliste",

    # LETZTE GENERATION
    "LETZTE GENERATION"       = "letzte_generation",

    # PDV
    "PDV"                     = "pdv",

    # PdF
    "PdF"                     = "pdf_partei",

    # PdH
    "PdH"                     = "pdh",

    # ABG
    "ABG"                     = "abg",

    # MERA25
    "MERA25"                  = "mera25",

    # Verjüngungsforschung
    "Verj\u00fcngungsforschung" = "verjuengungsforschung",
    "Verjüngungsforschung"    = "verjuengungsforschung",

    # 2009-specific parties
    "AUFBRUCH"                = "aufbruch",
    "50Plus"                  = "x50plus",
    "AUF"                     = "auf",
    "DIE GRAUEN"              = "die_grauen",
    "Die Grauen"              = "die_grauen",
    "DIE VIOLETTEN"           = "die_violetten",
    "EDE"                     = "ede",
    "FBI"                     = "fbi",
    "F\u00dcR VOLKSENTSCHEIDE" = "fuer_volksentscheide",
    "FÜR VOLKSENTSCHEIDE"     = "fuer_volksentscheide",
    "Newropeans"              = "newropeans",
    "RRP"                     = "rrp",
    "RENTNER"                 = "rentner",

    # 2014-specific
    "PRO NRW"                 = "pro_nrw",

    # 2019-specific
    "BGE"                     = "bge",
    "DIE DIREKTE!"            = "die_direkte",
    "DiEM25"                  = "diem25",
    "III. Weg"                = "iii_weg",
    "DIE RECHTE"              = "die_rechte",
    "LIEBE"                   = "liebe",
    "Graue Panther"           = "graue_panther",
    "LKR"                     = "lkr",
    "NL"                      = "nl",
    "\u00d6koLinX"            = "oekolinx",
    "ÖkoLinX"                 = "oekolinx",
    "Die Humanisten"          = "die_humanisten",
    "Gesundheitsforschung"    = "gesundheitsforschung"
  )

  result <- mapping[pname]
  if (!is.na(result)) return(unname(result))

  # Fallback: clean to snake_case
  cleaned <- tolower(pname)
  cleaned <- gsub("\u00e4", "ae", cleaned)
  cleaned <- gsub("\u00f6", "oe", cleaned)
  cleaned <- gsub("\u00fc", "ue", cleaned)
  cleaned <- gsub("\u00df", "ss", cleaned)
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  return(cleaned)
}


# --- 1. Year configurations --------------------------------------------------

ew_configs <- list(
  list(
    year       = 2009L,
    date       = "2009-06-07",
    file       = "data/european_elections/raw/ew09_wbz/EW09_Wbz_Ergebnisse_utf8.csv",
    sep        = "\t",
    skip       = 4,
    encoding   = "UTF-8",
    elig_col   = "Wahlberechtigte (A)",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte nach § 25 Abs. 2 BWO (A3)",
    voters_col = "Wähler (B)",
    wahlschein_col = "Wähler mit Wahlschein (B1)",
    invalid_col = "Ungültig",
    valid_col   = "Gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = c()
  ),
  list(
    year       = 2014L,
    date       = "2014-05-25",
    file       = "data/european_elections/raw/ew14_wbz/EW14_Wbz_Ergebnisse.csv",
    sep        = ";",
    skip       = 4,
    encoding   = "UTF-8",
    elig_col   = "Wahlberechtigte (A)",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte nach § 25 Abs. 2 BWO (A3)",
    voters_col = "Wähler (B)",
    wahlschein_col = "Wähler mit Wahlschein (B1)",
    invalid_col = "Ungültig",
    valid_col   = "Gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = c()
  ),
  list(
    year       = 2019L,
    date       = "2019-05-26",
    file       = "data/european_elections/raw/ew19_wbz/ew19_wbz_ergebnisse.csv",
    sep        = ";",
    skip       = 4,
    encoding   = "Latin-1",
    elig_col   = "Wahlberechtigte (A)",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte nach § 24 Abs. 2 EuWO (A3)",
    voters_col = "Wähler (B)",
    wahlschein_col = "Wähler mit Wahlschein (B1)",
    invalid_col = "Ungültig",
    valid_col   = "Gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = "Ungekürzte Wahlbezirksbezeichnung"
  ),
  list(
    year       = 2024L,
    date       = "2024-06-09",
    file       = "data/european_elections/raw/ew24_wbz/ew24_wbz_ergebnisse.csv",
    sep        = ";",
    skip       = 0,
    encoding   = "UTF-8",
    elig_col   = "Wahlberechtigte",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte § 24 (2) EuWO (A3)",
    voters_col = "Wählende",
    wahlschein_col = "dar. Wählende mit Wahlschein",
    invalid_col = "ungültig",
    valid_col   = "gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = c("Kennziffer zusammengelegte Urnenwahlbezirke § 61 EuWO",
                    "Optional: Ungekürzte Wahlbezirksbezeichnung")
  )
)


# --- 2. Processing function ---------------------------------------------------

process_ew_year <- function(cfg) {

  cat("\n========== Processing European Election", cfg$year, "==========\n")

  # 2a: Read raw data — read as character to prevent fread from misinterpreting
  # German thousands separators (e.g., "1.510" = 1510) as decimal values.
  # Without colClasses = "character", fread parses "1.510" as numeric 1.51,
  # then as.character(1.51) drops the trailing zero, and gsub produces "151" not "1510".
  df <- fread(
    cfg$file,
    sep = cfg$sep,
    skip = cfg$skip,
    header = TRUE,
    encoding = cfg$encoding,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  cat("Raw rows:", nrow(df), "| Cols:", ncol(df), "\n")

  # 2b: Drop non-data columns
  for (dc in cfg$drop_cols) {
    if (dc %in% names(df)) df[[dc]] <- NULL
  }

  # 2c: Standardize meta column names
  # Rename year-specific voter column names to common names
  rename_map <- c(
    "eligible_voters"        = cfg$elig_col,
    "voters_wo_sperrvermerk" = cfg$a1_col,
    "voters_w_sperrvermerk"  = cfg$a2_col,
    "voters_par24_2"         = cfg$a3_col,
    "number_voters"          = cfg$voters_col,
    "voters_w_wahlschein"    = cfg$wahlschein_col,
    "invalid_votes"          = cfg$invalid_col,
    "valid_votes"            = cfg$valid_col,
    "BA"                     = cfg$ba_col,
    "BWBez"                  = cfg$bwbez_col
  )
  df <- df |> rename(any_of(rename_map))

  # 2d: Construct 8-digit AGS
  df <- df |>
    mutate(
      Land = pad_zero_conditional(Land, 1),
      Kreis = pad_zero_conditional(Kreis, 1),
      Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
      Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
      ags = paste0(Land, Regierungsbezirk, Kreis, Gemeinde),
      county = substr(ags, 1, 5)
    )

  # 2e: Identify party columns (everything after valid_votes meta cols)
  vote_meta_cols <- c(
    "eligible_voters", "voters_wo_sperrvermerk", "voters_w_sperrvermerk",
    "voters_par24_2", "number_voters", "voters_w_wahlschein",
    "invalid_votes", "valid_votes"
  )
  geo_cols <- c("Land", "Regierungsbezirk", "Kreis", "Verbandsgemeinde",
                "Gemeinde", "Wahlbezirk", "ags", "county", "BA", "BWBez")
  party_cols <- setdiff(names(df), c(geo_cols, vote_meta_cols))
  numeric_cols <- c(vote_meta_cols, party_cols)

  cat("Party columns found:", length(party_cols), "\n")
  cat("  ", paste(head(party_cols, 10), collapse = ", "), "...\n")

  # 2f: Ensure numeric (strip German thousands separators)
  df <- df |>
    mutate(across(all_of(numeric_cols), ~ as.numeric(gsub("\\.", "", as.character(.x)))))

  # 2g: Remap BA=6→0 and BA=8→0 (Sonderwahlbezirke → polling station equivalent)
  n_ba6 <- sum(df$BA == 6, na.rm = TRUE)
  n_ba8 <- sum(df$BA == 8, na.rm = TRUE)
  if (n_ba6 > 0 || n_ba8 > 0) {
    cat("Remapping BA: 6→0 (", n_ba6, "rows), 8→0 (", n_ba8, "rows)\n")
  }
  df <- df |> mutate(BA = ifelse(BA %in% c(6, 8), 0L, as.integer(BA)))

  # 2h: Remove NI Samtgemeinde aggregate rows (Gemeinde suffix >= 400)
  n_sg <- sum(df$Land == "03" & as.integer(df$Gemeinde) >= 400, na.rm = TRUE)
  if (n_sg > 0) {
    cat("Removing", n_sg, "NI Samtgemeinde aggregate rows\n")
    df <- df |> filter(!(Land == "03" & as.integer(Gemeinde) >= 400))
  }

  # 2i: Distribute Gem=999 dummy rows (Sonderwahlbezirke without real municipality)
  # Remap their BA to 5 so they get distributed proportionally across real

  # municipalities in the same (county, BWBez) group via the Briefwahl logic
  n_gem999 <- sum(df$Gemeinde == "999" & df$BA == 0, na.rm = TRUE)
  if (n_gem999 > 0) {
    cat("Remapping", n_gem999, "Gem=999 BA=0 rows to BA=5 for proportional distribution\n")
    df <- df |> mutate(BA = ifelse(Gemeinde == "999" & BA == 0, 5L, BA))
  }

  # --- 3. Aggregate ballot districts by (ags, BWBez, BA) ---
  df_agg <- df |>
    group_by(ags, county, BWBez, BA) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # --- 4. Identify municipality types ---
  ags_with_ba0 <- df_agg |> filter(BA == 0) |> pull(ags) |> unique()
  ags_with_ba5 <- df_agg |> filter(BA == 5) |> pull(ags) |> unique()
  mailin_only_ags <- setdiff(ags_with_ba5, ags_with_ba0)
  real_no_mailin <- setdiff(ags_with_ba0, ags_with_ba5)

  cat("Real municipalities:", length(ags_with_ba0), "\n")
  cat("  with own mail-in:", length(intersect(ags_with_ba0, ags_with_ba5)), "\n")
  cat("  without own mail-in:", length(real_no_mailin), "\n")
  cat("Mail-in-only AGS (to distribute):", length(mailin_only_ags), "\n")

  # --- 5. Municipalities with own mail-in: sum all BA types ---
  df_own_mailin <- df_agg |>
    filter(ags %in% intersect(ags_with_ba0, ags_with_ba5)) |>
    group_by(ags, county) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # --- 6. Municipalities without own mail-in: allocate from Amt-level ---
  # 6a: Get polling station totals
  df_no_mailin <- df_agg |>
    filter(ags %in% real_no_mailin & BA == 0) |>
    group_by(ags, county, BWBez) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # 6b: Get Amt-level mail-in totals
  df_mailin <- df_agg |>
    filter(ags %in% mailin_only_ags & BA == 5) |>
    group_by(county, BWBez) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # 6c: Calculate eligible-voter weights within each (county, BWBez) group
  df_no_mailin <- df_no_mailin |>
    group_by(county, BWBez) |>
    mutate(
      group_elig = sum(eligible_voters, na.rm = TRUE),
      elig_weight = ifelse(
        group_elig > 0,
        eligible_voters / group_elig,
        1 / n()
      )
    ) |>
    ungroup()

  # 6d: Distribute mail-in votes
  mailin_long <- df_mailin |>
    pivot_longer(cols = all_of(numeric_cols), names_to = "var", values_to = "mailin_value")

  df_no_mailin_long <- df_no_mailin |>
    pivot_longer(cols = all_of(numeric_cols), names_to = "var", values_to = "ags_value") |>
    left_join(mailin_long, by = c("county", "BWBez", "var")) |>
    mutate(
      weighted_mailin = round(coalesce(mailin_value, 0) * elig_weight, 0),
      final_value = ags_value + weighted_mailin
    )

  # 6e: Pivot back to wide
  df_allocated <- df_no_mailin_long |>
    select(ags, county, var, final_value) |>
    pivot_wider(names_from = var, values_from = final_value)

  # --- 7. Combine all municipalities ---
  df_muni <- bind_rows(df_own_mailin, df_allocated) |> arrange(ags)

  # Remove uninhabited areas (eligible_voters=0, number_voters=0)
  n_zero <- sum(df_muni$eligible_voters == 0 & df_muni$number_voters == 0, na.rm = TRUE)
  if (n_zero > 0) {
    cat("Removing", n_zero, "zero-voter municipalities (gemeindefreie Gebiete)\n")
    df_muni <- df_muni |> filter(!(eligible_voters == 0 & number_voters == 0))
  }

  cat("Final municipality count:", nrow(df_muni), "\n")

  # Check duplicates
  dupl <- df_muni |> count(ags) |> filter(n > 1)
  if (nrow(dupl) > 0) {
    warning("Duplicate AGS found in ", cfg$year, ": ", paste(dupl$ags, collapse = ", "))
  }

  # --- 8. Normalise party column names ---
  # Apply normalise_party_eu to party columns
  old_party_names <- intersect(party_cols, names(df_muni))
  new_party_names <- sapply(old_party_names, normalise_party_eu)

  # Check for duplicates in new names
  if (any(duplicated(new_party_names))) {
    dups <- new_party_names[duplicated(new_party_names)]
    warning("Duplicate normalised party names in ", cfg$year, ": ",
            paste(unique(dups), collapse = ", "), " — will aggregate")
    # Rename then aggregate duplicates
    names(df_muni)[match(old_party_names, names(df_muni))] <- new_party_names
    df_muni <- df_muni |>
      group_by(ags, county) |>
      summarise(across(everything(), ~ if (is.numeric(.x)) sum(.x, na.rm = TRUE) else first(.x)),
                .groups = "drop")
  } else {
    names(df_muni)[match(old_party_names, names(df_muni))] <- new_party_names
  }

  # --- 9. Add metadata ---
  df_muni <- df_muni |>
    mutate(
      state = substr(ags, 1, 2),
      state_name = state_id_to_names(state),
      election_year = cfg$year,
      election_date = lubridate::ymd(cfg$date)
    )

  cat("Columns:", ncol(df_muni), "\n")
  return(df_muni)
}


# --- 3. Process all years -----------------------------------------------------

all_years <- map(ew_configs, process_ew_year)

# Combine: bind_rows fills missing party columns with NA
df_all <- bind_rows(all_years)

cat("\n========== Combined ==========\n")
cat("Total rows:", nrow(df_all), "\n")
cat("Total cols:", ncol(df_all), "\n")

# Identify all party columns (exclude meta/geo/flag columns)
meta_cols <- c("ags", "county", "state", "state_name", "election_year",
               "election_date", "eligible_voters", "number_voters",
               "valid_votes", "invalid_votes", "voters_wo_sperrvermerk",
               "voters_w_sperrvermerk", "voters_par24_2", "voters_w_wahlschein")
party_cols_all <- sort(setdiff(names(df_all), c(meta_cols, "turnout",
                                                 "flag_turnout_above_1")))

cat("Total unique parties:", length(party_cols_all), "\n")

# Replace NA with 0 for party columns (party not running = 0 votes)
df_all <- df_all |>
  mutate(across(all_of(party_cols_all), ~ replace_na(.x, 0)))


# --- 4. Compute turnout and vote shares ---------------------------------------

df_all <- df_all |>
  mutate(
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    across(all_of(party_cols_all), ~ ifelse(number_voters > 0, .x / number_voters, NA_real_))
  )

# Cap turnout at 1 and flag
df_all <- df_all |>
  mutate(
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout)
  )


# --- 5. Reorder columns and write ---------------------------------------------

df_all <- df_all |>
  select(
    ags, county, state, state_name,
    election_year, election_date,
    eligible_voters, number_voters, valid_votes, invalid_votes,
    voters_wo_sperrvermerk, voters_w_sperrvermerk, voters_par24_2, voters_w_wahlschein,
    turnout,
    all_of(party_cols_all),
    flag_turnout_above_1
  ) |>
  arrange(election_year, ags)

glimpse(df_all)

write_rds(df_all, "data/european_elections/final/european_muni_unharm.rds")
fwrite(df_all, "data/european_elections/final/european_muni_unharm.csv")

cat("\nWritten:", nrow(df_all), "rows x", ncol(df_all), "columns\n")


# --- 6. Sanity checks ---------------------------------------------------------

# Rows per year
cat("\nRows per year:\n")
df_all |> count(election_year) |> print()

# State distribution per year
cat("\nState distribution:\n")
df_all |>
  group_by(election_year, state, state_name) |>
  summarise(n = n(), mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop") |>
  arrange(election_year, state) |>
  print(n = 70)

# National totals per year
cat("\nNational totals per year:\n")
df_all |>
  group_by(election_year) |>
  summarise(
    n_muni = n(),
    eligible = sum(eligible_voters, na.rm = TRUE),
    voters = sum(number_voters, na.rm = TRUE),
    valid = sum(valid_votes, na.rm = TRUE),
    turnout = sum(number_voters, na.rm = TRUE) / sum(eligible_voters, na.rm = TRUE)
  ) |>
  print()

# Major party shares per year
cat("\nNational party shares per year:\n")
major_parties <- c("cdu", "csu", "spd", "gruene", "afd", "die_linke", "fdp", "bsw")
for (yr in c(2009, 2014, 2019, 2024)) {
  cat(sprintf("\n  %d:\n", yr))
  sub <- df_all |> filter(election_year == yr)
  total_v <- sum(sub$number_voters, na.rm = TRUE)
  for (p in major_parties) {
    if (p %in% names(sub)) {
      s <- sum(sub[[p]] * sub$number_voters, na.rm = TRUE) / total_v
      if (s > 0.001) cat(sprintf("    %s: %.4f\n", p, s))
    }
  }
}

# Check duplicates
cat("\nDuplicate (ags, year) pairs:\n")
dupl <- df_all |> count(ags, election_year) |> filter(n > 1)
cat("  Count:", nrow(dupl), "\n")

# Turnout flags
cat("\nTurnout > 1 flags per year:\n")
df_all |>
  group_by(election_year) |>
  summarise(n_flag = sum(flag_turnout_above_1, na.rm = TRUE)) |>
  print()


### END
