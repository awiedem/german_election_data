#### State Elections: Clean raw data (1990-2024) ####
## Builds state_unharm_raw from raw files, replacing the DESTATIS API source.
##
## This script reads municipality-level raw election data for all 16 German
## states (where machine-readable data is available) and produces a unified
## unharmonized dataset. It replaces 01_state_unharm.R (API-based, 2006-2019)
## and 03_state_2022-24.R (manual, 2022-2024) with a single source of truth.
##
## Output: data/state_elections/final/state_unharm_raw.rds / .csv
##
## Authors: Vincent Heddesheimer (with Claude Code assistance)
## Date: February 2026

#### Setup ####
rm(list = ls())

packages <- c("readxl", "xml2", "here", "janitor", "tidyverse", "data.table")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

setwd(here())

raw_path <- "data/state_elections/raw/Landtagswahlen"

## Standard output columns -------------------------------------------------
## Every state section must produce a data frame with exactly these columns.
output_cols <- c(
  "ags", "election_year", "state", "election_date",
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
  "turnout",
  "cdu", "csu", "spd", "gruene", "fdp", "linke_pds", "afd", "bsw",
  "other", "cdu_csu"
)

## Party columns that represent vote shares
party_cols <- c("cdu", "csu", "spd", "gruene", "fdp", "linke_pds",
                "afd", "bsw", "other", "cdu_csu")

## Helper: standardise a cleaned state data frame to output_cols -----------
standardise <- function(df) {
  # Ensure all output columns exist (fill missing with NA)
  for (col in output_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  # Coerce types

  df <- df |>
    mutate(
      ags = as.character(ags),
      election_year = as.integer(election_year),
      state = as.character(state),
      election_date = as.Date(election_date),
      across(c(eligible_voters, number_voters, valid_votes, invalid_votes),
             ~ as.numeric(.x)),
      across(all_of(party_cols), ~ as.numeric(.x))
    ) |>
    select(all_of(output_cols))
  return(df)
}

## Collector list: each state section appends its result here
all_states <- list()

###############################################################################
####                         THÜRINGEN (16)                                ####
###############################################################################
## 8 elections: 1990, 1994, 1999, 2004, 2009, 2014, 2019, 2024
## Format: XLSX, one file per year, Satzart "G" = Gemeinde rows
## AGS = "16" + kreisnr(3) + gemeindenr(3)
## Kreisfreie Städte may appear in multiple Wahlkreise → aggregate
## 1990 uses DDR-era Kreis codes (3-digit) → skip for now (needs crosswalk)
##
## Column layout groups (anchored on Landesstimmen header position):
## 1994-2009: eligible=col10, wahler=col11, LS_header=col15
## 2014-2019: eligible=col10, wahler=col14, LS_header=col19
## 2024:      eligible=col13, wahler=col17, LS_header=col22

th_dates <- c(
  "1990" = "1990-10-14", "1994" = "1994-10-16", "1999" = "1999-09-12",
  "2004" = "2004-06-13", "2009" = "2009-08-30", "2014" = "2014-09-14",
  "2019" = "2019-10-27", "2024" = "2024-09-01"
)

th_party_map <- list(
  "1994" = c("CDU" = "cdu", "SPD" = "spd", "PDS" = "linke_pds",
             "FDP" = "fdp", "GRÜNE" = "gruene"),
  "1999" = c("CDU" = "cdu", "SPD" = "spd", "PDS" = "linke_pds",
             "GRÜNE" = "gruene", "F.D.P." = "fdp"),
  "2004" = c("CDU" = "cdu", "SPD" = "spd", "PDS" = "linke_pds",
             "GRÜNE" = "gruene", "FDP" = "fdp"),
  "2009" = c("CDU" = "cdu", "SPD" = "spd", "DIE LINKE" = "linke_pds",
             "GRÜNE" = "gruene", "FDP" = "fdp"),
  "2014" = c("CDU" = "cdu", "SPD" = "spd", "DIE LINKE" = "linke_pds",
             "GRÜNE" = "gruene", "FDP" = "fdp", "AfD" = "afd"),
  "2019" = c("CDU" = "cdu", "SPD" = "spd", "DIE LINKE" = "linke_pds",
             "GRÜNE" = "gruene", "FDP" = "fdp", "AfD" = "afd"),
  "2024" = c("CDU" = "cdu", "SPD" = "spd", "DIE LINKE" = "linke_pds",
             "GRÜNE" = "gruene", "FDP" = "fdp", "AfD" = "afd",
             "BSW" = "bsw")
)

th_results <- list()

for (yr in names(th_dates)) {
  cat("Processing Thüringen", yr, "...\n")

  if (yr == "1990") {
    cat("  Skipping 1990 (DDR-era codes, needs crosswalk).\n")
    next
  }

  fpath <- here(raw_path, "Thüringen",
                paste0("Thüringen_", yr, "_Landtag.xlsx"))
  raw <- read_excel(fpath, sheet = 1, col_names = FALSE, col_types = "text")
  cnames <- names(raw)

  ## Helper: extract numeric column by index from a filtered data frame
  get_num <- function(df, col_idx) {
    as.numeric(na_if(df[[cnames[col_idx]]], "-"))
  }

  ## --- Locate Landesstimmen header (anchor point) ---
  ## Row 4 always contains the main section headers.
  ## Find the column where "Landesstimmen" appears.
  r4 <- as.character(raw[4, ])
  ls_header_col <- which(grepl("Landesstimmen", r4))[1]
  # Row 5 has "ungültige" / "gültige" directly under Landesstimmen
  r5 <- as.character(raw[5, ])
  ls_ungueltig_col <- ls_header_col      # "ungültige" under Landesstimmen
  ls_gueltig_col   <- ls_header_col + 1  # "gültige" next to it

  ## --- Locate Wahlberechtigte and Wähler ---
  ## These are always a few columns before the Landesstimmen block.
  ## Search row 5 for "berechtigte" (part of "Wahl-berechtigte insgesamt")
  wb_col <- which(grepl("berechtigte", r5))[1]
  # If not found in row 5, check row 4
  if (is.na(wb_col)) wb_col <- which(grepl("berechtigte", r4))[1]

  ## Wähler: search row 4 for "Wähler"
  wahler_col <- which(r4 == "Wähler")[1]
  # 2014+ have "Wähler" in row 4 at a different position; 1994-2009 have it at col 11
  if (is.na(wahler_col)) {
    # Fall back: Wähler is 1 column after Wahlberechtigte in 1994-2009
    wahler_col <- wb_col + 1
  }

  ## --- Locate party name row (row 6 = party_row_idx) ---
  r6 <- as.character(raw[6, ])

  ## --- Filter to Gemeinde-level rows ---
  df <- raw |> filter(.data[[cnames[2]]] == "G")

  ## --- Construct AGS: "16" + kreisnr(3-digit) + gemeindenr(3-digit) ---
  kreisnr  <- as.numeric(df[[cnames[4]]])
  gemeindenr <- as.numeric(df[[cnames[5]]])
  ags_vec <- paste0("16", sprintf("%03d", kreisnr), sprintf("%03d", gemeindenr))

  ## --- Extract admin columns ---
  eligible <- get_num(df, wb_col)
  voters   <- get_num(df, wahler_col)
  valid    <- get_num(df, ls_gueltig_col)
  invalid  <- get_num(df, ls_ungueltig_col)

  ## --- Extract party vote counts (Landesstimmen absolute) ---
  ## Party names in row 6 appear in pairs (WKS, LS) or quads (WKS abs, WKS%, LS abs, LS%).
  ## Second occurrence of each name = Landesstimmen column.
  ## The column right after is the percentage → we want the first (absolute).
  party_map <- th_party_map[[yr]]
  party_votes <- list()
  for (pname in names(party_map)) {
    positions <- which(r6 == pname)
    if (length(positions) >= 2) {
      # Second occurrence = Landesstimmen absolute
      party_votes[[party_map[pname]]] <- get_num(df, positions[2])
    } else if (length(positions) == 1 && positions[1] > ls_gueltig_col) {
      # Only one occurrence and it's after LS gültig → it's in the LS party block
      party_votes[[party_map[pname]]] <- get_num(df, positions[1])
    }
  }

  ## --- Compute "other" = valid_votes - sum(all LS party votes) ---
  ## Find ALL party LS absolute columns (not just mapped ones)
  admin_labels <- c("nr.", "Nr.", "Name", "insgesamt", "Wahlschein",
                    "nach § 23(2) LWO", "Briefwahl-", "Gemeinde/",
                    "bezirk", "Wahlbezirk")
  all_party_names <- unique(r6[!is.na(r6) & !r6 %in% admin_labels])

  all_ls_sum <- rep(0, nrow(df))
  for (pname in all_party_names) {
    positions <- which(r6 == pname)
    col_idx <- NA
    if (length(positions) >= 2) {
      col_idx <- positions[2]
    } else if (length(positions) == 1 && positions[1] > ls_gueltig_col) {
      col_idx <- positions[1]
    }
    if (!is.na(col_idx)) {
      v <- get_num(df, col_idx)
      v[is.na(v)] <- 0
      all_ls_sum <- all_ls_sum + v
    }
  }

  ## --- Build result ---
  result <- tibble(
    ags = ags_vec,
    election_year = as.integer(yr),
    state = "16",
    election_date = as.Date(th_dates[yr]),
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid,
    invalid_votes = invalid
  )

  # Add mapped party vote counts
  for (std_name in names(party_votes)) {
    result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
  }

  # Other = valid - sum(mapped party counts)
  mapped_sum <- rep(0, nrow(result))
  for (std_name in names(party_votes)) {
    v <- party_votes[[std_name]]
    v[is.na(v)] <- 0
    mapped_sum <- mapped_sum + v
  }
  result$other_n <- pmax(valid - mapped_sum, 0, na.rm = TRUE)

  ## --- Aggregate duplicate AGS (kreisfreie Städte split across Wahlkreise) ---
  count_cols <- c(paste0(names(party_votes), "_n"), "other_n")
  result <- result |>
    group_by(ags) |>
    summarise(
      election_year = first(election_year),
      state = first(state),
      election_date = first(election_date),
      eligible_voters = sum(eligible_voters, na.rm = TRUE),
      number_voters = sum(number_voters, na.rm = TRUE),
      valid_votes = sum(valid_votes, na.rm = TRUE),
      invalid_votes = sum(invalid_votes, na.rm = TRUE),
      across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  ## --- Convert counts to shares ---
  result <- result |>
    mutate(turnout = number_voters / eligible_voters, csu = NA_real_)
  for (std_name in c(names(party_votes), "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  ## --- Standardise and validate ---
  result <- standardise(result)
  n_muni <- nrow(result)
  total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
  cat("  TH", yr, ":", n_muni, "municipalities,",
      format(total_eligible, big.mark = ","), "eligible voters\n")

  th_results[[yr]] <- result
}

all_states[["th"]] <- bind_rows(th_results)
cat("Thüringen total:", nrow(all_states[["th"]]), "rows\n\n")


###############################################################################
####                     FINAL: Bind and write                             ####
###############################################################################

state_unharm_raw <- bind_rows(all_states)

# CDU/CSU consistency
state_unharm_raw <- state_unharm_raw |>
  mutate(
    cdu = ifelse(state != "09" & (cdu == 0 | is.na(cdu)), cdu_csu, cdu),
    csu = ifelse(state == "09" & (csu == 0 | is.na(csu)), cdu_csu, csu)
  )

cat("\n=== Final dataset ===\n")
cat("Total rows:", nrow(state_unharm_raw), "\n")
cat("State-year combinations:\n")
print(table(state_unharm_raw$state, state_unharm_raw$election_year))

# Write output
fwrite(state_unharm_raw, "data/state_elections/final/state_unharm_raw.csv")
write_rds(state_unharm_raw, "data/state_elections/final/state_unharm_raw.rds")
cat("Written to data/state_elections/final/state_unharm_raw.{csv,rds}\n")
