# =============================================================================
# parse_TH.R
# Stage-1 CLEANING parser: Thüringen (TH) Landtagswahl at Wahlkreis level
#
# Reads the official "Wahlkreisübersicht" xlsx files (one per election year)
# from data/state_elections/raw/Landtagswahlen_Wahlkreis/Thüringen/ and emits a
# long, tidy CSV: one row per (Wahlkreis x stimme x party).
#
# Vote system: erststimme (Wahlkreisstimmen) + zweitstimme (Landesstimmen).
# In 1990 the same two votes are labelled Erststimmen / Zweitstimmen.
#
# Layout families (all detected dynamically, never hardcoded by year):
#   * 1990: sheet "L90 Wahlkreise"; meta cols Satzart|Wkr-nr|Kreis-nr|Gem-nr|
#           Name|Wahlbezirke|Wahlberechtigte|Wähler|Wahlbeteiligung|
#           Erst ungültig|Erst gültig|Zweit ungültig|Zweit gültig; then party
#           blocks of 4 cols [Erst abs, Erst %, Zweit abs, Zweit %]; party name
#           in header row 6 at the absolut column.
#   * 1994-2024: sheet 1 ("Landtagswahl ..."); header row 4 carries the block
#           labels "Wahlkreisstimmen"/"Landesstimmen" for the valid/invalid
#           block; party blocks of 4 cols [WK abs, WK %, LS abs, LS %]; party
#           name in header row 6, "absolut"/"%" in row 7. Two meta-column
#           variants (with/without a Wahlberechtigte/Wähler breakdown) are both
#           handled by marker-based column detection.
#
# Validation: (a) per (wkr,stimme) sum(party votes) == valid_votes; (b) summed
# per-Wahlkreis party votes == the source statewide "Land"/"000" total row.
# =============================================================================

library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_TH.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Thüringen")
out_path <- here("data", "state_elections", "processed", "wahlkreis",
                 "TH_ltw_wkr_long.csv")

# Official Thüringen Landtagswahl polling dates
ELECTION_DATES <- c(
  "1990" = "1990-10-14",
  "1994" = "1994-10-16",
  "1999" = "1999-09-12",
  "2004" = "2004-06-13",
  "2009" = "2009-08-30",
  "2014" = "2014-09-14",
  "2019" = "2019-10-27",
  "2024" = "2024-09-01"
)

# --- helpers -----------------------------------------------------------------

# clean a header cell to a comparable token (collapse whitespace, drop soft
# hyphens / line breaks)
htok <- function(x) {
  x <- as.character(x)
  x <- gsub("[\r\n­]", "", x)
  x <- gsub("\\s+", " ", x)
  str_trim(x)
}

# numeric parse that tolerates German thousands separators / spaces / NA marks
as_num <- function(x) {
  x <- as.character(x)
  x <- gsub(" ", "", x)          # nbsp
  x <- gsub("[. ]", "", x)            # thousands sep / spaces
  x <- gsub(",", ".", x)              # decimal comma
  x[x %in% c("", "-", ".", "x", "X")] <- NA
  suppressWarnings(as.numeric(x))
}

# integer-pad wkr number to 3 chars (preserve as character)
pad_wkr <- function(x) {
  x <- str_trim(as.character(x))
  x <- gsub("\\.0$", "", x)           # 1.0 -> 1 (numeric coercion artefacts)
  formatC(as.integer(x), width = 3, flag = "0")
}

# =============================================================================
# Parser for the 1994-2024 family
# =============================================================================
parse_modern <- function(f, year) {
  d <- read_excel(f, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  nc <- ncol(d)

  r4 <- vapply(seq_len(nc), function(j) htok(d[4, j]), character(1))
  r5 <- vapply(seq_len(nc), function(j) htok(d[5, j]), character(1))
  r6 <- vapply(seq_len(nc), function(j) htok(d[6, j]), character(1))
  r7 <- vapply(seq_len(nc), function(j) htok(d[7, j]), character(1))

  # --- meta columns (marker-based) -------------------------------------------
  # Wahlkreis number = the column whose header (r4/r5/r6) reads "Wahl-/kreis-/nr."
  # Reliable: it is col 3 in every modern file (Stand|Satzart|Wkr-nr|Name|...).
  col_wkr  <- 3L
  col_name <- 4L

  # eligible voters: r4 "Wahl-" & r5 "berechtigte"
  col_elig <- which(grepl("^Wahl", r4) & grepl("^berechtigte", r5))
  col_elig <- col_elig[col_elig >= 5][1]

  # number of voters: a "Wähler" column whose r5 is NOT "mit" (drop the
  # "Wähler mit Wahlschein" breakdown column present in 2014+)
  cand_voters <- which(r4 == "Wähler" & !grepl("^mit", r5))
  col_voters  <- cand_voters[cand_voters >= 5][1]

  # valid/invalid: r4 block labels (these appear ONLY in the meta block; the
  # party area uses r4 == "Von den gültigen Stimmen entfielen auf")
  col_wk_block <- which(r4 == "Wahlkreisstimmen")[1]
  col_ls_block <- which(r4 == "Landesstimmen")[1]
  stopifnot(!is.na(col_wk_block), !is.na(col_ls_block))
  col_wk_inval <- col_wk_block      # r5 == "ungültige"
  col_wk_val   <- col_wk_block + 1L # r5 == "gültige"
  col_ls_inval <- col_ls_block
  col_ls_val   <- col_ls_block + 1L
  stopifnot(r5[col_wk_inval] == "ungültige", r5[col_wk_val] == "gültige",
            r5[col_ls_inval] == "ungültige", r5[col_ls_val] == "gültige")

  # --- party blocks ----------------------------------------------------------
  # In the party area, each value column pair is [absolut, %]; r7 == "absolut"
  # marks the value column, r5 says whether it is a Wahlkreisstimme or a
  # Landesstimme, r6 carries the party name. The two stimme-blocks can have
  # DIFFERENT party counts (an extra Einzelbewerber direct candidate runs for
  # the Wahlkreisstimme only, with no Landesliste), so read them independently.
  abs_cols <- which(tolower(r7) == "absolut")
  wk_abs <- abs_cols[r5[abs_cols] == "Wahlkreisstimmen"]
  ls_abs <- abs_cols[r5[abs_cols] == "Landesstimmen"]
  stopifnot(length(wk_abs) > 0, length(ls_abs) > 0)
  stopifnot(all(nzchar(r6[wk_abs])), all(nzchar(r6[ls_abs])))

  parties_wk <- data.frame(party_raw = r6[wk_abs], col = wk_abs,
                           stringsAsFactors = FALSE)
  parties_ls <- data.frame(party_raw = r6[ls_abs], col = ls_abs,
                           stringsAsFactors = FALSE)

  # --- data rows: Satzart "K" in col 2 (col 1 = Stand "E") -------------------
  satzart <- vapply(seq_len(nrow(d)), function(i) htok(d[i, 2]), character(1))
  rows_k  <- which(satzart == "K")
  row_l   <- which(satzart == "L")
  stopifnot(length(row_l) == 1)

  build_rows <- function(ridx) {
    map_dfr(ridx, function(i) {
      wkr_nr  <- pad_wkr(d[i, col_wkr])
      wname   <- str_trim(as.character(d[i, col_name]))
      elig    <- as_num(d[i, col_elig])
      nvot    <- as_num(d[i, col_voters])
      wk_val  <- as_num(d[i, col_wk_val])
      wk_inv  <- as_num(d[i, col_wk_inval])
      ls_val  <- as_num(d[i, col_ls_val])
      ls_inv  <- as_num(d[i, col_ls_inval])
      erst <- data.frame(
        wkr_nr = wkr_nr, wkr_name = wname, stimme = "erststimme",
        eligible_voters = elig, number_voters = nvot,
        valid_votes = wk_val, invalid_votes = wk_inv,
        party_raw = parties_wk$party_raw,
        votes = as_num(unlist(d[i, parties_wk$col])),
        stringsAsFactors = FALSE)
      zweit <- data.frame(
        wkr_nr = wkr_nr, wkr_name = wname, stimme = "zweitstimme",
        eligible_voters = elig, number_voters = nvot,
        valid_votes = ls_val, invalid_votes = ls_inv,
        party_raw = parties_ls$party_raw,
        votes = as_num(unlist(d[i, parties_ls$col])),
        stringsAsFactors = FALSE)
      rbind(erst, zweit)
    })
  }

  list(data = build_rows(rows_k), land = build_rows(row_l))
}

# =============================================================================
# Parser for 1990 (sheet "L90 Wahlkreise")
# =============================================================================
parse_1990 <- function(f, year) {
  d <- read_excel(f, sheet = "L90 Wahlkreise", col_names = FALSE,
                  .name_repair = "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  nc <- ncol(d)

  r5 <- vapply(seq_len(nc), function(j) htok(d[5, j]), character(1))
  r6 <- vapply(seq_len(nc), function(j) htok(d[6, j]), character(1))

  # meta columns per Satzbeschreibung:
  # 1 Satzart | 2 Wkr-nr | 3 Kreis-nr | 4 Gem-nr | 5 Name | 6 Wahlbezirke |
  # 7 Wahlberechtigte | 8 Wähler | 9 Wahlbeteiligung |
  # 10 Erst ungültig | 11 Erst gültig | 12 Zweit ungültig | 13 Zweit gültig
  col_wkr  <- 2L
  col_name <- 5L
  col_elig <- 7L
  col_voters <- 8L
  col_e_inval <- 10L
  col_e_val   <- 11L
  col_z_inval <- 12L
  col_z_val   <- 13L
  # sanity-check against the header text
  stopifnot(grepl("berechtigt", htok(d[5, col_elig])),
            grepl("ähler", htok(d[5, col_voters])),
            r6[col_e_inval] == "ungültig", r6[col_e_val] == "gültig",
            r6[col_z_inval] == "ungültig", r6[col_z_val] == "gültig")

  # Party blocks are 2 cols each [absolut, %]. The Erststimmen block and the
  # Zweitstimmen block have DIFFERENT party counts in 1990: Einzelbewerber run
  # only for the Erststimme (direct candidate), not on a Landesliste. So the two
  # blocks are read independently from their own party-name positions in r6.
  e_start <- which(grepl("Erststimmen entfielen",  r5))[1]   # = 14
  z_start <- which(grepl("Zweitstimmen entfielen", r5))[1]   # = 42
  stopifnot(!is.na(e_start), !is.na(z_start))

  # within each block, party-name (absolut) columns are at even offsets and the
  # % columns (odd offsets) carry NA party names.
  e_abs <- seq(e_start, z_start - 1L, by = 2L)
  z_abs <- seq(z_start, nc,           by = 2L)
  # keep only columns that actually carry a party name in r6
  e_abs <- e_abs[!is.na(r6[e_abs]) & nzchar(r6[e_abs])]
  z_abs <- z_abs[!is.na(r6[z_abs]) & nzchar(r6[z_abs])]

  parties_e <- data.frame(party_raw = r6[e_abs], col = e_abs,
                          stringsAsFactors = FALSE)
  parties_z <- data.frame(party_raw = r6[z_abs], col = z_abs,
                          stringsAsFactors = FALSE)
  stopifnot(nrow(parties_e) > 0, nrow(parties_z) > 0)

  satzart <- vapply(seq_len(nrow(d)), function(i) htok(d[i, 1]), character(1))
  rows_k  <- which(satzart == "K")
  row_l   <- which(satzart == "L")
  stopifnot(length(row_l) == 1, length(rows_k) > 0)

  build_rows <- function(ridx) {
    map_dfr(ridx, function(i) {
      wkr_nr <- pad_wkr(d[i, col_wkr])
      wname  <- str_trim(as.character(d[i, col_name]))
      elig   <- as_num(d[i, col_elig])
      nvot   <- as_num(d[i, col_voters])
      erst <- data.frame(
        wkr_nr = wkr_nr, wkr_name = wname, stimme = "erststimme",
        eligible_voters = elig, number_voters = nvot,
        valid_votes = as_num(d[i, col_e_val]),
        invalid_votes = as_num(d[i, col_e_inval]),
        party_raw = parties_e$party_raw,
        votes = as_num(unlist(d[i, parties_e$col])),
        stringsAsFactors = FALSE)
      zweit <- data.frame(
        wkr_nr = wkr_nr, wkr_name = wname, stimme = "zweitstimme",
        eligible_voters = elig, number_voters = nvot,
        valid_votes = as_num(d[i, col_z_val]),
        invalid_votes = as_num(d[i, col_z_inval]),
        party_raw = parties_z$party_raw,
        votes = as_num(unlist(d[i, parties_z$col])),
        stringsAsFactors = FALSE)
      rbind(erst, zweit)
    })
  }

  list(data = build_rows(rows_k), land = build_rows(row_l))
}

# =============================================================================
# Drive all years, validate, write
# =============================================================================
files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[grepl("Landtagswahl_Wahlkreis", basename(files))]

all_out  <- list()
val_log  <- list()

for (f in sort(files)) {
  year <- str_extract(basename(f), "(?<=TH_)[0-9]{4}")
  message("Parsing ", year, " ...")
  res <- if (year == "1990") parse_1990(f, year) else parse_modern(f, year)

  dat  <- res$data
  land <- res$land

  # --- validation (a): per (wkr,stimme) party-sum vs valid_votes ------------
  chk_a <- dat %>%
    group_by(wkr_nr, stimme) %>%
    summarise(party_sum = sum(votes, na.rm = TRUE),
              valid = first(valid_votes), .groups = "drop") %>%
    mutate(disc = abs(party_sum - valid))
  max_disc <- max(chk_a$disc, na.rm = TRUE)
  n_groups <- nrow(chk_a)

  # --- validation (b): statewide totals vs the Land row --------------------
  # aggregate both sides by (party_raw, stimme); multiple Einzelbewerber columns
  # in the same stimme are summed on BOTH sides so the comparison stays valid.
  mine <- dat %>%
    group_by(party_raw, stimme) %>%
    summarise(v = sum(votes, na.rm = TRUE), .groups = "drop")
  theirs <- land %>%
    group_by(party_raw, stimme) %>%
    summarise(land_v = sum(votes, na.rm = TRUE), .groups = "drop")
  cmp <- full_join(mine, theirs, by = c("party_raw", "stimme")) %>%
    mutate(d = abs(v - land_v))
  state_match <- all(cmp$d <= 1, na.rm = TRUE) && all(!is.na(cmp$land_v))
  state_max_d <- max(cmp$d, na.rm = TRUE)
  fails <- cmp %>% filter(d > 1 | is.na(land_v))

  n_wkr <- length(unique(dat$wkr_nr))

  val_log[[year]] <- list(
    year = year, n_wkr = n_wkr, max_disc = max_disc, n_groups = n_groups,
    state_match = state_match, state_max_d = state_max_d, fails = fails
  )

  message(sprintf("  %s: n_wkr=%d  max|party_sum-valid|=%.2f (%d groups)  state_match=%s (maxd=%.2f)",
                  year, n_wkr, max_disc, n_groups, state_match, state_max_d))
  if (nrow(fails) > 0) {
    message("  STATEWIDE FAILS:")
    print(fails)
  }

  dat$state_abbr    <- "TH"
  dat$state         <- "Thüringen"
  dat$election_year <- as.integer(year)
  dat$election_date <- ELECTION_DATES[[year]]
  all_out[[year]]   <- dat
}

# --- combine, order columns, write ------------------------------------------
final <- bind_rows(all_out) %>%
  select(state_abbr, state, election_year, election_date,
         wkr_nr, wkr_name, stimme,
         eligible_voters, number_voters, valid_votes, invalid_votes,
         party_raw, votes)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
fwrite(final, out_path)

message("\n==== SUMMARY ====")
message("Rows emitted: ", nrow(final))
message("Years: ", paste(sort(unique(final$election_year)), collapse = ", "))
message("Output: ", out_path)
for (y in names(val_log)) {
  v <- val_log[[y]]
  message(sprintf("  %s: n_wkr=%d  max_disc=%.2f  state_match=%s",
                  y, v$n_wkr, v$max_disc, v$state_match))
}
