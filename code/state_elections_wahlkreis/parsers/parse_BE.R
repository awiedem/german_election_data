# =============================================================================
# parse_BE.R  --  Stage-1 cleaning parser for Berlin (BE)
# Wahl zum Abgeordnetenhaus von Berlin (Landtagswahl equivalent),
# CONSTITUENCY (Wahlkreis) level.
#
# Vote system: erststimme (Direktstimme / Wahlkreiskandidat)
#            + zweitstimme (Bezirksliste / Landesstimme).
#
# Machine-readable constituency-level sources (others in the folder are PDF
# scans, deferred to OCR, or non-Wahlkreis time series):
#   - BE_2016_Abgeordnetenhauswahl_Wahlbezirk.xlsx  (sheets Erststimme/Zweitstimme)
#   - BE_2023_Abgeordnetenhauswahl_Wahlbezirk.xlsx  (sheets AGH_W1/AGH_W2)
# Both report at Wahlbezirk (precinct) level; they carry an
# "Abgeordnetenhauswahlkreis" id (per Bezirk, 1..n) plus "Bezirksnummer".
# The unique Wahlkreis key is (Bezirksnummer x Abgeordnetenhauswahlkreis) -> 78.
# Wahlbezirke nest cleanly inside Wahlkreise -> aggregate by sum.
#
# Validation source (statewide totals, both years):
#   BE_2023_Abgeordnetenhauswahl_Ergebnisbericht.xlsx, sheet "1" carries the
#   statewide Erst/Zweit Anzahl for 2023 (cols 2/4) AND 2016 (cols 6/8); its
#   per-Wahlkreis sheets "3.1".."3.78" were spot-checked too.
# =============================================================================

library(here)
library(readxl)
library(data.table)
library(tidyverse)

here::i_am("code/state_elections_wahlkreis/parsers/parse_BE.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Berlin")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

OUT_COLS <- c("state_abbr", "state", "election_year", "election_date",
              "wkr_nr", "wkr_name", "stimme",
              "eligible_voters", "number_voters", "valid_votes",
              "invalid_votes", "party_raw", "votes")

# -----------------------------------------------------------------------------
# Generic parser for a single Wahlbezirk sheet (one stimme).
# col_map: named integer positions of the META columns; party columns are the
# contiguous block from `party_start` to the last column. Party labels are read
# verbatim from the header row.
# -----------------------------------------------------------------------------
parse_wb_sheet <- function(file, sheet, stimme, year, date,
                            col_bez_nr, col_bez_name, col_agh_wk,
                            col_eligible, col_voters, col_valid, col_invalid,
                            party_start) {

  # header row (verbatim party labels)
  hdr_raw <- read_excel(file, sheet = sheet, col_names = FALSE, n_max = 1)
  hdr <- as.character(unlist(hdr_raw[1, ]))
  hdr <- gsub("[\r\n]+", " ", hdr)
  hdr <- trimws(gsub("\\s+", " ", hdr))
  ncol_tot <- length(hdr)

  # data (skip header)
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, skip = 1)
  raw <- as.data.frame(raw)
  n <- nrow(raw)

  # build clean meta frame column-by-column (avoid tibble name mangling)
  df <- data.frame(row.names = seq_len(n), check.names = FALSE)
  df[["bez_nr"]]   <- as.character(raw[[col_bez_nr]])
  df[["bez_name"]] <- as.character(raw[[col_bez_name]])
  df[["agh_wk"]]   <- as.character(raw[[col_agh_wk]])
  df[["eligible"]] <- suppressWarnings(as.numeric(raw[[col_eligible]]))
  df[["voters"]]   <- suppressWarnings(as.numeric(raw[[col_voters]]))
  df[["valid"]]    <- suppressWarnings(as.numeric(raw[[col_valid]]))
  df[["invalid"]]  <- suppressWarnings(as.numeric(raw[[col_invalid]]))

  # drop rows without a Wahlkreis id (defensive)
  keep <- !is.na(df$bez_nr) & !is.na(df$agh_wk) &
          df$bez_nr != "" & df$agh_wk != "" & df$agh_wk != "NA"
  df  <- df[keep, , drop = FALSE]
  raw <- raw[keep, , drop = FALSE]

  # normalise wahlkreis id: zero-pad agh_wk to 2 digits, key = bez-WK
  agh_int <- suppressWarnings(as.integer(df$agh_wk))
  df$wkr_nr   <- paste0(sprintf("%02s", df$bez_nr), "-",
                        sprintf("%02d", agh_int))
  df$wkr_name <- paste0(df$bez_name, " ", agh_int)

  party_cols <- party_start:ncol_tot
  party_lab  <- hdr[party_cols]

  # long-melt party columns
  long_list <- vector("list", length(party_cols))
  for (k in seq_along(party_cols)) {
    pc <- party_cols[k]
    v  <- suppressWarnings(as.numeric(raw[[pc]]))
    long_list[[k]] <- data.table(
      wkr_nr   = df$wkr_nr,
      wkr_name = df$wkr_name,
      eligible = df$eligible,
      voters   = df$voters,
      valid    = df$valid,
      invalid  = df$invalid,
      party_raw = party_lab[k],
      votes     = v
    )
  }
  L <- rbindlist(long_list)

  # aggregate Wahlbezirke -> Wahlkreis
  agg <- L[, .(
    eligible_voters = sum(eligible, na.rm = TRUE),
    number_voters   = sum(voters,   na.rm = TRUE),
    valid_votes     = sum(valid,    na.rm = TRUE),
    invalid_votes   = sum(invalid,  na.rm = TRUE),
    votes           = sum(votes,    na.rm = TRUE)
  ), by = .(wkr_nr, wkr_name, party_raw)]

  # eligible/voters/valid/invalid are per (wkr[,stimme]); recompute cleanly:
  # the per-party rows replicate the same wkr totals, so the sum above
  # over-counts eligible/voters/valid/invalid by n_parties. Fix by taking the
  # per-wkr value computed once from df.
  wkr_meta <- as.data.table(df)[, .(
    eligible_voters = sum(eligible, na.rm = TRUE),
    number_voters   = sum(voters,   na.rm = TRUE),
    valid_votes     = sum(valid,    na.rm = TRUE),
    invalid_votes   = sum(invalid,  na.rm = TRUE)
  ), by = .(wkr_nr)]

  agg[, c("eligible_voters","number_voters","valid_votes","invalid_votes") := NULL]
  agg <- merge(agg, wkr_meta, by = "wkr_nr", all.x = TRUE)

  # drop parties with zero votes in a Wahlkreis where the party is not on the
  # ballot (EB candidates are WK-specific). A named statewide party with 0 in a
  # WK is kept (it was on the ballot). We keep all named parties everywhere but
  # drop the all-zero EB/empty-label entries that are structurally absent.
  agg <- agg[!(votes == 0 & grepl("^EB", party_raw))]
  # drop empty/NA party labels (none expected, defensive)
  agg <- agg[!is.na(party_raw) & party_raw != ""]

  out <- data.table(
    state_abbr    = "BE",
    state         = "Berlin",
    election_year = year,
    election_date = date,
    wkr_nr        = agg$wkr_nr,
    wkr_name      = agg$wkr_name,
    stimme        = stimme,
    eligible_voters = agg$eligible_voters,
    number_voters   = agg$number_voters,
    valid_votes     = agg$valid_votes,
    invalid_votes   = agg$invalid_votes,
    party_raw       = agg$party_raw,
    votes           = as.integer(round(agg$votes))
  )
  setorder(out, wkr_nr, party_raw)
  out[]
}

# -----------------------------------------------------------------------------
# 2016  (18 Sep 2016)
#   Erststimme : meta bez=3 name=4 agh_wk=7 elig=10 voters=14 invalid=16 valid=17
#                parties 18..61
#   Zweitstimme: same meta, parties 18..43
# -----------------------------------------------------------------------------
f2016 <- file.path(raw_dir, "BE_2016_Abgeordnetenhauswahl_Wahlbezirk.xlsx")
be16_e <- parse_wb_sheet(f2016, "Erststimme", "erststimme", 2016L, "2016-09-18",
                         col_bez_nr = 3, col_bez_name = 4, col_agh_wk = 7,
                         col_eligible = 10, col_voters = 14,
                         col_valid = 17, col_invalid = 16,
                         party_start = 18)
be16_z <- parse_wb_sheet(f2016, "Zweitstimme", "zweitstimme", 2016L, "2016-09-18",
                         col_bez_nr = 3, col_bez_name = 4, col_agh_wk = 7,
                         col_eligible = 10, col_voters = 14,
                         col_valid = 17, col_invalid = 16,
                         party_start = 18)

# -----------------------------------------------------------------------------
# 2023  (12 Feb 2023, Wiederholungswahl)
#   AGH_W1: meta bez=3 name=4 agh_wk=8 elig=11 voters=15 valid=17 invalid=18
#           parties 19..66
#   AGH_W2: same meta, parties 19..54
# -----------------------------------------------------------------------------
f2023 <- file.path(raw_dir, "BE_2023_Abgeordnetenhauswahl_Wahlbezirk.xlsx")
be23_e <- parse_wb_sheet(f2023, "AGH_W1", "erststimme", 2023L, "2023-02-12",
                         col_bez_nr = 3, col_bez_name = 4, col_agh_wk = 8,
                         col_eligible = 11, col_voters = 15,
                         col_valid = 17, col_invalid = 18,
                         party_start = 19)
be23_z <- parse_wb_sheet(f2023, "AGH_W2", "zweitstimme", 2023L, "2023-02-12",
                         col_bez_nr = 3, col_bez_name = 4, col_agh_wk = 8,
                         col_eligible = 11, col_voters = 15,
                         col_valid = 17, col_invalid = 18,
                         party_start = 19)

combined <- rbindlist(list(be16_e, be16_z, be23_e, be23_z), use.names = TRUE)
setcolorder(combined, OUT_COLS)
setorder(combined, election_year, stimme, wkr_nr, party_raw)

out_csv <- file.path(out_dir, "BE_ltw_wkr_long.csv")
fwrite(combined, out_csv)
cat("Wrote", nrow(combined), "rows to", out_csv, "\n")
