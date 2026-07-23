# =============================================================================
# parse_BB.R  —  Stage-1 cleaning parser for Brandenburg (BB) Landtagswahl
#                results at the CONSTITUENCY (Wahlkreis) level.
#
# Source: Amt fuer Statistik Berlin-Brandenburg "Downloadtabelle" XLSX exports,
#         one workbook per election year (DL_BB_LT<year>.xlsx). These are at the
#         WAHLBEZIRK (ballot-district) level; this script aggregates them up to
#         the 44 Landtagswahlkreise by the "Landtags-wahlkreis" column.
#
#   1990-2014: sheets "Erststimme(n)" + "Zweitstimme(n)" (one per Stimme),
#              party columns are absolute votes, single header row.
#   2019:      sheets "..._W_1" (Erststimme) + "..._W_2" (Zweitstimme),
#              party columns are absolute votes.
#   2024:      sheets "..._W_1" (Erststimme) + "..._W_2" (Zweitstimme),
#              party columns ALTERNATE absolute / "... in Prozent" — only the
#              absolute columns are kept.
#
# Output: long, tidy CSV — one row per (Wahlkreis x stimme x party).
#
# Validation (printed at end):
#   (a) per (wkr,stimme): |sum(party votes) - valid_votes| ~ 0.
#   (b) statewide: aggregated Gueltige Zweitstimmen + major-party Zweitstimmen
#       totals vs the Lange-Reihe state-total workbook (sheet "2"); and for
#       2019/2024 the full per-party GI9900/12000000 "Land" total row in the
#       Kreise_Gemeinden A_2 workbook.
#   (c) Wahlkreis count per year (expect 44).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)
library(readxl)

here::i_am("code/state_elections_wahlkreis/parsers/parse_BB.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Brandenburg")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

STATE      <- "Brandenburg"
STATE_ABBR <- "BB"

# Election dates (verified from each workbook's Impressum sheet).
ELECTION_DATES <- c(
  "1990" = "1990-10-14", "1994" = "1994-09-11", "1999" = "1999-09-05",
  "2004" = "2004-09-19", "2009" = "2009-09-27", "2014" = "2014-09-14",
  "2019" = "2019-09-01", "2024" = "2024-09-22"
)

# ---- helpers ---------------------------------------------------------------

# clean a header label: collapse internal whitespace / newlines.
clean_hdr <- function(x) {
  x <- gsub("\\s+", " ", as.character(x))
  trimws(x)
}

# parse a German-formatted integer column (may carry "." thousands sep, "-", "x").
to_int <- function(x) {
  x <- as.character(x)
  x <- gsub("\\.", "", x)        # thousands separator
  x <- gsub("\\s", "", x)
  x[x %in% c("", "-", "x", "X", "NA", ".")] <- NA
  suppressWarnings(as.numeric(x))
}

# Parse ONE Wahlbezirk-level sheet -> aggregated long Wahlkreis table for one stimme.
parse_wb_sheet <- function(file, sheet, stimme, year) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE)
  hdr <- clean_hdr(unlist(raw[1, ]))
  body <- raw[-1, , drop = FALSE]

  find1 <- function(pat, exact = NULL) {
    if (!is.null(exact)) {
      idx <- which(hdr == exact)
    } else {
      idx <- grep(pat, hdr)
    }
    if (length(idx) == 0) stop(sprintf("[%s/%s] no column matching %s",
                                       year, sheet, if (is.null(exact)) pat else exact))
    idx[1]
  }

  # meta columns (by exact name; layouts differ across years)
  c_wkr   <- which(grepl("Landtags", hdr) & grepl("wahlkreis", hdr))[1]
  c_elig  <- find1(exact = "Wahlberechtigte insgesamt")
  # voters: "Wähler" (1990-2019) or "Wählende" (2024); never the B1 / Prozent variants
  c_vot   <- which(hdr %in% c("Wähler", "Wählende"))[1]
  c_inval <- which(hdr == "Ungültige Stimmen")[1]
  c_val   <- which(hdr == "Gültige Stimmen")[1]
  if (is.na(c_wkr) || is.na(c_vot) || is.na(c_inval) || is.na(c_val))
    stop(sprintf("[%s/%s] missing a required meta column", year, sheet))

  # party columns = everything to the RIGHT of the turnout/valid/invalid block.
  # In 2024 the column order is Gültige, Gültige%, Ungültige, Ungültige%, then
  # the parties — so parties start after the rightmost of (valid, invalid). We
  # also explicitly drop any percentage columns and the meta labels themselves.
  meta_labels <- c("Gültige Stimmen", "Ungültige Stimmen",
                   "Wähler", "Wählende", "Wähler B1", "Wählende mit Wahlschein",
                   "Wahlberechtigte insgesamt")
  start_after <- max(c_val, c_inval, na.rm = TRUE)
  cand_idx <- which(seq_along(hdr) > start_after)
  cand_idx <- cand_idx[!grepl("[Pp]rozent", hdr[cand_idx])]
  cand_idx <- cand_idx[!hdr[cand_idx] %in% meta_labels]
  cand_idx <- cand_idx[!is.na(hdr[cand_idx]) & hdr[cand_idx] != "NA" & nzchar(hdr[cand_idx])]
  if (length(cand_idx) == 0)
    stop(sprintf("[%s/%s] no party columns found", year, sheet))
  party_names <- hdr[cand_idx]

  # build a tidy data.table of Wahlbezirk rows
  dt <- data.table(
    wkr_raw = as.character(unlist(body[, c_wkr])),
    eligible_voters = to_int(unlist(body[, c_elig])),
    number_voters   = to_int(unlist(body[, c_vot])),
    invalid_votes   = to_int(unlist(body[, c_inval])),
    valid_votes     = to_int(unlist(body[, c_val]))
  )
  for (k in seq_along(cand_idx)) {
    dt[[paste0("P_", k)]] <- to_int(unlist(body[, cand_idx[k]]))
  }

  # keep only real Wahlbezirk rows (numeric Wahlkreis number present)
  dt[, wkr_int := suppressWarnings(as.integer(wkr_raw))]
  dt <- dt[!is.na(wkr_int) & wkr_int >= 1 & wkr_int <= 44]

  # aggregate to Wahlkreis
  pcols <- paste0("P_", seq_along(cand_idx))
  agg <- dt[, c(
    lapply(.SD[, .(eligible_voters, number_voters, invalid_votes, valid_votes)], sum, na.rm = TRUE),
    lapply(.SD[, ..pcols], sum, na.rm = TRUE)
  ), by = wkr_int]
  setnames(agg, pcols, party_names)

  # to long over parties
  long <- melt(agg,
               id.vars = c("wkr_int", "eligible_voters", "number_voters",
                           "invalid_votes", "valid_votes"),
               measure.vars = party_names,
               variable.name = "party_raw", value.name = "votes")
  long[, `:=`(
    state_abbr = STATE_ABBR, state = STATE,
    election_year = as.integer(year),
    election_date = ELECTION_DATES[[as.character(year)]],
    wkr_nr = sprintf("%03d", wkr_int),
    wkr_name = sprintf("Landtagswahlkreis %02d", wkr_int),
    stimme = stimme,
    party_raw = as.character(party_raw)
  )]
  long[]
}

# ---- file/sheet plan -------------------------------------------------------

plan <- list(
  list(year = "1990", file = "BB_1990_Landtagswahl_Wahlkreis.xlsx",
       e = "Erststimmen", z = "Zweitstimmen"),
  list(year = "1994", file = "BB_1994_Landtagswahl_Wahlkreis.xlsx",
       e = "Erststimme",  z = "Zweitstimme"),
  list(year = "1999", file = "BB_1999_Landtagswahl_Wahlkreis.xlsx",
       e = "Erststimme",  z = "Zweitstimme"),
  list(year = "2004", file = "BB_2004_Landtagswahl_Wahlkreis.xlsx",
       e = "Erststimme",  z = "Zweitstimme"),
  list(year = "2009", file = "BB_2009_Landtagswahl_Wahlkreis.xlsx",
       e = "Erststimme",  z = "Zweitstimme"),
  list(year = "2014", file = "BB_2014_Landtagswahl_Wahlkreis.xlsx",
       e = "Erststimme",  z = "Zweitstimme"),
  list(year = "2019", file = "BB_2019_Landtagswahl_Wahlkreis.xlsx",
       e = "Brandenburg_Landtagswahl_W_1", z = "Brandenburg_Landtagswahl_W_2"),
  list(year = "2024", file = "BB_2024_Landtagswahl_Wahlkreis.xlsx",
       e = "Brandenburg_Landtagswahl_W_1", z = "Brandenburg_Landtagswahl_W_2")
)

all_long <- list()
for (p in plan) {
  fp <- file.path(raw_dir, p$file)
  e <- parse_wb_sheet(fp, p$e, "erststimme",  p$year)
  z <- parse_wb_sheet(fp, p$z, "zweitstimme", p$year)
  all_long[[p$year]] <- rbind(e, z)
  cat(sprintf("parsed %s: erst %d wkr, zweit %d wkr\n",
              p$year, length(unique(e$wkr_int)), length(unique(z$wkr_int))))
}

out <- rbindlist(all_long, use.names = TRUE)

# column order
col_order <- c("state_abbr", "state", "election_year", "election_date",
               "wkr_nr", "wkr_name", "stimme",
               "eligible_voters", "number_voters", "valid_votes",
               "invalid_votes", "party_raw", "votes")
out <- out[, ..col_order]
setorder(out, election_year, stimme, wkr_nr, party_raw)

out_csv <- file.path(out_dir, "BB_ltw_wkr_long.csv")
fwrite(out, out_csv)
cat(sprintf("\nWROTE %s  (%d rows)\n\n", out_csv, nrow(out)))

# =============================================================================
# VALIDATION
# =============================================================================

cat("================ VALIDATION ================\n")

## (a) per (wkr,stimme): sum(party votes) vs valid_votes
chk <- out[, .(sum_party = sum(votes, na.rm = TRUE),
               valid_votes = valid_votes[1]),
           by = .(election_year, wkr_nr, stimme)]
chk[, disc := abs(sum_party - valid_votes)]
cat(sprintf("(a) per-(wkr,stimme) groups checked: %d ; MAX |sum-valid| = %d\n",
            nrow(chk), max(chk$disc)))
if (max(chk$disc) > 0) {
  cat("    groups with nonzero discrepancy:\n")
  print(chk[disc > 0][order(-disc)])
}

## (c) Wahlkreis count per year
wc <- out[stimme == "zweitstimme", .(n_wkr = uniqueN(wkr_nr)), by = election_year]
cat("\n(c) Wahlkreis count per year:\n"); print(wc)

## (b) statewide totals

# --- (b1) vs Lange-Reihe sheet "2": Gueltige Zweitstimmen + major parties
lr <- read_excel(file.path(raw_dir, "BB_1990-2024_Landtagswahlen_Lange-Reihe.xlsx"),
                 sheet = "2", col_names = FALSE)
lr_years <- as.character(unlist(lr[3, 3:10]))
lr_years <- gsub("[^0-9]", "", lr_years)        # strip footnote marks
# build a lookup: label -> named vector by year
lr_row <- function(label) {
  i <- which(grepl(label, as.character(unlist(lr[, 2])), fixed = TRUE))[1]
  v <- to_int(unlist(lr[i, 3:10]))
  setNames(v, lr_years)
}
lr_valid <- lr_row("Gültige Zweitstimmen")

my_valid <- out[stimme == "zweitstimme",
                .(v = sum(votes, na.rm = TRUE)), by = election_year]
# Note: per-(wkr,stimme) valid_votes equals sum(party) by check (a), so summing
# party votes statewide == statewide Gueltige Zweitstimmen.
cat("\n(b1) Gültige Zweitstimmen: aggregated vs Lange-Reihe sheet 2\n")
b1_ok <- TRUE
for (yr in as.character(my_valid$election_year)) {
  mv <- my_valid[election_year == as.integer(yr), v]
  lv <- lr_valid[[yr]]
  ok <- !is.na(lv) && abs(mv - lv) == 0
  b1_ok <- b1_ok && ok
  cat(sprintf("   %s: mine=%d  langereihe=%s  match=%s\n",
              yr, mv, ifelse(is.na(lv), "NA", as.character(lv)), ok))
}

# major-party Zweitstimmen vs Lange-Reihe. Map Lange-Reihe labels -> the raw
# party_raw labels used in the per-year sheets (names vary by year).
lr_party_labels <- c("SPD", "CDU", "DIE LINKE", "AfD", "GRÜNE/B 90",
                     "BVB / FREIE WÄHLER", "FDP", "BSW")
# candidate raw aliases per Lange-Reihe party (across years)
alias <- list(
  "SPD" = c("SPD"),
  "CDU" = c("CDU"),
  "DIE LINKE" = c("DIE LINKE", "PDS", "PDS/LL"),
  "AfD" = c("AfD"),
  "GRÜNE/B 90" = c("GRÜNE/B 90", "GRÜNE/B90", "Bündnis 90", "Grüne"),
  "BVB / FREIE WÄHLER" = c("BVB / FREIE WÄHLER", "FREIE WÄHLER"),
  "FDP" = c("FDP"),
  "BSW" = c("BSW")
)
cat("\n(b2) major-party Zweitstimmen vs Lange-Reihe sheet 2 (per year):\n")
b2_fail <- character(0)
for (lbl in lr_party_labels) {
  lr_vec <- lr_row(lbl)
  for (yr in lr_years) {
    lv <- lr_vec[[yr]]
    if (is.na(lv)) next   # party absent that year ("x")
    mv <- out[stimme == "zweitstimme" & election_year == as.integer(yr) &
                party_raw %in% alias[[lbl]], sum(votes, na.rm = TRUE)]
    if (mv == 0 && is.na(lv)) next
    if (abs(mv - lv) != 0) {
      b2_fail <- c(b2_fail, sprintf("%s %s: mine=%d lr=%d", yr, lbl, mv, lv))
    }
  }
}
if (length(b2_fail) == 0) {
  cat("   ALL major-party x year Zweitstimmen totals match Lange-Reihe.\n")
} else {
  cat("   MISMATCHES:\n"); cat(paste0("   ", b2_fail, collapse = "\n"), "\n")
}

# --- (b3) full per-party "Land" total row for 2019 & 2024 (Kreise_Gemeinden A_2)
cat("\n(b3) full per-party statewide check vs Kreise_Gemeinden 'Land' row (2019/2024):\n")
read_land_totals <- function(file, sheet) {
  x <- read_excel(file.path(raw_dir, file), sheet = sheet, col_names = FALSE)
  h1 <- clean_hdr(unlist(x[1, ]))
  h2 <- clean_hdr(unlist(x[2, ]))
  # locate the Land total row: key col (2) starts with GI / equals 12000000 / name "Bundesland"
  key <- as.character(unlist(x[, 2]))
  nm  <- as.character(unlist(x[, 3]))
  land_i <- which((grepl("^GI", key) | key == "12000000") |
                  grepl("^Bundesland", nm))[1]
  c_val <- which(h1 == "Gültige Stimmen" | h1 == "Gültige\r\nStimmen" |
                 grepl("^Gültige", h1))[1]
  # party columns: to the right of valid votes, header row1 has a name and row2 == "Anzahl"
  cand <- which(seq_along(h1) > c_val & h2 == "Anzahl" & !is.na(h1) & nzchar(h1) & h1 != "NA")
  cand <- cand[!grepl("^Ungültige|^Gültige", h1[cand])]
  setNames(to_int(unlist(x[land_i, cand])), h1[cand])
}
b3_ok <- TRUE
for (yr in c("2019", "2024")) {
  file <- sprintf("BB_%s_Landtagswahl_Kreise_Gemeinden.xlsx", yr)
  land <- read_land_totals(file, "Brandenburg_Landtagswahl_A_2")  # A_2 = Zweitstimme
  cat(sprintf("   --- %s Zweitstimme ---\n", yr))
  for (lbl in names(land)) {
    lv <- land[[lbl]]
    if (is.na(lv)) next
    mv <- out[stimme == "zweitstimme" & election_year == as.integer(yr) &
                party_raw == lbl, sum(votes, na.rm = TRUE)]
    ok <- abs(mv - lv) == 0
    if (!ok) b3_ok <- FALSE
    if (!ok) cat(sprintf("      MISMATCH %-22s mine=%d land=%d\n", lbl, mv, lv))
  }
}
if (b3_ok) cat("   ALL 2019 & 2024 per-party Zweitstimme totals match the Land row.\n")

# distinct party labels
cat("\nDistinct party_raw labels emitted:\n")
print(sort(unique(out$party_raw)))

cat("\n================ DONE ================\n")
