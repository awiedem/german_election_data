# =============================================================================
# parse_NW.R  —  Stage-1 cleaning parser for Nordrhein-Westfalen (NW)
# Landtagswahlen at the Wahlkreis (constituency) level.
#
# Source: IT.NRW / Landeswahlleiterin NRW open-data ";"-separated .txt files.
# Years parsed: 2000, 2005, 2010, 2012, 2017, 2022.
#   - 2000, 2005: single-vote NRW system  -> stimme = "einzelstimme"
#   - 2010+      : Erst-/Zweitstimme       -> stimme in {erststimme, zweitstimme}
#
# Output: long tidy CSV, one row per (Wahlkreis x stimme x party).
#   data/state_elections/processed/wahlkreis/NW_ltw_wkr_long.csv
#
# PDFs / TIFs (1954/1958/1962 and 2022 Hefte) are EXCLUDED (deferred to OCR).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_NW.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Nordrhein-Westfalen")
out_path <- here("data", "state_elections", "processed", "wahlkreis",
                 "NW_ltw_wkr_long.csv")

STATE_ABBR <- "NW"
STATE_NAME <- "Nordrhein-Westfalen"

# Election dates (official Wahltag)
ELECTION_DATE <- c(
  "2000" = "2000-05-14",
  "2005" = "2005-05-22",
  "2010" = "2010-05-09",
  "2012" = "2012-05-13",
  "2017" = "2017-05-14",
  "2022" = "2022-05-15"
)

# ---- helpers ----------------------------------------------------------------

# Parse a German integer-ish token to numeric. "-", "", "x", "." -> NA.
num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("-", "", "x", ".", "X", "/")] <- NA
  # thousands separators are absent in these files; strip any stray "." or spaces
  x <- gsub("[[:space:]]", "", x)
  suppressWarnings(as.numeric(x))
}

# read a ;-separated raw .txt with a given encoding, return character matrix
read_semicolon <- function(path, encoding) {
  con <- file(path, encoding = encoding)
  on.exit(close(con))
  lines <- readLines(con, warn = FALSE)
  lines <- gsub("\r$", "", lines)
  lines[lines != "" | TRUE]  # keep all; caller decides
}

split_semicolon <- function(lines) {
  strsplit(lines, ";", fixed = TRUE)
}

# Build a long data.frame for the "wide named header" years (2010/12/17/22).
# header_names: character vector of column header labels (descriptive form).
# data_rows   : list of character vectors (already split), one per Wahlkreis row.
# meta_map    : named list mapping output-meta-name -> header label, with the
#               stimme-specific valid/invalid handled separately.
# The function detects party columns by suffix " Erststimmen"/" Zweitstimmen"
# or " [E]"/" [Z]".
parse_wide <- function(header_names, rows, year,
                       col_nr, col_name,
                       col_eligible, col_voters,
                       lab_invalid_e, lab_invalid_z,
                       lab_valid_e, lab_valid_z) {

  # Identify party columns and their (party, stimme)
  party_idx_e <- list(); party_idx_z <- list()
  is_meta <- rep(FALSE, length(header_names))

  for (i in seq_along(header_names)) {
    h <- header_names[i]
    if (is.na(h) || h == "") { is_meta[i] <- TRUE; next }
  }

  # Determine suffix style
  e_suffix <- " Erststimmen"; z_suffix <- " Zweitstimmen"
  e_brak <- " [E]"; z_brak <- " [Z]"

  strip_party <- function(h) {
    if (endsWith(h, e_suffix)) return(list(p = sub(paste0(e_suffix, "$"), "", h), s = "erststimme"))
    if (endsWith(h, z_suffix)) return(list(p = sub(paste0(z_suffix, "$"), "", h), s = "zweitstimme"))
    if (endsWith(h, e_brak))   return(list(p = sub(" \\[E\\]$", "", h), s = "erststimme"))
    if (endsWith(h, z_brak))   return(list(p = sub(" \\[Z\\]$", "", h), s = "zweitstimme"))
    return(NULL)
  }

  # meta header labels to skip from party detection
  meta_labels <- c(col_nr, col_name, col_eligible, col_voters,
                   lab_invalid_e, lab_invalid_z, lab_valid_e, lab_valid_z,
                   "Wahl",
                   "Wahlberechtigte ohne Sperrvermerk \"W\"",
                   "Wahlberechtigte mit Sperrvermerk \"W\"",
                   "Wahlberechtigte mit selbständigem Wahlschein",
                   "Wahlberechtigte mit selbst. Wahlschein",
                   "darunter mit Wahlschein")

  party_cols <- list()  # list of (idx, party, stimme)
  for (i in seq_along(header_names)) {
    h <- header_names[i]
    if (is.na(h) || h == "") next
    if (h %in% meta_labels) next
    sp <- strip_party(h)
    if (!is.null(sp)) {
      party_cols[[length(party_cols) + 1]] <- list(idx = i, party = sp$p, stimme = sp$s)
    }
  }

  # column index lookups for meta
  idx_of <- function(lab) { w <- which(header_names == lab); if (length(w) == 0) NA_integer_ else w[1] }
  i_nr       <- idx_of(col_nr)
  i_name     <- idx_of(col_name)
  i_elig     <- idx_of(col_eligible)
  i_voters   <- idx_of(col_voters)
  i_inv_e    <- idx_of(lab_invalid_e)
  i_inv_z    <- idx_of(lab_invalid_z)
  i_val_e    <- idx_of(lab_valid_e)
  i_val_z    <- idx_of(lab_valid_z)

  out <- list()
  for (r in rows) {
    nr_raw <- trimws(r[i_nr])
    # statewide total row excluded from output (handled by caller for validation)
    name   <- trimws(r[i_name])
    elig   <- num(r[i_elig])
    voters <- num(r[i_voters])
    val_e  <- num(r[i_val_e]); val_z <- num(r[i_val_z])
    inv_e  <- num(r[i_inv_e]); inv_z <- num(r[i_inv_z])

    for (pc in party_cols) {
      v <- num(r[pc$idx])
      stimme <- pc$stimme
      out[[length(out) + 1]] <- data.table(
        state_abbr = STATE_ABBR, state = STATE_NAME,
        election_year = as.integer(year),
        election_date = ELECTION_DATE[[as.character(year)]],
        wkr_nr = nr_raw, wkr_name = name,
        stimme = stimme,
        eligible_voters = elig, number_voters = voters,
        valid_votes   = if (stimme == "erststimme") val_e else val_z,
        invalid_votes = if (stimme == "erststimme") inv_e else inv_z,
        party_raw = pc$party, votes = v
      )
    }
  }
  rbindlist(out)
}

# ---- 2000 -------------------------------------------------------------------
# Header on line 3. Single vote (einzelstimme).
# cols: 1 nr, 2 name, 3 Wahlberechtigte, 4 Wahlbeteiligung(=number_voters),
#       5 Ungültige, 6 Gültige, 7..N parties (last col may be trailing empty)
parse_2000 <- function() {
  path <- file.path(raw_dir, "NW_2000_Landtagswahl_Wahlkreis.txt")
  lines <- read_semicolon(path, "latin1")
  sp <- split_semicolon(lines)
  hdr <- sp[[3]]
  data_lines <- sp[4:length(sp)]
  # statewide row has nr == "0"
  statewide <- NULL
  out <- list()
  party_idx <- 7:length(hdr)
  party_idx <- party_idx[!is.na(hdr[party_idx]) & trimws(hdr[party_idx]) != ""]
  for (r in data_lines) {
    nr <- trimws(r[1])
    name <- trimws(r[2])
    if (nr == "0") { statewide <- r; next }
    elig <- num(r[3]); voters <- num(r[4]); inv <- num(r[5]); val <- num(r[6])
    for (i in party_idx) {
      out[[length(out)+1]] <- data.table(
        state_abbr=STATE_ABBR, state=STATE_NAME, election_year=2000L,
        election_date=ELECTION_DATE[["2000"]],
        wkr_nr=nr, wkr_name=name, stimme="einzelstimme",
        eligible_voters=elig, number_voters=voters,
        valid_votes=val, invalid_votes=inv,
        party_raw=trimws(hdr[i]), votes=num(r[i]))
    }
  }
  list(long = rbindlist(out), hdr = hdr, statewide = statewide,
       party_idx = party_idx, n_wkr = length(data_lines) - 1L)
}

# ---- 2005 -------------------------------------------------------------------
# Header on line 1. Single vote. Value;% interleaved.
# cols: 1 nr,2 name,3 elig,4 %,5 voters,6 %,7 inv,8 %,9 valid,10 %,
#       then party;% pairs (party at 11,13,15,...)
parse_2005 <- function() {
  path <- file.path(raw_dir, "NW_2005_Landtagswahl_Wahlkreis.txt")
  lines <- read_semicolon(path, "UTF-16LE")
  lines <- sub("^﻿", "", lines)
  sp <- split_semicolon(lines)
  hdr <- sp[[1]]
  data_lines <- sp[2:length(sp)]
  party_idx <- seq(11, length(hdr), by = 2)
  party_idx <- party_idx[!is.na(hdr[party_idx]) & trimws(hdr[party_idx]) != "" &
                           trimws(hdr[party_idx]) != "%"]
  statewide <- NULL
  out <- list()
  for (r in data_lines) {
    nr <- trimws(r[1]); name <- trimws(r[2])
    if (nr == "0") { statewide <- r; next }
    elig <- num(r[3]); voters <- num(r[5]); inv <- num(r[7]); val <- num(r[9])
    for (i in party_idx) {
      out[[length(out)+1]] <- data.table(
        state_abbr=STATE_ABBR, state=STATE_NAME, election_year=2005L,
        election_date=ELECTION_DATE[["2005"]],
        wkr_nr=nr, wkr_name=name, stimme="einzelstimme",
        eligible_voters=elig, number_voters=voters,
        valid_votes=val, invalid_votes=inv,
        party_raw=trimws(hdr[i]), votes=num(r[i]))
    }
  }
  list(long = rbindlist(out), hdr = hdr, statewide = statewide,
       party_idx = party_idx, n_wkr = length(data_lines) - 1L)
}

# ---- 2010 / 2012 (descriptive header on line 1) -----------------------------
parse_2010_2012 <- function(year, encoding) {
  fn <- sprintf("NW_%d_Landtagswahl_Wahlkreis.txt", year)
  path <- file.path(raw_dir, fn)
  lines <- read_semicolon(path, encoding)
  sp <- split_semicolon(lines)
  hdr <- trimws(sp[[1]])
  data_lines <- sp[2:length(sp)]
  statewide <- NULL; wkr_rows <- list()
  for (r in data_lines) {
    if (trimws(r[1]) == "0") statewide <- r else wkr_rows[[length(wkr_rows)+1]] <- r
  }
  long <- parse_wide(
    header_names = hdr, rows = wkr_rows, year = year,
    col_nr = "Wahlkreisnr.", col_name = "Wahlkreisname",
    col_eligible = "Wahlberechtigte insgesamt", col_voters = "Wähler/-innen",
    lab_invalid_e = "Ungültige Stimmen Erststimmen",
    lab_invalid_z = "Ungültige Stimmen Zweitstimmen",
    lab_valid_e   = "Gültige Stimmen Erststimmen",
    lab_valid_z   = "Gültige Stimmen Zweitstimmen")
  list(long = long, hdr = hdr, statewide = statewide, n_wkr = length(wkr_rows))
}

# ---- 2017 (2 title lines, descriptive header on line 3) ----------------------
parse_2017 <- function() {
  path <- file.path(raw_dir, "NW_2017_Landtagswahl_Wahlkreis.txt")
  lines <- read_semicolon(path, "UTF-8")
  sp <- split_semicolon(lines)
  hdr <- trimws(sp[[3]])
  data_lines <- sp[4:length(sp)]
  statewide <- NULL; wkr_rows <- list()
  for (r in data_lines) {
    if (trimws(r[1]) == "000") statewide <- r else wkr_rows[[length(wkr_rows)+1]] <- r
  }
  long <- parse_wide(
    header_names = hdr, rows = wkr_rows, year = 2017,
    col_nr = "Wahlkreisnr.", col_name = "Wahlkreisname",
    col_eligible = "Wahlberechtigte insgesamt", col_voters = "Wähler/-innen",
    lab_invalid_e = "Ungültige Stimmen Erststimmen",
    lab_invalid_z = "Ungültige Stimmen Zweitstimmen",
    lab_valid_e   = "Gültige Stimmen Erststimmen",
    lab_valid_z   = "Gültige Stimmen Zweitstimmen")
  list(long = long, hdr = hdr, statewide = statewide, n_wkr = length(wkr_rows))
}

# ---- 2022 (3 title lines, named header line 4, code header line 5) -----------
parse_2022 <- function() {
  path <- file.path(raw_dir, "NW_2022_Landtagswahl_Wahlkreis.txt")
  lines <- read_semicolon(path, "UTF-8")
  sp <- split_semicolon(lines)
  hdr <- trimws(sp[[4]])              # descriptive party-name header (with [E]/[Z])
  data_lines <- sp[6:length(sp)]      # data starts after both header rows
  statewide <- NULL; wkr_rows <- list()
  for (r in data_lines) {
    if (trimws(r[2]) == "000") statewide <- r else wkr_rows[[length(wkr_rows)+1]] <- r
  }
  long <- parse_wide(
    header_names = hdr, rows = wkr_rows, year = 2022,
    col_nr = "Wahlkreisnr.", col_name = "Wahlkreisname",
    col_eligible = "Wahlberechtigte insgesamt", col_voters = "Wähler/-innen",
    lab_invalid_e = "Ungültige Stimmen [E]",
    lab_invalid_z = "Ungültige Stimmen [Z]",
    lab_valid_e   = "Gültige Stimmen [E]",
    lab_valid_z   = "Gültige Stimmen [Z]")
  list(long = long, hdr = hdr, statewide = statewide, n_wkr = length(wkr_rows))
}

# =============================================================================
# RUN
# =============================================================================
res <- list(
  "2000" = parse_2000(),
  "2005" = parse_2005(),
  "2010" = parse_2010_2012(2010, "latin1"),
  "2012" = parse_2010_2012(2012, "latin1"),
  "2017" = parse_2017(),
  "2022" = parse_2022()
)

combined <- rbindlist(lapply(res, `[[`, "long"), use.names = TRUE)

# Column order exactly as specified
col_order <- c("state_abbr","state","election_year","election_date",
               "wkr_nr","wkr_name","stimme","eligible_voters","number_voters",
               "valid_votes","invalid_votes","party_raw","votes")
setcolorder(combined, col_order)

# =============================================================================
# VALIDATION
# =============================================================================
cat("\n==================== VALIDATION ====================\n")

# (a) per (wkr,stimme): |sum(party votes) - valid_votes| ~ 0
chk <- combined[!is.na(votes), .(sum_votes = sum(votes, na.rm = TRUE),
                                 valid = first(valid_votes)),
                by = .(election_year, wkr_nr, stimme)]
chk[, disc := abs(sum_votes - valid)]
max_disc <- max(chk$disc, na.rm = TRUE)
n_groups <- nrow(chk)
cat(sprintf("(a) per-(wkr,stimme) integrity: %d groups checked; MAX |sum-valid| = %s\n",
            n_groups, format(max_disc, big.mark = ",")))
print(chk[order(-disc)][1:5])

# (b) statewide total match per party x stimme
statewide_results <- list()
for (yr in names(res)) {
  obj <- res[[yr]]
  if (is.null(obj$statewide)) next
  # rebuild statewide as a 1-row "wahlkreis" through the same logic by faking
  # a parse over the statewide vector.
  sw <- obj$statewide
  hdr <- obj$hdr
  if (yr %in% c("2000")) {
    for (i in obj$party_idx)
      statewide_results[[length(statewide_results)+1]] <- data.table(
        election_year = as.integer(yr), stimme = "einzelstimme",
        party_raw = trimws(hdr[i]), sw_votes = num(sw[i]))
  } else if (yr %in% c("2005")) {
    for (i in obj$party_idx)
      statewide_results[[length(statewide_results)+1]] <- data.table(
        election_year = as.integer(yr), stimme = "einzelstimme",
        party_raw = trimws(hdr[i]), sw_votes = num(sw[i]))
  } else {
    # wide years: detect party columns the same way parse_wide does
    for (i in seq_along(hdr)) {
      h <- hdr[i]
      if (is.na(h) || h == "") next
      st <- NA_character_; p <- NA_character_
      if (endsWith(h, " Erststimmen")) { st <- "erststimme"; p <- sub(" Erststimmen$","",h) }
      else if (endsWith(h, " Zweitstimmen")) { st <- "zweitstimme"; p <- sub(" Zweitstimmen$","",h) }
      else if (endsWith(h, " [E]")) { st <- "erststimme"; p <- sub(" \\[E\\]$","",h) }
      else if (endsWith(h, " [Z]")) { st <- "zweitstimme"; p <- sub(" \\[Z\\]$","",h) }
      else next
      if (p %in% c("Ungültige Stimmen","Gültige Stimmen")) next
      statewide_results[[length(statewide_results)+1]] <- data.table(
        election_year = as.integer(yr), stimme = st,
        party_raw = p, sw_votes = num(sw[i]))
    }
  }
}
sw_dt <- rbindlist(statewide_results)

# Some years split individual candidates per Wahlkreis ("Einzelbewerber/-in 1/2",
# "Einzelbewerber1/2", "WGR/EinzBew.") but report a single aggregate
# ("Einzelbewerber/-innen") in the statewide row. Collapse these to one key on
# BOTH sides purely for the statewide-total reconciliation (the emitted CSV
# keeps the raw labels verbatim).
einzel_key <- function(p) {
  ifelse(grepl("^Einzelbewerber", p) | p == "WGR/EinzBew.",
         "__EINZELBEWERBER__", p)
}
sw_dt[,  pk := einzel_key(party_raw)]
our_sum <- combined[, .(our_votes = sum(votes, na.rm = TRUE)),
                    by = .(election_year, stimme, party_raw)]
our_sum[, pk := einzel_key(party_raw)]
sw_agg  <- sw_dt[,  .(sw_votes  = sum(sw_votes,  na.rm = TRUE)), by = .(election_year, stimme, pk)]
our_agg <- our_sum[, .(our_votes = sum(our_votes, na.rm = TRUE)), by = .(election_year, stimme, pk)]
cmp <- merge(sw_agg, our_agg, by = c("election_year","stimme","pk"), all = TRUE)
setnames(cmp, "pk", "party_raw")
cmp[is.na(sw_votes), sw_votes := 0]
cmp[is.na(our_votes), our_votes := 0]
cmp[, diff := our_votes - sw_votes]
fails <- cmp[abs(diff) > 0]
statewide_match <- nrow(fails) == 0
cat(sprintf("\n(b) STATEWIDE TOTAL MATCH per party x stimme: match = %s\n", statewide_match))
if (!statewide_match) {
  cat("   FAILURES:\n"); print(fails[order(-abs(diff))])
} else {
  cat(sprintf("   all %d (year x stimme x party) statewide totals match exactly.\n", nrow(cmp)))
}

# (c) Wahlkreis count per year
cat("\n(c) Wahlkreis count per year:\n")
wkrcnt <- combined[, .(n_wkr = uniqueN(wkr_nr)), by = election_year]
print(wkrcnt)

cat(sprintf("\nTotal rows emitted: %s\n", format(nrow(combined), big.mark=",")))
cat(sprintf("Distinct party_raw labels: %d\n", uniqueN(combined$party_raw)))

# =============================================================================
# WRITE
# =============================================================================
fwrite(combined, out_path)
cat(sprintf("\nWritten: %s\n", out_path))
