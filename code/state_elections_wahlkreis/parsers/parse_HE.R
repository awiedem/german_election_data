# =============================================================================
# parse_HE.R — Stage-1 CLEANING parser for Hessen (HE) Landtagswahl results
#              at the CONSTITUENCY (Wahlkreis) level.
#
# Output: long, tidy CSV with columns:
#   state_abbr,state,election_year,election_date,wkr_nr,wkr_name,stimme,
#   eligible_voters,number_voters,valid_votes,invalid_votes,party_raw,votes
#
# Vote system: erststimme = Wahlkreisstimme, zweitstimme = Landesstimme.
#
# Machine-readable sources in the HE folder:
#   - HE_2023_Landtagswahl_Wahlkreis_opendata.csv  -> 2023, 55 Wahlkreise (PARSED)
#   - HE_seit1946_..._Landesergebnisse_Tabelle1.xlsx -> STATE-LEVEL only (EXCLUDED:
#         not Wahlkreis-level; aggregate Land series).
#   - All .pdf / .tif files are scans -> deferred to OCR stage (EXCLUDED).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_HE.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Hessen")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 2023 — HSL open-data CSV (UTF-8, ';'-sep, CRLF; one header preamble line)
# -----------------------------------------------------------------------------
csv_2023 <- file.path(raw_dir, "HE_2023_Landtagswahl_Wahlkreis_opendata.csv")

# read everything as character to preserve leading zeros / exact tokens
dt <- fread(csv_2023, sep = ";", skip = 1, header = TRUE,
            encoding = "UTF-8", colClasses = "character")

# party labels present in the file (verbatim), in source column order
party_raw_levels <- c(
  "CDU", "GRÜNE", "SPD", "AfD", "FDP", "DIE LINKE", "FREIE WÄHLER",
  "Tierschutzpartei", "Die PARTEI", "PIRATEN", "ÖDP", "Verjüngungsforschung",
  "V-Partei³", "PdH", "ABG", "APPD", "dieBasis", "DKP", "DIE NEUE MITTE",
  "Volt", "KLIMALISTE WÄHLERL.", "Bündnis C", "WDMR", "BUNDESPA. KLIMALISTE",
  "MERA25", "NEV", "PP", "SGV", "Solibew"
)

# numeric helper (German thousands have NO separator in this file; plain digits)
as_int <- function(x) {
  x <- trimws(x)
  x[x == "" | x == "." | x == "-"] <- NA
  suppressWarnings(as.integer(x))
}

# --- build long table for one stimme -----------------------------------------
build_stimme <- function(d, suffix, stimme_label) {
  # column names for this stimme
  valid_col   <- paste0("gültige ", suffix)
  invalid_col <- paste0("ungültige ", suffix)
  party_cols  <- paste0(party_raw_levels, " ", suffix)

  long <- map_dfr(seq_along(party_raw_levels), function(i) {
    data.table(
      wkr_nr        = d$wkr_nr,
      wkr_name      = d$wkr_name,
      eligible_voters = d$eligible_voters,
      number_voters   = d$number_voters,
      valid_votes   = as_int(d[[valid_col]]),
      invalid_votes = as_int(d[[invalid_col]]),
      stimme        = stimme_label,
      party_raw     = party_raw_levels[i],
      votes         = as_int(d[[party_cols[i]]])
    )
  })
  long
}

# --- Wahlkreis rows -----------------------------------------------------------
wk <- dt[Gebietstyp == "WK"]
stopifnot(nrow(wk) == 55L)

# wkr_nr: first 3 chars of the 11-digit Gebietsschlüssel are the WK number
# (e.g. "00100000000" -> WK 1). Preserve as zero-padded 2-digit character.
wk[, wkr_nr := sprintf("%02d", as.integer(substr(Gebietsschlüssel, 1, 3)))]
wk[, wkr_name := Gebietsbezeichnung]
wk[, eligible_voters := as_int(Wahlberechtigte)]
wk[, number_voters   := as_int(`Wählerinnen und Wähler`)]

erst  <- build_stimme(wk, "Wahlkreisstimmen", "erststimme")
zweit <- build_stimme(wk, "Landesstimmen",    "zweitstimme")

long <- rbindlist(list(erst, zweit), use.names = TRUE)

# add fixed metadata columns
long[, `:=`(
  state_abbr    = "HE",
  state         = "Hessen",
  election_year = 2023L,
  election_date = "2023-10-08"
)]

# enforce exact column order
col_order <- c("state_abbr", "state", "election_year", "election_date",
               "wkr_nr", "wkr_name", "stimme",
               "eligible_voters", "number_voters",
               "valid_votes", "invalid_votes", "party_raw", "votes")
setcolorder(long, col_order)
setorder(long, election_year, stimme, wkr_nr, party_raw)

# =============================================================================
# VALIDATION
# =============================================================================
cat("=========== VALIDATION (HE 2023) ===========\n")

# (a) per (wkr, stimme): |sum(party votes) - valid_votes| ~ 0
chk <- long[, .(sum_party = sum(votes, na.rm = TRUE),
                valid = unique(valid_votes)),
            by = .(wkr_nr, stimme)]
chk[, disc := abs(sum_party - valid)]
cat("(a) per (wkr,stimme) integrity:\n")
cat("    groups checked   :", nrow(chk), "\n")
cat("    max abs discrep  :", max(chk$disc), "\n")
print(chk[disc > 0])

# (b) statewide total match vs source LD ("Hessen", Gebietsschlüssel all-zeros)
ld <- dt[Gebietstyp == "LD"]
stopifnot(nrow(ld) == 1L)

statewide_check <- function(suffix, stimme_label) {
  party_cols <- paste0(party_raw_levels, " ", suffix)
  src <- sapply(party_cols, function(cc) as_int(ld[[cc]]))
  names(src) <- party_raw_levels
  mine <- long[stimme == stimme_label,
               .(v = sum(votes, na.rm = TRUE)), by = party_raw]
  cmp <- data.table(party_raw = party_raw_levels,
                    source_total = as.integer(src),
                    my_total = mine$v[match(party_raw_levels, mine$party_raw)])
  # Parties that fielded no candidate for this stimme have a BLANK statewide
  # total in the source LD row (and NA/0 per-Wahlkreis). Treat NA as 0 so the
  # comparison is well-defined; a genuine mismatch still shows diff > 0.
  cmp[is.na(source_total), source_total := 0L]
  cmp[is.na(my_total),     my_total := 0L]
  cmp[, diff := abs(source_total - my_total)]
  cmp
}

cmp_e <- statewide_check("Wahlkreisstimmen", "erststimme")
cmp_z <- statewide_check("Landesstimmen",    "zweitstimme")

cat("\n(b) statewide total match (erststimme / Wahlkreisstimmen):\n")
cat("    max party diff:", max(cmp_e$diff), " | match:",
    all(cmp_e$diff == 0), "\n")
print(cmp_e[diff > 0])
cat("(b) statewide total match (zweitstimme / Landesstimmen):\n")
cat("    max party diff:", max(cmp_z$diff), " | match:",
    all(cmp_z$diff == 0), "\n")
print(cmp_z[diff > 0])

# also: statewide valid_votes sum match
src_valid_e <- as_int(ld[["gültige Wahlkreisstimmen"]])
src_valid_z <- as_int(ld[["gültige Landesstimmen"]])
my_valid_e <- sum(chk[stimme == "erststimme"]$valid)
my_valid_z <- sum(chk[stimme == "zweitstimme"]$valid)
cat("\n    valid_votes statewide  erst: source", src_valid_e,
    "mine", my_valid_e, "match", src_valid_e == my_valid_e, "\n")
cat("    valid_votes statewide zweit: source", src_valid_z,
    "mine", my_valid_z, "match", src_valid_z == my_valid_z, "\n")

# (c) Wahlkreis count per year
cat("\n(c) Wahlkreis count per year:\n")
print(long[, .(n_wkr = uniqueN(wkr_nr)), by = election_year])

cat("\nTotal rows emitted:", nrow(long), "\n")
cat("Distinct party_raw :", length(unique(long$party_raw)), "\n")

# =============================================================================
# WRITE OUTPUT
# =============================================================================
out_csv <- file.path(out_dir, "HE_ltw_wkr_long.csv")
fwrite(long, out_csv, bom = TRUE)
cat("\nWrote:", out_csv, "\n")
