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
# Sources in the HE folder:
#   - HE_2023_Landtagswahl_Wahlkreis_opendata.csv  -> 2023, 55 Wahlkreise (PARSED HERE)
#   - HE_2018_Landtagswahl_Wahlkreis_BVII2-4.pdf   -> 2018 AND 2013, 55 Wahlkreise,
#         parsed by the Stage-0 script 00_he_pdf_parse.py (which also cross-checks
#         every 2013 figure against HE_2018_2013_..._Vergleichszahlen_BVII2-1.pdf)
#         into he_pdf/HE_2018_2013_pdf_long.csv, appended below.
#   - HE_seit1946_..._Landesergebnisse_Tabelle1.xlsx -> STATE-LEVEL only (EXCLUDED:
#         not Wahlkreis-level; aggregate Land series).
#   - Remaining .pdf / .tif files are pre-2013 scans -> not yet ingested.
#
# NB on 2013: the Dec-2017 LWG amendment re-cut some Wahlkreise, and the source
# reports the 2013 results recomputed onto the 2018 Wahlkreiseinteilung.  Rows
# carry flag_wkr_boundaries_recomputed = 1 where that is the case (0 for 2018,
# and 0 for the two Frankfurt Wahlkreise 34/37 that B VII 2-4 left on their own
# 2013 boundaries - see 00_he_pdf_parse.py).
#
# Run order: python3 .../00_he_pdf_parse.py  ->  Rscript .../parse_HE.R
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
  election_date = "2023-10-08",
  flag_wkr_boundaries_recomputed = 0L
)]

# enforce exact column order
col_order <- c("state_abbr", "state", "election_year", "election_date",
               "wkr_nr", "wkr_name", "stimme",
               "eligible_voters", "number_voters",
               "valid_votes", "invalid_votes", "party_raw", "votes",
               "flag_wkr_boundaries_recomputed")
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

# =============================================================================
# 2018 + 2013 - Stage-0 PDF parse (B VII 2-4, cross-checked against B VII 2-1)
# =============================================================================
pdf_csv <- file.path(out_dir, "he_pdf", "HE_2018_2013_pdf_long.csv")
if (!file.exists(pdf_csv)) {
  stop("Missing ", pdf_csv,
       "\n  Run first:  python3 code/state_elections_wahlkreis/parsers/00_he_pdf_parse.py")
}
pdf_long <- fread(pdf_csv, encoding = "UTF-8",
                  colClasses = list(character = c("state_abbr", "state", "election_date",
                                                  "wkr_nr", "wkr_name", "stimme",
                                                  "party_raw")))
stopifnot(setequal(names(pdf_long), col_order))
setcolorder(pdf_long, col_order)

cat("\n=========== HE 2018 + 2013 (from B VII 2-4) ===========\n")
cat("    rows read        :", nrow(pdf_long), "\n")
cat("    Wahlkreise/year  :\n")
print(pdf_long[, .(n_wkr = uniqueN(wkr_nr), n_parties = uniqueN(party_raw)),
               by = .(election_year, stimme)])

# per (year, wkr, stimme): sum(party votes) must equal valid_votes
chk2 <- pdf_long[, .(sum_party = sum(votes, na.rm = TRUE), valid = unique(valid_votes)),
                 by = .(election_year, wkr_nr, stimme)]
chk2[, disc := abs(sum_party - valid)]
cat("    vote integrity   : groups", nrow(chk2), "| max abs discrepancy", max(chk2$disc), "\n")
if (any(chk2$disc > 0)) { print(chk2[disc > 0]); stop("HE PDF rows fail vote integrity") }

# the 55 Wahlkreis names must be the same objects the 2023 open data uses
n23 <- unique(long[, .(wkr_nr, wkr_name)])
n_pdf <- unique(pdf_long[, .(wkr_nr, wkr_name)])
cmp_names <- merge(n23, n_pdf, by = "wkr_nr", suffixes = c("_2023", "_pdf"))
cat("    Wahlkreis names identical to the 2023 open data:",
    sum(cmp_names$wkr_name_2023 == cmp_names$wkr_name_pdf), "/", nrow(cmp_names), "\n")
if (!all(cmp_names$wkr_name_2023 == cmp_names$wkr_name_pdf)) {
  print(cmp_names[wkr_name_2023 != wkr_name_pdf]); stop("HE Wahlkreis names diverge across years")
}

long <- rbindlist(list(long, pdf_long), use.names = TRUE)
setorder(long, election_year, stimme, wkr_nr, party_raw)

# =============================================================================
# 2009 - Stage-0 PDF parse (Staatsanzeiger Nr. 8/2009, own pre-2018 boundaries)
# =============================================================================
# NB: 2009 predates the Dec-2017 LWG re-cut entirely, so its 55 Wahlkreise are
# NOT the same geographic units as 2013/2018/2023 (even though both run 1-55).
# Unlike the 2018/2013 block above, we deliberately do NOT compare wkr_name
# against later years here - see 00_he09_pdf_parse.py for the boundary story.
# flag_wkr_boundaries_recomputed = 0 for every 2009 row (own boundaries, no
# recomputation happened).
pdf09_csv <- file.path(out_dir, "he_pdf", "HE_2009_pdf_long.csv")
if (!file.exists(pdf09_csv)) {
  stop("Missing ", pdf09_csv,
       "\n  Run first:  python3 code/state_elections_wahlkreis/parsers/00_he09_pdf_parse.py")
}
pdf09_long <- fread(pdf09_csv, encoding = "UTF-8",
                    colClasses = list(character = c("state_abbr", "state", "election_date",
                                                    "wkr_nr", "wkr_name", "stimme",
                                                    "party_raw")))
stopifnot(setequal(names(pdf09_long), col_order))
setcolorder(pdf09_long, col_order)

cat("\n=========== HE 2009 (Staatsanzeiger Nr. 8/2009) ===========\n")
cat("    rows read        :", nrow(pdf09_long), "\n")
print(pdf09_long[, .(n_wkr = uniqueN(wkr_nr), n_parties = uniqueN(party_raw)),
                 by = .(election_year, stimme)])

# per (wkr, stimme): sum(party votes) must equal valid_votes, EXCEPT the
# single pinned Wahlkreis-44/erststimme source defect (11-vote gap, kept as
# printed - see 00_he09_pdf_parse.py). The exception is pinned exactly, not a
# loosened global tolerance.
chk09 <- pdf09_long[, .(sum_party = sum(votes, na.rm = TRUE), valid = unique(valid_votes)),
                    by = .(wkr_nr, stimme)]
chk09[, disc := valid - sum_party]
pinned09 <- chk09[wkr_nr == "44" & stimme == "erststimme" & disc == 11L]
unexpected09 <- chk09[disc != 0L][!(wkr_nr == "44" & stimme == "erststimme" & disc == 11L)]
cat("    vote integrity   : groups", nrow(chk09),
    "| pinned Wahlkreis-44 erststimme gap found:", nrow(pinned09) == 1L,
    "| unexpected discrepancies:", nrow(unexpected09), "\n")
if (nrow(pinned09) != 1L || nrow(unexpected09) > 0L) {
  print(chk09[disc != 0L])
  stop("HE 2009 PDF rows fail vote integrity (expected exactly the pinned Wahlkreis-44 gap)")
}

long <- rbindlist(list(long, pdf09_long), use.names = TRUE)
setorder(long, election_year, stimme, wkr_nr, party_raw)

cat("\n=========== COMBINED (HE) ===========\n")
print(long[, .(rows = .N, n_wkr = uniqueN(wkr_nr)), by = .(election_year, stimme)])
cat("boundary flag rows by year:\n")
print(long[, .(recomputed = sum(flag_wkr_boundaries_recomputed), rows = .N),
           by = election_year])

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
