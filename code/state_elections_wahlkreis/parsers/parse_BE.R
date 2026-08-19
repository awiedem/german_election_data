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
#
# 1999 / 2001 / 2006 / 2011 / 2021: parsed by the Stage-0 script
# 00_be_pdf_parse.py from the five digital-text Ergebnisberichte into
# be_pdf/BE_1999_2021_pdf_long.csv, appended below.  That script hard-validates
# every figure against the reports' own printed totals - the Berlin grand-total
# row of each Wahlkreis table, the statewide table 1.x party by party, the
# Bezirk subtotals, the separate turnout table, and the pinned official
# Zweitstimmen shares - and writes nothing if any check fails.
#
# Run order: python3 .../00_be_pdf_parse.py  ->  Rscript .../parse_BE.R
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

xlsx_years <- rbindlist(list(be16_e, be16_z, be23_e, be23_z), use.names = TRUE)
setcolorder(xlsx_years, OUT_COLS)

# =============================================================================
# 1999-2021 - Stage-0 PDF parse of the five Ergebnisberichte
# =============================================================================
pdf_csv <- file.path(out_dir, "be_pdf", "BE_1999_2021_pdf_long.csv")
if (!file.exists(pdf_csv)) {
  stop("Missing ", pdf_csv,
       "\n  Run first:  python3 code/state_elections_wahlkreis/parsers/00_be_pdf_parse.py")
}
pdf_long <- fread(pdf_csv, encoding = "UTF-8",
                  colClasses = list(character = c("state_abbr", "state",
                                                  "election_date", "wkr_nr",
                                                  "wkr_name", "stimme",
                                                  "party_raw")))
stopifnot(setequal(names(pdf_long), OUT_COLS))
setcolorder(pdf_long, OUT_COLS)

cat("\n=========== BE 1999-2021 (from the Ergebnisbericht PDFs) ===========\n")
print(pdf_long[, .(rows = .N, n_wkr = uniqueN(wkr_nr), n_parties = uniqueN(party_raw)),
               by = .(election_year, stimme)])

# (a) 78 Wahlkreise in every (year, stimme), and one row per party in each
stopifnot(pdf_long[, uniqueN(wkr_nr), by = .(election_year, stimme)]$V1 == 78L)
dupes <- pdf_long[, .N, by = .(election_year, stimme, wkr_nr, party_raw)][N > 1L]
if (nrow(dupes)) { print(head(dupes)); stop("BE PDF rows: duplicate party keys") }

# (b) one date per election year, and the expected five elections
dates <- unique(pdf_long[, .(election_year, election_date)])
stopifnot(nrow(dates) == 5L,
          identical(sort(dates$election_year), c(1999L, 2001L, 2006L, 2011L, 2021L)),
          identical(dates[order(election_year)]$election_date,
                    c("1999-10-10", "2001-10-21", "2006-09-17", "2011-09-18",
                      "2021-09-26")))

# (c) per (year, stimme, Wahlkreis): the party counts must add up to
#     valid_votes.  "x" in the report (party not on that ballot) is NA here.
chk <- pdf_long[, .(sum_party = sum(votes, na.rm = TRUE),
                    valid = unique(valid_votes)),
                by = .(election_year, stimme, wkr_nr)]
chk[, disc := abs(sum_party - valid)]
cat("    vote integrity : groups", nrow(chk), "| max abs discrepancy",
    max(chk$disc), "\n")
if (any(chk$disc > 0)) { print(chk[disc > 0]); stop("BE PDF rows fail vote integrity") }

# (d) turnout is one value per (year, stimme, Wahlkreis), and is ordered
turn <- pdf_long[, .(n_e = uniqueN(eligible_voters), n_v = uniqueN(number_voters),
                     n_g = uniqueN(valid_votes), n_u = uniqueN(invalid_votes)),
                 by = .(election_year, stimme, wkr_nr)]
stopifnot(turn[, all(c(n_e, n_v, n_g, n_u) == 1L)])
# Berlin publishes Waehler per Wahlkreis, not "abgegebene Stimmen": a voter may
# hand in a ballot without marking one of the two votes, so gueltige+ungueltige
# normally falls slightly short of Waehler (and in a handful of Wahlkreise the
# reports print the reverse by one or two votes).  Both are properties of the
# source, so this is a band, not an identity; the exact pinning happens in
# Stage 0 against the reports' own printed totals.
bal <- unique(pdf_long[, .(election_year, stimme, wkr_nr, eligible_voters,
                           number_voters, valid_votes, invalid_votes)])
bal[, slack := number_voters - valid_votes - invalid_votes]
cat(sprintf("    unmarked ballots: %d of %d Wahlkreis-Stimme cells exceed 5%% of Waehler\n",
            bal[abs(slack) > 0.05 * number_voters, .N], nrow(bal)))
stopifnot(bal[, all(abs(slack) <= 0.05 * number_voters &
                    number_voters <= eligible_voters)])

# (e) Wahlkreis ids: "BB-WW" with the Bezirk number Berlin itself uses.  From
#     2001 on that is the 12-Bezirk numbering the 2016/2023 Wahlbezirk files
#     use, so the Bezirk part must map to the same name in both sources.  1999
#     predates the Bezirksreform and runs on the 23 old Wahlkreisverbaende, so
#     it deliberately does NOT line up - asserted here so the break is visible.
stopifnot(pdf_long[, all(grepl("^[0-9]{2}-[0-9]{2}$", wkr_nr))])
bez <- function(d) unique(d[, .(bez_nr = substr(wkr_nr, 1, 2),
                                bez = sub(" [0-9]+$", "", wkr_name))])
ref_bez <- bez(as.data.table(xlsx_years))
new_bez <- bez(pdf_long[election_year >= 2001L])
cmp <- merge(ref_bez, new_bez, by = "bez_nr", suffixes = c("_xlsx", "_pdf"))
if (nrow(cmp) != 12L || any(cmp$bez_xlsx != cmp$bez_pdf)) {
  print(cmp); stop("BE Bezirk numbering diverges between the xlsx and PDF years")
}
cat("    Bezirk numbering identical to the 2016/2023 files: 12 / 12\n")
old_bez <- bez(pdf_long[election_year == 1999L])
stopifnot(nrow(old_bez) == 23L,
          identical(sort(old_bez$bez_nr), sprintf("%02d", 1:23)))
# Of the twelve numbers the two layouts share, only 01 (Mitte) names the same
# Bezirk; 02 is Tiergarten in 1999 and Friedrichshain-Kreuzberg from 2001 on.
same <- merge(ref_bez, old_bez, by = "bez_nr", suffixes = c("_new", "_1999"))
stopifnot(sum(same$bez_new == same$bez_1999) == 1L,
          same[bez_nr == "02"]$bez_1999 == "Tiergarten")
cat("    1999 uses the 23 pre-2001 Wahlkreisverbaende (own numbering 01-23)\n")

# =============================================================================
# COMBINE + WRITE
# =============================================================================
combined <- rbindlist(list(xlsx_years, pdf_long), use.names = TRUE)
setorder(combined, election_year, stimme, wkr_nr, party_raw)

cat("\n=========== COMBINED (BE) ===========\n")
print(combined[, .(rows = .N, n_wkr = uniqueN(wkr_nr),
                   n_parties = uniqueN(party_raw)), by = election_year])
cat(sprintf("\nTotal rows emitted: %d\n", nrow(combined)))
cat(sprintf("Distinct party_raw: %d\n", uniqueN(combined$party_raw)))

out_csv <- file.path(out_dir, "BE_ltw_wkr_long.csv")
fwrite(combined, out_csv)
cat("Wrote", nrow(combined), "rows to", out_csv, "\n")
