# =============================================================================
# parse_SL.R
# Stage-1 cleaning parser: Saarland (SL) Landtagswahl at Wahlkreis level.
#
# Vote system: einzelstimme (one Listenstimme across 3 große Wahlkreise).
#
# Only machine-readable source: SL_2022_Landtagswahl_KERG.csv (votemanager KERG,
# ";"-separated, UTF-8 BOM). It contains Land + 3 Wahlkreise + 52 Gemeinden.
# We keep ONLY the 3 Wahlkreis rows (Nr 1/2/3, gehört-zu = 10). The Land total
# row (Nr 10) is used for validation only. Gemeinde rows (gehört-zu 1/2/3) are
# dropped. All other SL years are scans (PDF/TIF) -> excluded (future OCR stage).
#
# Output: a long tidy CSV, one row per (Wahlkreis x stimme x party).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_SL.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Saarland")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

in_csv  <- file.path(raw_dir, "SL_2022_Landtagswahl_KERG.csv")
out_csv <- file.path(out_dir, "SL_ltw_wkr_long.csv")

ELECTION_YEAR <- 2022L
ELECTION_DATE <- "2022-03-27"
STATE_ABBR    <- "SL"
STATE_NAME    <- "Saarland"
STIMME        <- "einzelstimme"

# ---------------------------------------------------------------------------
# Read raw lines and split on ";" preserving empty fields.
# ---------------------------------------------------------------------------
lines <- readLines(in_csv, encoding = "UTF-8", warn = FALSE)
# strip UTF-8 BOM from first line if present
lines[1] <- sub("^﻿", "", lines[1])

split_row <- function(x) strsplit(x, ";", fixed = TRUE)[[1]]

# Header row 3 carries the column names; each measure occupies two physical
# columns (Endgültig ; Vorperiode). We map measures to their "Endgültig" index.
hdr <- split_row(lines[3])
stopifnot(hdr[1] == "Nr", hdr[2] == "Gebiet", hdr[3] == "gehört zu")

# Measure header positions: every non-empty entry from column 4 onward begins a
# 2-column block (Endgültig at that index, Vorperiode at index+1).
measure_idx  <- which(nzchar(hdr) & seq_along(hdr) >= 4)
measure_name <- hdr[measure_idx]

# Sanity: the trailing 5 measures must be the turnout/aggregate columns.
expected_tail <- c("Wahlberechtigte", "Wähler", "Ungültige Stimmen",
                   "Gültige Stimmen", "Übrige")
tail_got <- tail(measure_name, 5)
stopifnot(identical(tail_got, expected_tail))

# Party measures = everything before the turnout block.
turnout_pos  <- match(expected_tail, measure_name)
first_meta   <- min(turnout_pos)
party_names  <- measure_name[seq_len(first_meta - 1)]
party_endidx <- measure_idx[seq_len(first_meta - 1)]

idx_wbr <- measure_idx[match("Wahlberechtigte",   measure_name)]
idx_wae <- measure_idx[match("Wähler",            measure_name)]
idx_ung <- measure_idx[match("Ungültige Stimmen", measure_name)]
idx_gue <- measure_idx[match("Gültige Stimmen",   measure_name)]

# ---------------------------------------------------------------------------
# Parse data rows (row 6 onward). Keep Wahlkreis rows and the Land row.
# ---------------------------------------------------------------------------
num <- function(v) {
  v <- trimws(v)
  ifelse(v == "" | is.na(v), NA_real_, suppressWarnings(as.numeric(v)))
}

data_rows <- lapply(lines[6:length(lines)], split_row)
# drop blank separator lines (";" only -> length 1-2 with empties)
data_rows <- Filter(function(r) length(r) >= 49 && nzchar(trimws(r[1])), data_rows)

parse_area <- function(r) {
  list(
    nr       = trimws(r[1]),
    name     = trimws(r[2]),
    gehoert  = trimws(r[3]),
    wbr      = num(r[idx_wbr]),
    wae      = num(r[idx_wae]),
    ung      = num(r[idx_ung]),
    gue      = num(r[idx_gue]),
    party    = setNames(num(r[party_endidx]), party_names)
  )
}
areas <- lapply(data_rows, parse_area)

# Wahlkreis rows: gehört-zu == "10" AND nr in 1/2/3 (the 3 große Wahlkreise).
is_wk   <- vapply(areas, function(a) a$gehoert == "10" && a$nr %in% c("1","2","3"), logical(1))
is_land <- vapply(areas, function(a) a$nr == "10", logical(1))

wk_areas   <- areas[is_wk]
land_area  <- areas[is_land][[1]]

stopifnot(length(wk_areas) == 3L)

# ---------------------------------------------------------------------------
# Build long output. wkr_nr CHARACTER with leading zeros as "001"/"002"/"003".
# ---------------------------------------------------------------------------
long <- map_dfr(wk_areas, function(a) {
  tibble(
    state_abbr      = STATE_ABBR,
    state           = STATE_NAME,
    election_year   = ELECTION_YEAR,
    election_date   = ELECTION_DATE,
    wkr_nr          = sprintf("%03d", as.integer(a$nr)),
    wkr_name        = a$name,
    stimme          = STIMME,
    eligible_voters = a$wbr,
    number_voters   = a$wae,
    valid_votes     = a$gue,
    invalid_votes   = a$ung,
    party_raw       = names(a$party),
    votes           = unname(a$party)
  )
})

# Drop parties with no Endgültig value in ANY Wahlkreis (e.g. Piratenpartei,
# Basisdemokratische, ÖDP, etc. — Vorperiode-only in 2022). These were not on
# the 2022 ballot; emitting empty rows would be noise.
party_any <- long %>% group_by(party_raw) %>%
  summarise(any_vote = any(!is.na(votes)), .groups = "drop")
keep_parties <- party_any$party_raw[party_any$any_vote]
long <- long %>% filter(party_raw %in% keep_parties)

# integer votes
long$votes <- as.integer(round(long$votes))

setorder(setDT(long), wkr_nr, party_raw)
fwrite(long, out_csv)

# ---------------------------------------------------------------------------
# VALIDATION
# ---------------------------------------------------------------------------
cat("\n==== VALIDATION: Saarland 2022 ====\n")

# (a) per (wkr,stimme): |sum(party votes) - valid_votes|
chk <- long %>% group_by(wkr_nr, stimme) %>%
  summarise(sum_party = sum(votes, na.rm = TRUE),
            valid = first(valid_votes), .groups = "drop") %>%
  mutate(disc = abs(sum_party - valid))
cat("(a) per-(wkr,stimme) sum vs valid_votes:\n")
print(chk)
cat(sprintf("    MAX abs discrepancy = %s over %d groups\n",
            max(chk$disc), nrow(chk)))

# (b) statewide total match vs Land row (Nr 10)
land_party <- land_area$party[keep_parties]
mine <- long %>% group_by(party_raw) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")
cmp <- mine %>%
  mutate(land = as.integer(round(land_party[party_raw])),
         diff = votes - land)
cat("\n(b) statewide total match (sum WK vs Land row):\n")
print(cmp)
fails <- cmp %>% filter(diff != 0)
cat(sprintf("    statewide_total_match = %s; failing parties = %d\n",
            nrow(fails) == 0, nrow(fails)))

# also check turnout aggregates sum to Land
agg <- tibble(
  metric = c("eligible_voters","number_voters","valid_votes","invalid_votes"),
  wk_sum = c(sum(sapply(wk_areas, `[[`, "wbr")),
             sum(sapply(wk_areas, `[[`, "wae")),
             sum(sapply(wk_areas, `[[`, "gue")),
             sum(sapply(wk_areas, `[[`, "ung"))),
  land   = c(land_area$wbr, land_area$wae, land_area$gue, land_area$ung)
) %>% mutate(diff = wk_sum - land)
cat("\n    turnout aggregates WK-sum vs Land:\n")
print(agg)

# (c) Wahlkreis count
cat(sprintf("\n(c) n_wahlkreise = %d (expected 3)\n",
            n_distinct(long$wkr_nr)))

cat(sprintf("\nTotal rows emitted: %d\n", nrow(long)))
cat(sprintf("Distinct party_raw: %d\n", n_distinct(long$party_raw)))
cat(sprintf("Output: %s\n", out_csv))
