#### Landtagswahlen at WAHLKREIS (constituency) level — Stage 1: unharmonized ####
## Combines the per-state long intermediates produced by parsers/parse_<ABBR>.R
## into a single unharmonized dataset, mirroring the conventions of the
## municipality-level state pipeline (code/state_elections/01b_state_unharm_raw.R):
##   - central party-name normalisation via normalise_party()
##   - party columns stored as VOTE SHARES (of valid_votes), counts kept in meta
##   - other = residual, cdu_csu = combined Union family, turnout, QA flags
##   - column order: flags, meta, sorted party shares, other, cdu_csu
##
## Geographic unit: Wahlkreis (Stimmkreis in BY; Wahlbereich in HB). Each row is
## one (state, election_year, Wahlkreis, stimme). stimme = erststimme /
## zweitstimme / einzelstimme (BW & SL have a single vote).
##
## Inputs : data/state_elections/processed/wahlkreis/<ABBR>_ltw_wkr_long.csv
## Outputs: data/state_elections/final/ltw_wkr_unharm_long.{csv,rds}  (tidy, COUNTS + shares)
##          data/state_elections/final/ltw_wkr_unharm.{csv,rds}       (wide, SHARES, GERDA-style)
##
## Authors: Vincent Heddesheimer (with Claude Code assistance), June 2026

#### Setup ####
rm(list = ls())
packages <- c("here", "tidyverse", "data.table")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressMessages(library(pkg, character.only = TRUE))
}
here::i_am("code/state_elections_wahlkreis/01_ltw_wkr_unharm.R")

inter_dir <- here("data", "state_elections", "processed", "wahlkreis")
out_dir   <- here("data", "state_elections", "final")

## Reuse the municipality pipeline's party normaliser (verbatim copy) ----------
source(here("code", "state_elections_wahlkreis", "_normalise_party.R"))
normalise_party_v <- function(x) vapply(x, normalise_party, character(1), USE.NAMES = FALSE)

## state_abbr -> 2-digit AGS state code (matches state_unharm "state") ----------
state_code <- c(
  SH = "01", HH = "02", NI = "03", HB = "04", NW = "05", HE = "06",
  RP = "07", BW = "08", BY = "09", SL = "10", BE = "11", BB = "12",
  MV = "13", SN = "14", ST = "15", TH = "16"
)

#### Read + bind all per-state intermediates ####
files <- list.files(inter_dir, pattern = "_ltw_wkr_long\\.csv$", full.names = TRUE)
stopifnot(length(files) > 0)

read_one <- function(f) {
  dt <- fread(
    f, encoding = "UTF-8", na.strings = c("", "NA", "-"),
    colClasses = list(character = c("state_abbr", "state", "election_date",
                                    "wkr_nr", "wkr_name", "stimme", "party_raw"))
  )
  setnames(dt, names(dt)[1], "state_abbr")   # strip any UTF-8 BOM on first col
  dt
}
raw <- rbindlist(lapply(files, read_one), use.names = TRUE, fill = TRUE)
cat(sprintf("Read %d rows from %d state files\n", nrow(raw), length(files)))

#### Clean / type ####
raw[, election_year := as.integer(election_year)]
raw[, election_date := as.Date(election_date)]
raw[, votes := as.numeric(votes)]
for (cc in c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")) {
  raw[[cc]] <- as.numeric(raw[[cc]])
}
raw[, state := state_code[state_abbr]]                       # overwrite with 2-digit code
stopifnot(!any(is.na(raw$state)))
raw[, party := normalise_party_v(party_raw)]

#### Long output: normalised party, counts + share ####
key_cols <- c("state", "state_abbr", "election_year", "election_date",
              "wkr_nr", "wkr_name", "stimme",
              "eligible_voters", "number_voters", "valid_votes", "invalid_votes")
long <- raw[, .(votes = sum(votes, na.rm = TRUE),
                votes_na = all(is.na(votes))),       # track genuinely-absent (no candidate)
            by = c(key_cols, "party")]
long[votes_na == TRUE, votes := NA_real_][, votes_na := NULL]
long[, vote_share := ifelse(!is.na(valid_votes) & valid_votes > 0, votes / valid_votes, NA_real_)]
long[, turnout := ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                         number_voters / eligible_voters, NA_real_)]
setcolorder(long, c(key_cols, "turnout", "party", "votes", "vote_share"))
setorder(long, state, election_year, stimme, wkr_nr, party)

#### Wide output: party SHARES, GERDA-style ####
meta_cols <- c("state", "election_year", "election_date", "wkr_nr", "wkr_name",
               "stimme", "eligible_voters", "number_voters", "valid_votes",
               "invalid_votes", "turnout")
# Explicit id formula (meta only) — one row per (state,year,wkr,stimme).
# Using "..." would wrongly fold `votes`/`vote_share` into the row key and explode rows.
id_form <- as.formula(paste(paste(meta_cols, collapse = " + "), "~ party"))
wide <- dcast(long, id_form, value.var = "vote_share", fun.aggregate = sum, fill = NA)
pcols <- setdiff(names(wide), meta_cols)

# other = residual share (≈0 since parsers captured all parties & Σ=valid); cdu_csu = Union family
wide[, other := pmax(1 - rowSums(.SD, na.rm = TRUE), 0), .SDcols = pcols]
has_cdu <- "cdu" %in% pcols; has_csu <- "csu" %in% pcols
wide[, cdu_csu := {
  a <- if (has_cdu) get("cdu") else 0
  b <- if (has_csu) get("csu") else 0
  s <- ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b)
  ifelse(s == 0, NA_real_, s)
}]

#### QA flags ####
wide[, flag_no_valid_votes := as.integer(is.na(valid_votes) | valid_votes == 0)]
wide[, flag_naive_turnout_above_1 := as.integer(!is.na(turnout) & turnout > 1)]
# turnout recode: drop only physically impossible values, keep mild >1 (Briefwahl)
wide[, turnout := ifelse(!is.na(turnout) & turnout > 1.5, NA_real_, turnout)]

#### Column ordering: flags, meta, sorted party shares, other, cdu_csu ####
front_flags <- c("flag_no_valid_votes", "flag_naive_turnout_above_1")
final_order <- c(front_flags, meta_cols, sort(pcols), "other", "cdu_csu")
final_order <- final_order[final_order %in% names(wide)]
setcolorder(wide, final_order)
wide <- wide[, ..final_order]
setorder(wide, state, election_year, stimme, wkr_nr)

#### Write ####
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fwrite(long, file.path(out_dir, "ltw_wkr_unharm_long.csv"))
saveRDS(long, file.path(out_dir, "ltw_wkr_unharm_long.rds"))
fwrite(wide, file.path(out_dir, "ltw_wkr_unharm.csv"))
saveRDS(wide, file.path(out_dir, "ltw_wkr_unharm.rds"))

#### Console summary ####
cat("\n==== ltw_wkr_unharm built ====\n")
cat(sprintf("LONG: %d rows | WIDE: %d rows (one per state x year x wahlkreis x stimme)\n",
            nrow(long), nrow(wide)))
cat(sprintf("States: %d | Years: %d-%d | Party columns: %d\n",
            uniqueN(wide$state), min(wide$election_year), max(wide$election_year), length(pcols)))
cat("Rows per stimme:\n"); print(wide[, .N, by = stimme])
cat("Rows per state x stimme:\n"); print(dcast(wide, state ~ stimme, value.var = "wkr_nr", fun.aggregate = length))
cat(sprintf("\nflag_no_valid_votes: %d | flag_naive_turnout_above_1: %d\n",
            sum(wide$flag_no_valid_votes), sum(wide$flag_naive_turnout_above_1)))
# integrity: share of party columns summing to ~1 (excluding all-NA rows)
ssum <- rowSums(as.matrix(wide[, ..pcols]), na.rm = TRUE)
cat(sprintf("Wide rows where Σ party shares in [0.99,1.01]: %d / %d\n",
            sum(ssum > 0.99 & ssum < 1.01), nrow(wide)))
