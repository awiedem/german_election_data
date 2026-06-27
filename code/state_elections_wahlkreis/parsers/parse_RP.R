# =====================================================================
# Stage-1 cleaning parser: Rheinland-Pfalz (RP) Landtagswahl, Wahlkreis level
# Vote system: erststimme (Wahlkreisstimme) + zweitstimme (Landesstimme)
# Output: long tidy CSV, one row per (Wahlkreis x stimme x party)
# =====================================================================

library(here)
library(tidyverse)
library(readxl)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_RP.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Rheinland-Pfalz")
out_path <- here("data", "state_elections", "processed", "wahlkreis",
                 "RP_ltw_wkr_long.csv")

STATE_ABBR <- "RP"
STATE_NAME <- "Rheinland-Pfalz"

# ---------------------------------------------------------------------
# Per-year configuration. Column indices are 1-based and were determined
# by direct inspection of each workbook's constituency sheet.
#   hdr_row   : row holding the party NAMES (and ungültig/gültig markers)
#   data_row0 : first data row (Landesergebnis total row)
#   col_bez   : Bezirk column (NA if combined id)
#   col_wk    : Wahlkreis-number column (NA if combined id)
#   col_idcomb: combined "<bez> <wk>" id column (2026) ; else NA
#   col_name  : Gebietsname column
#   col_elig  : eligible_voters (Wahlberechtigte/Stimmberechtigte insgesamt)
#   col_voters: number_voters (Wähler insgesamt)
#   wk_*      : Wahlkreisstimmen block:  ungültig col, gültig col,
#               party value cols span [pstart, pend] (every other col = value)
#   ls_*      : Landesstimmen block, same idea
# ---------------------------------------------------------------------
cfg <- list(
  "2001" = list(file="RP_2001_Landtagswahl_Wahlkreis.xlsx", sheet="Bezirke und Wahlkreise",
                date="2001-03-25", hdr_row=6, data_row0=9,
                col_bez=1, col_wk=2, col_idcomb=NA, col_name=9, col_elig=11, col_voters=15,
                wk_ung=18, wk_gue=20, wk_pstart=22, wk_pend=46,
                ls_ung=48, ls_gue=50, ls_pstart=52, ls_pend=70),
  "2006" = list(file="RP_2006_Landtagswahl_Wahlkreis.xlsx", sheet="Bezirke und Wahlkreise",
                date="2006-03-26", hdr_row=7, data_row0=NA,
                col_bez=1, col_wk=2, col_idcomb=NA, col_name=6, col_elig=8, col_voters=12,
                wk_ung=15, wk_gue=17, wk_pstart=19, wk_pend=43,
                ls_ung=45, ls_gue=47, ls_pstart=49, ls_pend=77),
  "2011" = list(file="RP_2011_Landtagswahl_Wahlkreis.xlsx", sheet="Bezirke und Wahlkreise",
                date="2011-03-27", hdr_row=6, data_row0=NA,
                col_bez=1, col_wk=2, col_idcomb=NA, col_name=6, col_elig=8, col_voters=12,
                wk_ung=15, wk_gue=17, wk_pstart=19, wk_pend=45,
                ls_ung=47, ls_gue=49, ls_pstart=51, ls_pend=73),
  "2016" = list(file="RP_2016_Landtagswahl_Wahlkreis.xlsx", sheet="Wahlkreisergebnisse",
                date="2016-03-13", hdr_row=6, data_row0=10,
                col_bez=1, col_wk=2, col_idcomb=NA, col_name=3, col_elig=4, col_voters=8,
                wk_ung=11, wk_gue=13, wk_pstart=15, wk_pend=41,
                ls_ung=43, ls_gue=45, ls_pstart=47, ls_pend=73),
  "2021" = list(file="RP_2021_Landtagswahl_Wahlkreis.xlsx", sheet="Wahlkreisergebnisse",
                date="2021-03-14", hdr_row=6, data_row0=NA,
                col_bez=1, col_wk=2, col_idcomb=NA, col_name=3, col_elig=4, col_voters=8,
                wk_ung=11, wk_gue=13, wk_pstart=15, wk_pend=45,
                ls_ung=47, ls_gue=49, ls_pstart=51, ls_pend=75),
  "2026" = list(file="RP_2026_Landtagswahl_Wahlkreis.xlsx", sheet="LW_2026_WK",
                date="2026-03-22", hdr_row=3, data_row0=6,
                col_bez=NA, col_wk=NA, col_idcomb=1, col_name=3, col_elig=9, col_voters=10,
                wk_ung=13, wk_gue=15, wk_pstart=17, wk_pend=45,
                ls_ung=47, ls_gue=49, ls_pstart=51, ls_pend=73)
)

# helper: numeric coercion that tolerates German text / blanks
as_num <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s", "", x)
  x <- gsub("\\.", "", x)         # thousands separators (rare here; values are numeric)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

# extract party (name, col) pairs in a block given header row vector + span
block_parties <- function(hdr_vec, pstart, pend) {
  out <- list()
  cols <- seq(pstart, pend, by = 2)
  for (cc in cols) {
    nm <- hdr_vec[cc]
    if (is.na(nm)) next
    nm <- gsub("\\s+", " ", trimws(gsub("[\r\n]", " ", nm)))
    if (nm == "" || grepl("^ungültig|^gültig$", nm, ignore.case = TRUE)) next
    out[[length(out)+1]] <- list(party = nm, col = cc)
  }
  out
}

parse_year <- function(yr, cf) {
  d <- read_excel(file.path(raw_dir, cf$file), sheet = cf$sheet,
                  col_names = FALSE, .name_repair = "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  hdr <- as.character(unlist(d[cf$hdr_row, ]))

  wk_p <- block_parties(hdr, cf$wk_pstart, cf$wk_pend)
  ls_p <- block_parties(hdr, cf$ls_pstart, cf$ls_pend)

  # identify the Land total row (for validation) and Wahlkreis rows
  name_col <- as.character(d[[cf$col_name]])

  # row classification
  is_land <- rep(FALSE, nrow(d))
  is_wk   <- rep(FALSE, nrow(d))

  if (is.na(cf$col_idcomb)) {
    bez <- suppressWarnings(as.numeric(as.character(d[[cf$col_bez]])))
    wk  <- suppressWarnings(as.numeric(as.character(d[[cf$col_wk]])))
    # Land total row: identified by the Gebietsname "Landesergebnis"
    # (id columns may be 0 OR blank depending on the year).
    is_land <- !is.na(name_col) & grepl("Landesergebnis", name_col, ignore.case = TRUE)
    # Wahlkreis rows: wk != 0 (Bezirk subtotals have wk==0; Land/blank rows NA)
    is_wk <- !is.na(wk) & wk != 0 & !is.na(bez) & bez != 0
    wkr_nr_vec <- sprintf("%03d", wk)
  } else {
    # 2026: combined "<bez> <wk>" e.g. "1 001"; no Land row in WK sheet.
    # Classify strictly on the id pattern (bezirk space 3-digit wk), NOT the
    # name (some names lack the ", WK" suffix, e.g. "Linz/Rengsdorf").
    idc <- as.character(d[[cf$col_idcomb]])
    idc_trim <- trimws(idc)
    is_wk <- grepl("^[0-9]+\\s+[0-9]{3}$", idc_trim)
    wknum <- ifelse(is_wk, suppressWarnings(as.numeric(sub("^[0-9]+\\s+([0-9]{3})$", "\\1", idc_trim))), NA)
    wkr_nr_vec <- ifelse(is_wk, sprintf("%03d", wknum), NA)
    is_land <- rep(FALSE, nrow(d))  # validated against Landesergebnis sheet below
  }

  build_long <- function(rows_idx, wkr_nr, wkr_name) {
    out <- list()
    for (k in seq_along(rows_idx)) {
      i <- rows_idx[k]
      elig   <- as_num(d[i, cf$col_elig])
      voters <- as_num(d[i, cf$col_voters])
      wk_val <- as_num(d[i, cf$wk_gue]); wk_ung <- as_num(d[i, cf$wk_ung])
      ls_val <- as_num(d[i, cf$ls_gue]); ls_ung <- as_num(d[i, cf$ls_ung])
      for (p in wk_p) {
        out[[length(out)+1]] <- data.frame(
          wkr_nr = wkr_nr[k], wkr_name = wkr_name[k], stimme = "erststimme",
          eligible_voters = elig, number_voters = voters,
          valid_votes = wk_val, invalid_votes = wk_ung,
          party_raw = p$party, votes = as_num(d[i, p$col]),
          stringsAsFactors = FALSE)
      }
      for (p in ls_p) {
        out[[length(out)+1]] <- data.frame(
          wkr_nr = wkr_nr[k], wkr_name = wkr_name[k], stimme = "zweitstimme",
          eligible_voters = elig, number_voters = voters,
          valid_votes = ls_val, invalid_votes = ls_ung,
          party_raw = p$party, votes = as_num(d[i, p$col]),
          stringsAsFactors = FALSE)
      }
    }
    rbindlist(out)
  }

  wk_idx <- which(is_wk)
  wkr_name_vec <- gsub("\\s+", " ", trimws(name_col))
  long <- build_long(wk_idx, wkr_nr_vec[wk_idx], wkr_name_vec[wk_idx])
  long[, `:=`(state_abbr = STATE_ABBR, state = STATE_NAME,
              election_year = as.integer(yr), election_date = cf$date)]

  # ---- statewide validation totals ----
  land_tot <- NULL
  if (any(is_land)) {
    li <- which(is_land)[1]
    rows <- list()
    for (p in wk_p) rows[[length(rows)+1]] <- data.frame(stimme="erststimme", party_raw=p$party, src_total=as_num(d[li,p$col]))
    for (p in ls_p) rows[[length(rows)+1]] <- data.frame(stimme="zweitstimme", party_raw=p$party, src_total=as_num(d[li,p$col]))
    land_tot <- rbindlist(rows)
  } else if (!is.na(cf$col_idcomb)) {
    # 2026: get Land totals from the Landesergebnis sheet
    le <- tryCatch(read_excel(file.path(raw_dir, cf$file), sheet = "Landesergebnis",
                              col_names = FALSE, .name_repair = "minimal"),
                   error = function(e) NULL)
    # not present in this single-sheet workbook -> validate via xlsx vs CSV instead (handled in caller)
    land_tot <- NULL
  }

  list(long = long, land_tot = land_tot, n_wk = length(wk_idx),
       wk_parties = sapply(wk_p, `[[`, "party"),
       ls_parties = sapply(ls_p, `[[`, "party"))
}

# ---- 2026 statewide totals from the all-levels CSV (Land row, level 'LD') ----
# Cross-source validation: the 2026 Wahlkreis xlsx has no Land total row, so we
# validate our per-Wahlkreis xlsx sums against the independent CSV's Land row.
get_2026_land_csv <- function() {
  csv <- file.path(raw_dir, "RP_2026_Landtagswahl_Stimmbezirksebene_alle_Ebenen.csv")
  x <- fread(csv, sep = ";", header = FALSE, skip = 2, encoding = "Latin-1",
             fill = TRUE, quote = "\"")
  hdr <- as.character(unlist(x[1, ]))
  hdr <- gsub("\\s+", " ", trimws(gsub("[\r\n]", " ", hdr)))
  land <- x[as.character(x[[1]]) == "0" & as.character(x[[3]]) == "LD", ]
  if (nrow(land) != 1L) stop("2026 CSV: could not uniquely locate the Land (LD) row")

  # Two party blocks: Wahlkreisstimmen (after 'gültige Wahlkreisstimmen'),
  # Landesstimmen (after 'gültige Landesstimmen'). Party value cols are those
  # whose header is a party name (the following '<name> Prozent' col is skipped).
  is_pct  <- grepl("Prozent", hdr)
  is_gue_wk <- grepl("g.ltige Wahlkreis", hdr)
  is_gue_ls <- grepl("g.ltige Landes",   hdr)
  wk_g <- which(is_gue_wk); wk_g <- wk_g[length(wk_g)]
  ls_g <- which(is_gue_ls); ls_g <- ls_g[length(ls_g)]

  mk <- function(start_col, end_col, stimme) {
    out <- list()
    for (cc in seq(start_col, end_col)) {
      if (is_pct[cc]) next
      nm <- hdr[cc]
      if (is.na(nm) || nm == "" ||
          grepl("g.ltige|ung.ltige|Prozent|Wahlbeteil", nm) ||
          nm %in% c("A","A1","A2","A3","B","B1")) next
      val <- as_num(land[[cc]])
      out[[length(out)+1]] <- data.frame(stimme = stimme, party_raw = nm,
                                         src_total = val, stringsAsFactors = FALSE)
    }
    rbindlist(out)
  }
  wk <- mk(wk_g + 1, ls_g - 1, "erststimme")
  # Landesstimmen block runs from ls_g+1 to end (drop trailing non-party cols)
  ls <- mk(ls_g + 1, length(hdr), "zweitstimme")
  rbindlist(list(wk, ls))
}

# =====================================================================
# RUN
# =====================================================================
all_long <- list()
report <- list()
for (yr in names(cfg)) {
  message("Parsing ", yr, " ...")
  res <- parse_year(yr, cfg[[yr]])
  all_long[[yr]] <- res$long
  report[[yr]] <- res
}

combined <- rbindlist(all_long, use.names = TRUE)

# column order
setcolorder(combined, c("state_abbr","state","election_year","election_date",
                        "wkr_nr","wkr_name","stimme","eligible_voters","number_voters",
                        "valid_votes","invalid_votes","party_raw","votes"))

# =====================================================================
# VALIDATION
# =====================================================================
cat("\n================ VALIDATION ================\n")

# (a) per (year,wkr,stimme): sum(party votes) vs valid_votes
chk <- combined[!is.na(votes), .(sum_party = sum(votes),
                                 valid = valid_votes[1]),
                by = .(election_year, wkr_nr, stimme)]
chk[, disc := abs(sum_party - valid)]
cat("\n(a) Per (wkr,stimme) vote-integrity:\n")
print(chk[, .(n_groups = .N, max_disc = max(disc), mean_disc = round(mean(disc),3)),
          by = election_year])
cat("Overall max abs discrepancy:", max(chk$disc), " across", nrow(chk), "groups\n")

# (b) statewide totals match.
# 2001/2006/2011/2016/2021: against each xlsx's own Landesergebnis row.
# 2026: cross-source against the independent all-levels CSV Land (LD) row.
cat("\n(b) Statewide total match (per party x stimme):\n")
land_totals <- list()
for (yr in names(report)) land_totals[[yr]] <- report[[yr]]$land_tot
land_totals[["2026"]] <- get_2026_land_csv()

for (yr in names(report)) {
  lt <- land_totals[[yr]]
  if (is.null(lt)) { cat(yr, ": NO statewide total available\n"); next }
  mine <- combined[election_year == as.integer(yr),
                   .(my_total = sum(votes, na.rm = TRUE)), by = .(stimme, party_raw)]
  cmp <- merge(lt, mine, by = c("stimme","party_raw"), all = TRUE)
  # treat NA source as 0 and NA mine as 0 (party listed in one block only / no votes)
  cmp[, src0 := fifelse(is.na(src_total), 0, src_total)]
  cmp[, my0  := fifelse(is.na(my_total), 0, my_total)]
  cmp[, diff := abs(src0 - my0)]
  bad <- cmp[diff > 1]
  src_label <- if (yr == "2026") "CSV-LD" else "xlsx-Land"
  cat(sprintf("%s: match=%s  max_diff=%s  n_parties=%d  n_fail=%d  [%s]\n",
              yr, nrow(bad) == 0, max(cmp$diff, na.rm = TRUE), nrow(cmp), nrow(bad), src_label))
  if (nrow(bad) > 0) print(bad[, .(stimme, party_raw, src_total, my_total, diff)])
}

# (c) Wahlkreis count per year
cat("\n(c) Wahlkreis count per year (expect ~52):\n")
print(combined[, .(n_wkr = uniqueN(wkr_nr)), by = election_year])

# =====================================================================
# WRITE
# =====================================================================
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
fwrite(combined, out_path)
cat("\nWrote", nrow(combined), "rows to", out_path, "\n")

# distinct party labels
cat("\nDistinct party_raw labels (", uniqueN(combined$party_raw), "):\n", sep="")
print(sort(unique(combined$party_raw)))
