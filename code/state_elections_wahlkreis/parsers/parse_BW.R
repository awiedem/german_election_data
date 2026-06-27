# =============================================================================
# parse_BW.R  —  Stage-1 cleaning parser, Baden-Wuerttemberg Landtagswahl
#                at CONSTITUENCY (Wahlkreis) level.
#
# Machine-readable sources only (PDF/TIF scans excluded -> future OCR stage):
#   2016 : BW_2016_Landtagswahl_Wahlkreis_BVII2.xlsx   (tiled "Tabelle1" report)
#   2021 : BW_2021_Landtagswahl_Wahlkreis_BVII2.xlsx   (tiled "Tabelle1" report)
#   2026 : BW_2026_Landtagswahl_alle_Ebenen_inkl_Wahlkreis.csv (all-levels open data)
#
# Vote system:
#   2016 & 2021 -> single vote  -> stimme = "einzelstimme"
#   2026        -> two-vote system introduced -> stimme = "erststimme" + "zweitstimme"
#
# Output: long, tidy CSV, one row per (Wahlkreis x stimme x party).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)
library(readxl)

here::i_am("code/state_elections_wahlkreis/parsers/parse_BW.R")

RAW_DIR <- here("data", "state_elections", "raw", "Landtagswahlen_Wahlkreis",
                "Baden-Württemberg")
OUT_DIR <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_COLS <- c("state_abbr","state","election_year","election_date","wkr_nr",
              "wkr_name","stimme","eligible_voters","number_voters","valid_votes",
              "invalid_votes","party_raw","votes")

# --- helper: clean German integer strings ("1.234", spaces, "x", ".") -> numeric
to_int <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("x","X","-","–","", ".", "/")] <- NA   # 'x' = party did not stand
  x <- gsub("[.  ]", "", x)                     # thousands sep / nbsp / space
  suppressWarnings(as.numeric(x))
}

# normalise a Wahlkreis header cell like "Wahlkreis 01    Stuttgart I"
# -> list(nr="01", name="Stuttgart I")
parse_wk_header <- function(s) {
  m <- regmatches(s, regexec("Wahlkreis\\s+(\\d+)\\s+(.*)$", s))[[1]]
  if (length(m) == 3) {
    return(list(nr = sprintf("%02d", as.integer(m[2])), name = trimws(m[3])))
  }
  NULL
}

# =============================================================================
# (A) GENERIC PARSER for the tiled "Tabelle1" XLSX report (2016, 2021)
#     Layout: each "Tabelle1_*" sheet holds vertical blocks; in each block the
#     value column carries a header cell "Wahlkreis NN  <name>" (or "Land ...");
#     the row labels live in column 1, aligned by row. "x" = party absent.
# =============================================================================
parse_tiled_xlsx <- function(file, year, edate) {
  sh  <- excel_sheets(file)
  t1  <- grep("^Tabelle1_", sh, value = TRUE)
  stopifnot(length(t1) > 0)

  rows_out   <- list()        # per-Wahlkreis long rows
  land_total <- NULL          # statewide validation row (party -> votes)
  land_meta  <- NULL

  for (s in t1) {
    d <- as.data.frame(read_excel(file, sheet = s, col_names = FALSE,
                                  .name_repair = "minimal"))
    nC <- ncol(d); nR <- nrow(d)
    lbl_col <- as.character(d[[1]])

    # find every header cell (Wahlkreis NN ... or Land ...) anywhere in the sheet
    for (i in seq_len(nR)) for (j in 2:nC) {
      v <- as.character(d[i, j]); if (is.na(v)) next
      is_land <- grepl("^Land Baden", v)
      is_wk   <- grepl("^Wahlkreis\\s+\\d+", v)
      if (!is_land && !is_wk) next

      vcol <- as.numeric(d[[j]])          # value column for this unit
      # block label rows are in col1; find them by matching known meta + party
      # within the window from this header down to next header / block end.
      # Determine block extent: from header row+1 to the row before the next
      # 'Gegenstand'/'Wahlkreis'/'Land' marker in col1 OR end of sheet.
      lab <- lbl_col

      # map: for each labelled row, take value from vcol at same row index
      grab <- function(pattern) {
        idx <- which(grepl(pattern, lab))
        idx <- idx[idx > i & idx < i + 40]   # within this block window
        if (length(idx) == 0) return(NA_real_)
        vcol[idx[1]]
      }

      eligible <- grab("^Wahlberechtigte")
      voters   <- grab("^Wähler")                 # Wähler / Wähler/-innen
      invalid  <- grab("^Ungültige Stimmen")
      valid    <- grab("^Gültige Stimmen")

      # party rows: everything between 'davon für' and the block end, excluding
      # the 'Sonstige (nur YYYY)' comparison-only row.
      dav_idx  <- which(grepl("^davon für", lab) & seq_along(lab) > i &
                          seq_along(lab) < i + 40)
      if (length(dav_idx) == 0) next
      pstart <- dav_idx[1] + 1
      # party block ends at next NA-run / 'Gegenstand'/'Sonstige (nur'
      pend <- pstart
      while (pend <= nR) {
        lv <- lab[pend]
        if (is.na(lv)) break
        if (grepl("^Sonstige \\(nur", lv)) break
        if (grepl("^Gegenstand der", lv)) break
        pend <- pend + 1
      }
      pend <- pend - 1
      if (pend < pstart) next

      parties <- lab[pstart:pend]
      pvals   <- vcol[pstart:pend]
      keep    <- !is.na(parties) & parties != "" & !is.na(pvals)
      parties <- parties[keep]; pvals <- pvals[keep]

      if (is_land) {
        land_total <- data.frame(party_raw = parties, votes = pvals,
                                 stringsAsFactors = FALSE)
        land_meta  <- list(eligible = eligible, voters = voters,
                           invalid = invalid, valid = valid)
        next
      }

      hdr <- parse_wk_header(v); if (is.null(hdr)) next
      rows_out[[length(rows_out) + 1]] <- data.frame(
        state_abbr = "BW", state = "Baden-Württemberg",
        election_year = year, election_date = edate,
        wkr_nr = hdr$nr, wkr_name = hdr$name, stimme = "einzelstimme",
        eligible_voters = eligible, number_voters = voters,
        valid_votes = valid, invalid_votes = invalid,
        party_raw = parties, votes = pvals,
        stringsAsFactors = FALSE)
    }
  }

  long <- rbindlist(rows_out, use.names = TRUE)
  attr(long, "land_total") <- land_total
  attr(long, "land_meta")  <- land_meta
  long
}

# =============================================================================
# (B) 2026 all-levels open-data CSV  (Erststimme D-cols + Zweitstimme F-cols)
# =============================================================================
# party maps from BW_2026_Hinweise-Datensatzbeschreibung.pdf
D_MAP <- c(D1="GRÜNE", D2="CDU", D3="SPD", D4="FDP", D5="AfD", D6="Die Linke",
           D7="FREIE WÄHLER", D8="Die PARTEI", D9="dieBasis", D11="ÖDP",
           D12="Volt", D13="Bündnis C", D16="BSW", D17="Die Gerechtigkeitspartei",
           D20="Tierschutzpartei", D21="Werteunion",
           D22="Anderer Kreiswahlvorschlag (D22)")
F_MAP <- c(F1="GRÜNE", F2="CDU", F3="SPD", F4="FDP", F5="AfD", F6="Die Linke",
           F7="FREIE WÄHLER", F8="Die PARTEI", F9="dieBasis", F10="KlimalisteBW",
           F11="ÖDP", F12="Volt", F13="Bündnis C", F14="PDH",
           F15="Verjüngungsforschung", F16="BSW", F17="Die Gerechtigkeitspartei",
           F18="PDR", F19="PdF", F20="Tierschutzpartei", F21="Werteunion")

parse_2026_csv <- function(file, year, edate) {
  d <- fread(file, sep = ";", encoding = "UTF-8", colClasses = "character")

  build <- function(rows, stimme, valid_col, invalid_col, code_map) {
    out <- list()
    for (r in seq_len(nrow(rows))) {
      row <- rows[r]
      nm  <- row$Gebietsname                       # "01 - Stuttgart I"
      nr  <- sub("\\s*-.*$", "", nm); nr <- trimws(nr)   # "01"
      name <- trimws(sub("^[0-9]+\\s*-\\s*", "", nm))    # "Stuttgart I"
      for (code in names(code_map)) {
        if (!code %in% names(row)) next
        out[[length(out)+1]] <- data.frame(
          state_abbr = "BW", state = "Baden-Württemberg",
          election_year = year, election_date = edate,
          wkr_nr = nr, wkr_name = name, stimme = stimme,
          eligible_voters = to_int(row[["Wahlberechtigte gesamt (A)"]]),
          number_voters   = to_int(row[["Waehler gesamt (B)"]]),
          valid_votes     = to_int(row[[valid_col]]),
          invalid_votes   = to_int(row[[invalid_col]]),
          party_raw = unname(code_map[code]),
          votes = to_int(row[[code]]),
          stringsAsFactors = FALSE)
      }
    }
    rbindlist(out, use.names = TRUE)
  }

  wk <- d[Gebietsart == "WAHLKREIS"]
  erst  <- build(wk, "erststimme",  "Erststimmen gueltige (D)",
                 "Erststimmen ungueltige (C)", D_MAP)
  zweit <- build(wk, "zweitstimme", "Zweitstimmen gueltige (F)",
                 "Zweitstimmen ungueltige (E)", F_MAP)
  long <- rbindlist(list(erst, zweit), use.names = TRUE)
  # drop NA votes (party not present in that Wahlkreis)
  long <- long[!is.na(votes)]

  # statewide LAND row for validation (per stimme)
  land <- d[Gebietsart == "LAND"]
  land_e <- data.frame(party_raw = unname(D_MAP[names(D_MAP)]),
                       votes = sapply(names(D_MAP), function(c) to_int(land[[c]])),
                       stimme = "erststimme", stringsAsFactors = FALSE)
  land_f <- data.frame(party_raw = unname(F_MAP[names(F_MAP)]),
                       votes = sapply(names(F_MAP), function(c) to_int(land[[c]])),
                       stimme = "zweitstimme", stringsAsFactors = FALSE)
  attr(long, "land_total") <- rbind(land_e, land_f)
  long
}

# =============================================================================
# RUN
# =============================================================================
f16 <- file.path(RAW_DIR, "BW_2016_Landtagswahl_Wahlkreis_BVII2.xlsx")
f21 <- file.path(RAW_DIR, "BW_2021_Landtagswahl_Wahlkreis_BVII2.xlsx")
f26 <- file.path(RAW_DIR, "BW_2026_Landtagswahl_alle_Ebenen_inkl_Wahlkreis.csv")

L16 <- parse_tiled_xlsx(f16, 2016L, "2016-03-13")
L21 <- parse_tiled_xlsx(f21, 2021L, "2021-03-14")
L26 <- parse_2026_csv(f26, 2026L, "2026-03-08")

# ---- VALIDATION -------------------------------------------------------------
validate <- function(long, label, has_stimme = FALSE) {
  cat("\n==== VALIDATION", label, "====\n")
  dt <- as.data.table(long)

  # (a) per (wkr,stimme): sum(party votes) vs valid_votes
  agg <- dt[!is.na(votes), .(sum_votes = sum(votes),
                             vv = valid_votes[1]),
            by = .(wkr_nr, stimme)]
  agg[, disc := abs(sum_votes - vv)]
  cat("(a) per-(wkr,stimme) groups checked:", nrow(agg),
      " MAX |sum-valid| =", max(agg$disc, na.rm = TRUE), "\n")
  if (max(agg$disc, na.rm = TRUE) > 0)
    print(agg[disc > 0][order(-disc)][1:min(10,.N)])

  # (c) Wahlkreis count
  cat("(c) n Wahlkreise:", length(unique(dt$wkr_nr)), "\n")

  # (b) statewide totals
  land <- attr(long, "land_total")
  if (!is.null(land)) {
    if ("stimme" %in% names(land)) {
      mine <- dt[, .(votes = sum(votes, na.rm = TRUE)),
                 by = .(party_raw, stimme)]
      cmp <- merge(mine, as.data.table(land),
                   by = c("party_raw","stimme"), all = TRUE,
                   suffixes = c("_mine","_land"))
    } else {
      mine <- dt[, .(votes = sum(votes, na.rm = TRUE)), by = .(party_raw)]
      cmp <- merge(mine, as.data.table(land), by = "party_raw", all = TRUE,
                   suffixes = c("_mine","_land"))
    }
    cmp[is.na(votes_mine), votes_mine := 0]
    cmp[is.na(votes_land), votes_land := 0]
    cmp[, diff := abs(votes_mine - votes_land)]
    cat("(b) statewide total check: MAX |diff| =", max(cmp$diff), "\n")
    fails <- cmp[diff > 0]
    if (nrow(fails) > 0) { cat("    MISMATCH parties:\n"); print(fails) }
    cat("(b) statewide match:", all(cmp$diff == 0), "\n")
  }
  invisible(NULL)
}

validate(L16, "2016")
validate(L21, "2021")
validate(L26, "2026")

# ---- COMBINE & WRITE --------------------------------------------------------
final <- rbindlist(list(
  as.data.table(L16)[, ..OUT_COLS],
  as.data.table(L21)[, ..OUT_COLS],
  as.data.table(L26)[, ..OUT_COLS]
), use.names = TRUE)

# integer-cast counts (keep NA)
for (cc in c("eligible_voters","number_voters","valid_votes","invalid_votes","votes"))
  final[[cc]] <- as.integer(round(final[[cc]]))

final[, election_year := as.integer(election_year)]
setcolorder(final, OUT_COLS)
setorder(final, election_year, wkr_nr, stimme, party_raw)

outfile <- file.path(OUT_DIR, "BW_ltw_wkr_long.csv")
fwrite(final, outfile)
cat("\nWROTE", nrow(final), "rows ->", outfile, "\n")
cat("distinct party_raw:\n"); print(sort(unique(final$party_raw)))
