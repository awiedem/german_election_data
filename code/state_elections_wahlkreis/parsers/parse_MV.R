# =============================================================================
# parse_MV.R  --  Stage-1 cleaning parser: Mecklenburg-Vorpommern Landtagswahl
#                 results at the CONSTITUENCY (Wahlkreis) level.
#
# Vote system: Erststimme (Wahlkreis/direct) + Zweitstimme (Landesliste).
# Output: long, tidy CSV (one row per Wahlkreis x stimme x party).
#
# Source files (LAIV-MV), all machine-readable .xls / .xlsx:
#   * 1994/1998/2002/2006/2011: "... nach Wahlbezirken" XLS  (precinct level;
#     first column = Wahlkreis number) -> aggregated up to the 36 Wahlkreise.
#     Cross-validated against the matching "... nach Gemeinden" XLS statewide sum.
#   * 2016/2021: official Statistischer Bericht XLSX -> per-Wahlkreis sheets
#     "Wahlkreisbew.-Erststimmen" / "Landeslisten-Zweitstimmen"; the trailing
#     "Mecklenburg-Vorpommern" row is the statewide total (used for validation).
#   * 1990: only a "nach Gemeinden" XLS exists (no Wahlbezirk/Wahlkreis file on
#     LAIV) -> NOT constituency-resolvable -> EXCLUDED.
#   * .pdf / .tif scans -> EXCLUDED (deferred to OCR stage).
#
# The old XLS files all carry a column LEGEND (Satzbeschreibung) row with codes
#   A1/A2/A3, A (eligible), B (voters), B1, C (erst invalid), D (erst valid),
#   D1.. (erst parties), E (zweit invalid), F (zweit valid), F1.. (zweit parties).
# We map columns by these codes (robust to year-to-year column drift) and read
# the party name for each Dn/Fn column from the header cells above the legend.
# =============================================================================

library(here)
library(tidyverse)
library(data.table)
library(readxl)

here::i_am("code/state_elections_wahlkreis/parsers/parse_MV.R")

RAW_DIR <- here("data", "state_elections", "raw", "Landtagswahlen_Wahlkreis",
                "Mecklenburg-Vorpommern")
OUT_CSV <- here("data", "state_elections", "processed", "wahlkreis",
                "MV_ltw_wkr_long.csv")

STATE      <- "Mecklenburg-Vorpommern"
STATE_ABBR <- "MV"

ELECTION_DATES <- c(
  "1990" = "1990-10-14", "1994" = "1994-10-16", "1998" = "1998-09-27",
  "2002" = "2002-09-22", "2006" = "2006-09-17", "2011" = "2011-09-04",
  "2016" = "2016-09-04", "2021" = "2021-09-26"
)

# ---- helpers ---------------------------------------------------------------

to_int <- function(x) {
  # "x" / "-" / "." mean "party not on ballot here" or "no value" -> NA
  x <- trimws(as.character(x))
  x[x %in% c("x", "X", "-", ".", "", "NA")] <- NA
  x <- gsub("[^0-9-]", "", x)            # strip thousands separators / spaces
  suppressWarnings(as.integer(x))
}

clean_name <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x))
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# Party labels in the modern XLSX are word-wrapped INSIDE one cell as
# "<frag>-\n<frag>" (e.g. "Sons-\ntige", "PIRA-\nTEN"). Join those soft-wrap
# hyphens so a party is one label across years. Genuine hyphens in party names
# (e.g. "UNAB-HÄNGIGE") are written "-\n" too in the source, so for the affected
# labels we standardise to the de-hyphenated form -- central normalisation later
# maps these anyway; the key property here is that one party = one party_raw.
clean_party <- function(x) {
  x <- as.character(x)
  x <- gsub("-[\r\n]+\\s*", "", x)   # hyphen + line break -> joined
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("-\\s+", "", x)          # hyphen + (wrap) space -> joined
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# Find the legend row (the one carrying "A" eligible + "D1" first erst party).
find_legend_row <- function(d) {
  for (i in seq_len(nrow(d))) {
    r <- as.character(unlist(d[i, ]))
    if (any(r == "D1", na.rm = TRUE) &&
        any(r %in% c("A", "A1"), na.rm = TRUE) &&
        any(r == "F1", na.rm = TRUE)) return(i)
  }
  stop("legend row not found")
}

# For a given column index, read the FULL party name from the header cells
# above the legend row. Party labels are sometimes wrapped across two rows
# (e.g. "NATUR" / "GESETZ", "Norddt." / "Bauern", "Einzel" / "bewerber") -- we
# concatenate, top-to-bottom, every header cell in that column that is neither a
# legend code (A/B1/D3/...) nor a "(un)gültig" caption.
party_name_for_col <- function(d, col, legrow) {
  parts <- character(0)
  for (i in max(1, legrow - 4):(legrow - 1)) {
    v <- clean_name(d[[col]][i])
    if (!is.na(v) && nzchar(v) &&
        !grepl("^(A|B|C|D|E|F)[0-9]*$", v) &&
        !grepl("^(un-?\\s*)?g.ltig", v, ignore.case = TRUE) &&
        !grepl("^Von den", v, ignore.case = TRUE) &&
        !grepl("^Wahl|^Stimmen$|^insgesamt$|^Nummer$|^name$|^nummer$", v, ignore.case = TRUE)) {
      parts <- c(parts, v)
    }
  }
  if (length(parts) == 0) return(NA_character_)
  trimws(paste(parts, collapse = " "))
}

# -------------------------------------------------------------------------
# READER B: old "nach Wahlbezirken" XLS -> aggregate precincts to Wahlkreis
# -------------------------------------------------------------------------
read_wahlbezirke <- function(file, sheet, year) {
  d <- read_excel(file, sheet = sheet, col_names = FALSE,
                  .name_repair = "minimal")
  legrow <- find_legend_row(d)
  codes  <- as.character(unlist(d[legrow, ]))

  col_A <- which(codes == "A")[1]      # eligible voters (total)
  col_B <- which(codes == "B")[1]      # number of voters
  col_C <- which(codes == "C")[1]      # erst invalid
  col_D <- which(codes == "D")[1]      # erst valid
  col_E <- which(codes == "E")[1]      # zweit invalid
  col_F <- which(codes == "F")[1]      # zweit valid
  erst_cols  <- which(grepl("^D[0-9]+$", codes))
  zweit_cols <- which(grepl("^F[0-9]+$", codes))

  if (any(is.na(c(col_A, col_B, col_C, col_D, col_E, col_F))) ||
      length(erst_cols) == 0 || length(zweit_cols) == 0)
    stop(sprintf("[%s] legend code mapping incomplete", year))

  # Wahlkreis number is the first column. Data rows = those with a numeric WK
  # number AND an 8-digit AGS-like value in col 2 (guards against header/footer).
  wk_raw  <- suppressWarnings(as.integer(as.character(unlist(d[[1]]))))
  is_data <- !is.na(wk_raw) & wk_raw >= 1 & wk_raw <= 36

  body <- d[is_data, , drop = FALSE]
  wk   <- as.integer(as.character(unlist(body[[1]])))

  # numeric matrices
  num <- function(cols) {
    m <- sapply(cols, function(j) to_int(body[[j]]))
    matrix(m, nrow = nrow(body))
  }
  meta <- data.table(
    wkr_nr_int      = wk,
    eligible_voters = to_int(body[[col_A]]),
    number_voters   = to_int(body[[col_B]]),
    erst_invalid    = to_int(body[[col_C]]),
    erst_valid      = to_int(body[[col_D]]),
    zweit_invalid   = to_int(body[[col_E]]),
    zweit_valid     = to_int(body[[col_F]])
  )

  agg_meta <- meta[, .(
    eligible_voters = sum(eligible_voters, na.rm = TRUE),
    number_voters   = sum(number_voters,   na.rm = TRUE),
    erst_invalid    = sum(erst_invalid,    na.rm = TRUE),
    erst_valid      = sum(erst_valid,      na.rm = TRUE),
    zweit_invalid   = sum(zweit_invalid,   na.rm = TRUE),
    zweit_valid     = sum(zweit_valid,     na.rm = TRUE)
  ), by = wkr_nr_int]

  build_party_long <- function(cols, stimme) {
    pnames <- vapply(cols, party_name_for_col, character(1), d = d, legrow = legrow)
    mat    <- num(cols)
    colnames(mat) <- pnames
    dt <- as.data.table(mat)
    dt[, wkr_nr_int := wk]
    long <- melt(dt, id.vars = "wkr_nr_int",
                 variable.name = "party_raw", value.name = "votes")
    long[, party_raw := as.character(party_raw)]
    long <- long[, .(votes = sum(votes, na.rm = TRUE)),
                 by = .(wkr_nr_int, party_raw)]
    long[, stimme := stimme]
    long
  }

  parties <- rbindlist(list(
    build_party_long(erst_cols,  "erststimme"),
    build_party_long(zweit_cols, "zweitstimme")
  ))

  list(meta = agg_meta, parties = parties, year = year,
       erst_codes = codes[erst_cols], zweit_codes = codes[zweit_cols])
}

# -------------------------------------------------------------------------
# Statewide cross-check from the matching "nach Gemeinden" XLS
# (independent file & aggregation level; same legend codes).
# Returns per-(party_raw, stimme) statewide totals.
# -------------------------------------------------------------------------
read_gemeinden_statewide <- function(file, sheet, year) {
  d <- read_excel(file, sheet = sheet, col_names = FALSE,
                  .name_repair = "minimal")
  legrow <- find_legend_row(d)
  codes  <- as.character(unlist(d[legrow, ]))
  col_A <- which(codes == "A")[1]
  erst_cols  <- which(grepl("^D[0-9]+$", codes))
  zweit_cols <- which(grepl("^F[0-9]+$", codes))
  col_Df <- which(codes == "D")[1]
  col_Ff <- which(codes == "F")[1]

  # data rows: have an 8-digit AGS in some early column. AGS column = first
  # column whose body values are mostly 8-digit numbers.
  ncol_d <- ncol(d)
  ags_col <- NA
  for (j in 1:min(5, ncol_d)) {
    v <- as.character(unlist(d[[j]]))
    if (mean(grepl("^13[0-9]{6}$", v), na.rm = TRUE) > 0.5) { ags_col <- j; break }
  }
  if (is.na(ags_col)) stop(sprintf("[%s gem] AGS column not found", year))
  v <- as.character(unlist(d[[ags_col]]))
  is_data <- grepl("^13[0-9]{6}$", v)
  body <- d[is_data, , drop = FALSE]

  sumcol <- function(j) sum(to_int(body[[j]]), na.rm = TRUE)
  erst <- data.table(party_raw = vapply(erst_cols, party_name_for_col,
                                        character(1), d = d, legrow = legrow),
                     votes = vapply(erst_cols, sumcol, numeric(1)),
                     stimme = "erststimme")
  zweit <- data.table(party_raw = vapply(zweit_cols, party_name_for_col,
                                         character(1), d = d, legrow = legrow),
                      votes = vapply(zweit_cols, sumcol, numeric(1)),
                      stimme = "zweitstimme")
  list(parties = rbindlist(list(erst, zweit)),
       erst_valid  = sumcol(col_Df),
       zweit_valid = sumcol(col_Ff))
}

# -------------------------------------------------------------------------
# AUTHORITATIVE statewide reference series (1990-2021) from the 2021 report's
# sheets "2.2 Erststimmen 1990-2021" / "2.3 Zweitstimmen 1990-2021".
# Gives, per year: eligible / voters / invalid / valid + the MAJOR parties
# (SPD, AfD, CDU, DIE LINKE, GRÜNE, FDP, NPD) and a "Sonstige" residual.
# This is the correctness anchor for the statewide check (the per-year
# "nach Gemeinden" XLS files are INCOMPLETE for 1994/1998/2002 -- they omit
# part of the Briefwahl -- so they cannot serve as the statewide reference).
# -------------------------------------------------------------------------
read_reference_series <- function(stimme) {
  sheet <- if (stimme == "erststimme") "2.2 Erststimmen 1990-2021"
           else "2.3 Zweitstimmen 1990-2021"
  d <- read_excel(file.path(RAW_DIR, "MV_2021_Landtagswahl_Wahlkreis.xlsx"),
                  sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  # header row with "Jahr"
  hdr <- which(sapply(seq_len(nrow(d)),
                      function(i) any(clean_name(unlist(d[i, ])) == "Jahr")))[1]
  r4 <- clean_name(unlist(d[hdr, ]))
  r6 <- clean_name(unlist(d[hdr + 2, ]))    # "absolut"/"%" sub-header
  # major-party columns = those whose r4 names a party and r6 == "absolut"
  party_cols <- which(grepl("absolut", r6, ignore.case = TRUE) &
                      grepl("SPD|AfD|CDU|LINKE|GR.NE|FDP|NPD|Sonstige", r4))
  pnames <- gsub("[0-9)]+$", "", r4[party_cols]) |> trimws()
  col_jahr  <- which(r4 == "Jahr")[1]
  col_elig  <- which(grepl("absolut", r6) & grepl("berecht", r4, ignore.case=TRUE))[1]
  col_voter <- which(grepl("W.hler", r4))[1]
  col_inv   <- which(grepl("absolut", r6) & grepl("Ung.ltig", r4, ignore.case=TRUE))[1]
  col_valid <- which(grepl("absolut", r6) & grepl("^G.ltig", r4, ignore.case=TRUE) &
                     !grepl("Ung.ltig", r4, ignore.case=TRUE))[1]

  jahr_raw <- as.character(unlist(d[[col_jahr]]))
  yr <- suppressWarnings(as.integer(gsub("[^0-9].*$", "", jahr_raw)))
  rows <- which(!is.na(yr))
  out <- list()
  for (i in rows) {
    y <- as.character(yr[i])
    parties <- data.table(party_raw = pnames,
                          votes_tot = to_int(unlist(d[i, party_cols])),
                          stimme = stimme)
    out[[y]] <- list(
      eligible = to_int(d[[col_elig]][i]),
      voters   = to_int(d[[col_voter]][i]),
      invalid  = to_int(d[[col_inv]][i]),
      valid    = to_int(d[[col_valid]][i]),
      parties  = parties
    )
  }
  out
}

# map a raw party label to a MAJOR-party reference key (or NA if minor)
major_key <- function(x) {
  k <- norm_party_local(x)
  dplyr::case_when(
    k == "SPD" ~ "SPD",
    k == "AFD" ~ "AFD",
    k == "CDU" ~ "CDU",
    grepl("^PDS$|^DIELINKE", k) ~ "DIELINKE",
    grepl("^GR.NE$|GRUENE", k) ~ "GRUENE",
    grepl("^FDP$|^FDP", k) | k == "FDP" ~ "FDP",
    k == "NPD" ~ "NPD",
    TRUE ~ NA_character_
  )
}
norm_party_local <- function(x) {
  k <- toupper(gsub("[^A-Za-z0-9ÄÖÜäöü]", "", clean_name(x)))
  k <- gsub("Ä","AE",k); k <- gsub("Ö","OE",k); k <- gsub("Ü","UE",k)
  k
}

# Compare my per-Wahlkreis sums to the authoritative reference for one year.
# Checks: valid (per stimme), eligible, voters, each major party, and that the
# sum of my MINOR parties equals the reference "Sonstige".
check_against_reference <- function(long_out, year, ref_e, ref_z) {
  tol <- 5
  fails <- list()
  ref_by <- list(erststimme = ref_e[[as.character(year)]],
                 zweitstimme = ref_z[[as.character(year)]])
  maxd <- 0
  for (st in c("erststimme", "zweitstimme")) {
    ref <- ref_by[[st]]
    if (is.null(ref)) { fails[[length(fails)+1]] <- paste(st, "no reference"); next }
    sub <- long_out[stimme == st]
    # valid / eligible / voters
    my_valid <- unique(sub[, .(wkr_nr, valid_votes)])[, sum(valid_votes)]
    my_elig  <- unique(sub[, .(wkr_nr, eligible_voters)])[, sum(eligible_voters)]
    my_vot   <- unique(sub[, .(wkr_nr, number_voters)])[, sum(number_voters)]
    for (nm in c("valid","eligible","voters")) {
      myv <- get(paste0("my_", c(valid="valid", eligible="elig", voters="vot")[nm]))
      rv  <- ref[[nm]]
      if (!is.na(rv)) { d <- abs(myv - rv); maxd <- max(maxd, d)
        if (d > tol) fails[[length(fails)+1]] <- sprintf("%s %s mine=%d ref=%d", st, nm, myv, rv) }
    }
    # major parties
    sub2 <- copy(sub); sub2[, mkey := major_key(party_raw)]
    refp <- copy(ref$parties)
    refp[, rkey := norm_party_local(party_raw)]
    for (cur in c("SPD","AFD","CDU","DIELINKE","GRUENE","FDP","NPD")) {
      myv <- sub2[mkey == cur, sum(votes, na.rm = TRUE)]
      rrow <- refp[rkey == cur]
      if (nrow(rrow) == 0) next
      rv <- rrow$votes_tot[1]
      if (is.na(rv)) next
      d <- abs(myv - rv); maxd <- max(maxd, d)
      if (d > tol) fails[[length(fails)+1]] <- sprintf("%s %s mine=%d ref=%d", st, cur, myv, rv)
    }
    # minor parties (mkey == NA) must equal reference Sonstige
    my_minor <- sub2[is.na(mkey), sum(votes, na.rm = TRUE)]
    son <- refp[grepl("Sonstige", party_raw, ignore.case = TRUE)]
    if (nrow(son) && !is.na(son$votes_tot[1])) {
      rv <- son$votes_tot[1]; d <- abs(my_minor - rv); maxd <- max(maxd, d)
      if (d > tol) fails[[length(fails)+1]] <- sprintf("%s Sonstige(minor) mine=%d ref=%d", st, my_minor, rv)
    }
  }
  list(match = length(fails) == 0, max_disc = maxd, fails = fails)
}

# -------------------------------------------------------------------------
# READER A: modern XLSX (2016/2021) per-Wahlkreis sheets
# -------------------------------------------------------------------------
read_modern <- function(file, erst_sheet, zweit_sheet, year) {
  read_one <- function(sheet, stimme) {
    d <- read_excel(file, sheet = sheet, col_names = FALSE,
                    .name_repair = "minimal")
    # locate the header: row with "Wahlbe-" (eligible) marker in cols, and the
    # party-name row. We anchor on the row that has "ungültig"/"gültig".
    ug_row <- NA
    for (i in seq_len(min(12, nrow(d)))) {
      r <- clean_name(unlist(d[i, ]))
      if (any(grepl("^un-?\\s*g", r, ignore.case = TRUE), na.rm = TRUE) &&
          any(grepl("^g.ltig$", r, ignore.case = TRUE), na.rm = TRUE)) {
        ug_row <- i; break
      }
    }
    if (is.na(ug_row)) stop(sprintf("[%s %s] ungültig/gültig header not found",
                                    year, sheet))
    rUG <- clean_name(unlist(d[ug_row, ]))
    col_inv   <- which(grepl("^un-?\\s*g", rUG, ignore.case = TRUE))[1]
    col_valid <- which(grepl("^g.ltig$", rUG, ignore.case = TRUE))[1]

    # meta header row (Wahlbe.. / Wähler) is a couple of rows up.
    metarow <- NA
    for (i in (ug_row - 1):max(1, ug_row - 3)) {
      r <- clean_name(unlist(d[i, ]))
      if (any(grepl("^Wahlbe", r), na.rm = TRUE)) { metarow <- i; break }
    }
    if (is.na(metarow)) stop(sprintf("[%s %s] meta header row not found",
                                     year, sheet))
    rM <- clean_name(unlist(d[metarow, ]))
    col_elig  <- which(grepl("^Wahlbe", rM))[1]
    col_voter <- which(grepl("^W.hle", rM))[1]
    col_wknr  <- which(grepl("^Wahl-?\\s*kreis", rM))[1]
    col_wknam <- which(grepl("^Wahlkreis", rM) & !grepl("Nr", rM))
    col_wknam <- col_wknam[col_wknam != col_wknr][1]
    col_jahr  <- which(grepl("^Wahl-?\\s*jahr", rM))[1]
    col_mass  <- which(grepl("^Ma.\\b|^Ma.-?\\s*einhe", rM))[1]

    # party-name row: the row that holds party names spanning the columns to the
    # right of col_valid. Scan rows metarow..ug_row for the one with the most
    # alpha tokens at col_valid+1.
    pcols <- (col_valid + 1):ncol(d)
    prow <- NA; best <- -1
    for (i in seq(metarow, ug_row)) {
      r <- clean_name(unlist(d[i, ]))
      n <- sum(!is.na(r[pcols]) & nzchar(r[pcols]) &
               grepl("[A-Za-zÄÖÜäöü]", r[pcols]))
      if (n > best) { best <- n; prow <- i }
    }
    pnames <- clean_name(unlist(d[prow, ]))[pcols]
    keep   <- !is.na(pnames) & nzchar(pnames)
    pcols  <- pcols[keep]; pnames <- pnames[keep]

    # data rows: Maßeinheit == "Anzahl" AND Wahljahr == election year
    jahr <- as.character(unlist(d[[col_jahr]]))
    mass <- clean_name(unlist(d[[col_mass]]))
    is_data <- !is.na(jahr) & jahr == as.character(year) &
               grepl("^Anzahl", mass, ignore.case = TRUE)

    # forward-fill WK number / name (only on first row of each WK block)
    wkr_nr   <- as.character(unlist(d[[col_wknr]]))
    wkr_name <- clean_name(unlist(d[[col_wknam]]))
    wkr_nr   <- zoo_fill(wkr_nr)
    wkr_name <- zoo_fill(wkr_name)

    body_idx <- which(is_data)
    out_meta <- data.table(
      wkr_nr_chr      = wkr_nr[body_idx],
      wkr_name        = wkr_name[body_idx],
      eligible_voters = to_int(unlist(d[body_idx, col_elig])),
      number_voters   = to_int(unlist(d[body_idx, col_voter])),
      invalid         = to_int(unlist(d[body_idx, col_inv])),
      valid           = to_int(unlist(d[body_idx, col_valid]))
    )
    # parties long
    pl <- rbindlist(lapply(seq_along(pcols), function(k) {
      data.table(
        wkr_nr_chr = wkr_nr[body_idx],
        wkr_name   = wkr_name[body_idx],
        party_raw  = pnames[k],
        votes      = to_int(unlist(d[body_idx, pcols[k]]))
      )
    }))
    pl[, stimme := stimme]
    out_meta[, stimme := stimme]
    list(meta = out_meta, parties = pl)
  }

  e <- read_one(erst_sheet,  "erststimme")
  z <- read_one(zweit_sheet, "zweitstimme")
  list(meta = rbindlist(list(e$meta, z$meta)),
       parties = rbindlist(list(e$parties, z$parties)))
}

# Extract a single year's statewide ("Mecklenburg-Vorpommern" Land row) per-party
# totals from a modern file. Used as an independent cross-check (the 2021 file
# carries both the 2021 AND the 2016 statewide totals; the 2016 file has no Land
# row, so its statewide check is sourced from the 2021 file).
read_modern_statewide <- function(file, erst_sheet, zweit_sheet, want_year) {
  one <- function(sheet, stimme) {
    d <- read_excel(file, sheet = sheet, col_names = FALSE,
                    .name_repair = "minimal")
    ug_row <- NA
    for (i in seq_len(min(12, nrow(d)))) {
      r <- clean_name(unlist(d[i, ]))
      if (any(grepl("^un-?\\s*g", r, ignore.case = TRUE), na.rm = TRUE) &&
          any(grepl("^g.ltig$", r, ignore.case = TRUE), na.rm = TRUE)) { ug_row <- i; break }
    }
    rUG <- clean_name(unlist(d[ug_row, ]))
    col_valid <- which(grepl("^g.ltig$", rUG, ignore.case = TRUE))[1]
    metarow <- NA
    for (i in (ug_row - 1):max(1, ug_row - 3)) {
      r <- clean_name(unlist(d[i, ]))
      if (any(grepl("^Wahlbe", r), na.rm = TRUE)) { metarow <- i; break }
    }
    rM <- clean_name(unlist(d[metarow, ]))
    col_wknam <- which(grepl("^Wahlkreis", rM) & !grepl("Nr", rM))[1]
    col_jahr  <- which(grepl("^Wahl-?\\s*jahr", rM))[1]
    col_mass  <- which(grepl("^Ma.\\b|^Ma.-?\\s*einhe", rM))[1]
    pcols <- (col_valid + 1):ncol(d)
    prow <- NA; best <- -1
    for (i in seq(metarow, ug_row)) {
      r <- clean_name(unlist(d[i, ]))
      n <- sum(!is.na(r[pcols]) & nzchar(r[pcols]) & grepl("[A-Za-zÄÖÜäöü]", r[pcols]))
      if (n > best) { best <- n; prow <- i }
    }
    pnames <- clean_name(unlist(d[prow, ]))[pcols]
    keep <- !is.na(pnames) & nzchar(pnames)
    pcols <- pcols[keep]; pnames <- pnames[keep]
    wkr_name <- zoo_fill(clean_name(unlist(d[[col_wknam]])))
    jahr <- as.character(unlist(d[[col_jahr]]))
    mass <- clean_name(unlist(d[[col_mass]]))
    is_total <- grepl("^Mecklenburg-?\\s*Vorpommern$", clean_name(wkr_name)) &
                jahr == as.character(want_year) &
                grepl("^Anzahl", mass, ignore.case = TRUE)
    idx <- which(is_total)
    if (length(idx) == 0) return(data.table(party_raw = character(),
                                            votes_tot = numeric(), stimme = character()))
    idx <- idx[1]
    data.table(party_raw = pnames,
               votes_tot = to_int(unlist(d[idx, pcols])),
               stimme = stimme)
  }
  rbindlist(list(one(erst_sheet, "erststimme"), one(zweit_sheet, "zweitstimme")))
}

# simple forward-fill (avoid extra deps)
zoo_fill <- function(x) {
  out <- x
  last <- NA
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && nzchar(trimws(as.character(x[i])))) last <- x[i]
    else out[i] <- last
    out[i] <- if (!is.na(x[i]) && nzchar(trimws(as.character(x[i])))) x[i] else last
  }
  out
}

# =============================================================================
# RUN
# =============================================================================

emitted   <- list()       # final long rows
validation <- list()      # per-year validation records
excluded   <- list()

# authoritative statewide reference series (1990-2021)
REF_E <- read_reference_series("erststimme")
REF_Z <- read_reference_series("zweitstimme")

# ---- 1990: excluded (no Wahlkreis/Wahlbezirk file) -------------------------
excluded[["1990"]] <- "Only 'nach Gemeinden' XLS + state-summary PDFs exist; no Wahlbezirk/Wahlkreis-resolvable machine-readable file on LAIV. kreisfreie Städte span multiple Wahlkreise and cannot be split. Constituency-level data not recoverable cleanly."

# ---- OLD YEARS via Wahlbezirke + Gemeinden cross-check ---------------------
old_spec <- list(
  list(year = "1994", wbz = "MV_1994_Landtagswahl_nach_Wahlbezirken.xls", wbz_sheet = "Lw94-wbz",
       gem = "MV_1994_Landtagswahl_nach_Gemeinden.xls", gem_sheet = "LW94-gem"),
  list(year = "1998", wbz = "MV_1998_Landtagswahl_nach_Wahlbezirken.xls", wbz_sheet = "B724W 199801",
       gem = "MV_1998_Landtagswahl_nach_Gemeinden.xls", gem_sheet = "B724G 199801"),
  list(year = "2002", wbz = "MV_2002_Landtagswahl_nach_Wahlbezirken.xls", wbz_sheet = "B724W 200201",
       gem = "MV_2002_Landtagswahl_nach_Gemeinden.xls", gem_sheet = "B724G 200201"),
  list(year = "2006", wbz = "MV_2006_Landtagswahl_nach_Wahlbezirken.xls", wbz_sheet = "Ergebnisse nach Wahlbezirken",
       gem = "MV_2006_Landtagswahl_nach_Gemeinden.xls", gem_sheet = "Ergebnisse nach Gemeinden"),
  list(year = "2011", wbz = "MV_2011_Landtagswahl_nach_Wahlbezirken.xls", wbz_sheet = "wbz",
       gem = "MV_2011_Landtagswahl_nach_Gemeinden.xls", gem_sheet = "gem")
)

for (sp in old_spec) {
  yr <- sp$year
  message("== ", yr, " (Wahlbezirke) ==")
  r <- read_wahlbezirke(file.path(RAW_DIR, sp$wbz), sp$wbz_sheet, yr)

  # build long output rows
  meta <- r$meta
  long <- merge(r$parties, meta, by = "wkr_nr_int", all.x = TRUE)
  long[, valid_votes := ifelse(stimme == "erststimme", erst_valid, zweit_valid)]
  long[, invalid_votes := ifelse(stimme == "erststimme", erst_invalid, zweit_invalid)]
  long_out <- long[, .(
    state_abbr = STATE_ABBR, state = STATE,
    election_year = as.integer(yr),
    election_date = ELECTION_DATES[[yr]],
    wkr_nr  = as.character(wkr_nr_int),
    wkr_name = NA_character_,
    stimme,
    eligible_voters, number_voters,
    valid_votes, invalid_votes,
    party_raw = clean_party(party_raw), votes
  )]

  # ---- validation (a): sum(party votes) vs valid per (wkr, stimme) ----------
  chk <- long_out[, .(sumv = sum(votes, na.rm = TRUE),
                      valid = valid_votes[1]), by = .(wkr_nr, stimme)]
  chk[, disc := abs(sumv - valid)]
  max_disc <- max(chk$disc)
  n_groups <- nrow(chk)

  # ---- validation (b): statewide vs AUTHORITATIVE 1990-2021 reference -------
  ref_chk <- check_against_reference(long_out, yr, REF_E, REF_Z)
  sw_max <- ref_chk$max_disc
  sw_match <- ref_chk$match

  validation[[yr]] <- list(
    year = yr, n_wkr = uniqueN(long_out$wkr_nr),
    max_abs_disc = max_disc, n_groups = n_groups,
    statewide_match = sw_match, statewide_max = sw_max,
    statewide_fail = ref_chk$fails
  )

  message(sprintf("   WK=%d  max(|sum-valid|)=%g over %d groups  | statewide max disc=%g match=%s",
                  uniqueN(long_out$wkr_nr), max_disc, n_groups, sw_max, sw_match))
  if (!sw_match) print(ref_chk$fails)

  if (max_disc <= 2 && sw_match && uniqueN(long_out$wkr_nr) == 36) {
    emitted[[yr]] <- long_out
  } else {
    excluded[[yr]] <- sprintf("FAILED validation: max|sum-valid|=%g, statewide_match=%s, n_wkr=%d",
                              max_disc, sw_match, uniqueN(long_out$wkr_nr))
  }
}

# ---- MODERN YEARS 2016 / 2021 ---------------------------------------------
# normalise a party label for cross-file matching (collapse spaces/case)
norm_party <- function(x) toupper(gsub("[^A-Za-z0-9ÄÖÜäöü]", "", clean_name(x)))

modern_spec <- list(
  list(year = "2016", file = "MV_2016_Landtagswahl_Wahlkreis.xlsx",
       erst = "3.5 Wahlkreisbew.-Erststimmen", zweit = "3.6 Landeslisten-Zweitstimmen",
       # 2016 file has no Land row -> statewide totals taken from the 2021 file
       sw_file = "MV_2021_Landtagswahl_Wahlkreis.xlsx",
       sw_erst = "3.4 Wahlkreisbew.-Erststimmen",
       sw_zweit = "3.5 Landeslisten-Zweitstimmen"),
  list(year = "2021", file = "MV_2021_Landtagswahl_Wahlkreis.xlsx",
       erst = "3.4 Wahlkreisbew.-Erststimmen", zweit = "3.5 Landeslisten-Zweitstimmen",
       sw_file = "MV_2021_Landtagswahl_Wahlkreis.xlsx",
       sw_erst = "3.4 Wahlkreisbew.-Erststimmen",
       sw_zweit = "3.5 Landeslisten-Zweitstimmen")
)

for (sp in modern_spec) {
  yr <- sp$year
  message("== ", yr, " (modern XLSX) ==")
  r <- read_modern(file.path(RAW_DIR, sp$file), sp$erst, sp$zweit, yr)

  meta <- r$meta
  parties <- r$parties

  # Identify the statewide total row ("Mecklenburg-Vorpommern") -> exclude from
  # output. In 2021 the marker is in the Wahlkreis-NAME column; in 2016 it is in
  # the Wahlkreis-NUMBER column. Check both.
  is_total <- function(nm) grepl("^Mecklenburg-?\\s*Vorpommern$", clean_name(nm))
  meta    <- meta[!(is_total(wkr_name) | is_total(wkr_nr_chr))]
  parties <- parties[!(is_total(wkr_name) | is_total(wkr_nr_chr))]

  # join meta onto parties
  long <- merge(parties, meta, by = c("wkr_nr_chr", "wkr_name", "stimme"),
                all.x = TRUE)
  long_out <- long[, .(
    state_abbr = STATE_ABBR, state = STATE,
    election_year = as.integer(yr),
    election_date = ELECTION_DATES[[yr]],
    wkr_nr = wkr_nr_chr,
    wkr_name = clean_name(wkr_name),
    stimme,
    eligible_voters, number_voters,
    valid_votes = valid, invalid_votes = invalid,
    party_raw = clean_party(party_raw), votes
  )]

  # ---- validation (a): sum(party) vs valid per (wkr, stimme) ----------------
  chk <- long_out[, .(sumv = sum(votes, na.rm = TRUE),
                      valid = valid_votes[1]), by = .(wkr_nr, stimme)]
  chk[, disc := abs(sumv - valid)]
  max_disc <- max(chk$disc)
  n_groups <- nrow(chk)

  # ---- validation (b): statewide vs AUTHORITATIVE 1990-2021 reference -------
  ref_chk <- check_against_reference(long_out, yr, REF_E, REF_Z)
  sw_max <- ref_chk$max_disc
  sw_match <- ref_chk$match

  validation[[yr]] <- list(
    year = yr, n_wkr = uniqueN(long_out$wkr_nr),
    max_abs_disc = max_disc, n_groups = n_groups,
    statewide_match = sw_match, statewide_max = sw_max,
    statewide_fail = ref_chk$fails
  )
  message(sprintf("   WK=%d  max(|sum-valid|)=%g over %d groups | statewide max disc=%g match=%s",
                  uniqueN(long_out$wkr_nr), max_disc, n_groups, sw_max, sw_match))
  if (!sw_match) print(ref_chk$fails)

  if (max_disc <= 2 && sw_match && uniqueN(long_out$wkr_nr) == 36) {
    emitted[[yr]] <- long_out
  } else {
    excluded[[yr]] <- sprintf("FAILED validation: max|sum-valid|=%g, statewide_match=%s, n_wkr=%d",
                              max_disc, sw_match, uniqueN(long_out$wkr_nr))
  }
}

# ---- WRITE -----------------------------------------------------------------
final <- rbindlist(emitted, use.names = TRUE)
setcolorder(final, c("state_abbr","state","election_year","election_date",
                     "wkr_nr","wkr_name","stimme","eligible_voters",
                     "number_voters","valid_votes","invalid_votes",
                     "party_raw","votes"))
setorder(final, election_year, stimme, wkr_nr, party_raw)

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
fwrite(final, OUT_CSV)
message("\nWROTE ", OUT_CSV, "  rows=", nrow(final))

# ---- SUMMARY printout ------------------------------------------------------
cat("\n================ VALIDATION SUMMARY ================\n")
for (yr in names(validation)) {
  v <- validation[[yr]]
  cat(sprintf("%s: n_wkr=%d  max|sum-valid|=%g (n=%d)  statewide_match=%s (max disc=%g)\n",
              yr, v$n_wkr, v$max_abs_disc, v$n_groups,
              v$statewide_match, v$statewide_max))
}
cat("\nEMITTED years:", paste(names(emitted), collapse=", "), "\n")
cat("EXCLUDED:\n"); for (yr in names(excluded)) cat("  ", yr, ":", excluded[[yr]], "\n")
cat("\nTotal rows:", nrow(final), "\n")
cat("Rows per year:\n"); print(final[, .N, by = election_year])
cat("\nDistinct party_raw:\n"); print(sort(unique(final$party_raw)))
