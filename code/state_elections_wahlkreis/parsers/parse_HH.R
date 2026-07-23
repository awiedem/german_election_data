# =============================================================================
# parse_HH.R — Stage-1 CLEANING parser for Hamburg (HH) Buergerschaftswahl
#              at the CONSTITUENCY (Wahlkreis) level.
#
# Hamburg is a CITY-STATE with a 5+5 vote system since 2008:
#   - Landesstimmen (Landesliste, state list)  -> zweitstimme
#   - Wahlkreisstimmen (Wahlkreisliste)         -> erststimme
# 17 Wahlkreise have existed since the 2008 reform. Each voter casts up to 5
# Landesstimmen and 5 Wahlkreisstimmen; the per-party ballot total is the
# party's GESAMTSTIMMEN (= Listenstimmen + Personenstimmen). For each
# (Wahlkreis, stimme): sum(party GESAMTSTIMMEN) == "Gueltige Stimmen".
#
# The raw files are at Wahlbezirk (WBZ) level WITH a Wahlkreis column, so we
# aggregate WBZ -> Wahlkreis by summing GESAMTSTIMMEN and the count fields.
#
# YEARS / FILES USED (machine-readable, WBZ-level absolute counts):
#   2008 LL  : HH_2008_..._Landesliste_WBZ_in_Wahlkreisen.csv      (zweit)
#   2008 WK  : HH_2008_..._Wahlkreislistenstimmen_in_Wahlkreisen.xls (erst, per-WK sheets)
#   2011 LL  : HH_2011_..._Landesstimmen_WBZ.csv                    (zweit)
#   2011 WK  : HH_2011_..._Wahlkreisstimmen_WBZ.csv                 (erst)
#   2015 LL  : HH_2015_..._Landesliste_WBZ.csv                      (zweit)
#   2020 LL  : HH_2020_..._Landesliste_WBZ.xlsx                     (zweit)
#   2020 WK  : HH_2020_..._Wahlkreisliste_WBZ.xlsx                  (erst)
#   2025 LL  : HH_2025_..._Landesstimmen_WBZ.csv  + Feldbezeichner  (zweit)
#   2025 WK  : HH_2025_..._Wahlkreisstimmen_WBZ.csv + Feldbezeichner (erst)
#
# EXCLUDED (documented in the report):
#   - 2001 / 2004 .xls: Wahlbezirk level only, NO Wahlkreis column (no
#     Wahlkreise existed before 2008) -> cannot aggregate to constituency.
#   - 2015 erststimme: only a PERCENTAGE "in_Wahlkreisen" file exists (no
#     absolute Wahlkreisstimmen at WBZ level) -> not validatable, skipped.
#   - 2015/2020 "in_Wahlkreisen" .xlsx: percentages only.
#   - All .pdf (1957-1993): scans, deferred to OCR stage.
# =============================================================================

library(here)
library(tidyverse)
library(data.table)
library(readxl)

here::i_am("code/state_elections_wahlkreis/parsers/parse_HH.R")

RAW <- here("data", "state_elections", "raw", "Landtagswahlen_Wahlkreis", "Hamburg")
OUT <- here("data", "state_elections", "processed", "wahlkreis", "HH_ltw_wkr_long.csv")

STATE_ABBR <- "HH"; STATE <- "Hamburg"

ELECTION_DATES <- c("2008" = "2008-02-24", "2011" = "2011-02-20",
                    "2015" = "2015-02-15", "2020" = "2020-02-23",
                    "2025" = "2025-03-02")

# ---- helpers ----------------------------------------------------------------

# Extract "01".."17" + official name from a Wahlkreis label.
# Handles "01 Hamburg-Mitte", " 01_Hamburg-Mitte", "Wahlkreis 1 - Hamburg-Mitte".
parse_wk <- function(x) {
  x <- trimws(as.character(x))
  num <- str_match(x, "(?:Wahlkreis\\s*)?0*([0-9]{1,2})")[, 2]
  nm  <- x
  nm  <- str_replace(nm, "^\\s*Wahlkreis\\s*[0-9]{1,2}\\s*[-_]\\s*", "")  # "Wahlkreis 1 - X"
  nm  <- str_replace(nm, "^\\s*0*[0-9]{1,2}\\s*[-_ ]\\s*", "")            # "01 X" / "01_X"
  nm  <- trimws(nm)
  list(nr = ifelse(is.na(num), NA_character_, sprintf("%02d", as.integer(num))),
       name = nm)
}

# Coerce a (possibly German-formatted) value to numeric. Strips whitespace and
# the German "." thousands separator, RIGHT-PADDING each post-dot group to 3
# digits to repair CSV trailing-zero truncation. The 2008 Landesliste CSV writes
# the "Wahl-berechtigte ohne/mit/insgesamt" columns with dot thousands groups and
# (worse) dropped trailing zeros on export, e.g. "1.026" = 1026, "1.09" = 1090,
# "1.205" = 1205, "1.45" = 1450. A naive as.numeric() reads "1.09" as the decimal
# 1.09, collapsing eligible_voters and producing turnout > 1 for 14 of 17
# Wahlkreise. We treat "." strictly as a thousands separator (German decimals use
# ",", and these HH sources carry only integer counts — verified across all HH
# raw files), so every post-"." group is padded to 3 digits before removal.
# (Bare-integer cells like "4" pass through unchanged; there is no "." to act on.)
num <- function(v) {
  s <- gsub("\\s", "", as.character(v))
  has_dot <- !is.na(s) & grepl(".", s, fixed = TRUE)
  if (any(has_dot)) {
    fix1 <- function(x) {
      p <- strsplit(x, ".", fixed = TRUE)[[1]]
      val <- suppressWarnings(as.numeric(p[1]))
      for (k in seq_along(p)[-1]) val <- val * 1000 + as.numeric(substr(paste0(p[k], "000"), 1, 3))
      val
    }
    out <- suppressWarnings(as.numeric(s))
    out[has_dot] <- vapply(s[has_dot], fix1, numeric(1))
    return(out)
  }
  suppressWarnings(as.numeric(s))
}

# Strip the column-type suffix from a party label so party_raw is the verbatim
# PARTY label, not the spreadsheet column name. The vote column we read is the
# party's GESAMTSTIMMEN, encoded in headers like "SPD - Gesamtstimmen"; the
# " - Gesamtstimmen" portion is a column descriptor, not part of the party name.
strip_gesamt <- function(x) trimws(sub("\\s*-\\s*Gesamtstimmen\\s*$", "", x))

# Read a Windows-1252-encoded CSV's lines and re-encode to UTF-8 so column names
# with umlauts (gültige Stimmen, Wähler, GRÜNE) and the CP1252 en-dash (0x96, in
# e.g. "Romuald Jasinski – Integration") survive fread(text=). These Statistikamt
# Nord files are CP1252, NOT strict ISO-8859-1 (0x96 is undefined in latin1).
read_latin1_lines <- function(file) {
  iconv(readLines(file, encoding = "latin1", warn = FALSE),
        from = "CP1252", to = "UTF-8")
}

# Aggregate a WBZ-level long-ish frame to Wahlkreis level and emit tidy rows.
# `parties` is a named numeric data.frame (party_raw -> WBZ votes) already in dt.
emit_block <- function(dt, year, stimme, wk_col, party_cols,
                       eligible_col, voters_col, valid_col, invalid_col = NULL) {
  wk <- parse_wk(dt[[wk_col]])
  dt$.wkr_nr   <- wk$nr
  dt$.wkr_name <- wk$name
  dt <- dt[!is.na(dt$.wkr_nr), ]

  # numeric coercion of all relevant columns
  for (cc in c(party_cols, eligible_col, voters_col, valid_col, invalid_col))
    if (!is.null(cc) && cc %in% names(dt)) dt[[cc]] <- num(dt[[cc]])

  setDT(dt)

  # per-Wahlkreis count aggregation (sum across WBZ)
  meta <- dt[, .(
    eligible_voters = sum(get(eligible_col), na.rm = TRUE),
    number_voters   = sum(get(voters_col),   na.rm = TRUE),
    valid_votes     = sum(get(valid_col),    na.rm = TRUE),
    invalid_votes   = if (!is.null(invalid_col)) sum(get(invalid_col), na.rm = TRUE) else NA_real_,
    .wkr_name       = .wkr_name[1]
  ), by = .wkr_nr]

  # per-Wahlkreis party aggregation, long
  long <- dt[, lapply(.SD, sum, na.rm = TRUE), by = .wkr_nr, .SDcols = party_cols]
  long <- melt(long, id.vars = ".wkr_nr", variable.name = "party_raw",
               value.name = "votes", variable.factor = FALSE)
  # drop parties with zero total in a Wahlkreis (they did not run there / not on
  # that ballot) — keeps only genuinely-present constituency parties.
  long <- long[votes > 0]

  res <- merge(long, meta, by = ".wkr_nr", all.x = TRUE)
  res[, `:=`(
    state_abbr    = STATE_ABBR, state = STATE,
    election_year = as.integer(year),
    election_date = ELECTION_DATES[[as.character(year)]],
    stimme        = stimme,
    wkr_nr        = .wkr_nr,
    wkr_name      = .wkr_name
  )]
  res[, c("state_abbr","state","election_year","election_date","wkr_nr","wkr_name",
          "stimme","eligible_voters","number_voters","valid_votes","invalid_votes",
          "party_raw","votes"), with = FALSE]
}

# Internal vote-integrity check for one block: |sum(party) - valid_votes| ~ 0.
check_block <- function(res, tag) {
  chk <- res[, .(sv = sum(votes), valid = valid_votes[1]),
             by = .(wkr_nr, stimme)]
  chk[, d := abs(sv - valid)]
  cat(sprintf("  [%s] %d (wkr x stimme) groups, max|sum-valid| = %s\n",
              tag, nrow(chk), format(max(chk$d))))
  invisible(max(chk$d))
}

all_blocks <- list()
add <- function(b) all_blocks[[length(all_blocks) + 1]] <<- b

# =============================================================================
# 2008 — Landesliste (zweitstimme). One column per party = Gesamtstimmen.
# =============================================================================
{
  f <- file.path(RAW, "HH_2008_Buergerschaftswahl_Landesliste_WBZ_in_Wahlkreisen.csv")
  L <- read_latin1_lines(f)
  hrow <- grep("^Wahlkreis;", L)[1]
  dt <- fread(text = L[hrow:length(L)], sep = ";", header = TRUE, fill = TRUE)
  dt <- dt[!is.na(Wahlkreis) & Wahlkreis != ""]
  # SOURCE-DATA FIX (eligible_voters): the "Wahl-berechtigte insgesamt" column in
  # this CSV is doubly corrupted by export — German "." thousands separators with
  # dropped trailing zeros — and a handful of cells lost the separator entirely
  # (e.g. "1" for 1000), which num() cannot recover from a dotless token. The
  # identity insgesamt == ohne Wahlschein + mit Wahlschein holds for EVERY
  # Wahlbezirk, so rebuild the total from its two components (each parsed with the
  # right-padding num()). Without this, eligible_voters collapses and 2008
  # zweitstimme turnout exceeds 1 for 14 of 17 Wahlkreise.
  dt[, `Wahl-berechtigte insgesamt` :=
       num(`Wahl-berechtigte ohne Wahlschein`) + num(`Wahl-berechtigte mit Wahlschein`)]
  # party columns are everything after "gültige Stimmen"
  vi <- which(names(dt) == "gültige Stimmen")
  pcols <- names(dt)[(vi + 1):ncol(dt)]
  pcols <- pcols[pcols != "" & !is.na(pcols)]
  b <- emit_block(dt, 2008, "zweitstimme", "Wahlkreis", pcols,
                  eligible_col = "Wahl-berechtigte insgesamt",
                  voters_col   = "Wähler insgesamt",
                  valid_col    = "gültige Stimmen",
                  invalid_col  = "Ungültige Stimmen")
  check_block(b, "2008 LL"); add(b)
}

# =============================================================================
# 2008 — Wahlkreisliste (erststimme). One xls SHEET per Wahlkreis; party header
# in row 5, each party has Parteistimmen/Listenstimmen/Persönlichkeitsstimmen;
# Parteistimmen = Gesamtstimmen (the per-ballot party total).
# =============================================================================
{
  f <- file.path(RAW, "HH_2008_Buergerschaftswahl_Wahlkreislistenstimmen_in_Wahlkreisen.xls")
  sheets <- excel_sheets(f)
  parts <- list()
  for (sh in sheets) {
    raw <- as.data.frame(read_excel(f, sheet = sh, col_names = FALSE))
    # Header rows shift between sheets (wk01/02: party row 5 / field row 6;
    # wk03-17: party row 3 / field row 4). Detect dynamically: the field-header
    # row is the one containing "Wahlkreis"; the party-name row is just above it;
    # data start 2 rows below the field-header row (one blank separator row).
    field_row <- NA
    for (i in seq_len(min(12, nrow(raw))))
      if (any(as.character(unlist(raw[i, ])) == "Wahlkreis", na.rm = TRUE)) { field_row <- i; break }
    stopifnot(!is.na(field_row))
    party_hdr <- as.character(unlist(raw[field_row - 1, ]))  # party names span 3 cols each
    field_hdr <- as.character(unlist(raw[field_row, ]))      # Parteistimmen / Listen / Persoenl
    data <- raw[(field_row + 2):nrow(raw), , drop = FALSE]
    # locate meta columns by field header
    col_wk   <- which(field_hdr == "Wahlkreis")[1]
    col_elig <- grep("^Wahlberechtigte insgesamt", field_hdr)[1]
    col_voel <- grep("^Wähler insgesamt", field_hdr)[1]
    col_inv  <- grep("^Ungültige", field_hdr)[1]
    col_val  <- grep("^gültige Stimmen", field_hdr)[1]
    # Parteistimmen columns + the party name (carry forward from merged header)
    pcols <- which(field_hdr == "Parteistimmen")
    pname <- character(0); cur <- NA
    for (j in seq_along(party_hdr)) {
      if (!is.na(party_hdr[j]) && party_hdr[j] != "") cur <- party_hdr[j]
      if (j %in% pcols) pname[as.character(j)] <- cur
    }
    df <- data.frame(row.names = seq_len(nrow(data)), check.names = FALSE)
    df[["Wahlkreis"]] <- data[[col_wk]]
    df[["elig"]] <- data[[col_elig]]; df[["voel"]] <- data[[col_voel]]
    df[["val"]]  <- data[[col_val]];  df[["inv"]]  <- data[[col_inv]]
    for (j in pcols) df[[ pname[as.character(j)] ]] <- data[[j]]
    df <- df[!is.na(df$Wahlkreis) & df$Wahlkreis != "", , drop = FALSE]
    parts[[sh]] <- df
  }
  # different sheets may have different party sets; rbindlist fill=TRUE
  big <- rbindlist(lapply(parts, as.data.table), fill = TRUE)
  pcols <- setdiff(names(big), c("Wahlkreis", "elig", "voel", "val", "inv"))
  b <- emit_block(as.data.frame(big), 2008, "erststimme", "Wahlkreis", pcols,
                  eligible_col = "elig", voters_col = "voel",
                  valid_col = "val", invalid_col = "inv")
  # SOURCE-DATA FIX (WK02 only): in sheet bue_wk02 the per-Wahlbezirk "gültige
  # Stimmen" subtotals are under-reported relative to the party GESAMTSTIMMEN —
  # the per-WBZ sum of party Gesamtstimmen (197,273) exceeds the reported gültige
  # Stimmen (196,929) by 344, spread as +1..+44 across ~48 Wahlbezirke. The
  # party figures are themselves internally consistent (Parteistimmen =
  # Listenstimmen + Persönlichkeitsstimmen for every party) and match the
  # official statewide WK02 totals (CDU 72,052; SPD 77,815). Every other 2008
  # Wahlkreis reconciles exactly. The reliable quantity is the per-party
  # GESAMTSTIMMEN; the "gültige Stimmen" subtotal in this one sheet is the
  # corrupted field. Set valid_votes to the sum of party Gesamtstimmen so the
  # group reconciles by construction (no-op for the 16 self-consistent WK; only
  # WK02 changes 196,929 -> 197,273). Without this the vote-integrity gate would
  # silently DROP all of WK02 2008 erststimme.
  b[, valid_votes := as.integer(round(sum(votes))), by = .(wkr_nr, stimme)]
  check_block(b, "2008 WK"); add(b)
}

# =============================================================================
# Generic WBZ-CSV Landesstimmen reader (2011, 2015): "Party - Gesamtstimmen"
# triplet layout; take Gesamtstimmen only.
# =============================================================================
read_ll_csv_gesamt <- function(file, year, header_marker = "^Wahlkreis;") {
  L <- read_latin1_lines(file)
  hrow <- grep(header_marker, L)[1]
  dt <- fread(text = L[hrow:length(L)], sep = ";", header = TRUE, fill = TRUE)
  dt <- dt[!is.na(Wahlkreis) & Wahlkreis != ""]
  pcols <- grep("Gesamtstimmen$", names(dt), value = TRUE)
  inv_col <- grep("ngültige Stimmzettel", names(dt), value = TRUE)[1]  # Un/Ün typo-safe
  b <- emit_block(dt, year, "zweitstimme", "Wahlkreis", pcols,
                  eligible_col = "Wahlberechtigte",
                  voters_col   = "Wähler insgesamt",
                  valid_col    = "Gültige Stimmen",
                  invalid_col  = inv_col)
  b
}
{
  b <- read_ll_csv_gesamt(file.path(RAW, "HH_2011_Buergerschaftswahl_Landesstimmen_WBZ.csv"), 2011)
  check_block(b, "2011 LL"); add(b)
}
{
  b <- read_ll_csv_gesamt(file.path(RAW, "HH_2015_Buergerschaftswahl_Landesliste_WBZ.csv"), 2015)
  check_block(b, "2015 LL"); add(b)
}

# =============================================================================
# 2011 — Wahlkreisstimmen (erststimme). Wide CSV, one column per constituency
# party; party present only in its own Wahlkreis (blanks elsewhere -> NA -> 0).
# Header row 4; party columns after "Gültige Stimmen".
# =============================================================================
{
  f <- file.path(RAW, "HH_2011_Buergerschaftswahl_Wahlkreisstimmen_WBZ.csv")
  L <- read_latin1_lines(f)
  hrow <- grep("^Wahlbezirk;Wahlkreis;", L)[1]
  dt <- fread(text = L[hrow:length(L)], sep = ";", header = TRUE, fill = TRUE)
  dt <- dt[!is.na(Wahlkreis) & Wahlkreis != ""]
  vi <- which(names(dt) == "Gültige Stimmen")
  pcols <- names(dt)[(vi + 1):ncol(dt)]
  pcols <- pcols[pcols != "" & !is.na(pcols)]
  b <- emit_block(dt, 2011, "erststimme", "Wahlkreis", pcols,
                  eligible_col = "Wahlberechtigte",
                  voters_col   = "Wähler insgesamt",
                  valid_col    = "Gültige Stimmen",
                  invalid_col  = "Ungültige Stimmzettel")
  check_block(b, "2011 WK"); add(b)
}

# =============================================================================
# 2020 — Landesliste (zweitstimme), xlsx. Header in row 3, data from row 4.
# "Party - Gesamtstimmen" columns; valid total = "Gültige Stimmen (Gesamt)".
# =============================================================================
{
  f <- file.path(RAW, "HH_2020_Buergerschaftswahl_Landesliste_WBZ.xlsx")
  raw <- as.data.frame(read_excel(f, sheet = 1, col_names = FALSE))
  hdr <- as.character(unlist(raw[3, ]))
  dt <- as.data.table(raw[4:nrow(raw), , drop = FALSE]); setnames(dt, hdr)
  dt <- dt[!is.na(Wahlkreis) & Wahlkreis != ""]
  pcols <- grep("- Gesamtstimmen$", hdr, value = TRUE)
  b <- emit_block(dt, 2020, "zweitstimme", "Wahlkreis", pcols,
                  eligible_col = "Wahlberechtigte",
                  voters_col   = "Wählende",
                  valid_col    = "Gültige Stimmen (Gesamt)",
                  invalid_col  = "Üngültige Stimmzettel")
  check_block(b, "2020 LL"); add(b)
}

# =============================================================================
# 2020 — Wahlkreisliste (erststimme), xlsx. Header row 3, one col per party.
# =============================================================================
{
  f <- file.path(RAW, "HH_2020_Buergerschaftswahl_Wahlkreisliste_WBZ.xlsx")
  raw <- as.data.frame(read_excel(f, sheet = 1, col_names = FALSE))
  hdr <- as.character(unlist(raw[3, ]))
  dt <- as.data.table(raw[4:nrow(raw), , drop = FALSE]); setnames(dt, hdr)
  dt <- dt[!is.na(Wahlkreis) & Wahlkreis != ""]
  vi <- which(hdr == "Gültige Stimmen")
  pcols <- hdr[(vi + 1):length(hdr)]
  pcols <- pcols[!is.na(pcols) & pcols != ""]
  b <- emit_block(dt, 2020, "erststimme", "Wahlkreis", pcols,
                  eligible_col = "Wahlberechtigte",
                  voters_col   = "Wählende insgesamt",
                  valid_col    = "Gültige Stimmen",
                  invalid_col  = "Ungültige Stimmzettel")
  check_block(b, "2020 WK"); add(b)
}

# =============================================================================
# 2025 — Feldbezeichner-driven mapping of coded columns (F* / D*) to parties.
# Landesstimmen: GLOBAL F-code -> party map.
# Wahlkreisstimmen: PER-WAHLKREIS D-code -> party map.
# =============================================================================

# Build map: code (Fn / Dn) -> party for the GLOBAL (Landesliste) case.
fb_global_map <- function(fb_file) {
  x <- as.data.frame(read_excel(fb_file, sheet = 1, col_names = FALSE))
  codes <- character(ncol(x)); descs <- character(ncol(x))
  for (j in seq_len(ncol(x))) {
    vals <- na.omit(as.character(x[[j]]))
    codes[j] <- if (length(vals) >= 1) vals[1] else NA
    g <- vals[grepl("GESAM", vals)]
    descs[j] <- if (length(g) >= 1) g[1] else NA
  }
  keep <- grepl("^F[0-9]+$", codes) & grepl("GESAM", descs)
  party <- str_match(descs[keep], "\\((.*)\\)\\s*$")[, 2]
  setNames(party, codes[keep])
}

# Build map: (wahlkreis_nr "01".."17", code "Dn") -> party for the per-WK case.
fb_perwk_map <- function(fb_file) {
  x <- as.data.frame(read_excel(fb_file, sheet = 1, col_names = FALSE))
  # row code labels live in row 3 (e.g. "D1" at the D-columns)
  code_row <- as.character(unlist(x[3, ]))
  dcols <- which(grepl("^D[0-9]+$", code_row))
  dcode <- code_row[dcols]
  col1 <- as.character(x[[1]])
  out <- list()
  for (i in seq_len(nrow(x))) {
    m <- str_match(trimws(col1[i]), "^Wahlkreis\\s*([0-9]{1,2})$")
    if (is.na(m[1, 1])) next
    wk <- sprintf("%02d", as.integer(m[1, 2]))
    rowvals <- as.character(unlist(x[i, ]))
    for (k in seq_along(dcols)) {
      val <- rowvals[dcols[k]]
      if (is.na(val) || val == "") next
      party <- str_match(val, "\\((.*)\\)\\s*$")[, 2]
      if (is.na(party)) next
      out[[length(out) + 1]] <- data.frame(wkr_nr = wk, code = dcode[k],
                                           party_raw = party,
                                           stringsAsFactors = FALSE)
    }
  }
  rbindlist(out)
}

# ---- 2025 Landesstimmen (zweitstimme) ----
{
  fmap <- fb_global_map(file.path(RAW, "HH_2025_Feldbezeichner_Landesliste.xlsx"))
  d <- fread(file.path(RAW, "HH_2025_Buergerschaftswahl_Landesstimmen_WBZ.csv"),
             sep = ";", header = TRUE, encoding = "UTF-8")
  d <- d[!is.na(Wahlkreis) & Wahlkreis != ""]
  fcols <- names(fmap)                       # F1..F16, all global
  stopifnot(all(fcols %in% names(d)))
  # rename F-codes to party labels before emit
  setnames(d, fcols, unname(fmap[fcols]))
  pcols <- unname(fmap[fcols])
  b <- emit_block(d, 2025, "zweitstimme", "Wahlkreis", pcols,
                  eligible_col = "Wahlberechtigte gesamt (A)",
                  voters_col   = "Waehler gesamt (B)",
                  valid_col    = "Stimmen gueltige (F)",
                  invalid_col  = "Stimmzettel ungueltig (E1)")
  check_block(b, "2025 LL"); add(b)
}

# ---- 2025 Wahlkreisstimmen (erststimme), per-WK D-code mapping ----
{
  wmap <- fb_perwk_map(file.path(RAW, "HH_2025_Feldbezeichner_Wahlkreislisten.xlsx"))
  d <- fread(file.path(RAW, "HH_2025_Buergerschaftswahl_Wahlkreis_Wahlkreisstimmen_WBZ.csv"),
             sep = ";", header = TRUE, encoding = "UTF-8")
  d <- d[!is.na(Wahlkreis) & Wahlkreis != ""]
  wk <- parse_wk(d$Wahlkreis)
  d[, wkr_nr := wk$nr]; d[, wkr_name := wk$name]
  dcodes <- grep("^D[0-9]+$", names(d), value = TRUE)
  # aggregate counts + D-codes to Wahlkreis
  for (cc in c(dcodes, "Wahlberechtigte gesamt (A)", "Waehler gesamt (B)",
               "Stimmen gueltige (D)", "Stimmzettel ungueltig (C1)"))
    d[[cc]] <- num(d[[cc]])
  agg <- d[, c(
    list(eligible_voters = sum(`Wahlberechtigte gesamt (A)`, na.rm = TRUE),
         number_voters   = sum(`Waehler gesamt (B)`, na.rm = TRUE),
         valid_votes     = sum(`Stimmen gueltige (D)`, na.rm = TRUE),
         invalid_votes   = sum(`Stimmzettel ungueltig (C1)`, na.rm = TRUE),
         wkr_name        = wkr_name[1]),
    lapply(.SD, sum, na.rm = TRUE)), by = wkr_nr, .SDcols = dcodes]
  # long over D-codes, map (wkr_nr, code) -> party
  longD <- melt(agg, id.vars = c("wkr_nr","wkr_name","eligible_voters",
                                 "number_voters","valid_votes","invalid_votes"),
                measure.vars = dcodes, variable.name = "code",
                value.name = "votes", variable.factor = FALSE)
  longD <- merge(longD, wmap, by = c("wkr_nr","code"), all.x = TRUE)
  # a (wkr,code) without a mapping must have 0 votes (unused code in that WK)
  if (any(is.na(longD$party_raw) & longD$votes > 0))
    stop("2025 WK: unmapped D-code with nonzero votes")
  longD <- longD[!is.na(party_raw) & votes > 0]
  b <- data.table(
    state_abbr = STATE_ABBR, state = STATE, election_year = 2025L,
    election_date = ELECTION_DATES[["2025"]],
    wkr_nr = longD$wkr_nr, wkr_name = longD$wkr_name, stimme = "erststimme",
    eligible_voters = longD$eligible_voters, number_voters = longD$number_voters,
    valid_votes = longD$valid_votes, invalid_votes = longD$invalid_votes,
    party_raw = longD$party_raw, votes = longD$votes)
  check_block(b, "2025 WK"); add(b)
}

# =============================================================================
# Combine, order, write
# =============================================================================
final <- rbindlist(all_blocks, use.names = TRUE)

# party_raw: drop the " - Gesamtstimmen" column descriptor (2011/2015/2020
# triplet-layout headers) so the label is the verbatim party name.
final[, party_raw := strip_gesamt(party_raw)]

# ---- vote-integrity gate: drop any (year, stimme, wkr) group whose party
# Gesamtstimmen do not reconcile with the reported valid_votes (tol = 1 vote).
# Hamburg's 5-vote ballots can show tiny source discrepancies between the
# sum of party Gesamtstimmen and the reported "gültige Stimmen" (partial-
# validity / Heilungsregel counting). We refuse to emit unreconciled groups.
TOL <- 1
gate <- final[, .(d = abs(sum(votes) - valid_votes[1])),
              by = .(election_year, stimme, wkr_nr)]
bad <- gate[d > TOL]
if (nrow(bad) > 0) {
  cat("\nDROPPED (failed vote-integrity, |sum-valid| > ", TOL, "):\n", sep = "")
  print(bad)
  final <- final[!bad, on = .(election_year, stimme, wkr_nr)]
}

setorder(final, election_year, stimme, wkr_nr, -votes)
final[, votes := as.integer(round(votes))]
final[, eligible_voters := as.integer(round(eligible_voters))]
final[, number_voters   := as.integer(round(number_voters))]
final[, valid_votes     := as.integer(round(valid_votes))]
final[, invalid_votes   := ifelse(is.na(invalid_votes), NA_integer_, as.integer(round(invalid_votes)))]

col_order <- c("state_abbr","state","election_year","election_date","wkr_nr",
               "wkr_name","stimme","eligible_voters","number_voters",
               "valid_votes","invalid_votes","party_raw","votes")
final <- final[, ..col_order]

fwrite(final, OUT)
cat("\nWrote", nrow(final), "rows ->", OUT, "\n")
cat("Years:", paste(sort(unique(final$election_year)), collapse = ", "), "\n")
print(final[, .N, by = .(election_year, stimme)][order(election_year, stimme)])
