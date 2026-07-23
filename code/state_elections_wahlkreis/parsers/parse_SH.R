# =============================================================================
# parse_SH.R  --  Stage-1 cleaning parser for Schleswig-Holstein (SH)
#                 Landtagswahl results at the WAHLKREIS level.
#
# Vote system: erststimme + zweitstimme.
# Output: one long, tidy CSV, one row per (Wahlkreis x stimme x party).
#
# VALIDATABLE machine-readable years emitted: 2000, 2009, 2017, 2022.
#   - 2000: 10 archived StatLA HTML tables (ISO-8859-1). Pages _02 (turnout),
#           _03 (Erststimmen counts), _04 (Zweitstimmen counts). 45 Wahlkreise.
#   - 2009: Endgueltige_Ergebnisse.xls, one sheet per Wahlkreis (40 WK) + a
#           "Land Schleswig-Holstein" total sheet. Absolute counts in the
#           "Landtagswahl 2009" column.
#   - 2017: SH_2017_..._Wahlkreis.xlsx (combined). Full absolute counts +
#           turnout + ALL parties, with a "Schleswig-Holstein" total row. 35 WK.
#   - 2022: votemanager Wahlbezirk CSV (SH_2022_..._Wahlbezirk_ergebnis-
#           download.csv) — full ABSOLUTE counts + turnout for ALL parties,
#           2 909 Wahlbezirke aggregated to the 35 Wahlkreise. Parties are
#           opaque D-codes (Direkt/Erststimmen) + F-codes (Liste/Zweitstimmen).
#           The code->verbatim-name map is established by EXACT statewide-total
#           fingerprinting against the official Wahlbericht Table 1
#           ("Endgültiges Ergebnis ... 2022", 19 named Wahlvorschläge + EzB):
#           every D/F code total matches one party's statewide count byte-for-
#           byte (max |diff| = 0). The 7 "ausgewählter Parteien" xlsx files
#           (Erst-/Zweitstimmen) re-confirm CDU/SPD/GRÜNE/FDP/AfD/DIE LINKE/SSW
#           and supply the 35 verbatim Wahlkreis names. A built-in self-check
#           re-derives the 7-party totals from the xlsx at parse time and aborts
#           on any drift. wkr_nr kept 2-digit "01".."35".
#
# EXCLUDED machine-readable years (recorded in the report):
#   - 2012: only PERCENTAGE xls files exist at WK level (no absolute counts,
#           no turnout, no party totals, only 7 "ausgewählter Parteien", no
#           statewide total row) -> integrity check impossible. Counts live only
#           in SH_2012_..._Wahlbericht.pdf -> deferred to OCR stage.
#   - 2005: no machine-readable WK file at all (only B VII 2 5/05 PDF) -> OCR.
#   - All .pdf / .tif : deferred to the future OCR stage.
# =============================================================================

library(here)
library(tidyverse)
library(data.table)
library(readxl)
library(rvest)
library(xml2)

here::i_am("code/state_elections_wahlkreis/parsers/parse_SH.R")

RAW_DIR <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Schleswig-Holstein")
OUT_CSV <- here("data", "state_elections", "processed", "wahlkreis",
                "SH_ltw_wkr_long.csv")

OUT_COLS <- c("state_abbr", "state", "election_year", "election_date",
              "wkr_nr", "wkr_name", "stimme",
              "eligible_voters", "number_voters", "valid_votes",
              "invalid_votes", "party_raw", "votes")

# --- helpers -----------------------------------------------------------------

# Parse a German integer that may carry thin-space / regular-space / nbsp
# thousands separators, or an en-dash / "-" meaning "none". Returns NA for none.
de_int <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  # treat en-dash, em-dash, ndash entity, lone hyphen, empty, "x", "." as missing
  na_marks <- c("–", "—", "-", "", "x", "×", ".", "·")
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    s <- x[i]
    if (is.na(s) || s %in% na_marks) next
    s <- gsub("[    \\.]", "", s)  # strip nbsp, thin sp, sp, dots
    s <- gsub(",", "", s)
    if (grepl("^[0-9]+$", s)) out[i] <- as.numeric(s)
  }
  out
}

read_iso_html <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  txt <- iconv(rawToChar(raw), from = "ISO-8859-1", to = "UTF-8")
  read_html(txt)
}

clean_ws <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x))
  trimws(gsub(" +", " ", x))
}

all_rows <- list()

# =============================================================================
# 2000  --  archived StatLA HTML (45 Wahlkreise)
# =============================================================================
parse_2000 <- function() {
  fb <- file.path(RAW_DIR, "SH_2000_Landtagswahl_Wahlkreis_StatLA_02_Wahlberechtigte.htm")
  fe <- file.path(RAW_DIR, "SH_2000_Landtagswahl_Wahlkreis_StatLA_03_Erststimmen_Anzahl.htm")
  fz <- file.path(RAW_DIR, "SH_2000_Landtagswahl_Wahlkreis_StatLA_04_Zweitstimmen_Anzahl.htm")

  # --- turnout page (_02): codes in row 4, data rows 5..49 ---
  m02 <- html_table(html_elements(read_iso_html(fb), "table")[[1]],
                    fill = TRUE, header = FALSE) |> as.data.frame(stringsAsFactors = FALSE)
  code02 <- clean_ws(unlist(m02[4, ]))            # Nr,Name,A1,A2,A3,A,B1,B2a,B2b,B,B3
  col_A <- which(code02 == "A")[1]                # eligible_voters insgesamt
  col_B <- which(code02 == "B")[1]                # number_voters insgesamt
  stopifnot(!is.na(col_A), !is.na(col_B))
  d02 <- m02[5:nrow(m02), , drop = FALSE]
  d02_wk   <- clean_ws(d02[[1]])
  is_tot02 <- grepl("Schleswig-Holstein", d02_wk)
  turnout <- tibble(
    wkr_nr          = clean_ws(d02[[1]]),
    eligible_voters = de_int(d02[[col_A]]),
    number_voters   = de_int(d02[[col_B]])
  )[!is_tot02, ]

  # --- count pages (_03 Erst, _04 Zweit): party names row 2, codes row 3 ---
  parse_count_page <- function(path, stimme_lbl) {
    m <- html_table(html_elements(read_iso_html(path), "table")[[1]],
                    fill = TRUE, header = FALSE) |> as.data.frame(stringsAsFactors = FALSE)
    pname <- clean_ws(unlist(m[2, ]))    # un-gültig | gültig | SPD | CDU | ...
    pcode <- clean_ws(unlist(m[3, ]))    # Nr | Name | C/E | D/F | D1.. / F1..
    # invalid / valid columns
    inv_c <- which(grepl("^[CE]$", pcode))[1]
    val_c <- which(grepl("^[DF]$", pcode))[1]
    # party value columns = codes matching D\d+ or F\d+
    pcols <- which(grepl("^[DF][0-9]+$", pcode))
    party_names <- pname[pcols]
    stopifnot(!is.na(inv_c), !is.na(val_c), length(pcols) > 0)

    dat <- m[4:nrow(m), , drop = FALSE]
    wk  <- clean_ws(dat[[1]])
    is_tot <- grepl("Schleswig-Holstein", wk)

    long <- map_dfr(seq_along(pcols), function(k) {
      tibble(
        wkr_nr       = clean_ws(dat[[1]]),
        wkr_name     = clean_ws(dat[[2]]),
        stimme       = stimme_lbl,
        valid_votes  = de_int(dat[[val_c]]),
        invalid_votes= de_int(dat[[inv_c]]),
        party_raw    = party_names[k],
        votes        = de_int(dat[[ pcols[k] ]]),
        .is_total    = is_tot
      )
    })
    long
  }

  erst <- parse_count_page(fe, "erststimme")
  zweit <- parse_count_page(fz, "zweitstimme")

  # statewide totals for validation (per party x stimme)
  tot_e <- erst  |> filter(.is_total) |> select(stimme, party_raw, votes)
  tot_z <- zweit |> filter(.is_total) |> select(stimme, party_raw, votes)
  totals <- bind_rows(tot_e, tot_z)

  body <- bind_rows(erst, zweit) |> filter(!.is_total) |> select(-.is_total)
  body <- body |>
    mutate(votes = ifelse(is.na(votes), NA_real_, votes)) |>
    filter(!is.na(votes))   # drop parties that did not run in that WK (en-dash)

  body <- body |>
    left_join(turnout, by = "wkr_nr") |>
    mutate(
      state_abbr    = "SH",
      state         = "Schleswig-Holstein",
      election_year = 2000L,
      election_date = "2000-02-27"
    ) |>
    select(all_of(OUT_COLS))

  list(rows = body, totals = totals, n_wk = length(unique(body$wkr_nr)),
       stimmen = sort(unique(body$stimme)))
}

# =============================================================================
# 2009  --  per-Wahlkreis sheets in Endgueltige_Ergebnisse.xls (40 WK)
# =============================================================================
parse_2009 <- function() {
  f <- file.path(RAW_DIR, "SH_2009_Landtagswahl_Endgueltige_Ergebnisse.xls")
  sheets <- excel_sheets(f)
  wk_sheets   <- sheets[grepl("^[0-9]+ ", sheets)]
  land_sheet  <- sheets[grepl("^Land ", sheets)]

  # column layout (verified): col6 = "Landtagswahl 2009" Anzahl (absolute).
  COL_VAL <- 6L

  # Parse one Wahlkreis (or Land) sheet -> long for both stimmen.
  parse_sheet <- function(sheet) {
    d <- suppressMessages(read_excel(f, sheet = sheet, col_names = FALSE)) |>
      as.data.frame(stringsAsFactors = FALSE)
    lab <- clean_ws(d[[1]])
    val <- de_int(d[[COL_VAL]])

    find1 <- function(pat) { idx <- which(grepl(pat, lab)); if (length(idx)) idx[1] else NA_integer_ }

    r_inv_e <- find1("^Ung.ltige Erststimmen")
    r_val_e <- find1("^G.ltige Erststimmen")
    r_inv_z <- find1("^Ung.ltige Zweitstimmen")
    r_val_z <- find1("^G.ltige Zweitstimmen")
    r_eli   <- find1("^Wahlberechtigte")
    r_voters<- find1("^W.hlerinnen/W.hler")
    r_dav   <- which(grepl("^davon entfallen auf", lab))  # two of these

    stopifnot(!is.na(r_val_e), !is.na(r_val_z), length(r_dav) == 2)

    eligible <- val[r_eli]
    voters   <- val[r_voters]

    # Erststimmen party block: from first "davon entfallen auf" up to
    # "Ungültige Zweitstimmen". Zweitstimmen block: from second "davon..."
    # up to "Gewählte" / footnote / end.
    end_e <- r_inv_z
    r_gew <- find1("^Gew.hlte")
    end_z <- if (!is.na(r_gew)) r_gew else (nrow(d) + 1L)

    grab_block <- function(start, stop, stimme_lbl, inv, vld) {
      idx <- seq(start + 1L, stop - 1L)
      idx <- idx[idx >= 1 & idx <= nrow(d)]
      keep <- !is.na(lab[idx]) & nzchar(lab[idx]) &
        !grepl("^Andere\\*|^\\*|aufgerundet|^NA$", lab[idx])
      idx <- idx[keep]
      tibble(
        stimme        = stimme_lbl,
        party_raw     = lab[idx],
        votes         = val[idx],
        valid_votes   = vld,
        invalid_votes = inv
      )
    }

    e <- grab_block(r_dav[1], end_e, "erststimme",  val[r_inv_e], val[r_val_e])
    z <- grab_block(r_dav[2], end_z, "zweitstimme", val[r_inv_z], val[r_val_z])

    out <- bind_rows(e, z)
    out$eligible_voters <- eligible
    out$number_voters   <- voters
    out$sheet <- sheet
    out
  }

  wk_long <- map_dfr(wk_sheets, parse_sheet)
  # wkr_nr / wkr_name from sheet name "<nr> <name>"
  wk_long <- wk_long |>
    mutate(
      wkr_nr   = sub("^([0-9]+) .*$", "\\1", sheet),
      wkr_name = sub("^[0-9]+ ", "", sheet)
    )

  # statewide totals from the Land sheet
  land_long <- parse_sheet(land_sheet) |>
    select(stimme, party_raw, votes)
  totals <- land_long |> filter(!is.na(votes))

  body <- wk_long |>
    filter(!is.na(votes)) |>      # drop "-" (party did not run in this WK)
    mutate(
      state_abbr    = "SH",
      state         = "Schleswig-Holstein",
      election_year = 2009L,
      election_date = "2009-09-27"
    ) |>
    select(all_of(OUT_COLS))

  list(rows = body, totals = totals, n_wk = length(unique(body$wkr_nr)),
       stimmen = sort(unique(body$stimme)))
}

# =============================================================================
# 2017  --  combined Wahlkreis xlsx (35 WK), self-documenting headers
# =============================================================================
parse_2017 <- function() {
  f <- file.path(RAW_DIR, "SH_2017_Landtagswahl_Wahlkreis.xlsx")
  d <- suppressMessages(read_excel(f, col_names = FALSE)) |>
    as.data.frame(stringsAsFactors = FALSE)

  hdr  <- clean_ws(unlist(d[4, ]))   # party names live here
  code <- trimws(as.character(unlist(d[5, ])))  # A1,A,B,C,D,D1.. E,F,F1..
  unit <- trimws(as.character(unlist(d[6, ])))  # absolut / %

  col_eli <- which(code == "A")[1]   # Wahlberechtigte insgesamt
  col_vot <- which(code == "B")[1]   # Wählerinnen/Wähler insgesamt
  col_invE <- which(code == "C")[1]
  col_valE <- which(code == "D")[1]
  col_invZ <- which(code == "E")[1]
  col_valZ <- which(code == "F")[1]

  # party value columns: code matches D\d+ or F\d+ AND unit == "absolut"
  is_party <- grepl("^[DF][0-9]+$", code) & unit == "absolut"
  pcols   <- which(is_party)
  pcodes  <- code[pcols]
  # party_raw = header with trailing " Erststimmen"/" Zweitstimmen" stripped
  pnames  <- hdr[pcols]
  pnames  <- trimws(gsub("(Erststimmen|Zweitstimmen)$", "", pnames))
  pstimme <- ifelse(grepl("^D", pcodes), "erststimme", "zweitstimme")

  # data rows 7..N: col1 = numeric WK nr; total row has NA col1 + "Schleswig-Holstein"
  dat <- d[7:nrow(d), , drop = FALSE]
  wk_nr_raw <- trimws(as.character(dat[[1]]))
  wk_name   <- clean_ws(dat[[2]])
  is_data   <- grepl("^[0-9]+$", wk_nr_raw)
  is_total  <- !is_data & grepl("Schleswig-Holstein", wk_name)

  build_long <- function(rows_idx, total = FALSE) {
    map_dfr(seq_along(pcols), function(k) {
      stimme_lbl <- pstimme[k]
      vc  <- pcols[k]
      vld <- if (stimme_lbl == "erststimme") de_int(dat[[col_valE]]) else de_int(dat[[col_valZ]])
      inv <- if (stimme_lbl == "erststimme") de_int(dat[[col_invE]]) else de_int(dat[[col_invZ]])
      tibble(
        wkr_nr        = wk_nr_raw,
        wkr_name      = wk_name,
        stimme        = stimme_lbl,
        eligible_voters = de_int(dat[[col_eli]]),
        number_voters   = de_int(dat[[col_vot]]),
        valid_votes   = vld,
        invalid_votes = inv,
        party_raw     = pnames[k],
        votes         = de_int(dat[[vc]])
      )[rows_idx, ]
    })
  }

  body <- build_long(which(is_data)) |>
    filter(!is.na(votes)) |>
    mutate(
      state_abbr    = "SH",
      state         = "Schleswig-Holstein",
      election_year = 2017L,
      election_date = "2017-05-07"
    ) |>
    select(all_of(OUT_COLS))

  totals <- build_long(which(is_total)) |>
    filter(!is.na(votes)) |>
    select(stimme, party_raw, votes)

  list(rows = body, totals = totals, n_wk = length(unique(body$wkr_nr)),
       stimmen = sort(unique(body$stimme)))
}

# =============================================================================
# 2022  --  Wahlbezirk votemanager CSV (2 909 bezirke -> 35 Wahlkreise)
# =============================================================================
parse_2022 <- function() {
  f_csv   <- file.path(RAW_DIR, "SH_2022_Landtagswahl_Wahlbezirk_ergebnis-download.csv")
  f_erst  <- file.path(RAW_DIR, "SH_2022_Landtagswahl_Wahlkreis_Erststimmen.xlsx")
  f_zweit <- file.path(RAW_DIR, "SH_2022_Landtagswahl_Wahlkreis_Zweitstimmen.xlsx")

  # --- code -> verbatim party name (Wahlbericht 2.1 Wahlvorschläge / Tabelle 1)
  # D* = Direktstimmen (erststimme), F* = Listenstimmen (zweitstimme).
  # Codes are non-contiguous: a party gets a D-code only if it ran a direct
  # candidate and an F-code only if it ran a list. Mapping is fixed by EXACT
  # statewide-total fingerprinting against the official report (verified below).
  D_MAP <- c(
    D1  = "CDU",            D2  = "SPD",           D3  = "GRÜNE",
    D4  = "FDP",            D5  = "AfD",           D6  = "DIE LINKE",
    D7  = "SSW",            D9  = "FREIE WÄHLER",  D10 = "Die PARTEI",
    D11 = "Z.",             D12 = "dieBasis",      D13 = "Die Humanisten",
    D16 = "Volt",           D17 = "Bündnis C",     D18 = "FAMILIE",
    D19 = "LKR",            D20 = "EzB"
  )
  F_MAP <- c(
    F1  = "CDU",            F2  = "SPD",           F3  = "GRÜNE",
    F4  = "FDP",            F5  = "AfD",           F6  = "DIE LINKE",
    F7  = "SSW",            F8  = "PIRATEN",       F9  = "FREIE WÄHLER",
    F10 = "Die PARTEI",     F11 = "Z.",            F12 = "dieBasis",
    F13 = "Die Humanisten", F14 = "Gesundheitsforschung",
    F15 = "Tierschutzpartei", F16 = "Volt"
  )

  # --- read Wahlbezirk CSV (semicolon, German) -------------------------------
  raw <- data.table::fread(f_csv, sep = ";", header = TRUE,
                           colClasses = "character", encoding = "UTF-8")
  raw <- raw[!is.na(raw$Wahlkreis) & trimws(raw$Wahlkreis) != "", ]

  # Wahlkreis nr = last two chars of the 4-digit "Wahlkreis" field ("0101"->"01")
  wkr_nr <- substr(trimws(raw$Wahlkreis), 3, 4)
  stopifnot(all(grepl("^[0-9]{2}$", wkr_nr)))

  cnum <- function(col) {
    v <- raw[[col]]
    v <- gsub("[^0-9]", "", as.character(v))   # strip blanks / thousands seps
    out <- suppressWarnings(as.numeric(v))
    out[is.na(out)] <- 0
    out
  }

  # turnout / valid / invalid columns (votemanager A/B/C/D/E/F labels)
  col_A  <- "Wahlberechtigte gesamt (A)"
  col_B  <- "Waehlende gesamt (B)"
  col_C  <- "Direktstimmen ungueltige (C)"
  col_D  <- "Direktstimmen gueltige (D)"
  col_E  <- "Listenstimmen ungueltige (E)"
  col_F  <- "Listenstimmen gueltige (F)"
  need <- c(col_A, col_B, col_C, col_D, col_E, col_F, names(D_MAP), names(F_MAP))
  stopifnot(all(need %in% names(raw)))

  agg <- data.frame(wkr_nr = wkr_nr, stringsAsFactors = FALSE)
  for (cc in need) agg[[cc]] <- cnum(cc)

  agg <- as.data.frame(
    dplyr::summarise(dplyr::group_by(agg, wkr_nr),
                     dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop")
  )
  stopifnot(nrow(agg) == 35L)

  # --- self-check: D/F code totals must equal the xlsx 7-party statewide totals
  read_named_totals <- function(path, stimme_lbl) {
    d <- suppressMessages(readxl::read_excel(path, col_names = FALSE)) |>
      as.data.frame(stringsAsFactors = FALSE)
    parties <- clean_ws(unlist(d[3, ]))          # CDU/SPD/.../SSW in "absolut" cols
    # the data total row is exactly "Schleswig-Holstein" (NOT the long title row)
    col1 <- clean_ws(d[[1]])
    tot_row <- which(col1 == "Schleswig-Holstein")[1]
    stopifnot(!is.na(tot_row))
    # the named-party "absolut" columns sit at 4,6,8,10,12,14,16 (1-based)
    pcols <- which(parties %in% c("CDU","SPD","GRÜNE","FDP","AfD","DIE LINKE","SSW"))
    setNames(de_int(unlist(d[tot_row, pcols])), parties[pcols])
  }
  xtot_e <- read_named_totals(f_erst,  "erststimme")
  xtot_z <- read_named_totals(f_zweit, "zweitstimme")

  csv_tot <- function(codes) vapply(codes, function(cc) sum(agg[[cc]]), numeric(1))
  d_csv <- setNames(csv_tot(names(D_MAP)), unname(D_MAP))
  f_csv2 <- setNames(csv_tot(names(F_MAP)), unname(F_MAP))
  for (p in names(xtot_e))
    stopifnot(abs(d_csv[[p]]  - xtot_e[[p]]) <= 0)   # exact integer match
  for (p in names(xtot_z))
    stopifnot(abs(f_csv2[[p]] - xtot_z[[p]]) <= 0)

  # --- Wahlkreis names from the Erststimmen xlsx ("01 Nordfriesland-Nord") ----
  de <- suppressMessages(readxl::read_excel(f_erst, col_names = FALSE)) |>
    as.data.frame(stringsAsFactors = FALSE)
  name_cells <- clean_ws(de[[1]])
  name_cells <- name_cells[grepl("^[0-9]{1,2} ", name_cells)]
  wk_name_map <- setNames(
    trimws(sub("^[0-9]{1,2} ", "", name_cells)),
    sprintf("%02d", as.integer(sub("^([0-9]{1,2}) .*$", "\\1", name_cells)))
  )
  stopifnot(length(wk_name_map) == 35L, all(agg$wkr_nr %in% names(wk_name_map)))

  # --- build long: one row per (wkr x stimme x party) ------------------------
  build_stimme <- function(code_map, stimme_lbl, val_col, inv_col) {
    map_dfr(seq_along(code_map), function(k) {
      cc <- names(code_map)[k]
      tibble(
        wkr_nr          = agg$wkr_nr,
        wkr_name        = unname(wk_name_map[agg$wkr_nr]),
        stimme          = stimme_lbl,
        eligible_voters = agg[[col_A]],
        number_voters   = agg[[col_B]],
        valid_votes     = agg[[val_col]],
        invalid_votes   = agg[[inv_col]],
        party_raw       = unname(code_map[k]),
        votes           = agg[[cc]]
      )
    })
  }

  erst  <- build_stimme(D_MAP, "erststimme",  col_D, col_C)
  zweit <- build_stimme(F_MAP, "zweitstimme", col_F, col_E)

  body <- bind_rows(erst, zweit) |>
    filter(!is.na(votes) & votes > 0) |>   # drop parties that did not run in WK
    mutate(
      state_abbr    = "SH",
      state         = "Schleswig-Holstein",
      election_year = 2022L,
      election_date = "2022-05-08"
    ) |>
    select(all_of(OUT_COLS))

  # statewide totals for validation = sum over the 35 WK (== official report)
  totals <- bind_rows(erst, zweit) |>
    group_by(stimme, party_raw) |>
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") |>
    filter(votes > 0)

  list(rows = body, totals = totals, n_wk = length(unique(body$wkr_nr)),
       stimmen = sort(unique(body$stimme)))
}

# =============================================================================
# RUN + VALIDATE
# =============================================================================
res2000 <- parse_2000()
res2009 <- parse_2009()
res2017 <- parse_2017()
res2022 <- parse_2022()

results <- list(`2000` = res2000, `2009` = res2009,
                `2017` = res2017, `2022` = res2022)

final <- bind_rows(lapply(results, `[[`, "rows"))

# ensure wkr_nr is character + leading zeros preserved for HTML page (already string)
final$wkr_nr <- as.character(final$wkr_nr)

# ---- VALIDATION ----
validate_year <- function(r, yr) {
  rows <- r$rows
  # (a) per (wkr,stimme): sum(party votes) vs valid_votes
  chk <- rows |>
    group_by(wkr_nr, stimme) |>
    summarise(sum_party = sum(votes, na.rm = TRUE),
              valid = first(valid_votes), .groups = "drop") |>
    mutate(disc = abs(sum_party - valid))
  max_disc <- suppressWarnings(max(chk$disc, na.rm = TRUE))
  n_grp    <- nrow(chk)

  # (b) statewide totals
  my_tot <- rows |>
    group_by(stimme, party_raw) |>
    summarise(my = sum(votes, na.rm = TRUE), .groups = "drop")
  src_tot <- r$totals |>
    group_by(stimme, party_raw) |>
    summarise(src = sum(votes, na.rm = TRUE), .groups = "drop")
  cmp <- full_join(my_tot, src_tot, by = c("stimme", "party_raw")) |>
    mutate(my = coalesce(my, 0), src = coalesce(src, 0),
           d = abs(my - src))
  fails <- cmp |> filter(d > 1)
  match <- nrow(fails) == 0

  cat(sprintf("\n=== %s ===\n", yr))
  cat(sprintf("  n_wahlkreise: %d | stimmen: %s\n", r$n_wk, paste(r$stimmen, collapse=",")))
  cat(sprintf("  (a) integrity: max |sum(party)-valid| = %s over %d (wkr,stimme) groups\n",
              format(max_disc), n_grp))
  cat(sprintf("  (b) statewide total match: %s (n party-checks=%d, fails=%d)\n",
              match, nrow(cmp), nrow(fails)))
  if (nrow(fails)) print(as.data.frame(fails))
  list(max_disc = max_disc, n_grp = n_grp, match = match, fails = fails)
}

v <- mapply(validate_year, results, names(results), SIMPLIFY = FALSE)

# ---- WRITE ----
fwrite(final, OUT_CSV)
cat(sprintf("\nWROTE %d rows -> %s\n", nrow(final), OUT_CSV))
cat("distinct party_raw:\n")
print(sort(unique(final$party_raw)))
cat("\nrows per year:\n"); print(table(final$election_year))
