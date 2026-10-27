# Rheinland-Pfalz direct elections (Statistisches Landesamt RLP), shared by
# 01_mayoral_unharm.R and 01b_mayoral_candidates.R.
#
# Two editions of the StaLA workbook list every direct election since 1994 of the
# Landräte, the Oberbürgermeister of the kreisfreie Städte and the Bürgermeister
# of the verbandsfreie Gemeinden and Verbandsgemeinden:
#   RLP_FILE_2025  delivered 19.12.2025, elections to 29.06.2025. Every candidate
#                  with first-round and runoff shares. The base of the series.
#   RLP_FILE_2026  delivered 12.10.2026 (Az. 111-AD26-1436), elections to
#                  13.09.2026, the ELECTED PERSON ONLY, plus term start/end.
# Neither edition is clean. The 2026 edition corrects some 2025-edition errors
# and introduces its own (wrong codes, dates and names, share and turnout
# swapped), so it is not a replacement: it contributes the elections the 2025
# edition does not have (rlp_additions_2026), and the 2025-edition errors it
# exposed are pinned cell by cell in metadata/rlp_source_corrections.csv, each
# with its evidence (rlp_read_sheet). Known issues §18.

RLP_FILE_2025 <- "data/mayoral_elections/raw/rlp/20251219_Heddesheimer_Direktwahlen 1994-2024.xlsx"
RLP_FILE_2026 <- "data/mayoral_elections/raw/rlp/111-AD26-1436_Heddesheimer_Princeton_Landräte und hauptamtliche Bürgermeister_Stand 12.10.2026.xlsx"
RLP_PIN_FILE <- "data/mayoral_elections/metadata/rlp_source_corrections.csv"
RLP_2025_LAST_DATE <- as.Date("2025-06-29")   # last election in the 2025 edition

# Elections before RLP_2025_LAST_DATE that only the 2026 edition has.
RLP_2026_FILL_GAPS <- data.frame(
  office = "vg", ags = "07013703", election_date = as.Date("2024-04-07"),
  why = "VG Vordereifel, Schomisch (CDU) 70.3 %, term from 01.01.2025; absent from the 2025 edition"
)

# A cell typed as text ("27.03.2011") arrives from read_excel() as NA in a date
# column. The 2025 edition types eight Verbandsgemeinde elections held with the
# Landtagswahl of 27.03.2011 that way; until October 2026 they took the previous
# election's date (07.11.2010) from the fill-down in the parsers.
rlp_parse_date <- function(x) {
  x <- trimws(x)
  out <- as.Date(rep(NA_character_, length(x)))
  serial <- !is.na(x) & grepl("^[0-9]{5}(\\.0+)?$", x)
  out[serial] <- as.Date(as.numeric(x[serial]), origin = "1899-12-30")
  dmy <- !is.na(x) & grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$", x)
  out[dmy] <- as.Date(x[dmy], format = "%d.%m.%Y")
  out
}

rlp_code_chr <- function(x) {
  if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else gsub("\\s", "", as.character(x))
}

# Read one sheet of the 2025 edition as the parsers expect it, with text-typed
# dates parsed and the pinned cell corrections applied. `cols` maps the roles
# wahltag, stichwahltag, schluessel, kandidat, ergebnis_hw, ergebnis_sw to the
# sheet's column positions.
rlp_read_sheet <- function(sheet, cols) {
  raw <- suppressMessages(suppressWarnings(read_excel(RLP_FILE_2025, sheet = sheet, skip = 4, col_names = FALSE)))
  txt <- suppressMessages(read_excel(RLP_FILE_2025, sheet = sheet, skip = 4, col_names = FALSE, col_types = "text"))
  stopifnot(nrow(raw) == nrow(txt))
  for (k in c(cols$wahltag, cols$stichwahltag)) {
    d <- rlp_parse_date(txt[[k]])
    fix <- is.na(raw[[k]]) & !is.na(d)
    if (any(fix)) raw[[k]][fix] <- as.POSIXct(format(d[fix]), tz = "UTC")
  }
  pins <- read.csv(RLP_PIN_FILE, colClasses = "character", encoding = "UTF-8")
  pins <- pins[pins$sheet == sheet, , drop = FALSE]
  if (!nrow(pins)) return(raw)
  key <- !is.na(raw[[cols$schluessel]])
  gid <- cumsum(key)
  for (i in seq_len(nrow(pins))) {
    p <- pins[i, ]
    code <- rlp_code_chr(raw[[cols$schluessel]])
    wt <- as.Date(raw[[cols$wahltag]])
    g <- which(key & code == gsub("\\s", "", p$code) & wt %in% as.Date(p$election_date))
    if (length(g) != 1) stop(sprintf("RLP pin %s %s %s: %d elections match", sheet, p$code, p$election_date, length(g)))
    rows <- which(gid == gid[g])
    r <- if (nzchar(p$candidate)) {
      rows[!is.na(raw[[cols$kandidat]][rows]) & startsWith(as.character(raw[[cols$kandidat]][rows]), p$candidate)]
    } else g
    if (length(r) != 1) stop(sprintf("RLP pin %s %s %s %s: %d rows match", sheet, p$code, p$election_date, p$candidate, length(r)))
    col <- cols[[p$field]]
    old <- raw[[col]][r]
    same <- if (inherits(old, "POSIXct")) {
      identical(format(as.Date(old)), p$old_value)
    } else if (is.numeric(old)) {
      if (is.na(old)) !nzchar(p$old_value) else nzchar(p$old_value) && abs(old - as.numeric(p$old_value)) < 1e-9
    } else {
      identical(gsub("\\s", "", dplyr::coalesce(as.character(old), "")), gsub("\\s", "", p$old_value))
    }
    if (!isTRUE(same)) stop(sprintf("RLP pin %s %s %s %s %s: source holds %s, not %s",
                                    sheet, p$code, p$election_date, p$candidate, p$field, format(old), p$old_value))
    raw[[col]][r] <- if (!nzchar(p$new_value)) NA                       # blank the cell
                     else if (inherits(old, "POSIXct")) as.POSIXct(p$new_value, tz = "UTC")
                     else if (is.numeric(old)) as.numeric(p$new_value) else p$new_value
  }
  cat(sprintf("  %s: %d pinned cell correction(s) applied\n", sheet, nrow(pins)))
  raw
}

rlp_ags <- function(office, code) {
  code <- gsub("\\D", "", code)
  dplyr::case_when(
    office == "landrat" ~ paste0("07", stringr::str_pad(code, 3, "left", "0"), "000"),
    office == "ob"      ~ paste0("07", substr(code, 1, 3), "000"),
    office == "vg"      ~ paste0("07", stringr::str_pad(code, 6, "left", "0")),
    office == "vfr"     ~ paste0("07", substr(code, 1, 3), substr(code, nchar(code) - 2, nchar(code))))
}

# The elections of the 2026 edition that the 2025 edition lacks: everything after
# RLP_2025_LAST_DATE plus RLP_2026_FILL_GAPS. One row per election, the elected
# person only; `pct` is the share with which they were elected (the runoff's if
# `runoff`). `known` = the (office, ags) pairs of the 2025 edition: a unit outside
# it, a date that is not a Sunday, an implausible runoff or a minority winner
# stops the build, since the 2026 edition has typos in codes and dates.
rlp_additions_2026 <- function(known) {
  sheets <- c(landrat = "Landräte", ob = "Oberbürgermeister Krfr.Städte",
              vfr = "Bürgermeister Vfr.Gemeinden", vg = "Bürgermeister Verbandsgemei")
  out <- list()
  for (off in names(sheets)) {
    t <- as.data.frame(suppressMessages(read_excel(RLP_FILE_2026, sheet = sheets[[off]], col_names = FALSE, col_types = "text")))
    if (ncol(t) < 16) t[(ncol(t) + 1):16] <- NA_character_
    # the women marker is headed "1 = Frau": column 14 on two sheets, 15 on two
    col_f <- which(vapply(t[1:6, ], function(x) any(trimws(x) %in% "1 = Frau"), logical(1)))
    stopifnot(length(col_f) == 1)
    d <- rlp_parse_date(t[[1]])
    t <- t[!is.na(d), ]; d <- d[!is.na(d)]
    out[[off]] <- data.frame(
      office = off, election_date = d, stichwahl_date = rlp_parse_date(t[[2]]),
      code = gsub("\\D", "", t[[3]]), ags_name = trimws(t[[4]]),
      candidate_name = trimws(gsub("\\s+", " ", t[[5]])), candidate_party = trimws(t[[6]]),
      pct = suppressWarnings(as.numeric(t[[7]])), runoff = tolower(trimws(dplyr::coalesce(t[[8]], ""))) == "x",
      turnout_hw = suppressWarnings(as.numeric(t[[9]])), turnout_sw = suppressWarnings(as.numeric(t[[10]])),
      female = trimws(dplyr::coalesce(t[[col_f]], "")) == "1", stringsAsFactors = FALSE)
  }
  a <- dplyr::bind_rows(out)
  a$ags <- rlp_ags(a$office, a$code)
  gap <- paste(a$office, a$ags, a$election_date) %in%
    paste(RLP_2026_FILL_GAPS$office, RLP_2026_FILL_GAPS$ags, RLP_2026_FILL_GAPS$election_date)
  a <- a[a$election_date > RLP_2025_LAST_DATE | gap, ]
  a$stichwahl_date[!a$runoff] <- as.Date(NA)
  a$turnout_sw[!a$runoff] <- NA_real_
  a$candidate_gender <- ifelse(a$female, "w", "m")
  a$election_type <- c(landrat = "Landratswahl", ob = "Oberbürgermeisterwahl",
                       vfr = "Bürgermeisterwahl", vg = "VG-Bürgermeisterwahl")[a$office]
  bad <- c(
    if (sum(gap) != nrow(RLP_2026_FILL_GAPS)) "a pinned gap election is missing from the 2026 edition",
    if (any(format(a$election_date, "%u") != "7")) paste("not a Sunday:", paste(a$ags_name[format(a$election_date, "%u") != "7"], collapse = ", ")),
    if (any(a$runoff & (is.na(a$stichwahl_date) | format(a$stichwahl_date, "%u") != "7" |
                        !as.numeric(a$stichwahl_date - a$election_date) %in% c(14, 21))))
      "a runoff is not 14 or 21 days after its first round",
    if (any(is.na(a$pct) | a$pct <= 50 | a$pct > 100)) "a winner without a majority or a share",
    if (any(!paste(a$office, a$ags) %in% paste(known$office, known$ags)))
      paste("unit not in the 2025 edition:", paste(a$ags[!paste(a$office, a$ags) %in% paste(known$office, known$ags)], collapse = ", ")),
    if (anyDuplicated(a[c("ags", "election_date")])) "an election twice")
  if (length(bad)) stop("RLP 2026 edition: ", paste(bad, collapse = "; "))
  cat(sprintf("  2026 edition: %d elections the 2025 edition lacks (%s)\n", nrow(a),
              paste(names(table(a$office)), table(a$office), sep = " ", collapse = ", ")))
  a[order(a$office, a$ags, a$election_date), ]
}
