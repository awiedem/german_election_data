# =============================================================================
# Stage-1 cleaning parser: Niedersachsen (NI) Landtagswahl, Wahlkreis level
# Vote system: erststimme + zweitstimme
# Machine-readable years parsed: 1998, 2003 (HTML tar.gz), 2013, 2017, 2022 (CSV+XML)
# PDFs (2008, + StatBerichte) are EXCLUDED (deferred to OCR stage).
# Output: long tidy CSV, one row per (Wahlkreis x stimme x party).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)
library(xml2)

here::i_am("code/state_elections_wahlkreis/parsers/parse_NI.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Niedersachsen")
out_csv <- here("data", "state_elections", "processed", "wahlkreis",
                "NI_ltw_wkr_long.csv")

STATE      <- "Niedersachsen"
STATE_ABBR <- "NI"

# Well-known NI Landtagswahl dates
ELECTION_DATES <- c(
  "1998" = "1998-03-01",
  "2003" = "2003-02-02",
  "2013" = "2013-01-20",
  "2017" = "2017-10-15",
  "2022" = "2022-10-09"
)

OUT_COLS <- c("state_abbr","state","election_year","election_date","wkr_nr",
              "wkr_name","stimme","eligible_voters","number_voters",
              "valid_votes","invalid_votes","party_raw","votes")

# ---- helpers --------------------------------------------------------------

# parse German-formatted integer ("15.841" -> 15841); "-" / "" / ".." -> NA
de_int <- function(x) {
  x <- trimws(gsub(" ", " ", x))          # nbsp -> space
  x <- gsub("\\s", "", x)                        # drop spaces
  x[x %in% c("", "-", "..", ".")] <- NA
  x <- gsub("\\.", "", x)                        # thousands sep
  suppressWarnings(as.integer(x))
}

collected <- list()

# ===========================================================================
# (A) CSV + XML based years: 2013, 2017, 2022
#     CSV = clean per-Wahlkreis party votes & counts (sums == valid_votes).
#     XML = wkr names, invalid votes (per stimme), and the 000 statewide row.
# ===========================================================================

# Parse the current-year <wahl> block of an LSN XML.
# Returns list(per_wk = data.frame, statewide = named list).
parse_ni_xml <- function(xml_path, year) {
  doc <- read_xml(xml_path)
  # current election = the <wahl> whose jahr matches the 2-digit year
  yy <- substr(year, 3, 4)
  wahl_nodes <- xml_find_all(doc, "/wahlapplet/wahl")
  jahr <- xml_attr(wahl_nodes, "jahr")
  cur <- wahl_nodes[jahr == yy]
  if (length(cur) != 1) stop(sprintf("XML %s: expected 1 wahl block jahr=%s, got %d",
                                      xml_path, yy, length(cur)))
  gebiete <- xml_find_all(cur, "./gebiet")

  rows <- list()
  statewide <- NULL
  for (g in gebiete) {
    schl <- xml_attr(g, "schluessel")
    txt  <- xml_attr(g, "text")
    # geographie polygon gebiete have no text attr -> skip
    if (is.na(txt) || txt == "") next
    # value gebiete: per-WK or 000
    wb  <- xml_find_first(g, "./wahlberechtigte") |> xml_text()
    wa  <- xml_find_first(g, "./waehler") |> xml_text()
    g_e <- xml_find_first(g, "./ergebnis[@kurz='GSTKAND']/stimmen") |> xml_text()
    g_z <- xml_find_first(g, "./ergebnis[@kurz='GSTWVT']/stimmen")  |> xml_text()
    u_e <- xml_find_first(g, "./ergebnis[@kurz='USTKAND']/stimmen") |> xml_text()
    u_z <- xml_find_first(g, "./ergebnis[@kurz='USTWVT']/stimmen")  |> xml_text()

    wvts <- xml_find_all(g, "./wvt")
    p_kurz <- xml_attr(wvts, "kurz")
    p_erst <- sapply(wvts, function(w)
      xml_text(xml_find_first(w, "./ergebnis[@kurz='STKAND']/stimmen")))
    p_zweit <- sapply(wvts, function(w)
      xml_text(xml_find_first(w, "./ergebnis[@kurz='STWVT']/stimmen")))

    rec <- list(
      schl = schl, text = txt,
      wb = de_int(wb), wa = de_int(wa),
      ge = de_int(g_e), gz = de_int(g_z),
      ue = de_int(u_e), uz = de_int(u_z),
      parties = data.frame(party = p_kurz,
                           erst = de_int(p_erst),
                           zweit = de_int(p_zweit),
                           stringsAsFactors = FALSE)
    )
    if (schl == "000") statewide <- rec else rows[[length(rows) + 1]] <- rec
  }
  list(rows = rows, statewide = statewide)
}

parse_csv_year <- function(year) {
  csv_path <- file.path(raw_dir, sprintf("NI_%s_Landtagswahl_Wahlkreis.csv", year))
  xml_path <- file.path(raw_dir, sprintf("NI_%s_Landtagswahl_Wahlkreis.xml", year))
  edate <- ELECTION_DATES[[as.character(year)]]

  dt <- fread(csv_path, sep = ";", header = TRUE, encoding = "UTF-8",
              colClasses = "character")
  # drop trailing empty column from terminal ';'
  dt <- dt[, which(names(dt) != "" & !is.na(names(dt))), with = FALSE]

  hdr <- names(dt)
  erst_cols  <- hdr[grepl(" I$",  hdr)]
  zweit_cols <- hdr[grepl(" II$", hdr)]
  erst_party  <- sub(" I$",  "", erst_cols)
  zweit_party <- sub(" II$", "", zweit_cols)
  # Einzelbewerber columns ("Einzelbewerber 1/2") are Erststimme-only direct
  # candidates with no " I"/" II" suffix; keep them as erststimme parties.
  eb_cols <- hdr[grepl("^Einzelbewerber [0-9]+$", hdr)]
  erst_cols  <- c(erst_cols,  eb_cols)
  erst_party <- c(erst_party, eb_cols)

  # XML for names + invalid votes
  xml <- parse_ni_xml(xml_path, year)
  name_map <- setNames(
    sub("^Wahlkreis:\\s*\\d+\\s*", "", sapply(xml$rows, `[[`, "text")),
    sprintf("%03d", as.integer(sapply(xml$rows, `[[`, "schl")))
  )
  inv_e_map <- setNames(sapply(xml$rows, `[[`, "ue"),
                        sprintf("%03d", as.integer(sapply(xml$rows, `[[`, "schl"))))
  inv_z_map <- setNames(sapply(xml$rows, `[[`, "uz"),
                        sprintf("%03d", as.integer(sapply(xml$rows, `[[`, "schl"))))

  long <- list()
  for (i in seq_len(nrow(dt))) {
    wkr_nr <- trimws(dt[["Wahlkreis"]][i])
    if (!grepl("^[0-9]+$", wkr_nr)) next   # skip any non-WK row
    wkr_nr <- sprintf("%03d", as.integer(wkr_nr))
    eg <- de_int(dt[["Gültige Erststimmen"]][i])
    zg <- de_int(dt[["Gültige Zweitstimmen"]][i])
    wb <- de_int(dt[["Wahlberechtigte"]][i])
    wa <- de_int(dt[["Wähler"]][i])
    wkr_name <- name_map[[wkr_nr]]
    inv_e <- inv_e_map[[wkr_nr]]; inv_z <- inv_z_map[[wkr_nr]]

    for (k in seq_along(erst_cols)) {
      v <- de_int(dt[[erst_cols[k]]][i])
      if (is.na(v)) next
      long[[length(long) + 1]] <- data.frame(
        state_abbr = STATE_ABBR, state = STATE,
        election_year = as.integer(year), election_date = edate,
        wkr_nr = wkr_nr, wkr_name = wkr_name, stimme = "erststimme",
        eligible_voters = wb, number_voters = wa,
        valid_votes = eg, invalid_votes = inv_e,
        party_raw = erst_party[k], votes = v, stringsAsFactors = FALSE)
    }
    for (k in seq_along(zweit_cols)) {
      v <- de_int(dt[[zweit_cols[k]]][i])
      if (is.na(v)) next
      long[[length(long) + 1]] <- data.frame(
        state_abbr = STATE_ABBR, state = STATE,
        election_year = as.integer(year), election_date = edate,
        wkr_nr = wkr_nr, wkr_name = wkr_name, stimme = "zweitstimme",
        eligible_voters = wb, number_voters = wa,
        valid_votes = zg, invalid_votes = inv_z,
        party_raw = zweit_party[k], votes = v, stringsAsFactors = FALSE)
    }
  }
  df <- rbindlist(long)
  attr(df, "statewide") <- xml$statewide
  df
}

# ===========================================================================
# (B) HTML (tar.gz) years: 1998, 2003
#     Per-WK pages interleave current + previous election in one table:
#       party rows come in PAIRS:
#         row A (7 cells): [party(+candidate), Erst_cur, %, Erst_prev, %, chg, %]
#         row B (6 cells): [Zweit_cur, %, Zweit_prev, %, chg, %]
#     We take ONLY the current-year absolute values (Erst_cur=cellA[2],
#     Zweit_cur=cellB[1]) and EXCLUDE the trailing "Sonstige" aggregate row.
# ===========================================================================

# Canonical party-name prefixes used to strip candidate suffixes (2003).
# Longest-prefix match; 1998 rows have no candidate so they match exactly.
CANON_PARTIES <- c(
  "Sozialdemokratische Partei Deutschlands",
  "Christlich Demokratische Union Deutschlands",
  "BÜNDNIS 90/DIE GRÜNEN", "BÜNDNIS 90/ DIE GRÜNEN",
  "Freie Demokratische Partei",
  "Partei des Demokratischen Sozialismus",
  "Deutsche Kommunistische Partei",
  "DEUTSCHE PARTEI",
  "DIE REPUBLIKANER",
  "Feministische Partei DIE FRAUEN",
  "Ökologisch- Demokratische Partei", "Ökologisch-Demokratische Partei",
  "Partei Bibeltreuer Christen",
  "Soziale Fortschritts - Partei",
  "STATT Partei DIE UNABHÄNGIGEN",
  "Bürgerrechtsbewegung Solidarität",
  "DIE GRAUEN - Graue Panther",
  "FAMILIEN-PARTEI DEUTSCHLANDS",
  "Partei Rechtsstaatlicher Offensive",
  "Einzelbewerberinnen /Einzelbewerber",
  "Einzelbewerberinnen/Einzelbewerber"
)
# order by descending length so longest prefix wins
CANON_PARTIES <- CANON_PARTIES[order(-nchar(CANON_PARTIES))]

clean_party_label <- function(lbl) {
  lbl <- trimws(gsub("\\s+", " ", lbl))
  for (cp in CANON_PARTIES) {
    cpn <- gsub("\\s+", " ", cp)
    if (startsWith(lbl, cpn)) return(cpn)
  }
  # No canonical match: if a candidate ", " is present, cut before it; else verbatim
  lbl
}

# Extract all <tr> rows from an HTML file as a list of character vectors.
html_table_rows <- function(path) {
  doc <- read_html(path, encoding = "ISO-8859-1")
  trs <- xml_find_all(doc, "//tr")
  lapply(trs, function(tr) {
    cells <- xml_find_all(tr, "./td | ./th")
    vapply(cells, function(c) trimws(gsub("\\s+", " ", xml_text(c))), character(1))
  })
}

# Parse one per-WK HTML page; returns long df (erststimme + zweitstimme) for the
# current year, excluding Sonstige. Also returns header counts.
parse_html_wk <- function(path, wkr_nr, year, edate) {
  rows <- html_table_rows(path)
  is_num_label <- function(s) grepl("^[\\d.\\- ]+$", s, perl = TRUE)

  wb <- wa <- inv_e <- inv_z <- val_e <- val_z <- NA_integer_
  wkr_name <- NA_character_
  parties <- list()

  i <- 1
  # find Wahlkreis name
  for (r in rows) {
    if (length(r) >= 1 && grepl("^Wahlkreis:", r[1])) {
      wkr_name <- trimws(sub("^Wahlkreis:\\s*[0-9]+\\s*", "", r[1])); break
    }
  }

  n <- length(rows)
  started <- FALSE  # set TRUE after "Gültige Stimmen" pair -> party block
  k <- 1
  while (k <= n) {
    r <- rows[[k]]
    if (length(r) == 0) { k <- k + 1; next }
    lab <- r[1]

    if (grepl("^Wahlberechtigte", lab))          wb <- de_int(r[2])
    else if (grepl("^Wählerinnen", lab))         wa <- de_int(r[2])
    else if (grepl("^Ungültige Stimmen", lab)) {
      inv_e <- de_int(r[2])
      # next row (6 cells) carries Zweitstimmen ungültig at [1]
      if (k + 1 <= n) inv_z <- de_int(rows[[k + 1]][1])
    }
    else if (grepl("^Gültige Stimmen", lab)) {
      val_e <- de_int(r[2])
      if (k + 1 <= n) val_z <- de_int(rows[[k + 1]][1])
      started <- TRUE
      k <- k + 2; next
    }
    else if (started) {
      # party block: pairs of (7-cell label row, 6-cell zweit row)
      if (grepl("^Quelle", lab)) break
      if (grepl("^Sonstige", lab)) { k <- k + 2; next }  # exclude aggregate
      if (!is_num_label(lab) && lab != "") {
        party <- clean_party_label(lab)
        erst <- if (length(r) >= 2) de_int(r[2]) else NA_integer_
        zweit <- NA_integer_
        # A normal party occupies a PAIR: label-row + a continuation row whose
        # first cell is numeric/"-" (the Zweitstimmen value). Einzelbewerber are
        # Erststimme-only SINGLE rows whose next row is another label (text) row.
        has_pair <- FALSE
        if (k + 1 <= n) {
          rz <- rows[[k + 1]]
          if (length(rz) >= 1 && is_num_label(rz[1])) {
            zweit <- de_int(rz[1]); has_pair <- TRUE
          }
        }
        parties[[length(parties) + 1]] <-
          data.frame(party = party, erst = erst, zweit = zweit,
                     stringsAsFactors = FALSE)
        k <- k + (if (has_pair) 2 else 1); next
      }
    }
    k <- k + 1
  }

  pp <- if (length(parties)) rbindlist(parties) else
        data.table(party = character(), erst = integer(), zweit = integer())

  long <- list()
  for (j in seq_len(nrow(pp))) {
    if (!is.na(pp$erst[j])) long[[length(long) + 1]] <- data.frame(
      stimme = "erststimme", valid_votes = val_e, invalid_votes = inv_e,
      party_raw = pp$party[j], votes = pp$erst[j], stringsAsFactors = FALSE)
    if (!is.na(pp$zweit[j])) long[[length(long) + 1]] <- data.frame(
      stimme = "zweitstimme", valid_votes = val_z, invalid_votes = inv_z,
      party_raw = pp$party[j], votes = pp$zweit[j], stringsAsFactors = FALSE)
  }
  out <- if (length(long)) rbindlist(long) else
         data.table(stimme = character(), valid_votes = integer(),
                    invalid_votes = integer(), party_raw = character(),
                    votes = integer())
  out[, `:=`(state_abbr = STATE_ABBR, state = STATE,
             election_year = as.integer(year), election_date = edate,
             wkr_nr = wkr_nr, wkr_name = wkr_name,
             eligible_voters = wb, number_voters = wa)]
  out
}

parse_html_year <- function(year, archive) {
  edate <- ELECTION_DATES[[as.character(year)]]
  tmp <- file.path(tempdir(), paste0("ni_html_", year))
  unlink(tmp, recursive = TRUE); dir.create(tmp, showWarnings = FALSE)
  untar(file.path(raw_dir, archive), exdir = tmp)
  sub <- list.dirs(tmp, recursive = FALSE)
  sub <- sub[grepl(sprintf("LW%s", year), sub)]
  wk_files <- list.files(sub, pattern = "^[0-9]{3}\\.htm$", full.names = TRUE)
  wk_files <- sort(wk_files)

  out <- list()
  for (f in wk_files) {
    wkr_nr <- sub("\\.htm$", "", basename(f))   # already 3-digit, zero-padded
    out[[length(out) + 1]] <- parse_html_wk(f, wkr_nr, year, edate)
  }
  df <- rbindlist(out, fill = TRUE)
  attr(df, "html_dir") <- sub
  df
}

# ---- run all years --------------------------------------------------------

message("Parsing 2013 / 2017 / 2022 (CSV + XML) ...")
csv_years <- list()
sw_xml <- list()
for (y in c("2013", "2017", "2022")) {
  d <- parse_csv_year(y)
  sw_xml[[y]] <- attr(d, "statewide")
  csv_years[[y]] <- d
}

message("Parsing 1998 (HTML) ...")
ni98 <- parse_html_year("1998", "NI_1998_Landtagswahl_Wahlkreis.tar.gz")
message("Parsing 2003 (HTML) ...")
ni03 <- parse_html_year("2003", "NI_2003_Landtagswahl_Wahlkreis.tar.gz")

# Statewide reference for 1998 from Endergebnis.htm
parse_html_statewide_1998 <- function(html_dir) {
  f <- file.path(html_dir, "Endergebnis.htm")
  if (!file.exists(f)) return(NULL)
  rows <- html_table_rows(f)
  is_num_label <- function(s) grepl("^[\\d.\\- ]+$", s, perl = TRUE)
  started <- FALSE; parties <- list(); val_e <- val_z <- NA_integer_
  n <- length(rows); k <- 1
  while (k <= n) {
    r <- rows[[k]]; if (length(r) == 0) { k <- k + 1; next }
    lab <- r[1]
    if (grepl("^Gültige Stimmen", lab)) {
      val_e <- de_int(r[2]); if (k+1<=n) val_z <- de_int(rows[[k+1]][1])
      started <- TRUE; k <- k + 2; next
    } else if (started) {
      if (grepl("^Quelle", lab)) break
      if (grepl("^Sonstige", lab)) { k <- k + 2; next }
      if (!is_num_label(lab) && lab != "") {
        party <- clean_party_label(lab)
        erst <- if (length(r) >= 2) de_int(r[2]) else NA_integer_
        zweit <- if (k+1<=n && length(rows[[k+1]])>=1) de_int(rows[[k+1]][1]) else NA
        parties[[length(parties)+1]] <- data.frame(party=party, erst=erst, zweit=zweit,
                                                   stringsAsFactors = FALSE)
        k <- k + 2; next
      }
    }
    k <- k + 1
  }
  list(parties = rbindlist(parties), val_e = val_e, val_z = val_z)
}
sw_1998 <- parse_html_statewide_1998(attr(ni98, "html_dir"))

# ---- combine --------------------------------------------------------------

all_df <- rbindlist(c(csv_years, list(ni98, ni03)), use.names = TRUE, fill = TRUE)
setcolorder(all_df, OUT_COLS)
all_df <- all_df[order(election_year, wkr_nr, stimme, party_raw)]

# =================== VALIDATION ============================================

cat("\n================ VALIDATION ================\n")

# (a) per (wkr,stimme): sum(party votes) vs valid_votes
agg <- all_df[, .(sum_votes = sum(votes, na.rm = TRUE),
                  valid = unique(valid_votes)),
              by = .(election_year, wkr_nr, stimme)]
agg[, disc := abs(sum_votes - valid)]
n_checked <- nrow(agg[!is.na(valid)])
max_disc  <- max(agg$disc, na.rm = TRUE)
cat(sprintf("(a) per-(wkr,stimme): groups checked=%d  MAX |sum-valid|=%s\n",
            n_checked, max_disc))
cat("    by year:\n")
print(agg[, .(groups=.N, max_disc=max(disc,na.rm=TRUE)), by=election_year])

# (c) Wahlkreis count per year
cat("\n(c) Wahlkreis count per year:\n")
print(all_df[, .(n_wkr = uniqueN(wkr_nr)), by = election_year])

# (b) statewide totals
cat("\n(b) STATEWIDE TOTAL MATCH:\n")
sw_check <- function(year, ref_parties, ref_val_e, ref_val_z) {
  mine <- all_df[election_year == as.integer(year),
                 .(votes = sum(votes, na.rm = TRUE)),
                 by = .(party_raw, stimme)]
  me <- mine[stimme == "erststimme"]; mz <- mine[stimme == "zweitstimme"]
  # valid totals
  my_ve <- sum(me$votes); my_vz <- sum(mz$votes)
  cat(sprintf("  %s: my valid_erst=%d (ref %s); my valid_zweit=%d (ref %s)\n",
              year, my_ve, ref_val_e, my_vz, ref_val_z))
  ok_e <- isTRUE(abs(my_ve - ref_val_e) <= 2)
  ok_z <- isTRUE(abs(my_vz - ref_val_z) <= 2)
  # per-party comparison.
  # Einzelbewerber are labeled per-candidate in the XML (EB, EB21, ...) but
  # generically in the CSV ("Einzelbewerber 1/2"); compare them in aggregate.
  is_eb <- function(p) grepl("^(EB|Einzelbewerber)", p)
  ref_eb_e <- sum(as.data.table(ref_parties)[is_eb(party), erst], na.rm = TRUE)
  my_eb_e  <- sum(me[is_eb(party_raw), votes], na.rm = TRUE)
  rp <- as.data.table(ref_parties)
  rp <- rp[!is_eb(party)]
  fails <- character()
  if (abs(my_eb_e - ref_eb_e) > 2)
    fails <- c(fails, sprintf("Einzelbewerber(agg) erst mine=%d ref=%d", my_eb_e, ref_eb_e))
  for (p in unique(rp$party)) {
    re <- rp[party == p, erst]; rz <- rp[party == p, zweit]
    mep <- me[party_raw == p, votes]; mzp <- mz[party_raw == p, votes]
    mep <- if (length(mep)) mep else 0L; mzp <- if (length(mzp)) mzp else 0L
    re <- if (length(re) && !is.na(re)) re else 0L
    rz <- if (length(rz) && !is.na(rz)) rz else 0L
    if (abs(mep - re) > 2) fails <- c(fails, sprintf("%s erst mine=%d ref=%d", p, mep, re))
    if (abs(mzp - rz) > 2) fails <- c(fails, sprintf("%s zweit mine=%d ref=%d", p, mzp, rz))
  }
  cat(sprintf("    valid_match=%s ; per-party fails=%d\n",
              ok_e && ok_z, length(fails)))
  if (length(fails)) for (x in fails) cat("      FAIL:", x, "\n")
  list(ok = ok_e && ok_z && length(fails) == 0, fails = fails,
       my_ve = my_ve, my_vz = my_vz)
}

sw_results <- list()
# XML-based years (2013/2017/2022): build ref from statewide wvt (drop "Sonstige")
for (y in c("2013", "2017", "2022")) {
  sw <- sw_xml[[y]]
  rp <- as.data.table(sw$parties)
  rp <- rp[party != "Sonstige"]
  setnames(rp, c("party","erst","zweit"))
  sw_results[[y]] <- sw_check(y, rp, sw$ge, sw$gz)
}
# 1998: Endergebnis.htm
if (!is.null(sw_1998)) {
  rp <- sw_1998$parties; setnames(rp, c("party","erst","zweit"))
  sw_results[["1998"]] <- sw_check("1998", rp, sw_1998$val_e, sw_1998$val_z)
}
# 2003: no statewide source page (404 in archive) -> rely on (a) per-WK integrity
cat("  2003: no statewide reference page in archive (Gesamtergebnis.htm is a 404);")
cat(" validated via per-(wkr,stimme) integrity check (a) only.\n")

# ---- write output ---------------------------------------------------------
fwrite(all_df, out_csv)
cat(sprintf("\nWROTE %d rows -> %s\n", nrow(all_df), out_csv))
cat("Distinct party_raw labels:", uniqueN(all_df$party_raw), "\n")
