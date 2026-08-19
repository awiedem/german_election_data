# =====================================================================
# Stage-1 cleaning parser: Sachsen (SN) Landtagswahl, Wahlkreis level
# Constituency unit: Landtagswahlkreis (60 WK since reunification)
# Vote system: erststimme (Direktstimme) + zweitstimme (Listenstimme)
#
# Machine-readable sources parsed (image-only scans excluded by design):
#   - SN_1999_Landtagswahl_Wahlkreis.csv  -> 1999 (60 WK, both stimmen)
#                                            +1994 zweitstimme (60 WK) [NOT emitted;
#                                             1994 comes from HTML which has both stimmen]
#   - SN_1994_Landtagswahl_Wahlkreis.html -> 1994 (49 of 60 WK, both stimmen)
#   - SN_2014_Landtagswahl_Wahlkreis_originale.xlsx -> 2014 (60 WK)
#   - SN_2019_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx -> 2019 (60 WK)
#   - SN_2024_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx -> 2024 (60 WK)
#
# 2004 + 2009 (60 WK each, both stimmen): parsed by the Stage-0 script
# 00_sn_pdf_parse.py from the two official B VII 2-2 reports (digital text
# layer, no OCR) into sn_pdf/SN_2004_2009_pdf_long.csv, appended below. The
# 2004 report's fonts carry no ToUnicode map; that script recovers the text
# deterministically from each font's /Encoding /Differences array. It
# hard-validates every row against the printed Sachsen total (per party),
# gültige Stimmen, the printed percent columns, the pinned official statewide
# shares, the candidate counts the reports state, and - for 2004 - the
# independent 2004 columns of the 2009 report.
#
# Run order: python3 .../00_sn_pdf_parse.py  ->  Rscript .../parse_SN.R
#
# Output: long tidy CSV, one row per (Wahlkreis x stimme x party_raw).
# =====================================================================

library(here)
library(tidyverse)
library(data.table)
library(readxl)

here::i_am("code/state_elections_wahlkreis/parsers/parse_SN.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Sachsen")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

STATE_ABBR <- "SN"
STATE_NAME <- "Sachsen"

OUT_COLS <- c("state_abbr", "state", "election_year", "election_date",
              "wkr_nr", "wkr_name", "stimme",
              "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
              "party_raw", "votes")

# helper: German number "67 056" / "1 130" -> integer
de_num <- function(x) {
  x <- gsub(" ", " ", x)          # nbsp -> space
  x <- gsub("[^0-9-]", "", x)          # drop spaces / dots
  ifelse(x == "" | is.na(x), NA_integer_, suppressWarnings(as.integer(x)))
}

# =====================================================================
# 1999 CSV
# =====================================================================
parse_1999 <- function() {
  f <- file.path(raw_dir, "SN_1999_Landtagswahl_Wahlkreis.csv")
  lines <- readLines(f, encoding = "latin1", warn = FALSE)
  # line1 = title, line2 = header, lines 3..63 = 60 WK + Land total
  hdr <- strsplit(lines[2], ";", fixed = TRUE)[[1]]
  body <- lines[-c(1, 2)]
  m <- do.call(rbind, lapply(body, function(l) strsplit(l, ";", fixed = TRUE)[[1]]))
  # ensure consistent ncol
  stopifnot(ncol(m) == length(hdr))
  colnames(m) <- hdr

  # column index map (1-based, verified against the file legend / arithmetic):
  #  3 Wahlberechtigte 1999, 5 Waehler 1999
  #  7 Ungueltige Direktstimmen 1999, 8 Gueltige Direktstimmen 1999
  #  9..21 Direkt party 1999 (CDU SPD PDS GRUENE BueSo DSU GRAUE REP F.D.P.
  #                           FP Deutschlands NPD FORUM Einzelvorschlaege)
  # 22 Ungueltige Listenstimmen 1999, 24 Gueltige Listenstimmen 1999
  # Listen 1999 party cols (1999-labelled in the Listen section):
  #   26 CDU,28 SPD,30 PDS,32 GRUENE,34 BueSo,35 DSU,37 GRAUE,38 REP,40 F.D.P.,
  #   42 FP Deutschlands,43 Pro DM,44 KPD,45 NPD,46 FORUM,48 PBC
  direkt_idx  <- 9:21
  direkt_name <- c("CDU","SPD","PDS","GRÜNE","BüSo","DSU","GRAUE","REP","F.D.P.",
                   "FP Deutschlands","NPD","FORUM","Einzelvorschläge")
  listen_idx  <- c(26,28,30,32,34,35,37,38,40,42,43,44,45,46,48)
  listen_name <- c("CDU","SPD","PDS","GRÜNE","BüSo","DSU","GRAUE","REP","F.D.P.",
                   "FP Deutschlands","Pro DM","KPD","NPD","FORUM","PBC")

  recs <- list()
  for (i in seq_len(nrow(m))) {
    wkr_nr   <- trimws(m[i, 1])
    wkr_name <- trimws(m[i, 2])
    if (wkr_nr == "" || grepl("Freistaat", wkr_name)) next   # skip Land total row
    elig <- de_num(m[i, 3]); voters <- de_num(m[i, 5])
    inv1 <- de_num(m[i, 7]); val1 <- de_num(m[i, 8])
    inv2 <- de_num(m[i, 22]); val2 <- de_num(m[i, 24])
    # erststimme
    for (k in seq_along(direkt_idx)) {
      recs[[length(recs)+1]] <- list(wkr_nr, wkr_name, "erststimme", elig, voters,
        val1, inv1, direkt_name[k], de_num(m[i, direkt_idx[k]]))
    }
    # zweitstimme
    for (k in seq_along(listen_idx)) {
      recs[[length(recs)+1]] <- list(wkr_nr, wkr_name, "zweitstimme", elig, voters,
        val2, inv2, listen_name[k], de_num(m[i, listen_idx[k]]))
    }
  }
  df <- rbindlist(lapply(recs, function(r) setNames(r,
    c("wkr_nr","wkr_name","stimme","eligible_voters","number_voters",
      "valid_votes","invalid_votes","party_raw","votes"))))
  df[, `:=`(election_year = 1999L, election_date = "1999-09-19")]
  df
}

# =====================================================================
# 1994 HTML (49 of 60 WK; ISO-8859-1, <pre> Merkmal tables)
# =====================================================================
parse_1994 <- function() {
  f <- file.path(raw_dir, "SN_1994_Landtagswahl_Wahlkreis.html")
  raw <- paste(readLines(f, encoding = "latin1", warn = FALSE), collapse = "\n")
  unesc <- function(s) {
    s <- gsub("&auml;","ä",s); s <- gsub("&ouml;","ö",s)
    s <- gsub("&uuml;","ü",s); s <- gsub("&Auml;","Ä",s)
    s <- gsub("&Ouml;","Ö",s); s <- gsub("&Uuml;","Ü",s)
    s <- gsub("&szlig;","ß",s); s <- gsub("&amp;","&",s)
    s <- gsub("&lt;","<",s); s <- gsub("&gt;",">",s); s
  }
  raw <- unesc(raw)
  blocks <- strsplit(raw, "<h2>Wahlkreis", fixed = TRUE)[[1]][-1]

  is_val <- function(s) {
    s == "x" || grepl("^[0-9 ]+$", s) || grepl("^[0-9]+,[0-9]+$", s)
  }

  recs <- list()
  for (b in blocks) {
    pre <- sub("(?s)^.*?<pre>", "", b, perl = TRUE)   # drop up to <pre>
    pre <- sub("(?s)</pre>.*$", "", pre, perl = TRUE)  # drop from </pre> onward
    lines <- trimws(strsplit(pre, "\n")[[1]])
    lines <- lines[lines != ""]
    # header line carries "im Wahlkreis NN Name"
    h <- lines[grepl("im Wahlkreis", lines)][1]
    mm <- regmatches(h, regexec("im Wahlkreis\\s+(\\d+)\\s+(.+)$", h))[[1]]
    wkr_nr <- mm[2]
    wkr_name <- trimws(mm[3])

    idx_w  <- which(lines == "Wahlberechtigte")[1]
    idx_v  <- which(lines == "Wähler")[1]
    idx_ug <- which(lines == "Ungültige Stimmen")[1]
    idx_g  <- which(lines == "Gültige Stimmen")[1]
    idx_dv <- which(lines == "davon entfielen auf")[1]

    elig   <- de_num(lines[idx_w + 1])              # Direkt absolut (== Listen)
    voters <- de_num(lines[idx_v + 1])
    inv1 <- de_num(lines[idx_ug + 1]); inv2 <- de_num(lines[idx_ug + 3])
    val1 <- de_num(lines[idx_g  + 1]); val2 <- de_num(lines[idx_g  + 3])

    j <- idx_dv + 1
    while (j <= length(lines)) {
      name <- lines[j]
      v <- lines[(j+1):(j+4)]
      if (length(v) < 4 || any(is.na(v))) break
      stopifnot(all(vapply(v, is_val, logical(1))))   # 4 value tokens
      d_abs <- if (v[1] == "x") NA_integer_ else de_num(v[1])
      l_abs <- if (v[3] == "x") NA_integer_ else de_num(v[3])
      # erststimme (only if party stood for Direktmandat, i.e. not "x")
      if (!is.na(d_abs)) {
        recs[[length(recs)+1]] <- list(wkr_nr, wkr_name, "erststimme", elig, voters,
          val1, inv1, name, d_abs)
      }
      if (!is.na(l_abs)) {
        recs[[length(recs)+1]] <- list(wkr_nr, wkr_name, "zweitstimme", elig, voters,
          val2, inv2, name, l_abs)
      }
      j <- j + 5
    }
  }
  df <- rbindlist(lapply(recs, function(r) setNames(r,
    c("wkr_nr","wkr_name","stimme","eligible_voters","number_voters",
      "valid_votes","invalid_votes","party_raw","votes"))))
  df[, `:=`(election_year = 1994L, election_date = "1994-09-11")]
  df
}

# =====================================================================
# Generic xlsx WK-sheet parser (2014 / 2019 / 2024)
#   _1 = Direktstimmen (erststimme), _2 = Listenstimmen (zweitstimme)
#   exclude "in %" cols, ungültige/gültige meta cols, the Land (Ebene==SN) row.
# =====================================================================
parse_xlsx <- function(file, sheet, year, date) {
  ws <- read_excel(file.path(raw_dir, file), sheet = sheet,
                   col_names = TRUE, .name_repair = "minimal")
  ws <- as.data.frame(ws, check.names = FALSE)
  cn <- colnames(ws)

  has_ebene <- "Ebene" %in% cn
  if (has_ebene) ws <- ws[!(ws$Ebene %in% c("SN")), , drop = FALSE]  # drop Land total

  # meta columns
  col_elig   <- "Wahlberechtigte"
  col_voters <- "Wähler"
  # party columns: end in _1 or _2, NOT "in %", NOT ungültige/gültige
  is_pct  <- grepl(" in %$", cn)
  is_p1   <- grepl("_1$", cn) & !is_pct
  is_p2   <- grepl("_2$", cn) & !is_pct
  meta1 <- c("ungültige_1","gültige_1"); meta2 <- c("ungültige_2","gültige_2")
  party1 <- cn[is_p1 & !(cn %in% meta1)]
  party2 <- cn[is_p2 & !(cn %in% meta2)]

  to_int <- function(v) {
    v <- as.character(v)
    v[v %in% c("x","X","-",".")] <- NA   # party not standing / no value
    suppressWarnings(as.integer(round(as.numeric(v))))
  }

  recs <- list()
  for (i in seq_len(nrow(ws))) {
    wkr_nr   <- as.character(ws[i, "WK-Nr"])
    wkr_name <- as.character(ws[i, "WK-Name"])
    elig   <- to_int(ws[i, col_elig]); voters <- to_int(ws[i, col_voters])
    val1 <- to_int(ws[i, "gültige_1"]); inv1 <- to_int(ws[i, "ungültige_1"])
    val2 <- to_int(ws[i, "gültige_2"]); inv2 <- to_int(ws[i, "ungültige_2"])
    for (p in party1) {
      vv <- to_int(ws[i, p])
      if (is.na(vv)) next                         # "x" -> party absent in this WK
      recs[[length(recs)+1]] <- list(wkr_nr, wkr_name, "erststimme", elig, voters,
        val1, inv1, sub("_1$","",p), vv)
    }
    for (p in party2) {
      vv <- to_int(ws[i, p])
      if (is.na(vv)) next
      recs[[length(recs)+1]] <- list(wkr_nr, wkr_name, "zweitstimme", elig, voters,
        val2, inv2, sub("_2$","",p), vv)
    }
  }
  df <- rbindlist(lapply(recs, function(r) setNames(r,
    c("wkr_nr","wkr_name","stimme","eligible_voters","number_voters",
      "valid_votes","invalid_votes","party_raw","votes"))))
  df[, `:=`(election_year = year, election_date = date)]
  df
}

# =====================================================================
# Build
# =====================================================================
d1994 <- parse_1994()
d1999 <- parse_1999()
d2014 <- parse_xlsx("SN_2014_Landtagswahl_Wahlkreis_originale.xlsx",
                    "LW14_Ergebnisse_WK", 2014L, "2014-08-31")
d2019 <- parse_xlsx("SN_2019_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx",
                    "LW19_endgErgebnisse_SN&WK", 2019L, "2019-09-01")
d2024 <- parse_xlsx("SN_2024_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx",
                    "LW24_endgErgebnisse_SN&WK", 2024L, "2024-09-01")

all <- rbindlist(list(d1994, d1999, d2014, d2019, d2024), use.names = TRUE)
all[, `:=`(state_abbr = STATE_ABBR, state = STATE_NAME)]
all[, wkr_nr := as.character(wkr_nr)]
setcolorder(all, OUT_COLS)

out_csv <- file.path(out_dir, "SN_ltw_wkr_long.csv")

# =====================================================================
# VALIDATION
# =====================================================================
cat("\n================ VALIDATION ================\n")

# (a) per (year,wkr,stimme): sum(party votes) vs valid_votes
chk <- all[, .(sum_votes = sum(votes, na.rm = TRUE),
               valid = first(valid_votes)),
           by = .(election_year, wkr_nr, stimme)]
chk[, disc := abs(sum_votes - valid)]
cat("(a) per-(wkr,stimme) integrity\n")
cat("    groups checked:", nrow(chk), "\n")
cat("    max abs discrepancy:", max(chk$disc, na.rm = TRUE), "\n")
print(chk[disc > 0][order(-disc)][1:min(10,.N)])

# (b) statewide total match — reconstruct source Land row per year where available
cat("\n(b) statewide-total match per year/stimme\n")

# 1999 + 1994(listen) from CSV Land row; 2019/2024 from xlsx SN row; 2014 has none.
statewide_match <- list()

## --- 1999 statewide from CSV Land row ---
{
  lines <- readLines(file.path(raw_dir, "SN_1999_Landtagswahl_Wahlkreis.csv"),
                     encoding = "latin1", warn = FALSE)
  land <- strsplit(lines[63], ";", fixed = TRUE)[[1]]
  direkt_idx <- 9:21
  listen_idx <- c(26,28,30,32,34,35,37,38,40,42,43,44,45,46,48)
  src_e <- sum(de_num(land[direkt_idx]), na.rm = TRUE)
  src_z <- sum(de_num(land[listen_idx]), na.rm = TRUE)
  my <- all[election_year==1999, .(s=sum(votes)), by=stimme]
  e_ok <- abs(my[stimme=="erststimme", s] - src_e) <= 1
  z_ok <- abs(my[stimme=="zweitstimme", s] - src_z) <= 1
  cat(sprintf("  1999 erst:  mine=%d  source=%d  match=%s\n",
              my[stimme=='erststimme',s], src_e, e_ok))
  cat(sprintf("  1999 zweit: mine=%d  source=%d  match=%s\n",
              my[stimme=='zweitstimme',s], src_z, z_ok))
  statewide_match[["1999"]] <- e_ok && z_ok
}

## --- 2019 / 2024 statewide from xlsx SN row ---
for (yr in c(2019L, 2024L)) {
  fl <- if (yr==2019) "SN_2019_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx" else
                      "SN_2024_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx"
  sh <- if (yr==2019) "LW19_endgErgebnisse_SN&WK" else "LW24_endgErgebnisse_SN&WK"
  ws <- as.data.frame(read_excel(file.path(raw_dir, fl), sheet=sh,
                                 .name_repair="minimal"), check.names=FALSE)
  cn <- colnames(ws)
  land <- ws[ws$Ebene=="SN", , drop=FALSE]
  is_pct <- grepl(" in %$", cn)
  p1 <- cn[grepl("_1$",cn) & !is_pct & !(cn %in% c("ungültige_1","gültige_1"))]
  p2 <- cn[grepl("_2$",cn) & !is_pct & !(cn %in% c("ungültige_2","gültige_2"))]
  toI <- function(v){v<-as.character(v);v[v %in% c("x","X","-",".")]<-NA;suppressWarnings(as.integer(round(as.numeric(v))))}
  src_e <- sum(sapply(p1, function(p) toI(land[1,p])), na.rm=TRUE)
  src_z <- sum(sapply(p2, function(p) toI(land[1,p])), na.rm=TRUE)
  my <- all[election_year==yr, .(s=sum(votes)), by=stimme]
  e_ok <- abs(my[stimme=="erststimme",s]-src_e)<=1
  z_ok <- abs(my[stimme=="zweitstimme",s]-src_z)<=1
  cat(sprintf("  %d erst:  mine=%d  source=%d  match=%s\n", yr, my[stimme=='erststimme',s], src_e, e_ok))
  cat(sprintf("  %d zweit: mine=%d  source=%d  match=%s\n", yr, my[stimme=='zweitstimme',s], src_z, z_ok))
  statewide_match[[as.character(yr)]] <- e_ok && z_ok
}

## --- 2014: no Land total row in the file; cross-check against 1999 CSV? no.
cat("  2014: source file has NO statewide total row -> per-WK integrity only\n")
## --- 1994: HTML has only 49 WK and no Land row -> no statewide check possible
cat("  1994: HTML has 49/60 WK and no Land row -> no statewide check possible\n")

# (c) Wahlkreis count per year
cat("\n(c) Wahlkreis count per year (expected 60)\n")
print(all[, .(n_wkr = uniqueN(wkr_nr)), by = election_year][order(election_year)])

# =====================================================================
# 2004 + 2009 - Stage-0 PDF parse (official B VII 2-2 reports)
# =====================================================================
pdf_csv <- file.path(out_dir, "sn_pdf", "SN_2004_2009_pdf_long.csv")
if (!file.exists(pdf_csv)) {
  stop("Missing ", pdf_csv,
       "\n  Run first:  python3 code/state_elections_wahlkreis/parsers/00_sn_pdf_parse.py")
}
pdf_long <- fread(pdf_csv, encoding = "UTF-8",
                  colClasses = list(character = c("state_abbr", "state",
                                                  "election_date", "wkr_nr",
                                                  "wkr_name", "stimme",
                                                  "party_raw")))
stopifnot(setequal(names(pdf_long), OUT_COLS))
setcolorder(pdf_long, OUT_COLS)

cat("\n=========== SN 2004 + 2009 (from the B VII 2-2 reports) ===========\n")
cat("    rows read      :", nrow(pdf_long), "\n")
print(pdf_long[, .(n_wkr = uniqueN(wkr_nr), n_parties = uniqueN(party_raw)),
               by = .(election_year, stimme)][order(election_year, stimme)])

# 60 Wahlkreise, numbered 01..60, in both years and both stimmen
chk_wkr <- pdf_long[, .(ok = identical(sort(unique(wkr_nr)),
                                       sprintf("%02d", 1:60))),
                    by = .(election_year, stimme)]
if (!all(chk_wkr$ok)) { print(chk_wkr); stop("SN PDF years: Wahlkreis set != 01..60") }
cat("    Wahlkreis set  : 01..60 in every (year, stimme) group\n")

# per (year, wkr, stimme): sum(party votes) must equal valid_votes, and
# Wähler must equal gültige + ungültige
chk2 <- pdf_long[, .(sum_party = sum(votes, na.rm = TRUE),
                     valid = unique(valid_votes),
                     voters = unique(number_voters),
                     invalid = unique(invalid_votes)),
                 by = .(election_year, wkr_nr, stimme)]
chk2[, `:=`(disc = abs(sum_party - valid), disc_turnout = abs(voters - valid - invalid))]
cat("    vote integrity : groups", nrow(chk2),
    "| max |sum(party) - gültig| =", max(chk2$disc),
    "| max |Wähler - gültig - ungültig| =", max(chk2$disc_turnout), "\n")
if (any(chk2$disc > 0) || any(chk2$disc_turnout > 0)) {
  print(chk2[disc > 0 | disc_turnout > 0]); stop("SN PDF rows fail vote integrity")
}

# statewide Listenstimmen shares against the official results
official <- data.table(
  election_year = c(rep(2004L, 6), rep(2009L, 6)),
  party_raw = c("CDU", "PDS", "SPD", "NPD", "FDP", "GRÜNE",
                "CDU", "DIE LINKE", "SPD", "FDP", "GRÜNE", "NPD"),
  share = c(41.1, 23.6, 9.8, 9.2, 5.9, 5.1,
            40.2, 20.6, 10.4, 10.0, 6.4, 5.6))
z <- pdf_long[stimme == "zweitstimme"]
denom <- unique(z[, .(election_year, wkr_nr, valid_votes)])[
  , .(valid = sum(valid_votes)), by = election_year]
got <- z[, .(votes = sum(votes)), by = .(election_year, party_raw)][
  denom, on = "election_year"][, share_got := 100 * votes / valid]
cmp <- merge(official, got, by = c("election_year", "party_raw"))
cmp[, diff := round(share_got - share, 2)]
cat("\n    statewide Listenstimmen shares vs official results:\n")
print(cmp[, .(election_year, party_raw, share_got = round(share_got, 2), share, diff)])
if (nrow(cmp) != nrow(official) || any(abs(cmp$diff) > 0.1)) {
  stop("SN PDF years: statewide Listenstimmen shares off by more than 0.1pp")
}

# Wahlkreis names must be consistent within a year
n_names <- pdf_long[, uniqueN(wkr_name), by = .(election_year, wkr_nr)]
stopifnot(all(n_names$V1 == 1L))

all <- rbindlist(list(all, pdf_long), use.names = TRUE)
setorder(all, election_year, wkr_nr, stimme, party_raw)

cat("\n=========== COMBINED (SN) ===========\n")
print(all[, .(rows = .N, n_wkr = uniqueN(wkr_nr),
              n_parties = uniqueN(party_raw)), by = election_year][order(election_year)])

cat("\nTotal rows emitted:", nrow(all), "\n")

# distinct party labels
cat("\nDistinct party_raw labels:\n")
print(sort(unique(all$party_raw)))

# =====================================================================
# WRITE OUTPUT
# =====================================================================
fwrite(all, out_csv)
cat("\nOutput:", out_csv, "\n")
