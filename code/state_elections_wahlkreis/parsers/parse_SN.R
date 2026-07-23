# =====================================================================
# Stage-1 cleaning parser: Sachsen (SN) Landtagswahl, Wahlkreis level
# Constituency unit: Landtagswahlkreis (60 WK since reunification)
# Vote system: erststimme (Direktstimme) + zweitstimme (Listenstimme)
#
# Machine-readable sources parsed (PDF/TIF scans excluded by design):
#   - SN_1999_Landtagswahl_Wahlkreis.csv  -> 1999 (60 WK, both stimmen)
#                                            +1994 zweitstimme (60 WK) [NOT emitted;
#                                             1994 comes from HTML which has both stimmen]
#   - SN_1994_Landtagswahl_Wahlkreis.html -> 1994 (49 of 60 WK, both stimmen)
#   - SN_2014_Landtagswahl_Wahlkreis_originale.xlsx -> 2014 (60 WK)
#   - SN_2019_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx -> 2019 (60 WK)
#   - SN_2024_Landtagswahl_Wahlkreis_Gemeinde_Kreis.xlsx -> 2024 (60 WK)
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
fwrite(all, out_csv)

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

cat("\nTotal rows emitted:", nrow(all), "\n")
cat("Output:", out_csv, "\n")

# distinct party labels
cat("\nDistinct party_raw labels:\n")
print(sort(unique(all$party_raw)))
