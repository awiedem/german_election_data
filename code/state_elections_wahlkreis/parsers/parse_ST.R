# =====================================================================
# Stage-1 cleaning parser: Sachsen-Anhalt (ST) Landtagswahl, Wahlkreis level
# Source: Statistisches Landesamt Sachsen-Anhalt / Landeswahlleiter portal
#   raw CSVs (semicolon-delimited, ISO-8859-1), one per election year.
# Vote system: Erststimme (Personen-/Direktstimme, blocks C/D/Dxx)
#              + Zweitstimme (Partei-/Listenstimme, blocks E/F/Fxx).
# Output: ONE long row per (Wahlkreis x stimme x party).
# =====================================================================

library(here)
library(tidyverse)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_ST.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Sachsen-Anhalt")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

STATE      <- "Sachsen-Anhalt"
STATE_ABBR <- "ST"

# ---------------------------------------------------------------------
# Per-year code -> party_raw maps, transcribed VERBATIM from each year's
# official Datensatzbeschreibung PDF (short label column). Erststimme codes
# are the D-block; Zweitstimme codes are the F-block. The two blocks use
# DIFFERENT code numbering and DIFFERENT party sets, so each is mapped
# independently. Codes are matched to CSV columns by their header name.
# ---------------------------------------------------------------------

# 1990  (D = Personenstimmen/Erststimme; F = Parteistimmen/Zweitstimme)
# NB: the 1990 PDF has a typo: F-block lists "F02 Chr.L." then "F02 CSP";
#     the CSV F-block header is F01..F13 (13 cols), so the second entry is F03=CSP.
d_1990 <- c(D01="CDU", D02="Chr. L.", D03="CSP", D04="DFD", D05="DBU", D06="DSU",
            D08="F.D.P.", D09="GRÜ-NF", D10="NPD", D11="PDS", D12="SPD",
            D14="EB", D15="EB", D16="EB")
f_1990 <- c(F01="CDU", F02="Chr. L.", F03="CSP", F04="DFD", F05="DBU", F06="DSU",
            F07="REP", F08="F.D.P.", F09="GRÜ-NF", F10="NPD", F11="PDS",
            F12="SPD", F13="USPD")

# 1994
d_1994 <- c(D01="CDU", D02="SPD", D03="F.D.P.", D04="PDS", D05="BÜ90/GRÜNE",
            D07="DE", D08="DSU", D09="GRAUE", D10="REP", D12="STATT Partei",
            D13="EB", D14="EB")
f_1994 <- c(F01="CDU", F02="SPD", F03="F.D.P.", F04="PDS", F05="BÜ90/GRÜNE",
            F06="ALP", F07="DE", F08="DSU", F09="GRAUE", F10="REP",
            F11="OPDE", F12="STATT Partei")

# 1998
d_1998 <- c(D01="CDU", D02="SPD", D03="PDS", D04="GRÜNE", D05="F.D.P.",
            D10="REP", D14="md-p", D16="FORUM", D17="PBC",
            D18="EB", D19="EB", D20="EB")
f_1998 <- c(F01="CDU", F02="SPD", F03="PDS", F04="GRÜNE", F05="F.D.P.",
            F08="DVU", F09="DMP", F10="REP", F11="future!", F16="FORUM")

# 2002
d_2002 <- c(D01="SPD", D02="CDU", D03="PDS", D05="FDP", D06="GRÜNE", D07="ZENTRUM",
            D11="MLPD", D14="OPdM", D16="SCHILL", D17="Pro DM", D18="R-B-P",
            D20="EB", D21="EB")
f_2002 <- c(F01="SPD", F02="CDU", F03="PDS", F05="FDP", F06="GRÜNE",
            F08="SPASSPARTEI", F09="FDVP", F11="MLPD", F12="ödp", F14="OPdM",
            F16="SCHILL", F17="Pro DM", F18="R-B-P", F19="B - DKP/KPD")

# 2006
d_2006 <- c(D01="CDU", D02="Die Linke.", D03="SPD", D04="FDP", D05="GRÜNE",
            D06="AGFG", D07="BBW", D15="MLPD", D18="Bü - DKP/KPD",
            D19="Offensive D - STATT Partei - DSU", D20="GUT", D21="EB", D22="EB")
f_2006 <- c(F01="CDU", F02="Die Linke.", F03="SPD", F04="FDP", F05="GRÜNE",
            F06="AGFG", F07="BBW", F08="DVU", F10="REP", F11="Eltern",
            F12="FP Deutschlands", F13="future!", F15="MLPD", F16="Pro DM",
            F18="Bü - DKP/KPD", F19="Offensive D - STATT Partei - DSU", F20="GUT")

# 2011
d_2011 <- c(D01="CDU", D02="DIE LINKE", D03="SPD", D04="FDP", D05="GRÜNE",
            D09="FREIE WÄHLER", D10="KPD", D11="MLPD", D12="NPD", D13="ödp",
            D15="PIRATEN", D17="EB")
f_2011 <- c(F01="CDU", F02="DIE LINKE", F03="SPD", F04="FDP", F05="GRÜNE",
            F09="FREIE WÄHLER", F10="KPD", F11="MLPD", F12="NPD", F13="ödp",
            F14="Tierschutzpartei", F15="PIRATEN", F16="SPV")

# 2016
d_2016 <- c(D01="CDU", D02="DIE LINKE", D03="SPD", D04="GRÜNE",
            D06="Tierschutzallianz", D07="AfD", D09="FBM", D10="FDP",
            D11="FREIE WÄHLER", D12="MG", D14="Die PARTEI", D17="STATT Partei",
            D18="EB")
f_2016 <- c(F01="CDU", F02="DIE LINKE", F03="SPD", F04="GRÜNE", F05="ALFA",
            F06="Tierschutzallianz", F07="AfD", F08="DIE RECHTE", F09="FBM",
            F10="FDP", F11="FREIE WÄHLER", F12="MG", F13="NPD",
            F14="Die PARTEI", F15="Tierschutzpartei")

# 2021  (CSV header already carries full names; map kept for completeness/robustness)
d_2021 <- c(D01="CDU", D02="AfD", D03="DIE LINKE", D04="SPD", D05="GRÜNE",
            D06="FDP", D07="FREIE WÄHLER", D08="NPD", D09="Tierschutzpartei",
            D10="Tierschutzallianz", D12="Die PARTEI", D13="Gartenpartei",
            D14="FBM", D16="dieBasis", D21="ÖDP",
            D26="Andere KWV", D27="Andere KWV", D28="Andere KWV")
f_2021 <- c(F01="CDU", F02="AfD", F03="DIE LINKE", F04="SPD", F05="GRÜNE",
            F06="FDP", F07="FREIE WÄHLER", F08="NPD", F09="Tierschutzpartei",
            F10="Tierschutzallianz", F11="LKR", F12="Die PARTEI",
            F13="Gartenpartei", F14="FBM", F15="TIERSCHUTZ hier!", F16="dieBasis",
            F19="Klimaliste ST", F21="ÖDP", F22="Die Humanisten",
            F23="Gesundheitsforschung", F24="PIRATEN", F25="WiR2020")

party_maps <- list(
  "1990" = list(d = d_1990, f = f_1990),
  "1994" = list(d = d_1994, f = f_1994),
  "1998" = list(d = d_1998, f = f_1998),
  "2002" = list(d = d_2002, f = f_2002),
  "2006" = list(d = d_2006, f = f_2006),
  "2011" = list(d = d_2011, f = f_2011),
  "2016" = list(d = d_2016, f = f_2016),
  "2021" = list(d = d_2021, f = f_2021)
)

# File layout family: "old" (1990-2002) vs "new" (2006-2021)
#   old:  WDATUM;SART;LT WKR;AGS;NAME;A;B;C;D;Dxx...;E;F;Fxx...
#   new:  ERGART;DATUM;LEER;LEER;LEER;SART;NR;NAME;A;B;E;F;Fxx...;C;D;Dxx...;WKSIEGER
layout_old <- c("1990", "1994", "1998", "2002")

files <- c(
  "1990" = "ST_1990_Landtagswahl_Wahlkreis.csv",
  "1994" = "ST_1994_Landtagswahl_Wahlkreis.csv",
  "1998" = "ST_1998_Landtagswahl_Wahlkreis.csv",
  "2002" = "ST_2002_Landtagswahl_Wahlkreis.csv",
  "2006" = "ST_2006_Landtagswahl_Wahlkreis.csv",
  "2011" = "ST_2011_Landtagswahl_Wahlkreis.csv",
  "2016" = "ST_2016_Landtagswahl_Wahlkreis.csv",
  "2021" = "ST_2021_Landtagswahl_Wahlkreis.csv"
)

# ---------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------
clean_num <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "." | x == "-"] <- NA
  suppressWarnings(as.integer(x))
}

parse_date <- function(x) {
  # vectorised: handles DDMMYYYY (old files) and DD.MM.YYYY (new files)
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  i1 <- grepl("^\\d{8}$", x)
  i2 <- grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", x)
  out[i1] <- as.character(as.Date(x[i1], format = "%d%m%Y"))
  out[i2] <- as.character(as.Date(x[i2], format = "%d.%m.%Y"))
  out
}

read_raw <- function(path) {
  # ISO-8859-1, semicolon, keep everything character, do not let fread guess
  raw <- readLines(path, encoding = "latin1", warn = FALSE)
  raw <- iconv(raw, from = "latin1", to = "UTF-8")
  # split on ';'
  hdr <- strsplit(raw[1], ";", fixed = TRUE)[[1]]
  body <- raw[-1]
  body <- body[nchar(trimws(body)) > 0]
  mat <- str_split(body, ";", n = length(hdr), simplify = TRUE)
  # pad short rows
  if (ncol(mat) < length(hdr)) {
    mat <- cbind(mat, matrix("", nrow(mat), length(hdr) - ncol(mat)))
  }
  df <- as.data.frame(mat[, seq_along(hdr), drop = FALSE],
                      stringsAsFactors = FALSE)
  names(df) <- trimws(hdr)
  df
}

# normalise a header name to bare code (strip trailing spaces, and for 2021
# the "F01 - CDU" style -> "F01")
hdr_code <- function(nm) {
  nm <- trimws(nm)
  toupper(sub("\\s*-.*$", "", nm))   # keep token before first " - "
}

# ---------------------------------------------------------------------
# core parser: returns list(long = <tidy WKR rows>, total = <LAN totals>)
# ---------------------------------------------------------------------
parse_year <- function(year) {
  path <- file.path(raw_dir, files[[year]])
  df   <- read_raw(path)
  pm   <- party_maps[[year]]
  is_old <- year %in% layout_old

  # locate structural columns
  if (is_old) {
    sart_col <- names(df)[2]            # SART / Satzart
    nr_col   <- names(df)[3]            # LT WKR ...
    name_col <- names(df)[5]            # NAME / Name
    date_col <- names(df)[1]            # WDATUM
  } else {
    # new layout is POSITIONAL: ERGART;DATE;LEER;LEER;LEER;SART;NR;NAME;...
    # (2021 names the columns Satzart/Schlüsselnummer/Name, others SART/NR/NAME)
    sart_col <- names(df)[6]
    nr_col   <- names(df)[7]
    name_col <- names(df)[8]
    date_col <- names(df)[2]            # DATUM / WDATUM (2016)
  }

  # map header codes -> column index
  codes <- hdr_code(names(df))

  col_for <- function(code) {
    idx <- which(codes == code)
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  }

  # count columns A,B,C,D,E,F
  ix_A <- col_for("A"); ix_B <- col_for("B")
  ix_C <- col_for("C"); ix_D <- col_for("D")
  ix_E <- col_for("E"); ix_F <- col_for("F")

  # party value columns
  d_idx <- vapply(names(pm$d), col_for, integer(1))
  f_idx <- vapply(names(pm$f), col_for, integer(1))
  if (any(is.na(d_idx))) stop(sprintf("%s: missing D columns: %s", year,
        paste(names(pm$d)[is.na(d_idx)], collapse = ",")))
  if (any(is.na(f_idx))) stop(sprintf("%s: missing F columns: %s", year,
        paste(names(pm$f)[is.na(f_idx)], collapse = ",")))

  sart <- trimws(df[[sart_col]])
  edate <- parse_date(df[[date_col]])

  # Disambiguate repeated raw labels (only "EB" Einzelbewerber occur multiple
  # times) by appending the source column code, so distinct individual
  # candidates stay distinct (e.g. "EB (D14)") instead of collapsing into one.
  disambig <- function(codes_vec, labs) {
    dup <- labs %in% labs[duplicated(labs)]
    labs[dup] <- paste0(labs[dup], " (", codes_vec[dup], ")")
    labs
  }
  d_lab_full <- disambig(names(pm$d), unname(pm$d))
  f_lab_full <- disambig(names(pm$f), unname(pm$f))

  build_block <- function(rows_idx, stimme) {
    # stimme: "erststimme" uses C/D/Dxx ; "zweitstimme" uses E/F/Fxx
    if (stimme == "erststimme") {
      inv_ix <- ix_C; val_ix <- ix_D; p_idx <- d_idx; p_lab <- d_lab_full
    } else {
      inv_ix <- ix_E; val_ix <- ix_F; p_idx <- f_idx; p_lab <- f_lab_full
    }
    out <- vector("list", length(p_idx))
    for (k in seq_along(p_idx)) {
      out[[k]] <- data.frame(
        row = rows_idx,
        stimme = stimme,
        invalid_votes = clean_num(df[[inv_ix]][rows_idx]),
        valid_votes   = clean_num(df[[val_ix]][rows_idx]),
        party_raw     = p_lab[k],
        votes         = clean_num(df[[p_idx[k]]][rows_idx]),
        stringsAsFactors = FALSE
      )
    }
    rbindlist(out)
  }

  # ---- WKR (constituency) rows ----
  wkr_rows <- which(sart == "WKR")
  wkr_meta <- data.frame(
    row = wkr_rows,
    election_year = as.integer(year),
    election_date = edate[wkr_rows],
    wkr_nr  = trimws(df[[nr_col]][wkr_rows]),
    wkr_name = trimws(df[[name_col]][wkr_rows]),
    eligible_voters = clean_num(df[[ix_A]][wkr_rows]),
    number_voters   = clean_num(df[[ix_B]][wkr_rows]),
    stringsAsFactors = FALSE
  )

  wkr_long <- rbindlist(list(
    build_block(wkr_rows, "erststimme"),
    build_block(wkr_rows, "zweitstimme")
  ))
  wkr_long <- merge(wkr_long, wkr_meta, by = "row", all.x = TRUE)

  long <- wkr_long %>%
    transmute(
      state_abbr = STATE_ABBR, state = STATE,
      election_year, election_date,
      wkr_nr, wkr_name, stimme,
      eligible_voters, number_voters,
      valid_votes, invalid_votes,
      party_raw, votes
    ) %>%
    # drop party value columns that are NA (party did not stand in this WKR)
    filter(!is.na(votes))

  # ---- LAN total row (for validation only) ----
  # Some years (2002) have several LAN rows (Urnen-/Brief-/Gesamtwahl);
  # the Gesamt row is the one with the largest number of Wähler (B).
  lan_rows <- which(sart == "LAN")
  lan_B <- clean_num(df[[ix_B]][lan_rows])
  lan_keep <- lan_rows[which.max(lan_B)]

  tot_long <- rbindlist(list(
    build_block(lan_keep, "erststimme"),
    build_block(lan_keep, "zweitstimme")
  ))
  tot <- tot_long %>%
    filter(!is.na(votes)) %>%
    transmute(election_year = as.integer(year), stimme, party_raw, votes)

  list(long = as.data.frame(long), total = as.data.frame(tot))
}

# ---------------------------------------------------------------------
# run all years
# ---------------------------------------------------------------------
all_long  <- list()
all_total <- list()
validation <- list()

for (yr in names(files)) {
  res <- parse_year(yr)
  all_long[[yr]]  <- res$long
  all_total[[yr]] <- res$total

  # (a) per (wkr,stimme): sum(party votes) vs valid_votes
  chk <- res$long %>%
    group_by(wkr_nr, stimme) %>%
    summarise(sum_p = sum(votes), vv = first(valid_votes), .groups = "drop") %>%
    mutate(disc = abs(sum_p - vv))
  max_disc <- max(chk$disc, na.rm = TRUE)
  n_grp <- nrow(chk)

  # (b) statewide total match: per party x stimme.
  #     The LAN total row reports Einzelbewerber / "Andere KWV" as a SINGLE
  #     summed code (per the official footnote), whereas at WKR level they are
  #     split across several codes (D14/D15..., D26/D27/D28). Collapse the
  #     per-code disambiguation suffix back to the base label on BOTH sides so
  #     the EB/KWV totals are compared as a single aggregate (as the source does).
  base_label <- function(x) trimws(sub("\\s*\\([DF][0-9]+\\)$", "", x))
  mine <- res$long %>%
    mutate(party_raw = base_label(party_raw)) %>%
    group_by(stimme, party_raw) %>%
    summarise(mine = sum(votes), .groups = "drop")
  src_tot <- res$total %>%
    mutate(party_raw = base_label(party_raw)) %>%
    group_by(stimme, party_raw) %>%
    summarise(votes = sum(votes), .groups = "drop")
  cmp <- full_join(mine, src_tot %>% rename(src = votes),
                   by = c("stimme", "party_raw")) %>%
    mutate(mine = coalesce(mine, 0L), src = coalesce(src, 0L),
           d = abs(mine - src))
  state_match <- all(cmp$d <= 1)        # allow tiny rounding
  fails <- cmp %>% filter(d > 1)

  n_wkr <- length(unique(res$long$wkr_nr))

  validation[[yr]] <- list(max_disc = max_disc, n_grp = n_grp,
                           state_match = state_match,
                           n_fail = nrow(fails), fails = fails,
                           n_wkr = n_wkr)
  cat(sprintf("[%s] n_wkr=%d  max|sum-valid|=%g over %d (wkr,stimme)  state_total_match=%s  fails=%d\n",
              yr, n_wkr, max_disc, n_grp, state_match, nrow(fails)))
  if (nrow(fails) > 0) print(fails)
}

final <- rbindlist(all_long, use.names = TRUE)

out_path <- file.path(out_dir, "ST_ltw_wkr_long.csv")
fwrite(final, out_path)
cat(sprintf("\nWROTE %s  (%d rows)\n", out_path, nrow(final)))
cat("Distinct party_raw:\n")
print(sort(unique(final$party_raw)))
