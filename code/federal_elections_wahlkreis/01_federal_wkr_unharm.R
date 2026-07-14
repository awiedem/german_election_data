#### Bundestagswahlen at WAHLKREIS level — Stage 1: unharmonized ####
## Parses the official Bundeswahlleiterin constituency result files grabbed by
## 00_download_raw.R and builds the unharmonized dataset, mirroring the
## conventions of the state Wahlkreis pipeline (code/state_elections_wahlkreis/)
## and the GERDA municipality-level federal data:
##   - party columns stored as VOTE SHARES (of valid_votes); counts kept in the
##     long file
##   - other = residual (independents/Übrige/tiny lists); cdu_csu = Union alias
##   - column order: flags, meta, sorted party shares, other, cdu_csu
##   - unharmonized: each election on its OWN 299-Wahlkreis definition
##
## Two source formats are handled:
##   - kerg2 (flat/long):  2017, 2021, 2025
##   - kerg  (classic wide, auto-detecting the 2002 "old" 2-col layout vs the
##            2005-2013 "modern" 4-col layout with Vorperiode):  2002-2013
##
## Inputs : data/federal_elections/wahlkreis_level/raw/BTW{02..25}/
## Outputs: data/federal_elections/wahlkreis_level/final/federal_wkr_unharm_long.{csv,rds}
##          data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.{csv,rds}
##
## Authors: Hanno Hilbig (with Claude Code assistance), July 2026

#### Setup ####
rm(list = ls())
for (pkg in c("here", "data.table")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressMessages(library(pkg, character.only = TRUE))
}
here::i_am("code/federal_elections_wahlkreis/01_federal_wkr_unharm.R")
source(here("code", "federal_elections_wahlkreis", "_normalise_party.R"))
normalise_party_v <- function(x) vapply(x, normalise_party, character(1), USE.NAMES = FALSE)

raw_root <- here("data", "federal_elections", "wahlkreis_level", "raw")
out_dir  <- here("data", "federal_elections", "wahlkreis_level", "final")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#### Generic helpers ####
de_num <- function(x) { x <- gsub("[.[:space:]]", "", x); x[x == ""] <- NA; as.numeric(x) }

to_matrix <- function(lines) {
  sp <- strsplit(lines, ";", fixed = TRUE)
  nc <- max(lengths(sp))
  t(vapply(sp, function(x) {
    x <- c(x, rep("", nc - length(x)))
    enc2utf8(trimws(gsub('^"|"$', "", x)))
  }, character(nc)))
}

#### Parser: classic kerg (2002 "old" + 2005-2013 "modern") ####
## Returns list(parties, meta): one row per (wkr_nr, stimme, party_raw) and per
## (wkr_nr, stimme). Only Wahlkreis rows (Nr 1-299) are kept; summary rows
## (Länder 901-916, Bundesgebiet 999) are used solely for the reconciliation
## check below.
## Column identity is read from the header rows, NOT assumed from fixed block
## widths: the 2002 file mixes 2-column (Erst+Zweit) parties with tail parties
## that have a single Erststimmen-only column, so a fixed stride mis-aligns them.
parse_classic <- function(path, year) {
  L <- readLines(path, warn = FALSE, encoding = "latin1")
  L <- L[!grepl("^#", L)]
  L <- L[nchar(trimws(L)) > 0]
  modern <- any(grepl("Vorperiode|Endg.ltig", L[1:3]))
  m  <- to_matrix(L)
  hdr_n <- if (modern) 3L else 2L
  H1 <- m[1, ]; H2 <- m[2, ]; H3 <- if (modern) m[3, ] else rep("Endgültig", ncol(m))
  D  <- m[-(1:hdr_n), , drop = FALSE]
  nc <- ncol(m)
  nr <- suppressWarnings(as.integer(D[, 1]))
  name <- D[, 2]
  land_raw <- suppressWarnings(as.integer(D[, 3]))
  land_nr <- if (modern) land_raw else land_raw - 900L        # modern: 1-16; old: 901-916

  locf <- function(x) { x[x == ""] <- NA; for (i in seq_along(x)) if (is.na(x[i]) && i > 1) x[i] <- x[i - 1]; x }
  grp <- locf(H1)                       # group (meta name or party) per column
  s   <- if (modern) locf(H2) else H2   # stimme label per column
  st  <- ifelse(grepl("^Erst", s), "erststimme",
                ifelse(grepl("^Zweit", s), "zweitstimme", NA_character_))
  endg <- if (modern) grepl("Endg", H3) else rep(TRUE, nc)
  data_cols <- (4:nc)[endg[4:nc]]       # skip id cols 1-3; keep Endgültig only

  meta_groups <- c("Wahlberechtigte", "Wähler", "Wählende", "Ungültige", "Gültige")
  # Meta single value (eligible/voters): first non-NA across that group's Endgültig cols.
  get_meta1 <- function(group) {
    cols <- data_cols[grp[data_cols] == group]
    if (!length(cols)) return(rep(NA_real_, nrow(D)))
    vals <- vapply(cols, function(c) de_num(D[, c]), numeric(nrow(D)))
    if (is.null(dim(vals))) vals <- matrix(vals, ncol = length(cols))
    apply(vals, 1, function(r) { r <- r[!is.na(r)]; if (length(r)) r[1] else NA_real_ })
  }
  get_meta_st <- function(group, stimme) {
    cols <- data_cols[grp[data_cols] == group & st[data_cols] == stimme & !is.na(st[data_cols])]
    if (!length(cols)) return(rep(NA_real_, nrow(D)))
    de_num(D[, cols[1]])
  }
  elig   <- get_meta1("Wahlberechtigte")
  voters <- if (any(grp == "Wähler", na.rm = TRUE)) get_meta1("Wähler") else get_meta1("Wählende")
  inv_e  <- get_meta_st("Ungültige", "erststimme"); inv_z <- get_meta_st("Ungültige", "zweitstimme")
  val_e  <- get_meta_st("Gültige",   "erststimme"); val_z <- get_meta_st("Gültige",   "zweitstimme")

  party_cols <- data_cols[!(grp[data_cols] %in% meta_groups) &
                          !grepl("^Übrige|^Sonstige", grp[data_cols]) &
                          !is.na(st[data_cols]) & !is.na(grp[data_cols]) & grp[data_cols] != ""]
  plist <- lapply(party_cols, function(c) data.table(
    gebiet_nr = nr, name = name, land_nr = land_nr,
    party_raw = grp[c], stimme = st[c], votes = de_num(D[, c])
  ))
  parties <- rbindlist(plist)
  meta <- rbind(
    data.table(gebiet_nr = nr, name, land_nr, stimme = "erststimme",
               eligible_voters = elig, number_voters = voters, valid_votes = val_e, invalid_votes = inv_e),
    data.table(gebiet_nr = nr, name, land_nr, stimme = "zweitstimme",
               eligible_voters = elig, number_voters = voters, valid_votes = val_z, invalid_votes = inv_z)
  )
  win <- parties[gebiet_nr >= 1 & gebiet_nr <= 299 & stimme == "erststimme",
                 .(elected_raw = party_raw[which.max(votes)]), by = gebiet_nr]
  list(parties = parties, meta = meta, win = win, year = year)
}

#### Parser: kerg2 (flat) ####
parse_kerg2 <- function(path, year) {
  d <- fread(path, skip = "Wahlart", sep = ";", encoding = "UTF-8", colClasses = "character", fill = TRUE)
  setnames(d, 1, "Wahlart")
  d[, Anzahl := de_num(Anzahl)]
  d[, stimme := fifelse(Stimme == "1", "erststimme", fifelse(Stimme == "2", "zweitstimme", NA_character_))]
  d[, gnr := suppressWarnings(as.integer(Gebietsnummer))]
  wk <- d[Gebietsart == "Wahlkreis"]

  parties <- wk[Gruppenart == "Partei" & !is.na(stimme),
                .(gebiet_nr = gnr, name = Gebietsname,
                  land_nr = suppressWarnings(as.integer(UegGebietsnummer)),
                  stimme, party_raw = Gruppenname, votes = Anzahl)]
  mk <- function(nm) wk[Gruppenname == nm]
  elig <- mk("Wahlberechtigte")[, .(gebiet_nr = gnr, eligible_voters = Anzahl)]
  vot  <- wk[Gruppenname %in% c("Wählende", "Wähler"), .(gebiet_nr = gnr, number_voters = Anzahl)]
  val  <- mk("Gültige")[!is.na(stimme),  .(gebiet_nr = gnr, stimme, valid_votes = Anzahl)]
  inv  <- mk("Ungültige")[!is.na(stimme), .(gebiet_nr = gnr, stimme, invalid_votes = Anzahl)]
  nm   <- unique(wk[, .(gebiet_nr = gnr, name = Gebietsname, land_nr = suppressWarnings(as.integer(UegGebietsnummer)))])
  meta <- CJ(gebiet_nr = unique(wk$gnr), stimme = c("erststimme", "zweitstimme"))
  meta <- Reduce(function(a, b) merge(a, b, by = intersect(names(a), names(b)), all.x = TRUE),
                 list(meta, nm, elig, vot, val, inv))
  # Direktmandat winner = party with most Erststimmen per Wahlkreis (the kerg2
  # "Gewählt" flag exists only from 2025; max-Erststimme reproduces it exactly).
  win <- parties[stimme == "erststimme",
                 .(elected_raw = party_raw[which.max(votes)]), by = gebiet_nr]
  list(parties = parties, meta = meta, win = win, year = year)
}

#### Drive all elections ####
sources <- list(
  list(2002, "BTW02/btw02_kerg.csv",      parse_classic),
  list(2005, "BTW05/btw05_kerg.csv",      parse_classic),
  list(2009, "BTW09/btw09_kerg.csv",      parse_classic),
  list(2013, "BTW13/btw13_kerg.csv",      parse_classic),
  list(2017, "BTW17/btw17_kerg2.csv",     parse_kerg2),
  list(2021, "BTW21/w-btw21_kerg2.csv",   parse_kerg2),
  list(2025, "BTW25/btw25_kerg2.csv",     parse_kerg2)
)

all_parties <- list(); all_meta <- list()
for (src in sources) {
  yr <- src[[1]]; path <- file.path(raw_root, src[[2]]); fn <- src[[3]]
  r <- fn(path, yr)
  P <- r$parties[gebiet_nr >= 1 & gebiet_nr <= 299]
  M <- r$meta[gebiet_nr >= 1 & gebiet_nr <= 299]
  # --- per-year national reconciliation against the parser's own summary rows ---
  natl_nr <- if (any(r$parties$gebiet_nr == 999, na.rm = TRUE)) 999L else NA_integer_
  if (!is.na(natl_nr)) {
    b <- r$parties[gebiet_nr == natl_nr, .(bund = sum(votes, na.rm = TRUE)), by = .(party_raw, stimme)]
    s <- P[, .(wk = sum(votes, na.rm = TRUE)), by = .(party_raw, stimme)]
    cc <- merge(b, s, by = c("party_raw", "stimme"), all = TRUE)
    cc[is.na(bund), bund := 0][is.na(wk), wk := 0]
    if (max(abs(cc$bund - cc$wk)) != 0)
      stop(sprintf("Year %d: Wahlkreis sums do not reconcile to national totals.", yr))
  }
  P[, year := yr]; M[, year := yr]
  M <- merge(M, r$win, by = "gebiet_nr", all.x = TRUE)
  all_parties[[as.character(yr)]] <- P
  all_meta[[as.character(yr)]]    <- M
  cat(sprintf("  %d: %d Wahlkreise, %d party rows%s\n", yr, uniqueN(P$gebiet_nr), nrow(P),
              if (!is.na(natl_nr)) " (reconciles to national totals)" else ""))
}
parties <- rbindlist(all_parties, use.names = TRUE)
meta    <- rbindlist(all_meta, use.names = TRUE, fill = TRUE)

#### Normalise party names + aggregate ####
parties[, party := normalise_party_v(party_raw)]
parties <- parties[, .(votes = sum(votes, na.rm = TRUE)),
                   by = .(year, gebiet_nr, name, land_nr, stimme, party)]
meta[, elected_party := ifelse(is.na(elected_raw), NA_character_, normalise_party_v(elected_raw))]

setnames(parties, c("gebiet_nr", "name"), c("wkr_nr", "wkr_name"))
setnames(meta,    c("gebiet_nr", "name"), c("wkr_nr", "wkr_name"))
key_cols <- c("year", "wkr_nr", "stimme")

#### LONG output (counts + shares) ####
long <- merge(parties, meta[, .(year, wkr_nr, stimme, eligible_voters, number_voters,
                                valid_votes, invalid_votes)],
              by = key_cols, all.x = TRUE)
long[, vote_share := ifelse(valid_votes > 0, votes / valid_votes, NA_real_)]
long[, turnout := ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_)]
setnames(long, "year", "election_year")
long[, wkr_nr := sprintf("%03d", as.integer(wkr_nr))]
setcolorder(long, c("election_year", "wkr_nr", "wkr_name", "land_nr", "stimme", "party",
                    "votes", "vote_share", "eligible_voters", "number_voters",
                    "valid_votes", "invalid_votes", "turnout"))
setorder(long, election_year, wkr_nr, stimme, -votes)

#### WIDE output (shares, GERDA-style) ####
named_votes <- parties[, .(named = sum(votes, na.rm = TRUE)), by = key_cols]
base <- merge(meta, named_votes, by = key_cols, all.x = TRUE)
base[is.na(named), named := 0]
base[, other_votes := pmax(valid_votes - named, 0)]

shares <- dcast(parties, year + wkr_nr + stimme ~ party, value.var = "votes",
                fun.aggregate = sum, fill = 0)
party_cols <- setdiff(names(shares), key_cols)
wide <- merge(base, shares, by = key_cols, all.x = TRUE)
for (p in party_cols) wide[is.na(get(p)), (p) := 0]
# convert counts -> shares of valid_votes
for (p in party_cols) wide[, (p) := ifelse(valid_votes > 0, get(p) / valid_votes, NA_real_)]
wide[, other := ifelse(valid_votes > 0, other_votes / valid_votes, NA_real_)]
wide[, cdu_csu := rowSums(cbind(if ("cdu" %in% party_cols) cdu else 0,
                                if ("csu" %in% party_cols) csu else 0), na.rm = TRUE)]
wide[, turnout := ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_)]
wide[, flag_no_valid_votes := as.integer(is.na(valid_votes) | valid_votes == 0)]
wide[, flag_naive_turnout_above_1 := as.integer(!is.na(turnout) & turnout > 1)]

setnames(wide, "year", "election_year")
wide[, wkr_nr := sprintf("%03d", as.integer(wkr_nr))]
meta_front <- c("flag_no_valid_votes", "flag_naive_turnout_above_1",
                "election_year", "wkr_nr", "wkr_name", "land_nr", "stimme",
                "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
                "turnout", "elected_party")
ordered_party_cols <- sort(party_cols)
setcolorder(wide, c(meta_front, ordered_party_cols, "other", "cdu_csu"))
wide[, c("named", "other_votes", "elected_raw") := NULL]
setorder(wide, election_year, wkr_nr, stimme)

#### Validation ####
stopifnot(
  "each election has 299 Wahlkreise" =
    all(wide[, uniqueN(wkr_nr), by = election_year]$V1 == 299),
  "two stimmen per Wahlkreis" =
    nrow(wide) == wide[, uniqueN(paste(election_year, wkr_nr))] * 2
)
share_sum <- wide[valid_votes > 0, rowSums(.SD, na.rm = TRUE),
                  .SDcols = c(ordered_party_cols, "other")]
cat(sprintf("\nShare rows summing to 1.0 (tol 1e-6): %d / %d\n",
            sum(abs(share_sum - 1) < 1e-6), length(share_sum)))
cat(sprintf("Turnout range: [%.3f, %.3f]; rows turnout>1: %d\n",
            min(wide$turnout, na.rm = TRUE), max(wide$turnout, na.rm = TRUE),
            sum(wide$flag_naive_turnout_above_1)))
cat(sprintf("Direktmandate with a winner: %d / %d wkr-years\n",
            sum(!is.na(wide[stimme == "erststimme"]$elected_party)),
            nrow(wide[stimme == "erststimme"])))
cat(sprintf("Rows: long=%d, wide=%d; party columns=%d; years=%s\n",
            nrow(long), nrow(wide), length(ordered_party_cols),
            paste(sort(unique(wide$election_year)), collapse = ",")))

#### Write ####
fwrite(long, file.path(out_dir, "federal_wkr_unharm_long.csv"))
saveRDS(as.data.frame(long), file.path(out_dir, "federal_wkr_unharm_long.rds"))
fwrite(wide, file.path(out_dir, "federal_wkr_unharm.csv"))
saveRDS(as.data.frame(wide), file.path(out_dir, "federal_wkr_unharm.rds"))
cat("\nWrote federal_wkr_unharm(.long).{csv,rds} to", out_dir, "\n")
