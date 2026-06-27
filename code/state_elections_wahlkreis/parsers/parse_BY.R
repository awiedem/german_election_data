# =============================================================================
# Stage-1 cleaning parser: Bayern (BY) Landtagswahl at Stimmkreis level
# Constituency unit: Stimmkreis (~91). Two votes: erststimme + zweitstimme.
#
# Machine-readable sources only (PDF scans excluded -> future OCR stage):
#   - BY_2018_Landtagswahl_Stimmkreise.csv  (91 Stimmkreise, current+prev year)
#   - BY_2023_Landtagswahl_Stimmkreise.csv  (91 Stimmkreise, current+prev year)
# Validation against the statewide "990;Bayern" row of the Wahlkreise CSVs.
#
# Output: long tidy CSV, one row per (Stimmkreis x stimme x party).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_BY.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Bayern")
out_dir <- here("data", "state_elections", "processed", "wahlkreis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ELECTION_DATES <- c("2018" = "2018-10-14", "2023" = "2023-10-08")

# ---------------------------------------------------------------------------
# Helper: read a BY votemanager-style ';'-separated, ISO-8859-1 CSV into a
# character data.frame whose names are the literal header tokens (verbatim).
# ---------------------------------------------------------------------------
read_by_csv <- function(path) {
  lines <- readLines(path, encoding = "latin1", warn = FALSE)
  lines <- enc2utf8(lines)
  # Some rows may have a trailing empty field; split on ';' keeping all fields.
  split_line <- function(x) strsplit(x, ";", fixed = TRUE)[[1]]
  hdr <- split_line(lines[1])
  body <- lines[-1]
  body <- body[nzchar(trimws(body))]
  mat <- lapply(body, function(x) {
    f <- split_line(x)
    length(f) <- length(hdr)            # pad short rows to header width
    f
  })
  df <- as.data.frame(do.call(rbind, mat), stringsAsFactors = FALSE)
  names(df) <- hdr
  df
}

# Parse German integer (no thousands sep here; "X" / "-" / "" = missing)
to_int <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("X", "-", "", ".", "NA")] <- NA
  suppressWarnings(as.integer(gsub("[^0-9-]", "", x)))
}

# ---------------------------------------------------------------------------
# Identify, for a given header + target year, the party blocks:
# columns named "Erststimmen <P> <year>" / "Zweitstimmen <P> <year>".
# Returns a data.frame: party_raw, erst_col_idx, zweit_col_idx.
# ---------------------------------------------------------------------------
party_blocks <- function(hdr, year) {
  hdr_t <- trimws(hdr)
  yr <- as.character(year)
  erst_pat  <- paste0("^Erststimmen (.+) ", yr, "$")
  zweit_pat <- paste0("^Zweitstimmen (.+) ", yr, "$")

  erst_idx  <- which(grepl(erst_pat,  hdr_t))
  zweit_idx <- which(grepl(zweit_pat, hdr_t))

  erst_party  <- sub(erst_pat,  "\\1", hdr_t[erst_idx])
  zweit_party <- sub(zweit_pat, "\\1", hdr_t[zweit_idx])

  e <- data.frame(party_raw = erst_party,  erst_col = erst_idx,
                  stringsAsFactors = FALSE)
  z <- data.frame(party_raw = zweit_party, zweit_col = zweit_idx,
                  stringsAsFactors = FALSE)
  m <- merge(e, z, by = "party_raw", all = TRUE)
  m
}

# ---------------------------------------------------------------------------
# Locate a meta column by exact name, with year-tolerant fallback.
# ---------------------------------------------------------------------------
col_idx <- function(hdr, name) {
  i <- which(trimws(hdr) == name)
  if (length(i) == 0) NA_integer_ else i[1]
}

# ---------------------------------------------------------------------------
# Parse one Stimmkreise file into long format for the given year.
# ---------------------------------------------------------------------------
parse_stimmkreise <- function(path, year) {
  df  <- read_by_csv(path)
  hdr <- names(df)
  yr  <- as.character(year)

  # meta columns (per Stimmkreis)
  i_key   <- col_idx(hdr, "Schlüsselnummer")
  i_name  <- col_idx(hdr, "Name der Regionaleinheit")
  i_eli   <- col_idx(hdr, "Stimmberechtigte")
  i_vot   <- col_idx(hdr, "Wähler")
  i_ge    <- col_idx(hdr, paste0("gültige Erststimmen insgesamt ", yr))
  i_ue    <- col_idx(hdr, paste0("ungültige Erststimmen ", yr))
  i_gz    <- col_idx(hdr, paste0("gültige Zweitstimmen insgesamt ", yr))
  i_uz    <- col_idx(hdr, paste0("ungültige Zweitstimmen ", yr))

  stopifnot(!is.na(i_key), !is.na(i_name), !is.na(i_eli), !is.na(i_vot),
            !is.na(i_ge), !is.na(i_ue), !is.na(i_gz), !is.na(i_uz))

  pb <- party_blocks(hdr, year)

  meta <- data.frame(
    wkr_nr          = trimws(df[[i_key]]),
    wkr_name        = trimws(df[[i_name]]),
    eligible_voters = to_int(df[[i_eli]]),
    number_voters   = to_int(df[[i_vot]]),
    valid_erst      = to_int(df[[i_ge]]),
    invalid_erst    = to_int(df[[i_ue]]),
    valid_zweit     = to_int(df[[i_gz]]),
    invalid_zweit   = to_int(df[[i_uz]]),
    stringsAsFactors = FALSE
  )

  n <- nrow(meta)
  rows <- vector("list", 0L)

  for (k in seq_len(nrow(pb))) {
    p  <- pb$party_raw[k]
    ec <- pb$erst_col[k]
    zc <- pb$zweit_col[k]

    if (!is.na(ec)) {
      ev <- to_int(df[[ec]])
      rows[[length(rows) + 1L]] <- data.frame(
        wkr_nr = meta$wkr_nr, wkr_name = meta$wkr_name,
        eligible_voters = meta$eligible_voters,
        number_voters = meta$number_voters,
        stimme = "erststimme",
        valid_votes = meta$valid_erst, invalid_votes = meta$invalid_erst,
        party_raw = p, votes = ev, stringsAsFactors = FALSE
      )
    }
    if (!is.na(zc)) {
      zv <- to_int(df[[zc]])
      rows[[length(rows) + 1L]] <- data.frame(
        wkr_nr = meta$wkr_nr, wkr_name = meta$wkr_name,
        eligible_voters = meta$eligible_voters,
        number_voters = meta$number_voters,
        stimme = "zweitstimme",
        valid_votes = meta$valid_zweit, invalid_votes = meta$invalid_zweit,
        party_raw = p, votes = zv, stringsAsFactors = FALSE
      )
    }
  }
  long <- bind_rows(rows)

  # Drop rows where the party did not contest in the target year (votes NA from "X").
  long <- long %>% filter(!is.na(votes))

  long$state_abbr     <- "BY"
  long$state          <- "Bayern"
  long$election_year  <- as.integer(year)
  long$election_date  <- ELECTION_DATES[[yr]]

  long <- long %>%
    select(state_abbr, state, election_year, election_date,
           wkr_nr, wkr_name, stimme,
           eligible_voters, number_voters, valid_votes, invalid_votes,
           party_raw, votes)
  long
}

# ---------------------------------------------------------------------------
# Build the statewide-total validation table from the Wahlkreise CSV's
# "990;Bayern" row: party_raw x stimme -> votes.
# ---------------------------------------------------------------------------
parse_state_total <- function(path, year) {
  df  <- read_by_csv(path)
  hdr <- names(df)
  i_key <- col_idx(hdr, "Schlüsselnummer")
  row   <- df[trimws(df[[i_key]]) == "990", , drop = FALSE]
  stopifnot(nrow(row) == 1)
  pb <- party_blocks(hdr, year)

  out <- vector("list", 0L)
  for (k in seq_len(nrow(pb))) {
    p <- pb$party_raw[k]
    if (!is.na(pb$erst_col[k])) {
      out[[length(out)+1L]] <- data.frame(
        party_raw = p, stimme = "erststimme",
        total = to_int(row[[pb$erst_col[k]]]), stringsAsFactors = FALSE)
    }
    if (!is.na(pb$zweit_col[k])) {
      out[[length(out)+1L]] <- data.frame(
        party_raw = p, stimme = "zweitstimme",
        total = to_int(row[[pb$zweit_col[k]]]), stringsAsFactors = FALSE)
    }
  }
  res <- bind_rows(out) %>% filter(!is.na(total))
  res$election_year <- as.integer(year)
  res
}

# ===========================================================================
# RUN
# ===========================================================================
jobs <- list(
  list(year = 2018,
       sk = "BY_2018_Landtagswahl_Stimmkreise.csv",
       wk = "BY_2018_Landtagswahl_Wahlkreise.csv"),
  list(year = 2023,
       sk = "BY_2023_Landtagswahl_Stimmkreise.csv",
       wk = "BY_2023_Landtagswahl_Wahlkreise.csv")
)

all_long   <- list()
all_totals <- list()

for (j in jobs) {
  cat("\n=== Parsing", j$year, "===\n")
  long <- parse_stimmkreise(file.path(raw_dir, j$sk), j$year)
  tot  <- parse_state_total(file.path(raw_dir, j$wk), j$year)
  all_long[[as.character(j$year)]]   <- long
  all_totals[[as.character(j$year)]] <- tot

  cat("  Stimmkreise:", length(unique(long$wkr_nr)), "\n")
  cat("  rows:", nrow(long), "\n")
}

combined <- bind_rows(all_long)

# ---------------------------------------------------------------------------
# VALIDATION
# ---------------------------------------------------------------------------
cat("\n===== VALIDATION =====\n")

# (a) per (wkr, stimme): sum(party votes) vs valid_votes
chk_a <- combined %>%
  group_by(election_year, wkr_nr, stimme) %>%
  summarise(sum_party = sum(votes), valid = first(valid_votes),
            .groups = "drop") %>%
  mutate(disc = abs(sum_party - valid))
cat("(a) per-(wkr,stimme) integrity:\n")
cat("    groups checked:", nrow(chk_a), "\n")
cat("    max abs discrepancy:", max(chk_a$disc, na.rm = TRUE), "\n")
bad_a <- chk_a %>% filter(disc > 0)
if (nrow(bad_a) > 0) {
  cat("    NONZERO discrepancies:\n"); print(as.data.frame(bad_a))
}

# (b) statewide total match per (party, stimme)
cat("\n(b) statewide-total match (vs Wahlkreise 990;Bayern):\n")
for (yr in names(all_totals)) {
  mine <- combined %>% filter(election_year == as.integer(yr)) %>%
    group_by(party_raw, stimme) %>%
    summarise(my_sum = sum(votes), .groups = "drop")
  src <- all_totals[[yr]] %>% select(party_raw, stimme, total)
  cmp <- full_join(mine, src, by = c("party_raw", "stimme")) %>%
    mutate(my_sum = coalesce(my_sum, 0L),
           total  = coalesce(total, 0L),
           disc = abs(my_sum - total))
  fails <- cmp %>% filter(disc > 0)
  cat("  ", yr, ": max disc =", max(cmp$disc), "; n_party_stimme =",
      nrow(cmp), "; match =", nrow(fails) == 0, "\n")
  if (nrow(fails) > 0) print(as.data.frame(fails))
}

# (c) Wahlkreis count per year
cat("\n(c) Stimmkreis count per year:\n")
print(combined %>% group_by(election_year) %>%
        summarise(n_wkr = n_distinct(wkr_nr)))

cat("\nDistinct party_raw:\n")
print(sort(unique(combined$party_raw)))

# ---------------------------------------------------------------------------
# WRITE
# ---------------------------------------------------------------------------
out_path <- file.path(out_dir, "BY_ltw_wkr_long.csv")
fwrite(combined, out_path)
cat("\nWrote", nrow(combined), "rows to", out_path, "\n")
