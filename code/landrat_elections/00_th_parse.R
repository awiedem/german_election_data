### Parse Thüringen Landratswahl data
# Vincent Heddesheimer, May 2026
#
# Source: pre-loaded raw files at data/mayoral_elections/raw/thueringen/
# (downloaded from wahlen.thueringen.de). Naming convention:
#   LRInfoG{YYYY}.xlsx  = Hauptwahl Landrat (Gemeinde-detail), ALL Kreise statewide
#   LSInfoG{YYYY}.xlsx  = Stichwahl Landrat (Gemeinde-detail), ALL Kreise statewide
#   LRInfoG{YYYY}_{KK}.xlsx  = Hauptwahl, single mid-term election in Kreis KK
#   LSInfoG{YYYY}_{KK}.xlsx  = Stichwahl,  single mid-term election in Kreis KK
#   LRInfo{YYYY}.xlsx   = Hauptwahl, Kreis-level only (no Gemeinde detail) [OPTIONAL]
#   LSInfo{YYYY}.xlsx   = Stichwahl, Kreis-level only [OPTIONAL]
#
# Each xlsx has sheets named "Kreis 0NN":
#   Kreis 051..056 = kreisfreie Städte (OB elections — skipped here)
#   Kreis 061..077 = Landkreise (Landratswahl — what we want)
#
# Sheet structure (per Kreis):
#   Row 1: title ("Landratswahl 2006 - Freistaat Thüringen")
#   Row 3: timestamp ("erstellt am: ...")
#   Row 5-7: 3-row header (multi-line column labels)
#   Row 6: candidate-names header (cols 14, 16, 18, ... = "Name (Partei)")
#   Row 8: Kreis-level summary (Satzart="K", Gemeinde-nr="000")
#   Row 10+: per-Gemeinde rows (Satzart="G")
#
# Cols (positional):
#   1=Stand (E/V/Z), 2=Satzart (K/G), 3=Kreis-nr, 4=Gemeinde-nr, 5=Stimmbezirksnr,
#   6=Name, 7=Stimmbezirke insgesamt, 8=Stimmbezirke erfasst,
#   9=Wahlberechtigte, 10=Wähler, 11=Wahlbeteiligung %,
#   12=Ungültige, 13=Gültige,
#   14+: candidate pairs (votes, %)
#
# Output: writes parsed data to .rds for the combine script to pick up.

rm(list = ls())
gc()

pacman::p_load(tidyverse, readxl, here, conflicted)
conflict_prefer("filter", "dplyr"); conflict_prefer("year", "lubridate")
setwd(here::here())

# Where the files live (user placed them under mayoral_elections/raw/thueringen/)
src_dir <- "data/mayoral_elections/raw/thueringen"
out_file <- "data/landrat_elections/raw/thueringen_parsed.rds"
dir.create("data/landrat_elections/raw", recursive = TRUE, showWarnings = FALSE)

# Map TH 3-digit Kreis-nr → Kreis name (matches GERDA convention)
th_kreis_lookup <- c(
  "061" = "Landkreis Eichsfeld",
  "062" = "Landkreis Nordhausen",
  "063" = "Wartburgkreis",
  "064" = "Unstrut-Hainich-Kreis",
  "065" = "Kyffhäuserkreis",
  "066" = "Landkreis Schmalkalden-Meiningen",
  "067" = "Landkreis Gotha",
  "068" = "Landkreis Sömmerda",
  "069" = "Landkreis Hildburghausen",
  "070" = "Ilm-Kreis",
  "071" = "Landkreis Weimarer Land",
  "072" = "Landkreis Sonneberg",
  "073" = "Landkreis Saalfeld-Rudolstadt",
  "074" = "Saale-Holzland-Kreis",
  "075" = "Saale-Orla-Kreis",
  "076" = "Landkreis Greiz",
  "077" = "Landkreis Altenburger Land"
)

split_name_party <- function(s) {
  s <- str_squish(s)
  pty <- str_match(s, "\\(([^)]+)\\)$")[, 2]
  nm  <- str_squish(sub("\\s*\\([^)]+\\)$", "", s))
  list(name = nm, party = pty)
}

parse_th_sheet <- function(file, sheet) {
  d <- suppressMessages(read_excel(file, sheet = sheet, col_names = FALSE))
  if (nrow(d) == 0 || ncol(d) == 0) return(NULL)

  # Title is in row 1 of column 1
  title <- as.character(d[[1]][1])
  # 2006/2012 use "Landratswahl"; 2018+ use "Wahl der Landräte und
  # Oberbürgermeister..." for combined files. Accept both.
  if (!grepl("Landrat|Landrät", title, ignore.case = TRUE)) return(NULL)

  # Year from title (4-digit 20xx) or filename
  yr <- as.integer(str_extract(title, "20\\d{2}"))
  if (is.na(yr)) yr <- as.integer(str_extract(basename(file), "20\\d{2}"))

  # Date from any row 1-4 ("erstellt am:" or "Stand:" + dd.mm.yyyy)
  date_str <- NA_character_
  for (i in 1:4) {
    ts <- as.character(d[[1]][i])
    m <- str_extract(ts, "\\d{2}\\.\\d{2}\\.\\d{4}")
    if (!is.na(m)) { date_str <- m; break }
  }
  election_date <- if (!is.na(date_str)) {
    as.Date(date_str, format = "%d.%m.%Y")
  } else {
    as.Date(sprintf("%d-01-01", yr))  # fallback
  }

  # Sheet name varies between years:
  #   2006/2012: "Kreis 061", "Kreis 077" (3-digit zero-padded)
  #   2014+:     "Kreis 61", "Kreis 77"   (2-digit, no padding)
  kreis_digits <- str_extract(sheet, "\\d+")
  if (is.na(kreis_digits)) return(NULL)
  kreis_nr3 <- str_pad(kreis_digits, width = 3, side = "left", pad = "0")
  if (!kreis_nr3 %in% names(th_kreis_lookup)) return(NULL)
  kreis_name <- th_kreis_lookup[[kreis_nr3]]
  ags8 <- paste0("16", kreis_nr3, "000")

  # Find the Kreis-level summary row (Satzart "K", Gemeinde-nr "000")
  # Use whatever row has K + 000 (varies between row 8 and row 10 across years).
  if (ncol(d) < 4) return(NULL)
  sa_col <- as.character(d[[2]])
  gn_col <- as.character(d[[4]])
  k_idx <- which(!is.na(sa_col) & sa_col == "K" &
                 !is.na(gn_col) & gn_col == "000")
  if (length(k_idx) == 0) return(NULL)
  k_row <- d[k_idx[1], ]

  # Find candidate-name header row. The K-row is preceded by 2-4 header rows.
  # Try to find a row above K_idx where ≥2 columns contain "Last, First" patterns.
  cand_cols <- integer(0)
  hdr_row <- character(ncol(d))
  for (try_row in (k_idx[1] - 1):max(1, k_idx[1] - 6)) {
    if (try_row < 1) break
    candidate_hdr <- as.character(d[try_row, ])
    # Look for typical candidate-name pattern: contains comma + alphabetic.
    # NB: perl = TRUE is REQUIRED. R's default TRE engine does not honour
    # \w / \s inside a bracket expression, so a class like [\\w\\s.\\-äöü...]
    # silently degrades to a literal set plus the range \ (0x5C) - ä (0xE4),
    # which excludes space, hyphen, digits and A-Z. That dropped every
    # hyphenated ("Schmidt-Rose, Christiane") and title-prefixed ("Dr.
    # Brodführer, Michael") candidate, and whole sheets where all candidates
    # were of that shape. "[^,]*" + perl is both simpler and correct; it still
    # rejects "Sonstige Wahlvorschläge", "Anzahl" and "%".
    name_like <- grepl("^\\s*[A-ZÄÖÜ][^,]*,\\s+[A-ZÄÖÜ]", candidate_hdr, perl = TRUE)
    cols <- which(name_like)
    if (length(cols) >= 1) {
      cand_cols <- cols
      hdr_row <- candidate_hdr
      break
    }
  }
  if (length(cand_cols) == 0) return(NULL)

  candidates <- list()
  for (col_idx in cand_cols) {
    nm_raw <- hdr_row[col_idx]
    sp <- split_name_party(nm_raw)
    votes <- suppressWarnings(as.numeric(k_row[[col_idx]]))
    if (is.na(votes)) next
    candidates[[length(candidates) + 1]] <- tibble(
      candidate_name = sp$name,
      candidate_party = sp$party,
      candidate_votes = votes
    )
  }
  if (length(candidates) == 0) return(NULL)
  cand_df <- bind_rows(candidates)

  # Vote-summary columns shift between years too. Find them by scanning
  # known label rows for "Wahlberechtigte", "Wähler", "Ungültige", "Gültige".
  eligible <- voters <- invalid <- valid <- NA_real_
  # Standard layout: cols 9-13 are eligible/voters/Wahlbet/invalid/valid
  if (ncol(d) >= 13) {
    eligible <- suppressWarnings(as.numeric(k_row[[9]]))
    voters   <- suppressWarnings(as.numeric(k_row[[10]]))
    invalid  <- suppressWarnings(as.numeric(k_row[[12]]))
    valid    <- suppressWarnings(as.numeric(k_row[[13]]))
  }

  cand_df %>%
    mutate(
      ags = ags8, ags_name = kreis_name,
      state = "16", state_name = "Thüringen",
      election_year = yr,
      election_date = election_date,
      election_type = "Landratswahl",
      eligible_voters = eligible,
      number_voters = voters,
      valid_votes = valid,
      invalid_votes = invalid,
      # NB: `eligible` / `valid` are length-1 scalars, so ifelse() would return
      # a length-1 result and mutate() would RECYCLE the first candidate's value
      # onto every candidate in the sheet. Use scalar `if`/`else` on the scalar
      # condition and let the vectorised division do the work.
      turnout = if (!is.na(eligible) && eligible > 0) {
        voters / eligible
      } else NA_real_,
      candidate_voteshare = if (!is.na(valid) && valid > 0) {
        candidate_votes / valid
      } else NA_real_
    )
}

cat("=== Thüringen Landratswahl parser ===\n\n")

# Process all LR* and LS* xlsx files (with Gemeinde detail = "G" suffix)
files <- list.files(src_dir, pattern = "^L[RS]InfoG?\\d{4}.*\\.xlsx$",
                     full.names = TRUE)
cat(sprintf("Found %d Thüringen files\n", length(files)))

all_rows <- list()
for (f in files) {
  fname <- basename(f)
  is_sw <- grepl("^LSInfo", fname)
  round <- if (is_sw) "stichwahl" else "hauptwahl"

  sheets <- excel_sheets(f)
  # Only Landrat sheets (Kreis 61..77 or Kreis 061..077). Skip the
  # kreisfreie-Stadt sheets (51..56 / 051..056) which are OB elections.
  is_lr <- function(s) {
    n <- suppressWarnings(as.integer(str_extract(s, "\\d+")))
    !is.na(n) && n >= 61 && n <= 77
  }
  lr_sheets <- sheets[map_lgl(sheets, is_lr)]

  cat(sprintf("\n%s: %d Landrat sheets\n", fname, length(lr_sheets)))
  for (sh in lr_sheets) {
    parsed <- tryCatch(parse_th_sheet(f, sh),
                       error = function(e) { cat("    ERR:", sh, ":", e$message, "\n"); NULL })
    if (!is.null(parsed) && nrow(parsed) > 0) {
      parsed$round <- round
      parsed$source_file <- fname
      all_rows[[length(all_rows) + 1]] <- parsed
    }
  }
}

if (length(all_rows) == 0) {
  stop("No Thüringen rows parsed")
}

th_data <- bind_rows(all_rows)

# ---------------------------------------------------------------------------
# Correct election_date: the sheets carry only a report timestamp
# ---------------------------------------------------------------------------
# The "erstellt am:"/"Stand:" line is the date the report was GENERATED, always
# a few days to three weeks AFTER polling day, and it is the only dd.mm.yyyy
# token anywhere in these workbooks — the true Wahltag is simply not in the
# file. Left uncorrected, every Thüringen Landrat date is a weekday (41 Thu,
# 36 Fri, 13 Mon, 6 Wed) and the three sheets with no timestamp at all fall
# back to 1 January.
#
# The true polling days are recoverable from the Gemeinde-level Bürgermeister
# scrape, which covers the same statewide Kommunalwahl days: for each round we
# take the latest polling day of that round on or before the timestamp. Where a
# sheet has no timestamp we fall back to that year's modal polling day, which is
# correct for the mid-term Kreise that voted with the statewide cycle.
th_bm_file <- "data/mayoral_elections/raw/thueringen_bm/th_bm_scraped.csv"
if (!file.exists(th_bm_file)) {
  stop("Cannot correct Thüringen election dates: ", th_bm_file, " is missing. ",
       "Run code/mayoral_elections/00_th_scrape.py first.")
}
th_polling <- readr::read_csv(th_bm_file, show_col_types = FALSE) %>%
  mutate(election_date = as.Date(election_date)) %>%
  count(election_date, round, name = "n_gemeinden") %>%
  filter(n_gemeinden >= 3)   # drop one-off single-Gemeinde dates

correct_th_date <- function(stamp, rnd, yr) {
  cands <- th_polling$election_date[th_polling$round == rnd]
  if (is.na(stamp) || format(stamp, "%m-%d") == "01-01") {
    # no timestamp in the sheet: use that year's modal polling day for the round
    same_yr <- th_polling[th_polling$round == rnd &
                            format(th_polling$election_date, "%Y") == as.character(yr), ]
    if (nrow(same_yr) == 0) return(NA_Date_)
    return(same_yr$election_date[which.max(same_yr$n_gemeinden)])
  }
  prior <- cands[cands <= stamp & as.numeric(stamp - cands) <= 30]
  if (length(prior) == 0) return(NA_Date_)
  max(prior)
}

th_data <- th_data %>%
  mutate(
    election_date_stand = election_date,
    election_date = as.Date(mapply(correct_th_date, election_date, round, election_year,
                                   SIMPLIFY = TRUE), origin = "1970-01-01")
  )

# A single-Kreis runoff need not coincide with any Gemeinde runoff, so it has no
# polling day to match against. Thüringen holds the Stichwahl exactly two weeks
# after the Hauptwahl (§ 24 ThürKWG) — a rule that holds for every one of the
# cycles resolved above — so derive those from the Kreis's own Hauptwahl.
sw_from_hw <- th_data %>%
  filter(round == "hauptwahl", !is.na(election_date)) %>%
  distinct(ags, election_year, hw_date = election_date)

th_data <- th_data %>%
  left_join(sw_from_hw, by = c("ags", "election_year")) %>%
  mutate(
    election_date = if_else(is.na(election_date) & round == "stichwahl" & !is.na(hw_date),
                            hw_date + 14, election_date)
  ) %>%
  select(-hw_date)

bad_date <- th_data %>% filter(is.na(election_date))
if (nrow(bad_date) > 0) {
  stop(sprintf("Could not resolve a polling day for %d Thüringen rows (%s). ",
               nrow(bad_date),
               paste(unique(format(bad_date$election_date_stand, "%Y-%m-%d")), collapse = ", ")),
       "Check th_bm_scraped.csv covers these cycles.")
}
not_sunday <- th_data %>% filter(weekdays(election_date) != "Sonntag" &
                                   weekdays(election_date) != "Sunday")
if (nrow(not_sunday) > 0) {
  stop(sprintf("%d Thüringen rows resolved to a non-Sunday date: %s",
               nrow(not_sunday),
               paste(unique(as.character(not_sunday$election_date)), collapse = ", ")))
}
cat("\nElection dates corrected from report timestamps to polling days:\n")
print(th_data %>% distinct(election_date_stand, round, election_date) %>%
        arrange(election_date))

cat(sprintf("\n=== Parsed %d candidate-rows from Thüringen ===\n", nrow(th_data)))
cat("By year/round:\n")
print(th_data %>% count(election_year, round))

# Cache to RDS for the combine script
saveRDS(th_data, out_file)
cat(sprintf("\n✓ Saved to %s\n", out_file))
