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
    # Look for typical candidate-name pattern: contains comma + alphabetic
    name_like <- grepl("^\\s*[A-ZÄÖÜ][\\w\\s.\\-äöüÄÖÜß]+,\\s+[A-ZÄÖÜ]", candidate_hdr)
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
      turnout = ifelse(!is.na(eligible) & eligible > 0,
                       voters / eligible, NA_real_),
      candidate_voteshare = ifelse(!is.na(valid) & valid > 0,
                                    candidate_votes / valid, NA_real_)
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

cat(sprintf("\n=== Parsed %d candidate-rows from Thüringen ===\n", nrow(th_data)))
cat("By year/round:\n")
print(th_data %>% count(election_year, round))

# Cache to RDS for the combine script
saveRDS(th_data, out_file)
cat(sprintf("\n✓ Saved to %s\n", out_file))
