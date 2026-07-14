#### Bundestagswahlen at WAHLKREIS (constituency) level — Stage 0: raw grab ####
## Downloads the official Bundeswahlleiterin constituency-level result files
## (kerg / kerg2) for every Bundestagswahl 2002-2025, plus the 2025 reference
## tables (constituency names, party list) and the official 2021->2025
## constituency recomputation used to build the boundary crosswalk.
##
## Source: Die Bundeswahlleiterin, Wiesbaden. Open data under
## "Datenlizenz Deutschland – Namensnennung – Version 2.0".
## Files are stored VERBATIM under data/federal_elections/wahlkreis_level/raw/,
## mirroring the federal municipality_level / county_level raw convention and
## the state_elections_wahlkreis raw tree. Cleaning happens in later stages.
##
## Two file formats appear:
##   - kerg2  (flat/long, one row per Gebiet x Gruppe x Stimme): 2017, 2021, 2025
##   - kerg   (classic wide, multi-row header):                   2002-2025
## The parser stage prefers kerg2 where available and falls back to classic kerg.
##
## Output: data/federal_elections/wahlkreis_level/raw/BTW{02..25}/<file>.csv
##
## Run: Rscript code/federal_elections_wahlkreis/00_download_raw.R
## Authors: Hanno Hilbig (with Claude Code assistance), July 2026

#### Setup ####
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
here::i_am("code/federal_elections_wahlkreis/00_download_raw.R")

raw_root <- here("data", "federal_elections", "wahlkreis_level", "raw")
dir.create(raw_root, recursive = TRUE, showWarnings = FALSE)

ROOT <- "https://www.bundeswahlleiterin.de"

## (election_folder, local_filename, absolute_url) ---------------------------
## URLs harvested from each election's official results page (DAM asset links
## for 2002-2021; the structured opendata directory for 2025).
files <- tibble::tribble(
  ~folder, ~name,                        ~url,
  # ---- classic kerg (wide) ----
  "BTW02", "btw02_kerg.csv",             paste0(ROOT, "/dam/jcr/ebcc973a-c60c-422a-a6e4-8c8626d13777/btw02_kerg.csv"),
  "BTW05", "btw05_kerg.csv",             paste0(ROOT, "/dam/jcr/01e65a67-6d5b-47ee-b8ec-0fd243493ebc/btw05_kerg.csv"),
  "BTW09", "btw09_kerg.csv",             paste0(ROOT, "/dam/jcr/0d80891b-aa0b-443b-9a45-007d28a9b817/btw09_kerg.csv"),
  "BTW13", "btw13_kerg.csv",             paste0(ROOT, "/dam/jcr/2ace94c3-15cb-476b-aa98-806b97209353/btw13_kerg.csv"),
  # ---- classic + kerg2 (flat) ----
  "BTW17", "btw17_kerg.csv",             paste0(ROOT, "/dam/jcr/72f186bb-aa56-47d3-b24c-6a46f5de22d0/btw17_kerg.csv"),
  "BTW17", "btw17_kerg2.csv",            paste0(ROOT, "/dam/jcr/0d1ea773-f3ca-40ea-b8ff-b031712707e1/btw17_kerg2.csv"),
  "BTW21", "w-btw21_kerg.csv",           paste0(ROOT, "/dam/jcr/73cb0955-d638-451c-8cda-44c3eb22d5a5/w-btw21_kerg.csv"),
  "BTW21", "w-btw21_kerg2.csv",          paste0(ROOT, "/dam/jcr/860495c9-83fb-4068-8a99-c1c985ffffd2/w-btw21_kerg2.csv"),
  "BTW25", "btw25_kerg.csv",             paste0(ROOT, "/bundestagswahlen/2025/ergebnisse/opendata/btw25/csv/kerg.csv"),
  "BTW25", "btw25_kerg2.csv",            paste0(ROOT, "/bundestagswahlen/2025/ergebnisse/opendata/btw25/csv/kerg2.csv"),
  # ---- 2025 reference tables + official 2021->2025 recomputation (crosswalk) ----
  "BTW25", "btw25_wahlkreisnamen_utf8.csv", paste0(ROOT, "/dam/jcr/17e066f6-a0af-42df-a5d2-365dc87769ab/btw25_wahlkreisnamen_utf8.csv"),
  "BTW25", "btw25_parteien.csv",         paste0(ROOT, "/dam/jcr/925d6e98-3616-465a-835a-cec1ea73abc2/btw25_parteien.csv"),
  "BTW25", "btwkr25_umrechnung_btw21.csv", paste0(ROOT, "/dam/jcr/201df610-802b-4121-9e6b-5d04504df5c8/btwkr25_umrechnung_btw21.csv"),
  "BTW25", "btw25_wkr_gemeinden_20241130_utf8.csv", paste0(ROOT, "/dam/jcr/aa868597-0e60-476c-bd2b-279c1e9a142a/btw25_wkr_gemeinden_20241130_utf8.csv")
)

#### Download ####
options(timeout = max(300, getOption("timeout")))

download_one <- function(folder, name, url) {
  dest_dir <- file.path(raw_root, folder)
  dir.create(dest_dir, showWarnings = FALSE)
  dest <- file.path(dest_dir, name)
  if (file.exists(dest) && file.info(dest)$size > 1024) {
    cat(sprintf("  skip  %-45s (exists, %d bytes)\n", file.path(folder, name), file.info(dest)$size))
    return(invisible(TRUE))
  }
  ok <- tryCatch({
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) { cat("  FAIL ", name, ":", conditionMessage(e), "\n"); FALSE })
  sz <- if (file.exists(dest)) file.info(dest)$size else 0
  if (ok && sz > 1024) {
    cat(sprintf("  ok    %-45s (%d bytes)\n", file.path(folder, name), sz))
  } else {
    cat(sprintf("  ERROR %-45s (%d bytes — likely an error page)\n", file.path(folder, name), sz))
  }
  invisible(ok && sz > 1024)
}

cat("Downloading", nrow(files), "files to", raw_root, "\n")
res <- mapply(download_one, files$folder, files$name, files$url)
cat(sprintf("\nDone: %d/%d files present.\n", sum(res), nrow(files)))
