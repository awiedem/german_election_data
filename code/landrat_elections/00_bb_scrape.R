### Scrape Landratswahl data for Brandenburg
# Vincent Heddesheimer, May 2026
#
# Sources:
#   Master index:
#     https://wahlen.brandenburg.de/wahlen/de/kommunalwahlen/ergebnisse/landraetewahlen/
#   Per-Kreis result pages (linked from the index):
#     /wahlen/de/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-{slug}/
#       (Hauptwahl)
#     /wahlen/de/kommunalwahlen/ergebnisse/landraetewahlen/ergebnis-stichwahl-landrat-{slug}/
#       (Stichwahl — note the slightly different path)
#
# Each result page contains a static HTML table with:
#   - Wahlberechtigte, Wählerinnen und Wähler/Wahlbeteiligung
#   - Ungültige Stimmen, Gültige Stimmen
#   - Per-candidate rows: "Name, Vorname (Partei)" + Anzahl + Prozent
#
# Coverage: most recent cycle of each Brandenburg Landkreis only.
# Pre-2018 (1993, 1994, 2002, 2010) are NOT on this portal.

rm(list = ls())
gc()

pacman::p_load(tidyverse, here, conflicted, rvest, xml2)
conflict_prefer("filter", "dplyr")
setwd(here::here())

raw_dir <- "data/landrat_elections/raw/brandenburg"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== BB Landratswahl scraper ===\n\n")

base <- "https://wahlen.brandenburg.de"
index_url <- paste0(base, "/wahlen/de/kommunalwahlen/ergebnisse/landraetewahlen/")

# Step 1: fetch master index, save it, extract Kreis-page URLs
idx_file <- file.path(raw_dir, "_master_index.html")
download.file(index_url, idx_file, mode = "wb", quiet = TRUE)
idx_html <- read_html(idx_file)
all_links <- idx_html %>% html_elements("a") %>% html_attr("href") %>%
  unique() %>% .[!is.na(.)]
result_links <- all_links[grepl("ergebnis-landratswahl[^/]*-|ergebnis-stichwahl-landrat-",
                                 all_links)]

cat(sprintf("Found %d Kreis result-page links in master index\n", length(result_links)))

# Step 2: download each page
download_if_missing <- function(url, out, label) {
  if (file.exists(out) && file.info(out)$size > 5000) return(invisible(FALSE))
  res <- tryCatch(
    download.file(url, out, mode = "wb", quiet = TRUE),
    error = function(e) -1L,
    warning = function(w) -1L
  )
  if (file.exists(out) && file.info(out)$size > 5000) {
    cat(sprintf("  ✓ %s\n", label))
    invisible(TRUE)
  } else {
    cat(sprintf("  ✗ %s (%s)\n", label, url))
    if (file.exists(out)) file.remove(out)
    invisible(FALSE)
  }
}

n_dl <- 0
for (link in result_links) {
  url <- if (startsWith(link, "http")) link else paste0(base, link)
  # Extract slug for filename
  slug <- sub(".*/(ergebnis-(landratswahl|stichwahl-landrat)[^/]*)/?$", "\\1", link)
  fname <- paste0("BB_", slug, ".html")
  out <- file.path(raw_dir, fname)
  if (download_if_missing(url, out, slug)) n_dl <- n_dl + 1
  Sys.sleep(0.2)  # be nice to the server
}

cat(sprintf("\nDone. %d new files downloaded.\n", n_dl))
files <- list.files(raw_dir, pattern = "\\.html$")
cat(sprintf("Cached files (%d):\n", length(files)))
for (f in sort(files)) cat(" -", f, "\n")
