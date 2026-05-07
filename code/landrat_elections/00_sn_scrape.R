### Scrape Landratswahl data for Sachsen
# Vincent Heddesheimer, May 2026
#
# Sources (mixed formats):
#   2002: https://www.wahlen.sachsen.de/wahlen/LR_2002/lr02.xls
#         (Single XLS — 3 Landkreise: Kamenz, Meißen, Vogtlandkreis)
#   2008: https://www.wahlen.sachsen.de/wahlen/LR_2008/pkg_w04_bmlr*.html?...
#         (HTML frameset with per-Kreis result pages — 10 LK + Stichwahlen)
#   2020: /download/Landrat/LR20_EE_LK_Meissen.xlsx
#   2022: /download/Landrat/statistik-sachsen_landratswahl2022_ergebnisse_endgueltig.xlsx
#         (One file with 9 LK — bigger 2022 cycle)
#   2025: /download/Landrat/statistik-sachsen_LR25_EE_LK_Mittelsachsen.xlsx
#         (Single LK so far — Mittelsachsen)
#
# The 2008 page has a "Übersicht" with all Kreise and links to result pages with
# parameters p_bz_bzid (LR081 = Hauptwahl, LR082 = Stichwahl), p_ebene=LK, p_ort=
# the 5-digit Landkreis code (14521..14730 for SN's LK at the time).
#
# Years not yet handled (HTML-only or JS-rendered): 2001, 2015. These
# require additional scraping work; documented in
# docs/landrat_data_sources.md.
#
# Output: cached raw files in data/landrat_elections/raw/sachsen/

rm(list = ls())
gc()

pacman::p_load(tidyverse, here, conflicted, rvest, xml2)
conflict_prefer("filter", "dplyr")
setwd(here::here())

raw_dir <- "data/landrat_elections/raw/sachsen"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== SN Landratswahl scraper ===\n\n")

# Helper: idempotent download
download_if_missing <- function(url, out, label = NULL) {
  if (file.exists(out) && file.info(out)$size > 100) {
    return(invisible(FALSE))
  }
  res <- tryCatch(
    download.file(url, out, mode = "wb", quiet = TRUE),
    error = function(e) -1L,
    warning = function(w) -1L
  )
  if (file.exists(out) && file.info(out)$size > 100) {
    cat(sprintf("  ✓ %s (%d bytes)\n",
                label %||% basename(out), file.info(out)$size))
    return(invisible(TRUE))
  } else {
    cat(sprintf("  ✗ failed: %s\n", url))
    if (file.exists(out)) file.remove(out)
    return(invisible(FALSE))
  }
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# 2002: Single XLS download
# ============================================================================
cat("--- 2002 ---\n")
download_if_missing(
  "https://www.wahlen.sachsen.de/wahlen/LR_2002/lr02.xls",
  file.path(raw_dir, "LR_2002_gewaehlte_landraete.xls"),
  "2002 Gewählte Landräte"
)

# ============================================================================
# 2008 + 2015: scrape Übersicht + per-Kreis HTML pages
# ============================================================================
# Both years use the same wahlarchiv layout: Übersicht page links to per-Kreis
# result pages. The bzid is `LR{YY}1` for Hauptwahl, `LR{YY}2` for Stichwahl.
# The 2015 archive is reached via the HTTrack-mirrored entry point
# pkg_w04_bmlr97bc.html?p_bz_bzid=LR15 (the index.html redirects to it).

scrape_year <- function(year, ueb_url) {
  cat(sprintf("\n--- %d ---\n", year))
  ueb_file <- file.path(raw_dir, sprintf("LR_%d_uebersicht.html", year))
  download_if_missing(ueb_url, ueb_file, sprintf("%d Übersicht", year))
  ueb_html <- read_html(ueb_file)
  links <- ueb_html %>% html_elements("a") %>% html_attr("href")
  yy <- sprintf("%02d", year %% 100)
  pattern <- sprintf("p_bz_bzid=LR%s[12]&", yy)
  result_links <- links[grepl(pattern, links)]
  cat(sprintf("  Found %d Landkreis result pages\n", length(result_links)))
  for (link in result_links) {
    ort  <- sub(".*p_ort=([0-9]+).*", "\\1", link)
    bzid <- sub(sprintf(".*p_bz_bzid=(LR%s[12]).*", yy), "\\1", link)
    fname <- sprintf("LR_%d_%s_%s.html", year, bzid, ort)
    out <- file.path(raw_dir, fname)
    url <- file.path(sprintf("https://www.wahlen.sachsen.de/wahlen/LR_%d", year), link)
    download_if_missing(url, out, fname)
  }
}

scrape_year(2008, "https://www.wahlen.sachsen.de/wahlen/LR_2008/pkg_w04_bmlre2e1.html?p_bz_bzid=LR08")
scrape_year(2015, "https://www.wahlen.sachsen.de/wahlen/LR_2015/pkg_w04_bmlr97bc.html?p_bz_bzid=LR15")

# ============================================================================
# 2020 / 2022 / 2025: Direct Excel downloads
# ============================================================================
cat("\n--- 2020 / 2022 / 2025 ---\n")

excel_targets <- list(
  list(url = "https://wahlen.sachsen.de/download/Landrat/LR20_EE_LK_Meissen.xlsx",
       out = "LR_2020_LK_Meissen.xlsx",
       label = "2020 Meißen"),
  list(url = "https://wahlen.sachsen.de/download/Landrat/statistik-sachsen_landratswahl2022_ergebnisse_endgueltig.xlsx",
       out = "LR_2022_ergebnisse_endgueltig.xlsx",
       label = "2022 alle Landkreise"),
  list(url = "https://wahlen.sachsen.de/download/Landrat/statistik-sachsen_landratswahl2022_gewaehlte-landraete.xlsx",
       out = "LR_2022_gewaehlte_landraete.xlsx",
       label = "2022 Gewählte Landräte"),
  list(url = "https://wahlen.sachsen.de/Landrat/statistik-sachsen_LR25_EE_LK_Mittelsachsen.xlsx",
       out = "LR_2025_LK_Mittelsachsen.xlsx",
       label = "2025 Mittelsachsen")
)

for (t in excel_targets) {
  download_if_missing(t$url, file.path(raw_dir, t$out), t$label)
}

# ============================================================================
# Summary
# ============================================================================
files <- list.files(raw_dir, recursive = TRUE)
cat(sprintf("\nDone. %d cached files:\n", length(files)))
for (f in sort(files)) cat(" -", f, "\n")
