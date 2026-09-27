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
# Coverage: the portal shows the most recent cycle of each Landkreis only.
# Earlier cycles back to the first direct election (10.01.2010) were added by
# hand from Wayback Machine captures of these URLs and of the Landeswahlleiter's
# earlier site; raw/brandenburg/README.md lists the source of every file.
# Before 2010 the Kreistag elected every Brandenburg Landrat.

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

# Step 1: fetch master index, extract Kreis-page URLs. Read from a temp file:
# raw/ holds source pages only and is never overwritten, and the committed
# _master_index.html stays as the snapshot of the first scrape.
idx_file <- tempfile(fileext = ".html")
download.file(index_url, idx_file, mode = "wb", quiet = TRUE)
idx_html <- read_html(idx_file)
all_links <- idx_html %>% html_elements("a") %>% html_attr("href") %>%
  unique() %>% .[!is.na(.)]
result_links <- all_links[grepl("ergebnis-landratswahl[^/]*-|ergebnis-stichwahl-landrat-",
                                 all_links)]

cat(sprintf("Found %d Kreis result-page links in master index\n", length(result_links)))

# Step 2: download each page, keeping every version.
# The portal shows only the MOST RECENT election per Kreis, under a URL that does
# not change between cycles. A cached page is therefore never overwritten (it may
# be the only copy of an older cycle). The live page is compared with the cached
# versions of its slug on (a) the election date in its heading and (b) the text of
# its result table:
#   new date                  -> the Kreis voted again: BB_<slug>_<date>.html
#   same date, same table     -> already cached, skip
#   same date, changed table  -> re-published (e.g. vorläufig -> endgültig):
#                                BB_<slug>_<date>_r<retrieval date>.html
# 01_landrat_combine.R keeps the newest version of each election. (Before
# September 2026 the scraper skipped any slug already on disk, so
# Ostprignitz-Ruppin stayed on its 2018 pages and the June 2026 election never
# arrived.)
de_months <- c("Januar" = 1, "Februar" = 2, "März" = 3, "April" = 4, "Mai" = 5,
               "Juni" = 6, "Juli" = 7, "August" = 8, "September" = 9,
               "Oktober" = 10, "November" = 11, "Dezember" = 12)
# The date in the page heading ("Endgültiges Ergebnis der Landratswahl am 7. Juni
# 2026", "... der Stichwahl zum Landrat am25. Januar 2026", "... des Landratesam
# 20. Februar 2022", "Endgültige Ergebnisse der Landratswahl im Landkreis
# Uckermark am 28. Februar 2010"), not the first date anywhere on the page. Same
# rule as parse_bb() in 01_landrat_combine.R.
heading_date <- function(file) {
  m <- str_match(html_text(read_html(file)),
                 paste0("Ergebnis(?:se)? der\\s*(?:Landratswahl|Stichwahl)[^0-9]{0,40}?am\\s*",
                        "(\\d{1,2})\\.\\s*(", paste(names(de_months), collapse = "|"),
                        ")\\s*(\\d{4})"))
  if (is.na(m[1, 1])) return(NA_character_)
  sprintf("%s-%02d-%02d", m[1, 4], de_months[[m[1, 3]]], as.integer(m[1, 2]))
}
result_table <- function(file) {
  tbl <- html_element(read_html(file), "table.bb-table-stripes")
  if (is.na(tbl)) NA_character_ else str_squish(html_text(tbl))
}

fetch_page <- function(url, slug) {
  tmp <- tempfile(fileext = ".html")
  ok <- tryCatch({
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(tmp) || file.info(tmp)$size <= 5000) {
    cat(sprintf("  ✗ %s (%s)\n", slug, url))
    return(invisible(FALSE))
  }
  cached <- list.files(raw_dir, full.names = TRUE,
                       pattern = paste0("^BB_", slug,
                                        "(_\\d{4}-\\d{2}-\\d{2}(_r\\d{4}-\\d{2}-\\d{2})?)?\\.html$"))
  if (length(cached) == 0) {
    file.copy(tmp, file.path(raw_dir, paste0("BB_", slug, ".html")))
    cat(sprintf("  ✓ %s (new)\n", slug))
    return(invisible(TRUE))
  }
  new_date <- heading_date(tmp)
  new_table <- result_table(tmp)
  if (is.na(new_date) || is.na(new_table)) {
    warning(sprintf("%s: live page has no dated heading or no result table; cache left untouched",
                    slug), call. = FALSE)
    return(invisible(FALSE))
  }
  cached_dates <- vapply(cached, heading_date, character(1))
  same <- cached[cached_dates %in% new_date]
  if (length(same) == 0) {
    file.copy(tmp, file.path(raw_dir, paste0("BB_", slug, "_", new_date, ".html")))
    cat(sprintf("  ✓ %s (new cycle %s, older version kept)\n", slug, new_date))
    return(invisible(TRUE))
  }
  if (new_table %in% vapply(same, result_table, character(1))) return(invisible(FALSE))
  out <- file.path(raw_dir, sprintf("BB_%s_%s_r%s.html", slug, new_date, Sys.Date()))
  if (file.exists(out)) return(invisible(FALSE))
  file.copy(tmp, out)
  cat(sprintf("  ✓ %s (re-published %s, older version kept)\n", slug, new_date))
  invisible(TRUE)
}

n_dl <- 0
for (link in result_links) {
  url <- if (startsWith(link, "http")) link else paste0(base, link)
  # Extract slug for filename
  slug <- sub(".*/(ergebnis-(landratswahl|stichwahl-landrat)[^/]*)/?$", "\\1", link)
  if (fetch_page(url, slug)) n_dl <- n_dl + 1
  Sys.sleep(0.2)  # be nice to the server
}

cat(sprintf("\nDone. %d new files saved.\n", n_dl))
files <- list.files(raw_dir, pattern = "\\.html$")
cat(sprintf("Cached files (%d):\n", length(files)))
for (f in sort(files)) cat(" -", f, "\n")
