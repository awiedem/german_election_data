### Scrape Landratswahl data for Sachsen-Anhalt
# Vincent Heddesheimer, May 2026
#
# Source: https://wahlergebnisse.sachsen-anhalt.de/wahlen/lr{YY}/erg/csv/lr{YY}dat{N}.csv
#
# The Statistisches Landesamt Sachsen-Anhalt publishes Landratswahl results as
# CSV files with one row per (geographic level, ags). The CSV schema is:
#   ERGART;DATUM;LEER;LEER;LEER;NR;NAME;A;B;C;D;L01;N01;D01;...;L11;N11;D11
# where:
#   ERGART = "E" (Endergebnis)
#   DATUM  = election date (DD.MM.YYYY)
#   NR     = Schlüsselnummer (8 digits for muni, 5 digits for Kreis)
#   NAME   = Kreis or municipality name
#   A,B,C,D = Wahlberechtigte, Wähler, Ungültige, Gültige
#   L01..L11 = lfd. Nr. of candidate (01..11)
#   N01..N11 = candidate name (with party in parentheses, e.g. "Müller, Karl (CDU)")
#   D01..D11 = candidate vote count
#
# File naming convention (when present):
#   lr{YY}dat1.csv = Stichwahl, Kreisfreie Städte und Landkreise
#   lr{YY}dat2.csv = Stichwahl, Gemeinden
#   lr{YY}dat3.csv = Hauptwahl, Kreisfreie Städte und Landkreise
#   lr{YY}dat4.csv = Hauptwahl, Gemeinden
#
# We only need files 1 and 3 (Kreis-level — `Satzart == "KRS"`).
#
# Encoding: all CSVs are ISO-8859-1 (Latin-1).

rm(list = ls())
gc()

pacman::p_load(tidyverse, here, conflicted)
conflict_prefer("filter", "dplyr")
setwd(here::here())

raw_dir <- "data/landrat_elections/raw/sachsen_anhalt"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# Discovery: probe known and likely year codes.
# Direct Landrat election introduced in ST in 2007.
candidate_years <- c("07", "08", "09", "10", "11", "12", "13", "14", "15",
                     "16", "17", "18", "19", "20", "21", "22", "23", "24",
                     "25", "26")

base_url <- "https://wahlergebnisse.sachsen-anhalt.de/wahlen"

cat("=== ST Landratswahl scraper ===\n\n")

n_downloaded <- 0
n_cached <- 0

for (yy in candidate_years) {
  for (datN in c(1L, 3L)) {  # 1 = SW Kreise, 3 = HW Kreise
    fname <- sprintf("lr%sdat%d.csv", yy, datN)
    out <- file.path(raw_dir, fname)

    if (file.exists(out) && file.info(out)$size > 100) {
      n_cached <- n_cached + 1
      next
    }

    url <- sprintf("%s/lr%s/erg/csv/%s", base_url, yy, fname)
    res <- tryCatch(
      download.file(url, out, mode = "wb", quiet = TRUE),
      error = function(e) -1L,
      warning = function(w) -1L
    )

    if (file.exists(out) && file.info(out)$size > 100) {
      # Read first line and convert from ISO-8859-1 → UTF-8 before regex check
      first_line <- tryCatch(
        iconv(readLines(out, n = 1, warn = FALSE),
              from = "ISO-8859-1", to = "UTF-8"),
        error = function(e) ""
      )
      if (length(first_line) > 0 &&
          grepl("DATUM|ERGART|Datum|Ergebnisart", first_line)) {
        cat(sprintf("  ✓ downloaded %s (%d bytes)\n", fname, file.info(out)$size))
        n_downloaded <- n_downloaded + 1
      } else {
        # Looks like an HTML 404 — clean up
        file.remove(out)
      }
    }
  }
}

cat(sprintf("\nDone. %d new files downloaded, %d already cached.\n",
            n_downloaded, n_cached))

# ============================================================================
# ST 2015 — special case, only 1 Landratswahl (Altmarkkreis Salzwedel)
# ============================================================================
# The 2015 Kommunalwahl page does not have CSV downloads. Each Kreis has a
# per-Kreis HTML at .../wahlen/lr15/erg/kreis/lr.{schluessel}.ergtab.frametab.html.
# 2015 had only 1 Landratswahl (15081 = Altmarkkreis Salzwedel) plus 1 OB
# (15003 = Magdeburg, which we skip).
st_2015_lk <- c("15081" = "Altmarkkreis Salzwedel")
for (sch in names(st_2015_lk)) {
  url <- sprintf(
    "https://wahlergebnisse.sachsen-anhalt.de/wahlen/lr15/erg/kreis/lr.%s.ergtab.frametab.html",
    sch)
  out <- file.path(raw_dir, sprintf("ST_2015_%s.html", sch))
  if (file.exists(out) && file.info(out)$size > 100) next
  res <- tryCatch(
    download.file(url, out, mode = "wb", quiet = TRUE),
    error = function(e) -1L,
    warning = function(w) -1L
  )
  if (file.exists(out) && file.info(out)$size > 1000) {
    cat(sprintf("  ✓ ST 2015 %s (%s)\n", st_2015_lk[[sch]], sch))
  } else if (file.exists(out)) {
    file.remove(out)
  }
}

# List what we have
files <- list.files(raw_dir, full.names = FALSE)
cat(sprintf("\nCached files (%d):\n", length(files)))
for (f in sort(files)) cat(" -", f, "\n")
