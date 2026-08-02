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
# File naming convention: NOT stable across years, so do not trust dat{N} to
# mean the same thing everywhere. lr14 follows dat1 = Stichwahl Kreise /
# dat3 = Hauptwahl Kreise, but lr07's own download page labels dat2 as
# "Endergebnisse der Kreisfreien Städte und Landkreise" and dat3 as
# "Endergebnisse von Gemeinden" -- both Hauptwahl. lr07dat1.csv is not a file
# that has gone missing: 2007 published no Stichwahl results at all, which is
# why six 2007 Landratswahlen sit in the data with a sub-50 % Hauptwahl winner
# and no runoff row.
#
# The scheme only ever existed for lr07 and lr14. Everything from 2019 on is in
# the rolling file fetched further down.
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
probe_log <- list()

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
    # Record what actually happened. Until July 2026 this loop swallowed every
    # error and warning and then silently file.remove()d anything that was not
    # a CSV, so a 404, a moved URL and "no election that year" were completely
    # indistinguishable -- which is why nobody noticed that the per-year scheme
    # covers only lr07 and lr14 and that the ST Landrat series had stopped at
    # 2015 while eleven Kreise voted between 2019 and 2026.
    probe_log[[length(probe_log) + 1]] <- data.frame(
      file = fname,
      outcome = if (identical(res, -1L)) "download failed"
                else if (!file.exists(out)) "no file written"
                else if (file.info(out)$size <= 100) "empty/404 stub"
                else "ok",
      bytes = if (file.exists(out)) file.info(out)$size else NA_integer_,
      stringsAsFactors = FALSE
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

# Report the probe rather than hiding it, and state plainly which years the
# per-year scheme actually covers -- everything else 404s.
if (length(probe_log) > 0) {
  pl <- do.call(rbind, probe_log)
  cat("\nProbe outcomes for the per-year lr{YY}dat{N}.csv scheme:\n")
  print(table(pl$outcome))
  ok <- pl$file[pl$outcome == "ok"]
  if (length(ok) > 0) cat("  reachable:", paste(ok, collapse = ", "), "\n")
}

# ============================================================================
# Rolling current-cycle file (2019 onward)
# ============================================================================
# The per-year scheme was only ever used for lr07 and lr14. Every Landrat and
# OB election from 2019 on lives in ONE rolling file in a different, wide
# schema, linked from /wahlen/lrlr/and/lr.download.html. Without it the ST
# Landrat series ends in 2015. It is re-fetched every run because it grows as
# new elections are held; the Kreis-level rows it adds are read by
# 01_landrat_combine.R.
rolling_url <- paste0(base_url, "/lrlr/erg/csv/lr.csv")
rolling_out <- file.path(raw_dir, "lr_rolling.csv")
res <- tryCatch(download.file(rolling_url, rolling_out, mode = "wb", quiet = TRUE),
                error = function(e) -1L, warning = function(w) -1L)
if (!file.exists(rolling_out) || file.info(rolling_out)$size < 500) {
  stop("ST: could not fetch the rolling Landrat file ", rolling_url,
       " -- without it the series stops at 2015. Check ",
       base_url, "/lrlr/and/lr.download.html for a renamed path.")
}
hdr <- iconv(readLines(rolling_out, n = 1, warn = FALSE),
             from = "ISO-8859-1", to = "UTF-8")
if (!grepl("GNR1994", hdr) || !grepl("B1_STI_SW", hdr)) {
  stop("ST: ", rolling_out, " does not have the expected wide schema ",
       "(GNR1994 ... B1_STI_SW). The portal layout changed; update the parser ",
       "in 01_landrat_combine.R before trusting this file.")
}
cat(sprintf("  \u2713 rolling lr.csv (%d bytes, %d elections)\n",
            file.info(rolling_out)$size,
            length(readLines(rolling_out, warn = FALSE)) - 1L))

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
