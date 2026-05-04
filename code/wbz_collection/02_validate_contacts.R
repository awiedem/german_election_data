## Sanity checks for the WBZ contacts DB. Run periodically and at handoff.
##
## Phases the checks cover:
##   A (skeleton): row count, AGS uniqueness, expected iconic cities present.
##   C (triage):   for every have_<year>==TRUE, the shapefile directory exists
##                  and at least one geometry file inside is openable via sf.
##   D (contacts): every row has at least one populated email column.
##   E (handoff):  print a summary by state and overall coverage.

set.seed(20260504)

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(sf)
})

CONTACTS_PATH <- "data/wbz/contacts.csv"
SHP_DIR <- "data/wbz/shapefiles"
TARGET_YEARS <- c(2013, 2017, 2021, 2025)

contacts <- read_csv(CONTACTS_PATH, show_col_types = FALSE, na = "")

issues <- character()
record <- function(level, msg) {
  cat(sprintf("[%s] %s\n", level, msg))
  if (level == "FAIL") issues <<- c(issues, msg)
}

cat("=== WBZ contacts.csv validation ===\n\n")

## ---- Phase A checks ----
cat("-- Phase A: skeleton --\n")
n_rows <- nrow(contacts)
record(if (n_rows >= 100 && n_rows <= 110) "OK" else "FAIL",
       sprintf("row count = %d (expected ~106-107 kreisfreie Städte)", n_rows))

n_unique_ags <- n_distinct(contacts$ags)
record(if (n_unique_ags == n_rows) "OK" else "FAIL",
       sprintf("AGS uniqueness: %d unique / %d rows", n_unique_ags, n_rows))

bad_ags <- contacts |> filter(nchar(ags) != 8)
record(if (nrow(bad_ags) == 0) "OK" else "FAIL",
       sprintf("AGS format (8 digits): %d malformed", nrow(bad_ags)))

iconic <- c("11000000" = "Berlin", "02000000" = "Hamburg",
            "09162000" = "München", "05315000" = "Köln",
            "06412000" = "Frankfurt am Main", "08111000" = "Stuttgart")
missing_iconic <- setdiff(names(iconic), contacts$ags)
record(if (length(missing_iconic) == 0) "OK" else "FAIL",
       sprintf("iconic cities present: %d / %d", length(iconic) - length(missing_iconic), length(iconic)))

## ---- Phase D checks (contacts) ----
cat("\n-- Phase D: contact emails --\n")
no_email <- contacts |>
  filter(is.na(geo_email) & is.na(wahl_email))
n_have_email <- n_rows - nrow(no_email)
record(if (nrow(no_email) == 0) "OK" else "WARN",
       sprintf("rows with at least one of geo_email/wahl_email: %d / %d",
               n_have_email, n_rows))
if (nrow(no_email) > 0 && nrow(no_email) <= 10) {
  cat("  Missing email rows:\n")
  print(no_email |> select(ags, state, muni_name_short))
}

n_opendata <- sum(!is.na(contacts$opendata_email))
cat(sprintf("  Cities with an open-data team contact: %d\n", n_opendata))

## ---- Phase C checks (shapefiles) ----
cat("\n-- Phase C: downloaded shapefiles --\n")
have_cols <- paste0("have_", TARGET_YEARS)
coverage <- sapply(have_cols, function(c) sum(contacts[[c]], na.rm = TRUE))
for (i in seq_along(TARGET_YEARS)) {
  cat(sprintf("  BTW %d: %d cities flagged have=TRUE\n", TARGET_YEARS[i], coverage[i]))
}

## have_<year>=TRUE means "publicly available online; URL recorded."
## Downloading the actual files is a separate phase; here we differentiate:
##   not-downloaded (dir missing)  -> INFO (expected before Phase F)
##   downloaded but unreadable     -> FAIL (real corruption)
not_downloaded <- list()
broken <- list()
for (year in TARGET_YEARS) {
  flag_col <- paste0("have_", year)
  rows <- contacts |> filter(.data[[flag_col]])
  for (i in seq_len(nrow(rows))) {
    ags <- rows$ags[i]
    dir <- file.path(SHP_DIR, ags, year)
    if (!dir.exists(dir)) {
      not_downloaded[[length(not_downloaded) + 1]] <- tibble(ags = ags, year = year)
      next
    }
    geo_files <- list.files(dir, pattern = "\\.(shp|gpkg|geojson|json|kml)$",
                            full.names = TRUE, ignore.case = TRUE)
    if (length(geo_files) == 0) {
      broken[[length(broken) + 1]] <- tibble(ags = ags, year = year, issue = "no geometry file")
      next
    }
    ok <- tryCatch({
      g <- sf::st_read(geo_files[1], quiet = TRUE)
      nrow(g) > 0 && !is.null(sf::st_crs(g))
    }, error = function(e) FALSE)
    if (!ok) {
      broken[[length(broken) + 1]] <- tibble(ags = ags, year = year,
                                             issue = paste0("unreadable: ", basename(geo_files[1])))
    }
  }
}
not_downloaded_df <- bind_rows(not_downloaded)
broken_df <- bind_rows(broken)
record("INFO", sprintf("not yet downloaded: %d / %d flagged (run Phase F download)",
                      nrow(not_downloaded_df), sum(coverage)))
record(if (nrow(broken_df) == 0) "OK" else "FAIL",
       sprintf("downloaded shapefile integrity: %d broken / %d on disk",
               nrow(broken_df),
               sum(coverage) - nrow(not_downloaded_df)))
if (nrow(broken_df) > 0) {
  cat("  Broken entries:\n")
  print(broken_df)
}

## Cross-check: online_source populated where any have_year==TRUE.
any_have <- contacts |>
  rowwise() |>
  mutate(any_year = any(c_across(all_of(have_cols)), na.rm = TRUE)) |>
  ungroup()
inconsistent <- any_have |> filter(any_year & is.na(online_source))
record(if (nrow(inconsistent) == 0) "OK" else "WARN",
       sprintf("online_source populated where have_<year>=TRUE: %d inconsistent",
               nrow(inconsistent)))

## ---- Phase E summary ----
cat("\n-- Phase E: handoff summary --\n")
by_state <- contacts |>
  group_by(state) |>
  summarise(
    n_cities = n(),
    n_with_email = sum(!is.na(geo_email) | !is.na(wahl_email)),
    have_2013 = sum(have_2013, na.rm = TRUE),
    have_2017 = sum(have_2017, na.rm = TRUE),
    have_2021 = sum(have_2021, na.rm = TRUE),
    have_2025 = sum(have_2025, na.rm = TRUE),
    .groups = "drop"
  )
print(by_state, n = Inf)

cat(sprintf(
  "\nOverall: %d cities; %d with at least one online year; %d need full email outreach.\n",
  n_rows,
  sum(any_have$any_year),
  sum(!any_have$any_year)
))

if (length(issues) > 0) {
  cat("\n=== FAILURES ===\n")
  for (i in issues) cat("  -", i, "\n")
  quit(status = 1)
}
cat("\n=== All checks passed ===\n")
