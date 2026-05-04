## Apply QA corrections: flip have_<year> from TRUE to FALSE for rows where
## review of agent findings showed the URL doesn't actually deliver usable WBZ
## polygons. Each correction is justified by either:
##   (a) wrong granularity (file exists but is Stadtteile/Wahlkreis aggregates,
##       not WBZ polygons),
##   (b) tabular/PDF/viewer only (no shapefile or geojson),
##   (c) URL is a placeholder ("NA (request needed)" etc.) — agent put text
##       not a real URL into the direct_url cell,
##   (d) file exists but vintage doesn't match the year (e.g. 2020 archive
##       relabeled as 2025).
##
## After flipping, also clear online_source for cities that now have all four
## have_<year>=FALSE (the source no longer points to anything we have).

set.seed(20260504)

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

CONTACTS_PATH <- "data/wbz/contacts.csv"

## (ags, year) pairs to flip TRUE -> FALSE, with the reason for the audit log.
corrections <- tribble(
  ~ags,       ~year, ~reason,
  "04011000", 2025L, "ZIP archive interior is WBZ_2020 — vintage doesn't match BTW 2025",
  "04012000", 2021L, "agent notes file 'formerly hosted', no longer accessible",
  "05314000", 2017L, "agent: BTW 2017 boundaries differ from current; URL is NA",
  "05316000", 2025L, "marked 'likely (non-public)' with NA URL — internal only",
  "05911000", 2021L, "no direct download confirmed; URL field literally 'NA (no direct download...)'",
  "05914000", 2025L, "only a council-resolution PDF published, not a shapefile",
  "06412000", 2025L, "BTW 2025 is in the WAHL.ATLAS dashboard viewer only, no standalone download",
  "06414000", 2021L, "URL 'NA (request)' — agent confirms file held internally only",
  "06414000", 2025L, "same — internal only",
  "07211000", 2025L, "marked 'likely (internal only)' with URL=NA",
  "07313000", 2021L, "WMS viewer only, GetCapabilities 403-blocked, no download",
  "07313000", 2025L, "same as 2021 row — viewer-only",
  "07315000", 2021L, "URL=NA, internal only",
  "07315000", 2025L, "same — internal only",
  "07319000", 2021L, "URL=NA, internal only",
  "07319000", 2025L, "same — internal only",
  "08212000", 2013L, "geojson is 47-polygon coarse aggregate, not the 179 actual WBZ (FragDenStaat reply confirms shapefile not on portal)",
  "08212000", 2017L, "same — published geojson is wrong-granularity aggregate, real WBZ shapefile internal-only",
  "08222000", 2021L, "URL 'NA (request)' — held by Statistikstelle, not publicly available",
  "08222000", 2025L, "same — internal only",
  "09763000", 2025L, "tabular street→Stimmbezirk CSV only, no polygon geometry",
  "14511000", 2017L, "URL=NA — only the 2025 snapshot is public, no historical layer",
  "15001000", 2025L, "marked 'likely (internal)' with URL=NA",
  "15002000", 2021L, "URL=NA — boundaries internal to Stadtvermessung",
  "15002000", 2025L, "same — internal only",
  "15003000", 2025L, "only a PDF map published, not vector data",
  "16051000", 2025L, "URL 'NA (request)' — held internally by Geoinformation",
  "16053000", 2025L, "URL 'NA (request needed)' — internal only",
  "16055000", 2025L, "URL 'NA (request needed)' — internal Bodenmanagement GIS"
)

cat("Applying", nrow(corrections), "corrections...\n\n")

contacts <- read_csv(
  CONTACTS_PATH, show_col_types = FALSE, na = "",
  col_types = cols(
    .default = col_character(),
    population = col_integer(),
    have_2013 = col_logical(),
    have_2017 = col_logical(),
    have_2021 = col_logical(),
    have_2025 = col_logical()
  )
)

before_counts <- list(
  total_true = sum(contacts$have_2013 | contacts$have_2017 | contacts$have_2021 | contacts$have_2025, na.rm = TRUE),
  by_year = c(
    have_2013 = sum(contacts$have_2013, na.rm = TRUE),
    have_2017 = sum(contacts$have_2017, na.rm = TRUE),
    have_2021 = sum(contacts$have_2021, na.rm = TRUE),
    have_2025 = sum(contacts$have_2025, na.rm = TRUE)
  )
)

## Apply corrections
for (i in seq_len(nrow(corrections))) {
  ags <- corrections$ags[i]
  year <- corrections$year[i]
  flag_col <- paste0("have_", year)
  match_idx <- which(contacts$ags == ags)
  if (length(match_idx) != 1) {
    warning("AGS '", ags, "' not found (or duplicate)")
    next
  }
  prev <- contacts[[flag_col]][match_idx]
  contacts[[flag_col]][match_idx] <- FALSE
  cat(sprintf("  %s %s: %s -> FALSE\n", ags, year, prev))
}

## For any city where ALL year-flags are FALSE, clear online_source/format/license
## (those fields previously pointed to the now-rejected URL).
all_false <- !contacts$have_2013 & !contacts$have_2017 & !contacts$have_2021 & !contacts$have_2025
clearable <- which(all_false & !is.na(contacts$online_source))
cat(sprintf("\nClearing online_source for %d cities now at 0/4 (was pointing to rejected URL).\n",
            length(clearable)))
contacts$online_source[clearable] <- NA_character_
contacts$online_format[clearable] <- NA_character_
contacts$online_license[clearable] <- NA_character_

write_csv(contacts, CONTACTS_PATH, na = "")

after_counts <- list(
  total_true = sum(contacts$have_2013 | contacts$have_2017 | contacts$have_2021 | contacts$have_2025, na.rm = TRUE),
  by_year = c(
    have_2013 = sum(contacts$have_2013, na.rm = TRUE),
    have_2017 = sum(contacts$have_2017, na.rm = TRUE),
    have_2021 = sum(contacts$have_2021, na.rm = TRUE),
    have_2025 = sum(contacts$have_2025, na.rm = TRUE)
  )
)

cat("\n=== Before ===\n")
cat("Cities with at least one online year:", before_counts$total_true, "/ 106\n")
print(before_counts$by_year)
cat("\n=== After ===\n")
cat("Cities with at least one online year:", after_counts$total_true, "/ 106\n")
print(after_counts$by_year)
