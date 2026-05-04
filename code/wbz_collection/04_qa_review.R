## QA review: flag have_<year>=TRUE rows where agent's per-year notes
## suggest the URL doesn't actually deliver usable WBZ polygons.
##
## Output: data/wbz/qa_flags.csv with one row per (ags, year, reason) — for
## manual review and correction.

set.seed(20260504)

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
})

FINDINGS_DIR <- "data/wbz/findings"
TARGET_YEARS <- c(2013, 2017, 2021, 2025)

is_na_token <- function(x) {
  if (length(x) == 0) return(logical(0))
  trimmed <- str_trim(x)
  out <- is.na(x)
  out <- out | (!is.na(trimmed) & trimmed == "")
  lc <- ifelse(is.na(trimmed), "", tolower(trimmed))
  out <- out | str_detect(lc, "^(na|n/a|none|—|-|unknown|unclear)$")
  out
}
clean_cell <- function(x) {
  if (length(x) == 0) return(x)
  x <- str_trim(x)
  ifelse(is_na_token(x), NA_character_, x)
}

parse_md_table <- function(lines, expected_cols) {
  lc <- tolower(lines)
  header_idx <- NA
  for (i in seq_along(lines)) {
    if (str_detect(lc[i], "^\\s*\\|") && all(str_detect(lc[i], expected_cols))) {
      header_idx <- i; break
    }
  }
  if (is.na(header_idx)) return(NULL)
  body <- character(); j <- header_idx + 2
  while (j <= length(lines) && str_detect(lines[j], "^\\s*\\|")) {
    body <- c(body, lines[j]); j <- j + 1
  }
  if (length(body) == 0) return(NULL)
  split_row <- function(line) {
    s <- str_trim(line); s <- str_remove(s, "^\\|"); s <- str_remove(s, "\\|$")
    map_chr(str_split(s, "\\|")[[1]], str_trim)
  }
  header_cells <- tolower(split_row(lines[header_idx]))
  rows <- map(body, split_row)
  ncol_h <- length(header_cells)
  rows <- map(rows, function(r) {
    if (length(r) < ncol_h) c(r, rep("", ncol_h - length(r)))
    else if (length(r) > ncol_h) r[seq_len(ncol_h)]
    else r
  })
  m <- do.call(rbind, rows) |> as.data.frame(stringsAsFactors = FALSE)
  names(m) <- header_cells
  as_tibble(m)
}

## Warning patterns that indicate URL doesn't deliver usable WBZ polygons.
WARNING_PATTERNS <- list(
  "wrong-granularity-wahlkreis" = "only.{0,30}wahlkreis|wahlkreise.{0,30}only|nur.{0,30}wahlkreise|wahlkreis.{0,15}\\(",
  "wrong-granularity-stadtteil" = "only.{0,30}stadtteil|stadtteile.{0,30}only|stadtteil.{0,30}aggregate|coarse aggregate|aggregat",
  "tabular-only"                = "tabular only|csv only|tables? only|nur tabellarisch|no geometry|geometr.{0,5}null|no polygons?|geometry not (yet )?exposed",
  "pdf-or-raster"               = "\\bpdf\\b|raster|image-only|static map|pdf map|pdf only|viewer-only|viewer only|dashboard only|frontend only",
  "viewer-no-download"          = "viewer.{0,30}no.{0,30}download|render(s|ed) only|map.{0,15}only|no.{0,5}(direct.{0,5})?download|not.{0,5}exposed",
  "needs-verification"          = "could not (be )?(verif|confirm)|not yet (verified|confirmed)|unverified|sample.{0,15}truncat|content.{0,15}uncertain|vintage.{0,15}unclear|may be.{0,30}overwritten",
  "low-feature-count"           = "(\\b|^)(?:[1-9]|[1-3][0-9])\\s*(?:feature|polygon)s? .{0,30}(?:expected|aggregate|stadtbezirke|coarse)",
  "wrong-dataset-type"          = "kommunalwahl(?!.{0,30}same|.{0,30}match|.{0,30}identisch)|gemeinderatswahl(?!.{0,30}same|.{0,30}match)"
)

## Permissive parsing of one findings file. Returns rows that the merge script
## would have classified as TRUE, with the per-row notes for QA.
parse_for_qa <- function(path) {
  ags <- str_extract(basename(path), "^[0-9]{8}")
  if (is.na(ags)) return(NULL)
  lines <- read_lines(path, progress = FALSE)
  tbl <- parse_md_table(lines, c("year", "found", "direct_url", "format", "license"))
  if (is.null(tbl)) return(NULL)
  tbl <- tbl |> mutate(across(everything(), clean_cell)) |>
    mutate(year = suppressWarnings(as.integer(year)))
  if (!"notes" %in% names(tbl)) tbl$notes <- NA_character_
  ## Replicate merge script's TRUE rule
  tbl <- tbl |>
    mutate(
      notes_safe = ifelse(is.na(notes), "", tolower(notes)),
      flagged_true = !is.na(found) &
        str_detect(tolower(found), "^(yes|likely|partial)\\b") &
        !str_detect(tolower(found), "point only|null") &
        !is.na(direct_url) &
        !str_detect(notes_safe,
                    "only.{0,20}wahlkreis|wahlkreise.{0,20}only|geometry.{0,5}null|point only")
    ) |>
    filter(flagged_true, year %in% TARGET_YEARS)
  if (nrow(tbl) == 0) return(NULL)
  ## Look for warning patterns in the notes column
  tbl |>
    transmute(
      ags = ags, year, found, direct_url,
      notes_lower = ifelse(is.na(notes), "", tolower(notes)),
      notes_raw = notes
    ) |>
    rowwise() |>
    mutate(
      warnings = list(names(WARNING_PATTERNS)[map_lgl(WARNING_PATTERNS, ~ str_detect(notes_lower, .x))])
    ) |>
    ungroup() |>
    filter(lengths(warnings) > 0) |>
    mutate(warnings = map_chr(warnings, ~ paste(.x, collapse = ";"))) |>
    select(ags, year, found, warnings, direct_url, notes = notes_raw)
}

files <- list.files(FINDINGS_DIR, pattern = "^[0-9]{8}\\.md$", full.names = TRUE)
flagged <- map_dfr(files, parse_for_qa)

cat("Cities scanned:", length(files), "\n")
cat("Flagged TRUE-rows for review:", nrow(flagged), "\n\n")

contacts <- read_csv("data/wbz/contacts.csv", show_col_types = FALSE, na = "",
                     col_types = cols(.default = col_character(),
                                      population = col_integer(),
                                      have_2013 = col_logical(), have_2017 = col_logical(),
                                      have_2021 = col_logical(), have_2025 = col_logical()))

flagged <- flagged |>
  left_join(contacts |> select(ags, muni_name_short, state), by = "ags") |>
  select(ags, state, muni_name_short, year, found, warnings, direct_url, notes) |>
  arrange(state, ags, year)

write_csv(flagged, "data/wbz/qa_flags.csv", na = "")
cat("Wrote data/wbz/qa_flags.csv (", nrow(flagged), "rows)\n\n")

## Print a digest by warning category
cat("Warning category counts:\n")
flagged |>
  separate_rows(warnings, sep = ";") |>
  count(warnings, sort = TRUE) |>
  print()
