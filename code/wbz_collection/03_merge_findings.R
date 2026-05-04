## Merge per-city findings markdown into the master contacts.csv.
##
## Reads every data/wbz/findings/<ags>.md and writes the parsed values back
## into data/wbz/contacts.csv. Idempotent: rerunning will overwrite the
## have_<year>, online_*, *_email, *_url, *_dept_label, and notes columns
## with whatever is in the latest findings file.
##
## Findings file structure (see prompts to triage agents):
##   ## Shapefile findings    -> markdown table with columns
##                                year | found | direct_url | format | license | feature_count | notes
##   "Primary online source (if any): <URL>"
##   "Default license observed: <license string>"
##   ## Contact emails        -> markdown table with columns
##                                role | email | source_url | dept_label
##                                where role ∈ {Geodatenstelle, Wahlamt, OpenData}
##   ## Notes                 -> freeform bullet list / paragraphs

set.seed(20260504)

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

CONTACTS_PATH <- "data/wbz/contacts.csv"
FINDINGS_DIR <- "data/wbz/findings"
TARGET_YEARS <- c(2013, 2017, 2021, 2025)

is_na_token <- function(x) {
  trimmed <- str_trim(x)
  is.na(x) | trimmed == "" |
    str_detect(replace_na(tolower(trimmed), ""), "^(na|n/a|none|—|-|unknown|unclear)$")
}

clean_cell <- function(x) {
  x <- str_trim(x)
  if_else(is_na_token(x), NA_character_, x)
}

## Parse a markdown table starting at the first line containing all expected
## columns (case-insensitive). Returns a tibble or NULL if not found.
parse_md_table <- function(lines, expected_cols) {
  lc <- tolower(lines)
  ## Find a header line containing all expected column names
  header_idx <- NA
  for (i in seq_along(lines)) {
    if (str_detect(lc[i], "^\\s*\\|") && all(str_detect(lc[i], expected_cols))) {
      header_idx <- i
      break
    }
  }
  if (is.na(header_idx)) return(NULL)
  ## Header is at i, separator at i+1, body starts at i+2 and continues while pipe lines
  body <- character()
  j <- header_idx + 2
  while (j <= length(lines) && str_detect(lines[j], "^\\s*\\|")) {
    body <- c(body, lines[j])
    j <- j + 1
  }
  if (length(body) == 0) return(NULL)
  ## Parse cells
  split_row <- function(line) {
    s <- str_trim(line)
    s <- str_remove(s, "^\\|"); s <- str_remove(s, "\\|$")
    map_chr(str_split(s, "\\|")[[1]], str_trim)
  }
  header_cells <- tolower(split_row(lines[header_idx]))
  rows <- map(body, split_row)
  ## Pad short rows
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

## Parse one findings file -> 1-row tibble with all writable contacts.csv fields.
parse_findings_file <- function(path) {
  ags <- str_extract(basename(path), "^[0-9]{8}")
  if (is.na(ags)) return(NULL)
  lines <- read_lines(path, progress = FALSE)

  ## Shapefile table
  shp_tbl <- parse_md_table(
    lines,
    c("year", "found", "direct_url", "format", "license")
  )
  have_flags <- set_names(rep(FALSE, length(TARGET_YEARS)), paste0("have_", TARGET_YEARS))
  online_source <- NA_character_
  online_format <- NA_character_
  online_license <- NA_character_

  if (!is.null(shp_tbl)) {
    shp_tbl <- shp_tbl |>
      mutate(across(everything(), clean_cell)) |>
      mutate(
        year = suppressWarnings(as.integer(year)),
        ## TRUE when there is a candidate URL the agent has positive or partial
        ## confidence in. The per-row notes column captures caveats so PIK can
        ## decide per case whether to verify before relying on it.
        ## Accept: yes, likely, partial (with URL).
        ## Reject: no; per-row notes that signal the URL doesn't deliver the
        ## right thing (point-only / null geometry / wrong-granularity Wahlkreis-only).
        found_yes = !is.na(found) &
          str_detect(tolower(found), "^(yes|likely|partial)\\b") &
          !str_detect(tolower(found), "point only|null") &
          !is.na(direct_url) &
          !(str_detect(tolower(notes %||% ""), "only.{0,20}wahlkreis|wahlkreise.{0,20}only|geometry.{0,5}null|point only"))
      )
    for (y in TARGET_YEARS) {
      r <- shp_tbl |> filter(year == y)
      if (nrow(r) >= 1 && any(r$found_yes, na.rm = TRUE)) {
        have_flags[paste0("have_", y)] <- TRUE
      }
    }
    ## Pick first found row as the "primary" source
    found_rows <- shp_tbl |> filter(found_yes & !is.na(direct_url))
    if (nrow(found_rows) >= 1) {
      online_source <- found_rows$direct_url[1]
      online_format <- found_rows$format[1]
      online_license <- found_rows$license[1]
    }
  }

  ## Override with explicit "Primary online source" line if present
  prim_line <- str_subset(lines, regex("^\\s*Primary online source", ignore_case = TRUE))
  if (length(prim_line) >= 1) {
    cand <- str_trim(str_remove(prim_line[1],
                                regex("^\\s*Primary online source[^:]*:\\s*", ignore_case = TRUE)))
    cand <- clean_cell(str_remove_all(cand, "[<>]"))
    if (!is.na(cand) && str_detect(cand, "^https?://")) online_source <- cand
  }
  lic_line <- str_subset(lines, regex("^\\s*Default license", ignore_case = TRUE))
  if (length(lic_line) >= 1) {
    cand <- clean_cell(str_remove(lic_line[1],
                                  regex("^\\s*Default license[^:]*:\\s*", ignore_case = TRUE)))
    if (!is.na(cand)) online_license <- cand
  }

  ## Contact table
  contact_tbl <- parse_md_table(
    lines,
    c("role", "email", "source_url")
  )
  emails <- list(
    geo_email = NA_character_, geo_url = NA_character_, geo_dept_label = NA_character_,
    wahl_email = NA_character_, wahl_url = NA_character_,
    opendata_email = NA_character_, opendata_url = NA_character_
  )
  if (!is.null(contact_tbl)) {
    contact_tbl <- contact_tbl |> mutate(across(everything(), clean_cell))
    for (i in seq_len(nrow(contact_tbl))) {
      role <- tolower(contact_tbl$role[i])
      if (is.na(role)) next
      if (str_detect(role, "geo|vermess")) {
        emails$geo_email <- contact_tbl$email[i]
        emails$geo_url <- if ("source_url" %in% names(contact_tbl)) contact_tbl$source_url[i] else NA_character_
        if ("dept_label" %in% names(contact_tbl)) emails$geo_dept_label <- contact_tbl$dept_label[i]
      } else if (str_detect(role, "wahl|election")) {
        emails$wahl_email <- contact_tbl$email[i]
        emails$wahl_url <- if ("source_url" %in% names(contact_tbl)) contact_tbl$source_url[i] else NA_character_
      } else if (str_detect(role, "open\\s*data|opendata")) {
        emails$opendata_email <- contact_tbl$email[i]
        emails$opendata_url <- if ("source_url" %in% names(contact_tbl)) contact_tbl$source_url[i] else NA_character_
      }
    }
  }

  ## Notes section: everything after a "## Notes" or "# Notes" heading.
  notes_idx <- which(str_detect(lines, regex("^#+\\s*Notes\\s*$", ignore_case = TRUE)))
  notes <- NA_character_
  if (length(notes_idx) >= 1) {
    body <- lines[(notes_idx[1] + 1):length(lines)]
    body <- body[!str_detect(body, "^#+\\s")]  # stop if another heading appears
    body <- str_trim(body)
    body <- body[body != ""]
    if (length(body) > 0) notes <- paste(body, collapse = " | ")
  }

  tibble(
    ags = ags,
    have_2013 = have_flags["have_2013"],
    have_2017 = have_flags["have_2017"],
    have_2021 = have_flags["have_2021"],
    have_2025 = have_flags["have_2025"],
    online_source = online_source,
    online_format = online_format,
    online_license = online_license,
    geo_email = emails$geo_email,
    geo_dept_label = emails$geo_dept_label,
    geo_url = emails$geo_url,
    wahl_email = emails$wahl_email,
    wahl_url = emails$wahl_url,
    opendata_email = emails$opendata_email,
    opendata_url = emails$opendata_url,
    notes = notes
  )
}

## ---------------------------------------------------------------------------
## Run

files <- list.files(FINDINGS_DIR, pattern = "^[0-9]{8}\\.md$", full.names = TRUE)
if (length(files) == 0) {
  stop("No findings files in ", FINDINGS_DIR)
}
cat("Parsing", length(files), "findings files...\n")

parsed <- map_dfr(files, parse_findings_file)
cat("Parsed", nrow(parsed), "rows.\n")

## Merge into contacts.csv (in-place update; preserve unmerged rows untouched).
## Force types so empty columns don't get inferred as logical.
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

write_cols <- c(
  "have_2013", "have_2017", "have_2021", "have_2025",
  "online_source", "online_format", "online_license",
  "geo_email", "geo_dept_label", "geo_url",
  "wahl_email", "wahl_url",
  "opendata_email", "opendata_url",
  "notes"
)

merged <- contacts |>
  rows_update(parsed |> select(ags, all_of(write_cols)), by = "ags")

write_csv(merged, CONTACTS_PATH, na = "")
cat("Updated", nrow(parsed), "rows in", CONTACTS_PATH, ".\n")

## Print a summary of what was merged.
parsed |>
  mutate(any_year = have_2013 | have_2017 | have_2021 | have_2025) |>
  left_join(contacts |> select(ags, muni_name_short, state), by = "ags") |>
  select(ags, state, muni_name_short, any_year,
         have_2013, have_2017, have_2021, have_2025,
         has_geo = geo_email, has_wahl = wahl_email) |>
  mutate(has_geo = !is.na(has_geo), has_wahl = !is.na(has_wahl)) |>
  print(n = Inf)
