### Scrape Schleswig-Holstein mayoral election data from wahlen-sh.de
# Vincent Heddesheimer
# March 2026
#
# This script scrapes mayoral election results from the Schleswig-Holstein
# election portal (wahlen-sh.de). The portal publishes results for individual
# elections as HTML pages with structured tables. There are no bulk downloads.
#
# Data source: https://www.wahlen-sh.de/andere_wahlen.html
# Coverage: 2023–2026 (all elections published on the portal)
#
# Output: data/mayoral_elections/raw/sh/sh_mayoral_scraped.rds
#         data/mayoral_elections/raw/sh/sh_mayoral_scraped.csv
#
# ============================================================================
# ARCHITECTURE
# ============================================================================
#
# Each election has its own subdirectory on wahlen-sh.de, e.g.:
#   https://www.wahlen-sh.de/bgmwahl_2025_kiel/
#   https://www.wahlen-sh.de/bgmstichwahl_2025_kiel/
#
# The results page contains:
#   1. A header with election date, municipality name, and result status
#   2. A "Stichwahlteilnehmer" table (only on Hauptwahl pages with a subsequent Stichwahl)
#   3. An "Ergebnisübersicht" / "Stimmen der Bewerber" table with candidate results
#   4. A <tfoot> section with Wahlberechtigte, Wähler, Ungültige, Gültige
#   5. A turnout figure ("Wahlbeteiligung: XX,X %")
#
# Candidate data is embedded in HTML `data-sort` attributes, which contain
# clean numeric values (no thousands separators or formatting).
#
# Table formats vary:
#   Format A (separate columns): Party | Candidate | Stimmen | Anteil
#     → Used for Hauptwahl with party affiliations (e.g., Kiel 2025)
#   Format B (combined column): Bewerber/in | Stimmen (Anzahl + Anteil)
#     → Used for simpler elections (e.g., Ahrensbök 2023)
#   Format C (Stichwahl): Direktkandidat | Stimmen | Anteil
#     → Used for Stichwahl pages (e.g., Pinneberg 2023)
#
# AGS assignment:
#   Municipality names are resolved against the authoritative 2021 register
#   (ags_crosswalks: ags_21 / ags_name_21) by a normalised name join, and the
#   result is asserted. There is deliberately NO hardcoded code table — see
#   the AGS MAPPING section for why.
#
# ============================================================================
# ELECTION URL REGISTRY
# ============================================================================
#
# The index page (andere_wahlen.html) links to ~36 elections. However, some
# Hauptwahl pages are not listed when only the Stichwahl is linked, and some
# Stichwahl pages are not listed when only the Hauptwahl is linked (the 2024
# runoffs live under "bgm_stichwahl_<year>_<name>" and appear nowhere on the
# index). These "hidden" pages were discovered by probing URLs; the integrity
# check at the end of the script fails loudly if another one is missing.
#
# Each entry below specifies:
#   - url_slug: path segment after wahlen-sh.de/
#   - round: "hauptwahl" or "stichwahl"
#   - municipality: name for AGS matching
#   - year: election year
#
# ============================================================================

rm(list = ls())
gc()

pacman::p_load(
  tidyverse,
  rvest,
  xml2,
  httr,
  data.table,
  lubridate,
  conflicted,
  here
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")

setwd(here::here())
options(scipen = 999)

# ============================================================================
# ELECTION REGISTRY
# ============================================================================
# All known mayoral election pages on wahlen-sh.de as of March 2026.
# Each row = one page to scrape. Hauptwahl and Stichwahl are separate entries.
# For elections with Stichwahl, there are always two pages (HW + SW).
#
# "url_suffix" is appended after the base URL slug when the main page
# shows a VG-level result and the municipality result is on a subpage.
# Most elections have url_suffix = "" (root page has municipality results).

elections <- tribble(
  ~url_slug,                              ~round,       ~municipality,         ~year, ~url_suffix,
  # ---- 2023 ----
  "bgmwahl_ahrensboek",                   "hauptwahl",  "Ahrensbök",           2023L, "",
  "bgmwahl_barsbuettel",                  "hauptwahl",  "Barsbüttel",          2023L, "",
  "bgmwahl_bredstedt",                    "hauptwahl",  "Bredstedt",           2023L, "",
  "bgmwahl_2023_hohenwestedt",            "hauptwahl",  "Hohenwestedt",        2023L, "",
  "bm_kaltenkirchen",                     "hauptwahl",  "Kaltenkirchen",       2023L, "",
  "bm_Luebeck",                           "hauptwahl",  "Lübeck",              2023L, "",
  "bm_Stichwahl_Luebeck",                 "stichwahl",  "Lübeck",              2023L, "",
  "ob_norderstedt",                       "hauptwahl",  "Norderstedt",         2023L, "",
  "ob_norderstedt_sw2023",                "stichwahl",  "Norderstedt",         2023L, "",
  "bgmwahl_pinneberg",                    "hauptwahl",  "Pinneberg",           2023L, "",
  "bgmstichwahl2301_pinneberg",           "stichwahl",  "Pinneberg",           2023L, "",
  "bgmwahl_tornesch",                     "hauptwahl",  "Tornesch",            2023L, "",
  # ---- 2024 ----
  "bgm_2024_bad_bramstedt",              "hauptwahl",  "Bad Bramstedt",       2024L, "",
  "bgm_2024_brunsbuettel",               "hauptwahl",  "Brunsbüttel",         2024L, "",
  "bgm_2024_buesum",                     "hauptwahl",  "Büsum",               2024L, "",
  "bgm_2024_Flintbek",                   "hauptwahl",  "Flintbek",            2024L, "",
  "bgm_stichwahl_2024_Flintbek",         "stichwahl",  "Flintbek",            2024L, "",
  "bgm_2024_harrislee",                  "hauptwahl",  "Harrislee",           2024L, "",
  "bgmwahl_2024_kronshagen",             "hauptwahl",  "Kronshagen",          2024L, "",
  "bgm_2024_luetjenburg",                "hauptwahl",  "Lütjenburg",          2024L, "",
  "bgm_2024_neustadt_in_holstein",        "hauptwahl",  "Neustadt in Holstein", 2024L, "",
  "bgm_2024_sankt_peter-ording",          "hauptwahl",  "Sankt Peter-Ording",  2024L, "",
  "bgm_stichwahl_2024_sankt_peter-ording", "stichwahl", "Sankt Peter-Ording",  2024L, "",
  "bm_wahl_2024_Stockelsdorf",           "hauptwahl",  "Stockelsdorf",        2024L, "",
  "bm_2024_Wahlstedt",                   "hauptwahl",  "Wahlstedt",           2024L, "",
  "bgmwahl_2024_wedel",                  "hauptwahl",  "Wedel",               2024L, "",
  "bgm_stichwahl_2024_wedel",            "stichwahl",  "Wedel",               2024L, "",
  # ---- 2025 ----
  "bgmwahl_2025_barmstedt",              "hauptwahl",  "Barmstedt",           2025L, "ergebnisse_gemeinde_010560002002.html",
  "bgmstichwahl_2025_barmstedt",         "stichwahl",  "Barmstedt",           2025L, "ergebnisse_gemeinde_010560002002.html",
  "bgmwahl_2025_elmshorn",               "hauptwahl",  "Elmshorn",            2025L, "",
  "bgmstichwahl_2025_elmshorn",          "stichwahl",  "Elmshorn",            2025L, "",
  "bgmwahl_2025_glinde",                 "hauptwahl",  "Glinde",              2025L, "",
  "bgmstichwahl_2025_glinde",            "stichwahl",  "Glinde",              2025L, "",
  "bgmwahl_2025_gluecksburg",            "hauptwahl",  "Glücksburg",          2025L, "",
  "bgmstichwahl_2025_gluecksburg",       "stichwahl",  "Glücksburg",          2025L, "",
  "bgmwahl_2025_grosshansdorf",           "hauptwahl",  "Großhansdorf",        2025L, "",
  "bgmwahl_2025_handewitt",              "hauptwahl",  "Handewitt",           2025L, "",
  "bgmwahl_2025_heiligenhafen",           "hauptwahl",  "Heiligenhafen",       2025L, "",
  "bgmwahl_2025_kiel",                   "hauptwahl",  "Kiel",                2025L, "",
  "bgmstichwahl_2025_kiel",              "stichwahl",  "Kiel",                2025L, "",
  "bgmwahl_2025_laboe",                  "hauptwahl",  "Laboe",               2025L, "",
  "bgmwahl_2025_meldorf",                "hauptwahl",  "Meldorf",             2025L, "",
  "bgmwahl_2025_oldenburg_in_holstein",  "hauptwahl",  "Oldenburg in Holstein", 2025L, "",
  "bgmwahl_2025_scharbeutz",             "hauptwahl",  "Scharbeutz",          2025L, "",
  "bgmwahl_2025_schleswig",              "hauptwahl",  "Schleswig",           2025L, "",
  "bgmstichwahl_2025_schleswig",         "stichwahl",  "Schleswig",           2025L, "",
  "bgmwahl_2025_sylt",                   "hauptwahl",  "Sylt",                2025L, "ergebnisse_gemeinde_54168.html",
  "bgmstichwahl_2025_sylt",              "stichwahl",  "Sylt",                2025L, "ergebnisse_gemeinde_54168.html",
  # ---- 2026 (upcoming — may not have results yet) ----
  "bgmwahl_2026_ratekau",                "hauptwahl",  "Ratekau",             2026L, "",
  "bgmwahl_2026_schwentinental",         "hauptwahl",  "Schwentinental",      2026L, ""
)

# ============================================================================
# AGS MAPPING
# ============================================================================
# Municipality names from the registry above are resolved to 8-digit 2021 AGS
# codes by joining against the authoritative register (ags_crosswalks:
# ags_21 / ags_name_21, restricted to state 01).
#
# DO NOT replace this with a hand-written lookup table. The table that stood
# here until July 2026 had 24 of its 37 codes wrong, and 21 of those were the
# valid code of a DIFFERENT Schleswig-Holstein Gemeinde — so those elections
# were silently filed under the wrong municipality in every harmonised output
# (several of the errors were chained: Oldenburg i.H. sat on Ratekau's code,
# Ratekau on Schönwalde's, Stockelsdorf on Scharbeutz's, Scharbeutz on
# Süsel's). The assertions below make any future mismatch fail loudly.

#' Normalise a Schleswig-Holstein municipality name for register matching
#'
#' Drops the administrative suffix after the comma (", Stadt", ", Hansestadt",
#' ", Landeshauptstadt", ", Kirchspiel"), drops parenthetical qualifiers
#' ("(Ostsee)"), expands "i. H." -> "in Holstein" and "St." -> "Sankt",
#' transliterates umlauts, and strips everything that is not alphanumeric.
#'
#' @param x Character vector of municipality names
#' @return Character vector of normalised match keys
normalise_sh_name <- function(x) {
  x <- as.character(x)
  x <- sub(",.*$", "", x)                       # ", Stadt" / ", Hansestadt" / ...
  x <- gsub("\\s*\\([^)]*\\)", "", x)           # "(Ostsee)", "(Forstgutsbez.)"
  x <- gsub("\\bi\\.?\\s*H\\.?\\b", "in Holstein", x, ignore.case = TRUE)
  x <- gsub("\\bSt\\.\\s*", "Sankt ", x)
  x <- gsub("Ä", "Ae", x)
  x <- gsub("Ö", "Oe", x)
  x <- gsub("Ü", "Ue", x)
  x <- gsub("ä", "ae", x)
  x <- gsub("ö", "oe", x)
  x <- gsub("ü", "ue", x)
  x <- gsub("ß", "ss", x)
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x)
}

# Authoritative 2021 register of Schleswig-Holstein municipalities
xwalk <- readRDS("data/crosswalks/final/ags_crosswalks.rds")

sh_register <- xwalk %>%
  filter(substr(ags_21, 1, 2) == "01") %>%
  distinct(ags_21, ags_name_21) %>%
  # gemeindefreie Gebiete are not municipalities and never hold a mayoral
  # election; keeping them would create spurious name ambiguity ("Buchholz")
  filter(!grepl("gemfr\\. Gebiet", ags_name_21)) %>%
  mutate(name_key = normalise_sh_name(ags_name_21))

ags_map <- tibble(municipality = sort(unique(elections$municipality))) %>%
  mutate(name_key = normalise_sh_name(municipality)) %>%
  left_join(sh_register, by = "name_key")

# --- hard assertions: a name that stops resolving must fail, not mis-file ---
unresolved <- ags_map$municipality[is.na(ags_map$ags_21)]
if (length(unresolved) > 0) {
  stop("AGS lookup failed for: ", paste(unresolved, collapse = ", "),
       "\n  -> fix the spelling in the election registry, or extend",
       " normalise_sh_name() to cover the new name form.")
}
ambiguous <- unique(ags_map$municipality[duplicated(ags_map$municipality)])
if (length(ambiguous) > 0) {
  stop("AGS lookup is ambiguous (several SH Gemeinden share the name): ",
       paste(ambiguous, collapse = ", "),
       "\n  -> disambiguate with a Kreis-qualified name or an explicit override.")
}

stopifnot(
  nrow(ags_map) == n_distinct(elections$municipality),
  is.character(ags_map$ags_21),
  all(nchar(ags_map$ags_21) == 8),
  all(substr(ags_map$ags_21, 1, 2) == "01"),
  all(ags_map$ags_21 %in% sh_register$ags_21),          # exists in 2021 register
  !any(duplicated(ags_map$ags_21)),                     # no two names, one code
  all(normalise_sh_name(ags_map$ags_name_21) ==         # register name == scraped name
        normalise_sh_name(ags_map$municipality))
)

# Regression fixture: the four chained mis-assignments of the old hardcoded
# table (each sat on the next municipality's real code) plus the two city AGS.
sh_ags_fixture <- c(
  "Kiel"                  = "01002000",
  "Lübeck"                = "01003000",
  "Oldenburg in Holstein" = "01055033",
  "Ratekau"               = "01055035",
  "Scharbeutz"            = "01055044",
  "Stockelsdorf"          = "01055040"
)
fixture_present <- intersect(names(sh_ags_fixture), ags_map$municipality)
stopifnot(all(
  ags_map$ags_21[match(fixture_present, ags_map$municipality)] ==
    sh_ags_fixture[fixture_present]
))

ags_map <- ags_map %>%
  select(municipality, ags = ags_21, register_name = ags_name_21)

cat("AGS lookup resolved", nrow(ags_map), "municipalities from the 2021 register\n")

# ============================================================================
# SCRAPING FUNCTIONS
# ============================================================================

BASE_URL <- "http://wahlen-sh.de"

#' Fetch and parse an election results page
#'
#' @param url_slug Path segment (e.g., "bgmwahl_2025_kiel")
#' @param url_suffix Optional subpage path (e.g., "ergebnisse_gemeinde_54168.html")
#' @return Parsed HTML document, or NULL on failure
fetch_page <- function(url_slug, url_suffix = "") {
  if (url_suffix != "") {
    url <- paste0(BASE_URL, "/", url_slug, "/", url_suffix)
  } else {
    url <- paste0(BASE_URL, "/", url_slug, "/")
  }

  cat("  Fetching:", url, "\n")

  tryCatch({
    resp <- GET(url, timeout(30))
    if (status_code(resp) != 200) {
      cat("  WARNING: HTTP", status_code(resp), "for", url, "\n")
      return(NULL)
    }
    read_html(resp)
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Extract election date from page header
#'
#' The date appears in a <p class="stand"> tag in the format "DD. Monat YYYY"
#' (German month names). Examples:
#'   "16. November 2025, Kiel, Landeshauptstadt"
#'   "10. September 2023"
#'
#' @param page Parsed HTML document
#' @return Date object, or NA
extract_date <- function(page) {
  # German month names
  months_de <- c(
    "Januar" = 1, "Februar" = 2, "März" = 3, "April" = 4,
    "Mai" = 5, "Juni" = 6, "Juli" = 7, "August" = 8,
    "September" = 9, "Oktober" = 10, "November" = 11, "Dezember" = 12
  )

  stand_nodes <- page %>% html_nodes("p.stand")
  for (node in stand_nodes) {
    text <- html_text(node)
    # Match "DD. Monat YYYY"
    m <- regmatches(text, regexpr("(\\d{1,2})\\.\\s+(\\w+)\\s+(\\d{4})", text))
    if (length(m) == 1 && nchar(m) > 0) {
      parts <- strsplit(m, "\\. |\\s+")[[1]]
      day <- as.integer(parts[1])
      month_name <- parts[2]
      yr <- as.integer(parts[3])
      month_num <- months_de[month_name]
      if (!is.na(month_num)) {
        return(as.Date(paste(yr, month_num, day, sep = "-")))
      }
    }
  }
  return(NA)
}

#' Extract election type from page title
#'
#' Determines whether the election is a Bürgermeisterwahl or
#' Oberbürgermeisterwahl based on the <h1> or <title> text.
#'
#' @param page Parsed HTML document
#' @return Character string: "Bürgermeisterwahl" or "Oberbürgermeisterwahl"
extract_election_type <- function(page) {
  title_text <- page %>% html_node("title") %>% html_text()
  h1_text <- page %>% html_node("h1") %>% html_text()
  combined <- paste(title_text, h1_text)

  if (grepl("Oberb", combined, ignore.case = TRUE)) {
    return("Oberbürgermeisterwahl")
  } else {
    return("Bürgermeisterwahl")
  }
}

#' Extract voter statistics from tfoot
#'
#' The <tfoot> of the main results table contains:
#'   Wahlberechtigte (eligible voters), Wähler (voters),
#'   Ungültige Stimmen (invalid), Gültige Stimmen (valid)
#' Values are in data-sort attributes.
#'
#' @param page Parsed HTML document
#' @return Named list with eligible_voters, number_voters, invalid_votes, valid_votes
extract_voter_stats <- function(page) {
  stats <- list(
    eligible_voters = NA_real_,
    number_voters = NA_real_,
    invalid_votes = NA_real_,
    valid_votes = NA_real_
  )

  # Find the main results table (table-stimmen class)
  tables <- page %>% html_nodes("table.table-stimmen")
  if (length(tables) == 0) {
    # Fallback: try any table with tfoot
    tables <- page %>% html_nodes("table")
  }

  for (tbl in tables) {
    tfoot <- tbl %>% html_node("tfoot")
    if (is.na(tfoot)) next

    rows <- tfoot %>% html_nodes("tr")
    for (row in rows) {
      label_node <- row %>% html_node("th")
      if (is.na(label_node)) next
      label <- label_node %>% html_attr("data-sort")
      if (is.na(label)) label <- html_text(label_node)
      label <- trimws(label)

      # Get the numeric value from the first td with a data-sort attribute
      tds <- row %>% html_nodes("td")
      value <- NA_real_
      for (td in tds) {
        ds <- td %>% html_attr("data-sort")
        if (!is.na(ds) && ds != "" && ds != "-") {
          value <- as.numeric(ds)
          break
        }
      }

      if (grepl("Wahlberechtigte", label, ignore.case = TRUE)) {
        stats$eligible_voters <- value
      } else if (grepl("hler$", label, ignore.case = TRUE)) {
        # "Wähler" — but not "Wahlberechtigte"
        stats$number_voters <- value
      } else if (grepl("ng.ltige", label, ignore.case = TRUE)) {
        stats$invalid_votes <- value
      } else if (grepl("ltige", label, ignore.case = TRUE) &&
                 !grepl("ng.ltige", label, ignore.case = TRUE)) {
        stats$valid_votes <- value
      }
    }
    # If we found data, stop looking at more tables
    if (!is.na(stats$eligible_voters)) break
  }

  return(stats)
}

#' Extract candidate results from the main table
#'
#' The results table (<table class="table-stimmen">) has candidates in <tbody>.
#' Table formats vary (see header documentation). This function handles all
#' three observed formats by inspecting <thead> column headers.
#'
#' Candidate names and votes are extracted from data-sort attributes on
#' <th> and <td> elements.
#'
#' @param page Parsed HTML document
#' @return tibble with candidate_name, candidate_party, candidate_votes, candidate_voteshare
extract_candidates <- function(page) {
  candidates <- tibble(
    candidate_name = character(),
    candidate_party = character(),
    candidate_votes = numeric(),
    candidate_voteshare = numeric()
  )

  # Find the main results table (table-stimmen)
  tables <- page %>% html_nodes("table.table-stimmen")
  if (length(tables) == 0) return(candidates)

  # Use the LAST table-stimmen (the first might be the "Stichwahlteilnehmer" table)
  main_table <- tables[[length(tables)]]

  # Find ALL tbody elements (some pages have multiple tbody sections)
  # We want the one with candidate results (has th + td rows with numeric data-sort)
  tbodies <- main_table %>% html_nodes("tbody")
  if (length(tbodies) == 0) return(candidates)

  # Collect all candidate rows from all tbody elements
  all_rows <- list()
  for (tb in tbodies) {
    all_rows <- c(all_rows, tb %>% html_nodes("tr"))
  }
  if (length(all_rows) == 0) return(candidates)

  # Robust extraction: for each row, extract candidate data regardless of format.
  # Strategy:
  # 1. Look for candidate name in <th> data-sort (non-empty, not a stat label)
  # 2. Look for party in <abbr> or <span class="partei__name"> in any cell
  # 3. Look for votes/share in <td> data-sort (numeric values)

  stat_labels <- c("Wahlberechtigte", "Wähler", "Ungültige Stimmen",
                    "Gültige Stimmen", "Datei", "Beschreibung")

  for (row in all_rows) {
    ths <- row %>% html_nodes("th")
    tds <- row %>% html_nodes("td")
    all_cells <- c(ths, tds)

    cand_name <- NA_character_
    cand_party <- NA_character_
    cand_votes <- NA_real_
    cand_share <- NA_real_

    # Skip tfoot-style rows (stat labels)
    first_th_sort <- if (length(ths) >= 1) html_attr(ths[[1]], "data-sort") else NA
    if (!is.na(first_th_sort) && first_th_sort %in% stat_labels) next

    # --- Extract candidate name ---
    # Look in all <th> elements for a non-empty data-sort that looks like a name
    for (th in ths) {
      ds <- trimws(html_attr(th, "data-sort"))
      if (!is.na(ds) && ds != "" && grepl(",", ds)) {
        # Looks like "Last, First" format
        cand_name <- ds
        break
      }
    }

    # --- Extract party ---
    # Look in all cells for <abbr> elements (party abbreviations)
    for (cell in all_cells) {
      abbr_node <- cell %>% html_node("abbr")
      if (!is.na(abbr_node)) {
        party_text <- trimws(html_text(abbr_node))
        party_title <- trimws(html_attr(abbr_node, "title"))
        if (!is.na(party_text) && party_text != "") {
          # Check if this is a Format B "Name (Party)" string or a pure party name
          if (grepl(",", party_text) && grepl("\\(", party_text)) {
            # Format B: "Last, First (Party)" — use title for fuller info
            if (!is.na(party_title) && party_title != "") {
              cand_party <- party_title
            } else {
              cand_party <- party_text
            }
          } else {
            # Pure party name — use abbreviated text
            cand_party <- party_text
          }
        }
        break
      }
      # Fallback: check for <span class="partei__name"> without <abbr>
      partei_span <- cell %>% html_node("span.partei__name")
      if (!is.na(partei_span)) {
        abbr_inner <- partei_span %>% html_node("abbr")
        if (is.na(abbr_inner)) {
          cand_party <- trimws(html_text(partei_span))
          break
        }
      }
    }

    # --- Handle Format B: combined "Name (Party)" in abbr title ---
    # If we didn't find a name yet but found a party that contains "Name (Party)",
    # or if we found an abbr title with parentheses
    if (is.na(cand_name) && !is.na(cand_party)) {
      # Check if cand_party is actually "Name (Party)" format
      m <- regmatches(cand_party, regexec("^(.+?)\\s*\\((.+)\\)$", cand_party))[[1]]
      if (length(m) == 3) {
        cand_name <- trimws(m[2])
        cand_party <- trimws(m[3])
      }
    }

    # If still no name, check for Format B with abbr in first th
    if (is.na(cand_name) && length(ths) >= 1) {
      abbr_node <- ths[[1]] %>% html_node("abbr")
      if (!is.na(abbr_node)) {
        title_text <- trimws(html_attr(abbr_node, "title"))
        if (!is.na(title_text) && title_text != "") {
          m <- regmatches(title_text, regexec("^(.+?)\\s*\\((.+)\\)$", title_text))[[1]]
          if (length(m) == 3) {
            cand_name <- trimws(m[2])
            cand_party <- trimws(m[3])
          } else {
            # No parentheses — might be "Nein-Stimmen" or "Ja-Stimmen"
            cand_name <- title_text
            if (grepl("Stimmen$", title_text)) {
              cand_party <- title_text
            }
          }
        }
      }
    }

    # --- Extract votes and voteshare ---
    # Look for numeric data-sort values in <td> elements
    numeric_values <- c()
    for (td in tds) {
      ds <- html_attr(td, "data-sort")
      if (!is.na(ds) && ds != "" && ds != "-" && !grepl("[a-zA-Z]", ds)) {
        numeric_values <- c(numeric_values, as.numeric(ds))
      }
    }

    # First numeric = votes, second = voteshare (percentage)
    if (length(numeric_values) >= 1) cand_votes <- numeric_values[1]
    if (length(numeric_values) >= 2) cand_share <- numeric_values[2] / 100

    # Only add if we got some candidate data
    if (!is.na(cand_name) || !is.na(cand_votes)) {
      candidates <- bind_rows(candidates, tibble(
        candidate_name = cand_name,
        candidate_party = cand_party,
        candidate_votes = cand_votes,
        candidate_voteshare = cand_share
      ))
    }
  }

  return(candidates)
}

#' Standardise party names
#'
#' Maps various party name formats from wahlen-sh.de to standard abbreviations.
#' Examples:
#'   "Christlich Demokratische Union Deutschlands" -> "CDU"
#'   "Einzelbewerber" / "unabhängiger Bewerber" -> "EB"
#'   "CDU und FDP" -> "CDU und FDP" (kept as coalition label)
#'
#' @param party Character vector of raw party names
#' @return Character vector of standardised names
standardise_sh_party <- function(party) {
  party <- as.character(party)

  mapping <- c(
    "Christlich Demokratische Union Deutschlands" = "CDU",
    "Sozialdemokratische Partei Deutschlands" = "SPD",
    "BÜNDNIS 90/DIE GRÜNEN" = "GRÜNE",
    "B\u00dcNDNIS 90/DIE GR\u00dcNEN" = "GRÜNE",
    "Freie Demokratische Partei" = "FDP",
    "DIE LINKE" = "LINKE",
    "Die Linke" = "LINKE",
    "Alternative für Deutschland" = "AfD",
    "Alternative f\u00fcr Deutschland" = "AfD",
    "Südschleswigscher Wählerverband" = "SSW",
    "S\u00fcdschleswigscher W\u00e4hlerverband" = "SSW",
    "Die PARTEI" = "Die PARTEI",
    "dieBasis" = "dieBasis",
    "Freie Wähler" = "FW",
    "Freie W\u00e4hler" = "FW",
    "Bündnis Sahra Wagenknecht" = "BSW",
    "B\u00fcndnis Sahra Wagenknecht" = "BSW"
  )

  result <- party
  for (i in seq_along(result)) {
    if (is.na(result[i])) next
    p <- result[i]

    # Check exact match first
    if (p %in% names(mapping)) {
      result[i] <- mapping[p]
      next
    }

    # Check if it's an Einzelbewerber variant
    # (the optional "e" also catches the site's "Einzelbwerber" typo on
    #  bgm_stichwahl_2024_sankt_peter-ording; "Einzelbewerberin" matches too)
    if (grepl("Einzelbe?werber|unabh.ngiger Bewerber", p, ignore.case = TRUE)) {
      result[i] <- "EB"
      next
    }

    # Check for EB: prefix (e.g., "EB:Ketelsen") or "EB Name" patterns
    if (grepl("^EB:", p) || grepl("^EB\\s+", p)) {
      result[i] <- "EB"
      next
    }

    # Partial match for common parties
    if (grepl("^CDU", p)) { result[i] <- p; next }  # Keep "CDU und FDP" etc.
    if (grepl("^SPD", p) || grepl("Sozialdemokratische", p)) { result[i] <- p; next }
    if (grepl("GR.NE", p) || grepl("B.NDNIS\\s*90", p)) { result[i] <- "GRÜNE"; next }
    if (grepl("^FDP", p)) { result[i] <- "FDP"; next }
    if (grepl("^AfD", p) || grepl("Alternative\\s+f", p)) { result[i] <- "AfD"; next }
    if (grepl("^SSW", p) || grepl("dschleswig", p, ignore.case = TRUE)) { result[i] <- p; next }
    if (grepl("^Einzelkandidatin$|^Einzelkandidat$", p)) { result[i] <- "EB"; next }
    if (grepl("Wahlvorschlag", p)) { result[i] <- p; next }  # Keep coalition labels
    if (grepl("^T.P$", p)) { result[i] <- p; next }  # Local party

    # Keep as-is for local parties (FWG, etc.)
  }

  return(result)
}

#' Parse candidate name into last and first name
#'
#' SH election pages use "Last, First" format in data-sort attributes.
#' Some also include titles (e.g., "Keller, Dr. Stephan").
#'
#' @param name Character vector of names in "Last, First" format
#' @return tibble with candidate_last_name and candidate_first_name
parse_sh_name <- function(name) {
  tibble(
    candidate_last_name = ifelse(
      !is.na(name) & grepl(",", name),
      trimws(sub(",.*", "", name)),
      NA_character_
    ),
    candidate_first_name = ifelse(
      !is.na(name) & grepl(",", name),
      trimws(sub("^[^,]+,\\s*", "", name)),
      NA_character_
    )
  )
}


# ============================================================================
# MAIN SCRAPING LOOP
# ============================================================================

cat("\n=== Scraping Schleswig-Holstein mayoral elections ===\n")
cat("Elections to scrape:", nrow(elections), "\n\n")

all_results <- list()
errors <- character()

for (i in seq_len(nrow(elections))) {
  el <- elections[i, ]
  cat(sprintf("[%d/%d] %s %s (%s, %d)\n",
              i, nrow(elections), el$municipality, el$round, el$url_slug, el$year))

  # Polite delay between requests
  if (i > 1) Sys.sleep(1)

  # Fetch page
  page <- fetch_page(el$url_slug, el$url_suffix)
  if (is.null(page)) {
    msg <- paste("Failed to fetch:", el$url_slug)
    cat("  SKIPPED:", msg, "\n\n")
    errors <- c(errors, msg)
    next
  }

  # Extract date
  election_date <- extract_date(page)
  if (is.na(election_date)) {
    cat("  WARNING: Could not extract date\n")
  }

  # Extract election type
  election_type <- extract_election_type(page)

  # Extract voter stats
  stats <- extract_voter_stats(page)

  # Extract candidates
  cands <- extract_candidates(page)

  if (nrow(cands) == 0) {
    msg <- paste("No candidates found:", el$url_slug)
    cat("  WARNING:", msg, "\n\n")
    errors <- c(errors, msg)
    next
  }

  # Compute turnout
  turnout <- NA_real_
  if (!is.na(stats$number_voters) && !is.na(stats$eligible_voters) &&
      stats$eligible_voters > 0) {
    turnout <- stats$number_voters / stats$eligible_voters
  }

  # Look up AGS
  ags_row <- ags_map %>% filter(municipality == el$municipality)
  ags_code <- if (nrow(ags_row) == 1) ags_row$ags else NA_character_

  # Parse names
  names_parsed <- parse_sh_name(cands$candidate_name)

  # Standardise party names
  cands$candidate_party_std <- standardise_sh_party(cands$candidate_party)

  # Fix party names that are actually candidate last names (EB candidates)
  # These appear when the wahlen-sh.de party abbreviation is just the surname
  for (j in seq_len(nrow(cands))) {
    if (!is.na(cands$candidate_party_std[j]) && !is.na(names_parsed$candidate_last_name[j])) {
      if (cands$candidate_party_std[j] == names_parsed$candidate_last_name[j]) {
        cands$candidate_party_std[j] <- "EB"
      }
    }
  }

  # Build output
  result <- tibble(
    ags = ags_code,
    ags_name = el$municipality,
    state = "01",
    state_name = "Schleswig-Holstein",
    election_year = el$year,
    election_date = election_date,
    election_type = election_type,
    round = el$round,
    eligible_voters = stats$eligible_voters,
    number_voters = stats$number_voters,
    valid_votes = stats$valid_votes,
    invalid_votes = stats$invalid_votes,
    turnout = turnout,
    candidate_name = cands$candidate_name,
    candidate_last_name = names_parsed$candidate_last_name,
    candidate_first_name = names_parsed$candidate_first_name,
    candidate_party = cands$candidate_party_std,
    candidate_party_raw = cands$candidate_party,
    candidate_votes = cands$candidate_votes,
    candidate_voteshare = cands$candidate_voteshare,
    source_url = paste0(BASE_URL, "/", el$url_slug, "/",
                        ifelse(el$url_suffix != "", el$url_suffix, ""))
  )

  cat(sprintf("  -> %d candidates, date: %s, EV: %s, turnout: %s%%\n",
              nrow(result),
              ifelse(is.na(election_date), "NA", as.character(election_date)),
              ifelse(is.na(stats$eligible_voters), "NA",
                     format(stats$eligible_voters, big.mark = ",")),
              ifelse(is.na(turnout), "NA",
                     sprintf("%.1f", turnout * 100))))

  all_results[[i]] <- result
  cat("\n")
}

# ============================================================================
# COMBINE AND CLEAN
# ============================================================================

cat("\n=== Combining results ===\n")

sh_raw <- bind_rows(all_results)

# Remove upcoming elections with no results (EV = 0 and all votes = 0)
# NA-safe: eligible_voters is NA whenever the <tfoot> turnout block failed to
# parse. A bare `!(eligible_voters == 0 & ...)` evaluates to NA for those rows
# and filter() DROPS NA, silently deleting elections that were scraped fine
# apart from the turnout block. coalesce(..., FALSE) makes NA mean "keep".
n_before <- nrow(sh_raw)
sh_raw <- sh_raw %>%
  filter(!coalesce(
    eligible_voters == 0 & (is.na(candidate_votes) | candidate_votes == 0),
    FALSE
  ))
n_upcoming <- n_before - nrow(sh_raw)
if (n_upcoming > 0) {
  cat("Removed", n_upcoming, "rows from upcoming elections (no results yet)\n")
}

cat("Total rows scraped:", nrow(sh_raw), "\n")
cat("Unique elections:", n_distinct(paste(sh_raw$ags, sh_raw$election_date, sh_raw$round)), "\n")

if (length(errors) > 0) {
  cat("\nErrors/warnings during scraping:\n")
  for (e in errors) cat("  -", e, "\n")
}

# Summary
cat("\nBy round:\n")
print(table(sh_raw$round))

cat("\nBy year:\n")
print(table(sh_raw$election_year))

cat("\nBy election_type:\n")
print(table(sh_raw$election_type))

cat("\nMunicipalities scraped:", n_distinct(sh_raw$ags), "\n")

# ============================================================================
# INTEGRITY CHECK — every sub-50 % Hauptwahl needs a paired Stichwahl
# ============================================================================
# In Schleswig-Holstein a mayor is only elected in the Hauptwahl with an
# absolute majority; otherwise a Stichwahl follows. So a Hauptwahl whose
# leading candidate polled < 50 % and for which no Stichwahl was scraped means
# the runoff page is MISSING from the registry above — downstream that seats
# the Hauptwahl leader as mayor, i.e. the wrong person.
#
# This is not hypothetical: the runoffs for Flintbek, Sankt Peter-Ording and
# Wedel (all 2024) were absent for exactly this reason. SH publishes them under
# a slug the index page does not link, "bgm_stichwahl_<year>_<name>" — and the
# name token sometimes keeps the capitalisation of the Hauptwahl slug
# (".../bgm_stichwahl_2024_Flintbek").

sw_rounds <- sh_raw %>%
  filter(round == "stichwahl") %>%
  distinct(ags, election_year, election_date)

hw_leaders <- sh_raw %>%
  filter(round == "hauptwahl", !is.na(candidate_votes)) %>%
  group_by(ags, ags_name, election_year) %>%
  slice_max(candidate_votes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(lead_share = coalesce(
    candidate_voteshare,
    ifelse(!is.na(valid_votes) & valid_votes > 0, candidate_votes / valid_votes, NA_real_)
  ))

missing_sw <- hw_leaders[0, ]
for (i in seq_len(nrow(hw_leaders))) {
  hw <- hw_leaders[i, ]
  if (is.na(hw$lead_share) || hw$lead_share >= 0.5) next
  sw <- sw_rounds %>% filter(ags == hw$ags)
  # a Stichwahl in the same election year, or within 90 days of the Hauptwahl
  paired <- any(sw$election_year == hw$election_year) ||
    (!is.na(hw$election_date) &&
       any(!is.na(sw$election_date) &
             sw$election_date >= hw$election_date &
             sw$election_date <= hw$election_date + 90))
  if (!paired) missing_sw <- bind_rows(missing_sw, hw)
}

if (nrow(missing_sw) > 0) {
  detail <- paste(sprintf(
    "  %s (%s, %d, %s): %s led with %.1f%% — no Stichwahl scraped",
    missing_sw$ags_name, missing_sw$ags, missing_sw$election_year,
    ifelse(is.na(missing_sw$election_date), "date NA",
           as.character(missing_sw$election_date)),
    missing_sw$candidate_name, missing_sw$lead_share * 100
  ), collapse = "\n")
  stop("INTEGRITY FAILURE: ", nrow(missing_sw),
       " Hauptwahl(en) below 50 % with no paired Stichwahl.\n", detail,
       "\nThe runoff page is missing from the ELECTION REGISTRY. Probe\n",
       "  http://wahlen-sh.de/bgm_stichwahl_<year>_<name>/  (and the\n",
       "  bgmstichwahl_<year>_<name> / bgmstichwahl<yy>01_<name> variants),\n",
       "  keeping the capitalisation used by the Hauptwahl slug, then add it.")
}

cat("Integrity check passed: no sub-50% Hauptwahl without a Stichwahl\n")

# ============================================================================
# SAVE RAW DATA
# ============================================================================

cat("\n=== Saving raw data ===\n")

outdir <- "data/mayoral_elections/raw/sh"

write_rds(sh_raw, file.path(outdir, "sh_mayoral_scraped.rds"))
fwrite(sh_raw, file.path(outdir, "sh_mayoral_scraped.csv"))

cat("Saved to:\n")
cat("  -", file.path(outdir, "sh_mayoral_scraped.rds"), "\n")
cat("  -", file.path(outdir, "sh_mayoral_scraped.csv"), "\n")

cat("\n=== Done ===\n")
