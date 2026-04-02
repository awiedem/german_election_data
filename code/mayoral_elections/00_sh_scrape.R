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
#   Municipality names from the page header are matched to AGS codes via
#   the crosswalk file. Manual overrides handle non-standard name formats.
#
# ============================================================================
# ELECTION URL REGISTRY
# ============================================================================
#
# The index page (andere_wahlen.html) links to ~36 elections. However, some
# Hauptwahl pages are not listed when only the Stichwahl is linked. These
# "hidden" Hauptwahl pages were discovered by probing URLs.
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
  "bgm_2024_harrislee",                  "hauptwahl",  "Harrislee",           2024L, "",
  "bgmwahl_2024_kronshagen",             "hauptwahl",  "Kronshagen",          2024L, "",
  "bgm_2024_luetjenburg",                "hauptwahl",  "Lütjenburg",          2024L, "",
  "bgm_2024_neustadt_in_holstein",        "hauptwahl",  "Neustadt in Holstein", 2024L, "",
  "bgm_2024_sankt_peter-ording",          "hauptwahl",  "Sankt Peter-Ording",  2024L, "",
  "bm_wahl_2024_Stockelsdorf",           "hauptwahl",  "Stockelsdorf",        2024L, "",
  "bm_2024_Wahlstedt",                   "hauptwahl",  "Wahlstedt",           2024L, "",
  "bgmwahl_2024_wedel",                  "hauptwahl",  "Wedel",               2024L, "",
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
# Map municipality names to 8-digit AGS codes. Names are matched against
# the crosswalk file first; manual overrides handle non-standard formats.

# Load crosswalk for AGS lookup
xwalk <- readRDS("data/crosswalks/final/ags_crosswalks.rds")
sh_ags <- xwalk %>%
  filter(ags < 2000000) %>%
  mutate(ags_str = sprintf("%08d", ags)) %>%
  # Take the most recent year for each AGS
  group_by(ags_str) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(ags_str, ags_name) %>%
  distinct()

# Manual AGS mapping for municipalities on wahlen-sh.de
# These are looked up from the crosswalk or official AGS registry
ags_map <- tribble(
  ~municipality,            ~ags,
  "Ahrensbök",              "01055001",
  "Bad Bramstedt",          "01060005",
  "Barmstedt",              "01056002",
  "Barsbüttel",             "01062005",
  "Bredstedt",              "01054015",
  "Brunsbüttel",            "01051011",
  "Büsum",                  "01051012",
  "Elmshorn",               "01056015",
  "Flintbek",               "01058043",
  "Glinde",                 "01062019",
  "Glücksburg",             "01059027",
  "Großhansdorf",           "01062022",
  "Handewitt",              "01059033",
  "Harrislee",              "01059035",
  "Heiligenhafen",          "01055019",
  "Hohenwestedt",           "01058072",
  "Kaltenkirchen",          "01060044",
  "Kiel",                   "01002000",
  "Kronshagen",             "01058087",
  "Laboe",                  "01057035",
  "Lübeck",                 "01003000",
  "Lütjenburg",             "01057037",
  "Meldorf",                "01051076",
  "Neustadt in Holstein",   "01055032",
  "Norderstedt",            "01060063",
  "Oldenburg in Holstein",  "01055035",
  "Pinneberg",              "01056039",
  "Ratekau",                "01055038",
  "Sankt Peter-Ording",     "01054116",
  "Scharbeutz",             "01055041",
  "Schleswig",              "01059075",
  "Schwentinental",         "01057050",
  "Stockelsdorf",           "01055044",
  "Sylt",                   "01054168",
  "Tornesch",               "01056047",
  "Wahlstedt",              "01060085",
  "Wedel",                  "01056050"
)

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
    if (grepl("Einzelbewerber|unabh.ngiger Bewerber|Einzelbewerberin", p, ignore.case = TRUE)) {
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
n_before <- nrow(sh_raw)
sh_raw <- sh_raw %>%
  filter(!(eligible_voters == 0 & (is.na(candidate_votes) | candidate_votes == 0)))
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
