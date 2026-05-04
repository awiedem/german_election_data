## Triage helper for WBZ shapefile collection.
##
## Usage (from the project root):
##   source("code/wbz_collection/01_triage_helper.R")
##   triage_city("09162000")               # München
##   triage_city("09162000", open = FALSE) # don't auto-open browser
##
## What it does:
##   1. Looks up the city in data/wbz/contacts.csv.
##   2. Generates a list of candidate URLs (slug-based + state geoportal + govdata + Google).
##   3. HEAD-checks each candidate with a polite delay; skips ones that 404.
##   4. Opens the live ones in the browser (one tab each).
##   5. Appends every attempt to data/wbz/triage_log.csv with timestamp + status.
##
## After triage, the human edits data/wbz/contacts.csv directly (have_<year>,
## *_email, online_source, etc.) based on what they found in the browser.

set.seed(20260504)

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(httr)
  library(tibble)
})

POLITE_DELAY_S <- 1.5
HEAD_TIMEOUT_S <- 8
USER_AGENT <- "GERDA-WBZ-collection/0.1 (research; hannohilbig@gmail.com)"

CONTACTS_PATH <- "data/wbz/contacts.csv"
TRIAGE_LOG_PATH <- "data/wbz/triage_log.csv"

## ---------------------------------------------------------------------------
## Slug helpers

slugify <- function(name) {
  s <- tolower(name)
  s <- str_replace_all(s, c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss"))
  s <- str_replace_all(s, "[^a-z0-9 ]", "")
  s <- str_squish(s)
  s
}

## Generate plausible slugs for a city name. Returns a character vector;
## triage will try each. Different cities use different conventions.
city_slugs <- function(muni_name_short) {
  base <- slugify(muni_name_short)
  ## "Frankfurt am Main" -> "frankfurt-am-main", but also try just "frankfurt"
  hyphenated <- str_replace_all(base, " ", "-")
  first_word <- str_split_i(base, " ", 1)
  unique(c(hyphenated, first_word))
}

## ---------------------------------------------------------------------------
## Candidate URL generation

state_geoportals <- c(
  SH = "https://opendata.schleswig-holstein.de",
  HH = "https://geoportal-hamburg.de",
  NI = "https://geoportal.geodaten.niedersachsen.de",
  HB = "https://geoportal.bremen.de",
  NW = "https://www.geoportal.nrw",
  HE = "https://www.geoportal.hessen.de",
  RP = "https://www.geoportal.rlp.de",
  BW = "https://www.geoportal-bw.de",
  BY = "https://geoportal.bayern.de",
  SL = "https://geoportal.saarland.de",
  BE = "https://fbinter.stadt-berlin.de",
  BB = "https://geoportal.brandenburg.de",
  MV = "https://www.geoportal-mv.de",
  SN = "https://www.geoportal.sachsen.de",
  ST = "https://www.geodatenportal.sachsen-anhalt.de",
  TH = "https://www.geoportal-th.de"
)

candidate_urls <- function(muni_name_short, state) {
  slugs <- city_slugs(muni_name_short)
  ## Per-city open data + geoportal patterns.
  per_city <- unlist(lapply(slugs, function(s) {
    c(
      paste0("https://opendata.", s, ".de"),
      paste0("https://www.opendata.", s, ".de"),
      paste0("https://offenedaten-", s, ".de"),
      paste0("https://geoportal.", s, ".de"),
      paste0("https://www.", s, ".de/opendata"),
      paste0("https://www.", s, ".de/leben/stadtportraet/wahlen"),
      paste0("https://daten.", s, ".de")
    )
  }))
  ## State-level geoportal as a fallback discoverability anchor.
  state_url <- unname(state_geoportals[state])
  ## Federal portal + Google query (manual click-through).
  q <- URLencode(paste0(muni_name_short, " wahlbezirk shapefile"), reserved = TRUE)
  govdata <- paste0(
    "https://www.govdata.de/web/guest/suchen/-/results?q=",
    URLencode(paste0(muni_name_short, " wahlbezirk"), reserved = TRUE)
  )
  google <- paste0("https://www.google.com/search?q=", q)
  c(per_city, state_url, govdata, google)
}

## ---------------------------------------------------------------------------
## URL liveness check (HEAD; falls back to GET if HEAD blocked)

check_url <- function(url) {
  res <- tryCatch(
    HEAD(url, user_agent(USER_AGENT), timeout(HEAD_TIMEOUT_S)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) %in% c(405, 501)) {
    res <- tryCatch(
      GET(url, user_agent(USER_AGENT), timeout(HEAD_TIMEOUT_S),
          config = config(followlocation = TRUE, nobody = TRUE)),
      error = function(e) NULL
    )
  }
  if (is.null(res)) return(list(status = NA_integer_, ok = FALSE))
  list(status = status_code(res), ok = status_code(res) >= 200 && status_code(res) < 400)
}

## ---------------------------------------------------------------------------
## Triage log append

append_triage_log <- function(rows) {
  if (!file.exists(TRIAGE_LOG_PATH)) {
    write_csv(rows, TRIAGE_LOG_PATH, na = "")
  } else {
    write_csv(rows, TRIAGE_LOG_PATH, append = TRUE, na = "")
  }
}

## ---------------------------------------------------------------------------
## Main entry point

triage_city <- function(ags, open = TRUE, contacts_path = CONTACTS_PATH) {
  contacts <- read_csv(contacts_path, show_col_types = FALSE, na = "")
  row <- contacts |> filter(ags == !!ags)
  if (nrow(row) != 1) stop("AGS '", ags, "' not found in contacts.csv (", nrow(row), " matches)")

  cat(sprintf("\n=== %s — %s (%s, pop %s) ===\n",
              row$ags, row$muni_name_short, row$state,
              format(row$population, big.mark = ",")))

  urls <- candidate_urls(row$muni_name_short, row$state)
  cat("Checking", length(urls), "candidate URLs...\n\n")

  results <- list()
  for (u in urls) {
    chk <- check_url(u)
    flag <- if (isTRUE(chk$ok)) "OK " else "-- "
    cat(sprintf("  %s [%s] %s\n", flag,
                ifelse(is.na(chk$status), "ERR", chk$status), u))
    results[[length(results) + 1]] <- tibble(
      ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      ags = row$ags,
      muni_name = row$muni_name_short,
      url = u,
      http_status = chk$status,
      live = chk$ok
    )
    Sys.sleep(POLITE_DELAY_S)
  }

  log_rows <- bind_rows(results)
  append_triage_log(log_rows)

  if (open) {
    live_urls <- log_rows |> filter(live) |> pull(url)
    if (length(live_urls) == 0) {
      cat("\n(No live URLs to open — manual lookup needed.)\n")
    } else {
      cat("\nOpening", length(live_urls), "live URLs in browser...\n")
      for (u in live_urls) {
        utils::browseURL(u)
        Sys.sleep(0.3)
      }
    }
  }

  cat("\nNext steps for", row$muni_name_short, ":\n")
  cat("  - Inspect open tabs for WBZ / Wahlbezirk / Stimmbezirk shapefiles.\n")
  cat("  - Save downloads to data/wbz/shapefiles/", row$ags, "/<year>/\n", sep = "")
  cat("  - Edit data/wbz/contacts.csv: have_<year>, *_email, online_source.\n")
  invisible(log_rows)
}

## Convenience: triage by name (case-insensitive substring match).
triage_by_name <- function(name_substr, ...) {
  contacts <- read_csv(CONTACTS_PATH, show_col_types = FALSE, na = "")
  hits <- contacts |> filter(str_detect(tolower(muni_name_short), tolower(name_substr)))
  if (nrow(hits) == 0) stop("No city matching '", name_substr, "'")
  if (nrow(hits) > 1) {
    cat("Multiple matches — pick AGS explicitly:\n")
    print(hits |> select(ags, state, muni_name_short, population))
    return(invisible(hits))
  }
  triage_city(hits$ags, ...)
}
