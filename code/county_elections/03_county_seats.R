### County Council Seat Distributions (Sitzverteilungen), 2008-2025
# Builds a county-year council-composition panel from a hand-compiled dataset
# of seat distributions in German county councils (Kreistage).
#
# Provenance:
#   - Hand-compiled dataset "Sitzverteilungen der Parteien 2008-2022" (v1.0.0),
#     added to the repo by coauthor Vincent Heddesheimer.
#   - Per-row source URLs are recorded in the `Quelle(n)` column (-> `source`).
#   - No upstream codebook exists for this file.
#
# Structure of the raw CSV (semicolon-separated, UTF-8 with BOM):
#   6 fixed cols: Kreisnummer (KNR), Kreisname, Regionale Bezeichnung, Jahr,
#                 Regierungspartei, Sitze gesamt
#   9 aggregate party cols: SPD, CDU, FDP, Bündnis 90/Die Grünen, Freie Wähler,
#                 Linke, AfD, Regional, Sonstige
#   ~45 "Sonstige: ..." detail cols decomposing the Sonstige aggregate (DROPPED)
#   3 trailing cols: Kommentar, Quelle(n), Zuletzt geprüft am
#   6000 data rows = 400 Kreise x 15 years (2008-2022), perfectly balanced.
#   Sitze gesamt = row-sum of the 9 aggregate party cols; blank cell = 0 seats.
#
# Vincent Heddesheimer / GERDA
# July 2026

set.seed(20260722)

rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(haschaR)
library(readxl)
library(rvest)
library(pdftools)
library(jsonlite)

conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)

# --- Read raw data ----------------------------------------------------------

raw_path <- here::here(
  "data/county_elections/raw/Kreistagswahlen",
  "Sitzverteilungen_der_Parteien_2008-2022_v1-0-0.csv"
)

# Read all cols as character for full control over coercion; locale(UTF-8)
# strips the BOM and preserves umlauts in party names.
raw <- read_delim(
  raw_path,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cat("Raw file read:", nrow(raw), "rows x", ncol(raw), "cols\n")

# Column-presence checks
fixed_cols <- c("Kreisnummer (KNR)", "Kreisname", "Regionale Bezeichnung",
                "Jahr", "Regierungspartei", "Sitze gesamt")
party_src_cols <- c("SPD", "CDU", "FDP", "Bündnis 90/Die Grünen",
                    "Freie Wähler", "Linke", "AfD", "Regional", "Sonstige")
trailing_cols <- c("Kommentar", "Quelle(n)", "Zuletzt geprüft am")

stopifnot(all(fixed_cols %in% names(raw)))
stopifnot(all(party_src_cols %in% names(raw)))
stopifnot(all(trailing_cols %in% names(raw)))

detail_cols <- names(raw)[str_detect(names(raw), "^Sonstige: ")]
cat("Detail 'Sonstige: ...' cols dropped:", length(detail_cols), "\n")

# --- Build published panel --------------------------------------------------

# Coerce a party seat cell to non-negative integer; blank/empty -> 0L
# (a blank party column means that party won zero seats).
to_seat_int <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "0"
  as.integer(x)
}

# Coerce the council-size cell (`Sitze gesamt`) to integer; blank/empty -> NA
# (a blank total is a MISSING value, not a zero-seat council). Nine Schwerin
# rows 2014-2022 have a blank total while their party columns sum to 45.
to_total_int <- function(x) {
  x <- as.character(x)
  x[x == ""] <- NA
  as.integer(x)
}

# new_name = source_name for the 9 aggregate party seat cols
seat_map <- c(
  seats_spd          = "SPD",
  seats_cdu_csu      = "CDU",
  seats_fdp          = "FDP",
  seats_gruene       = "Bündnis 90/Die Grünen",
  seats_freie_wahler = "Freie Wähler",
  seats_linke_pds    = "Linke",
  seats_afd          = "AfD",
  seats_regional     = "Regional",
  seats_other        = "Sonstige"
)

df <- raw |>
  transmute(
    county      = str_pad(as.character(`Kreisnummer (KNR)`), 5, "left", "0"),
    county_name = Kreisname,
    county_type = case_match(
      `Regionale Bezeichnung`,
      "1. Landkreis"        ~ "Landkreis",
      "2. Kreisfreie Stadt" ~ "kreisfreie Stadt",
      .default = NA_character_
    ),
    state       = substr(county, 1, 2),
    state_name  = haschaR::state_id_to_names(state),
    year        = as.integer(Jahr),
    government_party = Regierungspartei,
    seats_total = to_total_int(`Sitze gesamt`),
    seats_spd          = to_seat_int(SPD),
    seats_cdu_csu      = to_seat_int(CDU),
    seats_fdp          = to_seat_int(FDP),
    seats_gruene       = to_seat_int(`Bündnis 90/Die Grünen`),
    seats_freie_wahler = to_seat_int(`Freie Wähler`),
    seats_linke_pds    = to_seat_int(Linke),
    seats_afd          = to_seat_int(AfD),
    seats_regional     = to_seat_int(Regional),
    seats_other        = to_seat_int(Sonstige),
    comment      = Kommentar,
    source       = `Quelle(n)`,
    last_checked = lubridate::dmy(`Zuletzt geprüft am`)
  ) |>
  arrange(state, county, year)

seat_cols <- names(seat_map)  # the 9 party seat columns, published order

# seats_local_other combines the three non-major-party seat columns
# (seats_freie_wahler + seats_regional + seats_other). Unlike the individual
# three, this sum is comparable across all years and states: the historical
# hand-compiled rows and the newly parsed 2023-2025 rows split Freie Wähler /
# local voter groups / regional lists among those three columns under different
# conventions, but their total (= seats_total minus the six major parties) is
# defined identically everywhere. Use this column, not the three-way split, for
# cross-year comparisons of non-establishment seats.
#
# flag_seats_total_incongruent marks rows where the source council size
# disagrees with the sum of the 9 party seat columns (5 historical rows:
# Groß-Gerau 2011, Heilbronn LK 2008, Donau-Ries 2020-22). These are
# discrepancies in the hand-compiled source, kept as-recorded, not altered. NA
# totals (blank in source) are not flagged as incongruent.
df <- df |>
  mutate(
    seats_local_other =
      seats_freie_wahler + seats_regional + seats_other,
    flag_seats_total_incongruent =
      !is.na(seats_total) &
      seats_total != rowSums(across(all_of(seat_cols)))
  ) |>
  relocate(seats_local_other, flag_seats_total_incongruent, .after = seats_other)

# --- Extend the panel with the 2023 and 2024 elections ---------------------

# The hand-compiled input ends in 2022. Official result files are parsed below
# into election-event rows, then carried forward through 2025. The original
# 2008-2022 rows are never updated or reconstructed.

historical_df <- df
metadata_2022 <- historical_df |>
  filter(year == 2022) |>
  select(county, county_name, county_type, state, state_name)

official_check_date <- as.Date("2026-07-22")

# Source-coverage audit. This runs before parsing so missing acquisitions fail
# with a state-year label rather than inside a source-specific parser.
raw_root <- here::here("data/county_elections/raw/Kreistagswahlen")
source_coverage_audit <- tribble(
  ~state_year, ~format, ~geographic_identifier, ~seat_fields, ~available,
  "Schleswig-Holstein 2023", "XLSX", "official county order", "total and party seats",
  file.exists(file.path(raw_root, "Schleswig-Holstein", "Schleswig-Holstein_2023_Sitzverteilung.xlsx")),
  "Baden-Württemberg 2024", "PDF", "three-digit county suffix", "final seat table",
  file.exists(file.path(raw_root, "Baden-Württemberg", "Baden-Württemberg_2024_Kommunalwahlen.pdf")),
  "Brandenburg 2024", "HTML", "county number in URL", "elected candidates by party",
  length(list.files(file.path(raw_root, "Brandenburg", "2024_html"), pattern = "[.]html$")) == 18L,
  "Mecklenburg-Vorpommern 2024", "CSV", "official county number", "mandate columns",
  file.exists(file.path(raw_root, "Mecklenburg-Vorpommern", "Mecklenburg-Vorpommern_2024_Mandate.csv")),
  "Rheinland-Pfalz 2024", "PDF", "county heading", "2024 and 2019 party seats",
  file.exists(file.path(raw_root, "Rheinland-Pfalz", "Rheinland-Pfalz_2024_Ergebnisse_Kreisebene.pdf")),
  "Saarland 2024", "HTML", "county number in URL", "embedded seat chart",
  length(list.files(file.path(raw_root, "Saarland", "2024_html"), pattern = "[.]html$")) == 6L,
  "Sachsen 2024", "XLSX", "official area code", "final party seats",
  all(file.exists(file.path(raw_root, "Sachsen", c("Sachsen_2024_Sitzverteilung.xlsx",
    "Sachsen_2024_Gemeinderat_Sitzverteilung.xlsx")))),
  "Sachsen-Anhalt 2024", "CSV", "Schlüsselnummer", "total and party seats",
  file.exists(file.path(raw_root, "Sachsen-Anhalt", "Sachsen-Anhalt_2024_Ergebnisse_mit_Sitzen.csv")),
  "Thüringen 2024", "HTML", "county number in URL", "final party seat table",
  length(list.files(file.path(raw_root, "Thüringen", "2024_html"), pattern = "[.]html$")) == 22L,
  "Nordrhein-Westfalen 2025", "HTML", "county code in URL", "2025 seats total + per party",
  length(list.files(file.path(raw_root, "Nordrhein-Wetfalen", "2025_html"), pattern = "[.]shtml$")) == 53L
) |>
  mutate(status = if_else(available, "ready", "missing"))
if (any(!source_coverage_audit$available)) {
  stop("Missing official county-seat inputs: ",
       paste(source_coverage_audit$state_year[!source_coverage_audit$available],
             collapse = ", "))
}

zero_if_missing <- function(x) {
  x <- str_trim(as.character(x))
  x[is.na(x) | x == "" | x %in% c("x", "X", "-", "–", "—")] <- "0"
  out <- suppressWarnings(as.integer(str_replace_all(x, "[.]", "")))
  if (any(is.na(out))) stop("Non-numeric seat value encountered: ",
                            paste(unique(x[is.na(out)]), collapse = ", "))
  out
}

party_bucket <- function(party, local_groups = "other") {
  p <- str_to_upper(str_squish(as.character(party)))
  case_when(
    str_detect(p, "^SPD$|SOZIALDEMOKRAT") ~ "seats_spd",
    str_detect(p, "^CDU$|CHRISTLICH DEMOKRAT") ~ "seats_cdu_csu",
    str_detect(p, "^FDP$|FREIE DEMOKRAT") ~ "seats_fdp",
    str_detect(p, "GRÜNE|GRUENE|BÜNDNIS 90") ~ "seats_gruene",
    str_detect(p, "^DIE LINKE$|^LINKE$|^PDS$") ~ "seats_linke_pds",
    str_detect(p, "^AFD$|ALTERNATIVE FÜR DEUTSCHLAND") ~ "seats_afd",
    str_detect(p, "SSW") ~ "seats_regional",
    str_detect(p, "FREIE WÄHLER|FREIE WAEHLER|BVB / FREIE") ~
      "seats_freie_wahler",
    local_groups == "regional" &
      str_detect(p, "WÄHLERGRUP|WÄHLERVEREIN|^WG$|^WV$|WV INSGESAMT") ~
      "seats_regional",
    local_groups == "free" &
      str_detect(p, "WÄHLERGRUP|WÄHLERVEREIN|^WG$|^WV$|WV INSGESAMT") ~
      "seats_freie_wahler",
    TRUE ~ "seats_other"
  )
}

long_to_event <- function(x, local_groups = "other") {
  required <- c("county", "party", "seats", "seats_total", "source")
  stopifnot(all(required %in% names(x)))
  x |>
    mutate(bucket = party_bucket(party, local_groups),
           seats = zero_if_missing(seats)) |>
    group_by(county, seats_total, source, comment, last_checked, bucket) |>
    summarise(seats = sum(seats), .groups = "drop") |>
    tidyr::pivot_wider(names_from = bucket, values_from = seats,
                       values_fill = 0L) |>
    mutate(year = 2024L) |>
    select(county, year, seats_total, any_of(seat_cols),
           comment, source, last_checked)
}

complete_event_cols <- function(x) {
  for (nm in setdiff(seat_cols, names(x))) x[[nm]] <- 0L
  x |>
    mutate(
      county = str_pad(as.character(county), 5, "left", "0"),
      year = as.integer(year), seats_total = as.integer(seats_total),
      across(all_of(seat_cols), ~ replace_na(as.integer(.x), 0L)),
      comment = replace_na(as.character(comment), ""),
      source = as.character(source), last_checked = as.Date(last_checked)
    ) |>
    select(county, year, seats_total, all_of(seat_cols),
           comment, source, last_checked)
}

# Schleswig-Holstein 2023: final seat distribution for all 15 counties and
# county-equivalent cities.
parse_sh_2023 <- function() {
  path <- here::here("data/county_elections/raw/Kreistagswahlen",
                     "Schleswig-Holstein",
                     "Schleswig-Holstein_2023_Sitzverteilung.xlsx")
  x <- read_excel(path, sheet = "Tabelle1", skip = 4,
                  col_names = c("name", "total", "CDU", "GRÜNE", "SPD",
                                "FDP", "SSW", "AfD", "DIE LINKE", "WG",
                                "Übrige")) |>
    filter(!is.na(name), name != "Schleswig-Holstein",
           str_detect(as.character(total), "^[0-9]+$"))
  stopifnot(nrow(x) == 15L)
  codes <- metadata_2022 |> filter(state == "01") |> arrange(county) |> pull(county)
  tibble(
    county = codes, year = 2023L, seats_total = zero_if_missing(x$total),
    seats_spd = zero_if_missing(x$SPD), seats_cdu_csu = zero_if_missing(x$CDU),
    seats_fdp = zero_if_missing(x$FDP), seats_gruene = zero_if_missing(x$GRÜNE),
    seats_freie_wahler = 0L, seats_linke_pds = zero_if_missing(x$`DIE LINKE`),
    seats_afd = zero_if_missing(x$AfD), seats_regional = zero_if_missing(x$SSW),
    seats_other = zero_if_missing(x$WG) + zero_if_missing(x$Übrige),
    comment = "Official final 2023 seat distribution; WG and Übrige aggregated as other.",
    source = "https://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Schleswig-Holstein/Kommunalwahlen/2023/Endg%C3%BCltige_Ergebnisse/e_Sitze_nach-Kreisen-Kreisfreien-St%C3%A4dten_KMWSH-2023.xlsx",
    last_checked = official_check_date
  )
}

# Sachsen 2024: ten Landkreis councils plus the three city councils.
parse_sn_2024 <- function() {
  kt_path <- here::here("data/county_elections/raw/Kreistagswahlen", "Sachsen",
                        "Sachsen_2024_Sitzverteilung.xlsx")
  kt <- read_excel(kt_path, sheet = "KT24_Sitze", col_names = FALSE,
                   .name_repair = "minimal")
  lk <- kt[which(as.character(kt[[4]]) == "LK"), ]
  lk_event <- tibble(
    county = as.character(lk[[5]]), year = 2024L,
    seats_total = zero_if_missing(lk[[8]]),
    seats_spd = zero_if_missing(lk[[13]]),
    seats_cdu_csu = zero_if_missing(lk[[9]]),
    seats_fdp = zero_if_missing(lk[[14]]),
    seats_gruene = zero_if_missing(lk[[12]]),
    seats_freie_wahler = 0L,
    seats_linke_pds = zero_if_missing(lk[[11]]),
    seats_afd = zero_if_missing(lk[[10]]),
    seats_regional = 0L,
    seats_other = zero_if_missing(lk[[8]]) -
      rowSums(cbind(zero_if_missing(lk[[9]]), zero_if_missing(lk[[10]]),
                    zero_if_missing(lk[[11]]), zero_if_missing(lk[[12]]),
                    zero_if_missing(lk[[13]]), zero_if_missing(lk[[14]]))),
    comment = "Official final Kreistag seat distribution; local voter associations and minor parties aggregated as other.",
    source = "https://www.wahlen.sachsen.de/download/Kreistag/statistik-sachsen_KT24_EE_Sitze.xlsx",
    last_checked = official_check_date
  )

  city_path <- here::here("data/county_elections/raw/Kreistagswahlen", "Sachsen",
                          "Sachsen_2024_Gemeinderat_Sitzverteilung.xlsx")
  city <- read_excel(city_path, sheet = "GR24_EE_KS_GE", col_names = FALSE,
                     .name_repair = "minimal")
  city <- city[which(as.character(city[[5]]) == "KS" & !is.na(city[[6]])), ]
  city_event <- tibble(
    county = substr(as.character(city[[6]]), 1, 5), year = 2024L,
    seats_total = zero_if_missing(city[[9]]),
    seats_spd = zero_if_missing(city[[15]]),
    seats_cdu_csu = zero_if_missing(city[[11]]),
    seats_fdp = zero_if_missing(city[[16]]),
    seats_gruene = zero_if_missing(city[[14]]),
    seats_freie_wahler = 0L,
    seats_linke_pds = zero_if_missing(city[[13]]),
    seats_afd = zero_if_missing(city[[12]]),
    seats_regional = 0L,
    seats_other = zero_if_missing(city[[9]]) -
      rowSums(cbind(zero_if_missing(city[[11]]), zero_if_missing(city[[12]]),
                    zero_if_missing(city[[13]]), zero_if_missing(city[[14]]),
                    zero_if_missing(city[[15]]), zero_if_missing(city[[16]]))),
    comment = "Official final city-council seat distribution; minor parties and voter associations aggregated as other.",
    source = "https://www.wahlen.sachsen.de/download/Gemeinderat/statistik-sachsen_GR24_Sitze_SN_LK_KS_GE.xlsx",
    last_checked = official_check_date
  )
  bind_rows(lk_event, city_event)
}

parse_mv_2024 <- function() {
  path <- here::here("data/county_elections/raw/Kreistagswahlen",
                     "Mecklenburg-Vorpommern",
                     "Mecklenburg-Vorpommern_2024_Mandate.csv")
  x <- read_delim(path, delim = ";", skip = 5,
                  locale = locale(encoding = "ISO-8859-1"),
                  show_col_types = FALSE) |>
    filter(Mandate == "Ist", Kreis != 99)
  major <- c("CDU", "DIE LINKE", "SPD", "AfD", "GRÜNE", "FDP", "FREIE WÄHLER")
  tibble(
    county = paste0("13", str_pad(as.character(x$Kreis), 3, "left", "0")),
    year = 2024L, seats_total = zero_if_missing(x$Insgesamt),
    seats_spd = zero_if_missing(x$SPD), seats_cdu_csu = zero_if_missing(x$CDU),
    seats_fdp = zero_if_missing(x$FDP), seats_gruene = zero_if_missing(x$GRÜNE),
    seats_freie_wahler = zero_if_missing(x$`FREIE WÄHLER`),
    seats_linke_pds = zero_if_missing(x$`DIE LINKE`),
    seats_afd = zero_if_missing(x$AfD), seats_regional = 0L,
    seats_other = zero_if_missing(x$Insgesamt) -
      rowSums(as.data.frame(lapply(x[major], zero_if_missing))),
    comment = "Official final mandate file; minor and local lists aggregated as other.",
    source = "https://www.laiv-mv.de/static/LAIV/Wahlen/3-Kommunalwahlen/2024/Ergebnisse/k_mandate.csv",
    last_checked = official_check_date
  )
}

parse_st_2024 <- function() {
  path <- here::here("data/county_elections/raw/Kreistagswahlen",
                     "Sachsen-Anhalt",
                     "Sachsen-Anhalt_2024_Ergebnisse_mit_Sitzen.csv")
  x <- read_delim(path, delim = ";", locale = locale(encoding = "ISO-8859-1"),
                  show_col_types = FALSE) |>
    filter(Satzart == "KRS", str_trim(Wahllokal) == "")
  major <- c("S01 - CDU", "S02 - AfD", "S03 - DIE LINKE", "S04 - SPD",
             "S05 - FDP", "S06 - GRÜNE", "S07 - FREIE WÄHLER")
  tibble(
    county = as.character(x$Schlüsselnummer), year = 2024L,
    seats_total = zero_if_missing(x$`S - Sitze insgesamt`),
    seats_spd = zero_if_missing(x$`S04 - SPD`),
    seats_cdu_csu = zero_if_missing(x$`S01 - CDU`),
    seats_fdp = zero_if_missing(x$`S05 - FDP`),
    seats_gruene = zero_if_missing(x$`S06 - GRÜNE`),
    seats_freie_wahler = zero_if_missing(x$`S07 - FREIE WÄHLER`),
    seats_linke_pds = zero_if_missing(x$`S03 - DIE LINKE`),
    seats_afd = zero_if_missing(x$`S02 - AfD`), seats_regional = 0L,
    seats_other = zero_if_missing(x$`S - Sitze insgesamt`) -
      rowSums(as.data.frame(lapply(x[major], zero_if_missing))),
    comment = "Official final results including the Coswig repeat election; minor and local lists aggregated as other.",
    source = "https://wahlergebnisse.sachsen-anhalt.de/wahlen/kw24/erg/csv/kw24dat2.csv",
    last_checked = official_check_date
  )
}

parse_th_2024 <- function() {
  paths <- list.files(here::here("data/county_elections/raw/Kreistagswahlen",
                                 "Thüringen", "2024_html"),
                      pattern = "[.]html$", full.names = TRUE)
  map_dfr(paths, function(path) {
    code3 <- str_match(basename(path), "([0-9]{3})")[, 2]
    tables <- read_html(path, encoding = "Windows-1252") |> html_table(fill = TRUE)
    total_tab <- tables[[2]]
    total <- zero_if_missing(total_tab[[2]][str_detect(total_tab[[1]], "Zu vergebene Sitze")])
    # The official HTML omits many closing cell tags, which causes generic
    # table parsers to concatenate later rows. Split the cached source at each
    # raw table row and read the first three <nobr> values: party, votes, seats.
    html <- read_file(path, locale = locale(encoding = "Windows-1252"))
    rows <- str_split(html, "<tr>")[[1]]
    rows <- rows[str_detect(rows, "<td title=")]
    party_values <- str_match(rows, "<td title='[^']*'[^>]*><nobr>([^<]+)")[, 2]
    cells <- map(rows, ~ str_split(.x, "<td")[[1]])
    clean_cell <- function(x) {
      x |> str_remove("^[^>]*>") |> str_remove_all("<[^>]+>") |>
        str_replace_all("&nbsp;", "") |> str_squish()
    }
    parties <- tibble(
      party = party_values,
      seats = map_chr(cells, ~ clean_cell(.x[[5]]))
    )
    long_to_event(tibble(
      county = paste0("16", code3), party = parties$party,
      seats = parties$seats, seats_total = total,
      comment = "Official final county result page cached as HTML.",
      source = paste0("https://wahlen.thueringen.de/datenbank/wahl1/wahl.asp?wJahr=2024&wahlart=KW&wknr=",
                      code3, "&zeigeErg=WK"), last_checked = official_check_date
    ))
  })
}

parse_sl_2024 <- function() {
  paths <- list.files(here::here("data/county_elections/raw/Kreistagswahlen",
                                 "Saarland", "2024_html"),
                      pattern = "[.]html$", full.names = TRUE)
  map_dfr(paths, function(path) {
    code2 <- str_match(basename(path), "([0-9]{2})")[, 2]
    doc <- read_html(path)
    node <- html_element(doc, "[data-chartoptions*='sitz']")
    chart <- fromJSON(html_attr(node, "data-chartdata"))$dataSets
    total <- sum(as.integer(chart$value))
    long_to_event(tibble(
      county = paste0("10", str_pad(code2, 3, "left", "0")), party = chart$label,
      seats = chart$value, seats_total = total,
      comment = "Official final seat chart cached as HTML.",
      source = paste0("https://wahlergebnis.saarland.de/KTW/ergebnisse_kreis_",
                      code2, ".html"), last_checked = official_check_date
    ))
  })
}

parse_bb_2024 <- function() {
  paths <- list.files(here::here("data/county_elections/raw/Kreistagswahlen",
                                 "Brandenburg", "2024_html"),
                      pattern = "[.]html$", full.names = TRUE)
  map_dfr(paths, function(path) {
    code2 <- str_match(basename(path), "([0-9]{2})")[, 2]
    elected <- html_table(read_html(path), fill = TRUE)[[1]]
    seats <- elected |> count(Partei, name = "seats")
    long_to_event(tibble(
      county = paste0("12", str_pad(code2, 3, "left", "0")),
      party = seats$Partei, seats = seats$seats, seats_total = nrow(elected),
      comment = "Official final elected-candidates table cached as HTML; Cottbus includes the September repeat election.",
      source = paste0("https://wahlergebnisse.brandenburg.de/12/200/20240609/kreistagswahl_land/ergebnisse_kreis_",
                      code2, ".html"), last_checked = official_check_date
    ))
  })
}

# Baden-Württemberg publishes the final report as facing pages: the left page
# identifies the county, while the right page reports the seat distribution.
# The PDF's fixed x coordinates make it possible to read the official table
# without reconstructing seats from votes.
parse_bw_2024 <- function() {
  path <- here::here("data/county_elections/raw/Kreistagswahlen",
                     "Baden-Württemberg", "Baden-Württemberg_2024_Kommunalwahlen.pdf")
  pages <- pdf_data(path)
  extract_pages <- function(page_ids) {
    map_dfr(page_ids, function(page_id) {
      z <- pages[[page_id]]
      code_rows <- z |> filter(x >= 530, str_detect(text, "^[0-9]{3}$"))
      map_dfr(seq_len(nrow(code_rows)), function(i) {
        yy <- code_rows$y[i]
        row <- z |> filter(abs(y - yy) <= 1)
        at_x <- function(lo, hi) {
          v <- row |> filter(x >= lo, x < hi) |> pull(text)
          if (length(v) == 0) "0" else v[[1]]
        }
        is_county_table <- page_id >= 29
        x_ranges <- if (is_county_table) {
          list(total = c(235, 255), cdu = c(265, 290), spd = c(295, 320),
               gruene = c(325, 350), fdp = c(355, 380), afd = c(385, 410),
               linke = c(415, 440), other = c(445, 470), joint = c(480, 505),
               wv = c(510, 530))
        } else {
          list(total = c(205, 235), cdu = c(292, 322), spd = c(322, 350),
               gruene = c(350, 378), fdp = c(378, 405), afd = c(405, 431),
               linke = c(431, 457), other = c(457, 486), joint = c(486, 514),
               wv = c(514, 532))
        }
        value_at <- function(name) at_x(x_ranges[[name]][1], x_ranges[[name]][2])
        tibble(
          county = paste0("08", code_rows$text[i]), page = page_id,
          seats_total = zero_if_missing(value_at("total")),
          seats_cdu_csu = zero_if_missing(value_at("cdu")),
          seats_spd = zero_if_missing(value_at("spd")),
          seats_gruene = zero_if_missing(value_at("gruene")),
          seats_fdp = zero_if_missing(value_at("fdp")),
          seats_afd = zero_if_missing(value_at("afd")),
          seats_linke_pds = zero_if_missing(value_at("linke")),
          seats_other_party = zero_if_missing(value_at("other")),
          seats_joint = zero_if_missing(value_at("joint")),
          seats_wv = zero_if_missing(value_at("wv"))
        )
      })
    })
  }
  city <- extract_pages(seq(13, 25, 2)) |>
    filter(county %in% metadata_2022$county[metadata_2022$state == "08" &
                                            metadata_2022$county_type == "kreisfreie Stadt"])
  county <- extract_pages(seq(29, 39, 2)) |>
    filter(county %in% metadata_2022$county[metadata_2022$state == "08" &
                                            metadata_2022$county_type == "Landkreis"])
  bind_rows(city, county) |>
    transmute(
      county, year = 2024L, seats_total,
      seats_spd, seats_cdu_csu, seats_fdp, seats_gruene,
      seats_freie_wahler = seats_wv, seats_linke_pds, seats_afd,
      seats_regional = 0L, seats_other = seats_other_party + seats_joint,
      comment = "Official final report; Wählervereinigungen follow the existing Baden-Württemberg Freie-Wähler convention.",
      source = "https://www.statistik-bw.de/fileadmin/user_upload/Service/Veroeff/Statistische_Berichte/425224001_.pdf",
      last_checked = official_check_date
    )
}

parse_rp_2024 <- function(return_2019 = FALSE) {
  path <- here::here("data/county_elections/raw/Kreistagswahlen",
                     "Rheinland-Pfalz", "Rheinland-Pfalz_2024_Ergebnisse_Kreisebene.pdf")
  lines <- str_split(paste(pdf_text(path), collapse = "\n"), "\n")[[1]]
  lines <- str_squish(lines)
  aliases <- metadata_2022 |> filter(state == "07") |>
    transmute(county, alias = str_remove(county_name, ", kreisfreie Stadt$"))
  aliases$alias[aliases$county == "07132"] <- "Altenkirchen"
  parse_seat <- function(line, year_index) {
    # The PDF occasionally joins the 2024 seat count to a dash denoting that
    # the party did not contest in 2019 (for example, "4- 4").
    line <- str_replace_all(line, "([0-9])-(?=\\s)", "\\1 -")
    tokens <- str_split(str_squish(line), "\\s+")[[1]]
    tail3 <- tail(tokens, 3)
    if (!all(str_detect(tail3, "^(-|[-0-9]+)$"))) {
      # A party that did not run in 2019 may be printed with only its 2024
      # seat count at line end (for example, Volt in Mainz-Bingen).
      return(if (year_index == 1L) zero_if_missing(tail(tokens, 1)) else 0L)
    }
    zero_if_missing(tail3[[year_index]])
  }
  map_dfr(seq_len(nrow(aliases)), function(i) {
    hit <- which(lines == aliases$alias[i])
    hit <- hit[hit > 50][1]
    if (is.na(hit)) stop("Rheinland-Pfalz county heading not found: ", aliases$alias[i])
    next_hits <- which(lines[(hit + 1):length(lines)] %in% aliases$alias)
    next_heading <- if (length(next_hits)) hit + next_hits[1] - 1 else length(lines)
    # Detailed result blocks are short. Cap the block so the parser cannot
    # drift into the statewide summary table that follows a page break.
    end <- min(next_heading, hit + 25, length(lines))
    block <- lines[hit:end]
    total_line <- block[str_detect(block, "^Gültige Stimmen")][1]
    total_index <- which(str_detect(block, "^Gültige Stimmen"))[1]
    party_lines <- block[seq(total_index + 1, length(block))]
    stop_index <- which(str_detect(party_lines,
                                   "^darunter|^dar\\.|^da\\.|^Kommunalwahlen|^©"))[1]
    if (!is.na(stop_index)) party_lines <- head(party_lines, stop_index - 1)
    normalized_party_lines <- str_replace_all(
      party_lines, "([0-9])-(?=\\s)", "\\1 -"
    )
    party_lines <- party_lines[str_detect(
      normalized_party_lines,
      "\\s(-|[0-9]+)\\s+(-|[0-9]+)\\s+-?[0-9]+$|\\s[0-9]+$"
    )]
    year_index <- if (return_2019) 2L else 1L
    long_to_event(tibble(
      county = aliases$county[i],
      party = str_match(party_lines, "^(.*?)(?=\\s(?:[0-9]|-))")[, 2],
      seats = map_int(party_lines, parse_seat, year_index = year_index),
      seats_total = parse_seat(total_line, year_index),
      comment = "Official final result report; Wählergruppen follow the existing Rheinland-Pfalz regional-list convention.",
      source = "https://www.wahlen.rlp.de/fileadmin/wahlen.rlp.de/KW/KW_2024_Ergebisse_Kreisebene.pdf",
      last_checked = official_check_date
    ), local_groups = "regional") |>
      mutate(year = if (return_2019) 2019L else 2024L)
  })
}

# Nordrhein-Westfalen 2025 Kreistags-/Ratswahl (14 Sept 2025). One official
# results page per county at wahlergebnisse.nrw, cached as HTML; each has a
# single clean #mainErgTable. The council size is the "Gültige Stimmen / Vertr.
# insgesamt" row (col 7 = 2025 seats); every party/list row after it is bucketed
# by party_bucket() with the default local_groups = "other", so the generic
# "Wählergruppe N" lists fall to seats_other and the FREIE WÄHLER party keeps its
# own column. County identity comes from the URL/filename code (no code on page).
parse_nrw_2025 <- function() {
  paths <- list.files(here::here("data/county_elections/raw/Kreistagswahlen",
                                 "Nordrhein-Wetfalen", "2025_html"),
                      pattern = "[.]shtml$", full.names = TRUE)
  map_dfr(paths, function(path) {
    code3 <- str_match(basename(path), "a([0-9]{3})000")[, 2]
    tab <- read_html(path) |>
      html_element("#mainErgTable") |>
      html_table(fill = TRUE, header = FALSE)
    total_row <- which(grepl("Vertr\\. insgesamt", tab[[1]]))
    stopifnot(length(total_row) == 1L)
    total <- as.integer(str_trim(tab[[7]][total_row]))
    party <- tab[-seq_len(total_row), ]
    party <- party[!is.na(party[[1]]) & str_trim(party[[1]]) != "" &
                     !grepl("^Download", party[[1]]), ]
    seats <- suppressWarnings(as.integer(str_trim(party[[7]])))
    seats[is.na(seats)] <- 0L  # "—" / "X" / blank -> zero seats
    long_to_event(tibble(
      county = paste0("05", code3),
      party = party[[1]],
      seats = seats,
      seats_total = total,
      comment = "Official final 2025 Kreistags-/Ratswahl result page cached as HTML.",
      source = paste0("https://www.wahlergebnisse.nrw/kommunalwahlen/2025/aktuell/a",
                      code3, "000kw2500.shtml"),
      last_checked = official_check_date
    )) |>
      mutate(year = 2025L)
  })
}

event_rows <- bind_rows(
  parse_sh_2023(), parse_bw_2024(), parse_bb_2024(), parse_mv_2024(),
  parse_rp_2024(), parse_sl_2024(), parse_sn_2024(), parse_st_2024(),
  parse_th_2024(), parse_nrw_2025()
) |>
  complete_event_cols() |>
  arrange(county, year)

if (anyDuplicated(event_rows[c("county", "year")])) {
  duplicate_keys <- event_rows |>
    group_by(county, year) |>
    summarise(n = n(), sources = paste(unique(source), collapse = " | "),
              .groups = "drop") |>
    filter(n > 1) |>
    transmute(key = paste0(county, "-", year, " (", n, "): ", sources)) |>
    pull(key)
  stop("Duplicate county-year keys in new election-event rows: ",
       paste(duplicate_keys, collapse = ", "))
}
unknown_counties <- setdiff(event_rows$county, metadata_2022$county)
if (length(unknown_counties)) {
  stop("Unknown county codes in new event rows: ", paste(unknown_counties, collapse = ", "))
}
event_total_mismatch <- event_rows |>
  filter(!is.na(seats_total),
         seats_total != rowSums(across(all_of(seat_cols))))
if (nrow(event_total_mismatch)) {
  stop("New official event rows have incongruent seat totals: ",
       paste(event_total_mismatch$county, collapse = ", "))
}
expected_event_counts <- tibble(
  state = c("01", "07", "08", "10", "12", "13", "14", "15", "16", "05"),
  year = c(2023L, rep(2024L, 8), 2025L),
  expected_n = c(15L, 36L, 44L, 6L, 18L, 8L, 13L, 14L, 22L, 53L)
)
observed_event_counts <- event_rows |>
  mutate(state = substr(county, 1, 2)) |>
  count(state, year, name = "observed_n")
event_count_audit <- full_join(expected_event_counts, observed_event_counts,
                               by = c("state", "year"))
if (any(is.na(event_count_audit$expected_n)) ||
    any(is.na(event_count_audit$observed_n)) ||
    any(event_count_audit$expected_n != event_count_audit$observed_n)) {
  stop("Official election-event coverage counts differ from the approved scope")
}

# Overlap cross-check: the Rheinland-Pfalz official report contains 2019 seats
# alongside 2024. Compare those reconstructed 2019 vectors to the untouched
# hand-compiled panel and stop if the documented discrepancy set changes.
rp_2019 <- parse_rp_2024(return_2019 = TRUE) |> complete_event_cols()
rp_overlap <- historical_df |> filter(year == 2019, state == "07") |>
  select(county, all_of(c("seats_total", seat_cols))) |>
  inner_join(rp_2019 |> select(county, all_of(c("seats_total", seat_cols))),
             by = "county", suffix = c("_old", "_official"))
overlap_fields <- c("seats_total", seat_cols)
overlap_disagree <- map_lgl(seq_len(nrow(rp_overlap)), function(i) {
  any(map_lgl(overlap_fields, function(nm) {
    !identical(rp_overlap[[paste0(nm, "_old")]][i],
               rp_overlap[[paste0(nm, "_official")]][i])
  }))
})
known_rp_overlap_disagreements <- c("07140", "07235", "07335")
observed_rp_overlap_disagreements <- rp_overlap$county[overlap_disagree]
unexpected_rp_overlap_disagreements <- setdiff(
  observed_rp_overlap_disagreements, known_rp_overlap_disagreements
)
missing_known_rp_disagreements <- setdiff(
  known_rp_overlap_disagreements, observed_rp_overlap_disagreements
)
if (length(unexpected_rp_overlap_disagreements) ||
    length(missing_known_rp_disagreements)) {
  stop("Rheinland-Pfalz overlap discrepancy set changed. Observed: ",
       paste(observed_rp_overlap_disagreements, collapse = ", "))
}
if (length(observed_rp_overlap_disagreements)) {
  cat("Known Rheinland-Pfalz 2019 overlap discrepancies retained unchanged:",
      paste(observed_rp_overlap_disagreements, collapse = ", "), "\n")
}

# Create new annual rows from the 2022 composition, then overlay the election
# event from its election year onward. The governing party is carried forward
# from 2022 while the council is unchanged (non-electing states keep it through
# 2025); once a new election overlays a county, the governing party becomes NA
# because none of the seat-result sources identifies it for the new council.
future_df <- crossing(county = metadata_2022$county, year = 2023:2025) |>
  left_join(metadata_2022, by = "county") |>
  left_join(historical_df |> filter(year == 2022) |>
              select(county, seats_total, all_of(seat_cols), government_party,
                     comment, source, last_checked), by = "county")

for (event_year in sort(unique(event_rows$year))) {
  updates <- event_rows |> filter(year == event_year) |> select(-year)
  idx <- future_df$year >= event_year & future_df$county %in% updates$county
  match_idx <- match(future_df$county[idx], updates$county)
  update_cols <- c("seats_total", seat_cols, "comment", "source", "last_checked")
  for (nm in update_cols) future_df[[nm]][idx] <- updates[[nm]][match_idx]
  # A new election forms a new council; the seat sources do not report its
  # governing party, so drop the carried-forward value from the event year on.
  future_df$government_party[idx] <- NA_character_
}

future_df <- future_df |>
  mutate(
    seats_local_other = seats_freie_wahler + seats_regional + seats_other,
    flag_seats_total_incongruent = !is.na(seats_total) &
      seats_total != rowSums(across(all_of(seat_cols)))
  ) |>
  select(all_of(names(historical_df)))

df <- bind_rows(historical_df, future_df) |>
  arrange(state, county, year)

# Acceptance guards run before either public artifact is overwritten.
key_counts <- df |> count(county, year)
year_counts <- df |> count(year)
all_seat_values <- unlist(df[c("seats_total", seat_cols)])
stopifnot(
  !any(is.na(df$county_type)),
  nrow(df) == 7200L,
  n_distinct(df$county) == 400L,
  identical(sort(unique(df$year)), 2008:2025),
  all(key_counts$n == 1L),
  all(year_counts$n == 400L),
  all(str_detect(df$county, "^[0-9]{5}$")),
  is.integer(df$year),
  all(vapply(df[c("seats_total", seat_cols)], is.integer, logical(1))),
  !any(all_seat_values < 0, na.rm = TRUE),
  # seats_local_other is exactly the sum of the three non-major-party columns,
  # is a non-negative integer, and equals seats_total minus the six major
  # parties wherever the total is congruent.
  is.integer(df$seats_local_other),
  all(df$seats_local_other ==
        df$seats_freie_wahler + df$seats_regional + df$seats_other),
  !any(df$seats_local_other < 0),
  identical(names(df), names(historical_df))
)

# Verify carry-forward timing mechanically against each parsed election row.
for (i in seq_len(nrow(event_rows))) {
  event <- event_rows[i, ]
  panel_rows <- df |>
    filter(county == event$county, year >= event$year)
  stopifnot(nrow(panel_rows) == 2025L - event$year + 1L)
  for (nm in c("seats_total", seat_cols)) {
    stopifnot(all(panel_rows[[nm]] == event[[nm]], na.rm = FALSE))
  }
}
non_event_counties <- setdiff(metadata_2022$county, event_rows$county)
non_event_panel <- df |> filter(county %in% non_event_counties, year >= 2023)
baseline_2022 <- historical_df |> filter(year == 2022) |>
  select(county, all_of(c("seats_total", seat_cols)))
non_event_check <- non_event_panel |>
  select(county, year, all_of(c("seats_total", seat_cols))) |>
  left_join(baseline_2022, by = "county", suffix = c("_future", "_2022"))
for (nm in c("seats_total", seat_cols)) {
  stopifnot(identical(non_event_check[[paste0(nm, "_future")]],
                      non_event_check[[paste0(nm, "_2022")]]))
}

cat("Panel built:", nrow(df), "rows x", ncol(df), "cols\n")

# --- Write outputs ----------------------------------------------------------

out_rds <- here::here("data/county_elections/final/county_council_seats.rds")
out_csv <- here::here("data/county_elections/final/county_council_seats.csv")

write_rds(df, out_rds)
fwrite(df, out_csv)
cat("Written:\n  ", out_rds, "\n  ", out_csv, "\n")

# --- Validation -------------------------------------------------------------

cat("\n===== VALIDATION =====\n")

## (c1) Key uniqueness & balance
c1 <- df |> count(county, year)
cat("\n(c1) county x year key\n")
cat("  rows:", nrow(df), "| distinct counties:", n_distinct(df$county),
    "| distinct years:", n_distinct(df$year), "\n")
cat("  all (county,year) n==1:", all(c1$n == 1), "\n")
yr_counts <- df |> count(year)
cat("  every year has 400 rows:", all(yr_counts$n == 400), "\n")

## (c2) seats_total vs rowSums of 9 seat cols (NA-aware)
row_sum <- rowSums(as.matrix(df[seat_cols]))
n_total_na <- sum(is.na(df$seats_total))            # blank council size in source
mismatch <- which(!is.na(df$seats_total) & row_sum != df$seats_total)
cat("\n(c2) seats_total vs rowSums(9 party cols)\n")
cat("  rows with NA seats_total (blank in source):", n_total_na, "\n")
cat("  genuine incongruent rows (total != sum):", length(mismatch), "\n")
cat("  flag_seats_total_incongruent count:",
    sum(df$flag_seats_total_incongruent), "\n")
if (length(mismatch) > 0) {
  print(df[mismatch, c("county", "county_name", "year",
                       "seats_total", seat_cols)])
}

## (c3) non-negative integers; range of seats_total
all_seat_vals <- unlist(df[c("seats_total", seat_cols)])
cat("\n(c3) seat columns non-negative integers\n")
cat("  any negative:", any(all_seat_vals < 0, na.rm = TRUE), "\n")
cat("  all integer type:",
    all(vapply(df[c("seats_total", seat_cols)], is.integer, logical(1))), "\n")
cat("  seats_total range (non-NA):",
    min(df$seats_total, na.rm = TRUE), "-",
    max(df$seats_total, na.rm = TRUE), "\n")

## (c4) government_party value table
cat("\n(c4) government_party distinct values + counts\n")
gp_tab <- df |> count(government_party, sort = TRUE)
print(gp_tab, n = nrow(gp_tab))

## (c5) join rate against county_elec_unharm
cat("\n(c5) join rate vs county_elec_unharm$county\n")
unharm_path <- here::here("data/county_elections/final/county_elec_unharm.rds")
unharm_counties <- character(0)
if (file.exists(unharm_path)) {
  first_line <- readLines(unharm_path, n = 1, warn = FALSE)
  is_lfs_stub <- (file.size(unharm_path) < 1000) ||
    any(grepl("git-lfs", first_line))
  if (is_lfs_stub) {
    cat("  county_elec_unharm.rds appears to be a git-lfs stub; pulling...\n")
    system2("git", c("lfs", "pull", "--include",
                     "data/county_elections/final/county_elec_unharm.rds"))
  }
  unharm <- read_rds(unharm_path)
  unharm_counties <- unique(unharm$county)
} else {
  cat("  WARNING: county_elec_unharm.rds not found\n")
}
in_unharm <- df$county %in% unharm_counties
cat("  match rate of 400 counties in unharm:",
    sprintf("%.3f", mean(unique(df$county) %in% unharm_counties)),
    sprintf("(%d / %d)", sum(unique(df$county) %in% unharm_counties),
            n_distinct(df$county)), "\n")
unmatched <- df |>
  filter(!county %in% unharm_counties) |>
  distinct(county, county_name, state_name)
cat("  unmatched counties:", nrow(unmatched), "\n")
if (nrow(unmatched) > 0) print(head(unmatched, 15))

## (c6) composition constancy: distinct 9-seat-vectors per county
cat("\n(c6) distinct seat-vectors per county\n")
vec_per_county <- df |>
  group_by(county) |>
  summarise(n_vecs = n_distinct(across(all_of(seat_cols))), .groups = "drop")
cat("  distribution of distinct seat-vectors per county:\n")
print(vec_per_county |> count(n_vecs, name = "n_counties"))
flag_c6 <- vec_per_county |> filter(n_vecs > 4)
cat("  counties with >4 distinct seat-vectors:", nrow(flag_c6), "\n")
if (nrow(flag_c6) > 0) {
  print(df |> filter(county %in% flag_c6$county) |>
          distinct(county, county_name, state_name) )
}

## (d1) external spot-check: Flensburg 2008
cat("\n(d1) Flensburg 2008 (county 01001) spot-check\n")
flens <- df |> filter(county == "01001", year == 2008)
print(as.data.frame(flens[, c("county", "county_name", "year", "seats_total",
                              seat_cols)]))
# Flensburg's 19 non-establishment seats are the SSW, which the source files
# under the `Regional` column (SSW = regional party), not `Sonstige`.
expected <- c(seats_total = 43, seats_spd = 7, seats_cdu_csu = 9,
              seats_fdp = 2, seats_gruene = 3, seats_linke_pds = 3,
              seats_freie_wahler = 0, seats_afd = 0, seats_regional = 19,
              seats_other = 0)
got <- unlist(flens[1, names(expected)])
cat("  spot-check matches expected:", all(got == expected), "\n")

cat("\n===== FINAL COLUMN LIST =====\n")
cat(paste(names(df), collapse = ", "), "\n")

cat("\n===== DONE =====\n")
