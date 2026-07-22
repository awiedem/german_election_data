### County Council Seat Distributions (Sitzverteilungen), 2008-2022
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

# Flag rows where the source council size disagrees with the sum of the 9 party
# seat columns (5 rows: Groß-Gerau 2011, Heilbronn LK 2008, Donau-Ries 2020-22).
# These are discrepancies in the hand-compiled source, kept as-recorded, not
# altered. NA totals (blank in source) are not flagged as incongruent.
df <- df |>
  mutate(
    flag_seats_total_incongruent =
      !is.na(seats_total) &
      seats_total != rowSums(across(all_of(seat_cols)))
  ) |>
  relocate(flag_seats_total_incongruent, .after = seats_other)

# Guard: county_type recode should not introduce NAs
stopifnot(!any(is.na(df$county_type)))

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
