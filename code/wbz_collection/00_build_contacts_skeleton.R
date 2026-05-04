## Build the kreisfreie-Stadt skeleton for WBZ shapefile collection.
## Input:  data/crosswalks/raw/31122023_Auszug_GV.xlsx (DESTATIS Gemeindeverzeichnis)
## Output: data/wbz/contacts.csv (one row per kreisfreie Stadt)

set.seed(20260504)

suppressMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tidyr)
})

gv_path <- "data/crosswalks/raw/31122023_Auszug_GV.xlsx"

raw <- read_excel(
  gv_path,
  sheet = "Onlineprodukt_Gemeinden31122023",
  col_types = "text",
  skip = 7,
  col_names = FALSE
)

## DESTATIS GV column conventions (after skip=7):
##   ...1  Satzart  (10=Bund, 20=Land, 30=RegBezirk, 40=Kreis, 50=VB, 60=Gemeinde)
##   ...2  Textkennzeichen  (for Satzart=40: 41=Kreisfreie Stadt, 42=Stadtkreis,
##                                          43=Kreis, 44=Landkreis, 45=Regionalverband)
##   ...3  Land   ...4 RB   ...5 Kreis   ...6 VB   ...7 Gemeinde
##   ...8  Name   ...9 Fläche   ...10 Einwohner

## Kreisfreie Städte / Stadtkreise = Satzart=40 entries with Textkennzeichen 41 or 42.
## For each, the corresponding muni-level row has the same Land+RB+Kreis and is the
## one we care about. Every kreisfreie Stadt has exactly one muni row (Bezirke in
## Berlin/Hamburg are aggregated into one row in the GV).

state_lookup <- c(
  "01" = "SH", "02" = "HH", "03" = "NI", "04" = "HB",
  "05" = "NW", "06" = "HE", "07" = "RP", "08" = "BW",
  "09" = "BY", "10" = "SL", "11" = "BE", "12" = "BB",
  "13" = "MV", "14" = "SN", "15" = "ST", "16" = "TH"
)

kfs <- raw |>
  filter(...1 == "60") |>
  transmute(
    land = ...3, rb = ...4, kreis = ...5, vb = ...6, gem = ...7,
    muni_name = ...8,
    population = suppressWarnings(as.integer(...10))
  ) |>
  ## inner-join to the kreis row so we keep only kreisfreie Städte
  inner_join(
    raw |>
      filter(...1 == "40", ...2 %in% c("41", "42")) |>
      transmute(land = ...3, rb = ...4, kreis = ...5, kreis_text = ...2),
    by = c("land", "rb", "kreis")
  ) |>
  mutate(
    ags = paste0(land, rb, kreis, gem),
    state = unname(state_lookup[land]),
    kreis_type = if_else(kreis_text == "41", "Kreisfreie Stadt", "Stadtkreis"),
    ## strip ", Stadt" / ", Landeshauptstadt" etc. for cleaner display
    muni_name_short = str_remove(muni_name, ",.*$")
  ) |>
  select(ags, state, muni_name, muni_name_short, kreis_type, population)

stopifnot(nchar(kfs$ags) == 8)

## Add the empty contact + status columns. Wide format.
contacts <- kfs |>
  mutate(
    geo_email = NA_character_,
    geo_dept_label = NA_character_,
    geo_url = NA_character_,
    wahl_email = NA_character_,
    wahl_url = NA_character_,
    opendata_email = NA_character_,
    opendata_url = NA_character_,
    have_2013 = FALSE,
    have_2017 = FALSE,
    have_2021 = FALSE,
    have_2025 = FALSE,
    online_source = NA_character_,
    online_format = NA_character_,
    online_license = NA_character_,
    notes = NA_character_
  ) |>
  arrange(state, ags)

cat("Total kreisfreie Städte / Stadtkreise:", nrow(contacts), "\n")
cat("By state:\n")
print(table(contacts$state))

## Sanity: spot-check a few iconic cities we expect.
expected <- c(
  "11000000" = "Berlin",
  "02000000" = "Hamburg",
  "04011000" = "Bremen",
  "04012000" = "Bremerhaven",
  "08111000" = "Stuttgart",
  "09162000" = "München",
  "05315000" = "Köln",
  "06412000" = "Frankfurt am Main"
)
missing <- setdiff(names(expected), contacts$ags)
if (length(missing) > 0) {
  warning("Missing expected AGS: ", paste(missing, collapse = ", "))
} else {
  cat("Spot-check passed: all", length(expected), "iconic cities present.\n")
}

dir.create("data/wbz", showWarnings = FALSE, recursive = TRUE)
write_csv(contacts, "data/wbz/contacts.csv", na = "")
cat("Wrote data/wbz/contacts.csv (", nrow(contacts), "rows,", ncol(contacts), "cols)\n")
