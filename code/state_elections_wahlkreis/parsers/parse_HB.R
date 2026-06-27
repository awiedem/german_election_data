# =============================================================================
# parse_HB.R
# Stage-1 CLEANING parser: Bremen (HB) Buergerschaftswahl (Landtag),
# constituency level = WAHLBEREICH (Stadt Bremen 04011000, Stadt Bremerhaven
# 04012000).
#
# Output: one LONG, tidy CSV row per (Wahlbereich x stimme x party).
#
# SOURCE FILES (machine-readable only; PDFs/TIFs deferred to OCR stage):
#   votemanager OpenData CSV, Gemeinde level (= whole Wahlbereich), "ebene3":
#     HB_2015_Buergerschaftswahl_Bremen_ebene3.csv        (Stadt Bremen)
#     HB_2015_Buergerschaftswahl_Bremerhaven_ebene3.csv   (Stadt Bremerhaven)
#     HB_2019_Buergerschaftswahl_Bremen_ebene3.csv
#     HB_2019_Buergerschaftswahl_Bremerhaven_ebene3.csv
#
#   EXCLUDED: HB_2023_..._data.js (InstantAtlas) carries ONLY vote SHARES (%),
#   no absolute votes / no valid_votes -> cannot satisfy the integrity check,
#   so 2023 is excluded per the clean-only mandate. All other files are PDF/TIF
#   scans (deferred) or finer geo levels (ebene6/8/11 = Wahlbezirk/Stadtteil/
#   Ortsteil, below the Wahlbereich -> not the constituency unit).
#
# VOTE SYSTEM: Bremen is a city-state with a 5-vote proportional system since
#   2011. A voter distributes 5 votes among a party LIST and/or that party's
#   individual candidates. The party's TOTAL Stimmen (list + all its candidates)
#   determines seats -> this is the proportional "list" result. There is only
#   ONE ballot type (no separate Erst-/Zweitstimme). Per task spec it is mapped
#   to stimme = "zweitstimme" (the proportional list result).
#
# votemanager column legend (verified empirically against the data):
#   A  = Wahlberechtigte (eligible_voters)
#   B  = Waehler          (number_voters)
#   C  = ungueltige Stimmzettel (invalid ballots -> invalid_votes)
#   D1 = gueltige Stimmzettel (valid ballots; NOT used as denominator here)
#   D2 = gueltige STIMMEN  (valid_votes; = sum of all party totals)  <-- denom
#   For each party block n: Dn_SUMME_LISTE_KANDIDATEN = party total votes
#     (= Dn_LISTE + Dn_SUMME_KANDIDATEN). Verified: sum_n = D2 exactly.
#
# PARTY-NAME LEGEND: the OpenData CSVs ship D-codes only (no names). The D-code
#   -> party mapping below was recovered per-year by matching each D-code's
#   STATEWIDE total (Bremen + Bremerhaven) to the official/Wikipedia statewide
#   party totals. Every code matched a published total EXACTLY (0 residual,
#   statewide sums identical). The mapping is PER YEAR -- ballot order differs
#   between 2015 and 2019 (e.g. D2 = Gruene in 2015 but CDU in 2019).
# =============================================================================

library(here)
library(tidyverse)
library(data.table)

here::i_am("code/state_elections_wahlkreis/parsers/parse_HB.R")

raw_dir <- here("data", "state_elections", "raw",
                "Landtagswahlen_Wahlkreis", "Bremen")
out_csv <- here("data", "state_elections", "processed", "wahlkreis",
                "HB_ltw_wkr_long.csv")

# --- verified per-year D-code -> party_raw legend -----------------------------
# (matched to official statewide totals; see header)
legend_2015 <- c(
  D1 = "SPD", D2 = "GRÜNE", D3 = "CDU", D4 = "DIE LINKE", D5 = "BIW",
  D6 = "FDP", D7 = "Piraten", D8 = "NPD", D9 = "AfD", D10 = "DIE PARTEI",
  D11 = "Tierschutzpartei"
)
legend_2019 <- c(
  D1 = "SPD", D2 = "CDU", D3 = "GRÜNE", D4 = "DIE LINKE", D5 = "FDP",
  D6 = "AfD", D7 = "BIW", D8 = "DIE PARTEI", D9 = "Piraten",
  D10 = "Bündnis Grundeinkommen", D11 = "Die Rechte", D12 = "Freie Wähler",
  D13 = "Menschliche Welt", D14 = "Partei der Humanisten", D15 = "V-Partei³",
  D16 = "Willkommen in der Realität (WIR)"
)

# wkr_nr: stable per Wahlbereich, character with leading zeros.
# Bremen = "01", Bremerhaven = "02" (matches the AGS city suffix).
wkr_nr_for_ags <- c("04011000" = "01", "04012000" = "02")

# --- per-file spec ------------------------------------------------------------
files <- tribble(
  ~file,                                                  ~year, ~date,
  "HB_2015_Buergerschaftswahl_Bremen_ebene3.csv",         2015L, "2015-05-10",
  "HB_2015_Buergerschaftswahl_Bremerhaven_ebene3.csv",    2015L, "2015-05-10",
  "HB_2019_Buergerschaftswahl_Bremen_ebene3.csv",         2019L, "2019-05-26",
  "HB_2019_Buergerschaftswahl_Bremerhaven_ebene3.csv",    2019L, "2019-05-26"
)

legend_for_year <- function(y) if (y == 2015L) legend_2015 else legend_2019

parse_one <- function(file, year, date) {
  d <- fread(file.path(raw_dir, file), sep = ";", header = TRUE,
             colClasses = "character", encoding = "UTF-8",
             data.table = FALSE)
  stopifnot(nrow(d) == 1L)  # ebene3 = one Wahlbereich row

  ags <- d[["ags"]]
  stopifnot(ags %in% names(wkr_nr_for_ags))
  wkr_nr   <- wkr_nr_for_ags[[ags]]
  wkr_name <- d[["gebiet-name"]]

  eligible <- as.integer(d[["A"]])
  voters   <- as.integer(d[["B"]])
  invalid  <- as.integer(d[["C"]])   # ungueltige Stimmzettel
  valid    <- as.integer(d[["D2"]])  # gueltige STIMMEN (denominator)

  legend <- legend_for_year(year)
  rows <- map_dfr(names(legend), function(code) {
    col <- paste0(code, "_SUMME_LISTE_KANDIDATEN")
    if (!col %in% names(d)) return(NULL)
    v <- d[[col]]
    if (is.na(v) || v == "") return(NULL)
    tibble(party_raw = unname(legend[[code]]),
           votes     = as.integer(v))
  })

  rows %>%
    transmute(
      state_abbr     = "HB",
      state          = "Bremen",
      election_year  = year,
      election_date  = date,
      wkr_nr         = wkr_nr,
      wkr_name       = wkr_name,
      stimme         = "zweitstimme",
      eligible_voters = eligible,
      number_voters   = voters,
      valid_votes     = valid,
      invalid_votes   = invalid,
      party_raw       = party_raw,
      votes           = votes
    )
}

long <- pmap_dfr(files, parse_one)

# column order exactly as required
col_order <- c("state_abbr", "state", "election_year", "election_date",
               "wkr_nr", "wkr_name", "stimme", "eligible_voters",
               "number_voters", "valid_votes", "invalid_votes",
               "party_raw", "votes")
long <- long[, col_order]

fwrite(long, out_csv)
cat("Wrote", nrow(long), "rows to", out_csv, "\n")

# --- VALIDATION ---------------------------------------------------------------
cat("\n=== (a) per (wkr,stimme): |sum(party votes) - valid_votes| ===\n")
chk <- long %>%
  group_by(election_year, wkr_nr, wkr_name, stimme, valid_votes) %>%
  summarise(sum_votes = sum(votes), .groups = "drop") %>%
  mutate(disc = abs(sum_votes - valid_votes))
print(as.data.frame(chk))
cat("MAX abs discrepancy:", max(chk$disc),
    "| groups checked:", nrow(chk), "\n")

cat("\n=== (c) Wahlbereich count per year (expect 2) ===\n")
print(long %>% distinct(election_year, wkr_nr) %>% count(election_year))
