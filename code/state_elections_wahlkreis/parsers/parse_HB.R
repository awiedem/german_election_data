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
#   All other files are PDF scans or finer geo levels (ebene6/8/11 =
#   Wahlbezirk/Stadtteil/Ortsteil, below the Wahlbereich -> not the
#   constituency unit) -- EXCEPT four PDF Hefte with a digital text layer,
#   which the Stage-0 script 00_hb_pdf_parse.py hard-validates and parses:
#     2003 (Heft 106), 2007 (Heft 110), 2011 (Heft 113 Teil 1), 2023 (Heft 126)
#   -> hb_pdf/HB_2003_2023_pdf_long.csv, appended below (see that script's
#   header for source pages, table structure per year, and validations).
#
# VOTE SYSTEM: Bremen is a city-state with a 5-vote proportional system since
#   2011. A voter distributes 5 votes among a party LIST and/or that party's
#   individual candidates. The party's TOTAL Stimmen (list + all its candidates)
#   determines seats -> this is the proportional "list" result. There is only
#   ONE ballot type (no separate Erst-/Zweitstimme). Per task spec it is mapped
#   to stimme = "zweitstimme" (the proportional list result). Before 2011
#   (2003, 2007) Bremen used a single list vote (Listenstimme) -- also mapped
#   to stimme = "zweitstimme" per the same single-ballot convention.
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
#
# Run order: python3 .../00_hb_pdf_parse.py  ->  Rscript .../parse_HB.R
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

long_2015_2019 <- pmap_dfr(files, parse_one)

# column order exactly as required
col_order <- c("state_abbr", "state", "election_year", "election_date",
               "wkr_nr", "wkr_name", "stimme", "eligible_voters",
               "number_voters", "valid_votes", "invalid_votes",
               "party_raw", "votes")
long_2015_2019 <- long_2015_2019[, col_order]

# --- VALIDATION (2015 / 2019) --------------------------------------------------
cat("\n=== (a) per (wkr,stimme): |sum(party votes) - valid_votes| (2015/2019) ===\n")
chk <- long_2015_2019 %>%
  group_by(election_year, wkr_nr, wkr_name, stimme, valid_votes) %>%
  summarise(sum_votes = sum(votes), .groups = "drop") %>%
  mutate(disc = abs(sum_votes - valid_votes))
print(as.data.frame(chk))
cat("MAX abs discrepancy:", max(chk$disc),
    "| groups checked:", nrow(chk), "\n")
stopifnot(max(chk$disc) == 0)

cat("\n=== (c) Wahlbereich count per year (expect 2) (2015/2019) ===\n")
print(long_2015_2019 %>% distinct(election_year, wkr_nr) %>% count(election_year))

# =============================================================================
# 2003 / 2007 / 2011 / 2023 -- Stage-0 PDF parse (Statistische Mitteilungen
# Hefte 106 / 110 / 113 / 126), cross-validated (party sums vs Gueltige
# Stimmen, Bremen+Bremerhaven vs Land Bremen, pinned official shares, and for
# 2023 also the InstantAtlas .js shares) -- see 00_hb_pdf_parse.py header.
# =============================================================================
pdf_csv <- file.path(dirname(out_csv), "hb_pdf", "HB_2003_2023_pdf_long.csv")
if (!file.exists(pdf_csv)) {
  stop("Missing ", pdf_csv,
       "\n  Run first:  python3 code/state_elections_wahlkreis/parsers/00_hb_pdf_parse.py")
}
pdf_long <- fread(pdf_csv, encoding = "UTF-8",
                  colClasses = list(character = c("state_abbr", "state", "election_date",
                                                  "wkr_nr", "wkr_name", "stimme",
                                                  "party_raw")))
stopifnot(setequal(names(pdf_long), col_order))
setcolorder(pdf_long, col_order)

cat("\n=========== HB 2003/2007/2011/2023 (from the PDF Hefte) ===========\n")
cat("    rows read      :", nrow(pdf_long), "\n")
print(pdf_long[, .(n_wkr = uniqueN(wkr_nr), n_parties = uniqueN(party_raw)),
               by = .(election_year, stimme)])

# per (year, wkr): sum(party votes) must equal valid_votes exactly
chk2 <- pdf_long[, .(sum_party = sum(votes, na.rm = TRUE), valid = unique(valid_votes)),
                 by = .(election_year, wkr_nr)]
chk2[, disc := abs(sum_party - valid)]
cat("    vote integrity : groups", nrow(chk2), "| max abs discrepancy", max(chk2$disc), "\n")
if (any(chk2$disc > 0)) { print(chk2[disc > 0]); stop("HB PDF rows fail vote integrity") }

# the 2 Wahlbereich names must be the same objects the 2015/2019 data uses
n_2015_2019 <- unique(as.data.table(long_2015_2019)[, .(wkr_nr, wkr_name)])
n_pdf <- unique(pdf_long[, .(wkr_nr, wkr_name)])
cmp_names <- merge(n_2015_2019, n_pdf, by = "wkr_nr", suffixes = c("_15_19", "_pdf"))
if (!all(cmp_names$wkr_name_15_19 == cmp_names$wkr_name_pdf)) {
  print(cmp_names[wkr_name_15_19 != wkr_name_pdf])
  stop("HB Wahlbereich names diverge across years")
}
cat("    Wahlbereich names identical to the 2015/2019 data: 2 / 2\n")

# =============================================================================
# COMBINE + WRITE (single write)
# =============================================================================
long <- rbindlist(list(as.data.table(long_2015_2019), pdf_long), use.names = TRUE)
setorder(long, election_year, wkr_nr, party_raw)

cat("\n=========== COMBINED (HB) ===========\n")
print(long[, .(rows = .N, n_wkr = uniqueN(wkr_nr)), by = election_year])

cat(sprintf("\nTotal rows emitted: %d\n", nrow(long)))
cat(sprintf("Distinct party_raw: %d\n", n_distinct(long$party_raw)))

fwrite(long, out_csv)
cat("Wrote", nrow(long), "rows to", out_csv, "\n")
