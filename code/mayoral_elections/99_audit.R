### Mayoral / Landrat dataset audit
#
# Quick health check for the mayoral + landrat outputs after a pipeline run.
# Run after 04_candidate_characteristics.R completes. Catches the failure
# modes that have actually bitten this pipeline:
#   - NRW Landrat miscoded as Oberbürgermeisterwahl (AGS-suffix classifier bug)
#   - IT.NRW 2025 OB Stichwahl date typo (raw-file error patched in pipeline)
#   - Mayoral / Landrat cross-leakage after the May 2026 split
#   - Duplicate (ags, candidate_name) within the same year
#   - Orphan SW rows (HW votes NA, SW votes present)
#
# See docs/mayoral_elections_known_issues.md sections 10-13 for context.
#
# Exit code is 0 iff all checks pass.

suppressMessages({
  library(dplyr); library(readr); library(tibble); library(stringr)
  library(lubridate); library(conflicted)
  conflict_prefer("filter", "dplyr"); conflict_prefer("year", "lubridate")
})

setwd(here::here())

mc <- readRDS("data/mayoral_elections/final/mayoral_candidates.rds")
m  <- readRDS("data/mayoral_elections/final/mayoral_unharm.rds")
mh <- readRDS("data/mayoral_elections/final/mayoral_harm.rds")
lc <- readRDS("data/landrat_elections/final/landrat_candidates.rds")
l  <- readRDS("data/landrat_elections/final/landrat_unharm.rds")
mp <- readRDS("data/mayoral_elections/final/mayor_panel.rds")
if (!"state" %in% names(mp)) mp$state <- substr(mp$ags, 1, 2)

fail <- function(msg) {
  cat("  ✗ FAIL:", msg, "\n")
  failed <<- failed + 1
}
pass <- function(msg) cat("  ✓", msg, "\n")
failed <- 0

cat("════════════════════════════════════════════════════════════════════\n")
cat("Mayoral / Landrat audit\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

cat("1. Cross-leakage between mayoral and landrat (must all be 0)\n")
n1 <- sum(grepl("Krfr\\.|Kreisfreie Stadt", l$ags_name, ignore.case = TRUE))
if (n1 == 0) { pass("no kreisfreie Städte leaked into landrat_unharm")
} else { fail(sprintf("%d kreisfreie Städte leaked into landrat_unharm", n1)) }

# NB: "(?!stadt)" so a Gemeinde named "X, Kreisstadt" (a district seat with a
# Bürgermeister, common in Hessen) is NOT mistaken for a leaked Landkreis.
n2 <- sum(grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis(?!stadt)|Städteregion|Stadtregion", m$ags_name, perl = TRUE))
if (n2 == 0) { pass("no Kreise leaked into mayoral_unharm")
} else { fail(sprintf("%d Kreise leaked into mayoral_unharm", n2)) }

n3 <- sum(grepl("Krfr\\.|Kreisfreie Stadt", lc$ags_name, ignore.case = TRUE))
if (n3 == 0) { pass("no kreisfreie Städte leaked into landrat_candidates")
} else { fail(sprintf("%d kreisfreie Städte leaked into landrat_candidates", n3)) }

n4 <- sum(grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis(?!stadt)|Städteregion|Stadtregion", mc$ags_name, perl = TRUE))
if (n4 == 0) { pass("no Kreise leaked into mayoral_candidates")
} else { fail(sprintf("%d Kreise leaked into mayoral_candidates", n4)) }

cat("\n2. mayoral_harm contains no Landrat (filter at 02_mayoral_harm.R:54 must hold)\n")
n5 <- sum(mh$election_type == "Landratswahl")
if (n5 == 0) { pass("mayoral_harm has 0 Landratswahl rows")
} else { fail(sprintf("mayoral_harm has %d Landratswahl rows", n5)) }

cat("\n3. NRW 2020 — no duplicate (ags, candidate_name) [Issue 2 regression check]\n")
dup_2020 <- mc %>%
  filter(state == "05", election_year == 2020, !is.na(candidate_name)) %>%
  group_by(ags, candidate_name) %>% filter(n() > 1) %>% ungroup()
if (nrow(dup_2020) == 0) {
  pass("NRW 2020 mayoral has 0 duplicate (ags, candidate) pairs")
} else {
  fail(sprintf("NRW 2020 has %d duplicate (ags, candidate) pairs — IT.NRW patch may have failed",
               nrow(dup_2020)))
}

cat("\n4. IT.NRW 2025 patch worked — all NRW 2025 SW dates are 2025-09-28\n")
nrw_2025_sw <- mc %>% filter(state == "05", election_year == 2025, has_stichwahl) %>%
  distinct(ags, election_date_sw)
bad_dates <- nrw_2025_sw %>% filter(election_date_sw != as.Date("2025-09-28"))
if (nrow(bad_dates) == 0) {
  pass(sprintf("all %d NRW 2025 SW elections dated 2025-09-28", nrow(nrw_2025_sw)))
} else {
  fail(sprintf("%d NRW 2025 SW rows have wrong election_date_sw", nrow(bad_dates)))
  print(bad_dates)
}

cat("\n5. NRW orphan SW candidates (must be 0; other states have legitimate orphans)\n")
nrw_orph <- mc %>%
  filter(state == "05", is.na(candidate_votes_hw), !is.na(candidate_votes_sw))
if (nrow(nrw_orph) == 0) { pass("NRW has 0 orphan SW candidates")
} else { fail(sprintf("NRW has %d orphan SW candidates", nrow(nrw_orph))) }

cat("\n6. Date-year vs filename-year audit on raw NRW files\n")
ob_files <- list.files("data/mayoral_elections/raw/nrw/",
                        pattern = "Oberb.*xlsx$", full.names = TRUE)
suppressMessages({library(readxl); library(data.table)})
for (f in ob_files) {
  fy <- as.numeric(str_extract(basename(f), "20[0-9]{2}"))
  raw <- as.data.table(read_excel(f, sheet = 1, skip = 3, col_names = FALSE))
  raw <- raw[grepl("^[0-9]{6}$", as.character(raw[[1]])), ]
  raw[, dser := suppressWarnings(as.numeric(.SD[[1]])), .SDcols = 3]
  raw[, dt := as.Date(dser, origin = "1899-12-30")]
  raw[, dy := lubridate::year(dt)]
  miss <- sum(raw$dy != fy, na.rm = TRUE)
  if (miss == 0) {
    pass(sprintf("%s: 0 date-year mismatches", basename(f)))
  } else if (fy == 2025 && all(raw$dy[raw$dy != fy] == 2020, na.rm = TRUE)) {
    pass(sprintf("%s: %d 2020-09-27 rows (known IT.NRW typo, patched in pipeline)",
                  basename(f), miss))
  } else {
    fail(sprintf("%s: %d unexpected date-year mismatches — investigate",
                  basename(f), miss))
  }
}

cat("\n7. Vote-count integrity spot checks\n")
b25 <- mc %>% filter(ags == "05314000", election_year == 2025)
b25_hw <- sum(b25$candidate_votes_hw, na.rm = TRUE)
b25_sw <- sum(b25$candidate_votes_sw, na.rm = TRUE)
if (b25_hw == 158582) { pass(sprintf("Bonn 2025 HW total = %d", b25_hw))
} else { fail(sprintf("Bonn 2025 HW total = %d (expected 158582)", b25_hw)) }
if (b25_sw == 137646) { pass(sprintf("Bonn 2025 SW total = %d", b25_sw))
} else { fail(sprintf("Bonn 2025 SW total = %d (expected 137646)", b25_sw)) }

d20 <- mc %>% filter(ags == "05111000", election_year == 2020)
d20_hw <- sum(d20$candidate_votes_hw, na.rm = TRUE)
d20_sw <- sum(d20$candidate_votes_sw, na.rm = TRUE)
if (d20_hw == 244322) { pass(sprintf("Düsseldorf 2020 HW total = %d", d20_hw))
} else { fail(sprintf("Düsseldorf 2020 HW total = %d (expected 244322)", d20_hw)) }
if (d20_sw == 211307) { pass(sprintf("Düsseldorf 2020 SW total = %d", d20_sw))
} else { fail(sprintf("Düsseldorf 2020 SW total = %d (expected 211307)", d20_sw)) }

warned <- 0
warn <- function(msg) { cat("  ⚠ WARN:", msg, "\n"); warned <<- warned + 1 }
check <- function(cond, ok, ko) { if (isTRUE(cond)) pass(ok) else fail(ko) }

cat("\n8. Schema integrity\n")

expected_cols_unharm <- c("ags", "ags_name", "state", "state_name",
                           "election_year", "election_date", "election_type",
                           "round", "eligible_voters", "number_voters",
                           "valid_votes", "invalid_votes", "turnout",
                           "winner_party", "winner_votes", "winner_voteshare")
miss <- setdiff(expected_cols_unharm, names(m))
check(length(miss) == 0,
      "mayoral_unharm has all 16 expected columns",
      sprintf("mayoral_unharm missing: %s", paste(miss, collapse=", ")))

# All AGS in mayoral_unharm should be 8 chars
check(all(nchar(m$ags) == 8L),
      "all mayoral_unharm AGS are 8 chars",
      sprintf("%d AGS not 8 chars", sum(nchar(m$ags) != 8L)))

# Election dates should be Date type
check(inherits(m$election_date, "Date"),
      "mayoral_unharm$election_date is Date",
      "mayoral_unharm$election_date is not Date")

cat("\n9. Bayern classifier (Amtstitel-based) — verify per-state coverage\n")

by_breakdown <- m %>% filter(state == "09") %>% count(election_type)
n_by_bm <- by_breakdown %>% filter(election_type == "Bürgermeisterwahl") %>% pull(n)
n_by_ob <- by_breakdown %>% filter(election_type == "Oberbürgermeisterwahl") %>% pull(n)
if (length(n_by_bm) == 0) n_by_bm <- 0L
if (length(n_by_ob) == 0) n_by_ob <- 0L
check(n_by_bm >= 30000,
      sprintf("Bayern Bürgermeisterwahl: %d (≥30000 expected)", n_by_bm),
      sprintf("Bayern BM only %d (expected ≥30000)", n_by_bm))
check(n_by_ob >= 500,
      sprintf("Bayern Oberbürgermeisterwahl: %d (≥500 expected)", n_by_ob),
      sprintf("Bayern OB only %d (expected ≥500)", n_by_ob))

# Bayern should not contain Landratswahl (split-out)
n_by_lr <- m %>% filter(state == "09", election_type == "Landratswahl") %>% nrow()
check(n_by_lr == 0,
      "Bayern: 0 Landratswahl in mayoral (correctly split out)",
      sprintf("Bayern: %d Landratswahl rows leaked into mayoral", n_by_lr))

cat("\n10. Saarland Regionalverband Saarbrücken classifier\n")

# RVS should be in landrat, not mayoral
rvs_in_mayoral <- m %>% filter(state == "10", grepl("Regionalverband", ags_name))
check(nrow(rvs_in_mayoral) == 0,
      "Regionalverband Saarbrücken correctly excluded from mayoral",
      sprintf("%d RVS rows leaked into mayoral", nrow(rvs_in_mayoral)))

rvs_in_landrat <- l %>% filter(state == "10", grepl("Regionalverband", ags_name))
check(nrow(rvs_in_landrat) > 0,
      sprintf("RVS in landrat: %d rows", nrow(rvs_in_landrat)),
      "RVS missing from landrat")

cat("\n11. Mayoral/Landrat split totals\n")

# Mayoral row counts should be roughly stable (within 2% of expected baseline)
m_rows <- nrow(m)
check(m_rows > 35000 && m_rows < 55000,
      sprintf("mayoral_unharm has %d rows (expected 35-55k)", m_rows),
      sprintf("mayoral_unharm row count out of expected range: %d", m_rows))

# Landrat dataset minimum counts
l_rows <- nrow(l)
check(l_rows > 1500,
      sprintf("landrat_unharm has %d rows (≥1500 expected)", l_rows),
      sprintf("landrat_unharm only %d rows (expected ≥1500)", l_rows))

cat("\n12. NI Landrat persistence in landrat_unharm\n")
ni_landrat <- l %>% filter(state == "03") %>% nrow()
check(ni_landrat >= 80,
      sprintf("NI: %d Landrat rows in landrat_unharm (≥80 expected)", ni_landrat),
      sprintf("NI: only %d Landrat rows", ni_landrat))

cat("\n13. RLP per-sheet split (4 election types)\n")
rlp_types <- m %>% filter(state == "07") %>% count(election_type)
check(any(rlp_types$election_type == "VG-Bürgermeisterwahl"),
      "RLP has VG-Bürgermeisterwahl rows (sheet 'Bürgermeister Verbandsgemeinden')",
      "RLP missing VG-Bürgermeisterwahl rows")
check(any(rlp_types$election_type == "Oberbürgermeisterwahl"),
      "RLP has Oberbürgermeisterwahl rows (sheet 'Oberbürgermeister Krfr.Städte')",
      "RLP missing OB rows")
rlp_landrat <- l %>% filter(state == "07") %>% nrow()
check(rlp_landrat >= 100,
      sprintf("RLP: %d Landrat rows in landrat_unharm", rlp_landrat),
      sprintf("RLP: only %d Landrat rows", rlp_landrat))

cat("\n14. NRW classifier — every NRW Landkreis present in landrat\n")
expected_nrw_kreise <- c(
  "05154000","05158000","05162000","05166000","05170000",
  "05334000",
  "05358000","05362000","05366000","05370000","05374000","05378000","05382000",
  "05554000","05558000","05562000","05566000","05570000",
  "05754000","05758000","05762000","05766000","05770000","05774000",
  "05954000","05958000","05962000","05966000","05970000","05974000","05978000"
)
nrw_in_landrat <- l %>% filter(state == "05") %>% pull(ags) %>% unique()
missing_nrw <- setdiff(expected_nrw_kreise, nrw_in_landrat)
check(length(missing_nrw) == 0,
      sprintf("all 31 NRW Landkreise (incl. Städteregion) in landrat (%d found)",
              length(nrw_in_landrat)),
      sprintf("%d NRW Landkreise missing from landrat: %s",
              length(missing_nrw), paste(missing_nrw, collapse=", ")))

# 22 NRW kreisfreie Städte should be in mayoral
nrw_in_mayoral <- m %>%
  filter(state == "05", election_type == "Oberbürgermeisterwahl") %>%
  pull(ags) %>% unique()
check(length(nrw_in_mayoral) >= 22,
      sprintf("NRW kreisfreie Städte in mayoral: %d (≥22)", length(nrw_in_mayoral)),
      sprintf("NRW kreisfreie Städte: %d (expected ≥22)", length(nrw_in_mayoral)))

cat("\n15. mayoral_candidates regression — Bonn 2020 has exactly 8 rows\n")
bonn_2020 <- mc %>% filter(ags == "05314000", election_year == 2020) %>% nrow()
check(bonn_2020 == 8,
      sprintf("Bonn 2020: %d candidate rows (expected 8 — Issue 2 regression)", bonn_2020),
      sprintf("Bonn 2020: %d candidate rows (expected 8)", bonn_2020))

duesseldorf_2020 <- mc %>% filter(ags == "05111000", election_year == 2020) %>% nrow()
check(duesseldorf_2020 == 15,
      sprintf("Düsseldorf 2020: %d candidate rows (expected 15)", duesseldorf_2020),
      sprintf("Düsseldorf 2020: %d candidate rows (expected 15)", duesseldorf_2020))

cat("\n16. Baden-Württemberg coverage + Oberbürgermeister classifier\n")
# BW = last mayoral election per Gemeinde as of 31.12.2024 (StaLA B VII 3-j/25).
# 1101 Gemeinden, exactly 1101 winners (one per Gemeinde).
bw_u_munis <- m %>% filter(state == "08") %>% pull(ags) %>% unique()
check(length(bw_u_munis) == 1101,
      sprintf("BW: %d Gemeinden in mayoral_unharm (expected 1101)", length(bw_u_munis)),
      sprintf("BW: %d Gemeinden (expected 1101)", length(bw_u_munis)))
# Office classifier: 9 Stadtkreise + 96 Große Kreisstädte = 105 OB Gemeinden.
bw_ob <- m %>% filter(state == "08", election_type == "Oberbürgermeisterwahl") %>%
  pull(ags) %>% unique()
check(length(bw_ob) == 105,
      sprintf("BW: %d Oberbürgermeister Gemeinden (expected 105 = 9 SK + 96 GKS)", length(bw_ob)),
      sprintf("BW: %d Oberbürgermeister Gemeinden (expected 105)", length(bw_ob)))
# Stuttgart (Stadtkreis) must be OB; a small Gemeinde must be BM.
check("08111000" %in% bw_ob && !("08115001" %in% bw_ob),
      "BW classifier: Stuttgart=OB, Aidlingen=BM",
      "BW classifier: Stuttgart/Aidlingen office wrong")
# BW collects no party — winner_party must be entirely NA.
bw_party_na <- m %>% filter(state == "08") %>% pull(winner_party)
check(all(is.na(bw_party_na)),
      "BW: winner_party all NA (no party collected)",
      sprintf("BW: %d non-NA winner_party (expected 0)", sum(!is.na(bw_party_na))))
# Official gender → 114 elected women (matches StaLA press release 10.4%).
bw_women <- mc %>% filter(state == "08", candidate_gender == "female") %>% nrow()
check(bw_women == 114,
      sprintf("BW: %d elected women (matches StaLA's 114 / 10.4%%)", bw_women),
      sprintf("BW: %d elected women (expected 114)", bw_women))
# No BW Landrat (BW Landrat elections are not in this source).
check(sum(l$state == "08") == 0,
      "BW: 0 Landrat rows (none leaked / none expected)",
      sprintf("BW: %d Landrat rows (expected 0)", sum(l$state == "08")))
# Vote-count integrity: Stuttgart OB Neuwahl 2020 winner = 83,812 votes (42.3%).
stgt <- mc %>% filter(ags == "08111000")
check(nrow(stgt) == 1 && isTRUE(stgt$candidate_votes_sw[1] == 83812),
      "BW vote integrity: Stuttgart OB 2020 Neuwahl winner = 83,812 votes",
      "BW vote integrity: Stuttgart OB 2020 winner votes wrong")

cat("\n17. Brandenburg + Sachsen-Anhalt coverage (portal scrapes, recent cycle)\n")
# Brandenburg (state 12): amtsfreie Gemeinden/Städte + 4 kreisfreie Städte (OB),
# WITH party affiliation. Recent cycle only (~2018-2026).
bb_m <- m %>% filter(state == "12")
check(n_distinct(bb_m$ags) >= 70,
      sprintf("BB: %d Gemeinden in mayoral_unharm (≥70 expected)", n_distinct(bb_m$ags)),
      sprintf("BB: only %d Gemeinden (expected ≥70)", n_distinct(bb_m$ags)))
bb_ob <- bb_m %>% filter(election_type == "Oberbürgermeisterwahl") %>% pull(ags) %>% unique()
check(setequal(bb_ob, c("12051000", "12052000", "12053000", "12054000")),
      "BB: exactly the 4 kreisfreie Städte are Oberbürgermeisterwahl",
      sprintf("BB: OB cities wrong (%s)", paste(bb_ob, collapse = ", ")))
check(mean(!is.na(bb_m$winner_party)) > 0.95,
      "BB: winner_party populated (>95%; BB candidates carry party)",
      sprintf("BB: only %.0f%% winner_party non-NA", 100 * mean(!is.na(bb_m$winner_party))))
check(sum(l$state == "12") >= 0 && sum(m$state == "12" & m$election_type == "Landratswahl") == 0,
      "BB: no Landratswahl leaked into mayoral",
      "BB: Landratswahl leaked into mayoral")

# Sachsen-Anhalt (state 15): Bürgermeister-/OB-wahlen. PRIMARY source is now the
# StaLA HISTORICAL file "2026_0661_BM-Wahl_ab_1994.xlsx" (1994-2026, ~4140
# elections, 2057 historical AGS → 218 current Gemeinden), parsed by
# 00_st_hist_parse.py. The older bmbm.csv snapshot (00_st_stala_parse.py →
# st_bmbm_parsed.csv) stays AUTHORITATIVE for the 270 election-rounds it covers
# — it is the cleaner record (the historical file has ~16 surname typos, one
# vote typo, a swapped name field and 4 date typos in that window) — while the
# historical file supplies the whole back-catalogue and fixes bmbm's one known
# corruption (Genthin 2024, its 8th candidate). Landeswahlleiter portal remains a fallback.
st_m <- m %>% filter(state == "15")
check(n_distinct(st_m$ags) >= 2000,
      sprintf("ST: %d distinct AGS in mayoral_unharm (≥2000 expected — historical series uses AGS am Wahltag)", n_distinct(st_m$ags)),
      sprintf("ST: only %d distinct AGS (expected ≥2000)", n_distinct(st_m$ags)))
check(min(st_m$election_year) == 1994 && max(st_m$election_year) <= 2026,
      sprintf("ST: elections span %d-%d (historical series from 1994)",
              min(st_m$election_year), max(st_m$election_year)),
      sprintf("ST: unexpected election-year span (min=%d, max=%d; expected 1994-2026)",
              min(st_m$election_year), max(st_m$election_year)))
check(nrow(st_m) >= 4500,
      sprintf("ST: %d round-results (≥4500 expected from the 1994-2026 series)", nrow(st_m)),
      sprintf("ST: only %d round-results (expected ≥4500)", nrow(st_m)))
check(sum(m$state == "15" & m$election_type == "Landratswahl") == 0,
      "ST: no Landratswahl leaked into mayoral (ST Landrat is a separate dataset)",
      "ST: Landratswahl leaked into mayoral")

# Known OB fixtures — winners verifiable externally. `election_date` is the
# HAUPTWAHL date, because mayoral_candidates is the wide HW/SW format keyed on
# the cycle's Hauptwahl (the Stichwahl date lives in election_date_sw). Pinning
# the date is essential now that ST is a 1994-2026 series: without it a
# per-AGS lookup returns whichever election happens to sort first.
# The pre-2019 rows double as regression cover for the historical back-catalogue
# (Wiegand was Halle's OB 2012-2021, Trümper Magdeburg's 2001-2022).
st_ob_expected <- data.frame(
  ags = c("15001000", "15002000", "15003000", "15087370", "15082180",
          "15002000", "15002000", "15003000", "15003000", "15001000",
          "15087370", "15082180"),
  gemeinde = c("Dessau-Roßlau", "Halle (Saale)", "Magdeburg", "Sangerhausen", "Köthen",
               "Halle (Saale) 2019", "Halle (Saale) 2012", "Magdeburg 2015",
               "Magdeburg 2008", "Dessau-Roßlau 2014", "Sangerhausen 2010",
               "Köthen 2008"),
  election_date = as.Date(c("2021-06-06", "2025-02-02", "2022-04-24",
                            "2024-04-14", "2023-03-19",
                            "2019-10-13", "2012-07-01", "2015-03-15",
                            "2008-03-09", "2014-05-25", "2010-02-28",
                            "2008-02-17")),
  winner_last = c("Reck", "Vogt", "Borris", "Schweiger", "Buchheim",
                  "Wiegand", "Wiegand", "Trümper", "Trümper", "Kuras",
                  "Poschmann", "Zander")
)
# Cross-check winner votes match voteshare × valid_votes (rounding-safe)
st_vote_integrity_ok <- st_m %>%
  filter(!is.na(winner_votes) & !is.na(valid_votes) & valid_votes > 0 &
         !is.na(winner_voteshare)) %>%
  mutate(implied = winner_votes / valid_votes,
         diff    = abs(implied - winner_voteshare)) %>%
  pull(diff)
# Tolerance 5e-4 handles both StaLA's exact ratios AND the portal's 3-decimal
# rounded shares (e.g. Zerbst/Anhalt 2026 portal has 0.822 vs implied 0.82189).
check(all(st_vote_integrity_ok < 5e-4),
      sprintf("ST vote integrity: winner_votes / valid_votes == winner_voteshare (%d rows checked)",
              length(st_vote_integrity_ok)),
      "ST vote integrity: winner_votes/valid_votes mismatches winner_voteshare")

# Load candidates for winner-identity checks
st_c <- mc %>% filter(state == "15")
for (i in seq_len(nrow(st_ob_expected))) {
  row <- st_ob_expected[i, ]
  win <- st_c %>%
    filter(ags == row$ags & election_date == row$election_date &
             is_winner %in% TRUE)
  ok <- nrow(win) == 1 && !is.na(win$candidate_last_name) &&
    win$candidate_last_name == row$winner_last
  check(ok,
        sprintf("ST OB fixture: %s — winner is %s (expected)", row$gemeinde, row$winner_last),
        sprintf("ST OB fixture: %s — winner mismatch (got '%s')",
                row$gemeinde, if (nrow(win)==1) win$candidate_last_name else "<none>"))
}

# ART classifier — 38 distinct AGS appear as OB across 1994-2026. That is ~24
# cities counted under BOTH their pre-2007-Kreisreform code and their current
# one (Halle 15202000 + 15002000, Magdeburg 15303000 + 15003000, Dessau
# 15101000 + 15001000, …), plus cities that held an OB election historically but
# no longer do (Wolfen, Burg, Merseburg). The 19-Gemeinde figure was the count
# in the 2019-2026 snapshot and no longer applies to the historical series.
st_ob_ags <- st_m %>% filter(election_type == "Oberbürgermeisterwahl") %>%
  pull(ags) %>% unique()
check(length(st_ob_ags) == 38,
      sprintf("ST OB classifier: %d distinct AGS classified as OB across 1994-2026 (historical + current codes)", length(st_ob_ags)),
      sprintf("ST OB classifier: %d Gemeinden (expected 38)", length(st_ob_ags)))
# The three kreisfreie Städte must appear under their historical codes too.
check(all(c("15202000", "15303000", "15101000") %in% st_ob_ags),
      "ST OB: pre-2007 codes for Halle/Magdeburg/Dessau present (historical series)",
      "ST OB: pre-2007 kreisfreie-Stadt codes missing from the OB set")
kfs <- c("15001000", "15002000", "15003000")  # Dessau-Roßlau, Halle, Magdeburg
check(all(kfs %in% st_ob_ags),
      "ST OB: all 3 kreisfreie Städte classified as OB",
      "ST OB: kreisfreie Städte missing from OB set")

# Genthin regression — the known StaLA corruption (row 118, a missing 8th candidate).
# Ensure the portal-supplement recovered her candidate row so the election has
# the correct 8 candidates.
genthin <- st_c %>% filter(ags == "15086040" & election_date == as.Date("2024-11-10"))
check(nrow(genthin) == 8,
      "ST regression: Genthin 2024 has 8 candidates (StaLA=7 + the recovered 8th)",
      sprintf("ST regression: Genthin has %d candidates (expected 8)", nrow(genthin)))
# The recovered 8th candidate LOST, so under the StaLA licence their name is
# stripped from the published data (see the licence check below). Identify the
# row by its vote count (96) instead — that proves the record was recovered
# without republishing a losing candidate's name.
check(96 %in% genthin$candidate_votes_hw,
      "ST regression: the missing 8th Genthin 2024 candidate (96 votes) is present",
      "ST regression: the 96-vote Genthin 2024 candidate is NOT present")
check(sum(genthin$is_winner, na.rm = TRUE) == 1 &&
      genthin$candidate_last_name[which(genthin$is_winner)] == "Turian",
      "ST regression: Turian is the winner in Genthin 2024",
      "ST regression: wrong / missing winner for Genthin 2024")

# ONE winner per (ags, election_date) — no ambiguous or duplicate winner rows.
per_elec_st <- st_c %>%
  group_by(ags, election_date) %>%
  summarise(n_wins = sum(is_winner, na.rm = TRUE), .groups = "drop")
check(all(per_elec_st$n_wins == 1),
      sprintf("ST: exactly one winner per election (%d elections checked)", nrow(per_elec_st)),
      sprintf("ST: %d elections with ≠ 1 winner", sum(per_elec_st$n_wins != 1)))

# Ehrenamtliche Bürgermeister — the StaLA source lists them (ART=leer). Almost
# all of them ran as Einzelbewerber (winner_party empty). Confirm the winner_party
# distribution has a large Einzelbewerber share (the ehrenamtl. pattern).
st_bm_win_np <- mean(is.na(st_m$winner_party) | st_m$winner_party == "")
check(st_bm_win_np > 0.5,
      sprintf("ST: %.0f%% of winners are Einzelbewerber (expected majority — ehrenamtliche BM)",
              100 * st_bm_win_np),
      sprintf("ST: only %.0f%% Einzelbewerber (something changed)", 100 * st_bm_win_np))

# ---------------------------------------------------------------------------
# LICENCE COMPLIANCE — ST losing candidates must carry NO personal data.
# ---------------------------------------------------------------------------
# The Statistisches Landesamt Sachsen-Anhalt supplies the source with full names
# for scientific use only; publication is limited to anonymised data "analog zu
# den bayerischen Daten" (§ 80 KWO LSA likewise bars publishing candidate data
# more than six months after the result). 01b therefore strips every personal
# and name-derived field from ST non-winner rows. This check is the guard: if it
# ever fails, the published dataset is in breach — do not release it.
st_personal <- c("candidate_name", "candidate_last_name", "candidate_first_name",
                 "candidate_title", "candidate_gender", "candidate_birth_year",
                 "candidate_profession", "candidate_gender_source",
                 "candidate_gender_method", "candidate_gender_prob",
                 "candidate_name_origin", "candidate_name_origin_conf",
                 "candidate_name_origin_method", "candidate_migration_bg",
                 "candidate_migration_bg_prob", "candidate_local_surname",
                 "candidate_surname_county_share", "candidate_surname_n_counties",
                 "candidate_surname_overrep_ratio")
st_losers <- st_c %>% filter(!(is_winner %in% TRUE))
st_leaks <- sapply(intersect(st_personal, names(st_losers)), function(cl) {
  v <- st_losers[[cl]]
  sum(!is.na(v) & nzchar(trimws(as.character(v))))
})
check(sum(st_leaks) == 0,
      sprintf("ST licence: %d losing-candidate rows carry no personal data (Bayern-equivalent anonymisation)",
              nrow(st_losers)),
      sprintf("ST licence BREACH: personal data present on losing candidates — %s",
              paste(sprintf("%s=%d", names(st_leaks)[st_leaks > 0], st_leaks[st_leaks > 0]),
                    collapse = ", ")))
# The elected person stays named — they hold public office, as in Bayern.
check(sum(!is.na(st_c$candidate_last_name) &
            nzchar(trimws(st_c$candidate_last_name))) > 3000,
      "ST: elected mayors remain named (public office-holders, Bayern model)",
      "ST: winner names unexpectedly missing")

# Landrat unaffected — mayoral 01 overwrites landrat_unharm from HE/BY, but ST
# Landrat comes from a separate pipeline (00_st_scrape.R in landrat_elections/).
# Ensure ST Landrat coverage is untouched by this change (10 of 11 Landkreise —
# Stendal 15090 not in the landrat scrape window; not our concern here).
st_l <- l %>% filter(state == "15")
check(n_distinct(st_l$ags) >= 10,
      sprintf("ST Landrat: %d Landkreise present (≥10 expected, mayoral integration didn't affect)",
              n_distinct(st_l$ags)),
      sprintf("ST Landrat: only %d Landkreise", n_distinct(st_l$ags)))

cat("\n18. Hessen coverage + OB classifier (StaLA B VII m Direktwahlen snapshot)\n")
# Hessen = most recent Direktwahl per Gemeinde/Landkreis (~2017-2024). BM/OB go
# to mayoral; Landrat is split to the landrat dataset.
he_m <- m %>% filter(state == "06")
check(n_distinct(he_m$ags) >= 400,
      sprintf("HE: %d Gemeinden in mayoral_unharm (≥400 expected)", n_distinct(he_m$ags)),
      sprintf("HE: only %d Gemeinden (expected ≥400)", n_distinct(he_m$ags)))
# OB = 5 kreisfreie Städte + 7 Sonderstatusstädte = 12 cities.
he_ob <- he_m %>% filter(election_type == "Oberbürgermeisterwahl") %>% pull(ags) %>% unique()
check(length(he_ob) == 12 &&
        all(c("06412000", "06414000", "06435014", "06631009") %in% he_ob),
      sprintf("HE: %d Oberbürgermeister cities (expected 12 = 5 kfS + 7 Sonderstatus)", length(he_ob)),
      sprintf("HE: %d OB cities (expected 12)", length(he_ob)))
check(sum(m$state == "06" & m$election_type == "Landratswahl") == 0,
      "HE: no Landratswahl leaked into mayoral (HE Landrat split to landrat dataset)",
      "HE: Landratswahl leaked into mayoral")
# HE Landrat lives in the landrat dataset (21 Landkreise).
check(n_distinct(l$ags[l$state == "06"]) >= 21,
      sprintf("HE: %d Landkreise in landrat_unharm (≥21 expected)", n_distinct(l$ags[l$state == "06"])),
      sprintf("HE: only %d Landkreise in landrat (expected 21)", n_distinct(l$ags[l$state == "06"])))
# Vote-share integrity where the winner is Wahlvorschlag 1 (votes known).
he_winv <- mc %>% filter(state == "06", is_winner, !is.na(candidate_votes_hw),
                         !is.na(valid_votes), valid_votes > 0) %>%
  mutate(calc = candidate_votes_hw / valid_votes)
check(nrow(he_winv) == 0 || all(abs(he_winv$calc - he_winv$candidate_voteshare_hw) < 0.02, na.rm = TRUE),
      sprintf("HE vote integrity: winner votes/valid == voteshare (%d checked)", nrow(he_winv)),
      "HE vote integrity: winner voteshare mismatch")

cat("\n19. Bayern Kommunalwahl 2026 (added from the Landesamt Mandatsträger XLSX)\n")
by26 <- m %>% filter(state == "09", election_year == 2026)
check(n_distinct(by26$ags) >= 1800,
      sprintf("BY 2026: %d Gemeinden in mayoral_unharm (≥1800 expected)", n_distinct(by26$ags)),
      sprintf("BY 2026: only %d Gemeinden (expected ≥1800)", n_distinct(by26$ags)))
by26_ob <- by26 %>% filter(election_type == "Oberbürgermeisterwahl") %>% pull(ags) %>% n_distinct()
check(by26_ob >= 45,
      sprintf("BY 2026: %d Oberbürgermeister cities (≥45 expected)", by26_ob),
      sprintf("BY 2026: only %d OB cities (expected ≥45)", by26_ob))
check(sum(m$state == "09" & m$election_year == 2026 & m$election_type == "Landratswahl") == 0,
      "BY 2026: no Landratswahl leaked into mayoral (62 Landkreise split to landrat)",
      "BY 2026: Landratswahl leaked into mayoral")
# vote integrity: sum of candidate votes equals valid votes in 2026 candidate data
by26c <- mc %>% filter(state == "09", election_year == 2026)
check(nrow(by26c) > 4000 && mean(!is.na(by26c$candidate_votes_hw)) > 0.95,
      sprintf("BY 2026: %d candidate rows with votes (>95%% have HW votes)", nrow(by26c)),
      "BY 2026: candidate votes missing")
# panel: 2026 winners link to historical terms via Amtsantritt (incumbents present)
by26p <- mp %>% filter(state == "09", election_year == 2026)
check(nrow(by26p) >= 1800 && sum(by26p$is_incumbent == 1, na.rm = TRUE) >= 500,
      sprintf("BY 2026: %d panel rows, %d re-elected incumbents (Amtsantritt-linked)",
              nrow(by26p), sum(by26p$is_incumbent == 1, na.rm = TRUE)),
      "BY 2026: panel person-linking failed")

cat("\n20. Hessen Kommunalwahl 2026 (official XLSX full votes + hessenschau %-only)\n")
he26 <- m %>% filter(state == "06", election_year == 2026)
check(n_distinct(he26$ags) >= 25,
      sprintf("HE 2026: %d Gemeinden in mayoral_unharm (≥25 expected)", n_distinct(he26$ags)),
      sprintf("HE 2026: only %d Gemeinden (expected ≥25)", n_distinct(he26$ags)))
he26_ob <- he26 %>% filter(election_type == "Oberbürgermeisterwahl") %>% pull(ags) %>% n_distinct()
check(he26_ob == 1,
      sprintf("HE 2026: %d Oberbürgermeister city (Hanau)", he26_ob),
      sprintf("HE 2026: %d OB cities (expected 1 = Hanau)", he26_ob))
check(sum(m$state == "06" & m$election_year == 2026 & m$election_type == "Landratswahl") == 0 &&
        sum(l$state == "06" & l$election_year == 2026) == 0,
      "HE 2026: no Landratswahl (mayoral-only source)",
      "HE 2026: unexpected Landratswahl rows")
# Hybrid: the official May-2026 XLSX gives FULL votes for the ~12 Gemeinden it
# covers; the rest stay %-only from the hessenschau scrape (valid_votes NA).
he26_full <- he26 %>% filter(!is.na(valid_votes))
he26_pct <- he26 %>% filter(is.na(valid_votes))
check(nrow(he26_full) >= 10 && nrow(he26_pct) >= 8,
      sprintf("HE 2026: %d round-rows with full votes (official XLSX) + %d %%-only (hessenschau)",
              nrow(he26_full), nrow(he26_pct)),
      sprintf("HE 2026: expected XLSX-full + hessenschau-%%only mix, got %d/%d",
              nrow(he26_full), nrow(he26_pct)))
he26_dec <- he26 %>% filter(!is.na(winner_voteshare))
check(nrow(he26_dec) >= 25 && all(he26_dec$winner_voteshare >= 0.5 - 1e-9),
      sprintf("HE 2026: all %d decisive winners polled ≥50%% (no missing Stichwahl)", nrow(he26_dec)),
      sprintf("HE 2026: %d decisive winner(s) < 50%% — missing Stichwahl",
              sum(he26_dec$winner_voteshare < 0.5 - 1e-9)))
he26p <- mp %>% filter(state == "06", election_year == 2026)
check(nrow(he26p) >= 25 && n_distinct(he26p$ags) == nrow(he26p),
      sprintf("HE 2026: %d panel terms (one per Gemeinde, HW/SW deduped)", nrow(he26p)),
      "HE 2026: panel term dedup failed")

cat("\n════════════════════════════════════════════════════════════════════\n")
if (failed == 0) {
  cat(sprintf("All checks passed ✓ (%d warnings)\n", warned))
  quit(status = 0)
} else {
  cat(sprintf("%d check(s) failed ✗ (%d warnings)\n", failed, warned))
  quit(status = 1)
}
