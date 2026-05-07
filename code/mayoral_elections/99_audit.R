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

n2 <- sum(grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion", m$ags_name))
if (n2 == 0) { pass("no Kreise leaked into mayoral_unharm")
} else { fail(sprintf("%d Kreise leaked into mayoral_unharm", n2)) }

n3 <- sum(grepl("Krfr\\.|Kreisfreie Stadt", lc$ags_name, ignore.case = TRUE))
if (n3 == 0) { pass("no kreisfreie Städte leaked into landrat_candidates")
} else { fail(sprintf("%d kreisfreie Städte leaked into landrat_candidates", n3)) }

n4 <- sum(grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion", mc$ags_name))
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
check(m_rows > 35000 && m_rows < 45000,
      sprintf("mayoral_unharm has %d rows (expected 35-45k)", m_rows),
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

cat("\n════════════════════════════════════════════════════════════════════\n")
if (failed == 0) {
  cat(sprintf("All checks passed ✓ (%d warnings)\n", warned))
  quit(status = 0)
} else {
  cat(sprintf("%d check(s) failed ✗ (%d warnings)\n", failed, warned))
  quit(status = 1)
}
