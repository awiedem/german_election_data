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

# Mayoral row counts should be roughly stable (within 2% of expected baseline;
# 55,224 after the Hessen 1993-2026 historical series landed in July 2026)
m_rows <- nrow(m)
check(m_rows > 40000 && m_rows < 62000,
      sprintf("mayoral_unharm has %d rows (expected 40-62k)", m_rows),
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
# Keyed on candidate_gender_source == "raw": name-predicted genders share the
# same m/w vocabulary, so filtering on the value alone would also count them.
bw_women <- mc %>%
  filter(state == "08", candidate_gender == "w", candidate_gender_source == "raw") %>%
  nrow()
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

cat("\n17. Brandenburg + Sachsen-Anhalt coverage\n")
# Brandenburg (state 12): hauptamtliche Bürgermeister of the amtsfreie
# Gemeinden/Städte + the 4 kreisfreie Städte (OB), WITH party affiliation.
# PRIMARY source since Aug 2026 is the Landeswahlleiter workbook supplied on
# request (00_bb_lwl_parse.py -> bb_lwl_parsed.csv), a 2010-2026 series; the
# older web-portal scrape (bb_bm_parsed.csv, current cycle only) survives as the
# fallback for the 2 elections the workbook does not carry. Every one of the 114
# rounds both sources hold matched exactly on counts, parties and candidate
# votes when the workbook was ingested.
bb_m <- m %>% filter(state == "12")
check(n_distinct(bb_m$ags) >= 130,
      sprintf("BB: %d Gemeinden in mayoral_unharm (≥130 expected)", n_distinct(bb_m$ags)),
      sprintf("BB: only %d Gemeinden (expected ≥130)", n_distinct(bb_m$ags)))
check(nrow(bb_m) >= 420 && min(bb_m$election_year) <= 2010,
      sprintf("BB: %d round-results spanning %d-%d (LWL series from 2010)",
              nrow(bb_m), min(bb_m$election_year), max(bb_m$election_year)),
      sprintf("BB: %d round-results, %d-%d (expected ≥420 from 2010)",
              nrow(bb_m), min(bb_m$election_year), max(bb_m$election_year)))
bb_ob <- bb_m %>% filter(election_type == "Oberbürgermeisterwahl") %>% pull(ags) %>% unique()
check(setequal(bb_ob, c("12051000", "12052000", "12053000", "12054000")),
      "BB: exactly the 4 kreisfreie Städte are Oberbürgermeisterwahl",
      sprintf("BB: OB cities wrong (%s)", paste(bb_ob, collapse = ", ")))
# rounds without any result (the first rounds that the B VII 3-6 reports date
# for winner-only runoff cycles) have no winner by construction
bb_res <- bb_m %>% filter(!(is.na(valid_votes) & is.na(winner_votes)))
check(mean(!is.na(bb_res$winner_party)) > 0.95,
      "BB: winner_party populated on rounds with a result (>95%; BB candidates carry party)",
      sprintf("BB: only %.0f%% winner_party non-NA", 100 * mean(!is.na(bb_res$winner_party))))
check(sum(l$state == "12") >= 0 && sum(m$state == "12" & m$election_type == "Landratswahl") == 0,
      "BB: no Landratswahl leaked into mayoral",
      "BB: Landratswahl leaked into mayoral")
# The source publishes vote counts, not shares — so this must reconcile exactly.
# It did NOT before Aug 2026: the portal printed shares rounded to 4 decimals.
bb_share <- bb_m %>% filter(!is.na(winner_votes), !is.na(valid_votes), valid_votes > 0)
check(all(abs(bb_share$winner_votes / bb_share$valid_votes -
              bb_share$winner_voteshare) < 1e-9),
      sprintf("BB vote integrity: winner_voteshare = votes/valid (%d rounds)",
              nrow(bb_share)),
      "BB vote integrity: winner_voteshare != winner_votes / valid_votes")
check(all(bb_m$valid_votes + bb_m$invalid_votes == bb_m$number_voters, na.rm = TRUE),
      "BB vote integrity: gültige + ungültige = Wähler",
      "BB vote integrity: gültige + ungültige != Wähler")

# Externally verified fixtures spanning the series (Landeswahlleiter result
# pages + local press). `election_date` is the HAUPTWAHL date; a Stichwahl
# winner's decisive share lives in candidate_voteshare_sw.
bb_fix <- data.frame(
  ags          = c("12068264", "12069397", "12065256", "12067144",
                   "12054000", "12064044", "12065356"),
  gemeinde     = c("Kyritz 2010", "Michendorf 2011 (SW)", "Oranienburg 2017 (SW)",
                   "Fürstenwalde 2018", "Potsdam OB 2025 (SW)",
                   "Bad Freienwalde 2025 (SW)", "Zehdenick 2026 Neuwahl"),
  election_date = as.Date(c("2010-11-07", "2011-09-11", "2017-09-24",
                            "2018-02-25", "2025-09-21", "2025-09-28",
                            "2026-05-10")),
  winner_last   = c("Görke", "Mirbach", "Laesicke", "Rudolph",
                    "Aubel", "Heidemann", "Stadtkewitz"),
  share         = c(0.557, 0.601, 0.558, 0.522, 0.729, 0.516, 0.584),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(bb_fix))) {
  w <- mc %>% filter(ags == bb_fix$ags[i], election_date == bb_fix$election_date[i],
                     is_winner %in% TRUE)
  got_share <- if (nrow(w) == 1) {
    dplyr::coalesce(w$candidate_voteshare_sw[1], w$candidate_voteshare_hw[1])
  } else NA_real_
  check(nrow(w) == 1 && w$candidate_last_name[1] == bb_fix$winner_last[i] &&
          !is.na(got_share) && abs(got_share - bb_fix$share[i]) < 0.001,
        sprintf("BB fixture: %s -> %s (%.1f%%)", bb_fix$gemeinde[i],
                bb_fix$winner_last[i], 100 * bb_fix$share[i]),
        sprintf("BB fixture FAILED: %s (rows=%d, winner=%s, share=%s)",
                bb_fix$gemeinde[i], nrow(w),
                if (nrow(w) == 1) w$candidate_last_name[1] else "-",
                if (is.na(got_share)) "NA" else sprintf("%.3f", got_share)))
}

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

# At most ONE winner per (ags, election_date) — never two, and never an
# arbitrary one. A handful of 1994-2008 ST elections carry no votes, no shares
# and no winner flag in the source; is_winner is NA there rather than being
# assigned to whichever candidate happened to be listed first, so those
# elections legitimately have zero winners. Two winners is still an error.
per_elec_st <- st_c %>%
  group_by(ags, election_date) %>%
  summarise(n_wins = sum(is_winner, na.rm = TRUE),
            determinable = any(!is.na(is_winner)), .groups = "drop")
check(all(per_elec_st$n_wins <= 1),
      sprintf("ST: never more than one winner per election (%d checked)", nrow(per_elec_st)),
      sprintf("ST: %d elections with more than one winner", sum(per_elec_st$n_wins > 1)))
check(all(per_elec_st$n_wins == 1 | !per_elec_st$determinable),
      "ST: every election whose winner is determinable has exactly one",
      sprintf("ST: %d determinable elections without a winner",
              sum(per_elec_st$n_wins != 1 & per_elec_st$determinable)))

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

# Thüringen follows the same rule (decision of 2026-10-03). The Landesamt itself
# withdraws losing candidates' names after a period (§ 50 Abs. 2 ThürKWO), so
# losers carry no personal data in mayoral_candidates or landrat_candidates;
# elected persons stay named. Calibration: 116 mayoral / 176 Landrat named
# losers on 2b8b288, 0 after.
for (th_set in c("mayoral", "landrat")) {
  th_losers <- (if (th_set == "mayoral") mc else lc) %>%
    filter(state == "16", !(is_winner %in% TRUE))
  th_leaks <- sapply(intersect(st_personal, names(th_losers)), function(cl) {
    v <- th_losers[[cl]]
    sum(!is.na(v) & nzchar(trimws(as.character(v))))
  })
  check(sum(th_leaks) == 0,
        sprintf("TH %s: %d losing-candidate rows carry no personal data", th_set, nrow(th_losers)),
        sprintf("TH %s: personal data present on losing candidates — %s", th_set,
                paste(sprintf("%s=%d", names(th_leaks)[th_leaks > 0], th_leaks[th_leaks > 0]),
                      collapse = ", ")))
}
check(sum(mc$state == "16" & mc$is_winner %in% TRUE & !is.na(mc$candidate_last_name)) > 600,
      "TH: elected mayors remain named (public office-holders)",
      "TH: winner names unexpectedly missing")

# Landrat unaffected — mayoral 01 overwrites landrat_unharm from HE/BY, but ST
# Landrat comes from a separate pipeline (00_st_scrape.R in landrat_elections/).
# Ensure ST Landrat coverage is untouched by this change (10 of 11 Landkreise —
# Stendal 15090 not in the landrat scrape window; not our concern here).
st_l <- l %>% filter(state == "15")
check(n_distinct(st_l$ags) >= 10,
      sprintf("ST Landrat: %d Landkreise present (≥10 expected, mayoral integration didn't affect)",
              n_distinct(st_l$ags)),
      sprintf("ST Landrat: only %d Landkreise", n_distinct(st_l$ags)))

cat("\n18. Hessen coverage + OB classifier (HSL historical series 1993-2026)\n")
# Hessen = FULL historical series from the HSL "Direktwahlen seit 1993" file
# (00_he_hist_parse.py), winner names grafted from the B VII m snapshot +
# hessenschau. BM/OB go to mayoral; Landrat is split to the landrat dataset.
he_m <- m %>% filter(state == "06")
check(n_distinct(he_m$ags) >= 400,
      sprintf("HE: %d Gemeinden in mayoral_unharm (≥400 expected)", n_distinct(he_m$ags)),
      sprintf("HE: only %d Gemeinden (expected ≥400)", n_distinct(he_m$ags)))
# Full series: ~2,900 BM/OB round-results spanning 1993-2026.
check(nrow(he_m) >= 2800 && min(he_m$election_year) == 1993 &&
        max(he_m$election_year) >= 2026,
      sprintf("HE: %d round-results spanning %d-%d (historical series)",
              nrow(he_m), min(he_m$election_year), max(he_m$election_year)),
      sprintf("HE: %d rounds %d-%d — historical series shrank",
              nrow(he_m), min(he_m$election_year), max(he_m$election_year)))
# Fixture: Ahnatal 2020-11-22 ended in an exact 2106:2106 tie, decided by lot
# for the SPD candidate — the parser must break the tie via the winner column.
ahna <- he_m %>% filter(ags == "06633001", election_date == as.Date("2020-11-22"))
check(nrow(ahna) == 1 && ahna$winner_party == "SPD",
      "HE fixture: Ahnatal 2020 exact-tie winner is SPD (Losentscheid)",
      "HE fixture: Ahnatal 2020 tie-break failed")
# Fixture: Obertshausen's by-election is 2026-01-18 (the B VII m snapshot's
# 2025-01-18 is a date typo — not even a Sunday; hessenschau confirms 2026).
obh <- he_m %>% filter(ags == "06438010", election_year >= 2025)
check(nrow(obh) == 1 && obh$election_date == as.Date("2026-01-18"),
      "HE fixture: Obertshausen by-election dated 2026-01-18 (snapshot typo fixed)",
      "HE fixture: Obertshausen 2026-01-18 date wrong")
# Frankfurt OB series: 6 elections 1995-2023, 4 of them with a Stichwahl
# (2001, 2012, 2018, 2023).
ffm <- he_m %>% filter(ags == "06412000")
check(n_distinct(ffm$election_date[ffm$round == "hauptwahl"]) == 6 &&
        sum(ffm$round == "stichwahl") == 4,
      "HE fixture: Frankfurt 6 OB elections 1995-2023 (4 Stichwahlen)",
      sprintf("HE fixture: Frankfurt OB series wrong (%d HW / %d SW)",
              n_distinct(ffm$election_date[ffm$round == "hauptwahl"]),
              sum(ffm$round == "stichwahl")))
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


# ============================================================================
cat("\n21. MV Bürgermeister of the amtsfreien Gemeinden (Landkreis Ludwigslust-Parchim)\n")
# The LAIV publishes only OB + Landrat centrally; the hauptamtliche BM of the
# amtsfreie Gemeinden come from the Kreiswahlleitungen. LUP supplied its five
# (00_mv_lup_parse.py). Everything below is pinned against the delivered
# Bekanntmachungen / result prints.
lup <- c("13076014", "13076060", "13076088", "13076090", "13076108")
lup_u <- m %>% filter(ags %in% lup)
check(n_distinct(lup_u$ags) == 5 && nrow(lup_u) == 9,
      sprintf("MV LUP: %d Gemeinden, %d round-results (2014-2023)",
              n_distinct(lup_u$ags), nrow(lup_u)),
      sprintf("MV LUP: expected 5 Gemeinden / 9 rounds, got %d / %d",
              n_distinct(lup_u$ags), nrow(lup_u)))
check(all(lup_u$election_type == "B\u00fcrgermeisterwahl"),
      "MV LUP: all B\u00fcrgermeisterwahl (no OB \u2014 LUP has no kreisfreie Stadt)",
      "MV LUP: non-B\u00fcrgermeisterwahl election_type leaked in")
check(!any(lup %in% l$ags),
      "MV LUP: no Landrat leakage (the Kreis's own Landratswahlen stay LAIV-sourced)",
      "MV LUP: a municipal AGS reached landrat_unharm")
# Boizenburg 2021: Jakobeit (SPD/LINKE) lost the Hauptwahl 46.80:45.66 and won
# the Stichwahl 57.87:42.13.
boi <- lup_u %>% filter(ags == "13076014", election_year == 2021) %>% arrange(election_date)
check(nrow(boi) == 2 && boi$round[2] == "stichwahl" &&
        abs(boi$winner_voteshare[2] - 0.5786673) < 1e-4 &&
        boi$winner_party[2] == "SPD, DIE LINKE",
      "MV LUP fixture: Boizenburg 2021 Stichwahl won by SPD/DIE LINKE at 57.87%",
      "MV LUP fixture: Boizenburg 2021 Stichwahl wrong")
# L\u00fcbtheen 2022 is a sole-candidate Ja/Nein ballot: g\u00fcltige = Ja + Nein, so the
# candidate's votes are legitimately below valid_votes.
lue <- lup_u %>% filter(ags == "13076088")
check(nrow(lue) == 1 && lue$winner_votes == 1163 && lue$valid_votes == 1287 &&
        abs(lue$winner_voteshare - 0.9036519) < 1e-4,
      "MV LUP fixture: L\u00fcbtheen 2022 Ja/Nein ballot 1163 Ja of 1287 g\u00fcltige (90.4%)",
      "MV LUP fixture: L\u00fcbtheen 2022 Ja/Nein wrong")
check(all(abs(lup_u$winner_votes / lup_u$valid_votes - lup_u$winner_voteshare) < 1e-6) &&
        all(lup_u$valid_votes + lup_u$invalid_votes == lup_u$number_voters),
      sprintf("MV LUP vote integrity: shares = votes/valid and valid+invalid = voters (%d rounds)",
              nrow(lup_u)),
      "MV LUP vote integrity failed")
# Hagenow 2015: the Hauptwahl was inconclusive (CDU 41.5%) and the Stichwahl \u2014
# missing from the first delivery, supplied on request \u2014 was won by M\u00f6ller
# (Die Linke) 57.83%. The runner-up of the first round must be the winner of the
# cycle; ranking the Hauptwahl alone would seat the CDU candidate who lost.
hag <- mc %>% filter(ags == "13076060", election_year == 2015)
hag_w <- hag %>% filter(is_winner)
check(nrow(hag) == 3 && all(hag$has_stichwahl) && nrow(hag_w) == 1 &&
        hag_w$candidate_last_name == "M\u00f6ller" && hag_w$candidate_rank_hw == 2 &&
        hag_w$candidate_votes_sw == 2405,
      "MV LUP fixture: Hagenow 2015 decided by the Stichwahl (M\u00f6ller, 2nd in the Hauptwahl)",
      "MV LUP fixture: Hagenow 2015 winner wrong \u2014 the Hauptwahl leader must not be seated")
# Both Hagenow terms belong to ONE person, so 2022 is an incumbent re-election.
hag_p <- mp %>% filter(ags == "13076060") %>% arrange(election_year)
check(nrow(hag_p) == 2 && n_distinct(hag_p$person_id) == 1 &&
        hag_p$term_number[2] == 2 && hag_p$is_incumbent[2] == 1,
      "MV LUP: Hagenow 2015 + 2022 linked to one person (2022 = incumbent re-election)",
      "MV LUP: Hagenow terms not linked to a single person")
# The mechanism that kept the incomplete cycle winner-less is retained. No MV
# cycle uses it; since October 2026 it marks exactly the two Hessen Hauptwahlen
# that seated nobody (a sole candidate failed the Ja/Nein vote; section 22) and
# the Thüringen cycle whose elected person declined (16077047, 2026), plus the
# four Thüringen write-in elections of 27.06.2004 in which nobody won a majority
# ("Wiederholungswahl erforderlich", TLS 20421; section 25). Brandenburg's 36
# such elections (B VII 3-6 reports) are pinned in section 27e. Since 2026-10-13
# also 18 Thüringen cycles of 1994-2010 under the majority rule (section 25m/n):
# 14 first rounds without a majority, runoff or seated winner, and 4 elections of
# the 2009-2010 no-runoff window whose winner never took office; and the 2
# elections of 6 June 2010 that TLS 29926 marks "Neuwahl erforderlich" among the
# 300 it adds (section 25o).
th_neuwahl_2010 <- c("16063059 2010-06-06", "16075012 2010-06-06")
th_majority_rule <- c(
  "16064062 1994-06-12", "16063049 1995-12-03", "16061044 1999-06-13", "16061094 1999-06-13",
  "16064061 1999-06-13", "16069004 1999-06-13", "16071071 1999-06-13", "16074058 1999-06-13",
  "16074108 1999-06-13", "16076068 1999-06-13", "16076034 2002-12-01", "16061033 2006-05-07",
  "16064064 2007-06-03", "16069058 2008-03-02", "16064009 2010-02-21", "16073046 2010-06-06",
  "16074077 2010-06-06", "16075066 2010-06-06")
fdm <- mc %>% filter(flag_decisive_round_missing %in% TRUE, state != "12") %>%
  distinct(ags, election_date)
check("flag_decisive_round_missing" %in% names(mc) &&
        !any(substr(fdm$ags, 1, 2) == "13") &&
        setequal(paste(fdm$ags, fdm$election_date),
                 c("06532007 2016-03-06", "06634015 2022-03-06", "16077047 2026-03-01",
                   "16063001 2004-06-27", "16067046 2004-06-27", "16069047 2004-06-27",
                   "16076034 2004-06-27", th_majority_rule, th_neuwahl_2010)),
      "flag_decisive_round_missing: no MV cycle; only the 2 failed Hessen Ja/Nein votes, the declined TH election, the 4 failed TH write-in elections of 2004, the 18 TH majority-rule cycles and the 2 added TH Neuwahlen of 2010",
      sprintf("flag_decisive_round_missing on unexpected cycles: %s",
              paste(fdm$ags, fdm$election_date, collapse = ", ")))
lup_p <- mp %>% filter(ags %in% lup)
check(nrow(lup_p) == 7 && n_distinct(paste(lup_p$ags, lup_p$election_year)) == 7,
      sprintf("MV LUP: %d panel terms (one per cycle)", nrow(lup_p)),
      sprintf("MV LUP: expected 7 panel terms, got %d", nrow(lup_p)))

# ============================================================================
cat("\n22. Hessen names (Hessami 2018), runoff pairing, panel fixes (October 2026)\n")
# 1993-2012 names come from Hessami (2018, REStat, doi:10.7910/DVN/FZWOMK, CC0).
# The elected person is named publicly; other candidates' names live only in the
# restricted twin. Stage 0 now links each runoff candidate to their first-round
# row (pair_id). Calibrated on the September-2026 release, where 22a, 22c, 22d,
# 22e (twin), 22f, 22g and 22h all fail: 192 runoff-only rows, a runoff result
# on the third-placed candidate (2008), no 1993-2012 names, 285 winners without
# gender, Waldems 2000 joined to 1999, the 5 same-year terms missing. 22b and the
# 22e privacy check are invariants that held before as well.
he_c  <- mc %>% filter(state == "06")
he_lc <- lc %>% filter(state == "06")
runoff_only <- function(d) d %>%
  group_by(ags, election_date) %>%
  mutate(.hw = any(!is.na(candidate_votes_hw) | !is.na(candidate_voteshare_hw))) %>%
  ungroup() %>%
  filter(.hw, is.na(candidate_votes_hw), is.na(candidate_voteshare_hw),
         !is.na(candidate_votes_sw) | !is.na(candidate_voteshare_sw))
n_ro <- nrow(runoff_only(he_c)) + nrow(runoff_only(he_lc))
check(n_ro == 0,
      "22a HE: every runoff result sits on its candidate's first-round row (0 runoff-only rows)",
      sprintf("22a HE: %d runoff-only rows — the HW/SW pairing (pair_id) broke", n_ro))
rows_bad <- he_c %>% filter(!is.na(n_candidates_hw)) %>%
  count(ags, election_date, n_candidates_hw) %>% filter(n != n_candidates_hw)
check(nrow(rows_bad) == 0,
      "22b HE: every cycle has exactly as many rows as first-round Wahlvorschläge",
      sprintf("22b HE: %d cycles with rows != n_candidates_hw", nrow(rows_bad)))
# A runoff is between the first round's top two; the two exceptions are pinned:
# Nauheim 2023 (tie for 2nd, decided by lot) and Siegbach 1996 (2nd withdrew).
r3 <- he_c %>% filter(!is.na(candidate_votes_sw), candidate_rank_hw > 2) %>%
  distinct(ags, election_date)
check(setequal(paste(r3$ags, r3$election_date),
               c("06433009 2023-02-26", "06532019 1996-04-28")),
      "22c HE: runoff entrants are first-round ranks 1-2 (2 documented exceptions)",
      sprintf("22c HE: unexpected runoff entrants ranked >2: %s",
              paste(r3$ags, r3$election_date, collapse = ", ")))
w12 <- he_c %>% filter(election_year <= 2012)
check(sum(w12$is_winner %in% TRUE) == 1506 &&
        all(!is.na(w12$candidate_name[w12$is_winner %in% TRUE])),
      "22d HE 1993-2012: all 1,506 elected persons named (Hessami 2018)",
      sprintf("22d HE 1993-2012: %d of %d winners named",
              sum(!is.na(w12$candidate_name[w12$is_winner %in% TRUE])),
              sum(w12$is_winner %in% TRUE)))
check(sum(!(w12$is_winner %in% TRUE) & !is.na(w12$candidate_name)) == 0 &&
        sum(!(w12$is_winner %in% TRUE) & !is.na(w12$candidate_gender)) == 0,
      "22e HE 1993-2012: PRIVACY — no losing candidate is named (or gendered) in the public file",
      "22e HE 1993-2012: a losing candidate's name reached the public file")
rtwin <- "data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds"
if (file.exists(rtwin)) {
  mr12 <- readRDS(rtwin) %>% filter(state == "06", election_year <= 2012)
  check(sum(!(mr12$is_winner %in% TRUE) & !is.na(mr12$candidate_name)) == 2266,
        "22e HE: the restricted twin holds the 2,266 losing candidates' names",
        sprintf("22e HE: restricted twin has %d named non-winners (expected 2,266)",
                sum(!(mr12$is_winner %in% TRUE) & !is.na(mr12$candidate_name))))
}
he_w <- he_c %>% filter(is_winner %in% TRUE)
check(sum(is.na(he_w$candidate_gender)) == 0,
      sprintf("22f HE: all %d elected persons carry a gender (HSL records it for every winner)", nrow(he_w)),
      sprintf("22f HE: %d winners without gender (runoff gender not carried to the HW row?)",
              sum(is.na(he_w$candidate_gender))))
check(sum(he_c$election_year == 2013 & he_c$is_winner %in% TRUE & !is.na(he_c$candidate_name)) >= 11,
      "22g HE 2013: the 11 winners named in Hessami's collected.dta are present",
      "22g HE 2013: winner names from collected.dta missing")
he_p <- mp %>% filter(state == "06")
wal <- he_p %>% filter(ags == "06439016") %>% arrange(election_date)
check(wal$person_id[wal$election_date == as.Date("2000-05-21")] ==
        wal$person_id[wal$election_date == as.Date("1993-05-02")] &&
        wal$person_id[wal$election_date == as.Date("2000-05-21")] !=
        wal$person_id[wal$election_date == as.Date("1999-06-13")],
      "22h HE panel: Waldems 2000 is the 1993 mayor's return (the 1999 election was annulled)",
      "22h HE panel: Waldems 2000 not linked to the 1993 mayor")
same_yr <- he_p %>% count(ags, election_year) %>% filter(n > 1)
check(setequal(paste(same_yr$ags, same_yr$election_year),
               c("06432001 2007", "06432002 2002", "06433001 1993", "06440002 1999",
                 "06631002 2014")) &&
        nrow(he_p %>% filter(ags == "06633002", election_year == 2017)) == 1,
      "22h HE panel: the 5 counted same-year terms kept; Bad Karlshafen's superseded 2017 runoff excluded",
      sprintf("22h HE panel: same-year terms wrong (%s)",
              paste(same_yr$ags, same_yr$election_year, collapse = ", ")))
check(nrow(he_p) == n_distinct(paste(he_p$ags, he_p$election_date)) &&
        sum(he_p$person_id_method == "hessen_official_term_sequence") >= 2415,
      sprintf("22h HE panel: %d terms, one per decisive election, %d counter-linked",
              nrow(he_p), sum(he_p$person_id_method == "hessen_official_term_sequence")),
      "22h HE panel: duplicate terms or counter links lost")

# ----------------------------------------------------------------------------
cat("\n24. Bayern winner names from the Bürgermeisterverzeichnisse 1978-2014 (October 2026)\n")
# 00_by_verzeichnis_parse.py → 01b fill. The LfStat declined permission to
# redistribute the contents of its registers and candidate lists (16.10.2026), so
# their names, birth years and professions reach the restricted twin only, the
# elected persons' included. 24a checks that the public file holds none of them
# (calibrated: the 2026-10-13 build published 17,882 named winners before 2026) and
# that the twin holds them; 24c-24e run on the twin. Fixtures that name people sit
# in a gitignored file next to the registers. The winners' gender, derived from the
# withheld given name, IS published (user decision 2026-10-16); name origin and
# migration background are not.
by_c <- mc %>% filter(state == "09", election_year < 2026)
by_all <- mc %>% filter(state == "09")
by_personal <- c("candidate_name", "candidate_last_name", "candidate_first_name", "candidate_title",
                 "candidate_gender", "candidate_birth_year", "candidate_profession",
                 "candidate_name_origin", "candidate_migration_bg", "candidate_local_surname")
by_pub_leak <- sum(sapply(intersect(setdiff(by_personal, "candidate_gender"), names(by_c)),
                          function(cl) sum(!is.na(by_c[[cl]]))))
check(by_pub_leak == 0,
      "24a BY: LICENCE — no name, birth year, profession or name-origin field for any Bayern row before 2026 in the public file (LfStat, 16.10.2026)",
      sprintf("24a BY: %d personal values of Bayern rows before 2026 reached the public file", by_pub_leak))
n_by_gender <- sum(by_c$is_winner %in% TRUE & !is.na(by_c$candidate_gender))
check(n_by_gender >= 17000,
      sprintf("24a BY: gender published for %d Bayern winner rows before 2026 (names withheld)", n_by_gender),
      sprintf("24a BY: only %d Bayern winner rows before 2026 carry a gender (04's copy from the twin missing?)",
              n_by_gender))
by_loser_leak <- sum(sapply(intersect(by_personal, names(by_all)), function(cl)
  sum(!(by_all$is_winner %in% TRUE) & !is.na(by_all[[cl]]))))
check(by_loser_leak == 0,
      "24b BY: PRIVACY — no personal or name-derived field is set for any Bayern non-winner in the public file",
      sprintf("24b BY: %d personal values of Bayern losing candidates reached the public file", by_loser_leak))
tw_path <- "data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds"
by_fx_path <- "data/mayoral_elections/raw/bayern/lfstat_verzeichnisse/audit_fixtures_restricted.csv"
by_fx_all <- if (file.exists(by_fx_path)) {
  read.csv(by_fx_path, colClasses = "character", encoding = "UTF-8")
} else NULL
if (file.exists(tw_path)) {
  tw_by_all <- readRDS(tw_path) %>% filter(state == "09")
  tw_by_c <- tw_by_all %>% filter(election_year < 2026)
  by_w <- tw_by_c %>% filter(is_winner %in% TRUE)
  check(sum(!is.na(by_w$candidate_last_name)) >= 15000,
        sprintf("24a BY restricted twin: %d pre-2026 winner rows named from the registers",
                sum(!is.na(by_w$candidate_last_name))),
        sprintf("24a BY restricted twin: only %d pre-2026 winner rows named (register fill missing?)",
                sum(!is.na(by_w$candidate_last_name))))
  if (!is.null(by_fx_all)) {
    by_fix <- by_fx_all %>% filter(check == "24c") %>%
      transmute(ags, decisive = as.Date(date), last = last_name, birth = as.numeric(birth_year))
    by_fx <- by_w %>%
      mutate(decisive = coalesce(election_date_sw, election_date)) %>%
      inner_join(by_fix, by = c("ags", "decisive"))
    check(nrow(by_fix) == 5 && nrow(by_fx) == nrow(by_fix) && all(by_fx$candidate_last_name == by_fx$last) &&
            all(by_fx$candidate_birth_year == by_fx$birth),
          "24c BY restricted: five Günzburg register fixtures named with birth year",
          sprintf("24c BY restricted: register fixtures wrong (%d/%d found)", nrow(by_fx), nrow(by_fix)))
  } else {
    warn("24c BY restricted: fixture file absent (gitignored, next to the registers)")
  }
  # 24d: name plausibility. Calibrated on the first build, which named 119 BY
  # winners with a profession or a given name as surname and 15 with surname and
  # given name reversed.
  not_a_name <- "^(Bürgermeister(in)?|Oberbürgermeister(in)?|Erste[r]?|Landr(at|ätin)|Rentner(in)?|Hausfrau|Pensionär|Dipl\\b.*|Verw\\b.*|Techn\\b.*)$|^.{6,}(meister|beamter|beamtin|angestellter|angestellte|unternehmer|arbeiter)$"
  by_named <- by_w %>% filter(!is.na(candidate_last_name))
  n_no_first <- sum(is.na(by_named$candidate_first_name) | !nzchar(by_named$candidate_first_name))
  n_prof <- sum(grepl(not_a_name, by_named$candidate_last_name, ignore.case = TRUE, perl = TRUE))
  check(n_no_first == 0 && n_prof == 0,
        "24d BY restricted: every named winner has a given name and no office or profession word as surname",
        sprintf("24d BY restricted: %d named winners without given name, %d with an office/profession surname",
                n_no_first, n_prof))
  n_gender <- mean(!is.na(by_named$candidate_gender))
  check(n_gender >= 0.97,
        sprintf("24d BY restricted: %.1f%% of named winners have a gender from the first name", 100 * n_gender),
        sprintf("24d BY restricted: only %.1f%% of named winners get a gender — given names garbled or reversed?",
                100 * n_gender))
  by_age <- by_w %>% filter(!is.na(candidate_birth_year)) %>%
    mutate(age = election_year - candidate_birth_year)
  check(nrow(by_age) > 0 && all(by_age$age >= 18 & by_age$age <= 95),
        sprintf("24d BY restricted: all %d register birth years give an age of 18-95 at election", nrow(by_age)),
        sprintf("24d BY restricted: %d register birth years give an impossible age",
                sum(by_age$age < 18 | by_age$age > 95)))
  by_sup <- tw_by_c %>% filter(flag_superseded %in% TRUE, is_winner %in% TRUE, !is.na(candidate_last_name))
  check(nrow(by_sup) == 0,
        "24d BY restricted: no register name on a superseded round (e.g. Gnotzheim 1993-01-17)",
        sprintf("24d BY restricted: %d superseded rounds carry a register name", nrow(by_sup)))
  # Hohenthann's 2002/2008 rounds carry the NEXT mayor's first Amtsantritt (2014) in
  # "Wahlen seit 1945"; the parser must not carry the 2002-2014 mayor's name onto
  # his successor's rounds.
  hoh <- by_w %>% filter(ags == "09274141")
  hoh_old <- na.omit(hoh$candidate_last_name[hoh$election_year %in% c(2002, 2008)])
  hoh_new <- hoh %>% filter(election_year >= 2014)
  check(nrow(hoh_new) >= 2 && length(hoh_old) > 0 && !any(hoh_new$candidate_last_name %in% hoh_old),
        "24e BY restricted: a source key with an impossible Amtsantritt passes no name on (Hohenthann 2014/2020)",
        "24e BY restricted: Hohenthann 2014/2020 carry the 2002-2014 mayor's name")
  tw_by <- tw_by_all %>% filter(!(is_winner %in% TRUE))
  check(sum(!is.na(tw_by$candidate_last_name)) >= 1800,
        sprintf("24g BY restricted twin: %d named Bayern losing candidates (B VII 3-2 lists)",
                sum(!is.na(tw_by$candidate_last_name))),
        "24g BY restricted twin: Bayern candidate-list names missing")
} else {
  warn("24a/24c-24g BY restricted: mayoral_candidates_restricted.rds absent (scientific-use twin not built here)")
}
# 24h: Bayern runoffs paired across a relabelled list (01b Step 2c). Calibrated:
# 382 split cycles before the elimination step, 231 after; Krumbach 2008 split.
by_hw_any <- by_all %>% group_by(ags, election_date) %>%
  summarise(.hw_any = any(!is.na(candidate_votes_hw)), .groups = "drop")
by_split <- by_all %>% filter(is_winner %in% TRUE, !is.na(election_date_sw), is.na(candidate_votes_hw)) %>%
  inner_join(by_hw_any, by = c("ags", "election_date")) %>% filter(.hw_any)
kru <- by_all %>% filter(ags == "09774150", election_date == as.Date("2008-03-02"), is_winner %in% TRUE)
check(nrow(by_split) <= 250 && nrow(kru) == 1 && !is.na(kru$candidate_votes_hw) && !is.na(kru$candidate_votes_sw),
      sprintf("24h BY runoffs: %d winner rows still split from their first round (was 382); Krumbach 2008 joined",
              nrow(by_split)),
      sprintf("24h BY runoffs: %d split winner rows, or Krumbach 2008 still split", nrow(by_split)))
# 24i: panel person keys split where the workbook's Amtsantritt merges two mayors
# (03_mayor_panel.R). Calibrated on the previous panel, where all four pairs below
# shared one person_id and Kirchweidach was already (correctly) one person.
pid_at <- function(a, d) mp$person_id[mp$ags == a & mp$election_date == as.Date(d)]
check(pid_at("09761000", "2002-03-17") != pid_at("09761000", "1996-03-24") &&   # Augsburg: two OBs, one key
        pid_at("09274141", "2008-03-02") != pid_at("09274141", "2014-03-30") &&   # Hohenthann: predecessor / successor
        pid_at("09377141", "2008-03-02") != pid_at("09377141", "2026-03-08") &&   # Mitterteich: two namesakes
        pid_at("09779194", "1982-03-14") != pid_at("09779194", "1988-03-06") &&   # Nördlingen: two OBs, one key
        pid_at("09171122", "2002-03-03") == pid_at("09171122", "2020-03-15") &&   # Kirchweidach: a return, one person
        pid_at("09174115", "2014-03-30") == pid_at("09174115", "2020-03-15"),     # Dachau: a date error, one person
      "24i BY panel: merged person keys split (Augsburg, Hohenthann, Mitterteich, Nördlingen); returns and date-only errors kept together",
      "24i BY panel: a known merged person key is not split, or a same-person key was split")
reg_r <- "data/mayoral_elections/final_restricted/by_register_persons_restricted.rds"
if (file.exists(reg_r)) {
  rr <- readRDS(reg_r)
  check(mean(!is.na(rr$person_id)) >= 0.95 && sum(!is.na(rr$birth_date)) > 1000,
        sprintf("24f BY restricted: %d register persons, %.1f%% linked to person_id, %d exact birth dates",
                nrow(rr), 100 * mean(!is.na(rr$person_id)), sum(!is.na(rr$birth_date))),
        "24f BY restricted: register table poorly linked or missing birth dates")
} else {
  warn("24f BY restricted: by_register_persons_restricted.rds absent (scientific-use twin not built here)")
}

# ============================================================================
cat("\n25. Thüringen elections from the TLS reports 2000, 2004 and 2010 (October 2026)\n")
# 00_th_tls_reports_parse.py: TLS 20421 (Kommunalwahlen 2004, every candidate
# named) and the transcribed runoffs of 28.05.2000 (TLS 29924). Calibrated: on
# the outputs before the ingest everything but 25c fails (439 Gemeinden in 2004,
# no winner-only or runoff-only 2000 rounds). The TLS consented on 12.10.2026 to
# publishing the reports' results only "unter Löschung sämtlicher
# personenbezogenen Daten zu Bewerbern und gewählten Kandidaten" (§ 50 Abs. 2
# ThürKWO), so no name from them is public, the elected persons' included (25b,
# 25c, 25g-25i fail on the 2026-10-09 build, which published 996 such winners);
# the restricted twin keeps them (25d, 25j). The same e-mail corrected the
# Tautenburg spelling and the dates pinned in 25k (fails on the 2026-10-09 build:
# Leutenberg twice, Königsee on a Saturday, three runoffs missing).
mcr_path <- "data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds"
tw_th <- mcr_path   # also read by 27h
mcr <- if (file.exists(mcr_path)) readRDS(mcr_path) else NULL
th04 <- m %>% filter(state == "16", election_type == "Bürgermeisterwahl",
                     election_date == as.Date("2004-06-27"), round == "hauptwahl")
check(n_distinct(th04$ags) == 820,
      "25a TH: 820 Bürgermeisterwahlen on 27.06.2004 (TLS 20421 table 3.5; database 439)",
      sprintf("25a TH: %d Bürgermeisterwahlen on 27.06.2004, expected 820", n_distinct(th04$ags)))
thc <- mc %>% filter(state == "16", election_date == as.Date("2004-06-27"))
thw <- thc %>% group_by(ags) %>%
  summarise(failed = any(flag_decisive_round_missing %in% TRUE), n_w = sum(is_winner %in% TRUE),
            named = any(!is.na(candidate_last_name)), .groups = "drop")
check(nrow(thw) == 820 && sum(thw$failed) == 4 && all(thw$n_w[!thw$failed] == 1) &&
        !any(thw$named) && all(thw$n_w[thw$failed] == 0),
      "25b TH 2004: 816 cycles with exactly one winner, nobody named; the 4 failed write-in elections seat nobody",
      sprintf("25b TH 2004: %d cycles, %d failed, %d without one winner, %d with a name",
              nrow(thw), sum(thw$failed), sum(!thw$failed & thw$n_w != 1), sum(thw$named)))
th_pers <- mc$state == "16" & (!is.na(mc$candidate_name) | !is.na(mc$candidate_last_name) |
                                 !is.na(mc$candidate_gender))
check(!any(th_pers & !(mc$is_winner %in% TRUE)) && !any(th_pers & mc$election_year < 2005),
      "25c TH: no losing candidate named in the public file, and no candidate before 2005 (TLS reports)",
      sprintf("25c TH: %d losing candidates and %d candidates before 2005 carry a name or gender in the public file",
              sum(th_pers & !(mc$is_winner %in% TRUE)), sum(th_pers & mc$election_year < 2005)))
# 25d reads its name fixtures from a gitignored file next to the reports: no name
# from them may appear in tracked code (TLS, 12.10.2026).
fx_th <- "data/mayoral_elections/raw/thueringen/tls_wahlberichte_2000_2004/audit_fixtures_restricted.csv"
if (!is.null(mcr) && file.exists(fx_th)) {
  fx <- read.csv(fx_th, colClasses = "character", encoding = "UTF-8")
  ok <- vapply(seq_len(nrow(fx)), function(i) {
    w <- mcr %>% filter(ags == fx$ags[i], election_date == as.Date(fx$election_date[i]), is_winner %in% TRUE)
    nrow(w) == 1 && identical(w$candidate_last_name, fx$last_name[i]) &&
      (!nzchar(fx$first_name[i]) || identical(w$candidate_first_name, fx$first_name[i])) &&
      (!nzchar(fx$votes_hw[i]) || isTRUE(w$candidate_votes_hw == as.numeric(fx$votes_hw[i]))) &&
      (!nzchar(fx$votes_sw[i]) || isTRUE(w$candidate_votes_sw == as.numeric(fx$votes_sw[i])))
  }, logical(1))
  check(nrow(fx) == 5 && all(ok),
        "25d TH restricted fixtures: 5 winners 2000/2004 (Berlingerode name pin, Geismar runoff pairing, Ellrich runoff paired by elimination, Jena OB 2000, Tautenburg spelling confirmed by the TLS)",
        sprintf("25d TH restricted fixtures: %s wrong", paste(fx$ags[!ok], fx$election_date[!ok], collapse = "; ")))
  # Uhlstädt-Kirchhasel 2002: table 3.4 prints the runoff's own date; the winner's
  # two rounds are one row (they were split into a first-round-only and a
  # runoff-only row before 2026-10-12)
  uk <- mcr %>% filter(ags == "16073109", election_year == 2002)
  check(sum(uk$is_winner %in% TRUE) == 1 && sum(!is.na(uk$election_date_sw) & uk$is_winner %in% TRUE) == 1 &&
          nrow(uk) == 3,
        "25d TH: Uhlstädt-Kirchhasel 2002 runoff winner paired with the first round (3 rows)",
        sprintf("25d TH: Uhlstädt-Kirchhasel 2002 has %d rows, %d winners", nrow(uk), sum(uk$is_winner %in% TRUE)))
} else {
  warn("25d TH restricted: mayoral_candidates_restricted.rds or the fixture file absent")
}
wo <- m %>% filter(state == "16", election_year <= 2004, is.na(valid_votes), !is.na(winner_votes))
check(nrow(wo) == 63,
      "25e TH: 63 winner-only elections 1999-2004 (TLS 20421 table 3.4; 3 of them runoffs dated by the TLS)",
      sprintf("25e TH: %d winner-only elections 1999-2004, expected 63", nrow(wo)))
sw00 <- m %>% filter(state == "16", election_date == as.Date("2000-05-28"), round == "stichwahl")
hw00 <- m %>% filter(state == "16", election_date == as.Date("2000-05-14"), round == "hauptwahl")
check(nrow(sw00) == 25 + 3 && sum(!sw00$ags %in% hw00$ags) == 5,
      "25f TH 2000: 25 Bürgermeister + 3 OB runoffs; 5 runoff-only (first round in no source)",
      sprintf("25f TH 2000: %d runoffs, %d without a first round", nrow(sw00), sum(!sw00$ags %in% hw00$ags)))
lc00 <- lc %>% filter(state == "16", election_date == as.Date("2000-05-28"))
check(n_distinct(lc00$ags) == 3 && sum(lc00$is_winner %in% TRUE) == 3 &&
        all(is.na(lc00$candidate_name)) && all(is.na(lc00$candidate_last_name)),
      "25g TH Landrat 2000: 3 runoffs, one winner each, nobody named in the public file",
      "25g TH Landrat 2000: runoffs missing, winners wrong or a candidate named")
ann_path <- "data/mayoral_elections/final/mayor_panel_annual.rds"
ann <- readRDS(ann_path) %>% filter(state == "16", election_year <= 2004)
check(nrow(ann) == 0,
      "25h TH annual panel: no term from 1994-2004 (their names are withheld)",
      sprintf("25h TH annual panel: %d rows from 1994-2004 terms", nrow(ann)))
check(sum(mp$state == "16" & mp$election_year <= 2004) == 0 && sum(mp$state == "16") == 644,
      "25i TH panel: 644 terms, none from 1994-2004 (as published before the TLS reports)",
      sprintf("25i TH panel: %d terms, %d from 1994-2004", sum(mp$state == "16"),
              sum(mp$state == "16" & mp$election_year <= 2004)))
if (!is.null(mcr)) {
  tw <- mcr %>% filter(state == "16", election_year %in% c(2000, 2004))
  check(sum(!is.na(tw$candidate_name) & !(tw$is_winner %in% TRUE)) > 300 &&
          sum(!is.na(tw$candidate_name) & tw$is_winner %in% TRUE) > 900,
        sprintf("25j TH restricted: %d named winners and %d named losing candidates 2000/2004",
                sum(!is.na(tw$candidate_name) & tw$is_winner %in% TRUE),
                sum(!is.na(tw$candidate_name) & !(tw$is_winner %in% TRUE))),
        "25j TH restricted: the 2000/2004 names are missing")
} else {
  warn("25j TH restricted: mayoral_candidates_restricted.rds absent (scientific-use twin not built here)")
}
cyc <- function(a) mc %>% filter(ags == a, election_year <= 2005)
le <- cyc("16073106"); ko <- cyc("16073042")
rn <- mc %>% filter(paste(ags, election_date, election_date_sw) %in%
                      c("16063001 2004-09-26 2004-10-10", "16076004 2003-01-12 2003-01-26",
                        "16076075 1999-06-13 1999-06-27"))
check(n_distinct(le$election_date) == 1 && le$election_date[1] == as.Date("2003-06-15") &&
        sum(le$is_winner %in% TRUE) == 1 &&
        n_distinct(ko$election_date) == 1 && ko$election_date[1] == as.Date("2003-09-07") &&
        nrow(rn) == 3 && all(rn$is_winner %in% TRUE) &&
        identical(sort(rn$candidate_votes_sw), c(46, 1081, 1262)) &&
        sum(mc$ags == "16063001" & mc$election_date == as.Date("2004-06-27") &
              mc$is_winner %in% TRUE) == 0,
      "25k TH dates corrected by the TLS (12.10.2026): Leutenberg once, 15.06.2003; Königsee 07.09.2003; runoffs Andenhausen 26.09/10.10.2004 (apart from the failed 27.06.2004), Berga/Elster 12.01/26.01.2003, Teichwolframsdorf 13.06/27.06.1999",
      "25k TH: a date the TLS corrected is wrong, an election is duplicated or a runoff is missing")
if (!is.null(mcr)) {
  # No public Wahlvorschlag label of a Thüringen candidate named by the TLS
  # reports (pre-2005, 06.06.2010) carries that candidate's surname (or a
  # 4+-letter start of it) from the restricted twin ("BWV <Surname>", Bretleben 2004)
  key <- c("ags", "election_date", "candidate_votes_hw", "candidate_votes_sw")
  # Lindig 2004 keeps the database's own label (wahlen.thueringen.de prints a
  # list named after its candidate; known issues §16), so it is exempt here
  tls_yr <- function(d) d$election_year < 2005 | d$election_date == as.Date("2010-06-06")
  lab <- mc %>% filter(state == "16", tls_yr(.), !is.na(candidate_party),
                       !(ags == "16074052" & election_date == as.Date("2004-06-27"))) %>%
    select(all_of(key), candidate_party) %>%
    inner_join(mcr %>% filter(state == "16", tls_yr(.), !is.na(candidate_last_name)) %>%
                 select(all_of(key), candidate_last_name), by = key, relationship = "many-to-many")
  hit <- mapply(function(p, s) {
    s <- tolower(gsub("[^[:alpha:]]", "", s)); toks <- tolower(gsub("[^[:alpha:]]", "", strsplit(p, " +")[[1]]))
    any(nchar(toks) >= 4 & (startsWith(s, toks) | startsWith(toks, s)))
  }, lab$candidate_party, lab$candidate_last_name)
  check(nrow(lab) > 500 && !any(hit),
        sprintf("25l TH: no public Wahlvorschlag label of 1994-2004 or 06.06.2010 names its candidate (%d labels checked)", nrow(lab)),
        sprintf("25l TH: %d public labels of 1994-2004/2010 carry the candidate's surname: %s", sum(hit),
                paste(unique(lab$candidate_party[hit]), collapse = ", ")))
}

# Majority rule (2026-10-13, 00_th_tls_reports_parse.py decide_majority_rule):
# a Thüringen mayor needs an absolute majority, except in the 2009-2010 window
# without runoffs (TLS 29926: "genügte eine einfache Mehrheit"). Calibrated on
# the build before the rule: 25m found the 14 cycles outside the window seated
# below 50 % without a runoff, and 25n failed on the 4 window elections won by
# the pooled write-ins and the 4 seated although never accepted.
th_cyc <- mc %>%
  filter(state == "16", election_type %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl")) %>%
  group_by(ags, election_date) %>%
  summarise(sw = any(has_stichwahl %in% TRUE), flag = any(flag_decisive_round_missing %in% TRUE),
            top = suppressWarnings(max(candidate_voteshare_hw, na.rm = TRUE)),
            pooled_wins = any(is_winner %in% TRUE & candidate_party %in% "Weitere Personen"),
            .groups = "drop") %>%
  mutate(window = election_date >= as.Date("2009-05-01") & election_date <= as.Date("2010-07-31"))
no_major <- th_cyc %>% filter(!window, !sw, !flag, is.finite(top), top < 0.5)
check(nrow(no_major) == 0 && sum(th_cyc$flag & th_cyc$election_date < as.Date("2009-05-01") &
                                   th_cyc$election_date > as.Date("1993-12-31") &
                                   format(th_cyc$election_date, "%Y-%m-%d") != "2004-06-27") == 14,
      "25m TH: outside 2009-2010 every cycle without a runoff is won by a majority; the 14 first rounds without one seat nobody",
      sprintf("25m TH: %d cycles outside 2009-2010 seat a winner below 50 %% without a runoff: %s", nrow(no_major),
              paste(no_major$ags, no_major$election_date, collapse = ", ")))
win <- th_cyc %>% filter(window)
check(!any(win$sw) && !any(win$pooled_wins) &&
        setequal(paste(win$ags, win$election_date)[win$flag],
                 c("16073046 2010-06-06", "16074077 2010-06-06", "16075066 2010-06-06",
                   "16064009 2010-02-21", th_neuwahl_2010)),
      sprintf("25n TH 2009-2010 window: %d cycles, no runoff, the top named candidate wins (never the pooled write-ins); the 6 elections never accepted seat nobody", nrow(win)),
      "25n TH 2009-2010 window: a runoff, a pooled write-in winner, or a wrong set of not-accepted elections")

# The 689 Bürgermeisterwahlen of 6 June 2010 (TLS 29926 table 3.3; the database
# holds 389). Stage 0 asserts that they reconcile with the report's table 3.2
# and that the rule seats the report's elected person in each; here: coverage,
# one winner per accepted election, the 5 Neuwahlen seat nobody, and no name
# from the report in the public file (all in the restricted twin). Calibrated:
# 25o fails on the build before the ingest (389 cycles, none named in the twin).
th10 <- mc %>% filter(state == "16", election_date == as.Date("2010-06-06")) %>%
  group_by(ags) %>%
  summarise(flag = any(flag_decisive_round_missing %in% TRUE), n_w = sum(is_winner %in% TRUE),
            named = any(!is.na(candidate_name) | !is.na(candidate_last_name)), .groups = "drop")
check(nrow(th10) == 689 && sum(th10$flag) == 5 && all(th10$n_w[!th10$flag] == 1) &&
        all(th10$n_w[th10$flag] == 0) && !any(th10$named) &&
        n_distinct(m$ags[m$state == "16" & m$election_date == as.Date("2010-06-06")]) == 689,
      "25o TH 2010: 689 Bürgermeisterwahlen on 06.06.2010 (TLS 29926; database 389), one winner each but the 5 Neuwahlen, nobody named in the public file",
      sprintf("25o TH 2010: %d cycles, %d flagged, %d accepted without one winner, %d named in the public file",
              nrow(th10), sum(th10$flag), sum(!th10$flag & th10$n_w != 1), sum(th10$named)))
# TLS 29926 table 4.1 (every mayor in office at the end of 2010) names the
# mayors elected on other dates 2005-2010: 168 on database rounds (names in the
# twin only) and 95 winner-only elections in no other source, 8 of them filed as
# runoffs by the election calendar. It also settles Bad Berka's tied runoff of
# 21.05.2006 (1459:1459, decided by lot): the Einzelbewerber won, whose two
# rounds are one row. Calibrated: 25p fails on the build before (0 winner-only
# elections 2005-2010; Bad Berka seated the FWG candidate, 6 rows).
wo41 <- m %>% filter(state == "16", election_year >= 2005, election_year <= 2010, is.na(valid_votes),
                     !is.na(winner_votes))
bb06 <- mc %>% filter(ags == "16071003", election_year == 2006)
bbw <- bb06 %>% filter(is_winner %in% TRUE)
th_pub <- mc %>% filter(state == "16", election_year >= 2005, election_year <= 2019,
                        !is.na(candidate_name) | !is.na(candidate_last_name))
check(nrow(wo41) == 95 && sum(wo41$round == "stichwahl") == 8 &&
        nrow(bb06) == 5 && nrow(bbw) == 1 && isTRUE(bbw$candidate_party == "Einzelbewerber") &&
        isTRUE(bbw$candidate_votes_hw == 1118) && isTRUE(bbw$candidate_votes_sw == 1459) &&
        all(th_pub$election_type == "Oberbürgermeisterwahl"),
      "25p TH 2005-2010 (TLS 29926 table 4.1): 95 winner-only elections (8 runoffs); Bad Berka 2006 tie won by the Einzelbewerber (one row); no Bürgermeister of 2005-2019 named in the public file",
      sprintf("25p TH table 4.1: %d winner-only elections 2005-2010 (%d runoffs); Bad Berka 2006 %d rows, winner %s; %d public non-OB names 2005-2019",
              nrow(wo41), sum(wo41$round == "stichwahl"), nrow(bb06), paste(bbw$candidate_party, collapse = "/"),
              sum(th_pub$election_type != "Oberbürgermeisterwahl")))
if (!is.null(mcr)) {
  tw41 <- mcr %>% filter(state == "16", election_year >= 2005, election_year <= 2010,
                         election_type == "Bürgermeisterwahl", is_winner %in% TRUE, !is.na(candidate_last_name))
  check(nrow(tw41) >= 684 + 168 + 95,
        sprintf("25p TH restricted: %d named Bürgermeister winners 2005-2010 in the twin", nrow(tw41)),
        sprintf("25p TH restricted: only %d named Bürgermeister winners 2005-2010", nrow(tw41)))
}
if (!is.null(mcr)) {
  tw10 <- mcr %>% filter(state == "16", election_date == as.Date("2010-06-06"), is_winner %in% TRUE)
  check(nrow(tw10) == 684 && all(!is.na(tw10$candidate_last_name)),
        "25o TH 2010 restricted: all 684 elected persons of 06.06.2010 named in the twin",
        sprintf("25o TH 2010 restricted: %d winners, %d unnamed", nrow(tw10), sum(is.na(tw10$candidate_last_name))))
}

# ============================================================================
cat("\n26. Panel: municipality lineages, term ends, person links (October 2026)\n")
# 03_mayor_panel.R computes person keys, term counts, the previous/next election
# and annual term ends per LINEAGE (a chain of renumbered codes); a term ends at
# the next election (named or not), when the municipality ceases to exist, or at
# the longest legal term. Calibrated: every check fails on the 2026-10-08 build
# (2,556 terms over 12 years, Chemnitz's mayor two people, no
# was_predecessor_mayor, 548 more TH comparisons in the harm panel, the four NI
# elections typed SG).
ni_typed <- mc %>%
  filter(paste(ags, election_date) %in% c("03358008 2006-09-10", "03251037 2019-05-26",
                                         "03352011 2019-05-26", "03357039 2021-09-12",
                                         "03351404 2014-05-25", "03257026 2024-06-09")) %>%
  distinct(ags, election_type)
check(nrow(ni_typed) == 6 &&
        all(ni_typed$election_type[ni_typed$ags %in% c("03358008", "03251037", "03352011", "03357039")] ==
              "Bürgermeisterwahl") &&
        all(ni_typed$election_type[ni_typed$ags %in% c("03351404", "03257026")] == "SG-Bürgermeisterwahl") &&
        sum(paste(mp$ags, mp$election_date) %in%
              c("03358008 2006-09-10", "03251037 2019-05-26", "03352011 2019-05-26",
                "03357039 2021-09-12")) == 4,
      "26a NI: Bad Fallingbostel 2006, Stuhr + Cuxhaven 2019, Rotenburg (Wümme) 2021 are Bürgermeisterwahlen in the panel; SG Wathlingen 2014 + Nienstädt 2024 stay SG",
      "26a NI: a town election is typed SG-Bürgermeisterwahl (a candidate's Beruf), or a Samtgemeinde election lost its type")
ann_all <- readRDS("data/mayoral_elections/final/mayor_panel_annual.rds")
tl <- ann_all %>% count(person_id, ags, election_date, state, election_year)
long <- tl %>% filter(n > 12)
check(nrow(long) == 6 && all(long$state == "09" & long$election_year < 1995),
      "26b annual panel: no term longer than 12 years except 6 Bayern terms of 1948-1976 (legal maximum before 1995 not checked)",
      sprintf("26b annual panel: %d terms longer than 12 years (%d outside Bayern before 1995)",
              nrow(long), sum(!(long$state == "09" & long$election_year < 1995))))
lud <- mp %>% filter(ags %in% c("14161000", "14511000"), election_date %in% as.Date(c("2006-06-11", "2013-06-16")))
lud_end <- ann_all %>% filter(ags == "14161000", election_date == as.Date("2006-06-11"))
check(nrow(lud) == 2 && n_distinct(lud$person_id) == 1 &&
        lud$term_number[lud$ags == "14511000"] == 2 && lud$is_incumbent[lud$ags == "14511000"] == 1 &&
        lud$next_runs_again[lud$ags == "14161000"] == 1 && max(lud_end$year) == 2012,
      "26c renumbering: Chemnitz OB Ludwig 2006 (14161000) and 2013 (14511000) is one person, 2nd term; the 2006 term ends 2012",
      "26c renumbering: Chemnitz 14161000 -> 14511000 splits its mayor or runs the 2006 term on")
lb <- mp %>% filter(ags %in% c("14521350", "14521355"), election_date %in% as.Date(c("2008-06-08", "2013-04-14")))
lb_end <- ann_all %>% filter(ags == "14521350", election_date == as.Date("2008-06-08"))
check("was_predecessor_mayor" %in% names(mp) && nrow(lb) == 2 && n_distinct(lb$person_id) == 1 &&
        all(lb$person_id_method == "merger_name_link") &&
        lb$was_predecessor_mayor[lb$ags == "14521355"] == 1 && lb$term_number[lb$ags == "14521355"] == 1 &&
        max(lb_end$year) == 2012,
      "26d merger: Kunzmann, Lauter/Sa. 2008 -> Lauter-Bernsbach 2013 is one person (merger_name_link), first term there with was_predecessor_mayor = 1; Lauter's term ends 2012",
      "26d merger: the Lauter -> Lauter-Bernsbach mayor is split, mis-counted or Lauter's term runs on")
if ("was_predecessor_mayor" %in% names(mp)) {
  # Brandenburg's merger links (B VII 3-6 reports, 1998 -> 2003 Gemeinde
  # mergers) are pinned separately in 27i.
  mp_nb <- mp %>% filter(state != "12")
  # 165 -> 164 terms, 112 -> 109 people (2026-10-12 afternoon): three Sachsen-
  # Anhalt village elections of 2008/2010 filed under their successor's code
  # (Lagendorf, Gieseritz, Uchtdorf) moved to their own code (30e); two by-
  # elections the year dedup had dropped entered (Lützen 2010, Könnecke).
  check(all(mp$was_predecessor_mayor %in% 0:1) && sum(mp_nb$was_predecessor_mayor) == 164 &&
          all(mp$person_id_method[mp$was_predecessor_mayor == 1] %in%
                c("merger_name_link", "candidate_name_variant_link")) &&
          n_distinct(mp_nb$person_id[mp_nb$person_id_method == "merger_name_link"]) == 109,
        "26e was_predecessor_mayor: 164 terms of people linked across a merger (109 merger_name_link people + 1 whose name-variant link takes precedence; the 5 Thüringen 1999-2004 terms went with the TLS names)",
        sprintf("26e was_predecessor_mayor (outside BB): %d rows, %d merger-linked people",
                sum(mp_nb$was_predecessor_mayor), n_distinct(mp_nb$person_id[mp_nb$person_id_method == "merger_name_link"])))
} else {
  check(FALSE, "", "26e was_predecessor_mayor: column missing from mayor_panel")
}
mph <- readRDS("data/mayoral_elections/final/mayor_panel_harm.rds")
th_nra <- c(unharm = sum(mp$state == "16" & !is.na(mp$next_runs_again)),
            harm = sum(mph$state == "16" & !is.na(mph$next_runs_again)))
check(th_nra[["unharm"]] == th_nra[["harm"]],
      sprintf("26f gap rule: TH next_runs_again defined on %d rows in both mayor_panel and mayor_panel_harm", th_nra[["unharm"]]),
      sprintf("26f gap rule: TH next_runs_again defined on %d rows in mayor_panel but %d in mayor_panel_harm (harm recompute drops the gap rule)",
              th_nra[["unharm"]], th_nra[["harm"]]))
cap_end <- ann_all %>%
  filter(paste(ags, election_date) %in% c("06431014 2017-09-24", "05112000 2009-08-30")) %>%
  group_by(ags) %>% summarise(last = max(year), .groups = "drop")
check(nrow(cap_end) == 2 && cap_end$last[cap_end$ags == "06431014"] == 2023 &&
        cap_end$last[cap_end$ags == "05112000"] == 2015,
      "26g legal maximum: Lautertal (Odenwald) 2017 ends 2023 (HSL lacks its 2023 election), Duisburg OB 2009 ends 2015 (off-cycle elections not in the IT.NRW files)",
      "26g legal maximum: a term without its next election in the source runs past the legal maximum")

# ============================================================================
cat("\n27. Brandenburg 1993-2024 from the Statistische Berichte B VII 3-6 (October 2026)\n")
# 00_bb_afs_reports_parse.py: hauptamtliche elections 1993-2008 and the
# ehrenamtliche Bürgermeister of the general Kommunalwahl days 1998-2024, mostly
# winner-only. Calibrated: on the outputs before this change 27a-27e and 27g
# fail (Brandenburg starts in 2010 and has no ehrenamtliche mayor).
BB27_MIN_E <- 2590; BB27_MIN_H <- 194; BB27_FAILED <- 36; BB27_MIN_PANEL <- 1959; BB27_MIN_LOSERS <- 178
bb_c <- mc %>% filter(state == "12")
bb_e <- bb_c %>% filter(office_type == "Ehrenamtliche(r) Bürgermeister*in")
check(n_distinct(paste(bb_e$ags, bb_e$election_date)) >= BB27_MIN_E &&
        all(c(1998, 2003, 2008, 2014, 2019, 2024) %in% bb_e$election_year),
      sprintf("27a BB: %d ehrenamtliche elections (general days 1998-2024, 1993 runoffs)",
              n_distinct(paste(bb_e$ags, bb_e$election_date))),
      sprintf("27a BB: %d ehrenamtliche elections, years %s",
              n_distinct(paste(bb_e$ags, bb_e$election_date)),
              paste(sort(unique(bb_e$election_year)), collapse = ",")))
bb_h_old <- bb_c %>% filter(election_year < 2010, office_type != "Ehrenamtliche(r) Bürgermeister*in")
check(n_distinct(paste(bb_h_old$ags, bb_h_old$election_date)) >= BB27_MIN_H &&
        min(bb_c$election_year) == 1993,
      sprintf("27b BB: %d hauptamtliche/OB elections 1993-2009, series from 1993",
              n_distinct(paste(bb_h_old$ags, bb_h_old$election_date))),
      sprintf("27b BB: %d hauptamtliche/OB elections before 2010, first year %d",
              n_distinct(paste(bb_h_old$ags, bb_h_old$election_date)), min(bb_c$election_year)))
# 27c: since 2026-10-19 every Brandenburg loser is restricted, whatever the
# source (user decisions 2026-10-16/19; 01b withheld_loser_states): no name and
# no field derived from one on a non-winner row. Calibrated: fails on the
# 2026-10-19 morning build, whose workbook rows named 756 losers.
bb_losers <- bb_c %>% filter(!(is_winner %in% TRUE))
bb_leaks <- sapply(intersect(st_personal, names(bb_losers)), function(cl) {
  v <- bb_losers[[cl]]
  sum(!is.na(v) & nzchar(trimws(as.character(v))))
})
check(sum(bb_leaks) == 0 &&
        all(is.na(bb_c$candidate_name[bb_c$ags == "12071392" & bb_c$election_year == 2024])),
      sprintf("27c BB: none of the %d losing-candidate rows (all sources, all years) carries a name or a name-derived field (nor the declined Tschernitz 2024 winner)",
              nrow(bb_losers)),
      sprintf("27c BB: personal data on losing candidates in the public file - %s",
              paste(sprintf("%s=%d", names(bb_leaks)[bb_leaks > 0], bb_leaks[bb_leaks > 0]),
                    collapse = ", ")))
bw_at <- function(a, d) bb_c %>% filter(ags == a, election_date == as.Date(d), is_winner %in% TRUE)
f1 <- bw_at("12053000", "1993-12-05"); f2 <- bw_at("12052000", "2002-02-24")
f3 <- bw_at("12051000", "2003-10-26"); f4 <- bw_at("12054000", "1998-09-27")
f5 <- bw_at("12060012", "2003-10-26")
check(nrow(f1) == 1 && f1$candidate_last_name == "Pohl" && f1$candidate_votes_hw == 18785 &&
        nrow(f2) == 1 && f2$candidate_last_name == "Rätzel" && f2$candidate_votes_sw == 23352 &&
        nrow(f3) == 1 && f3$candidate_last_name == "Tiemann" && f3$candidate_votes_hw == 14527 &&
        f3$candidate_votes_sw == 15643 &&
        nrow(f4) == 1 && f4$candidate_last_name == "Platzeck" && f4$candidate_votes_hw == 51905 &&
        nrow(f5) == 1 && f5$candidate_last_name == "Ortlieb" &&
        f5$office_type == "Ehrenamtliche(r) Bürgermeister*in" && is.na(f5$n_candidates_hw),
      "27d BB fixtures: Frankfurt (Oder) 1993 Pohl 18785; Cottbus 2002 runoff Rätzel 23352; Brandenburg a. d. H. 2003 Tiemann 14527 -> 15643; Potsdam 1998 Platzeck 51905; Althüttendorf 2003 Ortlieb (ehrenamtlich, winner-only)",
      "27d BB fixtures: a 1993-2003 winner, vote count or office changed")
if (!"flag_elected_by_council" %in% names(bb_c)) bb_c$flag_elected_by_council <- NA
fl <- bb_c %>% filter(flag_decisive_round_missing %in% TRUE, !(flag_elected_by_council %in% TRUE))
check(n_distinct(paste(fl$ags, fl$election_date)) == BB27_FAILED && !any(fl$is_winner %in% TRUE),
      sprintf("27e BB: %d elections seated nobody (1993 runoffs below the quorum or tied, 32 rejected single candidates and the Neuhardenberg runoff of 1998, Tschernitz 2024 declined)", BB27_FAILED),
      sprintf("27e BB: %d flagged elections (expected %d) or a flagged one has a winner",
              n_distinct(paste(fl$ags, fl$election_date)), BB27_FAILED))
# Mayors the Gemeindevertretung chose (2003: 5 + 15 former Amtsdirektoren; 2008:
# 2; 2024: 4) are cycles in mayoral_candidates (flag_elected_by_council, no
# directly elected winner, no public name) and no round in mayoral_unharm; they
# end the previous mayor's term (Schilda: the 2019 mayor until 2023, not 2025).
# Calibrated: fails on the 2026-10-12 morning build (council cycles left out).
if (!"flag_elected_by_council" %in% names(bb_c)) bb_c$flag_elected_by_council <- NA
co <- bb_c %>% filter(flag_elected_by_council %in% TRUE)
sch <- readRDS("data/mayoral_elections/final/mayor_panel_annual.rds") %>%
  filter(ags == "12062440", election_year == 2019)
# Since 2026-10-19 27 cycles: the archived result pages (section 35) add Rosenau
# 2014, whose single candidate was rejected, and replace the B VII 3-6 council
# record of Pinnow 2008 by its archived Hauptwahl. A failed popular vote keeps
# its rounds in mayoral_unharm, without a winner; a council record alone does not.
co_u <- bb_m %>% semi_join(co %>% distinct(ags, election_date), by = c("ags", "election_date"))
check(n_distinct(paste(co$ags, co$election_date)) == 27 && all(co$flag_decisive_round_missing) &&
        !any(co$is_winner %in% TRUE) && all(is.na(co$candidate_name)) &&
        setequal(paste(co_u$ags, co_u$election_date), c("12069537 2014-05-25", "12073440 2008-09-28")) &&
        all(is.na(co_u$winner_party)) &&
        nrow(sch) > 0 && max(sch$year) == 2023,
      "27f BB: 27 mayors chosen by the council are flagged cycles (nobody named; only the failed popular votes of Rosenau 2014 and Pinnow 2008 in unharm, without a winner) and end the previous term (Schilda 2019 mayor until 2023)",
      "27f BB: council cycles missing, named, in mayoral_unharm with a winner, or the previous term runs on")
# 27j: no duplicate round, the Neuenhagen 1993 runoff under Neuenhagen b. Berlin,
# the result-less first rounds of winner-only runoff cycles, and a runoff cycle
# filed under its first round (Potsdam OB 2002)
pd02 <- bb_c %>% filter(ags == "12054000", election_year == 2002, is_winner %in% TRUE)
bb_sw <- bb_m %>% filter(round == "stichwahl") %>% select(ags, sw_date = election_date)
shell <- bb_m %>% filter(round == "hauptwahl", is.na(valid_votes), is.na(winner_votes), is.na(eligible_voters)) %>%
  inner_join(bb_sw, by = "ags", relationship = "many-to-many") %>%
  filter(as.numeric(sw_date - election_date) >= 7, as.numeric(sw_date - election_date) <= 42) %>%
  distinct(ags, election_date, winner_party)
check(!anyDuplicated(as.data.frame(bb_m)[, c("ags", "election_date", "round")]) &&
        nrow(bb_m %>% filter(ags == "12064336", election_year == 1993)) == 1 &&
        nrow(bb_m %>% filter(ags == "12064332", election_year == 1993)) == 0 &&
        # 225 until 2026-10-16; the 2004-2013 result reports (section 34) replaced
        # three winner-only runoff cycles of 2008 by full results and added one
        # result-less first round (Schwarzheide 28.09.2008); 223 until 2026-10-19,
        # when the archived result pages (section 35) gave 46 of them their result
        nrow(shell) == 177 && all(is.na(shell$winner_party)) &&
        nrow(pd02) == 1 && pd02$election_date == as.Date("2002-09-22") &&
        pd02$election_date_sw == as.Date("2002-10-27"),
      sprintf("27j BB: unique rounds; Neuenhagen b. Berlin 1993 runoff (12064336); %d result-less first rounds of winner-only runoff cycles; Potsdam OB 2002 filed under 22.09.", nrow(shell)),
      "27j BB: a duplicate round, the Neuenhagen 1993 runoff on the village, or a runoff cycle filed under its runoff")
# 27m: the tracked, name-stripped copy of the workbook input
# (bb_lwl_parsed_public.csv, written by 01b; the full file is gitignored) names
# only people the public file names in the same Gemeinde, has the full file's
# rows, and gives every named row of the full file a pairing key.
bb_pub_in <- "data/mayoral_elections/raw/brandenburg/bb_lwl_parsed_public.csv"
bb_full_in <- "data/mayoral_elections/raw/brandenburg/bb_lwl_parsed.csv"
name_tokens <- function(x) vapply(strsplit(tolower(trimws(x)), "[[:space:],]+"),
                                  function(t) paste(sort(t[nzchar(t)]), collapse = " "), "")
if (file.exists(bb_pub_in)) {
  pin <- read.csv(bb_pub_in, colClasses = "character", na.strings = character(0),
                  check.names = FALSE, encoding = "UTF-8")
  pin_named <- pin[nzchar(pin$candidate_name), ]
  bb_named <- bb_c %>% filter(!is.na(candidate_name))
  unknown <- !(paste(pin_named$ags, name_tokens(pin_named$candidate_name)) %in%
                 paste(bb_named$ags, name_tokens(bb_named$candidate_name)))
  full_rows <- if (file.exists(bb_full_in)) {
    nrow(read.csv(bb_full_in, colClasses = "character", na.strings = character(0)))
  } else nrow(pin)
  check(sum(unknown) == 0 && nrow(pin) == full_rows && nrow(pin_named) > 0 &&
          !any(nzchar(pin$candidate_last_name) & !nzchar(pin$candidate_name)),
        sprintf("27m BB tracked input copy: %d rows, %d named, every name one the public file shows in that Gemeinde",
                nrow(pin), nrow(pin_named)),
        sprintf("27m BB tracked input copy: %d names the public file does not show, or %d rows vs %d in the full file",
                sum(unknown), nrow(pin), full_rows))
} else {
  check(FALSE, "27m BB tracked input copy present",
        "27m BB: bb_lwl_parsed_public.csv is missing (run 01b with the full bb_lwl_parsed.csv)")
}
# 27n: a Brandenburg loser's own-name Wahlvorschlag label ("EB Muster", invented)
# is the plain list type in the public file (user decision 2026-10-19), in
# mayoral_candidates and in mayoral_unharm's winner_party of a Hauptwahl whose
# leader lost the runoff; the restricted twin keeps it as printed. Calibrated:
# fails on the 2026-10-19 midday build (229 own-name labels).
source("code/shared/bb_own_name_label.R")
tw27 <- "data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds"
if (file.exists(tw27)) {
  twc <- readRDS(tw27)
  same_rows <- nrow(twc) == nrow(mc) && identical(twc$ags, mc$ags) && identical(twc$election_date, mc$election_date) &&
    identical(twc$candidate_votes_hw, mc$candidate_votes_hw)
  i27 <- which(twc$state %in% "12" & !(twc$is_winner %in% TRUE))
  want <- bb_generic_own_label(twc$candidate_party[i27], twc$candidate_last_name[i27], twc$candidate_first_name[i27])
  bad_c <- sum(coalesce(mc$candidate_party[i27], "") != coalesce(want, ""))
  n_gen <- sum(coalesce(want, "") != coalesce(twc$candidate_party[i27], ""))
  # Hauptwahl leaders who lost the runoff: their label is unharm's winner_party
  lead <- twc[i27, ] %>% filter(candidate_rank_hw %in% 1, has_stichwahl %in% TRUE) %>%
    mutate(want = bb_generic_own_label(candidate_party, candidate_last_name, candidate_first_name)) %>%
    filter(want != candidate_party) %>% select(ags, election_date, want)
  um <- bb_m %>% filter(round == "hauptwahl") %>% inner_join(lead, by = c("ags", "election_date"))
  # the counts are pinned for a build with the gitignored workbook; a clone
  # builds the twin from the name-stripped copy and checks the rule only
  full27 <- file.exists("data/mayoral_elections/raw/brandenburg/bb_lwl_parsed.csv")
  check(same_rows && (n_gen == 228 || !full27) && bad_c == 0 && (nrow(um) == 7 || !full27) &&
          all(um$winner_party == um$want),
        sprintf("27n BB: %d own-name labels of losing candidates are the plain list type in the public file (%d of them unharm winner_party of a Hauptwahl whose leader lost the runoff)",
                n_gen, nrow(um)),
        sprintf("27n BB: %d losing candidates' labels differ from the rule, or %d unharm Hauptwahl labels of a beaten leader keep the name (rows aligned: %s)",
                bad_c, sum(um$winner_party != um$want), same_rows))
} else {
  warn("27n BB: mayoral_candidates_restricted.rds absent (scientific-use twin not built here)")
}
# 27k: generic list labels compare equal in the panel (1993 "Einzelwahlvorschlag"
# vs later "EB"): Luckau's mayor re-elected 2001 is no party switch
lk <- mp %>% filter(ags == "12061320", election_year == 2001)
check(nrow(lk) == 1 && lk$party_switch == 0,
      "27k BB panel: Luckau 1993 -> 2001 re-election is no party switch (Einzelwahlvorschlag = EB)",
      "27k BB panel: a generic list label reads as a party switch")
check(sum(mp$state == "12" & mp$election_year < 2010) >= BB27_MIN_PANEL,
      sprintf("27g BB panel: %d terms before 2010", sum(mp$state == "12" & mp$election_year < 2010)),
      "27g BB panel: the pre-2010 terms are missing")
if ("was_predecessor_mayor" %in% names(mp)) {
  mpb <- mp %>% filter(state == "12")
  # 219 / 83 until 2026-10-16; the 2004-2013 result reports add 4 terms of 3
  # people (Groß Leine 1998 -> Märkische Heide 2006/2022, Rehfeld 1998 ->
  # Falkenberg/Elster 2009, Mühlberg 1993/1998 -> Mühlberg/Elbe 2008; same names);
  # the archived result pages (section 35) add two re-elections of two of them
  # (Falkenberg/Elster 2017, Mühlberg/Elbe 2016)
  check(sum(mpb$was_predecessor_mayor) == 225 &&
          n_distinct(mpb$person_id[mpb$person_id_method == "merger_name_link"]) == 85,
        "27i BB panel: 225 terms linked across the 2001-2003 Gemeinde mergers, 85 merger_name_link people (5 more carry a name-variant link; e.g. Bindow -> Heidesee)",
        sprintf("27i BB panel: %d merger-linked terms, %d merger_name_link people (expected 225 / 85)",
                sum(mpb$was_predecessor_mayor),
                n_distinct(mpb$person_id[mpb$person_id_method == "merger_name_link"])))
}
if (file.exists(tw_th)) {
  twb <- readRDS(tw_th) %>% filter(state == "12", election_year < 2010, !(is_winner %in% TRUE))
  check(sum(!is.na(twb$candidate_name)) >= BB27_MIN_LOSERS,
        sprintf("27h BB restricted: %d named losing candidates 1993-2003", sum(!is.na(twb$candidate_name))),
        "27h BB restricted: the 1993-2003 losing candidates' names are missing")
} else {
  warn("27h BB restricted: mayoral_candidates_restricted.rds absent (scientific-use twin not built here)")
}

# ============================================================================
cat("\n28. Pinned name variants of one person (person_name_variant_links.csv, October 2026)\n")
# 36 pairs in which sources or report years spell one mayor differently
# (Horts/Horst, Clauß/Claus, a married name, added given names, a 1993 record
# with the surname only). 03 merges them after numbering, so no other id moves.
# Calibrated: before the change every pair held two person_ids. The 37th pair
# (Gutendorf, TLS reports only) went on 12.10.2026 with the TLS names.
vpairs <- read.csv("data/mayoral_elections/metadata/person_name_variant_links.csv",
                   colClasses = "character", encoding = "UTF-8")
mcw <- mc %>% filter(is_winner %in% TRUE) %>%
  select(ags, election_date, candidate_last_name)
mpn <- mp %>% left_join(mcw, by = c("ags", "election_date"))
n_one <- sum(vapply(seq_len(nrow(vpairs)), function(i) {
  ids <- mpn$person_id[mpn$ags == vpairs$ags[i] &
                         mpn$candidate_last_name %in% c(vpairs$alias_last[i], vpairs$canonical_last[i])]
  length(ids) >= 2 && n_distinct(ids) == 1
}, logical(1)))
check(nrow(vpairs) == 36 && n_one == 36,
      "28a every pinned name-variant pair is one person in mayor_panel (36/36)",
      sprintf("28a %d of %d pinned name-variant pairs share one person_id", n_one, nrow(vpairs)))
nv <- mp %>% filter(person_id_method == "candidate_name_variant_link")
# 70 -> 69 people, 179 -> 176 terms (2026-10-12): the alias Kerber Heinrich =
# Jörg Heinrich (Ellefeld) was father and son, so it went (30c).
check(n_distinct(nv$person_id) == 69 && nrow(nv) == 176,
      "28b candidate_name_variant_link: 69 people (33 from the August 2026 alias table + 36 pinned pairs), 176 terms",
      sprintf("28b candidate_name_variant_link: %d people, %d terms (expected 69 / 176)",
              n_distinct(nv$person_id), nrow(nv)))
ch <- mp %>% filter(ags == "12060045", election_year <= 2019) %>% arrange(election_date)
sa <- mp %>% filter(ags == "05382056", election_year == 2025)
bo <- mp %>% filter(ags == "15362038", election_year == 2005)
check(nrow(ch) == 5 && n_distinct(ch$person_id) == 1 && identical(as.integer(ch$term_number), 1:5) &&
        nrow(sa) == 1 && sa$term_number == 2 && sa$is_incumbent == 1 &&
        nrow(bo) == 1 && bo$term_number == 2 && bo$consecutive_terms == 1,
      "28c fixtures: Chorin Horts 1998 / Horst 2003-2019 one person (terms 1-5); Sankt Augustin 2025 Leitterstorf re-elected (term 2); Böhnke 2005 returns (term 2, consecutive 1)",
      "28c fixtures: a pinned name-variant person is split or mis-counted")

# ============================================================================
cat("\n29. Fixes of the October 2026 audit (2026-10-12)\n")
# Calibrated: 29a-29e fail on the build of 2026-10-12 morning.
pid <- function(a, d) mp$person_id[mp$ags == a & mp$election_date == as.Date(d)]
# 29a: a renumbering link needs a compatible given name -- father Lothar and son
# Lars Naumann, and Ulrich and Uwe Riedel, share surname, initial and lineage
check(length(pid("14182060", "2001-06-10")) == 1 && length(pid("14522060", "2008-06-08")) == 1 &&
        pid("14182060", "2001-06-10") != pid("14522060", "2008-06-08") &&
        pid("14178660", "2001-06-10") != pid("14523410", "2022-06-12"),
      "29a panel: Naumann Lothar 2001 / Lars 2008 and Riedel Ulrich 2001 / Uwe 2022 are different people",
      "29a panel: a father/son or namesake pair shares one person_id again")
# 29b: an election filed under a code whose crosswalk ends earlier keeps its
# term (Dessau OB 2007: 2007-2014, not one year)
dess <- readRDS("data/mayoral_elections/final/mayor_panel_annual.rds") %>%
  filter(ags == "15101000", election_year == 2007)
check(nrow(dess) >= 7,
      sprintf("29b annual panel: Dessau OB 2007 term has %d years (code ended 2007)", nrow(dess)),
      sprintf("29b annual panel: Dessau OB 2007 term cut to %d year(s)", nrow(dess)))
# 29c: Bayern register repairs -- a final "l" read as "!" (three names), a name
# pinned after the 2026 file (Tüßling), Pforzen 2014/2020 withheld. Run on the
# restricted twin (the register names are not public; fixtures in the gitignored
# file next to the registers).
if (file.exists(tw_path) && !is.null(by_fx_all)) {
  tw29 <- readRDS(tw_path) %>% filter(state == "09", is_winner %in% TRUE)
  bw <- function(a, d) tw29$candidate_last_name[tw29$ags == a & tw29$election_date == as.Date(d)]
  fx29 <- by_fx_all %>% filter(check == "29c")
  ok29 <- vapply(seq_len(nrow(fx29)), function(i) identical(bw(fx29$ags[i], fx29$date[i]), fx29$last_name[i]),
                 logical(1))
  check(nrow(fx29) == 4 && all(ok29) &&
          all(is.na(bw("09777158", "2014-03-16"))) && all(is.na(bw("09777158", "2020-03-15"))),
        "29c BY restricted: register repairs hold (three final-l names, Tüßling pinned), Pforzen 2014/2020 unnamed (register vs 2026 file)",
        "29c BY restricted: a register name is truncated, mis-split, or Pforzen 2014/2020 is named")
} else {
  warn("29c BY restricted: twin or fixture file absent (scientific-use files not built here)")
}
# 29d: mayoral_harm flags -- Groß Glienicke's 1998 runoff in Potsdam's row set;
# ten Friedland villages that each elected a mayor on one day; Bad Salzungen
# 2000 counts only one of its two predecessors; one name per 2021 unit
hm <- readRDS("data/mayoral_elections/final/mayoral_harm.rds")
hr <- function(a, d, r) hm[hm$ags == a & hm$election_date == as.Date(d) & hm$round == r, ]
check(all(c("flag_runoff_other_predecessor", "flag_partial_counts") %in% names(hm)) &&
        hr("12054000", "1998-10-11", "stichwahl")$flag_runoff_other_predecessor == 1 &&
        hr("12054000", "1998-09-27", "hauptwahl")$flag_multi_winner == 1 &&
        hr("12067137", "1998-09-27", "hauptwahl")$flag_multi_winner == 1 &&
        hr("16063003", "2000-05-14", "hauptwahl")$flag_partial_counts == 1 &&
        all(tapply(hm$ags_name, hm$ags, function(x) length(unique(x))) == 1),
      sprintf("29d harm: %d runoffs of a non-dominant predecessor flagged (Potsdam 1998); flag_multi_winner by predecessor (Friedland 1998); %d partial-count rows; one name per 2021 unit",
              sum(hm$flag_runoff_other_predecessor), sum(hm$flag_partial_counts)),
      "29d harm: a runoff of another predecessor, a multi-mayor or a partial-count row is unflagged, or a unit has two names")
# 29e: no public file names a withheld Thüringen TLS person through a pinned
# variant pair (the Gutendorf pair went with the TLS names)
check(nrow(vpairs) == 36 && !any(substr(vpairs$ags, 1, 2) == "16"),
      "29e variant pairs: no Thüringen pair (TLS names are withheld)",
      "29e variant pairs: a Thüringen pair is pinned")

# ============================================================================
cat("\n30. Pre-existing panel and source defects fixed after the audit (2026-10-12)\n")
# Calibrated: 30a-30f fail on the build of 2026-10-12 afternoon (before the fixes).
pid30 <- function(a, d) mp$person_id[mp$ags == a & mp$election_date == as.Date(d)]
# 30a: the panel keeps every election cycle. Two records of one municipality
# less than 60 days apart are one cycle; a later election of the same year (a
# by-election, a repeat) is kept -- Lützen 2010: Reichel in May, Könnecke in
# December. Bayern rounds that every source row marks superseded are dropped
# (Tittling 2011); a same-day decisive ballot is not (Würzburg 1948).
gap_lt60 <- mp %>% arrange(ags, election_date) %>% group_by(ags) %>%
  summarise(n = sum(diff(as.numeric(election_date)) < 60), .groups = "drop")
two_in_year <- mp %>% filter(state != "06") %>% count(ags, election_year) %>% filter(n > 1)
check(sum(gap_lt60$n) == 0 && nrow(two_in_year) >= 100 &&
        length(pid30("15084315", "2010-05-09")) == 1 && length(pid30("15084315", "2010-11-28")) == 1 &&
        pid30("15084315", "2010-05-09") != pid30("15084315", "2010-11-28") &&
        length(pid30("09275152", "2011-11-13")) == 0 && length(pid30("09663000", "1948-07-09")) == 1,
      sprintf("30a panel: one row per election cycle, %d municipality-years with two elections kept (Lützen 2010); superseded rounds out (Tittling 2011), Würzburg 1948 in",
              nrow(two_in_year)),
      sprintf("30a panel: %d rows < 60 days apart, %d municipality-years with two elections, or a Lützen/Tittling/Würzburg fixture fails",
              sum(gap_lt60$n), nrow(two_in_year)))
# 30b: Bayern -- consecutive first-Amtsantritt keys of one Gemeinde that are one
# person are merged (the 2026 file dates a re-elected mayor's first Amtsantritt
# a few days off); an appointed 1945 mayor and the 1946 elected one stay two.
byw <- mc %>% filter(state == "09", is_winner %in% TRUE, !is.na(candidate_last_name)) %>%
  mutate(sur = gsub("[^a-z]", "", tolower(iconv(candidate_last_name, to = "ASCII//TRANSLIT"))),
         fst = gsub("[^a-z]", "", tolower(iconv(sub(" .*$", "", sub("^((Dr|Prof)\\.? ?)+", "", coalesce(candidate_first_name, ""))), to = "ASCII//TRANSLIT"))),
         byr = candidate_birth_year)
byw <- bind_rows(byw %>% transmute(ags, election_date, sur, fst, byr),
                 byw %>% filter(!is.na(election_date_sw)) %>% transmute(ags, election_date = election_date_sw, sur, fst, byr)) %>%
  distinct(ags, election_date, .keep_all = TRUE)
byp <- mp %>% filter(state == "09") %>% select(person_id, ags, election_date) %>%
  left_join(byw, by = c("ags", "election_date")) %>% arrange(ags, election_date)
by_persons <- byp %>% group_by(ags, person_id) %>%
  summarise(first = min(election_date),
            sur_f = dplyr::first(na.omit(sur)), fst_f = dplyr::first(na.omit(fst)), byr_f = dplyr::first(na.omit(byr)),
            sur_l = dplyr::last(na.omit(sur)), fst_l = dplyr::last(na.omit(fst)), byr_l = dplyr::last(na.omit(byr)),
            .groups = "drop") %>%
  arrange(ags, first) %>% group_by(ags) %>%
  mutate(same_as_prev = !is.na(sur_f) & sur_f == lag(sur_l) & nzchar(fst_f) & fst_f == lag(fst_l) &
           (is.na(byr_f) | is.na(lag(byr_l)) | byr_f == lag(byr_l))) %>% ungroup()
n_same <- sum(by_persons$same_as_prev, na.rm = TRUE)
check(n_same == 0 &&
        n_distinct(c(pid30("09278179", "2020-03-15"), pid30("09278179", "2026-03-08"))) == 1 &&
        n_distinct(c(pid30("09376175", "2023-03-12"), pid30("09376175", "2026-03-08"))) == 1 &&
        n_distinct(c(pid30("09183144", "2020-03-15"), pid30("09183144", "2026-03-08"))) == 1 &&
        n_distinct(c(pid30("09263000", "1945-07-03"), pid30("09263000", "1946-05-26"))) == 2 &&
        sum(mp$person_id_method == "first_amtsantritt_merge") > 0,
      sprintf("30b BY: no two consecutive people of one Gemeinde with one name; re-elected 2026 incumbents keep their id (Rattiszell, Falter, Kamhuber; %d merged rows); Straubing 1945/46 two people",
              sum(mp$person_id_method == "first_amtsantritt_merge")),
      sprintf("30b BY: %d consecutive same-name people, or a 2026 incumbent / Straubing fixture fails", n_same))
# 30c: one name key, two people -- split on a given-name AND gender or birth-year
# conflict (Hotopp Angelika/Andy, Hampel Ingo/Ingeburg) or a reviewed pin
# (metadata/person_split_pins.csv: Vieker father/son, Kerber father/son, Hotopp,
# and Hampel, confirmed as two people by the StaLA on 16.10.2026)
spins <- read.csv("data/mayoral_elections/metadata/person_split_pins.csv", colClasses = "character", encoding = "UTF-8")
pins_ok <- all(vapply(seq_len(nrow(spins)), function(i) {
  a <- spins$ags[i]; d <- as.Date(spins$second_person_from[i])
  earlier <- mp$person_id[mp$ags == a & mp$election_date < d]
  length(pid30(a, d)) == 1 && !(pid30(a, d) %in% earlier)
}, logical(1)))
check(nrow(spins) == 4 && pins_ok &&
        pid30("15355016", "1994-06-12") != pid30("15355016", "2001-05-06") &&
        pid30("15266037", "1994-06-12") != pid30("15266037", "2001-05-06") &&
        pid30("05770008", "2020-09-13") == pid30("05770008", "2025-09-14"),
      sprintf("30c panel: %d pinned splits hold, %d people split off a name key (Vieker, Kerber, Hotopp, Hampel)",
              nrow(spins), n_distinct(mp$person_id[mp$person_id_method == "candidate_name_split"])),
      "30c panel: a pinned or gender/birth-year split is undone")
# 30d: stable ids -- the registry's active rows are exactly the panel's
# person-elections, and a retired id is never reissued
reg30 <- read.csv("data/mayoral_elections/metadata/person_id_registry.csv", colClasses = "character")
act30 <- reg30[reg30$active == "TRUE", ]
check(setequal(paste(act30$person_id, act30$ags, act30$election_date),
               paste(mp$person_id, mp$ags, format(mp$election_date))) &&
        !anyDuplicated(act30[c("ags", "election_date")]) &&
        !length(intersect(reg30$person_id[reg30$active == "FALSE"], mp$person_id)) &&
        all(substr(reg30$person_id, 3, 4) == substr(reg30$ags, 1, 2)),
      sprintf("30d registry: %d active rows = mayor_panel; %d retired ids not reissued",
              nrow(act30), n_distinct(reg30$person_id[reg30$active == "FALSE"])),
      "30d registry: person_id_registry.csv and mayor_panel disagree, or a retired id is reused")
# 30e: Sachsen-Anhalt -- every Gemeinde under its own AGS. The source filed some
# elections under a merger partner's code, on the same day (Heideloh/
# Großzöberitz, Cosa/Diebzig, Dannefeld/Dambeck) or another day (Liesten and
# Wieblitz-Eversdorf 2008 under Salzwedel); their mayors were unseated or
# collapsed into one cycle.
st2 <- mc %>% filter(state == "15") %>% group_by(ags, election_date) %>%
  summarise(n = n_distinct(ags_name), .groups = "drop") %>% filter(n > 1)
stw <- function(a, d, nm) any(mc$ags == a & mc$election_date == as.Date(d) & mc$is_winner %in% TRUE &
                               mc$candidate_last_name %in% nm) && length(pid30(a, d)) == 1
check(nrow(st2) == 0 && !("flag_shared_ags" %in% names(mc) && any(mc$flag_shared_ags %in% c(1, TRUE))) &&
        stw("15154013", "2001-05-06", "Blaha") && stw("15159006", "2001-06-10", "Feuerborn") &&
        stw("15370022", "1994-06-12", "Blankau") && stw("15081325", "2008-07-06", "Boesenhagen") &&
        stw("15081565", "2008-06-29", "Jürges"),
      "30e ST: no AGS holds two Gemeinden on one day; Blaha, Feuerborn, Blankau, Boesenhagen (Liesten 2008), Jürges (Wieblitz-Eversdorf 2008) seated and in the panel",
      sprintf("30e ST: %d AGS-days hold two Gemeinden, or a re-coded mayor is unseated", nrow(st2)))
# 30f: Mecklenburg-Vorpommern -- Wismar 2026 from the city's official result;
# Rügen Landrat May 2001 failed its Ja/Nein runoff (25 % quorum) and seats nobody
wis <- m %>% filter(ags == "13074087", election_year == 2026)
rug <- lc %>% filter(ags == "13061000", election_date == as.Date("2001-05-06"))
rug9 <- lc %>% filter(ags == "13061000", election_date == as.Date("2001-09-02"), is_winner %in% TRUE)
check(nrow(wis) == 2 && identical(wis$winner_votes[wis$round == "stichwahl"], 11785) &&
        length(pid30("13074087", "2026-04-12")) == 1 &&
        nrow(rug) >= 4 && all(is.na(rug$is_winner)) && nrow(rug9) == 1 &&
        all(is.na(l$winner_party[l$ags == "13061000" & l$election_date >= as.Date("2001-05-01") &
                                   l$election_date <= as.Date("2001-06-30")])),
      "30f MV: Wismar 2026 (Hauptwahl + Stichwahl, Junge 11,785) in unharm and panel; Rügen 5/2001 seats nobody, 9/2001 seats one",
      "30f MV: Wismar 2026 missing or Rügen 5/2001 seats a Landrat")

# ============================================================================
cat("\n31. Rheinland-Pfalz: 2026 edition of the StaLA workbook, source corrections (October 2026)\n")
# Calibrated: 31a-31d fail on the build of 2026-10-12 (before the 2026 edition).
rp_u <- bind_rows(m, l) %>% filter(state == "07")
rp_c <- bind_rows(mc, lc) %>% filter(state == "07")
rpd <- function(a, d, r = "hauptwahl") rp_u[rp_u$ags == a & rp_u$election_date == as.Date(d) & rp_u$round == r, ]
# 31a: the eight Verbandsgemeinde elections held with the Landtagswahl, which the
# source types as text dates, are on 27.03.2011 (read as NA, they had taken the
# previous election's date, 07.11.2010); every RLP round is on a Sunday
lw11 <- c("07013102", "07013504", "07014005", "07033103", "07033106", "07033302", "07033601", "07033705")
check(all(vapply(lw11, function(a) nrow(rpd(a, "2011-03-27")) == 1, logical(1))) &&
        !any(rp_u$ags %in% lw11 & rp_u$election_date == as.Date("2010-11-07")) &&
        all(format(rp_u$election_date, "%u") == "7"),
      "31a RLP: the eight VG elections of the Landtagswahl day are on 2011-03-27; every round is on a Sunday",
      "31a RLP: a Landtagswahl-day VG election is misdated or a round is not on a Sunday")
# 31b: cell corrections pinned in metadata/rlp_source_corrections.csv
rpins <- read.csv("data/mayoral_elections/metadata/rlp_source_corrections.csv", colClasses = "character", encoding = "UTF-8")
kob <- rp_c %>% filter(ags == "07111000", election_date == as.Date("2017-09-24"), candidate_last_name == "Langner")
check(nrow(rpd("07338000", "2017-03-05")) == 1 && nrow(rpd("07337000", "2017-03-05")) == 0 &&
        nrow(rpd("07033610", "2017-06-11")) == 1 && nrow(rpd("07013809", "2017-09-24")) == 1 &&
        nrow(rpd("07033106", "2018-10-28")) == 1 && nrow(rpd("07033106", "2019-10-28")) == 0 &&
        nrow(kob) == 1 && abs(kob$candidate_voteshare_hw - 0.4282) < 1e-9 && abs(kob$candidate_voteshare_sw - 0.698) < 1e-9 &&
        identical(rpd("07033207", "2025-06-15")$winner_party, "SPD") &&
        identical(rp_c$candidate_last_name[rp_c$ags == "07033207" & rp_c$election_date == as.Date("2025-06-15") & rp_c$is_winner %in% TRUE], "Krauß") &&
        nrow(rpd("07033608", "2018-05-06", "stichwahl")) == 1 &&
        nrow(rpd("07141000", "2022-03-13")) == 1 && nrow(rpd("07141000", "2022-01-23")) == 0 &&
        !any(grepl("Dennighoff", rp_c$candidate_name)) &&
        sum(rp_c$ags == "07137003" & rp_c$election_date == as.Date("2022-11-13")) == 4,
      sprintf("31b RLP: %d pinned source corrections hold (Körner 2017 in the Rhein-Pfalz-Kreis, Kusel-Altenglan and Rengsdorf-Waldbreitbach codes, Wörrstadt 2018, Koblenz 2017 shares, Leiningerland 2025 Krauß, Lauterecken-Wolfstein 2018 runoff, Denninghoff 13.03.2022, Andernach 2022 four candidates)", nrow(rpins)),
      "31b RLP: a pinned source correction is missing")
# 31c: 35 elections from the 2026 edition: 34 after the 2025 edition's last day
# (29.06.2025) and Vordereifel 2024, winner-only; a runoff cycle has a
# result-less Hauptwahl row
add_c <- rp_c %>% filter(election_date > as.Date("2025-06-29") | (ags == "07013703" & election_date == as.Date("2024-04-07")))
wo <- rp_c %>% filter(ags == "07314000", election_date == as.Date("2025-09-21"))
check(nrow(add_c) == 35 && all(add_c$is_winner %in% TRUE) && all(!is.na(add_c$candidate_last_name)) &&
        sum(add_c$candidate_gender == "w") == 7 &&
        nrow(wo) == 1 && wo$candidate_last_name == "Blettner" && is.na(wo$candidate_voteshare_hw) &&
        abs(wo$candidate_voteshare_sw - 0.585) < 1e-9 &&
        nrow(rpd("07143000", "2026-08-30", "stichwahl")) == 1 && rpd("07143000", "2026-08-16")$winner_party %in% NA,
      sprintf("31c RLP: 35 elections from the 2026 edition (to %s), winner-only, 7 women; Ludwigshafen 2025 and Westerwaldkreis 2026 runoffs",
              format(max(rp_u$election_date))),
      sprintf("31c RLP: %d elections from the 2026 edition (expected 35), or a winner-only runoff is malformed", nrow(add_c)))
# 31d: the panel holds the new Oberbürgermeister and verbandsfreie Bürgermeister
mp7 <- mp %>% filter(state == "07")
check(nrow(mp7) >= 205 && any(mp7$ags == "07319000" & mp7$election_date == as.Date("2026-09-13")) &&
        n_distinct(mp7$person_id[mp7$ags == "07111000" & mp7$election_year %in% c(2017, 2025)]) == 1,
      sprintf("31d panel: %d RLP person-elections; Worms 2026 in; Langner (Koblenz 2017, 2025) one person", nrow(mp7)),
      "31d panel: an RLP 2025/2026 election is missing or Langner is split")

# ============================================================================
cat("\n32. Fixes from Hanno's data audit of 2026-10-12 (2026-10-13)\n")
# 32a: the Thüringen database lists Eisenach's OB election of 2018 under today's
# code (16063105, Wartburgkreis since 01.07.2021) as a Bürgermeisterwahl; the OB
# file holds it under the code of election day (16056000). th_ob_overlap.R drops
# the database copy (MAY-11).
eis_u <- m %>% filter(ags %in% c("16056000", "16063105"), election_year == 2018)
eis_c <- mc %>% filter(ags %in% c("16056000", "16063105"), election_year == 2018)
check(nrow(eis_u) == 2 && all(eis_u$ags == "16056000") &&
        all(eis_u$election_type == "Oberbürgermeisterwahl") &&
        nrow(eis_c) > 0 && all(eis_c$ags == "16056000"),
      "32a TH: Eisenach 2018 only as the OB election of 16056000 (both rounds), not again under 16063105",
      sprintf("32a TH: Eisenach 2018 rows: %s",
              paste(eis_u$ags, eis_u$election_type, eis_u$round, collapse = "; ")))
# 32b: no Thüringen round of 1,000+ valid votes appears under two codes
th_fp <- m %>%
  filter(state == "16", !is.na(valid_votes), valid_votes >= 1000) %>%
  group_by(election_date, round, valid_votes, winner_votes) %>%
  summarise(n_ags = n_distinct(ags), ags = paste(unique(ags), collapse = "/"), .groups = "drop") %>%
  filter(n_ags > 1)
check(nrow(th_fp) == 0,
      "32b TH: no round of 1,000+ valid votes under two codes",
      sprintf("32b TH: identical results under two codes: %s",
              paste(th_fp$ags, th_fp$election_date, collapse = "; ")))


# ============================================================================
cat("\n33. Published CSVs are complete (2026-10-13)\n")
# On 2026-10-13 mayor_panel_annual.csv ended in ~16 KB of NUL bytes (about the
# last 100 rows lost; cause unknown) while its .rds was intact. Every check above
# reads the .rds, so only this one looks at the published CSVs: no NUL byte, and
# as many data rows as the .rds. Calibrated on the damaged copy (16,384 NUL
# bytes; fread reads 248,118 rows against the .rds's 248,254).
csv_complete <- function(f) {
  rds <- sub("[.]csv$", ".rds", f)
  n_nul <- sum(readBin(f, "raw", file.info(f)$size) == as.raw(0))
  n_csv <- tryCatch(nrow(data.table::fread(f, showProgress = FALSE)), error = function(e) -1L)
  n_rds <- nrow(readRDS(rds))
  list(ok = n_nul == 0 && n_csv == n_rds,
       msg = sprintf("%s (NUL %d, csv %d, rds %d)", basename(f), n_nul, n_csv, n_rds))
}
csv_files <- list.files(c("data/mayoral_elections/final", "data/landrat_elections/final"),
                        pattern = "[.]csv$", full.names = TRUE)
csv_files <- csv_files[file.exists(sub("[.]csv$", ".rds", csv_files))]
csv_res <- lapply(csv_files, csv_complete)
csv_bad <- vapply(csv_res[!vapply(csv_res, `[[`, logical(1), "ok")], `[[`, character(1), "msg")
check(length(csv_files) >= 9 && length(csv_bad) == 0,
      sprintf("33 published CSVs: %d files without NUL bytes, row counts equal to their .rds", length(csv_files)),
      sprintf("33 published CSVs damaged: %s", paste(csv_bad, collapse = "; ")))

# ============================================================================
cat("\n34. Brandenburg hauptamtliche 2004-2013 from the Landeswahlleiter's result reports (October 2026)\n")
# 00_bb_lwl_scans_parse.py: scans of the Gemeinden's Ergebnismeldungen (LWL,
# 15.10.2026), full results with every Bewerber. Calibrated: on the build before
# this change 34a, 34b and 34d fail (4 winner-only hauptamtliche elections
# 2004-2009, Belzig 2008 without counts, no Templin 2010) and 34e fails (no
# named loser 2004-2009 in the twin).
bb34 <- mc %>% filter(state == "12", office_type != "Ehrenamtliche(r) Bürgermeister*in")
h34 <- bb34 %>% filter(election_year %in% 2004:2009)
# a runoff cycle filed under a first round without result (Schwarzheide 2008)
# has no first-round field: n_candidates_hw is NA there by design
shell34 <- is.na(h34$candidate_votes_hw) & !is.na(h34$candidate_votes_sw)
check(n_distinct(paste(h34$ags, h34$election_date)) >= 85 &&
        all(!is.na(h34$n_candidates_hw[!(h34$flag_decisive_round_missing %in% TRUE) & !shell34])) &&
        all(is.na(h34$n_candidates_hw[shell34])) && all(!is.na(h34$n_candidates_sw[shell34])),
      sprintf("34a BB: %d hauptamtliche/OB elections 2004-2009, all with full results (%d runoff cycles without a first-round result, n_candidates_hw NA)",
              n_distinct(paste(h34$ags, h34$election_date)), n_distinct(paste(h34$ags, h34$election_date)[shell34])),
      sprintf("34a BB: %d hauptamtliche/OB elections 2004-2009 (expected >= 85), one without its candidate list, or a first round without result given a count",
              n_distinct(paste(h34$ags, h34$election_date))))
# both rounds add up: first round against the cycle's valid votes, runoff against
# the runoff row of mayoral_unharm
sw34 <- m %>% filter(state == "12", round == "stichwahl") %>% select(ags, election_date_sw = election_date, valid_sw = valid_votes)
vi34 <- bb34 %>% filter(election_year %in% 2004:2012, !is.na(n_candidates_hw) | !is.na(n_candidates_sw)) %>%
  left_join(sw34, by = c("ags", "election_date_sw")) %>%
  group_by(ags, election_date) %>%
  summarise(ok_hw = all(is.na(candidate_votes_hw)) | n() == 1 | is.na(first(valid_votes)) |
              sum(candidate_votes_hw, na.rm = TRUE) == first(valid_votes),
            ok_sw = all(is.na(candidate_votes_sw)) | sum(candidate_votes_sw, na.rm = TRUE) == first(valid_sw),
            .groups = "drop")
check(nrow(vi34) >= 150 && all(vi34$ok_hw) && all(vi34$ok_sw),
      sprintf("34b BB: candidate votes = valid votes in both rounds of all %d hauptamtliche cycles 2004-2012", nrow(vi34)),
      sprintf("34b BB: %d first rounds and %d runoffs of %d hauptamtliche cycles 2004-2012 do not add up",
              sum(!vi34$ok_hw), sum(!vi34$ok_sw), nrow(vi34)))
# no losing Bewerber of the result reports is named in the public file (they
# reach the restricted twin only, as the B VII 3-6 losers)
sc_file <- "data/mayoral_elections/raw/brandenburg/bb_lwl_scans_parsed.csv"
if (file.exists(sc_file)) {
  sc34 <- data.table::fread(sc_file, colClasses = "character") %>% as_tibble() %>%
    distinct(ags, election_date = as.Date(election_date))
  pub34 <- mc %>% semi_join(sc34, by = c("ags", "election_date"))
  check(nrow(distinct(pub34, ags, election_date)) >= 115 &&
          !any(!(pub34$is_winner %in% TRUE) & !is.na(pub34$candidate_name)) &&
          all((pub34 %>% group_by(ags, election_date) %>% summarise(w = sum(is_winner %in% TRUE), .groups = "drop"))$w == 1),
        sprintf("34c BB: %d result-report cycles, one named winner each, no losing Bewerber named in the public file",
                nrow(distinct(pub34, ags, election_date))),
        "34c BB: a losing Bewerber of the result reports is named in the public file, or a cycle has no single winner")
} else {
  warn("34c BB: bb_lwl_scans_parsed.csv absent (result reports not built here)")
}
# fixtures: Belzig 2008 electorate corrected from the misprinted 5 779; Templin
# 2010 is one row per person although both the winner's given names and the
# runner-up's Träger label differ between the rounds (pair_key); Schwarzheide
# 2008 is a runoff cycle filed under its 28.09. first round; Kremmen 2008 has
# both rounds in full (the B VII 3-6 report had the winner only)
bz <- m %>% filter(ags == "12069020", election_date == as.Date("2008-09-28"))
tp <- mc %>% filter(ags == "12073572", election_date == as.Date("2010-02-28"))
sz <- mc %>% filter(ags == "12066296", election_date == as.Date("2008-09-28"), is_winner %in% TRUE)
kr <- mc %>% filter(ags == "12065165", election_date == as.Date("2008-09-28"))
# Schöneiche bei Berlin 2012: first round 22.04. from the LWL's archived result page
sb <- mc %>% filter(ags == "12067440", election_date == as.Date("2012-04-22"))
check(nrow(bz) == 1 && bz$eligible_voters == 9779 &&
        nrow(tp) == 8 && sum(tp$is_winner %in% TRUE) == 1 &&
        tp$candidate_votes_hw[tp$is_winner %in% TRUE] == 2261 && tp$candidate_votes_sw[tp$is_winner %in% TRUE] == 4285 &&
        nrow(sz) == 1 && sz$election_date_sw == as.Date("2008-10-12") && sz$candidate_votes_sw == 1477 &&
        is.na(sz$n_candidates_hw) &&
        nrow(kr) == 4 && sum(!is.na(kr$candidate_votes_sw)) == 2 &&
        nrow(sb) == 6 && all(sb$n_candidates_hw == 6) && sum(sb$is_winner %in% TRUE) == 1 &&
        sb$election_date_sw[sb$is_winner %in% TRUE] == as.Date("2012-05-13") &&
        sb$candidate_votes_hw[sb$is_winner %in% TRUE] == 1379 &&
        sb$candidate_votes_sw[sb$is_winner %in% TRUE] == 2931,
      "34d BB fixtures: Belzig 2008 9 779 Wahlberechtigte; Templin 2010 8 Bewerber, winner 2261 -> 4285; Schwarzheide 2008 runoff 1477 under 28.09. (first round without result, n_candidates_hw NA); Kremmen 2008 both rounds; Schöneiche 2012 6 Bewerber 22.04., the runner-up 1379 wins the runoff of 13.05. with 2931",
      "34d BB fixtures: a 2004-2013 result-report election changed")
if (file.exists(tw_th) && file.exists(sc_file)) {
  # every losing Bewerber of the result reports (2004-2012) keeps the name in the twin
  tw34 <- readRDS(tw_th) %>% filter(state == "12", !(is_winner %in% TRUE)) %>%
    semi_join(sc34, by = c("ags", "election_date"))
  check(nrow(tw34) >= 290 && all(!is.na(tw34$candidate_name)),
        sprintf("34e BB restricted: all %d losing Bewerber of the result reports named in the twin", nrow(tw34)),
        sprintf("34e BB restricted: %d losing Bewerber of the result reports, %d without a name in the twin",
                nrow(tw34), sum(is.na(tw34$candidate_name))))
} else {
  warn("34e BB restricted: mayoral_candidates_restricted.rds absent (scientific-use twin not built here)")
}

# ---------------------------------------------------------------------------
# 35. Brandenburg 2001-2017 from the Landeswahlleiter's archived result pages
# ---------------------------------------------------------------------------
# 00_bb_lwl_archive_parse.py (October 2026) reads the LWL's old result pages
# (wahlen.brandenburg.de/bmwahlen/, Internet Archive): 411 B VII 3-6 elections
# held winner-only get their counts and every Bewerber, 7 elections are added,
# and the council record of Pinnow 2008 gets its failed popular vote. Calibrated:
# 35a-35c fail on the 2026-10-19 build without the archive file (2 901 rounds
# without counts; Lychen 2002 one misdated round; Forst 2015, Guben 2016, Rosenau
# 2014 missing).
bb_m35 <- bb_m %>% mutate(.y = format(election_date, "%Y"))
nc <- table(bb_m35$.y[is.na(bb_m35$valid_votes)])
check(sum(is.na(bb_m35$valid_votes)) == 2448 && nc[["2014"]] == 13 && nc[["2008"]] == 181 &&
        nc[["2001"]] == 34 && nc[["2002"]] == 20 && nc[["2003"]] == 336,
      "35a BB: 2 448 rounds without counts (2014: 13, 2008: 181, 2001-2003: 390), down from 2 901 before the archived result pages",
      sprintf("35a BB: %d rounds without counts (expected 2 448; by year %s)", sum(is.na(bb_m35$valid_votes)),
              paste(names(nc), nc, collapse = " ")))
ly <- bb_m %>% filter(ags == "12073384", format(election_date, "%Y") == "2002")
gu <- bb_c %>% filter(ags == "12071160", election_date == as.Date("2016-06-26"))
fo <- bb_m %>% filter(ags == "12071076", format(election_date, "%Y") == "2015")
pn <- bb_m %>% filter(ags == "12073440", election_date == as.Date("2008-09-28"))
fb <- bb_c %>% filter(ags == "12062128", election_date == as.Date("2017-09-24"), is_winner %in% TRUE)
check(setequal(paste(ly$election_date, ly$round), c("2002-04-21 hauptwahl", "2002-05-05 stichwahl")) &&
        ly$valid_votes[ly$round == "hauptwahl"] == 2090 &&
        nrow(gu) == 5 && sum(gu$is_winner %in% TRUE) == 1 && gu$candidate_votes_sw[gu$is_winner %in% TRUE] == 4679 &&
        all(gu$office_type == "Bürgermeister*in") &&
        nrow(fo) == 1 && fo$round == "stichwahl" && fo$winner_votes == 4530 &&
        nrow(pn) == 1 && pn$valid_votes == 528 && is.na(pn$winner_party) &&
        nrow(fb) == 1 && fb$candidate_votes_hw == 3238 && fb$office_type == "Bürgermeister*in",
      "35b BB archive fixtures: Lychen 2002 Hauptwahl 21.04 (2 090 gültige) + Stichwahl 05.05 (B VII 3-6 had one round on 05.05); Guben 2016 (5 Bewerber, runoff winner 4 679, hauptamtlich); Forst 2015 runoff-only (4 530); Pinnow 2008 failed Hauptwahl before the council choice (528, no winner); Falkenberg/Elster 2017 (3 238, hauptamtlich)",
      "35b BB archive fixtures: Lychen 2002, Guben 2016, Forst 2015, Pinnow 2008 or Falkenberg/Elster 2017 changed")
# 35c: every round with counts reconciles (votes, + Nein on a single-candidate
# ballot, = gültige) in the replaced and added elections
arc_keys <- bb_m %>% filter(format(election_date, "%Y") %in% c("2001", "2002", "2003", "2008", "2014", "2015", "2016", "2017"),
                            !is.na(valid_votes))
v35 <- bb_c %>% semi_join(arc_keys %>% filter(round == "hauptwahl"), by = c("ags", "election_date")) %>%
  group_by(ags, election_date) %>%
  summarise(n = n(), s = sum(candidate_votes_hw, na.rm = TRUE), v = first(valid_votes), .groups = "drop") %>%
  filter(n > 1, !is.na(v), s != v)
check(nrow(v35) == 0,
      "35c BB: every multi-candidate Hauptwahl with counts in 2001-2003 and 2008-2017 sums to its gültige Stimmen",
      sprintf("35c BB: %d Hauptwahlen whose Bewerber votes do not sum to the gültige Stimmen", nrow(v35)))

# ---------------------------------------------------------------------------
# 36. Party families (code/shared/party_family.R; TODO M22, 2026-10-19)
# ---------------------------------------------------------------------------
# candidate_party_family / party_family group the free-text Wahlvorschlag
# labels; party_switch and is_new_party_mayor compare the families. Calibrated
# on the 2026-10-19 build: before it, party_switch compared normalised labels
# and was 1 in 48.8 % of the comparable rows (17,897 of 36,642).
cat("\n36. Party families\n")
# pinned on the 2026-10-19 build
PF_NA_LABELS <- 290; PF_PANEL_DISAGREE <- 17
PF_N_SWITCH <- 8957; PF_N_CMP <- 39228; PF_N_NEW <- 11801
pf_allowed <- c("cdu_csu", "spd", "gruene", "linke_pds", "fdp", "afd", "fw_wg",
                "independent", "other", "joint_list")
pf_fix <- tribble(
  ~label, ~state, ~family,
  "CSU", NA, "cdu_csu",
  "CDU", NA, "cdu_csu",
  "SPD", NA, "spd",
  "GRÜNE", NA, "gruene",
  "Bündnis 90/Die Grünen", NA, "gruene",
  "DIE LINKE", NA, "linke_pds",
  "PDS", NA, "linke_pds",
  "F.D.P.", NA, "fdp",
  "AfD", NA, "afd",
  "FREIE WÄHLER", NA, "fw_wg",
  "Freie Wählergemeinschaft", NA, "fw_wg",
  "Parteilose Wählergruppe", NA, "fw_wg",
  "kein Wahlvorschlag", NA, "independent",
  "EB", NA, "independent",
  "Parteilos", NA, "independent",
  "Weitere Personen", NA, "independent",
  "", "15", "independent",
  "BP", NA, "other",
  "KPD", NA, "other",
  "CSU/Freie Wählergemeinschaft", NA, "joint_list",
  "SPD/Freie Wähler", NA, "joint_list",
  "Wählergr./Einzelbew.", NA, NA_character_
)
pf_bad <- pf_fix %>% rowwise() %>% mutate(
  n = sum(mc$candidate_party %in% label & (is.na(state) | mc$state %in% state)),
  wrong = sum(mc$candidate_party %in% label & (is.na(state) | mc$state %in% state) &
                coalesce(mc$candidate_party_family, "NA") != coalesce(family, "NA"))) %>%
  ungroup()
check(all(pf_bad$n > 0) && sum(pf_bad$wrong) == 0 &&
        all(is.na(mc$candidate_party_family[mc$state == "08"])),
      sprintf("36a families of %d pinned labels (%d rows; CSU, SPD, kein Wahlvorschlag, CSU/Freie Wählergemeinschaft, BP, ...; Sachsen-Anhalt's blank = independent; Baden-Württemberg NA)",
              nrow(pf_fix), sum(pf_bad$n)),
      sprintf("36a family differs from the pinned one for: %s",
              paste(pf_bad$label[pf_bad$wrong > 0 | pf_bad$n == 0], collapse = " | ")))
pf_na_lab <- sum(is.na(mc$candidate_party_family) & !is.na(mc$candidate_party) &
                   nzchar(trimws(mc$candidate_party)))
check(all(mc$candidate_party_family %in% c(pf_allowed, NA)) &&
        all(mp$party_family %in% c(pf_allowed, NA)) && pf_na_lab == PF_NA_LABELS,
      sprintf("36b family values in the allowed set; %d rows with a label but no family (mixed source categories such as NRW's \"Wählergr./Einzelbew.\")", pf_na_lab),
      sprintf("36b unexpected family values, or %d labelled rows without a family (expected %d)", pf_na_lab, PF_NA_LABELS))
pf_tw <- "data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds"
if (file.exists(pf_tw)) {
  twf <- readRDS(pf_tw)
  check(nrow(twf) == nrow(mc) && identical(twf$candidate_party_family, mc$candidate_party_family),
        "36c restricted twin and public file carry the same candidate_party_family",
        "36c candidate_party_family differs between the restricted twin and the public file")
} else {
  warn("36c mayoral_candidates_restricted.rds absent (scientific-use twin not built here)")
}
# 36d: the panel's family is the winner's in mayoral_candidates, except where
# the Bayern panel takes the label from the registers rather than the lists
pf_w <- mc %>% filter(is_winner %in% TRUE) %>%
  transmute(ags, d = election_date, cpf = candidate_party_family, d_sw = election_date_sw)
pf_w <- bind_rows(pf_w %>% select(ags, d, cpf), pf_w %>% filter(!is.na(d_sw)) %>% select(ags, d = d_sw, cpf)) %>%
  distinct(ags, d, .keep_all = TRUE)
pf_j <- mp %>% inner_join(pf_w, by = c("ags", "election_date" = "d"))
pf_dis <- pf_j %>% filter(coalesce(party_family, "NA") != coalesce(cpf, "NA"))
check(nrow(pf_j) > 48000 && all(pf_dis$state == "09") && nrow(pf_dis) <= PF_PANEL_DISAGREE,
      sprintf("36d mayor_panel party_family = the winner's candidate_party_family in %d of %d matched rows (the rest Bayern register labels)",
              nrow(pf_j) - nrow(pf_dis), nrow(pf_j)),
      sprintf("36d %d panel rows disagree with the winner's family (%s)", nrow(pf_dis),
              paste(unique(pf_dis$state), collapse = ",")))
# 36e: party_switch follows the families: within one AGS, two single-family
# winners give 0 when the family is the same and 1 when it differs ("other"
# compares the party itself; joint lists compare their members; NA after a gap)
pf_seq <- mp %>% arrange(ags, election_date) %>% group_by(ags) %>%
  mutate(pf_prev = lag(party_family)) %>% ungroup() %>%
  filter(!is.na(pf_prev), !is.na(party_family), !is.na(party_switch),
         !pf_prev %in% c("joint_list", "other"), !party_family %in% c("joint_list", "other"))
pf_viol <- sum(pf_seq$party_switch != as.integer(pf_seq$pf_prev != pf_seq$party_family))
check(nrow(pf_seq) > 30000 && pf_viol == 0,
      sprintf("36e party_switch = family change in all %d successive single-family pairs", nrow(pf_seq)),
      sprintf("36e party_switch contradicts the families in %d of %d pairs", pf_viol, nrow(pf_seq)))
check(sum(mp$party_switch %in% 1L) == PF_N_SWITCH && sum(!is.na(mp$party_switch)) == PF_N_CMP &&
        sum(mp$is_new_party_mayor %in% 1L) == PF_N_NEW,
      sprintf("36f party_switch = 1 in %d of %d comparable rows (%.1f %%); is_new_party_mayor = 1 in %d",
              PF_N_SWITCH, PF_N_CMP, 100 * PF_N_SWITCH / PF_N_CMP, PF_N_NEW),
      sprintf("36f party_switch = 1 in %d of %d rows, is_new_party_mayor = 1 in %d (expected %d / %d / %d)",
              sum(mp$party_switch %in% 1L), sum(!is.na(mp$party_switch)),
              sum(mp$is_new_party_mayor %in% 1L), PF_N_SWITCH, PF_N_CMP, PF_N_NEW))

# 36g: the election-level files carry the same families. A round's party_family
# is that of the candidate in mayoral_candidates whose label it is (the own-name
# rule needs the person: 235 rounds); mayoral_harm takes label and family from
# the same predecessor.
pf_cand <- bind_rows(
  mc %>% transmute(ags, d = election_date, lab = candidate_party, f = candidate_party_family),
  mc %>% filter(!is.na(election_date_sw)) %>%
    transmute(ags, d = election_date_sw, lab = candidate_party, f = candidate_party_family)) %>%
  distinct() %>% group_by(ags, d, lab) %>% filter(n() == 1) %>% ungroup()
pf_um <- m %>% inner_join(pf_cand, by = c("ags", "election_date" = "d", "winner_party" = "lab"))
pf_u_bad <- sum(coalesce(pf_um$party_family, "NA") != coalesce(pf_um$f, "NA"))
pf_h_bad <- sum(!paste(substr(mh$ags, 1, 2), mh$winner_party, mh$party_family) %in%
                  paste(substr(m$ags, 1, 2), m$winner_party, m$party_family))
check("party_family" %in% names(m) && "party_family" %in% names(mh) &&
        all(m$party_family %in% c(pf_allowed, NA)) && nrow(pf_um) > 55000 && pf_u_bad == 0 && pf_h_bad == 0,
      sprintf("36g mayoral_unharm party_family = the candidate's family in all %d rounds matched by label; mayoral_harm pairs label and family as unharm",
              nrow(pf_um)),
      sprintf("36g election-level families: %d unharm rounds differ from the candidate's, %d harm rows pair a label with another family",
              pf_u_bad, pf_h_bad))

cat("\n════════════════════════════════════════════════════════════════════\n")
if (failed == 0) {
  cat(sprintf("All checks passed ✓ (%d warnings)\n", warned))
  quit(status = 0)
} else {
  cat(sprintf("%d check(s) failed ✗ (%d warnings)\n", failed, warned))
  quit(status = 1)
}
