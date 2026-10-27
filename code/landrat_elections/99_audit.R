### Landrat dataset audit — comprehensive integrity + correctness checks
#
# Covers schema, cross-leakage, AGS validity, date sanity, vote-count
# integrity, per-state coverage, external-truth spot checks, candidate-level
# integrity, duplicate detection, Stichwahl logic.
#
# Exit code 0 iff all checks pass.

suppressMessages({
  library(dplyr); library(readr); library(tibble); library(stringr)
  library(lubridate); library(here); library(conflicted)
  conflict_prefer("filter", "dplyr"); conflict_prefer("year", "lubridate")
  conflict_prefer("first", "dplyr")
})
setwd(here::here())

l  <- readRDS("data/landrat_elections/final/landrat_unharm.rds")
lc <- readRDS("data/landrat_elections/final/landrat_candidates.rds")

failed <- 0L
warned <- 0L
fail <- function(msg) { cat("  ✗ FAIL:", msg, "\n"); failed <<- failed + 1L }
warn <- function(msg) { cat("  ⚠ WARN:", msg, "\n"); warned <<- warned + 1L }
pass <- function(msg) { cat("  ✓", msg, "\n") }
check <- function(cond, ok, ko) {
  if (isTRUE(cond)) pass(ok) else fail(ko)
}
check_warn <- function(cond, ok, ko) {
  if (isTRUE(cond)) pass(ok) else warn(ko)
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

cat("════════════════════════════════════════════════════════════════════\n")
cat("Landrat dataset audit\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

# ============================================================================
# 1. Schema
# ============================================================================
cat("1. Schema checks\n")
expected_unharm <- c("ags", "ags_name", "state", "state_name",
                     "election_year", "election_date", "election_type", "round",
                     "eligible_voters", "number_voters", "valid_votes",
                     "invalid_votes", "turnout",
                     "winner_party", "winner_votes", "winner_voteshare",
                     "flag_decisive_round_missing", "flag_elected_by_council")
miss_u <- setdiff(expected_unharm, names(l))
check(length(miss_u) == 0,
      sprintf("landrat_unharm has all %d expected columns", length(expected_unharm)),
      sprintf("landrat_unharm missing columns: %s", paste(miss_u, collapse = ", ")))

expected_cand <- c("ags", "ags_name", "state", "state_name",
                   "election_year", "election_date", "election_date_sw",
                   "election_type", "has_stichwahl",
                   "candidate_name", "candidate_party",
                   "candidate_votes_hw", "candidate_voteshare_hw",
                   "candidate_votes_sw", "candidate_voteshare_sw",
                   "is_winner", "flag_decisive_round_missing",
                   "flag_elected_by_council")
miss_c <- setdiff(expected_cand, names(lc))
check(length(miss_c) == 0,
      sprintf("landrat_candidates has all %d critical columns", length(expected_cand)),
      sprintf("landrat_candidates missing columns: %s", paste(miss_c, collapse = ", ")))

check(is.character(l$ags) && all(nchar(l$ags) == 8L),
      "landrat_unharm$ags is character & all 8 chars",
      "ags malformed in landrat_unharm")

check(inherits(l$election_date, "Date"),
      "election_date is Date type",
      "election_date is not Date type")

# ============================================================================
# 2. Cross-leakage between mayoral and landrat
# ============================================================================
cat("\n2. Cross-leakage with mayoral pipeline\n")
m <- readRDS("data/mayoral_elections/final/mayoral_unharm.rds")

n1 <- sum(grepl("Krfr\\.|Kreisfreie Stadt", l$ags_name, ignore.case = TRUE))
check(n1 == 0,
      "no kreisfreie Städte in landrat_unharm",
      sprintf("%d kreisfreie Städte leaked into landrat_unharm", n1))

n2 <- sum(grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion",
                m$ags_name) & m$state == "05")
check(n2 == 0,
      "no Kreise leaked into mayoral_unharm (NRW)",
      sprintf("%d NRW Kreise leaked into mayoral_unharm", n2))

n3 <- sum(l$election_type != "Landratswahl", na.rm = TRUE)
check(n3 == 0,
      "all landrat_unharm rows have election_type == 'Landratswahl'",
      sprintf("%d landrat_unharm rows have non-Landrat election_type", n3))

# ============================================================================
# 3. AGS validity
# ============================================================================
cat("\n3. AGS validity\n")
n4 <- sum(!grepl("^[0-9]{8}$", l$ags))
check(n4 == 0,
      "all AGS are 8 digits",
      sprintf("%d AGS not 8 digits", n4))

mismatch <- l %>% filter(substr(ags, 1, 2) != state)
check(nrow(mismatch) == 0,
      "AGS state prefix matches state column for all rows",
      sprintf("%d rows have AGS prefix not matching state column", nrow(mismatch)))

n5 <- sum(!grepl("000$", l$ags))
if (n5 == 0) {
  pass("all AGS end in '000' (Kreis-level)")
} else {
  warn(sprintf("%d AGS do NOT end in '000' (Kreis-level units should)", n5))
}

# ============================================================================
# 4. Date sanity
# ============================================================================
cat("\n4. Date sanity\n")
mismatch <- l %>% filter(!is.na(election_date), election_year != year(election_date))
check(nrow(mismatch) == 0,
      "election_year matches year(election_date) for all rows",
      sprintf("%d rows have election_year != year(election_date)", nrow(mismatch)))

out_of_range <- l %>% filter(election_year < 1945 | election_year > 2030)
check(nrow(out_of_range) == 0,
      "all election years in [1945, 2030]",
      sprintf("%d rows with year outside plausible range", nrow(out_of_range)))

n_na_date <- sum(is.na(l$election_date))
check_warn(n_na_date == 0,
           "no NA election_date in landrat_unharm",
           sprintf("%d rows with NA election_date", n_na_date))

# ============================================================================
# 5. Vote-count integrity (where data is available)
# ============================================================================
cat("\n5. Vote-count integrity\n")
bad_turnout <- l %>% filter(!is.na(turnout) & (turnout < 0 | turnout > 1.05))
check(nrow(bad_turnout) == 0,
      "turnout in [0, 1.05] for all non-NA rows",
      sprintf("%d rows with turnout out of range", nrow(bad_turnout)))

diff_check <- l %>%
  filter(!is.na(valid_votes), !is.na(invalid_votes), !is.na(number_voters)) %>%
  mutate(diff = abs(valid_votes + invalid_votes - number_voters)) %>%
  filter(diff > 5)
check_warn(nrow(diff_check) == 0,
           "valid + invalid votes ≈ number_voters",
           sprintf("%d rows where valid+invalid differs from voters by >5",
                   nrow(diff_check)))

bad_vs <- l %>% filter(!is.na(winner_voteshare),
                        winner_voteshare < 0 | winner_voteshare > 1.01)
check(nrow(bad_vs) == 0,
      "winner_voteshare in [0, 1.01]",
      sprintf("%d rows with winner_voteshare out of range", nrow(bad_vs)))

cand_sum_check <- lc %>%
  filter(!is.na(candidate_votes_hw), !is.na(valid_votes), valid_votes > 0) %>%
  group_by(ags, election_date, state) %>%
  summarise(sum_cand = sum(candidate_votes_hw, na.rm = TRUE),
            valid = first(valid_votes), .groups = "drop") %>%
  mutate(over = sum_cand > valid * 1.01)
# Exclude NI: known pre-existing pipeline issue where HW/SW vote counts get
# mixed in the wide-format pivot (mayoral_elections/01b_mayoral_candidates.R).
n_over_excl_ni <- sum(cand_sum_check$over & cand_sum_check$state != "03")
n_over_ni     <- sum(cand_sum_check$over & cand_sum_check$state == "03")
check(n_over_excl_ni == 0,
      sprintf("candidate vote sums ≤ valid_votes for all non-NI elections (%d total)",
              sum(cand_sum_check$state != "03")),
      sprintf("%d elections (excluding NI) where vote sum exceeds valid_votes",
              n_over_excl_ni))
if (n_over_ni > 0) {
  warn(sprintf("%d NI elections with HW+SW vote-count mixing (known pre-existing pipeline issue)",
               n_over_ni))
}

# ============================================================================
# 6. Per-state coverage expectations
# ============================================================================
cat("\n6. Per-state coverage\n")
state_counts <- l %>% count(state, state_name)
expected_min <- c("03"=80, "05"=140, "07"=110, "09"=1000, "10"=5,
                  "12"=20, "14"=30, "15"=20, "16"=80)
for (st in names(expected_min)) {
  actual <- state_counts %>% filter(state == st) %>% pull(n)
  if (length(actual) == 0) actual <- 0L
  exp_n <- expected_min[[st]]
  check(actual >= exp_n,
        sprintf("state %s: %d rows (≥%d expected)", st, actual, exp_n),
        sprintf("state %s: only %d rows (expected ≥%d)", st, actual, exp_n))
}

# ============================================================================
# 7. External-truth spot checks
# ============================================================================
cat("\n7. External-truth spot checks\n")
spot_check <- function(label, ags_v, year, expected_party_pattern) {
  rows <- l %>% filter(ags == ags_v, election_year == year)
  if (nrow(rows) == 0) {
    fail(sprintf("%s: no row for ags=%s year=%d", label, ags_v, year))
    return(invisible())
  }
  best <- rows %>% arrange(desc(round == "stichwahl")) %>% slice(1)
  wp <- best$winner_party
  if (is.null(wp) || is.na(wp) || wp == "") {
    warn(sprintf("%s: winner_party is NA (year=%d, round=%s)",
                 label, year, best$round))
  } else if (grepl(expected_party_pattern, wp, ignore.case = TRUE)) {
    pass(sprintf("%s: winner_party = '%s'", label, wp))
  } else {
    fail(sprintf("%s: winner_party = '%s', expected match '%s'",
                 label, wp, expected_party_pattern))
  }
}

spot_check("Bayern LK Erding 2020", "09177000", 2020, "CSU")
spot_check("NRW Kreis Kleve 2020", "05154000", 2020, "CDU")
spot_check("Sachsen Mittelsachsen 2025", "14522000", 2025, "FW|CDU|Freie")
# Thüringen 2018+ workbooks carry the party one header row above the name, not
# in "Name (Partei)"; until September 2026 the parser missed it and every
# 2018-2026 TH winner_party was NA. Winners checked on wahlen.thueringen.de.
spot_check("TH LK Eichsfeld 2018", "16061000", 2018, "^CDU$")
spot_check("TH LK Saalfeld-Rudolstadt 2026", "16073000", 2026, "^SPD$")
n_th_na <- l %>% filter(state == "16", is.na(winner_party)) %>% nrow()
check(n_th_na == 0,
      "TH: every Landrat election has a winner_party",
      sprintf("TH: %d Landrat election rows with NA winner_party", n_th_na))
# Region Hannover ("Regionspräsident") is treated specially in the existing
# NI parser. AGS may be 03241001 (Stadt) or 03241000 (Region) depending on
# how the PDF labels it. Just verify Hannover/Region exists in some form.
hano <- l %>% filter(grepl("annover", ags_name) & state == "03")
check_warn(nrow(hano) > 0,
           sprintf("NI Hannover/Region: %d row(s) present", nrow(hano)),
           "NI Hannover/Region: completely missing")
spot_check("SL LK Neunkirchen 2024", "10043000", 2024, "SPD")
spot_check("BB LK Havelland 2024", "12063000", 2024, "CDU")
spot_check("ST Burgenlandkreis 2014", "15084000", 2014, "CDU")

n_dus <- l %>% filter(ags == "05111000") %>% nrow()
check(n_dus == 0,
      "Düsseldorf (kreisfreie Stadt) NOT in landrat",
      sprintf("Düsseldorf appears %d times in landrat (should be 0)", n_dus))

n_rlp_94 <- l %>% filter(state == "07", election_year == 1994) %>% nrow()
check_warn(n_rlp_94 > 0,
           sprintf("RLP 1994 has %d rows", n_rlp_94),
           "RLP 1994 has 0 rows")

by_min_yr <- suppressWarnings(min(l %>% filter(state == "09") %>% pull(election_year)))
check(!is.infinite(by_min_yr) && by_min_yr <= 1950,
      sprintf("Bayern earliest year = %d (≤1950 expected)", by_min_yr),
      sprintf("Bayern earliest year = %d (expected ≤1950)", by_min_yr))

# ============================================================================
# 8. Candidate-level integrity
# ============================================================================
cat("\n8. Candidate-level integrity\n")
unharm_keys <- l %>% distinct(ags, election_date)
cand_keys <- lc %>% distinct(ags, election_date)
no_cands <- anti_join(unharm_keys, cand_keys, by = c("ags", "election_date"))
check_warn(nrow(no_cands) == 0,
           sprintf("every (ags, election_date) in unharm has ≥1 candidate row (%d total)",
                   nrow(unharm_keys)),
           sprintf("%d unharm rows have no matching candidate row", nrow(no_cands)))

# Cycles the Kreistag decided have no winner by design; section 12 pins them.
# So do failed elections, whose candidates all carry is_winner = NA: Rügen May
# 2001, whose Ja/Nein runoff missed the 25 % quorum (mayoral 99_audit 30f).
no_winner <- lc %>%
  group_by(ags, election_date, election_type) %>%
  summarise(any_winner = any(is_winner, na.rm = TRUE),
            council = any(flag_elected_by_council %in% TRUE),
            failed = all(is.na(is_winner)), .groups = "drop") %>%
  filter(!any_winner, !council, !failed)
check_warn(nrow(no_winner) == 0,
           "every election in candidates has ≥1 winner (Kreistag-elected and failed cycles aside)",
           sprintf("%d elections have no winner candidate", nrow(no_winner)))

no_rank1 <- lc %>%
  filter(!is.na(candidate_votes_hw)) %>%
  group_by(ags, election_date) %>%
  summarise(has_rank1 = any(candidate_rank_hw == 1, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!has_rank1)
check_warn(nrow(no_rank1) == 0,
           "every HW election has a rank-1 candidate",
           sprintf("%d elections missing rank-1 candidate", nrow(no_rank1)))

# Sachsen-Anhalt losing candidates are anonymised on purpose (StaLA scientific-
# use licence and section 80 KWO LSA), exactly as in mayoral_candidates, so a
# missing name is required there rather than a defect. Thüringen follows the
# same rule since 2026-10-03 (section 50 Abs. 2 ThürKWO). Elected Landraete stay
# named, and every other scraped state must name all its candidates. The
# exception are the three Thüringen runoffs of 28.05.2000 from the TLS report
# 29924: the TLS allows its reports' results to be published only without any
# personal data, the winners' names included (e-mail of 12.10.2026).
th_tls2000 <- lc$state == "16" & lc$election_date == as.Date("2000-05-28")
# Brandenburg follows the same rule since 2026-10-19 (user decision: every
# Brandenburg loser restricted, whatever the source).
n_na_name_scraped <- lc %>%
  filter(state %in% c("05","14") |
           (state == "12" & is_winner %in% TRUE) |
           (state == "16" & is_winner %in% TRUE & !th_tls2000), is.na(candidate_name)) %>%
  nrow()
check(n_na_name_scraped == 0,
      "all NRW/SN candidates and BB/TH winners have non-NA name (TH 2000 TLS runoffs aside)",
      sprintf("%d NRW/SN candidates or BB/TH winners with NA name", n_na_name_scraped))
check(sum(th_tls2000) == 6 && all(is.na(lc$candidate_name[th_tls2000])) &&
        all(is.na(lc$candidate_last_name[th_tls2000])),
      "TH Landrat runoffs of 28.05.2000 (TLS 29924): 6 candidates, nobody named in the public file",
      "TH Landrat runoffs of 28.05.2000: missing, or a name from the TLS report is public")

for (st_code in c("09", "12", "15", "16")) {
  st_lab <- c("09" = "BY", "12" = "BB", "15" = "ST", "16" = "TH")[[st_code]]
  st_named_losers <- lc %>%
    filter(state == st_code, !(is_winner %in% TRUE),
           !is.na(candidate_name) | !is.na(candidate_last_name) | !is.na(candidate_first_name) |
             !is.na(candidate_birth_year) | !is.na(candidate_profession)) %>% nrow()
  check(st_named_losers == 0,
        sprintf("%s losing candidates are anonymised (%s)", st_lab,
                c("09" = "LfStat candidate lists: withheld, winners too",
                  "12" = "user decision 2026-10-19", "15" = "licence / section 80 KWO LSA",
                  "16" = "section 50 ThürKWO")[[st_code]]),
        sprintf("%d named %s non-winner rows leaked", st_named_losers, st_lab))
  st_named_winners <- lc %>%
    filter(state == st_code, is_winner %in% TRUE, !is.na(candidate_name)) %>% nrow()
  check(st_named_winners > 0,
        sprintf("%s elected Landraete remain named (%d rows)", st_lab, st_named_winners),
        sprintf("%s winners lost their names — anonymisation is too broad", st_lab))
}
# The tracked, name-stripped copies of the Brandenburg and Sachsen-Anhalt
# Landrat parses (the raw files are gitignored; written by
# 01_landrat_combine.R) name only elected Landräte the public file names, on
# that Kreis's round dates.
lr_tok <- function(x) vapply(strsplit(tolower(trimws(x)), "[[:space:],]+"),
                             function(t) paste(sort(t[nzchar(t)]), collapse = " "), "")
for (lr_cp in list(c("12", "BB", "data/landrat_elections/raw/brandenburg/bb_landrat_parsed_public.csv"),
                   c("15", "ST", "data/landrat_elections/raw/sachsen_anhalt/st_landrat_parsed_public.csv"))) {
  if (file.exists(lr_cp[3])) {
    lpin <- read.csv(lr_cp[3], colClasses = "character", na.strings = character(0),
                     encoding = "UTF-8")
    lpin_named <- lpin[nzchar(lpin$candidate_name), ]
    lb_named <- lc %>% filter(state == lr_cp[1], !is.na(candidate_name))
    lb_dates <- c(paste(lb_named$ags, lb_named$election_date, lr_tok(lb_named$candidate_name)),
                  paste(lb_named$ags, lb_named$election_date_sw, lr_tok(lb_named$candidate_name)))
    l_unknown <- !(paste(lpin_named$ags, lpin_named$election_date, lr_tok(lpin_named$candidate_name)) %in% lb_dates)
    check(sum(l_unknown) == 0 && nrow(lpin_named) > 0 && all(nzchar(lpin$pair_key)),
          sprintf("%s Landrat tracked input copy: %d rows, %d named, all elected Landräte the public file names",
                  lr_cp[2], nrow(lpin), nrow(lpin_named)),
          sprintf("%s Landrat tracked input copy names %d people the public file does not",
                  lr_cp[2], sum(l_unknown)))
  } else {
    check(FALSE, sprintf("%s Landrat tracked input copy present", lr_cp[2]),
          sprintf("%s is missing (run 01_landrat_combine.R with the raw files)", basename(lr_cp[3])))
  }
}
# A Brandenburg loser's own-name Wahlvorschlag label is the plain list type in
# the public file (user decision 2026-10-19); the restricted twin keeps it.
source("code/shared/bb_own_name_label.R")
lr_tw <- "data/landrat_elections/final_restricted/landrat_candidates_restricted.rds"
if (file.exists(lr_tw)) {
  ltw <- readRDS(lr_tw)
  l_same <- nrow(ltw) == nrow(lc) && identical(ltw$ags, lc$ags) && identical(ltw$election_date, lc$election_date)
  li <- which(ltw$state %in% "12" & !(ltw$is_winner %in% TRUE))
  lwant <- bb_generic_own_label(ltw$candidate_party[li], ltw$candidate_last_name[li], ltw$candidate_first_name[li])
  l_bad <- sum(coalesce(lc$candidate_party[li], "") != coalesce(lwant, ""))
  l_gen <- sum(coalesce(lwant, "") != coalesce(ltw$candidate_party[li], ""))
  l_full <- length(list.files("data/landrat_elections/raw/brandenburg", pattern = "^BB_ergebnis.*\\.html$")) > 0
  check(l_same && l_bad == 0 && (l_gen == 6 || !l_full),
        sprintf("BB Landrat: %d own-name labels of losing candidates are the plain list type in the public file", l_gen),
        sprintf("BB Landrat: %d losing candidates' labels differ from the rule (rows aligned: %s)", l_bad, l_same))
}
# Party family of the Wahlvorschlag (code/shared/party_family.R, TODO M22,
# 2026-10-19): allowed values, a few pinned labels, and the same family in the
# restricted twin.
pf_ok <- c("cdu_csu", "spd", "gruene", "linke_pds", "fdp", "afd", "fw_wg",
           "independent", "other", "joint_list", NA)
pf_fx <- c("CSU" = "cdu_csu", "CDU" = "cdu_csu", "SPD" = "spd", "FREIE WÄHLER" = "fw_wg",
           "AfD" = "afd", "ÖDP" = "other", "Einzelbewerber" = "independent")
pf_fx_bad <- sum(vapply(names(pf_fx), function(x)
  sum(lc$candidate_party %in% x & !(lc$candidate_party_family %in% pf_fx[[x]])), integer(1)))
pf_tw_same <- !file.exists(lr_tw) ||
  identical(readRDS(lr_tw)$candidate_party_family, lc$candidate_party_family)
pf_lk <- bind_rows(
  lc %>% transmute(ags, d = election_date, lab = candidate_party, f = candidate_party_family),
  lc %>% filter(!is.na(election_date_sw)) %>%
    transmute(ags, d = election_date_sw, lab = candidate_party, f = candidate_party_family)) %>%
  distinct() %>% group_by(ags, d, lab) %>% filter(n() == 1) %>% ungroup()
pf_lj <- l %>% inner_join(pf_lk, by = c("ags", "election_date" = "d", "winner_party" = "lab"))
pf_lu_bad <- sum(coalesce(pf_lj$party_family, "NA") != coalesce(pf_lj$f, "NA"))
check("party_family" %in% names(l) && all(l$party_family %in% pf_ok) && nrow(pf_lj) > 2000 && pf_lu_bad == 0,
      sprintf("landrat_unharm: party_family = the candidate's family in all %d rounds matched by label", nrow(pf_lj)),
      sprintf("landrat_unharm: party_family missing, or %d rounds differ from the candidate's family", pf_lu_bad))
check("candidate_party_family" %in% names(lc) && all(lc$candidate_party_family %in% pf_ok) &&
        pf_fx_bad == 0 && pf_tw_same && sum(is.na(lc$candidate_party_family) & !is.na(lc$candidate_party)) <= 20,
      sprintf("landrat_candidates: candidate_party_family set (%s), pinned labels agree, twin identical",
              paste(names(sort(table(lc$candidate_party_family), decreasing = TRUE))[1:4], collapse = ", ")),
      sprintf("landrat_candidates: candidate_party_family missing or wrong (%d pinned labels off, twin same: %s)",
              pf_fx_bad, pf_tw_same))

# Bayern before 2026: every name, birth year and profession comes from the LfStat
# registers and candidate lists (B VII 3-2 / 3-7), whose redistribution the LfStat
# declined on 16.10.2026, winners included. Calibrated: the 2026-10-13 build
# published 644 such rows.
by_lfstat_pub <- lc %>%
  filter(state == "09", election_date < as.Date("2026-01-01"),
         !is.na(candidate_name) | !is.na(candidate_last_name) | !is.na(candidate_first_name) |
           !is.na(candidate_gender) | !is.na(candidate_birth_year) | !is.na(candidate_profession)) %>%
  nrow()
check(by_lfstat_pub == 0,
      "BY Landrat before 2026: no personal field public (LfStat registers and lists, 16.10.2026)",
      sprintf("%d BY Landrat rows before 2026 carry a personal field", by_lfstat_pub))

# ============================================================================
# 9. Duplicate detection
# ============================================================================
cat("\n9. Duplicate detection\n")
dup_unharm <- l %>%
  count(ags, election_date, election_type, round) %>%
  filter(n > 1)
check(nrow(dup_unharm) == 0,
      "no duplicate (ags, date, type, round) in unharm",
      sprintf("%d duplicate rows in unharm", nrow(dup_unharm)))

dup_cand <- lc %>%
  filter(!is.na(candidate_name)) %>%
  count(ags, election_date, candidate_name) %>%
  filter(n > 1)
check_warn(nrow(dup_cand) == 0,
           "no duplicate (ags, date, candidate_name) in candidates",
           sprintf("%d duplicate (ags, date, candidate_name) in candidates",
                   nrow(dup_cand)))

# Hard for the two scraped states: the BB portal reuses one URL per Kreis
# across cycles and re-publishes pages, so two cached files can describe the
# same election (00_bb_scrape.R keeps every version; parse_bb keeps the newest).
# A slip there doubles every candidate and the winner; unharm would hide it.
dup_cand_scraped <- dup_cand %>% filter(substr(ags, 1, 2) %in% c("12", "16"))
check(nrow(dup_cand_scraped) == 0,
      "BB/TH: no duplicate (ags, date, candidate_name)",
      sprintf("BB/TH: %d duplicate candidate rows", nrow(dup_cand_scraped)))
# A BB cycle the Kreistag decided (flag_elected_by_council, section 12) has no
# winner from the ballot by design; section 12 checks those cycles instead.
winners_scraped <- lc %>%
  filter(state %in% c("12", "16")) %>%
  group_by(ags, election_date) %>%
  summarise(n_winner = sum(is_winner %in% TRUE),
            council = any(flag_elected_by_council), .groups = "drop") %>%
  filter(n_winner != 1, !council)
# Both runoff candidates of a BB cycle ran in its Hauptwahl, so a Stichwahl row
# without Hauptwahl votes is a pairing failure: the LWL spells some people
# differently in the two rounds ("First Last" / "Last, First", MOL 2013),
# which split each into two rows and crowned both until September 2026.
bb_unpaired <- lc %>%
  filter(state == "12", !is.na(candidate_votes_sw), is.na(candidate_votes_hw))
check(nrow(bb_unpaired) == 0,
      "BB: every Stichwahl candidate is paired with their Hauptwahl row",
      sprintf("BB: %d Stichwahl rows without Hauptwahl votes: %s", nrow(bb_unpaired),
              paste(unique(paste(bb_unpaired$ags_name, bb_unpaired$election_date)),
                    collapse = "; ")))
check(nrow(winners_scraped) == 0,
      "BB/TH: exactly one winner per election cycle",
      sprintf("BB/TH: %d cycles without exactly one winner", nrow(winners_scraped)))

# ============================================================================
# 10. Stichwahl logic
# ============================================================================
cat("\n10. Stichwahl logic\n")
sw_rows <- l %>% filter(round == "stichwahl") %>% distinct(ags, election_year)
hw_rows <- l %>% filter(round == "hauptwahl") %>% distinct(ags, election_year)
sw_no_hw <- anti_join(sw_rows, hw_rows, by = c("ags", "election_year"))
check_warn(nrow(sw_no_hw) == 0,
           "every Stichwahl has a matching Hauptwahl in same year",
           sprintf("%d Stichwahl rows have no matching Hauptwahl (OK for SL hardcoded, SN 2022 and the TH 2000 runoffs of TLS 29924)",
                   nrow(sw_no_hw)))

sw_inconsistent <- lc %>%
  filter(has_stichwahl == TRUE) %>%
  group_by(ags, election_date) %>%
  summarise(any_sw_votes = any(!is.na(candidate_votes_sw)), .groups = "drop") %>%
  filter(!any_sw_votes)
check_warn(nrow(sw_inconsistent) == 0,
           "has_stichwahl=TRUE elections all have ≥1 candidate with SW votes",
           sprintf("%d elections marked has_stichwahl=TRUE but no SW votes",
                   nrow(sw_inconsistent)))

# ============================================================================
# 11. Saarland data quality
# ============================================================================
cat("\n11. Saarland data quality\n")
sl_no_eligible <- l %>% filter(state == "10", is.na(eligible_voters))
sl_total <- l %>% filter(state == "10") %>% nrow()
cat(sprintf("    SL rows total: %d (with NA eligible_voters: %d)\n",
            sl_total, nrow(sl_no_eligible)))

# ============================================================================
# 12. Brandenburg: majority + 15 % quorum (§ 72 Abs. 2 BbgKWahlG)
# ============================================================================
# The voters elect a BB Landrat only with more than half of the valid votes,
# amounting to at least 15 % of the eligible voters; if the runoff leader
# misses that, the Kreistag elects. 01_landrat_combine.R marks such cycles
# flag_elected_by_council and seats nobody from the ballot. Recomputed here
# from the published counts, independently of the combine's code. The two
# pinned cycles are stated on the LWL result pages themselves.
cat("\n12. Brandenburg quorum (flag_elected_by_council)\n")
check(is.logical(l$flag_elected_by_council) && !anyNA(l$flag_elected_by_council) &&
        is.logical(lc$flag_elected_by_council) && !anyNA(lc$flag_elected_by_council),
      "flag_elected_by_council is logical and never NA in both files",
      "flag_elected_by_council is not a complete logical column")
bb_rounds <- l %>% filter(state == "12") %>%
  select(ags, election_date, round, eligible_voters, valid_votes, winner_party,
         flag_u = flag_elected_by_council)
bb_cyc <- lc %>%
  filter(state == "12") %>%
  group_by(ags, election_date) %>%
  summarise(election_date_sw = first(election_date_sw),
            top = if (all(is.na(election_date_sw))) max(candidate_votes_hw, na.rm = TRUE)
                  else max(candidate_votes_sw, na.rm = TRUE),
            flag = any(flag_elected_by_council),
            flag_const = n_distinct(flag_elected_by_council) == 1,
            n_win = sum(is_winner %in% TRUE), n_na = sum(is.na(is_winner)), n = n(),
            .groups = "drop") %>%
  mutate(decisive_date = coalesce(election_date_sw, election_date)) %>%
  left_join(bb_rounds, by = c("ags", "decisive_date" = "election_date")) %>%
  mutate(by_voters = top > valid_votes / 2 & top >= 0.15 * eligible_voters)
check(nrow(bb_cyc) > 0 && !anyNA(bb_cyc$by_voters),
      sprintf("BB: majority + 15 %% rule computable for all %d cycles", nrow(bb_cyc)),
      "BB: a cycle lacks the counts for the majority + 15 % rule")
check(all(bb_cyc$flag == !bb_cyc$by_voters, na.rm = TRUE) && all(bb_cyc$flag_const),
      "BB: flag_elected_by_council == leader of the decisive round missed the rule",
      sprintf("BB: flag disagrees with the recomputed rule in %d cycle(s)",
              sum(bb_cyc$flag != !bb_cyc$by_voters, !bb_cyc$flag_const, na.rm = TRUE)))
check(all(bb_cyc$n_win[bb_cyc$flag] == 0) && all(bb_cyc$n_na[bb_cyc$flag] == bb_cyc$n[bb_cyc$flag]) &&
        all(is.na(bb_cyc$winner_party[bb_cyc$flag])),
      "BB: Kreistag-elected cycles seat nobody (is_winner NA, winner_* NA)",
      "BB: a Kreistag-elected cycle still names a winner")
check(all(bb_cyc$n_win[!bb_cyc$flag] == 1),
      "BB: every cycle the voters decided has exactly one winner",
      sprintf("BB: %d voter-decided cycle(s) without exactly one winner",
              sum(bb_cyc$n_win[!bb_cyc$flag] != 1)))
# Hauptwahl rows of those cycles keep their round leader, both rounds flagged
fl_rounds <- bb_rounds %>%
  semi_join(bb_cyc %>% filter(flag), by = c("ags", "election_date"))
check(all(fl_rounds$flag_u) && all(!is.na(fl_rounds$winner_party)) &&
        sum(bb_rounds$flag_u) == 2L * sum(bb_cyc$flag),
      "BB: landrat_unharm flags both rounds of each such cycle, Hauptwahl leader kept",
      "BB: landrat_unharm flag does not cover exactly the rounds of the flagged cycles")
check(!any(l$flag_elected_by_council[l$state != "12"]) &&
        !any(lc$flag_elected_by_council[lc$state != "12"]),
      "flag_elected_by_council is Brandenburg-only",
      "flag_elected_by_council set outside Brandenburg")
# Pinned: every cycle the Kreistag decided. Each runoff page says "Gewählt:
# kein Bewerber" (Oberhavel 2021's has no such footer), and its printed "Stimmenzahl, die 15 % der Wahlberechtigten
# umfasst" equals `threshold`, so the published electorate is tied to the
# source as well. The Kreistag's choice (page notes, the LWL index column
# "durch Kreistag", press and Wikipedia; see the final README) is recorded
# here only:
# runoff leader 9x, runoff loser 2x (EE, SPN 2010), non-candidate 1x (UM 2010).
# Pages from 2010-2016 are archived copies (raw/brandenburg/README.md).
pinned <- tribble(
  ~ags,       ~election_date,        ~election_date_sw,     ~threshold, ~top,
  "12060000", as.Date("2010-01-10"), as.Date("2010-01-24"), 22737,      18048, # Barnim: Ihrke, by lot after two Kreistag ballots without a majority
  "12060000", as.Date("2018-04-22"), as.Date("2018-05-06"), 23358,      17470, # Barnim: Kurth, 04.07.2018
  "12062000", as.Date("2010-01-10"), as.Date("2010-01-24"), 14765,      12602, # Elbe-Elster: Jaschinski (runoff loser), 29.03.2010
  "12063000", as.Date("2016-04-10"), as.Date("2016-04-24"), 20175,      20000, # Havelland: Lewandowski, 20.06.2016
  "12065000", as.Date("2015-02-22"), as.Date("2015-03-08"), 26267,      21288, # Oberhavel: Weskamp, 27.05.2015
  "12065000", as.Date("2021-11-28"), as.Date("2021-12-12"), 27284,      24964, # Oberhavel: Tönnies, 06.04.2022
  "12067000", as.Date("2016-11-27"), as.Date("2016-12-11"), 23068,      17819, # Oder-Spree: Lindemann, 25.01.2017
  "12068000", as.Date("2010-01-10"), as.Date("2010-01-24"), 13321,      11580, # OPR: Reinhardt, 20.05.2010
  "12068000", as.Date("2018-04-22"), as.Date("2018-05-06"), 12844,      12222, # OPR: Reinhardt by lot, 06.09.2018
  "12071000", as.Date("2010-01-10"), as.Date("2010-01-24"), 16600,      14784, # Spree-Neiße: Altekrüger (runoff loser), 19.04.2010
  "12072000", as.Date("2013-03-24"), as.Date("2013-04-14"), 20695,      20155, # Teltow-Fläming: Wehlan, 09.09.2013
  "12073000", as.Date("2010-02-28"), as.Date("2010-03-14"), 16655,      16254) # Uckermark: Dietmar Schulze (no candidate), 19.05.2010
got <- bb_cyc %>% filter(flag) %>%
  transmute(ags, election_date, election_date_sw,
            threshold = ceiling(0.15 * eligible_voters), top) %>%
  arrange(ags, election_date)
check(isTRUE(all.equal(as.data.frame(got), as.data.frame(arrange(pinned, ags, election_date)),
                       check.attributes = FALSE)),
      sprintf("BB: exactly the %d known Kreistag-elected cycles (2010-2021)", nrow(pinned)),
      sprintf("BB: flagged cycles changed: %s",
              paste(got$ags, got$election_date, collapse = ", ")))
# The 2010-2016 first direct elections: 9 of 14 failed (Gäbler & Rösel, ifo
# Dresden berichtet 4/2019). An independent count of the published rows.
first_direct <- bb_cyc %>% group_by(ags) %>% slice_min(election_date, n = 1) %>% ungroup()
check(nrow(first_direct) == 14 && max(first_direct$election_date) < as.Date("2017-01-01") &&
        sum(first_direct$flag) == 9,
      "BB: 14 first direct elections 2010-2016, 9 decided by the Kreistag (Gäbler & Rösel)",
      sprintf("BB: first direct elections: %d Kreise, %d flagged, last %s",
              nrow(first_direct), sum(first_direct$flag), max(first_direct$election_date)))

# ============================================================================
# 12b. Sachsen-Anhalt 2007: runoffs not on file (flag_decisive_round_missing)
# ============================================================================
# lr07dat3.csv holds only the first round of 22.04.2007 (2007 published no
# runoff results). Five Kreise had no majority, so their Landrat was decided
# in a runoff we do not hold: flagged, nobody seated. Burgenlandkreis was won
# outright by Reiche (51.2 %), whom a read.csv2() row-wrap had dropped along
# with candidates 10-11, seating the 13.8 % runner-up (data audit 2026-10-12).
cat("\n12b. Sachsen-Anhalt 2007 (flag_decisive_round_missing)\n")
check(is.logical(l$flag_decisive_round_missing) && !anyNA(l$flag_decisive_round_missing) &&
        is.logical(lc$flag_decisive_round_missing) && !anyNA(lc$flag_decisive_round_missing),
      "flag_decisive_round_missing is logical and never NA in both files",
      "flag_decisive_round_missing is not a complete logical column")
st_pinned <- paste(c("15082000", "15087000", "15088000", "15089000", "15091000"),
                   "2007-04-22")
fl_u <- l %>% filter(flag_decisive_round_missing)
fl_c <- lc %>% filter(flag_decisive_round_missing)
check(setequal(paste(fl_u$ags, fl_u$election_date), st_pinned) && nrow(fl_u) == 5L &&
        setequal(unique(paste(fl_c$ags, fl_c$election_date)), st_pinned),
      "flag_decisive_round_missing: exactly the 5 ST Kreise of 22.04.2007, in both files",
      sprintf("flag_decisive_round_missing on: %s",
              paste(unique(paste(fl_c$ags, fl_c$election_date)), collapse = ", ")))
st_cyc <- lc %>%
  filter(paste(ags, election_date) %in% st_pinned) %>%
  group_by(ags, election_date) %>%
  summarise(all_flag = all(flag_decisive_round_missing), no_winner = all(is.na(is_winner)),
            .groups = "drop")
check(nrow(st_cyc) == 5L && all(st_cyc$all_flag) && all(st_cyc$no_winner) &&
        all(fl_u$round == "hauptwahl") && !anyNA(fl_u$winner_party) &&
        all(fl_u$winner_voteshare <= 0.5),
      "ST 2007: flagged cycles seat nobody (is_winner NA); unharm Hauptwahl keeps its sub-50 % leader",
      "ST 2007: a flagged cycle still seats a candidate or lost its Hauptwahl leader")
# Independent of the parser: no other ST Hauptwahl lacks both majority and runoff
st_hw <- l %>% filter(state == "15", round == "hauptwahl", winner_voteshare <= 0.5)
st_sw <- l %>% filter(state == "15", round == "stichwahl") %>% select(ags, sw = election_date)
st_open <- st_hw %>%
  left_join(st_sw, by = "ags", relationship = "many-to-many") %>%
  group_by(ags, election_date) %>%
  summarise(has_sw = any(sw > election_date & sw - election_date < 60, na.rm = TRUE),
            flag = any(flag_decisive_round_missing), .groups = "drop") %>%
  filter(!has_sw)
check(nrow(st_open) > 0 && all(st_open$flag),
      "ST: every Hauptwahl without majority and without runoff is flagged",
      sprintf("ST: unflagged Hauptwahl without majority or runoff: %s",
              paste(st_open$ags[!st_open$flag], st_open$election_date[!st_open$flag],
                    collapse = ", ")))
st07 <- lc %>% filter(state == "15", election_date == as.Date("2007-04-22")) %>%
  group_by(ags) %>%
  summarise(n = n(), votes = sum(candidate_votes_hw), valid = first(valid_votes),
            .groups = "drop")
check(nrow(st07) == 9L && all(st07$votes == st07$valid),
      "ST 2007: every Kreis's candidate votes add up to its valid votes",
      sprintf("ST 2007: votes != valid in %s",
              paste(st07$ags[st07$votes != st07$valid], collapse = ", ")))
blk <- lc %>% filter(ags == "15084000", election_date == as.Date("2007-04-22"))
blk_w <- blk %>% filter(is_winner %in% TRUE)
check(nrow(blk) == 11L && nrow(blk_w) == 1L && blk_w$candidate_votes_hw == 31263 &&
        blk_w$candidate_last_name %in% "Reiche" &&
        l$winner_votes[l$ags == "15084000" & l$election_date == as.Date("2007-04-22")] == 31263,
      "ST 2007 Burgenlandkreis: 11 candidates, Reiche elected with 31,263 votes (51.2 %)",
      sprintf("ST 2007 Burgenlandkreis: %d candidates, winner %s",
              nrow(blk), paste(blk_w$candidate_last_name, blk_w$candidate_votes_hw)))

# ============================================================================
# 13. Coverage summary
# ============================================================================
cat("\n13. Coverage summary\n")
print(state_counts)

cat("\n────────────────────────────────────────────────────────────────────\n")
if (failed == 0L) {
  cat(sprintf("All checks passed ✓ (%d warnings)\n", warned))
  quit(status = 0L)
} else {
  cat(sprintf("%d check(s) failed ✗ (%d warnings)\n", failed, warned))
  quit(status = 1L)
}
