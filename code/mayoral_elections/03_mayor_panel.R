### Build within-mayor panel from GERDA mayoral elections
# Vincent Heddesheimer
# March 2026
#
# Creates a mayor-level panel with unique person IDs, enabling mayor fixed
# effects (person FE) estimation.
#
# Person identification:
# - Bayern: "Tag des ersten Amtsantritt" groups consecutive terms
# - Hessen: official "Amtszeiten seit 1993" / "Wiederwahl" counters, including
#   two documented mayors who left office and later returned
# - Other named states: candidate-name matching within municipality, where a
#   municipality is a lineage of renumbered codes; a mayor who wins in the
#   municipality their Gemeinde merged into keeps their id
#
# Outputs (both unharmonized + harmonized to 2021 boundaries):
# - mayor_panel.rds             — one row per person-election (original AGS)
# - mayor_panel_annual.rds      — one row per person-year (original AGS)
# - mayor_panel_harm.rds        — one row per person-election (AGS 2021)
# - mayor_panel_annual_harm.rds — one row per person-year (AGS 2021)

rm(list = ls())
gc()

# Load libraries
pacman::p_load(
  tidyverse,
  readxl,
  data.table,
  lubridate,
  conflicted,
  here
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("lag", "dplyr")
conflict_prefer("first", "dplyr")

setwd(here::here())
options(scipen = 999)
source("code/shared/harmonization_audit.R")
source("code/shared/mayoral_mapping.R")
source("code/shared/party_family.R")

# Helper function to pad AGS codes
pad_zero_conditional <- function(x, n, pad = "0") {
  x <- as.character(x)
  x[nchar(x) < n] <- paste0(pad, x[nchar(x) < n])
  return(x)
}

# Public Hessen snapshots disagree about whether a name is printed as
# "Surname, Given" or "Given Surname".  Sort transliterated name tokens so the
# two layouts compare equal; this key is used only as a fallback for recent
# hessenschau cycles not yet transmitted to HSL.
norm_person_name_unordered <- function(x) {
  y <- iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")
  y <- gsub("\\b(dr|prof)\\b", " ", y)
  y <- gsub("[^a-z0-9]+", " ", y)
  vapply(strsplit(trimws(y), "\\s+"), function(tokens) {
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) NA_character_ else paste(sort(tokens), collapse = "")
  }, character(1))
}

# Candidate sources are inconsistent about accents/punctuation and sometimes
# place an academic title in candidate_first_name in one year but in
# candidate_last_name in another (for example "Dr. Badenschier, Rico" versus
# "Badenschier, Dr. Rico").  The old raw `last name + first byte` key split
# those mechanically different renderings into separate people.  Normalise
# each component before building the key, while retaining the deliberately
# conservative surname + given-name-initial identity rule.
norm_person_component <- function(x) {
  y <- iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")
  y <- gsub("\\([^)]*\\)", " ", y)
  y <- gsub("\\b(prof|professor|dr|dipl|ing)\\b", " ", y)
  y <- gsub("[^a-z0-9]+", "", y)
  ifelse(is.na(x) | !nzchar(y), NA_character_, y)
}

# Bayern Landrat discriminator — the mayor panel must never ingest
# Landratswahlen (they are a separate dataset; see CLAUDE.md "Mayoral vs.
# Landrat elections"). Same rule as the Stage 1 scripts: an Amtstitel beginning
# "Landrat"/"Landrät" is unambiguous; a row with NO Amtstitel is a Landrat
# election iff its AGS is a Landkreis code (09 + KKK + "000" where KKK is not
# one of the 25 kreisfreien Städte). Verified 2026-07 against the raw sheet:
# all 1,222 Landkreis-AGS rows are Landrat elections (1,097 titled + 125
# untitled) and no Landkreis-AGS row carries a mayoral Amtstitel.
by_kreisfreie_kreis <- c("161", "162", "163", "261", "262", "263",
                         "361", "362", "363", "461", "462", "463", "464",
                         "561", "562", "563", "564", "565",
                         "661", "662", "663", "761", "762", "763", "764")

is_bayern_landrat <- function(amtstitel, ags) {
  amtstitel <- trimws(as.character(amtstitel))
  untitled <- is.na(amtstitel) | !nzchar(amtstitel)
  grepl("^Landr(at|ät)", amtstitel) |
    (untitled & grepl("^09[0-9]{3}000$", ags) &
       !(substr(ags, 3, 5) %in% by_kreisfreie_kreis))
}


# ============================================================================
# 1. LOAD DATA
# ============================================================================

cat("\n=== Loading data ===\n")

cand <- read_rds("data/mayoral_elections/final/mayoral_candidates.rds") |>
  as_tibble()
cat("Loaded mayoral_candidates:", nrow(cand), "rows\n")

# Filter to municipal mayor elections only
valid_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")
cand <- cand |> filter(election_type %in% valid_types)
cat("After filtering to BM/OBM:", nrow(cand), "rows\n")


# --- Municipality lineages, successors and election cycles (October 2026) ---
# An AGS changes for two reasons. A Kreisreform RENUMBERS a Gemeinde
# (Sachsen-Anhalt 1.7.2007, Sachsen 1.8.2008, the four MV cities 2011,
# Göttingen 2016): same territory, same mayor, new code. A MERGER ends a
# Gemeinde. Keyed on the code, a renumbered mayor became a new person in a new
# first term, and a term in a Gemeinde that no longer existed ran on in the
# annual panel to the last data year (45,684 rows and 2,556 terms of more than
# 12 years in the 2026-10-08 build, almost all Sachsen-Anhalt and Sachsen).
# Everything "within the municipality" -- person keys, term counts, the
# previous and next election, term ends -- is therefore computed per LINEAGE:
# the first code of a chain of pure renumberings. A code is renumbered when it
# ends in year Y and exactly one code that is new in Y + 1 has the same name, in
# the same Land, with a population within +-15 % (a same-name merger that
# creates a new code grows by far more).
xw_code_years <- bind_rows(
  readRDS("data/crosswalks/final/ags_crosswalks.rds") |>
    transmute(ags = str_pad(as.character(ags), 8, pad = "0"), year = as.integer(year),
              ags_name, population, pop_cw,
              target = str_pad(as.character(ags_21), 8, pad = "0")),
  readRDS("data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.rds") |>
    transmute(ags, year = as.integer(year), ags_name, population, pop_cw, target = ags_2023),
  readRDS("data/crosswalks/final/crosswalk_ags_2023_24_to_2025.rds") |>
    transmute(ags, year = as.integer(year), ags_name, population, pop_cw, target = ags_25)
) |>
  group_by(ags, year) |>
  slice_max(pop_cw, n = 1, with_ties = FALSE) |>
  ungroup()
xw_code_years <- bind_rows(
  xw_code_years,
  readRDS("data/crosswalks/final/crosswalk_ags_2023_24_to_2025.rds") |>
    distinct(ags = ags_25, .keep_all = TRUE) |>
    transmute(ags, year = 2025L, ags_name = ags_name_25, target = ags)
) |>
  mutate(name_key = gsub("[^a-zäöüß]", "", tolower(sub(",.*$", "", ags_name))))
xw_last_year <- max(xw_code_years$year)
code_last_year <- xw_code_years |>
  group_by(ags) |>
  summarise(last_year = max(year), .groups = "drop")

renumbered <- code_last_year |>
  filter(last_year < xw_last_year) |>
  inner_join(xw_code_years |> select(ags, year, name_key, pop_old = population),
             by = c("ags", "last_year" = "year")) |>
  mutate(year = last_year + 1L) |>
  inner_join(xw_code_years |>
               anti_join(xw_code_years |> distinct(ags, year) |> mutate(year = year + 1L),
                         by = c("ags", "year")) |>
               select(succ = ags, year, name_key, pop_new = population),
             by = c("year", "name_key"), relationship = "many-to-many") |>
  filter(substr(succ, 1, 2) == substr(ags, 1, 2)) |>
  group_by(ags) |>
  filter(n() == 1) |>
  ungroup() |>
  filter(pop_new / pop_old >= 0.85, pop_new / pop_old <= 1.15) |>
  # two same-name Gemeinden pointing at one new code (Hirschfeld, Hopfgarten,
  # Holzhausen, Grumbach, 1990-1993) cannot both be it: neither is taken
  group_by(succ) |>
  filter(n() == 1) |>
  ungroup() |>
  select(ags, renumbered_in = last_year, succ)

follow_codes <- function(x, map) {
  x <- as.character(x)
  repeat {
    nx <- unname(map[x])
    hit <- !is.na(nx)
    if (!any(hit)) return(x)
    x[hit] <- nx[hit]
  }
}
renum_pred <- setNames(renumbered$ags, renumbered$succ)
renum_next <- setNames(renumbered$succ, renumbered$ags)
lineage_of <- function(ags) follow_codes(ags, renum_pred)

# The year the municipality ceases to exist: the last crosswalk year of the
# lineage's final code, if that is before the last year the crosswalks cover.
# (Codes in no crosswalk at all, such as Merzien 15159029, get no end.)
lineage_end_of <- function(lineage) {
  final <- follow_codes(lineage, renum_next)
  y <- code_last_year$last_year[match(final, code_last_year$ags)]
  if_else(!is.na(y) & y < xw_last_year, y, NA_integer_)
}

# Where a merged-away Gemeinde went: the codes of the following year with the
# same (dominant) target code; at the 2020/2022/2024 breaks the target of the
# next crosswalk IS the successor code.
merged_away <- code_last_year |>
  filter(last_year < xw_last_year, !(ags %in% renumbered$ags)) |>
  inner_join(xw_code_years |> select(ags, year, target), by = c("ags", "last_year" = "year"))
merger_successors <- bind_rows(
  merged_away |>
    filter(last_year %in% c(2020L, 2022L, 2024L)) |>
    transmute(ags, merged_in = last_year, succ = target),
  merged_away |>
    filter(!(last_year %in% c(2020L, 2022L, 2024L))) |>
    mutate(year = last_year + 1L) |>
    inner_join(xw_code_years |> select(succ = ags, year, target),
               by = c("year", "target"), relationship = "many-to-many") |>
    filter(succ != ags) |>
    transmute(ags, merged_in = last_year, succ)
) |>
  transmute(from_lineage = lineage_of(ags), merged_in, to_lineage = lineage_of(succ)) |>
  distinct()

# Every BM/OB election of each municipality, whether or not its winner is named
# (and so whether or not it reaches the panel). A superseded round and a cycle
# whose decisive round is missing elected nobody -- unless the council then chose
# the mayor (Brandenburg, flag_elected_by_council): that also ends the term.
if (!"flag_elected_by_council" %in% names(cand)) cand$flag_elected_by_council <- FALSE
cycles <- cand |>
  filter(!(flag_superseded %in% TRUE),
         !(flag_decisive_round_missing %in% TRUE) | flag_elected_by_council %in% TRUE) |>
  distinct(ags, state, cycle_year = as.integer(election_year)) |>
  mutate(lineage = lineage_of(ags))
state_last_cycle <- cycles |>
  group_by(state) |>
  summarise(last_cycle = max(cycle_year), .groups = "drop")
cat(sprintf("Municipality lineages: %d renumberings, %d merger links, %d election cycles\n",
            nrow(renumbered), nrow(merger_successors), n_distinct(cycles$lineage, cycles$cycle_year)))


# ============================================================================
# 2. BAYERN — Person IDs from "Tag des ersten Amtsantritt"
# ============================================================================

cat("\n=== Processing Bayern: person IDs from Amtsantritt ===\n")

bayern_raw <- read_excel(
  "data/mayoral_elections/raw/bayern/20251114_Wahlen_seit_1945.xlsx",
  sheet = "20251114_bewerberRBZ1-7"
) |> as.data.table()

# Build AGS (matching 01_mayoral_unharm.R exactly)
bayern <- bayern_raw |>
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) |>
  mutate(
    gemeindeschluessel_padded = str_pad(as.character(Gemeindeschlüssel),
                                         width = 6, side = "left", pad = "0"),
    ags = paste0("09", gemeindeschluessel_padded),
    election_date = as.Date(`Tag der Wahl`),
    election_year = year(election_date),
    wahlart = as.character(Wahlart),
    amtsantritt = as.Date(`Tag des ersten Amtsantritt`),
    winner_party = `Wahlvorschlag Wahlgewinner`,
    winner_votes = as.numeric(`gültige Stimmen Wahlgewinner`),
    valid_votes = as.numeric(`Gültige Stimmen`),
    eligible_voters = as.numeric(Stimmberechtigte),
    number_voters = as.numeric(Wähler)
  ) |>
  filter(nchar(ags) == 8)

# Exclude Landratswahlen — this block reads the raw sheet directly and therefore
# bypasses the office classifier of 01_mayoral_unharm.R. Without this filter the
# panel treats 1,222 Landrat elections on 71 Landkreis AGS as mayors.
n_before_landrat <- nrow(bayern)
bayern <- bayern |> filter(!is_bayern_landrat(Amtstitel, ags))
cat("Bayern: dropped", n_before_landrat - nrow(bayern),
    "Landratswahl rows (belong to the landrat pipeline)\n")

# Exclude "ungültig" rows
bayern <- bayern |>
  filter(is.na(wahlart) | !grepl("ungültig", wahlart, ignore.case = TRUE))

cat("Bayern after excluding ungültig:", nrow(bayern), "rows\n")

# Build a row-level index to retrieve Bewerber columns later
# First, replicate the same filter pipeline on raw data to get aligned rows
bayern_filtered_raw <- bayern_raw |>
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) |>
  mutate(
    gemeindeschluessel_padded = str_pad(as.character(Gemeindeschlüssel),
                                         width = 6, side = "left", pad = "0"),
    ags_tmp = paste0("09", gemeindeschluessel_padded),
    wahlart_tmp = as.character(Wahlart)
  ) |>
  filter(nchar(ags_tmp) == 8) |>
  # Must apply exactly the same row filters as `bayern` above — the two frames
  # are matched positionally when the Bewerber columns are copied over below.
  filter(!is_bayern_landrat(Amtstitel, ags_tmp)) |>
  filter(is.na(wahlart_tmp) | !grepl("ungültig", wahlart_tmp, ignore.case = TRUE))

# Count candidates per row (winner + non-NA Bewerber 2-14)
bewerber_vote_cols <- paste0("gültige Stimmen Bewerber ", 2:14)
existing_bv_cols <- bewerber_vote_cols[bewerber_vote_cols %in% names(bayern_filtered_raw)]

bayern$runner_up_votes_clean <- as.numeric(bayern_filtered_raw$`gültige Stimmen Bewerber 2`)
bayern$n_candidates_raw <- 1L + rowSums(!is.na(
  as.data.frame(bayern_filtered_raw)[, existing_bv_cols, drop = FALSE]
))

# Superseded rounds seated nobody: a Hauptwahl without an absolute majority that
# a Neuwahl repeated (flag_superseded, 01_mayoral_unharm.R). Until October 2026
# the panel's municipality-year dedup removed most of them together with the
# year's later round; now that the panel keeps every election cycle they are
# dropped here (after the positional Bewerber copy above).
# A date is dropped only when every round on it is superseded: Würzburg's 1948
# council vote has a failed first ballot and the decisive one on one day.
by_superseded <- readRDS("data/mayoral_elections/final/mayoral_unharm.rds") |>
  filter(state == "09") |>
  group_by(ags, election_date) |>
  filter(all(flag_superseded %in% TRUE)) |>
  ungroup() |>
  distinct(ags, election_date)
n_before_sup <- nrow(bayern)
bayern <- bayern |> anti_join(by_superseded, by = c("ags", "election_date"))
cat("Bayern: dropped", n_before_sup - nrow(bayern), "superseded rounds\n")

# Deduplicate election cycles: keep Stichwahl where available
# Group elections in the same ags within 60 days
bayern <- bayern |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(
    date_gap = as.numeric(election_date - lag(election_date, default = election_date[1])),
    cycle = cumsum(date_gap > 60)
  ) |>
  ungroup()

# For each cycle: if Stichwahl exists, keep it; else keep the first round
# But carry forward n_candidates from first round
bayern_ncand <- bayern |>
  group_by(ags, cycle) |>
  summarise(
    n_candidates_first = max(n_candidates_raw[wahlart != "Stichwahl" |
                                                is.na(wahlart)],
                              na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(n_candidates_first = ifelse(is.infinite(n_candidates_first),
                                      NA_integer_,
                                      as.integer(n_candidates_first)))

bayern <- bayern |>
  group_by(ags, cycle) |>
  mutate(
    has_stichwahl = any(wahlart == "Stichwahl", na.rm = TRUE),
    is_final = case_when(
      has_stichwahl & wahlart == "Stichwahl" ~ TRUE,
      has_stichwahl & (wahlart != "Stichwahl" | is.na(wahlart)) ~ FALSE,
      !has_stichwahl ~ TRUE
    )
  ) |>
  ungroup() |>
  filter(is_final) |>
  left_join(bayern_ncand, by = c("ags", "cycle"))

# Compute voteshare and margin
bayern <- bayern |>
  mutate(
    winner_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                !is.na(winner_votes),
                              winner_votes / valid_votes, NA_real_),
    runner_up_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                   !is.na(runner_up_votes_clean),
                                 runner_up_votes_clean / valid_votes, NA_real_),
    winning_margin = winner_voteshare - coalesce(runner_up_voteshare, 0),
    n_candidates = coalesce(n_candidates_first, n_candidates_raw)
  )

cat("Bayern after Stichwahl dedup:", nrow(bayern), "rows\n")

# --- Bayern Kommunalwahl 2026 (separate source) ------------------------------
# The "Wahlen seit 1945" file ends at 2025; append the 8/22 March 2026 winners
# from by2026_parsed.csv. They carry Erster Amtsantritt, so binding them in here
# (before the person_id step) links re-elected mayors to their historical terms.
by26_file <- "data/mayoral_elections/raw/bayern/by2026_parsed.csv"
if (file.exists(by26_file)) {
  by26_all <- fread(by26_file, encoding = "UTF-8",
                    colClasses = list(character = c("ags", "candidate_party",
                                                    "candidate_name", "round",
                                                    "amtsantritt"))) |>
    filter(election_type %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl")) |>
    # by2026_parsed.csv is correctly RFC-4180 escaped, but fread() hands back the
    # doubled quotes un-collapsed (13 rows, all in candidate_party) — e.g.
    # 'Freie Wählergruppe ""Bayerisches Meran""'. Collapse them so the labels
    # match the "Wahlen seit 1945" spelling (audit 2026-07, M41).
    mutate(across(where(is.character), ~ gsub('""', '"', .x, fixed = TRUE))) |>
    mutate(election_date = as.Date(election_date),
           candidate_voteshare = as.numeric(candidate_voteshare),
           is_winner = as.logical(is_winner))
  by26_panel <- by26_all |>
    group_by(ags, election_date) |>
    filter(any(is_winner)) |>                     # decisive round only
    summarise(
      election_year = as.integer(first(election_year)),
      winner_party = candidate_party[is_winner][1],
      winner_voteshare = candidate_voteshare[is_winner][1],
      runner_up_voteshare = suppressWarnings(max(candidate_voteshare[!is_winner], na.rm = TRUE)),
      n_candidates = as.integer(first(n_candidates)),
      amtsantritt = as.Date(first(amtsantritt[is_winner])),
      .groups = "drop"
    ) |>
    mutate(
      runner_up_voteshare = ifelse(is.finite(runner_up_voteshare), runner_up_voteshare, NA_real_),
      winning_margin = winner_voteshare - coalesce(runner_up_voteshare, 0),
      winner_party = ifelse(nzchar(winner_party), winner_party, NA_character_)
    ) |>
    select(ags, election_date, election_year, winner_party, winner_voteshare,
           winning_margin, n_candidates, amtsantritt)
  bayern <- bind_rows(bayern, by26_panel)
  cat("Bayern + 2026:", nrow(bayern), "rows (",
      nrow(by26_panel), "from the 2026 Kommunalwahl )\n")
}

# Assign person_id from Amtsantritt
# Same (ags, amtsantritt) = same person
bayern_persons <- bayern |>
  filter(!is.na(amtsantritt)) |>
  distinct(ags, amtsantritt) |>
  arrange(ags, amtsantritt) |>
  group_by(ags) |>
  mutate(person_seq = row_number()) |>
  ungroup() |>
  mutate(person_id = sprintf("p_09_%05d", row_number()))

# RESTRICTED: the printed Bürgermeisterverzeichnisse 1978/1984 give the exact birth
# date and birthplace, which GERDA does not publish (it publishes the birth year).
# 00_by_verzeichnis_parse.py keys them on the same (ags, first Amtsantritt) as the
# person_id above; attach the id here so the scientific-use table joins to the
# panel directly. Gitignored, never redistributed; skipped if either file is absent.
by_reg_restricted <- "data/mayoral_elections/raw/bayern/by_verzeichnis_persons_restricted.csv"
if (file.exists(by_reg_restricted) && dir.exists("data/mayoral_elections/final_restricted")) {
  by_reg_r <- read_csv(by_reg_restricted, show_col_types = FALSE,
                       col_types = cols(.default = col_character())) |>
    mutate(amtsantritt = as.Date(amtsantritt)) |>
    left_join(bayern_persons |> select(ags, amtsantritt, person_id),
              by = c("ags", "amtsantritt")) |>
    relocate(person_id)            # the source key's id; mapped to the final id in section 6b
} else by_reg_r <- NULL

bayern_panel <- bayern |>
  left_join(bayern_persons, by = c("ags", "amtsantritt")) |>
  arrange(ags, election_date) |>
  mutate(
    state = "09",
    term_start_date = amtsantritt,
    person_id_method = "first_amtsantritt",
    source_person_term_number = NA_integer_,
    source_person_reelections = NA_integer_
  ) |>
  select(
    person_id, person_id_method, source_person_term_number,
    source_person_reelections, ags, state,
    election_year, election_date,
    winner_party, winner_voteshare, winning_margin,
    n_candidates, term_start_date
  )

# --- Bayern person-key corrections (October 2026) ---------------------------
# (ags, first Amtsantritt) is only as good as the workbook's date, and the printed
# Bürgermeisterverzeichnisse show where it fails:
#   A. A round whose Amtsantritt lies more than 200 days AFTER its own election
#      cannot belong to the person who took office on that date. Hohenthann's
#      2002/2008 rounds carry the 2014 date of the successor; Mitterteich's
#      2002/2008 rounds carry the date of a later namesake with another given name
#      and birth year. Where the names show it IS the same person (Dachau 2014;
#      Kirchweidach, a mayor who returned in 2020), the rounds stay together and
#      only the wrong start date is dropped. Where the names differ or are unknown,
#      the rounds are split off.
#   B. Inside one key, a named winner who is not the earlier named winner (surname,
#      given name and birth year compared, OCR variants tolerated) starts a new
#      person: the Augsburg OB 1990 key holds the OBs of 1990-2002 and of 2002,
#      and the keys of Nördlingen and Schongau two people each.
# The register names are restricted (LfStat, 16.10.2026; they reach this script
# through the restricted twin), so the examples here name no one.
# Existing person_ids are kept by the person the date belongs to; split-off people
# get new ids appended after the highest existing one, person_id_method
# "first_amtsantritt_split", and no start date (03 then uses their first election).
# The register names are withheld from the public file (LfStat, 16.10.2026), so
# these corrections read them from the restricted twin; only the pseudonymous
# person_id they produce is published.
by_win_file <- "data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds"
if (!file.exists(by_win_file)) {
  warning("No restricted candidates twin: the Bayern person-key corrections fall back to the ",
          "public file, which names no Bayern winner before 2026, so Bayern person_ids will ",
          "differ from the published panel.")
  by_win_file <- "data/mayoral_elections/final/mayoral_candidates.rds"
}
by_win <- as.data.frame(readRDS(by_win_file))
by_win <- by_win[by_win$state == "09" & by_win$is_winner %in% TRUE & !is.na(by_win$candidate_last_name), ]
nrm_sur <- function(x) gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
sig_dec <- data.frame(ags = by_win$ags,
                      .d = as.Date(ifelse(is.na(by_win$election_date_sw), as.character(by_win$election_date),
                                          as.character(by_win$election_date_sw))),
                      .sur = nrm_sur(by_win$candidate_last_name),
                      # first given-name token that is not a title ("Dr. Stefan" -> stefan)
                      .fst = nrm_sur(sapply(strsplit(dplyr::coalesce(by_win$candidate_first_name, ""), " +"),
                                            function(tk) { tk <- tk[!grepl("^(dr|prof|dipl|ing|mag)\\b", tolower(tk))]
                                                           if (length(tk)) tk[1] else "" })),
                      .byr = by_win$candidate_birth_year, .pri = 1L)
sig_hw <- sig_dec; sig_hw$.d <- as.Date(by_win$election_date); sig_hw$.pri <- 2L
by_sig <- rbind(sig_dec, sig_hw)
by_sig <- by_sig[order(by_sig$ags, by_sig$.d, by_sig$.pri), ]
by_sig <- by_sig[!duplicated(by_sig[c("ags", ".d")]), setdiff(names(by_sig), ".pri")]
bp <- dplyr::left_join(bayern_panel, by_sig, by = c("ags", "election_date" = ".d"))
bp$.amt0 <- bp$term_start_date     # the source key's Amtsantritt, before any correction
bp$.pid0 <- bp$person_id           # the source key's own id
bp <- bp[order(bp$person_id, bp$election_date), ]
bp$.impossible <- !is.na(bp$term_start_date) & as.numeric(bp$term_start_date - bp$election_date) > 200
str_close <- function(a, b, thr) a == b || grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE) ||
  1 - as.numeric(adist(a, b)) / max(nchar(a), nchar(b)) >= thr
same_person <- function(i, j) {           # NA = cannot tell
  if (is.na(bp$.sur[i]) || is.na(bp$.sur[j]) || !nzchar(bp$.sur[i]) || !nzchar(bp$.sur[j])) return(NA)
  fa <- bp$.fst[i]; fb <- bp$.fst[j]
  # Tolerated since the 2014 register (scan OCR, October 2026): a register that
  # prints the given name first (Kirchensittenbach 2014 vs 2026), and the same
  # surname with the same birth year and initial under another given name (a
  # short form, Achslach).
  if (!is.na(fa) && !is.na(fb) && nzchar(fa) && nzchar(fb) &&
      bp$.sur[i] == fb && bp$.sur[j] == fa) return(TRUE)
  if (bp$.sur[i] == bp$.sur[j] && !is.na(bp$.byr[i]) && !is.na(bp$.byr[j]) &&
      bp$.byr[i] == bp$.byr[j] &&
      (is.na(fa) || is.na(fb) || !nzchar(fa) || !nzchar(fb) || substr(fa, 1, 1) == substr(fb, 1, 1)))
    return(TRUE)
  # OCR reads "rn" as "m" (Vilgertshofen 2014)
  if (!str_close(gsub("rn", "m", bp$.sur[i]), gsub("rn", "m", bp$.sur[j]), 0.75)) return(FALSE)
  if (!is.na(fa) && !is.na(fb) && nzchar(fa) && nzchar(fb)) return(str_close(fa, fb, 0.6))
  # given names unknown: the birth year decides (a sources' birth-year slip alone,
  # with matching given names, does not make two people: a 2008/2026 case)
  if (!is.na(bp$.byr[i]) && !is.na(bp$.byr[j]) && abs(bp$.byr[i] - bp$.byr[j]) > 1) return(FALSE)
  TRUE
}
bp$.group <- 0L          # 0 = keeps the original id; 1, 2, ... = split off
bp$.drop_start <- FALSE
for (pid in unique(bp$person_id[!is.na(bp$person_id)])) {
  ix <- which(bp$person_id %in% pid)
  imp <- ix[bp$.impossible[ix]]; pos <- ix[!bp$.impossible[ix]]
  next_g <- 1L
  if (length(imp) && length(pos)) {
    ni <- imp[!is.na(bp$.sur[imp])]; np <- pos[!is.na(bp$.sur[pos])]
    verdict <- if (length(ni) && length(np)) same_person(ni[length(ni)], np[1]) else NA
    if (is.na(verdict)) {
      # No names on one side: the same list, held without another mayor in
      # between and within one term, is the same person with a wrong date
      # (Stulln, one CSU mayor 2002-2020); a list change is a new mayor.
      li <- imp[length(imp)]; fp <- pos[1]
      np_party <- function(x) tolower(gsub("[^[:alnum:]]", "", dplyr::coalesce(x, "")))
      between <- bp$ags == bp$ags[li] & bp$election_date > bp$election_date[li] &
        bp$election_date < bp$election_date[fp] & !(bp$person_id %in% pid)
      verdict <- nzchar(np_party(bp$winner_party[li])) &&
        np_party(bp$winner_party[li]) == np_party(bp$winner_party[fp]) && !any(between) &&
        as.numeric(bp$election_date[fp] - bp$election_date[li]) <= 7 * 365.25
    }
    if (isTRUE(verdict)) {
      bp$.drop_start[ix] <- TRUE                      # same person, wrong date
    } else {
      bp$.group[imp] <- next_g; next_g <- next_g + 1L # a different (or unknown) earlier mayor
    }
  } else if (length(imp)) {
    bp$.drop_start[ix] <- TRUE                        # only impossible rounds: keep id, drop date
  }
  # Rule B inside each group, in date order
  for (g in unique(bp$.group[ix])) {
    gx <- ix[bp$.group[ix] == g]
    ref <- NA_integer_; cur <- g
    for (k in gx) {
      if (is.na(bp$.sur[k])) { bp$.group[k] <- cur; next }
      if (is.na(ref)) { ref <- k; bp$.group[k] <- cur; next }
      if (isFALSE(same_person(ref, k))) { cur <- next_g; next_g <- next_g + 1L; ref <- k }
      bp$.group[k] <- cur
    }
  }
}
split_rows <- bp$.group > 0L
if (any(split_rows)) {
  max_id <- max(as.integer(sub("p_09_", "", bayern_persons$person_id)))
  keys <- unique(bp[split_rows, c("person_id", ".group")])
  first_d <- sapply(seq_len(nrow(keys)), function(r)
    min(bp$election_date[bp$person_id %in% keys$person_id[r] & bp$.group == keys$.group[r]]))
  keys <- keys[order(substr(keys$person_id, 1, 99), first_d), ]
  keys$.new_id <- sprintf("p_09_%05d", max_id + seq_len(nrow(keys)))
  bp <- dplyr::left_join(bp, keys, by = c("person_id", ".group"))
  bp$person_id <- ifelse(is.na(bp$.new_id), bp$person_id, bp$.new_id)
  bp$person_id_method[!is.na(bp$.new_id)] <- "first_amtsantritt_split"
  bp$term_start_date[!is.na(bp$.new_id) | bp$.drop_start] <- as.Date(NA)
}
cat("Bayern person-key corrections: rows split off", sum(split_rows), "into",
    if (any(split_rows)) nrow(keys) else 0L, "new person_ids; start date dropped (same person or",
    "all rounds impossible) on", sum(bp$.drop_start), "rows\n")
# C. The counterpart of B: consecutive keys of one Gemeinde that are one person.
#    The 2026 Mandatsträger file dates a re-elected mayor's first Amtsantritt a
#    few days off the workbook's (Reiner 09278179, Falter 09376175, ...), so the
#    2026 round got its own key and the incumbent a second person_id. A key is
#    merged into the preceding one when (1) the names agree (same surname and
#    first given name; birth years equal where both are known), or (2) the two
#    first-Amtsantritt dates lie within 120 days, the winning list is the same,
#    the names do not contradict each other, and the later key's Amtsantritt
#    lies more than a year before that key's own first election -- the key of a
#    re-elected incumbent. A key that starts with its own election (an appointed
#    1945 mayor and the 1946 elected one, a repeated election) is a successor.
bp <- bp[order(bp$ags, bp$election_date), ]
nrm_list <- function(x) tolower(gsub("[^[:alnum:]]", "", dplyr::coalesce(x, "")))
bp$.merge_into <- NA_character_
by_merge_log <- list()
for (a in unique(bp$ags)) {
  ix <- which(bp$ags == a)
  pids <- unique(bp$person_id[ix])          # in order of first election
  if (length(pids) < 2) next
  for (k in 2:length(pids)) {
    prev <- ix[bp$person_id[ix] == pids[k - 1]]; cur <- ix[bp$person_id[ix] == pids[k]]
    i <- prev[length(prev)]; j <- cur[1]
    pn <- prev[!is.na(bp$.sur[prev])]; cn <- cur[!is.na(bp$.sur[cur])]
    names_agree <- length(pn) && length(cn) &&
      bp$.sur[pn[length(pn)]] == bp$.sur[cn[1]] && nzchar(bp$.fst[pn[length(pn)]]) &&
      bp$.fst[pn[length(pn)]] == bp$.fst[cn[1]] &&
      (is.na(bp$.byr[pn[length(pn)]]) || is.na(bp$.byr[cn[1]]) || bp$.byr[pn[length(pn)]] == bp$.byr[cn[1]])
    names_clash <- length(pn) && length(cn) && isFALSE(same_person(pn[length(pn)], cn[1]))
    date_close <- !is.na(bp$.amt0[i]) && !is.na(bp$.amt0[j]) && bp$.amt0[i] != bp$.amt0[j] &&
      abs(as.numeric(bp$.amt0[j] - bp$.amt0[i])) <= 120 &&
      as.numeric(min(bp$election_date[cur]) - bp$.amt0[j]) > 365 &&
      nzchar(nrm_list(bp$winner_party[i])) && nrm_list(bp$winner_party[i]) == nrm_list(bp$winner_party[j])
    if (isTRUE(names_agree) || (date_close && !isTRUE(names_clash))) {
      target <- dplyr::coalesce(bp$.merge_into[i], pids[k - 1])
      bp$.merge_into[cur] <- target
      by_merge_log[[length(by_merge_log) + 1]] <- data.frame(
        ags = a, into = target, from = pids[k], rule = if (isTRUE(names_agree)) "names" else "date",
        amt_into = bp$.amt0[i], amt_from = bp$.amt0[j], first_from = min(bp$election_date[cur]),
        n_rows = length(cur))
    }
  }
}
key_merge <- !is.na(bp$.merge_into)
if (any(key_merge)) {
  merged_ids <- unique(bp$person_id[key_merge])
  bp$person_id[key_merge] <- bp$.merge_into[key_merge]
  bp$person_id_method[key_merge] <- "first_amtsantritt_merge"
  # a merged key's rows take the start of office of the key they join
  key_start <- tapply(bp$term_start_date[!key_merge], bp$person_id[!key_merge],
                      function(x) if (all(is.na(x))) NA_real_ else as.numeric(min(x, na.rm = TRUE)))
  bp$term_start_date[key_merge] <- as.Date(unname(key_start[bp$person_id[key_merge]]))
} else merged_ids <- character()
cat("Bayern key merges (one person under two first-Amtsantritt keys):", length(merged_ids),
    "keys,", sum(key_merge), "rows\n")
if (length(by_merge_log)) print(do.call(rbind, by_merge_log), row.names = FALSE)
# source key -> final person, for the restricted register table (section 6b)
by_key_rows <- bp[, c(".pid0", "person_id", "ags", "election_date", ".sur", ".byr")]
bayern_panel <- bp[order(bp$ags, bp$election_date), names(bayern_panel)]

cat("Bayern person-elections:", nrow(bayern_panel), "\n")
cat("Bayern unique persons:", n_distinct(bayern_panel$person_id, na.rm = TRUE), "\n")
cat("Bayern rows without person_id:", sum(is.na(bayern_panel$person_id)), "\n")


# ============================================================================
# 3. NAMED STATES — Person IDs from candidate names / Hessen term counters
# ============================================================================

cat("\n=== Processing named states: person IDs from names ===\n")

# States with candidate names (excluding Bayern). "08" = Baden-Württemberg, where
# each Gemeinde contributes a single election (the most recent as of 31.12.2024),
# so BW rows carry winner identity/gender but no cross-time incumbency history.
named_states <- c("01", "03", "05", "06", "07", "08", "10", "12", "13", "14", "15", "16")

winners_named <- cand |>
  filter(state %in% named_states, is_winner == TRUE)

cat("Winners in named states:", nrow(winners_named), "\n")

# Compute winning margin from candidate data
# Step 1: Get winner voteshare per election
winner_vs_data <- cand |>
  filter(state %in% named_states, is_winner == TRUE) |>
  mutate(
    winner_vs = case_when(
      has_stichwahl & !is.na(candidate_voteshare_sw) ~ candidate_voteshare_sw,
      TRUE ~ candidate_voteshare_hw
    )
  ) |>
  select(ags, election_date, winner_vs, n_candidates_hw) |>
  distinct(ags, election_date, .keep_all = TRUE)

# Step 2: Get runner-up voteshare
# For Stichwahl elections: runner-up is rank_sw == 2
# For non-Stichwahl: runner-up is rank_hw == 2
runner_up_sw <- cand |>
  filter(state %in% named_states, has_stichwahl == TRUE,
         !is.na(candidate_rank_sw), candidate_rank_sw == 2) |>
  select(ags, election_date, runner_up_vs = candidate_voteshare_sw) |>
  distinct(ags, election_date, .keep_all = TRUE)

runner_up_hw <- cand |>
  filter(state %in% named_states,
         (has_stichwahl == FALSE | is.na(has_stichwahl)),
         candidate_rank_hw == 2) |>
  select(ags, election_date, runner_up_vs = candidate_voteshare_hw) |>
  distinct(ags, election_date, .keep_all = TRUE)

runner_up_data <- bind_rows(runner_up_sw, runner_up_hw) |>
  distinct(ags, election_date, .keep_all = TRUE)

# Combine
margin_data <- winner_vs_data |>
  left_join(runner_up_data, by = c("ags", "election_date")) |>
  mutate(
    winning_margin = pmax(winner_vs - coalesce(runner_up_vs, 0), 0),
    n_candidates = as.integer(n_candidates_hw)
  ) |>
  select(ags, election_date, winning_margin, n_candidates)

# Join margin and n_candidates to winners
winners_named <- winners_named |>
  left_join(
    margin_data |> select(ags, election_date, winning_margin, n_candidates),
    by = c("ags", "election_date")
  ) |>
  mutate(
    # Effective winner voteshare (final round)
    winner_voteshare = case_when(
      has_stichwahl & !is.na(candidate_voteshare_sw) ~ candidate_voteshare_sw,
      TRUE ~ candidate_voteshare_hw
    )
  )

# Build name key for person matching outside Hessen.  Hessen is handled below
# from the official source's person-level term/re-election sequence.
# A name is usable only if it is neither NA nor blank: some sources redact
# candidate names (Thüringen §50 ThürKWO) or publish none at all (the Hessen
# historical series), and 01b writes those as "" rather than NA. Treating ""
# as a real name collapsed every unnamed winner of a municipality into ONE
# pseudo-person (1,995 Hessen elections -> 426 fake mayors, audit 2026-07),
# which corrupted term_number / is_incumbent / next_runs_again for that state.
# Unnamed winners get name_key = NA and are dropped below, exactly as the
# already-NA Thüringen and Sachsen winners are.
has_name <- function(x) !is.na(x) & nzchar(trimws(x))

winners_name_based <- winners_named |>
  filter(state != "06") |>
  mutate(
    .last_norm = norm_person_component(candidate_last_name),
    .first_norm = norm_person_component(candidate_first_name),
    name_key = case_when(
      !is.na(.last_norm) & !is.na(.first_norm) ~
        paste0(.last_norm, "_", substr(.first_norm, 1, 1), "_", state),
      !is.na(.last_norm) ~
        paste0(.last_norm, "__", state),
      TRUE ~ NA_character_
    )
  )

# A small set of source-level name variants still cannot be resolved by
# punctuation/title normalisation alone.  These are conservative, auditable
# links: same municipality, consecutive winning records, and either an obvious
# one-character transcription difference or an added/dropped given-name or
# married-name component.  Ambiguous same-surname pairs are deliberately not
# linked.  The map is expressed in the raw source spellings so regressions are
# visible when an upstream parser changes.
make_person_key <- function(last, first, state) {
  ln <- norm_person_component(last)
  fn <- norm_person_component(first)
  ifelse(!is.na(ln) & !is.na(fn), paste0(ln, "_", substr(fn, 1, 1), "_", state),
         ifelse(!is.na(ln), paste0(ln, "__", state), NA_character_))
}

candidate_name_aliases <- tribble(
  ~ags,       ~alias_last,          ~alias_first,                 ~canonical_last,      ~canonical_first,
  "03241008", "Bogye",              "Arpad",                      "Bogya",               "Arpad",
  "05558032", "Thönnes-Richard",    "Dr. Dietmar",                "Thönnes",             "Dr. Dietmar",
  "05974040", "Ruthemeyer",         "Dr. jur. Eckhard",           "Ruthemeyer",          "Dr. Eckhard Josef",
  "12065225", "Smaldino-Stattaus",  "Filippo",                    "Smaldino",            "Filippo",
  "16063068", "Hosenfeld",          "Bernadett (CDU)",             "Hosenfeld-Wald",      "Bernadett (CDU)",
  "15260007", "Klinger",            "Teiner",                      "Klinger",             "Reiner",
  "15260040", "Tomljanovic",        "Willi",                       "Tomljanocvic",        "Willi",
  "15260064", "Wernicker",          "Petra",                       "Wernicke",            "Petra",
  "15265026", "Henrich",            "Olaf",                        "Heinrich",            "Olaf",
  "15352021", "Herrmann",           "Klaus-Jürgen",                "Hermann",             "Klaus-Jürgen",
  "15355034", "Willamowski",        "Joachim",                     "Williamowski",        "Joachim",
  "15357016", "Barthels",           "Anno",                        "Bartels",             "Arno",
  "15358025", "Bother",             "Harald",                      "Bothe",               "Harald",
  "15358033", "Hildebrandt",        "Gunter",                      "Hildebrand",          "Gunter",
  "15362045", "Thraene",            "Hans",                        "Thräne",              "Hans",
  "15362069", "Schmid",             "Wolfgang",                    "Schmidt",             "Wolfgang",
  "15363029", "Ahrendt",            "Hans-Jürgen",                 "Ahrend",              "Hans-Jürgen",
  "15370034", "Kautge",             "Erhard",                      "Krautge",             "Erhard",
  "05382024", "Wirtz",              "Peter",                       "Wirtz",               "Franz Peter",
  "05758036", "Wilken",             "Rocco",                       "Wilken",              "Swen Rocco",
  "14521690", "Sigmund",            "Arne",                        "Sigmund",             "Hubert Arne",
  "14625040", "Große",              "Holm",                        "Große",               "André Holm",
  "14625320", "Mögel",              "Christian",                   "Mögel",               "Walter Christian",
  "14625470", "Brußk",              "Franz",                       "Brußk",               "Hubertus Franz",
  "14625580", "Hönicke",            "Lutz",                        "Hönicke",             "Uwe Lutz",
  "14626010", "Bänder",             "Andreas",                     "Bänder",              "Werner Andreas",
  "14626085", "Hergenröder",        "Verena",                      "Hergenröder",         "Maria Verena",
  "14626100", "Noack",              "Dietmar",                     "Noack",               "Friedrich Kurt Dietmar",
  "14626200", "Nitschke",           "Christian",                   "Nitschke",            "Roland Christian",
  "14627070", "Seifert",            "Conrad",                      "Seifert",             "Hugo Heinz Eberhard Conrad",
  "14627220", "Ritter",             "Michaela",                    "Ritter",              "Karin Michaela",
  "14628260", "Mühle",              "Peter",                       "Mühle",               "Wolfgang Peter",
  "15088205", "Hagenau",            "Dietlind",                    "Hagenau",             "Ellen Dietlind"
) |>
  mutate(
    state = substr(ags, 1, 2),
    alias_key = make_person_key(alias_last, alias_first, state),
    canonical_key = make_person_key(canonical_last, canonical_first, state)
  ) |>
  select(ags, alias_key, canonical_key)

observed_name_keys <- winners_name_based |> distinct(ags, name_key)
stopifnot(
  nrow(anti_join(candidate_name_aliases, observed_name_keys,
                 by = c("ags", "alias_key" = "name_key"))) == 0,
  nrow(anti_join(candidate_name_aliases, observed_name_keys,
                 by = c("ags", "canonical_key" = "name_key"))) == 0
)

winners_name_based <- winners_name_based |>
  left_join(candidate_name_aliases, by = c("ags", "name_key" = "alias_key")) |>
  mutate(
    .variant_link = !is.na(canonical_key),
    name_key = coalesce(canonical_key, name_key)
  ) |>
  select(-canonical_key)

cat("Non-Hessen winners with name_key:", sum(!is.na(winners_name_based$name_key)), "\n")
cat("Winners without name_key (unnamed at source, dropped):",
    sum(is.na(winners_name_based$name_key)), "\n")
if (any(is.na(winners_name_based$name_key))) {
  print(winners_name_based |>
          filter(is.na(name_key)) |>
          count(state, name = "unnamed_winners"))
}

# Assign person_id per (name_key, ags) combination, numbered within state.
named_persons <- winners_name_based |>
  filter(!is.na(name_key)) |>
  distinct(name_key, ags, state) |>
  arrange(state, ags, name_key) |>
  group_by(state) |>
  mutate(person_id = sprintf("p_%s_%05d", state[1], row_number())) |>
  ungroup()

# One person, one id across a municipality's history (October 2026). The key
# above is per code, so (1) a renumbering split one mayor into two people
# (846 of them, nearly all Sachsen and Sachsen-Anhalt), and (2) the mayor of a
# Gemeinde that merged into another and who then won there got a new id too.
# Now (1) the same name in the same lineage is one person, and (2) the same name
# winning in a successor municipality within 15 years of the merger is the same
# person. The later record takes the id of the earliest one, so no other id
# changes. A merger link is marked in person_id_method; the term counters still
# restart in the new municipality (was_predecessor_mayor marks those terms).
# The key holds only the first INITIAL, so a link also needs a compatible given
# name (equal, one a prefix of the other, or at most two edits apart; unknown on
# either side counts as compatible): Naumann Lothar (2001) and his son Lars
# (2008-2022), and Riedel Ulrich (2001) and Uwe (2022, three mayors later), share
# a key and a lineage but are different people.
first_compatible <- function(a, b) {
  if (!length(a) || !length(b)) return(TRUE)
  any(outer(a, b, Vectorize(function(x, y)
    x == y || startsWith(x, y) || startsWith(y, x) || adist(x, y) <= 2)))
}
name_records <- winners_name_based |>
  filter(!is.na(name_key)) |>
  group_by(name_key, ags) |>
  summarise(first_date = min(election_date), last_date = max(election_date),
            .firsts = list(unique(na.omit(sub(" .*$", "", .first_norm)))),
            .groups = "drop") |>
  left_join(named_persons |> select(name_key, ags, person_id), by = c("name_key", "ags")) |>
  mutate(lineage = lineage_of(ags))
renumbering_links <- name_records |>
  group_by(name_key, lineage) |>
  filter(n() > 1) |>
  arrange(first_date, .by_group = TRUE) |>
  transmute(from = first(person_id), to = person_id,
            .ok = vapply(.firsts, function(f) first_compatible(.firsts[[1]], f), logical(1))) |>
  ungroup() |>
  filter(from != to)
cat(sprintf("Renumbering links dropped for incompatible given names: %d\n", sum(!renumbering_links$.ok)))
renumbering_links <- renumbering_links |> filter(.ok) |> select(from, to)
merger_links <- name_records |>
  inner_join(merger_successors, by = c("lineage" = "from_lineage"),
             relationship = "many-to-many") |>
  inner_join(name_records |>
               select(name_key, to_lineage = lineage, to = person_id, to_first = first_date,
                      .to_firsts = .firsts),
             by = c("name_key", "to_lineage"), relationship = "many-to-many") |>
  filter(to_first > last_date,
         year(to_first) >= merged_in, year(to_first) <= merged_in + 15L,
         mapply(first_compatible, .firsts, .to_firsts)) |>
  distinct(from = person_id, to, to_lineage)

# Merge linked ids into the id of each group's earliest record.
first_seen <- name_records |>
  group_by(person_id) |>
  summarise(first_date = min(first_date), .groups = "drop")
id_rank <- setNames(rank(as.numeric(first_seen$first_date) +
                           seq_len(nrow(first_seen)) / (nrow(first_seen) + 1)),
                    first_seen$person_id)
merge_ids <- function(lab, links) {
  for (k in seq_len(nrow(links))) {
    a <- lab[[links$from[k]]]
    b <- lab[[links$to[k]]]
    if (a == b) next
    keep <- if (id_rank[[a]] < id_rank[[b]]) a else b
    lab[lab == setdiff(c(a, b), keep)] <- keep
  }
  lab
}
id_map <- merge_ids(setNames(first_seen$person_id, first_seen$person_id), renumbering_links)

# Name variants of one person between sources or report years (October 2026),
# pinned in data/mayoral_elections/metadata/person_name_variant_links.csv with
# the evidence for each: a misprinted or transliterated surname (Horts/Horst,
# Clauß/Claus, Fröhlich/Froehlich), a married or dropped second surname, given
# names added or dropped (Garn, Friedrich Wilhelm / Wilhelm), and a 1993 record
# that prints the surname only. Every pair is the same municipality and the
# same office, consecutive terms or a return after one term. Unlike the
# candidate_name_aliases table above, these links merge person ids AFTER
# numbering, like the renumbering links: the later record takes the earlier
# id and no other person_id changes. Both spellings must still occur.
variant_file <- "data/mayoral_elections/metadata/person_name_variant_links.csv"
variant_pairs <- if (file.exists(variant_file)) {
  fread(variant_file, encoding = "UTF-8", colClasses = "character") |>
    as_tibble() |>
    mutate(state = substr(ags, 1, 2),
           alias_key = make_person_key(alias_last, alias_first, state),
           canonical_key = make_person_key(canonical_last, canonical_first, state))
} else tibble()
variant_links <- tibble(from = character(), to = character())
if (nrow(variant_pairs) > 0) {
  variant_links <- variant_pairs |>
    left_join(named_persons |> select(ags, alias_key = name_key, from = person_id),
              by = c("ags", "alias_key")) |>
    left_join(named_persons |> select(ags, canonical_key = name_key, to = person_id),
              by = c("ags", "canonical_key"))
  unresolved <- variant_links |> filter(is.na(from) | is.na(to) | from == to)
  if (nrow(unresolved) > 0) {
    print(unresolved |> select(ags, alias_last, alias_first, canonical_last, canonical_first, from, to))
    stop("person_name_variant_links.csv: a pinned spelling no longer occurs, or both spellings ",
         "already share an id. Re-check the pair against the sources.")
  }
  variant_links <- variant_links |> select(from, to)
  id_map <- merge_ids(id_map, variant_links)
}
cat(sprintf("Name-variant links (pinned): %d pairs\n", nrow(variant_links)))
# A successor record that more than one predecessor mayor of the same name
# could claim is left unlinked.
merger_links <- merger_links |>
  mutate(from_c = unname(id_map[from])) |>
  group_by(to) |>
  filter(n_distinct(from_c) == 1) |>
  ungroup()
id_map <- merge_ids(id_map, merger_links)
merger_linked_ids <- unique(unname(id_map[c(merger_links$from, merger_links$to)]))
# after every merge, so a variant-linked person later joined across a merger
# keeps its variant label (the variant link takes precedence, as before)
variant_linked_ids <- unique(unname(id_map[c(variant_links$from, variant_links$to)]))
predecessor_terms <- merger_links |>
  transmute(person_id = unname(id_map[to]), lineage = to_lineage) |>
  distinct()
cat(sprintf("Person links: %d renumbering records, %d merger records; %d ids before, %d after\n",
            nrow(renumbering_links), nrow(merger_links),
            length(id_map), n_distinct(id_map)))

winners_name_based <- winners_name_based |>
  left_join(named_persons |> select(name_key, ags, person_id),
            by = c("name_key", "ags")) |>
  mutate(person_id = unname(id_map[person_id])) |>
  group_by(person_id) |>
  mutate(person_id_method = case_when(
    any(.variant_link) | person_id[1] %in% variant_linked_ids ~ "candidate_name_variant_link",
    person_id[1] %in% merger_linked_ids ~ "merger_name_link",
    TRUE ~ "candidate_name")) |>
  ungroup()

# One name key can hold two people with the same surname and initial in one
# municipality (Hotopp Angelika 1994 / Andy 2001, Hampel Ingo / Ingeburg). A
# record starts a new person when its given name is incompatible with the
# person's previous record AND gender or birth year contradict it; reviewed
# cases where the sources show a different person are pinned in
# metadata/person_split_pins.csv (ags + the second person's first election).
# Split-off people get new ids (person_id_method "candidate_name_split").
split_file <- "data/mayoral_elections/metadata/person_split_pins.csv"
split_pins <- if (file.exists(split_file)) {
  fread(split_file, encoding = "UTF-8", colClasses = "character") |>
    as_tibble() |> transmute(ags, election_date = as.Date(second_person_from), .pin = TRUE)
} else tibble(ags = character(), election_date = as.Date(character()), .pin = logical())
winners_name_based <- winners_name_based |>
  left_join(split_pins, by = c("ags", "election_date")) |>
  mutate(.pin = coalesce(.pin, FALSE), .ffirst = sub(" .*$", "", .first_norm)) |>
  arrange(person_id, election_date)
stopifnot(sum(winners_name_based$.pin) == nrow(split_pins))
winners_name_based$.grp <- 0L
for (pid in unique(winners_name_based$person_id[!is.na(winners_name_based$person_id)])) {
  ix <- which(winners_name_based$person_id %in% pid)
  if (length(ix) < 2) next
  g <- 0L; ref <- ix[1]
  for (k in ix[-1]) {
    w <- winners_name_based
    conflict <- !is.na(w$.ffirst[k]) && !is.na(w$.ffirst[ref]) && nzchar(w$.ffirst[k]) && nzchar(w$.ffirst[ref]) &&
      !first_compatible(w$.ffirst[ref], w$.ffirst[k]) &&
      ((!is.na(w$candidate_gender[k]) && !is.na(w$candidate_gender[ref]) &&
          w$candidate_gender[k] != w$candidate_gender[ref]) ||
         (!is.na(w$candidate_birth_year[k]) && !is.na(w$candidate_birth_year[ref]) &&
            abs(w$candidate_birth_year[k] - w$candidate_birth_year[ref]) > 1))
    if (conflict || w$.pin[k]) g <- g + 1L
    winners_name_based$.grp[k] <- g
    if (!is.na(w$.ffirst[k]) && nzchar(w$.ffirst[k])) ref <- k
  }
}
split_n <- winners_name_based |> filter(.grp > 0) |> distinct(person_id, .grp, state)
if (nrow(split_n)) {
  base_max <- winners_name_based |> filter(!is.na(person_id)) |>
    group_by(state) |> summarise(.m = max(as.integer(sub("^p_[0-9]+_", "", person_id))), .groups = "drop")
  split_n <- split_n |> left_join(base_max, by = "state") |> arrange(state, person_id, .grp) |>
    group_by(state) |> mutate(.new = sprintf("p_%s_%05d", state, .m + row_number())) |> ungroup()
  winners_name_based <- winners_name_based |>
    left_join(split_n |> select(person_id, .grp, .new), by = c("person_id", ".grp")) |>
    mutate(person_id_method = if_else(!is.na(.new), "candidate_name_split", person_id_method),
           person_id = coalesce(.new, person_id)) |>
    select(-.new)
}
cat(sprintf("Name-key splits: %d people split off (%d pinned)\n", nrow(split_n), nrow(split_pins)))
winners_name_based <- winners_name_based |> select(-.pin, -.ffirst, -.grp)

# Build panel rows for named states
named_panel <- winners_name_based |>
  select(
    person_id, person_id_method, ags, state, election_year, election_date,
    winner_party = candidate_party,
    winner_voteshare, winning_margin, n_candidates,
    # the winner's name, only for the own-name rule of party_family_parts()
    pf_last = candidate_last_name, pf_first = candidate_first_name,
    # Candidate characteristics (from 04_candidate_characteristics.R)
    candidate_gender, candidate_gender_source, candidate_gender_prob,
    candidate_gender_method,
    candidate_migration_bg, candidate_migration_bg_prob,
    candidate_name_origin, candidate_name_origin_conf,
    candidate_name_origin_method
  ) |>
  mutate(
    term_start_date = NA_Date_,
    source_person_term_number = NA_integer_,
    source_person_reelections = NA_integer_
  ) |>
  select(person_id, person_id_method, source_person_term_number,
         source_person_reelections, everything())

cat("\nNamed states person-elections:", nrow(named_panel), "\n")
cat("Named states unique persons:", n_distinct(named_panel$person_id, na.rm = TRUE), "\n")

# By state:
for (s in sort(unique(named_panel$state))) {
  sub <- named_panel |> filter(state == s)
  cat(sprintf("  State %s: %d elections, %d persons\n",
              s, nrow(sub), n_distinct(sub$person_id, na.rm = TRUE)))
}


# --- Hessen: source-indexed people, including redacted names -----------------
# HSL publishes, on each decisive winner row, the person's number of terms
# since direct elections began in 1993 and their number of re-elections.  This
# distinguishes people even when HSL must redact the name.  Ordinarily a
# positive Wiederwahl count links to the immediately preceding winner.  Two
# documented former mayors returned after another person held office; their
# source term count intentionally resumes rather than restarting:
#   Maintal: Erhard Rohrbach, 1995 -> 2003/2009
#   Frankenberg (Eder): Rüdiger Heß, 1998 -> 2012/2017
#   Waldems: Rudolf Dörr, 1993 -> 2000. The council declared the 1999 election
#     invalid; Dörr, mayor since 1993, won the May 2000 Neuwahl (term counter 2,
#     0 re-elections). Until October 2026 the panel joined the 2000 term to the
#     1999 winner instead; the Hessami (2018) names and the municipality's
#     history (dewiki "Waldems") show the 2000 winner is the 1993 mayor.
cat("\n=== Processing Hessen: official person term sequences ===\n")

he_source <- fread(
  "data/mayoral_elections/raw/hessen/he_hist_parsed.csv",
  encoding = "UTF-8",
  colClasses = list(character = c("ags", "is_winner"))
) |>
  as_tibble() |>
  filter(is_winner == "TRUE", election_type %in% valid_types) |>
  transmute(
    ags,
    decisive_date = as.Date(election_date),
    source_person_term_number = suppressWarnings(as.integer(winner_n_terms)),
    source_person_reelections = suppressWarnings(as.integer(winner_n_reelections))
  )

stopifnot(nrow(he_source) == nrow(distinct(he_source, ags, decisive_date)))

he_winners <- winners_named |>
  filter(state == "06") |>
  mutate(
    election_date = as.Date(election_date),
    decisive_date = coalesce(as.Date(election_date_sw), election_date),
    public_name_key = norm_person_name_unordered(candidate_name)
  ) |>
  left_join(he_source, by = c("ags", "decisive_date")) |>
  arrange(ags, election_date, decisive_date)

he_return_links <- c(
  "06435019|2003-09-28" = "1995-01-29",  # Erhard Rohrbach, Maintal
  "06635011|2012-02-26" = "1998-03-15",  # Rüdiger Heß, Frankenberg
  "06439016|2000-05-21" = "1993-05-02"   # Rudolf Dörr, Waldems
)

# A decisive round WITHOUT an official term counter, in a municipality-year that
# has a counted one, was superseded and is not a term: Bad Karlshafen's one-vote
# 2017-05-21 Stichwahl, repeated in one Wahlbezirk on 2017-11-05. (Every other
# HSL decisive round carries a counter; recent hessenschau-only cycles have none
# but never share a year with a counted one.)
he_superseded <- he_winners |>
  group_by(ags, election_year) |>
  filter(is.na(source_person_term_number) & any(!is.na(source_person_term_number))) |>
  ungroup()
stopifnot(nrow(he_superseded) == 1,
          he_superseded$ags == "06633002",
          he_superseded$decisive_date == as.Date("2017-05-21"))
he_winners <- he_winners |>
  anti_join(he_superseded |> select(ags, decisive_date), by = c("ags", "decisive_date"))

assign_he_group <- function(d, ags_value) {
  d <- d |> arrange(election_date, decisive_date)
  local_id <- rep(NA_integer_, nrow(d))
  next_id <- 0L

  new_id <- function() {
    next_id <<- next_id + 1L
    next_id
  }

  for (i in seq_len(nrow(d))) {
    nt <- d$source_person_term_number[i]
    nr <- d$source_person_reelections[i]
    row_key <- paste(ags_value, d$decisive_date[i], sep = "|")
    anchor_date <- unname(he_return_links[row_key])

    if (length(anchor_date) == 1 && !is.na(anchor_date)) {
      anchor <- which(as.character(d$decisive_date[seq_len(i - 1L)]) == anchor_date)
      if (length(anchor) != 1L) {
        stop("Hessen return-link anchor missing/ambiguous at ", row_key)
      }
      local_id[i] <- local_id[anchor]
    } else if (!is.na(nr) && nr > 0L) {
      if (i == 1L) stop("Hessen re-election has no prior winner at ", row_key)
      local_id[i] <- local_id[i - 1L]
    } else if (!is.na(nt) && nt == 1L) {
      local_id[i] <- new_id()
    } else if (!is.na(nt) && nt > 1L && !is.na(nr) && nr == 0L) {
      # A return after another mayor must be pinned in he_return_links above.
      stop("Unexpected non-consecutive Hessen term sequence at ", row_key)
    } else {
      # Very recent hessenschau cycles absent from the HSL transmission have
      # no counter. Link only when the same normalized public name was already
      # observed in this municipality; otherwise create a new person.
      same_name <- if (i == 1L || is.na(d$public_name_key[i])) integer() else
        which(d$public_name_key[seq_len(i - 1L)] == d$public_name_key[i])
      same_name <- same_name[!is.na(same_name)]
      local_id[i] <- if (length(same_name)) local_id[max(same_name)] else new_id()
    }
  }

  d$he_local_id <- local_id
  d
}

he_winners <- he_winners |>
  group_by(ags) |>
  group_modify(~ assign_he_group(.x, .y$ags)) |>
  ungroup()

he_persons <- he_winners |>
  distinct(ags, he_local_id) |>
  arrange(ags, he_local_id) |>
  mutate(person_id = sprintf("p_06_%05d", row_number()))

he_winners <- he_winners |>
  left_join(he_persons, by = c("ags", "he_local_id"))

hessen_panel <- he_winners |>
  transmute(
    person_id,
    person_id_method = case_when(
      !is.na(source_person_term_number) ~ "hessen_official_term_sequence",
      !is.na(public_name_key) ~ "candidate_name",
      TRUE ~ "election_record_only"
    ),
    source_person_term_number,
    source_person_reelections,
    ags, state, election_year, election_date,
    winner_party = candidate_party,
    winner_voteshare, winning_margin, n_candidates,
    term_start_date = as.Date(NA),
    candidate_gender, candidate_gender_source, candidate_gender_prob,
    candidate_gender_method,
    candidate_migration_bg, candidate_migration_bg_prob,
    candidate_name_origin, candidate_name_origin_conf,
    candidate_name_origin_method
  )

cat("Hessen person-elections:", nrow(hessen_panel), "\n")
cat("Hessen unique persons:", n_distinct(hessen_panel$person_id), "\n")
cat("Hessen elections with official term sequence:",
    sum(hessen_panel$person_id_method == "hessen_official_term_sequence"), "\n")
cat("Hessen rows without person_id:", sum(is.na(hessen_panel$person_id)), "\n")


# ============================================================================
# 4. COMBINE ALL STATES
# ============================================================================

cat("\n=== Combining all states ===\n")

# Bayern panel rows are built from the workbook, which names nobody, so they had
# no candidate characteristics. The 2026 winners are named in the public
# mayoral_candidates (Mandatsträger file), from which 04 derives gender and name
# origin. Attach them by the decisive round's date, falling back to the
# Hauptwahl date. person_id is untouched. The names from the printed
# Bürgermeisterverzeichnisse and the LfStat candidate lists are withheld from the
# public file (LfStat, 16.10.2026); 04 still publishes those winners' gender
# (user decision 2026-10-16), so they carry gender here and NA name origin.
by_char_cols <- c("candidate_gender", "candidate_gender_source", "candidate_gender_prob",
                  "candidate_gender_method", "candidate_migration_bg",
                  "candidate_migration_bg_prob", "candidate_name_origin",
                  "candidate_name_origin_conf", "candidate_name_origin_method")
by_w <- as.data.frame(readRDS("data/mayoral_elections/final/mayoral_candidates.rds"))
by_w <- by_w[by_w$state == "09" & by_w$election_type %in% valid_types &
               by_w$is_winner %in% TRUE &
               (!is.na(by_w$candidate_last_name) | !is.na(by_w$candidate_gender)), ]
by_dec <- by_w[, c("ags", by_char_cols)]
by_dec$.d <- as.Date(ifelse(is.na(by_w$election_date_sw), as.character(by_w$election_date),
                            as.character(by_w$election_date_sw)))
by_dec$.pri <- 1L
by_hw <- by_w[, c("ags", by_char_cols)]
by_hw$.d <- as.Date(by_w$election_date)
by_hw$.pri <- 2L
by_chars <- rbind(by_dec, by_hw)
by_chars <- by_chars[order(by_chars$ags, by_chars$.d, by_chars$.pri), ]
by_chars <- by_chars[!duplicated(by_chars[c("ags", ".d")]), setdiff(names(by_chars), ".pri")]
bayern_panel <- dplyr::left_join(bayern_panel, by_chars, by = c("ags", "election_date" = ".d"))
cat("Bayern panel rows with a named winner's characteristics:",
    sum(!is.na(bayern_panel$candidate_name_origin_method) | !is.na(bayern_panel$candidate_gender)),
    "of", nrow(bayern_panel), "\n")

panel <- bind_rows(bayern_panel, named_panel, hessen_panel) |>
  arrange(state, ags, election_date) |>
  mutate(
    # Party family components of the winner's list (code/shared/party_family.R):
    # INTERNAL, compared by party_switch / is_new_party_mayor; published as
    # party_family. An own-name label (Rheinland-Pfalz: "Muster") is an
    # Einzelbewerber only if the winner's name is known.
    party_parts = party_family_parts(winner_party, state, pf_last, pf_first),
    lineage = lineage_of(ags),
    # 1 = this person was mayor of a Gemeinde that merged into this municipality
    # (linked above); the term counters below count terms HERE only.
    was_predecessor_mayor = as.integer(paste(person_id, lineage) %in%
                                         paste(predecessor_terms$person_id,
                                               predecessor_terms$lineage))
  ) |>
  select(-pf_last, -pf_first)

cat("Combined panel:", nrow(panel), "rows\n")
cat("Unique persons:", n_distinct(panel$person_id, na.rm = TRUE), "\n")
cat("Rows without person_id:", sum(is.na(panel$person_id)), "\n")


# ============================================================================
# 5. BUILD PANEL COLUMNS
# ============================================================================

cat("\n=== Building panel columns ===\n")

# Remove rows without person_id and deduplicate
# When the same person has multiple elections in the same ags+year (HW + SW),
# keep only the latest date (the final/decisive round).
# HESSEN IS EXEMPT from both year-level dedups below: its rows are one decisive
# round per cycle by construction (00_he_hist_parse.py pairs the rounds), and
# HSL's official term counters show that a municipality can hold two genuine
# terms in one year — a repeated election (Alsbach-Hähnlein 2007, Babenhausen
# 2002) or a by-election after an early exit (Biebesheim am Rhein 1993, Bad
# Nauheim 1999, Burghaun 2014). The year-level dedups dropped those terms.
panel <- panel |>
  filter(!is.na(person_id)) |>
  distinct(person_id, ags, election_date, .keep_all = TRUE)
panel_he <- panel |> filter(state == "06")
stopifnot(!anyDuplicated(panel_he[c("ags", "election_date")]))
panel <- panel |>
  filter(state != "06") |>
  arrange(person_id, ags, election_year, desc(election_date)) |>
  distinct(person_id, ags, election_year, .keep_all = TRUE)

cat("After person-level dedup:", nrow(panel) + nrow(panel_he), "rows\n")

# When two winners of ONE election cycle reach the panel as separate records
# (HW winner vs SW winner), keep the one with the highest voteshare. Records
# belong to one cycle when they lie less than 60 days apart. Until October 2026
# this kept one winner per municipality-YEAR and so deleted 130 genuine second
# elections of a year (by-elections, repeats: Lützen 2010, Reichel in May and
# Könnecke in December); the annual panel gives such a year to the later term.
n_before <- nrow(panel)
panel <- panel |>
  arrange(ags, election_date) |>
  group_by(ags) |>
  mutate(.cyc = cumsum(c(TRUE, diff(as.numeric(election_date)) >= 60))) |>
  ungroup() |>
  arrange(ags, .cyc, desc(winner_voteshare)) |>
  distinct(ags, .cyc, .keep_all = TRUE) |>
  select(-.cyc)
cat("After ags-cycle dedup:", nrow(panel) + nrow(panel_he), "(dropped",
    n_before - nrow(panel), "dup winners)\n")
he_same_year <- panel_he |> count(ags, election_year) |> filter(n > 1)
cat("Hessen municipality-years with two counted terms (kept):", nrow(he_same_year), "\n")
panel <- bind_rows(panel, panel_he)

# Term number within each person in each municipality (lineage: a renumbered
# Gemeinde is the same municipality)
panel <- panel |>
  arrange(person_id, lineage, election_date) |>
  group_by(person_id, lineage) |>
  mutate(
    term_number = row_number(),
    tenure_start = min(election_year),
    years_in_office = election_year - tenure_start
  ) |>
  ungroup()

# is_incumbent: term_number >= 2
panel <- panel |>
  mutate(is_incumbent = as.integer(term_number >= 2))

# next_runs_again: does this person win the NEXT election in this municipality?
panel <- panel |>
  arrange(lineage, election_date) |>
  group_by(lineage) |>
  mutate(
    next_person_id = lead(person_id),
    next_runs_again = case_when(
      is.na(next_person_id) ~ NA_integer_,
      next_person_id == person_id ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  ungroup() |>
  select(-next_person_id)

# For Bayern: term_start_date is amtsantritt; for others: election_date of first term
panel <- panel |>
  group_by(person_id, lineage) |>
  mutate(
    term_start_date = case_when(
      !is.na(term_start_date) ~ term_start_date,  # Bayern: already set
      TRUE ~ min(election_date)  # Others: first election date
    )
  ) |>
  ungroup()

# --- Incumbency enrichment variables ---
# All of these compare elections of the same municipality (lineage), and are
# recomputed in the same way after harmonisation (section 7).
add_enrichment <- function(pnl) {
  # party_switch: did the winning party FAMILY change from the previous election
  #   here? 0 when the two winners' family components overlap (CSU -> CSU/Freie
  #   Wähler), 1 when they do not, NA when either is unknown.
  # is_new_party_mayor: 1 when none of the winner's family components has won
  #   here before (the first winner of a municipality included).
  # Until 2026-10-19 both compared normalised LABELS, so a re-elected mayor
  # whose list the next source spelled differently, or two successive local
  # lists, counted as a switch (TODO M22; code/shared/party_family.R).
  # margin_change: change in winning margin from the previous election
  pnl <- pnl |>
    arrange(lineage, election_date) |>
    group_by(lineage) |>
    mutate(
      prev_parts = lag(party_parts),
      x_overlap = party_parts_overlap(prev_parts, party_parts),
      party_switch = case_when(
        is.na(x_overlap) ~ NA_integer_,
        x_overlap ~ 0L,
        TRUE ~ 1L
      ),
      is_new_party_mayor = party_parts_new(party_parts),
      prev_margin = lag(winning_margin),
      margin_change = case_when(
        is.na(prev_margin) | is.na(winning_margin) ~ NA_real_,
        TRUE ~ winning_margin - prev_margin
      )
    ) |>
    ungroup() |>
    select(-prev_parts, -x_overlap, -prev_margin)

  # consecutive_terms: number of consecutive observed wins by the same person.
  # This must reset when a former mayor returns after somebody else held office
  # (Hessen contains two such source-verified cases), as well as after a >10-year
  # hole in the observed series.
  pnl |>
    arrange(lineage, election_date) |>
    group_by(lineage) |>
    mutate(
      new_consec_run = row_number() == 1L |
        person_id != lag(person_id) |
        election_year - lag(election_year) > 10L,
      new_consec_run = replace_na(new_consec_run, TRUE),
      consec_group = cumsum(new_consec_run)
    ) |>
    group_by(lineage, consec_group) |>
    mutate(consecutive_terms = row_number()) |>
    ungroup() |>
    select(-new_consec_run, -consec_group)
}

# Between two panel rows of one municipality there can be elections whose
# winner is not in the panel (Thüringen 2005-2019, a few Bayern elections of
# the 1950s-60s and of 2026). Comparisons with the
# "previous/next election" would skip them, so next_runs_again (does this
# person win the NEXT election) and party_switch / margin_change (vs the
# PREVIOUS election) are NA when an election of the municipality falls in a
# calendar year strictly between the two rows. Counted by year, as the annual
# panel is: same-year repeat and by-elections, which the ags-year dedup above
# keeps out of the panel, do not count. (Thüringen only from 2026-10-08, by
# date; all states from 2026-10-09.)
apply_gap_rule <- function(pnl, label) {
  pnl$.rid <- seq_len(nrow(pnl))
  gaps <- pnl |>
    select(.rid, lineage, election_date, election_year) |>
    arrange(lineage, election_date) |>
    group_by(lineage) |>
    mutate(prev_year = lag(election_year), next_year = lead(election_year)) |>
    ungroup() |>
    left_join(cycles |> distinct(lineage, cycle_year), by = "lineage",
              relationship = "many-to-many") |>
    group_by(.rid) |>
    summarise(
      gap_before = any(cycle_year > prev_year & cycle_year < election_year, na.rm = TRUE),
      gap_after = any(cycle_year > election_year & cycle_year < next_year, na.rm = TRUE),
      .groups = "drop")
  pnl <- pnl |>
    left_join(gaps, by = ".rid") |>
    mutate(
      next_runs_again = if_else(gap_after, NA_integer_, next_runs_again),
      party_switch = if_else(gap_before, NA_integer_, party_switch),
      margin_change = if_else(gap_before, NA_real_, margin_change)
    )
  cat(sprintf("  %s: %d rows with an election not in the panel before them, %d after (comparisons set to NA)\n",
              label, sum(pnl$gap_before), sum(pnl$gap_after)))
  pnl |> select(-.rid, -gap_before, -gap_after)
}

cat("\n  Adding incumbency enrichment variables...\n")
panel <- apply_gap_rule(add_enrichment(panel), "Gap rule")

cat(sprintf("  party_switch: %d switches out of %d with data (%.1f%%)\n",
            sum(panel$party_switch == 1L, na.rm = TRUE),
            sum(!is.na(panel$party_switch)),
            100 * mean(panel$party_switch == 1L, na.rm = TRUE)))
cat(sprintf("  is_new_party_mayor: %d first-time party wins\n",
            sum(panel$is_new_party_mayor == 1L, na.rm = TRUE)))


# ============================================================================
# 6. VALIDATE PERSON IDS
# ============================================================================

cat("\n=== Validation ===\n")

# Check for implausibly long tenures (>30 years)
long_tenures <- panel |>
  group_by(person_id) |>
  summarise(
    tenure_span = max(election_year) - min(election_year),
    n_terms = n(),
    state = first(state),
    ags = first(ags),
    .groups = "drop"
  ) |>
  filter(tenure_span > 30)

if (nrow(long_tenures) > 0) {
  cat("WARNING: Implausibly long tenures (>30 years):", nrow(long_tenures), "\n")
  print(long_tenures |> head(20))
} else {
  cat("No implausibly long tenures (>30 years) — OK\n")
}

# Check no person spans multiple states
multi_state <- panel |>
  group_by(person_id) |>
  summarise(n_states = n_distinct(state), .groups = "drop") |>
  filter(n_states > 1)

if (nrow(multi_state) > 0) {
  cat("WARNING: person_id spanning multiple states:", nrow(multi_state), "\n")
} else {
  cat("No person_id spans multiple states — OK\n")
}


# ============================================================================
# 6b. STABLE PERSON IDS
# ============================================================================
# The ids above are positional: every added election, split or merge renumbers
# the people after it. metadata/person_id_registry.csv records which id each
# person-election (ags, election_date) carried; it was seeded from the panel
# published in commit 176259e (October 2026). Each person of this build takes
# the registry id that covers most of its elections (greedy, largest overlap
# first); a person with no overlap takes a retired id whose elections it now
# holds, else a new one (the state's highest id ever issued + 1, in order of
# first election). Ids are never reused for another person: ids that lose all
# their elections stay in the registry as retired rows. The registry is
# rewritten by every run, so commit it together with the panel.
reg_file <- "data/mayoral_elections/metadata/person_id_registry.csv"
registry <- fread(reg_file, colClasses = "character", encoding = "UTF-8") |>
  as_tibble() |>
  mutate(election_date = as.Date(election_date), active = active == "TRUE")
stopifnot(!anyDuplicated(registry[registry$active, c("ags", "election_date")]),
          !anyDuplicated(panel[c("ags", "election_date")]))
cur_first <- panel |> group_by(person_id) |>
  summarise(.first = min(election_date), .ags = min(ags), .groups = "drop")
assign_ids <- function(pairs, cur_done, reg_used) {
  pairs <- pairs |> left_join(cur_first, by = c(".cur" = "person_id")) |>
    arrange(desc(n), .first, .cur, .reg)
  ci <- match(pairs$.cur, names(cur_done)); ri <- match(pairs$.reg, names(reg_used))
  take <- logical(nrow(pairs))
  for (k in seq_len(nrow(pairs))) if (!cur_done[ci[k]] && !reg_used[ri[k]]) {
    cur_done[ci[k]] <- TRUE; reg_used[ri[k]] <- TRUE; take[k] <- TRUE
  }
  list(map = pairs[take, c(".cur", ".reg")], cur_done = cur_done, reg_used = reg_used)
}
overlap <- function(reg_rows) panel |> select(.cur = person_id, ags, election_date) |>
  inner_join(reg_rows |> select(.reg = person_id, ags, election_date), by = c("ags", "election_date")) |>
  count(.cur, .reg, name = "n")
all_reg_ids <- unique(registry$person_id)
cur_done <- setNames(logical(nrow(cur_first)), cur_first$person_id)
reg_used <- setNames(logical(length(all_reg_ids)), all_reg_ids)
a1 <- assign_ids(overlap(registry |> filter(active)), cur_done, reg_used)
retired_ids <- setdiff(all_reg_ids, registry$person_id[registry$active])
a2 <- assign_ids(overlap(registry |> filter(person_id %in% retired_ids)), a1$cur_done, a1$reg_used)
id_stable <- bind_rows(a1$map, a2$map)
new_people <- cur_first |> filter(!person_id %in% id_stable$.cur) |>
  mutate(.st = substr(person_id, 3, 4)) |>
  left_join(registry |> distinct(person_id) |>
              transmute(.st = substr(person_id, 3, 4), .n = as.integer(sub("^p_[0-9]+_", "", person_id))) |>
              group_by(.st) |> summarise(.max = max(.n), .groups = "drop"), by = ".st") |>
  mutate(.max = coalesce(.max, 0L)) |>
  arrange(.st, .first, .ags, person_id) |>
  group_by(.st) |> mutate(.reg = sprintf("p_%s_%05d", .st, .max + row_number())) |> ungroup()
id_stable <- bind_rows(id_stable, new_people |> select(.cur = person_id, .reg))
id_stable <- setNames(id_stable$.reg, id_stable$.cur)
stopifnot(!anyDuplicated(id_stable), setequal(names(id_stable), cur_first$person_id))
panel <- panel |> mutate(person_id = unname(id_stable[person_id]))
cat(sprintf("Stable ids: %d people keep a registry id (%d of them a retired one), %d new; %d registry ids retired\n",
            nrow(a1$map) + nrow(a2$map), nrow(a2$map), nrow(new_people),
            length(setdiff(all_reg_ids, panel$person_id))))
id_num <- function(x) as.integer(sub("^p_[0-9]+_", "", x))
registry_new <- bind_rows(
  panel |> transmute(person_id, ags, election_date, active = TRUE),
  registry |> filter(!person_id %in% panel$person_id) |> mutate(active = FALSE)) |>
  arrange(substr(person_id, 3, 4), id_num(person_id), election_date, ags) |>
  mutate(election_date = format(election_date, "%Y-%m-%d"), active = if_else(active, "TRUE", "FALSE"))
fwrite(registry_new, reg_file)

# The restricted Bayern register table: source key -> final person -> stable id.
# Where a key holds two people (split above), the register entry goes to the one
# whose surname matches it, else to the person who kept the key's id.
if (!is.null(by_reg_r)) {
  key_people <- by_key_rows |>
    filter(!is.na(.pid0)) |>
    group_by(.pid0, person_id) |>
    summarise(.surs = list(unique(na.omit(.sur))), .first = min(election_date), .groups = "drop")
  pick_person <- function(base, last_name) {
    kp <- key_people[key_people$.pid0 %in% base, ]
    if (!nrow(kp)) return(NA_character_)
    if (nrow(kp) == 1) return(kp$person_id)
    hit <- kp$person_id[vapply(kp$.surs, function(s) nrm_sur(last_name) %in% s, logical(1))]
    if (length(hit) == 1) return(hit)
    if (base %in% kp$person_id) return(base)
    kp$person_id[which.min(kp$.first)]
  }
  by_reg_r$person_id <- unname(id_stable[mapply(pick_person, by_reg_r$person_id, by_reg_r$last_name)])
  write_csv(by_reg_r, "data/mayoral_elections/final_restricted/by_register_persons_restricted.csv")
  saveRDS(by_reg_r, "data/mayoral_elections/final_restricted/by_register_persons_restricted.rds")
  cat("Bayern register (restricted):", nrow(by_reg_r), "persons,",
      sum(!is.na(by_reg_r$person_id)), "linked to a panel person_id\n")
}

# Snapshot before crosswalk for unharmonized version
panel_unharm <- panel


# ============================================================================
# 7. HARMONIZE TO 2021 BOUNDARIES
# ============================================================================

cat("\n=== Harmonizing to 2021 boundaries ===\n")

# Crosswalk AGS stored as integers (leading zeros dropped). Pad to 8 digits
# to match the 8-digit string AGS used in the mayoral pipeline.
# For 1:N splits, keep only the dominant successor (highest pop_cw) so we
# don't duplicate person-election rows.
cw_full <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = str_pad(as.character(ags), width = 8, side = "left", pad = "0"),
    ags_21 = str_pad(as.character(ags_21), width = 8, side = "left", pad = "0")
  )

# All valid 2021 municipality codes (taken BEFORE the dominant-successor filter
# below, which would drop the minor targets of 1:N splits).
ags_2021_universe <- sort(unique(cw_full$ags_21))
cat("2021 municipality universe:", length(ags_2021_universe), "codes\n")

cw <- cw_full |>
  group_by(ags, year) |>
  slice_max(pop_cw, n = 1, with_ties = FALSE) |>
  ungroup()

# The person panel contains one decisive round, so round/type are absent.
# Project the reviewed exception list to its exact AGS/date keys.
exceptions <- gerda_mayoral_exceptions() |>
  select(ags, election_date, reason, evidence) |> distinct()
panel <- gerda_exclude_documented(panel, exceptions, c("ags", "election_date"), "mayor_panel_21")
panel$.source_id <- seq_len(nrow(panel))
panel_source <- panel

# Assign crosswalk lookup year
panel <- panel |>
  mutate(
    cw_year = case_when(
      election_year >= 2021 ~ NA_integer_,
      election_year >= 1990 ~ as.integer(election_year),
      election_year < 1990  ~ 1990L
    )
  )

# Post-2020: identity mapping
panel_post2020 <- panel |>
  filter(is.na(cw_year)) |>
  mutate(ags_21 = ags)

# Pre-2021: crosswalk lookup
panel_pre2021 <- panel |>
  filter(!is.na(cw_year)) |>
  left_join(
    cw |> select(ags, year, ags_21),
    by = c("ags", "cw_year" = "year")
  )

# Handle unmatched: try year-1
unmatched <- panel_pre2021 |> filter(is.na(ags_21))
if (nrow(unmatched) > 0) {
  matched <- panel_pre2021 |> filter(!is.na(ags_21))
  unmatched_try <- unmatched |>
    select(-ags_21) |>
    mutate(cw_year_try = pmax(cw_year - 1L, 1990L)) |>
    left_join(
      cw |> select(ags, year, ags_21),
      by = c("ags", "cw_year_try" = "year")
    ) |>
    select(-cw_year_try)

  # Try year+1 for still unmatched
  still_unmatched <- unmatched_try |> filter(is.na(ags_21))
  if (nrow(still_unmatched) > 0) {
    fixed_minus1 <- unmatched_try |> filter(!is.na(ags_21))
    still_unmatched_try <- still_unmatched |>
      select(-ags_21) |>
      mutate(cw_year_try = pmin(cw_year + 1L, 2020L)) |>
      left_join(
        cw |> select(ags, year, ags_21),
        by = c("ags", "cw_year_try" = "year")
      ) |>
      select(-cw_year_try)
    panel_pre2021 <- bind_rows(matched, fixed_minus1, still_unmatched_try)
  } else {
    panel_pre2021 <- bind_rows(matched, unmatched_try)
  }
} # else all matched

unresolved <- panel_pre2021 |> filter(is.na(ags_21))
recovered <- gerda_recover_mayoral_mapping(unresolved, cw_full)
panel <- bind_rows(panel_pre2021 |> filter(!is.na(ags_21)), recovered, panel_post2020) |>
  select(-cw_year)


# --- Validate ags_21 against the 2021 municipality universe -----------------
# Elections from 2021 onwards are identity-mapped above, which silently emits
# any AGS created AFTER 2021 (or simply wrong at source) as a bogus "2021" code
# (audit 2026-07, M29). Genuine post-2021 merger codes are back-mapped onto
# their dominant 2021 predecessor — dominant rather than split, because a
# person-election row must not be duplicated across predecessor municipalities.
# Everything else is reported and, unless explicitly allowlisted, is an error.

cw_21_23 <- readRDS("data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.rds") |>
  as_tibble() |>
  filter(year == 2021) |>
  select(ags_pre = ags, ags_2023, population)
cw_23_25 <- readRDS("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  as_tibble() |>
  filter(year == 2023) |>
  select(ags_2023 = ags, ags_25)

# many-to-many by construction: several 2021 codes merge into one 2023 code, and
# a 2023 code can be split across several 2025 codes.
backmap_src <- cw_21_23 |>
  left_join(cw_23_25, by = "ags_2023", relationship = "many-to-many")
post2021_backmap <- bind_rows(
    backmap_src |> transmute(ags_bad = ags_2023, ags_pre, population),
    backmap_src |> transmute(ags_bad = ags_25, ags_pre, population)
  ) |>
  # only codes that do NOT exist in 2021 can ever be back-mapped
  filter(!is.na(ags_bad), !(ags_bad %in% ags_2021_universe)) |>
  group_by(ags_bad) |>
  slice_max(population, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ags_bad, ags_pre)

n_backmapped <- sum(panel$ags_21 %in% post2021_backmap$ags_bad)
if (n_backmapped > 0) {
  cat("Back-mapping", n_backmapped, "rows from post-2021 merger codes:\n")
  print(panel |>
          filter(ags_21 %in% post2021_backmap$ags_bad) |>
          count(ags, ags_21, election_year) |>
          left_join(post2021_backmap, by = c("ags_21" = "ags_bad")) |>
          rename(ags_21_new = ags_pre))
}
panel <- panel |>
  left_join(post2021_backmap, by = c("ags_21" = "ags_bad")) |>
  mutate(ags_21 = coalesce(ags_pre, ags_21)) |>
  select(-ags_pre)

# Reject every missing or invalid target, including previously allowlisted codes.
gerda_audit_mapping(panel_source, panel, ".source_id", "ags_21", "mayor_panel_21",
                    weight = NULL, counts = character(), target_codes = ags_2021_universe)
panel <- panel |> select(-.source_id, -any_of(c("pop_cw", "crosswalk_year", "mapping_method")))


# Recompute term_number after crosswalk may have dropped some rows
panel <- panel |>
  arrange(person_id, lineage, election_date) |>
  group_by(person_id, lineage) |>
  mutate(
    term_number = row_number(),
    tenure_start = min(election_year),
    years_in_office = election_year - tenure_start,
    is_incumbent = as.integer(term_number >= 2)
  ) |>
  ungroup()

# Recompute next_runs_again
panel <- panel |>
  arrange(lineage, election_date) |>
  group_by(lineage) |>
  mutate(
    next_person_id = lead(person_id),
    next_runs_again = case_when(
      is.na(next_person_id) ~ NA_integer_,
      next_person_id == person_id ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  ungroup() |>
  select(-next_person_id)

# Recompute enrichment variables after harmonization, and the gap rule with
# them: until 2026-10-09 this recomputation dropped the Thüringen gap NAs, so
# mayor_panel_harm compared across 548 unnamed elections that mayor_panel
# did not.
panel <- apply_gap_rule(add_enrichment(panel), "Gap rule (harmonised)")


# ============================================================================
# 8. FINALIZE BOTH VERSIONS (UNHARMONIZED + HARMONIZED)
# ============================================================================

# --- Where a term ends in the annual panel (October 2026) --------------------
# Until 2026-10-08 a term ran to the next NAMED winner of its AGS, or to the
# last data year. Elections with unnamed winners and Gemeinden that no longer
# existed therefore kept a mayor in office for decades. A term now ends at the
# earliest of:
#   (a) the next election of the municipality, named or not;
#   (b) the end of the municipality: the last year of its lineage, so a merger
#       ends it and a renumbering does not;
#   (c) the longest term the law allows (below), where the source has no later
#       election although one must have been held;
#   (d) in Thüringen, six years after a pre-2020 election with no later one in
#       any source (the 2010/2016 elections of the 381 Gemeinden of 2004 that
#       the TLS database lacks).
# Longest legal term of a directly elected mayor, by-elections included (Land,
# election year; research 2026-10-09). Used only for (c), with one year of slack
# for the start of office and an incumbent staying until the successor takes
# over, so it never shortens a term the law allows. NA = no cap.
#   NI  5 y to 30.4.2005; 8 y to 2014; 2014-1/2025 single elections ran to the
#       end of the NEXT council period (Drs. 18/2022: up to ~10 y); 8 y again
#       from 1.2.2025 (§ 80 NKomVG).
#   NRW by-elections 2000-2007 ran to the end of the next council period (~10);
#       6 y for 2007-2013 elections (GO-Reformgesetz 2007); the 2013 transition
#       and the 2020 by-election rule allow ~8 (§ 65 GO NRW).
#   BY  6 y; by-elections up to 7 y (GLKrWG 1995) and 8 y (from 2008); the law
#       before 1995 was not checked, so no cap.
#   SL  8 y until 2007, 10 y from the Bündelungsgesetz of 29.8.2007.
#   BB  8 y (§ 53 BbgKVerf); ehrenamtliche 5 y with the council, a by-election
#       up to ~6 y. MV 7-9 y by Hauptsatzung. SH 6-8 y. HE 6, RP 8, BW 8, SN 7,
#       ST 7 (ehrenamtliche too), TH 6.
max_legal_term <- function(state, year, office_type = NA_character_) {
  case_when(
    state == "12" & office_type %in% "Ehrenamtliche(r) Bürgermeister*in" ~ 6,
    state == "01" ~ 8,
    state == "03" & year < 2005 ~ 5,
    state == "03" & year < 2014 ~ 8,
    state == "03" & year < 2025 ~ 10,
    state == "03" ~ 8,
    state == "05" & year < 2008 ~ 10,
    state == "05" & year < 2013 ~ 6,
    state == "05" ~ 8,
    state == "06" ~ 6,
    state == "07" ~ 8,
    state == "08" ~ 8,
    state == "09" & year < 1995 ~ NA_real_,
    state == "09" & year < 2008 ~ 7,
    state == "09" ~ 8,
    state == "10" & year < 2007 ~ 9,
    state == "10" ~ 10,
    state == "12" ~ 8,
    state == "13" ~ 9,
    state == "14" ~ 7,
    state == "15" ~ 7,
    state == "16" ~ 6,
    TRUE ~ NA_real_
  )
}
winner_office <- bind_rows(
  cand |> filter(is_winner %in% TRUE) |> select(ags, election_date, office_type),
  cand |> filter(is_winner %in% TRUE, !is.na(election_date_sw)) |>
    select(ags, election_date = election_date_sw, office_type)
) |>
  distinct(ags, election_date, .keep_all = TRUE)
term_caps <- panel_unharm |>
  distinct(person_id, ags, election_date, lineage, state, election_year) |>
  left_join(cycles |> distinct(lineage, cycle_year), by = "lineage",
            relationship = "many-to-many") |>
  group_by(person_id, ags, election_date, lineage, state, election_year) |>
  summarise(next_cycle = if (any(cycle_year > election_year, na.rm = TRUE))
              min(cycle_year[cycle_year > election_year], na.rm = TRUE) else NA_integer_,
            .groups = "drop") |>
  left_join(winner_office, by = c("ags", "election_date")) |>
  left_join(state_last_cycle, by = "state") |>
  mutate(lineage_end = lineage_end_of(lineage),
         max_term = max_legal_term(state, election_year, office_type)) |>
  select(person_id, ags, election_date, next_cycle, lineage_end, max_term, last_cycle)
stopifnot(!anyDuplicated(term_caps[, c("person_id", "ags", "election_date")]))
cat(sprintf("Term ends: %d terms followed by an election of the municipality, %d in a municipality that ceased to exist\n",
            sum(!is.na(term_caps$next_cycle)), sum(!is.na(term_caps$lineage_end))))

# Helper: compute within-mayor stats, format, and build annual panel
finalize_panel <- function(p, version = "harm") {
  has_ags_21 <- "ags_21" %in% names(p)
  p <- p |> mutate(party_family = party_family_from_parts(party_parts))

  cat(sprintf("\n--- Finalizing %s panel ---\n", version))

  # Within-mayor statistics
  person_stats <- p |>
    group_by(person_id) |>
    summarise(
      n_terms = n(),
      total_tenure_years = max(election_year) - min(election_year),
      has_margin_variation = n_distinct(round(winning_margin, 4), na.rm = TRUE) > 1,
      .groups = "drop"
    )
  p <- p |> left_join(person_stats, by = "person_id")

  # Final column selection
  panel_cols <- c(
    "person_id", "person_id_method", "source_person_term_number",
    "source_person_reelections", "ags",
    if (has_ags_21) "ags_21",
    "state", "election_year", "election_date",
    "term_number", "consecutive_terms", "winner_party", "party_family", "winner_voteshare",
    "winning_margin", "margin_change",
    "n_candidates", "is_incumbent", "was_predecessor_mayor", "next_runs_again",
    "party_switch", "is_new_party_mayor",
    "tenure_start", "years_in_office", "term_start_date",
    "n_terms", "total_tenure_years", "has_margin_variation",
    # Candidate characteristics
    "candidate_gender", "candidate_gender_source", "candidate_gender_prob",
    "candidate_gender_method",
    "candidate_migration_bg", "candidate_migration_bg_prob",
    "candidate_name_origin", "candidate_name_origin_conf",
    "candidate_name_origin_method",
    "lineage"  # internal, dropped below
  )
  p <- p |>
    select(all_of(panel_cols)) |>
    arrange(state, ags, election_date, person_id)

  # Build annual panel
  cat("  Building annual panel...\n")

  p <- p |>
    arrange(lineage, election_date) |>
    group_by(lineage) |>
    mutate(next_election_year = lead(election_year)) |>
    ungroup() |>
    # (a) the next election of the municipality, whether or not its winner is named
    left_join(term_caps, by = c("person_id", "ags", "election_date")) |>
    mutate(next_election_year = case_when(
      !is.na(next_cycle) &
        (is.na(next_election_year) | next_cycle < next_election_year) ~ as.integer(next_cycle),
      TRUE ~ next_election_year))

  # Last year the panel extends open terms to. Derived from the data — a
  # hardcoded 2025 combined with the missing pmax() guard below made every
  # term from a 2026 election generate a DESCENDING year sequence, emitting
  # 1,978 phantom rows dated BEFORE their own election (audit 2026-07, C-12).
  max_year <- max(p$election_year, na.rm = TRUE)
  cat(sprintf("  Open terms run to %d\n", max_year))

  # A term followed by another election in the SAME year (Hessen repeat and
  # by-elections, kept in the election panel above) holds office for none of
  # the calendar years: that year belongs to the later term. It gets no annual
  # row, which keeps one mayor per municipality-year. Every other term gets at
  # least its election year, as before.
  p_annual <- p |>
    filter(is.na(next_election_year) | next_election_year > election_year) |>
    mutate(
      term_end_year = case_when(
        !is.na(next_election_year) ~ pmax(next_election_year - 1L, election_year),
        TRUE ~ pmax(max_year, election_year)
      ),
      # (b) a municipality that ceased to exist ends the term -- not for an
      # election filed under a code whose last crosswalk year lies BEFORE it
      # (Brandenburg 1998 rows under codes ending 1997, Dessau OB 2007): that
      # term runs on to the next election or the legal maximum (c)
      term_end_year = if_else(!is.na(lineage_end) & lineage_end >= election_year,
                              pmin(term_end_year, lineage_end),
                              term_end_year),
      # (c) the longest legal term, where the source shows no election that
      # must have been held: only where that end lies inside the Land's data
      term_end_year = if_else(state != "16" & !is.na(max_term) &
                                election_year + max_term + 1L <= last_cycle,
                              pmin(term_end_year, as.integer(election_year + max_term)),
                              term_end_year),
      # (d) Thüringen: the six-year term (§ 28 ThürKO) when no later election
      # of the AGS is in any source. TH elections fall on fixed general dates
      # (2004 -> 2010), so here the cap is exact rather than lenient. Terms
      # from 2020 keep the rules above (their next elections are named).
      term_end_year = if_else(state == "16" & election_year < 2020 & is.na(next_cycle),
                              pmin(term_end_year, election_year + 5L),
                              term_end_year)
    ) |>
    rowwise() |>
    mutate(year = list(seq(election_year, term_end_year))) |>
    unnest(year) |>
    ungroup() |>
    mutate(
      years_since_election = year - election_year,
      years_to_next_election = case_when(
        !is.na(next_election_year) ~ next_election_year - year,
        TRUE ~ NA_integer_
      ),
      term_length = case_when(
        !is.na(next_election_year) ~ next_election_year - election_year,
        TRUE ~ NA_integer_
      ),
      electoral_cycle_pos = case_when(
        !is.na(term_length) & term_length > 0 ~
          years_since_election / term_length,
        TRUE ~ NA_real_
      )
    )

  annual_cols <- c(
    "ags",
    if (has_ags_21) "ags_21",
    "year", "person_id", "person_id_method", "source_person_term_number",
    "source_person_reelections", "state", "election_year", "election_date",
    "term_number", "winner_party", "party_family", "winner_voteshare", "winning_margin",
    "n_candidates", "is_incumbent", "was_predecessor_mayor", "next_runs_again",
    "years_since_election", "years_to_next_election",
    "electoral_cycle_pos", "tenure_start", "term_start_date",
    # Candidate characteristics (constant within term)
    "candidate_gender", "candidate_gender_source", "candidate_gender_prob",
    "candidate_gender_method",
    "candidate_migration_bg", "candidate_migration_bg_prob",
    "candidate_name_origin", "candidate_name_origin_conf",
    "candidate_name_origin_method"
  )
  p_annual <- p_annual |>
    select(all_of(annual_cols)) |>
    arrange(ags, year, person_id)

  # A municipality has exactly one mayor per year: no (ags, year) key may repeat,
  # and no row may predate its own election.
  stopifnot(all(p_annual$year >= p_annual$election_year))
  dup_ags_year <- p_annual |> count(ags, year) |> filter(n > 1)
  if (nrow(dup_ags_year) > 0) {
    print(as.data.frame(head(dup_ags_year, 20)))
    stop(sprintf("%s annual panel: %d duplicated (ags, year) keys",
                 version, nrow(dup_ags_year)))
  }

  # Remove temp column from election-level panel
  # (published row order: by code and date, as before lineages)
  p <- p |>
    select(-next_election_year, -next_cycle, -lineage_end, -max_term,
           -last_cycle, -lineage) |>
    arrange(ags, election_date, person_id)

  cat(sprintf("  Election panel: %d rows, %d persons\n",
              nrow(p), n_distinct(p$person_id)))
  cat(sprintf("  Annual panel: %d rows, years %d-%d\n",
              nrow(p_annual), min(p_annual$year), max(p_annual$year)))

  list(panel = p, annual = p_annual)
}

cat("\n=== Finalizing both versions ===\n")

unharm <- finalize_panel(panel_unharm, version = "unharm")
harm   <- finalize_panel(panel, version = "harm")


# ============================================================================
# 9. SUMMARY
# ============================================================================

cat("\n=== Mayor Panel Summary ===\n")
cat("Unharmonized (original boundaries):\n")
cat(sprintf("  Election panel: %d rows, %d persons\n",
            nrow(unharm$panel), n_distinct(unharm$panel$person_id)))
cat(sprintf("  Annual panel: %d rows\n", nrow(unharm$annual)))
cat("Harmonized (2021 boundaries):\n")
cat(sprintf("  Election panel: %d rows, %d persons\n",
            nrow(harm$panel), n_distinct(harm$panel$person_id)))
cat(sprintf("  Annual panel: %d rows\n", nrow(harm$annual)))

cat("\nState breakdown (harmonized):\n")
for (s in sort(unique(harm$panel$state))) {
  sub <- harm$panel |> filter(state == s)
  n_mayors <- n_distinct(sub$person_id)
  n_multi <- sub |>
    group_by(person_id) |>
    filter(n() >= 2) |>
    ungroup() |>
    pull(person_id) |>
    n_distinct()
  state_name <- case_when(
    s == "01" ~ "Schleswig-Holstein",
    s == "02" ~ "Hamburg",
    s == "03" ~ "Niedersachsen",
    s == "04" ~ "Bremen",
    s == "05" ~ "NRW",
    s == "06" ~ "Hessen",
    s == "07" ~ "Rheinland-Pfalz",
    s == "08" ~ "Baden-Württemberg",
    s == "09" ~ "Bayern",
    s == "10" ~ "Saarland",
    s == "11" ~ "Berlin",
    s == "12" ~ "Brandenburg",
    s == "13" ~ "Mecklenburg-Vorpommern",
    s == "14" ~ "Sachsen",
    s == "15" ~ "Sachsen-Anhalt",
    s == "16" ~ "Thüringen",
    TRUE ~ s
  )
  cat(sprintf("  %s (%s): %d mayors (%d with 2+ terms)\n",
              state_name, s, n_mayors, n_multi))
}


# ============================================================================
# 10. SAVE
# ============================================================================

cat("\n=== Saving ===\n")

# Unharmonized (original boundaries)
write_rds(unharm$panel, "data/mayoral_elections/final/mayor_panel.rds", compress = "gz")
fwrite(unharm$panel, "data/mayoral_elections/final/mayor_panel.csv")
cat("Saved mayor_panel.{rds,csv} (unharmonized)\n")

write_rds(unharm$annual, "data/mayoral_elections/final/mayor_panel_annual.rds", compress = "gz")
fwrite(unharm$annual, "data/mayoral_elections/final/mayor_panel_annual.csv")
cat("Saved mayor_panel_annual.{rds,csv} (unharmonized)\n")

# Harmonized (2021 boundaries)
write_rds(harm$panel, "data/mayoral_elections/final/mayor_panel_harm.rds", compress = "gz")
fwrite(harm$panel, "data/mayoral_elections/final/mayor_panel_harm.csv")
cat("Saved mayor_panel_harm.{rds,csv} (harmonized)\n")

write_rds(harm$annual, "data/mayoral_elections/final/mayor_panel_annual_harm.rds", compress = "gz")
fwrite(harm$annual, "data/mayoral_elections/final/mayor_panel_annual_harm.csv")
cat("Saved mayor_panel_annual_harm.{rds,csv} (harmonized)\n")

cat("\n=== Done ===\n")
