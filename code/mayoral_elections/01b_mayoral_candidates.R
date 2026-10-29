### Candidate-level mayoral election dataset
# Vincent Heddesheimer
# March 2026
#
# Creates a candidate-level dataset from mayoral election raw files.
# One row = one candidate in one election (long format).
# Companion to 01_mayoral_unharm.R which produces election-level summaries.
#
# Coverage:
# - Bayern (Bavaria) — 1945-2025 (party + votes only, no candidate names)
# - NRW (North Rhine-Westphalia) — 2009-2025 (full candidate names)
# - Saarland — 2019-2025 (names, gender, office type)
# - Sachsen (Saxony) — 2001-2024 (last names only)
# - Rheinland-Pfalz — 1994-2024 (names, percentages only)
# - Niedersachsen (Lower Saxony) — 2006-2025 (names, profession, birth year)

rm(list = ls())
gc()

# Load libraries
pacman::p_load(
  tidyverse,
  readxl,
  data.table,
  lubridate,
  conflicted,
  here,
  pdftools
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")

# Set working directory
setwd(here::here())

# Disallow scientific notation
options(scipen = 999)

# Helper function to pad AGS codes
pad_zero_conditional <- function(x, n, pad = "0") {
  x <- as.character(x)
  x[nchar(x) < n] <- paste0(pad, x[nchar(x) < n])
  return(x)
}

# Strip academic titles from a name part. Sources disagree on where the title
# sits — Sachsen's SIEGER column writes "Wilde, Dr. Manfred" while its own
# candidate column writes "Dr. Wilde, Manfred", and Niedersachsen 2013 prints
# "Dr. Junk, Oliver" — so the same person can end up with "Dr. Wilde" as a
# surname or "Dr. Manfred" as a given name. Both break the gender lookup and
# the HW/SW name pairing. `candidate_name` keeps the source string verbatim;
# only the split parts are cleaned. Loops because titles stack ("Prof. Dr.").
strip_name_titles <- function(x) {
  x <- as.character(x)
  ttl <- "^(Prof\\.|Dr\\.|Dipl\\.[-[:alpha:].]*|Ing\\.|med\\.|rer\\.|nat\\.|phil\\.|jur\\.|oec\\.|paed\\.|theol\\.|h\\.\\s?c\\.|habil\\.)\\s*"
  for (i in 1:4) x <- str_trim(str_replace(x, ttl, ""))
  ifelse(is.na(x) | !nzchar(x), NA_character_, x)
}

# Flag non-decisive Bayern rounds — i.e. rounds that did NOT seat the mayor and
# are superseded by a later valid round (mirrors compute_flag_superseded in
# 01_mayoral_unharm.R; KEEP THE TWO DEFINITIONS IN SYNC). Two cases:
#   (A) annulled rounds: Wahlart contains "ungültig" (Hauptwahl/Stichwahl ungültig).
#   (B) a Hauptwahl that did NOT seat a mayor — no absolute majority
#       (winner_voteshare < 0.5), NOT resolved by a Stichwahl within 60 days, and
#       followed by a repeat Hauptwahl (Neuwahl) for the same office within 250 days.
# The majority test separates a FAILED first round (<50%, repeated) from a by-election
# whose predecessor was duly ELECTED (>=50%, seated, then left office); the "completed
# by Stichwahl" test spares the normal first round of a runoff cycle.
# Rows are KEPT (never dropped); the flag just lets users exclude precursor rounds.
# Returns a logical vector aligned to the inputs; non-Bayern states default to FALSE.
compute_flag_superseded <- function(ags, election_type, election_date, round, wahlart,
                                    winner_voteshare) {
  d <- data.table::data.table(
    .i = seq_along(ags),
    ags = as.character(ags),
    etype = as.character(election_type),
    dt = as.Date(election_date),
    rnd = as.character(round),
    wa = as.character(wahlart),
    wv = as.numeric(winner_voteshare)
  )
  d[, annulled := !is.na(wa) & grepl("ungültig", wa, ignore.case = TRUE)]
  d[, `:=`(has_later_hw = FALSE, completed_by_sw = FALSE)]
  d[, `:=`(
    has_later_hw = vapply(seq_len(.N),
      function(k) any(rnd == "hauptwahl" & dt > dt[k] & dt <= dt[k] + 250, na.rm = TRUE),
      logical(1)),
    completed_by_sw = vapply(seq_len(.N),
      function(k) any(rnd == "stichwahl" & dt > dt[k] & dt - dt[k] < 60, na.rm = TRUE),
      logical(1))
  ), by = .(ags, etype)]
  d[, superseded_hw := rnd == "hauptwahl" & !is.na(wv) & wv < 0.5 &
                       has_later_hw & !completed_by_sw]
  d[order(.i), annulled | superseded_hw]
}

# ----------------------------------------------------------------------------
# Bayern office classifier — DUPLICATED VERBATIM in 01_mayoral_unharm.R.
# KEEP THE TWO COPIES IN SYNC (same convention as compute_flag_superseded above).
# ----------------------------------------------------------------------------
# The "Wahlen seit 1945" sheet leaves Amtstitel BLANK on many non-decisive rounds.
# A bare `TRUE ~ "Bürgermeisterwahl"` default therefore filed 125 Landrat rounds
# in the mayoral dataset (where they are also MISSING from landrat_unharm, which
# orphaned 119 of 148 Bayern Landrat runoffs) and typed the Hauptwahl of 129
# Oberbürgermeister cycles as an ordinary Bürgermeisterwahl.
# Two independent discriminators fill the blanks:
#   (1) THE AGS, at Kreis level. An 8-digit AGS ending in "000" is a Kreis: in
#       Bayern that is either one of the 25 kreisfreie Städte (Oberbürgermeister)
#       or a Landkreis (Landrat). The discriminator is clean — all 1,222
#       Landkreis-AGS rows in the raw sheet are Landrat elections and NOT ONE
#       carries a mayoral Amtstitel.
#   (2) THE COMPANION ROUND, below Kreis level. The ~57 blank rows sitting on a
#       Große-Kreisstadt AGS are the first round of a cycle whose Stichwahl two
#       weeks later IS labelled "Oberbürgermeister*in" — take the office from the
#       labelled round of the same election (same AGS, within 120 days).
# Anything still unresolved stays a Bürgermeisterwahl, as before.
BY_KREISFREIE_KREIS <- c(
  "09161", "09162", "09163",                              # Oberbayern
  "09261", "09262", "09263",                              # Niederbayern
  "09361", "09362", "09363",                              # Oberpfalz
  "09461", "09462", "09463", "09464",                     # Oberfranken
  "09561", "09562", "09563", "09564", "09565",            # Mittelfranken
  "09661", "09662", "09663",                              # Unterfranken
  "09761", "09762", "09763", "09764"                      # Schwaben
)

classify_bayern_office <- function(ags, amtstitel, election_date) {
  ags <- as.character(ags)
  dt  <- as.Date(election_date)

  # Office as stated by the source, NA where Amtstitel is blank
  titled <- dplyr::case_when(
    grepl("^Landrat|^Landrät", amtstitel)                 ~ "Landratswahl",
    grepl("^Oberbürgermeister", amtstitel)                ~ "Oberbürgermeisterwahl",
    !is.na(amtstitel) & nzchar(trimws(amtstitel))         ~ "Bürgermeisterwahl",
    TRUE                                                  ~ NA_character_
  )

  # (1) Kreis-level AGS
  kreis_level <- !is.na(ags) & nchar(ags) == 8 & substr(ags, 6, 8) == "000"
  from_ags <- ifelse(!kreis_level, NA_character_,
                     ifelse(substr(ags, 1, 5) %in% BY_KREISFREIE_KREIS,
                            "Oberbürgermeisterwahl", "Landratswahl"))
  out <- dplyr::coalesce(titled, from_ags)

  # (2) companion round of the same election
  # (skip NA/blank AGS — those rows are filtered out downstream anyway, and
  #  list[[NA_character_]] is an error, not a miss)
  need <- which(is.na(out) & !is.na(ags) & nzchar(ags))
  if (length(need) > 0) {
    idx_by_ags <- split(seq_along(ags), ags)
    for (i in need) {
      cand <- idx_by_ags[[ags[i]]]
      if (is.null(cand)) next
      cand <- cand[!is.na(titled[cand]) &
                     !is.na(dt[cand]) & !is.na(dt[i]) &
                     abs(as.numeric(dt[cand] - dt[i])) <= 120]
      if (!length(cand)) next
      lv <- unique(titled[cand])
      out[i] <- if ("Oberbürgermeisterwahl" %in% lv) "Oberbürgermeisterwahl" else
                if ("Landratswahl" %in% lv) "Landratswahl" else "Bürgermeisterwahl"
    }
  }

  dplyr::coalesce(out, "Bürgermeisterwahl")
}

# "" IS NOT NA. Several Stage-0 intermediates are CSVs and fread() returns a blank
# cell as an empty string, which then survives every !is.na() test downstream. The
# HW<->SW pairing engine keys on the candidate name, so an "" name made every
# unnamed row of an election share ONE match key — that is how 104 Hessen runoff
# winners ended up overwritten by their Hauptwahl leader. Normalise once, centrally.
na_if_blank <- function(x) {
  x <- as.character(x)
  ifelse(!is.na(x) & nzchar(trimws(x)), x, NA_character_)
}

# ONE gender vocabulary for the whole dataset: "m" / "w" (docs/codebook.md).
# The BW (00_bw_parse.py) and Bayern-2026 (00_by_kommunalwahl2026_parse.py)
# Stage-0 parsers emit male/female; every other state emits m/w. Recoding here
# means all 16 state blocks inherit it (they all call standardise_candidates()).
recode_gender <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y <- ifelse(y %in% c("m", "male", "mann", "maennlich", "männlich"), "m",
       ifelse(y %in% c("w", "f", "female", "frau", "weiblich"), "w", y))
  ifelse(!is.na(y) & nzchar(y), y, NA_character_)
}

# Standardise output columns — ensures all expected columns exist with correct types
standardise_candidates <- function(dt) {
  output_cols <- c(
    "ags", "ags_name", "state", "state_name", "election_year", "election_date",
    "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
    "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
    "candidate_first_name", "candidate_gender", "candidate_party",
    "candidate_votes", "candidate_voteshare", "candidate_birth_year",
    "candidate_profession", "office_type",
    "n_candidates", "candidate_rank", "is_winner", "flag_superseded",
    # Sachsen-Anhalt only: the source shares this AGS with a second Gemeinde on
    # this date (see the ST block). NA for every other state.
    "flag_shared_ags",
    # The round that DECIDED this election cycle is not in the source: the
    # rounds we hold are real but seated nobody, so the cycle has no winner.
    # Set by a Stage-0 parser that knows its delivery is incomplete (currently
    # only Hagenow 2015, MV); FALSE everywhere else. The global winner-repair
    # step at the end of this script keys on it — without it, "no candidate
    # flagged" is read as "the source is silent" and the leader of an
    # inconclusive Hauptwahl gets crowned.
    "flag_decisive_round_missing",
    # INTERNAL, never published. `pair_key`: a Stage-0 parser's explicit link
    # between a Stichwahl candidate and the same person's Hauptwahl row; when
    # set it outranks the name/party/rank match key in the wide pivot (Hessen,
    # 00_he_hist_parse.py). `name_restricted`: the name came from a source we
    # publish only for elected persons (Hessen losers named by Hessami 2018);
    # it is kept in the restricted twin and stripped from the public file.
    "pair_key", "name_restricted"
  )

  # Add missing columns as NA
  for (col in output_cols) {
    if (!col %in% names(dt)) dt[[col]] <- NA
  }

  dt <- dt %>%
    mutate(
      ags = as.character(ags),
      ags_name = as.character(ags_name),
      state = as.character(state),
      state_name = as.character(state_name),
      election_year = as.numeric(election_year),
      election_date = as.Date(election_date),
      election_type = as.character(election_type),
      round = as.character(round),
      eligible_voters = as.numeric(eligible_voters),
      number_voters = as.numeric(number_voters),
      valid_votes = as.numeric(valid_votes),
      invalid_votes = as.numeric(invalid_votes),
      turnout = as.numeric(turnout),
      # Blank -> NA (see na_if_blank above): the pairing engine keys on the name.
      candidate_name = na_if_blank(candidate_name),
      candidate_last_name = na_if_blank(candidate_last_name),
      candidate_first_name = na_if_blank(candidate_first_name),
      candidate_gender = recode_gender(candidate_gender),
      candidate_party = as.character(candidate_party),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      candidate_birth_year = as.numeric(candidate_birth_year),
      candidate_profession = as.character(candidate_profession),
      office_type = as.character(office_type),
      n_candidates = as.integer(n_candidates),
      candidate_rank = as.integer(candidate_rank),
      is_winner = as.logical(is_winner),
      flag_superseded = as.logical(flag_superseded),
      flag_shared_ags = as.logical(flag_shared_ags),
      flag_decisive_round_missing = as.logical(flag_decisive_round_missing),
      pair_key = na_if_blank(pair_key),
      name_restricted = dplyr::coalesce(as.logical(name_restricted), FALSE)
    ) %>%
    select(all_of(output_cols))

  return(dt)
}

# Initialize list to store all state data
all_candidate_data <- list()

# ============================================================================
# BAYERN (Bavaria)
# ============================================================================

cat("\n=== Processing Bayern mayoral elections ===\n")

bayern_file <- "data/mayoral_elections/raw/bayern/20251114_Wahlen_seit_1945.xlsx"

bayern_raw <- read_excel(bayern_file, sheet = "20251114_bewerberRBZ1-7") %>%
  as.data.table()
source("code/mayoral_elections/by_source_duplicates.R")
bayern_raw <- drop_by_source_duplicates(bayern_raw)

# Build base election-level info
bayern_base <- bayern_raw %>%
  filter(!is.na(Gemeindeschlüssel), !is.na(`Tag der Wahl`)) %>%
  mutate(
    gemeindeschluessel_char = as.character(Gemeindeschlüssel),
    gemeindeschluessel_padded = str_pad(gemeindeschluessel_char, width = 6,
                                         side = "left", pad = "0"),
    ags = paste0("09", gemeindeschluessel_padded),
    election_date = as.Date(`Tag der Wahl`),
    election_year = lubridate::year(election_date),
    state = "09",
    state_name = "Bayern",
    # Classify by the source's own Amtstitel column, falling back to the AGS and
    # to the companion round where Amtstitel is blank — see
    # classify_bayern_office() above and
    # docs/mayoral_elections_known_issues.md §14.
    election_type = classify_bayern_office(ags, Amtstitel, election_date),
    # Round: Wahlart distinguishes "erster Wahlgang" from "Stichwahl"
    round = case_when(
      Wahlart %in% c("Stichwahl", "Stichwahl ungültig", "Losentscheid") ~ "stichwahl",
      TRUE ~ "hauptwahl"  # "erster Wahlgang", "Hauptwahl ungültig", NA
    ),
    eligible_voters = as.numeric(Stimmberechtigte),
    number_voters = as.numeric(Wähler),
    valid_votes = as.numeric(`Gültige Stimmen`),
    invalid_votes = as.numeric(`ungültige Stimmen`),
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                     number_voters / eligible_voters, NA),
    ags_name = Gemeindename,
    office_type = as.character(Amtstitel)
  ) %>%
  # Flag annulled / superseded precursor rounds (see compute_flag_superseded).
  # winner_voteshare = the round winner's share of valid votes (same formula as
  # 01_mayoral_unharm.R) — used by the majority test inside the function.
  mutate(
    flag_superseded = compute_flag_superseded(
      ags, election_type, election_date, round, Wahlart,
      ifelse(!is.na(valid_votes) & valid_votes > 0,
             as.numeric(`gültige Stimmen Wahlgewinner`) / valid_votes, NA_real_))
  ) %>%
  filter(!is.na(ags), nchar(ags) == 8)

# Reshape candidates: winner + Bewerber 2-14 → long format
# Winner columns: Wahlvorschlag Wahlgewinner, gültige Stimmen Wahlgewinner
# Bewerber X columns: Wahlvorschlag Bewerber X, gültige Stimmen Bewerber X

# Build candidate rows for winner (rank 1)
bayern_winner <- bayern_base %>%
  mutate(
    candidate_party = `Wahlvorschlag Wahlgewinner`,
    candidate_votes = as.numeric(`gültige Stimmen Wahlgewinner`)
  ) %>%
  filter(!is.na(candidate_party) | !is.na(candidate_votes)) %>%
  select(ags, ags_name, state, state_name, election_year, election_date,
         election_type, round, eligible_voters, number_voters, valid_votes,
         invalid_votes, turnout, office_type, candidate_party, candidate_votes,
         flag_superseded)

# Build candidate rows for Bewerber 2-14
bayern_bewerber_list <- list()
for (i in 2:14) {
  party_col <- paste0("Wahlvorschlag Bewerber ", i)
  votes_col <- paste0("gültige Stimmen Bewerber ", i)

  if (!party_col %in% names(bayern_base) || !votes_col %in% names(bayern_base)) next

  bewerber_i <- bayern_base %>%
    mutate(
      candidate_party = .data[[party_col]],
      candidate_votes = as.numeric(.data[[votes_col]])
    ) %>%
    filter(!is.na(candidate_party) | !is.na(candidate_votes)) %>%
    select(ags, ags_name, state, state_name, election_year, election_date,
           election_type, round, eligible_voters, number_voters, valid_votes,
           invalid_votes, turnout, office_type, candidate_party, candidate_votes,
           flag_superseded)

  if (nrow(bewerber_i) > 0) {
    bayern_bewerber_list[[i]] <- bewerber_i
  }
}

bayern_candidates <- bind_rows(bayern_winner, bind_rows(bayern_bewerber_list)) %>%
  mutate(
    candidate_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                   !is.na(candidate_votes),
                                 candidate_votes / valid_votes, NA)
  ) %>%
  # Compute rank and n_candidates per election
  group_by(ags, election_date) %>%
  mutate(
    candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup()

bayern_clean <- standardise_candidates(bayern_candidates)

cat("Bayern: Processed", nrow(bayern_clean), "candidate rows from",
    n_distinct(paste(bayern_clean$ags, bayern_clean$election_date)), "elections\n")
all_candidate_data[["bayern"]] <- bayern_clean

# ============================================================================
# NRW (North Rhine-Westphalia)
# ============================================================================

cat("\n=== Processing NRW mayoral elections ===\n")

nrw_all_files <- list.files("data/mayoral_elections/raw/nrw/",
                             pattern = "\\.xlsx$",
                             full.names = TRUE,
                             ignore.case = TRUE)
nrw_bm_files <- nrw_all_files[grepl("B.*rgermeisterwahlen", basename(nrw_all_files),
                                      ignore.case = TRUE) &
                                 !grepl("Oberb.*rgermeister", basename(nrw_all_files),
                                        ignore.case = TRUE)]
nrw_ob_files <- nrw_all_files[grepl("Oberb.*rgermeister", basename(nrw_all_files),
                                      ignore.case = TRUE)]

cat("Found", length(nrw_bm_files), "NRW BM files,", length(nrw_ob_files), "NRW OB files\n")

# Process a single NRW Excel file — returns candidate-level long format
process_nrw_candidates <- function(file, skip_rows, default_election_type) {
  election_year <- as.numeric(str_extract(basename(file), "\\d{4}"))

  nrw_raw <- read_excel(file, sheet = 1, skip = skip_rows, col_names = FALSE) %>%
    as.data.table()

  nrw_raw <- nrw_raw %>%
    filter(!grepl("GKZ|Gemeinde|Kreis", as.character(.[[1]]), ignore.case = TRUE))

  if (ncol(nrw_raw) < 10) return(NULL)

  # Reshape: each candidate is a 3-column block starting at column 8
  n_cand_cols <- ncol(nrw_raw) - 7
  n_candidates <- n_cand_cols %/% 3

  cand_list <- list()
  for (i in seq_len(n_candidates)) {
    start_col <- 7 + (i - 1) * 3 + 1
    if (start_col + 2 > ncol(nrw_raw)) break
    cand_df <- nrw_raw[, c(1:7, start_col, start_col + 1, start_col + 2), with = FALSE]
    names(cand_df) <- c("gkz", "gemeinde", "datum", "wahlberechtigte", "waehler",
                         "ungueltige", "gueltige", "name", "wahlvorschlag", "stimmen")
    cand_list[[i]] <- cand_df
  }
  nrw_long <- bind_rows(cand_list)

  nrw_clean <- nrw_long %>%
    mutate(
      gkz = as.character(gkz),
      gemeinde = as.character(gemeinde),
      datum = as.character(datum),
      wahlberechtigte = as.numeric(wahlberechtigte),
      waehler = as.numeric(waehler),
      ungueltige = as.numeric(ungueltige),
      gueltige = as.numeric(gueltige),
      name = as.character(name),
      wahlvorschlag = as.character(wahlvorschlag),
      stimmen = as.numeric(stimmen)
    ) %>%
    filter(!is.na(gkz),
           !grepl("GKZ|Gemeinde|Kreis", gkz, ignore.case = TRUE),
           grepl("^[0-9]+$", gsub("[^0-9]", "", gkz)),
           nchar(gsub("[^0-9]", "", gkz)) >= 4,
           !is.na(stimmen),
           # Remove empty candidate slots (votes=0, no name, no party)
           !(stimmen == 0 & is.na(name) & is.na(wahlvorschlag)))

  if (nrow(nrw_clean) == 0) return(NULL)

  date_from_title <- str_extract(basename(file), "\\d{2}\\.\\d{2}\\.\\d{4}")
  default_date <- if (!is.na(date_from_title)) {
    as.Date(date_from_title, format = "%d.%m.%Y")
  } else {
    as.Date(paste0(election_year, "-01-01"))
  }

  nrw_clean <- nrw_clean %>%
    mutate(
      gkz_clean = str_pad(gsub("[^0-9]", "", gkz), width = 6, side = "left", pad = "0"),
      # IT.NRW still lists Aachen under its pre-reform code 313000 in every
      # file. The kreisfreie Stadt was dissolved into the Staedteregion on
      # 21.10.2009 and the city's code has been 05334002 since; 05313000 exists
      # in no crosswalk year after 2008, so the 2014 and 2020 elections were
      # silently dropped at harmonisation and 2025 was published under a code
      # that no longer exists. Recode everything after the reform.
      # (the 2009 election fell on 30 August, before the 21 October reform)
      ags = ifelse(gkz_clean == "313000" & election_year > 2009,
                   "05334002", paste0("05", gkz_clean)),
      state = "05",
      state_name = "Nordrhein-Westfalen",
      ags_name = gemeinde,
      # Determine election type from the gemeinde (name) column. The AGS suffix
      # alone is ambiguous in NRW — both kreisfreie Städte and Landkreise have
      # AGS ending in "000". Use the source's own naming convention:
      #   "Krfr. Stadt X" / "Kreisfreie Stadt X" → Oberbürgermeisterwahl
      #   "Kreis X" / "Rhein-Erft-Kreis" / "Hochsauerlandkreis" / "Städteregion X" → Landratswahl
      election_type = case_when(
        default_election_type == "Bürgermeisterwahl" ~ "Bürgermeisterwahl",
        grepl("^Kreis |[Kk]reis$|-Kreis|, Kreis|Städteregion|Stadtregion", gemeinde) ~ "Landratswahl",
        grepl("Stadt", gemeinde) ~ "Oberbürgermeisterwahl",
        TRUE ~ "Oberbürgermeisterwahl"
      ),
      datum_num = suppressWarnings(as.numeric(datum)),
      datum_text = suppressWarnings(as.Date(datum, format = "%d.%m.%Y")),
      election_date = as.Date(case_when(
        !is.na(datum_num) & datum_num > 30000 & datum_num < 60000 ~
          as.character(as.Date(datum_num, origin = "1899-12-30")),
        !is.na(datum_text) ~ as.character(datum_text),
        TRUE ~ as.character(default_date)
      )),
      election_year = year(election_date),
      eligible_voters = wahlberechtigte,
      number_voters = waehler,
      valid_votes = gueltige,
      invalid_votes = ungueltige,
      turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0 &
                         !is.na(number_voters),
                       number_voters / eligible_voters, NA),
      # Parse candidate names
      candidate_name = str_trim(name),
      candidate_last_name = str_trim(str_extract(name, "^[^,]+")),
      candidate_first_name = str_trim(str_extract(name, ",\\s*(.+)$", group = 1)),
      candidate_party = str_trim(wahlvorschlag),
      candidate_votes = stimmen,
      candidate_voteshare = ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                     !is.na(stimmen),
                                   stimmen / valid_votes, NA)
    )

  # IT.NRW data error: in `KW 2025 Oberbürgermeister-Landratswahlen.xlsx` every
  # Stichwahl row has cell value 44101 (= 2020-09-27) hardcoded in column C
  # instead of 45928 (= 2025-09-28). Verified via direct XML inspection of the
  # source file — the row's vote totals match the 2025 SW (e.g. Düsseldorf
  # Keller 120430 ≠ 2020 SW 118308; Bonn row pairs Déus + Dörner, but Déus
  # only ran in 2025). Patch the dates so the 2025 SW data is usable. Remove
  # this once IT.NRW corrects the source file.
  if (election_year == 2025 && grepl("Oberb", basename(file))) {
    n_patch <- sum(nrw_clean$election_date == as.Date("2020-09-27"), na.rm = TRUE)
    if (n_patch > 0) {
      cat("  IT.NRW 2025 patch:", n_patch, "rows 2020-09-27 → 2025-09-28\n")
      nrw_clean <- nrw_clean %>%
        mutate(
          election_date = if_else(election_date == as.Date("2020-09-27"),
                                  as.Date("2025-09-28"),
                                  election_date),
          election_year = year(election_date)
        )
    }
  }

  nrw_clean <- nrw_clean %>%
    # Detect Stichwahl: NRW Excel files list HW and SW as separate rows
    # with different dates. Within each ags+election_type, duplicate GKZ rows
    # with a date within 60 days of each other form an election cycle.
    # The earlier date is hauptwahl, the later is stichwahl.
    # We must handle this per-file since files can span multiple election cycles.
    group_by(ags, election_type) %>%
    arrange(election_date) %>%
    mutate(
      # Assign election cycle: dates more than 60 days apart = different cycle
      date_gap = as.numeric(election_date - lag(election_date, default = election_date[1])),
      cycle = cumsum(date_gap > 60),
      .groups = "drop"
    ) %>%
    group_by(ags, election_type, cycle) %>%
    mutate(
      min_date = min(election_date),
      round = ifelse(election_date == min_date, "hauptwahl", "stichwahl")
    ) %>%
    ungroup() %>%
    select(-date_gap, -cycle, -min_date) %>%
    # Compute rank and n_candidates per election-round
    group_by(ags, election_date, election_type, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    select(ags, ags_name, state, state_name, election_year, election_date,
           election_type, round, eligible_voters, number_voters, valid_votes,
           invalid_votes, turnout, candidate_name, candidate_last_name,
           candidate_first_name, candidate_party, candidate_votes,
           candidate_voteshare, candidate_rank, n_candidates, is_winner)

  return(nrw_clean)
}

nrw_list <- list()
for (file in nrw_bm_files) {
  cat("Processing BM:", basename(file), "\n")
  result <- process_nrw_candidates(file, skip_rows = 2,
                                    default_election_type = "Bürgermeisterwahl")
  if (!is.null(result) && nrow(result) > 0) nrw_list[[basename(file)]] <- result
}
for (file in nrw_ob_files) {
  cat("Processing OB:", basename(file), "\n")
  result <- process_nrw_candidates(file, skip_rows = 3,
                                    default_election_type = "Oberbürgermeisterwahl")
  if (!is.null(result) && nrow(result) > 0) nrw_list[[basename(file)]] <- result
}

if (length(nrw_list) > 0 && any(sapply(nrw_list, nrow) > 0)) {
  nrw_combined <- bind_rows(nrw_list) %>%
    # Deduplicate: same candidate in same election-round can appear in multiple files
    # (e.g. 2020 Stichwahl results appear in both 2020 and 2025 OB files)
    distinct(ags, election_date, election_type, round, candidate_name, .keep_all = TRUE) %>%
    # Recompute rank after dedup
    group_by(ags, election_date, election_type, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    arrange(ags, election_year, election_date, candidate_rank)
} else {
  nrw_combined <- data.frame()
}

nrw_clean <- standardise_candidates(nrw_combined)

cat("NRW: Processed", nrow(nrw_clean), "candidate rows from",
    n_distinct(paste(nrw_clean$ags, nrw_clean$election_date)), "elections\n")
all_candidate_data[["nrw"]] <- nrw_clean

# ============================================================================
# SAARLAND
# ============================================================================

cat("\n=== Processing Saarland mayoral elections ===\n")

saarland_file <- "data/mayoral_elections/raw/saarland/Wahldaten_Bürgermeisterwahlen_2019-2025.xlsx"

saarland_raw <- read_excel(saarland_file, sheet = "Erfassung") %>%
  as.data.table()

# Column: Wahlberechtigte/Wähler/Gültige/Ungültige/Partei/Einzelbewerber
# Summary rows have info_type in {Wahlberechtigte, Wähler/-innen, Wähler, Gültige, Ungültige}
# Candidate rows have info_type = party name or "Einzelbewerber"
summary_types <- c("Wahlberechtigte", "Wähler/-innen", "Wähler", "Gültige", "Ungültige")

saarland_candidates <- saarland_raw %>%
  mutate(
    # The source AGS column omits the state prefix. Municipalities carry a
    # 5-digit code ("41511" -> 10041511), but the Kreis-level Regionalverband
    # Saarbrücken carries only its 2-digit Kreis code ("41"). Left-padding that
    # to six digits produced 10000041, which is not a valid AGS: for a
    # Kreis-level unit the Kreis code belongs in positions 3-5 with "000" as the
    # municipality part, i.e. 10041000 (matching county_elec_unharm's 10041100
    # and the sibling Landkreise 10042000, 10043000, ...).
    ags = ifelse(
      nchar(as.character(AGS)) <= 3,
      paste0("10", str_pad(as.character(AGS), width = 3, side = "left", pad = "0"), "000"),
      paste0("10", str_pad(as.character(AGS), width = 6, side = "left", pad = "0"))
    ),
    state = "10",
    state_name = "Saarland",
    election_year = Wahljahr,
    election_date = as.Date(paste(Wahljahr, Monat, Tag, sep = "-")),
    ags_name = `Gemeinde/Kreis`,
    info_type = `Wahlberechtigte/Wähler/Gültige/Ungültige/Partei/Einzelbewerber`,
    value = as.numeric(`Absolute Stimmen`),
    pct = as.numeric(`Stimmen in Prozent`)
  ) %>%
  filter(!is.na(ags), nchar(ags) == 8) %>%
  # An "…abwahl" round is a RECALL referendum (Ja/Nein), not an election —
  # KEEP IN SYNC with the same filter in 01_mayoral_unharm.R (Homburg 10045114
  # 2021-11-28 entered mayor_panel as a mayor elected with 75.0% for party "Ja").
  filter(!grepl("abwahl", `Wahlart...3`, ignore.case = TRUE)) %>%
  # Classify the office. Regionalverband Saarbrücken is the SL equivalent of a
  # Landkreis (head election = "Regionalverbandsdirektor"). The Wahlart column
  # names the office only on Hauptwahl rows, so resolve it per municipality —
  # see the fuller note on the twin block in 01_mayoral_unharm.R.
  group_by(ags) %>%
  mutate(
    election_type = case_when(
      grepl("Regionalverband", ags_name) ~ "Landratswahl",
      any(grepl("Oberbürgermeister", `Wahlart...3`)) ~ "Oberbürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    )
  ) %>%
  ungroup()

# Extract election-level summary stats
saarland_election_stats <- saarland_candidates %>%
  filter(info_type %in% summary_types) %>%
  group_by(ags, ags_name, election_date) %>%
  summarise(
    eligible_voters = value[info_type == "Wahlberechtigte"][1],
    number_voters = value[info_type %in% c("Wähler/-innen", "Wähler")][1],
    invalid_votes = value[info_type == "Ungültige"][1],
    valid_votes = value[info_type == "Gültige"][1],
    .groups = "drop"
  ) %>%
  mutate(
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0 &
                       !is.na(number_voters),
                     number_voters / eligible_voters, NA)
  )

# Extract candidate rows
saarland_cand_rows <- saarland_candidates %>%
  filter(!info_type %in% summary_types) %>%
  mutate(
    candidate_first_name = as.character(Vorname),
    candidate_last_name = as.character(Nachname),
    candidate_name = ifelse(!is.na(candidate_last_name),
                            paste0(candidate_last_name,
                                   ifelse(!is.na(candidate_first_name),
                                          paste0(", ", candidate_first_name), "")),
                            NA_character_),
    candidate_gender = as.character(Geschlecht),
    candidate_party = as.character(info_type),
    candidate_votes = value,
    candidate_voteshare = pct / 100,
    office_type = as.character(`Amt (Ehrenamt/Hauptamt)`),
    # Round: Wahlart...3 distinguishes "Bürgermeisterwahl" from "Stichwahl"
    round = case_when(
      `Wahlart...3` == "Stichwahl" ~ "stichwahl",
      TRUE ~ "hauptwahl"
    )
  ) %>%
  select(ags, ags_name, state, state_name, election_year, election_date,
         election_type, round, candidate_name, candidate_last_name,
         candidate_first_name, candidate_gender, candidate_party,
         candidate_votes, candidate_voteshare, office_type)

# Merge election stats with candidate rows
saarland_merged <- saarland_cand_rows %>%
  left_join(saarland_election_stats,
            by = c("ags", "ags_name", "election_date")) %>%
  group_by(ags, election_date, round) %>%
  mutate(
    candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup()

cat("  Saarland round distribution:\n")
print(table(saarland_merged$round))
saarland_clean <- standardise_candidates(saarland_merged)

cat("Saarland: Processed", nrow(saarland_clean), "candidate rows from",
    n_distinct(paste(saarland_clean$ags, saarland_clean$election_date)), "elections\n")
all_candidate_data[["saarland"]] <- saarland_clean

# ============================================================================
# SACHSEN (Saxony)
# ============================================================================

cat("\n=== Processing Sachsen mayoral elections ===\n")

sachsen_file <- "data/mayoral_elections/raw/sachsen/Bürgermeisterlatlas2001bis2024.xlsx"

sachsen_raw <- read_excel(sachsen_file, sheet = "Bürgermeisteratlas", col_names = TRUE) %>%
  as.data.table()

# Build election-level base
sachsen_base <- sachsen_raw %>%
  filter(!is.na(ORTNR)) %>%
  mutate(
    ags = as.character(ORTNR),
    ags_name = ORTNAME,
    state = "14",
    state_name = "Sachsen",
    election_year = as.numeric(Jahr),
    election_date = dmy(KW_TERMIN),
    # The workbook's own KW_OB column (documented on its Erklärung_Legende
    # sheet: J = Oberbürgermeister, N = Bürgermeister) — 273 J rows across 96
    # ORTNR, i.e. the kreisfreie Städte PLUS the Großen Kreisstädte, the same
    # convention BW (96 GKS) and BY (Amtstitel) already follow. Hard-coding
    # "Bürgermeisterwahl" made Sachsen contribute 0 Oberbürgermeisterwahl rows
    # while 11 other states supply ~1,400. Do NOT substitute an
    # AGS-ends-in-"000" rule: it catches only 31 of the 273 rows and is static,
    # while Radeberg, Geithain, Torgau and Klingenthal switch J<->N over the
    # period as they gain or lose Große-Kreisstadt status.
    election_type = ifelse(!is.na(KW_OB) & KW_OB == "J",
                           "Oberbürgermeisterwahl", "Bürgermeisterwahl"),
    status_raw = Status,
    eligible_voters = as.numeric(Wahlberechtigte),
    number_voters = as.numeric(`Wähler`),
    valid_votes = as.numeric(`gültige Stimmen`),
    invalid_votes = as.numeric(`ungültige Stimmen`),
    turnout = ifelse(!is.na(eligible_voters) & eligible_voters > 0,
                     number_voters / eligible_voters, NA)
  )

# Detect round from Status: VE = first round (runoff needed), EE = final result
# EE rows with a matching VE row (same municipality, 7-35 days earlier) = Stichwahl
sn_ve_dates <- sachsen_base %>%
  filter(status_raw == "VE") %>%
  select(ags, ve_date = election_date) %>%
  distinct()

sn_ee_has_ve <- sachsen_base %>%
  filter(status_raw == "EE") %>%
  select(ags, election_date) %>%
  left_join(sn_ve_dates, by = "ags", relationship = "many-to-many") %>%
  mutate(gap = as.numeric(election_date - ve_date)) %>%
  filter(gap > 0, gap < 60) %>%
  distinct(ags, election_date) %>%
  mutate(is_stichwahl_ee = TRUE)

sachsen_base <- sachsen_base %>%
  left_join(sn_ee_has_ve, by = c("ags", "election_date")) %>%
  mutate(
    round = case_when(
      status_raw == "VE" ~ "hauptwahl",
      is_stichwahl_ee == TRUE ~ "stichwahl",
      TRUE ~ "hauptwahl"  # Standalone EE = won outright in first round
    )
  ) %>%
  select(-status_raw, -is_stichwahl_ee)

cat("  Sachsen round distribution:\n")
print(table(sachsen_base$round))

# Is an _LNAME cell a PERSON, or a Wahlvorschlag label?
#
# The column holds a person ("Möller, Ines") for Einzelbewerber/Einzelvorschlag
# candidates and the list's own name for everyone else. A bare `grepl(",", x)`
# test therefore misreads every JOINT list — "Freie Wähler, DIE LINKE, SPD",
# "Christlich Demokratische Union Deutschlands, Bürgerbewegung" — as
# "Surname, Firstname", which put 39 party labels into the published data as
# people (13 of them as election winners). Measured on the workbook: this rule
# demotes 59 such cells and promotes none, i.e. it loses no real name.
SN_LIST_WORDS <- paste0(
  "e\\. ?V\\.|Wahlvorschlag|Wählerverein|Wählergemeinschaft|",
  "Wählervereinigung|Freie Wähler|Wahlplattform|Bürgerinitiative|",
  "Bürgerbewegung|B[üu]ndnis|Partei|Union Deutschlands|Sozialdemokratische|",
  "Christlich|Demokratische|Liberale|DIE LINKE|GR[ÜU]NE|Initiative|",
  "Vereinigung|Plattform|Ortsverband|Gemeinsamer|SPD|CDU|FDP|AfD|NPD|CSU|PDS")

sn_is_person <- function(x) {
  x <- as.character(x)
  after <- str_trim(str_extract(x, ",\\s*(.+)$", group = 1))
  !is.na(x) &
    str_count(ifelse(is.na(x), "", x), ",") == 1 &      # "Last, First", not a list of parties
    ifelse(is.na(after), 99L, str_count(after, "\\S+")) <= 3 &
    nchar(x) <= 45 &
    !str_detect(x, regex(SN_LIST_WORDS, ignore_case = TRUE))
}

# Reshape candidate columns 1-13 to long format
# Columns: X_WVT (party), X_LNAME (last name), X_Stimmen (votes), X_% (pct)
sachsen_cand_list <- list()
for (i in 1:13) {
  wvt_col <- paste0(i, "_WVT")
  lname_col <- paste0(i, "_LNAME")
  stimmen_col <- paste0(i, "_Stimmen")
  pct_col <- paste0(i, "_%")

  # Check if columns exist
  cols_exist <- c(stimmen_col) %in% names(sachsen_base)
  if (!all(cols_exist)) next

  cand_i <- sachsen_base %>%
    mutate(
      candidate_party = if (wvt_col %in% names(.)) as.character(.data[[wvt_col]]) else NA_character_,
      candidate_last_name = if (lname_col %in% names(.)) as.character(.data[[lname_col]]) else NA_character_,
      candidate_votes = as.numeric(.data[[stimmen_col]]),
      candidate_voteshare_pct = if (pct_col %in% names(.)) as.numeric(.data[[pct_col]]) else NA_real_
    ) %>%
    # Only keep rows where this candidate slot has real data:
    # require votes > 0 OR a non-NA party/name (filters out empty Excel columns parsed as 0)
    filter((!is.na(candidate_votes) & candidate_votes > 0) |
             !is.na(candidate_party) | !is.na(candidate_last_name)) %>%
    mutate(
      # _LNAME contains actual name for EB/EV candidates, or the full list name
      # for party candidates — see sn_is_person() above for why the presence of
      # a comma is not sufficient to tell the two apart.
      is_person_name = sn_is_person(candidate_last_name),
      candidate_name = ifelse(is_person_name, candidate_last_name, NA_character_),
      # For person names: parse into last/first
      candidate_last_name = ifelse(is_person_name,
                                    strip_name_titles(str_trim(str_extract(candidate_last_name, "^[^,]+"))),
                                    NA_character_),
      candidate_first_name = ifelse(is_person_name,
                                     strip_name_titles(str_trim(str_extract(candidate_name, ",\\s*(.+)$", group = 1))),
                                     NA_character_),
      candidate_voteshare = ifelse(!is.na(candidate_voteshare_pct),
                                    candidate_voteshare_pct / 100,
                                    ifelse(!is.na(valid_votes) & valid_votes > 0 &
                                             !is.na(candidate_votes),
                                           candidate_votes / valid_votes, NA))
    ) %>%
    select(ags, ags_name, state, state_name, election_year, election_date,
           election_type, round, eligible_voters, number_voters, valid_votes,
           invalid_votes, turnout, candidate_name, candidate_last_name,
           candidate_first_name, candidate_party, candidate_votes,
           candidate_voteshare)

  if (nrow(cand_i) > 0) sachsen_cand_list[[i]] <- cand_i
}

sachsen_candidates <- bind_rows(sachsen_cand_list) %>%
  group_by(ags, election_date) %>%
  mutate(
    candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup() %>%
  arrange(ags, election_date, candidate_rank)

# ---- Recover the winner's name from the workbook's own SIEGER column --------
# SIEGER names the winner of every DECIDED round as "Last, First (Party)"
# ("Seifert, Dr. Peter (SPD)"); rounds that went to a runoff carry the literal
# "2. Wahlgang erforderlich" instead. 01_mayoral_unharm.R already reads the
# party out of it, but the NAME was never used, which is why Sachsen winners
# were less often named (28%) than Sachsen losers (56%): the _LNAME column
# names only the Einzelbewerber, and most winners run on a party list.
#
# The fill is strictly ADDITIVE — it only touches a winner row that has no name
# — so the 17 rounds where SIEGER and _LNAME disagree (title placement
# "Dr. Wilde, Manfred" vs "Wilde, Dr. Manfred", the spelling variant
# Sarembe/Sarambe, and the swapped fields "Michael, Hultsch") keep the
# candidate column's version and need no adjudication.
sn_sieger <- sachsen_base %>%
  filter(!is.na(SIEGER), SIEGER != "2. Wahlgang erforderlich") %>%
  transmute(
    ags, election_date,
    sg_name  = str_trim(str_replace(SIEGER, "\\s*\\([^)]*\\)\\s*$", "")),
    sg_last  = strip_name_titles(str_trim(str_extract(sg_name, "^[^,]+"))),
    sg_first = strip_name_titles(str_trim(str_extract(sg_name, ",\\s*(.+)$", group = 1)))
  ) %>%
  filter(!is.na(sg_last), !is.na(sg_first)) %>%
  distinct(ags, election_date, .keep_all = TRUE)

sachsen_candidates <- sachsen_candidates %>%
  left_join(sn_sieger, by = c("ags", "election_date")) %>%
  mutate(
    .filled = is_winner %in% TRUE & is.na(candidate_name) & !is.na(sg_last),
    candidate_name       = ifelse(.filled, sg_name,  candidate_name),
    candidate_last_name  = ifelse(.filled, sg_last,  candidate_last_name),
    candidate_first_name = ifelse(.filled, sg_first, candidate_first_name)
  )
cat("  Sachsen: named", sum(sachsen_candidates$.filled),
    "winner(s) from the SIEGER column\n")

# Carry that name back to the SAME PERSON in the Hauptwahl of a runoff cycle.
# SIEGER only ever names the DECISIVE round, so filling it alone would leave the
# runoff winner named in the Stichwahl row and unnamed in the Hauptwahl row —
# and the wide pivot downstream pairs the two rounds by candidate name, falling
# back to the party label and then to the rank. Naming one side only therefore
# SPLITS an already-paired person into two published rows (the failure mode
# recorded for Brandenburg).
#
# The propagation reuses the pivot's OWN key — name, else party, else rank —
# computed from the PRE-fill state, and requires the key to be unique on both
# sides, which is the pivot's ambiguity rule. So a Hauptwahl row is named if and
# only if it is the row the Stichwahl winner would have been paired with anyway:
# no pair is created, and none is destroyed. Keying on the party alone is NOT
# enough — four cycles pair on rank because neither round records a
# Wahlvorschlag, and they split when the name lands on one side only.
sn_prekey <- function(name, party, rank) {
  case_when(
    !is.na(name)  ~ paste0("__name__",  tolower(str_trim(name))),
    !is.na(party) ~ paste0("__party__", tolower(str_trim(party))),
    TRUE          ~ paste0("__rank__",  rank)
  )
}

sn_keyed <- sachsen_candidates %>%
  mutate(.prekey = sn_prekey(ifelse(.filled, NA_character_, candidate_name),
                             candidate_party, candidate_rank)) %>%
  add_count(ags, election_date, .prekey, name = ".n_key")

sn_sw_winners <- sn_keyed %>%
  filter(.filled, round == "stichwahl", .n_key == 1) %>%
  select(ags, sw_date = election_date, .prekey, sg_name, sg_last, sg_first)

sn_hw_targets <- sn_keyed %>%
  filter(round == "hauptwahl", is.na(candidate_name), .n_key == 1) %>%
  select(ags, hw_date = election_date, .prekey) %>%
  inner_join(sn_sw_winners, by = c("ags", ".prekey"),
             relationship = "many-to-many") %>%
  filter(as.numeric(sw_date - hw_date) > 0, as.numeric(sw_date - hw_date) < 60) %>%
  distinct(ags, hw_date, .prekey, .keep_all = TRUE)

sachsen_candidates <- sn_keyed %>%
  left_join(sn_hw_targets %>%
              transmute(ags, election_date = hw_date, .prekey,
                        hw_name = sg_name, hw_last = sg_last, hw_first = sg_first),
            by = c("ags", "election_date", ".prekey")) %>%
  mutate(
    .prop = round == "hauptwahl" & is.na(candidate_name) & !is.na(hw_last),
    candidate_name       = ifelse(.prop, hw_name,  candidate_name),
    candidate_last_name  = ifelse(.prop, hw_last,  candidate_last_name),
    candidate_first_name = ifelse(.prop, hw_first, candidate_first_name)
  )
cat("  Sachsen: propagated", sum(sachsen_candidates$.prop),
    "runoff-winner name(s) back to the Hauptwahl row\n")

sachsen_candidates <- sachsen_candidates %>%
  select(-sg_name, -sg_last, -sg_first, -hw_name, -hw_last, -hw_first,
         -.filled, -.prop, -.prekey, -.n_key)

sachsen_clean <- standardise_candidates(sachsen_candidates)

cat("Sachsen: Processed", nrow(sachsen_clean), "candidate rows from",
    n_distinct(paste(sachsen_clean$ags, sachsen_clean$election_date)), "elections\n")
all_candidate_data[["sachsen"]] <- sachsen_clean

# ============================================================================
# RHEINLAND-PFALZ (Rhineland-Palatinate)
# ============================================================================

cat("\n=== Processing Rheinland-Pfalz mayoral elections ===\n")

# The 2025 edition is the base; the 2026 edition adds the elections it lacks and
# pinned cell corrections fix its known errors (rlp_direktwahlen.R, shared with 01).
source("code/mayoral_elections/rlp_direktwahlen.R")
rlp_file <- RLP_FILE_2025

# RLP has 4 sheets with different municipality types and code formats.
# Multi-row-per-election format: row 1 = election info + first/winner candidate,
# subsequent rows = other candidates (same election, empty Wahltag).
# Data has ONLY percentages (no absolute vote counts).

process_rlp_candidates <- function(rlp_file, sheet_name, sheet_type) {

  # Column mapping depends on sheet type
  if (sheet_type == "landraete") {
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_geschlecht <- NA
    col_ergebnis_hw <- 7; col_ergebnis_sw <- 8
    col_beteiligung_hw <- 9; col_beteiligung_sw <- 10
    has_vorname <- FALSE
  } else if (sheet_type == "ob") {
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_geschlecht <- 7
    col_ergebnis_hw <- 8; col_ergebnis_sw <- 9
    col_beteiligung_hw <- 10; col_beteiligung_sw <- 11
    has_vorname <- FALSE
  } else {
    # VG and Vfr.Gemeinden: separate Nachname (6) and Vorname (7)
    col_wahltag <- 1; col_stichwahltag <- 2; col_schluessel <- 3; col_name <- 4
    col_partei <- 5; col_kandidat <- 6; col_geschlecht <- 8
    col_ergebnis_hw <- 9; col_ergebnis_sw <- 10
    col_beteiligung_hw <- 11; col_beteiligung_sw <- 12
    has_vorname <- TRUE
    col_vorname <- 7
  }

  raw <- rlp_read_sheet(sheet_name, list(
    wahltag = col_wahltag, stichwahltag = col_stichwahltag, schluessel = col_schluessel,
    kandidat = col_kandidat, ergebnis_hw = col_ergebnis_hw, ergebnis_sw = col_ergebnis_sw))

  # Remove completely blank rows
  raw <- raw[!apply(raw, 1, function(x) all(is.na(x))), ]
  if (nrow(raw) == 0) return(NULL)

  # Assign group ID: each row with a Schlüssel starts a new election.
  # Group on the SCHLÜSSEL, not on the Wahltag: grouping on
  # cumsum(!is.na(Wahltag)) merged the eight Verbandsgemeinde elections whose
  # Wahltag read as NA into the group before them (Altenahr, Treis-Karden,
  # Rheinböllen, Monsheim, Wörrstadt, Eisenberg (Pfalz), Altenglan, Landau-Land).
  # Their Wahltag was never missing: the source types it as text ("27.03.2011",
  # with the Landtagswahl), which read_excel() turned into NA, and the fill-down
  # below dated them 07.11.2010 until October 2026. rlp_read_sheet() now parses
  # those cells; the fill-down stays as a guard and changes nothing.
  raw$group_id <- cumsum(!is.na(raw[[col_schluessel]]))
  fill_down <- function(x) {
    if (all(is.na(x))) return(x)
    i <- cumsum(!is.na(x)); i[i == 0] <- NA_integer_
    x[!is.na(x)][i]
  }
  raw[[col_wahltag]] <- fill_down(raw[[col_wahltag]])
  raw[[col_stichwahltag]] <- fill_down(raw[[col_stichwahltag]])

  results_list <- list()

  for (gid in unique(raw$group_id)) {
    group <- raw[raw$group_id == gid, ]
    first_row <- group[1, ]

    wahltag <- as.Date(first_row[[col_wahltag]])
    stichwahltag <- as.Date(first_row[[col_stichwahltag]])
    schluessel <- as.character(first_row[[col_schluessel]])
    ort_name <- as.character(first_row[[col_name]])

    if (is.na(schluessel)) next

    beteiligung_hw <- as.numeric(first_row[[col_beteiligung_hw]])
    beteiligung_sw <- as.numeric(first_row[[col_beteiligung_sw]])

    # Process ALL candidates in this group (all rows)
    for (row_idx in seq_len(nrow(group))) {
      row <- group[row_idx, ]

      partei <- as.character(row[[col_partei]])

      if (has_vorname) {
        nachname <- as.character(row[[col_kandidat]])
        vorname <- as.character(row[[col_vorname]])
        kandidat_name <- ifelse(!is.na(nachname),
                                paste0(nachname,
                                       ifelse(!is.na(vorname),
                                              paste0(", ", vorname), "")),
                                NA_character_)
      } else {
        kandidat_name <- as.character(row[[col_kandidat]])
        # The OB sheet has no Vorname column but writes "Nachname, Vorname" in
        # the one name cell. Copied whole into the surname, all 201 OB candidate
        # rows had no first name (so 136 had no gender either), and particle
        # surnames came out as "van X, Vorname". Split at the comma; titles
        # come off the parts only (candidate_name keeps the source string).
        if (!is.na(kandidat_name) && grepl(",", kandidat_name, fixed = TRUE)) {
          nachname <- strip_name_titles(sub(",.*$", "", kandidat_name))
          vorname <- strip_name_titles(sub("^[^,]*,", "", kandidat_name))
        } else {
          nachname <- kandidat_name
          vorname <- NA_character_
        }
      }

      geschlecht <- if (!is.na(col_geschlecht) && col_geschlecht <= ncol(row)) {
        as.character(row[[col_geschlecht]])
      } else {
        NA_character_
      }

      ergebnis_hw <- as.numeric(row[[col_ergebnis_hw]])
      ergebnis_sw <- as.numeric(row[[col_ergebnis_sw]])

      # Skip rows with no candidate info at all
      if (is.na(partei) && is.na(kandidat_name) && is.na(ergebnis_hw)) next

      # Hauptwahl row for this candidate
      if (!is.na(ergebnis_hw)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          wahltag = wahltag,
          schluessel = schluessel,
          ort_name = ort_name,
          candidate_name = kandidat_name,
          candidate_last_name = nachname,
          candidate_first_name = vorname,
          candidate_gender = geschlecht,
          candidate_party = partei,
          candidate_voteshare = ergebnis_hw / 100,
          turnout = beteiligung_hw / 100,
          round = "hauptwahl",
          stringsAsFactors = FALSE
        )
      }

      # Stichwahl row for this candidate (if Stichwahl data exists)
      if (!is.na(ergebnis_sw) && !is.na(stichwahltag)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          wahltag = stichwahltag,
          schluessel = schluessel,
          ort_name = ort_name,
          candidate_name = kandidat_name,
          candidate_last_name = nachname,
          candidate_first_name = vorname,
          candidate_gender = geschlecht,
          candidate_party = partei,
          candidate_voteshare = ergebnis_sw / 100,
          turnout = beteiligung_sw / 100,
          round = "stichwahl",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(results_list) == 0) return(NULL)

  result <- bind_rows(results_list) %>%
    mutate(
      election_date = wahltag,
      election_year = lubridate::year(election_date)
    )

  return(result)
}

# Process each sheet
rlp_landraete <- process_rlp_candidates(rlp_file, "Landräte", "landraete")
rlp_ob <- process_rlp_candidates(rlp_file, "Oberbürgermeister Krfr.Städte", "ob")
rlp_vg <- process_rlp_candidates(rlp_file, "Bürgermeister Verbandsgemeinden", "vg")
rlp_vfr <- process_rlp_candidates(rlp_file, "Bürgermeister Vfr.Gemeinden", "vfr")

cat("  Landräte:", if(!is.null(rlp_landraete)) nrow(rlp_landraete) else 0, "candidate rows\n")
cat("  OB Krfr.Städte:", if(!is.null(rlp_ob)) nrow(rlp_ob) else 0, "candidate rows\n")
cat("  VG:", if(!is.null(rlp_vg)) nrow(rlp_vg) else 0, "candidate rows\n")
cat("  Vfr.Gemeinden:", if(!is.null(rlp_vfr)) nrow(rlp_vfr) else 0, "candidate rows\n")

# Map Schlüsselnummer to AGS for each sheet type
if (!is.null(rlp_landraete)) {
  rlp_landraete <- rlp_landraete %>%
    mutate(
      ags = paste0("07", str_pad(schluessel, width = 3, side = "left", pad = "0"), "000"),
      election_type = "Landratswahl"
    )
}

if (!is.null(rlp_ob)) {
  rlp_ob <- rlp_ob %>%
    mutate(
      ags = paste0("07", substr(schluessel, 1, 3), "000"),
      election_type = "Oberbürgermeisterwahl"
    )
}

if (!is.null(rlp_vg)) {
  rlp_vg <- rlp_vg %>%
    mutate(
      # STRIP whitespace BEFORE padding — as the Vfr branch below and the twin
      # block in 01_mayoral_unharm.R both already do. 152 of the 627 VG groups
      # write the Schlüssel as "DDD DD" ("131 01"), which is already 6 characters,
      # so str_pad() was a no-op and the AGS came out as "07131 01": 364 candidate
      # rows carried a literal space and could never join their own election in
      # mayoral_unharm.
      schluessel_clean = gsub("\\s", "", schluessel),
      ags = paste0("07", str_pad(schluessel_clean, width = 6, side = "left", pad = "0")),
      election_type = "VG-Bürgermeisterwahl"
    ) %>%
    select(-schluessel_clean)
}

if (!is.null(rlp_vfr)) {
  rlp_vfr <- rlp_vfr %>%
    mutate(
      schluessel_clean = gsub("\\s", "", schluessel),
      ags = paste0("07",
                   substr(schluessel_clean, 1, 3),
                   substr(schluessel_clean, nchar(schluessel_clean) - 2,
                          nchar(schluessel_clean))),
      election_type = "Bürgermeisterwahl"
    ) %>%
    select(-schluessel_clean)
}

# Combine all RLP sheets
rlp_combined <- bind_rows(rlp_landraete, rlp_ob, rlp_vg, rlp_vfr) %>%
  mutate(
    state = "07",
    state_name = "Rheinland-Pfalz",
    ags_name = ort_name,
    # RLP only has percentages, no absolute counts
    eligible_voters = NA_real_,
    number_voters = NA_real_,
    valid_votes = NA_real_,
    invalid_votes = NA_real_,
    candidate_votes = NA_real_
  ) %>%
  # Compute rank by voteshare within each election-round
  group_by(ags, election_date, round) %>%
  mutate(
    candidate_rank = rank(-candidate_voteshare, ties.method = "min", na.last = "keep"),
    n_candidates = n(),
    is_winner = candidate_rank == 1
  ) %>%
  ungroup() %>%
  select(ags, ags_name, state, state_name, election_year, election_date,
         election_type, round, eligible_voters, number_voters, valid_votes,
         invalid_votes, turnout, candidate_name, candidate_last_name,
         candidate_first_name, candidate_gender, candidate_party,
         candidate_votes, candidate_voteshare, candidate_rank, n_candidates,
         is_winner)

# Elections only the 2026 edition has: the elected person only, with the share
# with which they were elected. In a runoff cycle the Hauptwahl row has no share
# (the edition gives none) and is_winner NA; the runoff row seats them.
# n_candidates is unknown.
rlp_known <- rlp_combined %>%
  distinct(ags, election_type) %>%
  mutate(office = c("Landratswahl" = "landrat", "Oberbürgermeisterwahl" = "ob",
                    "Bürgermeisterwahl" = "vfr", "VG-Bürgermeisterwahl" = "vg")[election_type])
rlp_add <- rlp_additions_2026(rlp_known) %>%
  mutate(candidate_last_name = strip_name_titles(sub(",.*$", "", candidate_name)),
         candidate_first_name = ifelse(grepl(",", candidate_name, fixed = TRUE),
                                       strip_name_titles(sub("^[^,]*,", "", candidate_name)), NA_character_))
rlp_add_cand <- bind_rows(
  rlp_add %>% transmute(
    ags, ags_name, state = "07", state_name = "Rheinland-Pfalz",
    election_year = lubridate::year(election_date), election_date, election_type,
    round = "hauptwahl", turnout = turnout_hw / 100,
    candidate_share = if_else(runoff, NA_real_, pct / 100), runoff,
    candidate_name, candidate_last_name, candidate_first_name, candidate_gender, candidate_party),
  rlp_add %>% filter(runoff) %>% transmute(
    ags, ags_name, state = "07", state_name = "Rheinland-Pfalz",
    election_year = lubridate::year(stichwahl_date), election_date = stichwahl_date, election_type,
    round = "stichwahl", turnout = turnout_sw / 100, candidate_share = pct / 100, runoff = FALSE,
    candidate_name, candidate_last_name, candidate_first_name, candidate_gender, candidate_party)) %>%
  transmute(ags, ags_name, state, state_name, election_year, election_date, election_type, round,
            eligible_voters = NA_real_, number_voters = NA_real_, valid_votes = NA_real_,
            invalid_votes = NA_real_, turnout, candidate_name, candidate_last_name,
            candidate_first_name, candidate_gender, candidate_party, candidate_votes = NA_real_,
            candidate_voteshare = candidate_share,
            candidate_rank = if_else(is.na(candidate_share), NA_integer_, 1L),
            n_candidates = NA_integer_,
            is_winner = if_else(is.na(candidate_share), NA, TRUE))
rlp_combined <- bind_rows(rlp_combined, rlp_add_cand)

rlp_clean <- standardise_candidates(rlp_combined)

cat("RLP: Processed", nrow(rlp_clean), "candidate rows from",
    n_distinct(paste(rlp_clean$ags, rlp_clean$election_date, rlp_clean$round)),
    "election-rounds\n")
all_candidate_data[["rlp"]] <- rlp_clean

# ============================================================================
# NIEDERSACHSEN (Lower Saxony) - All years (2006-2025)
# ============================================================================

cat("\n=== Processing Niedersachsen mayoral elections (2006-2025) ===\n")

ns_base <- "data/mayoral_elections/raw/niedersachsen/Direktwahlen in Niedersachsen 2006-2025"

if (requireNamespace("pdftools", quietly = TRUE)) {

  # Party name mapping for 2006 (full names → abbreviations)
  ns_party_map_2006 <- c(
    "Christlich Demokratische Union Deutschlands" = "CDU",
    "Sozialdemokratische Partei Deutschlands" = "SPD",
    "BÜNDNIS 90/DIE GRÜNEN" = "GRÜNE",
    "B\u00dcNDNIS 90/DIE GR\u00dcNEN" = "GRÜNE",
    "Die Linkspartei." = "LINKE",
    "Freie Demokratische Partei" = "FDP",
    "Einzelwahlvorschlag" = "EB"
  )

  # A page is a Samtgemeinde election only if its HEADING (the text above
  # "Schlüssel:") names the Samtgemeindebürgermeister, or its key is a
  # Samtgemeinde key (Kreis + 4nn: SG Wathlingen 2014 is headed plain
  # "Bürgermeister/Bürgermeisterin"). Testing the whole page also matched a
  # candidate's Beruf (Stuhr 2019: Holle, Frank, "Samtgemeindebürgermeister")
  # and typed four town elections as SG-Bürgermeisterwahl, which the panel
  # drops: Bad Fallingbostel 2006, Stuhr and Cuxhaven 2019, Rotenburg (Wümme)
  # 2021.
  ns_is_sg_page <- function(page, schluessel) {
    head <- sub("(?s)Schlüssel:.*$", "", page, perl = TRUE)
    grepl("Samtgemeindebürgermeister", head) |
      (grepl("Samtgemeindebürgermeister", page) & grepl("^\\d{3}4\\d{2}$", schluessel))
  }

  # --------------------------------------------------------------------------
  # Standard page parser — returns one row per CANDIDATE
  # --------------------------------------------------------------------------
  parse_ns_candidates_standard <- function(page, election_year, election_date,
                                           german_nums = FALSE,
                                           round = "hauptwahl") {

    schluessel <- str_extract(page, "Schlüssel:\\s*(\\d+)", group = 1)
    if (is.na(schluessel)) return(NULL)

    ort_name <- str_extract(page,
      "in (?:der |dem )?(?:Stadt |Landkreis |SG |Samtgemeinde |Gemeinde |Flecken )?(.+?)\\n\\s*Schlüssel",
      group = 1)
    if (is.na(ort_name)) {
      ort_name <- str_extract(page, "in\\s+(.+?)\\n", group = 1)
    }
    ort_name <- str_trim(ort_name)

    # The Regionspräsident branch MUST come first — see the fuller note on the
    # twin case_when in 01_mayoral_unharm.R. Region Hannover (03241000) elects a
    # Kreis-level Regionspräsident/-in, not a mayor.
    election_type_raw <- case_when(
      grepl("Regionspräsident", page) ~ "Landratswahl",
      grepl("Landrat/Landrätin|Landrätin/Landrat|Landrätin / des Landrates", page) ~ "Landratswahl",
      grepl("Oberbürgermeister", page) ~ "Oberbürgermeisterwahl",
      ns_is_sg_page(page, schluessel) ~ "SG-Bürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    )

    is_stichwahl_required <- grepl("Stichwahl erforderlich", page)

    page_lines <- str_split(page, "\n")[[1]]

    # Extract election-level stats
    extract_last_num <- function(lines, pattern) {
      for (l in lines) {
        if (grepl(pattern, l)) {
          l_clean <- if (german_nums) gsub("\\.", "", l) else l
          nums <- str_extract_all(l_clean, "\\d+")[[1]]
          if (length(nums) > 0) return(as.numeric(tail(nums, 1)))
        }
      }
      return(NA_real_)
    }

    wahlberechtigte <- extract_last_num(page_lines, "Wahlberechtigte")
    waehler <- extract_last_num(page_lines, "Wählerinnen")
    ungueltige <- extract_last_num(page_lines, "Ungültige")
    gueltige <- extract_last_num(page_lines, "Gültige Stimmen")

    # AGS construction
    ags <- case_when(
      nchar(schluessel) <= 3 ~ paste0("03", str_pad(schluessel, 3, "left", "0"), "000"),
      nchar(schluessel) <= 6 ~ paste0("03", str_pad(schluessel, 6, "left", "0")),
      TRUE ~ paste0("03", schluessel)
    )
    ags <- str_pad(ags, width = 8, side = "right", pad = "0")

    # Extract individual candidate blocks
    # Both 2006 and post-2006 share a 3-line structure per candidate:
    # Line 1: "     N            Name, First                 Profession"
    # Line 2: "                                                            VOTES    PCT"
    # Line 3: "   BIRTHYEAR                   Party"
    # In 2006: votes use dots as thousands sep, pct uses comma; party = full name
    # Post-2006: votes are plain numbers, pct uses dot; party = abbreviation
    candidates <- list()

    # Find candidate block start lines: "^\s+\d+\s+\S" (number followed by name)
    # Exclude lines that start with year-like 4-digit numbers (birth year lines)
    # and lines containing "Wahlberechtigte", "Wähler", etc.
    # The text after the Lfd.-Nr. must NOT start with a digit: a candidate who
    # polled fewer than 100 votes has a votes line ("      75       4.4") that
    # otherwise reads as Lfd.-Nr. 75 followed by a name. That line became a
    # candidate called "4.4" (2006: "13,8") who took the real candidate's birth
    # year and party, while the real candidate above it lost votes, party and
    # birth year — 26 phantom rows in 20 NI elections until October 2026.
    cand_start_indices <- c()
    for (idx in seq_along(page_lines)) {
      l <- page_lines[idx]
      # Match: leading spaces, then a 1-2 digit number, then spaces, then text (name)
      if (grepl("^\\s{2,}\\d{1,2}\\s{2,}[^\\s0-9]", l, perl = TRUE) &&
          !grepl("Wahlberechtigte|Wähler|Ungültige|Gültige|Wahlbeteiligung|Zusammen|Bewerberin|Lfd|Geburtsjahr|Partei", l)) {
        cand_start_indices <- c(cand_start_indices, idx)
      }
    }

    # The 3-line block above is the TYPICAL layout, not a guaranteed one: the
    # "Beruf" column wraps on many pages, which pushes the votes line to ci+2 and
    # the birth-year/party line to ci+3. The old code assumed fixed ci+1 / ci+2
    # offsets and let the lazy name group match a single SPACE when the Lfd.-Nr.
    # line held only the number and the name — the result was an EMPTY
    # candidate_name, the real name in candidate_profession and NA votes/party
    # (210 such junk rows in NI mayoral + 20 in NI landrat). Because the winner is
    # picked by votes, that crowned the wrong person in 31 NI elections, among
    # them four Landratswahlen: Schaumburg 2006 (true winner Schöttelndreier SPD
    # 65.8%), Leer 2016 (Groote SPD 56.8%), Aurich 2019 (Meinen EB 53.5%) and
    # Heidekreis 2021 (Grote EB 52.8%).
    # Fix: require a non-space first character in every captured group, and SCAN
    # FORWARD inside the candidate's own block rather than trusting the offsets.
    for (ci in cand_start_indices) {
      name_line <- page_lines[ci]
      # Format: "     1            Bockhop, Cord                          Landrat"
      nm <- str_match(name_line, "^\\s+\\d+\\s{2,}([^\\s].*?)\\s{3,}([^\\s].*?)\\s*$")
      if (!is.na(nm[1])) {
        cand_name <- str_trim(nm[2])
        profession <- str_trim(nm[3])
      } else {
        # Beruf wrapped onto the next line -> the Lfd.-Nr. line carries only the name
        nm1 <- str_match(name_line, "^\\s+\\d+\\s{2,}([^\\s].*?)\\s*$")
        if (is.na(nm1[1])) next
        cand_name <- str_trim(nm1[2])
        profession <- NA_character_
        # 2006 long-name layout: a name too wide for its column is rendered ABOVE
        # the Lfd.-Nr. line (and continues below it), leaving only number + Beruf
        # on the line itself — so the single token here is the PROFESSION:
        #     "                Schöttelndreier, Heinz-"
        #     "     1                                        Landrat"
        #     "                       Gerhard"
        # The COLUMN decides which of the two it is: a wrapped BERUF also lands
        # above the Lfd.-Nr. line (Aurich 2019 "Bürgermeister, Verwaltungs-"),
        # but in the Beruf column, i.e. to the RIGHT of the token we just read.
        if (ci > 1) {
          prev_raw <- page_lines[ci - 1]
          prev <- str_trim(prev_raw)
          prev_col <- regexpr("[^ ]", prev_raw)[1]
          tok_col <- regexpr(cand_name, name_line, fixed = TRUE)[1]
          if (nzchar(prev) && grepl(",", prev, fixed = TRUE) &&
              !grepl("[0-9]", prev) &&
              prev_col > 0 && tok_col > 0 && prev_col < tok_col &&
              !grepl("Lfd|Bewerber|Geburtsjahr|Partei|Stimmen|Wahl|gewählt|Herr|Frau|Schlüssel|Landkreis|Bezirk",
                     prev)) {
            profession <- cand_name
            cand_name <- prev
          }
        }
      }

      # Hyphen-wrapped name ("Schöttelndreier, Heinz-" / "Gerhard", Schaumburg 2006)
      if (grepl("-$", cand_name) && ci + 1 <= length(page_lines)) {
        cont <- str_trim(page_lines[ci + 1])
        if (grepl("^[A-Za-zÄÖÜäöüß.'-]+$", cont)) cand_name <- paste0(cand_name, cont)
      }
      # Comma-wrapped name: the 2006 long-name layout breaks after the comma and
      # puts the given name on the line BELOW the Lfd.-Nr. line, in the name
      # column ("Doppel-Nachname," / "Vorname"). Reading only the line above
      # published four candidates with no first name. The given name must sit
      # left of the Beruf column, so a wrapped Beruf is never taken for it.
      if (grepl(",$", cand_name) && ci + 1 <= length(page_lines)) {
        cont_raw <- page_lines[ci + 1]
        cont <- str_trim(cont_raw)
        cont_col <- regexpr("[^ ]", cont_raw)[1]
        prof_col <- if (!is.na(profession)) regexpr(profession, name_line, fixed = TRUE)[1] else -1L
        if (grepl("^[A-Za-zÄÖÜäöüßé.' -]+$", cont) && cont_col > 0 &&
            (prof_col < 0 || cont_col < prof_col)) {
          cand_name <- paste(cand_name, cont)
        }
      }
      # A name cell can also be CLIPPED in the PDF itself: Ilsede 2021 prints
      # only "Surname," and pdf_data() holds no given name on the line. The page
      # header names the elected person and the runoff pair in full, as
      # "<Vorname> <Name>, <Wahlvorschlag>" (post-2006 layout only — 2006 prints
      # "Herr/Frau <Name>, <Vorname>"), so take the given name from there when
      # exactly one header entry ends in this surname.
      if (grepl(",$", cand_name) && !german_nums) {
        sur <- str_trim(sub(",$", "", cand_name))
        hdr_end <- which(grepl("Wahlberechtigte", page_lines))[1]
        hdr <- if (!is.na(hdr_end) && hdr_end > 1) str_squish(paste(page_lines[seq_len(hdr_end - 1)], collapse = " ")) else ""
        hm <- str_match_all(hdr, paste0("(?:gelten:|zwischen|\\bund)\\s+([\\p{L}.' -]+?)\\s+\\Q",
                                        sur, "\\E\\s*,"))[[1]]
        given <- unique(str_trim(hm[, 2]))
        given <- given[!grepl("^(Herr|Frau)$", given)]
        if (length(given) == 1) cand_name <- paste(cand_name, given)
      }

      # Parse name into last, first
      name_parts <- str_split(cand_name, ",\\s*")[[1]]
      last_name <- str_trim(name_parts[1])
      first_name <- if (length(name_parts) > 1) str_trim(paste(name_parts[-1], collapse = ", ")) else NA_character_

      # This candidate's block: up to the next Lfd.-Nr. line (hard cap 8 lines)
      nxt <- cand_start_indices[cand_start_indices > ci]
      block_end <- min(if (length(nxt) > 0) min(nxt) - 1L else length(page_lines),
                       ci + 8L, length(page_lines))
      # Only the trailing "Zusammen:" total can be confused with a result line.
      # Do NOT widen this to "Wähler"/"Gültige": the A-D turnout block sits ABOVE
      # the candidate list, while hundreds of party names read "WGR
      # Wählergemeinschaft ..." and would be skipped.
      skip_pat <- "Zusammen"

      # Votes: first "<votes> <pct>" line of the block. The percentage may be an
      # INTEGER (DW2016 has 14 such lines, DW2019 16) — the old pattern demanded
      # a decimal point and silently NA'd those candidates' votes. German
      # thousands dots are stripped, which is a no-op for the post-2006 files.
      votes <- NA_real_
      vote_idx <- NA_integer_
      if (ci < block_end) {
        for (j in (ci + 1L):block_end) {
          l2 <- page_lines[j]
          if (grepl(skip_pat, l2)) next
          vm <- str_match(l2, "(\\d[\\d.]*)\\s+\\d+(?:[,.]\\d+)?\\s*$")
          if (!is.na(vm[1])) {
            votes <- as.numeric(gsub("\\.", "", vm[2]))
            vote_idx <- j
            break
          }
        }
      }

      # Birth year + party: first "<yyyy> <party>" line AFTER the votes line.
      # Starting after the votes line, rejecting a numeric "party" and range-
      # checking the year all guard against re-reading a 4-digit vote count as a
      # birth year and its percentage as the party (Leer 2016 "Lenger, Tammo"
      # came out with candidate_party = 11.5).
      birth_year <- NA_real_
      party <- NA_character_
      start_p <- if (!is.na(vote_idx)) vote_idx + 1L else ci + 1L
      if (start_p <= block_end) {
        for (j in start_p:block_end) {
          l3 <- page_lines[j]
          if (grepl(skip_pat, l3)) next
          # Format: "   1967                                     EB"
          pm <- str_match(l3, "^\\s*(\\d{4})\\s+([^\\s].*?)\\s*$")
          if (is.na(pm[1])) next
          by_val <- suppressWarnings(as.numeric(pm[2]))
          party_raw <- str_trim(pm[3])
          if (grepl("^[0-9]+([.,][0-9]+)?$", party_raw)) next   # that is a votes line
          if (is.na(by_val) || by_val < 1900 || by_val > 2015) next
          birth_year <- by_val
          # Map full party name to abbreviation for 2006
          party <- if (german_nums && party_raw %in% names(ns_party_map_2006)) {
            unname(ns_party_map_2006[party_raw])
          } else {
            party_raw
          }
          break
        }
      }

      candidates[[length(candidates) + 1]] <- data.frame(
        candidate_name = cand_name,
        candidate_last_name = last_name,
        candidate_first_name = first_name,
        candidate_party = party,
        candidate_votes = votes,
        candidate_profession = profession,
        candidate_birth_year = birth_year,
        stringsAsFactors = FALSE
      )
    }

    if (length(candidates) == 0) return(NULL)

    cand_df <- bind_rows(candidates) %>%
      mutate(
        ags = ags,
        ags_name = ort_name,
        state = "03",
        state_name = "Niedersachsen",
        election_year = election_year,
        election_date = as.Date(election_date),
        election_type = election_type_raw,
        # The "standard" parser also reads runoff files (2021), so the round is
        # supplied by the caller from the file configuration. (is_stichwahl_required
        # says a runoff WILL be held, not that THIS page reports one.)
        round = round,
        # Used to attach the 2014/2016/2019 runoff tables to the right
        # Hauptwahl; dropped again before the file is written.
        needs_stichwahl = is_stichwahl_required,
        eligible_voters = wahlberechtigte,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = ungueltige,
        turnout = ifelse(!is.na(wahlberechtigte) & wahlberechtigte > 0 &
                           !is.na(waehler),
                         waehler / wahlberechtigte, NA),
        candidate_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 &
                                       !is.na(candidate_votes),
                                     candidate_votes / gueltige, NA)
      )

    return(cand_df)
  }

  # --------------------------------------------------------------------------
  # Stichwahl summary tables (2014, 2016, 2019) — returns candidate rows
  # --------------------------------------------------------------------------
  # See the matching block in 01_mayoral_unharm.R for why these years need a
  # separate parser and how the AGS is recovered. Keep the two in sync.
  #
  # These tables name ONLY the winner, so exactly one candidate row per runoff
  # is emitted. The runner-up is deliberately NOT reconstructed as
  # (valid_votes - winner_votes): that arithmetic is right only if the runoff
  # ran with the Hauptwahl's top two, and a withdrawal promotes the third — an
  # inference the source does not support and that would publish a named
  # candidate who may never have stood.
  ns_de_num <- function(x) as.numeric(gsub("\\.", "", x))
  ns_de_pct <- function(x) as.numeric(gsub(",", ".", sub("%", "", x))) / 100

  parse_ns_sw_rich <- function(pdf_path) {
    lines <- unlist(str_split(pdftools::pdf_text(pdf_path), "\n"))
    pat <- paste0("^\\s*(\\d{1,3})\\s+(.+?)\\s{2,}(.+?)\\s{2,}(\\S+)\\s+",
                  "([\\d.]+)\\s+(\\d+,\\d+%)\\s+([\\d.]+)\\s+([\\d.]+)\\s+([\\d.]+)\\s+(\\d+,\\d+%)\\s*$")
    hit <- str_match(lines, pat)
    hit <- hit[!is.na(hit[, 1]), , drop = FALSE]
    if (nrow(hit) == 0) return(NULL)
    mid <- lapply(str_trim(hit[, 4]), function(z) str_split(z, "\\s{2,}")[[1]])
    win <- vapply(mid, function(z) {
      g <- grep(",", z, fixed = TRUE)
      if (length(g)) z[max(g)] else NA_character_
    }, character(1))
    data.frame(sw_name = str_trim(hit[, 3]), winner_name = win,
               winner_party = hit[, 5], winner_votes = ns_de_num(hit[, 6]),
               eligible_voters = ns_de_num(hit[, 8]),
               number_voters = ns_de_num(hit[, 9]),
               valid_votes = ns_de_num(hit[, 10]),
               turnout = ns_de_pct(hit[, 11]), stringsAsFactors = FALSE)
  }

  parse_ns_sw_names <- function(pdf_path) {
    lines <- unlist(str_split(pdftools::pdf_text(pdf_path), "\n"))
    pat <- paste0("^(Braunschweig|Hannover|Lüneburg|Weser-Ems)\\s{2,}(.+?)\\s{2,}(.*?)\\s*",
                  "(Stadt|Gemeinde|Samtgemeinde|Flecken|Landkreis|Hansestadt|Region)\\s{2,}",
                  "([^,]+,\\s*\\S.*?)\\s{2,}(\\S+)\\s*$")
    hit <- str_match(lines, pat)
    hit <- hit[!is.na(hit[, 1]), , drop = FALSE]
    if (nrow(hit) == 0) return(NULL)
    lk <- str_trim(hit[, 3]); kom <- str_trim(hit[, 4])
    kom <- ifelse(hit[, 5] == "Landkreis", lk,
                  ifelse(nzchar(kom), kom, sub("^.*\\s", "", lk)))
    data.frame(sw_name = kom, winner_name = str_trim(hit[, 6]),
               winner_party = hit[, 7], winner_votes = NA_real_,
               eligible_voters = NA_real_, number_voters = NA_real_,
               valid_votes = NA_real_, turnout = NA_real_,
               stringsAsFactors = FALSE)
  }

  ns_sw_aliases <- c(
    "buchholzidnordheide" = "buchholzindernordheide",
    "neustadtarbge"       = "neustadtamruebenberge"
  )
  ns_norm_name <- function(x) {
    x <- tolower(x)
    x <- sub(",.*$", "", x)
    x <- gsub("\\s*\\(.*?\\)", "", x)
    x <- gsub("ä", "ae", x); x <- gsub("ö", "oe", x); x <- gsub("ü", "ue", x)
    x <- gsub("[^a-z]", "", x)
    ifelse(x %in% names(ns_sw_aliases), unname(ns_sw_aliases[x]), x)
  }

  # --------------------------------------------------------------------------
  # 2017 Übersicht table — candidate rows
  # --------------------------------------------------------------------------
  # See the matching block in 01_mayoral_unharm.R. The summary names only the
  # Wahlsieger, so one candidate row per decided election. Aue is omitted
  # entirely: it went to a runoff whose result was never published, so it has no
  # determinable winner and its two named runoff entrants are not the full field.
  # Keep the AGS map in sync with 01.
  ns_2017_ags <- c("Aue" = "03360408", "Berne" = "03461001",
                   "Helmstedt" = "03154010", "Langwedel" = "03361006",
                   "Uplengen" = "03457020")

  parse_ns_2017_candidates <- function(pdf_path) {
    lines <- unlist(str_split(pdftools::pdf_text(pdf_path), "\n"))
    pat <- paste0("^\\s*(\\d{1,2})\\s+(.+?)\\s{2,}(.+?)\\s+",
                  "([\\d.]+)\\s+([\\d.]+)\\s+([\\d.]+)\\s+(\\d+,\\d+%)\\s*$")
    hit <- str_match(lines, pat)
    hit <- hit[!is.na(hit[, 1]), , drop = FALSE]
    if (nrow(hit) == 0) return(NULL)
    out <- list()
    for (i in seq_len(nrow(hit))) {
      kom <- str_trim(hit[i, 3])
      mid <- str_split(str_trim(hit[i, 4]), "\\s{2,}")[[1]]
      # Comma preceded by letters: the votes/share field carries a decimal comma.
      wi <- grep("^[^0-9]+,", mid)
      if (!length(wi)) next                      # undecided (Aue)
      nm <- mid[max(wi)]
      tail_bits <- mid[(max(wi) + 1):length(mid)]
      vm <- str_match(paste(tail_bits[-1], collapse = " "), "([\\d.]+)\\s+(\\d+,\\d+)%")
      votes <- if (is.na(vm[1, 1])) NA_real_ else as.numeric(gsub("\\.", "", vm[1, 2]))
      vv <- as.numeric(gsub("\\.", "", hit[i, 7]))
      out[[length(out) + 1]] <- data.frame(
        candidate_name = nm,
        candidate_last_name = str_trim(sub(",.*$", "", nm)),
        candidate_first_name = str_trim(sub("^[^,]*,\\s*", "", nm)),
        candidate_party = tail_bits[1],
        candidate_votes = votes,
        candidate_profession = NA_character_, candidate_birth_year = NA_real_,
        ags = unname(ns_2017_ags[kom]), ags_name = kom,
        state = "03", state_name = "Niedersachsen",
        election_year = 2017L, election_date = as.Date("2017-09-24"),
        election_type = if (mid[1] == "SG") "SG-Bürgermeisterwahl" else "Bürgermeisterwahl",
        round = "hauptwahl", needs_stichwahl = FALSE,
        eligible_voters = as.numeric(gsub("\\.", "", hit[i, 5])),
        number_voters   = as.numeric(gsub("\\.", "", hit[i, 6])),
        valid_votes = vv,
        invalid_votes = as.numeric(gsub("\\.", "", hit[i, 6])) - vv,
        turnout = as.numeric(gsub(",", ".", sub("%", "", hit[i, 8]))) / 100,
        candidate_voteshare = if (!is.na(votes) && vv > 0) votes / vv else NA_real_,
        sw_winner_only = TRUE,      # winner named, rest of the field not published
        stringsAsFactors = FALSE
      )
    }
    if (!length(out)) return(NULL)
    bind_rows(out)
  }

  attach_ns_sw_candidates <- function(sw, hw_all, year, sw_date) {
    # The Hauptwahl rows are candidate-level here, so reduce to one row per
    # election before matching; needs_stichwahl is constant within an election.
    hw <- hw_all[hw_all$election_year == year & hw_all$needs_stichwahl, , drop = FALSE]
    hw <- hw[!duplicated(hw$ags), c("ags", "ags_name", "state", "state_name",
                                    "election_year", "election_type"), drop = FALSE]
    hk <- ns_norm_name(hw$ags_name)
    sk <- ns_norm_name(sw$sw_name)
    idx <- match(sk, hk)
    if (anyNA(idx) || anyDuplicated(hk) || anyDuplicated(sk) || nrow(sw) != nrow(hw)) {
      stop("NS ", year, " Stichwahl candidates: ", nrow(sw), " runoff rows vs ",
           nrow(hw), " Hauptwahl elections marked 'Stichwahl erforderlich'; ",
           "unmatched: ", paste(sw$sw_name[is.na(idx)], collapse = " | "))
    }
    out <- hw[idx, , drop = FALSE]
    out$election_date <- as.Date(sw_date)
    out$round <- "stichwahl"
    out$candidate_name <- sw$winner_name
    out$candidate_last_name <- str_trim(sub(",.*$", "", sw$winner_name))
    out$candidate_first_name <- str_trim(sub("^[^,]*,\\s*", "", sw$winner_name))
    out$candidate_party <- sw$winner_party
    out$candidate_votes <- sw$winner_votes
    out$candidate_profession <- NA_character_
    out$candidate_birth_year <- NA_real_
    out$eligible_voters <- sw$eligible_voters
    out$number_voters <- sw$number_voters
    out$valid_votes <- sw$valid_votes
    out$invalid_votes <- ifelse(!is.na(sw$number_voters) & !is.na(sw$valid_votes),
                                sw$number_voters - sw$valid_votes, NA_real_)
    out$turnout <- sw$turnout
    out$candidate_voteshare <- ifelse(!is.na(sw$valid_votes) & sw$valid_votes > 0,
                                      sw$winner_votes / sw$valid_votes, NA_real_)
    # Marks the rows where the source names the winner and nobody else, so the
    # combine step below can avoid two false statements: ranking by votes would
    # leave is_winner NA for 2019 (no counts published) even though the column
    # is headed "Wahlsieger", and n() would report a two-candidate runoff as
    # having had one candidate.
    out$sw_winner_only <- TRUE
    rownames(out) <- NULL
    out
  }

  # --------------------------------------------------------------------------
  # 2006 Stichwahl parser — returns candidate rows
  # --------------------------------------------------------------------------
  parse_ns_candidates_2006_stichwahl <- function(page, election_year, election_date) {

    schluessel <- str_extract(page, "Schlüssel:\\s*(\\d+)", group = 1)
    if (is.na(schluessel)) return(NULL)

    ort_name <- str_extract(page,
      "in (?:der |dem )?(?:Stadt |Landkreis |SG |Samtgemeinde |Gemeinde |Flecken )?(.+?)\\n\\s*Schlüssel",
      group = 1)
    if (is.na(ort_name)) {
      ort_name <- str_extract(page, "in\\s+(.+?)\\n", group = 1)
    }
    ort_name <- str_trim(ort_name)

    election_type_raw <- case_when(
      grepl("Landrätin / des Landrates|Landrat", page) ~ "Landratswahl",
      grepl("Oberbürgermeister", page) ~ "Oberbürgermeisterwahl",
      ns_is_sg_page(page, schluessel) ~ "SG-Bürgermeisterwahl",
      TRUE ~ "Bürgermeisterwahl"
    )

    page_lines <- str_split(page, "\n")[[1]]

    # Extract Stichwahl election stats (rightmost values in two-column layout)
    extract_rightmost_num <- function(lines, pattern) {
      for (l in lines) {
        if (grepl(pattern, l)) {
          l_clean <- gsub("\\.", "", l)
          nums <- str_extract_all(l_clean, "\\d+")[[1]]
          if (length(nums) >= 2) return(as.numeric(tail(nums, 1)))
          if (length(nums) == 1) return(as.numeric(nums[1]))
        }
      }
      return(NA_real_)
    }

    wahlberechtigte <- extract_rightmost_num(page_lines, "Wahlberechtigte")
    waehler <- extract_rightmost_num(page_lines, "Wählerinnen")
    ungueltige <- extract_rightmost_num(page_lines, "Ungültige")
    gueltige <- extract_rightmost_num(page_lines, "Gültige Stimmen")

    ags <- case_when(
      nchar(schluessel) <= 3 ~ paste0("03", str_pad(schluessel, 3, "left", "0"), "000"),
      nchar(schluessel) <= 6 ~ paste0("03", str_pad(schluessel, 6, "left", "0")),
      TRUE ~ paste0("03", schluessel)
    )
    ags <- str_pad(ags, width = 8, side = "right", pad = "0")

    # Extract candidates from Stichwahl two-column layout
    # Line 1: "   1         Name, First               Profession          VOTES   PCT%"
    # Line 2: " BIRTHYEAR        Party                                    FIRSTROUND_VOTES  FIRSTROUND_%"
    # We want the Stichwahl numbers (first pair on the candidate line)
    candidates <- list()

    cand_start_indices <- c()
    for (idx in seq_along(page_lines)) {
      l <- page_lines[idx]
      if (grepl("^\\s{2,}\\d{1,2}\\s{2,}\\S", l) &&
          !grepl("Wahlberechtigte|Wähler|Ungültige|Gültige|Wahlbeteiligung|Zusammen|Bewerberin|Lfd|Geburtsjahr|Partei|Ergebnis", l)) {
        cand_start_indices <- c(cand_start_indices, idx)
      }
    }

    for (ci in cand_start_indices) {
      name_line <- page_lines[ci]
      # Format: "   1         Proch, Heinz-Jürgen               Bürgermeister                 2.287           52,3%"
      # Extract: number, name, profession, votes, pct
      nm <- str_match(name_line,
        "^\\s+\\d+\\s{2,}(.+?)\\s{3,}(.+?)\\s+([\\d.]+)\\s+\\d+[,.]\\d+%?\\s*$")
      if (is.na(nm[1])) next

      cand_name <- str_trim(nm[2])
      profession <- str_trim(nm[3])
      vote_str <- gsub("\\.", "", nm[4])
      votes <- as.numeric(vote_str)
      # Three or more spaces after the comma ("Nachname,   Dr. Vorname",
      # Langenhagen 2006) end the lazy name group at the comma, so the given
      # name leads the Beruf field. Move it back: otherwise the runoff row has
      # no first name and no longer pairs with the same person's Hauptwahl row.
      if (grepl(",$", cand_name)) {
        pf <- str_split(profession, "\\s{3,}", n = 2)[[1]]
        if (length(pf) == 2) {
          cand_name <- paste(cand_name, pf[1])
          profession <- str_trim(pf[2])
        }
      }

      name_parts <- str_split(cand_name, ",\\s*")[[1]]
      last_name <- str_trim(name_parts[1])
      first_name <- if (length(name_parts) > 1) str_trim(paste(name_parts[-1], collapse = ", ")) else NA_character_

      # Line 2: birth year and party
      birth_year <- NA_real_
      party <- NA_character_
      if (ci + 1 <= length(page_lines)) {
        party_line <- page_lines[ci + 1]
        pm <- str_match(party_line, "^\\s+(\\d{4})\\s+(.+?)\\s+[\\d.]+\\s+\\d+[,.]\\d+")
        if (!is.na(pm[1])) {
          birth_year <- as.numeric(pm[2])
          party_raw <- str_trim(pm[3])
          if (party_raw %in% names(ns_party_map_2006)) {
            party <- unname(ns_party_map_2006[party_raw])
          } else {
            party <- party_raw
          }
        }
      }

      candidates[[length(candidates) + 1]] <- data.frame(
        candidate_name = cand_name,
        candidate_last_name = last_name,
        candidate_first_name = first_name,
        candidate_party = party,
        candidate_votes = votes,
        candidate_profession = profession,
        candidate_birth_year = birth_year,
        stringsAsFactors = FALSE
      )
    }

    if (length(candidates) == 0) return(NULL)

    cand_df <- bind_rows(candidates) %>%
      mutate(
        ags = ags,
        ags_name = ort_name,
        state = "03",
        state_name = "Niedersachsen",
        election_year = election_year,
        election_date = as.Date(election_date),
        election_type = election_type_raw,
        round = "stichwahl",
        eligible_voters = wahlberechtigte,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = ungueltige,
        turnout = ifelse(!is.na(wahlberechtigte) & wahlberechtigte > 0 &
                           !is.na(waehler),
                         waehler / wahlberechtigte, NA),
        candidate_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 &
                                       !is.na(candidate_votes),
                                     candidate_votes / gueltige, NA)
      )

    return(cand_df)
  }

  # --------------------------------------------------------------------------
  # 2013 tabular parser — winner-only rows (no individual candidate detail)
  # --------------------------------------------------------------------------
  parse_ns_candidates_2013_tabular <- function(pdf_file, election_date,
                                                is_stichwahl = FALSE) {
    pages <- pdftools::pdf_text(pdf_file)
    results <- list()

    page <- pages[1]
    page_lines <- str_split(page, "\n")[[1]]

    for (l in page_lines) {
      l_trimmed <- str_trim(l)
      if (l_trimmed == "" || !grepl("^\\d+\\s", l_trimmed)) next
      if (grepl("Gesamt:", l_trimmed)) next

      all_tokens <- str_extract_all(l_trimmed, "[\\d.,]+%?")[[1]]
      nr <- all_tokens[1]
      n <- length(all_tokens)

      is_stichwahl_required <- !is_stichwahl &&
        grepl("Stichwahl", l_trimmed) && !grepl("Wahlsieg", l_trimmed)

      if (is_stichwahl_required) {
        wahlber <- as.numeric(gsub("\\.", "", all_tokens[n-3]))
        waehler <- as.numeric(gsub("\\.", "", all_tokens[n-2]))
        gueltige <- as.numeric(gsub("\\.", "", all_tokens[n-1]))

        bez_match <- str_match(l_trimmed,
          "^\\d+\\s+(.+?)\\s+(LK|LHH|Stadt|Gem\\.|SG|Flecken)")
        kommune <- if (!is.na(bez_match[1])) str_trim(bez_match[2]) else NA_character_
        bezeichnung <- if (!is.na(bez_match[1])) bez_match[3] else NA_character_

        el_type <- case_when(
          bezeichnung %in% c("LK", "LHH") ~ "Landratswahl",
          bezeichnung == "SG" ~ "SG-Bürgermeisterwahl",
          TRUE ~ "Bürgermeisterwahl"
        )

        # No winner in Stichwahl-required first round — skip
        next
      }

      # Winner row
      stimmen <- as.numeric(gsub("\\.", "", all_tokens[n-5]))
      wahlber <- as.numeric(gsub("\\.", "", all_tokens[n-3]))
      waehler <- as.numeric(gsub("\\.", "", all_tokens[n-2]))
      gueltige <- as.numeric(gsub("\\.", "", all_tokens[n-1]))

      stimmen_str <- all_tokens[n-5]
      stimmen_pos <- regexpr(stimmen_str, l_trimmed, fixed = TRUE)[1]
      text_part <- str_trim(substr(l_trimmed, nchar(nr) + 1, stimmen_pos - 1))

      text_part <- sub("\\s+Wahlsieg\\s+", " ", text_part)

      winner_party_val <- str_extract(text_part, "\\S+$")
      text_rest <- str_trim(sub("\\S+\\s*$", "", text_part))

      bez_match <- str_match(text_rest,
        "^(.+?)\\s+(LK|LHH|Stadt|Gem\\.|SG|Flecken)\\s*(.*)")
      if (!is.na(bez_match[1])) {
        kommune <- str_trim(bez_match[2])
        bezeichnung <- bez_match[3]
        # Group 3 is the third column of the table, headed
        # "Wahlsieger (Name, Vorname)" — e.g. "Averbeck, Tobias",
        # "Dr. Junk, Oliver". The regex always captured it and the parser
        # discarded it, which is why all 28 Niedersachsen 2013 winners were
        # published with a party and a vote count but no name. Rows that went
        # to a runoff print no name here and are skipped further up.
        cand_name <- str_trim(bez_match[4])
        if (is.na(cand_name) || !nzchar(cand_name)) cand_name <- NA_character_
      } else {
        kommune <- NA_character_
        bezeichnung <- NA_character_
        cand_name <- NA_character_
      }

      el_type <- case_when(
        bezeichnung %in% c("LK", "LHH") ~ "Landratswahl",
        bezeichnung == "SG" ~ "SG-Bürgermeisterwahl",
        TRUE ~ "Bürgermeisterwahl"
      )

      results[[length(results) + 1]] <- data.frame(
        ags = NA_character_,
        ags_name = kommune,
        state = "03",
        state_name = "Niedersachsen",
        election_year = 2013L,
        election_date = as.Date(election_date),
        election_type = el_type,
        round = if (is_stichwahl) "stichwahl" else "hauptwahl",
        eligible_voters = wahlber,
        number_voters = waehler,
        valid_votes = gueltige,
        invalid_votes = NA_real_,
        turnout = ifelse(!is.na(wahlber) & wahlber > 0,
                         waehler / wahlber, NA),
        candidate_name = cand_name,
        candidate_last_name = strip_name_titles(
          str_trim(str_extract(cand_name, "^[^,]+"))),
        candidate_first_name = strip_name_titles(
          str_trim(str_extract(cand_name, ",\\s*(.+)$", group = 1))),
        candidate_party = winner_party_val,
        candidate_votes = stimmen,
        candidate_voteshare = ifelse(!is.na(gueltige) & gueltige > 0 & !is.na(stimmen),
                                     stimmen / gueltige, NA),
        stringsAsFactors = FALSE
      )
    }

    if (length(results) > 0) bind_rows(results) else NULL
  }

  # --------------------------------------------------------------------------
  # AGS lookup for 2013
  # --------------------------------------------------------------------------
  # 15 codes corrected — KEEP IN SYNC WITH THE TWIN IN 01_mayoral_unharm.R,
  # where the full note lives. Each correction is verified against
  # data/crosswalks/final/ags_crosswalks.csv for vintage 2013.
  ns_2013_ags_lookup <- c(
    "Adelebsen" = "03152001",
    "Bakum" = "03460001",            # was 03460002 = Damme, Stadt
    "Belm" = "03459008",             # was 03459002 = Ankum
    "Bückeburg" = "03257009",        # was 03257007 = Beckedorf
    "Cappeln" = "03453003",
    "Dahlenburg" = "03355401",
    "Faßberg" = "03351010",          # was 03350007 = not a 2013 code
    # Goslar, Stadt is 03153005 until the 2014 Kreis reform and 03153017 from
    # 2014; this lookup serves the 2013 election, and the 2006 and 2011 rows
    # already use 03153005. Harmonisation converges on 03153017 either way.
    "Goslar" = "03153005",
    "Hameln-Pyrmont" = "03252000",
    "Hannover" = "03241001",
    "Hildesheim" = "03254021",
    "Hollenstedt" = "03353403",
    "Jever" = "03455007",            # was 03455008 = not a 2013 code
    "Katlenburg-Lindau" = "03155007",# was 03155011 = Northeim, Stadt
    "Langelsheim" = "03153007",      # was 03153022 = not a 2013 code
    "Liebenburg" = "03153008",       # was 03153023 = not a 2013 code
    "Marklohe" = "03256403",
    "Neuenkirchen" = "03461401",
    "Northeim LK" = "03155000",
    "Northeim Stadt" = "03155011",   # was 03155024 = not a 2013 code
    "Osnabrück" = "03404000",
    "Osterholz" = "03356000",
    "Ronnenberg" = "03241014",       # was 03241016 = Sehnde, Stadt
    "Schladen-Werla" = "03158039",
    "Schüttorf" = "03456404",
    "Seevetal" = "03353031",         # was 03353029 = Rosengarten
    "Steyerberg" = "03256030",       # was 03256025 = Rehburg-Loccum, Stadt
    "Sulingen" = "03251040",         # was 03251037 = Stuhr
    "Syke" = "03251041",             # was 03251038 = Sudwalde
    "Velpke" = "03154403",
    "Wesermarsch" = "03461000",
    "Wiefelstede" = "03451008",      # was 03451020 = not a 2013 code
    "Wolfenbüttel" = "03158000"
  )

  # NI kreisfreie Städte + Landeshauptstadt — Oberbürgermeister/-in. See the
  # twin note in 01_mayoral_unharm.R.
  ns_ob_ags <- c("03101000", "03102000", "03103000", "03241001",
                 "03401000", "03402000", "03403000", "03404000", "03405000")

  # --------------------------------------------------------------------------
  # Year configuration
  # --------------------------------------------------------------------------
  ns_year_config <- list(
    list(year = 2006L, date = "2006-09-10",
         files = list(
           list(path = "DW2006/KW 2006 - Vorläufiges Ergebnis Direktwahlen.pdf",
                parser = "standard", german_nums = TRUE),
           list(path = "DW2006/Stichwahlen 2006.pdf",
                parser = "stichwahl_2006", date = "2006-09-24")
         )),
    list(year = 2011L, date = "2011-09-11",
         files = list(
           list(path = "DW2011/Einzelergebnisse.pdf", parser = "standard")
         )),
    list(year = 2013L, date = "2013-09-22",
         files = list(
           list(path = "DW2013/20130923_Direktwahlen_am_22-09-2013_Ergebnisse.pdf",
                parser = "tabular_2013"),
           list(path = "DW2013/20131007_Stichwahlen_am_06-10-2013_Ergebnisse.pdf",
                parser = "tabular_2013_sw", date = "2013-10-06")
         )),
    # 2014/2016/2019 runoffs exist only as a one-page summary table naming the
    # winner; see attach_ns_sw_candidates above. DW2016/Einzelergebnisse.pdf is
    # a text-identical duplicate of the Hauptwahl file, not the runoff.
    list(year = 2014L, date = "2014-05-25",
         files = list(
           list(path = "DW2014/einzelergebnisse-direktwahlen14.pdf", parser = "standard"),
           list(path = "DW2014/SW_Vorlaeufige_Ergebnisse_Stichwahlen_15.06.2014.pdf",
                parser = "sw_rich", date = "2014-06-15")
         )),
    list(year = 2016L, date = "2016-09-11",
         files = list(
           list(path = "DW2016/DW_Einzel.pdf", parser = "standard"),
           list(path = "DW2016/SW_Vorlaeufige_Ergebnisse_Stichwahlen_25.09.2016.pdf",
                parser = "sw_rich", date = "2016-09-25")
         )),
    list(year = 2017L, date = "2017-09-24",
         files = list(
           list(path = "DW2017/HW_Ergebnisuebersicht_Direktwahlen_24.09.2017.pdf",
                parser = "ue_2017")
         )),
    list(year = 2019L, date = "2019-05-26",
         files = list(
           list(path = "DW2019/DW_Einzel.pdf", parser = "standard"),
           list(path = "DW2019/SW_Ergebnisuebersicht_Stichwahlen_16.06.2019.pdf",
                parser = "sw_names", date = "2019-06-16")
         )),
    list(year = 2021L, date = "2021-09-12",
         files = list(
           list(path = "DW2021/20210912_DW_Einzel.pdf", parser = "standard"),
           # DW_Einzel.pdf is the STICHWAHL file of 26.09.2021, not a duplicate of
           # the Hauptwahl: 85 pages, zero "Stichwahl erforderlich" and 85x "kann
           # als gewählt gelten". Registered with the Hauptwahl's date and round
           # it collided with the first-round rows on the distinct() key and all
           # 85 runoff results were discarded, leaving 78 mayoral + 8 Landrat
           # elections with no winner at all.
           list(path = "DW2021/DW_Einzel.pdf", parser = "standard",
                date = "2021-09-26", round = "stichwahl")
         )),
    list(year = 2024L, date = "2024-06-09",
         files = list(
           list(path = "DW2024/DW_Einzel.pdf", parser = "standard")
         )),
    list(year = 2025L, date = "2025-02-23",
         files = list(
           list(path = "DW2025/DW_Einzel.pdf", parser = "standard")
         ))
  )

  # --------------------------------------------------------------------------
  # Process all years
  # --------------------------------------------------------------------------
  ns_all_results <- list()

  for (yc in ns_year_config) {
    cat("  Processing NS", yc$year, "...\n")

    for (fc in yc$files) {
      pdf_path <- file.path(ns_base, fc$path)
      if (!file.exists(pdf_path)) {
        cat("    WARNING: File not found:", fc$path, "\n")
        next
      }

      file_date <- if (!is.null(fc$date)) fc$date else yc$date
      file_year <- yc$year

      if (fc$parser == "standard") {
        pages <- pdftools::pdf_text(pdf_path)
        german_nums <- isTRUE(fc$german_nums)
        file_round <- if (!is.null(fc$round)) fc$round else "hauptwahl"
        file_results <- list()

        for (i in seq_along(pages)) {
          row <- parse_ns_candidates_standard(pages[i], file_year, file_date,
                                              german_nums = german_nums,
                                              round = file_round)
          if (!is.null(row)) {
            file_results[[length(file_results) + 1]] <- row
          }
        }

        if (length(file_results) > 0) {
          file_df <- bind_rows(file_results)
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser == "ue_2017") {
        file_df <- parse_ns_2017_candidates(pdf_path)
        if (!is.null(file_df)) {
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser %in% c("sw_rich", "sw_names")) {
        sw <- if (fc$parser == "sw_rich") parse_ns_sw_rich(pdf_path)
              else parse_ns_sw_names(pdf_path)
        if (is.null(sw) || nrow(sw) == 0) {
          stop("NS ", file_year, ": no rows parsed from ", fc$path)
        }
        file_df <- attach_ns_sw_candidates(sw, bind_rows(ns_all_results),
                                           file_year, file_date)
        cat("    ", fc$path, ":", nrow(file_df),
            "runoff winners matched to their Hauptwahl\n")
        ns_all_results[[length(ns_all_results) + 1]] <- file_df

      } else if (fc$parser == "stichwahl_2006") {
        pages <- pdftools::pdf_text(pdf_path)
        file_results <- list()

        for (i in seq_along(pages)) {
          row <- parse_ns_candidates_2006_stichwahl(pages[i], file_year, file_date)
          if (!is.null(row)) {
            file_results[[length(file_results) + 1]] <- row
          }
        }

        if (length(file_results) > 0) {
          file_df <- bind_rows(file_results)
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }

      } else if (fc$parser %in% c("tabular_2013", "tabular_2013_sw")) {
        is_sw <- fc$parser == "tabular_2013_sw"
        file_df <- parse_ns_candidates_2013_tabular(pdf_path, file_date,
                                                     is_stichwahl = is_sw)

        if (!is.null(file_df) && nrow(file_df) > 0) {
          # Apply AGS lookup
          file_df$ags <- ns_2013_ags_lookup[file_df$ags_name]
          na_mask <- is.na(file_df$ags)
          if (any(na_mask)) {
            type_suffix <- case_when(
              file_df$election_type == "Landratswahl" ~ "LK",
              file_df$election_type == "Oberbürgermeisterwahl" ~ "Stadt",
              TRUE ~ "Stadt"
            )
            compound_key <- paste(file_df$ags_name, type_suffix)
            file_df$ags[na_mask] <- ns_2013_ags_lookup[compound_key[na_mask]]
          }
          na_ags <- sum(is.na(file_df$ags))
          if (na_ags > 0) {
            cat("    WARNING:", na_ags, "municipalities without AGS mapping:",
                paste(file_df$ags_name[is.na(file_df$ags)], collapse = ", "), "\n")
          }
          # kreisfreie Stadt / Landeshauptstadt -> Oberbürgermeisterwahl
          # (the 2013 Bezeichnung column cannot express this; see ns_ob_ags)
          ob_mask <- !is.na(file_df$ags) & file_df$ags %in% ns_ob_ags &
                       file_df$election_type == "Bürgermeisterwahl"
          if (any(ob_mask)) {
            file_df$election_type[ob_mask] <- "Oberbürgermeisterwahl"
            cat("    Retyped", sum(ob_mask), "kreisfreie-Stadt row(s) as",
                "Oberbürgermeisterwahl\n")
          }
          cat("    ", fc$path, ":", nrow(file_df), "candidate rows\n")
          ns_all_results[[length(ns_all_results) + 1]] <- file_df
        }
      }
    }
  }

  # Combine all NS results
  if (length(ns_all_results) > 0) {
    ns_combined <- bind_rows(ns_all_results) %>%
      # Deduplicate: 2021 has two PDFs (Endergebnis + vorläufiges Ergebnis)
      # with same candidates but different vote counts.
      # Keep only the Endergebnis = the row with the highest eligible_voters.
      group_by(ags, election_date, round, candidate_name) %>%
      arrange(desc(eligible_voters)) %>%
      slice(1) %>%
      ungroup() %>%
      # Fix implausible birth years from OCR errors (e.g. 1076, 2539, 6935)
      mutate(candidate_birth_year = ifelse(
        !is.na(candidate_birth_year) &
          (candidate_birth_year < 1880 | candidate_birth_year > 2010),
        NA_real_, candidate_birth_year)) %>%
      # Remove rows where ALL candidate data is NA (parsing failures)
      filter(!is.na(candidate_votes) | !is.na(candidate_name)) %>%
      # Compute rank and n_candidates per election-round
      group_by(ags, election_date, round) %>%
      mutate(
        candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
        n_candidates = n(),
        is_winner = candidate_rank == 1
      ) %>%
      ungroup() %>%
      # The 2014/2016/2019 runoff tables publish the Wahlsieger and no one else.
      # Take the winner from the source rather than from vote rank (2019 has no
      # counts at all), and report the field size as unknown instead of 1 — a
      # runoff is contested by two candidates, we simply only know one of them.
      mutate(
        is_winner = if_else(!is.na(sw_winner_only) & sw_winner_only, TRUE, is_winner),
        candidate_rank = if_else(!is.na(sw_winner_only) & sw_winner_only,
                                 1, candidate_rank),
        n_candidates = if_else(!is.na(sw_winner_only) & sw_winner_only,
                               NA_integer_, as.integer(n_candidates))
      ) %>%
      # Both are internal pairing aids, not published columns.
      select(-any_of(c("sw_winner_only", "needs_stichwahl")))

    ns_clean <- standardise_candidates(ns_combined)

    cat("Niedersachsen: Processed", nrow(ns_clean), "candidate rows across",
        length(unique(ns_clean$election_year)), "years\n")
    all_candidate_data[["niedersachsen"]] <- ns_clean
  } else {
    cat("WARNING: No Niedersachsen data processed\n")
  }

} else {
  cat("Note: pdftools package not available, skipping Niedersachsen PDF extraction\n")
}

# ============================================================================
# SCHLESWIG-HOLSTEIN
# ============================================================================
# Data is scraped from wahlen-sh.de by 00_sh_scrape.R.
# The scraped data is already in candidate-level format.
# Special cases:
#   - 4 confirmation elections (Ja/Nein) from 2024: candidate appears twice
#     (once as "EB"/"Ja-Stimmen" for yes-votes, once as "Nein-Stimmen" for no-votes).
#     We keep only the Ja row and set candidate_party = "EB".
#   - Party names already standardised by the scraper (standardise_sh_party).
#   - All elections have candidate names (Name, Last format).

cat("\n=== Processing Schleswig-Holstein mayoral elections ===\n")

sh_file <- "data/mayoral_elections/raw/sh/sh_mayoral_scraped.rds"

if (file.exists(sh_file)) {
  sh_raw <- readRDS(sh_file)

  # The portal spells the confirmation-ballot options three ways -- "Ja-Stimmen",
  # "Ja Stimmen" and plain "Ja" (likewise Nein) -- so the literal comparisons
  # used here let the 2026 elections through with candidate_party = "Ja".
  # Keep in sync with the same helpers in 01_mayoral_unharm.R.
  sh_is_ja   <- function(x) !is.na(x) & grepl("^ja[ -]?(stimmen)?$",   x, ignore.case = TRUE)
  sh_is_nein <- function(x) !is.na(x) & grepl("^nein[ -]?(stimmen)?$", x, ignore.case = TRUE)

  sh_candidates <- sh_raw %>%
    # For Ja/Nein confirmation elections: drop the "Nein" rows, which are a
    # ballot option rather than a candidate, and record the candidate as EB.
    filter(!sh_is_nein(candidate_party)) %>%
    mutate(
      candidate_party = ifelse(sh_is_ja(candidate_party), "EB", candidate_party),
      # candidate_name is already in "Last, First" format from the scraper
      election_year = year(election_date),
      # Compute turnout
      turnout = ifelse(!is.na(number_voters) & !is.na(eligible_voters) &
                         eligible_voters > 0,
                       number_voters / eligible_voters, NA_real_)
    ) %>%
    # Compute rank and n_candidates per election-round
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      is_winner = candidate_rank == 1
    ) %>%
    ungroup() %>%
    # Ensure we have all required columns
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = NA_character_
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank,
      is_winner
    )

  sh_clean <- standardise_candidates(sh_candidates)

  cat("Schleswig-Holstein: Processed", nrow(sh_clean), "candidate rows across",
      length(unique(sh_clean$election_year)), "years\n")
  all_candidate_data[["schleswig_holstein"]] <- sh_clean
} else {
  cat("Note: SH scraped data not found at", sh_file, "\n")
  cat("  Run 00_sh_scrape.R first to generate the data.\n")
}

# ============================================================================
# MECKLENBURG-VORPOMMERN
# ============================================================================
# Two candidate-level Stage-0 sources, covering disjoint offices (see the
# matching block in 01_mayoral_unharm.R):
#   - mv_parsed.csv     : Oberbürgermeister + Landrat, 2000-2026, from 69
#     LAIV-MV PDFs (00_mv_parse.py).
#   - mv_lup_parsed.csv : Bürgermeister of the five amtsfreien Gemeinden of the
#     Landkreis Ludwigslust-Parchim, 2014-2023 (00_mv_lup_parse.py).
# Both are already candidate-level long (one row per candidate per round); we
# only need to add rank / n_candidates / is_winner, like SH.
# Special cases (parsed & flagged in the `notes` column):
#   - Ja/Nein single-candidate Stichwahl (Rügen 2001-05): only the "Ja" candidate
#     is recorded; its voteshare is NA. That Stichwahl failed (Ja votes below
#     25 % of the electorate) -> a Neuwahl (Rügen 2001-09) followed, recorded as
#     a separate cycle. 00_mv_parse.py flags the failed cycle
#     (flag_decisive_round_missing), so nobody is seated in May 2001.
#   - Wismar 2026 has no LAIV PDF: its official Bekanntmachungen are transcribed
#     (raw/mecklenburg_vorpommern/wismar_2026/) and appended by 00_mv_parse.py.
#   - Hagenow 2015-05-31: an inconclusive Hauptwahl (leader on 41.5 %) whose
#     deciding Stichwahl is not in the Landkreis delivery. mv_lup_parsed.csv
#     carries an explicit is_winner column for exactly this reason, and it wins
#     over rank below: ranking alone would seat the CDU candidate who lost the
#     runoff. Where the source states no flag (the LAIV file), rank decides.

cat("\n=== Processing Mecklenburg-Vorpommern mayoral/Landrat elections ===\n")

mv_files <- c("data/mayoral_elections/raw/mecklenburg_vorpommern/mv_parsed.csv",
              "data/mayoral_elections/raw/mecklenburg_vorpommern/mv_lup_parsed.csv")
mv_file <- mv_files[1]  # the LAIV file is required; the Kreis file is optional

if (file.exists(mv_file)) {
  mv_raw <- rbindlist(lapply(mv_files[file.exists(mv_files)], fread,
                             encoding = "UTF-8",
                             colClasses = list(character = c("ags", "ags_name",
                               "state", "state_name", "election_date",
                               "candidate_party", "candidate_name",
                               "candidate_last_name", "candidate_first_name"))),
                      fill = TRUE)
  # fill = TRUE gives the LAIV rows NA for the columns only the Kreis file has;
  # make sure they exist at all when only the LAIV file is present.
  if (!"is_winner" %in% names(mv_raw)) mv_raw[, is_winner := NA]
  if (!"flag_decisive_round_missing" %in% names(mv_raw))
    mv_raw[, flag_decisive_round_missing := NA]
  setnames(mv_raw, "is_winner", "is_winner_src")

  mv_candidates <- mv_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout)
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      # The source's own "was declared elected" flag beats rank wherever the
      # source states one; rank is the fallback for the LAIV rows, which carry
      # no such column. Leading a round is not the same as winning the election.
      is_winner = dplyr::coalesce(as.logical(is_winner_src), candidate_rank == 1)
    ) %>%
    ungroup() %>%
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = NA_character_
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner,
      flag_decisive_round_missing
    )

  mv_clean <- standardise_candidates(mv_candidates)

  cat("Mecklenburg-Vorpommern: Processed", nrow(mv_clean), "candidate rows across",
      length(unique(mv_clean$election_year)), "years\n")
  all_candidate_data[["mecklenburg_vorpommern"]] <- mv_clean
} else {
  cat("Note: MV parsed data not found at", mv_file, "\n")
  cat("  Run 00_mv_parse.py first to generate the data.\n")
}

# ============================================================================
# THÜRINGEN
# ============================================================================
# Candidate-level: th_bm_scraped.csv (all-Gemeinde Bürgermeisterwahlen, scraped
# from wahlen.thueringen.de) + th_ob_parsed.csv (kreisfreie-Stadt OB from raw
# files). Candidate names are mostly redacted (§50 ThürKWO) so candidate_party
# carries the Wahlvorschlag (party / Einzelbewerber label); candidate_name is
# populated only where the Wahlvorschlag names a person (and for the OB Info
# files 2006-2024, which list named candidates). See th_wahlvorschlag.R.

cat("\n=== Processing Thüringen mayoral elections (BM + OB) ===\n")

# th_mayoral_tls_augmented.csv (00_th_tls_reports_parse.py) = both files plus
# the TLS Kommunalwahl reports 2000/2004/2010: names grafted onto existing rows
# (tls_name), added rounds, runoff links (pair_key), the four failed write-in
# elections of 2004 (flag_decisive_round_missing) and winner-only elections
# (winner_only). See the Thüringen block of 01_mayoral_unharm.R.
th_files <- c("data/mayoral_elections/raw/thueringen_bm/th_bm_scraped.csv",
              "data/mayoral_elections/raw/thueringen/th_ob_parsed.csv")
th_aug <- "data/mayoral_elections/raw/thueringen_bm/th_mayoral_tls_augmented.csv"
th_have <- th_files[file.exists(th_files)]
if (file.exists(th_aug)) th_have <- th_aug

if (length(th_have) > 0) {
  th_raw <- bind_rows(lapply(th_have, function(f)
    fread(f, encoding = "UTF-8",
          colClasses = list(character = c("ags", "ags_name", "state", "state_name",
            "election_date", "candidate_party", "candidate_name")))))
  if (identical(th_have, th_aug)) {
    n_scraped <- nrow(fread(th_files[1], select = "ags"))
    if (sum(th_raw$source == "wahlen.thueringen.de") != n_scraped)
      stop("th_mayoral_tls_augmented.csv is out of date: re-run 00_th_tls_reports_parse.py")
  }
  # Eisenach 2018: the database repeats an OB election of the OB file
  source("code/mayoral_elections/th_ob_overlap.R")
  th_raw <- drop_th_db_ob_repeats(th_raw)
  for (col in c("tls_name", "pair_key")) {
    th_raw[[col]] <- if (col %in% names(th_raw)) as.character(th_raw[[col]]) else NA_character_
  }
  th_flag_tls <- if ("flag_decisive_round_missing" %in% names(th_raw))
    as.logical(th_raw$flag_decisive_round_missing) %in% TRUE else rep(FALSE, nrow(th_raw))
  th_winner_only <- if ("winner_only" %in% names(th_raw))
    as.logical(th_raw$winner_only) %in% TRUE else rep(FALSE, nrow(th_raw))

  # The database writes a named candidate's Wahlvorschlag as "Nachname, Vorname"
  # or "Nachname, Vorname (Träger)", and 00_th_scrape.py copied that string into
  # both candidate_name and candidate_party. Split it: the name without the
  # bracket, the Träger as the party (NA when the source gives none). Joint lists
  # such as "SPD, CDU, UWS" are not names (known issues §16).
  source("code/mayoral_elections/th_wahlvorschlag.R")
  from_db <- th_raw$source == "wahlen.thueringen.de"
  th_wv <- split_th_wahlvorschlag(th_raw$candidate_party[from_db])
  th_raw$candidate_name[from_db] <- ifelse(th_wv$person, th_wv$name, "")
  th_raw$candidate_party[from_db] <- th_wv$party
  # Names from the TLS reports 2000/2004/2010 for rows the database leaves unnamed
  # (§ 50 ThürKWO); the database's own name wins where it has one.
  th_raw$candidate_name[is.na(th_raw$candidate_name)] <- ""
  th_fill <- !nzchar(th_raw$candidate_name) & !is.na(th_raw$tls_name) & nzchar(th_raw$tls_name)
  th_raw$candidate_name[th_fill] <- th_raw$tls_name[th_fill]
  th_raw$candidate_party[!is.na(th_raw$candidate_party) & !nzchar(th_raw$candidate_party)] <- NA
  cat("  Names from the TLS reports 2000/2004/2010:", sum(th_fill), "input rows named,",
      sum(!from_db & grepl("^TLS", th_raw$source) & nzchar(th_raw$candidate_name)),
      "named rows added\n")
  # No name from the TLS reports is published, the elected persons' included:
  # the TLS consented to publishing the results only "unter Löschung sämtlicher
  # personenbezogenen Daten zu Bewerbern und gewählten Kandidaten" (§ 50 Abs. 2
  # ThürKWO; e-mail of 12.10.2026, Vorgang 1167/2026). anonymise_restricted_names()
  # strips them from the public file; the restricted twin keeps them.
  th_raw$name_restricted <- th_fill | (grepl("^TLS", th_raw$source) & nzchar(th_raw$candidate_name))

  # A round whose elected person declined ("Wahl nicht angenommen; Neuwahlen
  # erforderlich": 16077047, Stichwahl 2026-03-15) seated nobody, like the failed
  # Hessen Ja/Nein votes. Flag it and the Hauptwahl of its cycle (within 60 days
  # before) so the winner repair below leaves the cycle without a winner. The
  # four write-in elections of 27.06.2004 without a majority ("Wiederholungswahl
  # erforderlich", TLS 20421) come flagged from Stage 0.
  th_raw$flag_decisive_round_missing <- th_flag_tls
  for (i in which(from_db & grepl("nicht angenommen", th_raw$winner_name_raw))) {
    d <- as.Date(th_raw$election_date[i])
    th_raw$flag_decisive_round_missing[th_raw$ags == th_raw$ags[i] &
      as.Date(th_raw$election_date) <= d & as.Date(th_raw$election_date) >= d - 60] <- TRUE
  }

  th_candidates <- th_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      state = "16", state_name = "Thüringen",
      candidate_name = ifelse(candidate_name == "", NA_character_, candidate_name),
      winner_only = th_winner_only,
      # Stage 0's verdict where rank alone is wrong: in the 2009-2010 no-runoff
      # window the top NAMED candidate won, not the pooled write-ins
      is_winner_src = if ("is_winner_src" %in% names(th_raw)) is_winner_src else ""
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      # a winner-only election (TLS table 3.4) does not say how many stood
      n_candidates = if (any(winner_only)) NA_integer_ else n(),
      is_winner = if (any(is_winner_src %in% c("TRUE", "FALSE"))) is_winner_src %in% "TRUE"
                  else candidate_rank == 1
    ) %>%
    ungroup() %>%
    mutate(
      # Names are "Last, First" (OB Info files; named Wahlvorschläge, split above).
      # Most BM candidate_name is NA (redacted by §50 ThürKWO) -> no person tracking.
      # Academic titles leave the surname; candidate_name keeps the source string.
      candidate_last_name = ifelse(grepl(",", candidate_name),
                                   trimws(sub("^((Dr|Prof)\\.( [a-z]+\\.)* )+", "",
                                              sub(",.*$", "", candidate_name))), NA_character_),
      # The OB Info files write "Name, Vorname, Dr." -- the title leaves the given name.
      candidate_first_name = ifelse(grepl(",", candidate_name),
                                    trimws(sub(",\\s*((Dr|Prof)\\.[^,]*)$", "",
                                               sub("^[^,]*,\\s*", "", candidate_name))), NA_character_),
      candidate_gender = NA_character_, candidate_birth_year = NA_real_,
      candidate_profession = NA_character_, office_type = NA_character_
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner,
      flag_decisive_round_missing, pair_key, name_restricted
    )

  th_clean <- standardise_candidates(th_candidates)

  cat("Thüringen: Processed", nrow(th_clean), "candidate rows across",
      length(unique(th_clean$election_year)), "years\n")
  all_candidate_data[["thueringen"]] <- th_clean
} else {
  cat("Note: Thüringen parsed data not found.\n")
  cat("  Run 00_th_scrape.py and 00_th_mayoral_parse.py first.\n")
}

# ============================================================================
# BADEN-WÜRTTEMBERG  (hybrid: Komm.ONE candidate-level + StaLA winner-only fallback)
# ============================================================================
# Two BW sources, merged:
#  (1) StaLA report (Tables 13+14) -> bw_parsed.csv (00_bw_parse.py): the *winner*
#      only, but with official-register gender + birth year; "letzte Wahl je
#      Gemeinde" @31.12.2024. candidate_party is always NA (BW has no party).
#  (2) Komm.ONE votemanager -> bw_komm_one_parsed.csv (00_bw_komm_one_scrape.py):
#      ALL candidates + votes + full turnout + Hauptwahl & Stichwahl, for the
#      municipalities whose mayoral election is published in Komm.ONE's modern
#      JSON format (~2021-2026). No party, no birth year.
# Komm.ONE supersedes StaLA where it covers an election (same ags + date): we use
# its full candidate list and drop the StaLA winner-only row for that election.
# To stay strictly additive we graft StaLA's register gender + birth year onto the
# matching Komm.ONE candidate (by ags + date + last name) so the winner keeps those
# attributes; losing candidates get gender predicted downstream by 04. The winner is
# attached to every round so the HW shell pairs with the SW row by name in the wide
# reshaping; is_winner flags the decisive round.

cat("\n=== Processing Baden-Württemberg mayoral elections ===\n")

bw_file <- "data/mayoral_elections/raw/baden_wuerttemberg/bw_parsed.csv"
bw_komm_file <- "data/mayoral_elections/raw/baden_wuerttemberg/bw_komm_one_parsed.csv"
bw_augmented <- "data/mayoral_elections/raw/baden_wuerttemberg/bw_komm_one_augmented.csv"
if (file.exists(bw_augmented)) bw_komm_file <- bw_augmented

if (file.exists(bw_file)) {
  bw_raw <- fread(bw_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name",
                    "candidate_first_name", "candidate_gender", "round")))

  bw_stala <- bw_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      candidate_birth_year = as.numeric(candidate_birth_year),
      turnout = as.numeric(turnout),
      is_decisive = as.logical(is_decisive)
    ) %>%
    mutate(
      candidate_rank = 1L,           # winner is the only reported candidate
      n_candidates = NA_integer_,    # losing candidates not published by StaLA
      is_winner = is_decisive,       # winner counts as elected in decisive round
      candidate_party = NA_character_,  # BW mayoral candidates have no party
      candidate_profession = NA_character_,
      office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                           "Oberbürgermeister*in", "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  if (file.exists(bw_komm_file)) {
    bk <- fread(bw_komm_file, encoding = "UTF-8",
                colClasses = list(character = c("ags", "ags_name", "state",
                  "state_name", "election_date", "candidate_party", "candidate_name",
                  "candidate_last_name", "candidate_first_name", "round")))
    for (cl in c("flag_superseded", "flag_decisive_round_missing")) {
      if (!cl %in% names(bk)) bk[[cl]] <- FALSE
      bk[[cl]] <- as.logical(bk[[cl]])
    }
    bk[, pair_key := if ("pair_id" %in% names(bk)) as.character(pair_id) else NA_character_]
    bw_pair_file <- "data/mayoral_elections/raw/baden_wuerttemberg/bw_komm_one_fallback_pair_keys.csv"
    if (identical(bw_komm_file, bw_augmented) && file.exists(bw_pair_file)) {
      bp <- fread(bw_pair_file, colClasses = "character") %>% mutate(election_date = as.Date(election_date))
      bw_stala <- left_join(bw_stala, bp, by = c("ags", "election_date", "round"))
    }
    bk[, name_restricted := if ("name_withheld" %in% names(bk)) name_withheld %in% c("TRUE", TRUE) else FALSE]
    bw_names <- sub("\\.csv$", "_names_restricted.csv", bw_komm_file)
    if (file.exists(bw_names)) {
      bn <- fread(bw_names, colClasses = "character", na.strings = "")
      bn[, ballot_position := as.integer(ballot_position)]
      bk[bn, on = .(ags, election_date, round, ballot_position),
         `:=`(candidate_name = i.candidate_name, candidate_last_name = i.candidate_last_name,
              candidate_first_name = i.candidate_first_name)]
      stopifnot(sum(bk$name_restricted) == nrow(bn))
    }
    bk <- bk %>%
      mutate(
        election_date = as.Date(election_date),
        election_year = as.integer(election_year),
        eligible_voters = as.numeric(eligible_voters),
        number_voters = as.numeric(number_voters),
        valid_votes = as.numeric(valid_votes),
        invalid_votes = as.numeric(invalid_votes),
        turnout = as.numeric(turnout),
        candidate_votes = as.numeric(candidate_votes),
        candidate_voteshare = as.numeric(candidate_voteshare),
        candidate_rank = as.integer(candidate_rank),
        n_candidates = as.integer(n_candidates),
        is_winner = as.logical(is_winner)
      )
    # Complete cycles are keyed by their first round, not just the year.
    if ("cycle_complete" %in% names(bk)) {
      n_komm_cycles <- nrow(distinct(bk, ags, cycle_start))
      bk <- bk %>% filter(as.logical(cycle_complete))
      komm_complete <- distinct(bk, ags, cycle_start)
    } else {
      n_komm_cycles <- nrow(distinct(bk, ags, election_year))
      komm_complete <- bk %>% group_by(ags, election_year) %>%
        summarise(decisive = any(round == "stichwahl") ||
                    any(round == "hauptwahl" & is_winner &
                        !is.na(candidate_voteshare) & candidate_voteshare > 0.5),
                  .groups = "drop") %>% filter(decisive) %>% select(ags, election_year)
      bk <- semi_join(bk, komm_complete, by = c("ags", "election_year"))
    }
    komm_dates <- distinct(bk, ags, election_date)

    # Graft StaLA register gender + birth year onto the matching Komm.ONE candidate.
    # StaLA has one winner per (ags, election_date); match the Komm.ONE candidate
    # whose name tokens include ALL of the StaLA winner's last-name tokens AND the
    # first token of the StaLA first name. Whole-token (not substring) avoids "Rist"
    # matching inside "Christof"; multi-token surnames ("Franco Olias") still match;
    # and requiring the FIRST name disambiguates two same-surname candidates so the
    # winner's attributes are not copied onto a same-surname loser (e.g. winner
    # "<X> Seithel" must not graft onto the losing "Jana Seithel").
    norm_tok <- function(x) tolower(gsub("\\s+", " ", trimws(gsub("[-,.]", " ", x))))
    tok_match <- function(sl, sf, cn) {
      slt <- strsplit(sl, " "); cnt <- strsplit(cn, " ")
      mapply(function(a, fst, b) length(a) > 0 && all(a %in% b) &&
                                 (is.na(fst) || fst == "" || fst %in% b),
             slt, sf, cnt)
    }
    stala_reg <- bw_stala %>%
      filter(!is.na(candidate_gender) | !is.na(candidate_birth_year)) %>%
      transmute(ags, election_date,
                .sl = norm_tok(candidate_last_name),
                .sf = sub(" .*", "", norm_tok(candidate_first_name)),
                .graft_gender = candidate_gender, .graft_birth = candidate_birth_year) %>%
      filter(.sl != "" & !is.na(.sl)) %>%
      distinct(ags, election_date, .sl, .sf, .keep_all = TRUE)
    graft <- bk %>%
      transmute(ags, election_date, candidate_name, .cn = norm_tok(candidate_name)) %>%
      inner_join(stala_reg, by = c("ags", "election_date"),
                 relationship = "many-to-many") %>%
      filter(tok_match(.sl, .sf, .cn)) %>%
      distinct(ags, election_date, candidate_name, .keep_all = TRUE) %>%
      select(ags, election_date, candidate_name, .graft_gender, .graft_birth)
    # The register also settles the SURNAME of an elected candidate, which the
    # Komm.ONE string "Vorname Nachname" cannot: a two-word surname without a
    # particle ("Vorname A B"), a compound in front of a particle ("Vorname A von
    # B") and a cell written "Nachname Vorname" (08127089, 2023) are all split
    # wrongly by any rule. Where the register surname differs from the Stage-0
    # split and occurs verbatim in candidate_name, take it, and the rest of the
    # string (titles removed) as the given name. Matching as for the graft above.
    reg_split <- bk %>%
      transmute(ags, election_date, candidate_name, .cn = norm_tok(candidate_name),
                .split_last = norm_tok(candidate_last_name)) %>%
      inner_join(bw_stala %>%
                   filter(!is.na(candidate_last_name)) %>%
                   transmute(ags, election_date, .reg_last = candidate_last_name,
                             .sl = norm_tok(candidate_last_name),
                             .sf = sub(" .*", "", norm_tok(candidate_first_name))) %>%
                   distinct(ags, election_date, .sl, .sf, .keep_all = TRUE),
                 by = c("ags", "election_date"), relationship = "many-to-many") %>%
      filter(tok_match(.sl, .sf, .cn), .split_last != .sl,
             str_detect(candidate_name, fixed(.reg_last))) %>%
      distinct(ags, election_date, candidate_name, .keep_all = TRUE) %>%
      transmute(ags, election_date, candidate_name, .reg_last,
                .reg_first = strip_name_titles(str_squish(str_remove(candidate_name, fixed(.reg_last)))))
    bk <- bk %>%
      left_join(reg_split, by = c("ags", "election_date", "candidate_name")) %>%
      mutate(candidate_last_name = coalesce(.reg_last, candidate_last_name),
             candidate_first_name = if_else(!is.na(.reg_last), .reg_first, candidate_first_name))
    cat(sprintf("  register surname replaced the Komm.ONE split for %d candidate rows\n",
                sum(!is.na(bk$.reg_last))))
    # Keep the register's fuller given name where Komm.ONE prints only its first
    # part ("Anna" for "Anna Maria"; user decision 2026-10-29). Matching as above.
    reg_given <- bk %>%
      transmute(ags, election_date, candidate_name, .cn = norm_tok(candidate_name),
                .kf = norm_tok(candidate_first_name)) %>%
      inner_join(bw_stala %>%
                   filter(!is.na(candidate_first_name)) %>%
                   transmute(ags, election_date, .reg_given = candidate_first_name,
                             .sl = norm_tok(candidate_last_name),
                             .sf = sub(" .*", "", norm_tok(candidate_first_name)),
                             .rf = norm_tok(candidate_first_name)) %>%
                   distinct(ags, election_date, .sl, .sf, .keep_all = TRUE),
                 by = c("ags", "election_date"), relationship = "many-to-many") %>%
      filter(tok_match(.sl, .sf, .cn), startsWith(.rf, paste0(.kf, " "))) %>%
      distinct(ags, election_date, candidate_name, .keep_all = TRUE) %>%
      select(ags, election_date, candidate_name, .reg_given)
    bk <- bk %>%
      left_join(reg_given, by = c("ags", "election_date", "candidate_name")) %>%
      mutate(candidate_first_name = coalesce(.reg_given, candidate_first_name))
    cat(sprintf("  register given name kept for %d candidate rows\n", sum(!is.na(bk$.reg_given))))

    bk <- bk %>%
      left_join(graft, by = c("ags", "election_date", "candidate_name")) %>%
      mutate(
        candidate_gender = .graft_gender,           # NA where unmatched -> 04 predicts
        candidate_birth_year = .graft_birth,
        candidate_party = NA_character_,
        candidate_profession = NA_character_,
        office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                             "Oberbürgermeister*in", "Bürgermeister*in")
      ) %>%
      # A newly recovered first round must keep the register attributes found
      # on its runoff row (e.g. Boxberg 2021). Opaque links are person-specific.
      group_by(ags, pair_key) %>%
      mutate(
        candidate_gender = if (is.na(first(pair_key))) candidate_gender else
          coalesce(candidate_gender, first(candidate_gender[!is.na(candidate_gender)], default = NA_character_)),
        candidate_birth_year = if (is.na(first(pair_key))) candidate_birth_year else
          coalesce(candidate_birth_year, first(candidate_birth_year[!is.na(candidate_birth_year)], default = NA_real_))
      ) %>% ungroup() %>%
      select(
        ags, ags_name, state, state_name, election_year, election_date,
        election_type, round, eligible_voters, number_voters, valid_votes,
        invalid_votes, turnout, candidate_name, candidate_last_name,
        candidate_first_name, candidate_gender, candidate_party,
        candidate_votes, candidate_voteshare, candidate_birth_year,
        candidate_profession, office_type, n_candidates, candidate_rank, is_winner, name_restricted, pair_key,
        flag_superseded, flag_decisive_round_missing
      )
    # Drop only recovered dates; missing/conflicted rounds retain StaLA.
    bw_stala_keep <- bw_stala %>% anti_join(komm_dates, by = c("ags", "election_date"))
    bw_candidates <- bind_rows(bk, bw_stala_keep)
    cat(sprintf("  Komm.ONE: %d candidate rows, %d complete cycles used (%d incomplete -> StaLA fallback); ",
                nrow(bk), nrow(komm_complete), n_komm_cycles - nrow(komm_complete)))
    cat(sprintf("StaLA winner-only kept for %d elections\n",
                nrow(distinct(bw_stala_keep, ags, election_date))))
    cat(sprintf("  register gender grafted onto %d Komm.ONE candidate rows\n",
                sum(!is.na(bk$candidate_gender))))
  } else {
    bw_candidates <- bw_stala
    cat("  Note: Komm.ONE file not found; using StaLA winner-only data only.\n")
  }

  bw_clean <- standardise_candidates(bw_candidates)

  cat("Baden-Württemberg: Processed", nrow(bw_clean), "candidate rows across",
      length(unique(bw_clean$election_year)), "years\n")
  all_candidate_data[["baden_wuerttemberg"]] <- bw_clean
} else {
  cat("Note: BW parsed data not found at", bw_file, "\n")
  cat("  Run 00_bw_parse.py first to generate the data.\n")
}

# ============================================================================
# BRANDENBURG
# ============================================================================
# Candidate-level data for BB Bürgermeister-/Oberbürgermeisterwahlen of the
# amtsfreie Gemeinden/Städte + 4 kreisfreie Städte, 2010-2026.
#
# PRIMARY: bb_lwl_parsed.csv (00_bb_lwl_parse.py) — the workbook supplied by the
# Geschäftsstelle des Landeswahlleiters on 31.07.2026, already merged there with
# the older web-portal scrape (bb_bm_parsed.csv) for the 2 elections it lacks.
# Falls back to the portal scrape alone if the parser has not been run.
#
# The intermediate is candidate-level long (one row per candidate per round,
# with full names + party); we add rank / n_candidates here. `is_winner` comes
# from the source's own "sieger" record where it exists (it is filled for the
# DECISIVE round from 2018 on and survives Dr. titles), else from rank.
# Single-candidate rounds are Ja/Nein votes (candidate_votes = Ja-Stimmen);
# the Stage-0 parser sets is_winner = FALSE if such a ballot fell below 50 %,
# so a rejected candidate is never seated.
#
# HISTORICAL + EHRENAMTLICHE (October 2026): bb_afs_parsed.csv
# (00_bb_afs_reports_parse.py, Statistische Berichte B VII 3-6) is appended:
# hauptamtliche 1993-2008 and the ehrenamtliche Bürgermeister of 1998-2024.
#   * office "E" -> office_type "Ehrenamtliche(r) Bürgermeister*in";
#   * winner_only: only the elected person is known, so n_candidates is NA
#     (as for the Thüringen table-3.4 rows);
#   * flag_decisive_round_missing: the rounds seated nobody (is_winner NA);
#   * name_restricted: losing candidates' names, published in the restricted
#     twin only (anonymise_restricted_names() below, as for Hessen).
#
# HAUPTAMTLICHE 2004-2013 (October 2026): bb_lwl_scans_parsed.csv
# (00_bb_lwl_scans_parse.py, the Gemeinden's result reports from the
# Landeswahlleiter's archive) is appended the same way: full results with every
# Bewerber, office "H"; name_restricted on every Bewerber who did not win the
# cycle; flag_decisive_round_missing where the decisive round elected nobody and
# elected_by_council (first-round rows) where a runoff leader missed the 15 %
# quorum, after which the Gemeindevertretung elects (none in the delivery).
#
# 2001-2017 FROM THE LANDESWAHLLEITER'S ARCHIVED RESULT PAGES (October 2026):
# bb_lwl_archive_parsed.csv (00_bb_lwl_archive_parse.py, Internet Archive
# captures of wahlen.brandenburg.de/bmwahlen/) replaces 411 B VII 3-6 elections
# held winner-only and adds 7; the B VII 3-6 rounds it replaces are listed in
# bb_lwl_archive_supersedes.csv and dropped from bb_afs_parsed.csv first. A
# round the archive lacks is carried over into that file, so every election
# comes from one file and pairs by pair_key.

cat("\n=== Processing Brandenburg mayoral elections ===\n")

# bb_lwl_parsed.csv names every Bewerber and is gitignored (user decision
# 2026-10-19); a clone without it reads the tracked name-stripped copy that 01b
# writes from it (losers unnamed, rounds paired by pair_key), and only then the
# portal scrape.
bb_file <- "data/mayoral_elections/raw/brandenburg/bb_lwl_parsed.csv"
bb_public_file <- "data/mayoral_elections/raw/brandenburg/bb_lwl_parsed_public.csv"
if (!file.exists(bb_file) && file.exists(bb_public_file)) bb_file <- bb_public_file
if (!file.exists(bb_file)) {
  bb_file <- "data/mayoral_elections/raw/brandenburg/bb_bm_parsed.csv"
}
bb_afs_file <- "data/mayoral_elections/raw/brandenburg/bb_afs_parsed.csv"
bb_scan_file <- "data/mayoral_elections/raw/brandenburg/bb_lwl_scans_parsed.csv"
bb_arc_file <- "data/mayoral_elections/raw/brandenburg/bb_lwl_archive_parsed.csv"
bb_arc_sup_file <- "data/mayoral_elections/raw/brandenburg/bb_lwl_archive_supersedes.csv"
# The Landeswahlleiter's archived result pages (00_bb_lwl_archive_parse.py)
# replace B VII 3-6 elections held winner-only; the B VII 3-6 rounds they replace
# are listed in bb_lwl_archive_supersedes.csv and dropped here. Only together
# with the archive file, so a list without it can never delete rounds.
bb_drop_superseded <- function(d) {
  if (!file.exists(bb_arc_file)) return(d)
  if (!file.exists(bb_arc_sup_file)) stop(bb_arc_file, " exists without ", bb_arc_sup_file)
  sup <- fread(bb_arc_sup_file, colClasses = "character")
  n0 <- nrow(d)
  d <- d[!sup, on = c("ags", "election_date", "round")]
  stopifnot(n0 - nrow(d) == nrow(sup))      # every listed round existed, once
  d
}

if (file.exists(bb_file)) {
  bb_read <- function(f) fread(f, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name",
                    "candidate_first_name", "round")))
  bb_raw <- bb_read(bb_file)
  if (!"is_winner" %in% names(bb_raw)) bb_raw[, is_winner := NA]
  if (file.exists(bb_afs_file)) {
    bb_afs <- bb_drop_superseded(bb_read(bb_afs_file))
    cat("  + B VII 3-6 reports:", nrow(bb_afs), "candidate rows (",
        sum(bb_afs$office == "E"), "ehrenamtlich )\n")
    bb_raw <- rbindlist(list(bb_raw, bb_afs), fill = TRUE)
  }
  if (file.exists(bb_scan_file)) {
    bb_scan <- bb_read(bb_scan_file)
    cat("  + Landeswahlleiter result reports 2004-2013:", nrow(bb_scan), "candidate rows (",
        uniqueN(bb_scan, by = c("ags", "election_date", "round")), "rounds )\n")
    bb_raw <- rbindlist(list(bb_raw, bb_scan), fill = TRUE)
  }
  if (file.exists(bb_arc_file)) {
    bb_arc <- bb_read(bb_arc_file)
    cat("  + Landeswahlleiter result pages (Internet Archive):", nrow(bb_arc), "candidate rows (",
        uniqueN(bb_arc, by = c("ags", "election_date", "round")), "rounds; replacing",
        nrow(fread(bb_arc_sup_file)), "B VII 3-6 rounds )\n")
    bb_raw <- rbindlist(list(bb_raw, bb_arc), fill = TRUE)
  }
  for (col in c("winner_only", "flag_decisive_round_missing", "name_restricted")) {
    bb_raw[[col]] <- if (col %in% names(bb_raw)) as.logical(bb_raw[[col]]) %in% TRUE
                     else FALSE
  }
  if (!"office" %in% names(bb_raw)) bb_raw[, office := NA_character_]
  # The 2004-2013 result reports link each runoff Bewerber to the same person's
  # first-round row (pair_key, 00_bb_lwl_scans_parse.py): the printed names differ
  # between the two rounds for 4 Bewerber (3 cycles) and the Träger labels for 10
  # (8 cycles). NA for the other sources.
  if (!"pair_key" %in% names(bb_raw)) bb_raw[, pair_key := NA_character_]
  bb_raw[, pair_key := as.character(pair_key)]

  bb_candidates <- bb_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      .src_winner = as.logical(is_winner)
    ) %>%
    group_by(ags, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      # a winner-only round does not say how many stood
      n_candidates = if (any(winner_only)) NA_integer_ else n(),
      # Source flag first; rank only where the source has no elected-person
      # record (pre-2018 rounds and the portal-fallback elections). A round
      # that seated nobody has no winner.
      is_winner = if (any(flag_decisive_round_missing)) NA
                  else if (any(!is.na(.src_winner))) coalesce(.src_winner, FALSE)
                  else candidate_rank == 1
    ) %>%
    ungroup() %>%
    select(-.src_winner) %>%
    mutate(
      candidate_gender = NA_character_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        office %in% "E" ~ "Ehrenamtliche(r) Bürgermeister*in",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner,
      flag_decisive_round_missing, pair_key, name_restricted
    )

  bb_clean <- standardise_candidates(bb_candidates)

  cat("Brandenburg: Processed", nrow(bb_clean), "candidate rows across",
      length(unique(bb_clean$election_year)), "years  (source:",
      basename(bb_file), ")\n")
  all_candidate_data[["brandenburg"]] <- bb_clean
} else {
  cat("Note: BB parsed data not found at", bb_file, "\n")
  cat("  Run 00_bb_lwl_parse.py (and 00_bb_scrape.py) first.\n")
}

# ============================================================================
# SACHSEN-ANHALT
# ============================================================================
# HYBRID: StaLA primary (00_st_stala_parse.py → st_stala_parsed.csv, 218
# elections 2019-2026, all candidates, both rounds) + Landeswahlleiter portal
# supplements (00_st_scrape.py → st_bm_parsed.csv) for the 5 (ags, date)
# pairs post-2026-02-15 not yet in the StaLA extract, plus the 1 candidate
# (the 8th candidate, Genthin 2024) missing from StaLA due to a corrupted B08/B09
# record in bmbm.csv row 118.
#
# The merge rule mirrors 01_mayoral_unharm.R exactly (see there for the
# rationale). Winner identification: StaLA emits an `is_winner` flag from the
# row-level Elected record, which supersedes the rank-based inference for
# candidates whose surname carries a Dr. title. For portal-supplement rows
# (no is_winner column), we fall back to rank(-votes)=1.

cat("\n=== Processing Sachsen-Anhalt mayoral elections ===\n")

st_stala_file  <- "data/mayoral_elections/raw/sachsen_anhalt/st_stala_parsed.csv"
st_portal_file <- "data/mayoral_elections/raw/sachsen_anhalt/st_bm_parsed.csv"

st_col_classes <- list(character = c("ags", "ags_name", "state",
    "state_name", "election_date", "candidate_party",
    "candidate_name", "candidate_last_name", "candidate_first_name",
    "round"))

if (file.exists(st_stala_file)) {
  st_stala <- fread(st_stala_file, encoding = "UTF-8", colClasses = st_col_classes)

  if (file.exists(st_portal_file)) {
    st_portal <- fread(st_portal_file, encoding = "UTF-8", colClasses = st_col_classes)
    if (!"candidate_title" %in% names(st_portal)) st_portal[, candidate_title := NA_character_]
    if (!"is_winner"       %in% names(st_portal)) st_portal[, is_winner       := NA]
  } else {
    st_portal <- st_stala[0]
  }

  stala_pairs <- unique(st_stala[, .(ags, election_date)])
  st_portal[, candidate_votes := as.numeric(candidate_votes)]
  st_stala[, candidate_votes := as.numeric(candidate_votes)]

  portal_extras <- st_portal[!stala_pairs, on = c("ags", "election_date")]

  # Drop portal "extras" that are really a StaLA election under a wrong date
  # (mirrors 01_mayoral_unharm.R). Fingerprint = round + Gültige + the ordered
  # multiset of candidate votes; the same fingerprint for one AGS at two dates
  # means one election. StaLA dates Zerbst/Anhalt 2026 as 2026-02-08, the portal
  # carries the byte-identical result as 2026-04-12.
  round_fp <- function(dt) {
    x <- dt[!is.na(candidate_votes)][order(ags, election_date, round, candidate_votes)]
    if (!nrow(x)) return(x[, .(ags, election_date, round, fp = character(0))])
    x[, .(fp = paste0(round[1], "~", valid_votes[1], "~",
                      paste(candidate_votes, collapse = "|"))),
      by = .(ags, election_date, round)]
  }
  if (nrow(portal_extras)) {
    dup_keys <- round_fp(portal_extras)[round_fp(st_stala), on = c("ags", "fp"),
                                        nomatch = 0][
      election_date != i.election_date, .(ags, election_date, round)]
    if (nrow(dup_keys)) {
      cat("  Dropping ", nrow(dup_keys),
          " portal round(s) duplicating a StaLA round at another date\n", sep = "")
      portal_extras <- portal_extras[!dup_keys, on = c("ags", "election_date", "round")]
    }
  }

  # Robust portal-supplement rule (mirrors 01_mayoral_unharm.R): a portal row
  # is dropped as a dup of StaLA if StaLA has ANY row in the same
  # (ags, election_date, round) whose last_name OR first_name OR votes match
  # (case-insensitive). Catches a Genthin surname typo (same votes) and a
  # Barleben off-by-one scrape (same name); keeps genuinely missing
  # candidates (the Genthin record with 96 votes).
  st_portal[, .row_id := .I]
  portal_shared <- st_portal[stala_pairs, on = c("ags", "election_date"), nomatch = 0]

  claim_set <- function(dt, col, id_col = NULL) {
    vals <- as.character(.subset2(dt, col))
    keep <- !is.na(vals) & nzchar(trimws(vals))
    if (col == "candidate_votes") {
      vals[keep] <- paste0("V:", vals[keep])
    } else {
      vals[keep] <- paste0("N:", tolower(vals[keep]))
    }
    out <- data.table(ags = dt$ags[keep], election_date = dt$election_date[keep],
                      round = dt$round[keep], claim_key = vals[keep])
    if (!is.null(id_col)) out[, (id_col) := dt[[id_col]][keep]]
    out
  }
  stala_claims <- unique(rbindlist(list(
    claim_set(st_stala, "candidate_last_name"),
    claim_set(st_stala, "candidate_first_name"),
    claim_set(st_stala, "candidate_votes")
  )))
  portal_claims <- rbindlist(list(
    claim_set(portal_shared, "candidate_last_name", ".row_id"),
    claim_set(portal_shared, "candidate_first_name", ".row_id"),
    claim_set(portal_shared, "candidate_votes", ".row_id")
  ))

  matched_ids <- unique(portal_claims[stala_claims,
                                      on = c("ags", "election_date", "round", "claim_key"),
                                      nomatch = 0]$.row_id)
  portal_supp <- portal_shared[!.row_id %in% matched_ids]
  st_portal[, .row_id := NULL]
  portal_supp[, .row_id := NULL]

  st_raw <- rbindlist(list(st_stala, portal_extras, portal_supp),
                      use.names = TRUE, fill = TRUE)

  cat("  Sources merged: StaLA=", nrow(st_stala),
      " rows, portal-only elections=", nrow(portal_extras),
      " rows, portal supplements=", nrow(portal_supp), " rows\n", sep = "")

  st_candidates <- st_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout)
    ) %>%
    # The Gemeinde name MUST be part of the key — KEEP IN SYNC with the twin in
    # 01_mayoral_unharm.R. For a handful of 1994-2001 elections the source's
    # "AGS am Wahltag" carries the POST-merger code, so two then-independent
    # Gemeinden share one AGS on one day (flag_shared_ags). Without ags_name the
    # second Gemeinde's ELECTED MAYOR was ranked against the other Gemeinde's
    # candidates, demoted to a loser, and then stripped of his name by
    # anonymise_losers() — three real mayors erased from the person record
    # (Blaha/Heideloh, Feuerborn/Cosa, Blankau/Dannefeld). The name is used ONLY
    # where the source flags the AGS as shared: the StaLA and portal files spell
    # five Gemeinde names differently, so an unconditional ags_name key would
    # split an election whenever a portal supplement row joined it.
    mutate(.shared_key = ifelse(flag_shared_ags %in% TRUE, ags_name, NA_character_)) %>%
    group_by(ags, .shared_key, election_date, round) %>%
    mutate(
      candidate_rank = rank(-candidate_votes, ties.method = "min", na.last = "keep"),
      n_candidates = n(),
      # Prefer StaLA's explicit winner flag; else rank-based (portal-supp rows).
      is_winner = dplyr::coalesce(as.logical(is_winner), candidate_rank == 1)
    ) %>%
    ungroup() %>%
    mutate(
      # KEEP the register's own gender (8,250 of 9,464 rows carry m/w) and the
      # winner's birth year (2,330 winner rows). Blanking them here threw the
      # gender away and forced 04_*.R to PREDICT it from the first name, which
      # both mislabels people (8 verified disagreements with the register) and
      # leaves 307 ST winners genderless. anonymise_losers() further down
      # still strips both for non-winners, so the licence position is unchanged.
      candidate_gender = recode_gender(candidate_gender),
      candidate_birth_year = ifelse(is_winner %in% TRUE,
                                    suppressWarnings(as.numeric(winner_birth_year)),
                                    NA_real_),
      candidate_profession = NA_character_,
      office_type = ifelse(election_type == "Oberbürgermeisterwahl",
                           "Oberbürgermeister*in", "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner,
      flag_shared_ags
    )

  st_clean <- standardise_candidates(st_candidates)

  cat("Sachsen-Anhalt: Processed", nrow(st_clean), "candidate rows across",
      length(unique(st_clean$election_year)), "years\n")
  all_candidate_data[["sachsen_anhalt"]] <- st_clean
} else {
  cat("Note: ST StaLA data not found at", st_stala_file, "\n")
  cat("  Run 00_st_stala_parse.py first to generate the data.\n")
}

# ============================================================================
# HESSEN
# ============================================================================
# Candidate-level data for HE Bürgermeister-/Oberbürgermeister-/Landratswahlen:
# the FULL 1993-2026 historical series from the HSL file "Direktwahlen in
# Hessen seit 1993" (00_he_hist_parse.py -> he_hist_parsed.csv). Every
# Wahlvorschlag with votes/share/rank + full counts; candidate names are
# redacted by HSL except where the parser grafted them (winner names from the
# B VII m snapshot he_parsed.csv, all-candidate 2026 names from hessenschau;
# grafted Stichwahl winner names are propagated to their Hauptwahl row so the
# wide pivot pairs the rounds by name). Single-candidate rounds are Ja/Nein
# votes (candidate votes < valid_votes). is_winner comes EXPLICITLY from the
# source winner column (exact-tie Ahnatal 2020 broken by Losentscheid there),
# so we do NOT recompute it from ranks. Falls back to the snapshot-only
# he_parsed.csv when the historical intermediate is absent.

cat("\n=== Processing Hessen mayoral/Landrat elections ===\n")

he_file <- "data/mayoral_elections/raw/hessen/he_hist_parsed.csv"
if (!file.exists(he_file)) he_file <- "data/mayoral_elections/raw/hessen/he_parsed.csv"

if (file.exists(he_file)) {
  he_raw <- fread(he_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "ags_name", "state",
                    "state_name", "election_date", "candidate_party",
                    "candidate_name", "candidate_last_name",
                    "candidate_first_name", "candidate_gender", "round",
                    "pair_id", "pair_method", "name_source", "name_withheld",
                    "flag_decisive_round_missing")))
  for (cl in c("pair_id", "name_withheld", "ballot_position",
               "flag_decisive_round_missing")) {
    if (!cl %in% names(he_raw)) he_raw[[cl]] <- NA   # snapshot-only fallback file
  }

  # Names of Hessen LOSING candidates 1993-2012 (Hessami 2018) are not in the
  # public intermediate; 00_he_hist_parse.py writes them to a gitignored side
  # file. Join them here so the wide pivot and the restricted twin see them;
  # `name_restricted` makes the anonymisation step below strip them again from
  # the published file. Without the side file the public pipeline is unchanged.
  he_restricted_file <- "data/mayoral_elections/raw/hessen/he_hist_restricted_names.csv"
  he_raw[, name_restricted := FALSE]
  if (file.exists(he_restricted_file)) {
    he_rn <- fread(he_restricted_file, encoding = "UTF-8",
                   colClasses = list(character = c("ags", "election_date", "round",
                     "candidate_name", "candidate_last_name", "candidate_first_name")))
    he_raw[, ballot_position := as.integer(ballot_position)]
    he_raw[he_rn, on = .(ags, election_date, round, ballot_position),
           `:=`(candidate_name = i.candidate_name,
                candidate_last_name = i.candidate_last_name,
                candidate_first_name = i.candidate_first_name,
                name_restricted = TRUE)]
    stopifnot(sum(he_raw$name_restricted) == nrow(he_rn),
              all(he_raw$name_restricted == (he_raw$name_withheld %in% "TRUE")))
    cat("Hessen: joined", nrow(he_rn), "withheld (restricted-only) candidate names\n")
  }

  he_candidates <- he_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      is_winner = as.logical(is_winner),
      candidate_rank = as.integer(candidate_rank),
      # XLSX (B VII m 2026) lists every Wahlvorschlag -> real count; the 2024-PDF
      # fallback rows (~57 units the XLSX omits) have no n_candidates (NA).
      n_candidates = suppressWarnings(as.integer(n_candidates)),
      candidate_gender = if_else(nzchar(candidate_gender), candidate_gender, NA_character_),
      # Titles stay in candidate_name (verbatim) but not in the split parts; the
      # public snapshots disagree on where "Dr." goes (cleanup, October 2026).
      candidate_last_name = strip_name_titles(candidate_last_name),
      candidate_first_name = strip_name_titles(candidate_first_name),
      pair_key = na_if_blank(pair_id),
      # a sole candidate who failed the Ja/Nein vote seated nobody (Driedorf
      # 2016-03-06, Morschen 2022-03-06; a Neuwahl followed) — see
      # flag_decisive_round_missing in standardise_candidates()
      flag_decisive_round_missing = flag_decisive_round_missing %in% "TRUE",
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        election_type == "Landratswahl" ~ "Landrat/Landrätin",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner,
      pair_key, name_restricted, flag_decisive_round_missing
    )

  he_clean <- standardise_candidates(he_candidates)

  cat("Hessen: Processed", nrow(he_clean), "candidate rows across",
      length(unique(he_clean$election_year)), "years\n")
  all_candidate_data[["hessen"]] <- he_clean
} else {
  cat("Note: HE parsed data not found at", he_file, "\n")
  cat("  Run 00_he_parse.py first to generate the data.\n")
}

# ============================================================================
# BAYERN — Kommunalwahl 2026
# ============================================================================
# Candidate-level data for the 8/22 March 2026 Bayern Kommunalwahl, parsed from
# the Landesamt Mandatsträger XLSX by 00_by_kommunalwahl2026_parse.py into
# by2026_parsed.csv (one row per Wahlvorschlag per round; Kennwort = party, with
# the elected person's name/gender/birth year attached to the winning slot).
# is_winner / candidate_rank / n_candidates come from the parser. As in the
# historical Bayern data, only the winner has a name (HW/SW pair by Kennwort).

cat("\n=== Processing Bayern Kommunalwahl 2026 ===\n")

by26_file <- "data/mayoral_elections/raw/bayern/by2026_parsed.csv"

# Bayern Mandatsträger names are "Lastname Firstname" with no comma, so the
# surname is normally the first token. A nobility name breaks that rule: the
# title and the particle are part of the surname ("Graf von X Firstname",
# "Freifrau A von X Firstname", "Gräfin A-von B Firstname"), and splitting
# after the first token published "Graf" as the surname and "von X Firstname"
# as the given name. When a particle is present the surname therefore runs to
# the token AFTER the last particle, provided a given name is left over and that
# token is not an academic title ("X von Dr. Firstname" is the sorted
# form of "von X" and keeps the particle as the last surname token).
# Names without a particle are split exactly as before.
# Two more cases the first-token rule gets wrong:
#   - an entry written "Nachname, Vorname" (a later by-election, 09171114
#     2026-05-17) is split at the comma;
#   - a two-word surname with no particle or hyphen ("A B Firstname") looks
#     exactly like "Surname Firstname Secondname", so no rule can decide it.
#     Those are pinned, each against a public source, in
#     data/mayoral_elections/metadata/candidate_name_splits_verified.csv, keyed
#     on (ags, verbatim name); a pin that no longer matches the source stops the
#     run instead of going stale silently.
by26_particles <- c("von", "vom", "zu", "zum", "zur", "van", "de", "der", "den")
name_pins_file <- "data/mayoral_elections/metadata/candidate_name_splits_verified.csv"
name_pins <- fread(name_pins_file, encoding = "UTF-8", colClasses = "character")
split_by26_name <- function(x) {
  out <- matrix(NA_character_, length(x), 2)
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    if (grepl(",", x[i], fixed = TRUE)) {
      out[i, ] <- trimws(c(sub(",.*$", "", x[i]), sub("^[^,]*,", "", x[i])))
      next
    }
    toks <- strsplit(trimws(x[i]), "\\s+")[[1]]
    is_p <- tolower(toks) %in% by26_particles |
      grepl("-(von|vom|zu|zum|zur|van|de)$", toks, ignore.case = TRUE)
    p <- max(c(0L, which(is_p)))
    if (p == 0L) {
      out[i, ] <- c(sub("^(\\S+).*$", "\\1", x[i]), trimws(sub("^\\S+\\s*", "", x[i])))
      next
    }
    k <- if (p + 2L <= length(toks) && !grepl("^(Dr|Prof)\\.", toks[p + 1L])) p + 1L else p
    out[i, ] <- c(paste(toks[seq_len(k)], collapse = " "),
                  paste(toks[-seq_len(k)], collapse = " "))
  }
  out
}

if (file.exists(by26_file)) {
  by26_raw <- fread(by26_file, encoding = "UTF-8",
                    colClasses = list(character = c("ags", "ags_name", "state",
                      "state_name", "election_date", "candidate_party",
                      "candidate_name", "round")))
  by_pins <- name_pins[state == "09"]
  stale_pins <- by_pins[!paste(ags, candidate_name) %in% paste(by26_raw$ags, by26_raw$candidate_name)]
  if (nrow(stale_pins) > 0) {
    stop("Bayern 2026 name pin(s) no longer match the source: ",
         paste(stale_pins$ags, collapse = ", "), " (", name_pins_file, ")")
  }

  by26_candidates <- by26_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      candidate_votes = as.numeric(candidate_votes),
      candidate_voteshare = as.numeric(candidate_voteshare),
      candidate_birth_year = as.numeric(candidate_birth_year),
      turnout = as.numeric(turnout),
      is_winner = as.logical(is_winner),
      candidate_rank = as.integer(candidate_rank),
      n_candidates = as.integer(n_candidates),
      candidate_name = if_else(nzchar(candidate_name), candidate_name, NA_character_),
      # Bayern Mandatsträger names are "Lastname Firstname" (no comma); see
      # split_by26_name() above for surnames with a nobility particle.
      candidate_last_name = split_by26_name(candidate_name)[, 1],
      candidate_first_name = split_by26_name(candidate_name)[, 2],
      .pin = match(paste(ags, candidate_name), paste(by_pins$ags, by_pins$candidate_name)),
      candidate_last_name = if_else(!is.na(.pin), by_pins$candidate_last_name[.pin], candidate_last_name),
      candidate_first_name = if_else(!is.na(.pin), by_pins$candidate_first_name[.pin], candidate_first_name),
      candidate_gender = if_else(nzchar(candidate_gender), candidate_gender, NA_character_),
      candidate_party = if_else(nzchar(candidate_party), candidate_party, NA_character_),
      candidate_profession = NA_character_,
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        election_type == "Landratswahl" ~ "Landrat/Landrätin",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  by26_clean <- standardise_candidates(by26_candidates)

  cat("Bayern 2026: Processed", nrow(by26_clean), "candidate rows\n")
  all_candidate_data[["bayern_2026"]] <- by26_clean
} else {
  cat("Note: BY 2026 parsed data not found at", by26_file, "\n")
  cat("  Run 00_by_kommunalwahl2026_parse.py first to generate the data.\n")
}

# ============================================================================
# HESSEN — Kommunalwahl 2026 (15 March, Stichwahl 29 March / 12 April)
# ============================================================================
# Candidate-level data for the 2026 HE direct elections, scraped from the
# hessenschau result pages by 00_he_kommunalwahl2026_scrape.py into
# he2026_parsed.csv. PERCENTAGE-ONLY (like RLP): candidate_voteshare + turnout
# known, no absolute votes (candidate_votes / valid_votes NA). Candidate name +
# party + rank + n_candidates come from the scraper; gender is assigned later in
# 04_candidate_characteristics.R. is_winner is taken EXPLICITLY (decisive round).
# Both Hauptwahl and Stichwahl rounds are kept (paired wide in the next step).
# No Landrat elections in this source (mayoral only).

cat("\n=== Processing Hessen Kommunalwahl 2026 (%-only) ===\n")

he26_file <- "data/mayoral_elections/raw/hessen/he2026_parsed.csv"

if (file.exists(he26_file)) {
  he26_raw <- fread(he26_file, encoding = "UTF-8",
                    colClasses = list(character = c("ags", "ags_name", "state",
                      "state_name", "election_date", "candidate_party",
                      "candidate_name", "candidate_last_name",
                      "candidate_first_name", "round")))

  he26_candidates <- he26_raw %>%
    mutate(
      election_date = as.Date(election_date),
      election_year = as.integer(election_year),
      eligible_voters = NA_real_, number_voters = NA_real_,
      valid_votes = NA_real_, invalid_votes = NA_real_,
      candidate_votes = NA_real_,                    # %-only source
      candidate_voteshare = as.numeric(candidate_voteshare),
      turnout = as.numeric(turnout),
      is_winner = as.logical(is_winner),
      candidate_rank = as.integer(candidate_rank),
      n_candidates = as.integer(n_candidates),
      candidate_gender = NA_character_,              # assigned in 04
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      candidate_party = if_else(nzchar(candidate_party), candidate_party, NA_character_),
      office_type = case_when(
        election_type == "Oberbürgermeisterwahl" ~ "Oberbürgermeister*in",
        TRUE ~ "Bürgermeister*in")
    ) %>%
    select(
      ags, ags_name, state, state_name, election_year, election_date,
      election_type, round, eligible_voters, number_voters, valid_votes,
      invalid_votes, turnout, candidate_name, candidate_last_name,
      candidate_first_name, candidate_gender, candidate_party,
      candidate_votes, candidate_voteshare, candidate_birth_year,
      candidate_profession, office_type, n_candidates, candidate_rank, is_winner
    )

  he26_clean <- standardise_candidates(he26_candidates)

  # The official HSL data (he_hist_parsed, `hessen` block above) carries most
  # 2026 cycles with FULL
  # votes for the Gemeinden it covers. Drop those (ags, election_date) from the
  # hessenschau %-only scrape so each 2026 round comes from a single source; only
  # the rounds the XLSX lacks (the ~16 not yet in the May-2026 report, incl. the
  # Eschwege Stichwahl) remain %-only.
  if (!is.null(all_candidate_data[["hessen"]])) {
    xlsx26 <- all_candidate_data[["hessen"]] %>%
      filter(election_year == 2026) %>% distinct(ags, election_date)
    n_before <- nrow(he26_clean)
    he26_clean <- he26_clean %>% anti_join(xlsx26, by = c("ags", "election_date"))
    cat("Hessen 2026: dropped", n_before - nrow(he26_clean),
        "%-only rows now covered with full votes by the XLSX\n")
  }

  cat("Hessen 2026: Processed", nrow(he26_clean), "candidate rows across",
      n_distinct(he26_clean$ags), "Gemeinden\n")
  all_candidate_data[["hessen_2026"]] <- he26_clean
} else {
  cat("Note: HE 2026 parsed data not found at", he26_file, "\n")
  cat("  Run 00_he_kommunalwahl2026_scrape.py first to generate the data.\n")
}

# ============================================================================
# COMBINE ALL DATA
# ============================================================================

cat("\n=== Combining all candidate data ===\n")

mayoral_candidates <- bind_rows(all_candidate_data) %>%
  arrange(state, ags, election_year, election_date, round, candidate_rank) %>%
  # Ensure consistent column types (standardise_candidates already called per state)
  mutate(
    # fread() leaves RFC-4180 doubled quotes un-collapsed in a quoted field
    # (see 01_mayoral_unharm.R); collapse them before the HW/SW pivot, which may
    # pair rounds by label (audit 2026-10-19)
    candidate_party = gsub('""', '"', candidate_party, fixed = TRUE),
    ags = as.character(ags),
    state = as.character(state),
    election_year = as.numeric(election_year),
    election_date = as.Date(election_date),
    # Only Bayern computes this; default every other state to FALSE (not superseded)
    flag_superseded = dplyr::coalesce(as.logical(flag_superseded), FALSE),
    # Only a Stage-0 parser that knows its delivery is incomplete sets this;
    # for every other source, absence of the flag means "as far as we know the
    # deciding round is here".
    flag_decisive_round_missing =
      dplyr::coalesce(as.logical(flag_decisive_round_missing), FALSE),
    # internal (see standardise_candidates); FALSE for every block that does
    # not set them
    name_restricted = dplyr::coalesce(as.logical(name_restricted), FALSE)
  )

# ============================================================================
# WIDE FORMAT: One row per candidate per election (HW + SW columns)
# ============================================================================

cat("\n=== Restructuring to wide format (HW + SW columns per candidate) ===\n")

hw_rows <- mayoral_candidates %>% filter(round == "hauptwahl")
sw_rows <- mayoral_candidates %>% filter(round == "stichwahl")

cat("  HW candidate rows:", nrow(hw_rows), "\n")
cat("  SW candidate rows:", nrow(sw_rows), "\n")

# --- Step 1: Find HW-SW date pairs ---
# For each (ags, SW date), find the closest preceding HW date.
#   * gap >= 0, not gap > 0: a handful of early-postwar Bayern runoffs were held
#     on the SAME DAY as the first ballot (Würzburg 09663000, 1948-07-09). With
#     gap > 0 they were rejected here and re-entered through the orphan branch as
#     a phantom second election.
#   * gap <= 120, not gap < 60: two Sachsen-Anhalt cycles put 91 days between the
#     rounds, which the 60-day window split into two elections and crowned a
#     never-elected Hauptwahl leader.
# slice_min on (ags, sw_date) keeps the CLOSEST preceding Hauptwahl, so widening
# the window cannot steal a runoff from a nearer first round. The second
# slice_min guarantees at most one Stichwahl per Hauptwahl date, which is what
# makes the candidate-level join below provably one-to-one.
sw_election_dates <- sw_rows %>% distinct(ags, sw_date = election_date)
hw_election_dates <- hw_rows %>% distinct(ags, hw_date = election_date)

date_pairs <- sw_election_dates %>%
  inner_join(hw_election_dates, by = "ags", relationship = "many-to-many") %>%
  mutate(gap = as.numeric(sw_date - hw_date)) %>%
  filter(gap >= 0, gap <= 120) %>%
  group_by(ags, sw_date) %>%
  slice_min(gap, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(ags, hw_date) %>%
  slice_min(sw_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ags, hw_date, sw_date)

cat("  Election cycles with Stichwahl:", nrow(date_pairs), "\n")

# --- Step 1b: One election_type per cycle ---
# election_type is an attribute of the OFFICE, so it cannot differ between the
# two rounds of one election. The Bayern source nevertheless leaves Amtstitel
# blank on some non-decisive rounds, so a Hauptwahl could come out
# "Bürgermeisterwahl" while its own Stichwahl came out "Oberbürgermeisterwahl" —
# splitting one election into two phantom half-elections with a winner each
# (Bayreuth 09462000 2012). Take the Hauptwahl round's type for the whole cycle.
cycle_type <- hw_rows %>%
  count(ags, election_date, election_type, name = ".n") %>%
  group_by(ags, election_date) %>%
  slice_max(.n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(ags, hw_date = election_date, election_type_cycle = election_type)

hw_rows <- hw_rows %>%
  left_join(cycle_type, by = c("ags", "election_date" = "hw_date")) %>%
  mutate(election_type = dplyr::coalesce(election_type_cycle, election_type)) %>%
  select(-election_type_cycle)

# Attach each Stichwahl row to its Hauptwahl date once, here — every downstream
# step (join / SW-only / orphan) keys off this column instead of re-joining.
sw_rows <- sw_rows %>%
  left_join(date_pairs, by = c("ags", "election_date" = "sw_date")) %>%
  left_join(cycle_type, by = c("ags", "hw_date")) %>%
  mutate(election_type = dplyr::coalesce(election_type_cycle, election_type)) %>%
  select(-election_type_cycle)

# --- Step 2: Create candidate match key ---
# Use candidate_name where available, candidate_party otherwise.
# The key is NORMALISED first: cosmetic differences between the two rounds
# otherwise split one person in two (Kiel 01002000 2025 published the runoff
# winner as "Dr. Yilmaz, Samet" and his own first round as "Yilmaz, Samet").
normalise_match_key <- function(x) {
  y <- as.character(x)
  # academic titles ("Dr.", "Dr", "Prof.", "Dr.-Ing.", "Dr. h. c.")
  y <- gsub("(?i)\\b(prof|dr)\\b\\.?\\s*-?\\s*(ing\\b\\.?|habil\\b\\.?|h\\.\\s*c\\.)?",
            " ", y, perl = TRUE)
  y <- gsub("\\s*,\\s*", ", ", y)       # unify comma spacing
  y <- gsub("[[:space:]]+", " ", y)     # collapse internal whitespace
  tolower(trimws(y))
}

add_match_key <- function(df) {
  df %>%
    mutate(
      match_key = case_when(
        # an explicit Stage-0 link wins (Hessen: runoff names are often redacted
        # and several Wahlvorschläge share the label "Einzelbewerbung")
        !is.na(pair_key)        ~ paste0("__pair__", pair_key),
        !is.na(candidate_name)  ~ normalise_match_key(candidate_name),
        !is.na(candidate_party) ~ paste0("__party__", normalise_match_key(candidate_party)),
        TRUE ~ paste0("__rank__", candidate_rank)
      )
    )
}

hw_keyed <- add_match_key(hw_rows)
sw_keyed <- add_match_key(sw_rows)

# --- Step 2b: Make the key UNIQUE inside a cycle ---
# The raw key is not unique within an election: several name-redacted candidates
# routinely share one party label ("Einzelbewerbung", "Parteilos"), and a fully
# unnamed round keys every one of its rows to the same string. Joining on such a
# key fans a single Stichwahl row onto EVERY Hauptwahl row and silently drops the
# surplus Stichwahl rows — e.g. Darmstadt 06411000 1993, where all seven
# Hauptwahl rows came out carrying the same 20,282 runoff votes.
# Rule: a key occurring more than once on EITHER side of a cycle cannot be
# linked. Give those rows a row-unique sentinel so they can never join; the
# Stichwahl results are still published, via the SW-only branch below.
key_counts_hw <- hw_keyed %>%
  count(ags, .cycle_date = election_date, match_key, name = ".n_hw")
key_counts_sw <- sw_keyed %>%
  filter(!is.na(hw_date)) %>%
  count(ags, .cycle_date = hw_date, match_key, name = ".n_sw")

key_ambiguous <- full_join(key_counts_hw, key_counts_sw,
                           by = c("ags", ".cycle_date", "match_key")) %>%
  filter(dplyr::coalesce(.n_hw, 0L) > 1 | dplyr::coalesce(.n_sw, 0L) > 1) %>%
  transmute(ags, .cycle_date, match_key, .ambiguous = TRUE)

hw_keyed <- hw_keyed %>%
  left_join(key_ambiguous, by = c("ags", "election_date" = ".cycle_date", "match_key")) %>%
  mutate(match_key = ifelse(dplyr::coalesce(.ambiguous, FALSE),
                            paste0("__ambhw__", dplyr::row_number()), match_key)) %>%
  select(-.ambiguous)

sw_keyed <- sw_keyed %>%
  left_join(key_ambiguous, by = c("ags", "hw_date" = ".cycle_date", "match_key")) %>%
  mutate(match_key = ifelse(dplyr::coalesce(.ambiguous, FALSE),
                            paste0("__ambsw__", dplyr::row_number()), match_key)) %>%
  select(-.ambiguous)

cat("  Unlinkable (duplicate) candidate keys:",
    sum(grepl("^__amb(hw|sw)__", hw_keyed$match_key)), "HW /",
    sum(grepl("^__amb(hw|sw)__", sw_keyed$match_key)), "SW\n")

# --- Step 2c: Bayern runoffs — pair by elimination when the list label changed ---
# "Wahlen seit 1945" names nobody, so Bayern rounds pair on the Wahlvorschlag
# label, and a list that is relabelled between the rounds splits one person into a
# first-round-only row and a runoff-only winner row: 455 of 3,316 Bayern runoff
# cycles before this step (Krumbach 2008: "Junge Wähler" in the Hauptwahl, "Junge
# Wähler-Offene Liste e.V." in the Stichwahl; both rounds won by one person).
# A Bayern Stichwahl is held between the two leading Hauptwahl candidates, so a
# runoff candidate whose label matches no Hauptwahl row belongs to the top-two
# candidate still unpaired. Rules, in order:
#   * only cycles with exactly two runoff candidates, and only rows not declared
#     unlinkable in Step 2b;
#   * a label that is a clearly better match for a Hauptwahl row OUTSIDE the top
#     two (similarity >= 0.8, top-two candidate < 0.5) goes there instead — the
#     case where a top-two candidate withdrew and the third moved up;
#   * otherwise the single unpaired top-two row is taken if its label is not
#     entirely unrelated (similarity >= 0.2); two unpaired runoff rows are assigned
#     by the better total label similarity, each pair >= 0.3.
# Anything else stays split. Other states have their own pairing (BB
# reconcile_rounds, HE pair_id) and are untouched.
label_sim <- function(a, b) {
  if (is.na(a) || is.na(b)) return(0)
  na <- function(x) gsub("[^a-z0-9]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
  x <- na(a); y <- na(b)
  if (is.na(x) || is.na(y) || !nzchar(x) || !nzchar(y)) return(0)
  if (x == y) return(1)
  s <- 1 - as.numeric(adist(x, y)) / max(nchar(x), nchar(y))
  if (grepl(x, y, fixed = TRUE) || grepl(y, x, fixed = TRUE)) s <- max(s, 0.85)
  # an acronym printed for the full name ("CWG" / "Christliche Wählergemeinschaft"):
  # its letters appear in order in the long label, starting with its first letter
  short <- if (nchar(x) <= nchar(y)) x else y; long <- if (nchar(x) <= nchar(y)) y else x
  raw_short <- if (nchar(x) <= nchar(y)) a else b
  if (nchar(short) >= 2 && nchar(short) <= 6 && grepl("^[[:upper:][:digit:]/]+$", gsub("[^[:alnum:]/]", "", raw_short)) &&
      substr(short, 1, 1) == substr(long, 1, 1) &&
      grepl(paste0("^", paste(strsplit(short, "")[[1]], collapse = ".*")), long)) s <- max(s, 0.7)
  ta <- unique(regmatches(tolower(a), gregexpr("[[:alpha:]]{3,}", tolower(a)))[[1]])
  tb <- unique(regmatches(tolower(b), gregexpr("[[:alpha:]]{3,}", tolower(b)))[[1]])
  if (length(ta) && length(tb)) s <- max(s, length(intersect(ta, tb)) / length(union(ta, tb)))
  s
}
by_sw_cycles <- sw_keyed %>%
  filter(state == "09", !is.na(hw_date)) %>%
  group_by(ags, hw_date) %>% filter(n() <= 2) %>% ungroup()
n_relinked <- 0L
for (cyc in split(by_sw_cycles, paste(by_sw_cycles$ags, by_sw_cycles$hw_date))) {
  a <- cyc$ags[1]; d <- cyc$hw_date[1]
  hw_c <- hw_keyed %>% filter(ags == a, election_date == d)
  if (!nrow(hw_c)) next
  if (any(is.na(hw_c$candidate_votes))) next          # no Hauptwahl ranking to eliminate on
  hw_c <- hw_c %>% mutate(.rk = rank(-candidate_votes, ties.method = "first"))
  # Rows Step 2b declared unlinkable (a label shared by two lists, "Wählergruppe")
  # may take part only inside the top two, where the shared label no longer
  # matters: the runoff is between the top two, whatever the others are called.
  sw_un <- cyc %>% filter(!match_key %in% hw_c$match_key)
  hw_un <- hw_c %>% filter(!match_key %in% cyc$match_key, !grepl("^__amb", match_key) | .rk <= 2)
  if (!nrow(sw_un) || !nrow(hw_un)) next
  top_un <- hw_un %>% filter(.rk <= 2)
  assign_key <- function(sw_key, hw_key) {
    idx <- which(sw_keyed$ags == a & sw_keyed$hw_date %in% d & sw_keyed$match_key == sw_key)
    if (length(idx) == 1) { sw_keyed$match_key[idx] <<- hw_key; n_relinked <<- n_relinked + 1L }
  }
  if (nrow(sw_un) == 1 && nrow(cyc) == 1) {
    # a runoff recorded with its winner only (early postwar): the top-two row whose
    # label clearly matches, if exactly one does
    sims <- vapply(top_un$candidate_party, function(h) label_sim(sw_un$candidate_party[1], h), numeric(1))
    if (sum(sims >= 0.6) == 1) assign_key(sw_un$match_key[1], top_un$match_key[which(sims >= 0.6)])
  } else if (nrow(sw_un) == 1) {
    lab <- sw_un$candidate_party[1]
    sims <- vapply(hw_un$candidate_party, function(h) label_sim(lab, h), numeric(1))
    out_top <- which(hw_un$.rk > 2 & sims >= 0.8 & !grepl("^__amb", hw_un$match_key))
    top_sim <- if (nrow(top_un)) max(sims[hw_un$.rk <= 2]) else 0
    if (length(out_top) == 1 && top_sim < 0.5) {
      assign_key(sw_un$match_key[1], hw_un$match_key[out_top])
    } else if (nrow(top_un) == 1 && top_sim >= 0.2) {
      assign_key(sw_un$match_key[1], top_un$match_key[1])
    }
  } else if (nrow(sw_un) == 2 && nrow(top_un) == 2) {
    s <- outer(sw_un$candidate_party, top_un$candidate_party, Vectorize(label_sim))
    keep <- if (s[1, 1] + s[2, 2] >= s[1, 2] + s[2, 1]) c(1, 2) else c(2, 1)
    pair_s <- c(s[1, keep[1]], s[2, keep[2]])
    # both pairs plausible, or one clearly right and the other forced by elimination
    if ((min(pair_s) >= 0.3 || max(pair_s) >= 0.8) &&
        abs((s[1, 1] + s[2, 2]) - (s[1, 2] + s[2, 1])) >= 0.2) {
      sk <- sw_un$match_key
      assign_key(sk[1], top_un$match_key[keep[1]])
      assign_key(sk[2], top_un$match_key[keep[2]])
    }
  }
}
stopifnot(!any(duplicated(sw_keyed[!is.na(sw_keyed$hw_date), c("ags", "hw_date", "match_key")])))
cat("  Bayern runoff rows paired to their Hauptwahl row by elimination:", n_relinked, "\n")

# --- Step 3: Prepare SW columns for joining ---
sw_to_join <- sw_keyed %>%
  filter(!is.na(hw_date)) %>%
  transmute(
    ags, hw_date, match_key,
    election_date_sw = election_date,
    candidate_votes_sw = candidate_votes,
    candidate_voteshare_sw = candidate_voteshare,
    candidate_rank_sw = candidate_rank,
    is_winner_sw = is_winner,
    n_candidates_sw = n_candidates,
    turnout_sw = turnout,
    flag_superseded_sw = flag_superseded,  # so an annulled Stichwahl flags the cycle
    # Bayern: a runoff row that Step 2c joined to a first-round row under another
    # label can carry the elected person's 2026 Mandatsträger name, which the
    # first-round row lacks (Kolbermoor, Schwandorf 2026); carried over in Step 4.
    # Restricted names never travel this way.
    .sw_by = state == "09" & !(name_restricted %in% TRUE) & !is.na(candidate_last_name),
    .sw_name = if_else(.sw_by, candidate_name, NA_character_),
    .sw_last = if_else(.sw_by, candidate_last_name, NA_character_),
    .sw_first = if_else(.sw_by, candidate_first_name, NA_character_),
    .sw_gender = if_else(.sw_by, candidate_gender, NA_character_),
    .sw_birth = if_else(.sw_by, as.numeric(candidate_birth_year), NA_real_)
  ) %>%
  select(-.sw_by)

# Step 2b is supposed to have made this impossible — assert it rather than
# distinct()-ing the surplus rows away silently, which is what used to happen.
stopifnot(!any(duplicated(sw_to_join[, c("ags", "hw_date", "match_key")])))

# --- Step 4: Build wide dataset ---
# Base = HW candidates, with SW columns joined where available
wide <- hw_keyed %>%
  # Add election_date_sw for elections that have a Stichwahl
  left_join(
    date_pairs %>% select(ags, hw_date, election_date_sw = sw_date),
    by = c("ags", "election_date" = "hw_date")
  ) %>%
  # Join SW candidate results. one-to-one: a fan-out must ERROR, never smear one
  # runoff result across several first-round rows.
  left_join(
    sw_to_join,
    by = c("ags", "election_date" = "hw_date", "match_key"),
    relationship = "one-to-one"
  ) %>%
  # Resolve election_date_sw (from date_pairs or sw_to_join)
  mutate(
    election_date_sw = coalesce(election_date_sw.x, election_date_sw.y),
    has_stichwahl = !is.na(election_date_sw)
  ) %>%
  select(-election_date_sw.x, -election_date_sw.y) %>%
  # Rename HW-specific columns
  rename(
    candidate_votes_hw = candidate_votes,
    candidate_voteshare_hw = candidate_voteshare,
    candidate_rank_hw = candidate_rank,
    n_candidates_hw = n_candidates
  ) %>%
  # Recompute is_winner: if Stichwahl occurred, winner is SW winner
  mutate(
    is_winner = case_when(
      has_stichwahl & !is.na(is_winner_sw) ~ is_winner_sw,
      TRUE ~ is_winner
    ),
    # A cycle is superseded if EITHER its Hauptwahl (HW side) or its (annulled)
    # Stichwahl is flagged. The wide row is keyed on the HW, so without this an
    # annulled Stichwahl folded onto a valid HW would lose the flag.
    flag_superseded = flag_superseded | dplyr::coalesce(flag_superseded_sw, FALSE),
    .fill_sw = state == "09" & !is.na(.sw_last) &
      (is.na(candidate_last_name) | !nzchar(candidate_last_name)),
    candidate_name = if_else(.fill_sw, .sw_name, candidate_name),
    candidate_last_name = if_else(.fill_sw, .sw_last, candidate_last_name),
    candidate_first_name = if_else(.fill_sw, .sw_first, candidate_first_name),
    candidate_gender = if_else(.fill_sw & is.na(candidate_gender), .sw_gender, candidate_gender),
    candidate_birth_year = ifelse(.fill_sw & is.na(candidate_birth_year), .sw_birth, candidate_birth_year)
  )
cat("  Bayern first-round rows named from their linked runoff row:", sum(wide$.fill_sw), "\n")
wide <- wide %>%
  select(-match_key, -is_winner_sw, -round, -flag_superseded_sw, -starts_with(".sw_"), -.fill_sw)

# --- Step 5: Handle SW-only candidates (appear in SW but not HW) ---
# Includes the rows Step 2b declared unlinkable: their runoff result is real and
# must be published (it decides the election), only the link to a specific
# first-round row is unknown.
sw_only <- sw_keyed %>%
  filter(!is.na(hw_date)) %>%
  anti_join(
    hw_keyed,
    by = c("ags", "hw_date" = "election_date", "match_key")
  )

# Election-level counts belong to the HAUPTWAHL, because the row that carries
# them is filed under the Hauptwahl date. Carrying the runoff round's own
# eligible/valid/turnout in those columns made 267 elections report two different
# valid_votes for one election (Isernhagen 03241008, Hildesheim 03254000).
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0) x[1] else NA_real_
}
hw_election_stats <- hw_rows %>%
  group_by(ags, election_date) %>%
  summarise(
    # the cycle's year is the Hauptwahl's: a runoff in January otherwise filed
    # the runoff-only row under the next year (Beselich 06533001 2009/2010)
    hw_election_year   = min(election_year),
    hw_eligible_voters = first_non_na(eligible_voters),
    hw_number_voters   = first_non_na(number_voters),
    hw_valid_votes     = first_non_na(valid_votes),
    hw_invalid_votes   = first_non_na(invalid_votes),
    hw_turnout         = first_non_na(turnout),
    .groups = "drop"
  )

if (nrow(sw_only) > 0) {
  cat("  SW-only candidates (no matching HW row):", nrow(sw_only), "\n")
  # Add these with HW columns = NA
  sw_only_wide <- sw_only %>%
    left_join(hw_election_stats, by = c("ags", "hw_date" = "election_date")) %>%
    mutate(
      election_date_sw = election_date,
      election_date = hw_date,
      election_year = dplyr::coalesce(hw_election_year, election_year),
      has_stichwahl = TRUE,
      candidate_votes_sw = candidate_votes,
      candidate_voteshare_sw = candidate_voteshare,
      candidate_rank_sw = candidate_rank,
      n_candidates_sw = n_candidates,
      turnout_sw = turnout,
      candidate_votes_hw = NA_real_,
      candidate_voteshare_hw = NA_real_,
      candidate_rank_hw = NA_integer_,
      n_candidates_hw = NA_integer_,
      # election-level columns describe the Hauptwahl, not this runoff round
      eligible_voters = hw_eligible_voters,
      number_voters   = hw_number_voters,
      valid_votes     = hw_valid_votes,
      invalid_votes   = hw_invalid_votes,
      turnout         = hw_turnout,
      is_winner = is_winner  # SW winner
    ) %>%
    select(-match_key, -round, -hw_date,
           -candidate_votes, -candidate_voteshare,
           -candidate_rank, -n_candidates, -hw_election_year,
           -hw_eligible_voters, -hw_number_voters, -hw_valid_votes,
           -hw_invalid_votes, -hw_turnout)
  wide <- bind_rows(wide, sw_only_wide)
} else {
  cat("  No SW-only candidates\n")
}

# --- Step 6: Handle SW rows without matching HW date (orphaned SW) ---
sw_orphaned <- sw_keyed %>% filter(is.na(hw_date))

if (nrow(sw_orphaned) > 0) {
  cat("  WARNING: Orphaned SW rows (no matching HW date):", nrow(sw_orphaned), "\n")
  # These are Stichwahl elections where no first-round data exists at all, so the
  # runoff IS the election as far as this dataset is concerned: the row is filed
  # under the Stichwahl date and keeps that round's own election-level counts.
  # (Contrast Step 5, where an SW-only row joins an existing Hauptwahl-dated
  # election and therefore has to adopt the Hauptwahl's counts.)
  orphaned_wide <- sw_orphaned %>%
    mutate(
      election_date_sw = election_date,
      has_stichwahl = TRUE,
      candidate_votes_sw = candidate_votes,
      candidate_voteshare_sw = candidate_voteshare,
      candidate_rank_sw = candidate_rank,
      n_candidates_sw = n_candidates,
      turnout_sw = turnout,
      candidate_votes_hw = NA_real_,
      candidate_voteshare_hw = NA_real_,
      candidate_rank_hw = NA_integer_,
      n_candidates_hw = NA_integer_,
      is_winner = is_winner
    ) %>%
    select(-match_key, -round, -hw_date,
           -candidate_votes, -candidate_voteshare,
           -candidate_rank, -n_candidates)
  wide <- bind_rows(wide, orphaned_wide)
} else {
  cat("  No orphaned SW rows\n")
}

mayoral_candidates <- wide %>%
  arrange(state, ags, election_year, election_date, candidate_rank_hw) %>%
  # Final column order
  select(
    ags, ags_name, state, state_name, election_year, election_date,
    election_date_sw, election_type, has_stichwahl,
    eligible_voters, number_voters, valid_votes, invalid_votes, turnout,
    turnout_sw,
    candidate_name, candidate_last_name, candidate_first_name,
    candidate_gender, candidate_party,
    candidate_votes_hw, candidate_voteshare_hw, candidate_rank_hw,
    n_candidates_hw,
    candidate_votes_sw, candidate_voteshare_sw, candidate_rank_sw,
    n_candidates_sw,
    is_winner, flag_superseded, flag_shared_ags, flag_decisive_round_missing,
    candidate_birth_year, candidate_profession, office_type,
    name_restricted   # internal; dropped after the anonymisation below
  )

# Enforce exactly ONE is_winner per election. The HW<->SW candidate matching can
# still be unresolvable for name-redacted candidates that share a party label
# (e.g. multiple "Einzelbewerber"/"Parteilos"), which would otherwise produce
# several is_winner = TRUE per election.
#
# ORDER OF AUTHORITY — the source flag comes FIRST. Several Stage-0 parsers know
# the winner from the source's own "Gewählte/r" column (ST :2135, HE :2199,
# BY-2026 :2259, HE-2026 :2326) and that knowledge beats any vote arithmetic we
# can do here: it settles the Ahnatal 06633001 2020 exact tie (2106:2106,
# decided by Losentscheid) and the 3 Sachsen-Anhalt rounds where the recorded
# winner is not the recorded top-vote candidate. Recomputing unconditionally
# also silently crowned the FIRST PHYSICAL ROW of the 329 elections whose
# deciding-round metric is entirely missing (which.max over all -Inf returns 1).
# So: trust exactly-one source flag; recompute when there are none or several;
# and where there is neither a flag nor a usable metric, say so with NA instead
# of inventing a mayor.
#
# flag_decisive_round_missing outranks even the source flag, because it is a
# statement ABOUT the source: the round that decided this cycle is not in it.
# Such a cycle has votes, so `.has_metric` is TRUE and the recompute below would
# happily crown the leader of a round that seated nobody — which is how the
# Hagenow 2015 Hauptwahl (CDU on 41.5 %, beaten in a Stichwahl we do not hold)
# put the losing candidate into mayor_panel as that town's mayor.
mayoral_candidates <- as_tibble(mayoral_candidates) %>%
  mutate(.rid = dplyr::row_number()) %>%                       # global unique row id
  # group by election_type too: an AGS+date may host BOTH a Bürgermeister and a
  # Landrat election (combined election day) — they are separate elections.
  group_by(ags, election_date, election_type) %>%
  mutate(
    .any_sw = any(!is.na(candidate_votes_sw)) | any(!is.na(candidate_voteshare_sw)),
    .metric = dplyr::coalesce(
      ifelse(.any_sw, candidate_votes_sw, candidate_votes_hw),
      ifelse(.any_sw, candidate_voteshare_sw, candidate_voteshare_hw)
    ),
    .n_src_win = sum(is_winner, na.rm = TRUE),   # winners the SOURCE declared
    .has_metric = any(!is.na(.metric)),
    .sole = n() == 1,                            # only one candidate stood
    # the deciding round of this cycle is not in the source at all
    .no_decisive = any(flag_decisive_round_missing %in% TRUE),
    .win_rid = .rid[which.max(ifelse(is.na(.metric), -Inf, .metric))]  # global id of the group max
  ) %>%
  ungroup() %>%
  mutate(
    is_winner = case_when(
      .no_decisive    ~ NA,                                 # nobody was elected in the rounds we hold
      .n_src_win == 1 ~ dplyr::coalesce(is_winner, FALSE),  # source knows: keep it
      .has_metric     ~ .rid == .win_rid,                   # 0 or >1 flags: most votes
      # A sole candidate won by definition, whether or not the early-postwar
      # Bayern records give a vote count. Without this, 538 uncontested
      # elections would carry no winner at all while mayoral_unharm names one.
      .sole           ~ TRUE,
      TRUE            ~ NA                                  # unknowable — do not guess
    )
  ) %>%
  select(-.rid, -.any_sw, -.metric, -.n_src_win, -.has_metric, -.sole,
         -.no_decisive, -.win_rid)

# Safety net: guarantee at least one winner per election (covers rare edge cases
# where the deciding-round metric is degenerate). Flags the highest-vote row.
# Skips elections whose winner is deliberately NA above — re-crowning a row there
# would reintroduce exactly the arbitrary first-row winner we just removed.
mayoral_candidates <- mayoral_candidates %>%
  group_by(ags, election_date, election_type) %>%
  mutate(
    .nw = sum(is_winner, na.rm = TRUE),
    .unknown = all(is.na(is_winner)),
    .topvotes = dplyr::coalesce(candidate_votes_sw, candidate_votes_hw,
                                candidate_voteshare_sw, candidate_voteshare_hw, -Inf),
    is_winner = ifelse(!.unknown & .nw == 0 & dplyr::row_number() == which.max(.topvotes),
                       TRUE, is_winner)
  ) %>%
  ungroup() %>%
  select(-.nw, -.unknown, -.topvotes)

cat("  Elections with no determinable winner (is_winner = NA):",
    n_distinct(paste(mayoral_candidates$ags, mayoral_candidates$election_date,
                     mayoral_candidates$election_type)[is.na(mayoral_candidates$is_winner)]),
    "\n")

# flag_superseded is an ELECTION-level attribute, so make it constant within each
# (ags, election_date, election_type): a candidate that appears only in a folded
# Stichwahl (sw_only / orphaned-SW branch above) would otherwise carry the SW row's
# flag instead of the election's. any() also folds in the annulled-Stichwahl flag.
mayoral_candidates <- mayoral_candidates %>%
  group_by(ags, election_date, election_type) %>%
  mutate(flag_superseded = any(flag_superseded, na.rm = TRUE),
         # likewise election-level, and NA (no Stage-0 parser said anything)
         # means the delivery was complete as far as we know
         flag_decisive_round_missing = any(flag_decisive_round_missing,
                                           na.rm = TRUE)) %>%
  ungroup()

cat("\n  Wide format: ", nrow(mayoral_candidates), "rows ×",
    ncol(mayoral_candidates), "columns\n")
cat("  flag_superseded TRUE rows:", sum(mayoral_candidates$flag_superseded), "\n")
cat("  flag_decisive_round_missing TRUE rows:",
    sum(mayoral_candidates$flag_decisive_round_missing), "\n")

# ============================================================================
# BAYERN — winner names from the printed Bürgermeisterverzeichnisse 1978-2014
# ============================================================================
# "Wahlen seit 1945" names nobody. The LfStat library scanned the seven printed
# registers (raw/bayern/lfstat_verzeichnisse/, see its README), and
# 00_by_verzeichnis_parse.py matched every register entry to its round in that
# workbook on (Kreis, Wahltag, winner's share, Gemeinde name). It writes one row per
# source round whose winner the registers name: the round an entry reports, plus
# every other round the workbook assigns to the same (ags, first Amtsantritt) person
# key, provided no register entry contradicts that key and the round's Amtsantritt
# is possible for its date. The fill is strictly ADDITIVE and touches only the
# winner row of a Bayern election with no name yet, so the HW/SW pairing above is
# unchanged. Gender is derived from the first name in 04.
# The names, birth years and professions reach the RESTRICTED twin only: the
# LfStat declined permission to redistribute them (16.10.2026; § 9 Abs. 3 of its
# AGB bans electronic redistribution of Verzeichnisse, and it sees no legal basis
# for publishing the personal data elsewhere). `name_restricted` marks the rows.
by_reg_file <- "data/mayoral_elections/raw/bayern/by_verzeichnis_elections.csv"
if (file.exists(by_reg_file)) {
  by_reg <- fread(by_reg_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "candidate_name",
                                                  "candidate_last_name",
                                                  "candidate_first_name",
                                                  "candidate_profession")))
  blank_na <- function(x) if_else(nzchar(trimws(x)), x, NA_character_)
  by_reg <- by_reg %>%
    as_tibble() %>%
    transmute(ags,
              .decisive = as.Date(election_date),
              .reg_office = office,
              .reg_name = blank_na(candidate_name),
              .reg_last = blank_na(candidate_last_name),
              .reg_first = blank_na(candidate_first_name),
              .reg_birth = suppressWarnings(as.numeric(candidate_birth_year)),
              .reg_prof = blank_na(candidate_profession),
              .reg_wv = blank_na(workbook_winner_wv)) %>%
    filter(!is.na(.reg_last))
  # The workbook's winning list for that round must be the list of the row we name:
  # otherwise a tie-break or a HW/SW label split put another list's row on top
  # (Sulzdorf a.d.L. 2002, a 437:437 runoff; Bergtheim 2008).
  .wv_norm <- function(x) tolower(gsub("[^[:alnum:]]", "", iconv(x, to = "ASCII//TRANSLIT")))
  if (anyDuplicated(by_reg[c("ags", ".decisive")])) {
    stop("by_verzeichnis_elections.csv is not unique on (ags, election_date)")
  }
  .office_type_of <- c(BM = "Bürgermeisterwahl", OB = "Oberbürgermeisterwahl",
                       LR = "Landratswahl")
  before <- sum(mayoral_candidates$state == "09" &
                  mayoral_candidates$is_winner %in% TRUE &
                  !is.na(mayoral_candidates$candidate_last_name))
  mayoral_candidates <- mayoral_candidates %>%
    mutate(.decisive = coalesce(election_date_sw, election_date)) %>%
    left_join(by_reg, by = c("ags", ".decisive")) %>%
    mutate(
      # equal, or clearly the same list relabelled between the rounds (the row
      # carries its Hauptwahl label, the workbook names the Stichwahl label:
      # Krumbach 2008 "Junge Wähler" / "Junge Wähler-Offene Liste e.V.")
      .wv_ok = is.na(.reg_wv) | is.na(candidate_party) |
        .wv_norm(.reg_wv) == .wv_norm(candidate_party) |
        mapply(function(a, b) label_sim(a, b) >= 0.5, .reg_wv, candidate_party),
      .fill = state == "09" & is_winner %in% TRUE & !is.na(.reg_last) &
        (is.na(candidate_last_name) | !nzchar(candidate_last_name)) &
        unname(.office_type_of[.reg_office]) == election_type &
        !(flag_superseded %in% TRUE) & .wv_ok,
      candidate_name       = if_else(.fill, .reg_name, candidate_name),
      candidate_last_name  = if_else(.fill, .reg_last, candidate_last_name),
      candidate_first_name = if_else(.fill, .reg_first, candidate_first_name),
      candidate_birth_year = if_else(.fill & is.na(candidate_birth_year),
                                     .reg_birth, candidate_birth_year),
      candidate_profession = if_else(.fill & is.na(candidate_profession),
                                     .reg_prof, candidate_profession),
      # restricted twin only, winners included (LfStat, 16.10.2026; see the
      # anonymisation step below)
      name_restricted = name_restricted | .fill
    )
  n_type_clash <- sum(mayoral_candidates$state == "09" &
                        mayoral_candidates$is_winner %in% TRUE &
                        !is.na(mayoral_candidates$.reg_last) &
                        unname(.office_type_of[mayoral_candidates$.reg_office]) !=
                          mayoral_candidates$election_type, na.rm = TRUE)
  cat("\n=== Bayern register names (Bürgermeisterverzeichnisse 1978-2014) ===\n")
  cat("  Bayern winners named before:", before, "\n")
  cat("  filled from the registers:  ", sum(mayoral_candidates$.fill), "\n")
  cat("  register rows whose office disagrees with election_type (skipped):",
      n_type_clash, "\n")
  cat("  skipped: winner row on another list than the workbook's winner:",
      sum(mayoral_candidates$state == "09" & mayoral_candidates$is_winner %in% TRUE &
            !is.na(mayoral_candidates$.reg_last) & !mayoral_candidates$.wv_ok), "\n")
  mayoral_candidates <- mayoral_candidates %>%
    select(-.decisive, -starts_with(".reg_"), -.fill, -.wv_ok)
} else {
  cat("\n(Bayern register names skipped: ", by_reg_file, " not found)\n")
}

# ---- Bayern candidate lists (B VII 3-2), 2008 / 2014 / 2020 / 2026 ----------
# 00_by_bewerber_parse.py assigns every printed candidate of the main Kommunalwahl
# (Gemeinden > 10 000 inhabitants, and Landrat) to a Wahlvorschlag of the same
# election in the source workbook, by label, with the counts required to agree.
# Here the name is joined to the Hauptwahl row of that Wahlvorschlag. Additive:
# a row that already has a name (register or 2026 Mandatsträger) keeps it.
# Every name from these lists, the elected persons' included, reaches only the
# restricted twin (LfStat refusal of 16.10.2026, as for the registers above).
# The file itself is gitignored.
by_bew_file <- "data/mayoral_elections/raw/bayern/by_bewerber_parsed_restricted.csv"
if (file.exists(by_bew_file)) {
  blank_na <- function(x) if_else(nzchar(trimws(x)), x, NA_character_)
  by_bew <- fread(by_bew_file, encoding = "UTF-8",
                  colClasses = list(character = c("ags", "kennwort", "candidate_name",
                                                  "candidate_last_name", "candidate_first_name",
                                                  "candidate_profession"))) %>%
    as_tibble() %>%
    transmute(ags, .hw = as.Date(election_date), .kw = str_squish(kennwort),
              .kv = suppressWarnings(as.numeric(kennwort_votes)),
              .b_name = blank_na(candidate_name), .b_last = blank_na(candidate_last_name),
              .b_first = blank_na(candidate_first_name), .b_prof = blank_na(candidate_profession)) %>%
    filter(!is.na(.b_last))
  if (anyDuplicated(by_bew[c("ags", ".hw", ".kw", ".kv")])) {
    stop("by_bewerber_parsed_restricted.csv is not unique on (ags, date, Kennwort, votes)")
  }
  # Kennwort + Hauptwahl votes: a label alone repeats within a few elections.
  mayoral_candidates <- mayoral_candidates %>%
    mutate(.kw = str_squish(candidate_party), .kv = candidate_votes_hw) %>%
    left_join(by_bew, by = c("ags", "election_date" = ".hw", ".kw", ".kv")) %>%
    mutate(
      .fill = state == "09" & !is.na(.b_last) & !is.na(candidate_votes_hw) &
        !(flag_superseded %in% TRUE) &
        (is.na(candidate_last_name) | !nzchar(candidate_last_name)),
      candidate_name       = if_else(.fill, .b_name, candidate_name),
      candidate_last_name  = if_else(.fill, .b_last, candidate_last_name),
      candidate_first_name = if_else(.fill, .b_first, candidate_first_name),
      candidate_profession = if_else(.fill & is.na(candidate_profession), .b_prof,
                                     candidate_profession),
      name_restricted = name_restricted | .fill   # winners included (LfStat, 16.10.2026)
    )
  cat("\n=== Bayern candidate lists (B VII 3-2, 2008/2014/2020/2026) ===\n")
  cat("  printed candidates:", nrow(by_bew), "; joined to a Hauptwahl row:",
      sum(!is.na(mayoral_candidates$.b_last) & !is.na(mayoral_candidates$candidate_votes_hw)), "\n")
  cat("  names filled:", sum(mayoral_candidates$.fill), "(winners",
      sum(mayoral_candidates$.fill & mayoral_candidates$is_winner %in% TRUE),
      "; all restricted)\n")
  mayoral_candidates <- mayoral_candidates %>% select(-.kw, -.kv, -starts_with(".b_"), -.fill)
} else {
  cat("\n(Bayern candidate lists skipped: ", by_bew_file, " not found)\n")
}


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== Summary ===\n")
cat("Total candidate rows:", nrow(mayoral_candidates), "\n")
cat("Total elections:", n_distinct(paste(mayoral_candidates$ags,
                                          mayoral_candidates$election_date)), "\n")

cat("\nBy state:\n")
mayoral_candidates %>%
  group_by(state_name) %>%
  summarise(
    candidate_rows = n(),
    elections = n_distinct(paste(ags, election_date)),
    with_stichwahl = sum(has_stichwahl & !is.na(candidate_votes_sw)),
    has_name = sum(!is.na(candidate_name)),
    has_gender = sum(!is.na(candidate_gender)),
    .groups = "drop"
  ) %>%
  print(n = 20)

cat("\nStichwahl coverage:\n")
cat("  Elections with Stichwahl:",
    n_distinct(paste(mayoral_candidates$ags[mayoral_candidates$has_stichwahl],
                     mayoral_candidates$election_date[mayoral_candidates$has_stichwahl])), "\n")
cat("  Candidates with SW data:", sum(!is.na(mayoral_candidates$candidate_votes_sw)), "\n")

cat("\nCandidate name availability:\n")
cat("  With name:", sum(!is.na(mayoral_candidates$candidate_name)), "\n")
cat("  Without name:", sum(is.na(mayoral_candidates$candidate_name)), "\n")

# ============================================================================
# SPLIT MAYORAL VS LANDRAT
# ============================================================================
# Landrat (county/Städteregion head) elections live in their own dataset.

mayoral_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl",
                   "VG-Bürgermeisterwahl", "SG-Bürgermeisterwahl")

# A Landratswahl is a Kreis-level office, so its AGS must end in "000". Rows
# typed Landratswahl on a municipal AGS are source mislabels (Hannover 2013 is
# the Landeshauptstadt's Oberbürgermeisterwahl; Fahrenzhausen 1945 carries a
# stray "Landrat/Landrätin" Amtstitel in the Bayern file). Mirrors the guard in
# 01_mayoral_unharm.R — see the fuller note there.
lr_misfiled <- mayoral_candidates$election_type == "Landratswahl" &
  !grepl("000$", mayoral_candidates$ags)
if (any(lr_misfiled)) {
  muni_office <- mayoral_candidates %>%
    filter(election_type %in% c("Bürgermeisterwahl", "Oberbürgermeisterwahl")) %>%
    distinct(ags, election_type) %>%
    group_by(ags) %>% slice(1) %>% ungroup() %>%
    rename(inferred_type = election_type)
  fixed <- mayoral_candidates[lr_misfiled, ] %>%
    left_join(muni_office, by = "ags") %>%
    mutate(election_type = coalesce(inferred_type, "Bürgermeisterwahl")) %>%
    select(-inferred_type)
  cat("\nReclassified", sum(lr_misfiled),
      "Landratswahl candidate row(s) on a municipal AGS\n")
  mayoral_candidates <- bind_rows(mayoral_candidates[!lr_misfiled, ], fixed)
}

# These flags are scoped to the mayoral datasets; the Landrat dataset has its
# own downstream combine pipeline, so drop the columns from the Landrat split to
# keep landrat_candidates byte-identical to before.
landrat_candidates <- mayoral_candidates %>% filter(election_type == "Landratswahl") %>%
  select(-flag_superseded, -flag_shared_ags, -flag_decisive_round_missing)
# Hessami names no Landräte; the only restricted Landrat names here are the
# Bayern ones from the LfStat registers and candidate lists (stripped below,
# after the restricted twin is written; name_restricted is dropped there).
stopifnot(!any(landrat_candidates$name_restricted & landrat_candidates$state != "09"))
mayoral_candidates <- mayoral_candidates %>% filter(election_type %in% mayoral_types)

# Brandenburg (October 2026): cycles in which the Gemeindevertretung chose the
# mayor (after a direct election that seated nobody, or a former Amtsdirektor at
# the 2001-2003 Gemeindegebietsreform; 00_bb_afs_reports_parse.py). They stay
# flag_decisive_round_missing (no directly elected winner); this flag says that
# someone WAS seated, so 03 ends the previous mayor's term there. Same column as
# in landrat_candidates; FALSE everywhere else.
# Since 2026-10-16 also the 2004-2013 result reports (00_bb_lwl_scans_parse.py):
# a runoff leader below the 15 % quorum, after which the council elects.
# Since 2026-10-19 also the archived result pages (00_bb_lwl_archive_parse.py),
# whose replaced B VII 3-6 rounds are dropped first.
bb_council_files <- c(bb_afs_file, bb_scan_file, bb_arc_file)[file.exists(c(bb_afs_file, bb_scan_file, bb_arc_file))]
bb_council <- if (length(bb_council_files)) {
  bind_rows(lapply(bb_council_files, function(f) {
    d <- fread(f, encoding = "UTF-8", colClasses = "character")
    if (f == bb_afs_file) bb_drop_superseded(d) else d
  })) %>%
    as_tibble() %>%
    filter(elected_by_council %in% "TRUE") %>%
    distinct(ags, election_date = as.Date(election_date)) %>%
    mutate(.council = TRUE)
} else tibble(ags = character(), election_date = as.Date(character()), .council = logical())
mayoral_candidates <- mayoral_candidates %>%
  left_join(bb_council, by = c("ags", "election_date")) %>%
  mutate(flag_elected_by_council = coalesce(.council, FALSE)) %>%
  select(-.council) %>%
  relocate(flag_elected_by_council, .after = flag_decisive_round_missing)
stopifnot(n_distinct(paste(mayoral_candidates$ags, mayoral_candidates$election_date)[
            mayoral_candidates$flag_elected_by_council]) == nrow(bb_council),
          all(mayoral_candidates$flag_decisive_round_missing[mayoral_candidates$flag_elected_by_council]))
cat("  flag_elected_by_council: ", nrow(bb_council), " Brandenburg cycles\n", sep = "")

cat("\nDataset split:\n")
cat("  mayoral_candidates:", nrow(mayoral_candidates), "rows\n")
cat("  landrat_candidates:", nrow(landrat_candidates), "rows\n")

# ============================================================================
# ANONYMISATION — Sachsen-Anhalt (state 15) and Thüringen (16)
# ============================================================================
# The Statistisches Landesamt Sachsen-Anhalt supplies its Bürgermeisterwahlen
# data with full candidate names for SCIENTIFIC USE ONLY. Only anonymised data
# may be redistributed or published, "analog zu den bayerischen Daten" — i.e.
# the Bayern model, where the elected person is named and losing candidates
# carry no personal data whatsoever. § 80 KWO LSA independently bars publishing
# Wahlvorschlag (candidate) data more than six months after the final result.
#
# We therefore strip every personal / name-derived field from ST NON-WINNER
# rows. Vote counts, shares, ranks and the Wahlvorschlagsträger (party) are NOT
# personal data and are kept, so the electoral analysis is unaffected. Winners
# are public office-holders and keep their names, exactly as in Bayern.
#
# This must run AFTER the wide HW/SW pivot, which pairs the two rounds by
# candidate name. Downstream, 04_candidate_characteristics.R derives gender and
# name-origin FROM the name, so stripping here also keeps those NA for losers
# (matching Bayern, whose losers are NA throughout).
#
# Thüringen (16) follows the same rule (decision of 2026-10-03). Its Landesamt
# names every candidate of a recent election but withdraws the losers' names
# after a period (§ 50 Abs. 2 ThürKWO: "nicht mehr möglich"), naming only the
# elected person from then on. The official OB and Landrat files name losers
# too. GERDA publishes the elected person and keeps the others in the
# restricted twin.
#
# Bayern (09) since 2026-10-07: "Wahlen seit 1945" names nobody, and the only
# source of losing candidates' names is the LfStat candidate lists (B VII 3-2,
# 00_by_bewerber_parse.py). User decision: elected persons public, everyone
# else in the restricted twin only. Since 16.10.2026 the elected persons' names
# from the LfStat registers and lists are withheld as well (LfStat refusal); only
# the 2026 Mandatsträger names stay public.
#
# Brandenburg (12) since 2026-10-19 (user decisions 2026-10-16 and 2026-10-19):
# every losing candidate, whatever the source. The B VII 3-6 reports and the
# 2004-2013 result reports were restricted from the start (name_restricted);
# the Landeswahlleiter workbook of 31.07.2026, supplied for research under
# § 25 BbgDSG, its portal fallback and the Landrat portal pages now follow. The
# raw workbook, its parsed CSVs and the Landrat pages are gitignored; tracked
# name-stripped copies (bb_lwl_parsed_public.csv, bb_landrat_parsed_public.csv)
# rebuild the public files in a clone without them.
withheld_loser_states <- c("09", "12", "15", "16")   # Bayern, Brandenburg, Sachsen-Anhalt, Thüringen
st_personal_cols <- c("candidate_name", "candidate_last_name",
                      "candidate_first_name", "candidate_title",
                      "candidate_gender", "candidate_birth_year",
                      "candidate_profession")

anonymise_losers <- function(df, label) {
  if (!all(c("ags", "is_winner") %in% names(df)) || !nrow(df)) return(df)
  idx <- substr(as.character(df$ags), 1, 2) %in% withheld_loser_states & !(df$is_winner %in% TRUE)
  n_before <- sum(idx & !is.na(df$candidate_last_name) &
                    nzchar(trimws(as.character(df$candidate_last_name))))
  for (cl in intersect(st_personal_cols, names(df))) {
    df[[cl]][idx] <- NA
  }
  cat("  ", label, ": anonymised ", sum(idx), " BY/BB/ST/TH non-winner rows (",
      n_before, " carried a name)\n", sep = "")
  df
}

# ---- RESTRICTED twin (scientific use only) ---------------------------------
# The anonymisation is destructive and in-place, so the un-anonymised frame
# exists only here. Persist it first, into a gitignored sibling directory, so
# the project's own analyses can use the full ST candidate record while the
# published files stay licence-compliant. `write_restricted_candidates()` is
# defined identically in code/landrat_elections/01_landrat_combine.R — KEEP THE
# TWO COPIES IN SYNC (that script rewrites landrat_candidates after this one).
# Never redistribute anything under a final_restricted/ directory.
write_restricted_candidates <- function(df, dir, stem) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_rds(df, file.path(dir, paste0(stem, ".rds")), compress = "gz")
  fwrite(df, file.path(dir, paste0(stem, ".csv")))
  cat("  ", file.path(dir, stem), ".{rds,csv}: ", nrow(df), " rows (",
      sum(substr(as.character(df$ags), 1, 2) %in% withheld_loser_states &
            !(df$is_winner %in% TRUE) & !is.na(df$candidate_last_name)),
      " named ST/TH non-winners retained)\n", sep = "")
}

# ---- Hessen: names published for elected persons only ----------------------
# Hessami (2018, CC0) names every candidate 1993-2012. Losing candidates are
# private individuals, so GERDA publishes the elected person's name and keeps the
# others in the restricted twin, as for Sachsen-Anhalt (decision of 2026-10-01).
# `name_restricted` marks the rows whose name came only from that source; it is
# set in the HE block above and never published.
# Brandenburg (October 2026): the losing candidates named in the Statistische
# Berichte B VII 3-6 (1993 sheets, OB races 1998-2003, rejected single
# candidates) are flagged the same way by 00_bb_afs_reports_parse.py, and so
# are the losing Bewerber of the 2004-2013 result reports from the
# Landeswahlleiter's archive (00_bb_lwl_scans_parse.py).
# Thüringen (12.10.2026): every name from the TLS Kommunalwahl reports 2000/2004/2010
# is withheld, the ELECTED persons' too (TLS consent only "unter Löschung
# sämtlicher personenbezogenen Daten", § 50 Abs. 2 ThürKWO; see the Thüringen
# block above).
# Bayern (16.10.2026): every name, birth year and profession from the printed
# Bürgermeisterverzeichnisse 1978-2014 and the candidate lists B VII 3-2 / 3-7 is
# withheld, the ELECTED persons' too. The LfStat declined permission to
# redistribute them (§ 9 Abs. 3 of its AGB: no electronic redistribution of
# Verzeichnisse; publishing the personal data is reserved to the Landesamt by the
# GLKrWO). The 2026 Mandatsträger names come from another file and are not
# flagged. The restricted twin keeps all of them.
withheld_all_states <- c("09", "16")   # restricted-source names withheld for winners too
anonymise_restricted_names <- function(df, label) {
  st2 <- substr(as.character(df$ags), 1, 2)
  idx <- df$name_restricted %in% TRUE & (!(df$is_winner %in% TRUE) | st2 %in% withheld_all_states)
  for (cl in intersect(st_personal_cols, names(df))) df[[cl]][idx] <- NA
  cat("  ", label, ": withheld ", sum(idx), " restricted-source names (Hessen ",
      "non-winners named by Hessami 2018; Brandenburg B VII 3-6 and 2004-2013 result-report non-winners: ",
      sum(idx & st2 == "12"),
      "; Thüringen TLS reports 2000/2004/2010, winners included: ", sum(idx & st2 == "16"),
      "; Bayern LfStat registers and candidate lists, winners included: ", sum(idx & st2 == "09"),
      ")\n", sep = "")
  df
}
stopifnot(!any(mayoral_candidates$name_restricted & mayoral_candidates$is_winner %in% TRUE &
                 !substr(as.character(mayoral_candidates$ags), 1, 2) %in% withheld_all_states))

# Party family of the Wahlvorschlag (code/shared/party_family.R; TODO M22,
# user decision 2026-10-19). Computed here, on the full frame, so that the
# restricted twin and the public file carry the same family: an own-name label
# ("Muster", Rheinland-Pfalz) needs the candidate's name.
source("code/shared/party_family.R")
for (.nm in c("mayoral_candidates", "landrat_candidates")) {
  .df <- get(.nm)
  .df$candidate_party_family <- party_family(.df$candidate_party, substr(as.character(.df$ags), 1, 2),
                                             .df$candidate_last_name, .df$candidate_first_name)
  .df <- .df %>% relocate(candidate_party_family, .after = candidate_party)
  cat("  ", .nm, ": candidate_party_family\n", sep = "")
  print(table(.df$candidate_party_family, useNA = "ifany"))
  assign(.nm, .df)
}
# A first round that went to a runoff carries its leader in winner_party,
# winner_votes and winner_voteshare, as ten states' sources do (user decision
# 2026-10-29), in mayoral_unharm and landrat_unharm alike. Fill the fields the
# others leave NA (Niedersachsen, Hessen, Baden-Württemberg; Sachsen's party)
# from the candidate table's unique rank-1
# candidate, and only where the row's other winner fields agree with that
# candidate. A tied lead, or a winner-only table (one candidate with
# first-round figures), stays NA.
fill_runoff_leader <- function(rounds, cands, label) {
  lead <- cands %>%
    filter(!is.na(election_date_sw)) %>%
    group_by(ags, election_date, election_type) %>%
    filter(sum(!is.na(candidate_votes_hw) | !is.na(candidate_voteshare_hw)) >= 2,
           candidate_rank_hw %in% 1L) %>%
    filter(n() == 1) %>% ungroup() %>%
    transmute(ags, election_date, election_type, .lp = candidate_party,
              .lv = candidate_votes_hw, .ls = candidate_voteshare_hw)
  out <- rounds %>%
    left_join(lead, by = c("ags", "election_date", "election_type"), relationship = "many-to-one") %>%
    mutate(.hw = round == "hauptwahl" & (!is.na(.lv) | !is.na(.ls)),
           .ok = .hw & (is.na(winner_votes) | winner_votes %in% .lv) &
                       (is.na(winner_party) | is.na(.lp) | winner_party %in% .lp),
           .n = .ok & ((is.na(winner_party) & !is.na(.lp)) | (is.na(winner_votes) & !is.na(.lv)) |
                       (is.na(winner_voteshare) & !is.na(.ls))),
           winner_party = if_else(.ok & is.na(winner_party), .lp, winner_party),
           winner_votes = if_else(.ok & is.na(winner_votes), .lv, winner_votes),
           winner_voteshare = if_else(.ok & is.na(winner_voteshare), .ls, winner_voteshare))
  cat("  ", label, ": runoff first-round leader filled in ", sum(out$.n), " rounds\n", sep = "")
  print(table(out$state[out$.n]))
  out %>% select(-.lp, -.lv, -.ls, -.hw, -.ok, -.n)
}
# The same for the election-level files 01_mayoral_unharm.R wrote before the
# candidates existed: party_family of winner_party, the own-name rule applied
# through the candidate of the round who carries the label. 02_mayoral_harm.R
# carries it on to mayoral_harm, 01_landrat_combine.R to its own landrat_unharm
# rows.
for (.u in list(c("data/mayoral_elections/final/mayoral_unharm", "mayoral_candidates"),
                c("data/landrat_elections/final/landrat_unharm", "landrat_candidates"))) {
  .ud <- readRDS(paste0(.u[1], ".rds"))
  .ud <- fill_runoff_leader(.ud, get(.u[2]), basename(.u[1]))
  .ud$party_family <- party_family_rounds(.ud, get(.u[2]))
  .ud <- .ud %>% relocate(party_family, .after = winner_party)
  write_rds(.ud, paste0(.u[1], ".rds"), compress = "gz")
  fwrite(.ud, paste0(.u[1], ".csv"))
  cat("  ", basename(.u[1]), ": party_family written (", sum(!is.na(.ud$party_family)), " of ",
      nrow(.ud), " rounds)\n", sep = "")
}

cat("\n=== Writing RESTRICTED (un-anonymised) candidate twins ===\n")
write_restricted_candidates(mayoral_candidates %>% select(-name_restricted),
                            "data/mayoral_elections/final_restricted",
                            "mayoral_candidates_restricted")
write_restricted_candidates(landrat_candidates %>% select(-name_restricted),
                            "data/landrat_elections/final_restricted",
                            "landrat_candidates_restricted")

# Brandenburg: a losing candidate's own-name Wahlvorschlag label ("EB Muster",
# invented) becomes the plain list type in the public files; the twin above keeps
# it (code/shared/bb_own_name_label.R, user decision 2026-10-19).
source("code/shared/bb_own_name_label.R")
for (.nm in c("mayoral_candidates", "landrat_candidates")) {
  .df <- get(.nm)
  .i <- which(.df$state %in% "12" & !(.df$is_winner %in% TRUE))
  .new <- bb_generic_own_label(.df$candidate_party[.i], .df$candidate_last_name[.i],
                               .df$candidate_first_name[.i])
  cat("  ", .nm, ": ", sum(.new != .df$candidate_party[.i], na.rm = TRUE),
      " own-name labels of Brandenburg losing candidates made generic\n", sep = "")
  .df$candidate_party[.i] <- .new
  assign(.nm, .df)
}

cat("\n=== Anonymising Bayern, Brandenburg, Sachsen-Anhalt and Thüringen losing candidates ===\n")
mayoral_candidates <- anonymise_losers(mayoral_candidates, "mayoral_candidates")
landrat_candidates <- anonymise_losers(landrat_candidates, "landrat_candidates")
mayoral_candidates <- anonymise_restricted_names(mayoral_candidates, "mayoral_candidates") %>%
  select(-name_restricted)
landrat_candidates <- anonymise_restricted_names(landrat_candidates, "landrat_candidates") %>%
  select(-name_restricted)
# No Bayern name before 2026 is public: before the 2026 Mandatsträger file, the
# LfStat registers and candidate lists are the only source naming anyone.
for (.df in list(mayoral_candidates, landrat_candidates)) {
  stopifnot(!any(.df$state == "09" & .df$election_date < as.Date("2026-01-01") &
                   (!is.na(.df$candidate_last_name) | !is.na(.df$candidate_name) |
                      !is.na(.df$candidate_birth_year) | !is.na(.df$candidate_profession))))
}

# ---- Brandenburg: tracked, name-stripped copy of the workbook input ---------
# bb_lwl_parsed.csv (00_bb_lwl_parse.py) names every Bewerber of 2010-2026 and,
# like the workbook it is parsed from, is gitignored (user decision 2026-10-19).
# A clone without it builds Brandenburg from bb_lwl_parsed_public.csv, written
# here from the full file once the public file above is final:
#   * the same rows, in the same order, every value as the full file prints it;
#   * a name stays only where the public file shows it -- the elected person,
#     matched on Gemeinde, round date and the pivot's own normalised name key
#     (Step 2); every other Bewerber's name and title are blanked, and an
#     own-name Wahlvorschlag label of theirs is made generic, as in the public file;
#   * every row with a name gets an opaque pair_key ("c1", "c2", ...), unique to
#     that name within its Hauptwahl-Stichwahl cycle (Step 1: the closest
#     Hauptwahl 0-120 days before a Stichwahl), so the pivot pairs the rounds
#     exactly as it does by name. The key is local to one cycle and links nobody
#     across elections.
# Written only from the full file, so a clone never overwrites the copy.
# 99_audit.R §27m checks that it names nobody the public file does not.
if (basename(bb_file) == "bb_lwl_parsed.csv") {
  bb_full <- read.csv(bb_file, colClasses = "character", na.strings = character(0),
                      check.names = FALSE, encoding = "UTF-8")
  if ("pair_key" %in% names(bb_full)) {
    stop("bb_lwl_parsed.csv carries a pair_key column; the public copy would overwrite it")
  }
  bb_pub_named <- mayoral_candidates %>% filter(state == "12", !is.na(candidate_name))
  bb_shown <- bind_rows(
      bb_pub_named %>% transmute(ags, x_d = election_date, x_k = normalise_match_key(candidate_name)),
      bb_pub_named %>% filter(!is.na(election_date_sw)) %>%
        transmute(ags, x_d = election_date_sw, x_k = normalise_match_key(candidate_name))) %>%
    distinct() %>%
    mutate(x_shown = TRUE)
  bb_full <- bb_full %>%
    mutate(x_row = row_number(), x_d = as.Date(election_date),
           x_k = if_else(nzchar(trimws(candidate_name)), normalise_match_key(candidate_name),
                        NA_character_)) %>%
    left_join(bb_shown, by = c("ags", "x_d", "x_k"))
  bb_cycle <- bb_full %>% filter(round == "stichwahl") %>% distinct(ags, x_sw = x_d) %>%
    inner_join(bb_full %>% filter(round == "hauptwahl") %>% distinct(ags, x_hw = x_d),
               by = "ags", relationship = "many-to-many") %>%
    filter(as.numeric(x_sw - x_hw) >= 0, as.numeric(x_sw - x_hw) <= 120) %>%
    group_by(ags, x_sw) %>%
    slice_min(as.numeric(x_sw - x_hw), n = 1, with_ties = FALSE) %>%
    ungroup()
  bb_full <- bb_full %>%
    left_join(bb_cycle, by = c("ags", "x_d" = "x_sw")) %>%
    mutate(x_cycle = if_else(round == "stichwahl" & !is.na(x_hw), x_hw, x_d)) %>%
    group_by(ags, x_cycle) %>%
    mutate(pair_key = if_else(is.na(x_k), "",
                              paste0("c", match(x_k, unique(x_k[!is.na(x_k)]))))) %>%
    ungroup() %>%
    arrange(x_row)
  stopifnot(nrow(bb_full) == max(bb_full$x_row))
  .keep <- bb_full$x_shown %in% TRUE
  # a losing candidate's own-name label is made generic, as in the public file
  .gen <- bb_generic_own_label(bb_full$candidate_party, bb_full$candidate_last_name,
                               bb_full$candidate_first_name)
  bb_full$candidate_party[!.keep] <- .gen[!.keep]
  for (cl in intersect(c("candidate_name", "candidate_last_name", "candidate_first_name",
                         "candidate_title"), names(bb_full))) {
    bb_full[[cl]][!.keep] <- ""
  }
  bb_public <- bb_full %>% select(-x_row, -x_d, -x_k, -x_shown, -x_hw, -x_cycle)
  # the copy names nobody the public file does not, and no elected person is lost
  stopifnot(sum(nzchar(bb_public$candidate_name)) == sum(.keep),
            all(paste(bb_full$ags[.keep], bb_full$x_k[.keep]) %in%
                  paste(bb_pub_named$ags, normalise_match_key(bb_pub_named$candidate_name))))
  fwrite(bb_public, bb_public_file)
  cat("\nBrandenburg public input:", nrow(bb_public), "rows ->", bb_public_file, "(",
      sum(.keep), "named rows of elected persons;", sum(!.keep & !is.na(bb_full$x_k)),
      "names blanked )\n")
}

# ============================================================================
# SAVE DATA
# ============================================================================

cat("\n=== Saving data ===\n")

# Mayoral output
write_rds(mayoral_candidates, "data/mayoral_elections/final/mayoral_candidates.rds", compress = "gz")
fwrite(mayoral_candidates, "data/mayoral_elections/final/mayoral_candidates.csv")

# Landrat output (new standalone dataset)
dir.create("data/landrat_elections/final", recursive = TRUE, showWarnings = FALSE)
write_rds(landrat_candidates, "data/landrat_elections/final/landrat_candidates.rds", compress = "gz")
fwrite(landrat_candidates, "data/landrat_elections/final/landrat_candidates.csv")

cat("Data saved to:\n")
cat("  - data/mayoral_elections/final/mayoral_candidates.rds\n")
cat("  - data/mayoral_elections/final/mayoral_candidates.csv\n")
cat("  - data/landrat_elections/final/landrat_candidates.rds\n")
cat("  - data/landrat_elections/final/landrat_candidates.csv\n")

cat("\n=== Done ===\n")
