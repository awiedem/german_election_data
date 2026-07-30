### Harmonize mayoral election results to 2021 borders
# Vincent Heddesheimer
# First: March 02, 2026
#
# Mayoral elections differ fundamentally from party-vote elections (federal,
# state). Each row has ONE winner (party, votes, voteshare) rather than vote
# totals for multiple parties. This means:
#
# - Grouping by (ags_21, election_date, round) preserves runoff elections as
#   separate rows AND handles non-synchronized elections from different
#   predecessor municipalities naturally (they remain as separate rows). `round`
#   is part of the key because three Bayern cycles carry a Hauptwahl and a
#   Stichwahl on the SAME date (a source defect, see section 2).
# - For N:1 mergers with same-date elections (e.g. Bayern synchronized
#   elections): aggregate numeric counts via population-weighted sum, take the
#   winner (party, votes, voteshare) from the largest predecessor by population.
#   Winner votes are NOT summed — predecessors can have different winners. Where
#   they do (flag_multi_winner == 1) the winner columns describe the dominant
#   predecessor alone, so winner_votes / valid_votes != winner_voteshare there:
#   the counts cover the whole 2021 municipality, the winner does not.
# - For 1:N splits: duplicate the election result to each successor
#   municipality, weight-split the counts.
# - Rheinland-Pfalz has percentage-only data (no absolute counts) — turnout
#   and winner_voteshare use weighted-mean fallback.
#
# Election types excluded from harmonization:
# - VG-Bürgermeisterwahl (RLP) — uses VG pseudo-AGS, not municipality AGS
# - SG-Bürgermeisterwahl (NI) — uses Samtgemeinde AGS, not in muni crosswalk
# - Landratswahl (NI/RLP) — uses county-level AGS, not in muni crosswalk
#
# Crosswalk coverage:
# - 1990-2020: full annual coverage via ags_crosswalks.csv
# - 2021+: identity mapping (already in 2021 boundaries)
# - Pre-1990: uses 1990 crosswalk as fallback (flagged)

rm(list = ls())
gc()

conflicts_prefer(dplyr::filter)

options(scipen = 999)


# 1. Load data -------------------------------------------------------------

df <- read_rds("data/mayoral_elections/final/mayoral_unharm.rds") |>
  as_tibble()

cat("Loaded mayoral_unharm:", nrow(df), "rows\n")
cat("States:", paste(sort(unique(df$state)), collapse = ", "), "\n")
cat("Year range:", min(df$election_year), "-", max(df$election_year), "\n")
table(df$election_type, useNA = "ifany")


# 2. Filter election types -------------------------------------------------

# Only Bürgermeisterwahl and Oberbürgermeisterwahl can be harmonized —
# VG/SG/Landrat elections use pseudo-AGS codes that cannot map through the
# municipality crosswalk.
harmonizable_types <- c("Bürgermeisterwahl", "Oberbürgermeisterwahl")

df_excluded <- df |> filter(!election_type %in% harmonizable_types)
df <- df |> filter(election_type %in% harmonizable_types)

cat("\nHarmonizable rows:", nrow(df), "\n")
cat("Excluded rows (VG/SG/Landrat):", nrow(df_excluded), "\n")
if (nrow(df_excluded) > 0) {
  cat("Excluded by type:\n")
  print(table(df_excluded$election_type))
}

# Deduplicate: remove any exact duplicates (e.g. Bayern 1948).
# `round` MUST be part of the key: three Bayern cycles (Sulzbach 1948-05-24,
# Seukendorf 2022-07-10, Lohr a.Main 1948-07-06) carry a Hauptwahl and a
# Stichwahl on the same date in the source. Without `round` the two rows are
# treated as duplicates and the surviving one is arbitrary — for Seukendorf
# 2022 that deleted the real Hauptwahl (CSU 637/1,669) and kept a spurious
# Stichwahl row instead (audit 2026-07, M31/F100).
n_before <- nrow(df)
df <- df |> distinct(ags, election_date, election_type, round, .keep_all = TRUE)
if (nrow(df) < n_before) {
  cat("Removed", n_before - nrow(df), "duplicate rows\n")
}

same_date_rounds <- df |>
  group_by(ags, election_date, election_type) |>
  filter(n_distinct(round) > 1) |>
  ungroup()
if (nrow(same_date_rounds) > 0) {
  cat("\nNOTE:", nrow(same_date_rounds),
      "rows have a Hauptwahl and a Stichwahl on the same date (source defect);",
      "they are kept as separate rounds:\n")
  print(same_date_rounds |>
          select(ags, ags_name, election_date, round, winner_party,
                 valid_votes, winner_votes) |>
          arrange(ags, election_date, round))
}


# 3. Load crosswalks -------------------------------------------------------

cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_21 = pad_zero_conditional(ags_21, 7)
  )

# Crosswalk year range
cat("\nCrosswalk year range:", min(cw$year), "-", max(cw$year), "\n")

# Assign crosswalk lookup year
df <- df |>
  mutate(
    cw_year = case_when(
      election_year >= 2021 ~ NA_integer_,    # identity mapping
      election_year >= 1990 ~ election_year,   # direct crosswalk lookup
      election_year < 1990  ~ 1990L            # pre-1990 fallback
    ),
    flag_pre_1990 = as.integer(election_year < 1990)
  )

cat("Pre-1990 rows (using 1990 fallback):", sum(df$flag_pre_1990), "\n")
cat("Post-2020 rows (identity mapping):", sum(is.na(df$cw_year)), "\n")


# 4. Handle post-2020 data (identity mapping) ------------------------------

# AGS codes that legitimately cannot be mapped to 2021 boundaries, or whose
# defect is owned by a Stage 1 script. The project convention is a HARD STOP on
# unmatched / invalid AGS — this allowlist exists only so that already-diagnosed
# cases do not block the pipeline while their upstream fixes land. Anything not
# listed here is an error. Remove entries as the upstream fixes arrive.
# (audit 2026-07, M29 + M31/F103/F185 — 191 rows / 96 AGS were dropped silently
# and 13 invalid ags_21 codes were emitted.)
unmatched_allowlist <- c(
  # (a) PERMANENT — Bayern Gemeinden dissolved in the 1970s territorial reform
  #     and re-established later; their pre-1990 elections carry codes that do
  #     not exist in ANY crosswalk year from 1990 onwards. 33 rows.
  "09187186", "09374170", "09674223", "09771176", "09777183",
  # (b) Bayern LANDRAT elections mis-typed as Bürgermeisterwahl in Stage 1
  #     (Landkreis AGS 09KKK000, not municipalities). 120 unmatched rows plus
  #     10 post-2020 rows. Remove once 01_mayoral_unharm.R classifies untitled
  #     Landkreis rows as Landratswahl (they are then filtered in section 2).
  "09172000", "09173000", "09174000", "09175000", "09176000", "09177000",
  "09178000", "09179000", "09180000", "09181000", "09182000", "09183000",
  "09184000", "09185000", "09186000", "09187000", "09188000", "09189000",
  "09190000", "09272000", "09273000", "09274000", "09275000", "09276000",
  "09277000", "09278000", "09371000", "09372000", "09373000", "09375000",
  "09376000", "09377000", "09471000", "09472000", "09473000", "09474000",
  "09475000", "09476000", "09477000", "09478000", "09571000", "09572000",
  "09573000", "09574000", "09575000", "09576000", "09671000", "09672000",
  "09673000", "09674000", "09675000", "09676000", "09677000", "09679000",
  "09771000", "09774000", "09776000", "09777000", "09778000", "09779000",
  "09780000",
  #     ... and two Landkreise that leak only through the post-2020 identity
  #     path (2022/2024 Landratswahlen), never through the crosswalk:
  "09773000", "09775000",
  # (c) Niedersachsen 2013: AGS wrong at source (post-2013 codes on 2013 rows).
  "03153022", "03153023", "03154403", "03155024", "03256403", "03350007",
  "03353403", "03355401", "03451020", "03455008", "03456404", "03461401",
  # (d) Region Hannover / Aachen: a county-level body and a defunct city code,
  #     both wrong at source.
  "03241000",  # NI: Region Hannover (not a municipality)
  "05313000",  # NRW: Aachen, defunct since 21.10.2009 (true 05334002)
  # (e) Schleswig-Holstein 2025: wrong / stale codes at source.
  "01055019",  # true Heiligenhafen is 01055021
  "01059027",  # true Glücksburg is 01059113
  "01059033",  # stale pre-merger Handewitt, true 01059183
  # (f) Sachsen-Anhalt: the election year falls in a crosswalk gap for these
  #     historical Gemeinden (merged away before / re-coded after the lookup
  #     year); 15 rows, would need a nearest-available-year crosswalk.
  "15082241", "15083025", "15151011", "15151015", "15154003", "15159001",
  "15159010", "15159029", "15159030", "15370045", "15370058", "15370070",
  "15370111", "15370113", "15370116",
  # (g) Thüringen: Eisenach's 2018 election is recorded under its post-2021
  #     code 16063105 (kreisfrei 16056000 until the 2021 Kreisreform).
  "16063105"
)

# All valid 2021 municipality codes. The identity rule below copies `ags`
# verbatim, so without this check any AGS created AFTER 2021 (or simply wrong
# at source) is emitted as a bogus "2021" code (audit 2026-07, M29).
ags_2021_universe <- sort(unique(cw$ags_21))
cat("2021 municipality universe:", length(ags_2021_universe), "codes\n")

# Back-map for genuine post-2021 merger codes: 2021 -> 2023 -> 2025 composed and
# inverted, keeping the largest predecessor by population. Only codes that do
# not exist in 2021 can ever be rewritten by this map. As in 03_mayor_panel.R we
# take the dominant predecessor rather than splitting the result across all of
# them: a mayoral row has a single winner, and splitting would invent an
# election in every predecessor municipality.
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
  filter(!is.na(ags_bad), !(ags_bad %in% ags_2021_universe)) |>
  group_by(ags_bad) |>
  slice_max(population, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ags_bad, ags_pre)

df_post2020 <- df |>
  filter(is.na(cw_year)) |>
  mutate(
    ags_21 = ags,
    pop_cw = 1,
    population = NA_real_,
    flag_unsuccessful_naive_merge = 0L
  ) |>
  left_join(post2021_backmap, by = c("ags_21" = "ags_bad")) |>
  mutate(
    flag_post2021_backmap = as.integer(!is.na(ags_pre)),
    ags_21 = coalesce(ags_pre, ags_21)
  ) |>
  select(-ags_pre)

if (sum(df_post2020$flag_post2021_backmap) > 0) {
  cat("Back-mapped", sum(df_post2020$flag_post2021_backmap),
      "post-2021 merger codes onto their dominant 2021 predecessor:\n")
  print(as.data.frame(
    df_post2020 |>
      filter(flag_post2021_backmap == 1) |>
      count(ags, ags_name, election_year, ags_21)
  ))
}

# Post-2020 rows keep whatever AGS the source gave them, so this is the only
# place an invalid 2021 code can enter the output.
invalid_post2020 <- df_post2020 |> filter(!(ags_21 %in% ags_2021_universe))
if (nrow(invalid_post2020) > 0) {
  cat("\nags_21 codes outside the 2021 municipality universe:\n")
  print(as.data.frame(
    invalid_post2020 |> count(ags, ags_name, state, election_year) |> arrange(ags)
  ))
  unexpected_ags21 <- setdiff(unique(invalid_post2020$ags_21), unmatched_allowlist)
  if (length(unexpected_ags21) > 0) {
    stop("Post-2020 rows carry ags_21 codes that do not exist in 2021 and are ",
         "not allowlisted: ", paste(unexpected_ags21, collapse = ", "),
         ". Fix the AGS at source (Stage 1), extend the back-map, or add a ",
         "documented entry to `unmatched_allowlist`.")
  }
  warning(sprintf(
    "%d post-2020 rows carry an ags_21 outside the 2021 universe; all are on the documented allowlist.",
    nrow(invalid_post2020)))
}

df_pre2021 <- df |> filter(!is.na(cw_year))

cat("\nPost-2020:", nrow(df_post2020), "rows (identity)\n")
cat("Pre-2021:", nrow(df_pre2021), "rows (need crosswalk)\n")


# 5. Naive merge with crosswalk --------------------------------------------

df_cw_naive <- df_pre2021 |>
  left_join(
    cw |> select(ags, year, ags_21, ags_name_21, pop_cw, population),
    by = c("ags", "cw_year" = "year"),
    relationship = "many-to-many"
  )

not_merged_naive <- df_cw_naive |>
  filter(is.na(ags_21)) |>
  select(ags, election_year, cw_year) |>
  distinct() |>
  mutate(id = paste0(ags, "_", election_year))

cat("\nUnsuccessful naive merges:", nrow(not_merged_naive),
    "unique (ags, election_year) pairs\n")


# 6. Handle unsuccessful merges --------------------------------------------

# Strategy: try year - 1 as fallback, then year + 1
# (Some municipalities retain old AGS one year after boundary change)
if (nrow(not_merged_naive) > 0) {
  df_matched <- df_cw_naive |> filter(!is.na(ags_21))
  df_unmatched <- df_cw_naive |>
    filter(is.na(ags_21)) |>
    select(-ags_21, -ags_name_21, -pop_cw, -population)

  # Try year - 1
  df_try_minus1 <- df_unmatched |>
    mutate(cw_year_try = as.integer(pmax(cw_year - 1L, 1990L))) |>
    left_join(
      cw |> select(ags, year, ags_21, ags_name_21, pop_cw, population),
      by = c("ags", "cw_year_try" = "year")
    )

  df_fixed_minus1 <- df_try_minus1 |> filter(!is.na(ags_21))
  df_still_unmatched <- df_try_minus1 |>
    filter(is.na(ags_21)) |>
    select(-ags_21, -ags_name_21, -pop_cw, -population, -cw_year_try)

  # Try year + 1 for remaining
  if (nrow(df_still_unmatched) > 0) {
    df_try_plus1 <- df_still_unmatched |>
      mutate(cw_year_try = as.integer(pmin(cw_year + 1L, 2020L))) |>
      left_join(
        cw |> select(ags, year, ags_21, ags_name_21, pop_cw, population),
        by = c("ags", "cw_year_try" = "year")
      )
    df_fixed_plus1 <- df_try_plus1 |> filter(!is.na(ags_21))
    df_final_unmatched <- df_try_plus1 |> filter(is.na(ags_21))
  } else {
    df_fixed_plus1 <- df_still_unmatched[0, ]
    df_final_unmatched <- df_still_unmatched[0, ]
  }

  # Combine all matched rows
  df_cw <- bind_rows(
    df_matched,
    df_fixed_minus1 |> select(-cw_year_try),
    df_fixed_plus1 |> select(-cw_year_try)
  )

  cat("Fixed via year-1 fallback:", nrow(df_fixed_minus1), "rows\n")
  cat("Fixed via year+1 fallback:", nrow(df_fixed_plus1), "rows\n")
  cat("Still unmatched:", nrow(df_final_unmatched), "rows\n")

  if (nrow(df_final_unmatched) > 0) {
    still_unmatched_summary <- df_final_unmatched |>
      select(ags, ags_name, state, election_year) |>
      distinct() |>
      arrange(ags, election_year)
    cat("\nUnmatched AGS codes (no 2021 mapping, will be dropped):\n")
    print(as.data.frame(still_unmatched_summary))

    unexpected <- setdiff(unique(df_final_unmatched$ags), unmatched_allowlist)
    if (length(unexpected) > 0) {
      stop("Unmatched AGS with no crosswalk mapping and no allowlist entry: ",
           paste(unexpected, collapse = ", "),
           ". Fix the AGS at source (Stage 1) or add a documented entry to ",
           "`unmatched_allowlist` — AGS must never be dropped silently.")
    }
    warning(sprintf(
      paste("%d rows (%d AGS) have no 2021 mapping and are dropped;",
            "all are on the documented allowlist in 02_mayoral_harm.R."),
      nrow(df_final_unmatched), n_distinct(df_final_unmatched$ags)))
  }
} else {
  df_cw <- df_cw_naive
  df_final_unmatched <- df_cw_naive[0, ]
}

# Flag rows that required a fallback
df_cw <- df_cw |>
  mutate(
    id = paste0(ags, "_", election_year),
    flag_unsuccessful_naive_merge = as.integer(id %in% not_merged_naive$id)
  ) |>
  select(-id)


# 7. Combine pre-2021 and post-2020 data -----------------------------------

# Ensure consistent columns before binding
common_cols <- c("ags", "ags_name", "state", "state_name",
                 "election_year", "election_date", "election_type", "round",
                 "eligible_voters", "number_voters", "valid_votes",
                 "invalid_votes", "turnout", "winner_party",
                 "winner_votes", "winner_voteshare",
                 "cw_year", "flag_pre_1990",
                 "ags_21", "pop_cw", "population",
                 "flag_unsuccessful_naive_merge",
                 # Bayern annulled / failed-and-repeated rounds. Dropping this
                 # made 82 superseded rounds indistinguishable from decisive
                 # ones in the harmonized file (audit 2026-07, M31/F104/F179).
                 "flag_superseded")

# Add missing columns to post-2020 if needed
for (col in setdiff(common_cols, names(df_post2020))) {
  df_post2020[[col]] <- NA
}

df_all <- bind_rows(
  df_cw |> select(any_of(common_cols)),
  df_post2020 |> select(any_of(common_cols))
)

cat("\nTotal rows before aggregation:", nrow(df_all), "\n")

# Drop rows with no ags_21 mapping
n_dropped <- sum(is.na(df_all$ags_21))
cat("Dropping", n_dropped, "rows with no ags_21 mapping\n")
df_all <- df_all |> filter(!is.na(ags_21))

cat("Total rows after dropping unmatched:", nrow(df_all), "\n")


# 8. Aggregation -----------------------------------------------------------

# Weight for selecting the dominant predecessor
df_all <- df_all |>
  mutate(weight = pop_cw * coalesce(population, 1))

# 8a. Aggregate numeric count columns via weighted sum.
# winner_votes is deliberately NOT in this list: predecessor municipalities of
# the same 2021 municipality can have DIFFERENT winners, so summing their winner
# votes produced a chimera (617 multi-predecessor groups, 181 of them mixing
# more than one winner_party; e.g. 03157009 in 2006 reported the SPD as winner
# with 4,406 votes = SPD 2,186 + an Einzelbewerber who actually polled more —
# audit 2026-07, M31/F186). It is taken from the dominant predecessor in 8b.
count_cols <- c("eligible_voters", "number_voters", "valid_votes",
                "invalid_votes")

df_counts <- df_all |>
  group_by(ags_21, election_date, round) |>
  summarise(
    across(
      all_of(count_cols),
      ~ if (all(is.na(.x))) NA_real_ else sum(.x * pop_cw, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(across(all_of(count_cols), ~ round(.x, digits = 0)))

# 8b. Pick categorical variables AND the winner metrics from the dominant
# predecessor. winner_votes carries the same pop_cw weight as the aggregated
# counts so that a 1:N split stays internally consistent;
# winner_valid_votes is that predecessor's own denominator (used for the
# voteshare below) and is dropped again before the output.
df_categorical <- df_all |>
  group_by(ags_21, election_date, round) |>
  slice_max(weight, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(ags_21, election_date, round,
            winner_party, election_type, election_year,
            state, state_name, ags_name,
            winner_votes = round(winner_votes * pop_cw, digits = 0),
            winner_valid_votes = round(valid_votes * pop_cw, digits = 0),
            winner_voteshare_dom = winner_voteshare)

# 8c. Compute weighted means of share columns (fallback for RLP)
df_pct <- df_all |>
  group_by(ags_21, election_date, round) |>
  summarise(
    turnout_wmean = if (all(is.na(turnout))) NA_real_
                    else weighted.mean(turnout, w = weight, na.rm = TRUE),
    voteshare_wmean = if (all(is.na(winner_voteshare))) NA_real_
                      else weighted.mean(winner_voteshare, w = weight, na.rm = TRUE),
    .groups = "drop"
  )

# 8d. Track flags and predecessor count per group
df_flags <- df_all |>
  group_by(ags_21, election_date, round) |>
  summarise(
    flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE),
    flag_pre_1990 = max(flag_pre_1990, na.rm = TRUE),
    # TRUE if ANY predecessor's round was annulled / superseded (Bayern only)
    flag_superseded = any(flag_superseded, na.rm = TRUE),
    # TRUE where predecessors of this 2021 municipality had different winners —
    # the winner columns then describe the dominant predecessor only.
    flag_multi_winner = as.integer(
      n_distinct(winner_party[!is.na(winner_party)]) > 1
    ),
    n_predecessors = n(),
    .groups = "drop"
  )

# 8e. Combine everything
join_keys <- c("ags_21", "election_date", "round")
df_harm <- df_counts |>
  left_join(df_categorical, by = join_keys) |>
  left_join(df_pct, by = join_keys) |>
  left_join(df_flags, by = join_keys) |>
  mutate(
    # Recompute turnout from aggregated counts; fall back to weighted mean
    turnout = case_when(
      !is.na(number_voters) & !is.na(eligible_voters) & eligible_voters > 0 ~
        number_voters / eligible_voters,
      TRUE ~ turnout_wmean
    ),
    # Winner voteshare is the DOMINANT predecessor's own share: its winner_votes
    # over its own valid votes. Dividing the dominant winner's votes by the
    # aggregated valid votes of all predecessors would understate the share.
    # For single-predecessor groups this is identical to the old computation.
    winner_voteshare = case_when(
      !is.na(winner_votes) & !is.na(winner_valid_votes) & winner_valid_votes > 0 ~
        winner_votes / winner_valid_votes,
      !is.na(winner_voteshare_dom) ~ winner_voteshare_dom,
      TRUE ~ voteshare_wmean
    )
  ) |>
  select(-turnout_wmean, -voteshare_wmean,
         -winner_valid_votes, -winner_voteshare_dom) |>
  rename(ags = ags_21)


# 9. Quality flags ---------------------------------------------------------

df_harm <- df_harm |>
  mutate(
    flag_aggregated = as.integer(n_predecessors > 1),
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    flag_voteshare_above_1 = as.integer(!is.na(winner_voteshare) & winner_voteshare > 1),
    flag_pct_only = as.integer(is.na(eligible_voters) & is.na(valid_votes)),
    # Cap turnout and voteshare at 1 (preserve NAs)
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout),
    winner_voteshare = ifelse(!is.na(winner_voteshare) & winner_voteshare > 1,
                              1, winner_voteshare)
  )


# 10. Final formatting and output ------------------------------------------

df_harm <- df_harm |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  ) |>
  select(
    ags, ags_name, state, state_name,
    election_year, election_date, election_type, round,
    eligible_voters, number_voters, valid_votes, invalid_votes,
    turnout, winner_party, winner_votes, winner_voteshare,
    flag_unsuccessful_naive_merge, flag_pre_1990, flag_aggregated,
    flag_turnout_above_1, flag_voteshare_above_1, flag_pct_only,
    flag_superseded, flag_multi_winner,
    n_predecessors
  ) |>
  arrange(ags, election_date, round)


# 11. Verification ---------------------------------------------------------

cat("\n=== Verification ===\n")
cat("Output rows:", nrow(df_harm), "\n")
cat("Unique AGS:", n_distinct(df_harm$ags), "\n")
cat("Year range:", min(df_harm$election_year), "-", max(df_harm$election_year), "\n")

# Check for duplicate (ags, election_date, round). A Hauptwahl and a Stichwahl
# can share a date in the source (3 Bayern cycles), so `round` is part of the key.
dupes <- df_harm |>
  group_by(ags, election_date, round) |>
  filter(n() > 1)
if (nrow(dupes) > 0) {
  cat("WARNING: duplicate (ags, election_date, round) rows:", nrow(dupes), "\n")
  print(dupes |> select(ags, ags_name, election_date, round, election_type) |> head(20))
} else {
  cat("No duplicate (ags, election_date, round) rows — OK\n")
}

same_date_out <- df_harm |>
  group_by(ags, election_date) |>
  filter(n() > 1) |>
  ungroup()
cat("Rows sharing (ags, election_date) across rounds:", nrow(same_date_out), "\n")

# AGS length check
cat("AGS length distribution:\n")
print(table(nchar(df_harm$ags)))

# State distribution
cat("\nRows by state:\n")
print(df_harm |> count(state, state_name) |> arrange(state))

# Flag summary
cat("\nFlag summary:\n")
cat("  flag_unsuccessful_naive_merge:",
    sum(df_harm$flag_unsuccessful_naive_merge, na.rm = TRUE), "\n")
cat("  flag_pre_1990:",
    sum(df_harm$flag_pre_1990, na.rm = TRUE), "\n")
cat("  flag_aggregated:",
    sum(df_harm$flag_aggregated, na.rm = TRUE), "\n")
cat("  flag_pct_only:",
    sum(df_harm$flag_pct_only, na.rm = TRUE), "\n")
cat("  flag_turnout_above_1:",
    sum(df_harm$flag_turnout_above_1, na.rm = TRUE), "\n")
cat("  flag_voteshare_above_1:",
    sum(df_harm$flag_voteshare_above_1, na.rm = TRUE), "\n")
cat("  flag_superseded:",
    sum(df_harm$flag_superseded, na.rm = TRUE), "\n")
cat("  flag_multi_winner:",
    sum(df_harm$flag_multi_winner, na.rm = TRUE), "\n")

# Spot-check: München
muenchen <- df_harm |> filter(ags == "09162000")
cat("\nMünchen rows:", nrow(muenchen), "\n")
if (nrow(muenchen) > 0) {
  cat("  Year range:", min(muenchen$election_year), "-",
      max(muenchen$election_year), "\n")
  cat("  Max eligible_voters:", max(muenchen$eligible_voters, na.rm = TRUE), "\n")
}

# Spot-check: RLP data preserved
rlp <- df_harm |> filter(state == "07")
cat("\nRLP rows:", nrow(rlp), "\n")
cat("  Counts all NA:", all(is.na(rlp$eligible_voters)), "\n")
cat("  Turnout non-NA:", sum(!is.na(rlp$turnout)), "of", nrow(rlp), "\n")

# Sample aggregated rows
if (sum(df_harm$flag_aggregated, na.rm = TRUE) > 0) {
  cat("\nSample aggregated rows:\n")
  print(
    df_harm |>
      filter(flag_aggregated == 1) |>
      select(ags, ags_name, election_date, winner_party, n_predecessors) |>
      head(20)
  )
}

glimpse(df_harm)


# 12. Save -----------------------------------------------------------------

fwrite(df_harm, "data/mayoral_elections/final/mayoral_harm.csv")
write_rds(df_harm, "data/mayoral_elections/final/mayoral_harm.rds")

cat("\nSaved to data/mayoral_elections/final/mayoral_harm.{csv,rds}\n")
cat("Done.\n")

## END
