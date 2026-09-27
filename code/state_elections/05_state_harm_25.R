### Harmonize state election results to 2025 borders
# Based on 04_state_harm_23.R
# Date: 2026

rm(list = ls())

conflict_prefer("filter", "dplyr")

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)
source("code/shared/harmonization_audit.R")
source("code/shared/state_mapping.R")

pacman::p_load(
  "tidyverse",
  "data.table",
  "haschaR"
)

# Load unharmonized data ----------------------------------------------------

cat("Loading unharmonized state election data...\n")

state_source_all <- gerda_read_election_source("data/state_elections/final/state_unharm.rds")
gerda_audit_write(
  state_source_all |> filter(election_year < 1990) |>
    select(ags, election_year, state, eligible_voters, number_voters, valid_votes) |>
    mutate(reason = "Before annual crosswalk coverage (1990)",
           source = "BBSR crosswalk coverage"),
  "state_harm_25", "out_of_scope")
df <- state_source_all |>
  as_tibble() |>
  filter(election_year >= 1990) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    county = str_sub(ags, 1, 5),
    cdu = ifelse(state != "09" & (cdu == 0 | is.na(cdu)), cdu_csu, cdu),
    csu = ifelse(state == "09" & (csu == 0 | is.na(csu)), cdu_csu, csu)
  ) |>
  arrange(ags, election_year)

glimpse(df)
table(df$election_year)

# One column per party in the harmonized files. state_unharm keeps each source's
# own label, so the same party can sit in two columns there (no row has both):
# PdH = Partei der Humanisten, the ST 2011/2016 FREIE WÄHLER and Tierschutzpartei
# spellings, and Volt Hamburg 2020.
party_merges <- c(pdh = "die_humanisten", freiewaehler = "freie_wahler",
                  tier_schutz_partei = "tierschutz", volt_hamburg = "volt")
for (from in names(party_merges)) {
  to <- party_merges[[from]]
  if (!from %in% names(df)) next
  stopifnot(to %in% names(df), !any(!is.na(df[[from]]) & !is.na(df[[to]])))
  df[[to]] <- dplyr::coalesce(df[[to]], df[[from]])
  df[[from]] <- NULL
}
# Parties observed only in elections outside the harmonized range (BW 1952,
# SH 1983): all-NA here, so kept out of the harmonized schema.
df <- df |> select(-any_of(c("dg_bhe", "llsh", "uwg")))

# Convert vote shares to vote counts ----------------------------------------

# Identify party variables by excluding metadata columns
metadata_cols <- c("ags", "county", "election_year", "state", "election_date",
                   "eligible_voters", "number_voters", "valid_votes",
                   "invalid_votes", "turnout", "other", "cdu_csu",
                   "flag_naive_turnout_above_1", "flag_no_valid_votes",
                   "flag_briefwahl_only", "flag_pooled")
party_vars <- setdiff(names(df), metadata_cols)
df <- gerda_state_exclusions(df, party_vars, "state_harm_25")

cat("Party variables found:", paste(party_vars, collapse = ", "), "\n")

# For rows with NA valid_votes (e.g. HB pre-1999 percentage-only data),
# impute a weight from number_voters or eligible_voters so that the
# share→count→share round-trip preserves the original percentages.
# Rows with no electorate at all fall through to the unit-weight placeholder in
# the imputation below (valid_votes = 1). Mark them, so the weight-conservation
# check further down does not mistake that placeholder for a real vote: 35
# Bavarian gemeindefreie Gebiete carry no voters yet sit at crosswalk weight
# sums of up to 20.
df <- df |>
  mutate(flag_vv_placeholder = as.integer(
    is.na(valid_votes) &
      !(!is.na(number_voters) & number_voters > 0) &
      !(!is.na(eligible_voters) & eligible_voters > 0)
  ))

n_na_vv <- sum(is.na(df$valid_votes))
if (n_na_vv > 0) {
  cat(sprintf("Imputing valid_votes weight for %d rows (using number_voters/eligible_voters)\n", n_na_vv))
  df <- df |>
    mutate(valid_votes = case_when(
      !is.na(valid_votes) ~ valid_votes,
      !is.na(number_voters) & number_voters > 0 ~ number_voters,
      !is.na(eligible_voters) & eligible_voters > 0 ~ eligible_voters,
      TRUE ~ 1
    ))
}

# Convert vote shares to vote counts
df <- df |>
  mutate(
    across(all_of(party_vars), ~ .x * valid_votes)
  )

# The unit weight above only carries shares through the round trip. A
# placeholder row has no shares either, so it must not hand a phantom valid vote
# to its target -- before September 2026 three NI rows were published with
# valid_votes = 1, other = 1 and cdu_csu = 0, and the 58 RP 2026 Ortsgemeinden
# counted inside a neighbour (data/state_elections/metadata/
# rp_2026_pooled_municipalities.csv) would have joined them.
df <- df |>
  mutate(valid_votes = ifelse(flag_vv_placeholder == 1, NA_real_, valid_votes))
# ... which holds only while placeholder rows carry no shares. A future
# percentage-only row without electorate would lose them here, so stop instead.
stopifnot(all(rowSums(!is.na(as.data.frame(df)[df$flag_vv_placeholder == 1, party_vars, drop = FALSE])) == 0))

# Preserve the original code before any correction or city aggregation.
df$ags_original <- df$ags

# Handle Berlin, Hamburg districts and known problematic AGS codes -----------
# This should be done before crosswalk merge

df <- df |>
  mutate(
    ags = case_when(
      # Berlin: aggregate all districts to overall Berlin AGS
      str_sub(ags, 1, 2) == "11" ~ "11000000",
      # Hamburg: aggregate all districts to overall Hamburg AGS
      str_sub(ags, 1, 2) == "02" ~ "02000000",
      # Rhineland-Palatinate corrections (from 02_state_harm.R)
      ags == "07140502" & election_year == 2011 ~ "07135050", # Lahr
      ags == "07140503" & election_year == 2011 ~ "07135063", # Mörsdorf
      ags == "07140504" & election_year == 2011 ~ "07135094", # Zilshausen
      ags == "07232502" & election_year == 2011 ~ "07232021", # Brimingen
      ags == "07232502" & election_year == 2016 ~ "07232021", # Brimingen (2016!)
      ags == "07235207" & election_year == 2011 ~ "07231207", # Trittenheim
      # Eastern Germany AGS corrections (from federal harm scripts)
      # Sachsen-Anhalt 1990: wrong 3rd digit in DDR-era encoding
      ags == "15228170" & election_year == 1990 ~ "15028170", # Kleinheringen
      ags == "15228280" & election_year == 1990 ~ "15028280", # Neidschütz
      ags == "15228380" & election_year == 1990 ~ "15028380", # Wettaburg
      ags == "15320590" & election_year == 1990 ~ "15020590", # Wedringen
      ags == "15336010" & election_year == 1990 ~ "15036010", # Abbendorf
      ags == "15336290" & election_year == 1990 ~ "15036290", # Holzhausen
      ags == "15336660" & election_year == 1990 ~ "15036660", # Waddekath
      # Thuringia 1994: AGS renumbered during Kreisreform
      ags == "16063057" & election_year == 1994 ~ "16063094", # Moorgrund
      ags == "16063047" & election_year == 1994 ~ "16016410", # Kupfersuhl
      ags == "16063056" & election_year == 1994 ~ "16015420", # Möhra
      ags == "16069022" & election_year == 1994 ~ "16023360", # Heßberg
      ags == "16073098" & election_year == 1994 ~ "16033700", # Weißen
      # Saxony 1994: Kreis renumbered
      ags == "14082220" & election_year == 1994 ~ "14032270", # Krumbach
      ags == "14085170" & election_year == 1994 ~ "14031310", # Naunhof
      # Brandenburg 1990: kreisfreie Städte old encoding (060 suffix)
      ags == "12003060" & election_year == 1990 ~ "12003000", # Eisenhüttenstadt
      ags == "12006060" & election_year == 1990 ~ "12006000", # Schwedt/Oder
      # Sachsen-Anhalt 1994: Merzien post-Kreisreform code → pre-reform
      ags == "15159029" & election_year == 1994 ~ "15026310", # Merzien
      # Sachsen 1994: Cunsdorf dissolved 1995 into Elsterberg (not in CW)
      ags == "14045730" & election_year == 1994 ~ "14045610", # Cunsdorf → Elsterberg
      TRUE ~ ags
    )
  )

source_lineage <- df |> select(ags_original, ags, election_year, state) |> distinct()

# NA-preserving sum: returns NA when ALL inputs are NA (sum(NA, na.rm=TRUE) → 0 in base R)
sum_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# After correcting AGS codes, aggregate any duplicates
duplicate_keys <- duplicated(df[c("ags", "election_year", "state")]) |
  duplicated(df[c("ags", "election_year", "state")], fromLast = TRUE)
df_singletons <- df[!duplicate_keys, ] |>
  select(ags, election_year, state, eligible_voters, number_voters, valid_votes,
         invalid_votes, all_of(party_vars), any_of(c("election_date", "county")),
         flag_vv_placeholder, flag_pooled)
df <- df[duplicate_keys, ] |>
  group_by(ags, election_year, state) |>
  summarise(
    across(
      c(eligible_voters, number_voters, valid_votes, invalid_votes, all_of(party_vars)),
      ~ sum_na(.x)
    ),
    across(any_of(c("election_date", "county")), first),
    flag_vv_placeholder = max(flag_vv_placeholder),
    flag_pooled = max(flag_pooled),
    .groups = "drop"
  ) |>
  bind_rows(df_singletons) |>
  arrange(ags, election_year)

glimpse(df)

# Extract election_date lookup before harmonization (dates are lost in group_by)
date_lookup <- df |>
  distinct(state, election_year, election_date) |>
  filter(!is.na(election_date))

# Load crosswalks -----------------------------------------------------------

cat("Loading crosswalks...\n")

cw <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  as_tibble() |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  ) |>
  rename(election_year = year)

# Add Wasdow manually (not in crosswalk files)
# Wasdow (13072115) merged into Behren-Lübchin (13072010) in 2011
# Check what the 2025 AGS is for Behren-Lübchin
cw_25_check <- cw |>
  filter(ags == "13072010" & election_year == 2011)

ags_25_wasdow <- if (nrow(cw_25_check) > 0) {
  first(cw_25_check$ags_25)
} else {
  "13072010" # Fallback
}

ags_name_25_wasdow <- if (nrow(cw_25_check) > 0) {
  first(cw_25_check$ags_name_25)
} else {
  "Behren-Lübchin"
}

# Create wasdow entry with columns matching cw
wasdow <- tibble(
  ags = "13072115",
  ags_name = "Wasdow",
  election_year = 2011,
  ags_25 = ags_25_wasdow,
  ags_name_25 = ags_name_25_wasdow,
  pop_cw = 1,
  area_cw = 1,
  population = 0.39,
  area = 26.20
) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

# Ensure wasdow has all columns from cw
for (col in names(cw)) {
  if (!col %in% names(wasdow)) {
    wasdow[[col]] <- NA
  }
}

# Reorder to match cw column order
wasdow <- wasdow |>
  select(all_of(names(cw)))

# Bind to crosswalk
cw <- cw |>
  bind_rows(wasdow) |>
  arrange(ags, election_year)

glimpse(cw)
table(cw$election_year)

# Merge crosswalks with election data ---------------------------------------

cat("Merging crosswalks with election data...\n")

cw <- gerda_collapse_crosswalk(cw |>
  select(ags, election_year, ags_25, pop_cw, area_cw))

df_cw_naive <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year"))

# Check for unsuccessful merges
not_merged_naive <- df_cw_naive |>
  filter(is.na(ags_25)) |>
  select(ags, election_year) |>
  distinct() |>
  mutate(id = paste0(ags, "_", election_year))

if (nrow(not_merged_naive) > 0) {
  cat("WARNING: Unsuccessful merges found:", nrow(not_merged_naive), "\n")
  print(not_merged_naive)
}

# Handle special cases (similar to federal elections script)
# For now, we'll flag them and continue
df_cw <- df_cw_naive |>
  mutate(
    id = paste0(ags, "_", election_year),
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0),
    crosswalk_year = ifelse(!is.na(ags_25), election_year, NA_real_),
    mapping_method = ifelse(!is.na(ags_25), "exact_year", "unmatched")
  )

# For observations that didn't merge, try using year - 1 for crosswalk
# This handles cases where municipalities merged right after an election
# NOTE: Only apply fallback join to unmatched rows to avoid duplicating
# multi-target AGS (which map to >1 ags_25 with pop_cw weights)
df_matched <- df_cw |> filter(!is.na(ags_25))
df_unmatched <- df_cw |> filter(is.na(ags_25))

if (nrow(df_unmatched) > 0) {
  df_unmatched <- df_unmatched |>
    mutate(
      year_cw = pmax(pmin(election_year - 1, 2024), 1990)
    ) |>
    select(-ags_25, -pop_cw, -area_cw) |>
    left_join(
      cw |> select(ags, election_year, ags_25, pop_cw, area_cw) |>
        rename(year_cw = election_year),
      by = c("ags", "year_cw")
    ) |>
    mutate(crosswalk_year = ifelse(!is.na(ags_25), year_cw, NA_real_),
           mapping_method = ifelse(!is.na(ags_25), "previous_year", "unmatched"))
}

df_cw <- bind_rows(df_matched, df_unmatched)

## --- Fuzzy time matching: for still-unmatched AGS, find closest CW year ---
still_unmatched <- df_cw |> filter(is.na(ags_25))
if (nrow(still_unmatched) > 0) {
  n_unmatched_ags <- n_distinct(still_unmatched$ags)
  cat("Fuzzy time matching for", n_unmatched_ags, "unmatched AGS codes...\n")

  df_already_matched <- df_cw |> filter(!is.na(ags_25))

  # Step 1: find the closest CW year for each (ags, election_year)
  unmatched_keys <- still_unmatched |> select(ags, election_year) |> distinct()
  best_cw_year <- gerda_nearest_crosswalk_year(unmatched_keys, cw, "ags_25", "state_harm_25")

  still_unmatched <- still_unmatched |>
    select(-ags_25, -pop_cw, -area_cw) |>
    left_join(best_cw_year, by = c("ags", "election_year")) |>
    left_join(
      cw |> select(ags, election_year, ags_25, pop_cw, area_cw) |>
        rename(cw_year = election_year),
      by = c("ags", "cw_year")
    ) |>
    mutate(crosswalk_year = cw_year,
           mapping_method = ifelse(!is.na(ags_25), "nearest_year", "unmatched")) |>
    select(-cw_year)

  n_recovered <- sum(!is.na(still_unmatched$ags_25))
  cat("  Recovered", n_recovered, "of", nrow(still_unmatched),
      "rows via fuzzy time matching\n")
  df_cw <- bind_rows(df_already_matched, still_unmatched)
}

## --- Self-mapping: unmatched AGS that are already valid 2025 codes map to self ---
still_unmatched2 <- df_cw |> filter(is.na(ags_25))
if (nrow(still_unmatched2) > 0) {
  valid_targets <- unique(cw$ags_25)
  self_map <- still_unmatched2$ags %in% valid_targets
  if (any(self_map)) {
    cat("  Self-mapping", sum(self_map), "rows where AGS is already a valid ags_25\n")
    still_unmatched2$ags_25[self_map] <- still_unmatched2$ags[self_map]
    still_unmatched2$pop_cw[self_map] <- 1
    still_unmatched2$area_cw[self_map] <- 1
    still_unmatched2$crosswalk_year[self_map] <- NA_real_
    still_unmatched2$mapping_method[self_map] <- "target_identity"
    df_cw <- bind_rows(df_cw |> filter(!is.na(ags_25)), still_unmatched2)
  }
}

glimpse(df_cw)

# Check remaining unsuccessful merges
not_merged_final <- df_cw |>
  filter(is.na(ags_25)) |>
  select(ags, election_year) |>
  distinct()

if (nrow(not_merged_final) > 0) {
  cat("WARNING: Still have unsuccessful merges:", nrow(not_merged_final), "\n")
  print(not_merged_final, n = Inf)
}


# Validate against the complete pre-join input, before any filtering.
df_cw <- df_cw |>
  mutate(
    source_boundary_year = ifelse(ags == "07232503", 2025L, NA_integer_),
    mapping_method = ifelse(ags == "07232503" & !is.na(ags_25),
                           "later_boundary_code", mapping_method)
  )
gerda_audit_mapping(df, df_cw, c("ags", "election_year", "state"),
                    "ags_25", "state_harm_25", target_codes = unique(cw$ags_25))
gerda_audit_write(
  source_lineage |> left_join(
    df_cw |> select(ags, election_year, state, ags_25, crosswalk_year,
                    mapping_method, source_boundary_year, pop_cw),
    by = c("ags", "election_year", "state"), relationship = "many-to-many"),
  "state_harm_25", "provenance")

# Harmonize ----------------------------------------------------------------

cat("Harmonizing to 2025 borders...\n")

# Weighted sum that preserves NA when ALL source values are NA
# (sum(NA * w, na.rm=TRUE) returns 0, but semantically it should be NA)
wsum_na <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  sum(x * w, na.rm = TRUE)
}

# Harmonize vote counts with weighted sum
votes <- gerda_weighted_counts(
  df_cw, c("ags_25", "election_year"),
  c("eligible_voters", "number_voters", "valid_votes", "invalid_votes", party_vars),
  "pop_cw")

# Historical state-border transfers can move counts between present-day states.
# Check each source row above and national totals by election year here.
gerda_audit_totals(
  df, votes |> mutate(state = substr(ags_25, 1, 2)),
  "election_year",
  c("eligible_voters", "number_voters", "valid_votes", "invalid_votes", party_vars),
  "state_harm_25")
votes <- votes |>
  mutate(across(c(eligible_voters, number_voters, valid_votes, invalid_votes,
                  all_of(party_vars)), ~ round(.x, digits = 0)))

# flag_pooled on the target: 1 if any source row mapped onto it with positive
# weight belongs to a pooled count unit (see 01b_state_unharm_raw.R)
votes <- votes |>
  left_join(df_cw |>
              group_by(ags_25, election_year) |>
              summarise(flag_pooled = as.integer(any(flag_pooled == 1 & pop_cw > 0, na.rm = TRUE)),
                        .groups = "drop"),
            by = c("ags_25", "election_year"))

glimpse(votes)

# Convert vote counts back to vote shares
df_harm <- votes %>%
  mutate(
    across(all_of(party_vars), ~ ifelse(valid_votes > 0, .x / valid_votes, NA_real_)),
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    # Flag and cap harmonized turnout (mirrors unharm safety net)
    flag_harm_turnout_above_1 = ifelse(!is.na(turnout) & is.finite(turnout) & turnout > 1, 1L, 0L),
    turnout = ifelse(is.finite(turnout), turnout, NA_real_),
    turnout = ifelse(!is.na(turnout) & turnout > 1.5, NA_real_, turnout),
    # Recompute derived columns after harmonization
    # A row with no known party share (no valid votes) has no residual and no
    # Union share either: NA, not other = 1 and cdu_csu = 0
    n_party_known = rowSums(!is.na(across(all_of(party_vars)))),
    other = ifelse(n_party_known == 0, NA_real_,
                   pmax(1 - rowSums(across(all_of(party_vars)), na.rm = TRUE), 0)),
    cdu_csu = ifelse(n_party_known == 0, NA_real_, coalesce(cdu, 0) + coalesce(csu, 0))
  ) |>
  select(-n_party_known) |>
  rename(ags = ags_25) |>
  filter(!is.na(ags)) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  ) |>
  relocate(state, .after = election_year) |>
  relocate(state_name, .after = state) |>
  arrange(ags, election_year)

# Diagnostic: harmonized turnout flags
n_flagged <- sum(df_harm$flag_harm_turnout_above_1, na.rm = TRUE)
if (n_flagged > 0) {
  cat(sprintf("WARNING: %d rows with harmonized turnout > 1 (flagged)\n", n_flagged))
  n_capped <- sum(is.na(df_harm$turnout) & df_harm$flag_harm_turnout_above_1 == 1, na.rm = TRUE)
  if (n_capped > 0) cat(sprintf("  Of these, %d rows had turnout > 1.5 → set to NA\n", n_capped))
}

glimpse(df_harm)

# Add election_date from lookup (extracted before harmonization)
df_harm <- df_harm |>
  left_join(date_lookup, by = c("state", "election_year")) |>
  relocate(election_date, .after = election_year)

# Check for missing election dates
if (df_harm |> filter(is.na(election_date)) |> nrow() > 0) {
  cat("WARNING: Missing election dates found\n")
  df_harm |>
    filter(is.na(election_date)) |>
    select(state_name, election_year) |>
    distinct() |>
    print()
}

# Add flag for unsuccessful merges
df_harm <- df_harm |>
  mutate(
    id = paste0(ags, "_", election_year)
  ) |>
  left_join(
    df_cw |>
      mutate(id = paste0(ags_25, "_", election_year)) |>
      select(id, flag_unsuccessful_naive_merge) |>
      distinct() |>
      group_by(id) |>
      summarise(flag_unsuccessful_naive_merge = max(flag_unsuccessful_naive_merge, na.rm = TRUE)) |>
      ungroup(),
    by = "id"
  ) |>
  mutate(
    flag_unsuccessful_naive_merge = ifelse(is.na(flag_unsuccessful_naive_merge), 0, flag_unsuccessful_naive_merge)
  ) |>
  select(-id)

# Zero-vote party → NA recoding
derived_cols <- c("other", "cdu_csu", "far_right", "far_left", "far_left_w_linke")
share_cols <- setdiff(party_vars, derived_cols)
zero_party_lookup <- df_harm |>
  group_by(state, election_year) |>
  summarise(across(all_of(share_cols),
                   ~ all(. == 0 | is.na(.), na.rm = FALSE),
                   .names = "allzero__{.col}"),
            .groups = "drop") |>
  pivot_longer(starts_with("allzero__"),
               names_to = "party", values_to = "all_zero",
               names_prefix = "allzero__") |>
  filter(all_zero)

if (nrow(zero_party_lookup) > 0) {
  df_long <- df_harm |>
    mutate(.row_id = row_number()) |>
    pivot_longer(cols = all_of(share_cols), names_to = "party",
                 values_to = "vote_share") |>
    left_join(zero_party_lookup, by = c("state", "election_year", "party")) |>
    mutate(vote_share = if_else(!is.na(all_zero) & all_zero &
                                  (vote_share == 0 | is.na(vote_share)),
                                NA_real_, vote_share)) |>
    select(-all_zero) |>
    pivot_wider(names_from = "party", values_from = "vote_share")
  df_harm <- df_long |> select(-.row_id) |> arrange(ags, election_year)
  cat("Zero-vote → NA recoding applied\n")
}

# Pooled party columns
far_right_cols <- intersect(
  c("afd", "npd", "rep", "die_rechte", "dvu", "iii_weg", "fap", "ddd", "dsu"),
  names(df_harm))
far_left_cols <- intersect(
  c("dkp", "kpd", "mlpd", "sgp", "psg", "kbw"),
  names(df_harm))

cat("far_right cols:", paste(far_right_cols, collapse = ", "), "\n")
cat("far_left cols:", paste(far_left_cols, collapse = ", "), "\n")

df_harm <- df_harm |>
  mutate(
    far_right = rowSums(across(any_of(far_right_cols)), na.rm = TRUE),
    far_left = rowSums(across(any_of(far_left_cols)), na.rm = TRUE),
    far_left_w_linke = rowSums(across(any_of(c("linke_pds"))),
                                na.rm = TRUE) + far_left
  )

# Total vote share: sum ALL individual party columns (excludes "other")
# Note: total_vote_share < 1 means a non-trivial "other" residual, not data error
all_derived <- c("far_right", "far_left", "far_left_w_linke", "cdu_csu")
tvs_cols <- setdiff(party_vars, all_derived)
df_harm <- df_harm |>
  mutate(
    total_vote_share = round(rowSums(across(all_of(tvs_cols)), na.rm = TRUE), 8),
    flag_other_party_residual = ifelse(
      total_vote_share > 1.001 | total_vote_share < 0.999, 1, 0),
    perc_total_votes_incongruence = round(total_vote_share - 1, 6)
  )

# The same for the bloc and diagnostic columns, which rowSums(na.rm = TRUE) would
# otherwise publish as far_right = 0, total_vote_share = 0,
# perc_total_votes_incongruence = -1 and flag_other_party_residual = 1 on a row
# with no party share at all (the RP 2026 donors, NI 1990/2017, TH 2024).
no_party_share <- rowSums(!is.na(as.data.frame(df_harm)[tvs_cols])) == 0
df_harm[no_party_share, c("far_right", "far_left", "far_left_w_linke", "total_vote_share",
                          "perc_total_votes_incongruence", "flag_other_party_residual")] <- NA
cat("Rows with no party share (derived columns NA):", sum(no_party_share), "\n")

glimpse(df_harm)


# Add area and population --------------------------------------------------

cat("Adding covariates...\n")

# Load 2023 covariates
area_pop_23 <- read_rds("data/covars_municipality/final/ags_area_pop_emp_2023.rds") |>
  mutate(ags_2023 = pad_zero_conditional(ags_2023, 7))

# Load 23->25 crosswalk to map covariates to 2025 boundaries
cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  filter(year == 2023) |>
  select(ags, ags_25, ags_name_25, pop_cw, area_cw) |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    ags_25 = pad_zero_conditional(ags_25, 7)
  )

# Map covariates from 2023 to 2025 boundaries
area_pop <- area_pop_23 |>
  left_join(cw_23_25, by = c("ags_2023" = "ags")) |>
  mutate(
    ags_25 = coalesce(ags_25, ags_2023),
    ags_name_25 = coalesce(ags_name_25, ags_name_23)
  ) |>
  group_by(ags_25, year) |>
  summarize(
    ags_name_25 = first(ags_name_25),
    area_ags = sum(area_ags * coalesce(area_cw, 1), na.rm = TRUE),
    population_ags = sum(population_ags * coalesce(pop_cw, 1), na.rm = TRUE),
    # NA when no constituent reports employees (none before 1997 or after
    # 2021); sum(na.rm = TRUE) alone published those rows as 0.
    employees_ags = if (all(is.na(employees_ags))) NA_real_ else
      sum(employees_ags * coalesce(pop_cw, 1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Inhabitants per km2, as in the covariate panel (population is in thousands)
  mutate(pop_density_ags = population_ags * 1000 / area_ags)

glimpse(area_pop)

# The name is a property of the 2025 municipality, not of the election year:
# every row takes the crosswalk's target name. Joined on (ags, year) from the
# covariate panel, it was NA for every election after the panel's last year
# (2023).
ags_names_25 <- cw_23_25 |> distinct(ags_25, ags_name_25)
stopifnot(!anyDuplicated(ags_names_25$ags_25))

# Covariates describe the election year. Elections after the panel's last year
# take that year's values, marked by flag_covars_carried_forward (the federal
# and municipal files fill such years too, but without a flag).
covars_last_year <- max(area_pop$year)

df_final <- df_harm |>
  mutate(covars_year = pmin(election_year, covars_last_year)) |>
  left_join_check_obs(area_pop, by = c("ags" = "ags_25", "covars_year" = "year")) |>
  mutate(
    ags_name = ags_names_25$ags_name_25[match(ags, ags_names_25$ags_25)],
    flag_covars_carried_forward = as.integer(election_year > covars_last_year)
  ) |>
  relocate(ags_name, .after = ags) |>
  select(-ags_name_25, -covars_year)
stopifnot(!anyNA(df_final$ags_name))
stopifnot(nrow(df_final) == nrow(df_harm), !anyNA(df_final$area_ags),
          !anyNA(df_final$population_ags))

glimpse(df_final)


# Save ----------------------------------------------------------------------

cat("Saving harmonized data...\n")

fwrite(df_final, "data/state_elections/final/state_harm_25.csv")
write_rds(df_final, "data/state_elections/final/state_harm_25.rds", compress = "gz")

cat("Done!\n")
cat("Total observations:", nrow(df_final), "\n")
cat("Election years:", paste(sort(unique(df_final$election_year)), collapse = ", "), "\n")
cat("Number of unique municipalities:", n_distinct(df_final$ags), "\n")
cat("States:", paste(sort(unique(df_final$state)), collapse = ", "), "\n")

table(df_final$election_year)
table(df_final$state)

# Check BSW values present for 2024 elections
cat("\nBSW values for 2024 elections:\n")
df_final |>
  filter(election_year == 2024) |>
  group_by(state_name) |>
  summarize(
    n = n(),
    bsw_mean = mean(bsw, na.rm = TRUE),
    bsw_na_count = sum(is.na(bsw))
  ) |>
  print()

# Check CDU/CSU consistency
df_final %>%
  filter(election_year == 2024) %>%
  filter((is.na(csu) | csu == 0) & state == "09") |>
  glimpse()

df_final %>%
  filter(election_year == 2024) %>%
  filter((is.na(cdu) | cdu == 0) & state != "09") |>
  glimpse()
