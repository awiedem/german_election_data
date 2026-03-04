### European Elections 2024: Municipality-Level Results
# Aggregates ballot-district data to municipality level
# Handles Amt/VG-level mail-in (Briefwahl) vote allocation
# Vincent Heddesheimer
# March 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "haschaR")
conflict_prefer("filter", "dplyr")


# --- 1. Read raw data --------------------------------------------------------

df <- fread(
  "data/european_elections/raw/ew24_wbz/ew24_wbz_ergebnisse.csv",
  encoding = "UTF-8"
)

glimpse(df)


# --- 2. Construct 8-digit AGS ------------------------------------------------

df <- df |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, Regierungsbezirk, Kreis, Gemeinde),
    county = substr(ags, 1, 5)
  ) |>
  rename(
    BA = Bezirksart,
    BWBez = `Kennziffer Briefwahlzugehörigkeit`
  )

# Define column groups
vote_meta_cols <- c(
  "Wahlberechtigte",
  "Wahlberechtigte ohne Sperrvermerk (A1)",
  "Wahlberechtigte mit Sperrvermerk (A2)",
  "Wahlberechtigte § 24 (2) EuWO (A3)",
  "Wählende", "dar. Wählende mit Wahlschein",
  "ungültig", "gültig"
)
party_cols <- names(df)[which(names(df) == "CDU"):which(names(df) == "V-Partei³")]
numeric_cols <- c(vote_meta_cols, party_cols)

# Ensure numeric
df <- df |> mutate(across(all_of(numeric_cols), as.numeric))


# --- 3. Aggregate ballot districts by (ags, BWBez, BA) -----------------------

df_agg <- df |>
  group_by(ags, county, BWBez, BA) |>
  summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

# --- 4. Identify municipality types ------------------------------------------

# Real municipalities: have at least one BA=0 (polling station) row
ags_with_ba0 <- df_agg |> filter(BA == 0) |> pull(ags) |> unique()
# Municipalities with own mail-in: have BA=5 row for their own AGS
ags_with_ba5 <- df_agg |> filter(BA == 5) |> pull(ags) |> unique()
# Mail-in-only AGS (no BA=0): Amt-level mail-in districts
mailin_only_ags <- setdiff(ags_with_ba5, ags_with_ba0)
# Real municipalities without own mail-in: need allocated mail-in
real_no_mailin <- setdiff(ags_with_ba0, ags_with_ba5)

cat("Real municipalities:", length(ags_with_ba0), "\n")
cat("  with own mail-in:", length(intersect(ags_with_ba0, ags_with_ba5)), "\n")
cat("  without own mail-in:", length(real_no_mailin), "\n")
cat("Mail-in-only AGS (to distribute):", length(mailin_only_ags), "\n")


# --- 5. Municipalities with own mail-in: sum all BA types --------------------

df_own_mailin <- df_agg |>
  filter(ags %in% intersect(ags_with_ba0, ags_with_ba5)) |>
  group_by(ags, county) |>
  summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")


# --- 6. Municipalities without own mail-in: allocate from Amt-level ----------

# 6a: Get polling station totals for municipalities needing allocation
df_no_mailin <- df_agg |>
  filter(ags %in% real_no_mailin & BA == 0) |>
  group_by(ags, county, BWBez) |>
  summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

# 6b: Get Amt-level mail-in totals
df_mailin <- df_agg |>
  filter(ags %in% mailin_only_ags & BA == 5) |>
  group_by(county, BWBez) |>
  summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

# 6c: Calculate eligible-voter weights within each (county, BWBez) group
# Use Wahlberechtigte (total eligible voters) as proportional allocation key
# Note: A2 (Sperrvermerk) would be more precise but has data quality issues
# in a few municipalities where A2 >> Wahlberechtigte
elig_col <- "Wahlberechtigte"

df_no_mailin <- df_no_mailin |>
  group_by(county, BWBez) |>
  mutate(
    group_elig = sum(.data[[elig_col]], na.rm = TRUE),
    elig_weight = ifelse(
      group_elig > 0,
      .data[[elig_col]] / group_elig,
      1 / n()  # Equal split if all zero
    )
  ) |>
  ungroup()

# 6d: Distribute mail-in votes using pivot-longer approach (following BTW pattern)
mailin_long <- df_mailin |>
  pivot_longer(
    cols = all_of(numeric_cols),
    names_to = "var",
    values_to = "mailin_value"
  )

df_no_mailin_long <- df_no_mailin |>
  pivot_longer(
    cols = all_of(numeric_cols),
    names_to = "var",
    values_to = "ags_value"
  ) |>
  left_join(mailin_long, by = c("county", "BWBez", "var")) |>
  mutate(
    weighted_mailin = round(coalesce(mailin_value, 0) * elig_weight, 0),
    final_value = ags_value + weighted_mailin
  )

# 6e: Pivot back to wide
df_allocated <- df_no_mailin_long |>
  select(ags, county, var, final_value) |>
  pivot_wider(names_from = var, values_from = final_value)


# --- 7. Combine all municipalities -------------------------------------------

df_muni <- bind_rows(
  df_own_mailin,
  df_allocated
) |>
  arrange(ags)

cat("Final municipality count:", nrow(df_muni), "\n")
cat("Expected (real municipalities):", length(ags_with_ba0), "\n")

# Check duplicates
dupl <- df_muni |> count(ags) |> filter(n > 1)
if (nrow(dupl) > 0) {
  warning("Duplicate AGS found: ", paste(dupl$ags, collapse = ", "))
} else {
  cat("No duplicate AGS\n")
}


# --- 8. Standardize column names and add metadata ---------------------------

df_muni <- df_muni |>
  rename(
    eligible_voters = Wahlberechtigte,
    voters_wo_sperrvermerk = `Wahlberechtigte ohne Sperrvermerk (A1)`,
    voters_w_sperrvermerk = `Wahlberechtigte mit Sperrvermerk (A2)`,
    voters_par24_2 = `Wahlberechtigte § 24 (2) EuWO (A3)`,
    number_voters = `Wählende`,
    voters_w_wahlschein = `dar. Wählende mit Wahlschein`,
    invalid_votes = ungültig,
    valid_votes = gültig
  ) |>
  janitor::clean_names() |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state),
    election_year = 2024L,
    election_date = lubridate::ymd("2024-06-09")
  )

# Rename parties to match project conventions where possible
party_renames <- c(
  "gruene" = "grune",
  "afd" = "af_d",
  "die_linke" = "linke",
  "freie_waehler" = "freie_wahler",
  "tierschutz" = "tierschutzpartei",
  "oedp" = "odp",
  "buendnis_c" = "bundnis_c",
  "buendnis_deutschland" = "bundnis_deutschland",
  "v_partei3" = "v_partei_a3"
)
df_muni <- df_muni |> rename(any_of(party_renames))


# --- 9. Compute turnout and vote shares --------------------------------------

# Identify party columns (everything after valid_votes, before state)
all_names <- names(df_muni)
party_start <- which(all_names == "cdu")
party_end <- which(all_names == "v_partei3")
if (length(party_end) == 0) party_end <- which(all_names == "v_partei_a3")
party_cols_clean <- all_names[party_start:party_end]

df_muni <- df_muni |>
  mutate(
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    # Party shares: votes / number_voters (following federal pipeline convention)
    across(all_of(party_cols_clean), ~ ifelse(number_voters > 0, .x / number_voters, NA_real_))
  )

# Cap turnout at 1 and flag
df_muni <- df_muni |>
  mutate(
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout)
  )

cat("Rows with turnout > 1:", sum(df_muni$flag_turnout_above_1, na.rm = TRUE), "\n")
cat("Rows with NA turnout:", sum(is.na(df_muni$turnout)), "\n")


# --- 10. Reorder columns and write ------------------------------------------

df_muni <- df_muni |>
  select(
    ags, county, state, state_name,
    election_year, election_date,
    eligible_voters, number_voters, valid_votes, invalid_votes,
    voters_wo_sperrvermerk, voters_w_sperrvermerk, voters_par24_2, voters_w_wahlschein,
    turnout,
    all_of(party_cols_clean),
    flag_turnout_above_1
  )

glimpse(df_muni)

# Write
write_rds(df_muni, "data/european_elections/final/european_muni_unharm.rds")
fwrite(df_muni, "data/european_elections/final/european_muni_unharm.csv")

cat("Written:", nrow(df_muni), "rows x", ncol(df_muni), "columns\n")


# --- 11. Sanity checks -------------------------------------------------------

# Check state distribution
df_muni |>
  group_by(state, state_name) |>
  summarise(
    n = n(),
    mean_turnout = mean(turnout, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(state) |>
  print(n = 16)

# Check party vote share ranges
cat("\nParty share ranges (min / median / max):\n")
df_muni |>
  summarise(across(all_of(party_cols_clean), list(
    min = ~ min(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE)
  ))) |>
  pivot_longer(everything()) |>
  separate(name, into = c("party", "stat"), sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = stat, values_from = value) |>
  filter(median > 0.01) |>
  arrange(desc(median)) |>
  print(n = 20)

# Spot check: national totals
cat("\nNational totals:\n")
national <- df_muni |>
  summarise(
    eligible = sum(eligible_voters),
    voters = sum(number_voters),
    valid = sum(valid_votes),
    turnout = sum(number_voters) / sum(eligible_voters)
  )
print(national)


### END
