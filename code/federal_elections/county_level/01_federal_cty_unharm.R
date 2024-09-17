### Clean and combine BTW electoral results at municipality level 1953-2021
# Vincent Heddesheimer
# June, 18, 2024

rm(list=ls())


# Load datasets -----------------------------------------------------------

# Define the function to process election data
process_election_data <- function(file_path) {
  # Read the file
  df <- fread(file_path, encoding = "Latin-1")
  
  # Find the row number where "Lfd. Nr." or "Statistische Kennziffer" appears in the second column
  start_row <- which(grepl("Lfd. Nr.|Statistische Kennziffer", df[[2]]))[1]
  
  # Find the last non-empty row
  end_row <- max(which(df[[1]] != ""))
  
  # Check if start_row or end_row is NA
  if (is.na(start_row) | is.na(end_row)) {
    print(paste("Error in file:", file_path))
    print(paste("start_row:", start_row))
    print(paste("end_row:", end_row))
    stop("Start row or end row is NA")
  }
  
  # Process the data
  df <- df %>%
    slice(start_row:end_row) %>%
    # Remove all columns where first row contains "Erststimmen"
    select(-which(grepl("erststimmen", df[start_row+1, ], ignore.case = TRUE))) %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    # Remove rows that contain "zweitstimmen" in any column
    filter_all(all_vars(!grepl("zweitstimmen", ., ignore.case = TRUE)))
  
  # Add a column for the year
  df <- df |>
    mutate(year = as.numeric(gsub("btw|kreis.csv", "", basename(file_path)))
  )
  
  return(df)
}

# List of file paths for federal elections from 1953 to 2021
file_paths <- list.files("data/federal_elections/county_level/raw/", 
                         pattern = "btw[0-9]+kreis.csv", full.names = TRUE)

# Process all files and store results in a list
election_data <- map(file_paths, safely(process_election_data))

# check
df98 <- election_data[[13]]$result

# Check the first few rows of each data frame to ensure processing worked as expected
purrr::walk(election_data, function(x) {
  if (!is.null(x$result)) {
    print(head(x$result))
  } else {
    print(x$error)
  }
})


## Harmonize variable names ------------------------------------------------

renames <- c(
  "eligible_voters" = "wahlberechtigte",
  "eligible_voters" = "wahlbe_rechtigte",
  "number_voters" = "wa_hler",
  "number_voters" = "wahler",
  "number_voters" = "wählende",
  "invalid_votes" = "ung_a1_4ltige",
  "invalid_votes" = "ungultige",
  "valid_votes" = "gultige",
  "valid_votes" = "ga1_4ltige",
  # main parties
  "fdp" = "f_d_p",
  "gruene" = "grune",
  "gruene" = "gra_ne",
  "linke_pds" = "die_linke",
  "linke_pds" = "pds",
  "afd" = "af_d",
  # smaller
  "rep" = "republikaner",
  "cbv" = "c_b_v",
  "spad" = "sp_ad",
  "graue" = "die_grauen",
  "tierschutz" = "die_tier_schutzpartei",
  "tierschutz" = "tierschutzpartei", 
  "tierschutz" = "die_tierschutzpartei",
  "frauen" = "die_frauen",
  "violetten" = "die_violetten",
  "sgp" = "psg",
  "nichtwaehler" = "nicht_wahler",
  "graue" = "die_grauen",
  "buendnis21" = "bundnis_21_rrp",
  "buendnis21" = "ba1_4ndnis21",
  "freie_waehler" = "freie_wa_hler",
  "freie_waehler" = "freie_wahler",
  "oedp" = "a_dp",
  "oedp" = "odp",
  "buendnis_c" = "ba1_4ndnis_c",
  "buendnis_c" = "bundnis_c",
  "v_partei3" = "v_partei_a3",
  "bueso" = "ba1_4so",
  "buergerbewegung" = "ba_rgerbewegung",
  "team_todenhoefer" = "team_todenh_a_fer",
  "unabhaengige" = "unabha_ngige",
  "deutschland" = "deutsch_land",
  "50plus" = "x50plus",
  "muendige" = "mundige",
  "naturgesetz" = "natur_gesetz"
)

election_data_t <- election_data |>
  map("result") |>
  map( ~ .x |>
         rename_with(str_to_lower) |>
         rename(any_of(renames)))

# Create one dataframe ----------------------------------------------------


# Combine all data frames into a single data frame
comb <- election_data_t %>%
  bind_rows()

glimpse(comb)

inspect <- comb |>
  select(land, lfd_nr, statistische_kennziffer, statistische_kennziffer_bundestagswahl_1990)

# Create ags out of land & lfd_nr
comb2 <- comb |>
  mutate(
    land = as.numeric(land),
    statistische_kennziffer = as.numeric(statistische_kennziffer),
    statistische_kennziffer_bundestagswahl_1990 = as.numeric(statistische_kennziffer_bundestagswahl_1990),
    ags = paste0(land, lfd_nr),
    ags = as.numeric(ags), 
    ags = coalesce(ags, statistische_kennziffer, statistische_kennziffer_bundestagswahl_1990)
    ) |>
  select(-c(statistische_kennziffer_bundestagswahl_1990, statistische_kennziffer)) |>
  # filter all non numeric ags and ags that are smaller than 4 digits
  filter(!is.na(ags) & ags >= 1000)

## Some inspections
inspect <- comb2 |>
  select(ags, year, land, lfd_nr)

inspect_na <- comb2 |>
  filter(is.na(ags))
# zero

glimpse(comb2)

# Check duplicates
dupl <- comb2 |>
  count(ags, year) |>
  filter(n>1)
nrow(dupl)
# zero



# Some transformations ----------------------------------------------------

comb2$as
sort(names(comb2))

df <- comb2 |>
  # Get Bundesland / state from ags
  mutate(
    state = pad_zero_conditional(str_sub(ags, end = -4), 1),
    state_name = state_id_to_names(state)
    ) |>
  # Organize
  select(
    # Background
    ags, year, state, state_name, eligible_voters, number_voters, valid_votes, invalid_votes,
    # Main
    cdu, csu, spd, gruene, fdp, linke_pds, b90_gr,
    # Right-wing
    afd, npd, rep, die_rechte, dvu, iii_weg, bf_b, ddd, dg, dns, drp, dsu, # fap
    # Left-wing
    dkp, kpd, mlpd, sgp, spad, bsa, bwk, #  kbw, v
    # Others
    `50plus`, ab_2000, ad_demokraten, adf, adm, agfg, apd, ags, appd, aufbruch, b, bge, big, bp, buendnis_c, buendnis21, 
    buergerbewegung, bueso, cbv, chance_2000, cm, deutschland, di_b, die_basis, die_humanisten, die_partei, dm, dp, dpd, 
    du, eap, familie, forum, frauen, freie_waehler, fwd, gartenpartei, gb_bhe, gdp, gesundheitsforschung, graue, gvp, hp,
    liebe, liga, lkr,  menschliche_welt, mg, muendige, naturgesetz, nichtwaehler, oedp, offensive_d, oko_union, 
    partei_der_vernunft, pass, patrioten, pbc, pd_f, pdv, piraten, prg, pro_deutschland, pro_dm, rentner, rrp, schill, 
    ssw, statt_partei, team_todenhoefer, tierschutz, tierschutzallianz, unabhaengige, usd, v_partei3, vaa, violetten, 
    volksabstimmung, volt, zentrum 
      # asd, bfb, buergerpartei, dib, lfk, oeko_union, pdf, ust,
    ) %>%
  # from eligible_voters:zentrum as numeric
  mutate(across(eligible_voters:ncol(.), as.numeric)) %>%
  ungroup() %>%
  # Calculate extremist votes
  mutate(
    far_right = rowSums(select(., afd:dsu), na.rm = TRUE),
    far_left = rowSums(select(., dkp:bwk), na.rm = TRUE)
  ) %>%
  # Left wing with votes for Linke/PDS
  mutate(far_left_wLinke = rowSums(select(., linke_pds, far_left), na.rm = TRUE))
### Extremist parties
## Right wing
# npd, fap (freiheitliche deutsche arbeiterpartei), rep (die republikaner),
# dvu, ddd (bund der deutschen demokraten), dns (dachverband der nationalen sammlung),
# dg (deutsche gemeinschaft), drp (deutsche reichspartei),
# bf_b (bund freier bürger),
# die rechte, iii. weg, dsu (deutsche soziale union)
## Left wing
# dkp, kpd, v, kbw (kommunistischer bund westdeutschland),
# bwk (bund westdeutscher kommunisten),
# spad (spartakist-arbeiterparte deutschlands), 
# bsa (bund sozialistischer arbeiter),
# sgp  (= psg; sozialistische gleichheitspartei)
# (Linke/PDS) not entirely left-wing but parts of it (solid - youth organization) are investigted by Verfassungsschutz


# Some transformations
df <- df %>%
  # Harmonize Grüne: Grüne + B90/Gr ran in 1990 elections
  mutate(
    gruene = rowSums(select(., `b90_gr`, gruene), na.rm = TRUE),
    # Calculate CDU/CSU
    cdu_csu = ifelse(csu > 0, csu, cdu),
    # Get CDU & CSU votes for (non-)Bavarian municipalities if cdu is NA
    cdu = ifelse(is.na(cdu) & state != "09" & !is.na(cdu_csu), cdu_csu, cdu),
    csu = ifelse(is.na(csu) & state == "09" & !is.na(cdu_csu), cdu_csu, csu)
  ) |>
  select(-b90_gr)

glimpse(df)

# Cases with 0 eligible voters --------------------------------------------

(zero_elig <- df |> filter(eligible_voters == 0))
# 6 ags (all in 2021) for which there are zero eligible voters registered.
zero_elig |>
  select(ags, year)
# four counties in east germany in 1994 + 2 counties in east germany in 1998

# inspect
df94 <- fread("data/federal_elections/county_level/raw/btw1994kreis.csv")

df94 |> filter(V2 %in% c("12999", "13999", "14999", "15999")) |>
  select(V2, V3)
# nicht zuordenbare Briefwahl


# Inspections -------------------------------------------------------------

# Check duplicates
dupl <- df |>
  count(ags, year) |>
  filter(n>1)
nrow(dupl)
# zero

# counties per year
df |>
  group_by(year) |>
  summarise(n = n()) |>
  print(n = Inf)


# Party votes to NA if no votes in year -----------------------------------

# Identify parties that did not receive any votes in a given election year
no_votes_parties <- df %>%
  group_by(year) %>%
  summarise(across(cdu:zentrum, ~ all(. == 0), .names = "all_zero_{col}")) %>%
  pivot_longer(cols = starts_with("all_zero_"), names_to = "party", values_to = "all_zero") %>%
  mutate(party = sub("all_zero_", "", party)) %>%
  select(year, party, all_zero)

# Recode 0 vote shares to NA for parties that did not receive any votes in an election year
df <- df %>%
  pivot_longer(cols = cdu:zentrum, names_to = "party", values_to = "vote_share") %>%
  left_join(no_votes_parties, by = c("year", "party")) %>%
  mutate(vote_share = if_else(all_zero == TRUE & vote_share == 0, NA_real_, vote_share)) %>%
  select(-all_zero) %>%
  pivot_wider(names_from = "party", values_from = "vote_share")

glimpse(df)


# Vote shares + turnout ---------------------------------------------------

df <- df |>
  mutate(
    across(far_right:zentrum, ~ .x / number_voters),
    turnout = number_voters / eligible_voters
  )

# turnout > 1?
df |>
  filter(turnout > 1)
# undefined mail in districts from above

# Relocate
df <- df |>
  select(ags:invalid_votes, 
         turnout,
         cdu:zentrum, 
         cdu_csu, far_right:far_left_wLinke)




# Write -------------------------------------------------------------------

glimpse(df)

# pad zero to ags
df <- df |>
  mutate(ags = pad_zero_conditional(ags, 4))

# clean names
df <- df |>
  janitor::clean_names()

names(df)

write_rds(df, "data/federal_elections/county_level/final/federal_cty_unharm.rds")
fwrite(df, "data/federal_elections/county_level/final/federal_cty_unharm.csv")


# Inspect -----------------------------------------------------------------

df <- read_rds("data/federal_elections/county_level/final/federal_cty_unharm.rds")

names(df)

### END