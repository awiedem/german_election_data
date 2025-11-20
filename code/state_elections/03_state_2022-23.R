#### Preparations ####
## General Information
# Script written by Maurice Baudet von Gersdorff
# Contact information: https://m.baudetvg.net
# Date: 2025-10-19

### Machine Specifications
# This Script was reated and therefore extensively tested using a machine with
# the following specifications:

# OS: Ubuntu 24.04.3 LTS
# Kernel: 6.8.0-85-generic
# CPU: AMD Ryzen 7 7840U w/ Radeon 780M Graphics
# Physical RAM: 2×32 GB DDR5, 5600 MT/s
# Available RAM: 54.7 GB
# R version: 4.5.2 (2025-10-31)
# RStudio version: 2025.9.2.418

# Script runtime: ~ 4 seconds

## Clear Everything
rm(list = ls())

## Set English Locale
Sys.setlocale('LC_TIME', 'en_US.UTF-8')

## Packages Setup
packages <- c(
  ### Special Packages
  'readxl',
  'xml2',

  ### Standard Packages
  'here',
  'janitor',
  'tidyverse'
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

rm('packages', 'pkg')

## Working Directory Setup
# If you use 'german_election_data.Rproj', the following should suffice.
# Otherwise, set the working directory to the repository's root folder.

setwd(here())

path = 'data/state_elections/raw/Landtagswahlen'

#### Bavaria ####
## Party List
by23_partylist <- c(
  'cdu',
  'csu',
  'spd',
  'grune',
  'fdp',
  'linke_pds',
  'afd',
  'freie_wahler',
  'bp',
  'odp',
  'die_humanisten',
  'die_partei',
  'v_partei3',
  'cdu_csu'
)

## Read Raw File
by23_data <- read_xlsx(
  path = here(
    path,
    'Bayern/Selbst recherchiert/Bayern_2023_Landtagswahl.xlsx'
  ),
  sheet = 1,
  skip = 1
) |>
  select(
    -18:-35, # data on direct candidate votes is not needed
  ) |>
  clean_names()

## Clean Dataset
by23_data <- by23_data |>
  ### Clean Names of Party Columns
  rename_with(
    ~ str_remove(., "_(3[0-9]|4[0-9]|5[0-9])$"),
    .cols = matches("_(3[0-9]|4[0-9]|5[0-9])$")
  ) |>
  rename(
    afd = af_d,
    linke_pds = die_linke,
    die_humanisten = pd_h
  ) |>
  ### General Cleaning
  mutate(
    #### Add Needed Variables
    ags = as.character(paste0('09', regional_schlussel)),
    county = as.character(substr(ags, 1, 5)),
    election_year = as.numeric(2023),
    election_date = as.Date('2023-10-08'),
    state = as.character('09'),
    cdu = as.numeric(NA),
    cdu_csu = csu,
  )

### Get Checksum
# The raw file contains inconsistencies. There are observations with
# number_of_voters that is not equal to the sum of valid_votes and
# invalid_votes. For this reason, a checksum is saved early on for later
# verification.

by23_checksum <- by23_data |>
  summarise(
    total_number_voters = sum(wahler_insgesamt_b_b1_b2_b3, na.rm = TRUE),
    total_valid_votes = sum(gultige_zweitstimmen_zusammen_d, na.rm = TRUE),
    total_invalid_votes = sum(ungultige_zweitstimmen_c, na.rm = TRUE)
  ) |>
  mutate(
    difference = (total_valid_votes + total_invalid_votes) - total_number_voters
  ) |>
  pull(difference)

### Handle `ags` for Kreisfreie Städte
by23_kreisfrei <- c(
  "09361000", # Amberg
  "09561000", # Ansbach
  "09661000", # Aschaffenburg
  "09761000", # Augsburg
  "09462000", # Bayreuth
  "09463000", # Coburg
  "09562000", # Erlangen
  "09563000", # Fürth
  "09464000", # Hof
  "09161000", # Ingolstadt
  "09762000", # Kaufbeuren
  "09763000", # Kempten (Allgäu)
  "09261000", # Landshut
  "09764000", # Memmingen
  "09162000", # München
  "09564000", # Nürnberg
  "09262000", # Passau
  "09362000", # Regensburg
  "09163000", # Rosenheim
  "09565000", # Schwabach
  "09662000", # Schweinfurt
  "09263000", # Straubing
  "09363000", # Weiden in der Oberpfalz
  "09663000" # Würzburg
)

by23_data <- by23_data |>
  mutate(
    ags = if_else(
      substr(ags, 1, 5) %in% substr(by23_kreisfrei, 1, 5),
      by23_kreisfrei[match(substr(ags, 1, 5), substr(by23_kreisfrei, 1, 5))],
      ags
    )
  )

### Rename Variables For Consistency With Other Datasets
by23_data <- by23_data |>
  rename(
    eligible_voters = stimmberechtigte_insgesamt_a_a1_a2_a3,
    number_voters = wahler_insgesamt_b_b1_b2_b3,
    valid_votes = gultige_zweitstimmen_zusammen_d,
    invalid_votes = ungultige_zweitstimmen_c,
    voters_wo_blockingnotice = stimmberechtigte_ohne_sperrvermerk_w_a1,
    voters_blockingnotice = stimmberechtigte_mit_sperrvermerk_w_a2,
    voters_par22_2 = stimmberechtigte_nach_22_abs_2_lwo_a3,
    # corresponds in substance to voters_par25_2 in `federal_muni_unharm`
    voters_w_ballot = wahler_mit_wahlschein_b2
  ) |>
  ### Get Rid of Not Needed Variables
  select(
    ags,
    county,
    election_year,
    election_date,
    state,
    eligible_voters,
    number_voters,
    valid_votes,
    invalid_votes,
    voters_wo_blockingnotice,
    voters_blockingnotice,
    voters_par22_2,
    voters_w_ballot,
    all_of(by23_partylist),
    art_des_stimmbezirks,
    gemeinde_name
  )

## Check for Problematic Mail-In Votes
# If art_des_stimmbezirks == 0, it is a normal voting district. For
# art_des_stimmbezirks %in% c(1, 5, 6), eligible_voters == 0 &
# number_voters != 0, indicate that these are likely artificial mail-in vote
# districts. All other types are special in some way, with
# eligible_voters > number_voters. Therefore, for every art_des_stimmbezirks, if
# the corresponding AGS occurs multiple times, one can simply summarize by AGS.

by23_mailcheck <- by23_data |>
  group_by(ags) |>
  filter(art_des_stimmbezirks %in% c(1, 5, 6) & n() == 1) |>
  ungroup()

if (nrow(by23_mailcheck) == 0) {
  cat("There are no problematic observations.\n")
} else {
  cat("There are", nrow(by23_mailcheck), "problematic observations.\n")
}

rm(by23_mailcheck)

## Aggregate to Municipality Level
by23_data <- by23_data |>
  group_by(
    ags,
    county,
    gemeinde_name,
    election_year,
    state,
    election_date
  ) |>
  summarize(
    across(
      .cols = where(is.numeric),
      .fns = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(cdu = NA) |>
  arrange(ags)

## Check for Problematic Aggregation
by23_checkdata <- by23_data |>
  summarise(
    check_data = sum(
      (valid_votes + invalid_votes) - number_voters,
      na.rm = TRUE
    )
  ) |>
  pull(check_data)

if (by23_checkdata == by23_checksum) {
  cat('There is no problem.\n')
} else {
  cat("Houston, we've got a problem!\n")
}

## Calculations
by23_data <- by23_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(by23_partylist), ~ .x / valid_votes)
  ) |>
  ### Final Selecting (and Arranging) of Variables
  select(
    ags,
    county,
    election_year,
    state,
    election_date,
    eligible_voters,
    number_voters,
    valid_votes,
    turnout,
    all_of(by23_partylist),
    cdu_csu
  )

### Final Check
by23_data |>
  group_by(ags) |>
  summarize(n_rows = n()) |>
  filter(n_rows > 1)

rm(by23_checkdata, by23_checksum, by23_kreisfrei)

by23_totalvoters <- by23_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (by23_totalvoters == 9430600) {
  cat('There is no problem.\n')
} else {
  cat("Houston, we've got a problem!\n")
}

#### Hesse ####
## Party List
he23_partylist <- c(
  'cdu',
  'csu',
  'spd',
  'grune',
  'fdp',
  'linke_pds',
  'afd',
  'freie_wahler',
  'tierschutzpartei',
  'die_partei',
  'piraten',
  'odp',
  'verjungungsforschung',
  'v_partei3',
  'die_humanisten',
  'abg',
  'appd',
  'diebasis',
  'dkp',
  'neue_mitte',
  'volt',
  'klimaliste',
  'cdu_csu'
)

## Read Raw File
he23_data <- read_csv2(
  here(
    path,
    'Hessen/Hessen_2023_Landtagswahl.csv'
  ),
  skip = 1
) |>
  clean_names() |>
  slice(-1:-499) |> # loose pre-aggregated Data
  select(
    -matches('wahlkreisstimmen'), # data on direct candidate votes is not needed
    -matches('landesstimmen_percent'),
    -matches('_gewonnen_'),
    -matches('anzahl_wahlbezirke'),
    -'letzte_anderung',
    -'freigegeben',
    -'name_aufnehmender_wahlbezirk'
  )

## Clean Dataset
he23_data <- he23_data |>
  ### Clean Names of Party Columns
  rename_with(
    ~ str_remove(., "_landesstimmen$"),
    .cols = matches("_landesstimmen$")
  ) |>
  rename(
    afd = af_d,
    linke_pds = die_linke,
    die_humanisten = pd_h,
    diebasis = die_basis,
    neue_mitte = die_neue_mitte,
    klimaliste = klimaliste_wahlerl
  ) |>
  ### General Cleaning
  mutate(
    #### Add Needed Variables
    ags = as.character(paste0('06', substr(gebietsschlussel, 4, 9))),
    county = as.character(substr(ags, 1, 5)),
    election_year = as.numeric(2023),
    election_date = as.Date('2023-10-08'),
    state = as.character('06'),
    csu = as.numeric(NA),
    cdu_csu = cdu,
  ) |>
  ### Clean Parties w/o Landesstimmen
  select(
    -'bundnis_c',
    -'wdmr',
    -'bundespa_klimaliste',
    -'mera25',
    -'nev',
    -'pp',
    -'sgv',
    -'solibew'
  )

he23_data <- he23_data |>
  ### Rename Variables For Consistency With Other Datasets
  rename(
    eligible_voters = wahlberechtigte,
    number_voters = wahlerinnen_und_wahler,
    valid_votes = gultige,
    invalid_votes = ungultige,
    voters_wo_blockingnotice = wahlberechtigte_ohne_sperrvermerk,
    voters_blockingnotice = wahlberechtigte_mit_sperrvermerk,
    voters_par12a_2 = wahlberechtigte_nach_12a_2_lwo,
    # corresponds in substance to voters_par25_2 in `federal_muni_unharm`
    voters_w_ballot = wahlerinnen_und_wahler_mit_wahlschein
  ) |>
  ### Get Rid of Not Needed Variables
  select(
    ags,
    county,
    election_year,
    election_date,
    state,
    eligible_voters,
    number_voters,
    valid_votes,
    invalid_votes,
    voters_wo_blockingnotice,
    voters_blockingnotice,
    voters_par12a_2,
    voters_w_ballot,
    all_of(he23_partylist),
    wahllokal,
    wahlbezirksnummer,
    nummer_aufnehmender_wahlbezirk,
    aufnahme_wahlbezirk
  )

## Check for Problematic Voting Districts
# If aufnahme_wahlbezirk = 1, votes from another voting district are added.
# If aufnahme_wahlbezirk = 2, votes are assigned to another electoral district.
# If the receiving electoral district is in the same ags, there is no problem
# because the data is aggregated at the municipality level anyway.

he23_vodicheck <- he23_data |>
  filter(aufnahme_wahlbezirk == 2) |>
  anti_join(
    he23_data |> filter(aufnahme_wahlbezirk == 1),
    by = "ags"
  )

if (nrow(he23_vodicheck) == 0) {
  cat("There are no problematic observations.\n")
} else {
  cat("There are", nrow(he23_vodicheck), "problematic observations.\n")
}

rm(he23_vodicheck)

## Aggregate to Municipality Level
he23_data <- he23_data |>
  group_by(
    ags,
    county,
    election_year,
    state,
    election_date
  ) |>
  summarize(
    across(
      .cols = where(is.numeric),
      .fns = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(cdu = NA) |>
  arrange(ags)

## Calculations
he23_data <- he23_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(he23_partylist), ~ .x / valid_votes)
  ) |>
  ### Final Selecting (and Arranging) of Variables
  select(
    ags,
    county,
    election_year,
    state,
    election_date,
    eligible_voters,
    number_voters,
    valid_votes,
    turnout,
    all_of(he23_partylist),
    cdu_csu
  )

### Final Check
he23_data |>
  group_by(ags) |>
  summarize(n_rows = n()) |>
  filter(n_rows > 1)

he23_totalvoters <- he23_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (he23_totalvoters == 4332235) {
  cat('There is no problem.\n')
} else {
  cat("Houston, we've got a problem!\n")
}

#### Lower Saxony ####
## Party List
ni22_partylist <- c(
  'cdu',
  'csu',
  'spd',
  'grune',
  'fdp',
  'linke_pds',
  'afd',
  'diebasis',
  'die_humanisten',
  'die_partei',
  'freie_wahler',
  'verjungungsforschung',
  'piraten',
  'tierschutz',
  'volt',
  'cdu_csu'
)

## Read Raw File
ni22_xml <- read_xml(
  here(path, 'Niedersachsen/Niedersachsen_2022_Landtagswahl_ZS.xml')
)

## Get Data.Frame from XML
ni22_ns <- c(ss = 'urn:schemas-microsoft-com:office:spreadsheet')

ni22_rows <- xml_find_all(
  ni22_xml,
  './/ss:Worksheet/ss:Table/ss:Row',
  ni22_ns
)

extract_ni22row <- function(row) {
  cells <- xml_find_all(row, './/ss:Cell/ss:Data', ni22_ns)
  map_chr(cells, xml_text)
}

ni22_list <- ni22_rows |>
  map(extract_ni22row)

ni22_maxlen <- max(lengths(ni22_list))

ni22_list <- map(
  ni22_list,
  ~ {
    length(.x) <- ni22_maxlen
    .x
  }
)

ni22_data <- ni22_list |>
  do.call(what = rbind) |>
  as_tibble(.name_repair = 'unique') |>
  slice(-(1:10))

rm(ni22_list, ni22_rows, ni22_xml, ni22_maxlen, ni22_ns)

## Adjust Value Positions
ni22_movers <- ni22_data[2, 1:6] |> as.list()
ni22_data[1, 5:(5 + length(ni22_movers) - 1)] <- ni22_movers

ni22_movers <- ni22_data[3, 1:43] |> as.list()
ni22_data[1, 10:(10 + length(ni22_movers) - 1)] <- ni22_movers

rm(ni22_movers)

## Clean Dataset
ni22_data <- ni22_data |>
  slice(-(2:4)) |>
  row_to_names(row_number = 1) |>
  rename(ags = 1) |>
  rename_with(~ gsub('-', '', .)) |>
  clean_names() |>
  filter(!str_detect(ags, '%')) |>
  select(
    -where(~ all(is.na(.))),
    -sonst_ige
  ) |>
  mutate(
    ags = lag(gsub('[^0-9]', '', ags), 1),
    across(everything(), ~ na_if(., '-')),
    csu = as.numeric(NA),
    cdu_csu = cdu
  ) |>
  filter(!str_detect(ags, 'Anzahl')) |>
  filter(!if_all(-ags, is.na)) |>
  ### Rename Variables For Consistency With Other Datasets
  rename(
    eligible_voters = wahl_berech_tigte,
    valid_votes = gultige_stimmen,
    number_voters = wahler_innen_und_wahler_wahl_betei_ligung,
    afd = af_d,
    diebasis = die_basis_lv,
    die_humanisten = die_human_isten,
    linke_pds = die_linke_1,
    verjungungsforschung = gesund_heits_forsch,
    piraten = pi_raten,
    tierschutz = tier_schutz_partei,
  ) |>
  ### Various Mutatations
  mutate(
    state = as.character('03'),
    ags = paste0(state, ags),
    ags = str_pad(ags, width = 8, side = 'right', pad = '0'),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2022),
    election_date = as.Date('2022-10-09'),
    eligible_voters = as.numeric(eligible_voters),
    number_voters = as.numeric(number_voters),
    valid_votes = as.numeric(valid_votes),
    across(all_of(ni22_partylist), as.numeric)
  ) |>
  ### Get Rid of Not Needed Variables
  select(
    ags,
    county,
    election_year,
    election_date,
    state,
    eligible_voters,
    number_voters,
    valid_votes,
    all_of(ni22_partylist)
  )

## Handle Aggregated AGS
# The original XML file also contains data aggregated at higher administrative
# levels. This data must therefore be filtered out. To do this, municipal
# structure data already contained in the repository is used. The last municipal
# reform in Lower Saxony took place in 2021, so municipal structure data from
# 2024 can be used.

ni22_ags <- read_excel(
  here(
    path,
    '../../../covars_municipality/raw/municipality_sizes',
    'AuszugGV4QAktuell_2024.xlsx'
  ),
  sheet = 2,
  skip = 5,
  col_names = FALSE
) |>
  filter(...1 == 60, ...3 == '03') |>
  mutate(ags = paste0(...3, ...4, ...5, ...7)) |>
  select(ags)

ni22_data <- ni22_data |>
  filter(ags %in% ni22_ags$ags)

rm(ni22_ags)

## Calculations
ni22_data <- ni22_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(ni22_partylist), ~ .x / valid_votes)
  ) |>
  ### Final Selecting (and Arranging) of Variables
  select(
    ags,
    county,
    election_year,
    state,
    election_date,
    eligible_voters,
    number_voters,
    valid_votes,
    turnout,
    all_of(ni22_partylist),
    cdu_csu
  )

## Final Check
ni22_totalvoters <- ni22_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (ni22_totalvoters == 6064738) {
  cat('There is no problem.\n')
} else {
  cat("Houston, we've got a problem!\n")
}

#### Bind and Write ####
state2223 <- bind_rows(by23_data, he23_data, ni22_data)

write_csv(
  state2223,
  here(
    path,
    '../../../state_elections/final',
    'state_2223_unharm.csv'
  )
)

write_rds(
  state2223,
  here(
    path,
    '../../../state_elections/final',
    'state_2223_unharm.rds'
  )
)


# Inspect
state2223 <- read_rds(here(path, '../../../state_elections/final', 'state_2223_unharm.rds'))

glimpse(state2223)
