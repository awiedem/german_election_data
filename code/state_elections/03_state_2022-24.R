#### Preparations ####
## General Information
# Script written by Maurice Baudet von Gersdorff
# Contact information: https://m.baudetvg.net
# Date: 2025-10-19

### Machine Specifications
# This Script was created and therefore extensively tested using a machine with
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
Sys.setlocale("LC_TIME", "en_US.UTF-8")

## Packages Setup
packages <- c(
  ### Special Packages
  "readxl",
  "xml2",

  ### Standard Packages
  "here",
  "janitor",
  "tidyverse"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

rm("packages", "pkg")

## Working Directory Setup
# If you use 'german_election_data.Rproj', the following should suffice.
# Otherwise, set the working directory to the repository's root folder.

setwd(here())

path <- "data/state_elections/raw/Landtagswahlen"

#### Bavaria ####
## Party List
by23_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "bp",
  "odp",
  "die_humanisten",
  "die_partei",
  "v_partei3",
  "cdu_csu"
)

## Read Raw File
by23_data <- read_xlsx(
  path = here(
    path,
    "Bayern/Selbst recherchiert/Bayern_2023_Landtagswahl.xlsx"
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
    ags = as.character(paste0("09", regional_schlussel)),
    county = as.character(substr(ags, 1, 5)),
    election_year = as.numeric(2023),
    election_date = as.Date("2023-10-08"),
    state = as.character("09"),
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

by23_data |>
  filter(ags == "09564000") |>
  glimpse()

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

## Fill NA gemeinde_name values before aggregation
# Some voting districts have NA gemeinde_name but belong to the same AGS
# Fill with the most common gemeinde_name for each AGS to avoid duplicate rows
by23_data <- by23_data |>
  group_by(ags) |>
  mutate(
    gemeinde_name = if_else(
      is.na(gemeinde_name),
      names(sort(table(gemeinde_name), decreasing = TRUE))[1],
      gemeinde_name
    )
  ) |>
  ungroup()

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

by23_data |>
  filter(ags == "09564000") |>
  glimpse()

glimpse(by23_data)

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
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

## Calculations
by23_data <- by23_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(by23_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
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
    other,
    cdu_csu
  )

### Final Check
by23_data |>
  group_by(ags) |>
  summarize(n_rows = n()) |>
  filter(n_rows > 1)


# Duplicates?
by23_data %>%
  group_by(ags, election_year) %>%
  summarise(n = n()) %>%
  filter(n > 1)

by23_data %>%
  filter(ags == "09564000") |>
  glimpse()


rm(by23_checkdata, by23_checksum, by23_kreisfrei)

by23_totalvoters <- by23_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (by23_totalvoters == 9430600) {
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

# duplicates?
by23_data %>%
  group_by(ags, election_year) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# none

#### Hesse ####
## Party List
he23_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "tierschutzpartei",
  "die_partei",
  "piraten",
  "odp",
  "verjungungsforschung",
  "v_partei3",
  "die_humanisten",
  "abg",
  "appd",
  "diebasis",
  "dkp",
  "neue_mitte",
  "volt",
  "klimaliste",
  "cdu_csu"
)

## Read Raw File
he23_data <- read_csv2(
  here(
    path,
    "Hessen/Hessen_2023_Landtagswahl.csv"
  ),
  skip = 1
) |>
  clean_names() |>
  slice(-1:-499) |> # loose pre-aggregated Data
  select(
    -matches("wahlkreisstimmen"), # data on direct candidate votes is not needed
    -matches("landesstimmen_percent"),
    -matches("_gewonnen_"),
    -matches("anzahl_wahlbezirke"),
    -"letzte_anderung",
    -"freigegeben",
    -"name_aufnehmender_wahlbezirk"
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
    ags = as.character(paste0("06", substr(gebietsschlussel, 4, 9))),
    county = as.character(substr(ags, 1, 5)),
    election_year = as.numeric(2023),
    election_date = as.Date("2023-10-08"),
    state = as.character("06"),
    csu = as.numeric(NA),
    cdu_csu = cdu,
  ) |>
  ### Clean Parties w/o Landesstimmen
  select(
    -"bundnis_c",
    -"wdmr",
    -"bundespa_klimaliste",
    -"mera25",
    -"nev",
    -"pp",
    -"sgv",
    -"solibew"
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
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
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
    other,
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
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

#### Lower Saxony ####
## Party List
ni22_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "diebasis",
  "die_humanisten",
  "die_partei",
  "freie_wahler",
  "verjungungsforschung",
  "piraten",
  "tierschutz",
  "volt",
  "cdu_csu"
)

## Read Raw File
ni22_xml <- read_xml(
  here(path, "Niedersachsen/Niedersachsen_2022_Landtagswahl_ZS.xml")
)

## Get Data.Frame from XML
ni22_ns <- c(ss = "urn:schemas-microsoft-com:office:spreadsheet")

ni22_rows <- xml_find_all(
  ni22_xml,
  ".//ss:Worksheet/ss:Table/ss:Row",
  ni22_ns
)

extract_ni22row <- function(row) {
  cells <- xml_find_all(row, ".//ss:Cell/ss:Data", ni22_ns)
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
  as_tibble(.name_repair = "unique") |>
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
  rename_with(~ gsub("-", "", .)) |>
  clean_names() |>
  filter(!str_detect(ags, "%")) |>
  select(
    -where(~ all(is.na(.))),
    -sonst_ige
  ) |>
  mutate(
    ags = lag(gsub("[^0-9]", "", ags), 1),
    across(everything(), ~ na_if(., "-")),
    csu = as.numeric(NA),
    cdu_csu = cdu
  ) |>
  filter(!str_detect(ags, "Anzahl")) |>
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
    state = as.character("03"),
    ags = paste0(state, ags),
    ags = str_pad(ags, width = 8, side = "right", pad = "0"),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2022),
    election_date = as.Date("2022-10-09"),
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
    "../../../covars_municipality/raw/municipality_sizes",
    "AuszugGV4QAktuell_2024.xlsx"
  ),
  sheet = 2,
  skip = 5,
  col_names = FALSE
) |>
  filter(...1 == 60, ...3 == "03") |>
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
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
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
    other,
    cdu_csu
  )

## Final Check
ni22_totalvoters <- ni22_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (ni22_totalvoters == 6064738) {
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

#### Saarland ####
## Party List
sl22_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "piraten",
  "diebasis",
  "odp",
  "die_humanisten",
  "die_partei",
  "verjungungsforschung",
  "tierschutz",
  "volt",
  "cdu_csu"
)

## Read Raw File
# File has 5 header lines (title, subtitle, party names, "Stimmen", "Endgültig/Vorperiode")
# Data is semicolon-delimited with paired columns (current/previous election)
sl22_raw <- read_delim(
  here(path, "Saarland/Saarland_2022_Landtagswahl.csv"),
  delim = ";",
  skip = 5,
  col_names = FALSE,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

## Clean Dataset
# Column layout: Nr(1), Gebiet(2), gehört_zu(3), then paired party columns (Endgültig, Vorperiode)
# We only want the "Endgültig" (current election) values = even-indexed columns (4,6,8,...)
# 18 parties + 5 admin metrics (Wahlberechtigte, Wähler, Ungültige, Gültige, Übrige) = 23 pairs
# Cols 4-49 contain data; col 50 is trailing empty from semicolon delimiter
endgultig_cols <- seq(4, 48, by = 2)
sl22_data <- sl22_raw |>
  select(1, 2, 3, all_of(endgultig_cols))

# Assign column names
colnames(sl22_data) <- c(
  "nr", "gebiet", "gehort_zu",
  "cdu", "spd", "linke_pds", "afd", "grune", "fdp",
  "familien_partei", "piraten", "freie_wahler", "diebasis",
  "bunt_saar", "odp", "die_humanisten", "die_partei",
  "verjungungsforschung", "tierschutz", "sgv", "volt",
  "eligible_voters", "number_voters", "invalid_votes", "valid_votes", "ubrige"
)

## Filter to municipality-level rows only
# gehört_zu == 10 means it IS a Wahlkreis aggregate; no gehört_zu = state total
# Municipality rows have gehört_zu in {1, 2, 3} (Wahlkreis they belong to)
# Also remove empty separator rows
sl22_data <- sl22_data |>
  mutate(across(where(is.character), ~ na_if(., ""))) |>
  filter(!is.na(nr)) |>
  filter(gehort_zu %in% c(1, 2, 3))

## Construct AGS and other variables
sl22_data <- sl22_data |>
  mutate(
    ags = paste0("10", str_pad(nr, width = 6, side = "left", pad = "0")),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2022),
    election_date = as.Date("2022-03-27"),
    state = as.character("10"),
    csu = as.numeric(NA),
    cdu_csu = as.numeric(cdu),
    across(
      c(cdu, spd, linke_pds, afd, grune, fdp, piraten, freie_wahler,
        diebasis, odp, die_humanisten, die_partei, verjungungsforschung,
        tierschutz, volt, eligible_voters, number_voters, valid_votes),
      as.numeric
    )
  )

## Calculations
sl22_data <- sl22_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(sl22_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(sl22_partylist),
    other,
    cdu_csu
  )

### Final Check
sl22_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

sl22_totalvoters <- sl22_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (sl22_totalvoters == 746307) {
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

#### Schleswig-Holstein ####
## Party List
sh22_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "piraten",
  "freie_wahler",
  "die_partei",
  "diebasis",
  "die_humanisten",
  "verjungungsforschung",
  "tierschutzpartei",
  "volt",
  "cdu_csu"
)

## Read Raw File
# Polling-station level data with separate Briefwahl rows
# Row 1 = title, Row 2 = column headers -> skip = 2
sh22_raw <- read_xlsx(
  path = here(
    path,
    "Schleswig-Holstein/Schleswig-Holstein_2022_Landtag.xlsx"
  ),
  sheet = "2022",
  skip = 2,
  col_names = FALSE
)

## Assign column names
colnames(sh22_raw)[1:6] <- c(
  "kennziffer", "kreisname", "gemeindename", "wbz_name",
  "amterschlussel", "amtername"
)
colnames(sh22_raw)[c(11, 12, 14, 17, 18)] <- c(
  "ev_without_w", "ev_with_w", "eligible_voters",
  "brief_voters", "number_voters"
)
colnames(sh22_raw)[c(38, 39)] <- c("invalid_zweit", "valid_zweit")
colnames(sh22_raw)[40:55] <- c(
  "cdu", "spd", "grune", "fdp", "afd", "linke_pds", "ssw",
  "piraten", "freie_wahler", "die_partei", "zentrum", "diebasis",
  "die_humanisten", "verjungungsforschung", "tierschutzpartei", "volt"
)

## Convert to numeric
sh22_raw <- sh22_raw |>
  mutate(across(
    c(
      eligible_voters, ev_with_w, number_voters, valid_zweit,
      cdu, spd, grune, fdp, afd, linke_pds, ssw, piraten, freie_wahler,
      die_partei, zentrum, diebasis, die_humanisten, verjungungsforschung,
      tierschutzpartei, volt
    ),
    as.numeric
  ))

## Separate regular and Briefwahl rows
# Digit 3 of kennziffer: 9 = Briefwahl, 0/1 = regular
sh22_raw <- sh22_raw |>
  mutate(is_brief = substr(kennziffer, 3, 3) == "9")

sh22_regular <- sh22_raw |> filter(!is_brief)
sh22_brief_gem <- sh22_raw |>
  filter(is_brief & !is.na(gemeindename))
sh22_brief_amt <- sh22_raw |>
  filter(is_brief & is.na(gemeindename) & !is.na(amterschlussel))

## Allocate Amt-level Briefwahl to municipalities
# Aggregate Briefwahl by Amt
sh22_amt_brief <- sh22_brief_amt |>
  group_by(amterschlussel) |>
  summarize(
    across(
      c(
        number_voters, valid_zweit, cdu, spd, grune, fdp, afd, linke_pds,
        ssw, piraten, freie_wahler, die_partei, zentrum, diebasis,
        die_humanisten, verjungungsforschung, tierschutzpartei, volt
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Get municipality weights within each Amt (based on Wahlschein voters)
sh22_gem_weights <- sh22_regular |>
  filter(!is.na(gemeindename) & !is.na(amterschlussel)) |>
  group_by(amterschlussel, kreisname, gemeindename) |>
  summarize(ev_with_w = sum(ev_with_w, na.rm = TRUE), .groups = "drop") |>
  group_by(amterschlussel) |>
  mutate(weight = ev_with_w / sum(ev_with_w, na.rm = TRUE)) |>
  ungroup()

# Distribute Amt-level Briefwahl proportionally
sh22_brief_alloc <- sh22_gem_weights |>
  inner_join(sh22_amt_brief, by = "amterschlussel", suffix = c("_gem", "_brief")) |>
  mutate(
    across(
      c(
        number_voters, valid_zweit, cdu, spd, grune, fdp, afd, linke_pds,
        ssw, piraten, freie_wahler, die_partei, zentrum, diebasis,
        die_humanisten, verjungungsforschung, tierschutzpartei, volt
      ),
      ~ round(.x * weight)
    )
  ) |>
  mutate(eligible_voters = 0) |>
  select(
    kreisname, gemeindename, eligible_voters, number_voters,
    valid_zweit, cdu, spd, grune, fdp, afd, linke_pds, ssw,
    piraten, freie_wahler, die_partei, zentrum, diebasis,
    die_humanisten, verjungungsforschung, tierschutzpartei, volt
  )

## Combine all rows and aggregate to municipality level
sh22_party_cols <- c(
  "cdu", "spd", "grune", "fdp", "afd", "linke_pds", "ssw",
  "piraten", "freie_wahler", "die_partei", "zentrum", "diebasis",
  "die_humanisten", "verjungungsforschung", "tierschutzpartei", "volt"
)
sh22_select_cols <- c(
  "kreisname", "gemeindename", "eligible_voters", "number_voters",
  "valid_zweit", sh22_party_cols
)

sh22_combined <- bind_rows(
  sh22_regular |> select(all_of(sh22_select_cols)),
  sh22_brief_gem |> select(all_of(sh22_select_cols)),
  sh22_brief_alloc
) |>
  filter(!is.na(gemeindename))

sh22_data <- sh22_combined |>
  group_by(kreisname, gemeindename) |>
  summarize(
    eligible_voters = sum(eligible_voters, na.rm = TRUE),
    number_voters = sum(number_voters, na.rm = TRUE),
    valid_votes = sum(valid_zweit, na.rm = TRUE),
    across(all_of(sh22_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

## AGS Crosswalk
# SH uses non-standard Kennziffer identifiers; need to map Gemeindename to standard AGS
# Map kreisname to county code via the Kennziffer prefix
sh22_kreis_map <- tibble(
  kreisname = c(
    "Flensburg", "Kiel", "Lübeck", "Neumünster",
    "Dithmarschen", "Herzogtum Lauenburg", "Nordfriesland", "Ostholstein",
    "Pinneberg", "Plön", "Rendsburg-Eckernförde", "Schleswig-Flensburg",
    "Segeberg", "Steinburg", "Stormarn"
  ),
  county_code = c(
    "01001", "01002", "01003", "01004",
    "01051", "01053", "01054", "01055",
    "01056", "01057", "01058", "01059",
    "01060", "01061", "01062"
  )
)

# Load AGS reference from crosswalk
sh22_ags_ref <- read_rds(
  here("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")
) |>
  filter(startsWith(ags_2023, "01") & year == 2022) |>
  select(ags_2023, ags_name_23) |>
  distinct() |>
  mutate(
    county_code = substr(ags_2023, 1, 5),
    name_clean = str_trim(str_remove(ags_name_23, ",.*$"))
  )

# Match by county + cleaned name, then handle exceptions
sh22_data <- sh22_data |>
  left_join(sh22_kreis_map, by = "kreisname") |>
  mutate(name_clean = str_trim(str_remove(gemeindename, ",.*$")))

# Manual fixes for name mismatches before joining
sh22_data <- sh22_data |>
  mutate(
    name_clean = case_when(
      gemeindename == "Lauenburg/Elbe, Stadt" ~ "Lauenburg/ Elbe",
      gemeindename == "Niendorf a. d. Stecknitz" ~ "Niendorf/ Stecknitz",
      gemeindename == "Wentorf (Amt Sandesneben-Nusse)" ~ "Wentorf (Amt Sandesneben)",
      gemeindename == "Ülsby" ~ "Uelsby",
      TRUE ~ name_clean
    )
  )

# For Garding: match on full ags_name_23 including qualifier
# Override name_clean to include qualifier for disambiguation
sh22_data <- sh22_data |>
  mutate(
    name_clean = case_when(
      gemeindename == "Garding, Kirchspiel" ~ "Garding, Kirchspiel",
      gemeindename == "Garding, Stadt" ~ "Garding, Stadt",
      TRUE ~ name_clean
    )
  )

# Add matching full names to the AGS ref for Garding
sh22_ags_ref <- sh22_ags_ref |>
  mutate(
    name_clean = case_when(
      ags_name_23 %in% c("Garding, Kirchspiel", "Garding, Stadt") ~ ags_name_23,
      TRUE ~ name_clean
    )
  )

sh22_data <- sh22_data |>
  left_join(
    sh22_ags_ref |> select(ags_2023, county_code, name_clean),
    by = c("county_code", "name_clean")
  )

# Tastrup and Maasbüll were merged into Hürup (01059126) — assign manually
sh22_data <- sh22_data |>
  mutate(
    ags_2023 = case_when(
      gemeindename == "Tastrup" ~ "01059126",
      gemeindename == "Maasbüll" ~ "01059126",
      TRUE ~ ags_2023
    )
  )

# Aggregate Tastrup + Maasbüll + Hürup into single row
sh22_data <- sh22_data |>
  group_by(ags_2023) |>
  summarize(
    county = first(county_code),
    eligible_voters = sum(eligible_voters, na.rm = TRUE),
    number_voters = sum(number_voters, na.rm = TRUE),
    valid_votes = sum(valid_votes, na.rm = TRUE),
    across(all_of(sh22_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

## Construct final variables
sh22_data <- sh22_data |>
  mutate(
    ags = ags_2023,
    election_year = as.numeric(2022),
    election_date = as.Date("2022-05-08"),
    state = as.character("01"),
    csu = as.numeric(NA),
    cdu_csu = cdu / valid_votes * valid_votes # raw count preserved
  )

## Calculations
sh22_data <- sh22_data |>
  mutate(
    cdu_csu = cdu,
    turnout = valid_votes / eligible_voters,
    across(all_of(sh22_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(sh22_partylist),
    other,
    cdu_csu
  )

### Final Check
sh22_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

sh22_totalvoters <- sh22_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (sh22_totalvoters == 2314417) {
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

#### Nordrhein-Westfalen ####
## Party List
nrw22_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "piraten",
  "die_partei",
  "odp",
  "diebasis",
  "tierschutzpartei",
  "volt",
  "cdu_csu"
)

## Read Raw File
# Wahlbeteiligung file has all parties' Zweitstimmen at municipality level
# 7 header rows: title, subtitle, column headers, party group, party names, units, date
nrw22_raw <- read_xlsx(
  path = here(
    path,
    "Nordrhein-Westfalen/Nordrhein-Westfalen_2022_Landtagswahl_Wahlbeteiligung.xlsx"
  ),
  sheet = 1,
  skip = 7,
  col_names = FALSE
)

## Clean Dataset
# Col layout: 1=AGS, 2=Name, 3=Wahlberechtigte, 4=Wähler, 5=Zweitstimmen Insgesamt
# Cols 6-75: Party Zweitstimmen (CDU=6, SPD=7, FDP=8, AfD=9, GRÜNE=10, DIE LINKE=11,
# PIRATEN=12, Die PARTEI=13, FREIE WÄHLER=14, ÖDP=16, dieBasis=23, Tierschutzpartei=32, Volt=34)
# All values are character (due to mixed headers and "-" placeholders)

# Select relevant columns and assign names
nrw22_data <- nrw22_raw |>
  select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 23, 32, 34)

colnames(nrw22_data) <- c(
  "ags_raw", "name", "eligible_voters", "number_voters", "valid_votes",
  "cdu", "spd", "fdp", "afd", "grune", "linke_pds",
  "piraten", "die_partei", "freie_wahler", "odp", "diebasis",
  "tierschutzpartei", "volt"
)

# Replace "-" with NA and convert to numeric
nrw22_data <- nrw22_data |>
  mutate(across(-c(ags_raw, name), ~ as.numeric(na_if(., "-"))))

# Separate regular municipalities (8-digit AGS with valid data) and kreisfreie Städte
nrw22_muni <- nrw22_data |>
  filter(nchar(ags_raw) == 8 & !is.na(eligible_voters))

# Kreisfreie Städte: 5-digit AGS codes that have no valid 8-digit sub-rows
counties_with_munis <- unique(substr(nrw22_muni$ags_raw, 1, 5))
nrw22_krfr <- nrw22_data |>
  filter(
    nchar(ags_raw) == 5 &
      !is.na(eligible_voters) &
      !(ags_raw %in% counties_with_munis)
  ) |>
  mutate(ags_raw = paste0(ags_raw, "000"))

# Combine
nrw22_data <- bind_rows(nrw22_muni, nrw22_krfr)

## Construct AGS and other variables
nrw22_data <- nrw22_data |>
  mutate(
    ags = ags_raw,
    county = substr(ags, 1, 5),
    election_year = as.numeric(2022),
    election_date = as.Date("2022-05-15"),
    state = as.character("05"),
    csu = as.numeric(NA),
    cdu_csu = cdu
  )

## Calculations
nrw22_data <- nrw22_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(nrw22_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(nrw22_partylist),
    other,
    cdu_csu
  )

### Final Check
nrw22_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

nrw22_totalvoters <- nrw22_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (nrw22_totalvoters == 12965858) {
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

#### Bremen ####
## Party List
hb23_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "die_partei",
  "diebasis",
  "odp",
  "verjungungsforschung",
  "tierschutzpartei",
  "volt",
  "piraten",
  "cdu_csu"
)

## Read Raw Files
# Bremen uses 5-vote cumulative voting (Kumulieren/Panaschieren)
# Data is in LONG format (parties as rows) with German number formatting
# Two files: Bremen city + Bremerhaven
# Layout: rows 1-7 header; row 8+ = party data; then admin rows
# (Wahlberechtigte, Wähler:innen, Ungültige Stimmzettel, Gültige Stimmen)
hb23_parse <- function(file_path, ags, n_parties) {
  raw <- read_xlsx(file_path, sheet = 1, col_names = FALSE)
  # Party rows start at row 8, end at row 7 + n_parties
  parties <- raw[8:(7 + n_parties), 1:2]
  colnames(parties) <- c("party", "votes_str")
  # Admin rows follow
  admin_start <- 7 + n_parties + 1
  admin <- raw[admin_start:nrow(raw), 1:2]
  colnames(admin) <- c("metric", "value_str")

  # Parse German numbers (dots as thousands separators)
  parse_de <- function(x) as.numeric(gsub("\\.", "", x))

  party_votes <- parties |>
    mutate(votes = parse_de(votes_str)) |>
    select(party, votes)

  admin_vals <- admin |>
    mutate(value = parse_de(value_str)) |>
    select(metric, value)

  ev <- admin_vals |> filter(grepl("Wahlberechtigte", metric)) |> pull(value)
  nv <- admin_vals |>
    filter(grepl("hler", metric)) |>
    pull(value)
  vv <- admin_vals |> filter(grepl("ltige Stimmen", metric)) |> pull(value)

  list(party_votes = party_votes, eligible_voters = ev,
       number_voters = nv, valid_votes = vv, ags = ags)
}

hb23_bremen <- hb23_parse(
  here(path, "Bremen/Bremen_Bürgerschaft_2023.xlsx"),
  ags = "04011000", n_parties = 15
)
hb23_bhv <- hb23_parse(
  here(path, "Bremen/Bremen_Bürgerschaft_2023 BHV.xlsx"),
  ags = "04012000", n_parties = 8
)

## Build data rows
hb23_build_row <- function(parsed) {
  pv <- parsed$party_votes
  # Map party names to standardized names
  party_map <- c(
    "CDU" = "cdu", "SPD" = "spd",
    "GRÜNE" = "grune", "GRUNE" = "grune",
    "DIE LINKE" = "linke_pds",
    "FDP" = "fdp", "AfD" = "afd",
    "Die PARTEI" = "die_partei",
    "dieBasis" = "diebasis",
    "ÖDP" = "odp",
    "Verjüngungsforschung" = "verjungungsforschung",
    "Tierschutzpartei" = "tierschutzpartei",
    "Volt" = "volt",
    "PIRATEN" = "piraten"
  )

  row <- tibble(
    ags = parsed$ags,
    eligible_voters = parsed$eligible_voters,
    number_voters = parsed$number_voters,
    valid_votes = parsed$valid_votes
  )

  for (i in seq_len(nrow(pv))) {
    std_name <- party_map[pv$party[i]]
    if (!is.na(std_name)) {
      row[[std_name]] <- pv$votes[i]
    }
  }
  row
}

hb23_row1 <- hb23_build_row(hb23_bremen)
hb23_row2 <- hb23_build_row(hb23_bhv)
hb23_data <- bind_rows(hb23_row1, hb23_row2)

## Construct variables
# Some parties don't contest in Bremen (notably AfD) — ensure missing party
# columns are added as NA before computing shares
for (p in hb23_partylist) {
  if (!p %in% names(hb23_data)) {
    hb23_data[[p]] <- NA_real_
  }
}

hb23_data <- hb23_data |>
  mutate(
    county = substr(ags, 1, 5),
    election_year = as.numeric(2023),
    election_date = as.Date("2023-05-14"),
    state = as.character("04"),
    csu = as.numeric(NA),
    cdu_csu = cdu,
    turnout = valid_votes / eligible_voters,
    across(all_of(hb23_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(hb23_partylist),
    other,
    cdu_csu
  )

### Final Check
hb23_totalvoters <- hb23_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

if (hb23_totalvoters == 460778) {
  cat("There is no problem.\n")
} else {
  cat("Houston, we've got a problem!\n")
}

#### Berlin ####
## Party List
be23_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "piraten",
  "freie_wahler",
  "die_partei",
  "tierschutzpartei",
  "volt",
  "diebasis",
  "die_humanisten",
  "cdu_csu"
)

## Read Raw File
# Berlin AGH (Abgeordnetenhaus) 2023 - use AGH_W2 sheet (Zweitstimmen)
# Polling-district level; aggregate to 12 Bezirke + whole city
# Columns: stimmart, adresse, bezirksnummer(01-12), bezirksname, wahlbezirk,
#   wahlbezirksart(W/B), briefwahlbezirk, ..., wahlberechtigte_insgesamt,
#   wahlende, gultige_stimmen, ungultige_stimmen,
#   spd, cdu, grune, die_linke, af_d, fdp, die_partei, tierschutzpartei,
#   piraten, ..., freie_wahler, ..., die_basis, ..., die_humanisten, ..., volt
be23_raw <- read_xlsx(
  path = here(
    path,
    "Berlin/Berlin_2023_AGH_BVV.xlsx"
  ),
  sheet = "AGH_W2"
) |>
  clean_names()

## Clean Dataset
# W = Wahllokal (polling station), B = Briefwahl (mail-in)
# B rows have wahlberechtigte_insgesamt = 0 but contain valid votes
# Aggregate all rows (W + B) by Bezirk
be23_party_cols <- c("spd", "cdu", "grune", "die_linke", "af_d", "fdp",
  "die_partei", "tierschutzpartei", "piraten", "freie_wahler",
  "die_basis", "die_humanisten", "volt")

be23_data <- be23_raw |>
  mutate(across(c(wahlberechtigte_insgesamt, wahlende, gultige_stimmen,
    all_of(be23_party_cols)), as.numeric))

# Aggregate to Bezirk level
be23_bezirk <- be23_data |>
  group_by(bezirksnummer) |>
  summarize(
    eligible_voters = sum(wahlberechtigte_insgesamt, na.rm = TRUE),
    number_voters = sum(wahlende, na.rm = TRUE),
    valid_votes = sum(gultige_stimmen, na.rm = TRUE),
    across(all_of(be23_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Also create whole-city aggregate (AGS 11000000)
be23_total <- be23_bezirk |>
  summarize(
    bezirksnummer = "00",
    eligible_voters = sum(eligible_voters),
    number_voters = sum(number_voters),
    valid_votes = sum(valid_votes),
    across(all_of(be23_party_cols), ~ sum(.x))
  )

be23_combined <- bind_rows(be23_total, be23_bezirk)

## Map to standard party names and AGS
# In existing state_unharm, Berlin Bezirke have AGS pattern 110XX0XX
# where XX = Bezirk number (e.g., 11001001, 11002002, ..., 11012012)
be23_data <- be23_combined |>
  mutate(
    ags = case_when(
      bezirksnummer == "00" ~ "11000000",
      TRUE ~ paste0("110", sprintf("%02s", bezirksnummer), "0",
                     sprintf("%02s", bezirksnummer))
    ),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2023),
    election_date = as.Date("2023-02-12"),
    state = as.character("11"),
    csu = as.numeric(NA)
  ) |>
  # Rename to standard party names
  rename(
    linke_pds = die_linke,
    afd = af_d,
    diebasis = die_basis
  ) |>
  mutate(cdu_csu = cdu)

## Calculations
be23_data <- be23_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(be23_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(be23_partylist),
    other,
    cdu_csu
  )

### Final Check
be23_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

be23_totalvoters <- be23_data |>
  filter(ags == "11000000") |>
  pull(eligible_voters)

# Berlin 2023 total eligible voters (whole city row)
cat("Berlin 2023 eligible voters:", be23_totalvoters, "\n")
cat("Berlin 2023 rows:", nrow(be23_data), "\n")

#### Brandenburg ####
## Party List
bb24_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "bsw",
  "tierschutzpartei",
  "cdu_csu"
)

## Read Raw File
# Sheet 4 = Zweitstimme (Landesliste), polling-district level with W/B flag
# Columns: ars(12-digit), landkreis, amts_bzw_verbandsge_meindenummer,
#   gemeindenummer, gemeindename, wahlbezirksart(W/B),
#   wahlberechtigte_insgesamt, wahlende, gultige_stimmen, ungultige_stimmen,
#   spd, af_d, cdu, grune_b_90, die_linke, bvb_freie_wahler, fdp,
#   tierschutzpartei, plus, bsw, iii_weg, dkp, dlw, wu
#   (each with _in_prozent pair)
# gemeindenummer = "keine Gemeinde" for 50 Amt-level Briefwahl rows
# gemeindenummer = "000" for kreisfreie Städte (Brandenburg a.d.H., Cottbus, Frankfurt, Potsdam)
bb24_raw <- read_xlsx(
  path = here(
    path,
    "Brandenburg/Brandenburg_2024_Landtagswahl.xlsx"
  ),
  sheet = 4
) |>
  clean_names()

## Clean Dataset
# Absolute vote count columns (not _in_prozent)
bb24_party_cols <- c("spd", "af_d", "cdu", "grune_b_90", "die_linke",
  "bvb_freie_wahler", "fdp", "tierschutzpartei", "plus", "bsw",
  "iii_weg", "dkp", "dlw", "wu")

bb24_data <- bb24_raw |>
  mutate(across(c(wahlberechtigte_insgesamt, wahlende, gultige_stimmen,
    all_of(bb24_party_cols)), as.numeric))

# Extract AGS from 12-digit ARS: first 2 digits = state, next 1 = RB,
# next 2 = Kreis, next 4 = VG, last 3 = Gemeinde
# Standard 8-digit AGS = state(2) + county(3) + municipality(3)
# For BB: ars = 12 0 KK VVVV GGG → AGS = 12 0KK GGG
# Kreisfreie Städte: gemeindenummer = "000" → AGS = county_code + "000"
# "keine Gemeinde" rows: Amt-level Briefwahl, need proportional allocation

# Separate rows with valid Gemeinde vs Amt-level Briefwahl
bb24_with_gem <- bb24_data |>
  filter(!is.na(gemeindenummer) & gemeindenummer != "keine Gemeinde") |>
  mutate(
    # Extract county code (5 digits) and gemeinde (3 digits) from ARS
    ags = paste0(substr(ars, 1, 5), substr(ars, 10, 12))
  )

# Aggregate to municipality (combine W + B rows within each Gemeinde)
bb24_agg <- bb24_with_gem |>
  group_by(ags) |>
  summarize(
    eligible_voters = sum(wahlberechtigte_insgesamt, na.rm = TRUE),
    number_voters = sum(wahlende, na.rm = TRUE),
    valid_votes = sum(gultige_stimmen, na.rm = TRUE),
    across(all_of(bb24_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

## Handle Amt-level Briefwahl allocation
bb24_no_gem <- bb24_data |>
  filter(gemeindenummer == "keine Gemeinde")

if (nrow(bb24_no_gem) > 0) {
  cat("Amt-level Briefwahl rows without Gemeinde:", nrow(bb24_no_gem), "\n")

  # These are grouped by Amt (amts_bzw_verbandsge_meindenummer within each landkreis)
  # Allocate proportionally by eligible voters of municipalities in same Amt
  bb24_amt_brief <- bb24_no_gem |>
    mutate(amt_id = paste0(substr(ars, 1, 5), substr(ars, 6, 9))) |>
    group_by(amt_id) |>
    summarize(
      across(c(wahlende, gultige_stimmen, all_of(bb24_party_cols)),
        ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Get municipality weights within each Amt
  bb24_gem_weights <- bb24_with_gem |>
    mutate(amt_id = paste0(substr(ars, 1, 5), substr(ars, 6, 9))) |>
    group_by(amt_id, ags) |>
    summarize(ev = sum(wahlberechtigte_insgesamt, na.rm = TRUE), .groups = "drop") |>
    group_by(amt_id) |>
    mutate(weight = ev / sum(ev, na.rm = TRUE)) |>
    ungroup()

  # Allocate
  bb24_alloc <- bb24_gem_weights |>
    inner_join(bb24_amt_brief, by = "amt_id", suffix = c("_gem", "_brief")) |>
    mutate(
      across(c(wahlende, gultige_stimmen, all_of(bb24_party_cols)),
        ~ round(.x * weight))
    ) |>
    select(ags, number_voters = wahlende, valid_votes = gultige_stimmen,
      all_of(bb24_party_cols)) |>
    mutate(eligible_voters = 0L)

  # Add allocated votes to aggregated data
  bb24_agg <- bind_rows(bb24_agg, bb24_alloc) |>
    group_by(ags) |>
    summarize(
      eligible_voters = sum(eligible_voters, na.rm = TRUE),
      number_voters = sum(number_voters, na.rm = TRUE),
      valid_votes = sum(valid_votes, na.rm = TRUE),
      across(all_of(bb24_party_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

## Rename party columns and construct final variables
bb24_data <- bb24_agg |>
  rename(
    afd = af_d,
    grune = grune_b_90,
    linke_pds = die_linke,
    freie_wahler = bvb_freie_wahler
  ) |>
  mutate(
    county = substr(ags, 1, 5),
    election_year = as.numeric(2024),
    election_date = as.Date("2024-09-22"),
    state = as.character("12"),
    csu = as.numeric(NA),
    cdu_csu = cdu
  )

## Calculations
bb24_data <- bb24_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(bb24_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(bb24_partylist),
    other,
    cdu_csu
  )

### Final Check
bb24_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

bb24_totalvoters <- bb24_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("BB 2024 eligible voters:", bb24_totalvoters, "\n")
cat("BB 2024 rows:", nrow(bb24_data), "\n")

#### Sachsen ####
## Party List
sn24_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "bsw",
  "piraten",
  "die_partei",
  "tierschutzpartei",
  "cdu_csu"
)

## Read Raw File
# Sheet "LW24_endgErgebnisse_GE&TG" has municipality-level (GE) + Teilgemeinde (TG) data
# 100+ columns; _1 suffix = Direktstimmen, _2 suffix = Listenstimmen (what we want)
# No "art" column — GE rows have 8-digit AGS, TG rows have 9-digit AGS
# Columns: wahl, ebene_2, wk_nr, wk_name, ebene_5, ags, ortname,
#   briefwahl_sonderfall, genaue_briefwahlzuordung, a1, a2, a3,
#   wahlberechtigte, wahler, b1, darunter_briefwahler,
#   ungultige_1, gultige_1, [party_1 cols], ungultige_2, gultige_2,
#   [party_2 cols: cdu_2, af_d_2, die_linke_2, grune_2, spd_2, fdp_2,
#    freie_wahler_2, die_partei_2, piraten_2, odp_2, bu_so_2,
#    tierschutz_hier_2, die_basis_2, bundnis_c_2, bundnis_deutschland_2,
#    bsw_2, freie_sachsen_2, v_partei3_2, wu_2]
# Party columns may be character due to "x" values
sn24_raw <- read_xlsx(
  path = here(
    path,
    "Sachsen/Sachsen_2024_Landtagswahl.xlsx"
  ),
  sheet = "LW24_endgErgebnisse_GE&TG"
) |>
  clean_names()

## Clean Dataset
# Include both GE (Gemeinde, 8-digit AGS) and TG (Teilgemeinde, 9-digit AGS) rows.
# TG rows are sub-municipal splits of large cities (Dresden, Leipzig, Chemnitz,
# Zwickau) across Wahlkreise — these cities have NO GE row, only TG rows.
# Truncate TG 9-digit AGS to 8 digits and aggregate to get municipality totals.
sn24_data <- sn24_raw |>
  mutate(
    ags_raw = as.character(ags),
    # Truncate 9-digit TG codes to 8-digit municipality AGS
    ags_8 = substr(ags_raw, 1, 8)
  ) |>
  # Replace "x" (non-contesting party) with NA
  mutate(across(where(is.character), ~ na_if(.x, "x")))

# Listenstimmen columns (absolute counts, _2 suffix)
sn24_ls_cols <- c("cdu_2", "af_d_2", "die_linke_2", "grune_2", "spd_2",
  "fdp_2", "freie_wahler_2", "die_partei_2", "piraten_2", "odp_2",
  "bu_so_2", "tierschutz_hier_2", "die_basis_2", "bundnis_c_2",
  "bundnis_deutschland_2", "bsw_2", "freie_sachsen_2", "v_partei3_2", "wu_2")

sn24_data <- sn24_data |>
  mutate(across(c(wahlberechtigte, wahler, gultige_2, all_of(sn24_ls_cols)),
    as.numeric))

# Aggregate TG rows to municipality level (summing across Wahlkreis splits)
sn24_data <- sn24_data |>
  group_by(ags_8) |>
  summarize(
    wahlberechtigte = sum(wahlberechtigte, na.rm = TRUE),
    wahler = sum(wahler, na.rm = TRUE),
    gultige_2 = sum(gultige_2, na.rm = TRUE),
    across(all_of(sn24_ls_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

## Construct AGS and variables
sn24_data <- sn24_data |>
  mutate(
    ags = sprintf("%08d", as.numeric(ags_8)),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2024),
    election_date = as.Date("2024-09-01"),
    state = as.character("14"),
    csu = as.numeric(NA),
    eligible_voters = wahlberechtigte,
    number_voters = wahler,
    valid_votes = gultige_2
  )

# Rename Listenstimmen _2 columns to standard party names
sn24_data <- sn24_data |>
  rename(
    cdu = cdu_2,
    afd = af_d_2,
    linke_pds = die_linke_2,
    grune = grune_2,
    spd = spd_2,
    fdp = fdp_2,
    freie_wahler = freie_wahler_2,
    die_partei = die_partei_2,
    piraten = piraten_2,
    tierschutzpartei = tierschutz_hier_2,
    bsw = bsw_2
  ) |>
  mutate(cdu_csu = cdu)

## Calculations
sn24_data <- sn24_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(sn24_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(sn24_partylist),
    other,
    cdu_csu
  )

### Final Check
sn24_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

sn24_totalvoters <- sn24_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("SN 2024 eligible voters:", sn24_totalvoters, "\n")
cat("SN 2024 rows:", nrow(sn24_data), "\n")

#### Thüringen ####
## Party List
th24_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "bsw",
  "tierschutzpartei",
  "piraten",
  "cdu_csu"
)

## Read Raw File
# 85 columns, 7 multi-row header lines. Must skip=7 and assign names manually.
# Row structure: col 1=Stand, 2=Satzart(L/K/G/NA), 3=Wahlkreisnr, 4=Kreisnr,
#   5=Gemeindenr, 6=Wahlbezirksnr, 7=Name, 8-12=admin flags,
#   13=Wahlberechtigte, 14=WB ohne Wahlschein, 15=WB mit Wahlschein,
#   16=nach §23(2), 17=Wähler, 18=Wähler mit Wahlschein, 19=Wahlbeteiligung,
#   20=WKS ungültig, 21=WKS gültig, 22=LS ungültig, 23=LS gültig
# Parties in 4-col blocks (WKS abs, WKS %, LS abs, LS %):
#   24-27: DIE LINKE, 28-31: AfD, 32-35: CDU, 36-39: SPD, 40-43: GRÜNE,
#   44-47: FDP, 48-51: TIERSCHUTZ hier!, 52-55: ÖDP/Familie,
#   56-59: PIRATEN, 60-63: MLPD, 64-67: BÜNDNIS DEUTSCHLAND,
#   68-71: BSW, 72-75: FAMILIE, 76-79: FREIE WÄHLER, 80-83: WU,
#   84-85: Einzelbewerber (WKS only)
# Satzart "G" = 602 pre-aggregated municipality rows
# All numeric columns are character due to "-" dashes
th24_raw <- read_xlsx(
  path = here(
    path,
    "Thüringen/Thüringen_2024_Landtag.xlsx"
  ),
  sheet = 1,
  skip = 7,
  col_names = FALSE,
  col_types = "text"
)

# Assign meaningful column names
th24_colnames <- c(
  "stand", "satzart", "wahlkreisnr", "kreisnr", "gemeindenr",
  "wahlbezirksnr", "name", "eigener_briefwahlbezirk",
  "abgebend_nach", "aufnehmend_von",
  "wbz_insgesamt", "wbz_erfasst",
  "wahlberechtigte", "wb_ohne_ws", "wb_mit_ws", "nach_23_2",
  "wahler", "wahler_mit_ws", "wahlbeteiligung",
  "wks_ungultig", "wks_gultig", "ls_ungultig", "ls_gultig",
  # 16 parties × 4 cols (WKS abs, WKS %, LS abs, LS %) = 64 cols
  # But Einzelbewerber has only 2 (WKS abs, WKS %)
  "linke_wks", "linke_wks_pct", "linke_ls", "linke_ls_pct",
  "afd_wks", "afd_wks_pct", "afd_ls", "afd_ls_pct",
  "cdu_wks", "cdu_wks_pct", "cdu_ls", "cdu_ls_pct",
  "spd_wks", "spd_wks_pct", "spd_ls", "spd_ls_pct",
  "grune_wks", "grune_wks_pct", "grune_ls", "grune_ls_pct",
  "fdp_wks", "fdp_wks_pct", "fdp_ls", "fdp_ls_pct",
  "tierschutz_wks", "tierschutz_wks_pct", "tierschutz_ls", "tierschutz_ls_pct",
  "odp_wks", "odp_wks_pct", "odp_ls", "odp_ls_pct",
  "piraten_wks", "piraten_wks_pct", "piraten_ls", "piraten_ls_pct",
  "mlpd_wks", "mlpd_wks_pct", "mlpd_ls", "mlpd_ls_pct",
  "bundnis_de_wks", "bundnis_de_wks_pct", "bundnis_de_ls", "bundnis_de_ls_pct",
  "bsw_wks", "bsw_wks_pct", "bsw_ls", "bsw_ls_pct",
  "familie_wks", "familie_wks_pct", "familie_ls", "familie_ls_pct",
  "fw_wks", "fw_wks_pct", "fw_ls", "fw_ls_pct",
  "wu_wks", "wu_wks_pct", "wu_ls", "wu_ls_pct",
  "einzelbewerber_wks", "einzelbewerber_wks_pct"
)
names(th24_raw) <- th24_colnames[1:ncol(th24_raw)]

## Clean Dataset
# Filter to Gemeinde-level aggregate rows (Satzart = "G")
th24_data <- th24_raw |>
  filter(satzart == "G")

cat("TH Gemeinde rows:", nrow(th24_data), "\n")

# Replace "-" with NA and convert numeric columns
# Landesstimmen absolute columns we need:
th24_ls_party_cols <- c("linke_ls", "afd_ls", "cdu_ls", "spd_ls", "grune_ls",
  "fdp_ls", "tierschutz_ls", "odp_ls", "piraten_ls", "mlpd_ls",
  "bundnis_de_ls", "bsw_ls", "familie_ls", "fw_ls", "wu_ls")

th24_admin_cols <- c("wahlberechtigte", "wahler", "ls_gultig")

th24_data <- th24_data |>
  mutate(across(c(all_of(th24_admin_cols), all_of(th24_ls_party_cols)),
    ~ as.numeric(na_if(.x, "-"))))

## Construct AGS
# Thüringen: state(2) + county(3) + municipality(3) = 8 digits
# kreisnr is 2-digit county code within state; gemeindenr is 3-digit
# Kreisfreie Städte (Erfurt, Gera, Jena, Weimar, etc.) are split across
# multiple Wahlkreise with the same AGS → need aggregation
th24_data <- th24_data |>
  mutate(
    ags = paste0(
      "16",
      sprintf("%03d", as.numeric(kreisnr)),
      sprintf("%03d", as.numeric(gemeindenr))
    )
  )

# Aggregate duplicates (kreisfreie Städte split across Wahlkreise,
# Dingelstädt split by Ortsteil)
th24_data <- th24_data |>
  group_by(ags) |>
  summarize(
    eligible_voters = sum(wahlberechtigte, na.rm = TRUE),
    number_voters = sum(wahler, na.rm = TRUE),
    valid_votes = sum(ls_gultig, na.rm = TRUE),
    across(all_of(th24_ls_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(county = substr(ags, 1, 5))

## Rename Landesstimmen columns to standard party names
th24_data <- th24_data |>
  rename(
    linke_pds = linke_ls,
    afd = afd_ls,
    cdu = cdu_ls,
    spd = spd_ls,
    grune = grune_ls,
    fdp = fdp_ls,
    tierschutzpartei = tierschutz_ls,
    piraten = piraten_ls,
    bsw = bsw_ls,
    freie_wahler = fw_ls
  ) |>
  mutate(
    election_year = as.numeric(2024),
    election_date = as.Date("2024-09-01"),
    state = as.character("16"),
    csu = as.numeric(NA),
    cdu_csu = cdu
  )

## Calculations
th24_data <- th24_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(th24_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(th24_partylist),
    other,
    cdu_csu
  )

### Final Check
th24_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

th24_totalvoters <- th24_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("TH 2024 eligible voters:", th24_totalvoters, "\n")
cat("TH 2024 rows:", nrow(th24_data), "\n")

#### Baden-Württemberg 2021 ####
## Party List
bw21_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "odp",
  "piraten",
  "die_partei",
  "diebasis",
  "volt",
  "die_humanisten",
  "cdu_csu"
)

## Read Raw File
bw21_raw <- read_delim(
  here(path, "Baden-Württemberg/Baden-Württemberg_2021_Landtagswahl.csv"),
  delim = ";", show_col_types = FALSE,
  locale = locale(encoding = "latin1")
)

## Clean Dataset
bw21_data <- bw21_raw |>
  mutate(ags = str_extract(Gemeinde, "^[0-9]+")) |>
  # Drop gemeindefreie Gebiete (AGS ending in 99x)
  filter(!str_detect(ags, "99[0-9]$")) |>
  transmute(
    ags,
    county = substr(ags, 1, 5),
    election_year = as.numeric(2021),
    election_date = as.Date("2021-03-14"),
    state = as.character("08"),
    eligible_voters = Wahlberechtigte,
    number_voters = .data[["Wähler(innen)"]],
    valid_votes = .data[["Gültige Stimmen"]],
    cdu_n = CDU,
    spd_n = SPD,
    grune_n = .data[["GRÜNE"]],
    fdp_n = FDP,
    linke_n = .data[["DIE LINKE"]],
    afd_n = AfD,
    freie_wahler_n = .data[["FREIE WÄHLER"]],
    odp_n = .data[["ÖDP"]],
    piraten_n = PIRATEN,
    die_partei_n = .data[["Die PARTEI"]],
    diebasis_n = dieBasis,
    volt_n = Volt,
    die_humanisten_n = .data[["Die Humanisten"]]
  ) |>
  mutate(
    other_n = valid_votes - rowSums(across(any_of(c("cdu_n", "spd_n", "grune_n",
      "fdp_n", "linke_n", "afd_n"))), na.rm = TRUE),
    csu = as.numeric(NA),
    # Compute shares
    turnout = valid_votes / eligible_voters,
    cdu = cdu_n / valid_votes,
    spd = spd_n / valid_votes,
    grune = grune_n / valid_votes,
    fdp = fdp_n / valid_votes,
    linke_pds = linke_n / valid_votes,
    afd = afd_n / valid_votes,
    freie_wahler = freie_wahler_n / valid_votes,
    odp = odp_n / valid_votes,
    piraten = piraten_n / valid_votes,
    die_partei = die_partei_n / valid_votes,
    diebasis = diebasis_n / valid_votes,
    volt = volt_n / valid_votes,
    die_humanisten = die_humanisten_n / valid_votes,
    other = other_n / valid_votes,
    cdu_csu = cdu
  ) |>
  select(
    ags, county, election_year, state, election_date,
    eligible_voters, number_voters, valid_votes, turnout,
    all_of(bw21_partylist),
    other,
    cdu_csu
  )

### Final Check
bw21_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

bw21_totalvoters <- bw21_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("BW 2021 eligible voters:", bw21_totalvoters, "\n")
cat("BW 2021 valid votes:", sum(bw21_data$valid_votes, na.rm = TRUE), "\n")
cat("BW 2021 rows:", nrow(bw21_data), "\n")

#### Sachsen-Anhalt 2021 ####
## Party List
st21_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "npd",
  "tierschutzpartei",
  "die_partei",
  "diebasis",
  "piraten",
  "die_humanisten",
  "cdu_csu"
)

## Read Raw File
# Skip 7 header rows; use text for all columns to handle "-" and "x" values
st21_raw <- read_xlsx(
  path = here(path, "Sachsen-Anhalt/Sachsen-Anhalt_2021_Landtagswahl.xlsx"),
  sheet = "Gemeinden",
  col_names = FALSE,
  col_types = "text",
  skip = 7
)

## Clean Dataset
# Keep only "Insgesamt" rows (total of Brief + Urne)
# Exclude the Land-level total row (AGS = "15") — only keep 8-digit municipality AGS
st21_data <- st21_raw |>
  filter(...3 == "Insgesamt", nchar(...1) == 8)

cat("SA Insgesamt rows:", nrow(st21_data), "\n")

# Column mapping (from header exploration):
# col1 = AGS, col2 = Name, col3 = Wahllokalart
# col4 = Wahlberechtigte, col8 = Wähler/-innen
# col30 = Ungültige Zweitstimmen, col31 = Gültige Zweitstimmen
# col32-53 = Zweitstimmen party columns:
#   32=CDU, 33=AfD, 34=DIE LINKE, 35=SPD, 36=GRÜNE, 37=FDP,
#   38=FREIE WÄHLER, 39=NPD, 40=Tierschutzpartei, 41=Tierschutzallianz,
#   42=LKR, 43=Die PARTEI, 44=Gartenpartei, 45=FBM,
#   46=TIERSCHUTZ hier!, 47=dieBasis, 48=Klimaliste ST, 49=ÖDP,
#   50=Die Humanisten, 51=Gesundheitsforschung, 52=PIRATEN, 53=WiR2020

# Replace "-" and "x" with NA, convert numeric
st21_numeric_cols <- c(4, 8, 31, 32:53)
st21_data <- st21_data |>
  mutate(across(all_of(st21_numeric_cols),
    ~ as.numeric(na_if(na_if(.x, "-"), "x"))))

st21_data <- st21_data |>
  transmute(
    ags = as.character(...1),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2021),
    election_date = as.Date("2021-06-06"),
    state = as.character("15"),
    eligible_voters = ...4,
    number_voters = ...8,
    valid_votes = ...31,
    cdu_n = ...32,
    afd_n = ...33,
    linke_n = ...34,
    spd_n = ...35,
    grune_n = ...36,
    fdp_n = ...37,
    freie_wahler_n = ...38,
    npd_n = ...39,
    tierschutzpartei_n = ...40,
    die_partei_n = ...43,
    diebasis_n = ...47,
    piraten_n = ...52,
    die_humanisten_n = ...50
  ) |>
  mutate(
    other_n = valid_votes - rowSums(across(any_of(c("cdu_n", "spd_n", "grune_n",
      "fdp_n", "linke_n", "afd_n"))), na.rm = TRUE),
    csu = as.numeric(NA),
    turnout = valid_votes / eligible_voters,
    cdu = cdu_n / valid_votes,
    spd = spd_n / valid_votes,
    grune = grune_n / valid_votes,
    fdp = fdp_n / valid_votes,
    linke_pds = linke_n / valid_votes,
    afd = afd_n / valid_votes,
    freie_wahler = freie_wahler_n / valid_votes,
    npd = npd_n / valid_votes,
    tierschutzpartei = tierschutzpartei_n / valid_votes,
    die_partei = die_partei_n / valid_votes,
    diebasis = diebasis_n / valid_votes,
    piraten = piraten_n / valid_votes,
    die_humanisten = die_humanisten_n / valid_votes,
    other = other_n / valid_votes,
    cdu_csu = cdu
  ) |>
  select(
    ags, county, election_year, state, election_date,
    eligible_voters, number_voters, valid_votes, turnout,
    all_of(st21_partylist),
    other,
    cdu_csu
  )

### Final Check
st21_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

st21_totalvoters <- st21_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("SA 2021 eligible voters:", st21_totalvoters, "\n")
cat("SA 2021 valid votes:", sum(st21_data$valid_votes, na.rm = TRUE), "\n")
cat("SA 2021 rows:", nrow(st21_data), "\n")

#### Berlin 2021 ####
## Party List (follows be23 pattern)
be21_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "piraten",
  "freie_wahler",
  "die_partei",
  "tierschutzpartei",
  "volt",
  "diebasis",
  "die_humanisten",
  "cdu_csu"
)

## Read Raw File
# Berlin AGH (Abgeordnetenhaus) 2021 - use AGH_W2 sheet (Zweitstimmen)
# Polling-district level; aggregate to 12 Bezirke + whole city
be21_raw <- read_xlsx(
  path = here(path, "Berlin/Berlin_2021_AGH_BVV.xlsx"),
  sheet = "AGH_W2"
) |>
  clean_names()

## Clean Dataset
be21_party_cols <- c("spd", "cdu", "grune", "die_linke", "af_d", "fdp",
  "die_partei", "tierschutzpartei", "piraten", "freie_wahler",
  "die_basis", "die_humanisten", "volt")

be21_data <- be21_raw |>
  mutate(across(c(wahlberechtigte_insgesamt, wahlende, gultige_stimmen,
    all_of(be21_party_cols)), as.numeric))

# Aggregate to Bezirk level
be21_bezirk <- be21_data |>
  group_by(bezirksnummer) |>
  summarize(
    eligible_voters = sum(wahlberechtigte_insgesamt, na.rm = TRUE),
    number_voters = sum(wahlende, na.rm = TRUE),
    valid_votes = sum(gultige_stimmen, na.rm = TRUE),
    across(all_of(be21_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Also create whole-city aggregate (AGS 11000000)
be21_total <- be21_bezirk |>
  summarize(
    bezirksnummer = "00",
    eligible_voters = sum(eligible_voters),
    number_voters = sum(number_voters),
    valid_votes = sum(valid_votes),
    across(all_of(be21_party_cols), ~ sum(.x))
  )

be21_combined <- bind_rows(be21_total, be21_bezirk)

## Map to standard party names and AGS
# Berlin Bezirke have AGS pattern 110XX0XX (e.g., 11001001, 11002002, ..., 11012012)
be21_data <- be21_combined |>
  mutate(
    ags = case_when(
      bezirksnummer == "00" ~ "11000000",
      TRUE ~ paste0("110", sprintf("%02s", bezirksnummer), "0",
                     sprintf("%02s", bezirksnummer))
    ),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2021),
    election_date = as.Date("2021-09-26"),
    state = as.character("11"),
    csu = as.numeric(NA)
  ) |>
  rename(
    linke_pds = die_linke,
    afd = af_d,
    diebasis = die_basis
  ) |>
  mutate(cdu_csu = cdu)

## Calculations
be21_data <- be21_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(be21_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
  select(
    ags, county, election_year, state, election_date,
    eligible_voters, number_voters, valid_votes, turnout,
    all_of(be21_partylist),
    other,
    cdu_csu
  )

### Final Check
be21_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

be21_totalvoters <- be21_data |>
  filter(ags == "11000000") |>
  pull(eligible_voters)

cat("Berlin 2021 eligible voters:", be21_totalvoters, "\n")
cat("Berlin 2021 rows:", nrow(be21_data), "\n")

#### Mecklenburg-Vorpommern 2021 ####
## Party List
mv21_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "npd",
  "freie_wahler",
  "die_partei",
  "tierschutzpartei",
  "piraten",
  "diebasis",
  "die_humanisten",
  "cdu_csu"
)

## Read Raw File
# Sheet "3.12 Zweitst. n. Gemeinden" — municipality-level Zweitstimmen
# Complex multi-row header (skip 10 rows: 8 header + 2 numbering/blank rows)
# Each municipality has 4 rows: 2021 Anzahl, 2016 Anzahl, 2021 %, 2016 %
# Briefwahl is separate for amtsangehörige Gemeinden (76 Briefwahl entries)
# NO AGS column — need name-to-AGS mapping via BBSR reference
mv21_raw <- read_xlsx(
  path = here(path,
    "Mecklenburg-Vorpommern/Mecklenburg-Vorpommern_2021_2016_Landtagswahl.xlsx"),
  sheet = "3.12 Zweitst. n. Gemeinden",
  col_names = FALSE,
  col_types = "text",
  skip = 10
)

## Step 1: Filter to 2021 Anzahl rows only
# Column mapping from header:
# col1 = Municipality name, col2 = Wahljahr, col3 = Maßeinheit
# col4 = Wahlberechtigte, col5 = Wähler, col6 = ungültig, col7 = gültig
# col8=SPD, col9=AfD, col10=CDU, col11=DIE LINKE, col12=GRÜNE, col13=FDP,
# col14=NPD, col15=Tierschutzpartei, col16=FREiER HORIZONT,
# col17=Die PARTEI, col18=FREIE WÄHLER, col19=PIRATEN, col20=DKP,
# col21=Bündnis C, col22=TIERSCHUTZ hier!, col23=dieBasis, col24=DiB,
# col25=FPA, col26=LKR, col27=ÖDP, col28=Die Humanisten,
# col29=Gesundheitsforschung, col30=Team Todenhöfer, col31=UNABHÄNGIGE,
# col32=Sonstige

# Municipality names are in col1 of the first row of each 4-row block.
# Subsequent rows for the same municipality have NA in col1.
# Landkreis section headers appear in col1 with NA in col2/col3.
# Track Landkreis to disambiguate duplicate municipality names across counties.
# Landkreis -> county prefix mapping for MV:
mv21_lk_map <- c(
  "Mecklenburgische Seenplatte" = "13071",
  "Landkreis Rostock" = "13072",
  "Landkreis Vorpommern R\u00fcgen" = "13073",
  "Landkreis Nordwestmecklenburg" = "13074",
  "Landkreis Vorpommern-Greifswald" = "13075",
  "Landkreis Ludwigslust-Parchim" = "13076"
)

# Add Landkreis section tracking before filling municipality names
mv21_data <- mv21_raw |>
  mutate(
    name = ...1,
    # Detect Landkreis header rows: name present, but no Wahljahr/Maßeinheit
    is_lk_header = !is.na(name) & is.na(...2) & is.na(...3)
  )

# Build Landkreis assignment: fill Landkreis forward from header rows
# Kreisfreie Städte (Rostock, Schwerin) appear before any LK header
clean_name <- function(x) {
  x |>
    str_replace_all("\\r\\n|\\n", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

mv21_data <- mv21_data |>
  mutate(
    lk_name = if_else(is_lk_header, clean_name(name), NA_character_)
  ) |>
  fill(lk_name, .direction = "down") |>
  mutate(
    # Map LK name to county prefix; kreisfreie Städte have no LK header
    county_prefix = mv21_lk_map[lk_name]
  ) |>
  fill(name, .direction = "down") |>
  filter(...2 == "2021", ...3 == "Anzahl")

cat("MV 2021 Anzahl rows:", nrow(mv21_data), "\n")

# Separate Briefwahl rows from municipality rows
mv21_brief <- mv21_data |> filter(str_detect(name, "^Briefwahl"))
mv21_munis <- mv21_data |>
  filter(!str_detect(name, "^Briefwahl"),
         !str_detect(name, "^Mecklenburg-Vorpommern$"))

cat("MV municipality rows:", nrow(mv21_munis), "\n")
cat("MV Briefwahl rows:", nrow(mv21_brief), "\n")

## Step 2: Convert numeric columns
mv21_numeric_cols <- paste0("...", 4:32)
mv21_munis <- mv21_munis |>
  mutate(across(all_of(mv21_numeric_cols),
    ~ as.numeric(na_if(na_if(.x, "-"), "x"))))
mv21_brief <- mv21_brief |>
  mutate(across(all_of(mv21_numeric_cols),
    ~ as.numeric(na_if(na_if(.x, "-"), "x"))))

## Step 3: Build name-to-AGS crosswalk from BBSR reference
mv21_ref <- read_xlsx(
  here("data/crosswalks/raw/ref-gemeinden-ab-2020.xlsx"),
  sheet = "2021", col_types = "text"
)
# Column 1 = AGS, Column 2 = Gemeindename
mv21_ref_col1 <- names(mv21_ref)[1]
mv21_ref_col2 <- names(mv21_ref)[2]
mv21_ref_mv <- mv21_ref |>
  filter(str_detect(.data[[mv21_ref_col1]], "^13")) |>
  transmute(
    ref_ags = .data[[mv21_ref_col1]],
    ref_name = .data[[mv21_ref_col2]],
    ref_county = substr(ref_ags, 1, 5)
  )

mv21_munis <- mv21_munis |> mutate(name_clean = clean_name(name))
mv21_ref_mv <- mv21_ref_mv |> mutate(ref_name_clean = clean_name(ref_name))

# Manual name overrides for known discrepancies between election data and
# BBSR reference (different suffixes, word order)
mv21_name_overrides <- c(
  "Neubukow, Stadt" = "13072074",
  "Tessin, Stadt" = "13072105",
  "Hansestadt Stralsund" = "13073088",
  "Hansestadt Wismar" = "13074087"
)

# Match with county-aware disambiguation:
# For kreisfreie Städte (no county_prefix), match by name only.
# For others, restrict BBSR reference to same county prefix.
mv21_matched <- mv21_munis |>
  mutate(ref_ags = mv21_name_overrides[name_clean])

for (i in seq_len(nrow(mv21_matched))) {
  if (!is.na(mv21_matched$ref_ags[i])) next  # already matched via override
  nm <- mv21_matched$name_clean[i]
  cp <- mv21_matched$county_prefix[i]

  # Restrict reference to same county (if known)
  if (!is.na(cp)) {
    ref_sub <- mv21_ref_mv |> filter(ref_county == cp)
  } else {
    # Kreisfreie Städte: match from full MV reference
    ref_sub <- mv21_ref_mv
  }

  # Exact match
  exact <- ref_sub |> filter(ref_name_clean == nm)
  if (nrow(exact) == 1) {
    mv21_matched$ref_ags[i] <- exact$ref_ags
    next
  }

  # Fuzzy match
  dists <- adist(nm, ref_sub$ref_name_clean, ignore.case = TRUE, partial = FALSE)
  best <- which.min(dists)
  if (length(best) > 0 && dists[best] <= 5) {
    mv21_matched$ref_ags[i] <- ref_sub$ref_ags[best]
    if (dists[best] > 0) {
      cat("  Fuzzy matched:", nm, "->", ref_sub$ref_name_clean[best],
          "(AGS:", ref_sub$ref_ags[best], ")\n")
    }
  } else {
    cat("  FAILED to match:", nm, "(county:", cp, ")\n")
  }
}

mv21_munis <- mv21_matched |>
  filter(!is.na(ref_ags)) |>
  mutate(ags = ref_ags)

cat("Matched municipalities:", nrow(mv21_munis), "\n")

# Fix duplicate AGS: two "Neuenkirchen" in county 13075 get the same AGS.
# Use BBSR population to disambiguate: 13075101 (pop 221) and 13075102 (pop 2409).
# The Neuenkirchen with fewer eligible voters maps to the smaller population AGS.
mv21_dup_ags <- mv21_munis |>
  group_by(ags) |> filter(n() > 1) |> ungroup()
if (nrow(mv21_dup_ags) > 0) {
  cat("Resolving", nrow(mv21_dup_ags), "duplicate AGS rows by population\n")
  ref_pop <- mv21_ref_mv |>
    filter(ref_ags %in% mv21_dup_ags$ags |
           (ref_county %in% unique(substr(mv21_dup_ags$ags, 1, 5)) &
            ref_name_clean %in% mv21_dup_ags$name_clean))
  # For each set of duplicates, sort by EV and assign AGS in population order
  for (dup_name in unique(mv21_dup_ags$name_clean)) {
    dup_cp <- mv21_dup_ags$county_prefix[mv21_dup_ags$name_clean == dup_name][1]
    # Get all ref entries with this name in this county
    ref_candidates <- mv21_ref_mv |>
      filter(ref_name_clean == dup_name, ref_county == dup_cp) |>
      arrange(ref_ags)
    dup_rows <- which(mv21_munis$name_clean == dup_name &
                      mv21_munis$county_prefix == dup_cp)
    if (length(dup_rows) == nrow(ref_candidates)) {
      # Sort data rows by eligible voters, assign AGS in population order
      ev_order <- order(mv21_munis$...4[dup_rows])
      for (j in seq_along(dup_rows)) {
        mv21_munis$ags[dup_rows[ev_order[j]]] <- ref_candidates$ref_ags[j]
      }
      cat("  Resolved:", dup_name, "->",
          paste(ref_candidates$ref_ags, collapse = ", "), "\n")
    }
  }
}

# Final duplicate check
mv21_dup_check <- mv21_munis |> group_by(ags) |> filter(n() > 1)
if (nrow(mv21_dup_check) > 0) {
  cat("WARNING: Still have duplicate AGS:", nrow(mv21_dup_check), "rows\n")
}

## Step 4: Briefwahl allocation using Amt mapping from GV file
# Read GV (Gemeindeverzeichnis) for Amt membership
mv21_gv <- read_xlsx(
  here("data/crosswalks/raw/31122021_Auszug_GV.xlsx"),
  sheet = 2, col_types = "text", skip = 3
)

# Get municipality -> Amt mapping (MV, Satzart=60)
mv21_gem_amt <- mv21_gv |>
  filter(...1 == "60", Land == "13", VB != "9999", Gem != "999") |>
  mutate(
    ags = paste0(Land, "0", Kreis, Gem),
    amt_code = paste0(Land, "0", Kreis, VB)
  ) |>
  select(ags, amt_code, VB)

# Get Amt names (Satzart=50 for VG/Amt level, VB starting with 5 = actual Ämter)
mv21_amt_names <- mv21_gv |>
  filter(...1 == "50", Land == "13", str_detect(VB, "^5")) |>
  mutate(
    amt_code = paste0(Land, "0", Kreis, VB),
    amt_name = clean_name(...8)
  ) |>
  select(amt_code, amt_name)

# Map Briefwahl names to Amt names
# Briefwahl names are "Briefwahl [AmtName]" -> extract AmtName
mv21_brief <- mv21_brief |>
  mutate(
    brief_name = clean_name(str_remove(name, "^Briefwahl\\s+")),
    # "Amt Grabow" -> match to "Grabow" in amt_names
    brief_name_alt = str_remove(brief_name, "^Amt\\s+")
  )

# Match Briefwahl to Amt code
mv21_brief_matched <- mv21_brief |>
  left_join(mv21_amt_names, by = c("brief_name" = "amt_name"))
# Try alternative name for unmatched
unmatched_brief <- mv21_brief_matched |> filter(is.na(amt_code))
if (nrow(unmatched_brief) > 0) {
  for (i in seq_len(nrow(unmatched_brief))) {
    # Try exact match on alt name
    match_idx <- which(mv21_amt_names$amt_name == unmatched_brief$brief_name_alt[i])
    if (length(match_idx) == 1) {
      unmatched_brief$amt_code[i] <- mv21_amt_names$amt_code[match_idx]
    } else {
      # Fuzzy match
      dists <- adist(unmatched_brief$brief_name_alt[i], mv21_amt_names$amt_name,
                     ignore.case = TRUE)
      best <- which.min(dists)
      if (dists[best] <= 5) {
        unmatched_brief$amt_code[i] <- mv21_amt_names$amt_code[best]
        cat("  Briefwahl fuzzy:", unmatched_brief$brief_name[i], "->",
            mv21_amt_names$amt_name[best], "\n")
      } else {
        cat("  Briefwahl UNMATCHED:", unmatched_brief$brief_name[i], "\n")
      }
    }
  }
  mv21_brief_matched <- mv21_brief_matched |>
    filter(!is.na(amt_code)) |>
    bind_rows(unmatched_brief)
}

cat("Matched Briefwahl entries:", sum(!is.na(mv21_brief_matched$amt_code)),
    "of", nrow(mv21_brief_matched), "\n")

# Get amtsangehörige municipalities for each Amt
mv21_amt_members <- mv21_gem_amt |>
  filter(str_detect(VB, "^5")) |>
  select(ags, amt_code)

# Join to get Amt code for each municipality in our data
mv21_munis <- mv21_munis |>
  left_join(mv21_amt_members, by = "ags")

# Compute allocation weights (eligible voters per municipality within each Amt)
mv21_weights <- mv21_munis |>
  filter(!is.na(amt_code)) |>
  group_by(amt_code) |>
  mutate(weight = ...4 / sum(...4, na.rm = TRUE)) |>
  ungroup() |>
  select(ags, amt_code, weight)

# Allocate Briefwahl votes to municipalities
mv21_brief_alloc <- mv21_brief_matched |>
  filter(!is.na(amt_code)) |>
  select(amt_code, brief_voters = ...5, brief_valid = ...7,
    brief_spd = ...8, brief_afd = ...9, brief_cdu = ...10,
    brief_linke = ...11, brief_grune = ...12, brief_fdp = ...13,
    brief_npd = ...14, brief_tierschutz = ...15, brief_freier_horizont = ...16,
    brief_die_partei = ...17, brief_freie_wahler = ...18, brief_piraten = ...19,
    brief_dkp = ...20, brief_bundnis_c = ...21, brief_tierschutz_hier = ...22,
    brief_diebasis = ...23, brief_dib = ...24, brief_fpa = ...25,
    brief_lkr = ...26, brief_odp = ...27, brief_humanisten = ...28,
    brief_gesundheit = ...29, brief_todenhofer = ...30,
    brief_unabhangige = ...31, brief_sonstige = ...32) |>
  inner_join(mv21_weights, by = "amt_code", relationship = "many-to-many") |>
  mutate(across(starts_with("brief_"), ~ round(.x * weight))) |>
  select(-amt_code, -weight)

# Aggregate allocated Briefwahl by municipality
mv21_brief_by_muni <- mv21_brief_alloc |>
  group_by(ags) |>
  summarize(across(starts_with("brief_"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop")

## Step 5: Combine municipality (Urne) + allocated Briefwahl votes
# Build final dataset
mv21_final <- mv21_munis |>
  left_join(mv21_brief_by_muni, by = "ags") |>
  transmute(
    ags,
    county = substr(ags, 1, 5),
    election_year = as.numeric(2021),
    election_date = as.Date("2021-09-26"),
    state = as.character("13"),
    # Wahlberechtigte comes from Urne rows only (Briefwahl has "-")
    eligible_voters = ...4,
    # Add Briefwahl voters to Urne voters
    number_voters = ...5 + coalesce(brief_voters, 0),
    valid_votes = ...7 + coalesce(brief_valid, 0),
    spd_n   = ...8 + coalesce(brief_spd, 0),
    afd_n   = ...9 + coalesce(brief_afd, 0),
    cdu_n   = ...10 + coalesce(brief_cdu, 0),
    linke_n = ...11 + coalesce(brief_linke, 0),
    grune_n = ...12 + coalesce(brief_grune, 0),
    fdp_n   = ...13 + coalesce(brief_fdp, 0),
    npd_n   = ...14 + coalesce(brief_npd, 0),
    tierschutzpartei_n = ...15 + coalesce(brief_tierschutz, 0),
    die_partei_n    = ...17 + coalesce(brief_die_partei, 0),
    freie_wahler_n  = ...18 + coalesce(brief_freie_wahler, 0),
    piraten_n       = ...19 + coalesce(brief_piraten, 0),
    diebasis_n      = ...23 + coalesce(brief_diebasis, 0),
    die_humanisten_n = ...28 + coalesce(brief_humanisten, 0)
  ) |>
  mutate(
    other_n = valid_votes - rowSums(across(any_of(c("cdu_n", "spd_n", "grune_n",
      "fdp_n", "linke_n", "afd_n"))), na.rm = TRUE),
    csu = as.numeric(NA),
    turnout = valid_votes / eligible_voters,
    cdu = cdu_n / valid_votes,
    spd = spd_n / valid_votes,
    grune = grune_n / valid_votes,
    fdp = fdp_n / valid_votes,
    linke_pds = linke_n / valid_votes,
    afd = afd_n / valid_votes,
    npd = npd_n / valid_votes,
    freie_wahler = freie_wahler_n / valid_votes,
    die_partei = die_partei_n / valid_votes,
    tierschutzpartei = tierschutzpartei_n / valid_votes,
    piraten = piraten_n / valid_votes,
    diebasis = diebasis_n / valid_votes,
    die_humanisten = die_humanisten_n / valid_votes,
    other = other_n / valid_votes,
    cdu_csu = cdu
  ) |>
  select(
    ags, county, election_year, state, election_date,
    eligible_voters, number_voters, valid_votes, turnout,
    all_of(mv21_partylist),
    other,
    cdu_csu
  )

### Final Check
mv21_final |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

mv21_totalvoters <- mv21_final |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("MV 2021 eligible voters:", mv21_totalvoters, "\n")
cat("MV 2021 valid votes:", sum(mv21_final$valid_votes, na.rm = TRUE), "\n")
cat("MV 2021 rows:", nrow(mv21_final), "\n")

#### Hessen 2008 ####
## Party List
he08_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "freie_wahler",
  "npd",
  "piraten",
  "tierschutzpartei",
  "cdu_csu"
)

## Read Raw File
# 5 header rows; row 6 = state total ("Land Hessen"); rows 7+ = municipalities
# Landesstimmen: valid_votes=col 33, party cols 34-50
he08_raw <- read_xlsx(
  path = here(
    path,
    "Hessen/Hessen_1946-2018_Landtagswahl_Gemeindeebene .xlsx"
  ),
  sheet = "2008",
  skip = 5,
  col_names = FALSE,
  col_types = "text"
)

## Clean Dataset
# Row 1 = state total (Land Hessen); filter to rows with valid 6-digit GKZ
he08_data <- he08_raw |>
  filter(!is.na(...2) & str_detect(...2, "^[0-9]+$"))

cat("HE 2008 municipality rows:", nrow(he08_data), "\n")

# Column mapping:
# col 2 = GKZ (6-digit), col 7 = eligible_voters (Insgesamt),
# col 8 = number_voters (Wähler), col 33 = valid Landesstimmen
# Landesstimmen party cols:
# 34=CDU, 35=SPD, 36=GRÜNE, 37=FDP, 38=REP, 39=Tierschutz, 40=BüSo,
# 41=PSG, 42=Volksabstimmung, 43=GRAUE, 44=DIE LINKE, 45=Die Violetten,
# 46=FAMILIE, 47=FREIE WÄHLER, 48=NPD, 49=PIRATEN, 50=UB

he08_numeric_cols <- c(7, 8, 33:50)
he08_data <- he08_data |>
  mutate(across(all_of(he08_numeric_cols),
    ~ as.numeric(na_if(.x, "-"))))

he08_data <- he08_data |>
  transmute(
    ags = paste0("06", sprintf("%06d", as.numeric(...2))),
    county = substr(ags, 1, 5),
    election_year = as.numeric(2008),
    election_date = as.Date("2008-01-27"),
    state = as.character("06"),
    eligible_voters = ...7,
    number_voters = ...8,
    valid_votes = ...33,
    cdu_n = ...34,
    spd_n = ...35,
    grune_n = ...36,
    fdp_n = ...37,
    linke_n = ...44,
    freie_wahler_n = ...47,
    npd_n = ...48,
    piraten_n = ...49,
    tierschutzpartei_n = ...39
  ) |>
  mutate(
    other_n = valid_votes - rowSums(across(any_of(c("cdu_n", "spd_n", "grune_n",
      "fdp_n", "linke_n"))), na.rm = TRUE),
    csu = as.numeric(NA),
    turnout = valid_votes / eligible_voters,
    cdu = cdu_n / valid_votes,
    spd = spd_n / valid_votes,
    grune = grune_n / valid_votes,
    fdp = fdp_n / valid_votes,
    linke_pds = linke_n / valid_votes,
    freie_wahler = freie_wahler_n / valid_votes,
    npd = npd_n / valid_votes,
    piraten = piraten_n / valid_votes,
    tierschutzpartei = tierschutzpartei_n / valid_votes,
    other = other_n / valid_votes,
    cdu_csu = cdu
  ) |>
  select(
    ags, county, election_year, state, election_date,
    eligible_voters, number_voters, valid_votes, turnout,
    all_of(he08_partylist),
    other,
    cdu_csu
  )

### Final Check
he08_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

he08_totalvoters <- he08_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("HE 2008 eligible voters:", he08_totalvoters, "\n")
cat("HE 2008 valid votes:", sum(he08_data$valid_votes, na.rm = TRUE), "\n")
cat("HE 2008 rows:", nrow(he08_data), "\n")

#### Schleswig-Holstein 2017 ####
## Party List
sh17_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "piraten",
  "freie_wahler",
  "die_partei",
  "cdu_csu"
)

## Read Raw File
# Polling-station level data with separate Briefwahl rows
# Row 1 = title, Row 2 = column headers -> skip = 2
sh17_raw <- read_xlsx(
  path = here(
    path,
    "Schleswig-Holstein/Schleswig-Holstein_2017_Landtag.xlsx"
  ),
  sheet = "2017",
  skip = 2,
  col_names = FALSE
)

## Assign column names
colnames(sh17_raw)[1:6] <- c(
  "kennziffer", "kreisname", "gemeindename", "wbz_name",
  "amterschlussel", "amtername"
)
colnames(sh17_raw)[c(11, 12, 14, 17, 18)] <- c(
  "ev_without_w", "ev_with_w", "eligible_voters",
  "brief_voters", "number_voters"
)
colnames(sh17_raw)[c(35, 36)] <- c("invalid_zweit", "valid_zweit")
colnames(sh17_raw)[37:49] <- c(
  "cdu", "spd", "grune", "fdp", "piraten", "ssw",
  "linke_pds", "familie", "freie_wahler", "afd", "lkr",
  "die_partei", "z_sh"
)

## Convert to numeric
sh17_raw <- sh17_raw |>
  mutate(across(
    c(
      eligible_voters, ev_with_w, number_voters, valid_zweit,
      cdu, spd, grune, fdp, piraten, ssw, linke_pds, familie,
      freie_wahler, afd, lkr, die_partei, z_sh
    ),
    as.numeric
  ))

## Separate regular and Briefwahl rows
# Digit 3 of kennziffer: 9 = Briefwahl, 0/1 = regular
sh17_raw <- sh17_raw |>
  mutate(is_brief = substr(kennziffer, 3, 3) == "9")

sh17_regular <- sh17_raw |> filter(!is_brief)
sh17_brief_gem <- sh17_raw |>
  filter(is_brief & !is.na(gemeindename))
sh17_brief_amt <- sh17_raw |>
  filter(is_brief & is.na(gemeindename) & !is.na(amterschlussel))

## Allocate Amt-level Briefwahl to municipalities
sh17_party_cols <- c(
  "cdu", "spd", "grune", "fdp", "piraten", "ssw",
  "linke_pds", "familie", "freie_wahler", "afd", "lkr",
  "die_partei", "z_sh"
)

# Aggregate Briefwahl by Amt
sh17_amt_brief <- sh17_brief_amt |>
  group_by(amterschlussel) |>
  summarize(
    across(
      c(number_voters, valid_zweit, all_of(sh17_party_cols)),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Get municipality weights within each Amt (based on Wahlschein voters)
sh17_gem_weights <- sh17_regular |>
  filter(!is.na(gemeindename) & !is.na(amterschlussel)) |>
  group_by(amterschlussel, kreisname, gemeindename) |>
  summarize(ev_with_w = sum(ev_with_w, na.rm = TRUE), .groups = "drop") |>
  group_by(amterschlussel) |>
  mutate(weight = ev_with_w / sum(ev_with_w, na.rm = TRUE)) |>
  ungroup()

# Distribute Amt-level Briefwahl proportionally
sh17_brief_alloc <- sh17_gem_weights |>
  inner_join(sh17_amt_brief, by = "amterschlussel", suffix = c("_gem", "_brief")) |>
  mutate(
    across(
      c(number_voters, valid_zweit, all_of(sh17_party_cols)),
      ~ round(.x * weight)
    )
  ) |>
  mutate(eligible_voters = 0) |>
  select(
    kreisname, gemeindename, eligible_voters, number_voters,
    valid_zweit, all_of(sh17_party_cols)
  )

## Combine all rows and aggregate to municipality level
sh17_select_cols <- c(
  "kreisname", "gemeindename", "eligible_voters", "number_voters",
  "valid_zweit", sh17_party_cols
)

sh17_combined <- bind_rows(
  sh17_regular |> select(all_of(sh17_select_cols)),
  sh17_brief_gem |> select(all_of(sh17_select_cols)),
  sh17_brief_alloc
) |>
  filter(!is.na(gemeindename))

sh17_data <- sh17_combined |>
  group_by(kreisname, gemeindename) |>
  summarize(
    eligible_voters = sum(eligible_voters, na.rm = TRUE),
    number_voters = sum(number_voters, na.rm = TRUE),
    valid_votes = sum(valid_zweit, na.rm = TRUE),
    across(all_of(sh17_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

## AGS Crosswalk
# SH uses non-standard Kennziffer identifiers; need to map Gemeindename to standard AGS
sh17_kreis_map <- tibble(
  kreisname = c(
    "Flensburg", "Kiel", "Lübeck", "Neumünster",
    "Dithmarschen", "Herzogtum Lauenburg", "Nordfriesland", "Ostholstein",
    "Pinneberg", "Plön", "Rendsburg-Eckernförde", "Schleswig-Flensburg",
    "Segeberg", "Steinburg", "Stormarn"
  ),
  county_code = c(
    "01001", "01002", "01003", "01004",
    "01051", "01053", "01054", "01055",
    "01056", "01057", "01058", "01059",
    "01060", "01061", "01062"
  )
)

# Load AGS reference from crosswalk for 2017
sh17_ags_ref <- read_rds(
  here("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")
) |>
  filter(startsWith(ags, "01") & year == 2017) |>
  select(ags, ags_name) |>
  distinct() |>
  mutate(
    county_code = substr(ags, 1, 5),
    name_clean = str_trim(str_remove(ags_name, ",.*$"))
  )

# Match by county + cleaned name, then handle exceptions
sh17_data <- sh17_data |>
  left_join(sh17_kreis_map, by = "kreisname") |>
  mutate(name_clean = str_trim(str_remove(gemeindename, ",.*$")))

# Manual fixes for name mismatches before joining
sh17_data <- sh17_data |>
  mutate(
    name_clean = case_when(
      gemeindename == "Lauenburg/Elbe, Stadt" ~ "Lauenburg/ Elbe",
      gemeindename == "Niendorf a. d. Stecknitz" ~ "Niendorf/ Stecknitz",
      gemeindename == "Wentorf (Amt Sandesneben-Nusse)" ~ "Wentorf (Amt Sandesneben)",
      gemeindename == "Ülsby" ~ "Uelsby",
      gemeindename == "Alt-Mölln" ~ "Alt Mölln",
      TRUE ~ name_clean
    )
  )

# Handle Garding disambiguation (Kirchspiel vs Stadt in Nordfriesland)
sh17_data <- sh17_data |>
  mutate(
    name_clean = case_when(
      gemeindename == "Garding, Kirchspiel" ~ "Garding, Kirchspiel",
      gemeindename == "Garding, Stadt" ~ "Garding, Stadt",
      TRUE ~ name_clean
    )
  )

sh17_ags_ref <- sh17_ags_ref |>
  mutate(
    name_clean = case_when(
      ags_name %in% c("Garding, Kirchspiel", "Garding, Stadt") ~ ags_name,
      TRUE ~ name_clean
    )
  )

sh17_data <- sh17_data |>
  left_join(
    sh17_ags_ref |> select(ags, county_code, name_clean),
    by = c("county_code", "name_clean")
  )

# Oldenbüttel/Tackesdorf: combined Wahlbezirk for two tiny municipalities;
# Kennziffer 58119001 points to Oldenbüttel (01058119) — assign there
sh17_data <- sh17_data |>
  mutate(
    ags = case_when(
      gemeindename == "Oldenbüttel/Tackesdorf" ~ "01058119",
      TRUE ~ ags
    )
  )

# Check unmatched
sh17_unmatched <- sh17_data |> filter(is.na(ags))
if (nrow(sh17_unmatched) > 0) {
  cat("SH 2017 unmatched municipalities:", nrow(sh17_unmatched), "\n")
  print(sh17_unmatched |> select(kreisname, gemeindename, eligible_voters))
}

# Aggregate any duplicate AGS (municipalities that map to same AGS)
sh17_data <- sh17_data |>
  filter(!is.na(ags)) |>
  group_by(ags) |>
  summarize(
    county = first(county_code),
    eligible_voters = sum(eligible_voters, na.rm = TRUE),
    number_voters = sum(number_voters, na.rm = TRUE),
    valid_votes = sum(valid_votes, na.rm = TRUE),
    across(all_of(sh17_party_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

## Construct final variables
sh17_data <- sh17_data |>
  mutate(
    election_year = as.numeric(2017),
    election_date = as.Date("2017-05-07"),
    state = as.character("01"),
    csu = as.numeric(NA),
    cdu_csu = cdu
  )

## Calculations
sh17_data <- sh17_data |>
  mutate(
    turnout = valid_votes / eligible_voters,
    across(all_of(sh17_partylist), ~ .x / valid_votes)
  ) |>
  mutate(
    other = 1 - rowSums(across(any_of(c("cdu_csu", "spd", "grune", "fdp", "linke_pds", "afd"))), na.rm = TRUE)
  ) |>
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
    all_of(sh17_partylist),
    other,
    cdu_csu
  )

### Final Check
sh17_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

sh17_totalvoters <- sh17_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("SH 2017 eligible voters:", sh17_totalvoters, "\n")
cat("SH 2017 valid votes:", sum(sh17_data$valid_votes, na.rm = TRUE), "\n")
cat("SH 2017 rows:", nrow(sh17_data), "\n")

#### Rheinland-Pfalz 2021 ####
## Party List
rlp21_partylist <- c(
  "cdu",
  "csu",
  "spd",
  "grune",
  "fdp",
  "linke_pds",
  "afd",
  "freie_wahler",
  "piraten",
  "die_partei",
  "tierschutzpartei",
  "volt",
  "odp",
  "cdu_csu"
)

## Read Raw File
# 8440 rows, 85 cols; Stimmbezirk-level with hierarchy
# Col 1 = ID (13-digit), Col 2 = Stimmbezirk, Col 3 = Bezeichnung, Col 4 = GUW
# Zweitstimmen: valid=col 52, parties at even cols 54,56,...,84
# Admin: eligible_voters=col 6, number_voters=col 10
rlp21_raw <- read_xlsx(
  path = here(path, "Rheinland-Pfalz/LW_2021_GESAMT.xlsx"),
  col_names = FALSE,
  col_types = "text"
) |>
  slice(-1)  # Remove header row

## Filter to Gemeinde-level totals
# Stimmbezirk = "00000" and GUW = "G" gives pre-aggregated Gemeinde rows
rlp21_gem <- rlp21_raw |>
  filter(...2 == "00000" & ...4 == "G") |>
  filter(!grepl("Landesergebnis|Bezirk [0-9]", ...3))

cat("RLP 2021 Gemeinde-level rows:", nrow(rlp21_gem), "\n")

## Separate kreisfreie Städte and non-kreisfreie municipalities
# Kreisfreie: ID starts with "00", use only the state-level aggregate row
# (not Wahlkreis-level splits where ID starts with non-"00")
rlp21_kreisfrei <- rlp21_gem |>
  filter(substr(...1, 1, 2) == "00" & grepl("Kreisfreie", ...3)) |>
  mutate(ags = paste0("07", substr(...1, 4, 6), "000"))

cat("RLP 2021 kreisfreie Städte:", nrow(rlp21_kreisfrei), "\n")

# Non-kreisfreie: ID digits 1-2 != "00", digits 9-11 != "000", digits 12-13 == "00"
# AGS = "07" + substr(id, 4, 6) [county] + substr(id, 9, 11) [municipality]
rlp21_munis <- rlp21_gem |>
  filter(
    substr(...1, 1, 2) != "00",
    substr(...1, 9, 11) != "000",
    substr(...1, 12, 13) == "00"
  ) |>
  mutate(ags = paste0("07", substr(...1, 4, 6), substr(...1, 9, 11)))

cat("RLP 2021 non-kreisfreie municipalities:", nrow(rlp21_munis), "\n")

## Combine
rlp21_data <- bind_rows(rlp21_kreisfrei, rlp21_munis)
cat("RLP 2021 combined municipalities:", nrow(rlp21_data), "\n")

## Convert numeric columns
rlp21_numeric_cols <- c(6, 10, 52, seq(54, 84, by = 2))
rlp21_data <- rlp21_data |>
  mutate(across(all_of(rlp21_numeric_cols),
    ~ as.numeric(na_if(.x, "-"))))

## Aggregate by AGS (some municipalities split across Wahlkreise)
rlp21_data <- rlp21_data |>
  group_by(ags) |>
  summarize(
    eligible_voters = sum(...6, na.rm = TRUE),
    number_voters = sum(...10, na.rm = TRUE),
    valid_votes = sum(...52, na.rm = TRUE),
    spd_n = sum(...54, na.rm = TRUE),
    cdu_n = sum(...56, na.rm = TRUE),
    afd_n = sum(...58, na.rm = TRUE),
    fdp_n = sum(...60, na.rm = TRUE),
    grune_n = sum(...62, na.rm = TRUE),
    linke_n = sum(...64, na.rm = TRUE),
    freie_wahler_n = sum(...66, na.rm = TRUE),
    piraten_n = sum(...68, na.rm = TRUE),
    odp_n = sum(...70, na.rm = TRUE),
    klimaliste_n = sum(...72, na.rm = TRUE),
    die_partei_n = sum(...74, na.rm = TRUE),
    tierschutzpartei_n = sum(...76, na.rm = TRUE),
    volt_n = sum(...78, na.rm = TRUE),
    basisdemokratie_n = sum(...80, na.rm = TRUE),
    dr_moritz_n = sum(...82, na.rm = TRUE),
    siggi_n = sum(...84, na.rm = TRUE),
    .groups = "drop"
  )

cat("RLP 2021 after AGS aggregation:", nrow(rlp21_data), "\n")

## Compute shares
rlp21_data <- rlp21_data |>
  mutate(
    county = substr(ags, 1, 5),
    election_year = as.numeric(2021),
    election_date = as.Date("2021-03-14"),
    state = as.character("07"),
    other_n = valid_votes - rowSums(across(any_of(c("cdu_n", "spd_n", "grune_n",
      "fdp_n", "linke_n", "afd_n"))), na.rm = TRUE),
    csu = as.numeric(NA),
    turnout = valid_votes / eligible_voters,
    cdu = cdu_n / valid_votes,
    spd = spd_n / valid_votes,
    grune = grune_n / valid_votes,
    fdp = fdp_n / valid_votes,
    linke_pds = linke_n / valid_votes,
    afd = afd_n / valid_votes,
    freie_wahler = freie_wahler_n / valid_votes,
    piraten = piraten_n / valid_votes,
    odp = odp_n / valid_votes,
    die_partei = die_partei_n / valid_votes,
    tierschutzpartei = tierschutzpartei_n / valid_votes,
    volt = volt_n / valid_votes,
    other = other_n / valid_votes,
    cdu_csu = cdu
  ) |>
  select(
    ags, county, election_year, state, election_date,
    eligible_voters, number_voters, valid_votes, turnout,
    all_of(rlp21_partylist),
    other,
    cdu_csu
  )

### Final Check
rlp21_data |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1)

rlp21_totalvoters <- rlp21_data |>
  summarize(total = sum(eligible_voters, na.rm = TRUE)) |>
  pull(total)

cat("RLP 2021 eligible voters:", rlp21_totalvoters, "\n")
cat("RLP 2021 valid votes:", sum(rlp21_data$valid_votes, na.rm = TRUE), "\n")
cat("RLP 2021 rows:", nrow(rlp21_data), "\n")

#### Bind and Write ####
state2224 <- bind_rows(
  by23_data, he23_data, ni22_data,
  sl22_data, nrw22_data, sh22_data,
  hb23_data, be23_data,
  bb24_data, sn24_data, th24_data,
  bw21_data, st21_data, be21_data, mv21_final,
  he08_data, sh17_data, rlp21_data
)

# Change cdu / csu inconsistencies
state2224 <- state2224 |>
  mutate(
    cdu = ifelse(state != "09" & (cdu == 0 | is.na(cdu)), cdu_csu, cdu),
    csu = ifelse(state == "09" & (csu == 0 | is.na(csu)), cdu_csu, csu)
  )


write_csv(
  state2224,
  here(
    path,
    "../../../state_elections/final",
    "state_2224_unharm.csv"
  )
)

write_rds(
  state2224,
  here(
    path,
    "../../../state_elections/final",
    "state_2224_unharm.rds"
  )
)


# Inspect
state2224 <- read_rds(here(path, "../../../state_elections/final", "state_2224_unharm.rds"))

glimpse(state2224)
