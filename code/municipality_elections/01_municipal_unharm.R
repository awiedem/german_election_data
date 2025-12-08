###################################
###### German Kommunalwahlen ######
###################################

# Florian Sichart
# Last update: November 2025 (Luca Schenk)

########## PREPARATION ----
rm(list = ls())
gc()

library(pacman)
pacman::p_load(
  janitor,
  writexl,
  readr,
  stringr,
  tidyverse,
  grid,
  gridExtra,
  tidyverse,
  broom,
  tidyr,
  data.table,
  readxl,
  dplyr,
  stringi,
  conflicted
)

conflict_prefer_all("dplyr")

# https://github.com/seligerf/Imputation-of-missing-location-information-for-worldwide-patent-data
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OTTBDX
# https://www.nature.com/articles/s41597-019-0264-6

# Set WD
setwd(here::here("data/municipal_elections"))

options(scipen = 999)


########## DATA PROCESSING ----
######### BAYERN ----
#### Load election data ----
bayern_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/bayern/bayern_all_parties_copy.xlsx",
  sheet = "master"
))

bayern_kommunalwahlen_waehler <- as.data.table(read_excel(
  "raw/bayern/bayern_waehler_1996-2020.xlsx",
  sheet = "overall"
))
bayern_kommunalwahlen_gueltige <- as.data.table(read_excel(
  "raw/bayern/bayern_gueltige_1996-2020.xlsx",
  sheet = "master"
))

bayern_kommunalwahlen_data <- merge(
  bayern_kommunalwahlen_data,
  bayern_kommunalwahlen_waehler,
  by = "AGS",
  all.x = TRUE
)
bayern_kommunalwahlen_data <- merge(
  bayern_kommunalwahlen_data,
  bayern_kommunalwahlen_gueltige,
  by = "AGS",
  all.x = TRUE
)


###### Bayern 1990 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_1990 <- data.table(read_excel(
  "raw/bayern/bayern_1990new.xlsx",
  sheet = "results"
))
bayern_waehler_data_1990 <- data.table(read_excel(
  "raw/bayern/bayern_waehler_1990.xlsx",
  sheet = "results"
)) %>%
  filter(nchar(AGS) == 8)

# LK for Staedte
bayern_kreiswahlen_data_1990 <- data.table(read_excel(
  "raw/bayern/bayern_1990_krfr_staedte.xlsx",
  sheet = "results"
))
bayern_kreiswahlen_data_1990_abgegebene <- data.table(read_excel(
  "raw/bayern/bayern_1990_staedte_abgegebene.xlsx",
  sheet = "results"
))
bayern_kreiswahlen_data_1990_wahlberechtigte <- data.table(read_excel(
  "raw/bayern/bayern_1990_staedte_wahlberechtigte.xlsx",
  sheet = "results"
))

bayern_kreiswahlen_data_1990_sub <- bayern_kreiswahlen_data_1990 %>%
  left_join(
    select(bayern_kreiswahlen_data_1990_abgegebene, -Name),
    by = join_by(ags == ags)
  ) %>%
  left_join(
    select(bayern_kreiswahlen_data_1990_wahlberechtigte, -name),
    by = join_by(ags == ags)
  )

#### Recoding ----
# Create new dataframe ----
bayern_1990_kommunalwahlen_data_sub <- NA
bayern_1990_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_1990 %>%
  filter(nchar(AGS) == 8) %>%
  filter(!Gemeinde == "Gemeindefreie Gebiete") %>%
  filter(!is.na(Insgesamt)) %>%
  left_join(bayern_waehler_data_1990, by = join_by(AGS == AGS))

# Creating non-existing variables ----
bayern_1990_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_1990_kommunalwahlen_data_sub[, Bundesland := "Bayern"]
bayern_1990_kommunalwahlen_data_sub[, Gebietsname := ""]
bayern_1990_kommunalwahlen_data_sub[, election_year := "1990"]
bayern_1990_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
bayern_1990_kommunalwahlen_data_sub[, IDIRB := ""]
bayern_1990_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
bayern_1990_kommunalwahlen_data_sub$AGS_8dig <- bayern_1990_kommunalwahlen_data_sub$AGS
bayern_1990_kommunalwahlen_data_sub$Gebietsname <- bayern_1990_kommunalwahlen_data_sub$Gemeinde
bayern_1990_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_1990_kommunalwahlen_data_sub$Wahlberechtigte
bayern_1990_kommunalwahlen_data_sub$Wähler <- bayern_1990_kommunalwahlen_data_sub$Wähler
bayern_1990_kommunalwahlen_data_sub$GültigeStimmen <- bayern_1990_kommunalwahlen_data_sub$Insgesamt


bayern_1990_kommunalwahlen_data_sub$abs_CDU <- bayern_1990_kommunalwahlen_data_sub$CSU
bayern_1990_kommunalwahlen_data_sub$abs_SPD <- bayern_1990_kommunalwahlen_data_sub$SDP
bayern_1990_kommunalwahlen_data_sub$abs_DIELINKE <- bayern_1990_kommunalwahlen_data_sub$LINKE
bayern_1990_kommunalwahlen_data_sub$abs_GRÜNE <- bayern_1990_kommunalwahlen_data_sub$Gruene
bayern_1990_kommunalwahlen_data_sub$abs_AfD <- NA
bayern_1990_kommunalwahlen_data_sub$abs_PIRATEN <- NA
bayern_1990_kommunalwahlen_data_sub$abs_FDP <- bayern_1990_kommunalwahlen_data_sub$FDP
bayern_1990_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- bayern_1990_kommunalwahlen_data_sub$FW
bayern_1990_kommunalwahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_1990_kommunalwahlen_data_sub$GW
bayern_1990_kommunalwahlen_data_sub$abs_Wählergruppen <- bayern_1990_kommunalwahlen_data_sub$WG

bayern_1990_kommunalwahlen_data_sub$gew_CDU <- NA
bayern_1990_kommunalwahlen_data_sub$gew_SPD <- NA
bayern_1990_kommunalwahlen_data_sub$gew_DIELINKE <- NA
bayern_1990_kommunalwahlen_data_sub$gew_GRÜNE <- NA
bayern_1990_kommunalwahlen_data_sub$gew_AfD <- NA
bayern_1990_kommunalwahlen_data_sub$gew_PIRATEN <- NA
bayern_1990_kommunalwahlen_data_sub$gew_FDP <- NA
bayern_1990_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
bayern_1990_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_1990_kommunalwahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_1990_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

bayern_1990_kommunalwahlen_data_sub$sitze_CDU <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_SPD <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_AfD <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_FDP <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_1990_kommunalwahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_1990_kommunalwahlen_data_sub <- bayern_1990_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_1990_kommunalwahlen_data_sub <-
  bayern_1990_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_1990_kommunalwahlen_data_sub$Turnout <- as.numeric(
  bayern_1990_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(bayern_1990_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 1990 Kreiswahlen ----
# LK for Staedte
bayern_kreiswahlen_data_1990 <- data.table(read_excel(
  "raw/bayern/bayern_1990_krfr_staedte.xlsx",
  sheet = "results"
))
bayern_kreiswahlen_data_1990_abgegebene <- data.table(read_excel(
  "raw/bayern/bayern_1990_staedte_abgegebene.xlsx",
  sheet = "results"
))
bayern_kreiswahlen_data_1990_wahlberechtigte <- data.table(read_excel(
  "raw/bayern/bayern_1990_staedte_wahlberechtigte.xlsx",
  sheet = "results"
))

bayern_1990_kreiswahlen_data_sub <- bayern_kreiswahlen_data_1990 %>%
  left_join(
    select(bayern_kreiswahlen_data_1990_abgegebene, -Name),
    by = join_by(ags == ags)
  ) %>%
  left_join(
    select(bayern_kreiswahlen_data_1990_wahlberechtigte, -name),
    by = join_by(ags == ags)
  )

#### Recoding ----
# Create new dataframe ----
bayern_1990_kreiswahlen_data_sub <- bayern_1990_kreiswahlen_data_sub %>%
  filter(grepl("(Krfr.St)", name) | grepl("Landeshauptstadt", name)) %>%
  mutate(
    ags = paste0("0", ags, "000")
  )

# Creating non-existing variables ----
bayern_1990_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_1990_kreiswahlen_data_sub[, Bundesland := "Bayern"]
bayern_1990_kreiswahlen_data_sub[, Gebietsname := ""]
bayern_1990_kreiswahlen_data_sub[, election_year := "1990"]
bayern_1990_kreiswahlen_data_sub[, election_type := "Kreiswahlen"]
bayern_1990_kreiswahlen_data_sub[, IDIRB := ""]
bayern_1990_kreiswahlen_data_sub[, IDBA := ""]


# Renaming existing variables ----
bayern_1990_kreiswahlen_data_sub$AGS_8dig <- bayern_1990_kreiswahlen_data_sub$ags
bayern_1990_kreiswahlen_data_sub$Gebietsname <- bayern_1990_kreiswahlen_data_sub$name
bayern_1990_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_1990_kreiswahlen_data_sub$Wahlberechtigte
bayern_1990_kreiswahlen_data_sub$Wähler <- bayern_1990_kreiswahlen_data_sub$Wähler
bayern_1990_kreiswahlen_data_sub$GültigeStimmen <- bayern_1990_kreiswahlen_data_sub$gueltigeStimmen


bayern_1990_kreiswahlen_data_sub$abs_CDU <- bayern_1990_kreiswahlen_data_sub$CSU
bayern_1990_kreiswahlen_data_sub$abs_SPD <- bayern_1990_kreiswahlen_data_sub$SPD
bayern_1990_kreiswahlen_data_sub$abs_DIELINKE <- bayern_1990_kreiswahlen_data_sub$LINKE
bayern_1990_kreiswahlen_data_sub$abs_GRÜNE <- bayern_1990_kreiswahlen_data_sub$Gruene
bayern_1990_kreiswahlen_data_sub$abs_AfD <- NA
bayern_1990_kreiswahlen_data_sub$abs_PIRATEN <- NA
bayern_1990_kreiswahlen_data_sub$abs_FDP <- bayern_1990_kreiswahlen_data_sub$FDP
bayern_1990_kreiswahlen_data_sub$abs_FREIEWÄHLER <- bayern_1990_kreiswahlen_data_sub$FW
bayern_1990_kreiswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_1990_kreiswahlen_data_sub$`Gemeinsame Wahlvorschläge`
bayern_1990_kreiswahlen_data_sub$abs_Wählergruppen <- bayern_1990_kreiswahlen_data_sub$Wählergruppen

bayern_1990_kreiswahlen_data_sub$gew_CDU <- NA
bayern_1990_kreiswahlen_data_sub$gew_SPD <- NA
bayern_1990_kreiswahlen_data_sub$gew_DIELINKE <- NA
bayern_1990_kreiswahlen_data_sub$gew_GRÜNE <- NA
bayern_1990_kreiswahlen_data_sub$gew_AfD <- NA
bayern_1990_kreiswahlen_data_sub$gew_PIRATEN <- NA
bayern_1990_kreiswahlen_data_sub$gew_FDP <- NA
bayern_1990_kreiswahlen_data_sub$gew_DiePARTEI <- NA
bayern_1990_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_1990_kreiswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_1990_kreiswahlen_data_sub$gew_Wählergruppen <- NA

bayern_1990_kreiswahlen_data_sub$sitze_CDU <- NA
bayern_1990_kreiswahlen_data_sub$sitze_SPD <- NA
bayern_1990_kreiswahlen_data_sub$sitze_DIELINKE <- NA
bayern_1990_kreiswahlen_data_sub$sitze_GRÜNE <- NA
bayern_1990_kreiswahlen_data_sub$sitze_AfD <- NA
bayern_1990_kreiswahlen_data_sub$sitze_PIRATEN <- NA
bayern_1990_kreiswahlen_data_sub$sitze_FDP <- NA
bayern_1990_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_1990_kreiswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_1990_kreiswahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_1990_kreiswahlen_data_sub <- bayern_1990_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_1990_kreiswahlen_data_sub <-
  bayern_1990_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_1990_kreiswahlen_data_sub$Turnout <- as.numeric(
  bayern_1990_kreiswahlen_data_sub$Wähler
) /
  as.numeric(bayern_1990_kreiswahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 1996 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_1996 <- data.table(read_excel(
  "raw/bayern/bayern_1996new.xlsx",
  sheet = "results"
))


#### Recoding ----
# Create new dataframe ----
bayern_1996_kommunalwahlen_data_sub <- NA
bayern_1996_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_1996


# Creating non-existing variables ----
bayern_1996_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_1996_kommunalwahlen_data_sub[, Bundesland := "Bayern"]
bayern_1996_kommunalwahlen_data_sub[, Gebietsname := ""]
bayern_1996_kommunalwahlen_data_sub[, election_year := "1996"]
bayern_1996_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
bayern_1996_kommunalwahlen_data_sub[, IDIRB := ""]
bayern_1996_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
bayern_1996_kommunalwahlen_data_sub$AGS_8dig <- bayern_1996_kommunalwahlen_data_sub$AGS
bayern_1996_kommunalwahlen_data_sub$Gebietsname <- bayern_1996_kommunalwahlen_data_sub$Gemeinde
bayern_1996_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_1996_kommunalwahlen_data_sub$Wahlberechtigte
bayern_1996_kommunalwahlen_data_sub$Wähler <- bayern_1996_kommunalwahlen_data_sub$Waehler
bayern_1996_kommunalwahlen_data_sub$GültigeStimmen <- bayern_1996_kommunalwahlen_data_sub$Insgesamt


bayern_1996_kommunalwahlen_data_sub$abs_CDU <- bayern_1996_kommunalwahlen_data_sub$CSU
bayern_1996_kommunalwahlen_data_sub$abs_SPD <- bayern_1996_kommunalwahlen_data_sub$SDP
bayern_1996_kommunalwahlen_data_sub$abs_DIELINKE <- bayern_1996_kommunalwahlen_data_sub$DIELINKE
bayern_1996_kommunalwahlen_data_sub$abs_GRÜNE <- bayern_1996_kommunalwahlen_data_sub$GRUENE
bayern_1996_kommunalwahlen_data_sub$abs_AfD <- bayern_1996_kommunalwahlen_data_sub$AfD
bayern_1996_kommunalwahlen_data_sub$abs_PIRATEN <- bayern_1996_kommunalwahlen_data_sub$PIRATEN
bayern_1996_kommunalwahlen_data_sub$abs_FDP <- bayern_1996_kommunalwahlen_data_sub$FDP
bayern_1996_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- bayern_1996_kommunalwahlen_data_sub$FW
bayern_1996_kommunalwahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_1996_kommunalwahlen_data_sub$`Gemeinsame Wahlvorschläge`
bayern_1996_kommunalwahlen_data_sub$abs_Wählergruppen <- bayern_1996_kommunalwahlen_data_sub$`Wählergruppen`

bayern_1996_kommunalwahlen_data_sub$gew_CDU <- NA
bayern_1996_kommunalwahlen_data_sub$gew_SPD <- NA
bayern_1996_kommunalwahlen_data_sub$gew_DIELINKE <- NA
bayern_1996_kommunalwahlen_data_sub$gew_GRÜNE <- NA
bayern_1996_kommunalwahlen_data_sub$gew_AfD <- NA
bayern_1996_kommunalwahlen_data_sub$gew_PIRATEN <- NA
bayern_1996_kommunalwahlen_data_sub$gew_FDP <- NA
bayern_1996_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
bayern_1996_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_1996_kommunalwahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_1996_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

bayern_1996_kommunalwahlen_data_sub$sitze_CDU <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_SPD <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_AfD <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_FDP <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_1996_kommunalwahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_1996_kommunalwahlen_data_sub <- bayern_1996_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_1996_kommunalwahlen_data_sub <-
  bayern_1996_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_1996_kommunalwahlen_data_sub$Turnout <- as.numeric(
  bayern_1996_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(bayern_1996_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### Bayern 2002 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2002 <- data.table(read_excel(
  "raw/bayern/bayern_2002new.xlsx",
  sheet = "results"
))

#### Recoding ----
# Create new dataframe ----
bayern_2002_kommunalwahlen_data_sub <- NA
bayern_2002_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2002


# Creating non-existing variables ----
bayern_2002_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_2002_kommunalwahlen_data_sub[, Bundesland := "Bayern"]
bayern_2002_kommunalwahlen_data_sub[, Gebietsname := ""]
bayern_2002_kommunalwahlen_data_sub[, election_year := "2002"]
bayern_2002_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
bayern_2002_kommunalwahlen_data_sub[, IDIRB := ""]
bayern_2002_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
bayern_2002_kommunalwahlen_data_sub$AGS_8dig <- bayern_2002_kommunalwahlen_data_sub$AGS
bayern_2002_kommunalwahlen_data_sub$Gebietsname <- bayern_2002_kommunalwahlen_data_sub$Gemeinde
bayern_2002_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_2002_kommunalwahlen_data_sub$Wahlberechtigte
bayern_2002_kommunalwahlen_data_sub$Wähler <- bayern_2002_kommunalwahlen_data_sub$Waehler
bayern_2002_kommunalwahlen_data_sub$GültigeStimmen <- bayern_2002_kommunalwahlen_data_sub$Insgesamt


bayern_2002_kommunalwahlen_data_sub$abs_CDU <- bayern_2002_kommunalwahlen_data_sub$CSU
bayern_2002_kommunalwahlen_data_sub$abs_SPD <- bayern_2002_kommunalwahlen_data_sub$SDP
bayern_2002_kommunalwahlen_data_sub$abs_DIELINKE <- bayern_2002_kommunalwahlen_data_sub$DIELINKE
bayern_2002_kommunalwahlen_data_sub$abs_GRÜNE <- bayern_2002_kommunalwahlen_data_sub$GRUENE
bayern_2002_kommunalwahlen_data_sub$abs_AfD <- bayern_2002_kommunalwahlen_data_sub$AfD
bayern_2002_kommunalwahlen_data_sub$abs_PIRATEN <- bayern_2002_kommunalwahlen_data_sub$PIRATEN
bayern_2002_kommunalwahlen_data_sub$abs_FDP <- bayern_2002_kommunalwahlen_data_sub$FDP
bayern_2002_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- bayern_2002_kommunalwahlen_data_sub$FW
bayern_2002_kommunalwahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_2002_kommunalwahlen_data_sub$`Gemeinsame Wahlvorschläge`
bayern_2002_kommunalwahlen_data_sub$abs_Wählergruppen <- bayern_2002_kommunalwahlen_data_sub$`Wählergruppen`

bayern_2002_kommunalwahlen_data_sub$gew_CDU <- NA
bayern_2002_kommunalwahlen_data_sub$gew_SPD <- NA
bayern_2002_kommunalwahlen_data_sub$gew_DIELINKE <- NA
bayern_2002_kommunalwahlen_data_sub$gew_GRÜNE <- NA
bayern_2002_kommunalwahlen_data_sub$gew_AfD <- NA
bayern_2002_kommunalwahlen_data_sub$gew_PIRATEN <- NA
bayern_2002_kommunalwahlen_data_sub$gew_FDP <- NA
bayern_2002_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
bayern_2002_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_2002_kommunalwahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_2002_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

bayern_2002_kommunalwahlen_data_sub$sitze_CDU <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_SPD <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_AfD <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_FDP <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_2002_kommunalwahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_2002_kommunalwahlen_data_sub <- bayern_2002_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2002_kommunalwahlen_data_sub <-
  bayern_2002_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_2002_kommunalwahlen_data_sub$Turnout <- as.numeric(
  bayern_2002_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(bayern_2002_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 2008 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2008 <- data.table(read_excel(
  "raw/bayern/bayern_2008new.xlsx",
  sheet = "results"
))

#### Recoding ----
# Create new dataframe ----
bayern_2008_kommunalwahlen_data_sub <- NA
bayern_2008_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2008


# Creating non-existing variables ----
bayern_2008_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_2008_kommunalwahlen_data_sub[, Bundesland := "Bayern"]
bayern_2008_kommunalwahlen_data_sub[, Gebietsname := ""]
bayern_2008_kommunalwahlen_data_sub[, election_year := "2008"]
bayern_2008_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
bayern_2008_kommunalwahlen_data_sub[, IDIRB := ""]
bayern_2008_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
bayern_2008_kommunalwahlen_data_sub$AGS_8dig <- bayern_2008_kommunalwahlen_data_sub$AGS
bayern_2008_kommunalwahlen_data_sub$Gebietsname <- bayern_2008_kommunalwahlen_data_sub$Gemeinde
bayern_2008_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_2008_kommunalwahlen_data_sub$Wahlberechtigte
bayern_2008_kommunalwahlen_data_sub$Wähler <- bayern_2008_kommunalwahlen_data_sub$Waehler
bayern_2008_kommunalwahlen_data_sub$GültigeStimmen <- bayern_2008_kommunalwahlen_data_sub$Insgesamt


bayern_2008_kommunalwahlen_data_sub$abs_CDU <- bayern_2008_kommunalwahlen_data_sub$CSU
bayern_2008_kommunalwahlen_data_sub$abs_SPD <- bayern_2008_kommunalwahlen_data_sub$SDP
bayern_2008_kommunalwahlen_data_sub$abs_DIELINKE <- bayern_2008_kommunalwahlen_data_sub$DIELINKE
bayern_2008_kommunalwahlen_data_sub$abs_GRÜNE <- bayern_2008_kommunalwahlen_data_sub$GRUENE
bayern_2008_kommunalwahlen_data_sub$abs_AfD <- bayern_2008_kommunalwahlen_data_sub$AfD
bayern_2008_kommunalwahlen_data_sub$abs_PIRATEN <- bayern_2008_kommunalwahlen_data_sub$PIRATEN
bayern_2008_kommunalwahlen_data_sub$abs_FDP <- bayern_2008_kommunalwahlen_data_sub$FDP
bayern_2008_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- bayern_2008_kommunalwahlen_data_sub$FW
bayern_2008_kommunalwahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_2008_kommunalwahlen_data_sub$`Gemeinsame Wahlvorschläge`
bayern_2008_kommunalwahlen_data_sub$abs_Wählergruppen <- bayern_2008_kommunalwahlen_data_sub$`Wählergruppen`

bayern_2008_kommunalwahlen_data_sub$gew_CDU <- NA
bayern_2008_kommunalwahlen_data_sub$gew_SPD <- NA
bayern_2008_kommunalwahlen_data_sub$gew_DIELINKE <- NA
bayern_2008_kommunalwahlen_data_sub$gew_GRÜNE <- NA
bayern_2008_kommunalwahlen_data_sub$gew_AfD <- NA
bayern_2008_kommunalwahlen_data_sub$gew_PIRATEN <- NA
bayern_2008_kommunalwahlen_data_sub$gew_FDP <- NA
bayern_2008_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
bayern_2008_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_2008_kommunalwahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_2008_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

bayern_2008_kommunalwahlen_data_sub$sitze_CDU <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_SPD <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_AfD <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_FDP <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_2008_kommunalwahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_2008_kommunalwahlen_data_sub <- bayern_2008_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2008_kommunalwahlen_data_sub <-
  bayern_2008_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_2008_kommunalwahlen_data_sub$Turnout <- as.numeric(
  bayern_2008_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(bayern_2008_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 2014 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2014 <- data.table(read_excel(
  "raw/bayern/bayern_2014new.xlsx",
  sheet = "results"
))

#### Recoding ----
# Create new dataframe ----
bayern_2014_kommunalwahlen_data_sub <- NA
bayern_2014_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2014


# Creating non-existing variables ----
bayern_2014_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_2014_kommunalwahlen_data_sub[, Bundesland := "Bayern"]
bayern_2014_kommunalwahlen_data_sub[, Gebietsname := ""]
bayern_2014_kommunalwahlen_data_sub[, election_year := "2014"]
bayern_2014_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
bayern_2014_kommunalwahlen_data_sub[, IDIRB := ""]
bayern_2014_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
bayern_2014_kommunalwahlen_data_sub$AGS_8dig <- bayern_2014_kommunalwahlen_data_sub$AGS
bayern_2014_kommunalwahlen_data_sub$Gebietsname <- bayern_2014_kommunalwahlen_data_sub$Gemeinde
bayern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_2014_kommunalwahlen_data_sub$Wahlberechtigte
bayern_2014_kommunalwahlen_data_sub$Wähler <- bayern_2014_kommunalwahlen_data_sub$Waehler
bayern_2014_kommunalwahlen_data_sub$GültigeStimmen <- bayern_2014_kommunalwahlen_data_sub$Insgesamt


bayern_2014_kommunalwahlen_data_sub$abs_CDU <- bayern_2014_kommunalwahlen_data_sub$CSU
bayern_2014_kommunalwahlen_data_sub$abs_SPD <- bayern_2014_kommunalwahlen_data_sub$SDP
bayern_2014_kommunalwahlen_data_sub$abs_DIELINKE <- bayern_2014_kommunalwahlen_data_sub$DIELINKE
bayern_2014_kommunalwahlen_data_sub$abs_GRÜNE <- bayern_2014_kommunalwahlen_data_sub$GRUENE
bayern_2014_kommunalwahlen_data_sub$abs_AfD <- bayern_2014_kommunalwahlen_data_sub$AfD
bayern_2014_kommunalwahlen_data_sub$abs_PIRATEN <- bayern_2014_kommunalwahlen_data_sub$PIRATEN
bayern_2014_kommunalwahlen_data_sub$abs_FDP <- bayern_2014_kommunalwahlen_data_sub$FDP
bayern_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- bayern_2014_kommunalwahlen_data_sub$FW
bayern_2014_kommunalwahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_2014_kommunalwahlen_data_sub$`Gemeinsame Wahlvorschläge`
bayern_2014_kommunalwahlen_data_sub$abs_Wählergruppen <- bayern_2014_kommunalwahlen_data_sub$`Wählergruppen`

bayern_2014_kommunalwahlen_data_sub$gew_CDU <- NA
bayern_2014_kommunalwahlen_data_sub$gew_SPD <- NA
bayern_2014_kommunalwahlen_data_sub$gew_DIELINKE <- NA
bayern_2014_kommunalwahlen_data_sub$gew_GRÜNE <- NA
bayern_2014_kommunalwahlen_data_sub$gew_AfD <- NA
bayern_2014_kommunalwahlen_data_sub$gew_PIRATEN <- NA
bayern_2014_kommunalwahlen_data_sub$gew_FDP <- NA
bayern_2014_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
bayern_2014_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_2014_kommunalwahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_2014_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

bayern_2014_kommunalwahlen_data_sub$sitze_CDU <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_SPD <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_AfD <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_FDP <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_2014_kommunalwahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_2014_kommunalwahlen_data_sub <- bayern_2014_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2014_kommunalwahlen_data_sub <-
  bayern_2014_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_2014_kommunalwahlen_data_sub$Turnout <- as.numeric(
  bayern_2014_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(bayern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 2020 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2020 <- data.table(read_excel(
  "raw/bayern/bayern_2020new.xlsx",
  sheet = "results"
))

#### Recoding ----
# Create new dataframe ----
bayern_2020_kommunalwahlen_data_sub <- NA
bayern_2020_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2020


# Creating non-existing variables ----
bayern_2020_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bayern_2020_kommunalwahlen_data_sub[, Bundesland := "Bayern"]
bayern_2020_kommunalwahlen_data_sub[, Gebietsname := ""]
bayern_2020_kommunalwahlen_data_sub[, election_year := "2020"]
bayern_2020_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
bayern_2020_kommunalwahlen_data_sub[, IDIRB := ""]
bayern_2020_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
bayern_2020_kommunalwahlen_data_sub$AGS_8dig <- bayern_2020_kommunalwahlen_data_sub$AGS
bayern_2020_kommunalwahlen_data_sub$Gebietsname <- bayern_2020_kommunalwahlen_data_sub$Gemeinde
bayern_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- bayern_2020_kommunalwahlen_data_sub$Wahlberechtigte
bayern_2020_kommunalwahlen_data_sub$Wähler <- bayern_2020_kommunalwahlen_data_sub$Waehler
bayern_2020_kommunalwahlen_data_sub$GültigeStimmen <- bayern_2020_kommunalwahlen_data_sub$Insgesamt


bayern_2020_kommunalwahlen_data_sub$abs_CDU <- bayern_2020_kommunalwahlen_data_sub$CSU
bayern_2020_kommunalwahlen_data_sub$abs_SPD <- bayern_2020_kommunalwahlen_data_sub$SDP
bayern_2020_kommunalwahlen_data_sub$abs_DIELINKE <- bayern_2020_kommunalwahlen_data_sub$DIELINKE
bayern_2020_kommunalwahlen_data_sub$abs_GRÜNE <- bayern_2020_kommunalwahlen_data_sub$GRUENE
bayern_2020_kommunalwahlen_data_sub$abs_AfD <- bayern_2020_kommunalwahlen_data_sub$AfD
bayern_2020_kommunalwahlen_data_sub$abs_PIRATEN <- bayern_2020_kommunalwahlen_data_sub$PIRATEN
bayern_2020_kommunalwahlen_data_sub$abs_FDP <- bayern_2020_kommunalwahlen_data_sub$FDP
bayern_2020_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- bayern_2020_kommunalwahlen_data_sub$FW
bayern_2020_kommunalwahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- bayern_2020_kommunalwahlen_data_sub$`Gemeinsame Wahlvorschläge`
bayern_2020_kommunalwahlen_data_sub$abs_Wählergruppen <- bayern_2020_kommunalwahlen_data_sub$`Wählergruppen`

bayern_2020_kommunalwahlen_data_sub$gew_CDU <- NA
bayern_2020_kommunalwahlen_data_sub$gew_SPD <- NA
bayern_2020_kommunalwahlen_data_sub$gew_DIELINKE <- NA
bayern_2020_kommunalwahlen_data_sub$gew_GRÜNE <- NA
bayern_2020_kommunalwahlen_data_sub$gew_AfD <- NA
bayern_2020_kommunalwahlen_data_sub$gew_PIRATEN <- NA
bayern_2020_kommunalwahlen_data_sub$gew_FDP <- NA
bayern_2020_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
bayern_2020_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
bayern_2020_kommunalwahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
bayern_2020_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

bayern_2020_kommunalwahlen_data_sub$sitze_CDU <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_SPD <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_AfD <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_FDP <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
bayern_2020_kommunalwahlen_data_sub$sitze_Wählergruppen <- NA

# Creating new dataframe with selected vars ----
bayern_2020_kommunalwahlen_data_sub <- bayern_2020_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2020_kommunalwahlen_data_sub <-
  bayern_2020_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bayern_2020_kommunalwahlen_data_sub$Turnout <- as.numeric(
  bayern_2020_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(bayern_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


####### Merge files and save overall output for Bayern ----
# Merge
bayern_kommunalwahlen <- rbind(
  bayern_1990_kommunalwahlen_data_sub,
  bayern_1990_kreiswahlen_data_sub,
  bayern_1996_kommunalwahlen_data_sub,
  bayern_2002_kommunalwahlen_data_sub,
  bayern_2008_kommunalwahlen_data_sub,
  bayern_2014_kommunalwahlen_data_sub,
  bayern_2020_kommunalwahlen_data_sub
)

# Replace INF at Turnout
bayern_kommunalwahlen[bayern_kommunalwahlen == "-"] <- NA


# Fix AGS
bayern_kommunalwahlen <- bayern_kommunalwahlen %>%
  filter(nchar(AGS_8dig) > 3) %>%
  mutate(
    AGS_8dig = case_when(
      nchar(AGS_8dig) == 5 ~ paste0(AGS_8dig, "000"),
      TRUE ~ AGS_8dig
    )
  )


# Filter out Gemeindefreie Gebiete and Landkreise
bayern_kommunalwahlen <- bayern_kommunalwahlen %>%
  filter(!Gebietsname == "Gemeindefreie Gebiete", !grepl("(Lkr)", Gebietsname))

# ----
######### THUERINGEN ----
###### Thueringen 1994 Gemeinderatswahlen ----
#### Load election data ----
thueringen_1994_kommunalwahlen_data <- as.data.table(read_csv(
  "raw/thueringen/thueringen_1999.csv"
))

thueringen_1994_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/thueringen/thueringen_1994.xlsx",
  sheet = "thueringen_1994"
))
thueringen_1994_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/thueringen/thueringen_1994_sitze.xlsx",
  sheet = "thueringen_1994_sitze"
))
names(thueringen_1994_kommunalwahlen_data_sitze) <- str_c(
  names(thueringen_1994_kommunalwahlen_data_sitze),
  "_sitze",
  sep = ""
)


# Merge Stimmen and Sitze
thueringen_1994_kommunalwahlen_data <- merge(
  thueringen_1994_kommunalwahlen_data,
  thueringen_1994_kommunalwahlen_data_sitze,
  by.x = "Gemeinde",
  by.y = "Gemeinde_sitze"
)
thueringen_1994_kommunalwahlen_data[
  thueringen_1994_kommunalwahlen_data == "-"
] <- NA

#### Delete white space ----
names(thueringen_1994_kommunalwahlen_data) <- str_replace_all(
  names(thueringen_1994_kommunalwahlen_data),
  fixed(" "),
  ""
)


#### Recoding ----
# Create new dataframe ----
thueringen_1994_kommunalwahlen_data_sub <- thueringen_1994_kommunalwahlen_data

names(thueringen_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_1994_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
thueringen_1994_kommunalwahlen_data_sub[, Bundesland := "Thueringen"]
thueringen_1994_kommunalwahlen_data_sub[, Gebietsname := ""]
thueringen_1994_kommunalwahlen_data_sub[, election_year := "1994"]
thueringen_1994_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
thueringen_1994_kommunalwahlen_data_sub[, IDIRB := ""]
thueringen_1994_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
thueringen_1994_kommunalwahlen_data_sub$AGS_8dig <- thueringen_1994_kommunalwahlen_data_sub$Gemeinde
thueringen_1994_kommunalwahlen_data_sub$Gebietsname <- thueringen_1994_kommunalwahlen_data_sub$Gemeindename
thueringen_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$Wahlberechtigte
)
thueringen_1994_kommunalwahlen_data_sub$Wähler <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$Waehler
)
thueringen_1994_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$Gueltige_stimmen
)

thueringen_1994_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$CDU
)
thueringen_1994_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_CDU
)

thueringen_1994_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$SPD
)
thueringen_1994_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_SPD
)

thueringen_1994_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$PDS
)
thueringen_1994_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_DIELINKE
)

thueringen_1994_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$GRE
)
thueringen_1994_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_GRÜNE
)

thueringen_1994_kommunalwahlen_data_sub$abs_AfD <- NA
thueringen_1994_kommunalwahlen_data_sub$abs_AfD <- NA

thueringen_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
thueringen_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA

thueringen_1994_kommunalwahlen_data_sub$abs_DiePARTEI <- NA


thueringen_1994_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$FDP
)
thueringen_1994_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_FDP
)

thueringen_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$FREIE_WAEHLER
)
thueringen_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER
)

thueringen_1994_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$Waehlergruppen
)
thueringen_1994_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$abs_Wählergruppen
)


thueringen_1994_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_1994_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

thueringen_1994_kommunalwahlen_data_sub$sitze_CDU <- thueringen_1994_kommunalwahlen_data_sub$CDU_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_SPD <- thueringen_1994_kommunalwahlen_data_sub$SPD_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_DIELINKE <- thueringen_1994_kommunalwahlen_data_sub$PDS_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_GRÜNE <- thueringen_1994_kommunalwahlen_data_sub$GRE_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_AfD <- NA
thueringen_1994_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
thueringen_1994_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
thueringen_1994_kommunalwahlen_data_sub$sitze_FDP <- thueringen_1994_kommunalwahlen_data_sub$FDP_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_1994_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_1994_kommunalwahlen_data_sub$Waehlergruppen_sitze

# Creating new dataframe with selected vars ----
thueringen_1994_kommunalwahlen_data_sub <- thueringen_1994_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]

thueringen_1994_kommunalwahlen_data_sub[
  thueringen_1994_kommunalwahlen_data_sub == "-"
] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_1994_kommunalwahlen_data_sub <-
  thueringen_1994_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_1994_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_1994_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 1999 Gemeinderatswahlen ----
#### Load election data ----
thueringen_1999_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/thueringen/thueringen_1999.xlsx",
  sheet = "thueringen_1999"
))
thueringen_1999_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/thueringen/thueringen_1999_sitze.xlsx",
  sheet = "thueringen_1999_sitze"
))
names(thueringen_1999_kommunalwahlen_data_sitze) <- str_c(
  names(thueringen_1999_kommunalwahlen_data_sitze),
  "_sitze",
  sep = ""
)


# Merge Stimmen and Sitze
thueringen_1999_kommunalwahlen_data <- merge(
  thueringen_1999_kommunalwahlen_data,
  thueringen_1999_kommunalwahlen_data_sitze,
  by.x = "Gemeinde",
  by.y = "Gemeinde_sitze"
)
thueringen_1999_kommunalwahlen_data[
  thueringen_1999_kommunalwahlen_data == "-"
] <- NA

#### Delete white space ----
names(thueringen_1999_kommunalwahlen_data) <- str_replace_all(
  names(thueringen_1999_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
thueringen_1999_kommunalwahlen_data_sub <- thueringen_1999_kommunalwahlen_data

names(thueringen_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_1999_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
thueringen_1999_kommunalwahlen_data_sub[, Bundesland := "Thueringen"]
thueringen_1999_kommunalwahlen_data_sub[, Gebietsname := ""]
thueringen_1999_kommunalwahlen_data_sub[, election_year := "1999"]
thueringen_1999_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
thueringen_1999_kommunalwahlen_data_sub[, IDIRB := ""]
thueringen_1999_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
thueringen_1999_kommunalwahlen_data_sub$AGS_8dig <- thueringen_1999_kommunalwahlen_data_sub$Gemeinde
thueringen_1999_kommunalwahlen_data_sub$Gebietsname <- thueringen_1999_kommunalwahlen_data_sub$Gemeindename
thueringen_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$Wahlberechtigte
)
thueringen_1999_kommunalwahlen_data_sub$Wähler <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$Waehler
)
thueringen_1999_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$Gueltige_stimmen
)

thueringen_1999_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$CDU
)
thueringen_1999_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_CDU
)

thueringen_1999_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$SPD
)
thueringen_1999_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_SPD
)

thueringen_1999_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$PDS
)
thueringen_1999_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_DIELINKE
)

thueringen_1999_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$GRE
)
thueringen_1999_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_GRÜNE
)

thueringen_1999_kommunalwahlen_data_sub$abs_AfD <- as.numeric(0)
thueringen_1999_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_AfD
)

thueringen_1999_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_1999_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_PIRATEN
)

thueringen_1999_kommunalwahlen_data_sub$abs_DiePARTEI <- NA

thueringen_1999_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$FDP
)
thueringen_1999_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_FDP
)

thueringen_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$FREIE_WAEHLER
)
thueringen_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER
)

thueringen_1999_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$Waehlergruppen
)
thueringen_1999_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$abs_Wählergruppen
)


thueringen_1999_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_1999_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

thueringen_1999_kommunalwahlen_data_sub$sitze_CDU <- thueringen_1999_kommunalwahlen_data_sub$CDU_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_SPD <- thueringen_1999_kommunalwahlen_data_sub$SPD_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_DIELINKE <- thueringen_1999_kommunalwahlen_data_sub$PDS_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_GRÜNE <- thueringen_1999_kommunalwahlen_data_sub$GRE_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_AfD <- NA
thueringen_1999_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
thueringen_1999_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
thueringen_1999_kommunalwahlen_data_sub$sitze_FDP <- thueringen_1999_kommunalwahlen_data_sub$FDP_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_1999_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_1999_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_1999_kommunalwahlen_data_sub <- thueringen_1999_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]
thueringen_1999_kommunalwahlen_data_sub[
  thueringen_1999_kommunalwahlen_data_sub == "-"
] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_1999_kommunalwahlen_data_sub <-
  thueringen_1999_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_1999_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_1999_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2004 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2004_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2004.xlsx",
  sheet = "thueringen_2004"
))
thueringen_2004_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2004_sitze.xlsx",
  sheet = "thueringen_2004_sitze"
))
names(thueringen_2004_kommunalwahlen_data_sitze) <- str_c(
  names(thueringen_2004_kommunalwahlen_data_sitze),
  "_sitze",
  sep = ""
)


# Merge Stimmen and Sitze
thueringen_2004_kommunalwahlen_data <- merge(
  thueringen_2004_kommunalwahlen_data,
  thueringen_2004_kommunalwahlen_data_sitze,
  by.x = "Gemeinde",
  by.y = "Gemeinde_sitze"
)
thueringen_2004_kommunalwahlen_data[
  thueringen_2004_kommunalwahlen_data == "-"
] <- NA

#### Delete white space ----
names(thueringen_2004_kommunalwahlen_data) <- str_replace_all(
  names(thueringen_2004_kommunalwahlen_data),
  fixed(" "),
  ""
)


#### Recoding ----
# Create new dataframe ----
thueringen_2004_kommunalwahlen_data_sub <- thueringen_2004_kommunalwahlen_data

names(thueringen_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2004_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
thueringen_2004_kommunalwahlen_data_sub[, Bundesland := "Thueringen"]
thueringen_2004_kommunalwahlen_data_sub[, Gebietsname := ""]
thueringen_2004_kommunalwahlen_data_sub[, election_year := "2004"]
thueringen_2004_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
thueringen_2004_kommunalwahlen_data_sub[, IDIRB := ""]
thueringen_2004_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
thueringen_2004_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2004_kommunalwahlen_data_sub$Gemeinde
thueringen_2004_kommunalwahlen_data_sub$Gebietsname <- thueringen_2004_kommunalwahlen_data_sub$Gemeindename
thueringen_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$Wahlberechtigte
)
thueringen_2004_kommunalwahlen_data_sub$Wähler <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$Waehler
)
thueringen_2004_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$Gueltige_stimmen
)

thueringen_2004_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$CDU
)
thueringen_2004_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_CDU
)

thueringen_2004_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$SPD
)
thueringen_2004_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_SPD
)

thueringen_2004_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$PDS
)
thueringen_2004_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_DIELINKE
)

thueringen_2004_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$GRE
)
thueringen_2004_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_GRÜNE
)

thueringen_2004_kommunalwahlen_data_sub$abs_AfD <- as.numeric(0)
thueringen_2004_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_AfD
)

thueringen_2004_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2004_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_PIRATEN
)

thueringen_2004_kommunalwahlen_data_sub$abs_DiePARTEI <- NA

thueringen_2004_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$FDP
)
thueringen_2004_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_FDP
)

thueringen_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$FREIE_WAEHLER
)
thueringen_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER
)

thueringen_2004_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$Waehlergruppen
)
thueringen_2004_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$abs_Wählergruppen
)


thueringen_2004_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_2004_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

thueringen_2004_kommunalwahlen_data_sub$sitze_CDU <- thueringen_2004_kommunalwahlen_data_sub$CDU_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_SPD <- thueringen_2004_kommunalwahlen_data_sub$SPD_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_DIELINKE <- thueringen_2004_kommunalwahlen_data_sub$PDS_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_GRÜNE <- thueringen_2004_kommunalwahlen_data_sub$GRE_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_AfD <- NA
thueringen_2004_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
thueringen_2004_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
thueringen_2004_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2004_kommunalwahlen_data_sub$FDP_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2004_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2004_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2004_kommunalwahlen_data_sub <- thueringen_2004_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]
thueringen_2004_kommunalwahlen_data_sub[
  thueringen_2004_kommunalwahlen_data_sub == "-"
] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2004_kommunalwahlen_data_sub <-
  thueringen_2004_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_2004_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_2004_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2009 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2009_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2009.xlsx",
  sheet = "thueringen_2009"
))
thueringen_2009_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2009_sitze.xlsx",
  sheet = "thueringen_2009_sitze"
))
names(thueringen_2009_kommunalwahlen_data_sitze) <- str_c(
  names(thueringen_2009_kommunalwahlen_data_sitze),
  "_sitze",
  sep = ""
)


# Merge Stimmen and Sitze
thueringen_2009_kommunalwahlen_data <- merge(
  thueringen_2009_kommunalwahlen_data,
  thueringen_2009_kommunalwahlen_data_sitze,
  by.x = "Gemeinde",
  by.y = "Gemeinde_sitze"
)
thueringen_2009_kommunalwahlen_data[
  thueringen_2009_kommunalwahlen_data == "-"
] <- NA

#### Delete white space ----
names(thueringen_2009_kommunalwahlen_data) <- str_replace_all(
  names(thueringen_2009_kommunalwahlen_data),
  fixed(" "),
  ""
)


#### Recoding ----
# Create new dataframe ----
thueringen_2009_kommunalwahlen_data_sub <- thueringen_2009_kommunalwahlen_data

names(thueringen_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2009_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
thueringen_2009_kommunalwahlen_data_sub[, Bundesland := "Thueringen"]
thueringen_2009_kommunalwahlen_data_sub[, Gebietsname := ""]
thueringen_2009_kommunalwahlen_data_sub[, election_year := "2009"]
thueringen_2009_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
thueringen_2009_kommunalwahlen_data_sub[, IDIRB := ""]
thueringen_2009_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
thueringen_2009_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2009_kommunalwahlen_data_sub$Gemeinde
thueringen_2009_kommunalwahlen_data_sub$Gebietsname <- thueringen_2009_kommunalwahlen_data_sub$Gemeindename
thueringen_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$Wahlberechtigte
)
thueringen_2009_kommunalwahlen_data_sub$Wähler <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$Waehler
)
thueringen_2009_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$Gueltige_stimmen
)

thueringen_2009_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$CDU
)
thueringen_2009_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_CDU
)

thueringen_2009_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$SPD
)
thueringen_2009_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_SPD
)

thueringen_2009_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$DIE_LINKE
)
thueringen_2009_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_DIELINKE
)

thueringen_2009_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$GRE
)
thueringen_2009_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_GRÜNE
)

thueringen_2009_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$AfD
)
thueringen_2009_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_AfD
)

thueringen_2009_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2009_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_PIRATEN
)

thueringen_2009_kommunalwahlen_data_sub$abs_DiePARTEI <- NA

thueringen_2009_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$FDP
)
thueringen_2009_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_FDP
)

thueringen_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$FREIE_WAEHLER
)
thueringen_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER
)

thueringen_2009_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$Waehlergruppen
)
thueringen_2009_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$abs_Wählergruppen
)


thueringen_2009_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_2009_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

thueringen_2009_kommunalwahlen_data_sub$sitze_CDU <- thueringen_2009_kommunalwahlen_data_sub$CDU_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_SPD <- thueringen_2009_kommunalwahlen_data_sub$SPD_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_DIELINKE <- thueringen_2009_kommunalwahlen_data_sub$DIE_LINKE_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_GRÜNE <- thueringen_2009_kommunalwahlen_data_sub$GRE_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_AfD <- NA
thueringen_2009_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
thueringen_2009_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
thueringen_2009_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2009_kommunalwahlen_data_sub$FDP_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2009_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2009_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2009_kommunalwahlen_data_sub <- thueringen_2009_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]

thueringen_2009_kommunalwahlen_data_sub[
  thueringen_2009_kommunalwahlen_data_sub == "-"
] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2009_kommunalwahlen_data_sub <-
  thueringen_2009_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_2009_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_2009_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2014 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2014_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2014.xlsx",
  sheet = "thueringen_2014"
))
thueringen_2014_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2014_sitze.xlsx",
  sheet = "thueringen_2014_sitze"
))
names(thueringen_2014_kommunalwahlen_data_sitze) <- str_c(
  names(thueringen_2014_kommunalwahlen_data_sitze),
  "_sitze",
  sep = ""
)


# Merge Stimmen and Sitze
thueringen_2014_kommunalwahlen_data <- merge(
  thueringen_2014_kommunalwahlen_data,
  thueringen_2014_kommunalwahlen_data_sitze,
  by.x = "Gemeinde",
  by.y = "Gemeinde_sitze"
)
thueringen_2014_kommunalwahlen_data[
  thueringen_2014_kommunalwahlen_data == "-"
] <- NA

#### Delete white space ----
names(thueringen_2014_kommunalwahlen_data) <- str_replace_all(
  names(thueringen_2014_kommunalwahlen_data),
  fixed(" "),
  ""
)


#### Recoding ----
# Create new dataframe ----
thueringen_2014_kommunalwahlen_data_sub <- thueringen_2014_kommunalwahlen_data

names(thueringen_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2014_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
thueringen_2014_kommunalwahlen_data_sub[, Bundesland := "Thueringen"]
thueringen_2014_kommunalwahlen_data_sub[, Gebietsname := ""]
thueringen_2014_kommunalwahlen_data_sub[, election_year := "2014"]
thueringen_2014_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
thueringen_2014_kommunalwahlen_data_sub[, IDIRB := ""]
thueringen_2014_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
thueringen_2014_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2014_kommunalwahlen_data_sub$Gemeinde
thueringen_2014_kommunalwahlen_data_sub$Gebietsname <- thueringen_2014_kommunalwahlen_data_sub$Gemeindename
thueringen_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$Wahlberechtigte
)
thueringen_2014_kommunalwahlen_data_sub$Wähler <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$Waehler
)
thueringen_2014_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$Gueltige_stimmen
)

thueringen_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$CDU
)
thueringen_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_CDU
)

thueringen_2014_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$SPD
)
thueringen_2014_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_SPD
)

thueringen_2014_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$DIE_LINKE
)
thueringen_2014_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_DIELINKE
)

thueringen_2014_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$GRE
)
thueringen_2014_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_GRÜNE
)

thueringen_2014_kommunalwahlen_data_sub$abs_AfD <- thueringen_2014_kommunalwahlen_data_sub$AfD
thueringen_2014_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_AfD
)

thueringen_2014_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2014_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_PIRATEN
)

thueringen_2014_kommunalwahlen_data_sub$abs_DiePARTEI <- NA

thueringen_2014_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$FDP
)
thueringen_2014_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_FDP
)

thueringen_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$FREIE_WAEHLER
)
thueringen_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER
)

thueringen_2014_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$Waehlergruppen
)
thueringen_2014_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$abs_Wählergruppen
)

thueringen_2014_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_2014_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

thueringen_2014_kommunalwahlen_data_sub$sitze_CDU <- thueringen_2014_kommunalwahlen_data_sub$CDU_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_SPD <- thueringen_2014_kommunalwahlen_data_sub$SPD_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_DIELINKE <- thueringen_2014_kommunalwahlen_data_sub$DIE_LINKE_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_GRÜNE <- thueringen_2014_kommunalwahlen_data_sub$GRE_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_AfD <- thueringen_2014_kommunalwahlen_data_sub$AfD_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
thueringen_2014_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
thueringen_2014_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2014_kommunalwahlen_data_sub$FDP_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2014_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2014_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2014_kommunalwahlen_data_sub <- thueringen_2014_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]

thueringen_2014_kommunalwahlen_data_sub[
  thueringen_2014_kommunalwahlen_data_sub == "-"
] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2014_kommunalwahlen_data_sub <-
  thueringen_2014_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_2014_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_2014_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2019 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2019_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2019.xlsx",
  sheet = "thueringen_2019"
))
thueringen_2019_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/thueringen/thueringen_2019_sitze.xlsx",
  sheet = "thueringen_2019_sitze"
))
names(thueringen_2019_kommunalwahlen_data_sitze) <- str_c(
  names(thueringen_2019_kommunalwahlen_data_sitze),
  "_sitze",
  sep = ""
)


# Merge Stimmen and Sitze
thueringen_2019_kommunalwahlen_data <- merge(
  thueringen_2019_kommunalwahlen_data,
  thueringen_2019_kommunalwahlen_data_sitze,
  by.x = "Gemeinde",
  by.y = "Gemeinde_sitze"
)
thueringen_2019_kommunalwahlen_data[
  thueringen_2019_kommunalwahlen_data == "-"
] <- NA

#### Delete white space ----
names(thueringen_2019_kommunalwahlen_data) <- str_replace_all(
  names(thueringen_2019_kommunalwahlen_data),
  fixed(" "),
  ""
)


#### Recoding ----
# Create new dataframe ----
thueringen_2019_kommunalwahlen_data_sub <- thueringen_2019_kommunalwahlen_data

names(thueringen_2019_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2019_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
thueringen_2019_kommunalwahlen_data_sub[, Bundesland := "Thueringen"]
thueringen_2019_kommunalwahlen_data_sub[, Gebietsname := ""]
thueringen_2019_kommunalwahlen_data_sub[, election_year := "2019"]
thueringen_2019_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
thueringen_2019_kommunalwahlen_data_sub[, IDIRB := ""]
thueringen_2019_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
thueringen_2019_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2019_kommunalwahlen_data_sub$Gemeinde
thueringen_2019_kommunalwahlen_data_sub$Gebietsname <- thueringen_2019_kommunalwahlen_data_sub$Gemeindename
thueringen_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$Wahlberechtigte
)
thueringen_2019_kommunalwahlen_data_sub$Wähler <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$Waehler
)
thueringen_2019_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$Gueltige_stimmen
)

thueringen_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$CDU
)
thueringen_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_CDU
)

thueringen_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$SPD
)
thueringen_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_SPD
)

thueringen_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$PDS
)
thueringen_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_DIELINKE
)

thueringen_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$GRE
)
thueringen_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_GRÜNE
)

thueringen_2019_kommunalwahlen_data_sub$abs_AfD <- thueringen_2019_kommunalwahlen_data_sub$AfD
thueringen_2019_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_AfD
)

thueringen_2019_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2019_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_PIRATEN
)

thueringen_2019_kommunalwahlen_data_sub$abs_DiePARTEI <- NA

thueringen_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$FDP
)
thueringen_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_FDP
)

thueringen_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$FREIE_WAEHLER
)
thueringen_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER
)

thueringen_2019_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$Waehlergruppen
)
thueringen_2019_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$abs_Wählergruppen
)

thueringen_2019_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_2019_kommunalwahlen_data_sub$gew_Wählergruppen <- NA

thueringen_2019_kommunalwahlen_data_sub$sitze_CDU <- thueringen_2019_kommunalwahlen_data_sub$CDU_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_SPD <- thueringen_2019_kommunalwahlen_data_sub$SPD_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_DIELINKE <- thueringen_2019_kommunalwahlen_data_sub$DIE_LINKE_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_GRÜNE <- thueringen_2019_kommunalwahlen_data_sub$GRE_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_AfD <- thueringen_2019_kommunalwahlen_data_sub$AfD_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
thueringen_2019_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
thueringen_2019_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2019_kommunalwahlen_data_sub$FDP_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2019_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2019_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2019_kommunalwahlen_data_sub <- thueringen_2019_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]

thueringen_2019_kommunalwahlen_data_sub[
  thueringen_2019_kommunalwahlen_data_sub == "-"
] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2019_kommunalwahlen_data_sub <-
  thueringen_2019_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_2019_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_2019_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### Thueringen 2024 Gemeinderatswahlen ----
#### Load election data ----
thueringen_2024_kommunalwahlen_data <- lapply(
  excel_sheets("raw/thueringen/thueringen_2024.xlsx"),
  function(S) {
    as.data.table(read_excel(
      "raw/thueringen/thueringen_2024.xlsx",
      sheet = S,
      skip = 4
    )) |>
      mutate(across(everything(), ~ as.character(.x)))
  }
) |>
  bind_rows()

#### fix names ----
names(thueringen_2024_kommunalwahlen_data) <- str_replace(
  names(thueringen_2024_kommunalwahlen_data),
  "-",
  "_nr"
)


#### Recoding ----
# Create new dataframe ----
thueringen_2024_kommunalwahlen_data_sub <- thueringen_2024_kommunalwahlen_data |>

  # pivot to WIDE data
  fill(starts_with(c("Gemeinde", "Kreis"))) |>
  filter(Gemeinde_nr != "nr.") |>
  filter(!is.na(Wähler)) |>

  # only party lists // other
  mutate(
    AGS_8dig = paste0(Kreis_nr, Gemeinde_nr), # first three digits "160" are added later => adapted to pre-existing framework

    label = case_when(
      Wähler %in%
        c(
          "Stimmverhalten",
          "Wahlberechtigte",
          "Wähler/Wahlbeteiligung",
          "Ungültige Stimmzettel",
          "Zu vergebende Sitze",
          "Gültige Stimmen",
          "davon entfielen auf",

          "CDU",
          "SPD",
          "DIE LINKE",
          "GRÜNE",
          "AfD",
          "PIRATEN",
          "FDP",
          "Die PARTEI",
          "Freie Wähler"
        ) ~
        Wähler,

      T ~ "other"
    )
  )

# pivot both votes and seats to wide format
thueringen_2024_kommunalwahlen_data_sub <- thueringen_2024_kommunalwahlen_data_sub |>

  group_by(AGS_8dig, Gemeinde, label) |>
  summarise(
    Anzahl = sum(as.numeric(str_replace(Anzahl, "-", "0")), na.rm = T)
  ) |>

  ungroup() |>

  pivot_wider(
    id_cols = c("AGS_8dig", "Gemeinde"),
    names_from = "label",
    values_from = "Anzahl",
    values_fill = NA
  ) |>

  left_join(
    thueringen_2024_kommunalwahlen_data_sub |>

      filter(!is.na(Sitze) & Sitze != "Mehrheitswahl") |>

      group_by(AGS_8dig, Gemeinde, label) |>
      summarise(
        Sitze = sum(as.numeric(str_replace(Sitze, "-", "0")), na.rm = T)
      ) |>

      ungroup() |>

      pivot_wider(
        id_cols = c("AGS_8dig", "Gemeinde"),
        names_from = "label",
        values_from = "Sitze",
        values_fill = NA,
        names_prefix = "sitze_"
      ),

    by = c("AGS_8dig", "Gemeinde")
  )


# create non-existing and rename existing variables
thueringen_2024_kommunalwahlen_data_sub <- thueringen_2024_kommunalwahlen_data_sub |>

  mutate(
    Bundesland = "Thueringen",
    election_year = "2024",
    election_type = "Kommunalwahlen",
    IDIRB = "",
    IDBA = ""
  ) |>

  rename(
    Gebietsname = Gemeinde,
    Wahlberechtigteinsgesamt = Wahlberechtigte,
    Wähler = `Wähler/Wahlbeteiligung`,
    GültigeStimmen = `Gültige Stimmen`,
    abs_CDU = CDU,
    abs_SPD = SPD,
    abs_DIELINKE = `DIE LINKE`,
    abs_GRÜNE = GRÜNE,
    abs_AfD = AfD,
    abs_PIRATEN = PIRATEN,
    abs_FDP = FDP,
    abs_DiePARTEI = `Die PARTEI`,
    abs_FREIEWÄHLER = `Freie Wähler`,
    abs_Wählergruppen = other,

    # seats
    sitze_CDU = sitze_CDU,
    sitze_SPD = sitze_SPD,
    sitze_DIELINKE = `sitze_DIE LINKE`,
    sitze_GRÜNE = sitze_GRÜNE,
    sitze_AfD = sitze_AfD,
    sitze_PIRATEN = sitze_PIRATEN,
    sitze_FDP = sitze_FDP,
    sitze_DiePARTEI = `sitze_Die PARTEI`,
    sitze_FREIEWÄHLER = `sitze_Freie Wähler`,
    sitze_Wählergruppen = sitze_other
  ) |>

  as.data.table()

thueringen_2024_kommunalwahlen_data_sub$gew_CDU <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_SPD <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_DIELINKE <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_GRÜNE <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_AfD <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_PIRATEN <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_FDP <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA
thueringen_2024_kommunalwahlen_data_sub$gew_Wählergruppen <- NA


names(thueringen_2024_kommunalwahlen_data_sub)

# select vars ----
thueringen_2024_kommunalwahlen_data_sub <- thueringen_2024_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_DiePARTEI,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_DiePARTEI,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_DiePARTEI,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2024_kommunalwahlen_data_sub <-
  thueringen_2024_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains("abs")),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
thueringen_2024_kommunalwahlen_data_sub$Turnout <- as.numeric(
  thueringen_2024_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(thueringen_2024_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


####### Merge files and save overall output for Thueringen ----
# Merge
thueringen_kommunalwahlen <- rbind(
  thueringen_1994_kommunalwahlen_data_sub,
  thueringen_1999_kommunalwahlen_data_sub,
  thueringen_2004_kommunalwahlen_data_sub,
  thueringen_2009_kommunalwahlen_data_sub,
  thueringen_2014_kommunalwahlen_data_sub,
  thueringen_2019_kommunalwahlen_data_sub,
  thueringen_2024_kommunalwahlen_data_sub
)

thueringen_kommunalwahlen <-
  thueringen_kommunalwahlen %>%
  mutate(
    abs_Gemeinsame_Wahlvorschläge = NA,
    gew_Gemeinsame_Wahlvorschläge = NA,
    prop_Gemeinsame_Wahlvorschläge = NA,
    sitze_Gemeinsame_Wahlvorschläge = NA
  )

# Replace - with NA
thueringen_kommunalwahlen[thueringen_kommunalwahlen == "-"] <- NA

# Fix AGS
thueringen_kommunalwahlen$AGS_8dig <- paste(
  "160",
  thueringen_kommunalwahlen$AGS_8dig,
  sep = ""
)

# Save
#write_csv(thueringen_kommunalwahlen, here::here("output/thueringen_kommunalwahlen.csv")

# ----
# ----
######### HAMBURG ----
###### Hamburg 1991-1997 Buergerschaftswahl ----
#### Load election data ----
hamburg_1991_1997_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_1991_1997.xlsx",
  sheet = "results"
))

#### Delete white space ----
names(hamburg_1991_1997_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_1991_1997_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_1991_1997_buergerschaftswahl_data_sub <- hamburg_1991_1997_buergerschaftswahl_data

# Creating non-existing variables ----
hamburg_1991_1997_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_1991_1997_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_1991_1997_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_1991_1997_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_1991_1997_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_1991_1997_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_1991_1997_buergerschaftswahl_data_sub$election_year <- hamburg_1991_1997_buergerschaftswahl_data_sub$Year
hamburg_1991_1997_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_1991_1997_buergerschaftswahl_data_sub$Wahlberechtigte
hamburg_1991_1997_buergerschaftswahl_data_sub$Wähler <- hamburg_1991_1997_buergerschaftswahl_data_sub$Waehler
hamburg_1991_1997_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_1991_1997_buergerschaftswahl_data_sub$gueltigeStimmen

hamburg_1991_1997_buergerschaftswahl_data_sub$abs_CDU <- hamburg_1991_1997_buergerschaftswahl_data_sub$CDU
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_SPD <- hamburg_1991_1997_buergerschaftswahl_data_sub$SPD
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_DIELINKE <- hamburg_1991_1997_buergerschaftswahl_data_sub$LINKE
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_1991_1997_buergerschaftswahl_data_sub$Gruene
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_AfD <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_PIRATEN <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_FDP <- hamburg_1991_1997_buergerschaftswahl_data_sub$FDP
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_DiePARTEI <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- NA

hamburg_1991_1997_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_1991_1997_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_1991_1997_buergerschaftswahl_data_sub <- hamburg_1991_1997_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_1991_1997_buergerschaftswahl_data_sub <-
  hamburg_1991_1997_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_1991_1997_buergerschaftswahl_data_sub$Turnout <- hamburg_1991_1997_buergerschaftswahl_data_sub$Wähler /
  hamburg_1991_1997_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt


###### Hamburg 2001 Buergerschaftswahl ----
#### Load election data ----
hamburg_2001_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_2001.xls",
  sheet = "summary"
))

#### Delete white space ----
names(hamburg_2001_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_2001_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_2001_buergerschaftswahl_data_sub <- hamburg_2001_buergerschaftswahl_data[
  hamburg_2001_buergerschaftswahl_data$stadtteil_name == "Hamburg"
]

names(hamburg_2001_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2001_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2001_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_2001_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_2001_buergerschaftswahl_data_sub[, election_year := "2001"]
hamburg_2001_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_2001_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_2001_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_2001_buergerschaftswahl_data_sub$Gebietsname <- hamburg_2001_buergerschaftswahl_data_sub$stadtteil_name
hamburg_2001_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_2001_buergerschaftswahl_data_sub$wahlberecht_insges
hamburg_2001_buergerschaftswahl_data_sub$Wähler <- hamburg_2001_buergerschaftswahl_data_sub$waehler_insges
hamburg_2001_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_2001_buergerschaftswahl_data_sub$gueltige_stimmen

hamburg_2001_buergerschaftswahl_data_sub$abs_CDU <- hamburg_2001_buergerschaftswahl_data_sub$CDU
hamburg_2001_buergerschaftswahl_data_sub$abs_SPD <- hamburg_2001_buergerschaftswahl_data_sub$SPD
hamburg_2001_buergerschaftswahl_data_sub$abs_DIELINKE <- hamburg_2001_buergerschaftswahl_data_sub$PDSHamburg
hamburg_2001_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_2001_buergerschaftswahl_data_sub$'GRÜNE/GAL'
hamburg_2001_buergerschaftswahl_data_sub$abs_AfD <- NA
hamburg_2001_buergerschaftswahl_data_sub$abs_PIRATEN <- NA
hamburg_2001_buergerschaftswahl_data_sub$abs_FDP <- hamburg_2001_buergerschaftswahl_data_sub$F_D_P_
hamburg_2001_buergerschaftswahl_data_sub$abs_DiePARTEI <- NA
hamburg_2001_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- NA

hamburg_2001_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2001_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_2001_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_2001_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_2001_buergerschaftswahl_data_sub <- hamburg_2001_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2001_buergerschaftswahl_data_sub <-
  hamburg_2001_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2001_buergerschaftswahl_data_sub$Turnout <- hamburg_2001_buergerschaftswahl_data_sub$Wähler /
  hamburg_2001_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt


###### Hamburg 2004 Buergerschaftswahl ----
#### Load election data ----

hamburg_2004_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_2004.xls",
  sheet = "summary"
))

#### Delete white space ----
names(hamburg_2004_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_2004_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_2004_buergerschaftswahl_data_sub <- hamburg_2004_buergerschaftswahl_data[
  hamburg_2004_buergerschaftswahl_data$"...1" == "Hamburg"
]

names(hamburg_2004_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2004_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2004_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_2004_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_2004_buergerschaftswahl_data_sub[, election_year := "2004"]
hamburg_2004_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_2004_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_2004_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_2004_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_2004_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt
hamburg_2004_buergerschaftswahl_data_sub$Wähler <- hamburg_2004_buergerschaftswahl_data_sub$Wählerinsgesamt
hamburg_2004_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_2004_buergerschaftswahl_data_sub$GültigeStimmen

hamburg_2004_buergerschaftswahl_data_sub$abs_CDU <- hamburg_2004_buergerschaftswahl_data_sub$CDU
hamburg_2004_buergerschaftswahl_data_sub$abs_SPD <- hamburg_2004_buergerschaftswahl_data_sub$SPD
hamburg_2004_buergerschaftswahl_data_sub$abs_DIELINKE <- NA
hamburg_2004_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_2004_buergerschaftswahl_data_sub$'GRÜNE/GAL'
hamburg_2004_buergerschaftswahl_data_sub$abs_AfD <- NA
hamburg_2004_buergerschaftswahl_data_sub$abs_PIRATEN <- NA
hamburg_2004_buergerschaftswahl_data_sub$abs_FDP <- hamburg_2004_buergerschaftswahl_data_sub$FDP
hamburg_2004_buergerschaftswahl_data_sub$abs_DiePARTEI <- NA
hamburg_2004_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- NA

hamburg_2004_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2004_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_2004_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_2004_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_2004_buergerschaftswahl_data_sub <- hamburg_2004_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2004_buergerschaftswahl_data_sub <-
  hamburg_2004_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2004_buergerschaftswahl_data_sub$Turnout <- hamburg_2004_buergerschaftswahl_data_sub$Wähler /
  hamburg_2004_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2008 Buergerschaftswahl ----
#### Load election data ----

hamburg_2008_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_2008.xls",
  sheet = "summary"
))

#### Delete white space ----
names(hamburg_2008_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_2008_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_2008_buergerschaftswahl_data_sub <- hamburg_2008_buergerschaftswahl_data[
  hamburg_2008_buergerschaftswahl_data$Stadtteil == "Hamburg"
]

names(hamburg_2008_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2008_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2008_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_2008_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_2008_buergerschaftswahl_data_sub[, election_year := "2008"]
hamburg_2008_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_2008_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_2008_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_2008_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_2008_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt
hamburg_2008_buergerschaftswahl_data_sub$Wähler <- hamburg_2008_buergerschaftswahl_data_sub$Wählerinsgesamt
hamburg_2008_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_2008_buergerschaftswahl_data_sub$gültigeStimmen

hamburg_2008_buergerschaftswahl_data_sub$abs_CDU <- hamburg_2008_buergerschaftswahl_data_sub$CDU
hamburg_2008_buergerschaftswahl_data_sub$abs_SPD <- hamburg_2008_buergerschaftswahl_data_sub$SPD
hamburg_2008_buergerschaftswahl_data_sub$abs_DIELINKE <- hamburg_2008_buergerschaftswahl_data_sub$DieLinke
hamburg_2008_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_2008_buergerschaftswahl_data_sub$'GRÜNE/GAL'
hamburg_2008_buergerschaftswahl_data_sub$abs_AfD <- NA
hamburg_2008_buergerschaftswahl_data_sub$abs_PIRATEN <- hamburg_2008_buergerschaftswahl_data_sub$Piraten
hamburg_2008_buergerschaftswahl_data_sub$abs_FDP <- hamburg_2008_buergerschaftswahl_data_sub$FDP
hamburg_2008_buergerschaftswahl_data_sub$abs_DiePARTEI <- hamburg_2008_buergerschaftswahl_data_sub$DiePartei
hamburg_2008_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- NA

hamburg_2008_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2008_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_2008_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_2008_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_2008_buergerschaftswahl_data_sub <- hamburg_2008_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2008_buergerschaftswahl_data_sub <-
  hamburg_2008_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2008_buergerschaftswahl_data_sub$Turnout <- hamburg_2008_buergerschaftswahl_data_sub$Wähler /
  hamburg_2008_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2011 Buergerschaftswahl ----
#### Load election data ----

hamburg_2011_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_2011.xls",
  sheet = "summary"
))

#### Delete white space ----
names(hamburg_2011_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_2011_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_2011_buergerschaftswahl_data_sub <- hamburg_2011_buergerschaftswahl_data[
  hamburg_2011_buergerschaftswahl_data$Stadtteil == "Hamburg"
]

names(hamburg_2011_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2011_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2011_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_2011_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_2011_buergerschaftswahl_data_sub[, election_year := "2011"]
hamburg_2011_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_2011_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_2011_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_2011_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_2011_buergerschaftswahl_data_sub$Wahlberechtigte
hamburg_2011_buergerschaftswahl_data_sub$Wähler <- hamburg_2011_buergerschaftswahl_data_sub$Wählerinsgesamt
hamburg_2011_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_2011_buergerschaftswahl_data_sub$GültigeStimmen

hamburg_2011_buergerschaftswahl_data_sub$abs_CDU <- hamburg_2011_buergerschaftswahl_data_sub$"CDU-Gesamtstimmen"
hamburg_2011_buergerschaftswahl_data_sub$abs_SPD <- hamburg_2011_buergerschaftswahl_data_sub$"SPD-Gesamtstimmen"
hamburg_2011_buergerschaftswahl_data_sub$abs_DIELINKE <- hamburg_2011_buergerschaftswahl_data_sub$"DIELINKE-Gesamtstimmen"
hamburg_2011_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_2011_buergerschaftswahl_data_sub$"GRÜNE/GAL-Gesamtstimmen"
hamburg_2011_buergerschaftswahl_data_sub$abs_AfD <- NA
hamburg_2011_buergerschaftswahl_data_sub$abs_PIRATEN <- hamburg_2011_buergerschaftswahl_data_sub$`PIRATEN-Gesamtstimmen`
hamburg_2011_buergerschaftswahl_data_sub$abs_FDP <- hamburg_2011_buergerschaftswahl_data_sub$`FDP-Gesamtstimmen`
hamburg_2011_buergerschaftswahl_data_sub$abs_DiePARTEI <- hamburg_2011_buergerschaftswahl_data_sub$`DiePARTEI-Gesamtstimmen`
hamburg_2011_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- hamburg_2011_buergerschaftswahl_data_sub$"FREIEWÄHLER-Gesamtstimmen"

hamburg_2011_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2011_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_2011_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_2011_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_2011_buergerschaftswahl_data_sub <- hamburg_2011_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2011_buergerschaftswahl_data_sub <-
  hamburg_2011_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2011_buergerschaftswahl_data_sub$Turnout <- hamburg_2011_buergerschaftswahl_data_sub$Wähler /
  hamburg_2011_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2015 Buergerschaftswahl ----
#### Load election data ----

hamburg_2015_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_2015.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(hamburg_2015_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_2015_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_2015_buergerschaftswahl_data_sub <- hamburg_2015_buergerschaftswahl_data[
  hamburg_2015_buergerschaftswahl_data$Wahlkreis == "Hamburg"
]

names(hamburg_2015_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2015_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2015_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_2015_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_2015_buergerschaftswahl_data_sub[, election_year := "2015"]
hamburg_2015_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_2015_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_2015_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_2015_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_2015_buergerschaftswahl_data_sub$Wahlberechtigte
hamburg_2015_buergerschaftswahl_data_sub$Wähler <- hamburg_2015_buergerschaftswahl_data_sub$Wählerinsgesamt
hamburg_2015_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_2015_buergerschaftswahl_data_sub$GültigeStimmen

hamburg_2015_buergerschaftswahl_data_sub$abs_CDU <- hamburg_2015_buergerschaftswahl_data_sub$"CDU-Gesamtstimmen"
hamburg_2015_buergerschaftswahl_data_sub$abs_SPD <- hamburg_2015_buergerschaftswahl_data_sub$"SPD-Gesamtstimmen"
hamburg_2015_buergerschaftswahl_data_sub$abs_DIELINKE <- hamburg_2015_buergerschaftswahl_data_sub$"DIELINKE-Gesamtstimmen"
hamburg_2015_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_2015_buergerschaftswahl_data_sub$"GRÜNE-Gesamtstimmen"
hamburg_2015_buergerschaftswahl_data_sub$abs_AfD <- hamburg_2015_buergerschaftswahl_data_sub$"AfD-Gesamtstimmen"
hamburg_2015_buergerschaftswahl_data_sub$abs_PIRATEN <- hamburg_2015_buergerschaftswahl_data_sub$`PIRATEN-Gesamtstimmen`
hamburg_2015_buergerschaftswahl_data_sub$abs_FDP <- hamburg_2015_buergerschaftswahl_data_sub$`FDP-Gesamtstimmen`
hamburg_2015_buergerschaftswahl_data_sub$abs_DiePARTEI <- hamburg_2015_buergerschaftswahl_data_sub$`DiePARTEI-Gesamtstimmen`
hamburg_2015_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- NA

hamburg_2015_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2015_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_2015_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_2015_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_2015_buergerschaftswahl_data_sub <- hamburg_2015_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2015_buergerschaftswahl_data_sub <-
  hamburg_2015_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2015_buergerschaftswahl_data_sub$Turnout <- hamburg_2015_buergerschaftswahl_data_sub$Wähler /
  hamburg_2015_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2020 Buergerschaftswahl ----
#### Load election data ----

hamburg_2020_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/hamburg/hamburg_2020.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(hamburg_2020_buergerschaftswahl_data) <- str_replace_all(
  names(hamburg_2020_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hamburg_2020_buergerschaftswahl_data_sub <- hamburg_2020_buergerschaftswahl_data[
  hamburg_2020_buergerschaftswahl_data$Bezirk == "Hamburg"
]

names(hamburg_2020_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2020_buergerschaftswahl_data_sub[, AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2020_buergerschaftswahl_data_sub[, Bundesland := "Hamburg"]
hamburg_2020_buergerschaftswahl_data_sub[, Gebietsname := "Hamburg"]
hamburg_2020_buergerschaftswahl_data_sub[, election_year := "2020"]
hamburg_2020_buergerschaftswahl_data_sub[,
  election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"
]
hamburg_2020_buergerschaftswahl_data_sub[, IDIRB := ""]
hamburg_2020_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hamburg_2020_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- hamburg_2020_buergerschaftswahl_data_sub$Wahlberechtigte
hamburg_2020_buergerschaftswahl_data_sub$Wähler <- hamburg_2020_buergerschaftswahl_data_sub$Wählende
hamburg_2020_buergerschaftswahl_data_sub$GültigeStimmen <- hamburg_2020_buergerschaftswahl_data_sub$`GültigeStimmen(Gesamt)`

hamburg_2020_buergerschaftswahl_data_sub$abs_CDU <- hamburg_2020_buergerschaftswahl_data_sub$"CDU-Gesamtstimmen"
hamburg_2020_buergerschaftswahl_data_sub$abs_SPD <- hamburg_2020_buergerschaftswahl_data_sub$"SPD-Gesamtstimmen"
hamburg_2020_buergerschaftswahl_data_sub$abs_DIELINKE <- hamburg_2020_buergerschaftswahl_data_sub$"DIELINKE-Gesamtstimmen"
hamburg_2020_buergerschaftswahl_data_sub$abs_GRÜNE <- hamburg_2020_buergerschaftswahl_data_sub$"GRÜNE-Gesamtstimmen"
hamburg_2020_buergerschaftswahl_data_sub$abs_AfD <- hamburg_2020_buergerschaftswahl_data_sub$"AfD-Gesamtstimmen"
hamburg_2020_buergerschaftswahl_data_sub$abs_PIRATEN <- hamburg_2020_buergerschaftswahl_data_sub$`PIRATEN-Gesamtstimmen`
hamburg_2020_buergerschaftswahl_data_sub$abs_FDP <- hamburg_2020_buergerschaftswahl_data_sub$`FDP-Gesamtstimmen`
hamburg_2020_buergerschaftswahl_data_sub$abs_DiePARTEI <- hamburg_2020_buergerschaftswahl_data_sub$`DiePARTEI-Gesamtstimmen`
hamburg_2020_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- hamburg_2020_buergerschaftswahl_data_sub$"FREIEWÄHLER-Gesamtstimmen"

hamburg_2020_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2020_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

hamburg_2020_buergerschaftswahl_data_sub$sitze_CDU <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_SPD <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_DIELINKE <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_GRÜNE <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_AfD <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_FDP <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
hamburg_2020_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hamburg_2020_buergerschaftswahl_data_sub <- hamburg_2020_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2020_buergerschaftswahl_data_sub <-
  hamburg_2020_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2020_buergerschaftswahl_data_sub$Turnout <- hamburg_2020_buergerschaftswahl_data_sub$Wähler /
  hamburg_2020_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt


###### Hamburg 2025 Buergerschaftswahl ----
#### Load election data ----

hamburg_2025_buergerschaftswahl_data <- read_excel(
  "raw/hamburg/hamburg_2025.xlsx",
  sheet = 1,
  skip = 2
)

#### Recoding ----
# Create new dataframe in wide format ----
hamburg_2025_buergerschaftswahl_data_sub <- hamburg_2025_buergerschaftswahl_data |>

  filter(!is.na(Merkmal)) |>

  mutate(
    votes_total = as.numeric(Landesliste) |>
      replace_na(0) +
      as.numeric(Wahlkreislisten) |> replace_na(0),
    AGS_8dig = "02000000",
    Gebietsname = "Hamburg",
    across(2:8, ~ str_replace(.x, "×", NA_character_)),
    Merkmal = if_else(
      str_detect(
        Merkmal,
        "Wahlb|Stimm|SPD|CDU|FDP|GRÜNE|Linke|AfD|FREIE|PARTEI"
      ),
      Merkmal,
      "other"
    )
  ) |>

  group_by(AGS_8dig, Gebietsname, Merkmal) |>
  summarise(
    votes_total = sum(votes_total, na.rm = T),
    Mandatsverteilung = sum(as.numeric(Mandatsverteilung), na.rm = T)
  ) |>

  ungroup() |>

  pivot_wider(
    id_cols = c("AGS_8dig", "Gebietsname"),
    names_from = "Merkmal",
    values_from = c("votes_total", "Mandatsverteilung"),
    values_fill = NA
  )

names(hamburg_2025_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2025_buergerschaftswahl_data_sub <- hamburg_2025_buergerschaftswahl_data_sub |>
  mutate(
    Bundesland = "Hamburg",
    election_year = "2025",
    election_type = "Buergerschaftswahl (Gesamtstimmen Landesliste)",
    IDIRB = "",
    IDBA = ""
  )


# Renaming existing variables ----
hamburg_2025_buergerschaftswahl_data_sub <- hamburg_2025_buergerschaftswahl_data_sub |>
  rename(
    Gebietsname = Gebietsname,
    Wahlberechtigteinsgesamt = votes_total_Wahlberechtigte,
    Wähler = `votes_total_Wählende / Wahlbeteiligung`,
    GültigeStimmen = `votes_total_gültige Stimmen / Mandate`,
    abs_CDU = votes_total_CDU,
    abs_SPD = votes_total_SPD,
    abs_DIELINKE = `votes_total_Die Linke`,
    abs_GRÜNE = votes_total_GRÜNE,
    abs_AfD = votes_total_AfD,
    abs_FDP = votes_total_FDP,
    abs_DiePARTEI = `votes_total_Die PARTEI`,
    abs_FREIEWÄHLER = `votes_total_FREIE WÄHLER`,
    abs_Wählergruppen = votes_total_other,

    # seats
    sitze_CDU = Mandatsverteilung_CDU,
    sitze_SPD = Mandatsverteilung_SPD,
    sitze_DIELINKE = `Mandatsverteilung_Die Linke`,
    sitze_GRÜNE = Mandatsverteilung_GRÜNE,
    sitze_AfD = Mandatsverteilung_AfD,
    sitze_FDP = Mandatsverteilung_FDP,
    sitze_DiePARTEI = `Mandatsverteilung_Die PARTEI`,
    sitze_FREIEWÄHLER = `Mandatsverteilung_FREIE WÄHLER`,
    sitze_Wählergruppen = Mandatsverteilung_other
  ) |>

  mutate(
    sitze_PIRATEN = NA,
    abs_PIRATEN = NA
  ) |>

  as.data.table()

hamburg_2025_buergerschaftswahl_data_sub$gew_CDU <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_SPD <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_AfD <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_FDP <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
hamburg_2025_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA


# Creating new dataframe with selected vars ----
hamburg_2025_buergerschaftswahl_data_sub <- hamburg_2025_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2025_buergerschaftswahl_data_sub <-
  hamburg_2025_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hamburg_2025_buergerschaftswahl_data_sub$Turnout <- hamburg_2025_buergerschaftswahl_data_sub$Wähler /
  hamburg_2025_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Hamburg ----
# Merge
hamburg_kommunalwahlen <- rbind(
  hamburg_1991_1997_buergerschaftswahl_data_sub,
  hamburg_2001_buergerschaftswahl_data_sub,
  hamburg_2004_buergerschaftswahl_data_sub,
  hamburg_2008_buergerschaftswahl_data_sub,
  hamburg_2011_buergerschaftswahl_data_sub,
  hamburg_2015_buergerschaftswahl_data_sub,
  hamburg_2020_buergerschaftswahl_data_sub,
  hamburg_2025_buergerschaftswahl_data_sub
)

# Replace INF at Turnout
hamburg_kommunalwahlen$Turnout <- str_replace_all(
  hamburg_kommunalwahlen$Turnout,
  fixed("Inf"),
  NA
)

# Replace - with NA
hamburg_kommunalwahlen[hamburg_kommunalwahlen == "-"] <- NA

# Save
#write_csv(hamburg_kommunalwahlen, here::here("output/hamburg_kommunalwahlen.csv"))

######### BERLIN ----
###### Berlin 1990 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_1990_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_1990.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_1990_kommunalwahlen_data)

#### Recoding ----
# Create new dataframe ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen <- berlin_1990_kommunalwahlen_data[
  berlin_1990_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_1990_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, election_year := "1990"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigte insgesamt"
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$"Gültige Stimmen"

berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$PDS
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE/AL"
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1990_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_1990_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 1995 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_1995_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_1995.xlsx",
  sheet = "Erststimme"
))
names(berlin_1995_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1995_kommunalwahlen_data) <- str_replace_all(
  names(berlin_1995_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_1995_kommunalwahlen_data_sub_erststimmen <- berlin_1995_kommunalwahlen_data[
  berlin_1995_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_1995_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_1995_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1995_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[, election_year := "1995"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_1995_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_1995_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_1995_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_1995_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_1995_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_1995_kommunalwahlen_data_sub_erststimmen$Wähler
berlin_1995_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_1995_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_1995_kommunalwahlen_data_sub_erststimmen$CDU
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_1995_kommunalwahlen_data_sub_erststimmen$SPD
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_1995_kommunalwahlen_data_sub_erststimmen$PDS
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_1995_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_AfD <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_1995_kommunalwahlen_data_sub_erststimmen$FDP
berlin_1995_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- NA

berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_1995_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_1995_kommunalwahlen_data_sub_erststimmen <- berlin_1995_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1995_kommunalwahlen_data_sub_erststimmen <-
  berlin_1995_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_1995_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_1995_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_1995_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 1995 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_1995_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_1995.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_1995_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1995_kommunalwahlen_data) <- str_replace_all(
  names(berlin_1995_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen <- berlin_1995_kommunalwahlen_data[
  berlin_1995_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_1995_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, election_year := "1995"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$PDS
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1995_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_1995_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 1999 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_1999_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_1999.xlsx",
  sheet = "Erststimme"
))
names(berlin_1999_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1999_kommunalwahlen_data) <- str_replace_all(
  names(berlin_1999_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_1999_kommunalwahlen_data_sub_erststimmen <- berlin_1999_kommunalwahlen_data[
  berlin_1999_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_1999_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_1999_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1999_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[, election_year := "1999"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_1999_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_1999_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_1999_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_1999_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_1999_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_1999_kommunalwahlen_data_sub_erststimmen$Wähler
berlin_1999_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_1999_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_1999_kommunalwahlen_data_sub_erststimmen$CDU
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_1999_kommunalwahlen_data_sub_erststimmen$SPD
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_1999_kommunalwahlen_data_sub_erststimmen$PDS
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_1999_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_AfD <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_1999_kommunalwahlen_data_sub_erststimmen$FDP
berlin_1999_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- NA

berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_1999_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_1999_kommunalwahlen_data_sub_erststimmen <- berlin_1999_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1999_kommunalwahlen_data_sub_erststimmen <-
  berlin_1999_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_1999_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_1999_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_1999_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 1999 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_1999_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_1999.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_1999_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1999_kommunalwahlen_data) <- str_replace_all(
  names(berlin_1999_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen <- berlin_1999_kommunalwahlen_data[
  berlin_1999_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_1999_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, election_year := "1999"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$PDS
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1999_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_1999_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2001 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2001_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2001.xlsx",
  sheet = "Erststimme"
))
names(berlin_2001_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2001_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2001_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2001_kommunalwahlen_data_sub_erststimmen <- berlin_2001_kommunalwahlen_data[
  berlin_2001_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2001_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2001_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2001_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[, election_year := "2001"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_2001_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_2001_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2001_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_2001_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_2001_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_2001_kommunalwahlen_data_sub_erststimmen$Wähler
berlin_2001_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_2001_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_2001_kommunalwahlen_data_sub_erststimmen$CDU
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_2001_kommunalwahlen_data_sub_erststimmen$SPD
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_2001_kommunalwahlen_data_sub_erststimmen$PDS
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_2001_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_AfD <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_2001_kommunalwahlen_data_sub_erststimmen$FDP
berlin_2001_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- NA

berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_2001_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2001_kommunalwahlen_data_sub_erststimmen <- berlin_2001_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2001_kommunalwahlen_data_sub_erststimmen <-
  berlin_2001_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2001_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_2001_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_2001_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2001 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2001_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2001.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_2001_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2001_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2001_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen <- berlin_2001_kommunalwahlen_data[
  berlin_2001_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2001_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, election_year := "2001"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$PDS
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2001_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2001_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2006 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2006_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2006.xlsx",
  sheet = "Erststimme"
))
names(berlin_2006_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2006_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2006_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2006_kommunalwahlen_data_sub_erststimmen <- berlin_2006_kommunalwahlen_data[
  berlin_2006_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2006_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2006_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2006_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[, election_year := "2006"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_2006_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_2006_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2006_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_2006_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_2006_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_2006_kommunalwahlen_data_sub_erststimmen$Wähler
berlin_2006_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_2006_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_2006_kommunalwahlen_data_sub_erststimmen$CDU
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_2006_kommunalwahlen_data_sub_erststimmen$SPD
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_2006_kommunalwahlen_data_sub_erststimmen$`DieLinke.`
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_2006_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_AfD <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_2006_kommunalwahlen_data_sub_erststimmen$FDP
berlin_2006_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- NA

berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_2006_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2006_kommunalwahlen_data_sub_erststimmen <- berlin_2006_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2006_kommunalwahlen_data_sub_erststimmen <-
  berlin_2006_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2006_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_2006_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_2006_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2006 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2006_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2006.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_2006_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2006_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2006_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen <- berlin_2006_kommunalwahlen_data[
  berlin_2006_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2006_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, election_year := "2006"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$`DieLinke.`
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2006_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2006_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2011 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2011_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2011.xlsx",
  sheet = "Erststimme"
))
names(berlin_2011_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2011_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2011_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2011_kommunalwahlen_data_sub_erststimmen <- berlin_2011_kommunalwahlen_data[
  berlin_2011_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2011_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2011_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2011_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[, election_year := "2011"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_2011_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_2011_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2011_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_2011_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_2011_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_2011_kommunalwahlen_data_sub_erststimmen$Wähler
berlin_2011_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_2011_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_2011_kommunalwahlen_data_sub_erststimmen$CDU
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_2011_kommunalwahlen_data_sub_erststimmen$SPD
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_2011_kommunalwahlen_data_sub_erststimmen$DIELINKE
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_2011_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_AfD <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_2011_kommunalwahlen_data_sub_erststimmen$FDP
berlin_2011_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- berlin_2011_kommunalwahlen_data_sub_erststimmen$FREIEWÄHLER

berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_2011_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2011_kommunalwahlen_data_sub_erststimmen <- berlin_2011_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2011_kommunalwahlen_data_sub_erststimmen <-
  berlin_2011_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2011_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_2011_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_2011_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2011 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2011_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2011.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_2011_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2011_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2011_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen <- berlin_2011_kommunalwahlen_data[
  berlin_2011_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2011_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, election_year := "2011"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$DIELINKE
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2011_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2011_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2016 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2016_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2016.xlsx",
  sheet = "Erststimme"
))
names(berlin_2016_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2016_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2016_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2016_kommunalwahlen_data_sub_erststimmen <- berlin_2016_kommunalwahlen_data[
  berlin_2016_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2016_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2016_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2016_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[, election_year := "2016"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_2016_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_2016_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2016_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_2016_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_2016_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_2016_kommunalwahlen_data_sub_erststimmen$Wähler
berlin_2016_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_2016_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_2016_kommunalwahlen_data_sub_erststimmen$CDU
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_2016_kommunalwahlen_data_sub_erststimmen$SPD
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_2016_kommunalwahlen_data_sub_erststimmen$DIELINKE
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_2016_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_AfD <- berlin_2016_kommunalwahlen_data_sub_erststimmen$AfD
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- berlin_2016_kommunalwahlen_data_sub_erststimmen$PIRATEN
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_2016_kommunalwahlen_data_sub_erststimmen$FDP
berlin_2016_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- NA

berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_2016_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2016_kommunalwahlen_data_sub_erststimmen <- berlin_2016_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2016_kommunalwahlen_data_sub_erststimmen <-
  berlin_2016_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2016_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_2016_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_2016_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2016 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2016_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2016.xlsx",
  sheet = "Zweitstimme"
))
names(berlin_2016_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2016_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2016_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen <- berlin_2016_kommunalwahlen_data[
  berlin_2016_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2016_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, election_year := "2016"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wähler
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$DIELINKE
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$AfD
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$PIRATEN
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- NA

berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2016_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2016_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2021 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2021_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2021.xlsx",
  sheet = "AGH_W1"
))
names(berlin_2021_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2021_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2021_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2021_kommunalwahlen_data_sub_erststimmen <- berlin_2021_kommunalwahlen_data[
  berlin_2021_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2021_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2021_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2021_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[, election_year := "2021"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_2021_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_2021_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2021_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_2021_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_2021_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_2021_kommunalwahlen_data_sub_erststimmen$Wählende
berlin_2021_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_2021_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_2021_kommunalwahlen_data_sub_erststimmen$CDU
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_2021_kommunalwahlen_data_sub_erststimmen$SPD
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_2021_kommunalwahlen_data_sub_erststimmen$DIELINKE
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_2021_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_AfD <- berlin_2021_kommunalwahlen_data_sub_erststimmen$AfD
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- berlin_2021_kommunalwahlen_data_sub_erststimmen$PIRATEN
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_2021_kommunalwahlen_data_sub_erststimmen$FDP
berlin_2021_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- berlin_2021_kommunalwahlen_data_sub_erststimmen$FREIEWÄHLER

berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_2021_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2021_kommunalwahlen_data_sub_erststimmen <- berlin_2021_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2021_kommunalwahlen_data_sub_erststimmen <-
  berlin_2021_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2021_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_2021_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_2021_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2021 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2021_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/berlin/berlin_2021.xlsx",
  sheet = "AGH_W2"
))
names(berlin_2021_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2021_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2021_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen <- berlin_2021_kommunalwahlen_data[
  berlin_2021_kommunalwahlen_data$Bezirksname == "Berlin"
]

names(berlin_2021_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, election_year := "2021"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wählende
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$DIELINKE
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$AfD
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$PIRATEN
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen$FREIEWÄHLER

berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2021_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2021_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )


###### Berlin 2023 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2023_kommunalwahlen_data <- bind_rows(
  read_excel("raw/berlin/berlin_2023.xlsx", sheet = "AGH_W1"),
  read_excel("raw/berlin/berlin_2023.xlsx", sheet = "AGH_W2")
)
names(berlin_2023_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2023_kommunalwahlen_data) <- str_replace_all(
  names(berlin_2023_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
berlin_2023_kommunalwahlen_data_sub <- berlin_2023_kommunalwahlen_data |>

  group_by(Stimmart) |>
  summarise(across(where(is.numeric), ~ sum(.x)))


berlin_2023_kommunalwahlen_data_sub_erststimmen <- as.data.table(
  berlin_2023_kommunalwahlen_data_sub |>
    filter(Stimmart == "Erststimme")
)

# Creating non-existing variables ----
berlin_2023_kommunalwahlen_data_sub_erststimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2023_kommunalwahlen_data_sub_erststimmen[, Bundesland := "Berlin"]
berlin_2023_kommunalwahlen_data_sub_erststimmen[, Gebietsname := "Berlin"]
berlin_2023_kommunalwahlen_data_sub_erststimmen[, election_year := "2023"]
berlin_2023_kommunalwahlen_data_sub_erststimmen[,
  election_type := "Abgeordnetenhauswahl (Erststimmen)"
]
berlin_2023_kommunalwahlen_data_sub_erststimmen[, IDIRB := ""]
berlin_2023_kommunalwahlen_data_sub_erststimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2023_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt <- berlin_2023_kommunalwahlen_data_sub_erststimmen$"Wahlberechtigteinsgesamt"
berlin_2023_kommunalwahlen_data_sub_erststimmen$Wähler <- berlin_2023_kommunalwahlen_data_sub_erststimmen$Wählende
berlin_2023_kommunalwahlen_data_sub_erststimmen$GültigeStimmen <- berlin_2023_kommunalwahlen_data_sub_erststimmen$"GültigeStimmen"

berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_CDU <- berlin_2023_kommunalwahlen_data_sub_erststimmen$CDU
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_SPD <- berlin_2023_kommunalwahlen_data_sub_erststimmen$SPD
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_DIELINKE <- berlin_2023_kommunalwahlen_data_sub_erststimmen$DIELINKE
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_GRÜNE <- berlin_2023_kommunalwahlen_data_sub_erststimmen$"GRÜNE"
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_AfD <- berlin_2023_kommunalwahlen_data_sub_erststimmen$AfD
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_PIRATEN <- berlin_2023_kommunalwahlen_data_sub_erststimmen$PIRATEN
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_FDP <- berlin_2023_kommunalwahlen_data_sub_erststimmen$FDP
berlin_2023_kommunalwahlen_data_sub_erststimmen$abs_FREIEWÄHLER <- berlin_2023_kommunalwahlen_data_sub_erststimmen$FREIEWÄHLER

berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_CDU <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_SPD <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_DIELINKE <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_GRÜNE <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_AfD <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_PIRATEN <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_FDP <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$gew_FREIEWÄHLER <- NA

berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_CDU <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_SPD <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_DIELINKE <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_GRÜNE <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_AfD <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_PIRATEN <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_FDP <- NA
berlin_2023_kommunalwahlen_data_sub_erststimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2023_kommunalwahlen_data_sub_erststimmen <- berlin_2023_kommunalwahlen_data_sub_erststimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2023_kommunalwahlen_data_sub_erststimmen <-
  berlin_2023_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2023_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(
  berlin_2023_kommunalwahlen_data_sub_erststimmen$Wähler
) /
  as.numeric(
    berlin_2023_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt
  )

###### Berlin 2023 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2023_kommunalwahlen_data_sub_zweitstimmen <- as.data.table(
  berlin_2023_kommunalwahlen_data_sub |>
    filter(Stimmart == "Zweitstimme")
)


names(berlin_2023_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, Bundesland := "Berlin"]
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, Gebietsname := "Berlin"]
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, election_year := "2023"]
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[,
  election_type := "Abgeordnetenhauswahl (Zweitstimmen)"
]
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, IDIRB := ""]
berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, IDBA := ""]

# Renaming existing variables ----
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$"Wahlberechtigteinsgesamt"
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$Wähler <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$Wählende
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$GültigeStimmen <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$"GültigeStimmen"

berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_CDU <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$CDU
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_SPD <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$SPD
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_DIELINKE <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$DIELINKE
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_GRÜNE <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$"GRÜNE"
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_AfD <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$AfD
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_PIRATEN <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$PIRATEN
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_FDP <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$FDP
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$abs_FREIEWÄHLER <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen$FREIEWÄHLER

berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_CDU <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_SPD <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_DIELINKE <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_GRÜNE <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_AfD <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_PIRATEN <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_FDP <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$gew_FREIEWÄHLER <- NA

berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_CDU <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_SPD <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_DIELINKE <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_GRÜNE <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_AfD <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_PIRATEN <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_FDP <- NA
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
berlin_2023_kommunalwahlen_data_sub_zweitstimmen <- berlin_2023_kommunalwahlen_data_sub_zweitstimmen[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2023_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2023_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
berlin_2023_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(
  berlin_2023_kommunalwahlen_data_sub_zweitstimmen$Wähler
) /
  as.numeric(
    berlin_2023_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt
  )


####### Merge files and save overall output for Berlin ----
# Merge
berlin_kommunalwahlen <- rbind(
  berlin_1990_kommunalwahlen_data_sub_zweitstimmen,
  berlin_1995_kommunalwahlen_data_sub_zweitstimmen,
  berlin_1999_kommunalwahlen_data_sub_zweitstimmen,
  berlin_2001_kommunalwahlen_data_sub_zweitstimmen,
  berlin_2006_kommunalwahlen_data_sub_zweitstimmen,
  berlin_2011_kommunalwahlen_data_sub_zweitstimmen,
  berlin_2016_kommunalwahlen_data_sub_zweitstimmen,
  berlin_2021_kommunalwahlen_data_sub_zweitstimmen,
  berlin_2023_kommunalwahlen_data_sub_zweitstimmen
)

# Replace INF at Turnout
berlin_kommunalwahlen$Turnout <- str_replace_all(
  berlin_kommunalwahlen$Turnout,
  fixed("Inf"),
  NA
)

# Replace - with NA
berlin_kommunalwahlen[berlin_kommunalwahlen == "-"] <- NA

# Save
#write_csv(berlin_kommunalwahlen, here::here("output/berlin_kommunalwahlen.csv"))

######### NRW ----
###### NRW 1994 Kommunalwahlen ----
#### Load election data ----
nrw_1994_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_1994.xlsx",
  sheet = "summary"
))
nrw_1994_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/nrw/nrw_1994_sitze.xlsx",
  sheet = "summary"
))

# Merge Stimmen and Sitze
nrw_1994_kommunalwahlen_data <- merge(
  nrw_1994_kommunalwahlen_data,
  nrw_1994_kommunalwahlen_data_sitze,
  by.x = "Bezirksnummer",
  by.y = "Bezirksnummer"
)

names(nrw_1994_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_1994_kommunalwahlen_data) <- str_replace_all(
  names(nrw_1994_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Delete white space in Bezirksnummer ----
nrw_1994_kommunalwahlen_data$Bezirksnummer <- str_replace_all(
  nrw_1994_kommunalwahlen_data$Bezirksnummer,
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_1994_kommunalwahlen_data_sub <- nrw_1994_kommunalwahlen_data

names(nrw_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_1994_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_1994_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_1994_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_1994_kommunalwahlen_data_sub[, election_year := "1994"]
nrw_1994_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_1994_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_1994_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_1994_kommunalwahlen_data_sub$Wähler <- nrw_1994_kommunalwahlen_data_sub$Wähler
nrw_1994_kommunalwahlen_data_sub$AGS_8dig <- nrw_1994_kommunalwahlen_data_sub$Bezirksnummer
nrw_1994_kommunalwahlen_data_sub$Gebietsname <- nrw_1994_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_1994_kommunalwahlen_data_sub$GültigeStimmen <- nrw_1994_kommunalwahlen_data_sub$GültigeStimmen

nrw_1994_kommunalwahlen_data_sub$abs_CDU <- nrw_1994_kommunalwahlen_data_sub$CDU
nrw_1994_kommunalwahlen_data_sub$abs_SPD <- nrw_1994_kommunalwahlen_data_sub$SPD
nrw_1994_kommunalwahlen_data_sub$abs_DIELINKE <- nrw_1994_kommunalwahlen_data_sub$PDS
nrw_1994_kommunalwahlen_data_sub$abs_GRÜNE <- nrw_1994_kommunalwahlen_data_sub$GRÜNE
nrw_1994_kommunalwahlen_data_sub$abs_AfD <- NA
nrw_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
nrw_1994_kommunalwahlen_data_sub$abs_FDP <- nrw_1994_kommunalwahlen_data_sub$FDP
nrw_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_1994_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_1994_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_1994_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_1994_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_1994_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_1994_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_1994_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_1994_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_1994_kommunalwahlen_data_sub$sitze_CDU <- nrw_1994_kommunalwahlen_data_sub$CDU_Zusammen
nrw_1994_kommunalwahlen_data_sub$sitze_SPD <- nrw_1994_kommunalwahlen_data_sub$SPD_Zusammen
nrw_1994_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
nrw_1994_kommunalwahlen_data_sub$sitze_GRÜNE <- nrw_1994_kommunalwahlen_data_sub$Gruene_Zusammen
nrw_1994_kommunalwahlen_data_sub$sitze_AfD <- NA
nrw_1994_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
nrw_1994_kommunalwahlen_data_sub$sitze_FDP <- nrw_1994_kommunalwahlen_data_sub$FDP_Zusammen
nrw_1994_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
nrw_1994_kommunalwahlen_data_sub <- nrw_1994_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_1994_kommunalwahlen_data_sub <-
  nrw_1994_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_1994_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_1994_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 1999 Kommunalwahlen ----
#### Load election data ----
nrw_1999_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_1999.xlsx",
  sheet = "summary"
))
nrw_1999_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/nrw/nrw_1999_sitze.xlsx",
  sheet = "summary"
))

# Merge Stimmen and Sitze
nrw_1999_kommunalwahlen_data <- merge(
  nrw_1999_kommunalwahlen_data,
  nrw_1999_kommunalwahlen_data_sitze,
  by.x = "Bezirksnummer",
  by.y = "Bezirksnummer"
)

names(nrw_1999_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_1999_kommunalwahlen_data) <- str_replace_all(
  names(nrw_1999_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Delete white space in Bezirksnummer ----
nrw_1999_kommunalwahlen_data$Bezirksnummer <- str_replace_all(
  nrw_1999_kommunalwahlen_data$Bezirksnummer,
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_1999_kommunalwahlen_data_sub <- nrw_1999_kommunalwahlen_data

names(nrw_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_1999_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_1999_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_1999_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_1999_kommunalwahlen_data_sub[, election_year := "1999"]
nrw_1999_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_1999_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_1999_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_1999_kommunalwahlen_data_sub$AGS_8dig <- nrw_1999_kommunalwahlen_data_sub$Bezirksnummer
nrw_1999_kommunalwahlen_data_sub$Gebietsname <- nrw_1999_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_1999_kommunalwahlen_data_sub$Wähler <- nrw_1999_kommunalwahlen_data_sub$Wähler
nrw_1999_kommunalwahlen_data_sub$GültigeStimmen <- nrw_1999_kommunalwahlen_data_sub$GültigeStimmen

nrw_1999_kommunalwahlen_data_sub$abs_CDU <- nrw_1999_kommunalwahlen_data_sub$CDU
nrw_1999_kommunalwahlen_data_sub$abs_SPD <- nrw_1999_kommunalwahlen_data_sub$SPD
nrw_1999_kommunalwahlen_data_sub$abs_DIELINKE <- nrw_1999_kommunalwahlen_data_sub$PDS
nrw_1999_kommunalwahlen_data_sub$abs_GRÜNE <- nrw_1999_kommunalwahlen_data_sub$GRÜNE
nrw_1999_kommunalwahlen_data_sub$abs_AfD <- NA
nrw_1999_kommunalwahlen_data_sub$abs_PIRATEN <- NA
nrw_1999_kommunalwahlen_data_sub$abs_FDP <- nrw_1999_kommunalwahlen_data_sub$`F.D.P.`
nrw_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_1999_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_1999_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_1999_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_1999_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_1999_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_1999_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_1999_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_1999_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_1999_kommunalwahlen_data_sub$sitze_CDU <- nrw_1999_kommunalwahlen_data_sub$CDU_Zusammen
nrw_1999_kommunalwahlen_data_sub$sitze_SPD <- nrw_1999_kommunalwahlen_data_sub$SPD_Zusammen
nrw_1999_kommunalwahlen_data_sub$sitze_DIELINKE <- nrw_1999_kommunalwahlen_data_sub$PDS_Zusammen
nrw_1999_kommunalwahlen_data_sub$sitze_GRÜNE <- nrw_1999_kommunalwahlen_data_sub$Gruene_Zusammen
nrw_1999_kommunalwahlen_data_sub$sitze_AfD <- NA
nrw_1999_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
nrw_1999_kommunalwahlen_data_sub$sitze_FDP <- nrw_1999_kommunalwahlen_data_sub$FDP_Zusammen
nrw_1999_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
nrw_1999_kommunalwahlen_data_sub <- nrw_1999_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_1999_kommunalwahlen_data_sub <-
  nrw_1999_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_1999_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_1999_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 2004 Kommunalwahlen ----
#### Load election data ----
nrw_2004_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_2004.xlsx",
  sheet = "summary"
))
nrw_2004_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/nrw/nrw_2004_sitze.xlsx",
  sheet = "summary"
))

# Merge Stimmen and Sitze
nrw_2004_kommunalwahlen_data <- merge(
  nrw_2004_kommunalwahlen_data,
  nrw_2004_kommunalwahlen_data_sitze,
  by.x = "Bezirksnummer",
  by.y = "Bezirksnummer"
)

names(nrw_2004_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_2004_kommunalwahlen_data) <- str_replace_all(
  names(nrw_2004_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Delete white space in Bezirksnummer ----
nrw_2004_kommunalwahlen_data$Bezirksnummer <- str_replace_all(
  nrw_2004_kommunalwahlen_data$Bezirksnummer,
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_2004_kommunalwahlen_data_sub <- nrw_2004_kommunalwahlen_data

names(nrw_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2004_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_2004_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_2004_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_2004_kommunalwahlen_data_sub[, election_year := "2004"]
nrw_2004_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_2004_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_2004_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_2004_kommunalwahlen_data_sub$AGS_8dig <- nrw_2004_kommunalwahlen_data_sub$Bezirksnummer
nrw_2004_kommunalwahlen_data_sub$Gebietsname <- nrw_2004_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_2004_kommunalwahlen_data_sub$Wähler <- nrw_2004_kommunalwahlen_data_sub$Wähler
nrw_2004_kommunalwahlen_data_sub$GültigeStimmen <- nrw_2004_kommunalwahlen_data_sub$GültigeStimmen

nrw_2004_kommunalwahlen_data_sub$abs_CDU <- nrw_2004_kommunalwahlen_data_sub$CDU
nrw_2004_kommunalwahlen_data_sub$abs_SPD <- nrw_2004_kommunalwahlen_data_sub$SPD
nrw_2004_kommunalwahlen_data_sub$abs_DIELINKE <- nrw_2004_kommunalwahlen_data_sub$PDS
nrw_2004_kommunalwahlen_data_sub$abs_GRÜNE <- nrw_2004_kommunalwahlen_data_sub$GRÜNE
nrw_2004_kommunalwahlen_data_sub$abs_AfD <- NA
nrw_2004_kommunalwahlen_data_sub$abs_PIRATEN <- NA
nrw_2004_kommunalwahlen_data_sub$abs_FDP <- nrw_2004_kommunalwahlen_data_sub$FDP
nrw_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_2004_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_2004_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_2004_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_2004_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_2004_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_2004_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_2004_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_2004_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_2004_kommunalwahlen_data_sub$sitze_CDU <- nrw_2004_kommunalwahlen_data_sub$CDU_Zusammen
nrw_2004_kommunalwahlen_data_sub$sitze_SPD <- nrw_2004_kommunalwahlen_data_sub$SPD_Zusammen
nrw_2004_kommunalwahlen_data_sub$sitze_DIELINKE <- nrw_2004_kommunalwahlen_data_sub$PDS_Zusammen
nrw_2004_kommunalwahlen_data_sub$sitze_GRÜNE <- nrw_2004_kommunalwahlen_data_sub$Gruene_Zusammen
nrw_2004_kommunalwahlen_data_sub$sitze_AfD <- NA
nrw_2004_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
nrw_2004_kommunalwahlen_data_sub$sitze_FDP <- nrw_2004_kommunalwahlen_data_sub$FDP_Zusammen
nrw_2004_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
nrw_2004_kommunalwahlen_data_sub <- nrw_2004_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2004_kommunalwahlen_data_sub <-
  nrw_2004_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_2004_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_2004_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 2009 Kommunalwahlen ----
#### Load election data ----
nrw_2009_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_2009.xlsx",
  sheet = "summary"
))
nrw_2009_kommunalwahlen_data$Bezirksnummer <- as.numeric(
  nrw_2009_kommunalwahlen_data$Bezirksnummer
)
nrw_2009_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/nrw/nrw_2009_sitze.xlsx",
  sheet = "summary"
))

# Merge Stimmen and Sitze
nrw_2009_kommunalwahlen_data <- merge(
  nrw_2009_kommunalwahlen_data,
  nrw_2009_kommunalwahlen_data_sitze,
  by.x = "Bezirksnummer",
  by.y = "Bezirksnummer"
)

names(nrw_2009_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_2009_kommunalwahlen_data) <- str_replace_all(
  names(nrw_2009_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Delete white space in Bezirksnummer ----
nrw_2009_kommunalwahlen_data$Bezirksnummer <- str_replace_all(
  nrw_2009_kommunalwahlen_data$Bezirksnummer,
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_2009_kommunalwahlen_data_sub <- nrw_2009_kommunalwahlen_data

names(nrw_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2009_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_2009_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_2009_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_2009_kommunalwahlen_data_sub[, election_year := "2009"]
nrw_2009_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_2009_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_2009_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_2009_kommunalwahlen_data_sub$AGS_8dig <- nrw_2009_kommunalwahlen_data_sub$Bezirksnummer
nrw_2009_kommunalwahlen_data_sub$Gebietsname <- nrw_2009_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_2009_kommunalwahlen_data_sub$Wähler <- nrw_2009_kommunalwahlen_data_sub$Wähler
nrw_2009_kommunalwahlen_data_sub$GültigeStimmen <- nrw_2009_kommunalwahlen_data_sub$GültigeStimmen

nrw_2009_kommunalwahlen_data_sub$abs_CDU <- nrw_2009_kommunalwahlen_data_sub$CDU
nrw_2009_kommunalwahlen_data_sub$abs_SPD <- nrw_2009_kommunalwahlen_data_sub$SPD
nrw_2009_kommunalwahlen_data_sub$abs_DIELINKE <- nrw_2009_kommunalwahlen_data_sub$DIELINKE
nrw_2009_kommunalwahlen_data_sub$abs_GRÜNE <- nrw_2009_kommunalwahlen_data_sub$GRÜNE
nrw_2009_kommunalwahlen_data_sub$abs_AfD <- NA
nrw_2009_kommunalwahlen_data_sub$abs_PIRATEN <- NA
nrw_2009_kommunalwahlen_data_sub$abs_FDP <- nrw_2009_kommunalwahlen_data_sub$FDP
nrw_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_2009_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_2009_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_2009_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_2009_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_2009_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_2009_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_2009_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_2009_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_2009_kommunalwahlen_data_sub$sitze_CDU <- nrw_2009_kommunalwahlen_data_sub$CDU_Zusammen
nrw_2009_kommunalwahlen_data_sub$sitze_SPD <- nrw_2009_kommunalwahlen_data_sub$SPD_Zusammen
nrw_2009_kommunalwahlen_data_sub$sitze_DIELINKE <- nrw_2009_kommunalwahlen_data_sub$DIELINKE_Zusammen
nrw_2009_kommunalwahlen_data_sub$sitze_GRÜNE <- nrw_2009_kommunalwahlen_data_sub$Gruene_Zusammen
nrw_2009_kommunalwahlen_data_sub$sitze_AfD <- NA
nrw_2009_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
nrw_2009_kommunalwahlen_data_sub$sitze_FDP <- nrw_2009_kommunalwahlen_data_sub$FDP_Zusammen
nrw_2009_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
nrw_2009_kommunalwahlen_data_sub <- nrw_2009_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2009_kommunalwahlen_data_sub <-
  nrw_2009_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_2009_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_2009_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### NRW 2014 Kommunalwahlen ----
#### Load election data ----
nrw_2014_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_2014.xlsx",
  sheet = "summary"
))
nrw_2014_kommunalwahlen_data$Bezirksnummer <- as.numeric(
  nrw_2014_kommunalwahlen_data$Bezirksnummer
)
nrw_2014_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/nrw/nrw_2014_sitze.xlsx",
  sheet = "summary"
))

# Merge Stimmen and Sitze
nrw_2014_kommunalwahlen_data <- merge(
  nrw_2014_kommunalwahlen_data,
  nrw_2014_kommunalwahlen_data_sitze,
  by.x = "Bezirksnummer",
  by.y = "Bezirksnummer"
)

names(nrw_2014_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_2014_kommunalwahlen_data) <- str_replace_all(
  names(nrw_2014_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Delete white space in Bezirksnummer ----
nrw_2014_kommunalwahlen_data$Bezirksnummer <- str_replace_all(
  nrw_2014_kommunalwahlen_data$Bezirksnummer,
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_2014_kommunalwahlen_data_sub <- nrw_2014_kommunalwahlen_data

names(nrw_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2014_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_2014_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_2014_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_2014_kommunalwahlen_data_sub[, election_year := "2014"]
nrw_2014_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_2014_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_2014_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_2014_kommunalwahlen_data_sub$AGS_8dig <- nrw_2014_kommunalwahlen_data_sub$Bezirksnummer
nrw_2014_kommunalwahlen_data_sub$Gebietsname <- nrw_2014_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_2014_kommunalwahlen_data_sub$Wähler <- nrw_2014_kommunalwahlen_data_sub$Wähler
nrw_2014_kommunalwahlen_data_sub$GültigeStimmen <- nrw_2014_kommunalwahlen_data_sub$GültigeStimmen

nrw_2014_kommunalwahlen_data_sub$abs_CDU <- nrw_2014_kommunalwahlen_data_sub$CDU
nrw_2014_kommunalwahlen_data_sub$abs_SPD <- nrw_2014_kommunalwahlen_data_sub$SPD
nrw_2014_kommunalwahlen_data_sub$abs_DIELINKE <- nrw_2014_kommunalwahlen_data_sub$DIELINKE
nrw_2014_kommunalwahlen_data_sub$abs_GRÜNE <- nrw_2014_kommunalwahlen_data_sub$GRÜNE
nrw_2014_kommunalwahlen_data_sub$abs_AfD <- nrw_2014_kommunalwahlen_data_sub$AfD
nrw_2014_kommunalwahlen_data_sub$abs_PIRATEN <- nrw_2014_kommunalwahlen_data_sub$PIRATEN
nrw_2014_kommunalwahlen_data_sub$abs_FDP <- nrw_2014_kommunalwahlen_data_sub$FDP
nrw_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_2014_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_2014_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_2014_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_2014_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_2014_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_2014_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_2014_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_2014_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_2014_kommunalwahlen_data_sub$sitze_CDU <- nrw_2014_kommunalwahlen_data_sub$CDU_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_SPD <- nrw_2014_kommunalwahlen_data_sub$SPD_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_DIELINKE <- nrw_2014_kommunalwahlen_data_sub$DIELINKE_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_GRÜNE <- nrw_2014_kommunalwahlen_data_sub$Gruene_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_AfD <- nrw_2014_kommunalwahlen_data_sub$AfD_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_PIRATEN <- nrw_2014_kommunalwahlen_data_sub$PIRATEN_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_FDP <- nrw_2014_kommunalwahlen_data_sub$FDP_Zusammen
nrw_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
nrw_2014_kommunalwahlen_data_sub <- nrw_2014_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2014_kommunalwahlen_data_sub <-
  nrw_2014_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_2014_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_2014_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 2020 Kommunalwahlen ----
#### Load election data ----
nrw_2020_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_2020.xlsx",
  sheet = "summary"
))
nrw_2020_kommunalwahlen_data$Bezirksnummer <- as.numeric(
  nrw_2020_kommunalwahlen_data$Bezirksnummer
)
nrw_2020_kommunalwahlen_data_sitze <- as.data.table(read_excel(
  "raw/nrw/nrw_2020_sitze.xlsx",
  sheet = "summary"
))

# Merge Stimmen and Sitze
nrw_2020_kommunalwahlen_data <- merge(
  nrw_2020_kommunalwahlen_data,
  nrw_2020_kommunalwahlen_data_sitze,
  by.x = "Bezirksnummer",
  by.y = "Bezirksnummer"
)


#### Recoding ----
# Delete white space ----
names(nrw_2020_kommunalwahlen_data) <- str_replace_all(
  names(nrw_2020_kommunalwahlen_data),
  fixed(" "),
  ""
)
# Delete white space in Bezirksnummer ----
nrw_2020_kommunalwahlen_data$Bezirksnummer <- str_replace_all(
  nrw_2020_kommunalwahlen_data$Bezirksnummer,
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_2020_kommunalwahlen_data_sub <- nrw_2020_kommunalwahlen_data

names(nrw_2020_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2020_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_2020_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_2020_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_2020_kommunalwahlen_data_sub[, election_year := "2020"]
nrw_2020_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_2020_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_2020_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_2020_kommunalwahlen_data_sub[nrw_2020_kommunalwahlen_data_sub == "-"] <- NA

nrw_2020_kommunalwahlen_data_sub$AGS_8dig <- nrw_2020_kommunalwahlen_data_sub$Bezirksnummer
nrw_2020_kommunalwahlen_data_sub$Gebietsname <- nrw_2020_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_2020_kommunalwahlen_data_sub$Wähler <- nrw_2020_kommunalwahlen_data_sub$"Wähler/-inneninsgesamt"
nrw_2020_kommunalwahlen_data_sub$GültigeStimmen <- nrw_2020_kommunalwahlen_data_sub$GültigeStimmen

nrw_2020_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$CDU
)
nrw_2020_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$SPD
)
nrw_2020_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$DIELINKE
)
nrw_2020_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$GRÜNE
)
nrw_2020_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$AfD
)
nrw_2020_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$PIRATEN
)
nrw_2020_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$FDP
)
nrw_2020_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_2020_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_2020_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_2020_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_2020_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_2020_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_2020_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_2020_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_2020_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_2020_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$CDU_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$SPD_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$DIELINKE_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$Gruene_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_AfD <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$AfD_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_PIRATEN <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$PIRATEN_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$FDP_Zusammen
)
nrw_2020_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA


# Creating new dataframe with selected vars ----
nrw_2020_kommunalwahlen_data_sub <- nrw_2020_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2020_kommunalwahlen_data_sub <-
  nrw_2020_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_2020_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_2020_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

# Fix AGS ----
nrw_2020_kommunalwahlen_data_sub$AGS_8dig <- paste(
  "5",
  nrw_2020_kommunalwahlen_data_sub$AGS_8dig,
  sep = ""
)

###### NRW Kommunalwahlen for kreisfreie Städte ----
#### Load election data ----
nrw_kreisfreie_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/nrw/nrw_kreisfreie_staedte.xlsx",
  sheet = "summary"
))


#### Recoding ----
# Delete white space ----
names(nrw_kreisfreie_kommunalwahlen_data) <- str_replace_all(
  names(nrw_kreisfreie_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data

names(nrw_kreisfreie_kommunalwahlen_data_sub)

# Filter Kreise and Regierungsbezirke ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  filter(
    nchar(ags) > 3,
    !grepl("Kreis", gebiet)
  )

# Fix AGS ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  mutate(
    ags = paste0(ags, "000")
  )

# Creating non-existing variables ----
nrw_kreisfreie_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_kreisfreie_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_kreisfreie_kommunalwahlen_data_sub[, Gebietsname := ""]

nrw_kreisfreie_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_kreisfreie_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_kreisfreie_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_kreisfreie_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_kreisfreie_kommunalwahlen_data_sub$Wahlberechtigte
nrw_kreisfreie_kommunalwahlen_data_sub$Wähler <- nrw_kreisfreie_kommunalwahlen_data_sub$Wähler
nrw_kreisfreie_kommunalwahlen_data_sub$election_year <- nrw_kreisfreie_kommunalwahlen_data_sub$year
nrw_kreisfreie_kommunalwahlen_data_sub$AGS_8dig <- nrw_kreisfreie_kommunalwahlen_data_sub$ags
nrw_kreisfreie_kommunalwahlen_data_sub$Gebietsname <- nrw_kreisfreie_kommunalwahlen_data_sub$gebiet
nrw_kreisfreie_kommunalwahlen_data_sub$GültigeStimmen <- nrw_kreisfreie_kommunalwahlen_data_sub$GültigeStimmen

nrw_kreisfreie_kommunalwahlen_data_sub$abs_CDU <- nrw_kreisfreie_kommunalwahlen_data_sub$CDU
nrw_kreisfreie_kommunalwahlen_data_sub$abs_SPD <- nrw_kreisfreie_kommunalwahlen_data_sub$SPD
nrw_kreisfreie_kommunalwahlen_data_sub$abs_DIELINKE <- nrw_kreisfreie_kommunalwahlen_data_sub$DIELINKE
nrw_kreisfreie_kommunalwahlen_data_sub$abs_GRÜNE <- nrw_kreisfreie_kommunalwahlen_data_sub$GRÜNE
nrw_kreisfreie_kommunalwahlen_data_sub$abs_AfD <- nrw_kreisfreie_kommunalwahlen_data_sub$AfD
nrw_kreisfreie_kommunalwahlen_data_sub$abs_PIRATEN <- nrw_kreisfreie_kommunalwahlen_data_sub$PIRATEN
nrw_kreisfreie_kommunalwahlen_data_sub$abs_FDP <- nrw_kreisfreie_kommunalwahlen_data_sub$FDP
nrw_kreisfreie_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_kreisfreie_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_kreisfreie_kommunalwahlen_data_sub$sitze_CDU <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_SPD <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_AfD <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_FDP <- NA
nrw_kreisfreie_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_kreisfreie_kommunalwahlen_data_sub <-
  nrw_kreisfreie_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_kreisfreie_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_kreisfreie_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_kreisfreie_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

# Filter Hochsauerlandkreis ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  filter(!AGS_8dig == "05958000")

# Filter 1989 ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  filter(!election_year == "1989")


###### NRW 2025 Kommunalwahlen ----
#### Load election data ----
nrw_2025_kommunalwahlen_data <- as.data.table(
  read_delim("raw/nrw/nrw_2025.csv", delim = ";", col_types = "c") |>

    # add kreisfreie Städte
    bind_rows(
      read_delim("raw/nrw/nrw_2025_kreise.csv", delim = ";", col_types = "c") |>
        filter(str_detect(`Verwaltungsbezirks-Name`, "Krfr"))
    )
)

nrw_2025_kommunalwahlen_data_sitze <- as.data.table(
  read_delim(
    "raw/nrw/nrw_2025_sitze.csv",
    delim = ";",
    skip = 3,
    col_types = "c"
  ) |>

    # add kreisfreie Städte
    bind_rows(
      read_delim(
        "raw/nrw/nrw_2025_sitze_kreise.csv",
        delim = ";",
        skip = 3,
        col_types = "c"
      ) |>
        filter(str_detect(`Verwaltungsbezirks-Name`, "Krfr"))
    )
) |>
  rename_with(~ paste0(.x, "_sitze"))

# Merge Stimmen and Sitze
nrw_2025_kommunalwahlen_data <- merge(
  nrw_2025_kommunalwahlen_data,
  nrw_2025_kommunalwahlen_data_sitze,
  by.x = "Verwaltungsbezirks-Nr.",
  by.y = "Verwaltungsbezirks-Nr._sitze"
)


#### Recoding ----
# Delete white space ----
names(nrw_2025_kommunalwahlen_data) <- str_replace_all(
  names(nrw_2025_kommunalwahlen_data),
  fixed(" "),
  ""
)
# Delete white space in Bezirksnummer ----
nrw_2025_kommunalwahlen_data$"Verwaltungsbezirks-Nr." <- str_replace_all(
  nrw_2025_kommunalwahlen_data$"Verwaltungsbezirks-Nr.",
  fixed(" "),
  ""
)

# Create new dataframe ----
nrw_2025_kommunalwahlen_data_sub <- nrw_2025_kommunalwahlen_data

names(nrw_2025_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2025_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
nrw_2025_kommunalwahlen_data_sub[, Bundesland := "NRW"]
nrw_2025_kommunalwahlen_data_sub[, Gebietsname := ""]
nrw_2025_kommunalwahlen_data_sub[, election_year := "2025"]
nrw_2025_kommunalwahlen_data_sub[, election_type := "Kommunalwahl"]
nrw_2025_kommunalwahlen_data_sub[, IDIRB := ""]
nrw_2025_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
nrw_2025_kommunalwahlen_data_sub[nrw_2025_kommunalwahlen_data_sub == "-"] <- NA

nrw_2025_kommunalwahlen_data_sub$AGS_8dig <- nrw_2025_kommunalwahlen_data_sub$"Verwaltungsbezirks-Nr."
nrw_2025_kommunalwahlen_data_sub$Gebietsname <- nrw_2025_kommunalwahlen_data_sub$`Verwaltungsbezirks-Name`
nrw_2025_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_2025_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_2025_kommunalwahlen_data_sub$Wähler <- nrw_2025_kommunalwahlen_data_sub$"Wähler/-innen"
nrw_2025_kommunalwahlen_data_sub$GültigeStimmen <- nrw_2025_kommunalwahlen_data_sub$GültigeStimmen

nrw_2025_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$CDU
)
nrw_2025_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$SPD
)
nrw_2025_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$DieLinke
)
nrw_2025_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$GRÜNE
)
nrw_2025_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$AfD
)
nrw_2025_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$PIRATEN
)
nrw_2025_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$FDP
)
nrw_2025_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$FREIEWÄHLER
)

nrw_2025_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_2025_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_2025_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_2025_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_2025_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_2025_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_2025_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_2025_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_2025_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$CDU_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$SPD_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$DieLinke_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$GRÜNE_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_AfD <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$AfD_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_PIRATEN <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$PIRATEN_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$FDP_sitze
)
nrw_2025_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$FREIEWÄHLER_sitze
)


# Creating new dataframe with selected vars ----
nrw_2025_kommunalwahlen_data_sub <- nrw_2025_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2025_kommunalwahlen_data_sub <-
  nrw_2025_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
nrw_2025_kommunalwahlen_data_sub$Turnout <- as.numeric(
  nrw_2025_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(nrw_2025_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

# Fix AGS ----
nrw_2025_kommunalwahlen_data_sub$AGS_8dig <- paste(
  "05",
  nrw_2025_kommunalwahlen_data_sub$AGS_8dig,
  sep = ""
)


####### Merge files and save overall output for NRW ----
# Merge
nrw_kommunalwahlen <- rbind(
  nrw_1994_kommunalwahlen_data_sub,
  nrw_1999_kommunalwahlen_data_sub,
  nrw_2004_kommunalwahlen_data_sub,
  nrw_2009_kommunalwahlen_data_sub,
  nrw_2014_kommunalwahlen_data_sub,
  nrw_2020_kommunalwahlen_data_sub,
  nrw_2025_kommunalwahlen_data_sub,
  nrw_kreisfreie_kommunalwahlen_data_sub
) %>%
  filter(!grepl("Städteregion", Gebietsname)) %>%
  arrange(election_year, AGS_8dig)


# Replace INF at Turnout
nrw_kommunalwahlen$Turnout <- str_replace_all(
  nrw_kommunalwahlen$Turnout,
  fixed("Inf"),
  NA
)

# Replace - with NA
nrw_kommunalwahlen[nrw_kommunalwahlen == "-"] <- NA

# Fix AGS
nrw_kommunalwahlen$AGS_8dig <- stri_pad_left(
  nrw_kommunalwahlen$AGS_8dig,
  8,
  "0"
)

# Reorder
nrw_kommunalwahlen <- nrw_kommunalwahlen %>%
  arrange(election_year, AGS_8dig)

# Save
#write_csv(nrw_kommunalwahlen, here::here("output/nrw_kommunalwahlen.csv"))

######### SAARLAND ----
###### Saarland  Kommunalwahlen 1984 - 2019 ----
#### Load election data ----
saarland_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/saarland/saarland_grw.xlsx",
  sheet = "summary"
))

names(saarland_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(saarland_kommunalwahlen_data) <- str_replace_all(
  names(saarland_kommunalwahlen_data),
  fixed(" "),
  ""
)

# Create new dataframe ----
saarland_kommunalwahlen_data_sub <- saarland_kommunalwahlen_data

names(saarland_kommunalwahlen_data_sub)

# Creating non-existing variables ----
saarland_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
saarland_kommunalwahlen_data_sub[, Bundesland := "Saarland"]
saarland_kommunalwahlen_data_sub[, Gebietsname := ""]
saarland_kommunalwahlen_data_sub[, election_year := ""]
saarland_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
saarland_kommunalwahlen_data_sub[, IDIRB := ""]
saarland_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
saarland_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- saarland_kommunalwahlen_data_sub$Wahlberechtigte
saarland_kommunalwahlen_data_sub$Wähler <- saarland_kommunalwahlen_data_sub$Wähler
saarland_kommunalwahlen_data_sub$AGS_8dig <- saarland_kommunalwahlen_data_sub$AGS
saarland_kommunalwahlen_data_sub$Gebietsname <- saarland_kommunalwahlen_data_sub$Regionalverband
saarland_kommunalwahlen_data_sub$election_year <- saarland_kommunalwahlen_data_sub$Jahr
saarland_kommunalwahlen_data_sub$GültigeStimmen <- saarland_kommunalwahlen_data_sub$GültigeStimmen

saarland_kommunalwahlen_data_sub$abs_CDU <- saarland_kommunalwahlen_data_sub$CDU_abs
saarland_kommunalwahlen_data_sub$abs_SPD <- saarland_kommunalwahlen_data_sub$SPD_abs
saarland_kommunalwahlen_data_sub$abs_DIELINKE <- saarland_kommunalwahlen_data_sub$DIELINKE_abs
saarland_kommunalwahlen_data_sub$abs_GRÜNE <- saarland_kommunalwahlen_data_sub$GRÜNE
saarland_kommunalwahlen_data_sub$abs_AfD <- saarland_kommunalwahlen_data_sub$AfD_abs
saarland_kommunalwahlen_data_sub$abs_PIRATEN <- NA
saarland_kommunalwahlen_data_sub$abs_FDP <- saarland_kommunalwahlen_data_sub$FDP_abs
saarland_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

saarland_kommunalwahlen_data_sub$gew_CDU <- NA
saarland_kommunalwahlen_data_sub$gew_SPD <- NA
saarland_kommunalwahlen_data_sub$gew_DIELINKE <- NA
saarland_kommunalwahlen_data_sub$gew_GRÜNE <- NA
saarland_kommunalwahlen_data_sub$gew_AfD <- NA
saarland_kommunalwahlen_data_sub$gew_PIRATEN <- NA
saarland_kommunalwahlen_data_sub$gew_FDP <- NA
saarland_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

saarland_kommunalwahlen_data_sub$sitze_CDU <- NA
saarland_kommunalwahlen_data_sub$sitze_SPD <- NA
saarland_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
saarland_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
saarland_kommunalwahlen_data_sub$sitze_AfD <- NA
saarland_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
saarland_kommunalwahlen_data_sub$sitze_FDP <- NA
saarland_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
saarland_kommunalwahlen_data_sub <- saarland_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


###### Saarland  Kommunalwahlen 2024----
#### Load election data ----
saarland_2024_kommunalwahlen_data <- read_delim(
  "raw/saarland/saarland_2024.csv",
  delim = ";",
  skip = 2
)

names(saarland_2024_kommunalwahlen_data)


#### Recoding ----

# Create new dataframe ----
saarland_2024_kommunalwahlen_data_sub <- saarland_2024_kommunalwahlen_data |>
  filter(str_detect(Nr, "[:digit:]{5}")) |>

  # Creating non-existing variables ----
  mutate(
    AGS_8dig = paste0("100", Nr),
    Bundesland = "Saarland",
    election_year = 2024,
    election_type = "Kommunalwahlen",
    IDIRB = "",
    IDBA = "",

    # fix Freie Wähler
    across(starts_with("Freie Wähler", ignore.case = T), ~ as.numeric(.x)),

    FREIEWÄHLER = rowSums(
      across(starts_with("Freie Wähler", ignore.case = T)),
      na.rm = TRUE
    )
  ) |>

  # Renaming existing variables ----
  rename(
    Gebietsname = Gebiet,
    Wahlberechtigteinsgesamt = Wahlberechtigte,
    Wähler = Wähler,
    GültigeStimmen = Gültige,
    abs_CDU = `Christlich Demokratische Union Deutschlands`,
    abs_SPD = `Sozialdemokratische Partei Deutschlands`,
    abs_DIELINKE = `DIE LINKE`,
    abs_GRÜNE = `BÜNDNIS 90/DIE GRÜNEN`,
    abs_AfD = `Alternative für Deutschland`,
    abs_FDP = `Freie Demokratische Partei`,
    abs_DiePARTEI = `Partei für Arbeit, Rechtsstaat, Tierschutz, Elitenförderung und basisdemokratische Initiative`,
    abs_FREIEWÄHLER = FREIEWÄHLER
  ) |>

  mutate(
    sitze_PIRATEN = NA,
    abs_PIRATEN = NA,
    across(matches("^(abs|Wahl|Wähl|Gült)"), ~ as.numeric(.x))
  ) |>

  as.data.table()

saarland_2024_kommunalwahlen_data_sub$gew_CDU <- NA
saarland_2024_kommunalwahlen_data_sub$gew_SPD <- NA
saarland_2024_kommunalwahlen_data_sub$gew_DIELINKE <- NA
saarland_2024_kommunalwahlen_data_sub$gew_GRÜNE <- NA
saarland_2024_kommunalwahlen_data_sub$gew_AfD <- NA
saarland_2024_kommunalwahlen_data_sub$gew_PIRATEN <- NA
saarland_2024_kommunalwahlen_data_sub$gew_FDP <- NA
saarland_2024_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
saarland_2024_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

names(saarland_2024_kommunalwahlen_data_sub)

saarland_2024_kommunalwahlen_data_sub$sitze_CDU <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_SPD <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_AfD <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_FDP <- NA
saarland_2024_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
saarland_2024_kommunalwahlen_data_sub <- saarland_2024_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER
)]


####### Merge files and save overall output for Saarland ----
# Merge
saarland_kommunalwahlen_data_sub <- rbind(
  saarland_kommunalwahlen_data_sub,
  saarland_2024_kommunalwahlen_data_sub
)


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

saarland_kommunalwahlen_data_sub <-
  saarland_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
saarland_kommunalwahlen_data_sub$Turnout <- as.numeric(
  saarland_kommunalwahlen_data_sub$Wähler
) /
  as.numeric(saarland_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

saarland_kommunalwahlen_data_sub <- saarland_kommunalwahlen_data_sub[
  with(saarland_kommunalwahlen_data_sub, order(election_year, AGS_8dig)),
]


####### Merge files and save overall output for Saarland ----

# Save
#write_csv(saarland_kommunalwahlen_data_sub, here::here("output/saarland_kommunalwahlen.csv"))

######### SACHSEN-ANHALT ----
###### Sachsen-Anhalt 1994 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_1994_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/sachsen_anhalt/sachsen-anhalt_1994_to_2019.xlsx",
  sheet = "1994"
))

#### Delete white space ----
names(sachsen_anhalt_1994_kommunalwahlen_data) <- str_replace_all(
  names(sachsen_anhalt_1994_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_1994_kommunalwahlen_data_sub <- sachsen_anhalt_1994_kommunalwahlen_data

names(sachsen_anhalt_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_1994_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_1994_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_1994_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_1994_kommunalwahlen_data_sub[, election_year := "1994"]
sachsen_anhalt_1994_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_1994_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_1994_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_1994_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_1994_kommunalwahlen_data_sub$AGS
sachsen_anhalt_1994_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_1994_kommunalwahlen_data_sub$Gemeindename
sachsen_anhalt_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_1994_kommunalwahlen_data_sub$Wahlberechtigteabsolut
sachsen_anhalt_1994_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_1994_kommunalwahlen_data_sub$Waehler
sachsen_anhalt_1994_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_1994_kommunalwahlen_data_sub$gueltigeStimmen

sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_1994_kommunalwahlen_data_sub$CDU
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_1994_kommunalwahlen_data_sub$SPD
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_1994_kommunalwahlen_data_sub$PDS
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_1994_kommunalwahlen_data_sub$Gruene
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_1994_kommunalwahlen_data_sub$FDP
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_1994_kommunalwahlen_data_sub <- sachsen_anhalt_1994_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_1994_kommunalwahlen_data_sub <-
  sachsen_anhalt_1994_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_1994_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_1994_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Sachsen-Anhalt 1999 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_1999_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/sachsen_anhalt/sachsen-anhalt_1994_to_2019.xlsx",
  sheet = "1999"
))

#### Delete white space ----
names(sachsen_anhalt_1999_kommunalwahlen_data) <- str_replace_all(
  names(sachsen_anhalt_1999_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_1999_kommunalwahlen_data_sub <- sachsen_anhalt_1999_kommunalwahlen_data

names(sachsen_anhalt_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_1999_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_1999_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_1999_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_1999_kommunalwahlen_data_sub[, election_year := "1999"]
sachsen_anhalt_1999_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_1999_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_1999_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_1999_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_1999_kommunalwahlen_data_sub$AGS
sachsen_anhalt_1999_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_1999_kommunalwahlen_data_sub$Gemeindename
sachsen_anhalt_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_1999_kommunalwahlen_data_sub$Wahlberechtigteabsolut
sachsen_anhalt_1999_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_1999_kommunalwahlen_data_sub$Waehler
sachsen_anhalt_1999_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_1999_kommunalwahlen_data_sub$gueltigeStimmen

sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_1999_kommunalwahlen_data_sub$CDU
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_1999_kommunalwahlen_data_sub$SPD
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_1999_kommunalwahlen_data_sub$PDS
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_1999_kommunalwahlen_data_sub$Gruene
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_1999_kommunalwahlen_data_sub$FDP
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_1999_kommunalwahlen_data_sub <- sachsen_anhalt_1999_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_1999_kommunalwahlen_data_sub <-
  sachsen_anhalt_1999_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_1999_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_1999_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Sachsen-Anhalt 2004 Kommunalwahlen ----
#### Load election data ----
sachsen_anhalt_2004_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/sachsen_anhalt/sachsen-anhalt_1994_to_2019.xlsx",
  sheet = "2001-2004"
))

#### Delete white space ----
names(sachsen_anhalt_2004_kommunalwahlen_data) <- str_replace_all(
  names(sachsen_anhalt_2004_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2004_kommunalwahlen_data_sub <- sachsen_anhalt_2004_kommunalwahlen_data %>%
  dplyr::filter(
    Datum == as.Date("2004-06-13")
  )

names(sachsen_anhalt_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_2004_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2004_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2004_kommunalwahlen_data_sub[, election_year := "2004"]
sachsen_anhalt_2004_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_2004_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_2004_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_2004_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2004_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2004_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2004_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2004_kommunalwahlen_data_sub$Gemeindename
sachsen_anhalt_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2004_kommunalwahlen_data_sub$Wahlberechtigteabsolut
sachsen_anhalt_2004_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2004_kommunalwahlen_data_sub$Waehler
sachsen_anhalt_2004_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2004_kommunalwahlen_data_sub$gueltigeStimmen

sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2004_kommunalwahlen_data_sub$CDU
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2004_kommunalwahlen_data_sub$SPD
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2004_kommunalwahlen_data_sub$PDS
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2004_kommunalwahlen_data_sub$Gruene
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2004_kommunalwahlen_data_sub$FDP
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_2004_kommunalwahlen_data_sub <- sachsen_anhalt_2004_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2004_kommunalwahlen_data_sub <-
  sachsen_anhalt_2004_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2004_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2004_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Sachsen-Anhalt 2009 Kommunalwahlen ----
#### Load election data ----
sachsen_anhalt_2009_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/sachsen_anhalt/sachsen-anhalt_1994_to_2019.xlsx",
  sheet = "2005-2010"
))

#### Delete white space ----
names(sachsen_anhalt_2009_kommunalwahlen_data) <- str_replace_all(
  names(sachsen_anhalt_2009_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2009_kommunalwahlen_data_sub <- sachsen_anhalt_2009_kommunalwahlen_data %>%
  dplyr::mutate(
    election_year = lubridate::year(Datum)
  )

# Creating non-existing variables ----
sachsen_anhalt_2009_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2009_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2009_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_2009_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_2009_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_2009_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2009_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2009_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2009_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2009_kommunalwahlen_data_sub$Gemeindename
sachsen_anhalt_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2009_kommunalwahlen_data_sub$Wahlberechtigteabsolut
sachsen_anhalt_2009_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2009_kommunalwahlen_data_sub$Waehler
sachsen_anhalt_2009_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2009_kommunalwahlen_data_sub$gueltigeStimmen

sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2009_kommunalwahlen_data_sub$CDU
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2009_kommunalwahlen_data_sub$SPD
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2009_kommunalwahlen_data_sub$LINKE
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2009_kommunalwahlen_data_sub$Gruene
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2009_kommunalwahlen_data_sub$FDP
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_2009_kommunalwahlen_data_sub <- sachsen_anhalt_2009_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2009_kommunalwahlen_data_sub <-
  sachsen_anhalt_2009_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2009_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2009_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Sachsen-Anhalt 2014 Kommunalwahlen ----
#### Load election data ----
sachsen_anhalt_2014_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/sachsen_anhalt/sachsen-anhalt_1994_to_2019.xlsx",
  sheet = "2014-2015"
))

#### Delete white space ----
names(sachsen_anhalt_2014_kommunalwahlen_data) <- str_replace_all(
  names(sachsen_anhalt_2014_kommunalwahlen_data),
  fixed(" "),
  ""
)

lubridate::year(sachsen_anhalt_2014_kommunalwahlen_data$Datum)

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2014_kommunalwahlen_data_sub <- sachsen_anhalt_2014_kommunalwahlen_data

# Creating non-existing variables ----
sachsen_anhalt_2014_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2014_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2014_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_2014_kommunalwahlen_data_sub[, election_year := "2014"]
sachsen_anhalt_2014_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_2014_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_2014_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2014_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2014_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2014_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Gemeindename
sachsen_anhalt_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Wahlberechtigteabsolut
sachsen_anhalt_2014_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Waehler
sachsen_anhalt_2014_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2014_kommunalwahlen_data_sub$gueltigeStimmen

sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  sachsen_anhalt_2014_kommunalwahlen_data_sub$CDU
)
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2014_kommunalwahlen_data_sub$SPD
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2014_kommunalwahlen_data_sub$LINKE
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Gruene
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_AfD <- sachsen_anhalt_2014_kommunalwahlen_data_sub$AfD
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_PIRATEN <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Piraten
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2014_kommunalwahlen_data_sub$FDP
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_DiePARTEI <- sachsen_anhalt_2014_kommunalwahlen_data_sub$DiePartei
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- sachsen_anhalt_2014_kommunalwahlen_data_sub$FW

sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_2014_kommunalwahlen_data_sub <- sachsen_anhalt_2014_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2014_kommunalwahlen_data_sub <-
  sachsen_anhalt_2014_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2014_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Sachsen-Anhalt 2019 Kommunalwahlen ----
#### Load election data ----
sachsen_anhalt_2019_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/sachsen_anhalt/sachsen-anhalt_1994_to_2019.xlsx",
  sheet = "2019"
))

#### Delete white space ----
names(sachsen_anhalt_2019_kommunalwahlen_data) <- str_replace_all(
  names(sachsen_anhalt_2019_kommunalwahlen_data),
  fixed(" "),
  ""
)

lubridate::year(sachsen_anhalt_2019_kommunalwahlen_data$Datum)

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2019_kommunalwahlen_data_sub <- sachsen_anhalt_2019_kommunalwahlen_data

# Creating non-existing variables ----
sachsen_anhalt_2019_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2019_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2019_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_2019_kommunalwahlen_data_sub[, election_year := "2019"]
sachsen_anhalt_2019_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_2019_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_2019_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2019_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2019_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2019_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2019_kommunalwahlen_data_sub$Gemeindename
sachsen_anhalt_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2019_kommunalwahlen_data_sub$Wahlberechtigteabsolut
sachsen_anhalt_2019_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2019_kommunalwahlen_data_sub$Waehler
sachsen_anhalt_2019_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2019_kommunalwahlen_data_sub$gueltigeStimmen

sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  sachsen_anhalt_2019_kommunalwahlen_data_sub$CDU
)
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  sachsen_anhalt_2019_kommunalwahlen_data_sub$SPD
)
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2019_kommunalwahlen_data_sub$LINKE
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2019_kommunalwahlen_data_sub$Gruene
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_AfD <- sachsen_anhalt_2019_kommunalwahlen_data_sub$AfD
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2019_kommunalwahlen_data_sub$FDP
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_DiePARTEI <- sachsen_anhalt_2019_kommunalwahlen_data_sub$DiePartei
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- sachsen_anhalt_2019_kommunalwahlen_data_sub$FW

sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_2019_kommunalwahlen_data_sub <- sachsen_anhalt_2019_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2019_kommunalwahlen_data_sub <-
  sachsen_anhalt_2019_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2019_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2019_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt


###### Sachsen-Anhalt 2024 Kommunalwahlen ----
#### Load election data ----
sachsen_anhalt_2024_kommunalwahlen_data <- as.data.table(read_delim(
  "raw/sachsen_anhalt/sachsen-anhalt_2024.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
))


#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2024_kommunalwahlen_data_sub <- sachsen_anhalt_2024_kommunalwahlen_data

# Creating non-existing variables ----
sachsen_anhalt_2024_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2024_kommunalwahlen_data_sub[, Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2024_kommunalwahlen_data_sub[, Gebietsname := ""]
sachsen_anhalt_2024_kommunalwahlen_data_sub[, election_year := "2024"]
sachsen_anhalt_2024_kommunalwahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_anhalt_2024_kommunalwahlen_data_sub[, IDIRB := ""]
sachsen_anhalt_2024_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2024_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2024_kommunalwahlen_data_sub$Schlüsselnummer
sachsen_anhalt_2024_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2024_kommunalwahlen_data_sub$Name
sachsen_anhalt_2024_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`A - Wahlberechtigte`
sachsen_anhalt_2024_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`B - Wähler`
sachsen_anhalt_2024_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`C2 - Gültige Stimmzettel`

sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  sachsen_anhalt_2024_kommunalwahlen_data_sub$`D01 - CDU`
)
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  sachsen_anhalt_2024_kommunalwahlen_data_sub$`D04 - SPD`
)
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`D03 - DIE LINKE`
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`D06 - GRÜNE`
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_AfD <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`D02 - AfD`
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`D05 - FDP`
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_DiePARTEI <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`D11 - Die PARTEI`
sachsen_anhalt_2024_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- sachsen_anhalt_2024_kommunalwahlen_data_sub$`D07 - FREIE WÄHLER`

sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_2024_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_2024_kommunalwahlen_data_sub <- sachsen_anhalt_2024_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2024_kommunalwahlen_data_sub <-
  sachsen_anhalt_2024_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ as.numeric(.) / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2024_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2024_kommunalwahlen_data_sub$Wähler /
  sachsen_anhalt_2024_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Sachsen-Anhalt ----
# Merge
sachsen_anhalt_kommunalwahlen <- rbind(
  sachsen_anhalt_1994_kommunalwahlen_data_sub,
  sachsen_anhalt_1999_kommunalwahlen_data_sub,
  sachsen_anhalt_2004_kommunalwahlen_data_sub,
  sachsen_anhalt_2009_kommunalwahlen_data_sub,
  sachsen_anhalt_2014_kommunalwahlen_data_sub,
  sachsen_anhalt_2019_kommunalwahlen_data_sub,
  sachsen_anhalt_2024_kommunalwahlen_data_sub
)

# Replace INF at Turnout
sachsen_anhalt_kommunalwahlen$Turnout <- str_replace_all(
  sachsen_anhalt_kommunalwahlen$Turnout,
  fixed("Inf"),
  NA
)

# Replace - with NA
sachsen_anhalt_kommunalwahlen[sachsen_anhalt_kommunalwahlen == "-"] <- NA

# Save
#write_csv(sachsen_anhalt_kommunalwahlen, here::here("output/sachsen_anhalt_kommunalwahlen.csv"))

######### BADEN-WUERTTEMBERG ----
###### Baden-Wuerttemberg 1989 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_1989_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_1989.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_1989_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_1989_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1989_gemeinderatswahlen_data

names(baden_wuerttemberg_1989_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[, election_year := "1989"]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$AGS_8dig <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$AGS
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Gebietsname <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Gemeindename
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Waehler
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$GueltigeStimmen

baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$CDU_abs
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$SPD_abs
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Gruene_abs
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_AfD <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$FDP_abs
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$CDU_sitze
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$SPD_sitze
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Gruene_sitze
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_AfD <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$FDP_sitze
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_1989_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_1989_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 1994 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_1994_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_1994.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_1994_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_1994_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1994_gemeinderatswahlen_data

names(baden_wuerttemberg_1994_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[, election_year := "1994"]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$AGS_8dig <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$AGS
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Gebietsname <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Gemeindename
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Waehler
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$GueltigeStimmen

baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$CDU_abs
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$SPD_abs
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Gruene_abs
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_AfD <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$FDP_abs
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$CDU_sitze
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$SPD_sitze
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Gruene_sitze
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_AfD <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$FDP_sitze
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_1994_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 1999 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_1999_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_1999.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_1999_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_1999_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1999_gemeinderatswahlen_data

names(baden_wuerttemberg_1999_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[, election_year := "1999"]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$AGS_8dig <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$AGS
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Gebietsname <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Gemeindename
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Waehler
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$GueltigeStimmen

baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$CDU_abs
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$SPD_abs
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_DIELINKE <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$DIELINKE_abs
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Gruene_abs
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_AfD <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$FDP_abs
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$CDU_sitze
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$SPD_sitze
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_DIELINKE <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$DIELINKE_sitze
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Gruene_sitze
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_AfD <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$FDP_sitze
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_1999_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_1999_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2004 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_2004_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_2004.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_2004_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_2004_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2004_gemeinderatswahlen_data

names(baden_wuerttemberg_2004_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[, election_year := "2004"]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$AGS_8dig <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$AGS
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Gebietsname <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Gemeindename
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Waehler
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$GueltigeStimmen

baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$CDU_abs
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$SPD_abs
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_DIELINKE <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$DIELINKE_abs
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Gruene_abs
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_AfD <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$FDP_abs
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$CDU_sitze
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$SPD_sitze
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_DIELINKE <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$DIELINKE_sitze
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Gruene_sitze
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_AfD <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$FDP_sitze
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2004_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2004_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2009 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2009_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_2009.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_2009_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_2009_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2009_gemeinderatswahlen_data

names(baden_wuerttemberg_2009_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[, election_year := "2009"]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$AGS_8dig <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$AGS
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Gebietsname <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Gemeindename
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Waehler
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$GueltigeStimmen

baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$CDU_abs
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$SPD_abs
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_DIELINKE <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$DIELINKE_abs
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Gruene_abs
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_AfD <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$FDP_abs
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$CDU_sitze
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$SPD_sitze
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_DIELINKE <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$DIELINKE_sitze
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Gruene_sitze
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_AfD <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$FDP_sitze
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2009_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2009_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2014 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2014_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_2014.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_2014_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_2014_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2014_gemeinderatswahlen_data

names(baden_wuerttemberg_2014_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[, election_year := "2014"]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$AGS_8dig <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$AGS
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Gebietsname <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Gemeindename
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Waehler
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$GueltigeStimmen

baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$CDU_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$SPD_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_DIELINKE <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$DIELINKE_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Gruene_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_AfD <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$AfD_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_PIRATEN <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$PIRATEN_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$FDP_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_DiePARTEI <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$DIEPARTEI_abs
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$CDU_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$SPD_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_DIELINKE <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$DIELINKE_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Gruene_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_AfD <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$AfD_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_PIRATEN <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$PIRATEN_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$FDP_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$DIEPARTEI_sitze
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2014_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2014_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2019 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2019_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/baden_wuerttemberg/baden_wuerttemberg_2019.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(baden_wuerttemberg_2019_gemeinderatswahlen_data) <- str_replace_all(
  names(baden_wuerttemberg_2019_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2019_gemeinderatswahlen_data

names(baden_wuerttemberg_2019_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[,
  Bundesland := "Baden-Wuerttemberg"
]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[, Gebietsname := ""]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[, election_year := "2019"]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[, IDIRB := ""]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$AGS_8dig <- parse_number(
  baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Gemeinde
)
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Gebietsname <- gsub(
  '[0-9]+',
  '',
  baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Gemeinde
)
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wahlberechtigte
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wähler <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wähler
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$GültigeStimmen <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_insgesamt

baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_CDU <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_CDU
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_SPD <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_SPD
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_DIELINKE <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_DIE_LINKE
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_GRÜNE <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_GRÜNE
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_AfD <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_AfD
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_PIRATEN <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_PIRATEN
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_FDP <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_FDP
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_DiePARTEI <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Stimmen_Die_PARTEI
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_CDU <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_CDU
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_SPD <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_SPD
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_DIELINKE <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_DIE_LINKE
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_GRÜNE <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_GRÜNE
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_AfD <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_AfD
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_PIRATEN <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_PIRATEN
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_FDP <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_FDP
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Sitze_Die_PARTEI
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2019_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2019_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Baden-Wuerttemberg 2024 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2024_gemeinderatswahlen_data <- as.data.table(read_delim(
  "raw/baden_wuerttemberg/baden_wuerttemberg_2024.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
)) |>
  bind_rows(
    # add seat data
    as.data.table(read_delim(
      "raw/baden_wuerttemberg/baden_wuerttemberg_2024_seats.csv",
      delim = ";",
      locale = locale(encoding = "ISO-8859-1")
    )) |>
      # no distinction by gender
      filter(`3_variable_attribute_label` == "Insgesamt"),

    #add turnout data
    as.data.table(read_delim(
      "raw/baden_wuerttemberg/baden_wuerttemberg_2024_turnout.csv",
      delim = ";",
      locale = locale(encoding = "ISO-8859-1")
    )) |>
      filter(value_variable_label %in% c("Wahlberechtigte", "Wähler/-innen"))
  )


#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2024_gemeinderatswahlen_data |>

  # clean ID and party variables
  mutate(
    Gebietsname = `1_variable_attribute_label`,
    AGS_8dig = `1_variable_attribute_code` |>
      str_remove("GEMI") |>

      # for compatibility with preexisting code (adds NUTS1 code after combining years)
      str_remove("^08"),
    value = as.numeric(value),
    newnames = case_when(
      `2_variable_attribute_code` == "GTW-AFD" ~ "AfD",
      `2_variable_attribute_code` == "GTW-GRUENE" ~ "GRÜNE",
      `2_variable_attribute_code` == "GTW-DIE-LINKE" ~ "DIELINKE",
      `2_variable_attribute_code` == "GTW-DIE-PARTEI" ~ "DiePARTEI",
      `2_variable_attribute_code` == "GTW-FDP" ~ "FDP",
      `2_variable_attribute_code` == "GTW-FREIWAEHLER" ~ "FREIEWÄHLER",
      `2_variable_attribute_code` == "GTW-CDU" ~ "CDU",
      `2_variable_attribute_code` == "GTW-SPD" ~ "SPD",
      `2_variable_attribute_code` == "GTW-PIRATEN" ~ "PIRATEN",
      `2_variable_attribute_label` == "Insgesamt" ~ "Insgesamt",

      value_variable_label == "Wahlberechtigte" ~ "Wahlberechtigteinsgesamt",
      value_variable_label == "Wähler/-innen" ~ "Wähler",

      T ~ "other"
    ),

    newnames = case_when(
      value_variable_label == "Gültige Stimmen" ~ paste0("abs_", newnames),
      value_variable_label == "Sitze" ~ paste0("sitze_", newnames),
      T ~ newnames
    )
  ) |>

  filter(
    `1_variable_code` == "GEMI", # only municipal level data
    value_unit == "Anzahl",
    value_variable_label %in%
      c("Gültige Stimmen", "Sitze", "Wahlberechtigte", "Wähler/-innen"),
    !is.na(value)
  ) |> # only absolute votes

  # pivot to wide
  pivot_wider(
    id_cols = c("Gebietsname", "AGS_8dig"),
    values_from = "value",
    names_from = "newnames",
    values_fn = ~ sum(.x, na.rm = T) # (for summarising "other")
  ) |>

  # Creating non-existing variables ----
  mutate(
    Bundesland = "Baden-Wuerttemberg",
    election_year = 2024,
    election_type = "Kommunalwahlen",
    IDIRB = "",
    IDBA = ""
  ) |>

  # Renaming existing variables ----
  rename(
    GültigeStimmen = abs_Insgesamt,
  ) |>
  as.data.table()


baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_CDU <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_SPD <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_AfD <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_FDP <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2024_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2024_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2024_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$Wähler /
  baden_wuerttemberg_2024_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Baden-Wuerttemberg ----
# Merge
baden_wuerttemberg_kommunalwahlen <- rbind(
  baden_wuerttemberg_1989_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_1994_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_1999_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_2004_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_2009_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_2014_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_2019_gemeinderatswahlen_data_sub,
  baden_wuerttemberg_2024_gemeinderatswahlen_data_sub
)

# Replace INF at Turnout
baden_wuerttemberg_kommunalwahlen$Turnout <- str_replace_all(
  baden_wuerttemberg_kommunalwahlen$Turnout,
  fixed("Inf"),
  NA
)

# Replace - with NA
baden_wuerttemberg_kommunalwahlen[
  baden_wuerttemberg_kommunalwahlen == "-"
] <- NA

# Fix AGS
baden_wuerttemberg_kommunalwahlen$AGS_8dig <- paste(
  "08",
  baden_wuerttemberg_kommunalwahlen$AGS_8dig,
  sep = ""
)

# Save
#write_csv(baden_wuerttemberg_kommunalwahlen, here::here("output/baden_wuerttemberg_kommunalwahlen.csv"))

######## MECKLENBURG-VORPOMMERN
######### MECKLENBURG-VORPOMMERN ----

###### Mecklenburg-Vorpommern 1994 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_1994_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_1994.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_1994_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_1994_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe and filter Landkreise ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1994_kommunalwahlen_data %>%
  filter(
    !grepl("Landkreis", Gemeindename)
  )

names(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[, election_year := "1994"]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Waehlerinsgesamt
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Stimmengültig
)

mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$CDU
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$SPD
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$PDS
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$`F.D.P.`
)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1994_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 1999 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_1999_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_1999.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_1999_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_1999_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1999_kommunalwahlen_data

names(mecklenburg_vorpommern_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[, election_year := "1999"]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[,
  election_type := "kommunalwahlen"
]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Stimmengueltig

mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$CDU
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$SPD
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$PDS
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$GRÜNE
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$`F.D.P.`
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1999_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_1999_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2004 kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2004_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2004.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_2004_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2004_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2004_kommunalwahlen_data

names(mecklenburg_vorpommern_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[, election_year := "2004"]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[,
  election_type := "kommunalwahlen"
]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gueltigeStimmen

mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$CDU
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$SPD
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$PDS
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$GRÜNE
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$FDP
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2004_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2004_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2009 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2009_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2009.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_2009_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2009_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2009_kommunalwahlen_data

names(mecklenburg_vorpommern_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[, election_year := "2009"]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[,
  election_type := "kommunalwahlen"
]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gueltigeStimmen

mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$CDU
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$SPD
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$DIELINKE
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$GRÜNE
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$FDP
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2009_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2009_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt


# Mecklenburg-Vorpommern Cities 2009 ----------------------------------------

mecklenburg_vorpommern_2009_staedte <- read_excel(
  "processed/mecklenburg_vorpommern_2009_staedte.xlsx"
)

mecklenburg_vorpommern_2009_staedte <- mecklenburg_vorpommern_2009_staedte %>%
  mutate(election_year = "2009", election_type = "Kommunalwahlen") %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2009_staedte$Turnout <- as.numeric(
  mecklenburg_vorpommern_2009_staedte$Wähler
) /
  as.numeric(mecklenburg_vorpommern_2009_staedte$Wahlberechtigteinsgesamt)


###### Mecklenburg-Vorpommern 2014 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2014_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2014.xlsx",
  sheet = "summary"
))

mecklenburg_vorpommern_2014_kommunalwahlen_data[
  mecklenburg_vorpommern_2014_kommunalwahlen_data == "x"
] <- NA

#### Delete white space ----
names(mecklenburg_vorpommern_2014_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2014_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2014_kommunalwahlen_data

names(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[, election_year := "2014"]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wahlberechtigte
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wähler
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Stimmengueltig

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$CDU
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$SPD
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$FDP
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_CDU
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_SPD
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_DIELINKE
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_GRÜNE
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_FDP
)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2019 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2019_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2019.xlsx",
  sheet = "summary"
))

mecklenburg_vorpommern_2019_kommunalwahlen_data[
  mecklenburg_vorpommern_2019_kommunalwahlen_data == "x"
] <- NA

#### Delete white space ----
names(mecklenburg_vorpommern_2019_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2019_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data

names(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, election_year := "2019"]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigte
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Waehler
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gültig

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$CDU
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$SPD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$AfD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$FDP
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_CDU
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_SPD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DIELINKE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_GRÜNE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_AfD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_AfD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FDP
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Remove Mecklenburgische ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub %>%
  filter(
    !AGS_8dig == "13071000",
    !AGS_8dig == "13076000",
    !AGS_8dig == "13072000",
    !AGS_8dig == "13073000",
    !AGS_8dig == "13074000",
    !AGS_8dig == "13075000"
  )


###### Mecklenburg-Vorpommern 2019 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2019_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2019.xlsx",
  sheet = "summary"
))

mecklenburg_vorpommern_2019_kommunalwahlen_data[
  mecklenburg_vorpommern_2019_kommunalwahlen_data == "x"
] <- NA

#### Delete white space ----
names(mecklenburg_vorpommern_2019_kommunalwahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2019_kommunalwahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data

names(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, election_year := "2019"]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigte
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Waehler
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gültig

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$CDU
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$SPD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_AfD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$AfD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$FDP
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_CDU
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_SPD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DIELINKE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_GRÜNE
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_AfD <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_AfD
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FDP
)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Remove Mecklenburgische ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub %>%
  filter(
    !AGS_8dig == "13071000",
    !AGS_8dig == "13076000",
    !AGS_8dig == "13072000",
    !AGS_8dig == "13073000",
    !AGS_8dig == "13074000",
    !AGS_8dig == "13075000"
  )


###### Mecklenburg-Vorpommern 2024 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2024_kommunalwahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2024.xlsx",
  sheet = "3. Ergebnisse nach Gemeinden",
  skip = 3
))

mecklenburg_vorpommern_2024_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2024_kommunalwahlen_data |>
  rename_with(str_squish) |>
  fill(`Gemeinde-schlüssel`, `Landkreis Gemeinde`) |>

  # add krfr Städte
  bind_rows(
    as.data.table(read_excel(
      "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2024.xlsx",
      sheet = "3.3 Ergebnisse nach Wahlgeb.",
      skip = 6,
      col_types = "text"
    )) |>
      rename_with(str_squish) |>
      mutate(`Wahl-jahr` = as.numeric(`Wahl-jahr`)) |>
      fill(`Wahlgebiet Land`) |>
      filter(str_detect(
        `Wahlgebiet Land`,
        "Rostock, Hanse|Schwerin, Landes"
      )) |>

      left_join(
        as.data.table(read_excel(
          "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2024.xlsx",
          sheet = "3.4 Ergebnisse nach Sitzen",
          skip = 4,
          col_types = "text"
        )) |>
          rename_with(str_squish) |>
          mutate(`Wahljahr` = as.numeric(`Wahljahr`)) |>
          fill(`Wahlgebiet Land`),
        by = c("Wahlgebiet Land", "Wahl-jahr" = "Wahljahr")
      ) |>
      rename(`Landkreis Gemeinde` = `Wahlgebiet Land`)
  ) |>

  filter(`Wahl-jahr` == 2024, `Maß- einheit` %in% c("Anzahl", "Mandate")) |>
  pivot_longer(
    names_to = "newnames",
    values_to = "value",
    cols = matches("Gültige Stimmen|Wahlb|Wähl|Stimm|Sitze")
  ) |>

  mutate(
    value = as.numeric(value),

    `Maß- einheit` = if_else(
      str_detect(newnames, "Sitze"),
      "Mandate",
      `Maß- einheit`
    ),

    newnames = str_remove(newnames, "Gültige Stimmen |Sitze "),

    newnames = case_when(
      `newnames` == "DIE LINKE" ~ "DIELINKE",
      `newnames` == "Stimmen gültig" ~ "GültigeStimmen",
      `newnames` == "Stimmen ungültig" ~ "UngültigeStimmen",
      `newnames` %in% c("Wahlbe-rechtigte", "Wahl- berechtigte") ~
        "Wahlberechtigteinsgesamt",
      `newnames` %in% c("Wähler-innen und Wähler", "Wählerinnen und Wähler") ~
        "Wähler",
      `newnames` %in%
        c(
          "Sonstige",
          "Einzelbe- werber/in",
          "Wähler- gruppen",
          "BSW",
          "Einzel- bewerber/in und Sonstige"
        ) ~
        "other",

      T ~ newnames
    ),

    newnames = case_when(
      `newnames` %in%
        c(
          "GültigeStimmen",
          "UngültigeStimmen",
          "Wahlberechtigteinsgesamt",
          "Wähler"
        ) ~
        newnames,

      `Maß- einheit` == "Anzahl" ~ paste0("abs_", newnames),
      `Maß- einheit` == "Mandate" ~ paste0("sitze_", newnames)
    ),

    `Gemeinde-schlüssel` = case_when(
      str_detect(`Landkreis Gemeinde`, "Rostock, Hanse") ~ "13003000",
      str_detect(`Landkreis Gemeinde`, "Schwerin, Landes") ~ "13004000",
      T ~ as.character(`Gemeinde-schlüssel`)
    )
  ) |>

  filter(
    !str_detect(`Gemeinde-schlüssel`, "000$") |
      `Gemeinde-schlüssel` %in% c("13003000", "13004000"), # only municipal level data
    `Landkreis Gemeinde` != "Mecklenburg-Vorpommern",
    !is.na(value)
  ) |>

  rename(
    "Gebietsname" = "Landkreis Gemeinde",
    "AGS_8dig" = "Gemeinde-schlüssel",
    "election_year" = "Wahl-jahr"
  ) |>

  # pivot to wide
  pivot_wider(
    id_cols = c("Gebietsname", "AGS_8dig", "election_year"),
    values_from = "value",
    names_from = "newnames",
    values_fn = ~ sum(.x, na.rm = T) # (for summarising "other")
  ) |>

  # Creating non-existing variables ----
  mutate(
    Bundesland = "Mecklenburg-Vorpommern",
    election_year = 2024,
    election_type = "Kommunalwahlen",
    IDIRB = "",
    IDBA = ""
  ) |>
  as.data.table()

mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2024_kommunalwahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2024_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2024_kommunalwahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2024_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt


###### Mecklenburg-Vorpommern 1994 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_1994_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_1994.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_1994_kreiswahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_1994_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
mecklenburg_vorpommern_1994_kreiswahlen_data_sub <- mecklenburg_vorpommern_1994_kreiswahlen_data
# Creating non-existing variables ----
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[, election_year := "1994"]
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_1994_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Wahlberechtigteinsgesamt
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Waehlerinsgesamt
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Stimmengültig
)

mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$PDS
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$`F.D.P.`
)
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$FreieWähler
)

mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_1994_kreiswahlen_data_sub <- mecklenburg_vorpommern_1994_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1994_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 1999 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_1999_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_1999.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_1999_kreiswahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_1999_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
mecklenburg_vorpommern_1999_kreiswahlen_data_sub <- mecklenburg_vorpommern_1999_kreiswahlen_data
# Creating non-existing variables ----
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[, election_year := "1999"]
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_1999_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Wahlberechtigteinsgesamt
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Waehlerinsgesamt
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Stimmengueltig
)

mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$PDS
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$`F.D.P.`
)
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$FreieWähler
)

mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_1999_kreiswahlen_data_sub <- mecklenburg_vorpommern_1999_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1999_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2004 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2004_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2004.xls",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_2004_kreiswahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2004_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
mecklenburg_vorpommern_2004_kreiswahlen_data_sub <- mecklenburg_vorpommern_2004_kreiswahlen_data

# Creating non-existing variables ----
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[, election_year := "2004"]
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2004_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Wahlberechtigteinsgesamt
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Waehlerinsgesamt
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$GueltigeStimmen
)

mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$PDS
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$FDP
)
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2004_kreiswahlen_data_sub <- mecklenburg_vorpommern_2004_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2004_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2009 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2009_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2009.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_2009_kreiswahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2009_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
mecklenburg_vorpommern_2009_kreiswahlen_data_sub <- mecklenburg_vorpommern_2009_kreiswahlen_data

# Creating non-existing variables ----
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[, election_year := "2009"]
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2009_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Wahlberechtigteinsgesamt
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Waehlerinsgesamt
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Stimmengueltig
)

mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$FDP
)
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2009_kreiswahlen_data_sub <- mecklenburg_vorpommern_2009_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2009_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2009_kreiswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2014 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2014_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2014.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_2014_kreiswahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2014_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
mecklenburg_vorpommern_2014_kreiswahlen_data_sub <- mecklenburg_vorpommern_2014_kreiswahlen_data %>%
  group_by(Gemeindenummer, Gemeindename) %>%
  summarize(
    Wahlberechtigteinsgesamt = sum(Wahlberechtigteinsgesamt, na.rm = T),
    Waehlerinsgesamt = sum(Waehlerinsgesamt, na.rm = T),
    gueltigeStimmen = sum(gueltigeStimmen, na.rm = T),
    CDU = sum(CDU, na.rm = T),
    SPD = sum(SPD, na.rm = T),
    DIELINKE = sum(DIELINKE, na.rm = T),
    GRÜNE = sum(GRÜNE, na.rm = T),
    FDP = sum(FDP, na.rm = T),
    AfD = sum(AfD, na.rm = T)
  ) %>%
  ungroup()

mecklenburg_vorpommern_2014_kreiswahlen_data_sub <- as.data.table(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub
)

# Creating non-existing variables ----
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[, election_year := "2014"]
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2014_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Wahlberechtigteinsgesamt
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Waehlerinsgesamt
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gueltigeStimmen
)

mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_AfD <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$AfD
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$FDP
)
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2014_kreiswahlen_data_sub <- mecklenburg_vorpommern_2014_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2014_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2019 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2019_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2019.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(mecklenburg_vorpommern_2019_kreiswahlen_data) <- str_replace_all(
  names(mecklenburg_vorpommern_2019_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub <- mecklenburg_vorpommern_2019_kreiswahlen_data

# Creating non-existing variables ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, election_year := "2019"]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wahlberechtigte
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wähler
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$GültigeStimmen
)

mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_AfD <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$AfD
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_PIRATEN <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$PIRATEN
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$FDP
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_DiePARTEI <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$DiePARTEI
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$FREIEWÄHLER
)

mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2019_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wahlberechtigteinsgesamt


###### Mecklenburg-Vorpommern 2024 Kreiswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2024_kreiswahlen_data <- as.data.table(read_excel(
  "raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kreistagswahlen_2024.xlsx",
  sheet = "3.3 Ergebnisse nach Wahlgeb.",
  skip = 6
)) |>
  rename_with(str_squish)


#### Recoding ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub <- mecklenburg_vorpommern_2019_kreiswahlen_data

# Creating non-existing variables ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[,
  Bundesland := "Mecklenburg-Vorpommern"
]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, Gebietsname := ""]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, election_year := "2019"]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, IDIRB := ""]
mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wahlberechtigte
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wähler <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wähler
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$GültigeStimmen <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$GültigeStimmen
)

mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$CDU
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$SPD
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$DIELINKE
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$GRÜNE
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_AfD <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$AfD
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_PIRATEN <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$PIRATEN
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$FDP
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_DiePARTEI <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$DiePARTEI
)
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$FREIEWÄHLER
)

mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2019_kreiswahlen_data_sub <-
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wähler /
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Mecklenburg-Vorpommern ----
# Merge
mecklenburg_vorpommern_kommunalwahlen <- rbind(
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_1999_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_2004_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_2009_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_2024_kommunalwahlen_data_sub,
  mecklenburg_vorpommern_1994_kreiswahlen_data_sub,
  mecklenburg_vorpommern_1999_kreiswahlen_data_sub,
  mecklenburg_vorpommern_2004_kreiswahlen_data_sub,
  mecklenburg_vorpommern_2014_kreiswahlen_data_sub,
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub,
  mecklenburg_vorpommern_2019_kreiswahlen_data_sub
)


mecklenburg_vorpommern_kommunalwahlen <- mecklenburg_vorpommern_kommunalwahlen |>
  bind_rows(
    mecklenburg_vorpommern_2009_staedte |>
      mutate(AGS_8dig = as.character(AGS_8dig))
  )

# Replace INF at Turnout
mecklenburg_vorpommern_kommunalwahlen$Turnout <- str_replace_all(
  mecklenburg_vorpommern_kommunalwahlen$Turnout,
  fixed("Inf"),
  NA
)

# Replace - with NA
mecklenburg_vorpommern_kommunalwahlen[
  mecklenburg_vorpommern_kommunalwahlen == "-"
] <- NA

# Fix AGS
mecklenburg_vorpommern_kommunalwahlen$AGS_8dig <- stri_pad_left(
  mecklenburg_vorpommern_kommunalwahlen$AGS_8dig,
  8,
  0
)


# Save
#write_csv(mecklenburg_vorpommern_kommunalwahlen, here::here("output/mecklenburg_vorpommern_kommunalwahlen.csv"))

# ----
######### HESSEN ----
###### Hessen 1989 Gemeinderatswahl ----
#### Load election data ----

hessen_1989_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "1989"
))

#### Delete white space ----
names(hessen_1989_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_1989_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_1989_gemeinderatswahl_data_sub <- hessen_1989_gemeinderatswahl_data

names(hessen_1989_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_1989_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_1989_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_1989_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_1989_gemeinderatswahl_data_sub[, election_year := "1989"]
hessen_1989_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_1989_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_1989_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_1989_gemeinderatswahl_data_sub$AGS_8dig <- hessen_1989_gemeinderatswahl_data_sub$AGS
hessen_1989_gemeinderatswahl_data_sub$Gebietsname <- hessen_1989_gemeinderatswahl_data_sub$Name
hessen_1989_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_1989_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_1989_gemeinderatswahl_data_sub$Wähler <- hessen_1989_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_1989_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_1989_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_1989_gemeinderatswahl_data_sub$abs_CDU <- hessen_1989_gemeinderatswahl_data_sub$CDU
hessen_1989_gemeinderatswahl_data_sub$abs_SPD <- hessen_1989_gemeinderatswahl_data_sub$SPD
hessen_1989_gemeinderatswahl_data_sub$abs_DIELINKE <- NA
hessen_1989_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_1989_gemeinderatswahl_data_sub$GRÜNE
hessen_1989_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_1989_gemeinderatswahl_data_sub$abs_PIRATEN <- NA
hessen_1989_gemeinderatswahl_data_sub$abs_FDP <- hessen_1989_gemeinderatswahl_data_sub$FDP
hessen_1989_gemeinderatswahl_data_sub$abs_DiePARTEI <- NA
hessen_1989_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- NA

hessen_1989_gemeinderatswahl_data_sub$gew_CDU <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_SPD <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_DIELINKE <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_GRÜNE <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_PIRATEN <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_FDP <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_DiePARTEI <- NA
hessen_1989_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- NA

hessen_1989_gemeinderatswahl_data_sub$sitze_CDU <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_SPD <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_DIELINKE <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_GRÜNE <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_PIRATEN <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_FDP <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_DiePARTEI <- NA
hessen_1989_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hessen_1989_gemeinderatswahl_data_sub <- hessen_1989_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_1989_gemeinderatswahl_data_sub <-
  hessen_1989_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_1989_gemeinderatswahl_data_sub$Turnout <- hessen_1989_gemeinderatswahl_data_sub$Wähler /
  hessen_1989_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 1993 Gemeinderatswahl ----
#### Load election data ----

hessen_1993_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "1993"
))

#### Delete white space ----
names(hessen_1993_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_1993_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_1993_gemeinderatswahl_data_sub <- hessen_1993_gemeinderatswahl_data

names(hessen_1993_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_1993_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_1993_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_1993_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_1993_gemeinderatswahl_data_sub[, election_year := "1993"]
hessen_1993_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_1993_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_1993_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_1993_gemeinderatswahl_data_sub$AGS_8dig <- hessen_1993_gemeinderatswahl_data_sub$AGS
hessen_1993_gemeinderatswahl_data_sub$Gebietsname <- hessen_1993_gemeinderatswahl_data_sub$Name
hessen_1993_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_1993_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_1993_gemeinderatswahl_data_sub$Wähler <- hessen_1993_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_1993_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_1993_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_1993_gemeinderatswahl_data_sub$abs_CDU <- hessen_1993_gemeinderatswahl_data_sub$CDU
hessen_1993_gemeinderatswahl_data_sub$abs_SPD <- hessen_1993_gemeinderatswahl_data_sub$SPD
hessen_1993_gemeinderatswahl_data_sub$abs_DIELINKE <- NA
hessen_1993_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_1993_gemeinderatswahl_data_sub$Gruene
hessen_1993_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_1993_gemeinderatswahl_data_sub$abs_PIRATEN <- NA
hessen_1993_gemeinderatswahl_data_sub$abs_FDP <- hessen_1993_gemeinderatswahl_data_sub$FDP
hessen_1993_gemeinderatswahl_data_sub$abs_DiePARTEI <- NA
hessen_1993_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- NA

hessen_1993_gemeinderatswahl_data_sub$gew_CDU <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_SPD <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_DIELINKE <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_GRÜNE <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_PIRATEN <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_FDP <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_DiePARTEI <- NA
hessen_1993_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- NA

hessen_1993_gemeinderatswahl_data_sub$sitze_CDU <- hessen_1993_gemeinderatswahl_data_sub$CDU_sitze
hessen_1993_gemeinderatswahl_data_sub$sitze_SPD <- hessen_1993_gemeinderatswahl_data_sub$SPD_sitze
hessen_1993_gemeinderatswahl_data_sub$sitze_DIELINKE <- NA
hessen_1993_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_1993_gemeinderatswahl_data_sub$Gruene_sitze
hessen_1993_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_1993_gemeinderatswahl_data_sub$sitze_PIRATEN <- NA
hessen_1993_gemeinderatswahl_data_sub$sitze_FDP <- hessen_1993_gemeinderatswahl_data_sub$FDP_sitze
hessen_1993_gemeinderatswahl_data_sub$sitze_DiePARTEI <- NA
hessen_1993_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hessen_1993_gemeinderatswahl_data_sub <- hessen_1993_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_1993_gemeinderatswahl_data_sub <-
  hessen_1993_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_1993_gemeinderatswahl_data_sub$Turnout <- hessen_1993_gemeinderatswahl_data_sub$Wähler /
  hessen_1993_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 1997 Gemeinderatswahl ----
#### Load election data ----

hessen_1997_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "1997"
))

#### Delete white space ----
names(hessen_1997_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_1997_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_1997_gemeinderatswahl_data_sub <- hessen_1997_gemeinderatswahl_data

names(hessen_1997_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_1997_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_1997_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_1997_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_1997_gemeinderatswahl_data_sub[, election_year := "1997"]
hessen_1997_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_1997_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_1997_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_1997_gemeinderatswahl_data_sub$AGS_8dig <- hessen_1997_gemeinderatswahl_data_sub$AGS
hessen_1997_gemeinderatswahl_data_sub$Gebietsname <- hessen_1997_gemeinderatswahl_data_sub$Name
hessen_1997_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_1997_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_1997_gemeinderatswahl_data_sub$Wähler <- hessen_1997_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_1997_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_1997_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_1997_gemeinderatswahl_data_sub$abs_CDU <- hessen_1997_gemeinderatswahl_data_sub$CDU
hessen_1997_gemeinderatswahl_data_sub$abs_SPD <- hessen_1997_gemeinderatswahl_data_sub$SPD
hessen_1997_gemeinderatswahl_data_sub$abs_DIELINKE <- NA
hessen_1997_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_1997_gemeinderatswahl_data_sub$Gruene
hessen_1997_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_1997_gemeinderatswahl_data_sub$abs_PIRATEN <- NA
hessen_1997_gemeinderatswahl_data_sub$abs_FDP <- hessen_1997_gemeinderatswahl_data_sub$FDP
hessen_1997_gemeinderatswahl_data_sub$abs_DiePARTEI <- NA
hessen_1997_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- NA

hessen_1997_gemeinderatswahl_data_sub$gew_CDU <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_SPD <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_DIELINKE <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_GRÜNE <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_PIRATEN <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_FDP <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_DiePARTEI <- NA
hessen_1997_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- NA

hessen_1997_gemeinderatswahl_data_sub$sitze_CDU <- hessen_1997_gemeinderatswahl_data_sub$CDU_sitze
hessen_1997_gemeinderatswahl_data_sub$sitze_SPD <- hessen_1997_gemeinderatswahl_data_sub$SPD_sitze
hessen_1997_gemeinderatswahl_data_sub$sitze_DIELINKE <- NA
hessen_1997_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_1997_gemeinderatswahl_data_sub$Gruene_sitze
hessen_1997_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_1997_gemeinderatswahl_data_sub$sitze_PIRATEN <- NA
hessen_1997_gemeinderatswahl_data_sub$sitze_FDP <- hessen_1997_gemeinderatswahl_data_sub$FDP_sitze
hessen_1997_gemeinderatswahl_data_sub$sitze_DiePARTEI <- NA
hessen_1997_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hessen_1997_gemeinderatswahl_data_sub <- hessen_1997_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_1997_gemeinderatswahl_data_sub <-
  hessen_1997_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_1997_gemeinderatswahl_data_sub$Turnout <- hessen_1997_gemeinderatswahl_data_sub$Wähler /
  hessen_1997_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2001 Gemeinderatswahl ----
#### Load election data ----

hessen_2001_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "2001"
))

#### Delete white space ----
names(hessen_2001_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_2001_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_2001_gemeinderatswahl_data_sub <- hessen_2001_gemeinderatswahl_data

names(hessen_2001_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2001_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_2001_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_2001_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_2001_gemeinderatswahl_data_sub[, election_year := "2001"]
hessen_2001_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_2001_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_2001_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_2001_gemeinderatswahl_data_sub$AGS_8dig <- hessen_2001_gemeinderatswahl_data_sub$AGS
hessen_2001_gemeinderatswahl_data_sub$Gebietsname <- hessen_2001_gemeinderatswahl_data_sub$Name
hessen_2001_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_2001_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_2001_gemeinderatswahl_data_sub$Wähler <- hessen_2001_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_2001_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_2001_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_2001_gemeinderatswahl_data_sub$abs_CDU <- hessen_2001_gemeinderatswahl_data_sub$CDU
hessen_2001_gemeinderatswahl_data_sub$abs_SPD <- hessen_2001_gemeinderatswahl_data_sub$SPD
hessen_2001_gemeinderatswahl_data_sub$abs_DIELINKE <- NA
hessen_2001_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_2001_gemeinderatswahl_data_sub$Gruene
hessen_2001_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_2001_gemeinderatswahl_data_sub$abs_PIRATEN <- NA
hessen_2001_gemeinderatswahl_data_sub$abs_FDP <- hessen_2001_gemeinderatswahl_data_sub$FDP
hessen_2001_gemeinderatswahl_data_sub$abs_DiePARTEI <- NA
hessen_2001_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- NA

hessen_2001_gemeinderatswahl_data_sub$gew_CDU <- hessen_2001_gemeinderatswahl_data_sub$CDU_gew
hessen_2001_gemeinderatswahl_data_sub$gew_SPD <- hessen_2001_gemeinderatswahl_data_sub$SPD_gew
hessen_2001_gemeinderatswahl_data_sub$gew_DIELINKE <- NA
hessen_2001_gemeinderatswahl_data_sub$gew_GRÜNE <- hessen_2001_gemeinderatswahl_data_sub$Gruene_gew
hessen_2001_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_2001_gemeinderatswahl_data_sub$gew_PIRATEN <- NA
hessen_2001_gemeinderatswahl_data_sub$gew_FDP <- hessen_2001_gemeinderatswahl_data_sub$FDP_gew
hessen_2001_gemeinderatswahl_data_sub$gew_DiePARTEI <- NA
hessen_2001_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- NA

hessen_2001_gemeinderatswahl_data_sub$sitze_CDU <- hessen_2001_gemeinderatswahl_data_sub$CDU_sitze
hessen_2001_gemeinderatswahl_data_sub$sitze_SPD <- hessen_2001_gemeinderatswahl_data_sub$SPD_sitze
hessen_2001_gemeinderatswahl_data_sub$sitze_DIELINKE <- NA
hessen_2001_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_2001_gemeinderatswahl_data_sub$Gruene_sitze
hessen_2001_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_2001_gemeinderatswahl_data_sub$sitze_PIRATEN <- NA
hessen_2001_gemeinderatswahl_data_sub$sitze_FDP <- hessen_2001_gemeinderatswahl_data_sub$FDP_sitze
hessen_2001_gemeinderatswahl_data_sub$sitze_DiePARTEI <- NA
hessen_2001_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
hessen_2001_gemeinderatswahl_data_sub <- hessen_2001_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2001_gemeinderatswahl_data_sub <-
  hessen_2001_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_2001_gemeinderatswahl_data_sub$Turnout <- hessen_2001_gemeinderatswahl_data_sub$Wähler /
  hessen_2001_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2006 Gemeinderatswahl ----
#### Load election data ----

hessen_2006_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "2006"
))

#### Delete white space ----
names(hessen_2006_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_2006_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data

names(hessen_2006_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2006_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_2006_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_2006_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_2006_gemeinderatswahl_data_sub[, election_year := "2006"]
hessen_2006_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_2006_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_2006_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_2006_gemeinderatswahl_data_sub$AGS_8dig <- hessen_2006_gemeinderatswahl_data_sub$AGS
hessen_2006_gemeinderatswahl_data_sub$Gebietsname <- hessen_2006_gemeinderatswahl_data_sub$Name
hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_2006_gemeinderatswahl_data_sub$Wähler <- hessen_2006_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_2006_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_2006_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_2006_gemeinderatswahl_data_sub$abs_CDU <- hessen_2006_gemeinderatswahl_data_sub$CDU
hessen_2006_gemeinderatswahl_data_sub$abs_SPD <- hessen_2006_gemeinderatswahl_data_sub$SPD
hessen_2006_gemeinderatswahl_data_sub$abs_DIELINKE <- hessen_2006_gemeinderatswahl_data_sub$DIELINKE
hessen_2006_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_2006_gemeinderatswahl_data_sub$Gruene
hessen_2006_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_2006_gemeinderatswahl_data_sub$abs_PIRATEN <- NA
hessen_2006_gemeinderatswahl_data_sub$abs_FDP <- hessen_2006_gemeinderatswahl_data_sub$FDP
hessen_2006_gemeinderatswahl_data_sub$abs_DiePARTEI <- NA
hessen_2006_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- hessen_2006_gemeinderatswahl_data_sub$FreieWaehler

hessen_2006_gemeinderatswahl_data_sub$gew_CDU <- hessen_2006_gemeinderatswahl_data_sub$CDU_gew
hessen_2006_gemeinderatswahl_data_sub$gew_SPD <- hessen_2006_gemeinderatswahl_data_sub$SPD_gew
hessen_2006_gemeinderatswahl_data_sub$gew_DIELINKE <- hessen_2006_gemeinderatswahl_data_sub$DIELINKE_gew
hessen_2006_gemeinderatswahl_data_sub$gew_GRÜNE <- hessen_2006_gemeinderatswahl_data_sub$Gruene_gew
hessen_2006_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_2006_gemeinderatswahl_data_sub$gew_PIRATEN <- NA
hessen_2006_gemeinderatswahl_data_sub$gew_FDP <- hessen_2006_gemeinderatswahl_data_sub$FDP_gew
hessen_2006_gemeinderatswahl_data_sub$gew_DiePARTEI <- NA
hessen_2006_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- hessen_2006_gemeinderatswahl_data_sub$FreieWaehler_gew

hessen_2006_gemeinderatswahl_data_sub$sitze_CDU <- hessen_2006_gemeinderatswahl_data_sub$CDU_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_SPD <- hessen_2006_gemeinderatswahl_data_sub$SPD_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_DIELINKE <- hessen_2006_gemeinderatswahl_data_sub$DIELINKE_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_2006_gemeinderatswahl_data_sub$Gruene_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_2006_gemeinderatswahl_data_sub$sitze_PIRATEN <- NA
hessen_2006_gemeinderatswahl_data_sub$sitze_FDP <- hessen_2006_gemeinderatswahl_data_sub$FDP_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_DiePARTEI <- NA
hessen_2006_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- hessen_2006_gemeinderatswahl_data_sub$FreieWaehler_sitze

# Creating new dataframe with selected vars ----
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2006_gemeinderatswahl_data_sub <-
  hessen_2006_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_2006_gemeinderatswahl_data_sub$Turnout <- hessen_2006_gemeinderatswahl_data_sub$Wähler /
  hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2006 Gemeinderatswahl ----
#### Load election data ----

hessen_2006_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "2006"
))

#### Delete white space ----
names(hessen_2006_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_2006_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data

names(hessen_2006_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2006_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_2006_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_2006_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_2006_gemeinderatswahl_data_sub[, election_year := "2006"]
hessen_2006_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_2006_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_2006_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_2006_gemeinderatswahl_data_sub$AGS_8dig <- hessen_2006_gemeinderatswahl_data_sub$AGS
hessen_2006_gemeinderatswahl_data_sub$Gebietsname <- hessen_2006_gemeinderatswahl_data_sub$Name
hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_2006_gemeinderatswahl_data_sub$Wähler <- hessen_2006_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_2006_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_2006_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_2006_gemeinderatswahl_data_sub$abs_CDU <- hessen_2006_gemeinderatswahl_data_sub$CDU
hessen_2006_gemeinderatswahl_data_sub$abs_SPD <- hessen_2006_gemeinderatswahl_data_sub$SPD
hessen_2006_gemeinderatswahl_data_sub$abs_DIELINKE <- hessen_2006_gemeinderatswahl_data_sub$DIELINKE
hessen_2006_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_2006_gemeinderatswahl_data_sub$Gruene
hessen_2006_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_2006_gemeinderatswahl_data_sub$abs_PIRATEN <- NA
hessen_2006_gemeinderatswahl_data_sub$abs_FDP <- hessen_2006_gemeinderatswahl_data_sub$FDP
hessen_2006_gemeinderatswahl_data_sub$abs_DiePARTEI <- NA
hessen_2006_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- hessen_2006_gemeinderatswahl_data_sub$FreieWaehler

hessen_2006_gemeinderatswahl_data_sub$gew_CDU <- hessen_2006_gemeinderatswahl_data_sub$CDU_gew
hessen_2006_gemeinderatswahl_data_sub$gew_SPD <- hessen_2006_gemeinderatswahl_data_sub$SPD_gew
hessen_2006_gemeinderatswahl_data_sub$gew_DIELINKE <- hessen_2006_gemeinderatswahl_data_sub$DIELINKE_gew
hessen_2006_gemeinderatswahl_data_sub$gew_GRÜNE <- hessen_2006_gemeinderatswahl_data_sub$Gruene_gew
hessen_2006_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_2006_gemeinderatswahl_data_sub$gew_PIRATEN <- NA
hessen_2006_gemeinderatswahl_data_sub$gew_FDP <- hessen_2006_gemeinderatswahl_data_sub$FDP_gew
hessen_2006_gemeinderatswahl_data_sub$gew_DiePARTEI <- NA
hessen_2006_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- hessen_2006_gemeinderatswahl_data_sub$FreieWaehler_gew

hessen_2006_gemeinderatswahl_data_sub$sitze_CDU <- hessen_2006_gemeinderatswahl_data_sub$CDU_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_SPD <- hessen_2006_gemeinderatswahl_data_sub$SPD_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_DIELINKE <- hessen_2006_gemeinderatswahl_data_sub$DIELINKE_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_2006_gemeinderatswahl_data_sub$Gruene_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_2006_gemeinderatswahl_data_sub$sitze_PIRATEN <- NA
hessen_2006_gemeinderatswahl_data_sub$sitze_FDP <- hessen_2006_gemeinderatswahl_data_sub$FDP_sitze
hessen_2006_gemeinderatswahl_data_sub$sitze_DiePARTEI <- NA
hessen_2006_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- hessen_2006_gemeinderatswahl_data_sub$FreieWaehler_sitze

# Creating new dataframe with selected vars ----
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2006_gemeinderatswahl_data_sub <-
  hessen_2006_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_2006_gemeinderatswahl_data_sub$Turnout <- hessen_2006_gemeinderatswahl_data_sub$Wähler /
  hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2011 Gemeinderatswahl ----
#### Load election data ----

hessen_2011_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "2011"
))

#### Delete white space ----
names(hessen_2011_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_2011_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_2011_gemeinderatswahl_data_sub <- hessen_2011_gemeinderatswahl_data

names(hessen_2011_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2011_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_2011_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_2011_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_2011_gemeinderatswahl_data_sub[, election_year := "2011"]
hessen_2011_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_2011_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_2011_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_2011_gemeinderatswahl_data_sub$AGS_8dig <- hessen_2011_gemeinderatswahl_data_sub$AGS
hessen_2011_gemeinderatswahl_data_sub$Gebietsname <- hessen_2011_gemeinderatswahl_data_sub$Name
hessen_2011_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_2011_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_2011_gemeinderatswahl_data_sub$Wähler <- hessen_2011_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_2011_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_2011_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_2011_gemeinderatswahl_data_sub$abs_CDU <- hessen_2011_gemeinderatswahl_data_sub$CDU
hessen_2011_gemeinderatswahl_data_sub$abs_SPD <- hessen_2011_gemeinderatswahl_data_sub$SPD
hessen_2011_gemeinderatswahl_data_sub$abs_DIELINKE <- hessen_2011_gemeinderatswahl_data_sub$DIELINKE
hessen_2011_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_2011_gemeinderatswahl_data_sub$Gruene
hessen_2011_gemeinderatswahl_data_sub$abs_AfD <- NA
hessen_2011_gemeinderatswahl_data_sub$abs_PIRATEN <- hessen_2011_gemeinderatswahl_data_sub$PIRATEN
hessen_2011_gemeinderatswahl_data_sub$abs_FDP <- hessen_2011_gemeinderatswahl_data_sub$FDP
hessen_2011_gemeinderatswahl_data_sub$abs_DiePARTEI <- hessen_2011_gemeinderatswahl_data_sub$DiePartei
hessen_2011_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- hessen_2011_gemeinderatswahl_data_sub$FreieWaehler

hessen_2011_gemeinderatswahl_data_sub$gew_CDU <- hessen_2011_gemeinderatswahl_data_sub$CDU_gew
hessen_2011_gemeinderatswahl_data_sub$gew_SPD <- hessen_2011_gemeinderatswahl_data_sub$SPD_gew
hessen_2011_gemeinderatswahl_data_sub$gew_DIELINKE <- hessen_2011_gemeinderatswahl_data_sub$DIELINKE_gew
hessen_2011_gemeinderatswahl_data_sub$gew_GRÜNE <- hessen_2011_gemeinderatswahl_data_sub$Gruene_gew
hessen_2011_gemeinderatswahl_data_sub$gew_AfD <- NA
hessen_2011_gemeinderatswahl_data_sub$gew_PIRATEN <- hessen_2011_gemeinderatswahl_data_sub$PIRATEN_gew
hessen_2011_gemeinderatswahl_data_sub$gew_FDP <- hessen_2011_gemeinderatswahl_data_sub$FDP_gew
hessen_2011_gemeinderatswahl_data_sub$gew_DiePARTEI <- hessen_2011_gemeinderatswahl_data_sub$DiePartei_gew
hessen_2011_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- hessen_2011_gemeinderatswahl_data_sub$FreieWaehler_gew

hessen_2011_gemeinderatswahl_data_sub$sitze_CDU <- hessen_2011_gemeinderatswahl_data_sub$CDU_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_SPD <- hessen_2011_gemeinderatswahl_data_sub$SPD_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_DIELINKE <- hessen_2011_gemeinderatswahl_data_sub$DIELINKE_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_2011_gemeinderatswahl_data_sub$Gruene_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_AfD <- NA
hessen_2011_gemeinderatswahl_data_sub$sitze_PIRATEN <- hessen_2011_gemeinderatswahl_data_sub$PIRATEN_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_FDP <- hessen_2011_gemeinderatswahl_data_sub$FDP_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_DiePARTEI <- hessen_2011_gemeinderatswahl_data_sub$DiePartei_sitze
hessen_2011_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- hessen_2011_gemeinderatswahl_data_sub$FreieWaehler_sitze

# Creating new dataframe with selected vars ----
hessen_2011_gemeinderatswahl_data_sub <- hessen_2011_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2011_gemeinderatswahl_data_sub <-
  hessen_2011_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_2011_gemeinderatswahl_data_sub$Turnout <- hessen_2011_gemeinderatswahl_data_sub$Wähler /
  hessen_2011_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2016 Gemeinderatswahl ----
#### Load election data ----

hessen_2016_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "2016"
))

#### Delete white space ----
names(hessen_2016_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_2016_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_2016_gemeinderatswahl_data_sub <- hessen_2016_gemeinderatswahl_data

names(hessen_2016_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2016_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_2016_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_2016_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_2016_gemeinderatswahl_data_sub[, election_year := "2016"]
hessen_2016_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_2016_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_2016_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_2016_gemeinderatswahl_data_sub$AGS_8dig <- hessen_2016_gemeinderatswahl_data_sub$AGS
hessen_2016_gemeinderatswahl_data_sub$Gebietsname <- hessen_2016_gemeinderatswahl_data_sub$Name
hessen_2016_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_2016_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_2016_gemeinderatswahl_data_sub$Wähler <- hessen_2016_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_2016_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_2016_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_2016_gemeinderatswahl_data_sub$abs_CDU <- hessen_2016_gemeinderatswahl_data_sub$CDU
hessen_2016_gemeinderatswahl_data_sub$abs_SPD <- hessen_2016_gemeinderatswahl_data_sub$SPD
hessen_2016_gemeinderatswahl_data_sub$abs_DIELINKE <- hessen_2016_gemeinderatswahl_data_sub$DIELINKE
hessen_2016_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_2016_gemeinderatswahl_data_sub$Gruene
hessen_2016_gemeinderatswahl_data_sub$abs_AfD <- hessen_2016_gemeinderatswahl_data_sub$AfD
hessen_2016_gemeinderatswahl_data_sub$abs_PIRATEN <- hessen_2016_gemeinderatswahl_data_sub$PIRATEN
hessen_2016_gemeinderatswahl_data_sub$abs_FDP <- hessen_2016_gemeinderatswahl_data_sub$FDP
hessen_2016_gemeinderatswahl_data_sub$abs_DiePARTEI <- hessen_2016_gemeinderatswahl_data_sub$DiePartei
hessen_2016_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- hessen_2016_gemeinderatswahl_data_sub$FreieWaehler

hessen_2016_gemeinderatswahl_data_sub$gew_CDU <- hessen_2016_gemeinderatswahl_data_sub$CDU_gew
hessen_2016_gemeinderatswahl_data_sub$gew_SPD <- hessen_2016_gemeinderatswahl_data_sub$SPD_gew
hessen_2016_gemeinderatswahl_data_sub$gew_DIELINKE <- hessen_2016_gemeinderatswahl_data_sub$DIELINKE_gew
hessen_2016_gemeinderatswahl_data_sub$gew_GRÜNE <- hessen_2016_gemeinderatswahl_data_sub$Gruene_gew
hessen_2016_gemeinderatswahl_data_sub$gew_AfD <- hessen_2016_gemeinderatswahl_data_sub$AfD_gew
hessen_2016_gemeinderatswahl_data_sub$gew_PIRATEN <- hessen_2016_gemeinderatswahl_data_sub$PIRATEN_gew
hessen_2016_gemeinderatswahl_data_sub$gew_FDP <- hessen_2016_gemeinderatswahl_data_sub$FDP_gew
hessen_2016_gemeinderatswahl_data_sub$gew_DiePARTEI <- hessen_2016_gemeinderatswahl_data_sub$DiePartei_gew
hessen_2016_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- hessen_2016_gemeinderatswahl_data_sub$FreieWaehler_gew

hessen_2016_gemeinderatswahl_data_sub$sitze_CDU <- hessen_2016_gemeinderatswahl_data_sub$CDU_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_SPD <- hessen_2016_gemeinderatswahl_data_sub$SPD_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_DIELINKE <- hessen_2016_gemeinderatswahl_data_sub$DIELINKE_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_2016_gemeinderatswahl_data_sub$Gruene_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_AfD <- hessen_2016_gemeinderatswahl_data_sub$AfD_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_PIRATEN <- hessen_2016_gemeinderatswahl_data_sub$PIRATEN_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_FDP <- hessen_2016_gemeinderatswahl_data_sub$FDP_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_DiePARTEI <- hessen_2016_gemeinderatswahl_data_sub$DiePartei_sitze
hessen_2016_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- hessen_2016_gemeinderatswahl_data_sub$FreieWaehler_sitze

# Creating new dataframe with selected vars ----
hessen_2016_gemeinderatswahl_data_sub <- hessen_2016_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2016_gemeinderatswahl_data_sub <-
  hessen_2016_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_2016_gemeinderatswahl_data_sub$Turnout <- hessen_2016_gemeinderatswahl_data_sub$Wähler /
  hessen_2016_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2021 Gemeinderatswahl ----
#### Load election data ----

hessen_2021_gemeinderatswahl_data <- as.data.table(read_excel(
  "raw/hessen/overview_gemeinderatswahlen.xlsx",
  sheet = "2021"
))

#### Delete white space ----
names(hessen_2021_gemeinderatswahl_data) <- str_replace_all(
  names(hessen_2021_gemeinderatswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
hessen_2021_gemeinderatswahl_data_sub <- hessen_2021_gemeinderatswahl_data

names(hessen_2021_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2021_gemeinderatswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
hessen_2021_gemeinderatswahl_data_sub[, Bundesland := "Hessen"]
hessen_2021_gemeinderatswahl_data_sub[, Gebietsname := ""]
hessen_2021_gemeinderatswahl_data_sub[, election_year := "2021"]
hessen_2021_gemeinderatswahl_data_sub[, election_type := "Kommunalwahlen"]
hessen_2021_gemeinderatswahl_data_sub[, IDIRB := ""]
hessen_2021_gemeinderatswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
hessen_2021_gemeinderatswahl_data_sub$AGS_8dig <- hessen_2021_gemeinderatswahl_data_sub$AGS
hessen_2021_gemeinderatswahl_data_sub$Gebietsname <- hessen_2021_gemeinderatswahl_data_sub$Name
hessen_2021_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt <- hessen_2021_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt
hessen_2021_gemeinderatswahl_data_sub$Wähler <- hessen_2021_gemeinderatswahl_data_sub$Waehlerinsgesamt
hessen_2021_gemeinderatswahl_data_sub$GültigeStimmen <- hessen_2021_gemeinderatswahl_data_sub$GueltigeStimmen

hessen_2021_gemeinderatswahl_data_sub$abs_CDU <- hessen_2021_gemeinderatswahl_data_sub$CDU
hessen_2021_gemeinderatswahl_data_sub$abs_SPD <- hessen_2021_gemeinderatswahl_data_sub$SPD
hessen_2021_gemeinderatswahl_data_sub$abs_DIELINKE <- hessen_2021_gemeinderatswahl_data_sub$DIELINKE
hessen_2021_gemeinderatswahl_data_sub$abs_GRÜNE <- hessen_2021_gemeinderatswahl_data_sub$Gruene
hessen_2021_gemeinderatswahl_data_sub$abs_AfD <- hessen_2021_gemeinderatswahl_data_sub$AfD
hessen_2021_gemeinderatswahl_data_sub$abs_PIRATEN <- hessen_2021_gemeinderatswahl_data_sub$PIRATEN
hessen_2021_gemeinderatswahl_data_sub$abs_FDP <- hessen_2021_gemeinderatswahl_data_sub$FDP
hessen_2021_gemeinderatswahl_data_sub$abs_DiePARTEI <- hessen_2021_gemeinderatswahl_data_sub$DiePartei
hessen_2021_gemeinderatswahl_data_sub$abs_FREIEWÄHLER <- hessen_2021_gemeinderatswahl_data_sub$FreieWaehler

hessen_2021_gemeinderatswahl_data_sub$gew_CDU <- hessen_2021_gemeinderatswahl_data_sub$CDU_gew
hessen_2021_gemeinderatswahl_data_sub$gew_SPD <- hessen_2021_gemeinderatswahl_data_sub$SPD_gew
hessen_2021_gemeinderatswahl_data_sub$gew_DIELINKE <- hessen_2021_gemeinderatswahl_data_sub$DIELINKE_gew
hessen_2021_gemeinderatswahl_data_sub$gew_GRÜNE <- hessen_2021_gemeinderatswahl_data_sub$Gruene_gew
hessen_2021_gemeinderatswahl_data_sub$gew_AfD <- hessen_2021_gemeinderatswahl_data_sub$AfD_gew
hessen_2021_gemeinderatswahl_data_sub$gew_PIRATEN <- hessen_2021_gemeinderatswahl_data_sub$PIRATEN_gew
hessen_2021_gemeinderatswahl_data_sub$gew_FDP <- hessen_2021_gemeinderatswahl_data_sub$FDP_gew
hessen_2021_gemeinderatswahl_data_sub$gew_DiePARTEI <- hessen_2021_gemeinderatswahl_data_sub$DiePartei_gew
hessen_2021_gemeinderatswahl_data_sub$gew_FREIEWÄHLER <- hessen_2021_gemeinderatswahl_data_sub$FreieWaehler_gew

hessen_2021_gemeinderatswahl_data_sub$sitze_CDU <- hessen_2021_gemeinderatswahl_data_sub$CDU_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_SPD <- hessen_2021_gemeinderatswahl_data_sub$SPD_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_DIELINKE <- hessen_2021_gemeinderatswahl_data_sub$DIELINKE_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_GRÜNE <- hessen_2021_gemeinderatswahl_data_sub$Gruene_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_AfD <- hessen_2021_gemeinderatswahl_data_sub$AfD_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_PIRATEN <- hessen_2021_gemeinderatswahl_data_sub$PIRATEN_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_FDP <- hessen_2021_gemeinderatswahl_data_sub$FDP_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_DiePARTEI <- hessen_2021_gemeinderatswahl_data_sub$DiePartei_sitze
hessen_2021_gemeinderatswahl_data_sub$sitze_FREIEWÄHLER <- hessen_2021_gemeinderatswahl_data_sub$FreieWaehler_sitze

# Creating new dataframe with selected vars ----
hessen_2021_gemeinderatswahl_data_sub <- hessen_2021_gemeinderatswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2021_gemeinderatswahl_data_sub <-
  hessen_2021_gemeinderatswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
hessen_2021_gemeinderatswahl_data_sub$Turnout <- hessen_2021_gemeinderatswahl_data_sub$Wähler /
  hessen_2021_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Hessen ----
# Merge
hessen_kommunalwahlen <- rbind(
  hessen_1989_gemeinderatswahl_data_sub,
  hessen_1993_gemeinderatswahl_data_sub,
  hessen_1997_gemeinderatswahl_data_sub,
  hessen_2001_gemeinderatswahl_data_sub,
  hessen_2006_gemeinderatswahl_data_sub,
  hessen_2011_gemeinderatswahl_data_sub,
  hessen_2016_gemeinderatswahl_data_sub,
  hessen_2021_gemeinderatswahl_data_sub
)

# Replace - with NA
hessen_kommunalwahlen[hessen_kommunalwahlen == "-"] <- NA

# Fix AGS
hessen_kommunalwahlen$AGS_8dig <- paste(
  "06",
  hessen_kommunalwahlen$AGS_8dig,
  sep = ""
)

# Save
#write_csv(hessen_kommunalwahlen, here::here("output/hessen_kommunalwahlen.csv"))

# ----
######### NIEDERSACHSEN ----
###### Niedersachsen 1991 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_1991_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_1991.xls",
  sheet = "results"
)) %>%
  filter(Einheit == "ZAHL") %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde)
  )

niedersachsen_1991_kreiswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_kreiswahlen_1991.xls",
  sheet = "results"
)) %>%
  filter(
    Einheit == "ZAHL",
    grepl(",Stadt", Name) | grepl(",Landeshptst", Name),
    nchar(Gemeinde) == 3
  ) %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde, "000")
  )

#### Delete white space ----
names(niedersachsen_1991_gemeinderatswahlen_data) <- str_replace_all(
  names(niedersachsen_1991_gemeinderatswahlen_data),
  fixed(" "),
  ""
)
names(niedersachsen_1991_kreiswahlen_data) <- str_replace_all(
  names(niedersachsen_1991_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Bind ----
niedersachsen_1991_gemeinderatswahlen_data_sub <- niedersachsen_1991_gemeinderatswahlen_data %>%
  rbind(niedersachsen_1991_kreiswahlen_data)

#### Recoding ----
# Creating non-existing variables ----
niedersachsen_1991_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_1991_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_1991_gemeinderatswahlen_data_sub[, Gebietsname := ""]
niedersachsen_1991_gemeinderatswahlen_data_sub[, election_year := "1991"]
niedersachsen_1991_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_1991_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_1991_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_1991_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_1991_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_1991_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_1991_gemeinderatswahlen_data_sub$Name
niedersachsen_1991_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_1991_gemeinderatswahlen_data_sub$'Wahlberechtigte'
niedersachsen_1991_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_1991_gemeinderatswahlen_data_sub$Waehler
niedersachsen_1991_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_1991_gemeinderatswahlen_data_sub$gueltigeStimmen

niedersachsen_1991_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_1991_gemeinderatswahlen_data_sub$CDU
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_1991_gemeinderatswahlen_data_sub$SPD
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_1991_gemeinderatswahlen_data_sub$Gruene
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_AfD <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_FDP <- niedersachsen_1991_gemeinderatswahlen_data_sub$FDP
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

niedersachsen_1991_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_1991_gemeinderatswahlen_data_sub <- niedersachsen_1991_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_1991_gemeinderatswahlen_data_sub <-
  niedersachsen_1991_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_1991_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_1991_gemeinderatswahlen_data_sub$Wähler /
  niedersachsen_1991_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Niedersachsen 1996 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_1996_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_1996.xls",
  sheet = "results"
)) %>%
  filter(Einheit == "ZAHL") %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde)
  )

niedersachsen_1996_kreiswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_kreiswahlen_1996.xls",
  sheet = "results"
)) %>%
  filter(
    Einheit == "ZAHL",
    grepl(",Stadt", Name) | grepl(",Landeshptst", Name),
    nchar(Gemeinde) == 3
  ) %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde, "000")
  )

#### Delete white space ----
names(niedersachsen_1996_gemeinderatswahlen_data) <- str_replace_all(
  names(niedersachsen_1996_gemeinderatswahlen_data),
  fixed(" "),
  ""
)
names(niedersachsen_1996_kreiswahlen_data) <- str_replace_all(
  names(niedersachsen_1996_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Bind ----
niedersachsen_1996_gemeinderatswahlen_data_sub <- niedersachsen_1996_gemeinderatswahlen_data %>%
  rbind(niedersachsen_1996_kreiswahlen_data)

#### Recoding ----
# Creating non-existing variables ----
niedersachsen_1996_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_1996_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_1996_gemeinderatswahlen_data_sub[, Gebietsname := ""]
niedersachsen_1996_gemeinderatswahlen_data_sub[, election_year := "1996"]
niedersachsen_1996_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_1996_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_1996_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_1996_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_1996_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_1996_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_1996_gemeinderatswahlen_data_sub$Name
niedersachsen_1996_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_1996_gemeinderatswahlen_data_sub$`Wahlberechtigte`
niedersachsen_1996_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_1996_gemeinderatswahlen_data_sub$Waehler
niedersachsen_1996_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_1996_gemeinderatswahlen_data_sub$gueltigeStimmen

niedersachsen_1996_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_1996_gemeinderatswahlen_data_sub$CDU
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_1996_gemeinderatswahlen_data_sub$SPD
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_DIELINKE <- niedersachsen_1996_gemeinderatswahlen_data_sub$Linke
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_1996_gemeinderatswahlen_data_sub$Gruene
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_AfD <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_FDP <- niedersachsen_1996_gemeinderatswahlen_data_sub$FDP
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

niedersachsen_1996_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_1996_gemeinderatswahlen_data_sub <- niedersachsen_1996_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_1996_gemeinderatswahlen_data_sub <-
  niedersachsen_1996_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_1996_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_1996_gemeinderatswahlen_data_sub$Wähler /
  niedersachsen_1996_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Niedersachsen 2001 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2001_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_2001.xls",
  sheet = "results"
)) %>%
  filter(Einheit == "ZAHL") %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde)
  )

niedersachsen_2001_kreiswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_kreiswahlen_2001.xls",
  sheet = "results"
)) %>%
  filter(
    Einheit == "ZAHL",
    grepl(",Stadt", Name) | grepl(",Landeshptst", Name),
    nchar(Gemeinde) == 3
  ) %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde, "000")
  )

#### Delete white space ----
names(niedersachsen_2001_gemeinderatswahlen_data) <- str_replace_all(
  names(niedersachsen_2001_gemeinderatswahlen_data),
  fixed(" "),
  ""
)
names(niedersachsen_2001_kreiswahlen_data) <- str_replace_all(
  names(niedersachsen_2001_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Bind ----
niedersachsen_2001_gemeinderatswahlen_data_sub <- niedersachsen_2001_gemeinderatswahlen_data %>%
  rbind(niedersachsen_2001_kreiswahlen_data)

#### Recoding ----
# Creating non-existing variables ----
niedersachsen_2001_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2001_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_2001_gemeinderatswahlen_data_sub[, Gebietsname := ""]
niedersachsen_2001_gemeinderatswahlen_data_sub[, election_year := "2001"]
niedersachsen_2001_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_2001_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_2001_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_2001_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_2001_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_2001_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_2001_gemeinderatswahlen_data_sub$Name
niedersachsen_2001_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2001_gemeinderatswahlen_data_sub$`Wahlberechtigte`
niedersachsen_2001_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2001_gemeinderatswahlen_data_sub$Waehler
niedersachsen_2001_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2001_gemeinderatswahlen_data_sub$gueltigeStimmen

niedersachsen_2001_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_2001_gemeinderatswahlen_data_sub$CDU
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_2001_gemeinderatswahlen_data_sub$SPD
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_2001_gemeinderatswahlen_data_sub$Gruene
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_AfD <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_FDP <- niedersachsen_2001_gemeinderatswahlen_data_sub$FDP
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

niedersachsen_2001_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2001_gemeinderatswahlen_data_sub <- niedersachsen_2001_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2001_gemeinderatswahlen_data_sub <-
  niedersachsen_2001_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_2001_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_2001_gemeinderatswahlen_data_sub$Wähler /
  niedersachsen_2001_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Niedersachsen 2006 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2006_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_2006.xls",
  sheet = "results"
)) %>%
  filter(Einheit == "ZAHL") %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde)
  )

niedersachsen_2006_kreiswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_kreiswahlen_2006.xls",
  sheet = "results"
)) %>%
  filter(
    Einheit == "ZAHL",
    grepl(",Stadt", Name) | grepl(",Landeshptst", Name),
    nchar(Gemeinde) == 3
  ) %>%
  mutate(
    Gemeinde = paste0("03", Gemeinde, "000")
  )

#### Delete white space ----
names(niedersachsen_2006_gemeinderatswahlen_data) <- str_replace_all(
  names(niedersachsen_2006_gemeinderatswahlen_data),
  fixed(" "),
  ""
)
names(niedersachsen_2006_kreiswahlen_data) <- str_replace_all(
  names(niedersachsen_2006_kreiswahlen_data),
  fixed(" "),
  ""
)

#### Bind ----
niedersachsen_2006_gemeinderatswahlen_data_sub <- niedersachsen_2006_gemeinderatswahlen_data %>%
  rbind(niedersachsen_2006_kreiswahlen_data)

#### Recoding ----
# Creating non-existing variables ----
niedersachsen_2006_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2006_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_2006_gemeinderatswahlen_data_sub[, Gebietsname := ""]
niedersachsen_2006_gemeinderatswahlen_data_sub[, election_year := "2006"]
niedersachsen_2006_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_2006_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_2006_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_2006_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_2006_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_2006_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_2006_gemeinderatswahlen_data_sub$Name
niedersachsen_2006_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2006_gemeinderatswahlen_data_sub$`Wahlberechtigte`
niedersachsen_2006_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2006_gemeinderatswahlen_data_sub$Waehler
niedersachsen_2006_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2006_gemeinderatswahlen_data_sub$gueltigeStimmen

niedersachsen_2006_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_2006_gemeinderatswahlen_data_sub$CDU
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_2006_gemeinderatswahlen_data_sub$SPD
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_DIELINKE <- niedersachsen_2006_gemeinderatswahlen_data_sub$Linke
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_2006_gemeinderatswahlen_data_sub$Gruene
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_AfD <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_FDP <- niedersachsen_2006_gemeinderatswahlen_data_sub$FDP
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

niedersachsen_2006_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2006_gemeinderatswahlen_data_sub <- niedersachsen_2006_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2006_gemeinderatswahlen_data_sub <-
  niedersachsen_2006_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_2006_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_2006_gemeinderatswahlen_data_sub$Wähler /
  niedersachsen_2006_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Niedersachsen 2011 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2011_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_2011_anzahl.xlsx",
  sheet = "summary"
))
niedersachsen_2011_gemeinderatswahlen_data_sitze <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_2011_sitze.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(niedersachsen_2011_gemeinderatswahlen_data) <- str_replace_all(
  names(niedersachsen_2011_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
niedersachsen_2011_gemeinderatswahlen_data_sub <- niedersachsen_2011_gemeinderatswahlen_data %>%
  mutate(
    `AGS(Stadtkennziffer/Gemeindekennziffer)` = paste0(
      "03",
      `AGS(Stadtkennziffer/Gemeindekennziffer)`
    )
  )

names(niedersachsen_2011_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2011_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2011_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_2011_gemeinderatswahlen_data_sub[, Gebietsname := ""]
niedersachsen_2011_gemeinderatswahlen_data_sub[, election_year := "2011"]
niedersachsen_2011_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_2011_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_2011_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_2011_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_2011_gemeinderatswahlen_data_sub$"AGS(Stadtkennziffer/Gemeindekennziffer)"
niedersachsen_2011_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_2011_gemeinderatswahlen_data_sub$"NamederStadt/Gemeinde"
niedersachsen_2011_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2011_gemeinderatswahlen_data_sub$'Wahlberechtigtegesamt'
niedersachsen_2011_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2011_gemeinderatswahlen_data_sub$Wählerinsgesamt
niedersachsen_2011_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2011_gemeinderatswahlen_data_sub$GültigeStimmen

niedersachsen_2011_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_2011_gemeinderatswahlen_data_sub$CDU
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_2011_gemeinderatswahlen_data_sub$SPD
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_DIELINKE <- niedersachsen_2011_gemeinderatswahlen_data_sub$'DIELINKE.'
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_2011_gemeinderatswahlen_data_sub$GRÜNE
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_AfD <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_PIRATEN <- niedersachsen_2011_gemeinderatswahlen_data_sub$PIRATENNiedersachsen
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_FDP <- niedersachsen_2011_gemeinderatswahlen_data_sub$FDP
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

niedersachsen_2011_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2011_gemeinderatswahlen_data_sub <- niedersachsen_2011_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2011_gemeinderatswahlen_data_sub <-
  niedersachsen_2011_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_2011_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_2011_gemeinderatswahlen_data_sub$Wähler /
  niedersachsen_2011_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Niedersachsen 2016 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2016_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_2016.xlsx",
  sheet = "anzahl"
))
niedersachsen_2016_gemeinderatswahlen_data_sitze <- as.data.table(read_excel(
  "raw/niedersachsen/niedersachsen_gemeinderatswahlen_2016.xlsx",
  sheet = "sitze"
))

niedersachsen_2016_gemeinderatswahlen_data_sitze <-
  niedersachsen_2016_gemeinderatswahlen_data_sitze %>%
  filter(MG %in% niedersachsen_2016_gemeinderatswahlen_data$MG)

niedersachsen_2016_gemeinderatswahlen_data <-
  niedersachsen_2016_gemeinderatswahlen_data %>%
  mutate(
    EG = paste0("03", EG)
  )


#### Delete white space ----
names(niedersachsen_2016_gemeinderatswahlen_data) <- str_replace_all(
  names(niedersachsen_2016_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
niedersachsen_2016_gemeinderatswahlen_data_sub <- niedersachsen_2016_gemeinderatswahlen_data

names(niedersachsen_2016_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2016_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2016_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_2016_gemeinderatswahlen_data_sub[, Gebietsname := ""]
niedersachsen_2016_gemeinderatswahlen_data_sub[, election_year := "2016"]
niedersachsen_2016_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_2016_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_2016_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_2016_gemeinderatswahlen_data_sub$AGS_8dig <- parse_number(
  niedersachsen_2016_gemeinderatswahlen_data_sub$EG
)
niedersachsen_2016_gemeinderatswahlen_data_sub$Gebietsname <- gsub(
  "[^a-z A-Z]",
  "",
  niedersachsen_2016_gemeinderatswahlen_data_sub$MG
)
niedersachsen_2016_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2016_gemeinderatswahlen_data_sub$'Wahlberechtigung'
niedersachsen_2016_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2016_gemeinderatswahlen_data_sub$Waehler
niedersachsen_2016_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2016_gemeinderatswahlen_data_sub$GueltigeStimmen

niedersachsen_2016_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$CDU
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$SPD
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$DIELINKE
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$Gruene
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$AfD
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_PIRATEN <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$PIRATEN
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$FDP
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_DiePARTEI <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$DiePARTEI
)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$FreieWaehler
)

niedersachsen_2016_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2016_gemeinderatswahlen_data_sub <- niedersachsen_2016_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2016_gemeinderatswahlen_data_sub <-
  niedersachsen_2016_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_2016_gemeinderatswahlen_data_sub$Turnout <- as.numeric(
  niedersachsen_2016_gemeinderatswahlen_data_sub$Wähler
) /
  as.numeric(
    niedersachsen_2016_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
  )


# Niedersachsen Cities 2011 & 2016 ----------------------------------------

niedersachsen_2011_16_staedte <- read_excel(
  "processed/niedersachsen_2011_16_staedte.xlsx"
)

niedersachsen_2011_16_staedte <- niedersachsen_2011_16_staedte %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(
    vars(matches("_XXX")),
    list(~ paste(sub("_XXX", "", .), sep = ""))
  ) %>%
  mutate(election_year = as.character(election_year))

# Calculating turnout ----
niedersachsen_2011_16_staedte$Turnout <- as.numeric(
  niedersachsen_2011_16_staedte$Wähler
) /
  as.numeric(niedersachsen_2011_16_staedte$Wahlberechtigteinsgesamt)


###### Niedersachsen 2021 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2021_gemeinderatswahlen_data <- as.data.table(read_xlsx(
  "raw/niedersachsen/niedersachsen_kommunalwahlen_2021.xlsx",
  sheet = "M5000314",
  skip = 13
)) |>

  rename(label = 1) |>

  filter(label != "%") |>
  mutate(
    Gebietsname = lag(label),
    AGS_8dig = str_extract(Gebietsname, "^[:digit:]{6}(?= )")
  ) |>
  filter(!is.na(AGS_8dig), !str_detect(Gebietsname, "gemfr\\."), `3` != "-") |>

  mutate(
    AGS_8dig = paste0("03", AGS_8dig),
    Gebietsname = Gebietsname |> str_remove_all("[:digit:]") |> str_squish()
  ) |>

  # no samtgemeinden
  filter(!str_detect(Gebietsname, ", SG"))


niedersachsen_2021_gemeinderatswahlen_data_sub <- niedersachsen_2021_gemeinderatswahlen_data

names(niedersachsen_2021_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2021_gemeinderatswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_2021_gemeinderatswahlen_data_sub[, election_year := "2021"]
niedersachsen_2021_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
niedersachsen_2021_gemeinderatswahlen_data_sub[, IDIRB := ""]
niedersachsen_2021_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_2021_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`1`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`2`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`3`
)

niedersachsen_2021_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`4`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`5`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`22`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`6`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`9`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_PIRATEN <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`41`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`7`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_DiePARTEI <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`23`
)
niedersachsen_2021_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$`31`
)

niedersachsen_2021_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_CDU <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_SPD <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_FDP <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2021_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA


# add NUTS3 level election results for Kreisfreie Städte

v_cities_nds = c(
  "Braunschweig, Stadt" = "03101000",
  "Delmenhorst, Stadt" = "03401000",
  "Emden, Stadt" = "03402000",
  "Oldenburg (Oldb), Stadt" = "03403000",
  "Osnabrück, Stadt" = "03404000",
  "Salzgitter, Stadt" = "03102000",
  "Wilhelmshaven, Stadt" = "03405000",
  "Wolfsburg, Stadt" = "03103000"
)

niedersachsen_2021_kreiswahlen_data <- as.data.table(read_csv(
  "raw/niedersachsen/niedersachsen_kreiswahlen_2021.csv"
)) |>
  mutate(AGS_8dig = paste0("03", Wahlkreis, "000")) |>

  # only keep cities
  right_join(
    tibble(AGS_8dig = v_cities_nds, Gebietsname = names(v_cities_nds)),
    by = "AGS_8dig"
  )


niedersachsen_2021_kreiswahlen_data_sub <- niedersachsen_2021_kreiswahlen_data

names(niedersachsen_2021_kreiswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2021_kreiswahlen_data_sub[, Bundesland := "Niedersachsen"]
niedersachsen_2021_kreiswahlen_data_sub[, election_year := "2021"]
niedersachsen_2021_kreiswahlen_data_sub[, election_type := "Kommunalwahlen"]
niedersachsen_2021_kreiswahlen_data_sub[, IDIRB := ""]
niedersachsen_2021_kreiswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
niedersachsen_2021_kreiswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2021_kreiswahlen_data_sub$Wahlberechtigte
niedersachsen_2021_kreiswahlen_data_sub$Wähler <- niedersachsen_2021_kreiswahlen_data_sub$Wähler
niedersachsen_2021_kreiswahlen_data_sub$GültigeStimmen <- niedersachsen_2021_kreiswahlen_data_sub$`Gültige Stimmen`

niedersachsen_2021_kreiswahlen_data_sub$abs_CDU <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`CDU Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_SPD <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`SPD Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_DIELINKE <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`DIE LINKE. Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_GRÜNE <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`GRÜNE Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_AfD <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`AfD Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_PIRATEN <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`PIRATEN Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_FDP <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`FDP Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_DiePARTEI <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`Die PARTEI Stimmen`
)
niedersachsen_2021_kreiswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`FREIE WÄHLER Stimmen`
)

niedersachsen_2021_kreiswahlen_data_sub$sitze_CDU <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`CDU Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_SPD <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`SPD Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_DIELINKE <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`DIE LINKE. Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_GRÜNE <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`GRÜNE Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_AfD <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`AfD Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_PIRATEN <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`PIRATEN Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_FDP <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`FDP Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_DiePARTEI <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`Die PARTEI Sitze`
)
niedersachsen_2021_kreiswahlen_data_sub$sitze_FREIEWÄHLER <- as.numeric(
  niedersachsen_2021_kreiswahlen_data_sub$`FREIE WÄHLER Sitze`
)


# bind to dataframe
niedersachsen_2021_gemeinderatswahlen_data_sub <- bind_rows(
  niedersachsen_2021_gemeinderatswahlen_data_sub,
  niedersachsen_2021_kreiswahlen_data_sub
)


# Creating new dataframe with selected vars ----
niedersachsen_2021_gemeinderatswahlen_data_sub <- niedersachsen_2021_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2021_gemeinderatswahlen_data_sub <-
  niedersachsen_2021_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
niedersachsen_2021_gemeinderatswahlen_data_sub$Turnout <- as.numeric(
  niedersachsen_2021_gemeinderatswahlen_data_sub$Wähler
) /
  as.numeric(
    niedersachsen_2021_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
  )


####### Merge files and save overall output for Niedersachsen ----
# Merge
niedersachsen_kommunalwahlen <- rbind(
  niedersachsen_1991_gemeinderatswahlen_data_sub,
  niedersachsen_1996_gemeinderatswahlen_data_sub,
  niedersachsen_2001_gemeinderatswahlen_data_sub,
  niedersachsen_2006_gemeinderatswahlen_data_sub,
  niedersachsen_2011_gemeinderatswahlen_data_sub,
  niedersachsen_2016_gemeinderatswahlen_data_sub,
  niedersachsen_2021_gemeinderatswahlen_data_sub
)

niedersachsen_kommunalwahlen <- niedersachsen_kommunalwahlen |>
  bind_rows(niedersachsen_2011_16_staedte)

# Replace - with NA
niedersachsen_kommunalwahlen[niedersachsen_kommunalwahlen == "-"] <- NA


# Fix Gebietsnamen
niedersachsen_kommunalwahlen$Gebietsname <- str_replace(
  niedersachsen_kommunalwahlen$Gebietsname,
  "MG",
  ""
)
niedersachsen_kommunalwahlen$Gebietsname <- str_replace(
  niedersachsen_kommunalwahlen$Gebietsname,
  "EG",
  ""
)


# Save
#write_csv(niedersachsen_kommunalwahlen, here::here("output/niedersachsen_kommunalwahlen.csv"))

# ----
# ----

######### SACHSEN ----
###### Sachsen 1994 Gemeinderatswahlen ----
#### Load election data ----

sachsen_1994_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/sachsen/sachsen_1994.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sachsen_1994_gemeinderatswahlen_data) <- str_replace_all(
  names(sachsen_1994_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_1994_gemeinderatswahlen_data_sub <- sachsen_1994_gemeinderatswahlen_data

# Creating non-existing variables ----
sachsen_1994_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_1994_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_1994_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_1994_gemeinderatswahlen_data_sub[, election_year := "1994"]
sachsen_1994_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_1994_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_1994_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_1994_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_1994_gemeinderatswahlen_data_sub$AGS
sachsen_1994_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_1994_gemeinderatswahlen_data_sub$Gemeindename
sachsen_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_1994_gemeinderatswahlen_data_sub$Wahlberechtigte
sachsen_1994_gemeinderatswahlen_data_sub$Wähler <- sachsen_1994_gemeinderatswahlen_data_sub$Waehler
sachsen_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_1994_gemeinderatswahlen_data_sub$gueltigeStimmen

sachsen_1994_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_1994_gemeinderatswahlen_data_sub$CDU
sachsen_1994_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_1994_gemeinderatswahlen_data_sub$SPD
sachsen_1994_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_1994_gemeinderatswahlen_data_sub$DIELINKE
sachsen_1994_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_1994_gemeinderatswahlen_data_sub$GRUENE
sachsen_1994_gemeinderatswahlen_data_sub$abs_AfD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sachsen_1994_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_1994_gemeinderatswahlen_data_sub$FDP
sachsen_1994_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sachsen_1994_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
sachsen_1994_gemeinderatswahlen_data_sub$abs_BSW <- NA

sachsen_1994_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_BSW <- NA

sachsen_1994_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_1994_gemeinderatswahlen_data_sub <- sachsen_1994_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_1994_gemeinderatswahlen_data_sub <-
  sachsen_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_1994_gemeinderatswahlen_data_sub$Turnout <- sachsen_1994_gemeinderatswahlen_data_sub$Wähler /
  sachsen_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_1994_gemeinderatswahlen_data_sub <- sachsen_1994_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

###### Sachsen 1999 Gemeinderatswahlen ----
#### Load election data ----

sachsen_1999_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/sachsen/sachsen_1999.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sachsen_1999_gemeinderatswahlen_data) <- str_replace_all(
  names(sachsen_1999_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_1999_gemeinderatswahlen_data_sub <- sachsen_1999_gemeinderatswahlen_data

names(sachsen_1999_gemeinderatswahlen_data_sub)


# Creating non-existing variables ----
sachsen_1999_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_1999_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_1999_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_1999_gemeinderatswahlen_data_sub[, election_year := "1999"]
sachsen_1999_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_1999_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_1999_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_1999_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_1999_gemeinderatswahlen_data_sub$AGS
sachsen_1999_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_1999_gemeinderatswahlen_data_sub$Gemeindename
sachsen_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_1999_gemeinderatswahlen_data_sub$Wahlberechtigte
sachsen_1999_gemeinderatswahlen_data_sub$Wähler <- sachsen_1999_gemeinderatswahlen_data_sub$Waehler
sachsen_1999_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_1999_gemeinderatswahlen_data_sub$gueltigeStimmen

sachsen_1999_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_1999_gemeinderatswahlen_data_sub$CDU
sachsen_1999_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_1999_gemeinderatswahlen_data_sub$SPD
sachsen_1999_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_1999_gemeinderatswahlen_data_sub$DIELINKE
sachsen_1999_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_1999_gemeinderatswahlen_data_sub$GRUENE
sachsen_1999_gemeinderatswahlen_data_sub$abs_AfD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sachsen_1999_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_1999_gemeinderatswahlen_data_sub$FDP
sachsen_1999_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sachsen_1999_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
sachsen_1999_gemeinderatswahlen_data_sub$abs_BSW <- NA

sachsen_1999_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_BSW <- NA

sachsen_1999_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_1999_gemeinderatswahlen_data_sub <- sachsen_1999_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_1999_gemeinderatswahlen_data_sub <-
  sachsen_1999_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_1999_gemeinderatswahlen_data_sub$Turnout <- sachsen_1999_gemeinderatswahlen_data_sub$Wähler /
  sachsen_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


# Remove !nchar(AGS)==8 ----
sachsen_1999_gemeinderatswahlen_data_sub <- sachsen_1999_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

###### Sachsen 2004 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2004_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/sachsen/sachsen_2004.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sachsen_2004_gemeinderatswahlen_data) <- str_replace_all(
  names(sachsen_2004_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_2004_gemeinderatswahlen_data_sub <- sachsen_2004_gemeinderatswahlen_data

names(sachsen_2004_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2004_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_2004_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_2004_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_2004_gemeinderatswahlen_data_sub[, election_year := "2004"]
sachsen_2004_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_2004_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_2004_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_2004_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_2004_gemeinderatswahlen_data_sub$AGS
sachsen_2004_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_2004_gemeinderatswahlen_data_sub$Gemeindename
sachsen_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_2004_gemeinderatswahlen_data_sub$Wahlberechtigte
sachsen_2004_gemeinderatswahlen_data_sub$Wähler <- sachsen_2004_gemeinderatswahlen_data_sub$Waehler
sachsen_2004_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_2004_gemeinderatswahlen_data_sub$gueltigeStimmen

sachsen_2004_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_2004_gemeinderatswahlen_data_sub$CDU
sachsen_2004_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_2004_gemeinderatswahlen_data_sub$SPD
sachsen_2004_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_2004_gemeinderatswahlen_data_sub$DIELINKE
sachsen_2004_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_2004_gemeinderatswahlen_data_sub$GRUENE
sachsen_2004_gemeinderatswahlen_data_sub$abs_AfD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sachsen_2004_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_2004_gemeinderatswahlen_data_sub$FDP
sachsen_2004_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sachsen_2004_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
sachsen_2004_gemeinderatswahlen_data_sub$abs_BSW <- NA

sachsen_2004_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_BSW <- NA

sachsen_2004_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_2004_gemeinderatswahlen_data_sub <- sachsen_2004_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2004_gemeinderatswahlen_data_sub <-
  sachsen_2004_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_2004_gemeinderatswahlen_data_sub$Turnout <- sachsen_2004_gemeinderatswahlen_data_sub$Wähler /
  sachsen_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2004_gemeinderatswahlen_data_sub <- sachsen_2004_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

###### Sachsen 2009 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2009_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/sachsen/sachsen_2009.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sachsen_2009_gemeinderatswahlen_data) <- str_replace_all(
  names(sachsen_2009_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_2009_gemeinderatswahlen_data_sub <- sachsen_2009_gemeinderatswahlen_data

names(sachsen_2009_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2009_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_2009_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_2009_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_2009_gemeinderatswahlen_data_sub[, election_year := "2009"]
sachsen_2009_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_2009_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_2009_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_2009_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_2009_gemeinderatswahlen_data_sub$AGS
sachsen_2009_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_2009_gemeinderatswahlen_data_sub$Gemeindename
sachsen_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_2009_gemeinderatswahlen_data_sub$Wahlberechtigte
sachsen_2009_gemeinderatswahlen_data_sub$Wähler <- sachsen_2009_gemeinderatswahlen_data_sub$Waehler
sachsen_2009_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_2009_gemeinderatswahlen_data_sub$gueltigeStimmen

sachsen_2009_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_2009_gemeinderatswahlen_data_sub$CDU
sachsen_2009_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_2009_gemeinderatswahlen_data_sub$SPD
sachsen_2009_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_2009_gemeinderatswahlen_data_sub$DIELINKE
sachsen_2009_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_2009_gemeinderatswahlen_data_sub$GRUENE
sachsen_2009_gemeinderatswahlen_data_sub$abs_AfD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sachsen_2009_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_2009_gemeinderatswahlen_data_sub$FDP
sachsen_2009_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sachsen_2009_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
sachsen_2009_gemeinderatswahlen_data_sub$abs_BSW <- NA

sachsen_2009_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_BSW <- NA

sachsen_2009_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_2009_gemeinderatswahlen_data_sub <- sachsen_2009_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2009_gemeinderatswahlen_data_sub <-
  sachsen_2009_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_2009_gemeinderatswahlen_data_sub$Turnout <- sachsen_2009_gemeinderatswahlen_data_sub$Wähler /
  sachsen_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2009_gemeinderatswahlen_data_sub <- sachsen_2009_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

###### Sachsen 2014 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2014_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/sachsen/sachsen_2014.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sachsen_2014_gemeinderatswahlen_data) <- str_replace_all(
  names(sachsen_2014_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_2014_gemeinderatswahlen_data_sub <- sachsen_2014_gemeinderatswahlen_data

names(sachsen_2014_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2014_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_2014_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_2014_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_2014_gemeinderatswahlen_data_sub[, election_year := "2014"]
sachsen_2014_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_2014_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_2014_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_2014_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_2014_gemeinderatswahlen_data_sub$AGS
sachsen_2014_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_2014_gemeinderatswahlen_data_sub$Gemeindename
sachsen_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_2014_gemeinderatswahlen_data_sub$Wahlberechtigte
sachsen_2014_gemeinderatswahlen_data_sub$Wähler <- sachsen_2014_gemeinderatswahlen_data_sub$Waehler
sachsen_2014_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_2014_gemeinderatswahlen_data_sub$gueltigeStimmen

sachsen_2014_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_2014_gemeinderatswahlen_data_sub$CDU
sachsen_2014_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_2014_gemeinderatswahlen_data_sub$SPD
sachsen_2014_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_2014_gemeinderatswahlen_data_sub$DIELINKE
sachsen_2014_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_2014_gemeinderatswahlen_data_sub$GRUENE
sachsen_2014_gemeinderatswahlen_data_sub$abs_AfD <- sachsen_2014_gemeinderatswahlen_data_sub$AfD
sachsen_2014_gemeinderatswahlen_data_sub$abs_PIRATEN <- sachsen_2014_gemeinderatswahlen_data_sub$PIRATEN
sachsen_2014_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_2014_gemeinderatswahlen_data_sub$FDP
sachsen_2014_gemeinderatswahlen_data_sub$abs_DiePARTEI <- sachsen_2014_gemeinderatswahlen_data_sub$DiePARTEI
sachsen_2014_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
sachsen_2014_gemeinderatswahlen_data_sub$abs_BSW <- NA

sachsen_2014_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_BSW <- NA

sachsen_2014_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_2014_gemeinderatswahlen_data_sub <- sachsen_2014_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2014_gemeinderatswahlen_data_sub <-
  sachsen_2014_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_2014_gemeinderatswahlen_data_sub$Turnout <- sachsen_2014_gemeinderatswahlen_data_sub$Wähler /
  sachsen_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2014_gemeinderatswahlen_data_sub <- sachsen_2014_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

###### Sachsen 2019 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2019_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/sachsen/sachsen_2019.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sachsen_2019_gemeinderatswahlen_data) <- str_replace_all(
  names(sachsen_2019_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sachsen_2019_gemeinderatswahlen_data_sub <- sachsen_2019_gemeinderatswahlen_data

names(sachsen_2019_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2019_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_2019_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_2019_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_2019_gemeinderatswahlen_data_sub[, election_year := "2019"]
sachsen_2019_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_2019_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_2019_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_2019_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_2019_gemeinderatswahlen_data_sub$AGS
sachsen_2019_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_2019_gemeinderatswahlen_data_sub$Gemeindename
sachsen_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_2019_gemeinderatswahlen_data_sub$Wahlberechtigte
sachsen_2019_gemeinderatswahlen_data_sub$Wähler <- sachsen_2019_gemeinderatswahlen_data_sub$Waehler
sachsen_2019_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_2019_gemeinderatswahlen_data_sub$gueltigeStimmen

sachsen_2019_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_2019_gemeinderatswahlen_data_sub$CDU
sachsen_2019_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_2019_gemeinderatswahlen_data_sub$SPD
sachsen_2019_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_2019_gemeinderatswahlen_data_sub$DIELINKE
sachsen_2019_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_2019_gemeinderatswahlen_data_sub$GRUENE
sachsen_2019_gemeinderatswahlen_data_sub$abs_AfD <- sachsen_2019_gemeinderatswahlen_data_sub$AfD
sachsen_2019_gemeinderatswahlen_data_sub$abs_PIRATEN <- sachsen_2019_gemeinderatswahlen_data_sub$PIRATEN
sachsen_2019_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_2019_gemeinderatswahlen_data_sub$FDP
sachsen_2019_gemeinderatswahlen_data_sub$abs_DiePARTEI <- sachsen_2019_gemeinderatswahlen_data_sub$DiePARTEI
sachsen_2019_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- sachsen_2019_gemeinderatswahlen_data_sub$FREIEWAEHLER
sachsen_2019_gemeinderatswahlen_data_sub$abs_BSW <- NA

sachsen_2019_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_BSW <- NA

sachsen_2019_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_2019_gemeinderatswahlen_data_sub <- sachsen_2019_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2019_gemeinderatswahlen_data_sub <-
  sachsen_2019_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_2019_gemeinderatswahlen_data_sub$Turnout <- sachsen_2019_gemeinderatswahlen_data_sub$Wähler /
  sachsen_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2019_gemeinderatswahlen_data_sub <- sachsen_2019_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

###### Sachsen 2024 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2024_gemeinderatswahlen_data <- as.data.table(read_excel(
  'raw/sachsen/sachsen_2024.xlsx',
  sheet = 'GR24_EE_KS_GE'
)) |>
  clean_names()


#### Replace 'x' with NA ----
sachsen_2024_gemeinderatswahlen_data <- sachsen_2024_gemeinderatswahlen_data |>
  mutate(across(where(is.character), ~ na_if(., 'x')))

#### Recoding ----
# Create new dataframe ----
sachsen_2024_gemeinderatswahlen_data_sub <- sachsen_2024_gemeinderatswahlen_data

names(sachsen_2024_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2024_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sachsen_2024_gemeinderatswahlen_data_sub[, Bundesland := "Sachsen"]
sachsen_2024_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sachsen_2024_gemeinderatswahlen_data_sub[, election_year := "2024"]
sachsen_2024_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sachsen_2024_gemeinderatswahlen_data_sub[, IDIRB := ""]
sachsen_2024_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sachsen_2024_gemeinderatswahlen_data_sub$AGS_8dig <- sachsen_2024_gemeinderatswahlen_data_sub$ortnummer
sachsen_2024_gemeinderatswahlen_data_sub$Gebietsname <- sachsen_2024_gemeinderatswahlen_data_sub$ortname
sachsen_2024_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_2024_gemeinderatswahlen_data_sub$wahlberechtigte
sachsen_2024_gemeinderatswahlen_data_sub$Wähler <- sachsen_2024_gemeinderatswahlen_data_sub$wahler
sachsen_2024_gemeinderatswahlen_data_sub$GültigeStimmen <- sachsen_2024_gemeinderatswahlen_data_sub$gultige_stimmen

sachsen_2024_gemeinderatswahlen_data_sub$abs_CDU <- sachsen_2024_gemeinderatswahlen_data_sub$cdu
sachsen_2024_gemeinderatswahlen_data_sub$abs_SPD <- sachsen_2024_gemeinderatswahlen_data_sub$spd
sachsen_2024_gemeinderatswahlen_data_sub$abs_DIELINKE <- sachsen_2024_gemeinderatswahlen_data_sub$die_linke
sachsen_2024_gemeinderatswahlen_data_sub$abs_GRÜNE <- sachsen_2024_gemeinderatswahlen_data_sub$grune
sachsen_2024_gemeinderatswahlen_data_sub$abs_AfD <- sachsen_2024_gemeinderatswahlen_data_sub$af_d
sachsen_2024_gemeinderatswahlen_data_sub$abs_PIRATEN <- sachsen_2024_gemeinderatswahlen_data_sub$piraten
sachsen_2024_gemeinderatswahlen_data_sub$abs_FDP <- sachsen_2024_gemeinderatswahlen_data_sub$fdp
sachsen_2024_gemeinderatswahlen_data_sub$abs_DiePARTEI <- sachsen_2024_gemeinderatswahlen_data_sub$die_partei
sachsen_2024_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
sachsen_2024_gemeinderatswahlen_data_sub$abs_BSW <- sachsen_2024_gemeinderatswahlen_data_sub$bsw

sachsen_2024_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
sachsen_2024_gemeinderatswahlen_data_sub$gew_BSW <- NA


sachsen_2024_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
sachsen_2024_gemeinderatswahlen_data_sub$sitze_BSW <- NA

# Creating new dataframe with selected vars ----
sachsen_2024_gemeinderatswahlen_data_sub <- sachsen_2024_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  abs_BSW,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  gew_BSW,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER,
  sitze_BSW
)] |>
  mutate(across(
    c(starts_with('abs_'), starts_with('gew_'), starts_with('sitze_')),
    ~ as.numeric(.)
  ))

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2024_gemeinderatswahlen_data_sub <-
  sachsen_2024_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sachsen_2024_gemeinderatswahlen_data_sub$Turnout <- sachsen_2024_gemeinderatswahlen_data_sub$Wähler /
  sachsen_2024_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2024_gemeinderatswahlen_data_sub <- sachsen_2024_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig) == 8)

####### Merge files and save overall output for Sachsen ----
# Merge
sachsen_kommunalwahlen <- rbind(
  sachsen_1994_gemeinderatswahlen_data_sub,
  sachsen_1999_gemeinderatswahlen_data_sub,
  sachsen_2004_gemeinderatswahlen_data_sub,
  sachsen_2009_gemeinderatswahlen_data_sub,
  sachsen_2014_gemeinderatswahlen_data_sub,
  sachsen_2019_gemeinderatswahlen_data_sub,
  sachsen_2024_gemeinderatswahlen_data_sub
)

# Replace - with NA
sachsen_kommunalwahlen[sachsen_kommunalwahlen == "-"] <- NA

# Fix AGS ----
sachsen_kommunalwahlen$AGS_8dig <- stri_pad_right(
  sachsen_kommunalwahlen$AGS_8dig,
  8,
  0
)


# Save
#write_csv(sachsen_kommunalwahlen, here::here("output/sachsen_kommunalwahlen.csv"))

# ----
# ----
######### BREMEN ----
###### Bremen Bürgerschaftswahlen ----
#### Load election data ----

bremen_overall_buergerschaftswahl_data <- as.data.table(read_excel(
  "raw/bremen/bremen_summary.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(bremen_overall_buergerschaftswahl_data) <- str_replace_all(
  names(bremen_overall_buergerschaftswahl_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
bremen_overall_buergerschaftswahl_data_sub <- bremen_overall_buergerschaftswahl_data

# Creating non-existing variables ----
bremen_overall_buergerschaftswahl_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
bremen_overall_buergerschaftswahl_data_sub[, Bundesland := "Bremen"]
bremen_overall_buergerschaftswahl_data_sub[, Gebietsname := ""]
bremen_overall_buergerschaftswahl_data_sub[,
  election_type := "Bürgerschaftswahl"
]
bremen_overall_buergerschaftswahl_data_sub[, IDIRB := ""]
bremen_overall_buergerschaftswahl_data_sub[, IDBA := ""]

# Renaming existing variables ----
bremen_overall_buergerschaftswahl_data_sub$AGS_8dig <- bremen_overall_buergerschaftswahl_data_sub$AGS
bremen_overall_buergerschaftswahl_data_sub$Gebietsname <- bremen_overall_buergerschaftswahl_data_sub$Gebietseinheit
bremen_overall_buergerschaftswahl_data_sub$election_year <- bremen_overall_buergerschaftswahl_data_sub$election_year
bremen_overall_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt <- bremen_overall_buergerschaftswahl_data_sub$Wahlberechtigte
bremen_overall_buergerschaftswahl_data_sub$Wähler <- bremen_overall_buergerschaftswahl_data_sub$Wähler
bremen_overall_buergerschaftswahl_data_sub$GültigeStimmen <- bremen_overall_buergerschaftswahl_data_sub$GültigeStimmen

bremen_overall_buergerschaftswahl_data_sub$abs_CDU <- bremen_overall_buergerschaftswahl_data_sub$CDU
bremen_overall_buergerschaftswahl_data_sub$abs_SPD <- bremen_overall_buergerschaftswahl_data_sub$SPD
bremen_overall_buergerschaftswahl_data_sub$abs_DIELINKE <- bremen_overall_buergerschaftswahl_data_sub$DIELINKE
bremen_overall_buergerschaftswahl_data_sub$abs_GRÜNE <- bremen_overall_buergerschaftswahl_data_sub$GRÜNE
bremen_overall_buergerschaftswahl_data_sub$abs_AfD <- bremen_overall_buergerschaftswahl_data_sub$AfD
bremen_overall_buergerschaftswahl_data_sub$abs_PIRATEN <- bremen_overall_buergerschaftswahl_data_sub$PIRATEN
bremen_overall_buergerschaftswahl_data_sub$abs_FDP <- bremen_overall_buergerschaftswahl_data_sub$FDP
bremen_overall_buergerschaftswahl_data_sub$abs_DiePARTEI <- NA
bremen_overall_buergerschaftswahl_data_sub$abs_FREIEWÄHLER <- NA

bremen_overall_buergerschaftswahl_data_sub$gew_CDU <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_SPD <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_DIELINKE <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_GRÜNE <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_AfD <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_PIRATEN <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_FDP <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_DiePARTEI <- NA
bremen_overall_buergerschaftswahl_data_sub$gew_FREIEWÄHLER <- NA

bremen_overall_buergerschaftswahl_data_sub$sitze_CDU <- bremen_overall_buergerschaftswahl_data_sub$CDU_sitze
bremen_overall_buergerschaftswahl_data_sub$sitze_SPD <- bremen_overall_buergerschaftswahl_data_sub$SPD_sitze
bremen_overall_buergerschaftswahl_data_sub$sitze_DIELINKE <- bremen_overall_buergerschaftswahl_data_sub$DIELINKE_sitze
bremen_overall_buergerschaftswahl_data_sub$sitze_GRÜNE <- bremen_overall_buergerschaftswahl_data_sub$GRÜNE_sitze
bremen_overall_buergerschaftswahl_data_sub$sitze_AfD <- bremen_overall_buergerschaftswahl_data_sub$AfD_sitze
bremen_overall_buergerschaftswahl_data_sub$sitze_PIRATEN <- NA
bremen_overall_buergerschaftswahl_data_sub$sitze_FDP <- bremen_overall_buergerschaftswahl_data_sub$FDP_sitze
bremen_overall_buergerschaftswahl_data_sub$sitze_DiePARTEI <- NA
bremen_overall_buergerschaftswahl_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
bremen_overall_buergerschaftswahl_data_sub <- bremen_overall_buergerschaftswahl_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bremen_overall_buergerschaftswahl_data_sub <-
  bremen_overall_buergerschaftswahl_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
bremen_overall_buergerschaftswahl_data_sub$Turnout <- bremen_overall_buergerschaftswahl_data_sub$Wähler /
  bremen_overall_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

# Remove Bremen Land ----
bremen_overall_buergerschaftswahl_data_sub <- bremen_overall_buergerschaftswahl_data_sub %>%
  filter(
    !Gebietsname == "Land Bremen"
  )

####### Merge files and save overall output for Bremen ----
# Save
#write_csv(bremen_overall_buergerschaftswahl_data_sub, here::here("output/bremen_buergerschaftswahlen.csv"))

# ----
# ----
######### BRANDENBURG ----
###### Brandenburg 1993 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_1993_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/brandenburg/brandenburg_1993.xlsx",
  sheet = "Ergebnis"
))

# Transpose
brandenburg_1993_gemeinderatswahlen_data_recoded <- as.data.frame(t(
  brandenburg_1993_gemeinderatswahlen_data
))

# Rename the columns with the first row (AGS)
colnames(
  brandenburg_1993_gemeinderatswahlen_data_recoded
) <- brandenburg_1993_gemeinderatswahlen_data_recoded[1, ]

# Remove the first row, since it's now in the column names
brandenburg_1993_gemeinderatswahlen_data_recoded <- brandenburg_1993_gemeinderatswahlen_data_recoded[
  -c(1:2),
]

# Remove row names
rownames(brandenburg_1993_gemeinderatswahlen_data_recoded) <- NULL


#### Recoding ----
# Fix AGS
brandenburg_1993_gemeinderatswahlen_data_recoded <- brandenburg_1993_gemeinderatswahlen_data_recoded %>%
  mutate(AGS_8dig = paste0("12", str_remove_all(AGS, " ")))

#test <- brandenburg_1993_gemeinderatswahlen_data_recoded[nchar(brandenburg_1993_gemeinderatswahlen_data_recoded$AGS_8dig) !=8,]

# Creating non-existing variables ----
brandenburg_1993_gemeinderatswahlen_data_recoded <- as.data.table(
  brandenburg_1993_gemeinderatswahlen_data_recoded
)
brandenburg_1993_gemeinderatswahlen_data_recoded[, Bundesland := "Brandenburg"]
brandenburg_1993_gemeinderatswahlen_data_recoded[, election_year := "1993"]
brandenburg_1993_gemeinderatswahlen_data_recoded[,
  election_type := "Kommunalwahlen"
]
brandenburg_1993_gemeinderatswahlen_data_recoded[, IDIRB := ""]
brandenburg_1993_gemeinderatswahlen_data_recoded[, IDBA := ""]

# Renaming existing variables ----
brandenburg_1993_gemeinderatswahlen_data_recoded$Gebietsname <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$Wahlberechtigte
)
brandenburg_1993_gemeinderatswahlen_data_recoded$Wähler <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$Wähler
)
brandenburg_1993_gemeinderatswahlen_data_recoded$GültigeStimmen <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$`gültige Stimmen`
)

brandenburg_1993_gemeinderatswahlen_data_recoded$abs_CDU <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$CDU
)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_SPD <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$SPD
)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_DIELINKE <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$PDS
)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_GRÜNE <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$`GRÜNE/B90`
)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_AfD <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_PIRATEN <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_FDP <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$`F.D.P.`
)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- as.numeric(
  brandenburg_1993_gemeinderatswahlen_data_recoded$WG
)

brandenburg_1993_gemeinderatswahlen_data_recoded$gew_CDU <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_SPD <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_DIELINKE <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_GRÜNE <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_AfD <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_PIRATEN <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_FDP <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_DiePARTEI <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_FREIEWÄHLER <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$gew_Wählergruppen <- NA

brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_CDU <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_SPD <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_DIELINKE <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_GRÜNE <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_AfD <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_PIRATEN <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_FDP <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_DiePARTEI <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$sitze_Wählergruppen <- NA

names(brandenburg_1993_gemeinderatswahlen_data_recoded)

# Creating new dataframe with selected vars ----
brandenburg_1993_gemeinderatswahlen_data_recoded <- brandenburg_1993_gemeinderatswahlen_data_recoded[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_1993_gemeinderatswahlen_data_recoded <-
  brandenburg_1993_gemeinderatswahlen_data_recoded %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
brandenburg_1993_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_1993_gemeinderatswahlen_data_recoded$Wähler /
  brandenburg_1993_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 1998 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_1998_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/brandenburg/brandenburg_1998.xlsx",
  sheet = "Ergebnis"
))

# Transpose
brandenburg_1998_gemeinderatswahlen_data_recoded <- as.data.frame(t(
  brandenburg_1998_gemeinderatswahlen_data
))

# Rename the columns with the first row (AGS)
colnames(
  brandenburg_1998_gemeinderatswahlen_data_recoded
) <- brandenburg_1998_gemeinderatswahlen_data_recoded[1, ]

# Remove the first row, since it's now in the column names
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded[
  -c(1),
]

# Remove row names
rownames(brandenburg_1998_gemeinderatswahlen_data_recoded) <- NULL


#### Recoding ----
# Fix AGS
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded %>%
  mutate(AGS_8dig = str_remove_all(AGS, " ")) %>%
  filter(!is.na(AGS_8dig))

# Remove Wahlkreise
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded %>%
  filter(
    !AGS_name %in%
      c(
        "Uckermark",
        "Barnim",
        "Prignitz",
        "Ostprignitz-Ruppin",
        "Oberhavel",
        "Havelland",
        "Märkisch-Oderland",
        "Oder-Spree",
        "Teltow-Fläming",
        "Dahme-Spreewald",
        "Elbe-Elster",
        "Oberspreewald-Lausitz",
        "Spree-Neiße",
        "Potsdam-Mittelmark"
      )
  )

# test <- brandenburg_1998_gemeinderatswahlen_data_recoded[nchar(brandenburg_1998_gemeinderatswahlen_data_recoded$AGS_8dig) !=8,]
#
# test <- brandenburg_1998_gemeinderatswahlen_data_recoded %>%
#   filter(grepl("[^0-9]", AGS_8dig))

# Creating non-existing variables ----
brandenburg_1998_gemeinderatswahlen_data_recoded <- as.data.table(
  brandenburg_1998_gemeinderatswahlen_data_recoded
)
brandenburg_1998_gemeinderatswahlen_data_recoded[, Bundesland := "Brandenburg"]
brandenburg_1998_gemeinderatswahlen_data_recoded[, election_year := "1998"]
brandenburg_1998_gemeinderatswahlen_data_recoded[,
  election_type := "Kommunalwahlen"
]
brandenburg_1998_gemeinderatswahlen_data_recoded[, IDIRB := ""]
brandenburg_1998_gemeinderatswahlen_data_recoded[, IDBA := ""]

# Renaming existing variables ----
brandenburg_1998_gemeinderatswahlen_data_recoded$Gebietsname <- brandenburg_1998_gemeinderatswahlen_data_recoded$AGS_name
brandenburg_1998_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$Wahlberechtigte
)
brandenburg_1998_gemeinderatswahlen_data_recoded$Wähler <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$Wähler
)
brandenburg_1998_gemeinderatswahlen_data_recoded$GültigeStimmen <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$`gültige Stimmen`
)

brandenburg_1998_gemeinderatswahlen_data_recoded$abs_CDU <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$CDU
)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_SPD <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$SPD
)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_DIELINKE <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$PDS
)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_GRÜNE <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$`GRÜNE/B90`
)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_AfD <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_PIRATEN <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_FDP <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$`F.D.P.`
)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- as.numeric(
  brandenburg_1998_gemeinderatswahlen_data_recoded$WG
)

brandenburg_1998_gemeinderatswahlen_data_recoded$gew_CDU <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_SPD <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_DIELINKE <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_GRÜNE <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_AfD <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_PIRATEN <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_FDP <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_DiePARTEI <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_FREIEWÄHLER <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$gew_Wählergruppen <- NA

brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_CDU <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_SPD <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_DIELINKE <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_GRÜNE <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_AfD <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_PIRATEN <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_FDP <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_DiePARTEI <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$sitze_Wählergruppen <- NA

names(brandenburg_1998_gemeinderatswahlen_data_recoded)

# Creating new dataframe with selected vars ----
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_1998_gemeinderatswahlen_data_recoded <-
  brandenburg_1998_gemeinderatswahlen_data_recoded %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
brandenburg_1998_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_1998_gemeinderatswahlen_data_recoded$Wähler /
  brandenburg_1998_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 2003 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2003_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/brandenburg/brandenburg_2003.xlsx",
  sheet = "Ergebnis"
))
names(brandenburg_2003_gemeinderatswahlen_data) <- str_replace_all(
  names(brandenburg_2003_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recode to split by party vote ----
brandenburg_2003_gemeinderatswahlen_data_recoded <-
  brandenburg_2003_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(
    Wahlberechtigte = mean(Wahlberechtigte),
    Wähler = mean(Wähler),
    gültigeStimmen = mean(gültigeStimmen),
    Gebietsname = unique(Gemeindename)
  )
brandenburg_2003_gemeinderatswahlen_data_recoded <- as.data.frame(
  brandenburg_2003_gemeinderatswahlen_data_recoded
)

# CDU
brandenburg_2003_gemeinderatswahlen_data_CDU <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger == "CDU"
]
brandenburg_2003_gemeinderatswahlen_data_CDU <- brandenburg_2003_gemeinderatswahlen_data_CDU[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2003_gemeinderatswahlen_data_CDU) == "Stimmen"
] <- "CDU"
names(brandenburg_2003_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2003_gemeinderatswahlen_data_CDU) == "Sitze"
] <- "sitze_CDU"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_CDU,
  by = c('AGS'),
  all.x = TRUE
)

# SPD
brandenburg_2003_gemeinderatswahlen_data_SPD <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger == "SPD"
]
brandenburg_2003_gemeinderatswahlen_data_SPD <- brandenburg_2003_gemeinderatswahlen_data_SPD[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2003_gemeinderatswahlen_data_SPD) == "Stimmen"
] <- "SPD"
names(brandenburg_2003_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2003_gemeinderatswahlen_data_SPD) == "Sitze"
] <- "sitze_SPD"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_SPD,
  by = c('AGS'),
  all.x = TRUE
)

# FDP
brandenburg_2003_gemeinderatswahlen_data_FDP <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger == "FDP"
]
brandenburg_2003_gemeinderatswahlen_data_FDP <- brandenburg_2003_gemeinderatswahlen_data_FDP[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2003_gemeinderatswahlen_data_FDP) == "Stimmen"
] <- "FDP"
names(brandenburg_2003_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2003_gemeinderatswahlen_data_FDP) == "Sitze"
] <- "sitze_FDP"

brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_FDP,
  by = c('AGS'),
  all.x = TRUE
)

# DIELINKE
brandenburg_2003_gemeinderatswahlen_data_DIELINKE <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger == "PDS"
]
brandenburg_2003_gemeinderatswahlen_data_DIELINKE <- brandenburg_2003_gemeinderatswahlen_data_DIELINKE[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE) == "Stimmen"
] <- "DIELINKE"
names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE) == "Sitze"
] <- "sitze_DIELINKE"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_DIELINKE,
  by = c('AGS'),
  all.x = TRUE
)

# GRUENE
brandenburg_2003_gemeinderatswahlen_data_GRUENE <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger == "GRÜNE/B 90"
]
brandenburg_2003_gemeinderatswahlen_data_GRUENE <- brandenburg_2003_gemeinderatswahlen_data_GRUENE[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2003_gemeinderatswahlen_data_GRUENE) == "Stimmen"
] <- "GRUENE"
names(brandenburg_2003_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2003_gemeinderatswahlen_data_GRUENE) == "Sitze"
] <- "sitze_GRUENE"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_GRUENE,
  by = c('AGS'),
  all.x = TRUE
)

# Wählergruppen
brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger ==
    "Wählergruppen"
]
brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Stimmen"
] <- "WAEHLERGRUPPEN"
names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"
] <- "sitze_WAEHLERGRUPPEN"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN,
  by = c('AGS'),
  all.x = TRUE
)

# Einzelbewerber
brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2003_gemeinderatswahlen_data[
  brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger ==
    "Einzelbewerber"
]
brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER) == "Stimmen"
] <- "EINZELBEWERBER"
names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"
] <- "sitze_EINZELBEWERBER"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER,
  by = c('AGS'),
  all.x = TRUE
)

brandenburg_2003_gemeinderatswahlen_data_recoded <- as.data.table(
  brandenburg_2003_gemeinderatswahlen_data_recoded
)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2003_gemeinderatswahlen_data_recoded[, AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2003_gemeinderatswahlen_data_recoded[, Bundesland := "Brandenburg"]
brandenburg_2003_gemeinderatswahlen_data_recoded[, election_year := "2003"]
brandenburg_2003_gemeinderatswahlen_data_recoded[,
  election_type := "Kommunalwahlen"
]
brandenburg_2003_gemeinderatswahlen_data_recoded[, IDIRB := ""]
brandenburg_2003_gemeinderatswahlen_data_recoded[, IDBA := ""]

# Renaming existing variables ----
brandenburg_2003_gemeinderatswahlen_data_recoded$AGS_8dig <- brandenburg_2003_gemeinderatswahlen_data_recoded$AGS
brandenburg_2003_gemeinderatswahlen_data_recoded$Gebietsname <- brandenburg_2003_gemeinderatswahlen_data_recoded$Gebietsname
brandenburg_2003_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- brandenburg_2003_gemeinderatswahlen_data_recoded$Wahlberechtigte
brandenburg_2003_gemeinderatswahlen_data_recoded$Wähler <- brandenburg_2003_gemeinderatswahlen_data_recoded$Wähler
brandenburg_2003_gemeinderatswahlen_data_recoded$GültigeStimmen <- brandenburg_2003_gemeinderatswahlen_data_recoded$gültigeStimmen

brandenburg_2003_gemeinderatswahlen_data_recoded$abs_CDU <- brandenburg_2003_gemeinderatswahlen_data_recoded$CDU
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_SPD <- brandenburg_2003_gemeinderatswahlen_data_recoded$SPD
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_DIELINKE <- brandenburg_2003_gemeinderatswahlen_data_recoded$DIELINKE
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_GRÜNE <- brandenburg_2003_gemeinderatswahlen_data_recoded$GRUENE
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_AfD <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_PIRATEN <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_FDP <- brandenburg_2003_gemeinderatswahlen_data_recoded$FDP
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- brandenburg_2003_gemeinderatswahlen_data_recoded$`EINZELBEWERBER`
brandenburg_2003_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- brandenburg_2003_gemeinderatswahlen_data_recoded$`WAEHLERGRUPPEN`

brandenburg_2003_gemeinderatswahlen_data_recoded$gew_CDU <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_SPD <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_DIELINKE <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_GRÜNE <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_AfD <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_PIRATEN <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_FDP <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_DiePARTEI <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_FREIEWÄHLER <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_Gemeinsame_Wahlvorschläge <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$gew_Wählergruppen <- NA

brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_CDU <- brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_CDU
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_SPD <- brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_SPD
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_DIELINKE <- brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_DIELINKE
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_GRÜNE <- brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_GRUENE
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_AfD <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_PIRATEN <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_FDP <- brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_FDP
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_DiePARTEI <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER <- NA
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_Gemeinsame_Wahlvorschläge <- brandenburg_2003_gemeinderatswahlen_data_recoded$`sitze_WAEHLERGRUPPEN`
brandenburg_2003_gemeinderatswahlen_data_recoded$sitze_Wählergruppen <- brandenburg_2003_gemeinderatswahlen_data_recoded$`sitze_EINZELBEWERBER`

names(brandenburg_2003_gemeinderatswahlen_data_recoded)

# Creating new dataframe with selected vars ----
brandenburg_2003_gemeinderatswahlen_data_recoded <- brandenburg_2003_gemeinderatswahlen_data_recoded[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2003_gemeinderatswahlen_data_recoded <-
  brandenburg_2003_gemeinderatswahlen_data_recoded %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
brandenburg_2003_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2003_gemeinderatswahlen_data_recoded$Wähler /
  brandenburg_2003_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 2008 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2008_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/brandenburg/brandenburg_2008.xlsx",
  sheet = "Ergebnis"
))
names(brandenburg_2008_gemeinderatswahlen_data) <- str_replace_all(
  names(brandenburg_2008_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recode to split by party vote ----
brandenburg_2008_gemeinderatswahlen_data_recoded <-
  brandenburg_2008_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(
    Wahlberechtigte = mean(Wahlberechtigte),
    Wähler = mean(Wähler),
    gültigeStimmen = mean(gültigeStimmen),
    Gebietsname = unique(Gemeindename)
  )
brandenburg_2008_gemeinderatswahlen_data_recoded <- as.data.frame(
  brandenburg_2008_gemeinderatswahlen_data_recoded
)

# CDU
brandenburg_2008_gemeinderatswahlen_data_CDU <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger == "CDU"
]
brandenburg_2008_gemeinderatswahlen_data_CDU <- brandenburg_2008_gemeinderatswahlen_data_CDU[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2008_gemeinderatswahlen_data_CDU) == "Stimmen"
] <- "CDU"
names(brandenburg_2008_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2008_gemeinderatswahlen_data_CDU) == "Sitze"
] <- "sitze_CDU"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_CDU,
  by = c('AGS'),
  all.x = TRUE
)

# SPD
brandenburg_2008_gemeinderatswahlen_data_SPD <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger == "SPD"
]
brandenburg_2008_gemeinderatswahlen_data_SPD <- brandenburg_2008_gemeinderatswahlen_data_SPD[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2008_gemeinderatswahlen_data_SPD) == "Stimmen"
] <- "SPD"
names(brandenburg_2008_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2008_gemeinderatswahlen_data_SPD) == "Sitze"
] <- "sitze_SPD"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_SPD,
  by = c('AGS'),
  all.x = TRUE
)

# FDP
brandenburg_2008_gemeinderatswahlen_data_FDP <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger == "FDP"
]
brandenburg_2008_gemeinderatswahlen_data_FDP <- brandenburg_2008_gemeinderatswahlen_data_FDP[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2008_gemeinderatswahlen_data_FDP) == "Stimmen"
] <- "FDP"
names(brandenburg_2008_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2008_gemeinderatswahlen_data_FDP) == "Sitze"
] <- "sitze_FDP"

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_FDP,
  by = c('AGS'),
  all.x = TRUE
)

# DIELINKE
brandenburg_2008_gemeinderatswahlen_data_DIELINKE <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger == "DIE LINKE"
]
brandenburg_2008_gemeinderatswahlen_data_DIELINKE <- brandenburg_2008_gemeinderatswahlen_data_DIELINKE[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE) == "Stimmen"
] <- "DIELINKE"
names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE) == "Sitze"
] <- "sitze_DIELINKE"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_DIELINKE,
  by = c('AGS'),
  all.x = TRUE
)

# GRUENE
brandenburg_2008_gemeinderatswahlen_data_GRUENE <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger == "GRÜNE/B 90"
]
brandenburg_2008_gemeinderatswahlen_data_GRUENE <- brandenburg_2008_gemeinderatswahlen_data_GRUENE[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2008_gemeinderatswahlen_data_GRUENE) == "Stimmen"
] <- "GRUENE"
names(brandenburg_2008_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2008_gemeinderatswahlen_data_GRUENE) == "Sitze"
] <- "sitze_GRUENE"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_GRUENE,
  by = c('AGS'),
  all.x = TRUE
)

# FW
brandenburg_2008_gemeinderatswahlen_data_FW <- dplyr::filter(
  brandenburg_2008_gemeinderatswahlen_data,
  grepl('Freie Wähler|FW|Freie WG', Wahlvorschlagsträger)
)
brandenburg_2008_gemeinderatswahlen_data_FW <- brandenburg_2008_gemeinderatswahlen_data_FW[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_FW)[
  names(brandenburg_2008_gemeinderatswahlen_data_FW) == "Stimmen"
] <- "FREIEWÄHLER"
names(brandenburg_2008_gemeinderatswahlen_data_FW)[
  names(brandenburg_2008_gemeinderatswahlen_data_FW) == "Sitze"
] <- "sitze_FREIEWÄHLER"

brandenburg_2008_gemeinderatswahlen_data_FW <- brandenburg_2008_gemeinderatswahlen_data_FW %>%
  group_by(AGS) %>%
  mutate(
    FREIEWÄHLER = sum(FREIEWÄHLER, na.rm = T),
    sitze_FREIEWÄHLER = sum(sitze_FREIEWÄHLER, na.rm = T)
  ) %>%
  distinct()

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_FW,
  by = c('AGS'),
  all.x = TRUE
)

# Wählergruppen

brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$`ArtdesWahl-vorschlags-trägers` ==
    "WG"
]
brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Stimmen"
] <- "WAEHLERGRUPPEN"
names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"
] <- "sitze_WAEHLERGRUPPEN"

brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN %>%
  group_by(AGS) %>%
  mutate(
    WAEHLERGRUPPEN = sum(WAEHLERGRUPPEN, na.rm = T),
    sitze_WAEHLERGRUPPEN = sum(sitze_WAEHLERGRUPPEN, na.rm = T)
  ) %>%
  distinct()

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN,
  by = c('AGS'),
  all.x = TRUE
)

# Einzelbewerber
brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2008_gemeinderatswahlen_data[
  brandenburg_2008_gemeinderatswahlen_data$`ArtdesWahl-vorschlags-trägers` ==
    "EB"
]
brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER) == "Stimmen"
] <- "EINZELBEWERBER"
names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"
] <- "sitze_EINZELBEWERBER"

brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER %>%
  group_by(AGS) %>%
  mutate(
    EINZELBEWERBER = sum(EINZELBEWERBER, na.rm = T),
    sitze_EINZELBEWERBER = sum(sitze_EINZELBEWERBER, na.rm = T)
  ) %>%
  distinct()

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER,
  by = c('AGS'),
  all.x = TRUE
)

brandenburg_2008_gemeinderatswahlen_data_recoded <- as.data.table(
  brandenburg_2008_gemeinderatswahlen_data_recoded
)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2008_gemeinderatswahlen_data_recoded[, AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2008_gemeinderatswahlen_data_recoded[, Bundesland := "Brandenburg"]
brandenburg_2008_gemeinderatswahlen_data_recoded[, election_year := "2008"]
brandenburg_2008_gemeinderatswahlen_data_recoded[,
  election_type := "Kommunalwahlen"
]
brandenburg_2008_gemeinderatswahlen_data_recoded[, IDIRB := ""]
brandenburg_2008_gemeinderatswahlen_data_recoded[, IDBA := ""]

# Renaming existing variables ----
brandenburg_2008_gemeinderatswahlen_data_recoded$AGS_8dig <- brandenburg_2008_gemeinderatswahlen_data_recoded$AGS
brandenburg_2008_gemeinderatswahlen_data_recoded$Gebietsname <- brandenburg_2008_gemeinderatswahlen_data_recoded$Gebietsname
brandenburg_2008_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- brandenburg_2008_gemeinderatswahlen_data_recoded$Wahlberechtigte
brandenburg_2008_gemeinderatswahlen_data_recoded$Wähler <- brandenburg_2008_gemeinderatswahlen_data_recoded$Wähler
brandenburg_2008_gemeinderatswahlen_data_recoded$GültigeStimmen <- brandenburg_2008_gemeinderatswahlen_data_recoded$gültigeStimmen

brandenburg_2008_gemeinderatswahlen_data_recoded$abs_CDU <- brandenburg_2008_gemeinderatswahlen_data_recoded$CDU
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_SPD <- brandenburg_2008_gemeinderatswahlen_data_recoded$SPD
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_DIELINKE <- brandenburg_2008_gemeinderatswahlen_data_recoded$DIELINKE
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_GRÜNE <- brandenburg_2008_gemeinderatswahlen_data_recoded$GRUENE
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_AfD <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_PIRATEN <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_FDP <- brandenburg_2008_gemeinderatswahlen_data_recoded$FDP
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- brandenburg_2008_gemeinderatswahlen_data_recoded$FREIEWÄHLER
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- brandenburg_2008_gemeinderatswahlen_data_recoded$`EINZELBEWERBER`
brandenburg_2008_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- brandenburg_2008_gemeinderatswahlen_data_recoded$`WAEHLERGRUPPEN`

brandenburg_2008_gemeinderatswahlen_data_recoded$gew_CDU <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_SPD <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_DIELINKE <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_GRÜNE <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_AfD <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_PIRATEN <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_FDP <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_DiePARTEI <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_FREIEWÄHLER <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_Gemeinsame_Wahlvorschläge <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$gew_Wählergruppen <- NA

brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_CDU <- brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_CDU
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_SPD <- brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_SPD
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_DIELINKE <- brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_DIELINKE
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_GRÜNE <- brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_GRUENE
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_AfD <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_PIRATEN <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_FDP <- brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_FDP
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_DiePARTEI <- NA
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER <- brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_Gemeinsame_Wahlvorschläge <- brandenburg_2008_gemeinderatswahlen_data_recoded$`sitze_EINZELBEWERBER`
brandenburg_2008_gemeinderatswahlen_data_recoded$sitze_Wählergruppen <- brandenburg_2008_gemeinderatswahlen_data_recoded$`sitze_WAEHLERGRUPPEN`

names(brandenburg_2008_gemeinderatswahlen_data_recoded)

# Creating new dataframe with selected vars ----
brandenburg_2008_gemeinderatswahlen_data_recoded <- brandenburg_2008_gemeinderatswahlen_data_recoded[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2008_gemeinderatswahlen_data_recoded <-
  brandenburg_2008_gemeinderatswahlen_data_recoded %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
brandenburg_2008_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2008_gemeinderatswahlen_data_recoded$Wähler /
  brandenburg_2008_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 2014 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2014_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/brandenburg/brandenburg_2014.xlsx",
  sheet = "Ergebnis"
))
names(brandenburg_2014_gemeinderatswahlen_data) <- str_replace_all(
  names(brandenburg_2014_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recode to split by party vote ----
brandenburg_2014_gemeinderatswahlen_data_recoded <-
  brandenburg_2014_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(
    Wahlberechtigte = mean(Wahlberechtigte),
    Wähler = mean(Wähler),
    gültigeStimmen = mean(GültigeStimmen),
    Gebietsname = unique(Gemeindename)
  )
brandenburg_2014_gemeinderatswahlen_data_recoded <- as.data.frame(
  brandenburg_2014_gemeinderatswahlen_data_recoded
)

# CDU
brandenburg_2014_gemeinderatswahlen_data_CDU <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz == "CDU"
]
brandenburg_2014_gemeinderatswahlen_data_CDU <- brandenburg_2014_gemeinderatswahlen_data_CDU[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2014_gemeinderatswahlen_data_CDU) == "Stimmen"
] <- "CDU"
names(brandenburg_2014_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2014_gemeinderatswahlen_data_CDU) == "Sitze"
] <- "sitze_CDU"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_CDU,
  by = c('AGS'),
  all.x = TRUE
)

# SPD
brandenburg_2014_gemeinderatswahlen_data_SPD <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz == "SPD"
]
brandenburg_2014_gemeinderatswahlen_data_SPD <- brandenburg_2014_gemeinderatswahlen_data_SPD[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2014_gemeinderatswahlen_data_SPD) == "Stimmen"
] <- "SPD"
names(brandenburg_2014_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2014_gemeinderatswahlen_data_SPD) == "Sitze"
] <- "sitze_SPD"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_SPD,
  by = c('AGS'),
  all.x = TRUE
)

# FDP
brandenburg_2014_gemeinderatswahlen_data_FDP <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz == "FDP"
]
brandenburg_2014_gemeinderatswahlen_data_FDP <- brandenburg_2014_gemeinderatswahlen_data_FDP[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2014_gemeinderatswahlen_data_FDP) == "Stimmen"
] <- "FDP"
names(brandenburg_2014_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2014_gemeinderatswahlen_data_FDP) == "Sitze"
] <- "sitze_FDP"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_FDP,
  by = c('AGS'),
  all.x = TRUE
)

# DiePARTEI
brandenburg_2014_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz ==
    "Die PARTEI"
]
brandenburg_2014_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2014_gemeinderatswahlen_data_DiePARTEI[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI)[
  names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI) == "Stimmen"
] <- "DiePARTEI"
names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI)[
  names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI) == "Sitze"
] <- "sitze_DiePARTEI"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_DiePARTEI,
  by = c('AGS'),
  all.x = TRUE
)

# DIELINKE
brandenburg_2014_gemeinderatswahlen_data_DIELINKE <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz ==
    "DIE LINKE"
]
brandenburg_2014_gemeinderatswahlen_data_DIELINKE <- brandenburg_2014_gemeinderatswahlen_data_DIELINKE[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE) == "Stimmen"
] <- "DIELINKE"
names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE) == "Sitze"
] <- "sitze_DIELINKE"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_DIELINKE,
  by = c('AGS'),
  all.x = TRUE
)

# AfD
brandenburg_2014_gemeinderatswahlen_data_AfD <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz == "AfD"
]
brandenburg_2014_gemeinderatswahlen_data_AfD <- brandenburg_2014_gemeinderatswahlen_data_AfD[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_AfD)[
  names(brandenburg_2014_gemeinderatswahlen_data_AfD) == "Stimmen"
] <- "AfD"
names(brandenburg_2014_gemeinderatswahlen_data_AfD)[
  names(brandenburg_2014_gemeinderatswahlen_data_AfD) == "Sitze"
] <- "sitze_AfD"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_AfD,
  by = c('AGS'),
  all.x = TRUE
)

# PIRATEN
brandenburg_2014_gemeinderatswahlen_data_PIRATEN <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz ==
    "PIRATEN"
]
brandenburg_2014_gemeinderatswahlen_data_PIRATEN <- brandenburg_2014_gemeinderatswahlen_data_PIRATEN[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN)[
  names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN) == "Stimmen"
] <- "PIRATEN"
names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN)[
  names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN) == "Sitze"
] <- "sitze_PIRATEN"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_PIRATEN,
  by = c('AGS'),
  all.x = TRUE
)

# GRUENE
brandenburg_2014_gemeinderatswahlen_data_GRUENE <- dplyr::filter(
  brandenburg_2014_gemeinderatswahlen_data,
  grepl('GRÜNEN', Wahlvorschlagsträger_Kurz)
)
brandenburg_2014_gemeinderatswahlen_data_GRUENE <- brandenburg_2014_gemeinderatswahlen_data_GRUENE[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2014_gemeinderatswahlen_data_GRUENE) == "Stimmen"
] <- "GRUENE"
names(brandenburg_2014_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2014_gemeinderatswahlen_data_GRUENE) == "Sitze"
] <- "sitze_GRUENE"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_GRUENE,
  by = c('AGS'),
  all.x = TRUE
)

# FW
brandenburg_2014_gemeinderatswahlen_data_FW <- dplyr::filter(
  brandenburg_2014_gemeinderatswahlen_data,
  grepl('Freie Wähler|FW|Freie WG', Wahlvorschlagsträger_Kurz)
)
brandenburg_2014_gemeinderatswahlen_data_FW <- brandenburg_2014_gemeinderatswahlen_data_FW[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_FW)[
  names(brandenburg_2014_gemeinderatswahlen_data_FW) == "Stimmen"
] <- "FREIEWÄHLER"
names(brandenburg_2014_gemeinderatswahlen_data_FW)[
  names(brandenburg_2014_gemeinderatswahlen_data_FW) == "Sitze"
] <- "sitze_FREIEWÄHLER"

brandenburg_2014_gemeinderatswahlen_data_FW <- brandenburg_2014_gemeinderatswahlen_data_FW %>%
  group_by(AGS) %>%
  mutate(
    FREIEWÄHLER = sum(FREIEWÄHLER, na.rm = T),
    sitze_FREIEWÄHLER = sum(sitze_FREIEWÄHLER, na.rm = T)
  ) %>%
  distinct()

brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_FW,
  by = c('AGS'),
  all.x = TRUE
)

# Wählergruppen
brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$`ArtdesWahlvorschlags-trägers` ==
    "WG"
]
brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Stimmen"
] <- "WAEHLERGRUPPEN"
names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"
] <- "sitze_WAEHLERGRUPPEN"

brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN %>%
  group_by(AGS) %>%
  mutate(
    WAEHLERGRUPPEN = sum(WAEHLERGRUPPEN, na.rm = T),
    sitze_WAEHLERGRUPPEN = sum(sitze_WAEHLERGRUPPEN, na.rm = T)
  ) %>%
  distinct()

brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN,
  by = c('AGS'),
  all.x = TRUE
)

# Einzelbewerber
brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2014_gemeinderatswahlen_data[
  brandenburg_2014_gemeinderatswahlen_data$`ArtdesWahlvorschlags-trägers` ==
    "EB"
]
brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER[, c(
  'AGS',
  'Stimmen',
  'Sitze'
)]
names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER) == "Stimmen"
] <- "EINZELBEWERBER"
names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"
] <- "sitze_EINZELBEWERBER"

brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER %>%
  group_by(AGS) %>%
  mutate(
    EINZELBEWERBER = sum(EINZELBEWERBER, na.rm = T),
    sitze_EINZELBEWERBER = sum(sitze_EINZELBEWERBER, na.rm = T)
  ) %>%
  distinct()

brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER,
  by = c('AGS'),
  all.x = TRUE
)

brandenburg_2014_gemeinderatswahlen_data_recoded <- as.data.table(
  brandenburg_2014_gemeinderatswahlen_data_recoded
)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2014_gemeinderatswahlen_data_recoded[, AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2014_gemeinderatswahlen_data_recoded[, Bundesland := "Brandenburg"]
brandenburg_2014_gemeinderatswahlen_data_recoded[, election_year := "2014"]
brandenburg_2014_gemeinderatswahlen_data_recoded[,
  election_type := "Kommunalwahlen"
]
brandenburg_2014_gemeinderatswahlen_data_recoded[, IDIRB := ""]
brandenburg_2014_gemeinderatswahlen_data_recoded[, IDBA := ""]

# Renaming existing variables ----
brandenburg_2014_gemeinderatswahlen_data_recoded$AGS_8dig <- brandenburg_2014_gemeinderatswahlen_data_recoded$AGS
brandenburg_2014_gemeinderatswahlen_data_recoded$Gebietsname <- brandenburg_2014_gemeinderatswahlen_data_recoded$Gebietsname
brandenburg_2014_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- brandenburg_2014_gemeinderatswahlen_data_recoded$Wahlberechtigte
brandenburg_2014_gemeinderatswahlen_data_recoded$Wähler <- brandenburg_2014_gemeinderatswahlen_data_recoded$Wähler
brandenburg_2014_gemeinderatswahlen_data_recoded$GültigeStimmen <- brandenburg_2014_gemeinderatswahlen_data_recoded$gültigeStimmen

brandenburg_2014_gemeinderatswahlen_data_recoded$abs_CDU <- brandenburg_2014_gemeinderatswahlen_data_recoded$CDU
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_SPD <- brandenburg_2014_gemeinderatswahlen_data_recoded$SPD
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_DIELINKE <- brandenburg_2014_gemeinderatswahlen_data_recoded$DIELINKE
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_GRÜNE <- brandenburg_2014_gemeinderatswahlen_data_recoded$GRUENE
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_AfD <- brandenburg_2014_gemeinderatswahlen_data_recoded$AfD
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_PIRATEN <- brandenburg_2014_gemeinderatswahlen_data_recoded$PIRATEN
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_FDP <- brandenburg_2014_gemeinderatswahlen_data_recoded$FDP
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- brandenburg_2014_gemeinderatswahlen_data_recoded$DiePARTEI
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- brandenburg_2014_gemeinderatswahlen_data_recoded$FREIEWÄHLER
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- brandenburg_2014_gemeinderatswahlen_data_recoded$`EINZELBEWERBER`
brandenburg_2014_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- brandenburg_2014_gemeinderatswahlen_data_recoded$`WAEHLERGRUPPEN`

brandenburg_2014_gemeinderatswahlen_data_recoded$gew_CDU <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_SPD <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_DIELINKE <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_GRÜNE <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_AfD <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_PIRATEN <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_FDP <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_DiePARTEI <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_FREIEWÄHLER <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_Gemeinsame_Wahlvorschläge <- NA
brandenburg_2014_gemeinderatswahlen_data_recoded$gew_Wählergruppen <- NA

brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_CDU <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_CDU
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_SPD <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_SPD
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_DIELINKE <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_DIELINKE
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_GRÜNE <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_GRUENE
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_AfD <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_AfD
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_PIRATEN <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_PIRATEN
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_FDP <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_FDP
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_DiePARTEI <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_DiePARTEI
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER <- brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_Gemeinsame_Wahlvorschläge <- brandenburg_2014_gemeinderatswahlen_data_recoded$`sitze_WAEHLERGRUPPEN`
brandenburg_2014_gemeinderatswahlen_data_recoded$sitze_Wählergruppen <- brandenburg_2014_gemeinderatswahlen_data_recoded$`sitze_EINZELBEWERBER`


# Creating new dataframe with selected vars ----
brandenburg_2014_gemeinderatswahlen_data_recoded <- brandenburg_2014_gemeinderatswahlen_data_recoded[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2014_gemeinderatswahlen_data_recoded <-
  brandenburg_2014_gemeinderatswahlen_data_recoded %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
brandenburg_2014_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2014_gemeinderatswahlen_data_recoded$Wähler /
  brandenburg_2014_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt

###### Brandenburg 2019 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2019_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/brandenburg/brandenburg_2019.xlsx",
  sheet = "BB_GVW2019"
))
names(brandenburg_2019_gemeinderatswahlen_data) <- str_replace_all(
  names(brandenburg_2019_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recode to split by party vote ----
brandenburg_2019_gemeinderatswahlen_data_recoded <-
  brandenburg_2019_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(Gebietsname = unique(Gemeindename))
brandenburg_2019_gemeinderatswahlen_data_recoded <- as.data.frame(
  brandenburg_2019_gemeinderatswahlen_data_recoded
)

# Wahlberechtigte
brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "Wahlberechtigt"
]
brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte <- brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte[, c(
  'AGS',
  'Anzahl'
)]
names(brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte)[
  names(brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte) == "Anzahl"
] <- "Wahlberechtigte"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte,
  by = c('AGS'),
  all.x = TRUE
)

# Wähler
brandenburg_2019_gemeinderatswahlen_data_Wähler <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "Wähler"
]
brandenburg_2019_gemeinderatswahlen_data_Wähler <- brandenburg_2019_gemeinderatswahlen_data_Wähler[, c(
  'AGS',
  'Anzahl'
)]
names(brandenburg_2019_gemeinderatswahlen_data_Wähler)[
  names(brandenburg_2019_gemeinderatswahlen_data_Wähler) == "Anzahl"
] <- "Wähler"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_Wähler,
  by = c('AGS'),
  all.x = TRUE
)

# gültigeStimmen
brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "Gueltig"
]
brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen <- brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen[, c(
  'AGS',
  'Anzahl'
)]
names(brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen)[
  names(brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen) == "Anzahl"
] <- "gültigeStimmen"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen,
  by = c('AGS'),
  all.x = TRUE
)

# CDU
brandenburg_2019_gemeinderatswahlen_data_CDU <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "CDU"
]
brandenburg_2019_gemeinderatswahlen_data_CDU <- brandenburg_2019_gemeinderatswahlen_data_CDU[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2019_gemeinderatswahlen_data_CDU) == "Anzahl"
] <- "CDU"
names(brandenburg_2019_gemeinderatswahlen_data_CDU)[
  names(brandenburg_2019_gemeinderatswahlen_data_CDU) == "Sitze"
] <- "sitze_CDU"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_CDU,
  by = c('AGS'),
  all.x = TRUE
)

# SPD
brandenburg_2019_gemeinderatswahlen_data_SPD <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "SPD"
]
brandenburg_2019_gemeinderatswahlen_data_SPD <- brandenburg_2019_gemeinderatswahlen_data_SPD[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2019_gemeinderatswahlen_data_SPD) == "Anzahl"
] <- "SPD"
names(brandenburg_2019_gemeinderatswahlen_data_SPD)[
  names(brandenburg_2019_gemeinderatswahlen_data_SPD) == "Sitze"
] <- "sitze_SPD"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_SPD,
  by = c('AGS'),
  all.x = TRUE
)

# FDP
brandenburg_2019_gemeinderatswahlen_data_FDP <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "FDP"
]
brandenburg_2019_gemeinderatswahlen_data_FDP <- brandenburg_2019_gemeinderatswahlen_data_FDP[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2019_gemeinderatswahlen_data_FDP) == "Anzahl"
] <- "FDP"
names(brandenburg_2019_gemeinderatswahlen_data_FDP)[
  names(brandenburg_2019_gemeinderatswahlen_data_FDP) == "Sitze"
] <- "sitze_FDP"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_FDP,
  by = c('AGS'),
  all.x = TRUE
)

# DiePARTEI
brandenburg_2019_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "Die PARTEI"
]
brandenburg_2019_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2019_gemeinderatswahlen_data_DiePARTEI[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI)[
  names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI) == "Anzahl"
] <- "DiePARTEI"
names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI)[
  names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI) == "Sitze"
] <- "sitze_DiePARTEI"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_DiePARTEI,
  by = c('AGS'),
  all.x = TRUE
)

# DIELINKE
brandenburg_2019_gemeinderatswahlen_data_DIELINKE <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "DIE LINKE"
]
brandenburg_2019_gemeinderatswahlen_data_DIELINKE <- brandenburg_2019_gemeinderatswahlen_data_DIELINKE[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE) == "Anzahl"
] <- "DIELINKE"
names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE)[
  names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE) == "Sitze"
] <- "sitze_DIELINKE"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_DIELINKE,
  by = c('AGS'),
  all.x = TRUE
)

# AfD
brandenburg_2019_gemeinderatswahlen_data_AfD <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "AfD"
]
brandenburg_2019_gemeinderatswahlen_data_AfD <- brandenburg_2019_gemeinderatswahlen_data_AfD[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_AfD)[
  names(brandenburg_2019_gemeinderatswahlen_data_AfD) == "Anzahl"
] <- "AfD"
names(brandenburg_2019_gemeinderatswahlen_data_AfD)[
  names(brandenburg_2019_gemeinderatswahlen_data_AfD) == "Sitze"
] <- "sitze_AfD"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_AfD,
  by = c('AGS'),
  all.x = TRUE
)

# PIRATEN
brandenburg_2019_gemeinderatswahlen_data_PIRATEN <- brandenburg_2019_gemeinderatswahlen_data[
  brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname == "PIRATEN"
]
brandenburg_2019_gemeinderatswahlen_data_PIRATEN <- brandenburg_2019_gemeinderatswahlen_data_PIRATEN[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN)[
  names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN) == "Anzahl"
] <- "PIRATEN"
names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN)[
  names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN) == "Sitze"
] <- "sitze_PIRATEN"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_PIRATEN,
  by = c('AGS'),
  all.x = TRUE
)

# GRUENE
brandenburg_2019_gemeinderatswahlen_data_GRUENE <- dplyr::filter(
  brandenburg_2019_gemeinderatswahlen_data,
  grepl('GRÜNE', Merkmal_Kurzname)
)
brandenburg_2019_gemeinderatswahlen_data_GRUENE <- brandenburg_2019_gemeinderatswahlen_data_GRUENE[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2019_gemeinderatswahlen_data_GRUENE) == "Anzahl"
] <- "GRUENE"
names(brandenburg_2019_gemeinderatswahlen_data_GRUENE)[
  names(brandenburg_2019_gemeinderatswahlen_data_GRUENE) == "Sitze"
] <- "sitze_GRUENE"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_GRUENE,
  by = c('AGS'),
  all.x = TRUE
)

# FW
brandenburg_2019_gemeinderatswahlen_data_FW <- dplyr::filter(
  brandenburg_2019_gemeinderatswahlen_data,
  grepl('Freie Wähler|FW|Freie WG', Merkmal_Kurzname)
)
brandenburg_2019_gemeinderatswahlen_data_FW <- brandenburg_2019_gemeinderatswahlen_data_FW[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_FW)[
  names(brandenburg_2019_gemeinderatswahlen_data_FW) == "Anzahl"
] <- "FREIEWÄHLER"
names(brandenburg_2019_gemeinderatswahlen_data_FW)[
  names(brandenburg_2019_gemeinderatswahlen_data_FW) == "Sitze"
] <- "sitze_FREIEWÄHLER"

brandenburg_2019_gemeinderatswahlen_data_FW <- brandenburg_2019_gemeinderatswahlen_data_FW %>%
  group_by(AGS) %>%
  mutate(
    FREIEWÄHLER = sum(FREIEWÄHLER, na.rm = T),
    sitze_FREIEWÄHLER = sum(sitze_FREIEWÄHLER, na.rm = T)
  ) %>%
  distinct()

brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_FW,
  by = c('AGS'),
  all.x = TRUE
)

# Wählergruppen
brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN <- dplyr::filter(
  brandenburg_2019_gemeinderatswahlen_data,
  grepl('WG', `ArtdesWahl-vorschlags`)
)
brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Anzahl"
] <- "WAEHLERGRUPPEN"
names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN)[
  names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"
] <- "sitze_WAEHLERGRUPPEN"

brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN %>%
  group_by(AGS) %>%
  mutate(
    WAEHLERGRUPPEN = sum(WAEHLERGRUPPEN, na.rm = T),
    sitze_WAEHLERGRUPPEN = sum(sitze_WAEHLERGRUPPEN, na.rm = T)
  ) %>%
  distinct()

brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN,
  by = c('AGS'),
  all.x = TRUE
)

# Einzelbewerber
brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER <- dplyr::filter(
  brandenburg_2019_gemeinderatswahlen_data,
  grepl('EB', `ArtdesWahl-vorschlags`)
)
brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER[, c(
  'AGS',
  'Anzahl',
  'Sitze'
)]
names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER) == "Anzahl"
] <- "EINZELBEWERBER"
names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER)[
  names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"
] <- "sitze_EINZELBEWERBER"

brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER %>%
  group_by(AGS) %>%
  mutate(
    EINZELBEWERBER = sum(EINZELBEWERBER, na.rm = T),
    sitze_EINZELBEWERBER = sum(sitze_EINZELBEWERBER, na.rm = T)
  ) %>%
  distinct()

brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(
  brandenburg_2019_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER,
  by = c('AGS'),
  all.x = TRUE
)

brandenburg_2019_gemeinderatswahlen_data_recoded <- as.data.table(
  brandenburg_2019_gemeinderatswahlen_data_recoded
)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2019_gemeinderatswahlen_data_recoded[, AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2019_gemeinderatswahlen_data_recoded[, Bundesland := "Brandenburg"]
brandenburg_2019_gemeinderatswahlen_data_recoded[, election_year := "2019"]
brandenburg_2019_gemeinderatswahlen_data_recoded[,
  election_type := "Kommunalwahlen"
]
brandenburg_2019_gemeinderatswahlen_data_recoded[, IDIRB := ""]
brandenburg_2019_gemeinderatswahlen_data_recoded[, IDBA := ""]

# Renaming existing variables ----
brandenburg_2019_gemeinderatswahlen_data_recoded$AGS_8dig <- brandenburg_2019_gemeinderatswahlen_data_recoded$AGS
brandenburg_2019_gemeinderatswahlen_data_recoded$Gebietsname <- brandenburg_2019_gemeinderatswahlen_data_recoded$Gebietsname
brandenburg_2019_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- brandenburg_2019_gemeinderatswahlen_data_recoded$Wahlberechtigte
brandenburg_2019_gemeinderatswahlen_data_recoded$Wähler <- brandenburg_2019_gemeinderatswahlen_data_recoded$Wähler
brandenburg_2019_gemeinderatswahlen_data_recoded$GültigeStimmen <- brandenburg_2019_gemeinderatswahlen_data_recoded$gültigeStimmen

brandenburg_2019_gemeinderatswahlen_data_recoded$abs_CDU <- brandenburg_2019_gemeinderatswahlen_data_recoded$CDU
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_SPD <- brandenburg_2019_gemeinderatswahlen_data_recoded$SPD
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_DIELINKE <- brandenburg_2019_gemeinderatswahlen_data_recoded$DIELINKE
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_GRÜNE <- brandenburg_2019_gemeinderatswahlen_data_recoded$GRUENE
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_AfD <- brandenburg_2019_gemeinderatswahlen_data_recoded$AfD
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_PIRATEN <- brandenburg_2019_gemeinderatswahlen_data_recoded$PIRATEN
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_FDP <- brandenburg_2019_gemeinderatswahlen_data_recoded$FDP
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- brandenburg_2019_gemeinderatswahlen_data_recoded$DiePARTEI
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- brandenburg_2019_gemeinderatswahlen_data_recoded$FREIEWÄHLER
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- brandenburg_2019_gemeinderatswahlen_data_recoded$`EINZELBEWERBER`
brandenburg_2019_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- brandenburg_2019_gemeinderatswahlen_data_recoded$`WAEHLERGRUPPEN`

brandenburg_2019_gemeinderatswahlen_data_recoded$gew_CDU <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_SPD <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_DIELINKE <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_GRÜNE <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_AfD <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_PIRATEN <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_FDP <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_DiePARTEI <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_FREIEWÄHLER <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_Gemeinsame_Wahlvorschläge <- NA
brandenburg_2019_gemeinderatswahlen_data_recoded$gew_Wählergruppen <- NA

brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_CDU <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_CDU
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_SPD <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_SPD
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_DIELINKE <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_DIELINKE
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_GRÜNE <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_GRUENE
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_AfD <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_AfD
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_PIRATEN <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_PIRATEN
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_FDP <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_FDP
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_DiePARTEI <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_DiePARTEI
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER <- brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_FREIEWÄHLER
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_Gemeinsame_Wahlvorschläge <- brandenburg_2019_gemeinderatswahlen_data_recoded$`sitze_WAEHLERGRUPPEN`
brandenburg_2019_gemeinderatswahlen_data_recoded$sitze_Wählergruppen <- brandenburg_2019_gemeinderatswahlen_data_recoded$`sitze_EINZELBEWERBER`


# Creating new dataframe with selected vars ----
brandenburg_2019_gemeinderatswahlen_data_recoded <- brandenburg_2019_gemeinderatswahlen_data_recoded[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2019_gemeinderatswahlen_data_recoded <-
  brandenburg_2019_gemeinderatswahlen_data_recoded %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
brandenburg_2019_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2019_gemeinderatswahlen_data_recoded$Wähler /
  brandenburg_2019_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Brandenburg ----
# Merge
brandenburg_kommunalwahlen <- rbind(
  brandenburg_1993_gemeinderatswahlen_data_recoded,
  brandenburg_1998_gemeinderatswahlen_data_recoded,
  brandenburg_2003_gemeinderatswahlen_data_recoded,
  brandenburg_2008_gemeinderatswahlen_data_recoded,
  brandenburg_2014_gemeinderatswahlen_data_recoded,
  brandenburg_2019_gemeinderatswahlen_data_recoded
)

# Replace - with NA
brandenburg_kommunalwahlen[brandenburg_kommunalwahlen == "-"] <- NA

# Fix AGS
brandenburg_kommunalwahlen$AGS_8dig <- strtrim(
  brandenburg_kommunalwahlen$AGS_8dig,
  8
)

# Save
#write_csv(brandenburg_kommunalwahlen, here::here("output/brandenburg_kommunalwahlen.csv"))

brandenburg_kommunalwahlen %>%
  group_by(election_year) %>%
  summarize(mean = mean(prop_Gemeinsame_Wahlvorschläge, na.rm = T))

######### RLP ----
###### RLP 1994 Gemeinderatswahlen ----
#### Load election data ----
rlp_1994_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_1994_1999.xlsx",
  sheet = "1994"
))

#### Delete white space ----
names(rlp_1994_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_1994_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_1994_gemeinderatswahlen_data_sub <- rlp_1994_gemeinderatswahlen_data

# Creating non-existing variables ----
rlp_1994_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_1994_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_1994_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_1994_gemeinderatswahlen_data_sub[, election_year := "1994"]
rlp_1994_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
rlp_1994_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_1994_gemeinderatswahlen_data_sub[, IDBA := ""]


# Renaming existing variables ----
rlp_1994_gemeinderatswahlen_data_sub$AGS_8dig <- str_extract(
  rlp_1994_gemeinderatswahlen_data_sub$Gebiet,
  "\\d{8}"
)
rlp_1994_gemeinderatswahlen_data_sub$Gebietsname <- sub(
  "^\\d{8}\\s+(.*)$",
  "\\1",
  rlp_1994_gemeinderatswahlen_data_sub$Gebiet
)
rlp_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
rlp_1994_gemeinderatswahlen_data_sub$Wähler <- rlp_1994_gemeinderatswahlen_data_sub$Waehler
#rlp_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1994_gemeinderatswahlen_data_sub$GueltigeStimmen

rlp_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1994_gemeinderatswahlen_data_sub$gueltigeStimmen

rlp_1994_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_1994_gemeinderatswahlen_data_sub$CDU
)
rlp_1994_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_1994_gemeinderatswahlen_data_sub$SPD
)
rlp_1994_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_1994_gemeinderatswahlen_data_sub$DIELINKE
)
rlp_1994_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_1994_gemeinderatswahlen_data_sub$Gruene
)
rlp_1994_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_1994_gemeinderatswahlen_data_sub$fdp
)
rlp_1994_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_1994_gemeinderatswahlen_data_sub$WG
)


rlp_1994_gemeinderatswahlen_data_sub$gew_CDU <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_SPD <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_FDP <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
rlp_1994_gemeinderatswahlen_data_sub$gew_Wählergruppen <- NA

rlp_1994_gemeinderatswahlen_data_sub$sitze_CDU <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_SPD <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_AfD <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_FDP <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
rlp_1994_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- NA


# Creating new dataframe with selected vars ----
rlp_1994_gemeinderatswahlen_data_sub <- rlp_1994_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_1994_gemeinderatswahlen_data_sub <-
  rlp_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('gew')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("gew") & matches("X")),
    list(~ paste(sub("gew_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))


# Calculating turnout ----
rlp_1994_gemeinderatswahlen_data_sub$Turnout <- rlp_1994_gemeinderatswahlen_data_sub$Wähler /
  rlp_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 1999 Gemeinderatswahlen ----
#### Load election data ----
rlp_1999_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_1994_1999.xlsx",
  sheet = "1999"
))

#### Delete white space ----
names(rlp_1999_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_1999_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_1999_gemeinderatswahlen_data_sub <- rlp_1999_gemeinderatswahlen_data

# Creating non-existing variables ----
rlp_1999_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_1999_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_1999_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_1999_gemeinderatswahlen_data_sub[, election_year := "1999"]
rlp_1999_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
rlp_1999_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_1999_gemeinderatswahlen_data_sub[, IDBA := ""]


# Renaming existing variables ----
rlp_1999_gemeinderatswahlen_data_sub$AGS_8dig <- str_extract(
  rlp_1999_gemeinderatswahlen_data_sub$Gebiet,
  "\\d{8}"
)
rlp_1999_gemeinderatswahlen_data_sub$Gebietsname <- sub(
  "^\\d{8}\\s+(.*)$",
  "\\1",
  rlp_1999_gemeinderatswahlen_data_sub$Gebiet
)
rlp_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
rlp_1999_gemeinderatswahlen_data_sub$Wähler <- rlp_1999_gemeinderatswahlen_data_sub$Waehler
#rlp_1999_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1999_gemeinderatswahlen_data_sub$GueltigeStimmen

rlp_1999_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1999_gemeinderatswahlen_data_sub$gueltigeStimmen

rlp_1999_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_1999_gemeinderatswahlen_data_sub$CDU
)
rlp_1999_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_1999_gemeinderatswahlen_data_sub$SPD
)
rlp_1999_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_1999_gemeinderatswahlen_data_sub$DIELINKE
)
rlp_1999_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_1999_gemeinderatswahlen_data_sub$Gruene
)
rlp_1999_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_1999_gemeinderatswahlen_data_sub$fdp
)
rlp_1999_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_1999_gemeinderatswahlen_data_sub$WG
)


rlp_1999_gemeinderatswahlen_data_sub$gew_CDU <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_SPD <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_FDP <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
rlp_1999_gemeinderatswahlen_data_sub$gew_Wählergruppen <- NA

rlp_1999_gemeinderatswahlen_data_sub$sitze_CDU <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_SPD <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_AfD <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_FDP <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
rlp_1999_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- NA


# Creating new dataframe with selected vars ----
rlp_1999_gemeinderatswahlen_data_sub <- rlp_1999_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_1999_gemeinderatswahlen_data_sub <-
  rlp_1999_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
rlp_1999_gemeinderatswahlen_data_sub$Turnout <- rlp_1999_gemeinderatswahlen_data_sub$Wähler /
  rlp_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 2004 Gemeinderatswahlen ----
#### Load election data ----
rlp_2004_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_2004.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(rlp_2004_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_2004_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_2004_gemeinderatswahlen_data_sub <- rlp_2004_gemeinderatswahlen_data

names(rlp_2004_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2004_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_2004_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_2004_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_2004_gemeinderatswahlen_data_sub[, election_year := "2004"]
rlp_2004_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
rlp_2004_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_2004_gemeinderatswahlen_data_sub[, IDBA := ""]


# Renaming existing variables ----
rlp_2004_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2004_gemeinderatswahlen_data_sub$AGS
rlp_2004_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2004_gemeinderatswahlen_data_sub$Gemeindename
rlp_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_2004_gemeinderatswahlen_data_sub$Wahlberechtigte
rlp_2004_gemeinderatswahlen_data_sub$Wähler <- rlp_2004_gemeinderatswahlen_data_sub$Waehler
#rlp_2004_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2004_gemeinderatswahlen_data_sub$GueltigeStimmen

rlp_2004_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2004_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2004_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$CDU_gew
)
rlp_2004_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$SPD_gew
)
rlp_2004_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$DIELINKE_gew
)
rlp_2004_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$GRUENE_gew
)
rlp_2004_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$FDP_gew
)
rlp_2004_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew
)
rlp_2004_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$Waehlergruppen_gew
)


rlp_2004_gemeinderatswahlen_data_sub$gew_CDU <- rlp_2004_gemeinderatswahlen_data_sub$CDU_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_SPD <- rlp_2004_gemeinderatswahlen_data_sub$SPD_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_DIELINKE <- rlp_2004_gemeinderatswahlen_data_sub$DIELINKE_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_GRÜNE <- rlp_2004_gemeinderatswahlen_data_sub$GRUENE_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_FDP <- rlp_2004_gemeinderatswahlen_data_sub$FDP_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew
)
rlp_2004_gemeinderatswahlen_data_sub$gew_Wählergruppen <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$Waehlergruppen_gew
)

rlp_2004_gemeinderatswahlen_data_sub$sitze_CDU <- rlp_2004_gemeinderatswahlen_data_sub$CDU_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_SPD <- rlp_2004_gemeinderatswahlen_data_sub$SPD_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_DIELINKE <- rlp_2004_gemeinderatswahlen_data_sub$DIELINKE_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_GRÜNE <- rlp_2004_gemeinderatswahlen_data_sub$GRUENE_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_AfD <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_FDP <- rlp_2004_gemeinderatswahlen_data_sub$FDP_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze
)
rlp_2004_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(
  rlp_2004_gemeinderatswahlen_data_sub$Waehlergruppen_sitze
)


# Creating new dataframe with selected vars ----
rlp_2004_gemeinderatswahlen_data_sub <- rlp_2004_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2004_gemeinderatswahlen_data_sub <-
  rlp_2004_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('gew')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("gew") & matches("X")),
    list(~ paste(sub("gew_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))


# Calculating turnout ----
rlp_2004_gemeinderatswahlen_data_sub$Turnout <- rlp_2004_gemeinderatswahlen_data_sub$Wähler /
  rlp_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 2009 Gemeinderatswahlen ----
#### Load election data ----
rlp_2009_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_2009.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(rlp_2009_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_2009_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_2009_gemeinderatswahlen_data_sub <- rlp_2009_gemeinderatswahlen_data

names(rlp_2009_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2009_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_2009_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_2009_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_2009_gemeinderatswahlen_data_sub[, election_year := "2009"]
rlp_2009_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
rlp_2009_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_2009_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
rlp_2009_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2009_gemeinderatswahlen_data_sub$AGS
rlp_2009_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2009_gemeinderatswahlen_data_sub$Gemeindename
rlp_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_2009_gemeinderatswahlen_data_sub$Wahlberechtigte
rlp_2009_gemeinderatswahlen_data_sub$Wähler <- rlp_2009_gemeinderatswahlen_data_sub$Waehler
#rlp_2009_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2009_gemeinderatswahlen_data_sub$GueltigeStimmen

# Use Stimmzettel!
rlp_2009_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2009_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2009_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$CDU
)
rlp_2009_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$SPD
)
rlp_2009_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$DIELINKE
)
rlp_2009_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$GRUENE
)
rlp_2009_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$FDP
)
rlp_2009_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge
)
rlp_2009_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$Waehlergruppen
)

rlp_2009_gemeinderatswahlen_data_sub$gew_CDU <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_SPD <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_FDP <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
rlp_2009_gemeinderatswahlen_data_sub$gew_Wählergruppen <- NA

rlp_2009_gemeinderatswahlen_data_sub$sitze_CDU <- rlp_2009_gemeinderatswahlen_data_sub$CDU_sitze
rlp_2009_gemeinderatswahlen_data_sub$sitze_SPD <- rlp_2009_gemeinderatswahlen_data_sub$SPD_sitze
rlp_2009_gemeinderatswahlen_data_sub$sitze_DIELINKE <- rlp_2009_gemeinderatswahlen_data_sub$DIELINKE_sitze
rlp_2009_gemeinderatswahlen_data_sub$sitze_GRÜNE <- rlp_2009_gemeinderatswahlen_data_sub$GRUENE_sitze
rlp_2009_gemeinderatswahlen_data_sub$sitze_AfD <- NA
rlp_2009_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2009_gemeinderatswahlen_data_sub$sitze_FDP <- rlp_2009_gemeinderatswahlen_data_sub$FDP_sitze
rlp_2009_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_2009_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_2009_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze
)
rlp_2009_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(
  rlp_2009_gemeinderatswahlen_data_sub$Waehlergruppen_sitze
)

# Creating new dataframe with selected vars ----
rlp_2009_gemeinderatswahlen_data_sub <- rlp_2009_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2009_gemeinderatswahlen_data_sub <-
  rlp_2009_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
rlp_2009_gemeinderatswahlen_data_sub$Turnout <- rlp_2009_gemeinderatswahlen_data_sub$Wähler /
  rlp_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 2014 Gemeinderatswahlen ----
#### Load election data ----
rlp_2014_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_2014.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(rlp_2014_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_2014_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_2014_gemeinderatswahlen_data_sub <- rlp_2014_gemeinderatswahlen_data

names(rlp_2014_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2014_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_2014_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_2014_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_2014_gemeinderatswahlen_data_sub[, election_year := "2014"]
rlp_2014_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
rlp_2014_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_2014_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
rlp_2014_gemeinderatswahlen_data_sub$AGS_8dig <- as.character(
  rlp_2014_gemeinderatswahlen_data_sub$AGS
)
rlp_2014_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2014_gemeinderatswahlen_data_sub$Gemeindename
rlp_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$Wahlberechtigte
)
rlp_2014_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$Waehler
)
rlp_2014_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2014_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2014_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$CDU
)
rlp_2014_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$SPD
)
rlp_2014_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$DIELINKE
)
rlp_2014_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$GRUENE
)
rlp_2014_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$AfD
)
rlp_2014_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2014_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$FDP
)
rlp_2014_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2014_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2014_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge
)
rlp_2014_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$Waehlergruppen
)

rlp_2014_gemeinderatswahlen_data_sub$gew_CDU <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_SPD <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_FDP <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
rlp_2014_gemeinderatswahlen_data_sub$gew_Wählergruppen <- NA

rlp_2014_gemeinderatswahlen_data_sub$sitze_CDU <- rlp_2014_gemeinderatswahlen_data_sub$CDU_sitze
rlp_2014_gemeinderatswahlen_data_sub$sitze_SPD <- rlp_2014_gemeinderatswahlen_data_sub$SPD_sitze
rlp_2014_gemeinderatswahlen_data_sub$sitze_DIELINKE <- rlp_2014_gemeinderatswahlen_data_sub$DIELINKE_sitze
rlp_2014_gemeinderatswahlen_data_sub$sitze_GRÜNE <- rlp_2014_gemeinderatswahlen_data_sub$GRUENE_sitze
rlp_2014_gemeinderatswahlen_data_sub$sitze_AfD <- rlp_2014_gemeinderatswahlen_data_sub$AfD_sitze
rlp_2014_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2014_gemeinderatswahlen_data_sub$sitze_FDP <- rlp_2014_gemeinderatswahlen_data_sub$FDP_sitze
rlp_2014_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_2014_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_2014_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze
)
rlp_2014_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(
  rlp_2014_gemeinderatswahlen_data_sub$Waehlergruppen_sitze
)


# Creating new dataframe with selected vars ----
rlp_2014_gemeinderatswahlen_data_sub <- rlp_2014_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2014_gemeinderatswahlen_data_sub <-
  rlp_2014_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

rlp_2014_gemeinderatswahlen_data_sub[AGS_8dig == "33205032"]

# Calculating turnout ----
rlp_2014_gemeinderatswahlen_data_sub$Turnout <- rlp_2014_gemeinderatswahlen_data_sub$Wähler /
  rlp_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


# Filter Landkreise ----
rlp_2014_gemeinderatswahlen_data_sub <- rlp_2014_gemeinderatswahlen_data_sub %>%
  filter(!Gebietsname %in% c("Rhein-Hunsrück-Kreis", "Ahrweiler, Landkreis"))

###### RLP 2019 Gemeinderatswahlen ----
#### Load election data ----
rlp_2019_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_2019.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(rlp_2019_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_2019_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_2019_gemeinderatswahlen_data_sub <- rlp_2019_gemeinderatswahlen_data

names(rlp_2019_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2019_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_2019_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_2019_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_2019_gemeinderatswahlen_data_sub[, election_year := "2019"]
rlp_2019_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
rlp_2019_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_2019_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
rlp_2019_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2019_gemeinderatswahlen_data_sub$AGS
rlp_2019_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2019_gemeinderatswahlen_data_sub$Gemeindename
rlp_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Wahlberechtigte
)
rlp_2019_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Waehler
)
rlp_2019_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2019_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2019_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$CDU_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$SPD_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$DIELINKE_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$GRUENE_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$AfD_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2019_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$FDP_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_DiePARTEI <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$DiePARTEI_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$FREIEWAEHLER_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew
)
rlp_2019_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Waehlergruppen_gew
)

rlp_2019_gemeinderatswahlen_data_sub$gew_CDU <- rlp_2019_gemeinderatswahlen_data_sub$CDU_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_SPD <- rlp_2019_gemeinderatswahlen_data_sub$SPD_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_DIELINKE <- rlp_2019_gemeinderatswahlen_data_sub$DIELINKE_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_GRÜNE <- rlp_2019_gemeinderatswahlen_data_sub$GRUENE_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_AfD <- rlp_2019_gemeinderatswahlen_data_sub$AfD_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2019_gemeinderatswahlen_data_sub$gew_FDP <- rlp_2019_gemeinderatswahlen_data_sub$FDP_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_DiePARTEI <- rlp_2019_gemeinderatswahlen_data_sub$DiePARTEI_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- rlp_2019_gemeinderatswahlen_data_sub$FREIEWAEHLER_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew
)
rlp_2019_gemeinderatswahlen_data_sub$gew_Wählergruppen <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Waehlergruppen_gew
)

rlp_2019_gemeinderatswahlen_data_sub$sitze_CDU <- rlp_2019_gemeinderatswahlen_data_sub$CDU_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_SPD <- rlp_2019_gemeinderatswahlen_data_sub$SPD_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_DIELINKE <- rlp_2019_gemeinderatswahlen_data_sub$DIELINKE_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_GRÜNE <- rlp_2019_gemeinderatswahlen_data_sub$GRUENE_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_AfD <- rlp_2019_gemeinderatswahlen_data_sub$AfD_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2019_gemeinderatswahlen_data_sub$sitze_FDP <- rlp_2019_gemeinderatswahlen_data_sub$FDP_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- rlp_2019_gemeinderatswahlen_data_sub$DiePARTEI_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- rlp_2019_gemeinderatswahlen_data_sub$FREIEWAEHLER_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze
)
rlp_2019_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(
  rlp_2019_gemeinderatswahlen_data_sub$Waehlergruppen_sitze
)


# Creating new dataframe with selected vars ----
rlp_2019_gemeinderatswahlen_data_sub <- rlp_2019_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2019_gemeinderatswahlen_data_sub <-
  rlp_2019_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('gew')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("gew") & matches("X")),
    list(~ paste(sub("gew_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
rlp_2019_gemeinderatswahlen_data_sub$Turnout <- rlp_2019_gemeinderatswahlen_data_sub$Wähler /
  rlp_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 2009_2019_krfr_steadte Gemeinderatswahlen ----
#### Load election data ----
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/rlp/rlp_2009_2014_2019_krfr_Staedte.xlsx",
  sheet = "results"
))

#### Delete white space ----
names(rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data) <- str_replace_all(
  names(rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data

names(rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[, Bundesland := "RLP"]
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[, Gebietsname := ""]
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[,
  election_type := "Kommunalwahlen"
]
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[, IDIRB := ""]
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$election_year <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$year
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$ags
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gemeinde
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Wahlberechtigte
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Waehler
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gueltigeStimmen

rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$CDU
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$SPD
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Linke
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Gruene
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$AfD
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_PIRATEN <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$PIRATEN
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$FDP
)
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$WG
)

rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_CDU <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_SPD <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_FDP <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$gew_Wählergruppen <- NA

rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_CDU <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_SPD <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_AfD <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_FDP <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- NA
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- NA


# Creating new dataframe with selected vars ----
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_FREIEWÄHLER,
  abs_Gemeinsame_Wahlvorschläge,
  abs_Wählergruppen,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_FREIEWÄHLER,
  gew_Gemeinsame_Wahlvorschläge,
  gew_Wählergruppen,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_FREIEWÄHLER,
  sitze_Gemeinsame_Wahlvorschläge,
  sitze_Wählergruppen
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub <-
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Turnout <- rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Wähler /
  rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for RLP ----
# Merge
rlp_kommunalwahlen <- rbind(
  rlp_1994_gemeinderatswahlen_data_sub,
  rlp_1999_gemeinderatswahlen_data_sub,
  rlp_2004_gemeinderatswahlen_data_sub,
  rlp_2009_gemeinderatswahlen_data_sub,
  rlp_2014_gemeinderatswahlen_data_sub,
  rlp_2019_gemeinderatswahlen_data_sub
)

# Replace - with NA
rlp_kommunalwahlen[rlp_kommunalwahlen == "-"] <- NA


# Fix AGS for RLP ----
rlp_kommunalwahlen$AGS_8dig <- paste0("07", rlp_kommunalwahlen$AGS_8dig)

rlp_kommunalwahlen <- rlp_kommunalwahlen %>%
  filter(nchar(AGS_8dig) == 10) %>%
  mutate(AGS_8dig = paste0(substr(AGS_8dig, 1, 5), substr(AGS_8dig, 8, 10))) %>%
  rbind(rlp_2009_2019_krfr_steadte_gemeinderatswahlen_data_sub)

# Remove LK and VG ----
rlp_kommunalwahlen <- rlp_kommunalwahlen %>%
  filter(
    !grepl(", LK", Gebietsname),
    !grepl(", VG", Gebietsname),
    !grepl(", Landkreis", Gebietsname),
    !AGS_8dig %in%
      c("07141000", "07143000", "07232000", "07333000", "07338000", "07140000")
  ) %>%
  arrange(election_year)


# Save
#write_csv(rlp_kommunalwahlen, "processed/rlp_kommunalwahlen.csv")

######### Schleswig-Holstein ----
###### SH 2018 Gemeinderatswahlen ----
#### Load election data ----

sh_2018_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/schleswig_holstein/sh_2018.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sh_2018_gemeinderatswahlen_data) <- str_replace_all(
  names(sh_2018_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sh_2018_gemeinderatswahlen_data_sub <- sh_2018_gemeinderatswahlen_data

names(sh_2018_gemeinderatswahlen_data)


# Creating non-existing variables ----
sh_2018_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sh_2018_gemeinderatswahlen_data_sub[, Bundesland := "Schleswig-Holstein"]
sh_2018_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sh_2018_gemeinderatswahlen_data_sub[, election_year := "2018"]
sh_2018_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sh_2018_gemeinderatswahlen_data_sub[, IDIRB := ""]
sh_2018_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sh_2018_gemeinderatswahlen_data_sub$Gebietsname <- sh_2018_gemeinderatswahlen_data_sub$Gemeindename
sh_2018_gemeinderatswahlen_data_sub$AGS_8dig <- sh_2018_gemeinderatswahlen_data_sub$Gemeindekennziffer
sh_2018_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sh_2018_gemeinderatswahlen_data_sub$Wahlberechtigte
sh_2018_gemeinderatswahlen_data_sub$Wähler <- sh_2018_gemeinderatswahlen_data_sub$Wählerinsgesamt
sh_2018_gemeinderatswahlen_data_sub$GültigeStimmen <- sh_2018_gemeinderatswahlen_data_sub$gültigeStimmen

sh_2018_gemeinderatswahlen_data_sub$abs_CDU <- sh_2018_gemeinderatswahlen_data_sub$CDU
sh_2018_gemeinderatswahlen_data_sub$abs_SPD <- sh_2018_gemeinderatswahlen_data_sub$SPD
sh_2018_gemeinderatswahlen_data_sub$abs_DIELINKE <- sh_2018_gemeinderatswahlen_data_sub$DIELINKE
sh_2018_gemeinderatswahlen_data_sub$abs_GRÜNE <- sh_2018_gemeinderatswahlen_data_sub$Gruene
sh_2018_gemeinderatswahlen_data_sub$abs_AfD <- sh_2018_gemeinderatswahlen_data_sub$AfD
sh_2018_gemeinderatswahlen_data_sub$abs_PIRATEN <- sh_2018_gemeinderatswahlen_data_sub$PIRATEN
sh_2018_gemeinderatswahlen_data_sub$abs_FDP <- sh_2018_gemeinderatswahlen_data_sub$FDP
sh_2018_gemeinderatswahlen_data_sub$abs_DiePARTEI <- sh_2018_gemeinderatswahlen_data_sub$DiePARTEI
sh_2018_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- sh_2018_gemeinderatswahlen_data_sub$FREIEWAEHLER

sh_2018_gemeinderatswahlen_data_sub$gew_CDU <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_SPD <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_AfD <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_FDP <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sh_2018_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sh_2018_gemeinderatswahlen_data_sub$sitze_CDU <- sh_2018_gemeinderatswahlen_data_sub$CDU_maenner +
  sh_2018_gemeinderatswahlen_data_sub$CDU_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_SPD <- sh_2018_gemeinderatswahlen_data_sub$SPD_maenner +
  sh_2018_gemeinderatswahlen_data_sub$SPD_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_DIELINKE <- sh_2018_gemeinderatswahlen_data_sub$DIELINKE_maenner +
  sh_2018_gemeinderatswahlen_data_sub$DIELINKE_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_GRÜNE <- sh_2018_gemeinderatswahlen_data_sub$Gruene_maenner +
  sh_2018_gemeinderatswahlen_data_sub$Gruene_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_AfD <- sh_2018_gemeinderatswahlen_data_sub$AfD_maenner +
  sh_2018_gemeinderatswahlen_data_sub$AfD_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_PIRATEN <- sh_2018_gemeinderatswahlen_data_sub$PIRATEN_maenner +
  sh_2018_gemeinderatswahlen_data_sub$PIRATEN_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_FDP <- sh_2018_gemeinderatswahlen_data_sub$FDP_maenner +
  sh_2018_gemeinderatswahlen_data_sub$FDP_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- sh_2018_gemeinderatswahlen_data_sub$DiePARTEI_maenner +
  sh_2018_gemeinderatswahlen_data_sub$DiePARTEI_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- sh_2018_gemeinderatswahlen_data_sub$FREIEWAEHLER_maenner +
  sh_2018_gemeinderatswahlen_data_sub$FREIEWAEHLER_frauen

# Creating new dataframe with selected vars ----
sh_2018_gemeinderatswahlen_data_sub <- sh_2018_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2018_gemeinderatswahlen_data_sub <-
  sh_2018_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sh_2018_gemeinderatswahlen_data_sub$Turnout <- sh_2018_gemeinderatswahlen_data_sub$Wähler /
  sh_2018_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 2013 Gemeinderatswahlen ----
#### Load election data ----

sh_2013_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/schleswig_holstein/sh_2013.xlsx",
  sheet = "summary"
))

#### Delete white space ----
names(sh_2013_gemeinderatswahlen_data) <- str_replace_all(
  names(sh_2013_gemeinderatswahlen_data),
  fixed(" "),
  ""
)

#### Recoding ----
# Create new dataframe ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data

# Create corrected AGS ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data_sub %>%
  mutate(
    AGS_8dig = paste0(
      "010",
      substr(
        GemeindekennziffermitWahlbezirk,
        1,
        nchar(GemeindekennziffermitWahlbezirk) - 3
      )
    )
  )

# Summarize by Gemeinde ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data_sub %>%
  group_by(Gemeindename, AGS_8dig) %>%
  summarise(
    Wahlberechtigte = sum(Wahlberechtigte, na.rm = T),
    Wählerinsgesamt = sum(Wählerinsgesamt, na.rm = T),
    gültigeStimmen = sum(gültigeStimmen, na.rm = T),
    CDU = sum(CDU, na.rm = T),
    SPD = sum(SPD, na.rm = T),
    GRÜNE = sum(GRÜNE, na.rm = T),
    FDP = sum(FDP, na.rm = T),
    PIRATEN = sum(PIRATEN, na.rm = T),
    DIELINKE = sum(DIELINKE, na.rm = T),
    NPD = sum(NPD, na.rm = T),
    FREIEWÄHLER = sum(FREIEWÄHLER, na.rm = T),
    DiePARTEI = sum(DiePARTEI, na.rm = T)
  ) %>%
  ungroup()

sh_2013_gemeinderatswahlen_data_sub <- as.data.table(
  sh_2013_gemeinderatswahlen_data_sub
)

# Creating non-existing variables ----
sh_2013_gemeinderatswahlen_data_sub$Bundesland <- "Schleswig-Holstein"
sh_2013_gemeinderatswahlen_data_sub$Gebietsname <- ""
sh_2013_gemeinderatswahlen_data_sub$election_year <- "2013"
sh_2013_gemeinderatswahlen_data_sub$election_type <- "Gemeinderatswahlen"
sh_2013_gemeinderatswahlen_data_sub$IDIRB <- ""
sh_2013_gemeinderatswahlen_data_sub$IDBA <- ""

# Renaming existing variables ----
sh_2013_gemeinderatswahlen_data_sub$Gebietsname <- sh_2013_gemeinderatswahlen_data_sub$Gemeindename
sh_2013_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sh_2013_gemeinderatswahlen_data_sub$Wahlberechtigte
sh_2013_gemeinderatswahlen_data_sub$Wähler <- sh_2013_gemeinderatswahlen_data_sub$Wählerinsgesamt
sh_2013_gemeinderatswahlen_data_sub$GültigeStimmen <- sh_2013_gemeinderatswahlen_data_sub$gültigeStimmen

sh_2013_gemeinderatswahlen_data_sub$abs_CDU <- sh_2013_gemeinderatswahlen_data_sub$CDU
sh_2013_gemeinderatswahlen_data_sub$abs_SPD <- sh_2013_gemeinderatswahlen_data_sub$SPD
sh_2013_gemeinderatswahlen_data_sub$abs_DIELINKE <- sh_2013_gemeinderatswahlen_data_sub$DIELINKE
sh_2013_gemeinderatswahlen_data_sub$abs_GRÜNE <- sh_2013_gemeinderatswahlen_data_sub$GRÜNE
sh_2013_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_2013_gemeinderatswahlen_data_sub$abs_PIRATEN <- sh_2013_gemeinderatswahlen_data_sub$PIRATEN
sh_2013_gemeinderatswahlen_data_sub$abs_FDP <- sh_2013_gemeinderatswahlen_data_sub$FDP
sh_2013_gemeinderatswahlen_data_sub$abs_DiePARTEI <- sh_2013_gemeinderatswahlen_data_sub$DiePARTEI
sh_2013_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- sh_2013_gemeinderatswahlen_data_sub$FREIEWÄHLER

sh_2013_gemeinderatswahlen_data_sub$gew_CDU <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_SPD <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_AfD <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_FDP <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sh_2013_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sh_2013_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sh_2013_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]
# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2013_gemeinderatswahlen_data_sub <-
  sh_2013_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sh_2013_gemeinderatswahlen_data_sub$Turnout <- sh_2013_gemeinderatswahlen_data_sub$Wähler /
  sh_2013_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### SH 2008 Gemeinderatswahlen ----
#### Load election data ----

sh_2008_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/schleswig_holstein/sh_2008.xls",
  sheet = "summary"
))

#### Summarize data to fix poor data structure ----
sh_2008_gemeinderatswahlen_data_sub <-
  sh_2008_gemeinderatswahlen_data %>%
  mutate(AGS_processed = paste0("010", substr(Gemeindekennziffer, 1, 5))) %>%
  group_by(Gemeindename) %>%
  summarise(
    Wahlberechtigte = sum(Wahlberechtigteinsgesamt),
    Wähler = sum(Waehler),
    gültigeStimmen = sum(gueltigeStimmen),
    CDU = sum(CDU),
    SPD = sum(SPD),
    Gruene = sum(Gruene),
    FDP = sum(FDP),
    DIELINKE = sum(DIELINKE),
    AGS_8dig = first(AGS_processed)
  )
sh_2008_gemeinderatswahlen_data_sub <- as.data.table(
  sh_2008_gemeinderatswahlen_data_sub
)

#### Recoding ----
# Creating non-existing variables ----
sh_2008_gemeinderatswahlen_data_sub[, Bundesland := "Schleswig-Holstein"]
sh_2008_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sh_2008_gemeinderatswahlen_data_sub[, election_year := "2008"]
sh_2008_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sh_2008_gemeinderatswahlen_data_sub[, IDIRB := ""]
sh_2008_gemeinderatswahlen_data_sub[, IDBA := ""]


# Renaming existing variables ----
sh_2008_gemeinderatswahlen_data_sub$Gebietsname <- sh_2008_gemeinderatswahlen_data_sub$Gemeindename
sh_2008_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- sh_2008_gemeinderatswahlen_data_sub$Wahlberechtigte
sh_2008_gemeinderatswahlen_data_sub$Wähler <- sh_2008_gemeinderatswahlen_data_sub$Wähler
sh_2008_gemeinderatswahlen_data_sub$GültigeStimmen <- sh_2008_gemeinderatswahlen_data_sub$gültigeStimmen

sh_2008_gemeinderatswahlen_data_sub$abs_CDU <- sh_2008_gemeinderatswahlen_data_sub$CDU
sh_2008_gemeinderatswahlen_data_sub$abs_SPD <- sh_2008_gemeinderatswahlen_data_sub$SPD
sh_2008_gemeinderatswahlen_data_sub$abs_DIELINKE <- sh_2008_gemeinderatswahlen_data_sub$DIELINKE
sh_2008_gemeinderatswahlen_data_sub$abs_GRÜNE <- sh_2008_gemeinderatswahlen_data_sub$Gruene
sh_2008_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_2008_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_2008_gemeinderatswahlen_data_sub$abs_FDP <- sh_2008_gemeinderatswahlen_data_sub$FDP
sh_2008_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sh_2008_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

sh_2008_gemeinderatswahlen_data_sub$gew_CDU <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_SPD <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_AfD <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_FDP <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sh_2008_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sh_2008_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sh_2008_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sh_2008_gemeinderatswahlen_data_sub <- sh_2008_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2008_gemeinderatswahlen_data_sub <-
  sh_2008_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))

# Calculating turnout ----
sh_2008_gemeinderatswahlen_data_sub$Turnout <- sh_2008_gemeinderatswahlen_data_sub$Wähler /
  sh_2008_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 2003 Gemeinderatswahlen ----
#### Load election data ----

sh_2003_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/schleswig_holstein/sh_2003.xlsx",
  sheet = "summary"
))
sh_2003_gemeinderatswahlen_data_sub <- sh_2003_gemeinderatswahlen_data

#### Recoding ----
# Creating non-existing variables ----
sh_2003_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sh_2003_gemeinderatswahlen_data_sub[, Bundesland := "Schleswig-Holstein"]
sh_2003_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sh_2003_gemeinderatswahlen_data_sub[, election_year := "2003"]
sh_2003_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sh_2003_gemeinderatswahlen_data_sub[, IDIRB := ""]
sh_2003_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sh_2003_gemeinderatswahlen_data_sub$Gebietsname <- sh_2003_gemeinderatswahlen_data_sub$Gemeindename
sh_2003_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
)
sh_2003_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$Waehler
)
sh_2003_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$gueltigeStimmen
)

sh_2003_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$CDU
)
sh_2003_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$SPD
)
sh_2003_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$Gruene
)
sh_2003_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  sh_2003_gemeinderatswahlen_data_sub$FDP
)
sh_2003_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

sh_2003_gemeinderatswahlen_data_sub$gew_CDU <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_SPD <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_AfD <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_FDP <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sh_2003_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sh_2003_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sh_2003_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Fix AGS ----
sh_2003_gemeinderatswahlen_data_sub <- sh_2003_gemeinderatswahlen_data_sub %>%
  mutate(
    AGS_8dig = paste0("01 0", Gemeindekennziffer)
  ) %>%
  mutate(
    AGS_8dig = str_replace(AGS_8dig, " ", ""),
    AGS_8dig = str_replace(AGS_8dig, " ", "")
  )

# Creating new dataframe with selected vars ----
sh_2003_gemeinderatswahlen_data_sub <- sh_2003_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2003_gemeinderatswahlen_data_sub <-
  sh_2003_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))


# Calculating turnout ----
sh_2003_gemeinderatswahlen_data_sub$Turnout <- sh_2003_gemeinderatswahlen_data_sub$Wähler /
  sh_2003_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 1998 Gemeinderatswahlen ----
#### Load election data ----

sh_1998_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/schleswig_holstein/sh_1998.xlsx",
  sheet = "summary"
))
sh_1998_gemeinderatswahlen_data_sub <- sh_1998_gemeinderatswahlen_data

#### Recoding ----
# Creating non-existing variables ----
sh_1998_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sh_1998_gemeinderatswahlen_data_sub[, Bundesland := "Schleswig-Holstein"]
sh_1998_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sh_1998_gemeinderatswahlen_data_sub[, election_year := "1998"]
sh_1998_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sh_1998_gemeinderatswahlen_data_sub[, IDIRB := ""]
sh_1998_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sh_1998_gemeinderatswahlen_data_sub$Gebietsname <- sh_1998_gemeinderatswahlen_data_sub$Gemeindename
sh_1998_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
)
sh_1998_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$Wahler
)
sh_1998_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$gueltigeStimmen
)

sh_1998_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$CDU
)
sh_1998_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$SPD
)
sh_1998_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$Gruene
)
sh_1998_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  sh_1998_gemeinderatswahlen_data_sub$FDP
)
sh_1998_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

sh_1998_gemeinderatswahlen_data_sub$gew_CDU <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_SPD <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_AfD <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_FDP <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sh_1998_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sh_1998_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sh_1998_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Fix AGS ----
sh_1998_gemeinderatswahlen_data_sub <- sh_1998_gemeinderatswahlen_data_sub %>%
  mutate(
    AGS_8dig = paste0("01 0", Gemeindekennziffer)
  ) %>%
  mutate(
    AGS_8dig = str_replace(AGS_8dig, " ", ""),
    AGS_8dig = str_replace(AGS_8dig, " ", "")
  )

# Creating new dataframe with selected vars ----
sh_1998_gemeinderatswahlen_data_sub <- sh_1998_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_1998_gemeinderatswahlen_data_sub <-
  sh_1998_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))


# Calculating turnout ----
sh_1998_gemeinderatswahlen_data_sub$Turnout <- sh_1998_gemeinderatswahlen_data_sub$Wähler /
  sh_1998_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 1994 Gemeinderatswahlen ----
#### Load election data ----

sh_1994_gemeinderatswahlen_data <- as.data.table(read_excel(
  "raw/schleswig_holstein/sh_1994.xlsx",
  sheet = "summary"
))
sh_1994_gemeinderatswahlen_data_sub <- sh_1994_gemeinderatswahlen_data

#### Recoding ----
# Creating non-existing variables ----
sh_1994_gemeinderatswahlen_data_sub[, AGS_8dig := ""] # 8 digits with leading zero
sh_1994_gemeinderatswahlen_data_sub[, Bundesland := "Schleswig-Holstein"]
sh_1994_gemeinderatswahlen_data_sub[, Gebietsname := ""]
sh_1994_gemeinderatswahlen_data_sub[, election_year := "1994"]
sh_1994_gemeinderatswahlen_data_sub[, election_type := "Kommunalwahlen"]
sh_1994_gemeinderatswahlen_data_sub[, IDIRB := ""]
sh_1994_gemeinderatswahlen_data_sub[, IDBA := ""]

# Renaming existing variables ----
sh_1994_gemeinderatswahlen_data_sub$Gebietsname <- sh_1994_gemeinderatswahlen_data_sub$Gemeindename
sh_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
)
sh_1994_gemeinderatswahlen_data_sub$Wähler <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$Waehler
)
sh_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$gueltigeStimmen
)

sh_1994_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$CDU
)
sh_1994_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$SPD
)
sh_1994_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$Gruene
)
sh_1994_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(
  sh_1994_gemeinderatswahlen_data_sub$FDP
)
sh_1994_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA

sh_1994_gemeinderatswahlen_data_sub$gew_CDU <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_SPD <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_AfD <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_FDP <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sh_1994_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sh_1994_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sh_1994_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Fix AGS ----
sh_1994_gemeinderatswahlen_data_sub <- sh_1994_gemeinderatswahlen_data_sub %>%
  mutate(
    AGS_8dig = paste0("01 0", Gemeindekennziffer)
  ) %>%
  mutate(
    AGS_8dig = str_replace(AGS_8dig, " ", ""),
    AGS_8dig = str_replace(AGS_8dig, " ", "")
  )

# Creating new dataframe with selected vars ----
sh_1994_gemeinderatswahlen_data_sub <- sh_1994_gemeinderatswahlen_data_sub[, .(
  AGS_8dig,
  Bundesland,
  Gebietsname,
  election_year,
  election_type,
  IDIRB,
  IDBA,
  Wahlberechtigteinsgesamt,
  Wähler,
  GültigeStimmen,
  abs_CDU,
  abs_SPD,
  abs_DIELINKE,
  abs_GRÜNE,
  abs_AfD,
  abs_PIRATEN,
  abs_FDP,
  abs_DiePARTEI,
  abs_FREIEWÄHLER,
  gew_CDU,
  gew_SPD,
  gew_DIELINKE,
  gew_GRÜNE,
  gew_AfD,
  gew_PIRATEN,
  gew_FDP,
  gew_DiePARTEI,
  gew_FREIEWÄHLER,
  sitze_CDU,
  sitze_SPD,
  sitze_DIELINKE,
  sitze_GRÜNE,
  sitze_AfD,
  sitze_PIRATEN,
  sitze_FDP,
  sitze_DiePARTEI,
  sitze_FREIEWÄHLER
)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_1994_gemeinderatswahlen_data_sub <-
  sh_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(
    vars(contains('abs')),
    .funs = list(XXX = ~ . / as.numeric(GültigeStimmen))
  ) %>%
  rename_at(
    vars(matches("abs") & matches("X")),
    list(~ paste(sub("abs_", "prop_", .), sep = "_"))
  ) %>%
  rename_at(vars(matches("_XXX")), list(~ paste(sub("_XXX", "", .), sep = "")))


# Calculating turnout ----
sh_1994_gemeinderatswahlen_data_sub$Turnout <- sh_1994_gemeinderatswahlen_data_sub$Wähler /
  sh_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for SH ----
# Merge
sh_kommunalwahlen <- rbind(
  sh_1994_gemeinderatswahlen_data_sub,
  sh_1998_gemeinderatswahlen_data_sub,
  sh_2003_gemeinderatswahlen_data_sub,
  sh_2008_gemeinderatswahlen_data_sub,
  sh_2013_gemeinderatswahlen_data_sub
) %>%
  filter(
    AGS_8dig %in% c("01001000", "01002000", "01003000", "01004000")
  )

sh_kommunalwahlen <- rbind(
  sh_kommunalwahlen,
  sh_2018_gemeinderatswahlen_data_sub
)

# Replace - with NA
sh_kommunalwahlen[sh_kommunalwahlen == "-"] <- NA

# Fix AGS
sh_kommunalwahlen$AGS_8dig <- stri_pad_left(sh_kommunalwahlen$AGS_8dig, 8, 0)

# Save
#write_csv(sh_kommunalwahlen, "processed/sh_kommunalwahlen.csv")

# ----
# ----
# ----
########## MERGE FINAL FILES ----

# Merge ----
kommunalwahlen_merge <- rbind(
  baden_wuerttemberg_kommunalwahlen,
  bayern_kommunalwahlen,
  berlin_kommunalwahlen,
  brandenburg_kommunalwahlen,
  bremen_overall_buergerschaftswahl_data_sub,
  hamburg_kommunalwahlen,
  hessen_kommunalwahlen,
  mecklenburg_vorpommern_kommunalwahlen,
  niedersachsen_kommunalwahlen,
  nrw_kommunalwahlen,
  rlp_kommunalwahlen,
  saarland_kommunalwahlen_data_sub,
  sachsen_anhalt_kommunalwahlen,
  sachsen_kommunalwahlen,
  sh_kommunalwahlen,
  thueringen_kommunalwahlen,
  fill = TRUE
)

# Fix AGS ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(
    AGS_8dig = if_else(
      str_length(AGS_8dig) == 7,
      str_pad(AGS_8dig, width = 8, side = "left", pad = "0"),
      AGS_8dig
    )
  ) %>%
  filter(!str_length(AGS_8dig) %in% c(10, 4)) %>%
  mutate(AGS_8dig = if_else(AGS_8dig == "04", "04011000", AGS_8dig)) %>%
  filter(!is.na(AGS_8dig))


# Change FW and remainder category ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(across(starts_with("abs_"), ~ as.numeric(.x))) %>%
  rowwise %>%
  mutate(
    abs_OTHER = (GültigeStimmen -
      sum(
        abs_CDU,
        abs_SPD,
        abs_DIELINKE,
        abs_GRÜNE,
        abs_AfD,
        abs_PIRATEN,
        abs_FDP,
        abs_DiePARTEI,
        abs_FREIEWÄHLER,
        na.rm = T
      )),
    prop_OTHER = (1 -
      sum(
        prop_CDU,
        prop_SPD,
        prop_DIELINKE,
        prop_GRÜNE,
        prop_AfD,
        prop_PIRATEN,
        prop_FDP,
        prop_DiePARTEI,
        prop_FREIEWÄHLER,
        na.rm = T
      ))
  ) %>%
  dplyr::select(
    AGS_8dig:abs_FREIEWÄHLER,
    abs_OTHER,
    gew_CDU:prop_FREIEWÄHLER,
    prop_OTHER,
    Turnout
  ) %>%
  ungroup()


# Change vote_share to NA where == 0, create indicator variables to show where this is the case ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(
    replaced_0_with_NA_CDU = case_when(
      abs_CDU == 0 ~ 1,
      abs_CDU != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_SPD = case_when(
      abs_SPD == 0 ~ 1,
      abs_SPD != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_DIELINKE = case_when(
      abs_DIELINKE == 0 ~ 1,
      abs_DIELINKE != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_GRÜNE = case_when(
      abs_GRÜNE == 0 ~ 1,
      abs_GRÜNE != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_AfD = case_when(
      abs_AfD == 0 ~ 1,
      abs_AfD != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_PIRATEN = case_when(
      abs_PIRATEN == 0 ~ 1,
      abs_PIRATEN != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_FDP = case_when(
      abs_FDP == 0 ~ 1,
      abs_FDP != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_DiePARTEI = case_when(
      abs_DiePARTEI == 0 ~ 1,
      abs_DiePARTEI != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_FDP = case_when(
      abs_FDP == 0 ~ 1,
      abs_FDP != 0 ~ 0,
      TRUE ~ 0
    ),
    replaced_0_with_NA_FREIEWÄHLER = case_when(
      abs_FREIEWÄHLER == 0 ~ 1,
      abs_FREIEWÄHLER != 0 ~ 0,
      TRUE ~ 0
    )
  )

kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(
    abs_CDU = ifelse(replaced_0_with_NA_CDU == 1, NA, abs_CDU),
    prop_CDU = ifelse(replaced_0_with_NA_CDU == 1, NA, prop_CDU),
    abs_SPD = ifelse(replaced_0_with_NA_SPD == 1, NA, abs_SPD),
    prop_SPD = ifelse(replaced_0_with_NA_SPD == 1, NA, prop_SPD),
    abs_DIELINKE = ifelse(replaced_0_with_NA_DIELINKE == 1, NA, abs_DIELINKE),
    prop_DIELINKE = ifelse(replaced_0_with_NA_DIELINKE == 1, NA, prop_DIELINKE),
    abs_GRÜNE = ifelse(replaced_0_with_NA_GRÜNE == 1, NA, abs_GRÜNE),
    prop_GRÜNE = ifelse(replaced_0_with_NA_GRÜNE == 1, NA, prop_GRÜNE),
    abs_AfD = ifelse(replaced_0_with_NA_AfD == 1, NA, abs_AfD),
    prop_AfD = ifelse(replaced_0_with_NA_AfD == 1, NA, prop_AfD),
    abs_PIRATEN = ifelse(replaced_0_with_NA_PIRATEN == 1, NA, abs_PIRATEN),
    prop_PIRATEN = ifelse(replaced_0_with_NA_PIRATEN == 1, NA, prop_PIRATEN),
    abs_FDP = ifelse(replaced_0_with_NA_FDP == 1, NA, abs_FDP),
    prop_FDP = ifelse(replaced_0_with_NA_FDP == 1, NA, prop_FDP),
    abs_DiePARTEI = ifelse(
      replaced_0_with_NA_DiePARTEI == 1,
      NA,
      abs_DiePARTEI
    ),
    prop_DiePARTEI = ifelse(
      replaced_0_with_NA_DiePARTEI == 1,
      NA,
      prop_DiePARTEI
    ),
    abs_FREIEWÄHLER = ifelse(
      replaced_0_with_NA_FREIEWÄHLER == 1,
      NA,
      abs_FREIEWÄHLER
    ),
    prop_FREIEWÄHLER = ifelse(
      replaced_0_with_NA_FREIEWÄHLER == 1,
      NA,
      prop_FREIEWÄHLER
    )
  )

kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(
    prop_CDU = ifelse(abs_CDU == 0, NA, prop_CDU),
    prop_SPD = ifelse(abs_SPD == 0, NA, prop_SPD),
    prop_DIELINKE = ifelse(abs_DIELINKE == 0, NA, prop_DIELINKE),
    prop_GRÜNE = ifelse(abs_GRÜNE == 0, NA, prop_GRÜNE),
    prop_AfD = ifelse(abs_AfD == 0, NA, prop_AfD),
    prop_PIRATEN = ifelse(abs_PIRATEN == 0, NA, prop_PIRATEN),
    prop_FDP = ifelse(abs_FDP == 0, NA, prop_FDP),
    prop_DiePARTEI = ifelse(abs_DiePARTEI == 0, NA, prop_DiePARTEI),
    prop_FREIEWÄHLER = ifelse(abs_FREIEWÄHLER == 0, NA, prop_FREIEWÄHLER)
  )

# Fix prop_other ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(
    prop_OTHER = ifelse(prop_OTHER < 0, 0, prop_OTHER)
  )

# Transform some variables for congruence reasons -------------------------

kommunalwahlen_merge <- kommunalwahlen_merge |>
  rename(
    ags = AGS_8dig,
    state = Bundesland,
    ags_name = Gebietsname,
    eligible_voters = Wahlberechtigteinsgesamt,
    number_voters = Wähler,
    valid_votes = GültigeStimmen,
    turnout = Turnout
  ) |>
  select(-c(IDIRB, IDBA)) |>
  mutate(
    turnout = as.numeric(turnout),
    eligible_voters = as.numeric(eligible_voters),
    sitze_CDU = as.numeric(sitze_CDU),
    sitze_SPD = as.numeric(sitze_SPD),
    sitze_GRÜNE = as.numeric(sitze_GRÜNE),
    sitze_FDP = as.numeric(sitze_FDP),
    sitze_DIELINKE = as.numeric(sitze_DIELINKE),
    sitze_AfD = as.numeric(sitze_AfD),
    sitze_PIRATEN = as.numeric(sitze_PIRATEN),
    sitze_DiePARTEI = as.numeric(sitze_DiePARTEI),
    sitze_FREIEWÄHLER = as.numeric(sitze_FREIEWÄHLER)
  )


kommunalwahlen_merge <- kommunalwahlen_merge |>
  # change column names that start with sitze_ to seats_
  rename_with(~ str_replace(., "sitze_", "seats_"), starts_with("sitze_")) |>
  rename_with(~ str_replace(., "gew_", "weighted_"), starts_with("gew_")) |>
  # all variable names to lower
  rename_with(tolower, everything())

glimpse(kommunalwahlen_merge)

# Add election dates ------------------------------------------------

# output file containing combinations of state_name and election_year
# file is then used to scrap election_year from google searches
# see code/municipality_elections/other/01_scrap_municipal_elections_dates.ipynb for details

kommunalwahlen_merge |>
  distinct(state, election_year) |>
  # just developing acronyms
  mutate(
    state = case_when(
      state == "RLP" ~ "Rhineland-Palatinate",
      state == "NRW" ~ "North Rhine-Westphalia",
      .default = state
    )
  ) |>
  write_csv(
    file = here::here(
      "data/municipal_elections/processed/municipal_elections_combinations.csv"
    )
  )

## import it back with the election dates, fix two missing values, and join it back to original data
election_dates <- data.table::fread(here::here(
  "data/municipal_elections/processed/municipal_elections_dates.csv"
)) |>
  mutate(
    year = as.character(year),
    extracted_date = lubridate::parse_date_time(
      ifelse(extracted_date == "NULL", NA, extracted_date),
      orders = c("mdy", "ymd")
    ),
    # reverting back to previous state names to match the original data
    state = case_when(
      state == "Rhineland-Palatinate" ~ "RLP",
      state == "North Rhine-Westphalia" ~ "NRW",
      .default = state
    )
  ) |>
  rename(election_year = year, election_date = extracted_date)

# check if there are missing values for election dates
if (election_dates |> filter(is.na(election_date)) |> nrow() > 0) {
  message("There are missing values in the election dates.")
  print(election_dates |> filter(is.na(election_date)))
} else {
  message("No missing values in the election dates.")
}

# there are two, for Sachsen-Anhalt in 2005 and 2006

kommunalwahlen_merge <- kommunalwahlen_merge |>
  left_join(election_dates, by = c("state", "election_year")) |>
  relocate(election_date, .after = election_year)


# Reduce to prop_ only ----------------------------------------------------

kommunalwahlen_merge <- kommunalwahlen_merge |>
  select(
    ags,
    ags_name,
    state,
    election_year,
    election_date,
    election_type,
    eligible_voters,
    number_voters,
    valid_votes,
    turnout,
    starts_with("prop_"),
    starts_with("replaced")
  )

glimpse(kommunalwahlen_merge)

# change names
kommunalwahlen_merge <- kommunalwahlen_merge |>
  rename_with(~ str_replace(., "prop_", ""), starts_with("prop_")) |>
  rename_with(~ str_replace(., "cdu", "cdu_csu")) |>
  rename_with(~ str_replace(., "diepartei", "die_partei")) |>
  rename_with(~ str_replace(., "freiewähler", "freie_wahler")) |>
  rename_with(~ str_replace(., "grüne", "gruene")) |>
  rename_with(~ str_replace(., "dielinke", "linke_pds"))

# Sort ----------------------------------------------------
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  arrange(state, election_year, eligible_voters)


# Save ----

write_rds(
  kommunalwahlen_merge,
  file = here::here("data/municipal_elections/final/municipal_unharm.rds")
)
fwrite(
  kommunalwahlen_merge,
  file = here::here("data/municipal_elections/final/municipal_unharm.csv")
)

# View(kommunalwahlen_merge)

#kommunalwahlen_merge <- read_rds(file=here::here("data/municipal_elections/final/municipal_unharm.rds"))

# inspect -----------------------------------------------------------------

glimpse(kommunalwahlen_merge)

test <- kommunalwahlen_merge %>%
  filter(ags == "07133080", election_year == "2019")

### END
