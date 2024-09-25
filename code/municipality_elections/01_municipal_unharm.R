###################################
###### German Kommunalwahlen ######
###################################

# Florian Sichart
# Last update: August 2024

########## PREPARATION ----
rm(list = ls())
gc()

library(pacman)
pacman::p_load(writexl, readr, stringr, tidyverse, grid, gridExtra, tidyverse, broom, tidyr, data.table, readxl, dplyr, stringi)

# https://github.com/seligerf/Imputation-of-missing-location-information-for-worldwide-patent-data
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OTTBDX
# https://www.nature.com/articles/s41597-019-0264-6

# Set WD
setwd(here::here("data/municipal_elections"))

# if (Sys.info()['user'] == 'flosic') {
#   setwd("/Users/flosic/Dropbox/RA_work/Data collection/Germany_Kommunalwahlen")
# }

########## DATA PROCESSING ----
######### BAYERN ----
#### Load election data ----
bayern_kommunalwahlen_data <- as.data.table(read_excel("raw/bayern/bayern_all_parties_copy.xlsx", sheet="master"))

bayern_kommunalwahlen_waehler <- as.data.table(read_excel("raw/bayern/bayern_waehler_1996-2020.xlsx", sheet="overall"))
bayern_kommunalwahlen_gueltige <- as.data.table(read_excel("raw/bayern/bayern_gueltige_1996-2020.xlsx", sheet="master"))

bayern_kommunalwahlen_data <- merge(bayern_kommunalwahlen_data, bayern_kommunalwahlen_waehler, by="AGS", all.x=TRUE)
bayern_kommunalwahlen_data <- merge(bayern_kommunalwahlen_data, bayern_kommunalwahlen_gueltige, by="AGS", all.x=TRUE)


###### Bayern 1996 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_1996 <- data.table(read_excel("raw/bayern/bayern_1996new.xlsx", sheet="results"))


#### Recoding ----
# Create new dataframe ----
bayern_1996_kommunalwahlen_data_sub <- NA
bayern_1996_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_1996


# Creating non-existing variables ----
bayern_1996_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
bayern_1996_kommunalwahlen_data_sub[ , Bundesland := "Bayern"]
bayern_1996_kommunalwahlen_data_sub[ , Gebietsname := ""]
bayern_1996_kommunalwahlen_data_sub[ , election_year := "1996"]
bayern_1996_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
bayern_1996_kommunalwahlen_data_sub[ , IDIRB := ""]
bayern_1996_kommunalwahlen_data_sub[ , IDBA := ""]

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
bayern_1996_kommunalwahlen_data_sub <- bayern_1996_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_1996_kommunalwahlen_data_sub <-
  bayern_1996_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
bayern_1996_kommunalwahlen_data_sub$Turnout <- as.numeric(bayern_1996_kommunalwahlen_data_sub$Wähler) / as.numeric(bayern_1996_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### Bayern 2002 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2002 <- data.table(read_excel("raw/bayern/bayern_2002new.xlsx", sheet="results"))

#### Recoding ----
# Create new dataframe ----
bayern_2002_kommunalwahlen_data_sub <- NA
bayern_2002_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2002


# Creating non-existing variables ----
bayern_2002_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
bayern_2002_kommunalwahlen_data_sub[ , Bundesland := "Bayern"]
bayern_2002_kommunalwahlen_data_sub[ , Gebietsname := ""]
bayern_2002_kommunalwahlen_data_sub[ , election_year := "2002"]
bayern_2002_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
bayern_2002_kommunalwahlen_data_sub[ , IDIRB := ""]
bayern_2002_kommunalwahlen_data_sub[ , IDBA := ""]

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
bayern_2002_kommunalwahlen_data_sub <- bayern_2002_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2002_kommunalwahlen_data_sub <-
  bayern_2002_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
bayern_2002_kommunalwahlen_data_sub$Turnout <- as.numeric(bayern_2002_kommunalwahlen_data_sub$Wähler) / as.numeric(bayern_2002_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 2008 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2008 <- data.table(read_excel("raw/bayern/bayern_2008new.xlsx", sheet="results"))

#### Recoding ----
# Create new dataframe ----
bayern_2008_kommunalwahlen_data_sub <- NA
bayern_2008_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2008


# Creating non-existing variables ----
bayern_2008_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
bayern_2008_kommunalwahlen_data_sub[ , Bundesland := "Bayern"]
bayern_2008_kommunalwahlen_data_sub[ , Gebietsname := ""]
bayern_2008_kommunalwahlen_data_sub[ , election_year := "2008"]
bayern_2008_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
bayern_2008_kommunalwahlen_data_sub[ , IDIRB := ""]
bayern_2008_kommunalwahlen_data_sub[ , IDBA := ""]

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
bayern_2008_kommunalwahlen_data_sub <- bayern_2008_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2008_kommunalwahlen_data_sub <-
  bayern_2008_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
bayern_2008_kommunalwahlen_data_sub$Turnout <- as.numeric(bayern_2008_kommunalwahlen_data_sub$Wähler) / as.numeric(bayern_2008_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 2014 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2014 <- data.table(read_excel("raw/bayern/bayern_2014new.xlsx", sheet="results"))

#### Recoding ----
# Create new dataframe ----
bayern_2014_kommunalwahlen_data_sub <- NA
bayern_2014_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2014


# Creating non-existing variables ----
bayern_2014_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
bayern_2014_kommunalwahlen_data_sub[ , Bundesland := "Bayern"]
bayern_2014_kommunalwahlen_data_sub[ , Gebietsname := ""]
bayern_2014_kommunalwahlen_data_sub[ , election_year := "2014"]
bayern_2014_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
bayern_2014_kommunalwahlen_data_sub[ , IDIRB := ""]
bayern_2014_kommunalwahlen_data_sub[ , IDBA := ""]

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
bayern_2014_kommunalwahlen_data_sub <- bayern_2014_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2014_kommunalwahlen_data_sub <-
  bayern_2014_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
bayern_2014_kommunalwahlen_data_sub$Turnout <- as.numeric(bayern_2014_kommunalwahlen_data_sub$Wähler) / as.numeric(bayern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Bayern 2020 Gemeinderatswahlen ----
bayern_kommunalwahlen_data_2020 <- data.table(read_excel("raw/bayern/bayern_2020new.xlsx", sheet="results"))

#### Recoding ----
# Create new dataframe ----
bayern_2020_kommunalwahlen_data_sub <- NA
bayern_2020_kommunalwahlen_data_sub <- bayern_kommunalwahlen_data_2020


# Creating non-existing variables ----
bayern_2020_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
bayern_2020_kommunalwahlen_data_sub[ , Bundesland := "Bayern"]
bayern_2020_kommunalwahlen_data_sub[ , Gebietsname := ""]
bayern_2020_kommunalwahlen_data_sub[ , election_year := "2020"]
bayern_2020_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
bayern_2020_kommunalwahlen_data_sub[ , IDIRB := ""]
bayern_2020_kommunalwahlen_data_sub[ , IDBA := ""]

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
bayern_2020_kommunalwahlen_data_sub <- bayern_2020_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bayern_2020_kommunalwahlen_data_sub <-
  bayern_2020_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
bayern_2020_kommunalwahlen_data_sub$Turnout <- as.numeric(bayern_2020_kommunalwahlen_data_sub$Wähler) / as.numeric(bayern_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)



####### Merge files and save overall output for Bayern ----
# Merge
bayern_kommunalwahlen <- rbind(bayern_1996_kommunalwahlen_data_sub,
                               bayern_2002_kommunalwahlen_data_sub,
                               bayern_2008_kommunalwahlen_data_sub,
                               bayern_2014_kommunalwahlen_data_sub,
                               bayern_2020_kommunalwahlen_data_sub)

# Replace INF at Turnout
bayern_kommunalwahlen[bayern_kommunalwahlen == "-"] <- NA


# Fix AGS
bayern_kommunalwahlen <- bayern_kommunalwahlen %>%
  filter(nchar(AGS_8dig)>3) %>%
  mutate(
    AGS_8dig = case_when(nchar(AGS_8dig)==5 ~ paste0(AGS_8dig, "000"),
                         TRUE ~ AGS_8dig)
  )
  

# Filter out Gemeindefreie Gebiete and Landkreise
bayern_kommunalwahlen <- bayern_kommunalwahlen %>%
  filter(!Gebietsname == "Gemeindefreie Gebiete",
         !grepl("(Lkr)", Gebietsname))


# ----
######### THUERINGEN ----
###### Thueringen 1994 Gemeinderatswahlen ----
#### Load election data ----
thueringen_1994_kommunalwahlen_data <- as.data.table(read_csv("raw/thueringen/thueringen_1999.csv"))

thueringen_1994_kommunalwahlen_data <- as.data.table(read_excel("raw/thueringen/thueringen_1994.xlsx", sheet="thueringen_1994"))
thueringen_1994_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/thueringen/thueringen_1994_sitze.xlsx", sheet="thueringen_1994_sitze"))
names(thueringen_1994_kommunalwahlen_data_sitze) <- str_c(names(thueringen_1994_kommunalwahlen_data_sitze), "_sitze", sep="")


# Merge Stimmen and Sitze
thueringen_1994_kommunalwahlen_data <- merge(thueringen_1994_kommunalwahlen_data, thueringen_1994_kommunalwahlen_data_sitze, by.x="Gemeinde", by.y="Gemeinde_sitze")
thueringen_1994_kommunalwahlen_data[thueringen_1994_kommunalwahlen_data == "-"] <- NA

#### Delete white space ----
names(thueringen_1994_kommunalwahlen_data) <-  str_replace_all(names(thueringen_1994_kommunalwahlen_data), fixed(" "), "")


#### Recoding ----
# Create new dataframe ----
thueringen_1994_kommunalwahlen_data_sub <- thueringen_1994_kommunalwahlen_data

names(thueringen_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_1994_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
thueringen_1994_kommunalwahlen_data_sub[ , Bundesland := "Thueringen"]
thueringen_1994_kommunalwahlen_data_sub[ , Gebietsname := ""]
thueringen_1994_kommunalwahlen_data_sub[ , election_year := "1994"]
thueringen_1994_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
thueringen_1994_kommunalwahlen_data_sub[ , IDIRB := ""]
thueringen_1994_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
thueringen_1994_kommunalwahlen_data_sub$AGS_8dig <- thueringen_1994_kommunalwahlen_data_sub$Gemeinde
thueringen_1994_kommunalwahlen_data_sub$Gebietsname <- thueringen_1994_kommunalwahlen_data_sub$Gemeindename
thueringen_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$Wahlberechtigte)
thueringen_1994_kommunalwahlen_data_sub$Wähler <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$Waehler)
thueringen_1994_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$Gueltige_stimmen)

thueringen_1994_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$CDU)
thueringen_1994_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_CDU)

thueringen_1994_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$SPD)
thueringen_1994_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_SPD)

thueringen_1994_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$PDS)
thueringen_1994_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_DIELINKE)

thueringen_1994_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$GRE)
thueringen_1994_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_GRÜNE)

thueringen_1994_kommunalwahlen_data_sub$abs_AfD <- NA
thueringen_1994_kommunalwahlen_data_sub$abs_AfD <- NA

thueringen_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
thueringen_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA

thueringen_1994_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$FDP)
thueringen_1994_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_FDP)

thueringen_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$FREIE_WAEHLER)
thueringen_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_FREIEWÄHLER)

thueringen_1994_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$Waehlergruppen)
thueringen_1994_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$abs_Wählergruppen)


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
thueringen_1994_kommunalwahlen_data_sub$sitze_FDP <- thueringen_1994_kommunalwahlen_data_sub$FDP_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_1994_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_1994_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_1994_kommunalwahlen_data_sub$Waehlergruppen_sitze

# Creating new dataframe with selected vars ----
thueringen_1994_kommunalwahlen_data_sub <- thueringen_1994_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Wählergruppen,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Wählergruppen,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Wählergruppen)]

thueringen_1994_kommunalwahlen_data_sub[thueringen_1994_kommunalwahlen_data_sub == "-"] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_1994_kommunalwahlen_data_sub <-
  thueringen_1994_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains("abs")), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
thueringen_1994_kommunalwahlen_data_sub$Turnout <- as.numeric(thueringen_1994_kommunalwahlen_data_sub$Wähler) / as.numeric(thueringen_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 1999 Gemeinderatswahlen ----
#### Load election data ----
thueringen_1999_kommunalwahlen_data <- as.data.table(read_excel("raw/thueringen/thueringen_1999.xlsx", sheet="thueringen_1999"))
thueringen_1999_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/thueringen/thueringen_1999_sitze.xlsx", sheet="thueringen_1999_sitze"))
names(thueringen_1999_kommunalwahlen_data_sitze) <- str_c(names(thueringen_1999_kommunalwahlen_data_sitze), "_sitze", sep="")


# Merge Stimmen and Sitze
thueringen_1999_kommunalwahlen_data <- merge(thueringen_1999_kommunalwahlen_data, thueringen_1999_kommunalwahlen_data_sitze, by.x="Gemeinde", by.y="Gemeinde_sitze")
thueringen_1999_kommunalwahlen_data[thueringen_1999_kommunalwahlen_data == "-"] <- NA

#### Delete white space ----
names(thueringen_1999_kommunalwahlen_data) <-  str_replace_all(names(thueringen_1999_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
thueringen_1999_kommunalwahlen_data_sub <- thueringen_1999_kommunalwahlen_data

names(thueringen_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_1999_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
thueringen_1999_kommunalwahlen_data_sub[ , Bundesland := "Thueringen"]
thueringen_1999_kommunalwahlen_data_sub[ , Gebietsname := ""]
thueringen_1999_kommunalwahlen_data_sub[ , election_year := "1999"]
thueringen_1999_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
thueringen_1999_kommunalwahlen_data_sub[ , IDIRB := ""]
thueringen_1999_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
thueringen_1999_kommunalwahlen_data_sub$AGS_8dig <- thueringen_1999_kommunalwahlen_data_sub$Gemeinde
thueringen_1999_kommunalwahlen_data_sub$Gebietsname <- thueringen_1999_kommunalwahlen_data_sub$Gemeindename
thueringen_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$Wahlberechtigte)
thueringen_1999_kommunalwahlen_data_sub$Wähler <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$Waehler)
thueringen_1999_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$Gueltige_stimmen)

thueringen_1999_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$CDU)
thueringen_1999_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_CDU)

thueringen_1999_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$SPD)
thueringen_1999_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_SPD)

thueringen_1999_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$PDS)
thueringen_1999_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_DIELINKE)

thueringen_1999_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$GRE)
thueringen_1999_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_GRÜNE)

thueringen_1999_kommunalwahlen_data_sub$abs_AfD <- as.numeric(0)
thueringen_1999_kommunalwahlen_data_sub$abs_AfD <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_AfD)

thueringen_1999_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_1999_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_PIRATEN)

thueringen_1999_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$FDP)
thueringen_1999_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_FDP)

thueringen_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$FREIE_WAEHLER)
thueringen_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_FREIEWÄHLER)

thueringen_1999_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$Waehlergruppen)
thueringen_1999_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$abs_Wählergruppen)


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
thueringen_1999_kommunalwahlen_data_sub$sitze_FDP <- thueringen_1999_kommunalwahlen_data_sub$FDP_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_1999_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_1999_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_1999_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_1999_kommunalwahlen_data_sub <- thueringen_1999_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Wählergruppen,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Wählergruppen,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Wählergruppen)]

thueringen_1999_kommunalwahlen_data_sub[thueringen_1999_kommunalwahlen_data_sub == "-"] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_1999_kommunalwahlen_data_sub <-
  thueringen_1999_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains("abs")), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
thueringen_1999_kommunalwahlen_data_sub$Turnout <- as.numeric(thueringen_1999_kommunalwahlen_data_sub$Wähler) / as.numeric(thueringen_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2004 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2004_kommunalwahlen_data <- as.data.table(read_excel("raw/thueringen/thueringen_2004.xlsx", sheet="thueringen_2004"))
thueringen_2004_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/thueringen/thueringen_2004_sitze.xlsx", sheet="thueringen_2004_sitze"))
names(thueringen_2004_kommunalwahlen_data_sitze) <- str_c(names(thueringen_2004_kommunalwahlen_data_sitze), "_sitze", sep="")


# Merge Stimmen and Sitze
thueringen_2004_kommunalwahlen_data <- merge(thueringen_2004_kommunalwahlen_data, thueringen_2004_kommunalwahlen_data_sitze, by.x="Gemeinde", by.y="Gemeinde_sitze")
thueringen_2004_kommunalwahlen_data[thueringen_2004_kommunalwahlen_data == "-"] <- NA

#### Delete white space ----
names(thueringen_2004_kommunalwahlen_data) <-  str_replace_all(names(thueringen_2004_kommunalwahlen_data), fixed(" "), "")


#### Recoding ----
# Create new dataframe ----
thueringen_2004_kommunalwahlen_data_sub <- thueringen_2004_kommunalwahlen_data

names(thueringen_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2004_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
thueringen_2004_kommunalwahlen_data_sub[ , Bundesland := "Thueringen"]
thueringen_2004_kommunalwahlen_data_sub[ , Gebietsname := ""]
thueringen_2004_kommunalwahlen_data_sub[ , election_year := "2004"]
thueringen_2004_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
thueringen_2004_kommunalwahlen_data_sub[ , IDIRB := ""]
thueringen_2004_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
thueringen_2004_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2004_kommunalwahlen_data_sub$Gemeinde
thueringen_2004_kommunalwahlen_data_sub$Gebietsname <- thueringen_2004_kommunalwahlen_data_sub$Gemeindename
thueringen_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$Wahlberechtigte)
thueringen_2004_kommunalwahlen_data_sub$Wähler <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$Waehler)
thueringen_2004_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$Gueltige_stimmen)

thueringen_2004_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$CDU)
thueringen_2004_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_CDU)

thueringen_2004_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$SPD)
thueringen_2004_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_SPD)

thueringen_2004_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$PDS)
thueringen_2004_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_DIELINKE)

thueringen_2004_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$GRE)
thueringen_2004_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_GRÜNE)

thueringen_2004_kommunalwahlen_data_sub$abs_AfD <- as.numeric(0)
thueringen_2004_kommunalwahlen_data_sub$abs_AfD <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_AfD)

thueringen_2004_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2004_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_PIRATEN)

thueringen_2004_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$FDP)
thueringen_2004_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_FDP)

thueringen_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$FREIE_WAEHLER)
thueringen_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_FREIEWÄHLER)

thueringen_2004_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$Waehlergruppen)
thueringen_2004_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$abs_Wählergruppen)


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
thueringen_2004_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2004_kommunalwahlen_data_sub$FDP_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2004_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2004_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2004_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2004_kommunalwahlen_data_sub <- thueringen_2004_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Wählergruppen,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Wählergruppen,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Wählergruppen)]

thueringen_2004_kommunalwahlen_data_sub[thueringen_2004_kommunalwahlen_data_sub == "-"] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2004_kommunalwahlen_data_sub <-
  thueringen_2004_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains("abs")), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
thueringen_2004_kommunalwahlen_data_sub$Turnout <- as.numeric(thueringen_2004_kommunalwahlen_data_sub$Wähler) / as.numeric(thueringen_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2009 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2009_kommunalwahlen_data <- as.data.table(read_excel("raw/thueringen/thueringen_2009.xlsx", sheet="thueringen_2009"))
thueringen_2009_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/thueringen/thueringen_2009_sitze.xlsx", sheet="thueringen_2009_sitze"))
names(thueringen_2009_kommunalwahlen_data_sitze) <- str_c(names(thueringen_2009_kommunalwahlen_data_sitze), "_sitze", sep="")


# Merge Stimmen and Sitze
thueringen_2009_kommunalwahlen_data <- merge(thueringen_2009_kommunalwahlen_data, thueringen_2009_kommunalwahlen_data_sitze, by.x="Gemeinde", by.y="Gemeinde_sitze")
thueringen_2009_kommunalwahlen_data[thueringen_2009_kommunalwahlen_data == "-"] <- NA

#### Delete white space ----
names(thueringen_2009_kommunalwahlen_data) <-  str_replace_all(names(thueringen_2009_kommunalwahlen_data), fixed(" "), "")


#### Recoding ----
# Create new dataframe ----
thueringen_2009_kommunalwahlen_data_sub <- thueringen_2009_kommunalwahlen_data

names(thueringen_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2009_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
thueringen_2009_kommunalwahlen_data_sub[ , Bundesland := "Thueringen"]
thueringen_2009_kommunalwahlen_data_sub[ , Gebietsname := ""]
thueringen_2009_kommunalwahlen_data_sub[ , election_year := "2009"]
thueringen_2009_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
thueringen_2009_kommunalwahlen_data_sub[ , IDIRB := ""]
thueringen_2009_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
thueringen_2009_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2009_kommunalwahlen_data_sub$Gemeinde
thueringen_2009_kommunalwahlen_data_sub$Gebietsname <- thueringen_2009_kommunalwahlen_data_sub$Gemeindename
thueringen_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$Wahlberechtigte)
thueringen_2009_kommunalwahlen_data_sub$Wähler <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$Waehler)
thueringen_2009_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$Gueltige_stimmen)

thueringen_2009_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$CDU)
thueringen_2009_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_CDU)

thueringen_2009_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$SPD)
thueringen_2009_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_SPD)

thueringen_2009_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$DIE_LINKE)
thueringen_2009_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_DIELINKE)

thueringen_2009_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$GRE)
thueringen_2009_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_GRÜNE)

thueringen_2009_kommunalwahlen_data_sub$abs_AfD <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$AfD)
thueringen_2009_kommunalwahlen_data_sub$abs_AfD <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_AfD)

thueringen_2009_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2009_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_PIRATEN)

thueringen_2009_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$FDP)
thueringen_2009_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_FDP)

thueringen_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$FREIE_WAEHLER)
thueringen_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_FREIEWÄHLER)

thueringen_2009_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$Waehlergruppen)
thueringen_2009_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$abs_Wählergruppen)


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
thueringen_2009_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2009_kommunalwahlen_data_sub$FDP_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2009_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2009_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2009_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2009_kommunalwahlen_data_sub <- thueringen_2009_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Wählergruppen,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Wählergruppen,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Wählergruppen)]

thueringen_2009_kommunalwahlen_data_sub[thueringen_2009_kommunalwahlen_data_sub == "-"] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2009_kommunalwahlen_data_sub <-
  thueringen_2009_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains("abs")), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
thueringen_2009_kommunalwahlen_data_sub$Turnout <- as.numeric(thueringen_2009_kommunalwahlen_data_sub$Wähler) / as.numeric(thueringen_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2014 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2014_kommunalwahlen_data <- as.data.table(read_excel("raw/thueringen/thueringen_2014.xlsx", sheet="thueringen_2014"))
thueringen_2014_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/thueringen/thueringen_2014_sitze.xlsx", sheet="thueringen_2014_sitze"))
names(thueringen_2014_kommunalwahlen_data_sitze) <- str_c(names(thueringen_2014_kommunalwahlen_data_sitze), "_sitze", sep="")


# Merge Stimmen and Sitze
thueringen_2014_kommunalwahlen_data <- merge(thueringen_2014_kommunalwahlen_data, thueringen_2014_kommunalwahlen_data_sitze, by.x="Gemeinde", by.y="Gemeinde_sitze")
thueringen_2014_kommunalwahlen_data[thueringen_2014_kommunalwahlen_data == "-"] <- NA

#### Delete white space ----
names(thueringen_2014_kommunalwahlen_data) <-  str_replace_all(names(thueringen_2014_kommunalwahlen_data), fixed(" "), "")


#### Recoding ----
# Create new dataframe ----
thueringen_2014_kommunalwahlen_data_sub <- thueringen_2014_kommunalwahlen_data

names(thueringen_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2014_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
thueringen_2014_kommunalwahlen_data_sub[ , Bundesland := "Thueringen"]
thueringen_2014_kommunalwahlen_data_sub[ , Gebietsname := ""]
thueringen_2014_kommunalwahlen_data_sub[ , election_year := "2014"]
thueringen_2014_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
thueringen_2014_kommunalwahlen_data_sub[ , IDIRB := ""]
thueringen_2014_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
thueringen_2014_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2014_kommunalwahlen_data_sub$Gemeinde
thueringen_2014_kommunalwahlen_data_sub$Gebietsname <- thueringen_2014_kommunalwahlen_data_sub$Gemeindename
thueringen_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$Wahlberechtigte)
thueringen_2014_kommunalwahlen_data_sub$Wähler <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$Waehler)
thueringen_2014_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$Gueltige_stimmen)

thueringen_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$CDU)
thueringen_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_CDU)

thueringen_2014_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$SPD)
thueringen_2014_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_SPD)

thueringen_2014_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$DIE_LINKE)
thueringen_2014_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_DIELINKE)

thueringen_2014_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$GRE)
thueringen_2014_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_GRÜNE)

thueringen_2014_kommunalwahlen_data_sub$abs_AfD <- thueringen_2014_kommunalwahlen_data_sub$AfD
thueringen_2014_kommunalwahlen_data_sub$abs_AfD <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_AfD)

thueringen_2014_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2014_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_PIRATEN)

thueringen_2014_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$FDP)
thueringen_2014_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_FDP)

thueringen_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$FREIE_WAEHLER)
thueringen_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER)

thueringen_2014_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$Waehlergruppen)
thueringen_2014_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$abs_Wählergruppen)

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
thueringen_2014_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2014_kommunalwahlen_data_sub$FDP_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2014_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2014_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2014_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2014_kommunalwahlen_data_sub <- thueringen_2014_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Wählergruppen,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Wählergruppen,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Wählergruppen)]

thueringen_2014_kommunalwahlen_data_sub[thueringen_2014_kommunalwahlen_data_sub == "-"] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2014_kommunalwahlen_data_sub <-
  thueringen_2014_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains("abs")), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
thueringen_2014_kommunalwahlen_data_sub$Turnout <- as.numeric(thueringen_2014_kommunalwahlen_data_sub$Wähler) / as.numeric(thueringen_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### Thueringen 2019 Gemeinderatswahlen ----
#### Load election data ----

thueringen_2019_kommunalwahlen_data <- as.data.table(read_excel("raw/thueringen/thueringen_2019.xlsx", sheet="thueringen_2019"))
thueringen_2019_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/thueringen/thueringen_2019_sitze.xlsx", sheet="thueringen_2019_sitze"))
names(thueringen_2019_kommunalwahlen_data_sitze) <- str_c(names(thueringen_2019_kommunalwahlen_data_sitze), "_sitze", sep="")


# Merge Stimmen and Sitze
thueringen_2019_kommunalwahlen_data <- merge(thueringen_2019_kommunalwahlen_data, thueringen_2019_kommunalwahlen_data_sitze, by.x="Gemeinde", by.y="Gemeinde_sitze")
thueringen_2019_kommunalwahlen_data[thueringen_2019_kommunalwahlen_data == "-"] <- NA

#### Delete white space ----
names(thueringen_2019_kommunalwahlen_data) <-  str_replace_all(names(thueringen_2019_kommunalwahlen_data), fixed(" "), "")


#### Recoding ----
# Create new dataframe ----
thueringen_2019_kommunalwahlen_data_sub <- thueringen_2019_kommunalwahlen_data

names(thueringen_2019_kommunalwahlen_data_sub)

# Creating non-existing variables ----
thueringen_2019_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
thueringen_2019_kommunalwahlen_data_sub[ , Bundesland := "Thueringen"]
thueringen_2019_kommunalwahlen_data_sub[ , Gebietsname := ""]
thueringen_2019_kommunalwahlen_data_sub[ , election_year := "2019"]
thueringen_2019_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
thueringen_2019_kommunalwahlen_data_sub[ , IDIRB := ""]
thueringen_2019_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
thueringen_2019_kommunalwahlen_data_sub$AGS_8dig <- thueringen_2019_kommunalwahlen_data_sub$Gemeinde
thueringen_2019_kommunalwahlen_data_sub$Gebietsname <- thueringen_2019_kommunalwahlen_data_sub$Gemeindename
thueringen_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$Wahlberechtigte)
thueringen_2019_kommunalwahlen_data_sub$Wähler <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$Waehler)
thueringen_2019_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$Gueltige_stimmen)

thueringen_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$CDU)
thueringen_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_CDU)

thueringen_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$SPD)
thueringen_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_SPD)

thueringen_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$PDS)
thueringen_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_DIELINKE)

thueringen_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$GRE)
thueringen_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_GRÜNE)

thueringen_2019_kommunalwahlen_data_sub$abs_AfD <- thueringen_2019_kommunalwahlen_data_sub$AfD
thueringen_2019_kommunalwahlen_data_sub$abs_AfD <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_AfD)

thueringen_2019_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(0)
thueringen_2019_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_PIRATEN)

thueringen_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$FDP)
thueringen_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_FDP)

thueringen_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$FREIE_WAEHLER)
thueringen_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER)

thueringen_2019_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$Waehlergruppen)
thueringen_2019_kommunalwahlen_data_sub$abs_Wählergruppen <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$abs_Wählergruppen)

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
thueringen_2019_kommunalwahlen_data_sub$sitze_FDP <- thueringen_2019_kommunalwahlen_data_sub$FDP_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- thueringen_2019_kommunalwahlen_data_sub$FREIE_WAEHLER_sitze
thueringen_2019_kommunalwahlen_data_sub$sitze_Wählergruppen <- thueringen_2019_kommunalwahlen_data_sub$Waehlergruppen_sitze


# Creating new dataframe with selected vars ----
thueringen_2019_kommunalwahlen_data_sub <- thueringen_2019_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Wählergruppen,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Wählergruppen,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Wählergruppen)]

thueringen_2019_kommunalwahlen_data_sub[thueringen_2019_kommunalwahlen_data_sub == "-"] <- NA


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

thueringen_2019_kommunalwahlen_data_sub <-
  thueringen_2019_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains("abs")), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
thueringen_2019_kommunalwahlen_data_sub$Turnout <- as.numeric(thueringen_2019_kommunalwahlen_data_sub$Wähler) / as.numeric(thueringen_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


####### Merge files and save overall output for Thueringen ----
# Merge
thueringen_kommunalwahlen <- rbind(thueringen_1994_kommunalwahlen_data_sub, thueringen_1999_kommunalwahlen_data_sub,thueringen_2004_kommunalwahlen_data_sub,thueringen_2009_kommunalwahlen_data_sub,thueringen_2014_kommunalwahlen_data_sub, thueringen_2019_kommunalwahlen_data_sub)

thueringen_kommunalwahlen <-
  thueringen_kommunalwahlen %>%
  mutate(abs_Gemeinsame_Wahlvorschläge = NA,
         gew_Gemeinsame_Wahlvorschläge = NA,
         prop_Gemeinsame_Wahlvorschläge = NA,
         sitze_Gemeinsame_Wahlvorschläge = NA)

# Replace - with NA
thueringen_kommunalwahlen[thueringen_kommunalwahlen == "-"] <- NA

# Fix AGS
thueringen_kommunalwahlen$AGS_8dig <- paste("160", thueringen_kommunalwahlen$AGS_8dig, sep="")

# Save
#write_csv(thueringen_kommunalwahlen, here::here("output/thueringen_kommunalwahlen.csv")

# ----
# ----
######### HAMBURG ----
###### Hamburg 1991-1997 Buergerschaftswahl ----
#### Load election data ----
hamburg_1991_1997_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_1991_1997.xlsx", sheet="results"))

#### Delete white space ----
names(hamburg_1991_1997_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_1991_1997_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_1991_1997_buergerschaftswahl_data_sub <- hamburg_1991_1997_buergerschaftswahl_data

# Creating non-existing variables ----
hamburg_1991_1997_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_1991_1997_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_1991_1997_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_1991_1997_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_1991_1997_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_1991_1997_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_1991_1997_buergerschaftswahl_data_sub <- hamburg_1991_1997_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_1991_1997_buergerschaftswahl_data_sub <-
  hamburg_1991_1997_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_1991_1997_buergerschaftswahl_data_sub$Turnout <- hamburg_1991_1997_buergerschaftswahl_data_sub$Wähler / hamburg_1991_1997_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt


###### Hamburg 2001 Buergerschaftswahl ----
#### Load election data ----
hamburg_2001_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_2001.xls", sheet="summary"))

#### Delete white space ----
names(hamburg_2001_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_2001_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_2001_buergerschaftswahl_data_sub <- hamburg_2001_buergerschaftswahl_data[hamburg_2001_buergerschaftswahl_data$stadtteil_name=="Hamburg"]

names(hamburg_2001_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2001_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2001_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_2001_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_2001_buergerschaftswahl_data_sub[ , election_year := "2001"]
hamburg_2001_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_2001_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_2001_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_2001_buergerschaftswahl_data_sub <- hamburg_2001_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2001_buergerschaftswahl_data_sub <-
  hamburg_2001_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_2001_buergerschaftswahl_data_sub$Turnout <- hamburg_2001_buergerschaftswahl_data_sub$Wähler / hamburg_2001_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt


###### Hamburg 2004 Buergerschaftswahl ----
#### Load election data ----

hamburg_2004_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_2004.xls", sheet="summary"))

#### Delete white space ----
names(hamburg_2004_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_2004_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_2004_buergerschaftswahl_data_sub <- hamburg_2004_buergerschaftswahl_data[hamburg_2004_buergerschaftswahl_data$"...1"=="Hamburg"]

names(hamburg_2004_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2004_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2004_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_2004_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_2004_buergerschaftswahl_data_sub[ , election_year := "2004"]
hamburg_2004_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_2004_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_2004_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_2004_buergerschaftswahl_data_sub <- hamburg_2004_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2004_buergerschaftswahl_data_sub <-
  hamburg_2004_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_2004_buergerschaftswahl_data_sub$Turnout <- hamburg_2004_buergerschaftswahl_data_sub$Wähler / hamburg_2004_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2008 Buergerschaftswahl ----
#### Load election data ----

hamburg_2008_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_2008.xls", sheet="summary"))

#### Delete white space ----
names(hamburg_2008_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_2008_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_2008_buergerschaftswahl_data_sub <- hamburg_2008_buergerschaftswahl_data[hamburg_2008_buergerschaftswahl_data$Stadtteil=="Hamburg"]

names(hamburg_2008_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2008_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2008_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_2008_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_2008_buergerschaftswahl_data_sub[ , election_year := "2008"]
hamburg_2008_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_2008_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_2008_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_2008_buergerschaftswahl_data_sub <- hamburg_2008_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2008_buergerschaftswahl_data_sub <-
  hamburg_2008_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_2008_buergerschaftswahl_data_sub$Turnout <- hamburg_2008_buergerschaftswahl_data_sub$Wähler / hamburg_2008_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2011 Buergerschaftswahl ----
#### Load election data ----

hamburg_2011_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_2011.xls", sheet="summary"))

#### Delete white space ----
names(hamburg_2011_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_2011_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_2011_buergerschaftswahl_data_sub <- hamburg_2011_buergerschaftswahl_data[hamburg_2011_buergerschaftswahl_data$Stadtteil=="Hamburg"]

names(hamburg_2011_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2011_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2011_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_2011_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_2011_buergerschaftswahl_data_sub[ , election_year := "2011"]
hamburg_2011_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_2011_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_2011_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_2011_buergerschaftswahl_data_sub <- hamburg_2011_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2011_buergerschaftswahl_data_sub <-
  hamburg_2011_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_2011_buergerschaftswahl_data_sub$Turnout <- hamburg_2011_buergerschaftswahl_data_sub$Wähler / hamburg_2011_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2015 Buergerschaftswahl ----
#### Load election data ----

hamburg_2015_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_2015.xlsx", sheet="summary"))

#### Delete white space ----
names(hamburg_2015_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_2015_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_2015_buergerschaftswahl_data_sub <- hamburg_2015_buergerschaftswahl_data[hamburg_2015_buergerschaftswahl_data$Wahlkreis=="Hamburg"]

names(hamburg_2015_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2015_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2015_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_2015_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_2015_buergerschaftswahl_data_sub[ , election_year := "2015"]
hamburg_2015_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_2015_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_2015_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_2015_buergerschaftswahl_data_sub <- hamburg_2015_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2015_buergerschaftswahl_data_sub <-
  hamburg_2015_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_2015_buergerschaftswahl_data_sub$Turnout <- hamburg_2015_buergerschaftswahl_data_sub$Wähler / hamburg_2015_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

###### Hamburg 2020 Buergerschaftswahl ----
#### Load election data ----

hamburg_2020_buergerschaftswahl_data <- as.data.table(read_excel("raw/hamburg/hamburg_2020.xlsx", sheet="summary"))

#### Delete white space ----
names(hamburg_2020_buergerschaftswahl_data) <-  str_replace_all(names(hamburg_2020_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hamburg_2020_buergerschaftswahl_data_sub <- hamburg_2020_buergerschaftswahl_data[hamburg_2020_buergerschaftswahl_data$Bezirk=="Hamburg"]

names(hamburg_2020_buergerschaftswahl_data_sub)

# Creating non-existing variables ----
hamburg_2020_buergerschaftswahl_data_sub[ , AGS_8dig := "02000000"] # 8 digits with leading zero
hamburg_2020_buergerschaftswahl_data_sub[ , Bundesland := "Hamburg"]
hamburg_2020_buergerschaftswahl_data_sub[ , Gebietsname := "Hamburg"]
hamburg_2020_buergerschaftswahl_data_sub[ , election_year := "2020"]
hamburg_2020_buergerschaftswahl_data_sub[ , election_type := "Buergerschaftswahl (Gesamtstimmen Landesliste)"]
hamburg_2020_buergerschaftswahl_data_sub[ , IDIRB := ""]
hamburg_2020_buergerschaftswahl_data_sub[ , IDBA := ""]

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
hamburg_2020_buergerschaftswahl_data_sub <- hamburg_2020_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hamburg_2020_buergerschaftswahl_data_sub <-
  hamburg_2020_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hamburg_2020_buergerschaftswahl_data_sub$Turnout <- hamburg_2020_buergerschaftswahl_data_sub$Wähler / hamburg_2020_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

####### Merge files and save overall output for Hamburg ----
# Merge
hamburg_kommunalwahlen <- rbind(hamburg_1991_1997_buergerschaftswahl_data_sub,
                                hamburg_2001_buergerschaftswahl_data_sub,
                                hamburg_2004_buergerschaftswahl_data_sub,
                                hamburg_2008_buergerschaftswahl_data_sub,
                                hamburg_2011_buergerschaftswahl_data_sub,
                                hamburg_2015_buergerschaftswahl_data_sub, 
                                hamburg_2020_buergerschaftswahl_data_sub)

# Replace INF at Turnout
hamburg_kommunalwahlen$Turnout <-  str_replace_all(hamburg_kommunalwahlen$Turnout, fixed("Inf"), NA)

# Replace - with NA
hamburg_kommunalwahlen[hamburg_kommunalwahlen == "-"] <- NA

# Save
#write_csv(hamburg_kommunalwahlen, here::here("output/hamburg_kommunalwahlen.csv"))

######### BERLIN ----
###### Berlin 1990 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_1990_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_1990.xlsx", sheet="Zweitstimme"))
names(berlin_1990_kommunalwahlen_data)

#### Recoding ----
# Create new dataframe ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen <- berlin_1990_kommunalwahlen_data[berlin_1990_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_1990_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "1990"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_1990_kommunalwahlen_data_sub_zweitstimmen <- berlin_1990_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1990_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_1990_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_1990_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 1995 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_1995_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_1995.xlsx", sheet="Erststimme"))
names(berlin_1995_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1995_kommunalwahlen_data) <-  str_replace_all(names(berlin_1995_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_1995_kommunalwahlen_data_sub_erststimmen <- berlin_1995_kommunalwahlen_data[berlin_1995_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_1995_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , election_year := "1995"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_1995_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_1995_kommunalwahlen_data_sub_erststimmen <- berlin_1995_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1995_kommunalwahlen_data_sub_erststimmen <-
  berlin_1995_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_1995_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_1995_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_1995_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 1995 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_1995_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_1995.xlsx", sheet="Zweitstimme"))
names(berlin_1995_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1995_kommunalwahlen_data) <-  str_replace_all(names(berlin_1995_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen <- berlin_1995_kommunalwahlen_data[berlin_1995_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_1995_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "1995"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_1995_kommunalwahlen_data_sub_zweitstimmen <- berlin_1995_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1995_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_1995_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_1995_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 1999 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_1999_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_1999.xlsx", sheet="Erststimme"))
names(berlin_1999_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1999_kommunalwahlen_data) <-  str_replace_all(names(berlin_1999_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_1999_kommunalwahlen_data_sub_erststimmen <- berlin_1999_kommunalwahlen_data[berlin_1999_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_1999_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , election_year := "1999"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_1999_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_1999_kommunalwahlen_data_sub_erststimmen <- berlin_1999_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1999_kommunalwahlen_data_sub_erststimmen <-
  berlin_1999_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_1999_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_1999_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_1999_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 1999 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_1999_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_1999.xlsx", sheet="Zweitstimme"))
names(berlin_1999_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_1999_kommunalwahlen_data) <-  str_replace_all(names(berlin_1999_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen <- berlin_1999_kommunalwahlen_data[berlin_1999_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_1999_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "1999"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_1999_kommunalwahlen_data_sub_zweitstimmen <- berlin_1999_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_1999_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_1999_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_1999_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 2001 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2001_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2001.xlsx", sheet="Erststimme"))
names(berlin_2001_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2001_kommunalwahlen_data) <-  str_replace_all(names(berlin_2001_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2001_kommunalwahlen_data_sub_erststimmen <- berlin_2001_kommunalwahlen_data[berlin_2001_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2001_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , election_year := "2001"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_2001_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_2001_kommunalwahlen_data_sub_erststimmen <- berlin_2001_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2001_kommunalwahlen_data_sub_erststimmen <-
  berlin_2001_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2001_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_2001_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_2001_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 2001 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2001_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2001.xlsx", sheet="Zweitstimme"))
names(berlin_2001_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2001_kommunalwahlen_data) <-  str_replace_all(names(berlin_2001_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen <- berlin_2001_kommunalwahlen_data[berlin_2001_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2001_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "2001"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_2001_kommunalwahlen_data_sub_zweitstimmen <- berlin_2001_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2001_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2001_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_2001_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 2006 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2006_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2006.xlsx", sheet="Erststimme"))
names(berlin_2006_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2006_kommunalwahlen_data) <-  str_replace_all(names(berlin_2006_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2006_kommunalwahlen_data_sub_erststimmen <- berlin_2006_kommunalwahlen_data[berlin_2006_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2006_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , election_year := "2006"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_2006_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_2006_kommunalwahlen_data_sub_erststimmen <- berlin_2006_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2006_kommunalwahlen_data_sub_erststimmen <-
  berlin_2006_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2006_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_2006_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_2006_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 2006 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2006_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2006.xlsx", sheet="Zweitstimme"))
names(berlin_2006_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2006_kommunalwahlen_data) <-  str_replace_all(names(berlin_2006_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen <- berlin_2006_kommunalwahlen_data[berlin_2006_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2006_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "2006"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_2006_kommunalwahlen_data_sub_zweitstimmen <- berlin_2006_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2006_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2006_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_2006_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 2011 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2011_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2011.xlsx", sheet="Erststimme"))
names(berlin_2011_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2011_kommunalwahlen_data) <-  str_replace_all(names(berlin_2011_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2011_kommunalwahlen_data_sub_erststimmen <- berlin_2011_kommunalwahlen_data[berlin_2011_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2011_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , election_year := "2011"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_2011_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_2011_kommunalwahlen_data_sub_erststimmen <- berlin_2011_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2011_kommunalwahlen_data_sub_erststimmen <-
  berlin_2011_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2011_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_2011_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_2011_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 2011 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2011_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2011.xlsx", sheet="Zweitstimme"))
names(berlin_2011_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2011_kommunalwahlen_data) <-  str_replace_all(names(berlin_2011_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen <- berlin_2011_kommunalwahlen_data[berlin_2011_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2011_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "2011"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_2011_kommunalwahlen_data_sub_zweitstimmen <- berlin_2011_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2011_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2011_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_2011_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 2016 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2016_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2016.xlsx", sheet="Erststimme"))
names(berlin_2016_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2016_kommunalwahlen_data) <-  str_replace_all(names(berlin_2016_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2016_kommunalwahlen_data_sub_erststimmen <- berlin_2016_kommunalwahlen_data[berlin_2016_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2016_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , election_year := "2016"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_2016_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_2016_kommunalwahlen_data_sub_erststimmen <- berlin_2016_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2016_kommunalwahlen_data_sub_erststimmen <-
  berlin_2016_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2016_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_2016_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_2016_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 2016 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2016_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2016.xlsx", sheet="Zweitstimme"))
names(berlin_2016_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2016_kommunalwahlen_data) <-  str_replace_all(names(berlin_2016_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen <- berlin_2016_kommunalwahlen_data[berlin_2016_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2016_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "2016"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_2016_kommunalwahlen_data_sub_zweitstimmen <- berlin_2016_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2016_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2016_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_2016_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

###### Berlin 2021 Kommunalwahlen Erststimmen ----
#### Load election data ----
berlin_2021_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2021.xlsx", sheet="AGH_W1"))
names(berlin_2021_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2021_kommunalwahlen_data) <-  str_replace_all(names(berlin_2021_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2021_kommunalwahlen_data_sub_erststimmen <- berlin_2021_kommunalwahlen_data[berlin_2021_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2021_kommunalwahlen_data_sub_erststimmen)

# Creating non-existing variables ----
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , Bundesland := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , Gebietsname := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , election_year := "2021"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , election_type := "Abgeordnetenhauswahl (Erststimmen)"]
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , IDIRB := ""]
berlin_2021_kommunalwahlen_data_sub_erststimmen[ , IDBA := ""]

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
berlin_2021_kommunalwahlen_data_sub_erststimmen <- berlin_2021_kommunalwahlen_data_sub_erststimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2021_kommunalwahlen_data_sub_erststimmen <-
  berlin_2021_kommunalwahlen_data_sub_erststimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2021_kommunalwahlen_data_sub_erststimmen$Turnout <- as.numeric(berlin_2021_kommunalwahlen_data_sub_erststimmen$Wähler) / as.numeric(berlin_2021_kommunalwahlen_data_sub_erststimmen$Wahlberechtigteinsgesamt)

###### Berlin 2021 Kommunalwahlen Zweitstimmen ----
#### Load election data ----
berlin_2021_kommunalwahlen_data <- as.data.table(read_excel("raw/berlin/berlin_2021.xlsx", sheet="AGH_W2"))
names(berlin_2021_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(berlin_2021_kommunalwahlen_data) <-  str_replace_all(names(berlin_2021_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen <- berlin_2021_kommunalwahlen_data[berlin_2021_kommunalwahlen_data$Bezirksname=="Berlin"]

names(berlin_2021_kommunalwahlen_data_sub_zweitstimmen)

# Creating non-existing variables ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , AGS_8dig := "11000000"] # 8 digits with leading zero
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , Bundesland := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , Gebietsname := "Berlin"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , election_year := "2021"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , election_type := "Abgeordnetenhauswahl (Zweitstimmen)"]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , IDIRB := ""]
berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ , IDBA := ""]

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
berlin_2021_kommunalwahlen_data_sub_zweitstimmen <- berlin_2021_kommunalwahlen_data_sub_zweitstimmen[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

berlin_2021_kommunalwahlen_data_sub_zweitstimmen <-
  berlin_2021_kommunalwahlen_data_sub_zweitstimmen %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Turnout <- as.numeric(berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wähler) / as.numeric(berlin_2021_kommunalwahlen_data_sub_zweitstimmen$Wahlberechtigteinsgesamt)

####### Merge files and save overall output for Berlin ----
# Merge
berlin_kommunalwahlen <- rbind(berlin_1990_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_1995_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_1999_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_2001_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_2006_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_2011_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_2016_kommunalwahlen_data_sub_zweitstimmen,
                               berlin_2021_kommunalwahlen_data_sub_zweitstimmen)

# Replace INF at Turnout
berlin_kommunalwahlen$Turnout <-  str_replace_all(berlin_kommunalwahlen$Turnout, fixed("Inf"), NA)

# Replace - with NA
berlin_kommunalwahlen[berlin_kommunalwahlen == "-"] <- NA

# Save
#write_csv(berlin_kommunalwahlen, here::here("output/berlin_kommunalwahlen.csv"))


######### NRW ----
###### NRW 1994 Kommunalwahlen ----
#### Load election data ----
nrw_1994_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_1994.xlsx", sheet="summary"))
nrw_1994_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/nrw/nrw_1994_sitze.xlsx", sheet="summary"))

# Merge Stimmen and Sitze
nrw_1994_kommunalwahlen_data <- merge(nrw_1994_kommunalwahlen_data, nrw_1994_kommunalwahlen_data_sitze, by.x="Bezirksnummer", by.y="Bezirksnummer")

names(nrw_1994_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_1994_kommunalwahlen_data) <-  str_replace_all(names(nrw_1994_kommunalwahlen_data), fixed(" "), "")

# Delete white space in Bezirksnummer ----
nrw_1994_kommunalwahlen_data$Bezirksnummer <-  str_replace_all(nrw_1994_kommunalwahlen_data$Bezirksnummer, fixed(" "), "")

# Create new dataframe ----
nrw_1994_kommunalwahlen_data_sub <- nrw_1994_kommunalwahlen_data

names(nrw_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_1994_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_1994_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_1994_kommunalwahlen_data_sub[ , Gebietsname := ""]
nrw_1994_kommunalwahlen_data_sub[ , election_year := "1994"]
nrw_1994_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_1994_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_1994_kommunalwahlen_data_sub[ , IDBA := ""]

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
nrw_1994_kommunalwahlen_data_sub <- nrw_1994_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_1994_kommunalwahlen_data_sub <-
  nrw_1994_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_1994_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_1994_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 1999 Kommunalwahlen ----
#### Load election data ----
nrw_1999_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_1999.xlsx", sheet="summary"))
nrw_1999_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/nrw/nrw_1999_sitze.xlsx", sheet="summary"))

# Merge Stimmen and Sitze
nrw_1999_kommunalwahlen_data <- merge(nrw_1999_kommunalwahlen_data, nrw_1999_kommunalwahlen_data_sitze, by.x="Bezirksnummer", by.y="Bezirksnummer")

names(nrw_1999_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_1999_kommunalwahlen_data) <-  str_replace_all(names(nrw_1999_kommunalwahlen_data), fixed(" "), "")

# Delete white space in Bezirksnummer ----
nrw_1999_kommunalwahlen_data$Bezirksnummer <-  str_replace_all(nrw_1999_kommunalwahlen_data$Bezirksnummer, fixed(" "), "")

# Create new dataframe ----
nrw_1999_kommunalwahlen_data_sub <- nrw_1999_kommunalwahlen_data

names(nrw_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_1999_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_1999_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_1999_kommunalwahlen_data_sub[ , Gebietsname := ""]
nrw_1999_kommunalwahlen_data_sub[ , election_year := "1999"]
nrw_1999_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_1999_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_1999_kommunalwahlen_data_sub[ , IDBA := ""]

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
nrw_1999_kommunalwahlen_data_sub <- nrw_1999_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                           Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                           abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                           gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                           sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_1999_kommunalwahlen_data_sub <-
  nrw_1999_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_1999_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_1999_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 2004 Kommunalwahlen ----
#### Load election data ----
nrw_2004_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_2004.xlsx", sheet="summary"))
nrw_2004_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/nrw/nrw_2004_sitze.xlsx", sheet="summary"))

# Merge Stimmen and Sitze
nrw_2004_kommunalwahlen_data <- merge(nrw_2004_kommunalwahlen_data, nrw_2004_kommunalwahlen_data_sitze, by.x="Bezirksnummer", by.y="Bezirksnummer")

names(nrw_2004_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_2004_kommunalwahlen_data) <-  str_replace_all(names(nrw_2004_kommunalwahlen_data), fixed(" "), "")

# Delete white space in Bezirksnummer ----
nrw_2004_kommunalwahlen_data$Bezirksnummer <-  str_replace_all(nrw_2004_kommunalwahlen_data$Bezirksnummer, fixed(" "), "")

# Create new dataframe ----
nrw_2004_kommunalwahlen_data_sub <- nrw_2004_kommunalwahlen_data

names(nrw_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2004_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_2004_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_2004_kommunalwahlen_data_sub[ , Gebietsname := ""]
nrw_2004_kommunalwahlen_data_sub[ , election_year := "2004"]
nrw_2004_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_2004_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_2004_kommunalwahlen_data_sub[ , IDBA := ""]

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
nrw_2004_kommunalwahlen_data_sub <- nrw_2004_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2004_kommunalwahlen_data_sub <-
  nrw_2004_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_2004_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_2004_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 2009 Kommunalwahlen ----
#### Load election data ----
nrw_2009_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_2009.xlsx", sheet="summary"))
nrw_2009_kommunalwahlen_data$Bezirksnummer <- as.numeric(nrw_2009_kommunalwahlen_data$Bezirksnummer)
nrw_2009_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/nrw/nrw_2009_sitze.xlsx", sheet="summary"))

# Merge Stimmen and Sitze
nrw_2009_kommunalwahlen_data <- merge(nrw_2009_kommunalwahlen_data, nrw_2009_kommunalwahlen_data_sitze, by.x="Bezirksnummer", by.y="Bezirksnummer")

names(nrw_2009_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_2009_kommunalwahlen_data) <-  str_replace_all(names(nrw_2009_kommunalwahlen_data), fixed(" "), "")

# Delete white space in Bezirksnummer ----
nrw_2009_kommunalwahlen_data$Bezirksnummer <-  str_replace_all(nrw_2009_kommunalwahlen_data$Bezirksnummer, fixed(" "), "")

# Create new dataframe ----
nrw_2009_kommunalwahlen_data_sub <- nrw_2009_kommunalwahlen_data

names(nrw_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2009_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_2009_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_2009_kommunalwahlen_data_sub[ , Gebietsname := ""]
nrw_2009_kommunalwahlen_data_sub[ , election_year := "2009"]
nrw_2009_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_2009_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_2009_kommunalwahlen_data_sub[ , IDBA := ""]

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
nrw_2009_kommunalwahlen_data_sub <- nrw_2009_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2009_kommunalwahlen_data_sub <-
  nrw_2009_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_2009_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_2009_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

###### NRW 2014 Kommunalwahlen ----
#### Load election data ----
nrw_2014_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_2014.xlsx", sheet="summary"))
nrw_2014_kommunalwahlen_data$Bezirksnummer <- as.numeric(nrw_2014_kommunalwahlen_data$Bezirksnummer)
nrw_2014_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/nrw/nrw_2014_sitze.xlsx", sheet="summary"))

# Merge Stimmen and Sitze
nrw_2014_kommunalwahlen_data <- merge(nrw_2014_kommunalwahlen_data, nrw_2014_kommunalwahlen_data_sitze, by.x="Bezirksnummer", by.y="Bezirksnummer")

names(nrw_2014_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(nrw_2014_kommunalwahlen_data) <-  str_replace_all(names(nrw_2014_kommunalwahlen_data), fixed(" "), "")

# Delete white space in Bezirksnummer ----
nrw_2014_kommunalwahlen_data$Bezirksnummer <-  str_replace_all(nrw_2014_kommunalwahlen_data$Bezirksnummer, fixed(" "), "")

# Create new dataframe ----
nrw_2014_kommunalwahlen_data_sub <- nrw_2014_kommunalwahlen_data

names(nrw_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2014_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_2014_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_2014_kommunalwahlen_data_sub[ , Gebietsname := ""]
nrw_2014_kommunalwahlen_data_sub[ , election_year := "2014"]
nrw_2014_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_2014_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_2014_kommunalwahlen_data_sub[ , IDBA := ""]

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
nrw_2014_kommunalwahlen_data_sub <- nrw_2014_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2014_kommunalwahlen_data_sub <-
  nrw_2014_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_2014_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_2014_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)


###### NRW 2020 Kommunalwahlen ----
#### Load election data ----
nrw_2020_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_2020.xlsx", sheet="summary"))
nrw_2020_kommunalwahlen_data$Bezirksnummer <- as.numeric(nrw_2020_kommunalwahlen_data$Bezirksnummer)
nrw_2020_kommunalwahlen_data_sitze <- as.data.table(read_excel("raw/nrw/nrw_2020_sitze.xlsx", sheet="summary"))

# Merge Stimmen and Sitze
nrw_2020_kommunalwahlen_data <- merge(nrw_2020_kommunalwahlen_data, nrw_2020_kommunalwahlen_data_sitze, by.x="Bezirksnummer", by.y="Bezirksnummer")


#### Recoding ----
# Delete white space ----
names(nrw_2020_kommunalwahlen_data) <-  str_replace_all(names(nrw_2020_kommunalwahlen_data), fixed(" "), "")
# Delete white space in Bezirksnummer ----
nrw_2020_kommunalwahlen_data$Bezirksnummer <-  str_replace_all(nrw_2020_kommunalwahlen_data$Bezirksnummer, fixed(" "), "")

# Create new dataframe ----
nrw_2020_kommunalwahlen_data_sub <- nrw_2020_kommunalwahlen_data

names(nrw_2020_kommunalwahlen_data_sub)

# Creating non-existing variables ----
nrw_2020_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_2020_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_2020_kommunalwahlen_data_sub[ , Gebietsname := ""]
nrw_2020_kommunalwahlen_data_sub[ , election_year := "2020"]
nrw_2020_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_2020_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_2020_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
nrw_2020_kommunalwahlen_data_sub[nrw_2020_kommunalwahlen_data_sub == "-"] <- NA

nrw_2020_kommunalwahlen_data_sub$AGS_8dig <- nrw_2020_kommunalwahlen_data_sub$Bezirksnummer
nrw_2020_kommunalwahlen_data_sub$Gebietsname <- nrw_2020_kommunalwahlen_data_sub$`Bezirksname.x`
nrw_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- nrw_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt
nrw_2020_kommunalwahlen_data_sub$Wähler <- nrw_2020_kommunalwahlen_data_sub$"Wähler/-inneninsgesamt"
nrw_2020_kommunalwahlen_data_sub$GültigeStimmen <- nrw_2020_kommunalwahlen_data_sub$GültigeStimmen

nrw_2020_kommunalwahlen_data_sub$abs_CDU <- as.numeric(nrw_2020_kommunalwahlen_data_sub$CDU)
nrw_2020_kommunalwahlen_data_sub$abs_SPD <- as.numeric(nrw_2020_kommunalwahlen_data_sub$SPD)
nrw_2020_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(nrw_2020_kommunalwahlen_data_sub$DIELINKE)
nrw_2020_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(nrw_2020_kommunalwahlen_data_sub$GRÜNE)
nrw_2020_kommunalwahlen_data_sub$abs_AfD <- as.numeric(nrw_2020_kommunalwahlen_data_sub$AfD)
nrw_2020_kommunalwahlen_data_sub$abs_PIRATEN <- as.numeric(nrw_2020_kommunalwahlen_data_sub$PIRATEN)
nrw_2020_kommunalwahlen_data_sub$abs_FDP <- as.numeric(nrw_2020_kommunalwahlen_data_sub$FDP)
nrw_2020_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

nrw_2020_kommunalwahlen_data_sub$gew_CDU <- NA
nrw_2020_kommunalwahlen_data_sub$gew_SPD <- NA
nrw_2020_kommunalwahlen_data_sub$gew_DIELINKE <- NA
nrw_2020_kommunalwahlen_data_sub$gew_GRÜNE <- NA
nrw_2020_kommunalwahlen_data_sub$gew_AfD <- NA
nrw_2020_kommunalwahlen_data_sub$gew_PIRATEN <- NA
nrw_2020_kommunalwahlen_data_sub$gew_FDP <- NA
nrw_2020_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

nrw_2020_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(nrw_2020_kommunalwahlen_data_sub$CDU_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(nrw_2020_kommunalwahlen_data_sub$SPD_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(nrw_2020_kommunalwahlen_data_sub$DIELINKE_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(nrw_2020_kommunalwahlen_data_sub$Gruene_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_AfD <- as.numeric(nrw_2020_kommunalwahlen_data_sub$AfD_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_PIRATEN <- as.numeric(nrw_2020_kommunalwahlen_data_sub$PIRATEN_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(nrw_2020_kommunalwahlen_data_sub$FDP_Zusammen)
nrw_2020_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA


# Creating new dataframe with selected vars ----
nrw_2020_kommunalwahlen_data_sub <- nrw_2020_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_2020_kommunalwahlen_data_sub <-
  nrw_2020_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_2020_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_2020_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_2020_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

# Fix AGS ----
nrw_2020_kommunalwahlen_data_sub$AGS_8dig <- paste("5", nrw_2020_kommunalwahlen_data_sub$AGS_8dig, sep="")

###### NRW Kommunalwahlen for kreisfreie Städte ----
#### Load election data ----
nrw_kreisfreie_kommunalwahlen_data <- as.data.table(read_excel("raw/nrw/nrw_kreisfreie_staedte.xlsx", sheet="summary"))


#### Recoding ----
# Delete white space ----
names(nrw_kreisfreie_kommunalwahlen_data) <-  str_replace_all(names(nrw_kreisfreie_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data

names(nrw_kreisfreie_kommunalwahlen_data_sub)

# Filter Kreise and Regierungsbezirke ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  filter(
    nchar(ags) > 3, 
    !grepl("Kreis", gebiet))

# Fix AGS ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  mutate(
    ags = paste0(ags, "000"))

# Creating non-existing variables ----
nrw_kreisfreie_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
nrw_kreisfreie_kommunalwahlen_data_sub[ , Bundesland := "NRW"]
nrw_kreisfreie_kommunalwahlen_data_sub[ , Gebietsname := ""]

nrw_kreisfreie_kommunalwahlen_data_sub[ , election_type := "Kommunalwahl"]
nrw_kreisfreie_kommunalwahlen_data_sub[ , IDIRB := ""]
nrw_kreisfreie_kommunalwahlen_data_sub[ , IDBA := ""]

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
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

nrw_kreisfreie_kommunalwahlen_data_sub <-
  nrw_kreisfreie_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
nrw_kreisfreie_kommunalwahlen_data_sub$Turnout <- as.numeric(nrw_kreisfreie_kommunalwahlen_data_sub$Wähler) / as.numeric(nrw_kreisfreie_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

# Filter Hochsauerlandkreis ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  filter(!AGS_8dig == "05958000")

# Filter 1989 ----
nrw_kreisfreie_kommunalwahlen_data_sub <- nrw_kreisfreie_kommunalwahlen_data_sub %>%
  filter(!election_year == "1989")

####### Merge files and save overall output for NRW ----
# Merge
nrw_kommunalwahlen <- rbind(nrw_1994_kommunalwahlen_data_sub,
                            nrw_1999_kommunalwahlen_data_sub,
                            nrw_2004_kommunalwahlen_data_sub,
                            nrw_2009_kommunalwahlen_data_sub,
                            nrw_2014_kommunalwahlen_data_sub,
                            nrw_2020_kommunalwahlen_data_sub,
                            nrw_kreisfreie_kommunalwahlen_data_sub) %>%
  filter(!grepl("Städteregion", Gebietsname)) %>%
  arrange(election_year, AGS_8dig)


# Replace INF at Turnout
nrw_kommunalwahlen$Turnout <-  str_replace_all(nrw_kommunalwahlen$Turnout, fixed("Inf"), NA)

# Replace - with NA
nrw_kommunalwahlen[nrw_kommunalwahlen == "-"] <- NA

# Fix AGS
nrw_kommunalwahlen$AGS_8dig <- stri_pad_left(nrw_kommunalwahlen$AGS_8dig, 8, "0")

# Reorder
nrw_kommunalwahlen <- nrw_kommunalwahlen %>%
  arrange(election_year, AGS_8dig)

# Save
#write_csv(nrw_kommunalwahlen, here::here("output/nrw_kommunalwahlen.csv"))

######### SAARLAND ----
###### Saarland  Kommunalwahlen ----
#### Load election data ----
saarland_kommunalwahlen_data <- as.data.table(read_excel("raw/saarland/saarland_grw.xlsx", sheet="summary"))

names(saarland_kommunalwahlen_data)


#### Recoding ----
# Delete white space ----
names(saarland_kommunalwahlen_data) <-  str_replace_all(names(saarland_kommunalwahlen_data), fixed(" "), "")

# Create new dataframe ----
saarland_kommunalwahlen_data_sub <- saarland_kommunalwahlen_data

names(saarland_kommunalwahlen_data_sub)

# Creating non-existing variables ----
saarland_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
saarland_kommunalwahlen_data_sub[ , Bundesland := "Saarland"]
saarland_kommunalwahlen_data_sub[ , Gebietsname := ""]
saarland_kommunalwahlen_data_sub[ , election_year := ""]
saarland_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahl"]
saarland_kommunalwahlen_data_sub[ , IDIRB := ""]
saarland_kommunalwahlen_data_sub[ , IDBA := ""]

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
saarland_kommunalwahlen_data_sub <- saarland_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER,
                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER,
                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

saarland_kommunalwahlen_data_sub <-
  saarland_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
saarland_kommunalwahlen_data_sub$Turnout <- as.numeric(saarland_kommunalwahlen_data_sub$Wähler) / as.numeric(saarland_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)

saarland_kommunalwahlen_data_sub <- saarland_kommunalwahlen_data_sub[
  with(saarland_kommunalwahlen_data_sub, order(election_year, AGS_8dig)),]



####### Merge files and save overall output for Saarland ----

# Save
#write_csv(saarland_kommunalwahlen_data_sub, here::here("output/saarland_kommunalwahlen.csv"))


######### SACHSEN-ANHALT ----
###### Sachsen-Anhalt 1994 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_1994_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_1994.csv"))

#### Delete white space ----
names(sachsen_anhalt_1994_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_1994_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_1994_kommunalwahlen_data_sub <- sachsen_anhalt_1994_kommunalwahlen_data

names(sachsen_anhalt_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , election_year := "1994"]
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_1994_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_1994_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_1994_kommunalwahlen_data_sub$AGS
sachsen_anhalt_1994_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_1994_kommunalwahlen_data_sub$NAME
sachsen_anhalt_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_1994_kommunalwahlen_data_sub$A
sachsen_anhalt_1994_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_1994_kommunalwahlen_data_sub$B
sachsen_anhalt_1994_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_1994_kommunalwahlen_data_sub$`F`

sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_1994_kommunalwahlen_data_sub$F01
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_1994_kommunalwahlen_data_sub$F02
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_1994_kommunalwahlen_data_sub$F04
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_1994_kommunalwahlen_data_sub$F05
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_1994_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_1994_kommunalwahlen_data_sub$F03
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
sachsen_anhalt_1994_kommunalwahlen_data_sub <- sachsen_anhalt_1994_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_1994_kommunalwahlen_data_sub <-
  sachsen_anhalt_1994_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_1994_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_1994_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Filter Kreise out ----
sachsen_anhalt_1994_kommunalwahlen_data_sub <- sachsen_anhalt_1994_kommunalwahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen-Anhalt 1999 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_1999_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_1999.csv"))

#### Delete white space ----
names(sachsen_anhalt_1999_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_1999_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_1999_kommunalwahlen_data_sub <- sachsen_anhalt_1999_kommunalwahlen_data

names(sachsen_anhalt_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , election_year := "1999"]
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_1999_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_1999_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_1999_kommunalwahlen_data_sub$AGS
sachsen_anhalt_1999_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_1999_kommunalwahlen_data_sub$NAME
sachsen_anhalt_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_1999_kommunalwahlen_data_sub$A
sachsen_anhalt_1999_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_1999_kommunalwahlen_data_sub$B
sachsen_anhalt_1999_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_1999_kommunalwahlen_data_sub$`F`

sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_1999_kommunalwahlen_data_sub$F02
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_1999_kommunalwahlen_data_sub$F01
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_1999_kommunalwahlen_data_sub$F03
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_1999_kommunalwahlen_data_sub$F06
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_1999_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_1999_kommunalwahlen_data_sub$F05
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
sachsen_anhalt_1999_kommunalwahlen_data_sub <- sachsen_anhalt_1999_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_1999_kommunalwahlen_data_sub <-
  sachsen_anhalt_1999_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_1999_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_1999_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Filter Kreise out ----
sachsen_anhalt_1999_kommunalwahlen_data_sub <- sachsen_anhalt_1999_kommunalwahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

sachsen_anhalt_1999_kommunalwahlen_data_sub

###### Sachsen-Anhalt 2004 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_2004_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_2004.csv"))

#### Delete white space ----
names(sachsen_anhalt_2004_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_2004_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2004_kommunalwahlen_data_sub <- sachsen_anhalt_2004_kommunalwahlen_data

names(sachsen_anhalt_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , election_year := "2004"]
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_2004_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2004_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2004_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2004_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2004_kommunalwahlen_data_sub$NAME
sachsen_anhalt_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2004_kommunalwahlen_data_sub$A
sachsen_anhalt_2004_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2004_kommunalwahlen_data_sub$B
sachsen_anhalt_2004_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2004_kommunalwahlen_data_sub$D

sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2004_kommunalwahlen_data_sub$D01
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2004_kommunalwahlen_data_sub$D03
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2004_kommunalwahlen_data_sub$D02
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2004_kommunalwahlen_data_sub$D05
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2004_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2004_kommunalwahlen_data_sub$D04
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
sachsen_anhalt_2004_kommunalwahlen_data_sub <- sachsen_anhalt_2004_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2004_kommunalwahlen_data_sub <-
  sachsen_anhalt_2004_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2004_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2004_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Filter Kreise out ----
sachsen_anhalt_2004_kommunalwahlen_data_sub <- sachsen_anhalt_2004_kommunalwahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen-Anhalt 2007 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_2007_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_2007.csv"))

#### Delete white space ----
names(sachsen_anhalt_2007_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_2007_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2007_kommunalwahlen_data_sub <- sachsen_anhalt_2007_kommunalwahlen_data

names(sachsen_anhalt_2007_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , election_year := "2007"]
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_2007_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2007_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2007_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2007_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2007_kommunalwahlen_data_sub$NAME
sachsen_anhalt_2007_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2007_kommunalwahlen_data_sub$A
sachsen_anhalt_2007_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2007_kommunalwahlen_data_sub$B
sachsen_anhalt_2007_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2007_kommunalwahlen_data_sub$D

sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2007_kommunalwahlen_data_sub$D01
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2007_kommunalwahlen_data_sub$D03
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2007_kommunalwahlen_data_sub$D02
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2007_kommunalwahlen_data_sub$D06
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2007_kommunalwahlen_data_sub$D04
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_DiePARTEI <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- NA

sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_CDU <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_SPD <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_DIELINKE <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_GRÜNE <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_AfD <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_PIRATEN <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_FDP <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_DiePARTEI <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_CDU <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_SPD <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_DIELINKE <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_GRÜNE <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_AfD <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_FDP <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_anhalt_2007_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_anhalt_2007_kommunalwahlen_data_sub <- sachsen_anhalt_2007_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2007_kommunalwahlen_data_sub <-
  sachsen_anhalt_2007_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2007_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2007_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_2007_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

table(sachsen_anhalt_2007_kommunalwahlen_data_sub$Gebietsname)

###### Sachsen-Anhalt 2009 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_2009_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_2009.csv"))

#### Delete white space ----
names(sachsen_anhalt_2009_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_2009_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2009_kommunalwahlen_data_sub <- sachsen_anhalt_2009_kommunalwahlen_data

names(sachsen_anhalt_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , election_year := "2009"]
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_2009_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2009_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2009_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2009_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2009_kommunalwahlen_data_sub$NAME
sachsen_anhalt_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2009_kommunalwahlen_data_sub$A
sachsen_anhalt_2009_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2009_kommunalwahlen_data_sub$B
sachsen_anhalt_2009_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2009_kommunalwahlen_data_sub$D

sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2009_kommunalwahlen_data_sub$D01
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2009_kommunalwahlen_data_sub$D03
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2009_kommunalwahlen_data_sub$D02
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2009_kommunalwahlen_data_sub$D05
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_AfD <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2009_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2009_kommunalwahlen_data_sub$D04
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
sachsen_anhalt_2009_kommunalwahlen_data_sub <- sachsen_anhalt_2009_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2009_kommunalwahlen_data_sub <-
  sachsen_anhalt_2009_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2009_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2009_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Remove Ortsteile ----

sachsen_anhalt_2009_kommunalwahlen_data_sub <- sachsen_anhalt_2009_kommunalwahlen_data_sub %>%
  filter(
    !AGS_8dig %in% c("15081010",
                     "15081015",
                     "15081025",
                     "15081055",
                     "15081115",
                     "15081140",
                     "15081155",
                     "15081275",
                     "15081420",
                     "15081465",
                     "15081495",
                     "15081530",
                     "15081580",
                     "15090030",
                     "15090035",
                     "15090130",
                     '15090140',
                     '15090155',
                     '15090175',
                     '15090315',
                     '15090335',
                     '15090380',
                     '15090440',
                     '15090450',
                     '15090455',
                     '15090590'))
  

###### Sachsen-Anhalt 2014 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_2014_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_2014.csv"))

#### Delete white space ----
names(sachsen_anhalt_2014_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_2014_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2014_kommunalwahlen_data_sub <- sachsen_anhalt_2014_kommunalwahlen_data

names(sachsen_anhalt_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , election_year := "2014"]
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_2014_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2014_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2014_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2014_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2014_kommunalwahlen_data_sub$NAME
sachsen_anhalt_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2014_kommunalwahlen_data_sub$A
sachsen_anhalt_2014_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2014_kommunalwahlen_data_sub$B
sachsen_anhalt_2014_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D

sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D01
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D03
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D02
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D04
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_AfD <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D11
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_PIRATEN <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D15
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D05
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_DiePARTEI <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D14
sachsen_anhalt_2014_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- sachsen_anhalt_2014_kommunalwahlen_data_sub$D12

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
sachsen_anhalt_2014_kommunalwahlen_data_sub <- sachsen_anhalt_2014_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2014_kommunalwahlen_data_sub <-
  sachsen_anhalt_2014_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2014_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2014_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Sachsen-Anhalt 2019 Kommunalwahlen ----
#### Load election data ----

sachsen_anhalt_2019_kommunalwahlen_data <- as.data.table(read_csv2("raw/sachsen_anhalt/sachsen-anhalt_2019.csv"))

#### Delete white space ----
names(sachsen_anhalt_2019_kommunalwahlen_data) <-  str_replace_all(names(sachsen_anhalt_2019_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_anhalt_2019_kommunalwahlen_data_sub <- sachsen_anhalt_2019_kommunalwahlen_data

names(sachsen_anhalt_2019_kommunalwahlen_data_sub)

# Creating non-existing variables ----
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , Bundesland := "Sachsen-Anhalt"]
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , Gebietsname := ""]
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , election_year := "2019"]
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , IDIRB := ""]
sachsen_anhalt_2019_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sachsen_anhalt_2019_kommunalwahlen_data_sub$AGS_8dig <- sachsen_anhalt_2019_kommunalwahlen_data_sub$AGS
sachsen_anhalt_2019_kommunalwahlen_data_sub$Gebietsname <- sachsen_anhalt_2019_kommunalwahlen_data_sub$NAME
sachsen_anhalt_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- sachsen_anhalt_2019_kommunalwahlen_data_sub$A
sachsen_anhalt_2019_kommunalwahlen_data_sub$Wähler <- sachsen_anhalt_2019_kommunalwahlen_data_sub$B
sachsen_anhalt_2019_kommunalwahlen_data_sub$GültigeStimmen <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D

sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_CDU <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D01
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_SPD <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D04
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_DIELINKE <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D03
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_GRÜNE <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D05
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_AfD <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D02
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_PIRATEN <- NA
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_FDP <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D06
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_DiePARTEI <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D12
sachsen_anhalt_2019_kommunalwahlen_data_sub$abs_FREIEWÄHLER <- sachsen_anhalt_2019_kommunalwahlen_data_sub$D07

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
sachsen_anhalt_2019_kommunalwahlen_data_sub <- sachsen_anhalt_2019_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_anhalt_2019_kommunalwahlen_data_sub <-
  sachsen_anhalt_2019_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_anhalt_2019_kommunalwahlen_data_sub$Turnout <- sachsen_anhalt_2019_kommunalwahlen_data_sub$Wähler / sachsen_anhalt_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

####### Merge files and save overall output for Sachsen-Anhalt ----
# Merge
sachsen_anhalt_kommunalwahlen <- rbind(sachsen_anhalt_1994_kommunalwahlen_data_sub,
                                       sachsen_anhalt_1999_kommunalwahlen_data_sub,
                                       sachsen_anhalt_2004_kommunalwahlen_data_sub,
                                       sachsen_anhalt_2007_kommunalwahlen_data_sub,
                                       sachsen_anhalt_2009_kommunalwahlen_data_sub,
                                       sachsen_anhalt_2014_kommunalwahlen_data_sub,
                                       sachsen_anhalt_2019_kommunalwahlen_data_sub)

# Replace INF at Turnout
sachsen_anhalt_kommunalwahlen$Turnout <-  str_replace_all(sachsen_anhalt_kommunalwahlen$Turnout, fixed("Inf"), NA)

# Replace - with NA
sachsen_anhalt_kommunalwahlen[sachsen_anhalt_kommunalwahlen == "-"] <- NA

# Exclude Hochsauerlandkreis
sachsen_anhalt_kommunalwahlen <- sachsen_anhalt_kommunalwahlen %>%
  filter(!Gebietsname == "Hochsauerlandkreis")

# Save
#write_csv(sachsen_anhalt_kommunalwahlen, here::here("output/sachsen_anhalt_kommunalwahlen.csv"))

######### BADEN-WUERTTEMBERG ----
###### Baden-Wuerttemberg 1989 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_1989_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_1989.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_1989_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_1989_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1989_gemeinderatswahlen_data

names(baden_wuerttemberg_1989_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , election_year := "1989"]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ , IDBA := ""]

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
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_1989_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_1989_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_1989_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 1994 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_1994_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_1994.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_1994_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_1994_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1994_gemeinderatswahlen_data

names(baden_wuerttemberg_1994_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , election_year := "1994"]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ , IDBA := ""]

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
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_1994_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 1999 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_1999_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_1999.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_1999_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_1999_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1999_gemeinderatswahlen_data

names(baden_wuerttemberg_1999_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , election_year := "1999"]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ , IDBA := ""]

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
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_1999_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_1999_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2004 Gemeinderatswahlen ----
#### Load election data ----

baden_wuerttemberg_2004_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_2004.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_2004_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_2004_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2004_gemeinderatswahlen_data

names(baden_wuerttemberg_2004_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , election_year := "2004"]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ , IDBA := ""]

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
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2004_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2004_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2009 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2009_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_2009.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_2009_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_2009_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2009_gemeinderatswahlen_data

names(baden_wuerttemberg_2009_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , election_year := "2009"]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ , IDBA := ""]

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
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2009_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2009_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2014 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2014_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_2014.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_2014_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_2014_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2014_gemeinderatswahlen_data

names(baden_wuerttemberg_2014_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , election_year := "2014"]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ , IDBA := ""]

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
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2014_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2014_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### Baden-Wuerttemberg 2019 Gemeinderatswahlen ----
#### Load election data ----
baden_wuerttemberg_2019_gemeinderatswahlen_data <- as.data.table(read_excel("raw/baden_wuerttemberg/baden_wuerttemberg_2019.xlsx", sheet="summary"))

#### Delete white space ----
names(baden_wuerttemberg_2019_gemeinderatswahlen_data) <-  str_replace_all(names(baden_wuerttemberg_2019_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2019_gemeinderatswahlen_data

names(baden_wuerttemberg_2019_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , Bundesland := "Baden-Wuerttemberg"]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , election_year := "2019"]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , IDIRB := ""]
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$AGS_8dig <- parse_number(baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Gemeinde)
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Gebietsname <- gsub('[0-9]+', '', baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Gemeinde)
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
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

baden_wuerttemberg_2019_gemeinderatswahlen_data_sub <-
  baden_wuerttemberg_2019_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Turnout <- baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wähler / baden_wuerttemberg_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

####### Merge files and save overall output for Baden-Wuerttemberg ----
# Merge
baden_wuerttemberg_kommunalwahlen <- rbind(baden_wuerttemberg_1989_gemeinderatswahlen_data_sub,baden_wuerttemberg_1994_gemeinderatswahlen_data_sub, baden_wuerttemberg_1999_gemeinderatswahlen_data_sub,
                                           baden_wuerttemberg_2004_gemeinderatswahlen_data_sub, baden_wuerttemberg_2009_gemeinderatswahlen_data_sub, baden_wuerttemberg_2014_gemeinderatswahlen_data_sub,
                                           baden_wuerttemberg_2019_gemeinderatswahlen_data_sub)

# Replace INF at Turnout
baden_wuerttemberg_kommunalwahlen$Turnout <-  str_replace_all(baden_wuerttemberg_kommunalwahlen$Turnout, fixed("Inf"), NA)

# Replace - with NA
baden_wuerttemberg_kommunalwahlen[baden_wuerttemberg_kommunalwahlen == "-"] <- NA

# Fix AGS
baden_wuerttemberg_kommunalwahlen$AGS_8dig <- paste("08",baden_wuerttemberg_kommunalwahlen$AGS_8dig, sep="")

# Save
#write_csv(baden_wuerttemberg_kommunalwahlen, here::here("output/baden_wuerttemberg_kommunalwahlen.csv"))


######## MECKLENBURG-VORPOMMERN
######### MECKLENBURG-VORPOMMERN ----

######## LANDTAGSWAHLEN ----
###### Mecklenburg-Vorpommern 1994 Landtagswahlen ----
#### Load election data ----
mecklenburg_vorpommern_1994_landtagswahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_landtagswahlen_1994.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_1994_landtagswahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_1994_landtagswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_1994_landtagswahlen_data_sub <- mecklenburg_vorpommern_1994_landtagswahlen_data

names(mecklenburg_vorpommern_1994_landtagswahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , election_year := "1994"]
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , election_type := "Landtagswahlen"]
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Wähler <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Stimmengültig

mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$CDU
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$SPD
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$PDS
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$GRÜNE
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$`F.D.P.`
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$abs_FREIEWÄHLER <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$FreieWähler

mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_1994_landtagswahlen_data_sub <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1994_landtagswahlen_data_sub <-
  mecklenburg_vorpommern_1994_landtagswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Turnout <- mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Wähler / mecklenburg_vorpommern_1994_landtagswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 1999 Landtagswahlen ----
#### Load election data ----
mecklenburg_vorpommern_1999_landtagswahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_landtagswahlen_1999.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_1999_landtagswahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_1999_landtagswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_1999_landtagswahlen_data_sub <- mecklenburg_vorpommern_1999_landtagswahlen_data

names(mecklenburg_vorpommern_1999_landtagswahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , election_year := "1999"]
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , election_type := "Landtagswahlen"]
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Wähler <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Stimmengueltig

mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$CDU
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$SPD
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$PDS
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$GRÜNE
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$`F.D.P.`
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$abs_FREIEWÄHLER <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$FreieWähler

mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_1999_landtagswahlen_data_sub <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1999_landtagswahlen_data_sub <-
  mecklenburg_vorpommern_1999_landtagswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Turnout <- mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Wähler / mecklenburg_vorpommern_1999_landtagswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2004 Landtagswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2004_landtagswahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_landtagswahlen_2004.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_2004_landtagswahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2004_landtagswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2004_landtagswahlen_data_sub <- mecklenburg_vorpommern_2004_landtagswahlen_data

names(mecklenburg_vorpommern_2004_landtagswahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , election_year := "2004"]
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , election_type := "Landtagswahlen"]
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Wähler <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$GueltigeStimmen

mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$CDU
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$SPD
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$PDS
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$GRÜNE
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$FDP
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2004_landtagswahlen_data_sub <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2004_landtagswahlen_data_sub <-
  mecklenburg_vorpommern_2004_landtagswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Wähler / mecklenburg_vorpommern_2004_landtagswahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2009 Landtagswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2009_landtagswahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_landtagswahlen_2009.xlsx", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_2009_landtagswahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2009_landtagswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2009_landtagswahlen_data_sub <- mecklenburg_vorpommern_2009_landtagswahlen_data

names(mecklenburg_vorpommern_2009_landtagswahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , election_year := "2009"]
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , election_type := "Landtagswahlen"]
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Wähler <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Stimmengueltig

mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$CDU
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$SPD
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$DIELINKE
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$GRÜNE
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$FDP
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2009_landtagswahlen_data_sub <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2009_landtagswahlen_data_sub <-
  mecklenburg_vorpommern_2009_landtagswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Wähler / mecklenburg_vorpommern_2009_landtagswahlen_data_sub$Wahlberechtigteinsgesamt




###### Mecklenburg-Vorpommern 2014 Landtagswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2014_landtagswahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_landtagswahlen_2014.xlsx", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_2014_landtagswahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2014_landtagswahlen_data), fixed(" "), "")

#### Recoding ----
# Summarize per Gemeinde ----
mecklenburg_vorpommern_2014_landtagswahlen_data_sub <- mecklenburg_vorpommern_2014_landtagswahlen_data %>%
  group_by(Gemeindenummer, Gemeindename) %>%
  summarize(
    Wahlberechtigteinsgesamt = sum(Wahlberechtigteinsgesamt, na.rm=T),
    Waehlerinsgesamt = sum(Waehlerinsgesamt, na.rm=T),
    gueltigeStimmen = sum(gueltigeStimmen, na.rm=T),
    CDU = sum(CDU, na.rm=T),
    SPD = sum(SPD, na.rm=T),
    DIELINKE = sum(DIELINKE, na.rm=T),
    GRÜNE = sum(GRÜNE, na.rm=T),
    NDP = sum(NDP, na.rm=T),
    FDP = sum(FDP, na.rm=T),
    AfD = sum(AfD, na.rm=T),
    PIRATEN = sum(PIRATEN, na.rm=T)) %>%
  ungroup()

names(mecklenburg_vorpommern_2014_landtagswahlen_data_sub)

mecklenburg_vorpommern_2014_landtagswahlen_data_sub <- as.data.table(mecklenburg_vorpommern_2014_landtagswahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , election_year := "2014"]
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , election_type := "Landtagswahlen"]
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Wahlberechtigteinsgesamt
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Wähler <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Waehlerinsgesamt
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gueltigeStimmen

mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$CDU
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$SPD
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$DIELINKE
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$GRÜNE
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_AfD <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$AfD
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_PIRATEN <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$PIRATEN
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$FDP
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_DiePARTEI <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$abs_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2014_landtagswahlen_data_sub <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2014_landtagswahlen_data_sub <-
  mecklenburg_vorpommern_2014_landtagswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Wähler / mecklenburg_vorpommern_2014_landtagswahlen_data_sub$Wahlberechtigteinsgesamt


###### Mecklenburg-Vorpommern 2019 Landtagswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2019_landtagswahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_landtagswahlen_2019.xlsx", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_2019_landtagswahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2019_landtagswahlen_data), fixed(" "), "")

#### Recoding ----
# Summarize per Gemeinde ----
mecklenburg_vorpommern_2019_landtagswahlen_data_sub <- mecklenburg_vorpommern_2019_landtagswahlen_data 

# Creating non-existing variables ----
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , election_year := "2019"]
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , election_type := "Landtagswahlen"]
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Wahlberechtigte
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Wähler <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Wähler
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$GültigeStimmen

mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_CDU <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$CDU
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_SPD <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$SPD
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_DIELINKE <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$DIELINKE
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_GRÜNE <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$GRÜNE
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_AfD <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$AfD
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_PIRATEN <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$PIRATEN
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_FDP <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$FDP
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_DiePARTEI <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$DiePARTEI
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$abs_FREIEWÄHLER <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$FREIEWÄHLER

mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_CDU <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_SPD <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_DIELINKE <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_GRÜNE <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_AfD <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_PIRATEN <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_FDP <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_DiePARTEI <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$gew_FREIEWÄHLER <- NA

mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_CDU <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_SPD <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_DIELINKE <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_GRÜNE <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_FDP <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2019_landtagswahlen_data_sub <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2019_landtagswahlen_data_sub <-
  mecklenburg_vorpommern_2019_landtagswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Turnout <- mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Wähler / mecklenburg_vorpommern_2019_landtagswahlen_data_sub$Wahlberechtigteinsgesamt


######## KOMMUNALWAHLEN ----
###### Mecklenburg-Vorpommern 1994 kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_1994_kommunalwahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_1994.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_1994_kommunalwahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_1994_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe and filter Landkreise ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1994_kommunalwahlen_data %>%
  filter(
    !grepl("Landkreis", Gemeindename))

names(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , election_year := "1994"]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , election_type := "Kommunalwahlen"]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wähler <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Waehlerinsgesamt)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$GültigeStimmen <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Stimmengültig)

mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_CDU <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$CDU)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_SPD <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$SPD)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$PDS)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$GRÜNE)
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$abs_FDP <- as.numeric(mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$`F.D.P.`)
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
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1994_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_1994_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wähler / mecklenburg_vorpommern_1994_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 1999 kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_1999_kommunalwahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_1999.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_1999_kommunalwahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_1999_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1999_kommunalwahlen_data

names(mecklenburg_vorpommern_1999_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , election_year := "1999"]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , election_type := "kommunalwahlen"]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ , IDBA := ""]

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
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_1999_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_1999_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wähler / mecklenburg_vorpommern_1999_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2004 kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2004_kommunalwahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2004.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_2004_kommunalwahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2004_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2004_kommunalwahlen_data

names(mecklenburg_vorpommern_2004_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , election_year := "2004"]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , election_type := "kommunalwahlen"]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ , IDBA := ""]

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
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2004_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2004_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wähler / mecklenburg_vorpommern_2004_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2009 Kommunalwahlen ----
#### Load election data ----
mecklenburg_vorpommern_2009_kommunalwahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2009.xls", sheet="summary"))

#### Delete white space ----
names(mecklenburg_vorpommern_2009_kommunalwahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2009_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2009_kommunalwahlen_data

names(mecklenburg_vorpommern_2009_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , election_year := "2009"]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , election_type := "kommunalwahlen"]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ , IDBA := ""]

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
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2009_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2009_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wähler / mecklenburg_vorpommern_2009_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt


###### Mecklenburg-Vorpommern 2014 Gemeinderatswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2014_kommunalwahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2014.xlsx", sheet="summary"))

mecklenburg_vorpommern_2014_kommunalwahlen_data[mecklenburg_vorpommern_2014_kommunalwahlen_data == "x"] <- NA

#### Delete white space ----
names(mecklenburg_vorpommern_2014_kommunalwahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2014_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2014_kommunalwahlen_data

names(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , election_year := "2014"]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wahlberechtigte
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wähler
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Stimmengueltig

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_CDU <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$CDU)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_SPD <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$SPD)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$DIELINKE)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$GRÜNE)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_AfD <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$abs_FDP <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$FDP)
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

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_CDU)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_SPD)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_DIELINKE)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_GRÜNE)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_AfD <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_FDP)
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2014_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2014_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wähler / mecklenburg_vorpommern_2014_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

###### Mecklenburg-Vorpommern 2019 Gemeinderatswahlen ----
#### Load election data ----
mecklenburg_vorpommern_2019_kommunalwahlen_data <- as.data.table(read_excel("raw/mecklenburg_vorpommern/mecklenburg_vorpommern_kommunalwahlen_2019.xlsx", sheet="summary"))

mecklenburg_vorpommern_2019_kommunalwahlen_data[mecklenburg_vorpommern_2019_kommunalwahlen_data == "x"] <- NA

#### Delete white space ----
names(mecklenburg_vorpommern_2019_kommunalwahlen_data) <-  str_replace_all(names(mecklenburg_vorpommern_2019_kommunalwahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data

names(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub)

# Creating non-existing variables ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , Bundesland := "Mecklenburg-Vorpommern"]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , Gebietsname := ""]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , election_year := "2019"]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , IDIRB := ""]
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$AGS_8dig <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gemeindenummer
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gebietsname <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Gemeindename
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigte
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wähler <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Waehler
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$GültigeStimmen <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$gültig

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_CDU <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$CDU)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_SPD <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$SPD)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_DIELINKE <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$DIELINKE)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_GRÜNE <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$GRÜNE)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_AfD <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$AfD)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$abs_FDP <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$FDP)
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

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_CDU <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_CDU)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_SPD <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_SPD)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DIELINKE <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DIELINKE)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_GRÜNE <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_GRÜNE)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_AfD <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_AfD)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_PIRATEN <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FDP <- as.numeric(mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FDP)
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_DiePARTEI <- NA
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]


# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <-
  mecklenburg_vorpommern_2019_kommunalwahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Turnout <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wähler / mecklenburg_vorpommern_2019_kommunalwahlen_data_sub$Wahlberechtigteinsgesamt

# Remove Mecklenburgische ----
mecklenburg_vorpommern_2019_kommunalwahlen_data_sub <- mecklenburg_vorpommern_2019_kommunalwahlen_data_sub %>%
  filter(!AGS_8dig == "13071000",
         !AGS_8dig == "13076000",
         !AGS_8dig == "13072000",
         !AGS_8dig == "13073000",
         !AGS_8dig == "13074000",
         !AGS_8dig == "13075000")


####### Merge files and save overall output for Mecklenburg-Vorpommern ----
# Merge
mecklenburg_vorpommern_kommunalwahlen <- rbind(mecklenburg_vorpommern_1994_landtagswahlen_data_sub,
                                               mecklenburg_vorpommern_1994_kommunalwahlen_data_sub,
                                               mecklenburg_vorpommern_1999_landtagswahlen_data_sub,
                                               mecklenburg_vorpommern_1999_kommunalwahlen_data_sub,
                                               mecklenburg_vorpommern_2004_landtagswahlen_data_sub,
                                               mecklenburg_vorpommern_2004_kommunalwahlen_data_sub,
                                               mecklenburg_vorpommern_2009_landtagswahlen_data_sub,
                                               mecklenburg_vorpommern_2009_kommunalwahlen_data_sub,
                                               mecklenburg_vorpommern_2014_landtagswahlen_data_sub,
                                               mecklenburg_vorpommern_2014_kommunalwahlen_data_sub,
                                               mecklenburg_vorpommern_2019_landtagswahlen_data_sub,
                                               mecklenburg_vorpommern_2019_kommunalwahlen_data_sub)

# Replace INF at Turnout
mecklenburg_vorpommern_kommunalwahlen$Turnout <-  str_replace_all(mecklenburg_vorpommern_kommunalwahlen$Turnout, fixed("Inf"), NA)

# Replace - with NA
mecklenburg_vorpommern_kommunalwahlen[mecklenburg_vorpommern_kommunalwahlen == "-"] <- NA

# Fix AGS
mecklenburg_vorpommern_kommunalwahlen$AGS_8dig <- stri_pad_left(mecklenburg_vorpommern_kommunalwahlen$AGS_8dig, 8, 0)

# Save
#write_csv(mecklenburg_vorpommern_kommunalwahlen, here::here("output/mecklenburg_vorpommern_kommunalwahlen.csv"))

# ----
######### HESSEN ----
###### Hessen 1989 Gemeinderatswahl ----
#### Load election data ----

hessen_1989_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="1989"))

#### Delete white space ----
names(hessen_1989_gemeinderatswahl_data) <-  str_replace_all(names(hessen_1989_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_1989_gemeinderatswahl_data_sub <- hessen_1989_gemeinderatswahl_data

names(hessen_1989_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_1989_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_1989_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_1989_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_1989_gemeinderatswahl_data_sub[ , election_year := "1989"]
hessen_1989_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_1989_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_1989_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_1989_gemeinderatswahl_data_sub <- hessen_1989_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_1989_gemeinderatswahl_data_sub <-
  hessen_1989_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_1989_gemeinderatswahl_data_sub$Turnout <- hessen_1989_gemeinderatswahl_data_sub$Wähler / hessen_1989_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 1993 Gemeinderatswahl ----
#### Load election data ----

hessen_1993_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="1993"))

#### Delete white space ----
names(hessen_1993_gemeinderatswahl_data) <-  str_replace_all(names(hessen_1993_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_1993_gemeinderatswahl_data_sub <- hessen_1993_gemeinderatswahl_data

names(hessen_1993_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_1993_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_1993_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_1993_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_1993_gemeinderatswahl_data_sub[ , election_year := "1993"]
hessen_1993_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_1993_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_1993_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_1993_gemeinderatswahl_data_sub <- hessen_1993_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_1993_gemeinderatswahl_data_sub <-
  hessen_1993_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_1993_gemeinderatswahl_data_sub$Turnout <- hessen_1993_gemeinderatswahl_data_sub$Wähler / hessen_1993_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 1997 Gemeinderatswahl ----
#### Load election data ----

hessen_1997_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="1997"))

#### Delete white space ----
names(hessen_1997_gemeinderatswahl_data) <-  str_replace_all(names(hessen_1997_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_1997_gemeinderatswahl_data_sub <- hessen_1997_gemeinderatswahl_data

names(hessen_1997_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_1997_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_1997_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_1997_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_1997_gemeinderatswahl_data_sub[ , election_year := "1997"]
hessen_1997_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_1997_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_1997_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_1997_gemeinderatswahl_data_sub <- hessen_1997_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_1997_gemeinderatswahl_data_sub <-
  hessen_1997_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_1997_gemeinderatswahl_data_sub$Turnout <- hessen_1997_gemeinderatswahl_data_sub$Wähler / hessen_1997_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2001 Gemeinderatswahl ----
#### Load election data ----

hessen_2001_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="2001"))

#### Delete white space ----
names(hessen_2001_gemeinderatswahl_data) <-  str_replace_all(names(hessen_2001_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_2001_gemeinderatswahl_data_sub <- hessen_2001_gemeinderatswahl_data

names(hessen_2001_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2001_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_2001_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_2001_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_2001_gemeinderatswahl_data_sub[ , election_year := "2001"]
hessen_2001_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_2001_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_2001_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_2001_gemeinderatswahl_data_sub <- hessen_2001_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2001_gemeinderatswahl_data_sub <-
  hessen_2001_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_2001_gemeinderatswahl_data_sub$Turnout <- hessen_2001_gemeinderatswahl_data_sub$Wähler / hessen_2001_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2006 Gemeinderatswahl ----
#### Load election data ----

hessen_2006_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="2006"))

#### Delete white space ----
names(hessen_2006_gemeinderatswahl_data) <-  str_replace_all(names(hessen_2006_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data

names(hessen_2006_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2006_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_2006_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_2006_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_2006_gemeinderatswahl_data_sub[ , election_year := "2006"]
hessen_2006_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_2006_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_2006_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2006_gemeinderatswahl_data_sub <-
  hessen_2006_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_2006_gemeinderatswahl_data_sub$Turnout <- hessen_2006_gemeinderatswahl_data_sub$Wähler / hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2006 Gemeinderatswahl ----
#### Load election data ----

hessen_2006_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="2006"))

#### Delete white space ----
names(hessen_2006_gemeinderatswahl_data) <-  str_replace_all(names(hessen_2006_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data

names(hessen_2006_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2006_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_2006_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_2006_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_2006_gemeinderatswahl_data_sub[ , election_year := "2006"]
hessen_2006_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_2006_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_2006_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_2006_gemeinderatswahl_data_sub <- hessen_2006_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2006_gemeinderatswahl_data_sub <-
  hessen_2006_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_2006_gemeinderatswahl_data_sub$Turnout <- hessen_2006_gemeinderatswahl_data_sub$Wähler / hessen_2006_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2011 Gemeinderatswahl ----
#### Load election data ----

hessen_2011_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="2011"))

#### Delete white space ----
names(hessen_2011_gemeinderatswahl_data) <-  str_replace_all(names(hessen_2011_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_2011_gemeinderatswahl_data_sub <- hessen_2011_gemeinderatswahl_data

names(hessen_2011_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2011_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_2011_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_2011_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_2011_gemeinderatswahl_data_sub[ , election_year := "2011"]
hessen_2011_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_2011_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_2011_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_2011_gemeinderatswahl_data_sub <- hessen_2011_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2011_gemeinderatswahl_data_sub <-
  hessen_2011_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_2011_gemeinderatswahl_data_sub$Turnout <- hessen_2011_gemeinderatswahl_data_sub$Wähler / hessen_2011_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2016 Gemeinderatswahl ----
#### Load election data ----

hessen_2016_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="2016"))

#### Delete white space ----
names(hessen_2016_gemeinderatswahl_data) <-  str_replace_all(names(hessen_2016_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_2016_gemeinderatswahl_data_sub <- hessen_2016_gemeinderatswahl_data

names(hessen_2016_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2016_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_2016_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_2016_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_2016_gemeinderatswahl_data_sub[ , election_year := "2016"]
hessen_2016_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_2016_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_2016_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_2016_gemeinderatswahl_data_sub <- hessen_2016_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2016_gemeinderatswahl_data_sub <-
  hessen_2016_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_2016_gemeinderatswahl_data_sub$Turnout <- hessen_2016_gemeinderatswahl_data_sub$Wähler / hessen_2016_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


###### Hessen 2021 Gemeinderatswahl ----
#### Load election data ----

hessen_2021_gemeinderatswahl_data <- as.data.table(read_excel("raw/hessen/overview_gemeinderatswahlen.xlsx", sheet="2021"))

#### Delete white space ----
names(hessen_2021_gemeinderatswahl_data) <-  str_replace_all(names(hessen_2021_gemeinderatswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
hessen_2021_gemeinderatswahl_data_sub <- hessen_2021_gemeinderatswahl_data

names(hessen_2021_gemeinderatswahl_data_sub)

# Creating non-existing variables ----
hessen_2021_gemeinderatswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
hessen_2021_gemeinderatswahl_data_sub[ , Bundesland := "Hessen"]
hessen_2021_gemeinderatswahl_data_sub[ , Gebietsname := ""]
hessen_2021_gemeinderatswahl_data_sub[ , election_year := "2021"]
hessen_2021_gemeinderatswahl_data_sub[ , election_type := "Gemeinderatswahl"]
hessen_2021_gemeinderatswahl_data_sub[ , IDIRB := ""]
hessen_2021_gemeinderatswahl_data_sub[ , IDBA := ""]

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
hessen_2021_gemeinderatswahl_data_sub <- hessen_2021_gemeinderatswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                   Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                   abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                   gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                   sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

hessen_2021_gemeinderatswahl_data_sub <-
  hessen_2021_gemeinderatswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
hessen_2021_gemeinderatswahl_data_sub$Turnout <- hessen_2021_gemeinderatswahl_data_sub$Wähler / hessen_2021_gemeinderatswahl_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Hessen ----
# Merge
hessen_kommunalwahlen <- rbind(hessen_1989_gemeinderatswahl_data_sub, hessen_1993_gemeinderatswahl_data_sub,
                               hessen_1997_gemeinderatswahl_data_sub, hessen_2001_gemeinderatswahl_data_sub,
                               hessen_2006_gemeinderatswahl_data_sub, hessen_2011_gemeinderatswahl_data_sub,
                               hessen_2016_gemeinderatswahl_data_sub, hessen_2021_gemeinderatswahl_data_sub)

# Replace - with NA
hessen_kommunalwahlen[hessen_kommunalwahlen == "-"] <- NA

# Fix AGS
hessen_kommunalwahlen$AGS_8dig <- paste("06",hessen_kommunalwahlen$AGS_8dig, sep="")

# Save
#write_csv(hessen_kommunalwahlen, here::here("output/hessen_kommunalwahlen.csv"))

# ----
######### NIEDERSACHSEN ----
###### Niedersachsen 1991 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_1991_gemeinderatswahlen_data <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_1991.xls", sheet="anzahl"))
niedersachsen_1991_gemeinderatswahlen_data_sitze <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_1991.xls", sheet="sitze"))

#### Delete white space ----
names(niedersachsen_1991_gemeinderatswahlen_data) <-  str_replace_all(names(niedersachsen_1991_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
niedersachsen_1991_gemeinderatswahlen_data_sub <- niedersachsen_1991_gemeinderatswahlen_data

names(niedersachsen_1991_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_1991_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_1991_gemeinderatswahlen_data_sub[ , Bundesland := "Niedersachsen"]
niedersachsen_1991_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
niedersachsen_1991_gemeinderatswahlen_data_sub[ , election_year := "1991"]
niedersachsen_1991_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
niedersachsen_1991_gemeinderatswahlen_data_sub[ , IDIRB := ""]
niedersachsen_1991_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
niedersachsen_1991_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_1991_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_1991_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_1991_gemeinderatswahlen_data_sub$Name
niedersachsen_1991_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_1991_gemeinderatswahlen_data_sub$'Wahl-berechtigte'
niedersachsen_1991_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_1991_gemeinderatswahlen_data_sub$Wähler
niedersachsen_1991_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_1991_gemeinderatswahlen_data_sub$GültigeStimmen

niedersachsen_1991_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_1991_gemeinderatswahlen_data_sub$CDU
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_1991_gemeinderatswahlen_data_sub$SPD
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_1991_gemeinderatswahlen_data_sub$GRÜNE
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

niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_CDU <- niedersachsen_1991_gemeinderatswahlen_data_sitze$CDU
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_SPD <- niedersachsen_1991_gemeinderatswahlen_data_sitze$SPD
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_GRÜNE <- niedersachsen_1991_gemeinderatswahlen_data_sitze$GRÜNE
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_FDP <- niedersachsen_1991_gemeinderatswahlen_data_sitze$FDP
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_1991_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_1991_gemeinderatswahlen_data_sub <- niedersachsen_1991_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_1991_gemeinderatswahlen_data_sub <-
  niedersachsen_1991_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
niedersachsen_1991_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_1991_gemeinderatswahlen_data_sub$Wähler / niedersachsen_1991_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Niedersachsen 1996 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_1996_gemeinderatswahlen_data <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_1996.xls", sheet="anzahl"))
niedersachsen_1996_gemeinderatswahlen_data_sitze <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_1996.xls", sheet="sitze"))

#### Delete white space ----
names(niedersachsen_1996_gemeinderatswahlen_data) <-  str_replace_all(names(niedersachsen_1996_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
niedersachsen_1996_gemeinderatswahlen_data_sub <- niedersachsen_1996_gemeinderatswahlen_data

names(niedersachsen_1996_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_1996_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_1996_gemeinderatswahlen_data_sub[ , Bundesland := "Niedersachsen"]
niedersachsen_1996_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
niedersachsen_1996_gemeinderatswahlen_data_sub[ , election_year := "1996"]
niedersachsen_1996_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
niedersachsen_1996_gemeinderatswahlen_data_sub[ , IDIRB := ""]
niedersachsen_1996_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
niedersachsen_1996_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_1996_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_1996_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_1996_gemeinderatswahlen_data_sub$Name
niedersachsen_1996_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_1996_gemeinderatswahlen_data_sub$'Wahl-berechtigte'
niedersachsen_1996_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_1996_gemeinderatswahlen_data_sub$Wähler
niedersachsen_1996_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_1996_gemeinderatswahlen_data_sub$GültigeStimmen

niedersachsen_1996_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_1996_gemeinderatswahlen_data_sub$CDU
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_1996_gemeinderatswahlen_data_sub$SPD
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_DIELINKE <- niedersachsen_1996_gemeinderatswahlen_data_sub$PDSLinkeListe
niedersachsen_1996_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_1996_gemeinderatswahlen_data_sub$GRÜNE
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

niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_CDU <- niedersachsen_1996_gemeinderatswahlen_data_sitze$CDU
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_SPD <- niedersachsen_1996_gemeinderatswahlen_data_sitze$SPD
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_DIELINKE <- niedersachsen_1996_gemeinderatswahlen_data_sitze$'PDS Linke Liste'
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_GRÜNE <- niedersachsen_1996_gemeinderatswahlen_data_sitze$GRÜNE
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_FDP <- niedersachsen_1996_gemeinderatswahlen_data_sitze$FDP
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_1996_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_1996_gemeinderatswahlen_data_sub <- niedersachsen_1996_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                     Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                     abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                     gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                     sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_1996_gemeinderatswahlen_data_sub <-
  niedersachsen_1996_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
niedersachsen_1996_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_1996_gemeinderatswahlen_data_sub$Wähler / niedersachsen_1996_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Niedersachsen 2001 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2001_gemeinderatswahlen_data <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2001.xls", sheet="anzahl"))
niedersachsen_2001_gemeinderatswahlen_data_sitze <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2001.xls", sheet="sitze"))

#### Delete white space ----
names(niedersachsen_2001_gemeinderatswahlen_data) <-  str_replace_all(names(niedersachsen_2001_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
niedersachsen_2001_gemeinderatswahlen_data_sub <- niedersachsen_2001_gemeinderatswahlen_data

names(niedersachsen_2001_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2001_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2001_gemeinderatswahlen_data_sub[ , Bundesland := "Niedersachsen"]
niedersachsen_2001_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
niedersachsen_2001_gemeinderatswahlen_data_sub[ , election_year := "2001"]
niedersachsen_2001_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
niedersachsen_2001_gemeinderatswahlen_data_sub[ , IDIRB := ""]
niedersachsen_2001_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
niedersachsen_2001_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_2001_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_2001_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_2001_gemeinderatswahlen_data_sub$Name
niedersachsen_2001_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2001_gemeinderatswahlen_data_sub$'Wahl-berechtigte'
niedersachsen_2001_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2001_gemeinderatswahlen_data_sub$Wähler
niedersachsen_2001_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2001_gemeinderatswahlen_data_sub$GültigeStimmen

niedersachsen_2001_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_2001_gemeinderatswahlen_data_sub$CDU
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_2001_gemeinderatswahlen_data_sub$SPD
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_DIELINKE <- niedersachsen_2001_gemeinderatswahlen_data_sub$PDS
niedersachsen_2001_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_2001_gemeinderatswahlen_data_sub$GRÜNE
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

niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_CDU <- niedersachsen_2001_gemeinderatswahlen_data_sitze$CDU
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_SPD <- niedersachsen_2001_gemeinderatswahlen_data_sitze$SPD
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_DIELINKE <- niedersachsen_2001_gemeinderatswahlen_data_sitze$'PDS'
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_GRÜNE <- niedersachsen_2001_gemeinderatswahlen_data_sitze$GRÜNE
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_FDP <- niedersachsen_2001_gemeinderatswahlen_data_sitze$FDP
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2001_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2001_gemeinderatswahlen_data_sub <- niedersachsen_2001_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                     Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                     abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                     gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                     sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2001_gemeinderatswahlen_data_sub <-
  niedersachsen_2001_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
niedersachsen_2001_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_2001_gemeinderatswahlen_data_sub$Wähler / niedersachsen_2001_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Niedersachsen 2006 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2006_gemeinderatswahlen_data <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2006.xls", sheet="anzahl"))
niedersachsen_2006_gemeinderatswahlen_data_sitze <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2006.xls", sheet="sitze"))

#### Delete white space ----
names(niedersachsen_2006_gemeinderatswahlen_data) <-  str_replace_all(names(niedersachsen_2006_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
niedersachsen_2006_gemeinderatswahlen_data_sub <- niedersachsen_2006_gemeinderatswahlen_data

names(niedersachsen_2006_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2006_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2006_gemeinderatswahlen_data_sub[ , Bundesland := "Niedersachsen"]
niedersachsen_2006_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
niedersachsen_2006_gemeinderatswahlen_data_sub[ , election_year := "2006"]
niedersachsen_2006_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
niedersachsen_2006_gemeinderatswahlen_data_sub[ , IDIRB := ""]
niedersachsen_2006_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
niedersachsen_2006_gemeinderatswahlen_data_sub$AGS_8dig <- niedersachsen_2006_gemeinderatswahlen_data_sub$Gemeinde
niedersachsen_2006_gemeinderatswahlen_data_sub$Gebietsname <- niedersachsen_2006_gemeinderatswahlen_data_sub$Name
niedersachsen_2006_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2006_gemeinderatswahlen_data_sub$'Wahlberechtigte'
niedersachsen_2006_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2006_gemeinderatswahlen_data_sub$Wähler
niedersachsen_2006_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2006_gemeinderatswahlen_data_sub$GültigeStimmen

niedersachsen_2006_gemeinderatswahlen_data_sub$abs_CDU <- niedersachsen_2006_gemeinderatswahlen_data_sub$CDU
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_SPD <- niedersachsen_2006_gemeinderatswahlen_data_sub$SPD
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_DIELINKE <- niedersachsen_2006_gemeinderatswahlen_data_sub$'DieLinke.'
niedersachsen_2006_gemeinderatswahlen_data_sub$abs_GRÜNE <- niedersachsen_2006_gemeinderatswahlen_data_sub$GRÜNE
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

niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_CDU <- niedersachsen_2006_gemeinderatswahlen_data_sitze$CDU
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_SPD <- niedersachsen_2006_gemeinderatswahlen_data_sitze$SPD
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_DIELINKE <- niedersachsen_2006_gemeinderatswahlen_data_sitze$'Die Linke.'
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_GRÜNE <- niedersachsen_2006_gemeinderatswahlen_data_sitze$GRÜNE
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_FDP <- niedersachsen_2006_gemeinderatswahlen_data_sitze$FDP
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2006_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2006_gemeinderatswahlen_data_sub <- niedersachsen_2006_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                     Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                     abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                     gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                     sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2006_gemeinderatswahlen_data_sub <-
  niedersachsen_2006_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
niedersachsen_2006_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_2006_gemeinderatswahlen_data_sub$Wähler / niedersachsen_2006_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Niedersachsen 2011 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2011_gemeinderatswahlen_data <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2011_anzahl.xlsx", sheet="summary"))
niedersachsen_2011_gemeinderatswahlen_data_sitze <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2011_sitze.xlsx", sheet="summary"))

#### Delete white space ----
names(niedersachsen_2011_gemeinderatswahlen_data) <-  str_replace_all(names(niedersachsen_2011_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
niedersachsen_2011_gemeinderatswahlen_data_sub <- niedersachsen_2011_gemeinderatswahlen_data

names(niedersachsen_2011_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2011_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2011_gemeinderatswahlen_data_sub[ , Bundesland := "Niedersachsen"]
niedersachsen_2011_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
niedersachsen_2011_gemeinderatswahlen_data_sub[ , election_year := "2011"]
niedersachsen_2011_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
niedersachsen_2011_gemeinderatswahlen_data_sub[ , IDIRB := ""]
niedersachsen_2011_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_CDU <- niedersachsen_2011_gemeinderatswahlen_data_sitze$'CDU(insgesamt)'
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_SPD <- niedersachsen_2011_gemeinderatswahlen_data_sitze$'SPD(insgesamt)'
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_DIELINKE <- niedersachsen_2011_gemeinderatswahlen_data_sitze$'DIE LINKE.(insgesamt)'
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_GRÜNE <- niedersachsen_2011_gemeinderatswahlen_data_sitze$'GRÜNE(insgesamt)'
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_AfD <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_PIRATEN <- niedersachsen_2011_gemeinderatswahlen_data_sitze$'PIRATEN Niedersachsen(insgesamt)'
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_FDP <- niedersachsen_2011_gemeinderatswahlen_data_sitze$'FDP(insgesamt)'
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2011_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2011_gemeinderatswahlen_data_sub <- niedersachsen_2011_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                     Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                     abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                     gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                     sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2011_gemeinderatswahlen_data_sub <-
  niedersachsen_2011_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
niedersachsen_2011_gemeinderatswahlen_data_sub$Turnout <- niedersachsen_2011_gemeinderatswahlen_data_sub$Wähler / niedersachsen_2011_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### Niedersachsen 2016 Gemeinderatswahlen ----
#### Load election data ----
niedersachsen_2016_gemeinderatswahlen_data <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2016.xlsx", sheet="anzahl"))
niedersachsen_2016_gemeinderatswahlen_data_sitze <- as.data.table(read_excel("raw/niedersachsen/niedersachsen_gemeinderatswahlen_2016.xlsx", sheet="sitze"))

niedersachsen_2016_gemeinderatswahlen_data_sitze <-
  niedersachsen_2016_gemeinderatswahlen_data_sitze %>%
  filter(MG %in%  niedersachsen_2016_gemeinderatswahlen_data$MG)

niedersachsen_2016_gemeinderatswahlen_data <-
  niedersachsen_2016_gemeinderatswahlen_data %>%
  filter(MG %in%  niedersachsen_2016_gemeinderatswahlen_data_sitze$MG)


#### Delete white space ----
names(niedersachsen_2016_gemeinderatswahlen_data) <-  str_replace_all(names(niedersachsen_2016_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
niedersachsen_2016_gemeinderatswahlen_data_sub <- niedersachsen_2016_gemeinderatswahlen_data

names(niedersachsen_2016_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
niedersachsen_2016_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
niedersachsen_2016_gemeinderatswahlen_data_sub[ , Bundesland := "Niedersachsen"]
niedersachsen_2016_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
niedersachsen_2016_gemeinderatswahlen_data_sub[ , election_year := "2016"]
niedersachsen_2016_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
niedersachsen_2016_gemeinderatswahlen_data_sub[ , IDIRB := ""]
niedersachsen_2016_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
niedersachsen_2016_gemeinderatswahlen_data_sub$AGS_8dig <- parse_number(niedersachsen_2016_gemeinderatswahlen_data_sub$EG)
niedersachsen_2016_gemeinderatswahlen_data_sub$Gebietsname <- gsub("[^a-z A-Z]", "", niedersachsen_2016_gemeinderatswahlen_data_sub$MG)
niedersachsen_2016_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- niedersachsen_2016_gemeinderatswahlen_data_sub$'Wahlberechtigung'
niedersachsen_2016_gemeinderatswahlen_data_sub$Wähler <- niedersachsen_2016_gemeinderatswahlen_data_sub$Waehler
niedersachsen_2016_gemeinderatswahlen_data_sub$GültigeStimmen <- niedersachsen_2016_gemeinderatswahlen_data_sub$GueltigeStimmen

niedersachsen_2016_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$CDU)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$SPD)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$DIELINKE)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$Gruene)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$AfD)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_PIRATEN <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$PIRATEN)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$FDP)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_DiePARTEI <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$DiePARTEI)
niedersachsen_2016_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$FreieWaehler)

niedersachsen_2016_gemeinderatswahlen_data_sub$gew_CDU <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_SPD <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_AfD <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_FDP <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_CDU <- niedersachsen_2016_gemeinderatswahlen_data_sitze$CDU
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_SPD <- niedersachsen_2016_gemeinderatswahlen_data_sitze$SPD
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_DIELINKE <- niedersachsen_2016_gemeinderatswahlen_data_sitze$DIELINKE
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_GRÜNE <- niedersachsen_2016_gemeinderatswahlen_data_sitze$Gruene
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_AfD <- niedersachsen_2016_gemeinderatswahlen_data_sitze$AfD
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_FDP <- niedersachsen_2016_gemeinderatswahlen_data_sitze$FDP
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
niedersachsen_2016_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
niedersachsen_2016_gemeinderatswahlen_data_sub <- niedersachsen_2016_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                     Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                     abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                                     gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                                     sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

niedersachsen_2016_gemeinderatswahlen_data_sub <-
  niedersachsen_2016_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
niedersachsen_2016_gemeinderatswahlen_data_sub$Turnout <- as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$Wähler) / as.numeric(niedersachsen_2016_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt)

####### Merge files and save overall output for Niedersachsen ----
# Merge
niedersachsen_kommunalwahlen <- rbind(niedersachsen_1991_gemeinderatswahlen_data_sub, niedersachsen_1996_gemeinderatswahlen_data_sub,
                                      niedersachsen_2001_gemeinderatswahlen_data_sub, niedersachsen_2006_gemeinderatswahlen_data_sub,
                                      niedersachsen_2011_gemeinderatswahlen_data_sub, niedersachsen_2016_gemeinderatswahlen_data_sub)

# Replace - with NA
niedersachsen_kommunalwahlen[niedersachsen_kommunalwahlen == "-"] <- NA

# Fix AGS
niedersachsen_kommunalwahlen$AGS_8dig <- paste("03", niedersachsen_kommunalwahlen$AGS_8dig, sep="")

# Fix Gebietsnamen
niedersachsen_kommunalwahlen$Gebietsname <- str_replace(niedersachsen_kommunalwahlen$Gebietsname, "MG","")
niedersachsen_kommunalwahlen$Gebietsname <- str_replace(niedersachsen_kommunalwahlen$Gebietsname, "EG","")


# Save
#write_csv(niedersachsen_kommunalwahlen, here::here("output/niedersachsen_kommunalwahlen.csv"))

# ----
# ----

######### SACHSEN ----
###### Sachsen 1994 Gemeinderatswahlen ----
#### Load election data ----

sachsen_1994_gemeinderatswahlen_data <- as.data.table(read_excel("raw/sachsen/sachsen_1994.xlsx", sheet="summary"))

#### Delete white space ----
names(sachsen_1994_gemeinderatswahlen_data) <-  str_replace_all(names(sachsen_1994_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_1994_gemeinderatswahlen_data_sub <- sachsen_1994_gemeinderatswahlen_data

# Creating non-existing variables ----
sachsen_1994_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_1994_gemeinderatswahlen_data_sub[ , Bundesland := "Sachsen"]
sachsen_1994_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sachsen_1994_gemeinderatswahlen_data_sub[ , election_year := "1994"]
sachsen_1994_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sachsen_1994_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sachsen_1994_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sachsen_1994_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_1994_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_1994_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_1994_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_1994_gemeinderatswahlen_data_sub <- sachsen_1994_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_1994_gemeinderatswahlen_data_sub <-
  sachsen_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_1994_gemeinderatswahlen_data_sub$Turnout <- sachsen_1994_gemeinderatswahlen_data_sub$Wähler / sachsen_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_1994_gemeinderatswahlen_data_sub <- sachsen_1994_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen 1999 Gemeinderatswahlen ----
#### Load election data ----

sachsen_1999_gemeinderatswahlen_data <- as.data.table(read_excel("raw/sachsen/sachsen_1999.xlsx", sheet="summary"))

#### Delete white space ----
names(sachsen_1999_gemeinderatswahlen_data) <-  str_replace_all(names(sachsen_1999_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_1999_gemeinderatswahlen_data_sub <- sachsen_1999_gemeinderatswahlen_data

names(sachsen_1999_gemeinderatswahlen_data_sub)


# Creating non-existing variables ----
sachsen_1999_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_1999_gemeinderatswahlen_data_sub[ , Bundesland := "Sachsen"]
sachsen_1999_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sachsen_1999_gemeinderatswahlen_data_sub[ , election_year := "1999"]
sachsen_1999_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sachsen_1999_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sachsen_1999_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sachsen_1999_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_1999_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_1999_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_1999_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_1999_gemeinderatswahlen_data_sub <- sachsen_1999_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_1999_gemeinderatswahlen_data_sub <-
  sachsen_1999_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_1999_gemeinderatswahlen_data_sub$Turnout <- sachsen_1999_gemeinderatswahlen_data_sub$Wähler / sachsen_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


# Remove !nchar(AGS)==8 ----
sachsen_1999_gemeinderatswahlen_data_sub <- sachsen_1999_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen 2004 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2004_gemeinderatswahlen_data <- as.data.table(read_excel("raw/sachsen/sachsen_2004.xlsx", sheet="summary"))

#### Delete white space ----
names(sachsen_2004_gemeinderatswahlen_data) <-  str_replace_all(names(sachsen_2004_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_2004_gemeinderatswahlen_data_sub <- sachsen_2004_gemeinderatswahlen_data

names(sachsen_2004_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2004_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_2004_gemeinderatswahlen_data_sub[ , Bundesland := "Sachsen"]
sachsen_2004_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sachsen_2004_gemeinderatswahlen_data_sub[ , election_year := "2004"]
sachsen_2004_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sachsen_2004_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sachsen_2004_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sachsen_2004_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2004_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_2004_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2004_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_2004_gemeinderatswahlen_data_sub <- sachsen_2004_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2004_gemeinderatswahlen_data_sub <-
  sachsen_2004_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_2004_gemeinderatswahlen_data_sub$Turnout <- sachsen_2004_gemeinderatswahlen_data_sub$Wähler / sachsen_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2004_gemeinderatswahlen_data_sub <- sachsen_2004_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen 2009 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2009_gemeinderatswahlen_data <- as.data.table(read_excel("raw/sachsen/sachsen_2009.xlsx", sheet="summary"))

#### Delete white space ----
names(sachsen_2009_gemeinderatswahlen_data) <-  str_replace_all(names(sachsen_2009_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_2009_gemeinderatswahlen_data_sub <- sachsen_2009_gemeinderatswahlen_data

names(sachsen_2009_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2009_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_2009_gemeinderatswahlen_data_sub[ , Bundesland := "Sachsen"]
sachsen_2009_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sachsen_2009_gemeinderatswahlen_data_sub[ , election_year := "2009"]
sachsen_2009_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sachsen_2009_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sachsen_2009_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sachsen_2009_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2009_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_2009_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2009_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_2009_gemeinderatswahlen_data_sub <- sachsen_2009_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2009_gemeinderatswahlen_data_sub <-
  sachsen_2009_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_2009_gemeinderatswahlen_data_sub$Turnout <- sachsen_2009_gemeinderatswahlen_data_sub$Wähler / sachsen_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2009_gemeinderatswahlen_data_sub <- sachsen_2009_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen 2014 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2014_gemeinderatswahlen_data <- as.data.table(read_excel("raw/sachsen/sachsen_2014.xlsx", sheet="summary"))

#### Delete white space ----
names(sachsen_2014_gemeinderatswahlen_data) <-  str_replace_all(names(sachsen_2014_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_2014_gemeinderatswahlen_data_sub <- sachsen_2014_gemeinderatswahlen_data

names(sachsen_2014_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2014_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_2014_gemeinderatswahlen_data_sub[ , Bundesland := "Sachsen"]
sachsen_2014_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sachsen_2014_gemeinderatswahlen_data_sub[ , election_year := "2014"]
sachsen_2014_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sachsen_2014_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sachsen_2014_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sachsen_2014_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2014_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_2014_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2014_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_2014_gemeinderatswahlen_data_sub <- sachsen_2014_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2014_gemeinderatswahlen_data_sub <-
  sachsen_2014_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_2014_gemeinderatswahlen_data_sub$Turnout <- sachsen_2014_gemeinderatswahlen_data_sub$Wähler / sachsen_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2014_gemeinderatswahlen_data_sub <- sachsen_2014_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)

###### Sachsen 2019 Gemeinderatswahlen ----
#### Load election data ----

sachsen_2019_gemeinderatswahlen_data <- as.data.table(read_excel("raw/sachsen/sachsen_2019.xlsx", sheet="summary"))

#### Delete white space ----
names(sachsen_2019_gemeinderatswahlen_data) <-  str_replace_all(names(sachsen_2019_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sachsen_2019_gemeinderatswahlen_data_sub <- sachsen_2019_gemeinderatswahlen_data

names(sachsen_2019_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sachsen_2019_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sachsen_2019_gemeinderatswahlen_data_sub[ , Bundesland := "Sachsen"]
sachsen_2019_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sachsen_2019_gemeinderatswahlen_data_sub[ , election_year := "2019"]
sachsen_2019_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sachsen_2019_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sachsen_2019_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sachsen_2019_gemeinderatswahlen_data_sub$gew_CDU <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_SPD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_DIELINKE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_GRÜNE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_AfD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_FDP <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
sachsen_2019_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA

sachsen_2019_gemeinderatswahlen_data_sub$sitze_CDU <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_SPD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_DIELINKE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_GRÜNE <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_AfD <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_FDP <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
sachsen_2019_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA

# Creating new dataframe with selected vars ----
sachsen_2019_gemeinderatswahlen_data_sub <- sachsen_2019_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sachsen_2019_gemeinderatswahlen_data_sub <-
  sachsen_2019_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sachsen_2019_gemeinderatswahlen_data_sub$Turnout <- sachsen_2019_gemeinderatswahlen_data_sub$Wähler / sachsen_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

# Remove !nchar(AGS)==8 ----
sachsen_2019_gemeinderatswahlen_data_sub <- sachsen_2019_gemeinderatswahlen_data_sub %>%
  filter(nchar(AGS_8dig)==8)


####### Merge files and save overall output for Sachsen ----
# Merge
sachsen_kommunalwahlen <- rbind(sachsen_1994_gemeinderatswahlen_data_sub, sachsen_1999_gemeinderatswahlen_data_sub,
                                sachsen_2004_gemeinderatswahlen_data_sub, sachsen_2009_gemeinderatswahlen_data_sub,
                                sachsen_2014_gemeinderatswahlen_data_sub, sachsen_2019_gemeinderatswahlen_data_sub)

# Replace - with NA
sachsen_kommunalwahlen[sachsen_kommunalwahlen == "-"] <- NA

# Fix AGS ----
sachsen_kommunalwahlen$AGS_8dig <- stri_pad_right(sachsen_kommunalwahlen$AGS_8dig, 8, 0)


# Save
#write_csv(sachsen_kommunalwahlen, here::here("output/sachsen_kommunalwahlen.csv"))


# ----
# ----
######### BREMEN ----
###### Bremen Bürgerschaftswahlen ----
#### Load election data ----

bremen_overall_buergerschaftswahl_data <- as.data.table(read_excel("raw/bremen/bremen_summary.xlsx", sheet="summary"))

#### Delete white space ----
names(bremen_overall_buergerschaftswahl_data) <-  str_replace_all(names(bremen_overall_buergerschaftswahl_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
bremen_overall_buergerschaftswahl_data_sub <- bremen_overall_buergerschaftswahl_data

# Creating non-existing variables ----
bremen_overall_buergerschaftswahl_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
bremen_overall_buergerschaftswahl_data_sub[ , Bundesland := "Bremen"]
bremen_overall_buergerschaftswahl_data_sub[ , Gebietsname := ""]
bremen_overall_buergerschaftswahl_data_sub[ , election_type := "Bürgerschaftswahl"]
bremen_overall_buergerschaftswahl_data_sub[ , IDIRB := ""]
bremen_overall_buergerschaftswahl_data_sub[ , IDBA := ""]

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
bremen_overall_buergerschaftswahl_data_sub <- bremen_overall_buergerschaftswahl_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                       Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                       abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                                       gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                                       sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

bremen_overall_buergerschaftswahl_data_sub <-
  bremen_overall_buergerschaftswahl_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
bremen_overall_buergerschaftswahl_data_sub$Turnout <- bremen_overall_buergerschaftswahl_data_sub$Wähler / bremen_overall_buergerschaftswahl_data_sub$Wahlberechtigteinsgesamt

# Remove Bremen Land ----
bremen_overall_buergerschaftswahl_data_sub <- bremen_overall_buergerschaftswahl_data_sub %>%
  filter(
    ! Gebietsname == "Land Bremen"
  )

####### Merge files and save overall output for Bremen ----
# Save
#write_csv(bremen_overall_buergerschaftswahl_data_sub, here::here("output/bremen_buergerschaftswahlen.csv"))

# ----
# ----
######### BRANDENBURG ----
###### Brandenburg 1993 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_1993_gemeinderatswahlen_data <- as.data.table(read_excel("raw/brandenburg/brandenburg_1993.xlsx", sheet="Ergebnis"))

# Transpose
brandenburg_1993_gemeinderatswahlen_data_recoded <- as.data.frame(t(brandenburg_1993_gemeinderatswahlen_data))

# Rename the columns with the first row (AGS)
colnames(brandenburg_1993_gemeinderatswahlen_data_recoded) <- brandenburg_1993_gemeinderatswahlen_data_recoded[1, ]

# Remove the first row, since it's now in the column names
brandenburg_1993_gemeinderatswahlen_data_recoded <- brandenburg_1993_gemeinderatswahlen_data_recoded[-c(1:2), ]

# Remove row names
rownames(brandenburg_1993_gemeinderatswahlen_data_recoded) <- NULL


#### Recoding ----
# Fix AGS 
brandenburg_1993_gemeinderatswahlen_data_recoded <- brandenburg_1993_gemeinderatswahlen_data_recoded %>%
  mutate(AGS_8dig = paste0("12", str_remove_all(AGS, " ")))

#test <- brandenburg_1993_gemeinderatswahlen_data_recoded[nchar(brandenburg_1993_gemeinderatswahlen_data_recoded$AGS_8dig) !=8,]

# Creating non-existing variables ----
brandenburg_1993_gemeinderatswahlen_data_recoded <- as.data.table(brandenburg_1993_gemeinderatswahlen_data_recoded)
brandenburg_1993_gemeinderatswahlen_data_recoded[ , Bundesland := "Brandenburg"]
brandenburg_1993_gemeinderatswahlen_data_recoded[ , election_year := "1993"]
brandenburg_1993_gemeinderatswahlen_data_recoded[ , election_type := "Gemeinderatswahlen"]
brandenburg_1993_gemeinderatswahlen_data_recoded[ , IDIRB := ""]
brandenburg_1993_gemeinderatswahlen_data_recoded[ , IDBA := ""]

# Renaming existing variables ----
brandenburg_1993_gemeinderatswahlen_data_recoded$Gebietsname <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$Wahlberechtigte)
brandenburg_1993_gemeinderatswahlen_data_recoded$Wähler <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$Wähler)
brandenburg_1993_gemeinderatswahlen_data_recoded$GültigeStimmen <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$`gültige Stimmen`)

brandenburg_1993_gemeinderatswahlen_data_recoded$abs_CDU <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$CDU)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_SPD <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$SPD)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_DIELINKE <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$PDS)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_GRÜNE <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$`GRÜNE/B90`)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_AfD <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_PIRATEN <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_FDP <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$`F.D.P.`)
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1993_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- as.numeric(brandenburg_1993_gemeinderatswahlen_data_recoded$WG)

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
brandenburg_1993_gemeinderatswahlen_data_recoded <- brandenburg_1993_gemeinderatswahlen_data_recoded[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_1993_gemeinderatswahlen_data_recoded <-
  brandenburg_1993_gemeinderatswahlen_data_recoded %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
brandenburg_1993_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_1993_gemeinderatswahlen_data_recoded$Wähler / brandenburg_1993_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 1998 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_1998_gemeinderatswahlen_data <- as.data.table(read_excel("raw/brandenburg/brandenburg_1998.xlsx", sheet="Ergebnis"))

# Transpose
brandenburg_1998_gemeinderatswahlen_data_recoded <- as.data.frame(t(brandenburg_1998_gemeinderatswahlen_data))

# Rename the columns with the first row (AGS)
colnames(brandenburg_1998_gemeinderatswahlen_data_recoded) <- brandenburg_1998_gemeinderatswahlen_data_recoded[1, ]

# Remove the first row, since it's now in the column names
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded[-c(1), ]

# Remove row names
rownames(brandenburg_1998_gemeinderatswahlen_data_recoded) <- NULL


#### Recoding ----
# Fix AGS 
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded %>%
  mutate(AGS_8dig = str_remove_all(AGS, " ")) %>%
  filter(!is.na(AGS_8dig))

# Remove Wahlkreise
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded %>%
  filter(!AGS_name %in% c("Uckermark", "Barnim", "Prignitz", "Ostprignitz-Ruppin", "Oberhavel",
                          "Havelland", "Märkisch-Oderland", "Oder-Spree", "Teltow-Fläming", 
                          "Dahme-Spreewald", "Elbe-Elster", "Oberspreewald-Lausitz",
                          "Spree-Neiße", "Potsdam-Mittelmark"))

# test <- brandenburg_1998_gemeinderatswahlen_data_recoded[nchar(brandenburg_1998_gemeinderatswahlen_data_recoded$AGS_8dig) !=8,]
# 
# test <- brandenburg_1998_gemeinderatswahlen_data_recoded %>%
#   filter(grepl("[^0-9]", AGS_8dig))

# Creating non-existing variables ----
brandenburg_1998_gemeinderatswahlen_data_recoded <- as.data.table(brandenburg_1998_gemeinderatswahlen_data_recoded)
brandenburg_1998_gemeinderatswahlen_data_recoded[ , Bundesland := "Brandenburg"]
brandenburg_1998_gemeinderatswahlen_data_recoded[ , election_year := "1998"]
brandenburg_1998_gemeinderatswahlen_data_recoded[ , election_type := "Gemeinderatswahlen"]
brandenburg_1998_gemeinderatswahlen_data_recoded[ , IDIRB := ""]
brandenburg_1998_gemeinderatswahlen_data_recoded[ , IDBA := ""]

# Renaming existing variables ----
brandenburg_1998_gemeinderatswahlen_data_recoded$Gebietsname <- brandenburg_1998_gemeinderatswahlen_data_recoded$AGS_name
brandenburg_1998_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$Wahlberechtigte)
brandenburg_1998_gemeinderatswahlen_data_recoded$Wähler <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$Wähler)
brandenburg_1998_gemeinderatswahlen_data_recoded$GültigeStimmen <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$`gültige Stimmen`)

brandenburg_1998_gemeinderatswahlen_data_recoded$abs_CDU <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$CDU)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_SPD <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$SPD)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_DIELINKE <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$PDS)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_GRÜNE <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$`GRÜNE/B90`)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_AfD <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_PIRATEN <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_FDP <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$`F.D.P.`)
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_DiePARTEI <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_FREIEWÄHLER <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_Gemeinsame_Wahlvorschläge <- NA
brandenburg_1998_gemeinderatswahlen_data_recoded$abs_Wählergruppen <- as.numeric(brandenburg_1998_gemeinderatswahlen_data_recoded$WG)

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
brandenburg_1998_gemeinderatswahlen_data_recoded <- brandenburg_1998_gemeinderatswahlen_data_recoded[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_1998_gemeinderatswahlen_data_recoded <-
  brandenburg_1998_gemeinderatswahlen_data_recoded %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
brandenburg_1998_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_1998_gemeinderatswahlen_data_recoded$Wähler / brandenburg_1998_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 2003 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2003_gemeinderatswahlen_data <- as.data.table(read_excel("raw/brandenburg/brandenburg_2003.xlsx", sheet="Ergebnis"))
names(brandenburg_2003_gemeinderatswahlen_data) <-  str_replace_all(names(brandenburg_2003_gemeinderatswahlen_data), fixed(" "), "")

#### Recode to split by party vote ----
brandenburg_2003_gemeinderatswahlen_data_recoded <-
  brandenburg_2003_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(Wahlberechtigte=mean(Wahlberechtigte),
            Wähler=mean(Wähler),
            gültigeStimmen=mean(gültigeStimmen),
            Gebietsname=unique(Gemeindename))
brandenburg_2003_gemeinderatswahlen_data_recoded <- as.data.frame(brandenburg_2003_gemeinderatswahlen_data_recoded)

# CDU
brandenburg_2003_gemeinderatswahlen_data_CDU <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="CDU"]
brandenburg_2003_gemeinderatswahlen_data_CDU <- brandenburg_2003_gemeinderatswahlen_data_CDU[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_CDU)[names(brandenburg_2003_gemeinderatswahlen_data_CDU) == "Stimmen"] <- "CDU"
names(brandenburg_2003_gemeinderatswahlen_data_CDU)[names(brandenburg_2003_gemeinderatswahlen_data_CDU) == "Sitze"] <- "sitze_CDU"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_CDU, by=c('AGS'), all.x=TRUE)

# SPD
brandenburg_2003_gemeinderatswahlen_data_SPD <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="SPD"]
brandenburg_2003_gemeinderatswahlen_data_SPD <- brandenburg_2003_gemeinderatswahlen_data_SPD[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_SPD)[names(brandenburg_2003_gemeinderatswahlen_data_SPD) == "Stimmen"] <- "SPD"
names(brandenburg_2003_gemeinderatswahlen_data_SPD)[names(brandenburg_2003_gemeinderatswahlen_data_SPD) == "Sitze"] <- "sitze_SPD"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_SPD, by=c('AGS'), all.x=TRUE)

# FDP
brandenburg_2003_gemeinderatswahlen_data_FDP <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="FDP"]
brandenburg_2003_gemeinderatswahlen_data_FDP <- brandenburg_2003_gemeinderatswahlen_data_FDP[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_FDP)[names(brandenburg_2003_gemeinderatswahlen_data_FDP) == "Stimmen"] <- "FDP"
names(brandenburg_2003_gemeinderatswahlen_data_FDP)[names(brandenburg_2003_gemeinderatswahlen_data_FDP) == "Sitze"] <- "sitze_FDP"

brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_FDP, by=c('AGS'), all.x=TRUE)

# DIELINKE
brandenburg_2003_gemeinderatswahlen_data_DIELINKE <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="PDS"]
brandenburg_2003_gemeinderatswahlen_data_DIELINKE <- brandenburg_2003_gemeinderatswahlen_data_DIELINKE[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE) == "Stimmen"] <- "DIELINKE"
names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2003_gemeinderatswahlen_data_DIELINKE) == "Sitze"] <- "sitze_DIELINKE"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_DIELINKE, by=c('AGS'), all.x=TRUE)

# GRUENE
brandenburg_2003_gemeinderatswahlen_data_GRUENE <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="GRÜNE/B 90"]
brandenburg_2003_gemeinderatswahlen_data_GRUENE <- brandenburg_2003_gemeinderatswahlen_data_GRUENE[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2003_gemeinderatswahlen_data_GRUENE) == "Stimmen"] <- "GRUENE"
names(brandenburg_2003_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2003_gemeinderatswahlen_data_GRUENE) == "Sitze"] <- "sitze_GRUENE"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_GRUENE, by=c('AGS'), all.x=TRUE)

# Wählergruppen
brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="Wählergruppen"]
brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Stimmen"] <- "WAEHLERGRUPPEN"
names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"] <- "sitze_WAEHLERGRUPPEN"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_WAEHLERGRUPPEN, by=c('AGS'), all.x=TRUE)

# Einzelbewerber
brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2003_gemeinderatswahlen_data[brandenburg_2003_gemeinderatswahlen_data$Wahlvorschlagsträger=="Einzelbewerber"]
brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER) == "Stimmen"] <- "EINZELBEWERBER"
names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"] <- "sitze_EINZELBEWERBER"
brandenburg_2003_gemeinderatswahlen_data_recoded <- merge(brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2003_gemeinderatswahlen_data_EINZELBEWERBER, by=c('AGS'), all.x=TRUE)

brandenburg_2003_gemeinderatswahlen_data_recoded <- as.data.table(brandenburg_2003_gemeinderatswahlen_data_recoded)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2003_gemeinderatswahlen_data_recoded[ , AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2003_gemeinderatswahlen_data_recoded[ , Bundesland := "Brandenburg"]
brandenburg_2003_gemeinderatswahlen_data_recoded[ , election_year := "2003"]
brandenburg_2003_gemeinderatswahlen_data_recoded[ , election_type := "Gemeinderatswahlen"]
brandenburg_2003_gemeinderatswahlen_data_recoded[ , IDIRB := ""]
brandenburg_2003_gemeinderatswahlen_data_recoded[ , IDBA := ""]

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
brandenburg_2003_gemeinderatswahlen_data_recoded <- brandenburg_2003_gemeinderatswahlen_data_recoded[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2003_gemeinderatswahlen_data_recoded <-
  brandenburg_2003_gemeinderatswahlen_data_recoded %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
brandenburg_2003_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2003_gemeinderatswahlen_data_recoded$Wähler / brandenburg_2003_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 2008 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2008_gemeinderatswahlen_data <- as.data.table(read_excel("raw/brandenburg/brandenburg_2008.xlsx", sheet="Ergebnis"))
names(brandenburg_2008_gemeinderatswahlen_data) <-  str_replace_all(names(brandenburg_2008_gemeinderatswahlen_data), fixed(" "), "")

#### Recode to split by party vote ----
brandenburg_2008_gemeinderatswahlen_data_recoded <-
  brandenburg_2008_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(Wahlberechtigte=mean(Wahlberechtigte),
            Wähler=mean(Wähler),
            gültigeStimmen=mean(gültigeStimmen),
            Gebietsname=unique(Gemeindename))
brandenburg_2008_gemeinderatswahlen_data_recoded <- as.data.frame(brandenburg_2008_gemeinderatswahlen_data_recoded)

# CDU
brandenburg_2008_gemeinderatswahlen_data_CDU <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger=="CDU"]
brandenburg_2008_gemeinderatswahlen_data_CDU <- brandenburg_2008_gemeinderatswahlen_data_CDU[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_CDU)[names(brandenburg_2008_gemeinderatswahlen_data_CDU) == "Stimmen"] <- "CDU"
names(brandenburg_2008_gemeinderatswahlen_data_CDU)[names(brandenburg_2008_gemeinderatswahlen_data_CDU) == "Sitze"] <- "sitze_CDU"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_CDU, by=c('AGS'), all.x=TRUE)

# SPD
brandenburg_2008_gemeinderatswahlen_data_SPD <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger=="SPD"]
brandenburg_2008_gemeinderatswahlen_data_SPD <- brandenburg_2008_gemeinderatswahlen_data_SPD[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_SPD)[names(brandenburg_2008_gemeinderatswahlen_data_SPD) == "Stimmen"] <- "SPD"
names(brandenburg_2008_gemeinderatswahlen_data_SPD)[names(brandenburg_2008_gemeinderatswahlen_data_SPD) == "Sitze"] <- "sitze_SPD"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_SPD, by=c('AGS'), all.x=TRUE)

# FDP
brandenburg_2008_gemeinderatswahlen_data_FDP <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger=="FDP"]
brandenburg_2008_gemeinderatswahlen_data_FDP <- brandenburg_2008_gemeinderatswahlen_data_FDP[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_FDP)[names(brandenburg_2008_gemeinderatswahlen_data_FDP) == "Stimmen"] <- "FDP"
names(brandenburg_2008_gemeinderatswahlen_data_FDP)[names(brandenburg_2008_gemeinderatswahlen_data_FDP) == "Sitze"] <- "sitze_FDP"

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_FDP, by=c('AGS'), all.x=TRUE)

# DIELINKE
brandenburg_2008_gemeinderatswahlen_data_DIELINKE <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger=="DIE LINKE"]
brandenburg_2008_gemeinderatswahlen_data_DIELINKE <- brandenburg_2008_gemeinderatswahlen_data_DIELINKE[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE) == "Stimmen"] <- "DIELINKE"
names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2008_gemeinderatswahlen_data_DIELINKE) == "Sitze"] <- "sitze_DIELINKE"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_DIELINKE, by=c('AGS'), all.x=TRUE)

# GRUENE
brandenburg_2008_gemeinderatswahlen_data_GRUENE <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$Wahlvorschlagsträger=="GRÜNE/B 90"]
brandenburg_2008_gemeinderatswahlen_data_GRUENE <- brandenburg_2008_gemeinderatswahlen_data_GRUENE[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2008_gemeinderatswahlen_data_GRUENE) == "Stimmen"] <- "GRUENE"
names(brandenburg_2008_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2008_gemeinderatswahlen_data_GRUENE) == "Sitze"] <- "sitze_GRUENE"
brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_GRUENE, by=c('AGS'), all.x=TRUE)

# FW
brandenburg_2008_gemeinderatswahlen_data_FW <- dplyr::filter(brandenburg_2008_gemeinderatswahlen_data, grepl('Freie Wähler|FW|Freie WG', Wahlvorschlagsträger))
brandenburg_2008_gemeinderatswahlen_data_FW <- brandenburg_2008_gemeinderatswahlen_data_FW[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_FW)[names(brandenburg_2008_gemeinderatswahlen_data_FW) == "Stimmen"] <- "FREIEWÄHLER"
names(brandenburg_2008_gemeinderatswahlen_data_FW)[names(brandenburg_2008_gemeinderatswahlen_data_FW) == "Sitze"] <- "sitze_FREIEWÄHLER"

brandenburg_2008_gemeinderatswahlen_data_FW <- brandenburg_2008_gemeinderatswahlen_data_FW %>%
  group_by(AGS) %>%
  mutate(FREIEWÄHLER = sum(FREIEWÄHLER, na.rm=T),
         sitze_FREIEWÄHLER = sum(sitze_FREIEWÄHLER, na.rm = T)) %>%
  distinct()

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_FW, by=c('AGS'), all.x=TRUE)

# Wählergruppen

brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$`ArtdesWahl-vorschlags-trägers`=="WG"]
brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Stimmen"] <- "WAEHLERGRUPPEN"
names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"] <- "sitze_WAEHLERGRUPPEN"

brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN %>%
  group_by(AGS) %>%
  mutate(WAEHLERGRUPPEN = sum(WAEHLERGRUPPEN, na.rm=T),
         sitze_WAEHLERGRUPPEN = sum(sitze_WAEHLERGRUPPEN, na.rm = T)) %>%
  distinct()

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_WAEHLERGRUPPEN, by=c('AGS'), all.x=TRUE)

# Einzelbewerber
brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2008_gemeinderatswahlen_data[brandenburg_2008_gemeinderatswahlen_data$`ArtdesWahl-vorschlags-trägers`=="EB"]
brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER) == "Stimmen"] <- "EINZELBEWERBER"
names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"] <- "sitze_EINZELBEWERBER"

brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER %>%
  group_by(AGS) %>%
  mutate(EINZELBEWERBER = sum(EINZELBEWERBER, na.rm=T),
         sitze_EINZELBEWERBER = sum(sitze_EINZELBEWERBER, na.rm = T)) %>%
  distinct()

brandenburg_2008_gemeinderatswahlen_data_recoded <- merge(brandenburg_2008_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_EINZELBEWERBER, by=c('AGS'), all.x=TRUE)

brandenburg_2008_gemeinderatswahlen_data_recoded <- as.data.table(brandenburg_2008_gemeinderatswahlen_data_recoded)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2008_gemeinderatswahlen_data_recoded[ , AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2008_gemeinderatswahlen_data_recoded[ , Bundesland := "Brandenburg"]
brandenburg_2008_gemeinderatswahlen_data_recoded[ , election_year := "2008"]
brandenburg_2008_gemeinderatswahlen_data_recoded[ , election_type := "Gemeinderatswahlen"]
brandenburg_2008_gemeinderatswahlen_data_recoded[ , IDIRB := ""]
brandenburg_2008_gemeinderatswahlen_data_recoded[ , IDBA := ""]

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
brandenburg_2008_gemeinderatswahlen_data_recoded <- brandenburg_2008_gemeinderatswahlen_data_recoded[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2008_gemeinderatswahlen_data_recoded <-
  brandenburg_2008_gemeinderatswahlen_data_recoded %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
brandenburg_2008_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2008_gemeinderatswahlen_data_recoded$Wähler / brandenburg_2008_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


###### Brandenburg 2014 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2014_gemeinderatswahlen_data <- as.data.table(read_excel("raw/brandenburg/brandenburg_2014.xlsx", sheet="Ergebnis"))
names(brandenburg_2014_gemeinderatswahlen_data) <-  str_replace_all(names(brandenburg_2014_gemeinderatswahlen_data), fixed(" "), "")

#### Recode to split by party vote ----
brandenburg_2014_gemeinderatswahlen_data_recoded <-
  brandenburg_2014_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(Wahlberechtigte=mean(Wahlberechtigte),
            Wähler=mean(Wähler),
            gültigeStimmen=mean(GültigeStimmen),
            Gebietsname=unique(Gemeindename))
brandenburg_2014_gemeinderatswahlen_data_recoded <- as.data.frame(brandenburg_2014_gemeinderatswahlen_data_recoded)

# CDU
brandenburg_2014_gemeinderatswahlen_data_CDU <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="CDU"]
brandenburg_2014_gemeinderatswahlen_data_CDU <- brandenburg_2014_gemeinderatswahlen_data_CDU[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_CDU)[names(brandenburg_2014_gemeinderatswahlen_data_CDU) == "Stimmen"] <- "CDU"
names(brandenburg_2014_gemeinderatswahlen_data_CDU)[names(brandenburg_2014_gemeinderatswahlen_data_CDU) == "Sitze"] <- "sitze_CDU"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_CDU, by=c('AGS'), all.x=TRUE)

# SPD
brandenburg_2014_gemeinderatswahlen_data_SPD <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="SPD"]
brandenburg_2014_gemeinderatswahlen_data_SPD <- brandenburg_2014_gemeinderatswahlen_data_SPD[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_SPD)[names(brandenburg_2014_gemeinderatswahlen_data_SPD) == "Stimmen"] <- "SPD"
names(brandenburg_2014_gemeinderatswahlen_data_SPD)[names(brandenburg_2014_gemeinderatswahlen_data_SPD) == "Sitze"] <- "sitze_SPD"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_SPD, by=c('AGS'), all.x=TRUE)

# FDP
brandenburg_2014_gemeinderatswahlen_data_FDP <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="FDP"]
brandenburg_2014_gemeinderatswahlen_data_FDP <- brandenburg_2014_gemeinderatswahlen_data_FDP[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_FDP)[names(brandenburg_2014_gemeinderatswahlen_data_FDP) == "Stimmen"] <- "FDP"
names(brandenburg_2014_gemeinderatswahlen_data_FDP)[names(brandenburg_2014_gemeinderatswahlen_data_FDP) == "Sitze"] <- "sitze_FDP"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_FDP, by=c('AGS'), all.x=TRUE)

# DiePARTEI
brandenburg_2014_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="Die PARTEI"]
brandenburg_2014_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2014_gemeinderatswahlen_data_DiePARTEI[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI)[names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI) == "Stimmen"] <- "DiePARTEI"
names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI)[names(brandenburg_2014_gemeinderatswahlen_data_DiePARTEI) == "Sitze"] <- "sitze_DiePARTEI"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_DiePARTEI, by=c('AGS'), all.x=TRUE)

# DIELINKE
brandenburg_2014_gemeinderatswahlen_data_DIELINKE <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="DIE LINKE"]
brandenburg_2014_gemeinderatswahlen_data_DIELINKE <- brandenburg_2014_gemeinderatswahlen_data_DIELINKE[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE) == "Stimmen"] <- "DIELINKE"
names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2014_gemeinderatswahlen_data_DIELINKE) == "Sitze"] <- "sitze_DIELINKE"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_DIELINKE, by=c('AGS'), all.x=TRUE)

# AfD
brandenburg_2014_gemeinderatswahlen_data_AfD <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="AfD"]
brandenburg_2014_gemeinderatswahlen_data_AfD <- brandenburg_2014_gemeinderatswahlen_data_AfD[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_AfD)[names(brandenburg_2014_gemeinderatswahlen_data_AfD) == "Stimmen"] <- "AfD"
names(brandenburg_2014_gemeinderatswahlen_data_AfD)[names(brandenburg_2014_gemeinderatswahlen_data_AfD) == "Sitze"] <- "sitze_AfD"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_AfD, by=c('AGS'), all.x=TRUE)

# PIRATEN
brandenburg_2014_gemeinderatswahlen_data_PIRATEN <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$Wahlvorschlagsträger_Kurz=="PIRATEN"]
brandenburg_2014_gemeinderatswahlen_data_PIRATEN <- brandenburg_2014_gemeinderatswahlen_data_PIRATEN[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN)[names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN) == "Stimmen"] <- "PIRATEN"
names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN)[names(brandenburg_2014_gemeinderatswahlen_data_PIRATEN) == "Sitze"] <- "sitze_PIRATEN"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_PIRATEN, by=c('AGS'), all.x=TRUE)

# GRUENE
brandenburg_2014_gemeinderatswahlen_data_GRUENE <- dplyr::filter(brandenburg_2014_gemeinderatswahlen_data, grepl('GRÜNEN', Wahlvorschlagsträger_Kurz))
brandenburg_2014_gemeinderatswahlen_data_GRUENE <- brandenburg_2014_gemeinderatswahlen_data_GRUENE[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2014_gemeinderatswahlen_data_GRUENE) == "Stimmen"] <- "GRUENE"
names(brandenburg_2014_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2014_gemeinderatswahlen_data_GRUENE) == "Sitze"] <- "sitze_GRUENE"
brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_GRUENE, by=c('AGS'), all.x=TRUE)

# FW
brandenburg_2014_gemeinderatswahlen_data_FW <- dplyr::filter(brandenburg_2014_gemeinderatswahlen_data, grepl('Freie Wähler|FW|Freie WG', Wahlvorschlagsträger_Kurz))
brandenburg_2014_gemeinderatswahlen_data_FW <- brandenburg_2014_gemeinderatswahlen_data_FW[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_FW)[names(brandenburg_2014_gemeinderatswahlen_data_FW) == "Stimmen"] <- "FREIEWÄHLER"
names(brandenburg_2014_gemeinderatswahlen_data_FW)[names(brandenburg_2014_gemeinderatswahlen_data_FW) == "Sitze"] <- "sitze_FREIEWÄHLER"

brandenburg_2014_gemeinderatswahlen_data_FW <- brandenburg_2014_gemeinderatswahlen_data_FW %>%
  group_by(AGS) %>%
  mutate(FREIEWÄHLER = sum(FREIEWÄHLER, na.rm=T),
         sitze_FREIEWÄHLER = sum(sitze_FREIEWÄHLER, na.rm = T)) %>%
  distinct()

brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_FW, by=c('AGS'), all.x=TRUE)

# Wählergruppen
brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$`ArtdesWahlvorschlags-trägers`=="WG"]
brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Stimmen"] <- "WAEHLERGRUPPEN"
names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"] <- "sitze_WAEHLERGRUPPEN"

brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN %>%
  group_by(AGS) %>%
  mutate(WAEHLERGRUPPEN = sum(WAEHLERGRUPPEN, na.rm=T),
         sitze_WAEHLERGRUPPEN = sum(sitze_WAEHLERGRUPPEN, na.rm = T)) %>%
  distinct()

brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_WAEHLERGRUPPEN, by=c('AGS'), all.x=TRUE)

# Einzelbewerber
brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2014_gemeinderatswahlen_data[brandenburg_2014_gemeinderatswahlen_data$`ArtdesWahlvorschlags-trägers`=="EB"]
brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER[,c('AGS', 'Stimmen', 'Sitze')]
names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER) == "Stimmen"] <- "EINZELBEWERBER"
names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"] <- "sitze_EINZELBEWERBER"

brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER %>%
  group_by(AGS) %>%
  mutate(EINZELBEWERBER = sum(EINZELBEWERBER, na.rm=T),
         sitze_EINZELBEWERBER = sum(sitze_EINZELBEWERBER, na.rm = T)) %>%
  distinct()

brandenburg_2014_gemeinderatswahlen_data_recoded <- merge(brandenburg_2014_gemeinderatswahlen_data_recoded,brandenburg_2014_gemeinderatswahlen_data_EINZELBEWERBER, by=c('AGS'), all.x=TRUE)

brandenburg_2014_gemeinderatswahlen_data_recoded <- as.data.table(brandenburg_2014_gemeinderatswahlen_data_recoded)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2014_gemeinderatswahlen_data_recoded[ , AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2014_gemeinderatswahlen_data_recoded[ , Bundesland := "Brandenburg"]
brandenburg_2014_gemeinderatswahlen_data_recoded[ , election_year := "2014"]
brandenburg_2014_gemeinderatswahlen_data_recoded[ , election_type := "Gemeinderatswahlen"]
brandenburg_2014_gemeinderatswahlen_data_recoded[ , IDIRB := ""]
brandenburg_2014_gemeinderatswahlen_data_recoded[ , IDBA := ""]

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
brandenburg_2014_gemeinderatswahlen_data_recoded <- brandenburg_2014_gemeinderatswahlen_data_recoded[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2014_gemeinderatswahlen_data_recoded <-
  brandenburg_2014_gemeinderatswahlen_data_recoded %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
brandenburg_2014_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2014_gemeinderatswahlen_data_recoded$Wähler / brandenburg_2014_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt

###### Brandenburg 2019 Gemeinderatswahlen ----
#### Load election data ----
brandenburg_2019_gemeinderatswahlen_data <- as.data.table(read_excel("raw/brandenburg/brandenburg_2019.xlsx", sheet="BB_GVW2019"))
names(brandenburg_2019_gemeinderatswahlen_data) <-  str_replace_all(names(brandenburg_2019_gemeinderatswahlen_data), fixed(" "), "")

#### Recode to split by party vote ----
brandenburg_2019_gemeinderatswahlen_data_recoded <-
  brandenburg_2019_gemeinderatswahlen_data %>%
  group_by(AGS) %>%
  summarise(Gebietsname=unique(Gemeindename))
brandenburg_2019_gemeinderatswahlen_data_recoded <- as.data.frame(brandenburg_2019_gemeinderatswahlen_data_recoded)

# Wahlberechtigte
brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="Wahlberechtigt"]
brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte <- brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte[,c('AGS', 'Anzahl')]
names(brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte)[names(brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte) == "Anzahl"] <- "Wahlberechtigte"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_Wahlberechtigte, by=c('AGS'), all.x=TRUE)

# Wähler
brandenburg_2019_gemeinderatswahlen_data_Wähler <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="Wähler"]
brandenburg_2019_gemeinderatswahlen_data_Wähler <- brandenburg_2019_gemeinderatswahlen_data_Wähler[,c('AGS', 'Anzahl')]
names(brandenburg_2019_gemeinderatswahlen_data_Wähler)[names(brandenburg_2019_gemeinderatswahlen_data_Wähler) == "Anzahl"] <- "Wähler"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_Wähler, by=c('AGS'), all.x=TRUE)

# gültigeStimmen
brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="Gueltig"]
brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen <- brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen[,c('AGS', 'Anzahl')]
names(brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen)[names(brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen) == "Anzahl"] <- "gültigeStimmen"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_gültigeStimmen, by=c('AGS'), all.x=TRUE)

# CDU
brandenburg_2019_gemeinderatswahlen_data_CDU <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="CDU"]
brandenburg_2019_gemeinderatswahlen_data_CDU <- brandenburg_2019_gemeinderatswahlen_data_CDU[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_CDU)[names(brandenburg_2019_gemeinderatswahlen_data_CDU) == "Anzahl"] <- "CDU"
names(brandenburg_2019_gemeinderatswahlen_data_CDU)[names(brandenburg_2019_gemeinderatswahlen_data_CDU) == "Sitze"] <- "sitze_CDU"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_CDU, by=c('AGS'), all.x=TRUE)

# SPD
brandenburg_2019_gemeinderatswahlen_data_SPD <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="SPD"]
brandenburg_2019_gemeinderatswahlen_data_SPD <- brandenburg_2019_gemeinderatswahlen_data_SPD[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_SPD)[names(brandenburg_2019_gemeinderatswahlen_data_SPD) == "Anzahl"] <- "SPD"
names(brandenburg_2019_gemeinderatswahlen_data_SPD)[names(brandenburg_2019_gemeinderatswahlen_data_SPD) == "Sitze"] <- "sitze_SPD"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_SPD, by=c('AGS'), all.x=TRUE)

# FDP
brandenburg_2019_gemeinderatswahlen_data_FDP <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="FDP"]
brandenburg_2019_gemeinderatswahlen_data_FDP <- brandenburg_2019_gemeinderatswahlen_data_FDP[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_FDP)[names(brandenburg_2019_gemeinderatswahlen_data_FDP) == "Anzahl"] <- "FDP"
names(brandenburg_2019_gemeinderatswahlen_data_FDP)[names(brandenburg_2019_gemeinderatswahlen_data_FDP) == "Sitze"] <- "sitze_FDP"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_FDP, by=c('AGS'), all.x=TRUE)

# DiePARTEI
brandenburg_2019_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="Die PARTEI"]
brandenburg_2019_gemeinderatswahlen_data_DiePARTEI <- brandenburg_2019_gemeinderatswahlen_data_DiePARTEI[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI)[names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI) == "Anzahl"] <- "DiePARTEI"
names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI)[names(brandenburg_2019_gemeinderatswahlen_data_DiePARTEI) == "Sitze"] <- "sitze_DiePARTEI"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_DiePARTEI, by=c('AGS'), all.x=TRUE)

# DIELINKE
brandenburg_2019_gemeinderatswahlen_data_DIELINKE <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="DIE LINKE"]
brandenburg_2019_gemeinderatswahlen_data_DIELINKE <- brandenburg_2019_gemeinderatswahlen_data_DIELINKE[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE) == "Anzahl"] <- "DIELINKE"
names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE)[names(brandenburg_2019_gemeinderatswahlen_data_DIELINKE) == "Sitze"] <- "sitze_DIELINKE"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_DIELINKE, by=c('AGS'), all.x=TRUE)

# AfD
brandenburg_2019_gemeinderatswahlen_data_AfD <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="AfD"]
brandenburg_2019_gemeinderatswahlen_data_AfD <- brandenburg_2019_gemeinderatswahlen_data_AfD[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_AfD)[names(brandenburg_2019_gemeinderatswahlen_data_AfD) == "Anzahl"] <- "AfD"
names(brandenburg_2019_gemeinderatswahlen_data_AfD)[names(brandenburg_2019_gemeinderatswahlen_data_AfD) == "Sitze"] <- "sitze_AfD"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_AfD, by=c('AGS'), all.x=TRUE)

# PIRATEN
brandenburg_2019_gemeinderatswahlen_data_PIRATEN <- brandenburg_2019_gemeinderatswahlen_data[brandenburg_2019_gemeinderatswahlen_data$Merkmal_Kurzname=="PIRATEN"]
brandenburg_2019_gemeinderatswahlen_data_PIRATEN <- brandenburg_2019_gemeinderatswahlen_data_PIRATEN[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN)[names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN) == "Anzahl"] <- "PIRATEN"
names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN)[names(brandenburg_2019_gemeinderatswahlen_data_PIRATEN) == "Sitze"] <- "sitze_PIRATEN"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_PIRATEN, by=c('AGS'), all.x=TRUE)

# GRUENE
brandenburg_2019_gemeinderatswahlen_data_GRUENE <- dplyr::filter(brandenburg_2019_gemeinderatswahlen_data, grepl('GRÜNE', Merkmal_Kurzname))
brandenburg_2019_gemeinderatswahlen_data_GRUENE <- brandenburg_2019_gemeinderatswahlen_data_GRUENE[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2019_gemeinderatswahlen_data_GRUENE) == "Anzahl"] <- "GRUENE"
names(brandenburg_2019_gemeinderatswahlen_data_GRUENE)[names(brandenburg_2019_gemeinderatswahlen_data_GRUENE) == "Sitze"] <- "sitze_GRUENE"
brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_GRUENE, by=c('AGS'), all.x=TRUE)

# FW
brandenburg_2019_gemeinderatswahlen_data_FW <- dplyr::filter(brandenburg_2019_gemeinderatswahlen_data, grepl('Freie Wähler|FW|Freie WG', Merkmal_Kurzname))
brandenburg_2019_gemeinderatswahlen_data_FW <- brandenburg_2019_gemeinderatswahlen_data_FW[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_FW)[names(brandenburg_2019_gemeinderatswahlen_data_FW) == "Anzahl"] <- "FREIEWÄHLER"
names(brandenburg_2019_gemeinderatswahlen_data_FW)[names(brandenburg_2019_gemeinderatswahlen_data_FW) == "Sitze"] <- "sitze_FREIEWÄHLER"

brandenburg_2019_gemeinderatswahlen_data_FW <- brandenburg_2019_gemeinderatswahlen_data_FW %>%
  group_by(AGS) %>%
  mutate(FREIEWÄHLER = sum(FREIEWÄHLER, na.rm=T),
         sitze_FREIEWÄHLER = sum(sitze_FREIEWÄHLER, na.rm = T)) %>%
  distinct()

brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_FW, by=c('AGS'), all.x=TRUE)

# Wählergruppen
brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN <- dplyr::filter(brandenburg_2019_gemeinderatswahlen_data, grepl('WG', `ArtdesWahl-vorschlags`))
brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Anzahl"] <- "WAEHLERGRUPPEN"
names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN)[names(brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN) == "Sitze"] <- "sitze_WAEHLERGRUPPEN"

brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN <- brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN %>%
  group_by(AGS) %>%
  mutate(WAEHLERGRUPPEN = sum(WAEHLERGRUPPEN, na.rm=T),
         sitze_WAEHLERGRUPPEN = sum(sitze_WAEHLERGRUPPEN, na.rm = T)) %>%
  distinct()

brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_WAEHLERGRUPPEN, by=c('AGS'), all.x=TRUE)

# Einzelbewerber
brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER <- dplyr::filter(brandenburg_2019_gemeinderatswahlen_data, grepl('EB', `ArtdesWahl-vorschlags`))
brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER[,c('AGS', 'Anzahl', 'Sitze')]
names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER) == "Anzahl"] <- "EINZELBEWERBER"
names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER)[names(brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER) == "Sitze"] <- "sitze_EINZELBEWERBER"

brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER <- brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER %>%
  group_by(AGS) %>%
  mutate(EINZELBEWERBER = sum(EINZELBEWERBER, na.rm=T),
         sitze_EINZELBEWERBER = sum(sitze_EINZELBEWERBER, na.rm = T)) %>%
  distinct()

brandenburg_2019_gemeinderatswahlen_data_recoded <- merge(brandenburg_2019_gemeinderatswahlen_data_recoded,brandenburg_2019_gemeinderatswahlen_data_EINZELBEWERBER, by=c('AGS'), all.x=TRUE)

brandenburg_2019_gemeinderatswahlen_data_recoded <- as.data.table(brandenburg_2019_gemeinderatswahlen_data_recoded)

#### Recoding ----
# Creating non-existing variables ----
brandenburg_2019_gemeinderatswahlen_data_recoded[ , AGS_8dig := ""] # 8 digits with leading zero
brandenburg_2019_gemeinderatswahlen_data_recoded[ , Bundesland := "Brandenburg"]
brandenburg_2019_gemeinderatswahlen_data_recoded[ , election_year := "2019"]
brandenburg_2019_gemeinderatswahlen_data_recoded[ , election_type := "Gemeinderatswahlen"]
brandenburg_2019_gemeinderatswahlen_data_recoded[ , IDIRB := ""]
brandenburg_2019_gemeinderatswahlen_data_recoded[ , IDBA := ""]

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
brandenburg_2019_gemeinderatswahlen_data_recoded <- brandenburg_2019_gemeinderatswahlen_data_recoded[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                                         Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                                         abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                                         gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                                         sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

brandenburg_2019_gemeinderatswahlen_data_recoded <-
  brandenburg_2019_gemeinderatswahlen_data_recoded %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
brandenburg_2019_gemeinderatswahlen_data_recoded$Turnout <- brandenburg_2019_gemeinderatswahlen_data_recoded$Wähler / brandenburg_2019_gemeinderatswahlen_data_recoded$Wahlberechtigteinsgesamt


####### Merge files and save overall output for Brandenburg ----
# Merge
brandenburg_kommunalwahlen <- rbind(brandenburg_1993_gemeinderatswahlen_data_recoded, brandenburg_1998_gemeinderatswahlen_data_recoded,
                                    brandenburg_2003_gemeinderatswahlen_data_recoded,brandenburg_2008_gemeinderatswahlen_data_recoded,
                                    brandenburg_2014_gemeinderatswahlen_data_recoded, brandenburg_2019_gemeinderatswahlen_data_recoded)

# Replace - with NA
brandenburg_kommunalwahlen[brandenburg_kommunalwahlen == "-"] <- NA

# Fix AGS
brandenburg_kommunalwahlen$AGS_8dig <- strtrim(brandenburg_kommunalwahlen$AGS_8dig, 8)

# Save
#write_csv(brandenburg_kommunalwahlen, here::here("output/brandenburg_kommunalwahlen.csv"))

brandenburg_kommunalwahlen %>%
  group_by(election_year) %>%
  summarize(mean = mean(prop_Gemeinsame_Wahlvorschläge, na.rm=T))

######### RLP ----
###### RLP 1994 Gemeinderatswahlen ----
#### Load election data ----
rlp_1994_gemeinderatswahlen_data <- as.data.table(read_excel("raw/rlp/rlp_1994_1999.xlsx", sheet="1994"))

#### Delete white space ----
names(rlp_1994_gemeinderatswahlen_data) <-  str_replace_all(names(rlp_1994_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
rlp_1994_gemeinderatswahlen_data_sub <- rlp_1994_gemeinderatswahlen_data

# Creating non-existing variables ----
rlp_1994_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
rlp_1994_gemeinderatswahlen_data_sub[ , Bundesland := "RLP"]
rlp_1994_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
rlp_1994_gemeinderatswahlen_data_sub[ , election_year := "1994"]
rlp_1994_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
rlp_1994_gemeinderatswahlen_data_sub[ , IDIRB := ""]
rlp_1994_gemeinderatswahlen_data_sub[ , IDBA := ""]


# Renaming existing variables ----
rlp_1994_gemeinderatswahlen_data_sub$AGS_8dig <- str_extract(rlp_1994_gemeinderatswahlen_data_sub$Gebiet, "\\d{8}")
rlp_1994_gemeinderatswahlen_data_sub$Gebietsname <- sub("^\\d{8}\\s+(.*)$", "\\1", rlp_1994_gemeinderatswahlen_data_sub$Gebiet)
rlp_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
rlp_1994_gemeinderatswahlen_data_sub$Wähler <- rlp_1994_gemeinderatswahlen_data_sub$Waehler
#rlp_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1994_gemeinderatswahlen_data_sub$GueltigeStimmen

rlp_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1994_gemeinderatswahlen_data_sub$gueltigeStimmen

rlp_1994_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(rlp_1994_gemeinderatswahlen_data_sub$CDU)
rlp_1994_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(rlp_1994_gemeinderatswahlen_data_sub$SPD)
rlp_1994_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(rlp_1994_gemeinderatswahlen_data_sub$DIELINKE)
rlp_1994_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(rlp_1994_gemeinderatswahlen_data_sub$Gruene)
rlp_1994_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(rlp_1994_gemeinderatswahlen_data_sub$fdp)
rlp_1994_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- NA
rlp_1994_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(rlp_1994_gemeinderatswahlen_data_sub$WG)


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
rlp_1994_gemeinderatswahlen_data_sub <- rlp_1994_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                 Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                 abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                 gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                 sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_1994_gemeinderatswahlen_data_sub <-
  rlp_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('gew')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("gew") & matches("X")), list(~paste(sub("gew_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))



# Calculating turnout ----
rlp_1994_gemeinderatswahlen_data_sub$Turnout <- rlp_1994_gemeinderatswahlen_data_sub$Wähler / rlp_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 1999 Gemeinderatswahlen ----
#### Load election data ----
rlp_1999_gemeinderatswahlen_data <- as.data.table(read_excel("raw/rlp/rlp_1994_1999.xlsx", sheet="1999"))

#### Delete white space ----
names(rlp_1999_gemeinderatswahlen_data) <-  str_replace_all(names(rlp_1999_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
rlp_1999_gemeinderatswahlen_data_sub <- rlp_1999_gemeinderatswahlen_data

# Creating non-existing variables ----
rlp_1999_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
rlp_1999_gemeinderatswahlen_data_sub[ , Bundesland := "RLP"]
rlp_1999_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
rlp_1999_gemeinderatswahlen_data_sub[ , election_year := "1999"]
rlp_1999_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
rlp_1999_gemeinderatswahlen_data_sub[ , IDIRB := ""]
rlp_1999_gemeinderatswahlen_data_sub[ , IDBA := ""]


# Renaming existing variables ----
rlp_1999_gemeinderatswahlen_data_sub$AGS_8dig <- str_extract(rlp_1999_gemeinderatswahlen_data_sub$Gebiet, "\\d{8}")
rlp_1999_gemeinderatswahlen_data_sub$Gebietsname <- sub("^\\d{8}\\s+(.*)$", "\\1", rlp_1999_gemeinderatswahlen_data_sub$Gebiet)
rlp_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt
rlp_1999_gemeinderatswahlen_data_sub$Wähler <- rlp_1999_gemeinderatswahlen_data_sub$Waehler
#rlp_1999_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1999_gemeinderatswahlen_data_sub$GueltigeStimmen

rlp_1999_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_1999_gemeinderatswahlen_data_sub$gueltigeStimmen

rlp_1999_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(rlp_1999_gemeinderatswahlen_data_sub$CDU)
rlp_1999_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(rlp_1999_gemeinderatswahlen_data_sub$SPD)
rlp_1999_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(rlp_1999_gemeinderatswahlen_data_sub$DIELINKE)
rlp_1999_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(rlp_1999_gemeinderatswahlen_data_sub$Gruene)
rlp_1999_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(rlp_1999_gemeinderatswahlen_data_sub$fdp)
rlp_1999_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- NA
rlp_1999_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(rlp_1999_gemeinderatswahlen_data_sub$WG)


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
rlp_1999_gemeinderatswahlen_data_sub <- rlp_1999_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                 Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                 abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                 gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                 sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_1999_gemeinderatswahlen_data_sub <-
  rlp_1999_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('gew')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("gew") & matches("X")), list(~paste(sub("gew_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))



# Calculating turnout ----
rlp_1999_gemeinderatswahlen_data_sub$Turnout <- rlp_1999_gemeinderatswahlen_data_sub$Wähler / rlp_1999_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### RLP 2004 Gemeinderatswahlen ----
#### Load election data ----
rlp_2004_gemeinderatswahlen_data <- as.data.table(read_excel("raw/rlp/rlp_2004.xlsx", sheet="summary"))

#### Delete white space ----
names(rlp_2004_gemeinderatswahlen_data) <-  str_replace_all(names(rlp_2004_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
rlp_2004_gemeinderatswahlen_data_sub <- rlp_2004_gemeinderatswahlen_data

names(rlp_2004_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2004_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
rlp_2004_gemeinderatswahlen_data_sub[ , Bundesland := "RLP"]
rlp_2004_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
rlp_2004_gemeinderatswahlen_data_sub[ , election_year := "2004"]
rlp_2004_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
rlp_2004_gemeinderatswahlen_data_sub[ , IDIRB := ""]
rlp_2004_gemeinderatswahlen_data_sub[ , IDBA := ""]


# Renaming existing variables ----
rlp_2004_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2004_gemeinderatswahlen_data_sub$AGS
rlp_2004_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2004_gemeinderatswahlen_data_sub$Gemeindename
rlp_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_2004_gemeinderatswahlen_data_sub$Wahlberechtigte
rlp_2004_gemeinderatswahlen_data_sub$Wähler <- rlp_2004_gemeinderatswahlen_data_sub$Waehler
#rlp_2004_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2004_gemeinderatswahlen_data_sub$GueltigeStimmen

rlp_2004_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2004_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2004_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$CDU_gew)
rlp_2004_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$SPD_gew)
rlp_2004_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$DIELINKE_gew)
rlp_2004_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$GRUENE_gew)
rlp_2004_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$FDP_gew)
rlp_2004_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2004_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew)
rlp_2004_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$Waehlergruppen_gew)


rlp_2004_gemeinderatswahlen_data_sub$gew_CDU <- rlp_2004_gemeinderatswahlen_data_sub$CDU_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_SPD <- rlp_2004_gemeinderatswahlen_data_sub$SPD_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_DIELINKE <- rlp_2004_gemeinderatswahlen_data_sub$DIELINKE_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_GRÜNE <- rlp_2004_gemeinderatswahlen_data_sub$GRUENE_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_AfD <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_FDP <- rlp_2004_gemeinderatswahlen_data_sub$FDP_gew
rlp_2004_gemeinderatswahlen_data_sub$gew_DiePARTEI <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- NA
rlp_2004_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew)
rlp_2004_gemeinderatswahlen_data_sub$gew_Wählergruppen <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$Waehlergruppen_gew)

rlp_2004_gemeinderatswahlen_data_sub$sitze_CDU <- rlp_2004_gemeinderatswahlen_data_sub$CDU_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_SPD <- rlp_2004_gemeinderatswahlen_data_sub$SPD_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_DIELINKE <- rlp_2004_gemeinderatswahlen_data_sub$DIELINKE_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_GRÜNE <- rlp_2004_gemeinderatswahlen_data_sub$GRUENE_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_AfD <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_FDP <- rlp_2004_gemeinderatswahlen_data_sub$FDP_sitze
rlp_2004_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- NA
rlp_2004_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze)
rlp_2004_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(rlp_2004_gemeinderatswahlen_data_sub$Waehlergruppen_sitze)


# Creating new dataframe with selected vars ----
rlp_2004_gemeinderatswahlen_data_sub <- rlp_2004_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                 Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                 abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                 gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                 sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2004_gemeinderatswahlen_data_sub <-
  rlp_2004_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('gew')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("gew") & matches("X")), list(~paste(sub("gew_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))



# Calculating turnout ----
rlp_2004_gemeinderatswahlen_data_sub$Turnout <- rlp_2004_gemeinderatswahlen_data_sub$Wähler / rlp_2004_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt



###### RLP 2009 Gemeinderatswahlen ----
#### Load election data ----
rlp_2009_gemeinderatswahlen_data <- as.data.table(read_excel("raw/rlp/rlp_2009.xlsx", sheet="summary"))

#### Delete white space ----
names(rlp_2009_gemeinderatswahlen_data) <-  str_replace_all(names(rlp_2009_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
rlp_2009_gemeinderatswahlen_data_sub <- rlp_2009_gemeinderatswahlen_data

names(rlp_2009_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2009_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
rlp_2009_gemeinderatswahlen_data_sub[ , Bundesland := "RLP"]
rlp_2009_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
rlp_2009_gemeinderatswahlen_data_sub[ , election_year := "2009"]
rlp_2009_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
rlp_2009_gemeinderatswahlen_data_sub[ , IDIRB := ""]
rlp_2009_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
rlp_2009_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2009_gemeinderatswahlen_data_sub$AGS
rlp_2009_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2009_gemeinderatswahlen_data_sub$Gemeindename
rlp_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- rlp_2009_gemeinderatswahlen_data_sub$Wahlberechtigte
rlp_2009_gemeinderatswahlen_data_sub$Wähler <- rlp_2009_gemeinderatswahlen_data_sub$Waehler
#rlp_2009_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2009_gemeinderatswahlen_data_sub$GueltigeStimmen

# Use Stimmzettel!
rlp_2009_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2009_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2009_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$CDU)
rlp_2009_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$SPD)
rlp_2009_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$DIELINKE)
rlp_2009_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$GRUENE)
rlp_2009_gemeinderatswahlen_data_sub$abs_AfD <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$FDP)
rlp_2009_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2009_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge)
rlp_2009_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$Waehlergruppen)

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
rlp_2009_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze)
rlp_2009_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(rlp_2009_gemeinderatswahlen_data_sub$Waehlergruppen_sitze)

# Creating new dataframe with selected vars ----
rlp_2009_gemeinderatswahlen_data_sub <- rlp_2009_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                 Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                 abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                 gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                 sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2009_gemeinderatswahlen_data_sub <-
  rlp_2009_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
rlp_2009_gemeinderatswahlen_data_sub$Turnout <- rlp_2009_gemeinderatswahlen_data_sub$Wähler / rlp_2009_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### RLP 2014 Gemeinderatswahlen ----
#### Load election data ----
rlp_2014_gemeinderatswahlen_data <- as.data.table(read_excel("raw/rlp/rlp_2014.xlsx", sheet="summary"))

#### Delete white space ----
names(rlp_2014_gemeinderatswahlen_data) <-  str_replace_all(names(rlp_2014_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
rlp_2014_gemeinderatswahlen_data_sub <- rlp_2014_gemeinderatswahlen_data

names(rlp_2014_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2014_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
rlp_2014_gemeinderatswahlen_data_sub[ , Bundesland := "RLP"]
rlp_2014_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
rlp_2014_gemeinderatswahlen_data_sub[ , election_year := "2014"]
rlp_2014_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
rlp_2014_gemeinderatswahlen_data_sub[ , IDIRB := ""]
rlp_2014_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
rlp_2014_gemeinderatswahlen_data_sub$AGS_8dig <- as.character(rlp_2014_gemeinderatswahlen_data_sub$AGS)
rlp_2014_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2014_gemeinderatswahlen_data_sub$Gemeindename
rlp_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$Wahlberechtigte)
rlp_2014_gemeinderatswahlen_data_sub$Wähler <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$Waehler)
rlp_2014_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2014_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2014_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$CDU)
rlp_2014_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$SPD)
rlp_2014_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$DIELINKE)
rlp_2014_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$GRUENE)
rlp_2014_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$AfD)
rlp_2014_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2014_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$FDP)
rlp_2014_gemeinderatswahlen_data_sub$abs_DiePARTEI <- NA
rlp_2014_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- NA
rlp_2014_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge)
rlp_2014_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$Waehlergruppen)

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
rlp_2014_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze)
rlp_2014_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(rlp_2014_gemeinderatswahlen_data_sub$Waehlergruppen_sitze)



# Creating new dataframe with selected vars ----
rlp_2014_gemeinderatswahlen_data_sub <- rlp_2014_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                 Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                 abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                 gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                 sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2014_gemeinderatswahlen_data_sub <-
  rlp_2014_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

rlp_2014_gemeinderatswahlen_data_sub[ AGS_8dig == "33205032" ]

# Calculating turnout ----
rlp_2014_gemeinderatswahlen_data_sub$Turnout <- rlp_2014_gemeinderatswahlen_data_sub$Wähler / rlp_2014_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


# Filter Landkreise ----
rlp_2014_gemeinderatswahlen_data_sub <- rlp_2014_gemeinderatswahlen_data_sub %>%
  filter(!Gebietsname %in% c("Rhein-Hunsrück-Kreis", "Ahrweiler, Landkreis"))

###### RLP 2019 Gemeinderatswahlen ----
#### Load election data ----
rlp_2019_gemeinderatswahlen_data <- as.data.table(read_excel("raw/rlp/rlp_2019.xlsx", sheet="summary"))

#### Delete white space ----
names(rlp_2019_gemeinderatswahlen_data) <-  str_replace_all(names(rlp_2019_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
rlp_2019_gemeinderatswahlen_data_sub <- rlp_2019_gemeinderatswahlen_data

names(rlp_2019_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
rlp_2019_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
rlp_2019_gemeinderatswahlen_data_sub[ , Bundesland := "RLP"]
rlp_2019_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
rlp_2019_gemeinderatswahlen_data_sub[ , election_year := "2019"]
rlp_2019_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
rlp_2019_gemeinderatswahlen_data_sub[ , IDIRB := ""]
rlp_2019_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
rlp_2019_gemeinderatswahlen_data_sub$AGS_8dig <- rlp_2019_gemeinderatswahlen_data_sub$AGS
rlp_2019_gemeinderatswahlen_data_sub$Gebietsname <- rlp_2019_gemeinderatswahlen_data_sub$Gemeindename
rlp_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Wahlberechtigte)
rlp_2019_gemeinderatswahlen_data_sub$Wähler <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Waehler)
rlp_2019_gemeinderatswahlen_data_sub$GültigeStimmen <- rlp_2019_gemeinderatswahlen_data_sub$GueltigeStimmzettel

rlp_2019_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$CDU_gew)
rlp_2019_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$SPDgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_DIELINKE <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$DIELINKEgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$GRUENEgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_AfD <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$AfDgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
rlp_2019_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$FDPgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_DiePARTEI <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$DiePARTEIgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_FREIEWÄHLER <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$FREIEWAEHLERgew)
rlp_2019_gemeinderatswahlen_data_sub$abs_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschlägegew)
rlp_2019_gemeinderatswahlen_data_sub$abs_Wählergruppen <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Waehlergruppengew)

rlp_2019_gemeinderatswahlen_data_sub$gew_CDU <- rlp_2019_gemeinderatswahlen_data_sub$CDU_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_SPD <- rlp_2019_gemeinderatswahlen_data_sub$SPD_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_DIELINKE <- rlp_2019_gemeinderatswahlen_data_sub$DIELINKE_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_GRÜNE <- rlp_2019_gemeinderatswahlen_data_sub$GRUENE_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_AfD <- rlp_2019_gemeinderatswahlen_data_sub$AfD_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_PIRATEN <- NA
rlp_2019_gemeinderatswahlen_data_sub$gew_FDP <- rlp_2019_gemeinderatswahlen_data_sub$FDP_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_DiePARTEI <- rlp_2019_gemeinderatswahlen_data_sub$DiePARTEI_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_FREIEWÄHLER <- rlp_2019_gemeinderatswahlen_data_sub$FREIEWAEHLER_gew
rlp_2019_gemeinderatswahlen_data_sub$gew_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_gew)
rlp_2019_gemeinderatswahlen_data_sub$gew_Wählergruppen <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Waehlergruppen_gew)

rlp_2019_gemeinderatswahlen_data_sub$sitze_CDU <- rlp_2019_gemeinderatswahlen_data_sub$CDU_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_SPD <- rlp_2019_gemeinderatswahlen_data_sub$SPD_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_DIELINKE <- rlp_2019_gemeinderatswahlen_data_sub$DIELINKE_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_GRÜNE <- rlp_2019_gemeinderatswahlen_data_sub$GRUENE_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_AfD <- rlp_2019_gemeinderatswahlen_data_sub$AfD_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_PIRATEN <- NA
rlp_2019_gemeinderatswahlen_data_sub$sitze_FDP <- rlp_2019_gemeinderatswahlen_data_sub$FDP_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- rlp_2019_gemeinderatswahlen_data_sub$DiePARTEI_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- rlp_2019_gemeinderatswahlen_data_sub$FREIEWAEHLER_sitze
rlp_2019_gemeinderatswahlen_data_sub$sitze_Gemeinsame_Wahlvorschläge <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Gemeinsame_Wahlvorschläge_sitze)
rlp_2019_gemeinderatswahlen_data_sub$sitze_Wählergruppen <- as.numeric(rlp_2019_gemeinderatswahlen_data_sub$Waehlergruppen_sitze)


# Creating new dataframe with selected vars ----
rlp_2019_gemeinderatswahlen_data_sub <- rlp_2019_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                                 Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                                 abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_FREIEWÄHLER, abs_Gemeinsame_Wahlvorschläge, abs_Wählergruppen,
                                                                                 gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_FREIEWÄHLER, gew_Gemeinsame_Wahlvorschläge, gew_Wählergruppen,
                                                                                 sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_FREIEWÄHLER, sitze_Gemeinsame_Wahlvorschläge, sitze_Wählergruppen)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

rlp_2019_gemeinderatswahlen_data_sub <-
  rlp_2019_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('gew')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("gew") & matches("X")), list(~paste(sub("gew_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
rlp_2019_gemeinderatswahlen_data_sub$Turnout <- rlp_2019_gemeinderatswahlen_data_sub$Wähler / rlp_2019_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


####### Merge files and save overall output for RLP ----
# Merge
rlp_kommunalwahlen <- rbind(rlp_1994_gemeinderatswahlen_data_sub, rlp_1999_gemeinderatswahlen_data_sub,
                            rlp_2004_gemeinderatswahlen_data_sub, rlp_2009_gemeinderatswahlen_data_sub,
                            rlp_2014_gemeinderatswahlen_data_sub, rlp_2019_gemeinderatswahlen_data_sub)

# Replace - with NA
rlp_kommunalwahlen[rlp_kommunalwahlen == "-"] <- NA


# Fix AGS for RLP ----
rlp_kommunalwahlen$AGS_8dig <- paste0("07", rlp_kommunalwahlen$AGS_8dig)

rlp_kommunalwahlen <- rlp_kommunalwahlen %>%
  filter(nchar(AGS_8dig)==10) %>%
  mutate(AGS_8dig = paste0(substr(AGS_8dig,1,5), substr(AGS_8dig,8,10)))

# Remove LK and VG ----
rlp_kommunalwahlen <- rlp_kommunalwahlen %>%
  filter(
    !grepl(", LK", Gebietsname),
    !grepl(", VG", Gebietsname),
    !grepl(", Landkreis", Gebietsname),
    !AGS_8dig %in% c("07141000", "07143000", "07232000", "07333000", "07338000", "07140000"))


# Save
#write_csv(rlp_kommunalwahlen, "processed/rlp_kommunalwahlen.csv")


######### Schleswig-Holstein ----
###### SH 2018 Gemeinderatswahlen ----
#### Load election data ----

sh_2018_gemeinderatswahlen_data <- as.data.table(read_excel("raw/schleswig_holstein/sh_2018.xlsx", sheet="summary"))

#### Delete white space ----
names(sh_2018_gemeinderatswahlen_data) <-  str_replace_all(names(sh_2018_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sh_2018_gemeinderatswahlen_data_sub <- sh_2018_gemeinderatswahlen_data

names(sh_2018_gemeinderatswahlen_data)


# Creating non-existing variables ----
sh_2018_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sh_2018_gemeinderatswahlen_data_sub[ , Bundesland := "Schleswig-Holstein"]
sh_2018_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sh_2018_gemeinderatswahlen_data_sub[ , election_year := "2018"]
sh_2018_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sh_2018_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sh_2018_gemeinderatswahlen_data_sub[ , IDBA := ""]

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

sh_2018_gemeinderatswahlen_data_sub$sitze_CDU <- sh_2018_gemeinderatswahlen_data_sub$CDU_maenner + sh_2018_gemeinderatswahlen_data_sub$CDU_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_SPD <- sh_2018_gemeinderatswahlen_data_sub$SPD_maenner + sh_2018_gemeinderatswahlen_data_sub$SPD_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_DIELINKE <- sh_2018_gemeinderatswahlen_data_sub$DIELINKE_maenner + sh_2018_gemeinderatswahlen_data_sub$DIELINKE_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_GRÜNE <- sh_2018_gemeinderatswahlen_data_sub$Gruene_maenner + sh_2018_gemeinderatswahlen_data_sub$Gruene_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_AfD <- sh_2018_gemeinderatswahlen_data_sub$AfD_maenner + sh_2018_gemeinderatswahlen_data_sub$AfD_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_PIRATEN <- sh_2018_gemeinderatswahlen_data_sub$PIRATEN_maenner + sh_2018_gemeinderatswahlen_data_sub$PIRATEN_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_FDP <- sh_2018_gemeinderatswahlen_data_sub$FDP_maenner + sh_2018_gemeinderatswahlen_data_sub$FDP_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_DiePARTEI <- sh_2018_gemeinderatswahlen_data_sub$DiePARTEI_maenner + sh_2018_gemeinderatswahlen_data_sub$DiePARTEI_frauen
sh_2018_gemeinderatswahlen_data_sub$sitze_FREIEWÄHLER <- sh_2018_gemeinderatswahlen_data_sub$FREIEWAEHLER_maenner + sh_2018_gemeinderatswahlen_data_sub$FREIEWAEHLER_frauen

# Creating new dataframe with selected vars ----
sh_2018_gemeinderatswahlen_data_sub <- sh_2018_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2018_gemeinderatswahlen_data_sub <-
  sh_2018_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sh_2018_gemeinderatswahlen_data_sub$Turnout <- sh_2018_gemeinderatswahlen_data_sub$Wähler / sh_2018_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 2013 Gemeinderatswahlen ----
#### Load election data ----

sh_2013_gemeinderatswahlen_data <- as.data.table(read_excel("raw/schleswig_holstein/sh_2013.xlsx", sheet="summary"))

#### Delete white space ----
names(sh_2013_gemeinderatswahlen_data) <-  str_replace_all(names(sh_2013_gemeinderatswahlen_data), fixed(" "), "")

#### Recoding ----
# Create new dataframe ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data

# Create corrected AGS ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data_sub %>%
  mutate(AGS_8dig = paste0("010", substr(GemeindekennziffermitWahlbezirk, 1, nchar(GemeindekennziffermitWahlbezirk) - 3)))

# Summarize by Gemeinde ----
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data_sub %>%
  group_by(Gemeindename, AGS_8dig) %>%
  summarise(
    Wahlberechtigte = sum(Wahlberechtigte, na.rm=T),
    Wählerinsgesamt = sum(Wählerinsgesamt, na.rm=T),
    gültigeStimmen = sum(gültigeStimmen, na.rm=T),
    CDU = sum(CDU, na.rm=T),
    SPD = sum(SPD, na.rm=T),
    GRÜNE = sum(GRÜNE, na.rm=T),
    FDP = sum(FDP, na.rm=T),
    PIRATEN = sum(PIRATEN, na.rm=T),
    DIELINKE = sum(DIELINKE, na.rm=T),
    NPD = sum(NPD, na.rm=T),
    FREIEWÄHLER = sum(FREIEWÄHLER, na.rm=T),
    DiePARTEI = sum(DiePARTEI, na.rm=T)) %>%
  ungroup()

sh_2013_gemeinderatswahlen_data_sub <- as.data.table(sh_2013_gemeinderatswahlen_data_sub)

# Creating non-existing variables ----
sh_2013_gemeinderatswahlen_data_sub$Bundesland <- "Schleswig-Holstein"
sh_2013_gemeinderatswahlen_data_sub$Gebietsname <- ""
sh_2013_gemeinderatswahlen_data_sub$election_year <-  "2013"
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
sh_2013_gemeinderatswahlen_data_sub <- sh_2013_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]
# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2013_gemeinderatswahlen_data_sub <-
  sh_2013_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sh_2013_gemeinderatswahlen_data_sub$Turnout <- sh_2013_gemeinderatswahlen_data_sub$Wähler / sh_2013_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt

###### SH 2008 Gemeinderatswahlen ----
#### Load election data ----

sh_2008_gemeinderatswahlen_data <- as.data.table(read_excel("raw/schleswig_holstein/sh_2008.xls", sheet="summary"))

#### Summarize data to fix poor data structure ----
sh_2008_gemeinderatswahlen_data_sub <-
  sh_2008_gemeinderatswahlen_data %>%
  group_by(Gemeindename) %>%
  summarise(Wahlberechtigte=sum(Wahlberechtigteinsgesamt),
            Wähler=sum(Waehler),
            gültigeStimmen=sum(gueltigeStimmen),
            CDU=sum(CDU),
            SPD=sum(SPD),
            Gruene=sum(Gruene),
            FDP=sum(FDP),
            DIELINKE=sum(DIELINKE))
sh_2008_gemeinderatswahlen_data_sub <- as.data.table(sh_2008_gemeinderatswahlen_data_sub)

#### Recoding ----
# Creating non-existing variables ----
sh_2008_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sh_2008_gemeinderatswahlen_data_sub[ , Bundesland := "Schleswig-Holstein"]
sh_2008_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sh_2008_gemeinderatswahlen_data_sub[ , election_year := "2008"]
sh_2008_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sh_2008_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sh_2008_gemeinderatswahlen_data_sub[ , IDBA := ""]



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

# Merge in AGS from 2018 data ----
sh_AGS_key <- sh_2018_gemeinderatswahlen_data %>%
  group_by(Gemeindename) %>%
  summarise(AGS_merged=unique(Gemeindekennziffer))
sh_AGS_key

sh_2008_gemeinderatswahlen_data_sub <- merge(sh_2008_gemeinderatswahlen_data_sub, sh_AGS_key, by.x="Gebietsname", by.y="Gemeindename")

sh_2008_gemeinderatswahlen_data_sub$AGS_8dig <- sh_2008_gemeinderatswahlen_data_sub$AGS_merged

# Creating new dataframe with selected vars ----
sh_2008_gemeinderatswahlen_data_sub <- sh_2008_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2008_gemeinderatswahlen_data_sub <-
  sh_2008_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))

# Calculating turnout ----
sh_2008_gemeinderatswahlen_data_sub$Turnout <- sh_2008_gemeinderatswahlen_data_sub$Wähler / sh_2008_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 2003 Gemeinderatswahlen ----
#### Load election data ----

sh_2003_gemeinderatswahlen_data <- as.data.table(read_excel("raw/schleswig_holstein/sh_2003.xlsx", sheet="summary"))
sh_2003_gemeinderatswahlen_data_sub <- sh_2003_gemeinderatswahlen_data

#### Recoding ----
# Creating non-existing variables ----
sh_2003_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sh_2003_gemeinderatswahlen_data_sub[ , Bundesland := "Schleswig-Holstein"]
sh_2003_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sh_2003_gemeinderatswahlen_data_sub[ , election_year := "2003"]
sh_2003_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sh_2003_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sh_2003_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sh_2003_gemeinderatswahlen_data_sub$Gebietsname <- sh_2003_gemeinderatswahlen_data_sub$Gemeindename
sh_2003_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt)
sh_2003_gemeinderatswahlen_data_sub$Wähler <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$Waehler)
sh_2003_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$gueltigeStimmen)

sh_2003_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$CDU)
sh_2003_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$SPD)
sh_2003_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$Gruene)
sh_2003_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_2003_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(sh_2003_gemeinderatswahlen_data_sub$FDP)
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
    AGS_8dig = paste0("01 0", Gemeindekennziffer)) %>%
  mutate(
    AGS_8dig = str_replace(AGS_8dig, " ", ""),
    AGS_8dig = str_replace(AGS_8dig, " ", ""))

# Creating new dataframe with selected vars ----
sh_2003_gemeinderatswahlen_data_sub <- sh_2003_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_2003_gemeinderatswahlen_data_sub <-
  sh_2003_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))


# Calculating turnout ----
sh_2003_gemeinderatswahlen_data_sub$Turnout <- sh_2003_gemeinderatswahlen_data_sub$Wähler / sh_2003_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt


###### SH 1998 Gemeinderatswahlen ----
#### Load election data ----

sh_1998_gemeinderatswahlen_data <- as.data.table(read_excel("raw/schleswig_holstein/sh_1998.xlsx", sheet="summary"))
sh_1998_gemeinderatswahlen_data_sub <- sh_1998_gemeinderatswahlen_data

#### Recoding ----
# Creating non-existing variables ----
sh_1998_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sh_1998_gemeinderatswahlen_data_sub[ , Bundesland := "Schleswig-Holstein"]
sh_1998_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sh_1998_gemeinderatswahlen_data_sub[ , election_year := "1998"]
sh_1998_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sh_1998_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sh_1998_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sh_1998_gemeinderatswahlen_data_sub$Gebietsname <- sh_1998_gemeinderatswahlen_data_sub$Gemeindename
sh_1998_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt)
sh_1998_gemeinderatswahlen_data_sub$Wähler <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$Waehler)
sh_1998_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$gueltigeStimmen)

sh_1998_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$CDU)
sh_1998_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$SPD)
sh_1998_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$Gruene)
sh_1998_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_1998_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(sh_1998_gemeinderatswahlen_data_sub$FDP)
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
    AGS_8dig = paste0("01 0", Gemeindekennziffer)) %>%
  mutate(
    AGS_8dig = str_replace(AGS_8dig, " ", ""),
    AGS_8dig = str_replace(AGS_8dig, " ", ""))

# Creating new dataframe with selected vars ----
sh_1998_gemeinderatswahlen_data_sub <- sh_1998_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_1998_gemeinderatswahlen_data_sub <-
  sh_1998_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))


# Calculating turnout ----
sh_1998_gemeinderatswahlen_data_sub$Turnout <- sh_1998_gemeinderatswahlen_data_sub$Wähler / sh_1998_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt



###### SH 1994 Gemeinderatswahlen ----
#### Load election data ----

sh_1994_gemeinderatswahlen_data <- as.data.table(read_excel("raw/schleswig_holstein/sh_1994.xlsx", sheet="summary"))
sh_1994_gemeinderatswahlen_data_sub <- sh_1994_gemeinderatswahlen_data

#### Recoding ----
# Creating non-existing variables ----
sh_1994_gemeinderatswahlen_data_sub[ , AGS_8dig := ""] # 8 digits with leading zero
sh_1994_gemeinderatswahlen_data_sub[ , Bundesland := "Schleswig-Holstein"]
sh_1994_gemeinderatswahlen_data_sub[ , Gebietsname := ""]
sh_1994_gemeinderatswahlen_data_sub[ , election_year := "1994"]
sh_1994_gemeinderatswahlen_data_sub[ , election_type := "Gemeinderatswahlen"]
sh_1994_gemeinderatswahlen_data_sub[ , IDIRB := ""]
sh_1994_gemeinderatswahlen_data_sub[ , IDBA := ""]

# Renaming existing variables ----
sh_1994_gemeinderatswahlen_data_sub$Gebietsname <- sh_1994_gemeinderatswahlen_data_sub$Gemeindename
sh_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt)
sh_1994_gemeinderatswahlen_data_sub$Wähler <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$Waehler)
sh_1994_gemeinderatswahlen_data_sub$GültigeStimmen <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$gueltigeStimmen)

sh_1994_gemeinderatswahlen_data_sub$abs_CDU <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$CDU)
sh_1994_gemeinderatswahlen_data_sub$abs_SPD <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$SPD)
sh_1994_gemeinderatswahlen_data_sub$abs_DIELINKE <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_GRÜNE <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$Gruene)
sh_1994_gemeinderatswahlen_data_sub$abs_AfD <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_PIRATEN <- NA
sh_1994_gemeinderatswahlen_data_sub$abs_FDP <- as.numeric(sh_1994_gemeinderatswahlen_data_sub$FDP)
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
    AGS_8dig = paste0("01 0", Gemeindekennziffer)) %>%
  mutate(
    AGS_8dig = str_replace(AGS_8dig, " ", ""),
    AGS_8dig = str_replace(AGS_8dig, " ", ""))

# Creating new dataframe with selected vars ----
sh_1994_gemeinderatswahlen_data_sub <- sh_1994_gemeinderatswahlen_data_sub[ ,.(AGS_8dig, Bundesland, Gebietsname, election_year, election_type, IDIRB, IDBA,
                                                                               Wahlberechtigteinsgesamt, Wähler, GültigeStimmen,
                                                                               abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER,
                                                                               gew_CDU, gew_SPD, gew_DIELINKE, gew_GRÜNE, gew_AfD, gew_PIRATEN, gew_FDP, gew_DiePARTEI, gew_FREIEWÄHLER,
                                                                               sitze_CDU, sitze_SPD, sitze_DIELINKE, sitze_GRÜNE, sitze_AfD, sitze_PIRATEN, sitze_FDP, sitze_DiePARTEI, sitze_FREIEWÄHLER)]

# Calculating vote shares ----
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones

sh_1994_gemeinderatswahlen_data_sub <-
  sh_1994_gemeinderatswahlen_data_sub %>%
  mutate_at(vars(contains('abs')), .funs = list(XXX= ~./as.numeric(GültigeStimmen))) %>%
  rename_at(vars(matches("abs") & matches("X")), list(~paste(sub("abs_","prop_",.), sep = "_"))) %>%
  rename_at(vars(matches("_XXX")), list(~paste(sub("_XXX","",.), sep = "")))


# Calculating turnout ----
sh_1994_gemeinderatswahlen_data_sub$Turnout <- sh_1994_gemeinderatswahlen_data_sub$Wähler / sh_1994_gemeinderatswahlen_data_sub$Wahlberechtigteinsgesamt



####### Merge files and save overall output for SH ----
# Merge
sh_kommunalwahlen <- rbind(sh_1994_gemeinderatswahlen_data_sub, sh_1998_gemeinderatswahlen_data_sub,
                           sh_2003_gemeinderatswahlen_data_sub, sh_2008_gemeinderatswahlen_data_sub,
                           sh_2013_gemeinderatswahlen_data_sub, sh_2018_gemeinderatswahlen_data_sub)

# Replace - with NA
sh_kommunalwahlen[sh_kommunalwahlen == "-"] <- NA

# Fix AGS
sh_kommunalwahlen$AGS_8dig <- stri_pad_left(sh_kommunalwahlen$AGS_8dig, 8, 0)

# Save
write_csv(sh_kommunalwahlen, "processed/sh_kommunalwahlen.csv")


# ----
# ----
# ----
########## MERGE FINAL FILES ----

# Merge ----
kommunalwahlen_merge <- rbind(baden_wuerttemberg_kommunalwahlen, bayern_kommunalwahlen,
                           berlin_kommunalwahlen, brandenburg_kommunalwahlen,
                           bremen_overall_buergerschaftswahl_data_sub, hamburg_kommunalwahlen,
                          hessen_kommunalwahlen, mecklenburg_vorpommern_kommunalwahlen,
                          niedersachsen_kommunalwahlen, nrw_kommunalwahlen,
                          rlp_kommunalwahlen, saarland_kommunalwahlen_data_sub,
                          sachsen_anhalt_kommunalwahlen, sachsen_kommunalwahlen,
                          sh_kommunalwahlen, thueringen_kommunalwahlen, fill=TRUE)
              
# Fix AGS ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(AGS_8dig = if_else(str_length(AGS_8dig) == 7,
                            str_pad(AGS_8dig, width = 8, side = "left", pad = "0"),
                            AGS_8dig)) %>%
  filter(!str_length(AGS_8dig) %in% c(10, 4, 5)) %>%
  mutate(AGS_8dig = if_else(AGS_8dig=="04", "04011000", AGS_8dig)) %>%
  filter(!is.na(AGS_8dig))


# Change FW and remainder category ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  rowwise%>%
  mutate(
    abs_OTHER = (GültigeStimmen-sum(abs_CDU, abs_SPD, abs_DIELINKE, abs_GRÜNE, abs_AfD, abs_PIRATEN, abs_FDP, abs_DiePARTEI, abs_FREIEWÄHLER, na.rm=T)),
    prop_OTHER = (1-sum(prop_CDU, prop_SPD, prop_DIELINKE, prop_GRÜNE, prop_AfD, prop_PIRATEN, prop_FDP, prop_DiePARTEI, prop_FREIEWÄHLER, na.rm=T))) %>%
  dplyr::select(AGS_8dig:abs_FREIEWÄHLER, abs_OTHER, gew_CDU:prop_FREIEWÄHLER, prop_OTHER, Turnout) %>%
  ungroup()


# Change vote_share to NA where == 0, create indicator variables to show where this is the case ----
kommunalwahlen_merge <- kommunalwahlen_merge %>%
  mutate(
    replaced_0_with_NA_CDU = case_when(
      abs_CDU == 0 ~ 1,
      abs_CDU != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_SPD = case_when(
      abs_SPD == 0 ~ 1,
      abs_SPD != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_DIELINKE = case_when(
      abs_DIELINKE == 0 ~ 1,
      abs_DIELINKE != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_GRÜNE = case_when(
      abs_GRÜNE == 0 ~ 1,
      abs_GRÜNE != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_AfD = case_when(
      abs_AfD == 0 ~ 1,
      abs_AfD != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_PIRATEN = case_when(
      abs_PIRATEN == 0 ~ 1,
      abs_PIRATEN != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_FDP = case_when(
      abs_FDP == 0 ~ 1,
      abs_FDP != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_DiePARTEI = case_when(
      abs_DiePARTEI == 0 ~ 1,
      abs_DiePARTEI != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_FDP = case_when(
      abs_FDP == 0 ~ 1,
      abs_FDP != 0 ~ 0,
      TRUE ~ 0),
    replaced_0_with_NA_FREIEWÄHLER = case_when(
      abs_FREIEWÄHLER == 0 ~ 1,
      abs_FREIEWÄHLER != 0 ~ 0,
      TRUE ~ 0))

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
    abs_DiePARTEI = ifelse(replaced_0_with_NA_DiePARTEI == 1, NA, abs_DiePARTEI),
    prop_DiePARTEI = ifelse(replaced_0_with_NA_DiePARTEI == 1, NA, prop_DiePARTEI),
    abs_FREIEWÄHLER = ifelse(replaced_0_with_NA_FREIEWÄHLER == 1, NA, abs_FREIEWÄHLER),
    prop_FREIEWÄHLER = ifelse(replaced_0_with_NA_FREIEWÄHLER == 1, NA, prop_FREIEWÄHLER))

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
    prop_FREIEWÄHLER = ifelse(abs_FREIEWÄHLER == 0, NA, prop_FREIEWÄHLER))

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
  select(-c(IDIRB,IDBA)) |>
  mutate(
    turnout = as.numeric(turnout),
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
  rename_with(~str_replace(., "sitze_", "seats_"), starts_with("sitze_")) |>
  rename_with(~str_replace(., "gew_", "weighted_"), starts_with("gew_")) |>
  # all variable names to lower
  rename_with(tolower, everything())

glimpse(kommunalwahlen_merge)



# Reduce to prop_ only ----------------------------------------------------

kommunalwahlen_merge <- kommunalwahlen_merge |>
  select(
    ags, ags_name, state, election_year, election_type, eligible_voters, number_voters, valid_votes, turnout,
    starts_with("prop_"), starts_with("replaced")
  )

glimpse(kommunalwahlen_merge)

# change names
kommunalwahlen_merge <- kommunalwahlen_merge |>
  rename_with(~str_replace(., "prop_", ""), starts_with("prop_")) |>
  rename_with(~str_replace(., "cdu", "cdu_csu")) |>
  rename_with(~str_replace(., "diepartei", "die_partei")) |>
  rename_with(~str_replace(., "freiewähler", "freie_wahler")) |>
  rename_with(~str_replace(., "grüne", "gruene")) |>
  rename_with(~str_replace(., "dielinke", "linke_pds"))

# Save ----

write_rds(kommunalwahlen_merge, file=here::here("data/municipal_elections/final/municipal_unharm.rds"))
fwrite(kommunalwahlen_merge, file=here::here("data/municipal_elections/final/municipal_unharm.csv"))

# View(kommunalwahlen_merge)


# inspect -----------------------------------------------------------------

kommunalwahlen_merge <- read_rds(here::here("data/municipal_elections/final/municipal_unharm.rds"))

glimpse(kommunalwahlen_merge)

test <- kommunalwahlen_merge %>%
  filter(ags== "07133080", election_year=="2019")  


### END