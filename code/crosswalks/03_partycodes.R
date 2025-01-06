# Create party crosswalks to other datasets
# Vincent Heddesheimer
# 2024-12-10

# Load our most comprehensive party data
df <- read_rds("data/federal_elections/municipality_level/final/federal_muni_raw.rds")
df_harm <- read_rds("data/federal_elections/municipality_level/final/federal_muni_harm.rds")


glimpse(df)
glimpse(df_harm)

party_raw = df %>% select(cdu:zentrum) %>% colnames()
party_harm = df_harm %>% select(cdu:zentrum) %>% colnames()

# Save parties in a dataframe: get all parties from party_raw and party_harm
# Make sure that there are no duplicates
parties <- tibble(
    party = c(party_raw, party_harm) %>% unique()
)

# ParlGov data ------------------------------------------------------------

# Get it from harvard dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC#:~:text=Access%20File-,view_party.tab,-Tabular%20Data%20%2D%20276.7
pacman::p_load(dataverse)

parlgov <- get_dataframe_by_name(
  filename = "view_party.tab",
  dataset = "10.7910/DVN/2VZ5ZC", 
  server = "dataverse.harvard.edu"
  )

glimpse(parlgov)

parlgov_ger <- parlgov %>% 
  dplyr::filter(country_name == "Germany") %>%
  select(-c(country_name_short, country_name)) %>% 
  distinct()

glimpse(parlgov_ger)



# Build crosswalk --------------------------------------------------------

print(parties %>% arrange(party), n = 120)

print(parlgov_ger %>% distinct(party_name_ascii) %>% arrange(party_name_ascii), n = 120)

# Create crosswalk with all original parties, matching to ParlGov where possible
crosswalk <- tibble(
  party_gerda = parties$party,
  party_name_ascii = case_when(
    party_gerda == "afd" ~ "Alternative fuer Deutschland",
    party_gerda %in% c("b90_gr", "gruene", "grune") ~ "Buendnis 90 / Die Gruenen",
    party_gerda %in% c("die_linke", "linke_pds", "pds") ~ "PDS | Die Linke",
    party_gerda == "fdp" ~ "Freie Demokratische Partei",
    party_gerda == "cdu" ~ "Christlich Demokratische Union",
    party_gerda == "csu" ~ "Christlich Soziale Union",
    party_gerda == "npd" ~ "Nationaldemokratische Partei Deutschlands",
    party_gerda == "piraten" ~ "Piratenpartei Deutschland",
    party_gerda == "spd" ~ "Sozialdemokratische Partei Deutschlands",
    party_gerda == "ssw" ~ "Suedschleswigscher Waehlerverband",
    party_gerda %in% c("tierschutzpartei", "tierschutz", "die_tierschutzpartei") ~ "Tierschutzpartei",
    party_gerda == "die_partei" ~ "Die PARTEI",
    party_gerda == "die_grauen" ~ "Die Grauen -- Graue Panther",
    party_gerda == "dkp" ~ "Kommunistische Partei Deutschlands",
    party_gerda == "freie_wahler" ~ "Freie Waehler",
    party_gerda == "odp" ~ "Okologisch-Demokratische Partei",
    party_gerda == "diebasis" ~ "Basisdemokratische Partei Deutschland",
    party_gerda == "zentrum" ~ "Deutsche Zentrumspartei",
    # Add additional mappings as you identify them
    TRUE ~ NA_character_
  )
)

crosswalk %>% print(n = 120)

# Merge with parlgov data
crosswalk <- crosswalk %>% 
  left_join(parlgov_ger, by = "party_name_ascii")

glimpse(crosswalk)

# Save crosswalk
write_rds(crosswalk, "data/crosswalks/party_crosswalk.rds")

### END



# Party facts data --------------------------------------------------------

# Get it from: https://partyfacts.herokuapp.com/
# download and read Party Facts mapping table
# file_name <- "partyfacts-mapping.csv"
# if( ! file_name %in% list.files()) {
#   url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
#   download.file(url, file_name)
# }
# partyfacts_raw <- read_csv(file_name, guess_max = 50000)

# partyfacts_ger <- partyfacts_raw %>% 
#   dplyr::filter(country == "DEU")

# glimpse(partyfacts_ger)
# table(partyfacts_ger$dataset_key)
# table(partyfacts_ger$name_short)

# afd <- partyfacts_ger %>% 
#   dplyr::filter(partyfacts_id == "1976")
# View(afd)


# partyfacts_reduced <- partyfacts_ger %>% 
#   select(partyfacts_id, name_short, name, name_english) %>% 
#   mutate(
#     name_short = str_to_lower(name_short),
#     name = str_to_lower(name),
#     name_english = str_to_lower(name_english)
#   ) %>% 
#   distinct() %>% 
#   arrange(partyfacts_id) %>% 
#   print(n = 600)

# View(partyfacts_reduced)

# partyfacts_reduced <- partyfacts_ger %>% 
#   select(partyfacts_id, name_short) %>% 
#   mutate(name_short = str_to_lower(name_short)) %>% 
#   distinct() %>% 
#   arrange(partyfacts_id) %>% 
#   print(n = 600)


# crosswalk <- parties %>%
#   mutate(
#     partyfacts_id = case_when(
#       # Major established parties:
#       party == "afd" ~ 1976,
#       party %in% c("b90_gr", "gruene", "grune") ~ 1816,  # Bündnis 90/Die Grünen
#       party %in% c("die_linke", "linke_pds", "pds") ~ 1545,
#       party == "fdp" ~ 573,
#       party == "cdu" ~ 1375,
#       party == "csu" ~ 1731,
#       party == "spd" ~ 383,
#       party == "ssw" ~ 50,
#       party == "piraten" ~ 1287,
#       party == "diebasis" ~ 8993,
#       party == "die_partei" ~ 6126,
#       party == "die_grauen" ~ 1652,
#       party == "dkp" ~ 1135,
#       party == "kpd" ~ 1135,    # Both mapped to KPD/DKP ID
#       party == "freie_wahler" ~ 1090,
#       party == "odp" ~ 7345,
#       party == "zentrum" ~ 1798,
#       party == "npd" ~ 1723,
#       party == "dvu" ~ 695,
#       party == "rep" ~ 720,
      
#       # Tierschutz
#       party %in% c("tierschutzpartei", "tierschutz", "die_tierschutzpartei") ~ 749,
      
#       # Minor or newer parties appearing under 2702 (based on provided snippet):
#       # Note: This is tentative. If you have official mappings, use them instead.
#       party == "mlpd" ~ 2702,
#       party == "familie" ~ 2702,
#       party == "lkr" ~ 2702,
      
#       # Parties not found or no clear mapping:
#       TRUE ~ NA_real_
#     )
#   )

# crosswalk