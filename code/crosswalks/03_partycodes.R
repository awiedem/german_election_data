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