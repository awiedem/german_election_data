### Clean and combine BTW electoral results at municipality level 1980-2021 
# Disregard multi mail-in voting districts
# Vincent Heddesheimer
# Oct, 01, 2024

rm(list = ls())


# Create dataframe to store mail-in descriptives --------------------------

# Create df with election year column number of join mail-in districts
mailin_df <- data.frame(
  election_year = c(1980, 1983, 1987, 1990, 1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021),
  mailin_join = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  share = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0)
)


# 1980 --------------------------------------------------------------------

df80 <- fread("data/federal_elections/municipality_level/raw/BTW80/BTW80_Zweitstimmen_Gemeinden.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable BA (Bezirksart) 
  # 0 = Urnenwahlbezirk, 5 = Briefwahlberzirk, 6 = Sonderwahlbezirk, 8 = Bezirke für Wahlberechtigte ohne nähere Angaben
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  group_by(ags) |>
  summarise_at(vars(A:Sonstige), sum, na.rm = TRUE) |>
  mutate(
    election_year = 1980,
    county = substr(ags, 1 , 5)) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0)))

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin80 <- df80 |> filter(str_ends(ags, "999"))
# no combined mail-in districts
# store in mailin_df
mailin_df[1,2] <- nrow(mailin80)

# Check duplicates
dupl <- df80 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0 duplicates

# 1983 --------------------------------------------------------------------

df83 <- fread("data/federal_elections/municipality_level/raw/BTW83/BTW83_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable WBZ (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    GEM = pad_zero_conditional(GEM, 1, "00"),
    GEM = pad_zero_conditional(GEM, 2, "0"),
    ags = paste0(Land, RB, Kreis, GEM)
  ) 


# Summarize all variables by ags & Bezirksart
df83_bezirksarten <- df83 |>
  group_by(ags, BZA) |>
  summarise_at(vars(A:Übrige), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin83 <- df83_bezirksarten |>
  filter(BZA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df83 <- df83_bezirksarten |>
  group_by(ags) |>
  summarise_at(vars(A:Übrige), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 1983) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin83, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 1, 0)
    )

# check whether unique mailin districts are ever joint_mailin
inspect <- df83 |>
  filter(unique_mailin == 1) |>
  filter(joint_mailin == 1)
# yes they are because all joint-mailin that end on 999 are also BZA == 5
# does not matter though


## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin83 <- df83 |> filter(str_ends(ags, "999"))
# 81 combined mail-in districts
# store in mailin_df
mailin_df[2,2] <- nrow(mailin83)

# Load population data
pop83 <- read_excel("data/covars_municipality/raw/municipality_sizes/31121983_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...2`,
    RB = `...3`,
    Kreis = `...4`,
    Gemeinde = `...5`,
    area = `...10`,
    pop = `...11`
  ) |>
  slice(8:8972) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df83 <- df83 |>
  left_join(pop83, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county population & area for ags with unique mailin
  group_by(county, unique_mailin) |>
  mutate(county_pop = sum(pop, na.rm = T),
         county_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_pop,
         area_weight = area / county_area)

# Check duplicates
dupl <- df83 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0

# 1987 --------------------------------------------------------------------

df87 <- fread("data/federal_elections/municipality_level/raw/BTW87/BTW87_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  )

# Summarize all variables by ags & Bezirksart
df87_bezirksarten <- df87 |>
  group_by(ags, BA) |>
  summarise_at(vars(A:Patrioten), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin87 <- df87_bezirksarten |>
  filter(BA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df87 <- df87_bezirksarten |>
  group_by(ags) |>
  summarise_at(vars(A:Patrioten), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 1987) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin87, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 1, 0)
    )


## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin87 <- df87 |> filter(str_ends(ags, "999"))
# 86 combined mail-in districts
# store in mailin_df
mailin_df[3,2] <- nrow(mailin87)

# Load population data
pop87 <- read_excel("data/covars_municipality/raw/municipality_sizes/31121987_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...2`,
    RB = `...3`,
    Kreis = `...4`,
    Gemeinde = `...5`,
    area = `...10`,
    pop = `...11`
  ) |>
  slice(8:8964) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df87 <- df87 |>
  left_join(pop87, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county population & area for ags with unique mailin
  group_by(county, unique_mailin) |>
  mutate(county_pop = sum(pop, na.rm = T),
         county_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_pop,
         area_weight = area / county_area)

# Check duplicates
dupl <- df87 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0


# 1990 --------------------------------------------------------------------

df90 <- fread("data/federal_elections/municipality_level/raw/BTW90/BTW90_Zweitstimmen_Wahlbezirke.csv") |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    # Verbandsgemeinde & Gemeinde in same column: separate and only take Gemeinde for ags
    VG = ifelse(nchar(`Verbandsgemeinde und Gemeinde`) > 3, str_sub(`Verbandsgemeinde und Gemeinde`, end = -4), 0),
    Gem = ifelse(nchar(`Verbandsgemeinde und Gemeinde`) <= 3, `Verbandsgemeinde und Gemeinde`, str_sub(`Verbandsgemeinde und Gemeinde`, start = -3)),
    Gem = pad_zero_conditional(Gem, 1, "00"),
    Gem = pad_zero_conditional(Gem, 2, "0"),
    ags = paste0(Land, Regierungsbezirk, Kreis, Gem)
  )

# Summarize all variables by ags & Bezirksart
df90_bezirksarten <- df90 |>
  group_by(ags, Bezirksart) |>
  summarise_at(vars(`Wahlberechtigte (A)`:VAA), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin90 <- df90_bezirksarten |>
  filter(Bezirksart == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df90 <- df90_bezirksarten |>
  group_by(ags) |>
  summarise_at(vars(`Wahlberechtigte (A)`:VAA), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 1990) |>
  # one ags does not have any values
  filter(ags != "01000000") |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin90, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 1, 0)
    )



## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin90 <- df90 |> filter(str_ends(ags, "999"))
# 257 combined mail-in districts
# store in mailin_df
mailin_df[4,2] <- nrow(mailin90)


# Load population data
pop90 <- read_excel("data/covars_municipality/raw/municipality_sizes/31121990_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...2`,
    RB = `...3`,
    Kreis = `...4`,
    Gemeinde = `...5`,
    area = `...10`,
    pop = `...11`
  ) |>
  slice(8:16796) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df90 <- df90 |>
  left_join(pop90, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county population & area for ags with unique mailin
  group_by(county, unique_mailin) |>
  mutate(county_pop = sum(pop, na.rm = T),
         county_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_pop,
         area_weight = area / county_area)

# Check duplicates
dupl <- df90 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0


# 1994 --------------------------------------------------------------------

df94 <- fread("data/federal_elections/municipality_level/raw/BTW94/BTW94_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) 

# Summarize all variables by ags & Bezirksart
df94_bezirksarten <- df94 |>
  group_by(ags, Bezirksart) |>
  summarise_at(vars(A:`STATT Partei`), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin94 <- df94_bezirksarten |>
  filter(Bezirksart == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df94 <- df94_bezirksarten |>
  group_by(ags) |>
  summarise_at(vars(A:`STATT Partei`), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 1994) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin94, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 1, 0)
    )



## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin94 <- df94 |> filter(str_ends(ags, "999"))
# 115 combined mail-in districts
# store in mailin_df
mailin_df[5,2] <- nrow(mailin94)


# Load population data
pop94 <- read_excel("data/covars_municipality/raw/municipality_sizes/31121994_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(8:16691) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df94 <- df94 |>
  left_join(pop94, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county population & area for ags with unique mailin
  group_by(county, unique_mailin) |>
  mutate(county_pop = sum(pop, na.rm = T),
         county_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_pop,
         area_weight = area / county_area)

# Check duplicates
dupl <- df94 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0

# 1998 --------------------------------------------------------------------

df98 <- fread("data/federal_elections/municipality_level/raw/BTW98/BTW98_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  )


# Summarize all variables by ags & Bezirksart
df98_bezirksarten <- df98 |>
  group_by(ags, Bezirksart) |>
  summarise_at(vars(A:PSG), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin98 <- df98_bezirksarten |>
  filter(Bezirksart == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df98 <- df98_bezirksarten |>
  group_by(ags) |>
  summarise_at(vars(A:PSG), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 1998) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin98, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 1, 0)
    )



## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin98 <- df98 |> filter(str_ends(ags, "999"))
# 115 combined mail-in districts
# store in mailin_df
mailin_df[6,2] <- nrow(mailin98)

# Load population data
pop98 <- read_excel("data/covars_municipality/raw/municipality_sizes/31121998_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(8:16470) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df98 <- df98 |>
  left_join(pop98, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county population & area for ags with unique mailin
  group_by(county, unique_mailin) |>
  mutate(county_pop = sum(pop, na.rm = T),
         county_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_pop,
         area_weight = area / county_area)

# Check duplicates
dupl <- df98 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0



# 2002 --------------------------------------------------------------------

df02 <- fread("data/federal_elections/municipality_level/raw/BTW02/BTW02_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(L, 1),
    Kreis = pad_zero_conditional(KR, 1),
    GEM = pad_zero_conditional(GEM, 1, "00"),
    GEM = pad_zero_conditional(GEM, 2, "0"),
    ags = paste0(Land, RB, Kreis, GEM)
  ) 

# inspect <- df02 |> select(ags, VG, BZA)

# Eine Besonderheit stellen gemeinsame Briefwahlbezirke für mehrere Gemeinden dar.
# Diese sind an der Gemeindekennziffer „999“ zu erkennen. 
# Das Feld „Verbandsgemeinde“ enthält in diesen Fällen folgende Schlüsselnummern:
#   - Wenn alle Gemeinden dem gleichen Verband angehören den amtlichen Verbands-gemeindeschlüssel.
#   - Wenn die Gemeinden nicht dem gleichen Verband angehören, wurden die ersten beiden Stellen 
#     durch eine Ziffernkombination ersetzt, die für alle Gemeinden, die einen gemeinsamen
#     Briefwahlvorstand gebildet haben, identisch ist und die letzten beiden Stellen des V
#     erbandsgemeindeschlüssels beibehalten.
###
# Joint postal voting districts for several municipalities represent 
# a special feature. These can be identified by the municipality code 
# number "999". In these cases, the field "Verbandsgemeinde" contains 
# the following key numbers:
# - If all municipalities belong to the same association, the official
#   association municipality key.
# - If the municipalities do not belong to the same association, the 
#   first two digits have been replaced by a combination of digits 
#   that is identical for all municipalities that have formed a joint 
#   postal voting committee and the last two digits of the association
#   municipality key have been retained.

# Summarize all variables by ags & Bezirksart
df02_vg_bezirksarten <- df02 |>
  group_by(ags, VG, BZA) |>
  summarise_at(vars(A:Schill), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin02 <- df02_vg_bezirksarten |>
  filter(BZA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df02 <- df02_vg_bezirksarten |>
  group_by(ags, VG) |>
  summarise_at(vars(A:Schill), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 2002) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin02, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 0, 1)
    )



## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin02 <- df02 |> 
  select(ags) |>
  distinct() |>
  filter(str_ends(ags, "999"))
# 184 combined mail-in districts
# store in mailin_df
mailin_df[7,2] <- nrow(mailin02)

# Load population data
pop02 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122002_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(8:15411) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df02 <- df02 |>
  left_join(pop02, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county-VG population & area for ags with unique mailin
  group_by(county, VG, unique_mailin) |>
  mutate(county_vg_pop = sum(pop, na.rm = T),
         county_vg_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_vg_pop,
         area_weight = area / county_vg_area)

# Check duplicates
dupl <- df02 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0

# 2005 --------------------------------------------------------------------

df05 <- fread("data/federal_elections/municipality_level/raw/BTW05/BTW05_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Ld, 1),
    Kreis = pad_zero_conditional(Kr, 1),
    Gem = pad_zero_conditional(Gem, 1, "00"),
    Gem = pad_zero_conditional(Gem, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gem)
  ) 

# inspect <- df05 |> select(ags, BWBez, BA)

# Eine Besonderheit stellen gemeinsame Briefwahlbezirke für mehrere 
# Gemeinden dar. Alle Gemeinden eines Kreises, die einen gemeinsamen 
# Briefwahlvorstand bilden, erhalten im zusätzlichen Feld EF7 
# „Briefwahlzugehörigkeit“ die gleiche 2-stellige Ziffer. Der 
# Briefwahl-bezirk selbst ist an der Gemeindekennziffer „999“ zu 
# erkennen. Der 4-stellige Verbands-gemeindeschlüssel dieses Bezirks 
# entspricht, wenn alle Gemeinden dem gleichen Verband angehören, 
# deren Verbandsgemeindeschlüssel. Andernfalls wurde als Schlüssel 
# „0000“ eingesetzt.
###
# Joint postal voting districts for several municipalities represent 
# a special feature. All municipalities in a district that form a joint 
# postal voting district are given the same 2-digit number in the 
# additional field EF7 "Postal voting affiliation". The postal voting 
# district itself can be identified by the municipality code number 
# "999". The 4-digit association municipality key of this district 
# corresponds, if all municipalities belong to the same association, 
# to their association municipality key. Otherwise, "0000" was used 
# as the key.


# Summarize all variables by ags & BWBez & Bezirksart
df05_bezirksarten <- df05 |>
  group_by(ags, BWBez, BA) |>
  summarise_at(vars(A:`Pro DM`), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin05 <- df05_bezirksarten |>
  filter(BA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df05 <- df05_bezirksarten |>
  group_by(ags, BWBez) |>
  summarise_at(vars(A:`Pro DM`), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 2005) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin05, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "999"), 1, 0)
    )

# Check duplicates
dupl <- df05 |>
  count(ags, election_year) |>
  filter(n>1)

## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin05 <- df05 |> 
  select(ags) |>
  distinct() |>
  filter(str_ends(ags, "999"))
# 186 combined mail-in districts
# store in mailin_df
mailin_df[8,2] <- nrow(mailin05)

# Load population data
pop05 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122005_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(8:14427) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df05 <- df05 |>
  left_join(pop05, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county-VG population & area for ags with unique mailin
  group_by(county, BWBez, unique_mailin) |>
  mutate(county_bwbez_pop = sum(pop, na.rm = T),
         county_bwbez_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_bwbez_pop,
         area_weight = area / county_bwbez_area) |>
  # BWBez to character
  mutate(BWBez = as.character(BWBez))

# Check duplicates
dupl <- df05 |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 0


# 2009 --------------------------------------------------------------------

df09 <- fread("data/federal_elections/municipality_level/raw/BTW09/BTW09_Zweitstimmen Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  rename(BWBez = `BW-Bez`) |>
  mutate(across(A:RENTNER, ~ as.numeric(.x)))

inspect <- df09 |> select(ags, BWBez, BA)

### 
# When running the code as for the previous years, there are 
# three ags for which there is a multi mail-in district but also a 
# unique mail-in district.
# This creates duplicates if this is not accounted for.
# 12064044, 15089310, 16071003
# Therefore, the code that follows is different from the code before.


# Create ags_bw_bez indicator
#df09 <- df09 |> mutate(ags_bwbez = paste0(ags, "_", BWBez))

# Eine Besonderheit stellen gemeinsame Briefwahlbezirke für mehrere
# Gemeinden dar. Alle Gemeinden eines Kreises, die einen gemeinsamen 
# Briefwahlvorstand bilden, erhalten im zusätzlichen Feld EF7 
# „Briefwahlzugehörigkeit“ die gleiche 2-stellige Ziffer. Der 
# Briefwahlbezirk selbst ist an der Gemeindekennziffer „999“ (in 
# einigen Fällen auch „996“, „997“ oder „998“) zu erkennen. Der 
# 4-stellige Verbandsgemeindeschlüssel dieses Bezirks entspricht,
# wenn alle Gemeinden dem gleichen Verband angehören, deren 
# Verbandsgemeindeschlüssel. Andernfalls setzt er sich zusammen aus
# den Ziffern „11“ und dem Eingabefeld EF7.
###
# Joint postal voting districts for several municipalities represent 
# a special feature. All municipalities of a district that form a 
# joint absentee ballot committee are given the same additional field 
# EF7 "Absentee ballot affiliation" the same 2-digit number. The 
# postal voting district itself can be identified by the municipality 
# code "999" (in some cases also "996", "997" or "998"). The 4-digit 
# association municipality code of this district corresponds, if all 
# municipalities belong to the same association, their association 
# municipality key. Otherwise it is composed of the digits "11" and 
# the input field EF7.

# Summarize all variables by ags & BWBez & Bezirksart
df09_bezirksarten <- df09 |>
  group_by(ags, BWBez, BA) |>
  summarize_at(vars(A:RENTNER), sum, na.rm = TRUE)
# |>
#   select(-c(Wkr:Gemeinde, Wbz))

# Get ags that have their own mailin data
ags_w_mailin09 <- df09_bezirksarten |>
  #mutate(ags_bwbez = paste0(ags, "_", BWBez)) |>
  filter(BA == 5) |>
  #pull(ags_bwbez) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df09 <- df09_bezirksarten |>
  group_by(ags, BWBez) |>
  summarize_at(vars(A:RENTNER), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 2009) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin09, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "996|997|998|999"), 1, 0)
  )

# Check duplicates
dupl <- df09 |>
  count(ags, election_year) |>
  filter(n>1)

## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin09 <- df09 |> 
  select(ags) |>
  distinct() |>
  # Mail-in districts end in 996/..7/8/9
  filter(str_ends(ags, "996|997|998|999"))
# 160 combined mail-in districts
# store in mailin_df
mailin_df[9,2] <- nrow(mailin09)

# Load population data
pop09 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122009_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(9:17173) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df09 <- df09 |>
  left_join(pop09, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county-VG population & area for ags with unique mailin
  group_by(county, BWBez, unique_mailin) |>
  mutate(county_bwbez_pop = sum(pop, na.rm = T),
         county_bwbez_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_bwbez_pop,
         area_weight = area / county_bwbez_area) |>
  # BWBez to character
  mutate(BWBez = as.character(BWBez))

# Check duplicates
dupl <- df09 |>
  count(ags, election_year) |>
  filter(n>1) |>
  pull(ags)
length(dupl)

# 2013 --------------------------------------------------------------------

df13 <- fread("data/federal_elections/municipality_level/raw/BTW13/BTW13_Zweitstimmen_Wahlbezirke.txt", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  rename(BWBez = `BW-Bez`)

inspect <- df13 |> select(ags, BWBez, BA)

### 
# When running the code as for the previous years, there is 
# one ags for which there is a multi mail-in district but also a 
# unique mail-in district.
# This creates duplicates if this is not accounted for.
# 16064074
# Therefore, the code that follows is different from the code before.

# Create ags_bw_bez indicator
# df13 <- df13 |> mutate(ags_bwbez = paste0(ags, "_", BWBez))

# Eine Besonderheit stellen gemeinsame Briefwahlbezirke für mehrere
# Gemeinden dar. Alle Gemeinden eines Kreises, die einen gemeinsamen 
# Briefwahlvorstand bilden, erhalten im zusätzlichen Feld EF7 
# „Briefwahlzugehörigkeit“ die gleiche 2-stellige Ziffer. Der 
# Briefwahlbezirk selbst ist an der Gemeindekennziffer „999“ (in 
# einigen Fällen auch „996“, „997“ oder „998“) zu erkennen. Der 
# 4-stellige Verbandsgemeindeschlüssel dieses Bezirks entspricht,
# wenn alle Gemeinden dem gleichen Verband angehören, deren 
# Verbandsgemeindeschlüssel. Andernfalls setzt er sich zusammen aus
# den Ziffern „11“ und dem Eingabefeld EF7.
###
# Joint postal voting districts for several municipalities represent 
# a special feature. All municipalities of a district that form a 
# joint absentee ballot committee are given the same additional field 
# EF7 "Absentee ballot affiliation" the same 2-digit number. The 
# postal voting district itself can be identified by the municipality 
# code "999" (in some cases also "996", "997" or "998"). The 4-digit 
# association municipality code of this district corresponds, if all 
# municipalities belong to the same association, their association 
# municipality key. Otherwise it is composed of the digits "11" and 
# the input field EF7.

# Summarize all variables by ags & BWBez & Bezirksart
df13_bezirksarten <- df13 |>
  group_by(ags, BWBez, BA) |>
  summarize_at(vars(A:`Die PARTEI`), sum, na.rm = TRUE)
# |>
#   select(-c(Wkr:Gemeinde, Wbz))

# Get ags that have their own mailin data
ags_w_mailin13 <- df13_bezirksarten |>
  filter(BA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df13 <- df13_bezirksarten |>
  group_by(ags, BWBez) |>
  summarize_at(vars(A:`Die PARTEI`), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 2013) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    #ags_bwbez = paste0(ags, "_", BWBez),
    unique_mailin = ifelse(ags %in% ags_w_mailin13, 1, 0),
    joint_mailin = ifelse(str_ends(ags, "996|997|998|999"), 1, 0)
  )


## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin13 <- df13 |> 
  select(ags) |>
  distinct() |>
  # Mail-in districts end in 996/..7/8/9
  filter(str_ends(ags, "996|997|998|999"))
# 144 combined mail-in districts
# store in mailin_df
mailin_df[10,2] <- nrow(mailin13)

# Load population data
pop13 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122013_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(9:16272) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df13 <- df13 |>
  left_join(pop13, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county-VG population & area for ags with unique mailin
  group_by(county, BWBez, unique_mailin) |>
  mutate(county_bwbez_pop = sum(pop, na.rm = T),
         county_bwbez_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_bwbez_pop,
         area_weight = area / county_bwbez_area) |>
  # BWBez to character
  mutate(BWBez = as.character(BWBez))


# Check duplicates
dupl <- df13 |>
  count(ags, election_year) |>
  filter(n>1) |>
  pull(ags)
length(dupl) # 0


# 2017 --------------------------------------------------------------------

df17 <- fread("data/federal_elections/municipality_level/raw/BTW17/btw17_wbz_zweitstimmen.csv", encoding = 'Latin-1') |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, Regierungsbezirk, Kreis, Gemeinde)
  ) |>
  rename(BWBez = `Kennziffer Briefwahlzugehörigkeit`,
         BA = Bezirksart)

inspect <- df17 |> select(ags, BWBez, BA)


# Eine Besonderheit stellen gemeinsame Briefwahlbezirke für mehrere
# Gemeinden dar. Alle Gemeinden eines Kreises, die einen gemeinsamen 
# Briefwahlvorstand bilden, erhalten im zusätz-lichen Feld EF7 
# „Briefwahlzugehörigkeit“ die gleiche 2-stellige Ziffer. Die 
# Gemeindekennziffer der gemeinsamen Briefwahlbezirke setzt sich 
# zusammen aus der Ziffer „9“ und dieser 2-stelligen Ziffer. Der 
# 4-stellige Verbandsgemeindeschlüssel dieses Bezirks entspricht, 
# wenn alle Gemeinden dem gleichen Verband angehören, deren 
# Verbandsgemeindeschlüssel. Andernfalls setzt er sich zusammen aus
# den Ziffern „11“ und der 2-stelligen Briefwahlzugehörigkeits-Ziffer.
###
# Joint postal voting districts for several municipalities represent 
# a special feature. All municipalities of a district that form a 
# joint postal voting district receive the same 2-digit number in the 
# additional field EF7 "Postal voting affiliation". The municipality 
# code number of the joint postal voting districts is composed of the 
# digit "9" and this 2-digit number. The 4-digit association 
# municipality code of this district corresponds, if all 
# municipalities belong to the same association, to their association 
# municipality code. Otherwise, it is composed of the digits "11" and 
# the 2-digit absentee ballot affiliation code.

# Summarize all variables by ags & BWBez & Bezirksart
df17_bezirksarten <- df17 |>
  group_by(ags, BWBez, BA) |>
  summarise_at(vars(`Wahlberechtigte (A)`:`V-Partei³`), sum, na.rm = TRUE) 

# Get ags that have their own mailin data
ags_w_mailin17 <- df17_bezirksarten |>
  filter(BA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df17 <- df17_bezirksarten |>
  group_by(ags, BWBez) |>
  summarise_at(vars(`Wahlberechtigte (A)`:`V-Partei³`), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 2017) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin13, 1, 0),
    joint_mailin = ifelse(str_sub(ags, -3, -3) == "9", 1, 0)
    )


## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin17 <- df17 |> 
  # Mail-in districts have 9 as third to last place
  filter(str_sub(ags, -3, -3) == "9")
# 707 combined mail-in districts
# store in mailin_df
mailin_df[11,2] <- nrow(mailin17)

# Load population data
pop17 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122017_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(9:16132) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df17 <- df17 |>
  left_join(pop17, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county-VG population & area for ags with unique mailin
  group_by(county, BWBez, unique_mailin) |>
  mutate(county_bwbez_pop = sum(pop, na.rm = T),
         county_bwbez_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_bwbez_pop,
         area_weight = area / county_bwbez_area) |>
  # BWBez to character
  mutate(BWBez = as.character(BWBez))

# Check duplicates
dupl <- df17 |>
  count(ags, election_year) |>
  filter(n>1) |>
  pull(ags)
nrow(dupl) # 0


# 2021 --------------------------------------------------------------------

df21 <- fread("data/federal_elections/municipality_level/raw/BTW21/btw21_wbz_ergebnisse.csv", encoding = 'UTF-8') |>
  # csv file includes erst- and zweitstimme; select only variables for zweitstimme
  select(c(1:17, 65:106)) |>
  slice(-1) |>
  # Multiple entries for each municipality: variable Wbz (Wahlbezirk) 
  ###
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, Regierungsbezirk, Kreis, Gemeinde),
    across(`Wahlberechtigte (A)`:Volt, as.numeric)
  ) |>
  rename(BWBez = `Kennziffer Briefwahlzugehörigkeit`,
         BA = Bezirksart)

(inspect <- df21 |> select(ags, BWBez, BA))

# Eine Besonderheit stellen gemeinsame Briefwahlbezirke für mehrere
# Gemeinden dar. Alle Gemeinden eines Kreises, die einen gemeinsamen 
# Briefwahlvorstand bilden, erhalten im zusätz-lichen Feld EF7 
# „Briefwahlzugehörigkeit“ die gleiche 2-stellige Ziffer. Die 
# Gemeindekennziffer der gemeinsamen Briefwahlbezirke setzt sich 
# zusammen aus der Ziffer „9“ und dieser 2-stelligen Ziffer. Der 
# 4-stellige Verbandsgemeindeschlüssel dieses Bezirks entspricht, 
# wenn alle Gemeinden dem gleichen Verband angehören, deren 
# Verbandsgemeindeschlüssel. Andernfalls setzt er sich zusammen aus
# den Ziffern „11“ und der 2-stelligen Briefwahlzugehörigkeits-Ziffer.
###
# Joint postal voting districts for several municipalities represent 
# a special feature. All municipalities of a district that form a 
# joint postal voting district receive the same 2-digit number in the 
# additional field EF7 "Postal voting affiliation". The municipality 
# code number of the joint postal voting districts is composed of the 
# digit "9" and this 2-digit number. The 4-digit association 
# municipality code of this district corresponds, if all 
# municipalities belong to the same association, to their association 
# municipality code. Otherwise, it is composed of the digits "11" and 
# the 2-digit absentee ballot affiliation code.

# Summarize all variables by ags & BWBez & Bezirksart
df21_bezirksarten <- df21 |>
  group_by(ags, BWBez, BA) |>
  summarise_at(vars(`Wahlberechtigte (A)`:Volt), sum, na.rm = TRUE)

# Get ags that have their own mailin data
ags_w_mailin21 <- df21_bezirksarten |>
  filter(BA == 5) |>
  pull(ags)

# Summarize variables for all ags and create variable for whether ags had unique mailin
df21 <- df21_bezirksarten |>
  group_by(ags, BWBez) |>
  summarise_at(vars(`Wahlberechtigte (A)`:Volt), sum, na.rm = TRUE) |>
  ungroup() |>
  mutate(election_year = 2021) |>
  # Remove parties that got no votes
  select(where(~ any(. != 0))) |>
  # variable for whether ags had unique mailin
  mutate(
    unique_mailin = ifelse(ags %in% ags_w_mailin21, 1, 0),
    join_mailin = ifelse(str_sub(ags, -3, -3) == "9", 1, 0)
    )



## Mail-in voting ---------------------------------------------------------

# Mail-in voting districts for several municipalities can be identified by the municipality code "999".
mailin21 <- df21 |> 
  # Mail-in districts have 9 as third to last place
  filter(str_sub(ags, -3, -3) == "9")
# 621 combined mail-in districts
# store in mailin_df
mailin_df[12,2] <- nrow(mailin21)

# Load population data
pop21 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122021_Auszug_GV.xlsx", sheet = 2, col_types = "numeric") |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    pop = `...10`
  ) |>
  slice(9:16065) |>
  # delete NAs
  filter(!is.na(Gemeinde)) |>
  # Create ags
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = paste0(Land, RB, Kreis, Gemeinde)
  ) |>
  select(ags, area, pop)

# Merge with BTW data
df21 <- df21 |>
  left_join(pop21, by = "ags") |>
  # calculate county population & area
  mutate(county = substr(ags, 1 , 5)) |>
  # calculate county-VG population & area for ags with unique mailin
  group_by(county, BWBez, unique_mailin) |>
  mutate(county_bwbez_pop = sum(pop, na.rm = T),
         county_bwbez_area = sum(area, na.rm = T)) |>
  ungroup() |>
  # calculate weights (i.e. shares)
  mutate(pop_weight = pop / county_bwbez_pop,
         area_weight = area / county_bwbez_area) |>
  # BWBez to character
  mutate(BWBez = as.character(BWBez))

# Check duplicates
dupl <- df21 |>
  count(ags, election_year) |>
  filter(n>1) |>
  pull(ags)
nrow(dupl) # 0 duplicates


# Recoding ----------------------------------------------------------------

# List all data frames in the environment that start with "df" and end with a number
df_list <- ls(pattern = "^df.*\\d$")

# Filter the data frames from the environment
df_objects <- Filter(is.data.frame, mget(df_list))


## NAs in ags --------------------------------------------------------------
sapply(df_objects, function(x)
  x |>
    filter(is.na(ags)) |>
    nrow())
# None are empty


## Harmonize variable names ------------------------------------------------

renames <- c(
  "eligible_voters" = "a",
  "eligible_voters" = "wahlberechtigte (a)",
  "number_voters" = "b",
  "number_voters" = "wähler (b)",
  "number_voters" = "wählende (b)",
  "valid_votes" = "gültig",
  "valid_votes" = "gültige",
  "voters_wo_blockingnotice" = "a1",
  "voters_wo_blockingnotice" = "wahlberechtigte ohne sperrvermerk (a1)",
  "voters_blockingnotice" = "a2",
  "voters_blockingnotice" = "wahlberechtigte mit sperrvermerk (a2)",
  "voters_par25_2" = "a3",
  "voters_par25_2" = "wahlberechtigte nach § 25 abs. 2 bwo (a3)",
  "voters_w_ballot" = "b1",
  "voters_w_ballot" = "wähler mit wahlschein (b1)",
  "voters_w_ballot" = "wählende mit wahlschein (b1)",
  "linke_pds" = "die linke",
  "linke_pds" = "pds",
  "sgp" = "psg",
  "tierschutz" = "die tierschutzpartei",
  "tierschutz" = "tierschutzpartei", 
  "frauen" = "die frauen",
  "violetten" = "die violetten",
  "nichtwähler" = "partei der nichtwähler",
  "graue" = "die grauen",
  "bündnis21" = "bündnis 21/rrp",
  "mündige" = "mündige bürger",
  "cdu_csu" = "cdu/csu"
)

df_objects_t <- df_objects |>
  map( ~ .x |>
         rename_with(str_to_lower) |>
         rename(any_of(renames)))




# Create one dataframe ----------------------------------------------------

## Sort col names alphabetically
# sort(colnames(df_objects_t |> reduce(bind_rows)))

# for each dataframe in the list of dfs that have the column bwbez, transform the bwbez column to a character


df <- df_objects_t |>
  reduce(bind_rows)
# Check variable names

# Check duplicates
dupl <- df |>
  count(ags, election_year) |>
  filter(n>1)
nrow(dupl) # 603
# all are the join_mailin districts

df <- df |>
  # Get Bundesland / state from ags
  mutate(state = str_sub(ags, end = -7)) |>
  # Organize
  select(
    # Background
    ags, county, election_year, state, eligible_voters, number_voters, valid_votes,
    voters_wo_blockingnotice, voters_blockingnotice, voters_par25_2, voters_w_ballot,
    # Mail-in voting
    unique_mailin, joint_mailin, pop, area, pop_weight, area_weight,
    # Main
    cdu, csu, cdu_csu, spd, grüne, fdp, linke_pds, `b90/gr`,
    # Right-wing
    afd, npd, rep, `die rechte`, dvu, `iii. weg`, fap, ddd, dsu,
    # Left-wing
    dkp, kpd, mlpd, sgp, kbw, v, spad, bsa,
    # Others
    `50plus`, `ab 2000`, `ad-demokraten`, adm, agfg, apd, appd, asd, aufbruch, `b*`, bfb, bge, big, bp, bündnis21, `bündnis c`, bürgerbewegung, bürgerpartei, büso, bwk, `chance 2000`, cbv, cm, deutschland, dib, diebasis, `die partei`, `die humanisten`, dm, dpd, `du.`, eap, familie, forum, frauen, `freie wähler`,fwd, gartenpartei, gesundheitsforschung, graue, hp, lfk, liebe, liga, lkr,  mg, `menschliche welt`, mündige, naturgesetz, nichtwähler, ödp, `offensive d`, `öko-union`, `partei der vernunft`, pass, patrioten, pbc, pdf, pdv, piraten, prg, `pro deutschland`,`pro dm`, rentner, rrp, schill, ssw, `statt partei`, tierschutz, `team todenhöfer`, tierschutzallianz, unabhängige, ust, vaa, violetten, volksabstimmung, volt, `v-partei³`, zentrum 
    ) %>%
  # Calculate extremist votes
  mutate(
    far_right = rowSums(select(., afd:dsu), na.rm = TRUE),
    far_left = rowSums(select(., dkp:bsa), na.rm = TRUE)
  ) %>%
  # Left wing with votes for Linke/PDS
  mutate(
    far_left_wLinke = rowSums(select(., linke_pds, far_left), na.rm = TRUE)
  )
### Extremist parties
## Right wing
# npd, fap (freiheitliche deutsche arbeiterpartei), rep (die republikaner),
# dvu, ddd (bund der deutschen demokraten),
# die rechte, iii. weg, dsu (deutsche soziale union)
## Left wing
# dkp, kpd, v, kbw (kommunistischer bund westdeutschland),
# spad (spartakist-arbeiterparte deutschlands), 
# bsa (bund sozialistischer arbeiter),
# sgp  (= psg; sozialistische gleichheitspartei)
# (Linke/PDS) not entirely left-wing but parts of it (solid - youth organization) are investigted by Verfassungsschutz


# Some transformations
df <- df %>%
  # Harmonize Grüne: Grüne + B90/Gr ran in 1990 elections
  mutate(
    grüne = rowSums(select(., `b90/gr`, grüne), na.rm = TRUE),
    # Calculate CDU/CSU
    cdu_csu = ifelse(!is.na(cdu_csu), cdu_csu,
                     ifelse(csu > 0, csu, cdu)),
    # Get CDU & CSU votes for (non-)Bavarian municipalities if cdu is NA
    cdu = ifelse(is.na(cdu) & state != "09" & !is.na(cdu_csu), cdu_csu, cdu),
    csu = ifelse(is.na(csu) & state == "09" & !is.na(cdu_csu), cdu_csu, csu)
  ) |>
  select(-`b90/gr`) %>%
  # Get state name
  mutate(
    state_name = state_id_to_names(state)
  ) |>
  rename(gruene = grüne) |>
  janitor::clean_names() |>
  # relocate
  relocate(cdu_csu, .after = zentrum) |> 
  select(-state_name)

names(df)

# Write unharmonized df ---------------------------------------------------

write_rds(df, file = "data/federal_elections/municipality_level/final/federal_muni_raw.rds")
fwrite(df, file = "data/federal_elections/municipality_level/final/federal_muni_raw.csv")


# Inspect -----------------------------------------------------------------

df <- read_rds("data/federal_elections/municipality_level/final/federal_muni_raw.rds")

names(df)

# when did party v have non NA?
df |>
  filter(!is.na(v)) |>
  select(election_year) |>
  distinct() |>
  print(n = Inf)

### END