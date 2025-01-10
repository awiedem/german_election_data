### Harmonize municipal electoral results to 2021 borders
# Vincent Heddesheimer
# First: Aug 08, 2024
# Last: Nov 22, 2024

rm(list = ls())

conflicts_prefer(dplyr::filter)

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# Set working directory if running 01_municipal_unharm.R before this
setwd(here::here())

# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/final/ags_crosswalks.csv") |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    weights = pop_cw * population
    )
# 
# cw <- fread("../crosswalks/final/ags_crosswalks.csv") |>
#   mutate(
#     ags = pad_zero_conditional(ags, 7),
#     weights = pop_cw * population
#   )

# Merge with unharmonized election data -----------------------------------

df <- readr::read_rds("data/municipal_elections/final/municipal_unharm.rds") |>
  # filter years before 1990: no crosswalks available
  filter(election_year >= 1990) |>
  mutate(election_year = as.numeric(election_year))

glimpse(df)
glimpse(cw)
table(df$election_year, useNA = "ifany")
table(is.na(df$ags_name))

# inspect -----------------------------------------------------------------

state_id_to_names

# is there more than one election in one ags in one year?
dupl <-df |>
  group_by(ags, ags_name, election_year) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  print(n=Inf) |>
  mutate(id = paste0(ags,"_",election_year))
# no: none

# # Create .rds file of duplicates
# inspect <- df |>
#   mutate(id = paste0(ags,"_",election_year)) |>
#   filter(id %in% dupl$id) |>
#   arrange(ags, election_year)
# 
# saveRDS(inspect, "/Users/vincentheddesheimer/Downloads/duplicates.rds")

# Merge w/ cw -------------------------------------------------------------

# bind with crosswalks
df_naive_merge <- df |>
  left_join_check_obs(cw |> select(-ags_name), by = c("ags", "election_year" = "year")) |>
  arrange(ags, election_year)
# number of obs increases: but this is wanted, as we want to harmonize the data

# is there any ags that did not get merged to ags_21?
not_merged_naive <- df_naive_merge %>%
  filter(election_year < 2021) %>%
  filter(is.na(ags_21)) %>%
  select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))
not_merged_naive
# If we do not follow the steps below, there are 1,056 cases of unsuccessful merges.


# Dealing with unsuccessful mergers ---------------------------------------


# # 1. in 2011 for MeckPomm 
# 
# # define cases where we want to use year - 1
# ags_year_cw <- not_merged_naive %>%
#   filter(
#     grepl("^14", id)            # id starts with 14
#   ) %>%
#   pull(id)

# apply the rules
df <- df |>
  mutate(
    id = paste0(ags, "_", election_year),
    # X. wrong AGS: checked with election results Leitband 
    # and manually matched ags names btw. election results & crosswalk files
    ags = case_when(
      id == "01051141_2008" ~ "01051111", # Süderheistedt 
      id == "01059186_2008" ~ "01059165", # Steinbergkirche 
      id == "01059187_2008" ~ "01059011", # Boren
      id == "03361013_2001" ~ "03361010", # Riede
      id == "05313000_2009" ~ "05334002", # Aachen
      id == "05313000_2014" ~ "05334002", # Aachen
      id == "05313000_2020" ~ "05334002", # Aachen
      id == "07140502_1994" ~ "07135050", # Lahr
      id == "07140502_1999" ~ "07135050", # Lahr
      id == "07140503_1994" ~ "07135063", # Mörsdorf
      id == "07140503_1999" ~ "07135063", # Mörsdorf
      id == "07140504_1994" ~ "07135094", # Zilshausen
      id == "07140504_1999" ~ "07135094", # Zilshausen
      id == "07232502_1994" ~ "07232021", # Brimingen
      id == "07232502_1999" ~ "07232021", # Brimingen
      id == "07235207_1994" ~ "07231207", # Trittenheim
      id == "07235207_1999" ~ "07231207", # Trittenheim
      id == "07235207_2004" ~ "07231207", # Trittenheim
      id == "07235207_2009" ~ "07231207", # Trittenheim
      id == "13053108_2004" ~ "13053109", # Prebberede
      # SA 1994
      id == "15159029_1994" ~ "15126310", # Merzien
      # SA 2005
      id == "15087275_2005" ~ "15260039", # Mansfeld
      id == "15086055_2005" ~ "15358017", # Gommern
      # SA 2006
      id == "15088195_2006" ~ "15265026", # Landsberg
      id == "15088235_2006" ~ "15261039", # Müseln
      
      # SA 2007
      id == "15086270_2007" ~ "15151066", # Zeppernick
      id == "15089040_2007" ~ "15367003", # Biere
      id == "15089080_2007" ~ "15367007", # Eggersdorf
      id == "15089085_2007" ~ "15362031", #	Eickendorf
      id == "15089160_2007" ~ "15367013", # Großmühlingen
      id == "15089190_2007" ~ "15367015", # Kleinmühlingen
      id == "15089335_2007" ~ "15367027", # Welsleben
      id == "15089370_2007" ~ "15367029", # Zens,
      
      # Saxony 
      id == "14017410_1994" ~ "14077300", # Neuhausen/ Erzgeb.
      id == "14018410_1994" ~ "14091200", # Lichtenau
      id == "14019510_1994" ~ "14074250", # Neukyhna
      id == "14020610_1994" ~ "14079210", # Frauendorf
      id == "14022610_1994" ~ "14094220", # Schönfeld-Weißig
      id == "14022620_1994" ~ "14094160", # Promnitztal
      id == "14025410_1994" ~ "14077020", # Bobritzsch
      id == "14027410_1994" ~ "14079190", # Eulatal
      id == "14027420_1994" ~ "14082230", # Langensteinbach
      id == "14029510_1994" ~ "14084430", # Vierkirchen
      id == "14029520_1994" ~ "14084370", # Schöpstal
      id == "14030710_1994" ~ "14083250", # Parthenstein
      id == "14030720_1994" ~ "14083020", # Bad Lausick
      id == "14030730_1994" ~ "14083320", # Thümmlitzwalde
      id == "14032510_1994" ~ "14082210", # Kriebstein
      id == "14032520_1994" ~ "14082410", # Striegistal
      id == "14032530_1994" ~ "07140150", # Tiefenbach
      id == "14033310_1994" ~ "14073040", # Chursbachtal
      id == "14035810_1994" ~ "14092480", # Schönteichen
      id == "14037710_1994" ~ "14079070", # Bienitz
      id == "14038610_1994" ~ "14086370", # Rosenbach
      id == "14039410_1994" ~ "14081190", # Hirtstein
      id == "14040710_1994" ~ "14080150", # Käbschütztal
      id == "14040720_1994" ~ "14080160", # Ketzerbachtal
      id == "14040730_1994" ~ "14080400", # Triebischtal
      id == "14041510_1994" ~ "14084300", # Quitzdorf am See
      id == "14041520_1994" ~ "14084440", # Waldhufen
      id == "14043610_1994" ~ "14089170", # Liebschützberg
      id == "14044810_1994" ~ "14087040", # Bahretal
      id == "14044820_1994" ~ "14087260", # Müglitztal
      id == "14047610_1994" ~ "14085270", # Röderaue
      id == "14047620_1994" ~ "14085110", # Hirschstein
      id == "14050410_1994" ~ "14087170", # Hohwald
      id == "14050420_1994" ~ "14087180", # Kirnitzschtal
      id == "14052510_1994" ~ "14089110", # Dreiheide
      id == "14052520_1994" ~ "14089250", # Pflückuff
      id == "14052530_1994" ~ "14089020", # Audenhain
      id == "14057310_1994" ~ "14081010", # Amtsberg
      id == "14058510_1994" ~ "14093030", # Crinitzberg
      # Thuriniga
      id == "16063047_1994" ~ "16016410", # Kupfersuhl
      id == "16063056_1994" ~ "16015420", # Möhra
      id == "16063057_1994" ~ "16063094", # Moorgrund
      id == "16068054_1994" ~ "16018580", # Töttelstädt
      id == "16069022_1994" ~ "16023360", # Heßberg
      id == "16073098_1994" ~ "16033700", # Weißen
      id == "16074023_1994" ~ "16041070", # Gernewitz
      TRUE ~ ags
    ),
    # 1. if ags is in ags_year_cw, use year - 1
    year_cw = case_when(
      # X. change year manually
      # NS
      id == "03355049_1991" ~ 1993, # Amt Neuhaus
      # # MV
      # id == "13053024_1999" ~ 1998, # Groß Nieköhr
      # id == "13053079_1999" ~ 1998, # Sabel
      # id == "13053084_1999" ~ 1998, # Striesdorf
      # id == "13055068_2009" ~ 2008, # Teschendorf
      # id == "13057045_1999" ~ 1998, # Kenz
      # id == "13057052_1999" ~ 1998, # Küstrow
      # id == "13062015_1999" ~ 1998, # Glashütte
      id == "13053108_2004" ~ 2004, # Prebberede
      # SA
      # id == "15159029_1994" ~ 1993, # Merzien
      # id == "15086270_2007" ~ 2006, # Zeppernick
      # id == "15089040_2007" ~ 2006, # Biere
      # id == "15089080_2007" ~ 2006, # Eggersdorf
      # id == "15089085_2007" ~ 2006, # Eickendorf
      # id == "15089160_2007" ~ 2006, # Großmühlingen
      # id == "15089190_2007" ~ 2006, # Kleinmühlingen
      # id == "15089335_2007" ~ 2006, # Welsleben
      # id == "15089370_2007" ~ 2006, # Zens
      id == "15087101_2008" ~ 2009, # Brücken-Hackpfüffel
      id == "15090635_2009" ~ 2010, # Zehrental
      id == "15090008_2009" ~ 2010, # Altmärkische Wische
      id == "15084442_2009" ~ 2010, # Schnaudertal
      id == "15083361_2009" ~ 2010, # Loitsche-Heinrichsberg
      # id == "15083362_2009" ~ 2010, # Niedere Börde
      id == "15084013_2009" ~ 2010, # Anhalt Süd
      id == "15084341_2009" ~ 2010, # Molauer Land
      # id == "15084014_2009" ~ 2010, # Molauer Land
      id == "15090003_2009" ~ 2010, # Aland
      id == "15090631_2009" ~ 2010, # Wust-Fischbeck
      id == "15083323_2009" ~ 2010, # Ingersleben
      id == "15085287_2009" ~ 2010, # Selke-Aue
      id == "15083557_2009" ~ 2010, # Westheide
      id == "15089041_2009" ~ 2010, # Bördeaue
      id == "15084207_2009" ~ 2010, # Gutenborn
      id == "15090007_2009" ~ 2010, # Altmärkische Höhe
      id == "15089043_2009" ~ 2010, # Börde-Hakel
      id == "15087031_2009" ~ 2010, # Arnstein
      id == "15087412_2009" ~ 2010, # Südharz
      id == "15089026_2009" ~ 2010, # Barby
      id == "15082256_2009" ~ 2010, # Osternienburger Land
      id == "15087386_2009" ~ 2010, # Seegebiet Mansfelder Land
      id == "15082301_2009" ~ 2010, # Raguhn-Jeßnitz
      id == "15082377_2009" ~ 2010, # Südliches Anhalt
      id == "15082241_2009" ~ 2010, # Muldestausee
      id == "15085228_2009" ~ 2010, # Oberharz am Brocken
      id == "15083411_2009" ~ 2010, # Oebisfelde-Weferlingen
      id == "15083298_2009" ~ 2010, # Hohe Börde
      
      # Saxony 1994 (the ones where we changed the ags)
      id == "14017410_1994" ~ 1994, # Neuhausen/ Erzgeb.
      id == "14018410_1994" ~ 1994, # Lichtenau
      id == "14019510_1994" ~ 1994, # Neukyhna
      id == "14020610_1994" ~ 1994, # Frauendorf
      id == "14022610_1994" ~ 1994, # Schönfeld-Weißig
      id == "14022620_1994" ~ 1994, # Promnitztal
      id == "14025410_1994" ~ 1994, # Bobritzsch
      id == "14027410_1994" ~ 1994, # Eulatal
      id == "14027420_1994" ~ 1994, # Langensteinbach
      id == "14029510_1994" ~ 1994, # Vierkirchen
      id == "14029520_1994" ~ 1994, # Schöpstal
      id == "14030710_1994" ~ 1994, # Parthenstein
      id == "14030720_1994" ~ 1994, # Bad Lausick
      id == "14030730_1994" ~ 1994, # Thümmlitzwalde
      id == "14032510_1994" ~ 1994, # Kriebstein
      id == "14032520_1994" ~ 1994, # Striegistal
      id == "14032530_1994" ~ 1994, # Tiefenbach
      id == "14033310_1994" ~ 1994, # Chursbachtal
      id == "14035810_1994" ~ 1994, # Schönteichen
      id == "14037710_1994" ~ 1994, # Bienitz
      id == "14038610_1994" ~ 1994, # Rosenbach
      id == "14039410_1994" ~ 1994, # Hirtstein
      id == "14040710_1994" ~ 1994, # Käbschütztal
      id == "14040720_1994" ~ 1994, # Ketzerbachtal
      id == "14040730_1994" ~ 1994, # Triebischtal
      id == "14041510_1994" ~ 1994, # Quitzdorf am See
      id == "14041520_1994" ~ 1994, # Waldhufen
      id == "14043610_1994" ~ 1994, # Liebschützberg
      id == "14044810_1994" ~ 1994, # Bahretal
      id == "14044820_1994" ~ 1994, # Müglitztal
      id == "14047610_1994" ~ 1994, # Röderaue
      id == "14047620_1994" ~ 1994, # Hirschstein
      id == "14050410_1994" ~ 1994, # Hohwald
      id == "14050420_1994" ~ 1994, # Kirnitzschtal
      id == "14052510_1994" ~ 1994, # Dreiheide
      id == "14052520_1994" ~ 1994, # Pflückuff
      id == "14052530_1994" ~ 1994, # Audenhain
      id == "14057310_1994" ~ 1994, # Amtsberg
      id == "14058510_1994" ~ 1994, # Crinitzberg
      id == "16063057_1994" ~ 1994, # Moorgrund
      # X. use 1998 for merging for ags in MeckPomm in 1999
      # id %in% not_merged_naive[not_merged_naive$election_year == 1999 & grepl("^13", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[not_merged_naive$election_year %in% c(2004, 2009) & grepl("^15", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[not_merged_naive$election_year %in% c(2009) & grepl("^15", not_merged_naive$id), ]$id ~ election_year + 1,
      id %in% not_merged_naive[not_merged_naive$election_year %in% c(2010) & grepl("^15", not_merged_naive$id), ]$id ~ election_year + 1,
      # X. use election_year - 1 for merging for ags in Saxony
      id %in% not_merged_naive[grepl("^12", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[grepl("^13", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[grepl("^14", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[grepl("^16", not_merged_naive$id), ]$id ~ election_year - 1,
      TRUE ~ election_year
    )
  )

# Merge crosswalks with election data -------------------------------------

# Merge crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags", "year_cw" = "year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)

# is there any ags that did not get merged to ags_21?
not_merged <- df_cw %>%
  filter(election_year < 2021) %>%
  filter(is.na(ags_21)) %>%
  select(ags, election_year, id, year_cw) %>%
  distinct()
not_merged %>%
  select(ags, election_year) %>%
  arrange(ags, election_year) %>%
  print(n=Inf)
# now, there is no unsuccessful merge.

# Flag the cases where we had to change the ags
df_cw <- df_cw |>
  mutate(
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

table(df_cw$flag_unsuccessful_naive_merge, useNA = "ifany")

glimpse(df_cw)



# Harmonize ---------------------------------------------------------------

# Weighted sums
sums <- df_cw |>
  group_by(ags_21, ags_name_21, election_year) |>
  summarize_at(
    # 1+2+3: Weighted sum
    vars(eligible_voters:valid_votes),
    ~ sum(.x * pop_cw, na.rm = TRUE)
  ) |>
  rename(
    ags = ags_21, year = election_year, ags_name = ags_name_21
  ) |>
  ungroup() |>
  mutate(
    turnout = number_voters / eligible_voters
  )

# Weighted mean
means <- df_cw %>%
  # replace NAs with 0
  mutate(across(cdu_csu:other, ~ ifelse(is.na(.), 0, .))) %>%
  group_by(ags_21, election_year) %>%
  summarize_at(
    # 4: Weighted mean
    vars(cdu_csu:other),
    ~ weighted.mean(.x, w = pop_cw, na.rm = TRUE)
  ) %>%
  rename(
    ags = ags_21, year = election_year
  ) %>%
  ungroup() %>%
  # replace 0 with NA for all replaced_ variables
  mutate(across(cdu_csu:other, ~ ifelse(. == 0, NA, .)))

# flags
flags <- df_cw |>
  group_by(ags_21, election_year) |>
  summarize_at(
    # for all that start with replaced_ take maximum
    vars(starts_with("replaced_"), flag_unsuccessful_naive_merge),
    ~ max(.x, na.rm = TRUE)
  ) |>
  rename(
    ags = ags_21, year = election_year
  ) |>
  ungroup() 


## Population & area: weighted sums ----------------------------------------

area_pop <- df_cw |>
  filter(election_year < 2021) |>
  group_by(ags_21, election_year) |>
  summarise(
    area = sum(area * area_cw, na.rm = TRUE),
    population = sum(population * pop_cw, na.rm = TRUE)
  ) |>
  # Round
  mutate(
    area = round(area, digits = 2),
    population = round(population, digits = 1)
  ) |>
  ungroup() |>
  rename(ags = ags_21, year = election_year)

# Get population & area for 2021
ags21 <- read_excel(path = "data/crosswalks/raw/31122021_Auszug_GV.xlsx", sheet = 2) |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    population = `...10`
  ) |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = as.numeric(paste0(Land, RB, Kreis, Gemeinde)),
    year = 2021,
    population = as.numeric(population) / 1000,
    area = as.numeric(area)
  ) |>
  slice(6:16065) |>
  filter(!is.na(Gemeinde)) |>
  select(ags, year, area, population)

# Create full df ----------------------------------------------------------


glimpse(sums)
glimpse(means)
glimpse(flags)
glimpse(area_pop)
glimpse(ags21)


# Merge
df_harm <- sums |>
  left_join_check_obs(means, by = c("ags", "year")) |>
  left_join_check_obs(flags, by = c("ags", "year")) |>
  left_join_check_obs(area_pop, by = c("ags", "year")) |>
  left_join_check_obs(ags21, by = c("ags", "year")) |>
  # Create state variable
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = str_sub(ags, end = -7),
    county = substr(ags, 1, 5)
  ) |>
  relocate(state, .after = year) |>
  relocate(county, .after = state) |>
  mutate(
    area = ifelse(!is.na(area.x), area.x, area.y),
    population = ifelse(!is.na(population.x), population.x, population.y)
  ) |>
  select(-c(area.x, area.y, population.x, population.y)) |>
  rename(election_year = year)

glimpse(df_harm)

# remove ags == NA
df_harm <- df_harm |>
  filter(!is.na(ags))


## save
fwrite(df_harm, "data/municipal_elections/final/municipal_harm.csv")
write_rds(df_harm, "data/municipal_elections/final/municipal_harm.rds")


# Create plot -------------------------------------------------------------

# Load municipality level data
muni <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7))

df_harm <- read_rds("data/municipal_elections/final/municipal_harm.rds")

muni_21 <- df_harm |>
  filter(election_year == 2021) |>
  dplyr::select(ags, cdu_csu, gruene, spd)

# count number of municipalities
df_harm |>
  distinct(ags) |>
  nrow()

# count number of election years
df_harm |>
  distinct(election_year) |>
  nrow()

# Merge
df_final <- muni |>
  left_join_check_obs(df_harm, by = c("ags", "year" = "election_year"))

# Create variable indicating whether there was election in given year
df_final <- df_final |>
  mutate(
    election_bin = ifelse(!is.na(turnout), 1, 0)
  )

glimpse(df_final)

# state variable
df_final <- df_final |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  )

table(df_harm$election_year)

# create plot_df
plot_df <- df_final |>
  group_by(state_name, year) |>
  summarise(election_bin = max(election_bin, na.rm = TRUE)) |>
  ungroup() |>
  # Add special handling for Saxony-Anhalt 2006-2009 and 2011
  mutate(
    election_bin = case_when(
      state_name == "Saxony-Anhalt" & year %in% c(2005:2008, 2010) ~ 2, # Use 2 for irregular elections
      TRUE ~ election_bin
    )
  )

plot_df |>
  ggplot(aes(x = as.numeric(year), 
             y = factor(state_name, 
                        levels = rev(levels(factor(state_name)))), 
             fill = as.factor(election_bin))
  ) +
  geom_tile(color = "white") + # Add borders to the squares
  scale_fill_manual(
    values = c("1" = "grey20", "0" = "white", "2" = "grey70"), # grey70 for irregular elections
    name = "Election",
    labels = c("1" = "Election Held", "0" = "Data Unavailable", "2" = "Irregular Election")
  ) +
  labs(x = "Year", y = "State") +
  theme_hanno() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(1990, 2021, 5))

ggsave("output/figures/muni_elections.pdf", width = 7, height = 4)

# Plot to show when AfD enters 

# Step 1: Identify AfD presence and classify state-year pairs
afd_plot_data <- df_final |>
  group_by(state_name, year) |>
  summarise(
    afd_present = any(afd != 0 | !is.na(afd), na.rm = TRUE),
    election_held = any(!is.na(election_bin) & election_bin == 1, na.rm = TRUE)
  ) |>
  mutate(
    category = case_when(
      !election_held ~ "No Election",                  # No election
      election_held & !afd_present ~ "Election No AfD", # Election but no AfD
      election_held & afd_present ~ "Election With AfD" # Election with AfD
    )
  ) |>
  ungroup()

# Step 2: Plot the data
afd_plot_data |>
  ggplot(aes(x = as.numeric(year), 
             y = factor(state_name, 
                        levels = rev(levels(factor(state_name)))), 
             fill = category)
  ) +
  geom_tile(color = "white") + # Add borders to the squares
  scale_fill_manual(
    values = c(
      "No Election" = "white", 
      "Election No AfD" = "grey", 
      "Election With AfD" = "#009EE0" # AfD blue
    ), 
    name = "Category",
    labels = c(
      "No Election" = "No Election", 
      "Election No AfD" = "Election but No AfD", 
      "Election With AfD" = "Election with AfD"
    )
  ) +
  labs(
    x = "Year", 
    y = "State"
  ) +
  theme_hanno() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(1990, 2021, 5))

ggsave("output/figures/muni_elections_afd.pdf", width = 7, height = 4)

# view(df_final %>%
#        filter(state_name == "Mecklenburg-Vorpommern", year == 2014, !is.na(afd)))


move_plots_to_overleaf("code")

### END