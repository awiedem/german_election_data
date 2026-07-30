### Harmonize municipal electoral results to 2025 borders
# Vincent Heddesheimer
# First: Mar 05, 2025

rm(list = ls())

conflicts_prefer(dplyr::filter)

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# Set working directory if running 01_municipal_unharm.R before this
setwd(here::here())

# Read crosswalk files ----------------------------------------------------
cw <- read_rds("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds") |>
  mutate(ags = pad_zero_conditional(ags, 7))

# Add the two post-2020 RLP codes manually (mirrors 02_municipal_harm.R). The
# Statistisches Landesamt Rheinland-Pfalz delivers its whole 1969-2019
# Gemeinderatswahl series on 2025 boundaries, so two Gemeinden carry codes the
# 1990-2020 part of the crosswalk does not know:
#   07132502 Neitersen   (= Neitersen + Obernau, merged 2021)
#   07232503 Obergeckler (= Niedergeckler + Obergeckler, merged 2025)
# Both ARE valid 2025 units, so on 2025 boundaries they simply map to
# themselves.  Without this block they had zero rows in municipal_harm_25 in
# every election year.
cw_rp_2025 <- expand.grid(
  year   = sort(unique(cw$year)),
  ags_25 = c("07132502", "07232503"),
  stringsAsFactors = FALSE
) |>
  mutate(
    ags         = ags_25,
    ags_name    = c("07132502" = "Neitersen", "07232503" = "Obergeckler")[ags_25],
    ags_name_25 = ags_name,
    pop_cw      = 1,
    area_cw     = 1,
    # area/population are the sums of the merged constituents
    area        = c("07132502" = 7.14, "07232503" = 7.75)[ags_25],
    population  = c("07132502" = 1.10, "07232503" = 0.20)[ags_25]
  )
for (col in names(cw)) {
  if (!col %in% names(cw_rp_2025)) cw_rp_2025[[col]] <- NA
  cw_rp_2025[[col]] <- methods::as(cw_rp_2025[[col]], class(cw[[col]])[1])
}
cw <- bind_rows(
  cw,
  cw_rp_2025[, names(cw)] |>
    # never shadow a code the real crosswalk already carries for that year
    anti_join(cw |> distinct(ags, year), by = c("ags", "year"))
)

# Get population & area for 2025 (also used to describe the pre-1990 RP rows
# added below)
ags25 <- read_excel(
  "data/covars_municipality/raw/municipality_sizes/AuszugGV4QAktuell_2024.xlsx",
  sheet = 2
) |>
  slice(9:16018) |>
  select(
    Land = `...3`,
    RB   = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    area = `...9`,
    population  = `...10`
  ) |>
  filter(!is.na(Gemeinde)) |>
  mutate(
    Land     = pad_zero_conditional(Land, 1),
    Kreis    = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags = as.numeric(paste0(Land, RB, Kreis, Gemeinde)),
    year = 2025,
    population = as.numeric(population) / 1000,
    area = as.numeric(area)
  ) |>
  filter(!is.na(Gemeinde)) |>
  select(ags, year, area, population)

# Rheinland-Pfalz before 1990: the StaLA Sonderauswertung back-casts the whole
# 1969-2019 series onto 2025 boundaries, so the pre-1990 rows are ALREADY
# harmonised and only need to be carried through as identity — exactly the
# treatment the >= 2025 rows get at the end of this script. They were dropped
# outright before (11,474 rows, 1969-1989). Restricted to state 07: pre-1990
# rows of every other state (HE 1989, BW 1989, SL 1984/1989) are on their own
# election-year boundaries and must NOT be passed through.
rp_pre1990_years <- c(1969, 1974, 1979, 1984, 1989)
cw_rp_pre1990 <- cw |>
  filter(str_sub(ags_25, 1, 2) == "07") |>
  distinct(ags_25, ags_name_25) |>
  group_by(ags_25) |>
  slice(1) |>
  ungroup() |>
  tidyr::crossing(year = rp_pre1990_years) |>
  transmute(
    ags = ags_25, ags_name = ags_name_25, year = as.integer(year),
    ags_25, ags_name_25, pop_cw = 1, area_cw = 1
  ) |>
  left_join(
    ags25 |> transmute(ags_25 = pad_zero_conditional(ags, 7), area, population),
    by = "ags_25"
  )
for (col in names(cw)) {
  if (!col %in% names(cw_rp_pre1990)) cw_rp_pre1990[[col]] <- NA
  cw_rp_pre1990[[col]] <- methods::as(cw_rp_pre1990[[col]], class(cw[[col]])[1])
}
cw <- bind_rows(cw, cw_rp_pre1990[, names(cw)])

# how many ags_25 for each year?
cw |>
  distinct(ags_25, year) |>
  count(year) |>
  print(n = Inf)

# Merge with unharmonized election data -----------------------------------

df <- readr::read_rds("data/municipal_elections/final/municipal_unharm.rds") |>
  mutate(election_year = as.numeric(election_year)) |>
  # Years before 1990 have no crosswalk, EXCEPT Rheinland-Pfalz: the StaLA
  # Sonderauswertung reports its 1969-1989 Gemeinderatswahlen on 2025
  # boundaries, so those rows pass through as identity (cw_rp_pre1990 above).
  filter(
    election_year >= 1990 |
      (str_sub(ags, 1, 2) == "07" & election_year %in% rp_pre1990_years)
  )

# look at how many observations for each state and year
df |>
  group_by(state, election_year) |>
  summarise(n = n()) |>
  arrange(state, election_year) |>
  print(n = Inf)

# how many obs per year?
df |>
  group_by(election_year) |>
  summarise(n = n()) |>
  print(n = Inf)

glimpse(df)
glimpse(cw)
table(df$election_year, useNA = "ifany")

# inspect -----------------------------------------------------------------

# is there more than one election in one ags in one year?
dupl <- df |>
  group_by(ags, ags_name, election_year) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  print(n = Inf) |>
  mutate(id = paste0(ags, "_", election_year))

# Merge w/ cw -------------------------------------------------------------

# bind with crosswalks
df_naive_merge <- df |>
  left_join_check_obs(cw |> select(-ags_name), by = c("ags", "election_year" = "year")) |>
  arrange(ags, election_year)
# number of obs increases: but this is wanted, as we want to harmonize the data

# is there any ags that did not get merged to ags_25?
not_merged_naive <- df_naive_merge %>%
  filter(election_year < 2025) %>%
  filter(is.na(ags_25)) %>%
  select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))
not_merged_naive


# Dealing with unsuccessful mergers ---------------------------------------

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
      # TODO(audit 2026-07): see the same line in 02_municipal_harm.R — 03361013
      # is Gemeinde Thedinghausen (only its 2001 row is named "Riede"), so this
      # remap adds 9,595 valid votes to Riede 03361010 in 2001 and leaves
      # Thedinghausen without a 2001 observation. Likely fix: year_cw = 2006.
      id == "03361013_2001" ~ "03361010", # Riede
      id == "05313000_2009" ~ "05334002", # Aachen
      id == "05313000_2014" ~ "05334002", # Aachen
      id == "05313000_2020" ~ "05334002", # Aachen
      id == "05313000_2025" ~ "05334002", # Aachen
      # Rheinland-Pfalz: the StaLA Sonderauswertung reports the whole 1969-2019
      # series on 2025 boundaries, so these five Gemeinden appear under codes
      # that only enter the crosswalk part-way through its span (07140502/03/04
      # from 2014, 07232502 from 2018, 07235207 from 2012). Election years
      # between 1990 and that cut-off have to be looked up under the code of the
      # day; keying this on the id (= ags + year) covered only 1994/1999
      # (1994-2009 for Trittenheim) and dropped 2004/2009/2014 silently.
      # Pre-1990 RP rows are excluded: they pass through as identity because the
      # source already reports them on 2025 boundaries.
      ags == "07140502" & election_year >= 1990 & election_year < 2014 ~ "07135050", # Lahr
      ags == "07140503" & election_year >= 1990 & election_year < 2014 ~ "07135063", # Mörsdorf
      ags == "07140504" & election_year >= 1990 & election_year < 2014 ~ "07135094", # Zilshausen
      ags == "07232502" & election_year >= 1990 & election_year < 2018 ~ "07232021", # Brimingen
      ags == "07235207" & election_year >= 1990 & election_year < 2012 ~ "07231207", # Trittenheim
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
      # Tiefenbach b. Döbeln. Was mapped onto 07140150 (Tiefenbach in
      # Rheinland-Pfalz!) until 2026-07, which moved 6,219 Saxon votes into RP.
      id == "14032530_1994" ~ "14082450", # Tiefenbach -> Striegistal
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
      # Thuringia
      id == "16063047_1994" ~ "16016410", # Kupfersuhl
      id == "16063056_1994" ~ "16015420", # Möhra
      id == "16063057_1994" ~ "16063094", # Moorgrund
      id == "16068054_1994" ~ "16018580", # Töttelstädt
      id == "16069022_1994" ~ "16023360", # Heßberg
      id == "16073098_1994" ~ "16033700", # Weißen
      id == "16074023_1994" ~ "16041070", # Gernewitz
      TRUE ~ ags
    ),
    # year_cw: adjust crosswalk year for unsuccessful merges
    year_cw = case_when(
      # NS
      id == "03355049_1991" ~ 1993, # Amt Neuhaus
      # MV
      id == "13053108_2004" ~ 2004, # Prebberede
      # SA
      id == "15087101_2008" ~ 2009, # Brücken-Hackpfüffel
      id == "15090635_2009" ~ 2010, # Zehrental
      id == "15090008_2009" ~ 2010, # Altmärkische Wische
      id == "15084442_2009" ~ 2010, # Schnaudertal
      id == "15083361_2009" ~ 2010, # Loitsche-Heinrichsberg
      id == "15084013_2009" ~ 2010, # Anhalt Süd
      id == "15084341_2009" ~ 2010, # Molauer Land
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
      # Generic rules for remaining unsuccessful merges
      id %in% not_merged_naive[not_merged_naive$election_year %in% c(2004, 2009) & grepl("^15", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[not_merged_naive$election_year %in% c(2009) & grepl("^15", not_merged_naive$id), ]$id ~ election_year + 1,
      id %in% not_merged_naive[not_merged_naive$election_year %in% c(2010) & grepl("^15", not_merged_naive$id), ]$id ~ election_year + 1,
      id %in% not_merged_naive[grepl("^12", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[grepl("^13", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[grepl("^14", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[grepl("^16", not_merged_naive$id), ]$id ~ election_year - 1,
      TRUE ~ election_year
    )
  )

# Guard the manual AGS remaps ---------------------------------------------
# Every hand-written remap above must (a) stay inside its own Bundesland and
# (b) point at a code the crosswalk knows for the year it is looked up in. This
# is what would have caught Saxon Tiefenbach 14032530 being rewritten to
# 07140150 — Tiefenbach in Rheinland-Pfalz — which parked 6,219 Saxon votes in
# RP in both harm files.
cw_keys <- cw |> distinct(ags, year)
remapped <- df |>
  mutate(ags_orig = str_sub(id, 1, 8)) |>
  filter(ags != ags_orig) |>
  distinct(ags_orig, ags, election_year, year_cw)
bad_remap <- bind_rows(
  remapped |>
    filter(str_sub(ags, 1, 2) != str_sub(ags_orig, 1, 2)) |>
    mutate(problem = "remap crosses state border"),
  remapped |>
    # only pre-2025 rows are looked up in `cw`; 2025+ is already on 2025
    # boundaries and is bound unharmonised further down
    filter(election_year < 2025) |>
    anti_join(cw_keys, by = c("ags", "year_cw" = "year")) |>
    mutate(problem = "remap target absent from crosswalk at year_cw")
)
if (nrow(bad_remap) > 0) {
  print(as.data.frame(bad_remap %>% arrange(ags_orig)))
  stop(nrow(bad_remap), " invalid manual AGS remap(s) — see table above")
}
cat("[OK] all", nrow(remapped), "manual AGS remaps stay in-state and resolve in the crosswalk\n")

# Merge crosswalks with election data -------------------------------------

# Merge crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags", "year_cw" = "year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)

# is there any ags that did not get merged to ags_25?
# HARD STOP: an AGS the crosswalk cannot place is silently deleted from the
# output together with all of its votes (this is how Neitersen and Obergeckler
# went missing from every year of municipal_harm_25, and the four other
# Rheinland-Palatine StaLA codes from 2004/2009/2014).
# NOTE: until 01_municipal_unharm.R drops them, this fires on the three
# Niedersachsen Samtgemeinde AGGREGATES that escape the ", SG" name filter
# because the raw name column is width-truncated (03255409 Eschershausen-
# Stadtoldendorf 22,270, 03354407 Lüchow (Wendland) 36,809, 03359409
# Oldendorf-Himmelpforten 27,942 = 87,021 double-counted votes in 2021). They
# must be removed at source, NOT allowlisted here.
allowed_unmatched <- character(0) # (ags, election_year) ids allowed to fail
not_merged <- df_cw %>%
  filter(election_year < 2025) %>%
  filter(is.na(ags_25)) %>%
  select(ags, ags_name.x, election_year, id, year_cw, valid_votes) %>%
  distinct()
if (nrow(not_merged) > 0) {
  print(as.data.frame(not_merged %>% arrange(ags, election_year)), max = 2000)
  cat("unmatched rows:", nrow(not_merged),
      "| valid votes at stake:", sum(not_merged$valid_votes, na.rm = TRUE), "\n")
}
stopifnot(all(not_merged$id %in% allowed_unmatched))

# Every source row must hand out exactly 100% of itself across its 2025 targets.
# Grouped on `id` (the ORIGINAL ags + year), not on the possibly remapped ags:
# several source rows may legitimately be routed through one crosswalk entry.
w_chk <- df_cw %>%
  filter(election_year < 2025, !is.na(ags_25)) %>%
  group_by(id) %>%
  summarise(w = sum(pop_cw, na.rm = TRUE), .groups = "drop") %>%
  filter(abs(w - 1) > 0.01)
if (nrow(w_chk) > 0) print(as.data.frame(w_chk), max = 2000)
stopifnot(nrow(w_chk) == 0)

# Flag the cases where we had to change the ags
df_cw <- df_cw |>
  mutate(
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

table(df_cw$flag_unsuccessful_naive_merge, useNA = "ifany")

glimpse(df_cw)



# Harmonize ---------------------------------------------------------------

# Canonical 2025 name per code. The name must NEVER be part of the grouping
# key: a stray variant ("Glowe, Seebad" alongside "Glowe, Ostseebad") split the
# group and left the municipality as two half-sized duplicate rows — the only
# duplicate (ags, election_year) keys the harmonised files ever had.
ags_names_25 <- df_cw |>
  filter(!is.na(ags_25)) |>
  distinct(ags_25, ags_name_25) |>
  group_by(ags_25) |>
  slice(1) |>
  ungroup()

# Weighted sums
sums <- df_cw |>
  filter(election_year < 2025) |>  # Only harmonize years before 2025
  group_by(ags_25, election_year) |>
  summarize_at(
    # 1+2+3: Weighted sum
    vars(eligible_voters:valid_votes),
    ~ sum(.x * pop_cw, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(ags_names_25, by = "ags_25") |>
  rename(
    ags = ags_25, year = election_year, ags_name = ags_name_25
  ) |>
  relocate(ags_name, .after = ags) |>
  mutate(
    turnout = number_voters / eligible_voters
  )

# Weighted mean
means <- df_cw %>%
  filter(election_year < 2025) |>  # Only harmonize years before 2025
  # replace NAs with 0
  mutate(across(cdu_csu:other, ~ ifelse(is.na(.), 0, .))) %>%
  group_by(ags_25, election_year) %>%
  summarize_at(
    # 4: Weighted mean
    vars(cdu_csu:other),
    ~ weighted.mean(.x, w = pop_cw, na.rm = TRUE)
  ) %>%
  rename(
    ags = ags_25, year = election_year
  ) %>%
  ungroup() %>%
  # replace 0 with NA for all replaced_ variables
  mutate(across(cdu_csu:other, ~ ifelse(. == 0, NA, .)))

# flags
flags <- df_cw |>
  filter(election_year < 2025) |>  # Only harmonize years before 2025
  group_by(ags_25, election_year) |>
  summarize_at(
    # for all that start with replaced_ take maximum
    vars(starts_with("replaced_"), flag_unsuccessful_naive_merge),
    ~ max(.x, na.rm = TRUE)
  ) |>
  rename(
    ags = ags_25, year = election_year
  ) |>
  ungroup()

## Population & area: weighted sums ----------------------------------------

area_pop <- df_cw |>
  filter(election_year < 2025) |>
  group_by(ags_25, election_year) |>
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
  rename(ags = ags_25, year = election_year)

# Create full df ----------------------------------------------------------


glimpse(sums)
glimpse(means)
glimpse(flags)
glimpse(area_pop)
glimpse(ags25)


# Merge harmonized data
df_harm <- sums |>
  left_join_check_obs(means, by = c("ags", "year")) |>
  left_join_check_obs(flags, by = c("ags", "year")) |>
  left_join_check_obs(area_pop, by = c("ags", "year")) |>
  # Convert ags to numeric for compatibility
  mutate(ags = as.numeric(ags)) |>
  # Bind 2025+ data unharmonized (already on 2025 boundaries; incl. the 2026 HE
  # Kommunalwahl, whose AGS are 2025-boundary codes)
  bind_rows(df_cw |>
    filter(election_year >= 2025) |>
    mutate(ags_name = ags_name.x) |>
    select(-any_of(c("ags_name.x", "ags_name.y", "ags_name_25", "year_cw", "id"))) |>
    rename(year = election_year) |>
    mutate(ags = as.numeric(ags))) |>
  # Create state variable
  mutate(
    ags = pad_zero_conditional(ags, 7),
    state = str_sub(ags, end = -7),
    county = substr(ags, 1, 5)
  ) |>
  relocate(state, .after = year) |>
  relocate(county, .after = state) |>
  mutate(ags = as.numeric(ags)) |>
  # Merge with 2025 area and population data
  left_join_check_obs(ags25, by = c("ags", "year")) |>
  mutate(
    area = ifelse(!is.na(area.x), area.x, area.y),
    population = ifelse(!is.na(population.x), population.x, population.y)
  ) |>
  select(-c(area.x, area.y, population.x, population.y)) |>
  rename(election_year = year) |>
  arrange(ags, election_year) |>
  mutate(ags = pad_zero_conditional(ags, 7))

glimpse(df_harm)

# remove ags == NA
df_harm <- df_harm |>
  filter(!is.na(ags))

glimpse(df_harm)

## Add state_name and relocate metadata to the front
df_harm <- df_harm |>
  mutate(state_name = haschaR::state_id_to_names(substr(as.character(ags), 1, 2))) |>
  dplyr::relocate(dplyr::any_of(c(
    "ags", "election_year", "election_date",
    "ags_name", "state_name", "state"
  )))

## save
fwrite(df_harm, "data/municipal_elections/final/municipal_harm_25.csv")
write_rds(df_harm, "data/municipal_elections/final/municipal_harm_25.rds")


# Create plot -------------------------------------------------------------

# Load municipality level data
# muni <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
#   rename(ags = ags_21) |>
#   mutate(ags = pad_zero_conditional(ags, 7))

df_harm <- read_rds("data/municipal_elections/final/municipal_harm_25.rds")

# look at how many obs per year
df_harm |>
  group_by(election_year) |>
  summarise(n = n()) |>
  print(n = Inf)

# look at how many observations for each state and year
df_harm |>
  group_by(state, election_year) |>
  summarise(n = n()) |>
  arrange(state, election_year) |>
  print(n = Inf)

# count number of municipalities
df_harm |>
  distinct(ags) |>
  nrow()

# count number of election years
df_harm |>
  distinct(election_year) |>
  nrow()

# check number of munis in schleswig holstein per year
df_harm %>%
  filter(state == "01") %>%
  group_by(election_year) %>%
  summarise(n = n_distinct(ags)) %>%
  print(n = Inf)

### END
