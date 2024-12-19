pacman::p_load(
    tidyverse, data.table, readxl, haschaR,
    conflicted, sf
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

rm(list = ls())

# Load 2021 data

df21 <- fread("data/federal_elections/municipality_level/raw/BTW21/btw21_wbz_ergebnisse.csv", encoding = "UTF-8") |>
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
    rename(
        BWBez = `Kennziffer Briefwahlzugehörigkeit`,
        BA = Bezirksart
    )

# Summarize all variables by ags & BWBez & Bezirksart
# (code from Vincent)

df21_bezirksarten <- df21 |>
    group_by(ags, BWBez, BA) |>
    summarise_at(vars(`Wahlberechtigte (A)`:Volt), sum, na.rm = TRUE) |>
    mutate(number_voters_orig = ifelse(BA == 5, 0, `Wählende (B)`)) |>
    relocate(number_voters_orig, .before = `Wählende (B)`)

# Get ags that have their own mailin data
# Hanno: the way I understand this, "own mailin data" means that the ags has only one mailin district - correct? @Vincent
ags_w_mailin21 <- df21_bezirksarten |>
    filter(BA == 5) |>
    pull(ags)

# Subset main data to ags with mailin data
df21_mailin <- df21_bezirksarten |>
    filter(ags %in% ags_w_mailin21)

# What do the fields mean?

# Anzahl der Wahlberechtigten (A)
# Anzahl der Wahlberechtigten ohne Sperrvermerk (Al) Anzahl der Wahlberechtigten mit Sperrvermerk (A2)
# Anzahl der Wahlberechtigten nach § 25 Abs. 2 (A3)
# Anzahl der Wähler (B)
# darunter: Anzahl der Wähler mit Wahlschein (B1)

# Get centroids of the munis in question
# @Vincent: this is not used right now, you can disregard it

centroids <- sf::st_read("data/shapefiles/2021/vg250_ebenen_0101/VG250_GEM.shp") %>%
    st_transform(crs = 4326) %>%
    filter(AGS %in% ags_w_mailin21) %>%
    st_centroid() %>%
    mutate(
        coords = st_coordinates(.),
        lon = coords[, 1],
        lat = coords[, 2]
    ) %>%
    dplyr::select(-coords) %>%
    dplyr::select(AGS, lon, lat) %>%
    dplyr::rename(ags = AGS) %>%
    distinct(ags, .keep_all = TRUE)

centroids$geometry <- NULL

# Merge centroids to df21

df21_mailin <- df21_mailin %>%
    left_join_check_obs(centroids, by = "ags")

# Population data ---------------------------------------------------------

# Load population data
# @Vincent: also not used for now, you can disregard it

pop21 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122021_Auszug_GV.xlsx",
    sheet = 2, col_types = "numeric"
) |>
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

# Merge pop data to df21

df21_mailin <- df21_mailin |>
    left_join_check_obs(pop21, by = "ags")

## Rename some variables -----------------------

df21_mailin <- df21_mailin %>%
    dplyr::rename(
        A = `Wahlberechtigte (A)`,
        A2 = `Wahlberechtigte mit Sperrvermerk (A2)`,
        A3 = `Wahlberechtigte nach § 25 Abs. 2 BWO (A3)`,
        B = `Wählende (B)`,
        B1 = `Wählende mit Wahlschein (B1)`
    )

# @Vincent: below is where I am not sure if I am doing this right

# Data frame that is just briefwahlbezirke

df21_mailin_bwbez <- df21_mailin %>%
    filter(BA == 5) %>%
    filter(BWBez == "00")

# Data frame that is !briefwahlbezirke

df21_mailin_not_bwbez <- df21_mailin %>%
    filter(BA != 5)

# Vincent :

# Mein Ziel ist es hier, dass wir ein DF haben was nur Briefwahlbezirke enthält (fuer Gemeinden, die genau einen Briefwahlbezirk haben)
#   das ist: df21_mailin_bwbez

# Und dass wir ein df, wo die non-mailin votes drin sind. allerdings nur fuer gemeinden, die nur einen briefwahlbezirk haben.
#   das ist: df21_mailin_not_bwbez

# Allerdings erwarte ich, dass df21_mailin_bwbez und df21_mailin_not_bwbez genau die gleiche Anzahl an Zeilen haben -- das ist aber nicht der Fall.

# Kannst du da helfen?

# @Vincent : Code below is your old code, you can disregard it.


# Other Vincent code ------------------------------------------------------

# Merge with BTW data
df21 <- df21 |>
    left_join(pop21, by = "ags") |>
    mutate(county = substr(ags, 1, 5)) |>
    # calculate county-VG population & area & eligible voters
    # for ags with unique mailin
    group_by(county, BWBez, unique_mailin) |>
    mutate(
        county_bwbez_pop = sum(pop, na.rm = T),
        county_bwbez_area = sum(area, na.rm = T),
        county_bwbez_voters = sum(A, na.rm = T),
        county_bwbez_blocked = sum(A2 + A3, na.rm = T)
    ) |>
    ungroup() |>
    # calculate weights (i.e. shares)
    mutate(
        pop_weight = pop / county_bwbez_pop,
        area_weight = area / county_bwbez_area,
        voters_weight = A / county_bwbez_voters,
        eligible_voters_orig = A,
        blocked_weight = (A2 + A3) / county_bwbez_blocked,
        blocked_voters_orig = A2 + A3
    )

# # Inspect
# inspect <- df21 |>
#   filter(county == "01213") |>
#   select(ags, BWBez, unique_mailin, pop, county_bwbez_pop)
# # works!

# mail-in counties in long format
mailin21_long <- df21 |>
    filter(str_sub(ags, -3, -3) == "9") |>
    # add number_voters to eligible_voters
    rowwise() |>
    mutate(A = B) |>
    ungroup() |>
    select(c(A:Volt, county, BWBez)) |>
    # pivot longer
    pivot_longer(
        cols = A:Volt,
        names_to = "var",
        values_to = "mailin_value"
    )


# Distribute multi mail-in votes across ags by population weight
df21_long <- df21 |>
    # pivot longer
    pivot_longer(
        cols = A:Volt,
        names_to = "var",
        values_to = "ags_value"
    ) |>
    left_join(mailin21_long, by = c("county", "BWBez", "var")) |>
    rowwise() |>
    mutate(
        # weight multi mail-in values by eligible voters share
        # but only for the ones that have shared mail-in
        weighted_value = round((mailin_value * blocked_weight), digits = 0),
        # add to original ags value
        ags_value_v2 = ifelse(
            unique_mailin == 0,
            sum(ags_value, weighted_value, na.rm = T),
            ags_value
        )
    )
# Inspect
inspect_dupls <- df21_long |> filter(ags == "01053010")
# works!

# Bring back to wide format
df21 <- df21_long |>
    select(-ags_value) |>
    rename(ags_value = ags_value_v2) |>
    select(-c(mailin_value, weighted_value)) |>
    # pivot back to wide format
    pivot_wider(
        names_from = var,
        values_from = ags_value
    ) |>
    # remove multi mail-in ags
    filter(str_sub(ags, -3, -3) != "9") |>
    select(-BWBez)

# Check duplicates
dupl <- df21 |>
    count(ags, election_year) |>
    filter(n > 1)
nrow(dupl)
###
# Berlin ags have multiple multi mail-in districts causing duplicates.
# Therefore, the code that follows is different from the code before.

# Sum up
df21 <- df21 |>
    group_by(ags) |>
    mutate_at(vars(eligible_voters_orig:Volt, county_bwbez_voters, county_bwbez_blocked), sum, na.rm = TRUE) |>
    ungroup() |>
    distinct()
