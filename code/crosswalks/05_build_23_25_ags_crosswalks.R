# Create 2023-2025 crosswalk & 1990-2025 crosswalk
# Vincent Heddesheimer

rm(list = ls())

options(scipen = 999)
pacman::p_load(tidyverse, readxl, data.table, janitor)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

# paths ------------------------------------------------------
path_24 <- "data/crosswalks/raw/2024.xlsx"
path_25 <- "data/crosswalks/raw/2025-02.xlsx"
muni_2023 <- "data/covars_municipality/raw/municipality_sizes/31122023_Auszug_GV.xlsx" # pop, area, employees for 2023
muni_2024 <- "data/covars_municipality/raw/municipality_sizes/AuszugGV4QAktuell_2024.xlsx" # pop, area, employees for 2024

# helper: slim column names for the StatBA sheets -----------
std_names <- c(
    "event_id", "entity", "ars_old", "ags_old",
    "name_old", "law_short",
    "area_old", "pop_old",
    "ars_new", "ags_new",
    "name_new", "effective", "stat_effective"
)

read_changes <- function(path) {
    read_excel(path,
        sheet = "Gebietsaenderungen",
        col_names = FALSE
    ) %>%
        slice(-(1:4)) %>% # drop title rows
        set_names(std_names[seq_len(ncol(.))]) %>%
        filter(!is.na(ags_old)) # 14/2024 sheets use D.M.YYYY
}

# Read changes ------------------------------------------------------------
chg_2024 <- read_changes(path_24) # 1 Jan 2024 – 31 Dec 2024
chg_2025 <- read_changes(path_25) # 1 Jan 2025 – 28 Feb 2025
changes <- bind_rows(chg_2024, chg_2025)

# Classify changes ------------------------------------------------------------
changes <- changes %>%
    mutate(
        type = case_when(
            ags_new == ags_old ~ "boundary_shift",
            ags_new != ags_old & !is.na(ags_new) ~ "merge_or_split",
            is.na(ags_new) ~ "dissolution" # rare; handled later
        )
    )

# Helpers ---------------------------------------------------------------------

## Robust parser for the "Wirksamkeitsdatum" column: the StatBA sheets ship it
## as dd.mm.yyyy text, but older/newer vintages have used real dates and Excel
## serial numbers.  Only the ORDER of the events matters downstream.
parse_effective <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    d <- suppressWarnings(as.Date(as.character(x), format = "%d.%m.%Y"))
    ser <- suppressWarnings(as.numeric(as.character(x)))
    fill <- is.na(d) & !is.na(ser)
    if (any(fill)) d[fill] <- as.Date(ser[fill], origin = "1899-12-30")
    d
}

## Weight of a crosswalk row = the share of the DONOR municipality that moves to
## the target.  The "Gebietsaenderungen" sheet lists one row per
## (abgebende Gemeinde -> aufnehmende Gemeinde) pair, and the Aenderungsart
## (`law_short`) tells us how the rows of one donor relate to each other:
##   1 = Eingliederung / Aufloesung — the donor's rows together account for the
##       WHOLE donor (Rodeberg 16064055: 1,552 inhabitants to Dingelstaedt and
##       481 to Muehlhausen, i.e. a genuine 76/24 split);
##   2 = Umgliederung von Gebietsteilen — one row per transferred fragment PLUS a
##       residual row with ags_new == ags_old carrying whatever stays behind
##       (Bad Neuenahr-Ahrweiler 07131007: 13.7 ha / 35 inhabitants to Dernau,
##       63.2 km2 / 26,634 inhabitants stay);
##   3/4 = Schluesselaenderung / Umbenennung — identity-shaped rows without any
##       population or area figures.
## Normalising by the donor therefore works uniformly for all four.
##
## Until 2026-07 this script grouped by (event_id, ags_new) instead, which
## computes the TARGET's composition: every single-donor partial transfer came
## out with weight 1 (so Bad Neuenahr-Ahrweiler was "half Dernau" once the final
## rescale had divided the pair by 2), and every merger constituent carried the
## target-share instead of 1.
compute_change_weights <- function(m) {
    m %>%
        group_by(event_id, ags_old) %>%
        mutate(
            is_identity = ags_new == ags_old,
            has_identity = any(ags_new == ags_old),
            n_rows = n(),
            pop_total = sum(pop_old, na.rm = TRUE),
            area_total = sum(area_old, na.rm = TRUE),
            area_cw = case_when(
                area_total > 0 ~ coalesce(area_old, 0) / area_total,
                # no figures at all: a residual row keeps everything, otherwise
                # split the (empty) unit evenly
                has_identity ~ as.numeric(is_identity),
                TRUE ~ 1 / n_rows
            ),
            pop_cw = case_when(
                pop_total > 0 ~ coalesce(pop_old, 0) / pop_total,
                has_identity ~ as.numeric(is_identity),
                # gemeindefreie Gebiete / Forsten have 0 inhabitants in every
                # row; weighting them by area keeps the crosswalk normalised
                # (they carry no election data, so the split is vacuous)
                TRUE ~ area_cw
            )
        ) %>%
        ungroup()
}

## A Gemeinde can appear as donor in more than one event between the base year
## and 2025.  Those events are SEQUENTIAL and must be composed, not stacked:
## Boergerende-Rethwisch 13072017 and Glowe 13073030 were each renamed twice
## (", Seebad" in 2023, ", Ostseebad" in 2024), which produced two identity rows
## of weight 1 (sum 2.0) and, after the final rescale, two 0.5 rows — the only
## duplicate (ags, year, ags_25) keys in any harmonised municipal file.
collapse_change_events <- function(m) {
    m %>%
        mutate(eff = parse_effective(effective)) %>%
        arrange(ags_old, ags_new, eff, event_id) %>%
        group_by(ags_old, ags_new) %>%
        summarise(
            name_old = dplyr::first(name_old),
            name_new = dplyr::last(name_new), # the name in force in 2025
            pop_cw = prod(pop_cw),
            area_cw = prod(area_cw),
            # kept only as a fallback for area/population when a donor is absent
            # from the base GV extract (should never happen)
            pop_old = dplyr::last(pop_old),
            area_old = dplyr::last(area_old),
            n_events = dplyr::n_distinct(event_id),
            .groups = "drop"
        )
}

## Hard stop: every source (ags, year) must distribute exactly 100% of itself,
## and no (ags, year, ags_25) key may appear twice.
assert_crosswalk_valid <- function(x, label, tol = 0.01) {
    sums <- x %>%
        group_by(ags, year) %>%
        summarise(
            pop = sum(pop_cw, na.rm = TRUE),
            area = sum(area_cw, na.rm = TRUE),
            .groups = "drop"
        )
    bad <- sums %>% filter(abs(pop - 1) > tol | abs(area - 1) > tol | is.na(pop) | is.na(area))
    if (nrow(bad) > 0) {
        print(as.data.frame(bad %>% arrange(ags, year) %>% head(100)))
        stop(sprintf(
            "%s: %d (ags, year) groups whose weights do not sum to 1 (tol %.3f)",
            label, nrow(bad), tol
        ))
    }
    dup <- x %>% count(ags, year, ags_25) %>% filter(n > 1)
    if (nrow(dup) > 0) {
        print(as.data.frame(dup %>% head(100)))
        stop(sprintf("%s: %d duplicate (ags, year, ags_25) keys", label, nrow(dup)))
    }
    cat(sprintf(
        "[OK] %s: %d rows, %d (ags, year) groups, all weights sum to 1, no duplicate keys\n",
        label, nrow(x), nrow(sums)
    ))
    invisible(x)
}

# Build crosswalk ------------------------------------------------------------

## --- start from every 2023 municipality -------------
muni_2023 <- read_excel(muni_2023, sheet = 2) %>% # ags, name, pop_2023, area_2023, employees_2023 …
    select(
        Land = `...3`,
        RB = `...4`,
        Kreis = `...5`,
        Gemeinde = `...7`,
        Gemeindename = `...8`,
        area = `...9`,
        population = `...10`
    ) |>
    slice(9:16041) |>
    filter(!is.na(Gemeinde)) |>
    mutate(
        Land = pad_zero_conditional(Land, 1),
        Kreis = pad_zero_conditional(Kreis, 1),
        Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
        Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
        ags = paste0(Land, RB, Kreis, Gemeinde),
        population = as.numeric(population) / 1000,
        area = as.numeric(area),
        year = 2023,
        ags_name = Gemeindename
    ) |>
    mutate(
        ags_25 = ags, # initialise: identity mapping
        ags_name_25 = ags_name,
        pop_cw = 1, area_cw = 1
    ) |>
    select(ags, ags_name, year, ags_25, ags_name_25, pop_cw, area_cw, area, population)

glimpse(muni_2023)


### Apply changes ------------------------------------------------------------

#### One  to one changes ------------------------------------------------------------

cw_23_25 <- muni_2023 %>%
    left_join(
        changes %>% filter(type == "boundary_shift") %>%
            select(ags_old, ags_new, name_new),
        by = c("ags" = "ags_old")
    ) %>%
    mutate(
        ags_25 = coalesce(ags_new, ags_25),
        ags_name_25 = coalesce(name_new, ags_name_25)
    ) %>%
    select(-ags_new, -name_new)

#### Many to one mergers ------------------------------------------------------------

mergers <- changes %>%
    filter(type %in% c("merge_or_split", "boundary_shift")) %>% # take both
    select(
        event_id, ags_old, name_old,
        ags_new, name_new,
        pop_old, area_old, law_short, effective
    ) %>%
    mutate(
        pop_old  = as.numeric(gsub("[^0-9]", "", pop_old)), # strip spaces & dots
        area_old = as.numeric(gsub("[^0-9,.]", "", area_old)),
        pop_old  = pop_old / 1000 # keep units = 1000 inh.
    )

# inspect
mergers %>%
    filter(str_detect(name_old, "Börnichen/Erzgeb.")) %>%
    select(ags_old, name_old, ags_new, name_new, pop_old, area_old) %>%
    arrange(ags_new) %>%
    print(n = 100)
## Worked


# Compute weights (share of the DONOR that moves to each target), then compose
# sequential events for the same (ags_old, ags_new) pair
mergers <- mergers %>%
    compute_change_weights() %>%
    collapse_change_events()

stopifnot(all(abs(
    (mergers %>% group_by(ags_old) %>% summarise(w = sum(pop_cw)) %>% pull(w)) - 1
) < 0.01))

glimpse(mergers)

#### overwrite the default identity rows -------------------
## `area` / `population` describe the SOURCE municipality and are repeated on
## each of its rows (the convention of the 1990-2023 crosswalk), so they are
## taken from the base GV extract.  The change sheet's own figures describe the
## transferred FRAGMENT and are reported in m2 — carrying them over used to give
## ~50 municipalities square-metre areas and fragment populations in the output.
muni_base_23 <- muni_2023 %>% distinct(ags, ags_name, area, population)

cw_23_25 <- cw_23_25 %>%
    anti_join(mergers, by = c("ags" = "ags_old")) %>%
    bind_rows(
        mergers %>%
            transmute(
                ags = ags_old,
                ags_name_chg = name_old,
                year = 2023L,
                ags_25 = ags_new,
                ags_name_25 = name_new,
                pop_cw, area_cw,
                area_chg = area_old / 1e6, # sheet reports square metres
                population_chg = pop_old
            ) %>%
            left_join(muni_base_23, by = "ags") %>%
            mutate(
                ags_name = coalesce(ags_name, ags_name_chg),
                area = coalesce(area, area_chg),
                population = coalesce(population, population_chg)
            ) %>%
            select(ags, ags_name, year, ags_25, ags_name_25, pop_cw, area_cw, area, population)
    )

# Manual fix for Obergeckler (07232096 -> 07232503): set weights to 1
# This is a key change (law_short == 3) with missing population/area data.
# Redundant since compute_change_weights() gives a figure-less single-target
# donor weight 1 by itself; kept as a belt-and-braces safety net.
cw_23_25 <- cw_23_25 %>%
    mutate(
        pop_cw = ifelse(ags == "07232096" & ags_25 == "07232503", 1, pop_cw),
        area_cw = ifelse(ags == "07232096" & ags_25 == "07232503", 1, area_cw)
    )

glimpse(cw_23_25)

#### Inspections ------------------------------------------------------------
cw_23_25 <- cw_23_25 %>%
    mutate(
        year = 2023L,
        ags_name_25 = ags_name_25
    ) %>%
    arrange(ags, year)

#### sanity check: every 2023 AGS still represented ----------
n_distinct(cw_23_25$ags) == n_distinct(muni_2023$ags)
# WORKED

table(cw_23_25$year)

cw_23_25 %>%
    filter(str_detect(ags_name, "Börnichen/Erzgeb.")) %>%
    arrange(year) %>%
    select(year, ags, ags_25, pop_cw, area_cw)

## 2024 data ------------------------------------------------------------

muni_2024 <- read_excel(muni_2024, sheet = 2) %>% # ags, name, pop_2023, area_2023, employees_2023 …
    select(
        Land = `...3`,
        RB = `...4`,
        Kreis = `...5`,
        Gemeinde = `...7`,
        Gemeindename = `...8`,
        area = `...9`,
        population = `...10`
    ) |>
    slice(9:16018) |>
    filter(!is.na(Gemeinde)) |>
    mutate(
        Land = pad_zero_conditional(Land, 1),
        Kreis = pad_zero_conditional(Kreis, 1),
        Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
        Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
        ags = paste0(Land, RB, Kreis, Gemeinde),
        population = as.numeric(population) / 1000,
        area = as.numeric(area),
        year = 2024,
        ags_name = Gemeindename
    ) |>
    mutate(
        ags_25 = ags, # initialise: identity mapping
        ags_name_25 = ags_name,
        pop_cw = 1, area_cw = 1
    ) |>
    select(ags, ags_name, year, ags_25, ags_name_25, pop_cw, area_cw, area, population)

glimpse(muni_2024)

## Apply changes ------------------------------------------------------------

## One  to one changes ------------------------------------------------------------

glimpse(changes)

cw_24_25 <- muni_2024 %>%
    left_join(
        changes %>% filter(type == "boundary_shift" & str_detect(event_id, "2025")) %>%
            select(ags_old, ags_new, name_new),
        by = c("ags" = "ags_old")
    ) %>%
    mutate(
        ags_25 = coalesce(ags_new, ags_25),
        ags_name_25 = coalesce(name_new, ags_name_25)
    ) %>%
    select(-ags_new, -name_new)

glimpse(cw_24_25)

## Many to one mergers ------------------------------------------------------------

chg_2025 <- chg_2025 %>%
    mutate(
        type = case_when(
            ags_new == ags_old ~ "boundary_shift",
            ags_new != ags_old & !is.na(ags_new) ~ "merge_or_split",
            is.na(ags_new) ~ "dissolution" # rare; handled later
        )
    )

mergers <- chg_2025 %>%
    filter(type %in% c("merge_or_split", "boundary_shift")) %>% # take both
    select(
        event_id, ags_old, name_old,
        ags_new, name_new,
        pop_old, area_old, law_short, effective
    ) %>%
    mutate(
        pop_old  = as.numeric(gsub("[^0-9]", "", pop_old)), # strip spaces & dots
        area_old = as.numeric(gsub("[^0-9,.]", "", area_old)),
        pop_old  = pop_old / 1000 # keep units = 1000 inh.
    )


# Compute weights (share of the DONOR that moves to each target), then compose
# sequential events for the same (ags_old, ags_new) pair
mergers <- mergers %>%
    compute_change_weights() %>%
    collapse_change_events()

stopifnot(all(abs(
    (mergers %>% group_by(ags_old) %>% summarise(w = sum(pop_cw)) %>% pull(w)) - 1
) < 0.01))

glimpse(mergers)

## overwrite the default identity rows -------------------
muni_base_24 <- muni_2024 %>% distinct(ags, ags_name, area, population)

cw_24_25 <- cw_24_25 %>%
    anti_join(mergers, by = c("ags" = "ags_old")) %>% # drop donors (they re-enter below)
    bind_rows(
        mergers %>%
            transmute(
                ags = ags_old,
                ags_name_chg = name_old,
                year = 2024L,
                ags_25 = ags_new,
                ags_name_25 = name_new,
                pop_cw, area_cw,
                area_chg = area_old / 1e6, # sheet reports square metres
                population_chg = pop_old
            ) %>%
            left_join(muni_base_24, by = "ags") %>%
            mutate(
                ags_name = coalesce(ags_name, ags_name_chg),
                area = coalesce(area, area_chg),
                population = coalesce(population, population_chg)
            ) %>%
            select(ags, ags_name, year, ags_25, ags_name_25, pop_cw, area_cw, area, population)
    )

# Manual fix for Obergeckler (07232096 -> 07232503): set weights to 1
# This is a key change (law_short == 3) with missing population/area data.
# Redundant since compute_change_weights() gives a figure-less single-target
# donor weight 1 by itself; kept as a belt-and-braces safety net.
cw_24_25 <- cw_24_25 %>%
    mutate(
        pop_cw = ifelse(ags == "07232096" & ags_25 == "07232503", 1, pop_cw),
        area_cw = ifelse(ags == "07232096" & ags_25 == "07232503", 1, area_cw)
    )

glimpse(cw_24_25)

# Inspections ------------------------------------------------------------
cw_24_25 <- cw_24_25 %>%
    mutate(
        year = 2024L,
        ags_name_25 = ags_name_25
    ) %>%
    arrange(ags, year)

## sanity check: every 2023 AGS still represented ----------
n_distinct(cw_24_25$ags) == n_distinct(muni_2024$ags)

cw_24_25 %>%
    filter(str_detect(ags_name, "Dernau")) %>%
    arrange(year) %>%
    select(year, ags, ags_25, pop_cw, area_cw)
# WORKED

# Save the crosswalks
assert_crosswalk_valid(cw_23_25, "crosswalk_ags_2023_to_2025")
assert_crosswalk_valid(cw_24_25, "crosswalk_ags_2024_to_2025")

write_rds(cw_23_25, "data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
write_rds(cw_24_25, "data/crosswalks/final/crosswalk_ags_2024_to_2025.rds")

# merge the two crosswalks
cw_23_24_25 <- cw_23_25 %>%
    bind_rows(cw_24_25)

assert_crosswalk_valid(cw_23_24_25, "crosswalk_ags_2023_24_to_2025")

glimpse(cw_23_24_25)

# inspect
cw_23_24_25 %>%
    filter(str_detect(ags_name, "Wilkau")) %>%
    arrange(year) %>%
    print(n = 100)

# save
write_rds(cw_23_24_25, "data/crosswalks/final/crosswalk_ags_2023_24_to_2025.rds")



# 1990-2025 crosswalk ------------------------------------------------------------

getwd()

# load the 1990-2023 crosswalk
cw_1990_23 <- read_rds("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")

# load the 2023-2025 crosswalk
cw_2023_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_24_to_2025.rds")

# build the lookup table
lookup_23_25 <- cw_2023_25 %>% # keep only the mapping bits
    filter(year == 2023) %>%
    select(
        ags_2023 = ags,
        ags_25,
        ags_name_25,
        pop_w_23_25 = pop_cw,
        area_w_23_25 = area_cw
    )

# join the crosswalks
cw_1990_25 <- cw_1990_23 %>%
    left_join(lookup_23_25, by = "ags_2023") %>%
    ## chain the weights
    mutate(
        pop_cw = pop_cw * coalesce(pop_w_23_25, 1),
        area_cw = area_cw * coalesce(area_w_23_25, 1),
        ags_25 = coalesce(ags_25, ags_2023),
        ags_name_25 = coalesce(ags_name_25, ags_name_23)
    ) %>%
    ## keep a tidy layout
    select(
        ags, ags_name, year,
        ags_25, ags_name_25,
        pop_cw, area_cw,
        area, population
    ) # drop ags_2023 etc.

glimpse(cw_1990_25)


# Full 1990-2025 crosswalk ------------------------------------------------------------

full_cw <- bind_rows(
    cw_1990_25, # 1990–2022
    cw_2023_25 # 2023 (+ 2024 if you kept them)
) %>%
    arrange(ags, year)

## Collapse parallel paths: a source can reach the same 2025 unit through more
## than one 2023 unit, and those rows must be ADDED, not left as duplicate keys
## (03_municipal_harm_25.R groups on ags_25 together with ags_name_25, so a
## second row with a different name variant silently splits the group).
full_cw <- full_cw %>%
    group_by(ags, year, ags_25) %>%
    summarise(
        ags_name = dplyr::first(ags_name),
        ags_name_25 = dplyr::first(ags_name_25),
        pop_cw = sum(pop_cw, na.rm = TRUE),
        area_cw = sum(area_cw, na.rm = TRUE),
        area = dplyr::first(area),
        population = dplyr::first(population),
        .groups = "drop"
    ) %>%
    select(ags, ags_name, year, ags_25, ags_name_25, pop_cw, area_cw, area, population) %>%
    arrange(ags, year)

## Hard stop BEFORE the cosmetic rescale below, so that fabricated weights
## (a group summing to 2.0 that the rescale would silently turn into 0.5/0.5)
## cannot reach the artefact.
assert_crosswalk_valid(full_cw, "ags_1990_to_2025_crosswalk (pre-rescale)")

## Rescale weights to sum to 1.0 per (ags, year) — mirrors 04_build script.
## After the assertion above this only removes floating-point noise.
full_cw <- full_cw %>%
    group_by(ags, year) %>%
    mutate(
        pop_cw  = pop_cw / sum(pop_cw),
        area_cw = area_cw / sum(area_cw)
    ) %>%
    ungroup()

write_rds(full_cw,
          "data/crosswalks/final/ags_1990_to_2025_crosswalk.rds",
          compress = "xz")

data.table::fwrite(full_cw,
          "data/crosswalks/final/ags_1990_to_2025_crosswalk.csv")




### END