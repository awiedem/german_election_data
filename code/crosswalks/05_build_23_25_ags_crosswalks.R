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
        pop_old, area_old, law_short
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


# Compute weights
mergers <- mergers %>%
    group_by(event_id, ags_new) %>%
    mutate(
        pop_total = sum(pop_old, na.rm = TRUE),
        area_total = sum(area_old, na.rm = TRUE),
        # For identity mappings (ags_old == ags_new) with missing data, set weights to 1
        is_identity = ags_old == ags_new,
        pop_cw = ifelse(
            is_identity & (is.na(pop_old) | pop_old == 0),
            1,
            ifelse(pop_total > 0,
                pop_old / pop_total,
                0
            )
        ),
        area_cw = ifelse(
            is_identity & (is.na(area_old) | area_old == 0),
            1,
            ifelse(area_total > 0,
                area_old / area_total,
                0
            )
        )
    ) %>%
    ungroup()

glimpse(mergers)

#### overwrite the default identity rows -------------------
cw_23_25 <- cw_23_25 %>%
    anti_join(mergers, by = c("ags" = "ags_old")) %>%
    bind_rows(
        mergers %>%
            transmute(
                ags = ags_old,
                ags_name = name_old,
                year = 2023L,
                ags_25 = ags_new,
                ags_name_25 = name_new,
                pop_cw, area_cw,
                area = area_old,
                population = pop_old
            )
    )

# Manual fix for Obergeckler (07232096 -> 07232503): set weights to 1
# This is a key change (law_short == 3) with missing population/area data
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
        pop_old, area_old, law_short
    ) %>%
    mutate(
        pop_old  = as.numeric(gsub("[^0-9]", "", pop_old)), # strip spaces & dots
        area_old = as.numeric(gsub("[^0-9,.]", "", area_old)),
        pop_old  = pop_old / 1000 # keep units = 1000 inh.
    )


# Compute weights
mergers <- mergers %>%
    group_by(event_id, ags_new) %>%
    mutate(
        pop_total = sum(pop_old, na.rm = TRUE),
        area_total = sum(area_old, na.rm = TRUE),
        # For identity mappings (ags_old == ags_new) with missing data, set weights to 1
        is_identity = ags_old == ags_new,
        pop_cw = ifelse(
            is_identity & (is.na(pop_old) | pop_old == 0),
            1,
            ifelse(pop_total > 0,
                pop_old / pop_total,
                0
            )
        ),
        area_cw = ifelse(
            is_identity & (is.na(area_old) | area_old == 0),
            1,
            ifelse(area_total > 0,
                area_old / area_total,
                0
            )
        )
    ) %>%
    ungroup()

glimpse(mergers)

## overwrite the default identity rows -------------------
cw_24_25 <- cw_24_25 %>%
    anti_join(mergers, by = c("ags" = "ags_old")) %>% # drop donors (they re-enter below)
    bind_rows(
        mergers %>%
            transmute(
                ags = ags_old,
                ags_name = name_old,
                year = 2024L,
                ags_25 = ags_new,
                ags_name_25 = name_new,
                pop_cw, area_cw,
                area = area_old,
                population = pop_old
            )
    )

# Manual fix for Obergeckler (07232096 -> 07232503): set weights to 1
# This is a key change (law_short == 3) with missing population/area data
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
write_rds(cw_23_25, "data/crosswalks/final/crosswalk_ags_2023_to_2025.rds")
write_rds(cw_24_25, "data/crosswalks/final/crosswalk_ags_2024_to_2025.rds")

# merge the two crosswalks
cw_23_24_25 <- cw_23_25 %>%
    bind_rows(cw_24_25)

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


write_rds(full_cw,
          "data/crosswalks/final/ags_1990_to_2025_crosswalk.rds",
          compress = "xz")

data.table::fwrite(full_cw,
          "data/crosswalks/final/ags_1990_to_2025_crosswalk.csv")




### END