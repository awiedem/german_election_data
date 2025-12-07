# Build crosswalks for 2021 → 2022 and 2022 → 2023

rm(list = ls())

# ────────────────────────────────────────────────────────────────
# libraries & options
# ────────────────────────────────────────────────────────────────
pacman::p_load(readxl, dplyr, stringr, janitor, data.table, readr)
options(scipen = 999)

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

path_xlsx <- "data/crosswalks/raw/ref-gemeinden-ab-2020.xlsx"

# ────────────────────────────────────────────────────────────────
# 1.   2021 → 2022 Umsteigeschlüssel
# ────────────────────────────────────────────────────────────────
glimpse(read_excel(path_xlsx, sheet = "2021"))

cw_21_22 <- read_excel(path_xlsx, sheet = "2021") %>%
  clean_names() %>%
  transmute(
    ags = str_pad(gemeinden_31_12_2021, 8, pad = "0"),
    ags_name = gemeindename_2021,
    ags_2022 = str_pad(gemeinden_31_12_2022, 8, pad = "0"),
    ags_name_22 = gemeindename_2022,
    year = 2021,
    w_pop_21_22 = bevolkerungs_proportionaler_umsteige_schlussel,
    w_area_21_22 = flachen_proportionaler_umsteige_schlussel,
    w_emp_21_22 = beschaftigten_proportionaler_umsteige_schlussel,
    population = bevolkerung_am_31_12_2021_in_100 / 10,
    area = flache_am_31_12_2021_in_km2,
    employees = sozialvers_pflichtig_beschaftigte_am_arbeitsort_am_30_6_2021_in_100 / 10
  )

# ────────────────────────────────────────────────────────────────
# 2.   2022 → 2023 Umsteigeschlüssel
# ────────────────────────────────────────────────────────────────

glimpse(read_excel(path_xlsx, sheet = "2022"))
cw_22_23 <- read_excel(path_xlsx, sheet = "2022") %>%
  clean_names() %>%
  transmute(
    ags = str_pad(gemeinden_31_12_2022, 8, pad = "0"),
    ags_name = gemeindename_2022,
    ags_2023 = str_pad(gemeinden_31_12_2023, 8, pad = "0"),
    ags_name_23 = gemeindename_2023,
    year = 2022,
    w_pop_22_23 = bevolkerungs_proportionaler_umsteige_schlussel,
    w_area_22_23 = flachen_proportionaler_umsteige_schlussel,
    population = bevolkerung_am_31_12_2022 / 1000,
    area = flache_am_31_12_2022_in_km2
  )

# ────────────────────────────────────────────────────────────────
# 3A.  Build 2021 → 2023
# ────────────────────────────────────────────────────────────────
cw_21_23 <- cw_21_22 %>%
  left_join(cw_22_23 |> select(-c(year, ags_name, population, area)), by = c("ags_2022" = "ags")) %>%
  mutate(
    # Handle NA weights: if identity mapping (ags == ags_2023) and weight is NA, set to 1
    w_pop_21_22 = ifelse(ags == ags_2023 & is.na(w_pop_21_22), 1, w_pop_21_22),
    w_area_21_22 = ifelse(ags == ags_2023 & is.na(w_area_21_22), 1, w_area_21_22),
    w_pop_22_23 = ifelse(ags == ags_2023 & is.na(w_pop_22_23), 1, w_pop_22_23),
    w_area_22_23 = ifelse(ags == ags_2023 & is.na(w_area_22_23), 1, w_area_22_23),
    pop_cw  = w_pop_21_22 * w_pop_22_23,
    area_cw = w_area_21_22 * w_area_22_23
  ) %>%
  select(ags, ags_name, ags_2023, ags_name_23, year, pop_cw, area_cw, population, area, employees)

# ────────────────────────────────────────────────────────────────
# 3B.  Build 2022 → 2023 (direct)
# ────────────────────────────────────────────────────────────────
cw_22_23_direct <- cw_22_23 %>%
  mutate(
    # Handle NA weights: if identity mapping (ags == ags_2023) and weight is NA, set to 1
    w_pop_22_23 = ifelse(ags == ags_2023 & is.na(w_pop_22_23), 1, w_pop_22_23),
    w_area_22_23 = ifelse(ags == ags_2023 & is.na(w_area_22_23), 1, w_area_22_23)
  ) %>%
  transmute(
    ags,
    ags_name,
    ags_2023,
    ags_name_23,
    year,
    pop_cw = w_pop_22_23,
    area_cw = w_area_22_23,
    population = population,
    area = area
  )

# ────────────────────────────────────────────────────────────────
# 4.   Combine 2021 and 2022 origins
# ────────────────────────────────────────────────────────────────
cw_21_22_23 <- bind_rows(cw_21_23, cw_22_23_direct) %>%
  arrange(ags, year, ags_2023)

# inspect
cw_21_22_23 %>%
  filter(pop_cw < 1) %>%
  arrange(desc(ags)) %>%
  print(n = 100)

cw_21_22_23 %>%
  filter(str_detect(ags, "1051141")) %>%
  arrange(year) %>%
  print(n = 100)

# # sum to 1?
# cw_21_22_23 %>%
#   group_by(year, ags) %>%
#   summarise(across(c(pop_cw, area_cw), sum, na.rm = TRUE), .groups = "drop") %>%
#   filter(pop_cw < 1) %>%
#   print(n = 100)

# works!

# optional exact rescale (≈ 0.01 % rounding drift)
cw_21_22_23 <- cw_21_22_23 %>%
  group_by(year, ags) %>%
  mutate(
    pop_cw  = pop_cw / sum(pop_cw),
    area_cw = area_cw / sum(area_cw)
  ) %>%
  ungroup()

# save
fwrite(cw_21_22_23, "data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.csv")
write_rds(cw_21_22_23, "data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.rds")

# ────────────────────────────────────────────────────────────────
# 5.   Extend existing 1990-2021 crosswalk to 1990-2023 (+2022)
# ────────────────────────────────────────────────────────────────

cw_1990_21 <- read_rds("data/crosswalks/final/ags_crosswalks.rds") %>%
  mutate(
    ags = str_pad(ags, 8, pad = "0"),
    ags_21 = str_pad(ags_21, 8, pad = "0")
  ) # join helper

cw_21_22_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.rds")
# columns: ags (origin-code; 2021 or 2022), year, ags_2023, pop_cw, area_cw …

## ───────────────────────────────────────────────────────────────
## 6.  Split the 21→23 and 22→23 pieces
## ───────────────────────────────────────────────────────────────
cw_21to23_merging <- cw_21_22_23 %>%
  filter(year == 2021) %>%
  select(
    ags_21 = ags, ags_2023, ags_name_23,
    pop_w_21_23 = pop_cw,
    area_w_21_23 = area_cw
  )

cw_21to23 <- cw_21_22_23 %>%
  filter(year == 2021) %>%
  transmute(
    ags = ags, # origin code (2021 borders)
    ags_name = ags_name,
    year = 2021L,
    ags_2023,
    ags_name_23,
    pop_cw = pop_cw, # already 2021→23 weights
    area_cw = area_cw,
    population, area, employees
  )

cw_22to23 <- cw_21_22_23 %>% # direct, no extra work
  filter(year == 2022) %>%
  transmute(
    ags, ags_name, year, # origin = 2022
    ags_2023, ags_name_23,
    pop_cw = pop_cw,
    area_cw = area_cw,
    population, area, employees
  )

## ───────────────────────────────────────────────────────────────
## 7.  Bring 1990-2021 rows forward to 2023 borders
## ───────────────────────────────────────────────────────────────
cw_1990_23_pre22 <- cw_1990_21 %>%
  left_join(cw_21to23_merging, by = "ags_21") %>%
  mutate(
    pop_cw  = pop_cw * pop_w_21_23,
    area_cw = area_cw * area_w_21_23
  ) %>%
  select(
    ags, ags_name, year,
    ags_2023, ags_name_23,
    pop_cw, area_cw,
    population, area, employees
  )

cw_1990_23_pre22 %>%
  filter(str_detect(ags, "1051141")) %>%
  arrange(year) %>%
  print(n = 100)

## ───────────────────────────────────────────────────────────────
## 8b.  Bind 1990‑2020, 2021, and 2022 origins
## ───────────────────────────────────────────────────────────────
cw_1990_23 <- bind_rows(
  cw_1990_23_pre22, # 1990‑2020 rows (now includes 2020 only)
  cw_21to23, # freshly added 2021 rows
  cw_22to23 # 2022 rows
) %>%
  arrange(year, ags, ags_2023)

cw_1990_23 %>%
  filter(str_detect(ags, "1051141")) %>%
  arrange(year) %>%
  print(n = 100)

## (Optional) exact rescale
cw_1990_23 <- cw_1990_23 %>%
  group_by(ags, year) %>%
  mutate(
    pop_cw  = pop_cw / sum(pop_cw),
    area_cw = area_cw / sum(area_cw)
  ) %>%
  ungroup()

table(cw_1990_23$year)

cw_1990_23 %>%
  filter(str_detect(ags, "1051141")) %>%
  arrange(year) %>%
  print(n = 100)

## ───────────────────────────────────────────────────────────────
## 9.  Save
## ───────────────────────────────────────────────────────────────
write_rds(cw_1990_23, "data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")
data.table::fwrite(
  cw_1990_23,
  "data/crosswalks/final/ags_1990_to_2023_crosswalk.csv"
)


# Create covariate dataframe ----------------------------------------------

# filter cw_combined to only German election years since 1990
cw <- cw_1990_23

glimpse(cw)

# Harmonize to ags 2021
cw <- cw |>
  group_by(ags_2023, ags_name_23, year) |>
  summarise(
    area = sum(area, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    employees = sum(employees, na.rm = TRUE)
  ) |>
  ungroup() |>
  # get population density
  mutate(pop_density = population * 1000 / area) |>
  mutate(employees = ifelse(employees == 0, NA, employees))

# Duplicates?
cw |>
  count(ags_2023, year) |>
  filter(n > 1)
# no duplicates

# ags per year
cw |>
  count(year) |>
  print(n = 35)
# homogenous 10978

# pop_density = Inf?
cw |> filter(pop_density == Inf)

# area = 0?
cw |>
  filter(area == 0) |>
  print(n = 35)

# Inf to NAs
cw <- cw |>
  mutate(pop_density = ifelse(is.infinite(pop_density), NA, pop_density))


# Get population & area for 2021
ags23 <- read_excel(path = "data/crosswalks/raw/31122023_Auszug_GV.xlsx", sheet = 2) |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    ags_name_23 = `...8`,
    area = `...9`,
    population = `...10`
  ) |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags_2023 = paste0(Land, RB, Kreis, Gemeinde),
    year = 2023
  ) |>
  slice(6:16040) |>
  filter(!is.na(Gemeinde)) |>
  select(ags_2023, ags_name_23, year, area, population) |>
  # ags_23, area, population to numeric
  mutate(
    ags_2023 = ags_2023,
    area = as.numeric(area),
    population = as.numeric(population) / 1000
  ) |>
  # filter NAs
  filter(!is.na(ags_2023)) |>
  # get population density
  mutate(pop_density = population * 1000 / area)

glimpse(ags23)
glimpse(cw)

# Bind
cw_full <- cw |>
  bind_rows(ags23) |>
  arrange(ags_2023, year)

glimpse(cw_full)

# Duplicates?
cw_full |>
  count(ags_2023, year) |>
  filter(n > 1)
# no duplicates

# ags per year
cw |>
  count(year) |>
  print(n = 32)


# which ags_23 are in 2023 but not in 2022?
cw |>
  filter(year == 2023) |>
  anti_join(cw_full %>% filter(year == 2022), by = "ags_2023") |>
  select(ags_2023, ags_name_23) |>
  arrange(ags_2023) |>
  print(n = 32)
# none


# Rename with suffix _ags to avoid confusion with county level covariates
cw_full <- cw_full |>
  rename(
    area_ags = area,
    population_ags = population,
    employees_ags = employees,
    pop_density_ags = pop_density
  )

glimpse(cw_full)


# check Berlin
cw_full |>
  filter(ags_2023 == "11000000") |>
  select(ags_2023, ags_name_23, year, population_ags) |>
  print(n = 200)
# looks good

# check states
cw_full |>
  mutate(
    ags_2023 = pad_zero_conditional(ags_2023, 7),
    state = substr(ags_2023, 1, 2)
  ) |>
  filter(state %in% c("02", "04", "11")) |>
  select(ags_2023, ags_name_23, year, state) |>
  print(n = 200)
# looks good

# # write
fwrite(cw_full, "data/covars_municipality/final/ags_area_pop_emp_2023.csv")
write_rds(cw_full, "data/covars_municipality/final/ags_area_pop_emp_2023.rds")

### END