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
    w_pop_21_22  = bevolkerungs_proportionaler_umsteige_schlussel,
    w_area_21_22 = flachen_proportionaler_umsteige_schlussel,
    w_emp_21_22  = beschaftigten_proportionaler_umsteige_schlussel,
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
    w_pop_22_23  = bevolkerungs_proportionaler_umsteige_schlussel,
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
    pop_cw  = w_pop_21_22  * w_pop_22_23,
    area_cw = w_area_21_22 * w_area_22_23
  ) %>% 
  select(ags, ags_name, ags_2023, ags_name_23, year, pop_cw, area_cw, population, area, employees)

# ────────────────────────────────────────────────────────────────
# 3B.  Build 2022 → 2023 (direct)
# ────────────────────────────────────────────────────────────────
cw_22_23_direct <- cw_22_23 %>% 
  transmute(
    ags,
    ags_name,
    ags_2023,
    ags_name_23,
    year,
    pop_cw  = w_pop_22_23,
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
    pop_cw  = pop_cw  / sum(pop_cw),
    area_cw = area_cw / sum(area_cw)
  ) %>% 
  ungroup()

# save
fwrite(cw_21_22_23, "data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.csv")
write_rds(cw_21_22_23, "data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.rds")

# ────────────────────────────────────────────────────────────────
# 5.   Extend your existing 1990-2021 key to 1990-2023 (+2022)
# ────────────────────────────────────────────────────────────────

cw_1990_21 <- read_rds("data/crosswalks/final/ags_crosswalks.rds") %>% 
  mutate(
    ags = str_pad(ags, 8, pad = "0"),
    ags_21 = str_pad(ags_21, 8, pad = "0")
    )       # join helper

cw_21_22_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_2022_to_2023.rds")
# columns: ags (origin-code; 2021 or 2022), year, ags_2023, pop_cw, area_cw …

## ───────────────────────────────────────────────────────────────
## 6.  Split the 21→23 and 22→23 pieces
## ───────────────────────────────────────────────────────────────
cw_21to23_merging <- cw_21_22_23 %>% 
  filter(year == 2021) %>% 
  select(ags_21 = ags, ags_2023, ags_name_23,
         pop_w_21_23  = pop_cw,
         area_w_21_23 = area_cw)

cw_21to23 <- cw_21_22_23 %>% 
  filter(year == 2021) %>% 
  transmute(
    ags        = ags,          # origin code (2021 borders)
    ags_name   = ags_name,
    year       = 2021L,
    ags_2023,
    ags_name_23,
    pop_cw  = pop_cw,          # already 2021→23 weights
    area_cw = area_cw,
    population, area, employees
  )

cw_22to23 <- cw_21_22_23 %>%           # direct, no extra work
  filter(year == 2022) %>% 
  transmute(
    ags, ags_name, year,                 # origin = 2022
    ags_2023, ags_name_23,
    pop_cw  = pop_cw,
    area_cw = area_cw,
    population, area, employees
  )

## ───────────────────────────────────────────────────────────────
## 7.  Bring 1990-2021 rows forward to 2023 borders
## ───────────────────────────────────────────────────────────────
cw_1990_23_pre22 <- cw_1990_21 %>% 
  left_join(cw_21to23_merging, by = "ags_21") %>% 
  mutate(
    pop_cw  = pop_cw  * pop_w_21_23,
    area_cw = area_cw * area_w_21_23
  ) %>% 
  select(ags, ags_name, year,
         ags_2023, ags_name_23,
         pop_cw, area_cw,
         population, area, employees)

cw_1990_23_pre22 %>%
    filter(str_detect(ags, "1051141")) %>%
    arrange(year) %>%
    print(n = 100)

## ───────────────────────────────────────────────────────────────
## 8b.  Bind 1990‑2020, 2021, and 2022 origins
## ───────────────────────────────────────────────────────────────
cw_1990_23 <- bind_rows(
  cw_1990_23_pre22,   # 1990‑2020 rows (now includes 2020 only)
  cw_21to23,         # freshly added 2021 rows
  cw_22to23           # 2022 rows
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
    pop_cw  = pop_cw  / sum(pop_cw),
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
data.table::fwrite(cw_1990_23,
                   "data/crosswalks/final/ags_1990_to_2023_crosswalk.csv")

### END