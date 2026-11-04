### Extend ags_area_pop_emp (2021 boundaries) through 2024
# 01_ags_crosswalk.R builds the panel for 1990-2021. This adds 2022-2024, so that
# files on 2021 boundaries can describe elections after 2021:
#   2022, 2023: ags_area_pop_emp_2023, back from 2023 boundaries
#   2024:       BBSR's 31.12.2024 in crosswalk_ags_2024_to_2025, back from 2025 boundaries
# The backward weights invert the forward crosswalks by 2021 mass within each target,
# as 02_municipal_harm.R does for election results. Unlike there, 2021 units without
# inhabitants stay in, so that area is kept and every unit has a row in every year.
# Run after 05_build_23_25_ags_crosswalks.R. Safe to re-run: it starts from the
# 1990-2021 rows.

rm(list = ls())

options(scipen = 999)
pacman::p_load(tidyverse, data.table)

conflicted::conflict_prefer("filter", "dplyr")

panel <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  filter(year <= 2021)

mass_21 <- panel |>
  filter(year == 2021) |>
  transmute(ags_21, ags_name_21, pop_21 = coalesce(population_ags, 0), area_21 = coalesce(area_ags, 0))
stopifnot(!anyDuplicated(mass_21$ags_21))

# Forward weights ---------------------------------------------------------
cw_21_23 <- read_rds("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds") |>
  transmute(ags_21 = ags_2021, target = ags_2023, pop_w = w_pop, area_w = w_area)

cw_23_25 <- read_rds("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds") |>
  filter(year == 2023) |>
  transmute(target = ags, ags_25, pop_w_25 = pop_cw, area_w_25 = area_cw)

cw_21_25 <- cw_21_23 |>
  left_join(cw_23_25, by = "target", relationship = "many-to-many") |>
  transmute(
    ags_21,
    target = coalesce(ags_25, target), # 2023 code unchanged in 2025
    pop_w = pop_w * coalesce(pop_w_25, 1),
    area_w = area_w * coalesce(area_w_25, 1)
  ) |>
  group_by(ags_21, target) |>
  summarise(pop_w = sum(pop_w), area_w = sum(area_w), .groups = "drop")

# Backward weights: a 2021 unit's share of its target's 2021 mass
invert <- function(forward) {
  forward |>
    group_by(ags_21, target) |>
    summarise(pop_w = sum(pop_w), area_w = sum(area_w), .groups = "drop") |>
    inner_join(mass_21, by = "ags_21") |>
    mutate(mass_pop = pop_21 * pop_w, mass_area = area_21 * area_w) |>
    group_by(target) |>
    mutate(
      n_parts = n(),
      pop_back = case_when(
        sum(mass_pop) > 0 ~ mass_pop / sum(mass_pop),
        sum(mass_area) > 0 ~ mass_area / sum(mass_area), # unpopulated targets
        TRUE ~ 1 / n_parts
      ),
      area_back = case_when(
        sum(mass_area) > 0 ~ mass_area / sum(mass_area),
        TRUE ~ 1 / n_parts
      )
    ) |>
    ungroup() |>
    select(ags_21, target, pop_back, area_back)
}

# Values on their own boundaries ------------------------------------------
y_22_23 <- read_rds("data/covars_municipality/final/ags_area_pop_emp_2023.rds") |>
  filter(year %in% c(2022, 2023)) |>
  transmute(target = ags_2023, year, area = area_ags, population = population_ags)

y_24 <- read_rds("data/crosswalks/final/crosswalk_ags_2024_to_2025.rds") |>
  group_by(target = ags_25) |>
  summarise(
    area = sum(area * area_cw, na.rm = TRUE),
    population = sum(population * pop_cw, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(year = 2024)

to_21 <- function(values, weights) {
  values |>
    inner_join(weights, by = "target", relationship = "many-to-many") |>
    group_by(ags_21, year) |>
    summarise(
      area_ags = sum(area * area_back, na.rm = TRUE),
      population_ags = sum(population * pop_back, na.rm = TRUE),
      .groups = "drop"
    )
}

ext <- bind_rows(to_21(y_22_23, invert(cw_21_23)), to_21(y_24, invert(cw_21_25))) |>
  mutate(
    employees_ags = NA_real_, # BBSR's 2022-2024 sheets have no employment
    pop_density_ags = ifelse(area_ags > 0, population_ags * 1000 / area_ags, NA_real_)
  ) |>
  left_join(mass_21 |> select(ags_21, ags_name_21), by = "ags_21")

# Every unit in every year, and the national totals survive the mapping. Only the
# non-municipal territories, which have no inhabitants, have no 2021 ancestor.
stopifnot(all(count(ext, year)$n == nrow(mass_21)), !anyDuplicated(ext[c("ags_21", "year")]))
totals <- bind_rows(y_22_23, y_24) |>
  group_by(year) |>
  summarise(source = sum(population, na.rm = TRUE), .groups = "drop") |>
  left_join(ext |> group_by(year) |> summarise(ext = sum(population_ags), .groups = "drop"), by = "year")
print(totals)
stopifnot(all(abs(totals$source - totals$ext) < 0.01))

out <- bind_rows(panel, ext[, names(panel)]) |>
  arrange(ags_21, year)
stopifnot(!anyDuplicated(out[c("ags_21", "year")]))

# write
fwrite(out, "data/covars_municipality/final/ags_area_pop_emp.csv")
write_rds(out, "data/covars_municipality/final/ags_area_pop_emp.rds", compress = "gz")
