### Municipality level crosswalks
# Vincent Heddesheimer
# Last update: May, 23, 2024

# remove scientific notation
options(scipen = 999)

rm(list=ls())

# pacman::p_load(readxl, haschaR, data.table)


# Define column names for the years 1990 to 1999
names_90to96 <- c(
  "ags", "ags_name", "area_cw",
  "pop_cw", "area", "population", "ags_21", "ags_name_21"
)

# Define column names for the years 1997 to 2020
names_97to20 <- c(
  "ags", "ags_name", "area_cw",
  "pop_cw", "emp_cw", "area", "population", "employees", "ags_21", "ags_name_21"
)


# 1990-1999 ---------------------------------------------------------------

# Specify the Excel file path
excel_file <- "data/crosswalks/gmd_auf_2021/ref-gemeinden-umrech-2021-1990-1999.xlsx"

# Read all sheets from the Excel file into a list of dataframes
cw_list <- excel_sheets(excel_file) %>%
  map(~ read_excel(excel_file, sheet = .)) %>%
  map2(1990:1999, ~ {
    colnames(.x) <- if (.y < 1997) {
      names_90to96
    } else {
      names_97to20
    }
    .x$year <- .y
    .x
  })

# Combine all dataframes into one
cw_combined <- bind_rows(cw_list)

# 2000-2010 ---------------------------------------------------------------

# Specify the Excel file path
excel_file <- "data/crosswalks/gmd_auf_2021/ref-gemeinden-umrech-2021-2000-2010.xlsx"

# Read all sheets from the Excel file into a list of dataframes
cw_list <- excel_sheets(excel_file) %>%
  map(~ read_excel(excel_file, sheet = .)) %>%
  map2(2000:2010, ~ {
    colnames(.x) <- names_97to20
    .x$year <- .y
    .x
  })

# Combine all dataframes into one
cw_combined <- bind_rows(cw_combined, cw_list)

# 2011-2020 ---------------------------------------------------------------

# Specify the Excel file path
excel_file <- "data/crosswalks/gmd_auf_2021/ref-gemeinden-umrech-2021-2011-2020.xlsx"

# Read all sheets from the Excel file into a list of dataframes
cw_list <- excel_sheets(excel_file) %>%
  map(~ read_excel(excel_file, sheet = .)) %>%
  map2(2011:2020, ~ {
    colnames(.x) <- names_97to20
    .x$year <- .y
    .x
  })

# Combine all dataframes into one
cw_combined <- bind_rows(cw_combined, cw_list) |>
  relocate(year, .after = ags_name) |>
  arrange(ags, year)

# write crosswalk df
fwrite(cw_combined, "data/crosswalks/ags_crosswalks.csv")

# Create covariate dataframe ----------------------------------------------

# filter cw_combined to only German election years since 1990
cw <- cw_combined

# Harmonize to county 2021
cw <- cw |>
  group_by(ags_21, ags_name_21, year) |>
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
  count(ags_21, year) |>
  filter(n>1)
# no duplicates

# ags per year
cw |> count(year) |> print(n = 32)
# homogenous 10994


# Get population & area for 2021
ags21 <- read_excel(path = "data/crosswalks//31122021_Auszug_GV.xlsx", sheet = 2) |>
  select(
    Land = `...3`,
    RB = `...4`,
    Kreis = `...5`,
    Gemeinde = `...7`,
    ags_name_21 = `...8`,
    area = `...9`,
    population = `...10`
  ) |>
  mutate(
    Land = pad_zero_conditional(Land, 1),
    Kreis = pad_zero_conditional(Kreis, 1),
    Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
    Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
    ags_21 = paste0(Land, RB, Kreis, Gemeinde),
    year = 2021
  ) |>
  slice(6:16065) |>
  filter(!is.na(Gemeinde)) |>
  select(ags_21, ags_name_21, year, area, population) |>
  # ags_21, area, population to numeric
  mutate(
    ags_21 = as.numeric(ags_21),
    area = as.numeric(area),
    population = as.numeric(population) / 100
  ) |>
  # filter NAs
  filter(!is.na(ags_21)) |>
  # get population density
  mutate(pop_density = population * 1000 / area)


# Bind 
cw <- cw |>
  bind_rows(ags21) |>
  arrange(ags_21, year)


# Duplicates?
cw |>
  count(ags_21, year) |>
  filter(n>1)
# no duplicates

# ags per year
cw |> 
  count(year) |>
  print(n = 32)


# which ags_21 are in 2021 but not in 2020?
cw |>
  filter(year == 2021) |>
  anti_join(cw %>% filter(year == 2020), by = "ags_21") |>
  select(ags_21, ags_name_21) |>
  arrange(ags_21) |>
  print(n = 32)
# ags_21 ags_name_21                                      
# <dbl> <chr>                                            
#   1  7000999 Gemeinsames deutsch-luxemburgisches Hoheitsgebiet
# 2 10042999 Deutsch-luxemburgisches Hoheitsgebiet            
# 3 13000999 Küstengewässer einschl. Anteil am Festlandsockel 

# remove these
cw <- cw |>
  filter(ags_21 != 7000999 & ags_21 != 10042999 & ags_21 != 13000999)


# Rename with suffix _ags to avoid confusion with county level covariates
cw <- cw |>
  rename(
    area_ags = area,
    population_ags = population,
    employees_ags = employees,
    pop_density_ags = pop_density
  )

# write
fwrite(cw, "data/municipal_covars/ags_area_pop_emp.csv")

### END