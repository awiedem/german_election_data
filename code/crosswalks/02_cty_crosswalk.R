### County level crosswalks
# Vincent Heddesheimer
# Last update: May, 23, 2024

# remove scientific notation
options(scipen = 999)

rm(list=ls())

# Define column names for the years 1990 to 1996
names_90to96 <- c("county_code", "county_name", "area_cw", "pop_cw", "area", "population", "county_code_21", "county_name_21")

# Define column names for the years 1997 to 2020
names_97to20 <- c("county_code", "county_name", "area_cw", "pop_cw", "emp_cw", "area", "population", "employees", "county_code_21", "county_name_21")

# Specify the Excel file path
excel_file <- "data/crosswalks/ref-kreise-umrech-2021-1990-2020.xlsx"

# Read all sheets from the Excel file into a list of dataframes
cw_list <- excel_sheets(excel_file) %>%
  map(~ read_excel(excel_file, sheet = .)) %>%
  map2(1990:2020, ~ {
    colnames(.x) <- if (.y < 1997) {
      names_90to96
    } else {
      names_97to20
    }
    .x$year <- .y
    .x
  })

# Combine all dataframes into one
cw_combined <- bind_rows(cw_list) |>
  relocate(year, .after = county_name) |>
  arrange(county_code, year)


# Transform
cw_combined <- cw_combined |>
  mutate(
    # Remove last three digits from county_code
    county_code = str_remove(county_code, "\\d{3}$"),
    # pad 0 to county_code if it is only 4 digits long
    county_code = str_pad(county_code, width = 5, side = "left", pad = "0"),
    # now do both steps for county_code_21
    county_code_21 = str_remove(county_code_21, "\\d{3}$"),
    county_code_21 = str_pad(county_code_21, width = 5, side = "left", pad = "0")
  )


# write crosswalk df
fwrite(cw_combined, "data/crosswalks/county_crosswalks.csv")


# Create covariate dataframe ----------------------------------------------

# filter cw_combined to only German election years since 1990
cw <- cw_combined

# Harmonize to county 2021
cw <- cw |>
  group_by(county_code_21, county_name_21, year) |>
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
  count(county_code_21, year) |>
  filter(n>1)
# no duplicates

# counties per year
cw |> count(year)
# homogenous 400



# Get population & area for 2021
cw21 <- read_excel(path = "data/crosswalks/04_KreiseVorjahr.xlsx", sheet = 2) |>
  select(
    county_code_21 = `Kreisfreie Städte und Landkreise nach Fläche, Bevölkerung und Bevölkerungsdichte`,
    area = `...5`,
    population = `...6`
  ) |>
  mutate(year = 2021) |>
  slice(-c(1:7)) |>
  filter(!is.na(county_code_21) & nchar(county_code_21) == 5) |>
  select(county_code_21, year, area, population) |>
  mutate(
    population = as.numeric(population) / 1000,
    area = as.numeric(area),
    population = as.numeric(population),
    pop_density = population * 1000 / area
    )

# Bind 
cw <- cw |>
  bind_rows(cw21) |>
  arrange(county_code_21, year)


# Duplicates?
cw |>
  count(county_code_21, year) |>
  filter(n>1)
# no duplicates

# counties per year
cw |> 
  count(year) |>
  print(n = 32)


# Rename with suffix cty_ to avoid confusion with municipality level covariates
cw <- cw |>
  rename(
    area_cty = area,
    population_cty = population,
    pop_density_cty = pop_density,
    employees_cty = employees
  )

# write
fwrite(cw, "data/county_covars/cty_area_pop_emp.csv")

### END