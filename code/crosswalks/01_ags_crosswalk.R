### Municipality level crosswalks
# Vincent Heddesheimer
# First draft: May, 23, 2024
# Last update: Aug, 07, 2024

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
write_rds(cw_combined, "data/crosswalks/ags_crosswalks.rds")

fwrite(cw_combined, "output/ags_crosswalks.csv")
write_rds(cw_combined, "output/ags_crosswalks.rds")

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
# 1  7000999 Gemeinsames deutsch-luxemburgisches Hoheitsgebiet
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
write_rds(cw, "data/municipal_covars/ags_area_pop_emp.rds")
fwrite(cw, "output/ags_area_pop_emp.csv")
write_rds(cw, "output/ags_area_pop_emp.rds")



# Create table harmonizations per year -------------------------------------------

cw_combined <- read_rds("data/crosswalks/ags_crosswalks.rds")

glimpse(cw_combined)

# Want to create a table and a plot that shows the number of ags affected by harmonization per year
# For this, we need to count the number of ags that are affected by harmonization per year
# We can do this by counting the number of ags per year that have a pop_cw value below 1


# First, check whether there are cases where area_cw is below 1 but pop_cw is 1
inspect <- cw_combined %>%
  filter(area_cw < 1 & pop_cw == 1)
# that means, the population comes from exactly one ags, but the area is below 1
# that also means, that we should use pop_cw as crosswalk

# Second, check whether there are cases where pop_cw is below 1 but area_cw is 1
inspect <- cw_combined %>%
  filter(pop_cw < 1 & area_cw == 1)
# zero!

# merged_ags
merged_ags <- cw_combined %>%
  filter(pop_cw < 1)


# Count the number of ags per year that are affected by harmonization
harmonization_counts <- cw_combined %>%
  mutate(population = population / 10) %>%
  group_by(year) %>%
  summarise(
    n_ags = n(),
    n_ags_harmonized = sum(pop_cw < 1),
    # calculate total population & area for ags with pop_cw < 1
    total_population_harmonized = sum(population[pop_cw < 1], na.rm = TRUE),
    total_area_harmonized = sum(area[pop_cw < 1], na.rm = TRUE),
    # calculate average population & area for ags with pop_cw < 1
    avg_population_harmonized = mean(population[pop_cw < 1], na.rm = TRUE),
    avg_area_harmonized = mean(area[pop_cw < 1], na.rm = TRUE)
  )
# population measured in 1000s
# area measured in km^2

# produce latex table
harm_tab <- harmonization_counts %>%
  select(
    Year = year, 
    N = n_ags_harmonized, 
    `Total population (1000s)` = total_population_harmonized,
    `Total area (km$^{2}$)` = total_area_harmonized,
    `Average population (1000s)` = avg_population_harmonized,
    `Average area (km$^{2}$)` = avg_area_harmonized
  ) %>%
  kable(
    format = "latex", 
    digits = 2, 
    escape = FALSE,
    booktabs = TRUE, 
    align = "lrrrrrr",
    linesep = "",
    caption = "Number of municipalities affected by mergers per year \\label{tab:mergers}") %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position")) %>%
  column_spec(1, "1cm") %>%
  column_spec(2, ".5cm") %>%
  column_spec(3, "3cm") %>%
  column_spec(4, "3cm") %>%
  column_spec(5, "3cm") %>%
  column_spec(6, "3cm")

save_kable(harm_tab, "tables/harmonization_counts.tex", keep_tex = T)
save_kable(harm_tab, file = "~/Dropbox (Princeton)/Apps/Overleaf/ElectionPaper/tables/harmonization_counts.tex", keep_tex = T)

# produce plot
harmonization_counts %>%
  ggplot(aes(x = year, y = n_ags_harmonized)) +
  geom_col() +
  geom_text(aes(label = n_ags_harmonized), vjust = -0.5, size = 2.75) +
  labs(
    x = "Year",
    y = "Number of municipalities\n affected by mergers"
  ) +
  theme_hanno() +
  scale_y_continuous(limits = c(0, 85))

ggsave("figures/mergers.pdf", width = 7, height = 3.5)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/ElectionPaper/figures/mergers.pdf", width = 7, height = 3.5)

### END