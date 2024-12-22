# Check types of boundary changes in 2023

pacman::p_load(tidyverse)

# Load data
df <- readxl::read_excel("data/crosswalks/raw/2023.xlsx", sheet = 2)

glimpse(df)
head(df)

# check type of boundary changes
colnames(df)

df %>% 
  select(region_type = `...2`, boundary_change_type = `...6`) %>% 
  slice(4:n()) %>% 
  dplyr::filter(region_type == "Gemeinde") %>% 
  mutate(
    boundary_change_type = case_when(
        boundary_change_type == "1" ~ "dissolution",
        boundary_change_type == "2" ~ "partial_separation", 
        boundary_change_type == "3" ~ "key_change",
        boundary_change_type == "4" ~ "name_change",
        TRUE ~ boundary_change_type
    )
  ) %>% 
  group_by(boundary_change_type) %>% 
  summarise(n = n())

### END OF SCRIPT