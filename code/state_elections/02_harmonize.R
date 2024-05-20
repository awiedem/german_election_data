# Harmonize state elections data

rm(list = ls())

# load
df <- read_rds("01_Data/13_LTW/vh_Elections_Clean.RDS")
# df <- read_rds("01_Data/13_LTW/Elections_Clean.RDS")

glimpse(df)

# Load crosswalks
cw <- fread("01_Data/02_umsteigeschlüssel/ags_crosswalks.csv")

# Mutate df
df <- df |> mutate( ags = as.numeric(ags) )

# duplicates
df %>% 
  group_by(ags, year) %>% 
  filter(n() > 1) %>% 
  arrange(ags, year)

# Merge crosswalks
df_cw <- df |>
  left_join(cw, by = c("ags", "year"))

# Harmonize with weighted_population average
df_harm <- df_cw |>
  mutate(weights = pop_cw*population) |>
  group_by(ags_21, year) |>
  summarise_at(vars(afd:turnout), ~ weighted.mean(.x, weights, na.rm = TRUE)) |>
  rename(ags = ags_21)

# write
fwrite(df_harm, "01_Data/13_LTW/StateElections_harmonized.csv")

# Merge with municipality level data --------------------------------------

# Load municipality level data
muni <- fread("01_Data/ags_covars_subsidy.csv")

# Merge
df_final <- muni |>
  left_join(df_harm, by = c("ags", "year"))

# Create variable indicating whether there was election in given year
df_final <- df_final |>
  mutate(
    election_bin = ifelse(!is.na(turnout), 1, 0)
  )

# Write
fwrite(df_final, "01_Data/13_LTW/StateElections_final.csv")


# Inspect -----------------------------------------------------------------

df_final <- fread("01_Data/13_LTW/StateElections_final.csv")

# when are the elections in each state?
df_final %>%
  select(year, election_bin, states_east_west, state_name) %>%
  filter(states_east_west == "East" & year > 2005) %>%
  ggplot(aes(x = as.character(year), y = state_name, fill = as.character(election_bin))) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("1" = "black", "0" = "white")) +
  theme_hanno() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


ggsave("03_Plots/Descriptive/state_elections.pdf", width = 7, height = 4)

ggsave("~/Dropbox (Princeton)/Apps/Overleaf/GRW/figures/state_elections.pdf", width = 7, height = 4)




## END