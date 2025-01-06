### Create plot for incomplete muni election data
# Florian Sichart
# First: Jan 6, 2025
# Last: Jan 6, 2025

rm(list = ls())

conflicts_prefer(dplyr::filter)

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# Merge with unharmonized election data -----------------------------------

df <- readr::read_rds("data/municipal_elections/final/municipal_unharm.rds") |>
  # filter years before 1990: no crosswalks available
  filter(election_year >= 1990) |>
  mutate(election_year = as.numeric(election_year))

# Rename states ------------------------------------------------------------
state_rename_map <- c(
  "Baden-Wuerttemberg" = "Baden-Württemberg",
  "Bayern" = "Bavaria",
  "Berlin" = "Berlin",
  "Brandenburg" = "Brandenburg",
  "Bremen" = "Bremen",
  "Hamburg" = "Hamburg",
  "Hessen" = "Hesse",
  "Mecklenburg-Vorpommern" = "Mecklenburg-Vorpommern",
  "NRW" = "North Rhine-Westphalia",
  "Niedersachsen" = "Niedersachsen",
  "RLP" = "Rhineland-Palatinate",
  "Saarland" = "Saarland",
  "Sachsen" = "Saxony",
  "Sachsen-Anhalt" = "Saxony-Anhalt",
  "Schleswig-Holstein" = "Schleswig-Holstein",
  "Thueringen" = "Thuringia"
)

# Rename the states in the dataframe
df <- df |>
  mutate(state = recode(state, !!!state_rename_map))

# Plot to show incomplete data ------------------------------------------------
df <- df |>
  mutate(
    election_bin = ifelse(!is.na(turnout), 1, 0)
  )

# Create plot_df
plot_df <- df |>
  group_by(state, election_year) |>
  summarise(election_bin = max(election_bin, na.rm = TRUE)) |>
  ungroup() |>
  # Add special handling for Saxony-Anhalt 2006-2009 and 2011
  mutate(
    election_bin = case_when(
      state == "Saxony-Anhalt" & election_year %in% c(2005:2008, 2010) ~ 2, # Use 2 for irregular elections
      TRUE ~ election_bin
    )
  )

# Calculate the maximum number of municipalities for each state
state_max_munis <- df |>
  group_by(state) |>
  summarise(max_munis = max(n_distinct(ags), na.rm = TRUE)) 

# Join the maximum municipalities back to the original data
df_with_threshold <- df |>
  left_join(state_max_munis, by = "state") |>
  group_by(state, election_year) |>
  summarise(
    n_munis = n_distinct(ags), 
    max_munis = max(max_munis, na.rm = TRUE)
  ) |>
  mutate(
    threshold_10_percent = max_munis * 0.10, # Calculate 10% threshold
    below_threshold = n_munis < threshold_10_percent # Indicator for below 10%
  ) |>
  ungroup()

# Merge this indicator into `plot_df`
plot_df <- plot_df |>
  left_join(df_with_threshold |> select(state, election_year, below_threshold), 
            by = c("state", "election_year")) |>
  mutate(
    election_bin = case_when(
      below_threshold == TRUE & !election_bin == 2 ~ 3, # Use 3 for insufficient municipality data
      TRUE ~ election_bin
    )
  )

# Plot
plot_df |>
  ggplot(aes(x = as.numeric(election_year), 
             y = factor(state, 
                        levels = rev(levels(factor(state)))), 
             fill = as.factor(election_bin))
  ) +
  geom_tile(color = "white") + # Add borders to the squares
  scale_fill_manual(
    values = c(
      "1" = "grey20", 
      "0" = "white", 
      "2" = "grey70", # Irregular elections
      "3" = "red3"     # Insufficient municipality data
    ), 
    name = "Election",
    labels = c(
      "1" = "Election Held", 
      "0" = "Data Unavailable", 
      "2" = "Irregular Election",
      "3" = "Insufficient Municipality Data"
    )
  ) +
  labs(x = "Year", y = "State") +
  theme_hanno() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(1990, 2021, 5))

ggsave("output/figures/muni_elections_incomplete.pdf", width = 7, height = 4)

### END