### Harmonize municipal electoral results to 2021 borders
# Vincent Heddesheimer
# First: Aug 08, 2024
# Last: Aug 14, 2024

rm(list = ls())

# Disallow scientific notation: leads to errors when loading data
options(scipen = 999)

# Read crosswalk files ----------------------------------------------------
cw <- fread("data/crosswalks/ags_crosswalks.csv") |>
  mutate(
    ags = pad_zero_conditional(ags, 7),
    year = as.character(year),
    weights = pop_cw * population
    ) |>
  select(-ags_name)


# Merge with unharmonized election data -----------------------------------

df <- read_rds("output/municipal_unharm.rds") |>
  # filter years before 1990: no crosswalks available
  filter(election_year >= 1990)

glimpse(df)
glimpse(cw)
table(df$election_year, useNA = "ifany")
table(is.na(df$ags_name))


# inspect -----------------------------------------------------------------

# is there more than one election in one ags in one year?
dupl <-df |>
  group_by(ags, election_year) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  print(n=Inf) |>
  mutate(id = paste0(ags,"_",election_year))
# no: none

# inspect <- df |>
#   mutate(id = paste0(ags,"_",election_year)) |>
#   filter(id %in% dupl$id) |>
#   arrange(ags, election_year)
# 
# fwrite(inspect, "data/municipal_elections/processed_data/duplicates.csv")


# Merge w/ cw -------------------------------------------------------------

# bind with crosswalks
df_naive_merge <- df |>
  left_join_check_obs(cw, by = c("ags", "election_year" = "year")) |>
  arrange(ags, election_year)
# number of obs increases: but this is wanted, as we want to harmonize the data

# is there any ags that did not get merged to ags_21?
not_merged_naive <- df_naive_merge %>%
  filter(election_year < 2021) %>%
  filter(is.na(ags_21)) %>%
  select(ags, election_year) %>%
  distinct() %>%
  mutate(id = paste0(ags, "_", election_year))
not_merged_naive
# If we do not follow the steps below, there are >1,485 cases.
# We found these by the below code.

fwrite(not_merged_naive, "data/municipal_elections/processed_data/unsuccessful_mergers.csv")

glimpse(df_cw)


# Dealing with unsuccessful mergers ---------------------------------------


# 1. in 2011 for MeckPomm 

# define cases where we want to use year - 1
ags_year_cw <- not_merged_naive %>%
  filter(
    grepl("^14", id)            # id starts with 14
  ) %>%
  pull(id)

# apply the rules
df <- df |>
  mutate(
    id = paste0(ags, "_", election_year),
    # 1. if ags is in ags_year_cw, use year - 1
    year_cw = case_when(
      # X. change year manually
      # SA 2007
      id == "15086270_2007" ~ 2006, # Zeppernick
      id == "15089040_2007" ~ 2006, # Biere
      id == "15089080_2007" ~ 2006, # Eggersdorf
      id == "15089085_2007" ~ 2006, # Eickendorf
      id == "15089160_2007" ~ 2006, # Großmühlingen
      id == "15089190_2007" ~ 2006, # Kleinmühlingen
      id == "15089335_2007" ~ 2006, # Welsleben
      id == "15089370_2007" ~ 2006, # Zens
      # X. use 1998 for merging for ags in MeckPomm in 1999
      id %in% not_merged_naive[not_merged_naive$election_year == 1999 & grepl("^13", not_merged_naive$id), ]$id ~ election_year - 1,
      id %in% not_merged_naive[not_merged_naive$election_year == 2004 & grepl("^15", not_merged_naive$id), ]$id ~ election_year - 1,
      # X. use election_year - 1 for merging for ags in Saxony
      id %in% not_merged_naive[grepl("^14", not_merged_naive$id), ]$id ~ election_year - 1,
      TRUE ~ election_year
    ),
    # X. wrong AGS: checked with election results Leitband 
    # and manually matched ags names btw. election results & crosswalk files
    ags = case_when(
      id == "01051141_2008" ~ "01051040", # Süderheistedt 
      id == "01059186_2008" ~ "01059153", # Steinbergkirche 
      id == "01059187_2008" ~ "01059011", # Boren
      id == "03361013_2001" ~ "03361010", # Riede
      id == "05313000_2009" ~ "05334002", # Aachen
      id == "05314000_2014" ~ "05334002", # Aachen
      id == "05314000_2020" ~ "05334002", # Aachen
      id == "07140502_1994" ~ "07135050", # Lahr
      id == "07140502_1999" ~ "07135050", # Lahr
      id == "07140503_1994" ~ "07135063", # Mörsdorf
      id == "07140503_1999" ~ "07135063", # Mörsdorf
      id == "07140504_1994" ~ "07135094", # Zilshausen
      id == "07140504_1999" ~ "07135094", # Zilshausen
      id == "07232502_1994" ~ "07232021", # Brimingen
      id == "07232502_1999" ~ "07232021", # Brimingen
      id == "07235207_1994" ~ "07231207", # Trittenheim
      id == "07235207_1999" ~ "07231207", # Trittenheim
      id == "07235207_2004" ~ "07231207", # Trittenheim
      id == "07235207_2009" ~ "07231207", # Trittenheim
      # SA 2007
      id == "15086270_2007" ~ "15151066", # Zeppernick
      id == "15089040_2007" ~ "15367003", # Biere
      id == "15089080_2007" ~ "15367007", # Eggersdorf
      id == "15089085_2007" ~ "15362031", #	Eickendorf
      id == "15089160_2007" ~ "15367013", # Großmühlingen
      id == "15089190_2007" ~ "15367015", # Kleinmühlingen
      id == "15089335_2007" ~ "15367027", # Welsleben
      id == "15089370_2007" ~ "15367029", # Zens
      
      
      TRUE ~ ags
    )
  )

# new_cw <- data.frame(
#   ags = c("15086270", "15089040"),
#   ags_name = c("Zeppernick", "Biere"),
#   year = c(2007, 2007),
#   area_cw = c(1, 1), 	
#   pop_cw = c(1, 1),
#   area = c(33620000, 24656261,	),
#   population = c(733, 2431, ),
#   ags_21 = c("15086140", "15089042",	), 
#   ags_name_21 = c("Möckern, Stadt", "Bördeland",)
# )

names(cw)

# bind to cw
cw <- cw |>
  bind_rows(streitholz) |>
  arrange(ags, year)


# Merge crosswalks with election data -------------------------------------

# Merge crosswalks
df_cw <- df |>
  left_join_check_obs(cw, by = c("ags", "year_cw" = "year"))
# number of obs increases: but this is wanted, as we want to harmonize the data

glimpse(df_cw)

# is there any ags that did not get merged to ags_21?
not_merged <- df_cw %>%
  filter(election_year < 2021) %>%
  filter(is.na(ags_21)) %>%
  select(ags, election_year) %>%
  distinct()
not_merged
# now, there is no unsuccessful merge.

# Flag the cases where we had to change the ags
df_cw <- df_cw |>
  mutate(
    flag_unsuccessful_naive_merge = ifelse(id %in% not_merged_naive$id, 1, 0)
  )

table(df_cw$flag_unsuccessful_naive_merge, useNA = "ifany")

glimpse(df_cw)



# Harmonize ---------------------------------------------------------------

# We have four different outcomes per party that we want to harmonize:
# 1. Absolute votes: weighted sum
# 2. Weighted votes: weighted sum
# 3. Absolute seats: weighted sum
# 4. Vote share + turnout: weighted mean

# Weighted sum
sums <- df_cw |>
  group_by(ags_21, ags_name_21, election_year) |>
  summarize_at(
    # 1+2+3: Weighted sum
    vars(eligible_voters:seats_FREIEWÄHLER),
    ~ sum(.x * pop_cw, na.rm = TRUE)
  )

# Weighted mean
means <- df_cw |>
  group_by(ags_21, election_year) |>
  summarize_at(
    # 4: Weighted mean
    vars(prop_CDU:turnout),
    ~ weighted.mean(.x, w = pop_cw, na.rm = TRUE)
  )

  |>
  rename(
    ags = ags_21, year = election_year
  ) |>
  ungroup()


# Create plot -------------------------------------------------------------

# Load municipality level data
muni <- read_rds("data/municipal_covars/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7)) |>
  filter(year >= 2006 & year < 2020)

# Merge
df_final <- muni |>
  left_join_check_obs(df_harm, by = c("ags", "year"))

# Create variable indicating whether there was election in given year
df_final <- df_final |>
  mutate(
    election_bin = ifelse(!is.na(turnout), 1, 0)
  )

glimpse(df_final)

# state variable
df_final <- df_final |>
  mutate(
    state = substr(ags, 1, 2),
    state_name = state_id_to_names(state)
  )

table(df_harm$year)

# create plot_df
plot_df <- df_final |>
  group_by(state_name, year) |>
  summarise(election_bin = max(election_bin, na.rm = TRUE)) |>
  ungroup()

plot_df |>
  ggplot(aes(x = as.factor(year), 
             y = factor(state_name, 
                        levels = rev(levels(factor(state_name)))), 
             fill = as.factor(election_bin))
  ) +
  geom_tile(color = "white") + # Add borders to the squares
  scale_fill_manual(values = c("1" = "darkgrey", "0" = "white"), name = "Election") +
  labs(x = "Year", y = "State") +
  theme_hanno() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave("output/figures/state_elections.pdf", width = 7, height = 4)

move_plots_to_overleaf("code")


### END
