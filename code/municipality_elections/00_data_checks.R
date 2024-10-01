kommunalwahlen_merge[grepl("\.", kommunalwahlen_merge$AGS_8dig),]


non_digit_matrix <- grepl("[^0-9]", as.matrix(kommunalwahlen_merge$AGS_8dig))

# Identify rows that have any non-digit characters
rows_with_non_digits <- kommunalwahlen_merge[rowSums(non_digit_matrix) > 0, ]


kommunalwahlen_merge[grepl("\\.", kommunalwahlen_merge$AGS_8dig),]


table(grepl("\\.", kommunalwahlen_merge$AGS_8dig))

table(nchar(kommunalwahlen_merge$AGS_8dig))

names(kommunalwahlen_merge)


# Define the Bundesland to filter by
bundesland_filter <- "Schleswig-Holstein"  # Replace with your desired Bundesland

# Define the two-digit string to check in AGS_8dig
ags_prefix <- "01"  # Replace with your desired two-digit prefix

# Filter the dataframe by Bundesland
filtered_df <- kommunalwahlen_merge[kommunalwahlen_merge$Bundesland == bundesland_filter, ]

# Further filter rows where AGS_8dig starts with the specified two-digit string
final_df <- filtered_df[!grep(paste0("^", ags_prefix), filtered_df$AGS_8dig), ]

table(nchar(filtered_df$AGS_8dig))

test <- sachsen_kommunalwahlen %>%
  group_by(AGS_8dig, election_year) %>%
  mutate(
    n_unique = n()
  ) %>%
  ungroup() %>%
  filter(n_unique > 1)


table(grepl("\\.", kommunalwahlen_merge$ags))


# Check missigness --------------------------------------------------------

# Load municipality level data
muni <- read_rds("data/covars_municipality/final/ags_area_pop_emp.rds") |>
  rename(ags = ags_21) |>
  mutate(ags = pad_zero_conditional(ags, 7))

df_harm <- read_rds("data/municipal_elections/final/municipal_harm.rds")

# Merge
df_final <- muni |>
  left_join_check_obs(df_harm, by = c("ags", "year")) |>
  mutate(
    state = substr(ags, 1, 2),
    state = pad_zero_conditional(state, 1),
    state_name = state_id_to_names(state)
  )

# Create variable indicating whether there was election in given year
df_final <- df_final |>
  # election == 1 if any ags in a state and year has election
  group_by(state, year) |>
  mutate(
    election = ifelse(any(!is.na(turnout)), 1, 0)
  ) |>
  ungroup()

# niedersachsen
niedersachsen <- df_final |>
  filter(state == "03") |>
  arrange(ags, year)

# Filter all ags where election == 1 but turnout is missing and that have other observations with non-missing turnout
ever_election <- df_final |>
  filter(!is.na(turnout) == 1) |>
  distinct(ags)

# Filter all ags where election == 1 but turnout is missing and that have other observations with non-missing turnout
inspection <- df_final |>
  filter(election == 1 & is.na(turnout) == 1) |>
  filter(ags %in% ever_election$ags | population_ags > 1) |>
  mutate(state = state_id_to_names(state)) |>
  select(ags, ags_name_21, year, state, population_ags, turnout) |>
  arrange(state, ags, year) |>
  filter(state != "Saxony-Anhalt")

# any duplicates?
df_final %>%
  group_by(ags, year) %>%
  summarize(n = n()) %>%
  filter(n > 1)
# none

fwrite(inspection, "data/municipal_elections/missing_ags.csv")

table(inspection$year, inspection$state, useNA = "ifany")

  