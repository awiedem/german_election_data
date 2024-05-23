
df_harm <- fread("data/federal_elections/municipality_level/processed_data/df_harm.csv")

# inspect ---------------------------------------------------------------- (hanno)
inspect <- df_harm %>%
  select(-c(left_wing, left_wing_wLinke, right_wing, cdu_csu)) %>%
  mutate(total_votes = rowSums(select(., cdu:zentrum), na.rm = TRUE))

mean(inspect$total_votes > 1.000001) * 100
quantile(inspect$total_votes, probs = seq(0.90, 1, 0.01))
quantile(inspect$total_votes, probs = seq(0.0, 0.1, 0.01))
nrow(inspect)

## Code below is based on previous procedure where we had places w/ sum > 1 -> this is not the case anymore now

# Sum greater than one ---------------------------------------------

df_harm_greater_one <- inspect %>%
  filter(election_year > 2004) %>%
  group_by(ags) %>%
  filter(any(total_votes > 1.07)) %>%
  distinct(ags, .keep_all = T) %>%
  ungroup()

## Compare this to the df that indicates whether there was ever merger

df_harm_greater_one <- df_harm_greater_one %>%
  left_join_check_obs(
    cw_info_ever_merged_ags_21 %>%
      mutate(ags_21 = as.character(ags_21)) %>%
      mutate(ags_21 = pad_zero_conditional(ags_21, 7)),
    by = c("ags" = "ags_21")
  )

## Pick a random AGs

ags_check <- df_harm_greater_one %>%
  filter(ever_merged == 0) %>%
  slice(1) %>%
  pull(ags)

## Check original data set

cw_check <- cw %>%
  filter(ags_21 == as.numeric(ags_check))

glimpse(cw_check)

## This is the original election df

df_check <- df %>%
  filter(ags == as.numeric(ags_check)) %>%
  dplyr::select(-right_wing, -left_wing, -left_wing_wLinke, -cdu_csu)

## Row sums of all parties as above:

df_check <- df_check %>%
  mutate(
    total_votes = rowSums(select(., cdu:zentrum), na.rm = TRUE)
  )

View(df_check)

## Compare to valid votes

data.frame(
  valid = df_check$valid_votes,
  total = df_check$total_votes
) %>%
  mutate(
    diff = valid - total,
    ratio = valid / total
  )

## Check the same place in the harmonized data

df_harm_check <- inspect %>%
  filter(ags == ags_check)

df_harm_check$total_votes

View(df_harm_check)

## Check distribution of population in inspect for total_votes > 1.0001

inspect %>%
  filter(total_votes > 1.002) %>%
  pull(valid_votes) %>%
  summary()

## all these places are pretty small

glimpse(df_check)

