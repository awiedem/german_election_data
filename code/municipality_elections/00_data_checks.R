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

test <- kommunalwahlen_merge %>%
  