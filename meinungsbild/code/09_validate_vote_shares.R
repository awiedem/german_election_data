# ==============================================================================
# 09_validate_vote_shares.R
# Validate MRP county-level vote share predictions against BTW 2021 results
# ==============================================================================

set.seed(20260404)

library(tidyverse)
library(ggrepel)

# --- Paths -------------------------------------------------------------------

base_dir   <- "meinungsbild"
gerda_dir  <- "data/federal_elections/county_level/final"
output_dir <- file.path(base_dir, "output", "validation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load MRP estimates ------------------------------------------------------

estimates_kreis <- readRDS(file.path(base_dir, "output", "estimates_kreis.rds"))

# Filter to vote_* issues
vote_issues <- c("vote_cdu", "vote_spd", "vote_gruene", "vote_fdp",
                  "vote_afd", "vote_linke")

mrp <- estimates_kreis |>
  filter(issue_id %in% vote_issues) |>
  select(issue_id, county_code, estimate)

stopifnot("No vote_* issues found in estimates_kreis" = nrow(mrp) > 0)

message("MRP estimates: ", n_distinct(mrp$issue_id), " issues, ",
        n_distinct(mrp$county_code), " counties")

# --- Load GERDA ground truth (BTW 2021) --------------------------------------

fed_cty <- readRDS(file.path(gerda_dir, "federal_cty_harm.rds"))

btw21 <- fed_cty |>
  filter(election_year == 2021) |>
  select(county_code, cdu_csu, spd, gruene, fdp, afd, linke_pds)

stopifnot("No BTW 2021 data found" = nrow(btw21) > 0)
message("GERDA BTW 2021: ", nrow(btw21), " counties")

# Reshape to long format matching MRP issue_id naming
btw21_long <- btw21 |>
  pivot_longer(-county_code, names_to = "party_col", values_to = "actual") |>
  mutate(issue_id = case_when(
    party_col == "cdu_csu"   ~ "vote_cdu",
    party_col == "spd"       ~ "vote_spd",
    party_col == "gruene"    ~ "vote_gruene",
    party_col == "fdp"       ~ "vote_fdp",
    party_col == "afd"       ~ "vote_afd",
    party_col == "linke_pds" ~ "vote_linke"
  )) |>
  select(county_code, issue_id, actual)

# --- Merge MRP predictions with actual results --------------------------------

# Ensure county_code types match
mrp <- mrp |> mutate(county_code = as.character(county_code))
btw21_long <- btw21_long |> mutate(county_code = as.character(county_code))

val <- inner_join(mrp, btw21_long, by = c("county_code", "issue_id"))

n_matched <- n_distinct(val$county_code)
message("Matched counties: ", n_matched, " / ", n_distinct(btw21_long$county_code),
        " GERDA, ", n_distinct(mrp$county_code), " MRP")

if (n_matched < 100) {
  warning("Few counties matched. Check county_code alignment between MRP and GERDA.")
  # Try zero-padding if needed
  mrp_padded <- mrp |> mutate(county_code = str_pad(county_code, 5, pad = "0"))
  btw21_padded <- btw21_long |> mutate(county_code = str_pad(county_code, 5, pad = "0"))
  val_padded <- inner_join(mrp_padded, btw21_padded, by = c("county_code", "issue_id"))
  if (nrow(val_padded) > nrow(val)) {
    message("Zero-padding improved match: ", n_distinct(val_padded$county_code), " counties")
    val <- val_padded
  }
}

# --- Compute validation metrics per party ------------------------------------

party_labels <- c(
  vote_cdu    = "CDU/CSU",
  vote_spd    = "SPD",
  vote_gruene = "Grüne",
  vote_fdp    = "FDP",
  vote_afd    = "AfD",
  vote_linke  = "DIE LINKE"
)

metrics <- val |>
  group_by(issue_id) |>
  summarise(
    n_counties  = n(),
    r           = cor(estimate, actual, use = "complete.obs"),
    rmse_pp     = sqrt(mean((estimate - actual)^2, na.rm = TRUE)) * 100,
    bias_pp     = mean(estimate - actual, na.rm = TRUE) * 100,
    mean_mrp    = mean(estimate, na.rm = TRUE) * 100,
    mean_actual = mean(actual, na.rm = TRUE) * 100,
    .groups     = "drop"
  ) |>
  mutate(party = party_labels[issue_id]) |>
  arrange(desc(r)) |>
  select(party, issue_id, n_counties, r, rmse_pp, bias_pp, mean_mrp, mean_actual)

message("\n=== Validation: MRP vote share vs. BTW 2021 actual ===\n")
print(metrics, n = 10)

write_csv(metrics, file.path(output_dir, "vote_share_validation.csv"))
message("\nSaved: ", file.path(output_dir, "vote_share_validation.csv"))

# --- Scatter plots per party -------------------------------------------------

scatter_data <- val |>
  mutate(
    estimate_pct = estimate * 100,
    actual_pct   = actual * 100,
    party        = party_labels[issue_id]
  )

p_scatter <- ggplot(scatter_data, aes(x = actual_pct, y = estimate_pct)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, color = "steelblue") +
  facet_wrap(~ party, scales = "free") +
  labs(
    x = "Actual vote share (BTW 2021, %)",
    y = "MRP predicted vote share (%)",
    title = "MRP vote share validation: county-level predictions vs. BTW 2021",
    subtitle = paste0("N = ", n_matched, " counties")
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# Add r and RMSE annotations
label_data <- metrics |>
  mutate(
    party = party_labels[issue_id],
    label = paste0("r = ", round(r, 3), "\nRMSE = ", round(rmse_pp, 1), "pp")
  )

# Position labels in upper-left of each facet
p_scatter <- p_scatter +
  geom_text(
    data = label_data,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.3,
    size = 3, color = "grey30",
    inherit.aes = FALSE
  )

ggsave(file.path(output_dir, "vote_share_scatter.pdf"),
       p_scatter, width = 10, height = 7)
message("Saved: ", file.path(output_dir, "vote_share_scatter.pdf"))

# --- Summary statistics ------------------------------------------------------

message("\n=== Summary ===")
message("Median r:    ", round(median(metrics$r), 3))
message("Median RMSE: ", round(median(metrics$rmse_pp), 1), " pp")
message("Median bias: ", round(median(metrics$bias_pp), 1), " pp")
