set.seed(20260730)

library(tidyverse)

source("code/county_elections/county_election_support.R")

manifest_path <- "data/county_elections/final/county_election_coverage.csv"
assert_county_election_sources(manifest_path, "county-election coverage manifest")

state_order <- c(
  "Schleswig-Holstein", "Lower Saxony", "North Rhine-Westphalia", "Hesse",
  "Rhineland-Palatinate", "Baden-Württemberg", "Bavaria", "Saarland",
  "Brandenburg", "Mecklenburg-Vorpommern", "Saxony", "Saxony-Anhalt",
  "Thuringia"
)

status_labels <- c(
  municipality_available = "Municipality results",
  municipality_partial = "Municipality results, partial",
  county_only = "County totals only",
  raw_only = "Raw files, parser needed",
  missing_source = "Source needed"
)

coverage <- read_csv(manifest_path, show_col_types = FALSE) |>
  filter(!is.na(election_year), coverage_status != "not_applicable") |>
  mutate(
    state_name = factor(state_name, levels = rev(state_order)),
    coverage_status = factor(
      coverage_status,
      levels = names(status_labels),
      labels = unname(status_labels)
    )
  )

if (anyNA(coverage$coverage_status)) {
  stop("Coverage manifest contains an unknown status.", call. = FALSE)
}
if (anyDuplicated(coverage[c("state", "election_year")])) {
  stop("Coverage manifest has duplicate state x election-year rows.", call. = FALSE)
}

decade_lines <- tibble(election_year = c(1990L, 2000L, 2010L, 2020L))

coverage_plot <- ggplot(
  coverage,
  aes(x = election_year, y = state_name)
) +
  geom_vline(
    data = decade_lines,
    aes(xintercept = election_year),
    colour = "#E4DED4",
    linewidth = 0.35
  ) +
  geom_point(
    aes(shape = coverage_status, colour = coverage_status),
    size = 3.0,
    stroke = 0.8
  ) +
  scale_shape_manual(
    values = c(
      "Municipality results" = 16,
      "Municipality results, partial" = 17,
      "County totals only" = 15,
      "Raw files, parser needed" = 1,
      "Source needed" = 4
    )
  ) +
  scale_colour_manual(
    values = c(
      "Municipality results" = "#263238",
      "Municipality results, partial" = "#8F6B32",
      "County totals only" = "#B56B2D",
      "Raw files, parser needed" = "#4C6A76",
      "Source needed" = "#C8C1B7"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1990L, 2025L, 5L),
    minor_breaks = NULL,
    limits = c(1989.5, 2026.5),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "County-election data coverage",
    subtitle = "Each mark is an expected election event, 1990-2026",
    x = NULL,
    y = NULL,
    shape = NULL,
    colour = NULL,
    caption = paste(
      "County totals include Kreistage and separately labelled",
      "county-equivalent city councils."
    )
  ) +
  guides(
    shape = guide_legend(nrow = 1, byrow = TRUE),
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(colour = "#2D2B28", hjust = 1),
    axis.text.x = element_text(colour = "#5D5851"),
    plot.title = element_text(face = "bold", size = 16, colour = "#22201D"),
    plot.subtitle = element_text(colour = "#5D5851", margin = margin(b = 14)),
    plot.caption = element_text(colour = "#6B655D", hjust = 0, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left",
    legend.margin = margin(t = 8),
    plot.margin = margin(14, 18, 12, 12)
  )

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(
  "output/figures/county_election_coverage.pdf",
  coverage_plot,
  width = 11,
  height = 6.5,
  device = cairo_pdf
)
ggsave(
  "output/figures/county_election_coverage.png",
  coverage_plot,
  width = 11,
  height = 6.5,
  dpi = 180,
  bg = "white"
)

for (path in c(
  "output/figures/county_election_coverage.pdf",
  "output/figures/county_election_coverage.png"
)) {
  if (!file.exists(path) || file.info(path)$size < 1000L) {
    stop("Coverage figure was not created correctly: ", path, call. = FALSE)
  }
}

print(coverage_plot)
