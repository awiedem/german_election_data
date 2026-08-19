set.seed(20260730)

library(tidyverse)
library(scales)

year_min <- 1990L
year_max <- 2025L

input_files <- c(
  "Municipal" = "data/municipal_elections/final/municipal_harm.rds",
  "County" = "data/county_elections/final/county_elec_harm_21_muni.rds",
  "Mayoral" = "data/mayoral_elections/final/mayoral_harm.rds",
  "State" = "data/state_elections/final/state_harm_21.rds",
  "Federal" = "data/federal_elections/municipality_level/final/federal_muni_harm_21.rds",
  "European" = "data/european_elections/final/european_muni_harm.rds"
)

missing_files <- input_files[!file.exists(input_files)]
if (length(missing_files) > 0L) {
  stop("Missing input files: ", paste(missing_files, collapse = ", "))
}

is_lfs_pointer <- function(path) {
  identical(readLines(path, n = 1L, warn = FALSE), "version https://git-lfs.github.com/spec/v1")
}

normalize_ags <- function(x) {
  x <- as.character(x)
  if_else(
    nchar(x) == 9L & str_starts(x, "0"),
    str_sub(x, 2L),
    x
  )
}

lfs_pointers <- input_files[vapply(input_files, is_lfs_pointer, logical(1))]
if (length(lfs_pointers) > 0L) {
  stop(
    "The following inputs are Git LFS pointers rather than data: ",
    paste(lfs_pointers, collapse = ", "),
    ". Run git lfs pull for these files."
  )
}

state_lookup <- tribble(
  ~state, ~state_name, ~state_label,
  "01", "Schleswig-Holstein", "SH",
  "02", "Hamburg", "HH",
  "03", "Lower Saxony", "NI",
  "04", "Bremen", "HB",
  "05", "North Rhine-Westphalia", "NW",
  "06", "Hesse", "HE",
  "07", "Rhineland-Palatinate", "RP",
  "08", "Baden-Württemberg", "BW",
  "09", "Bavaria", "BY",
  "10", "Saarland", "SL",
  "11", "Berlin", "BE",
  "12", "Brandenburg", "BB",
  "13", "Mecklenburg-Vorpommern", "MV",
  "14", "Saxony", "SN",
  "15", "Saxony-Anhalt", "ST",
  "16", "Thuringia", "TH"
)

municipality_universe <- readRDS(input_files[["Municipal"]]) |>
  transmute(
    ags = normalize_ags(ags),
    state = substr(ags, 1L, 2L)
  ) |>
  filter(state %in% state_lookup$state) |>
  distinct(ags, state)

state_denominators <- municipality_universe |>
  count(state, name = "n_municipalities_state")

read_coverage_input <- function(path, election_type) {
  dat <- readRDS(path)

  required_columns <- c("ags", "election_year")
  absent_columns <- setdiff(required_columns, names(dat))
  if (length(absent_columns) > 0L) {
    stop(
      election_type, " input is missing required columns: ",
      paste(absent_columns, collapse = ", ")
    )
  }

  dat |>
    transmute(
      election_type = .env$election_type,
      ags = normalize_ags(ags),
      state = substr(ags, 1L, 2L),
      election_year = as.integer(election_year)
    ) |>
    filter(
      state %in% state_lookup$state,
      dplyr::between(election_year, year_min, year_max)
    ) |>
    distinct()
}

coverage_records <- imap_dfr(input_files, ~ read_coverage_input(.x, .y)) |>
  left_join(
    municipality_universe |> mutate(in_2021_universe = TRUE),
    by = c("ags", "state")
  )

coverage_observed <- coverage_records |>
  group_by(election_type, state, election_year) |>
  summarise(
    n_records_total = n_distinct(ags),
    n_municipalities = n_distinct(ags[in_2021_universe %in% TRUE]),
    n_outside_universe = n_distinct(ags[is.na(in_2021_universe)]),
    .groups = "drop"
  )

coverage_grid <- crossing(
  election_type = factor(names(input_files), levels = names(input_files)),
  state = state_lookup$state,
  election_year = seq.int(year_min, year_max)
) |>
  left_join(coverage_observed, by = c("election_type", "state", "election_year")) |>
  left_join(state_denominators, by = "state") |>
  left_join(state_lookup, by = "state") |>
  mutate(
    election_type = factor(election_type, levels = names(input_files)),
    across(c(n_records_total, n_municipalities, n_outside_universe), ~ replace_na(.x, 0L)),
    coverage = n_municipalities / n_municipalities_state,
    coverage_plot = if_else(n_records_total == 0L, NA_real_, coverage),
    state_label = factor(state_label, levels = rev(state_lookup$state_label))
  )

stopifnot(
  nrow(coverage_grid) == length(input_files) * nrow(state_lookup) *
    (year_max - year_min + 1L),
  !anyDuplicated(coverage_grid[c("election_type", "state", "election_year")]),
  all(dplyr::between(coverage_grid$coverage, 0, 1)),
  all(names(input_files) %in% coverage_grid$election_type)
)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

write_csv(
  coverage_grid |>
    transmute(
      election_type = as.character(election_type),
      state,
      state_name,
      election_year,
      n_records_total,
      n_municipalities,
      n_outside_universe,
      n_municipalities_state,
      coverage
    ),
  "output/tables/election_coverage.csv"
)

state_type_layout <- tribble(
  ~election_type, ~x_offset,
  "Municipal", -0.24,
  "County", -0.08,
  "Mayoral", 0.08,
  "State", 0.24
)

state_events <- coverage_grid |>
  filter(n_records_total > 0L, election_type %in% state_type_layout$election_type) |>
  mutate(election_type = as.character(election_type)) |>
  left_join(state_type_layout, by = "election_type") |>
  mutate(
    state_y = 17L - match(state, state_lookup$state),
    x_position = election_year + x_offset,
    y_position = state_y,
    coverage_class = if_else(
      coverage >= 0.95,
      "Near-complete (≥95%)",
      "Partial (<95%)"
    )
  )

national_events <- coverage_grid |>
  filter(n_records_total > 0L, election_type %in% c("Federal", "European")) |>
  mutate(election_type = as.character(election_type)) |>
  group_by(election_type, election_year) |>
  summarise(
    n_municipalities = sum(n_municipalities),
    n_municipalities_state = sum(n_municipalities_state),
    .groups = "drop"
  ) |>
  mutate(
    coverage = n_municipalities / n_municipalities_state,
    x_position = election_year,
    y_position = if_else(election_type == "Federal", 18, 17),
    coverage_class = if_else(
      coverage >= 0.95,
      "Near-complete (≥95%)",
      "Partial (<95%)"
    )
  )

coverage_events <- bind_rows(
  state_events |>
    select(election_type, election_year, coverage, coverage_class, x_position, y_position),
  national_events |>
    select(election_type, election_year, coverage, coverage_class, x_position, y_position)
)

stopifnot(
  nrow(state_events) ==
    sum(coverage_grid$n_records_total > 0L &
      as.character(coverage_grid$election_type) %in% state_type_layout$election_type),
  !anyNA(coverage_events[c("x_position", "y_position", "coverage_class")]),
  all(dplyr::between(coverage_events$coverage, 0, 1))
)

decade_bands <- tibble(
  xmin = c(1989.5, 2009.5),
  xmax = c(1999.5, 2019.5),
  ymin = 0.5,
  ymax = 18.5
)

coverage_plot <- ggplot() +
  geom_rect(
    data = decade_bands,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#EFE5D5",
    colour = NA
  ) +
  geom_hline(
    yintercept = seq(1.5, 15.5, by = 1),
    colour = "#E5D9C8",
    linewidth = 0.25
  ) +
  geom_hline(
    yintercept = 16.5,
    colour = "#CDBEA9",
    linewidth = 0.55
  ) +
  geom_point(
    data = coverage_events,
    aes(
      x = x_position,
      y = y_position,
      fill = election_type,
      alpha = coverage_class
    ),
    shape = 21,
    size = 3.1,
    stroke = 0.25,
    colour = "#FFF8ED"
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5),
    minor_breaks = NULL,
    limits = c(1989.5, 2025.5),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = c(18, 17, seq(16, 1)),
    labels = c("FED", "EU", state_lookup$state_label),
    limits = c(0.5, 18.5),
    expand = expansion(mult = 0)
  ) +
  scale_fill_manual(
    values = c(
      "Municipal" = "#009E9A",
      "County" = "#F28E2B",
      "Mayoral" = "#E05263",
      "State" = "#8B5FBF",
      "Federal" = "#3374D5",
      "European" = "#E5B000"
    ),
    breaks = c(
      state_type_layout$election_type,
      "Federal",
      "European"
    ),
    name = NULL
  ) +
  scale_alpha_manual(
    values = c(
      "Near-complete (≥95%)" = 1,
      "Partial (<95%)" = 0.25
    ),
    breaks = c("Near-complete (≥95%)", "Partial (<95%)"),
    name = "Coverage"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(alpha = 1, size = 4)
    ),
    alpha = guide_legend(
      order = 2,
      nrow = 1,
      override.aes = list(fill = "#5A5047", size = 4)
    )
  ) +
  labs(
    title = "When Germany votes",
    subtitle = "GERDA election coverage across sixteen states, 1990–2025",
    x = NULL,
    y = NULL,
    caption = str_wrap(
      paste0(
        "Federal and European elections appear once in the two national rows. Within each state, ",
        "marks are ordered municipal, county, mayoral, and state from left to right. Solid marks ",
        "cover at least 95% of 2021-boundary municipalities; faded marks have partial coverage. ",
        "Empty positions mean no records, not necessarily no election."
      ),
      width = 150
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.background = element_rect(fill = "#FFF8ED", colour = NA),
    panel.background = element_rect(fill = "#FFF8ED", colour = NA),
    legend.background = element_rect(fill = "#FFF8ED", colour = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 9, colour = "#675B50"),
    axis.text.y = element_text(size = 9.5, face = "bold", colour = "#4A4038"),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = "left",
    legend.key.width = unit(16, "pt"),
    legend.spacing.y = unit(1, "pt"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", colour = "#2B2118", size = 24),
    plot.subtitle = element_text(colour = "#675B50", size = 11.5),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, colour = "#766A5F", size = 8.5),
    plot.margin = margin(12, 14, 10, 12)
  )

ggsave(
  "output/figures/election_coverage.pdf",
  plot = coverage_plot,
  device = cairo_pdf,
  width = 12,
  height = 7.2,
  units = "in"
)

ggsave(
  "output/figures/election_coverage.png",
  plot = coverage_plot,
  width = 12,
  height = 7.2,
  units = "in",
  dpi = 300
)

message(
  "Wrote coverage plot for ", nrow(coverage_observed),
  " observed election type-state-year cells. ",
  sum(coverage_observed$n_outside_universe),
  " distinct cell-level AGS records fall outside the 2021 municipality universe."
)
