#!/usr/bin/env Rscript

set.seed(20260731)

library(readr)
library(dplyr)

source("code/county_elections/04_county_election_coverage.R")

manifest <- read_csv(
  "data/county_elections/final/county_election_coverage.csv",
  show_col_types = FALSE,
  col_types = cols(state = col_character())
)
manifest <- manifest |> mutate(election_year = as.integer(election_year))

required_columns <- c(
  "state", "election_year", "coverage_status", "gap_type",
  "acquisition_feasibility", "priority", "next_action"
)
stopifnot(all(required_columns %in% names(manifest)))
stopifnot(!anyDuplicated(manifest |> filter(!is.na(election_year)) |>
  select(state, election_year)))
stopifnot(!anyNA(manifest$gap_type))
stopifnot(!anyNA(manifest$acquisition_feasibility))
stopifnot(!anyNA(manifest$priority))
stopifnot(!anyNA(manifest$next_action))

expected_priorities <- tribble(
  ~state, ~election_year, ~priority,
  "01", 1990L, "P0",
  "01", 1994L, "P0",
  "07", 1994L, "P0",
  "07", 1999L, "P1",
  "07", 2004L, "P1",
  "07", 2009L, "P1",
  "07", 2014L, "P1",
  "07", 2019L, "P1",
  "12", 1993L, "P2",
  "12", 1998L, "P2"
)

observed_priorities <- manifest |>
  inner_join(expected_priorities |> select(state, election_year),
             by = c("state", "election_year")) |>
  select(state, election_year, priority)

stopifnot(
  identical(
    arrange(observed_priorities, state, election_year),
    arrange(expected_priorities, state, election_year)
  )
)
stopifnot(
  manifest |>
    filter(state == "13", election_year == 2011L) |>
    pull(gap_type) == "partial_unallocatable_postal_pools"
)
stopifnot(
  all(
    manifest |>
      filter(coverage_status == "municipality_available") |>
      pull(gap_type) == "none"
  )
)

nrw_2025 <- manifest |>
  filter(state == "05", election_year == 2025L)
stopifnot(
  nrow(nrw_2025) == 1L,
  nrw_2025$n_municipality_records == 374L,
  nrw_2025$n_city_council_records == 22L,
  abs(nrw_2025$municipality_record_ratio - 1) < 1e-12,
  nrw_2025$coverage_status == "municipality_available",
  nrw_2025$gap_type == "none",
  nrw_2025$priority == "none",
  nrw_2025$next_action == "none"
)

cat("County-election coverage manifest test passed.\n")
