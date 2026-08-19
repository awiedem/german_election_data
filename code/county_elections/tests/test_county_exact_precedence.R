#!/usr/bin/env Rscript

set.seed(20260731)

library(dplyr)

unharm <- readRDS("data/county_elections/final/county_elec_unharm.rds")
county_harm <- readRDS("data/county_elections/final/county_elec_harm_21_cty.rds")

events <- tibble::tribble(
  ~state, ~election_year, ~expected_counties,
  "07", 2024L, 36L,
  "13", 2011L, 6L
)

for (i in seq_len(nrow(events))) {
  event <- events[i, ]
  exact <- unharm |>
    filter(
      state == event$state,
      election_year == event$election_year,
      result_level == "county"
    )
  output <- county_harm |>
    filter(
      state == event$state,
      election_year == event$election_year
    ) |>
    rename(county = county_code)

  stopifnot(nrow(exact) == event$expected_counties)
  stopifnot(nrow(output) == event$expected_counties)
  stopifnot(!anyDuplicated(exact$county))
  stopifnot(!anyDuplicated(output$county))

  fields <- intersect(
    c(
      "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
      "turnout", "cdu", "spd", "gruene", "fdp", "afd", "linke_pds", "fwg"
    ),
    intersect(names(exact), names(output))
  )
  comparison <- exact |>
    select(county, all_of(fields)) |>
    inner_join(
      output |> select(county, all_of(fields)),
      by = "county",
      suffix = c("_exact", "_output")
    )
  stopifnot(nrow(comparison) == event$expected_counties)

  for (field in fields) {
    exact_values <- comparison[[paste0(field, "_exact")]]
    output_values <- comparison[[paste0(field, "_output")]]
    # The harmonized output deliberately recodes never-contesting party zeros
    # to NA. Treat only that zero/NA pair as equivalent.
    equivalent <- isTRUE(all.equal(exact_values, output_values,
                                   tolerance = 1e-10, check.attributes = FALSE)) ||
      all(
        is.na(exact_values) & is.na(output_values) |
          !is.na(exact_values) & !is.na(output_values) &
            abs(exact_values - output_values) <= 1e-10 |
          exact_values == 0 & is.na(output_values) |
          is.na(exact_values) & output_values == 0,
        na.rm = TRUE
      )
    stopifnot(equivalent)
  }
}

cat(paste0(
  "Exact county precedence test passed for RLP 2024 (36 rows) and MV 2011 ",
  "(6 rows); incomplete lower-level detail did not replace official totals.\n"
))
