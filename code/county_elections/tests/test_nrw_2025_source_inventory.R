#!/usr/bin/env Rscript

set.seed(20260731)

library(readr)
library(dplyr)

inventory <- read_csv(
  "output/tables/nrw_2025_municipality_source_inventory.csv",
  show_col_types = FALSE,
  col_types = cols(county_ags = col_character())
)

stopifnot(nrow(inventory) == 30L)
stopifnot(!anyDuplicated(inventory$county_ags))
stopifnot(sum(inventory$municipality_rows_in_export) == 364L)
stopifnot(sum(inventory$expected_municipalities) == 364L)
stopifnot(sum(inventory$structured_export_status == "available") == 30L)
stopifnot(all(
  inventory$aggregate_check[inventory$structured_export_status == "available"] ==
    "passed"
))
stopifnot(!any(inventory$structured_export_status == "not_found"))
stopifnot(all(
  inventory$municipality_rows_in_export == inventory$expected_municipalities
))

cat(paste0(
  "NRW 2025 source inventory test passed: all 364 municipalities in the 30 ",
  "Landkreise are recoverable and reconcile to county totals.\n"
))
