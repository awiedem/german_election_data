#!/usr/bin/env Rscript

set.seed(20260731)

source("code/county_elections/parsers/pilot_bb_1993_1998_muni_ocr.R")

raw_dir <- file.path(
  "data", "county_elections", "raw", "Kreistagswahlen", "Brandenburg"
)
output_dir <- file.path(raw_dir, "derived", "municipality_pilot")

pilot <- run_bb_1993_1998_municipality_ocr_pilot(
  raw_dir = raw_dir,
  output_dir = output_dir,
  repo_root = "."
)

stopifnot(nrow(pilot$rows) == 4L)
stopifnot(!anyDuplicated(pilot$rows[c("ags", "election_year")]))
stopifnot(all(table(
  pilot$rows$election_year, pilot$rows$unit_type
) == 1L))
stopifnot(all(rowSums(pilot$rows[c(
  "spd", "linke_pds", "cdu", "fdp", "gruene",
  "bue_barnim", "bv", "ev", "eb"
)]) == pilot$rows$valid_votes))

valid_ballots <- pilot$rows$number_voters - pilot$rows$invalid_ballots
stopifnot(all(pilot$rows$valid_votes >= valid_ballots))
stopifnot(all(pilot$rows$valid_votes <= 3L * valid_ballots))
stopifnot(nrow(pilot$audit) == 44L)
stopifnot(nrow(pilot$corrections) == 3L)
stopifnot(all(pilot$corrections$election_year == 1993L))
stopifnot(setequal(
  paste(pilot$corrections$ags, pilot$corrections$field),
  c(
    "12060004 eligible_voters",
    "12060004 linke_pds",
    "12060052 bv"
  )
))
stopifnot(all(pilot$corrections$image_verified))
stopifnot(all(pilot$county_checks$within_county_total))
stopifnot(isTRUE(pilot$feasibility$manual_verified_extraction_feasible))
stopifnot(!pilot$feasibility$automatic_import_ready)
stopifnot(abs(pilot$feasibility$correction_rate - 3 / 44) < 1e-12)

expected_outputs <- file.path(output_dir, c(
  "brandenburg_municipality_pilot_rows.csv",
  "brandenburg_municipality_pilot_ocr_audit.csv",
  "brandenburg_municipality_pilot_corrections.csv",
  "brandenburg_municipality_pilot_county_checks.csv",
  "brandenburg_municipality_pilot_feasibility.csv"
))
stopifnot(all(file.exists(expected_outputs)))
stopifnot(all(file.info(expected_outputs)$size > 50L))

cat(
  "Brandenburg 1993/1998 municipality OCR pilot passed:",
  nrow(pilot$rows), "rows,", nrow(pilot$corrections), "corrections across",
  nrow(pilot$audit), "numeric fields.\n"
)
