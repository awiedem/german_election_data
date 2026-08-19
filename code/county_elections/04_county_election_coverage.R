set.seed(20260730)

library(tidyverse)

source("code/county_elections/county_election_support.R")

input_path <- "data/county_elections/final/county_elec_unharm.rds"
output_path <- "data/county_elections/final/county_election_coverage.csv"
additions_path <- "output/tables/county_election_additions.csv"
denominator_paths <- c(
  "data/municipal_elections/final/municipal_harm.rds",
  "data/county_elections/final/county_council_seats.rds"
)

assert_county_election_sources(
  c(input_path, denominator_paths),
  "coverage inputs"
)

observed <- readRDS(input_path) |>
  as_tibble() |>
  mutate(
    ags = stringr::str_pad(as.character(ags), 8L, pad = "0"),
    county = stringr::str_pad(as.character(county), 5L, pad = "0"),
    state = stringr::str_pad(as.character(state), 2L, pad = "0"),
    election_year = as.integer(election_year)
  ) |>
  add_county_election_metadata()

if (anyDuplicated(observed[c("ags", "election_year")])) {
  stop("Unharmonized data contain duplicate AGS x election-year rows.", call. = FALSE)
}

# Detect tracked raw files independently of whether Git LFS content is hydrated.
# Observed parser output takes precedence over raw-file availability.
raw_inventory <- county_election_raw_file_inventory() |>
  semi_join(
    county_election_expected_events(),
    by = c("state", "election_year")
  ) |>
  mutate(source_note = paste0("Repository raw source: ", raw_files))

observed_events <- observed |>
  group_by(state, election_year) |>
  summarise(
    n_municipality_records = n_distinct(ags[result_level == "municipality"]),
    n_city_council_records = n_distinct(ags[
      result_level == "county" & contest_type == "kreisfreie_city_council"
    ]),
    n_county_records = n_distinct(county[result_level == "county"]),
    n_limited_records = n_distinct(ags[source_limitation %in% TRUE]),
    observed_result_levels = paste(sort(unique(result_level)), collapse = "+"),
    observed_contest_types = paste(sort(unique(contest_type)), collapse = "+"),
    observed_source_notes = {
      notes <- sort(unique(stats::na.omit(source_note)))
      if (length(notes)) paste(notes, collapse = " | ") else NA_character_
    },
    .groups = "drop"
  )

municipality_denominators <- readRDS(
  "data/municipal_elections/final/municipal_harm.rds"
) |>
  as_tibble() |>
  transmute(
    ags = stringr::str_pad(as.character(ags), 8L, pad = "0"),
    state = substr(ags, 1L, 2L)
  ) |>
  distinct(ags, state) |>
  count(state, name = "n_municipalities_2021")

county_denominators <- readRDS(
  "data/county_elections/final/county_council_seats.rds"
) |>
  as_tibble() |>
  distinct(state, county) |>
  count(state, name = "n_counties_current")

coverage <- county_election_expected_events() |>
  left_join(observed_events, by = c("state", "election_year")) |>
  left_join(raw_inventory, by = c("state", "election_year")) |>
  left_join(municipality_denominators, by = "state") |>
  left_join(county_denominators, by = "state") |>
  mutate(
    across(
      c(
        n_municipality_records, n_city_council_records, n_county_records,
        n_limited_records
      ),
      ~ replace_na(.x, 0L)
    ),
    coverage_status = case_when(
      state == "13" & election_year == 2011L &
        n_municipality_records > 0L ~ "municipality_partial",
      n_municipality_records > 0L ~ "municipality_available",
      n_county_records > 0L ~ "county_only",
      !is.na(raw_files) ~ "raw_only",
      TRUE ~ "missing_source"
    ),
    recommended_action = case_when(
      coverage_status == "municipality_available" ~ "none",
      coverage_status == "municipality_partial" ~
        "retain exact municipality rows and separate postal pools; do not impute",
      coverage_status == "county_only" ~
        "retain county totals; acquire municipality contributions only if needed",
      coverage_status == "raw_only" ~ "build and validate parser for existing raw files",
      coverage_status == "missing_source" ~
        "acquire official county totals and, where available, municipality contributions"
    ),
    municipality_record_ratio =
      (n_municipality_records + n_city_council_records) /
      n_municipalities_2021,
    county_record_ratio = n_county_records / n_counties_current
  ) |>
  mutate(
    gap_type = case_when(
      coverage_status == "municipality_available" ~ "none",
      state == "13" & election_year == 2011L ~
        "partial_unallocatable_postal_pools",
      state == "01" & election_year %in% c(1990L, 1994L) ~ "raw_detail_unparsed",
      state == "07" & election_year == 1994L ~ "missing_election_event",
      state == "12" & election_year %in% c(1993L, 1998L) ~
        "raw_detail_unparsed",
      coverage_status == "county_only" ~ "county_only_missing_municipal_detail",
      coverage_status == "raw_only" ~ "raw_detail_unparsed",
      coverage_status == "missing_source" ~ "missing_election_event",
      TRUE ~ "none"
    ),
    acquisition_feasibility = case_when(
      coverage_status == "municipality_available" ~ "complete",
      state == "13" & election_year == 2011L ~ "structurally_limited",
      state == "01" & election_year %in% c(1990L, 1994L) ~
        "low_dedicated_cell_ocr_and_manual_verification_required",
      state == "07" & election_year == 1994L ~ "medium_official_request",
      state == "05" & election_year == 2025L ~
        "medium_27_exports_available_3_counties_require_request",
      state == "07" & election_year %in% c(1999L, 2004L, 2009L, 2014L, 2019L) ~
        "medium_official_request",
      state == "12" & election_year %in% c(1993L, 1998L) ~
        "medium_pilot_passed_manual_image_verification_required",
      state == "08" ~ "low_not_centrally_published",
      state == "09" ~ "low_not_centrally_published",
      state == "14" & election_year == 1994L ~ "low_official_or_decentralized_request",
      state == "05" & election_year == 1994L ~ "low_official_request",
      coverage_status == "county_only" ~ "unknown_requires_source_search",
      coverage_status == "raw_only" ~ "medium_existing_raw_source",
      coverage_status == "missing_source" ~ "unknown_requires_acquisition",
      TRUE ~ "complete"
    ),
    priority = case_when(
      coverage_status == "municipality_available" ~ "none",
      state == "01" & election_year %in% c(1990L, 1994L) ~ "P0",
      state == "07" & election_year == 1994L ~ "P0",
      state == "05" & election_year == 2025L ~ "P1",
      state == "07" & election_year %in% c(1999L, 2004L, 2009L, 2014L, 2019L) ~ "P1",
      state == "12" & election_year %in% c(1993L, 1998L) ~ "P2",
      state == "05" & election_year == 1994L ~ "P2",
      TRUE ~ "defer"
    ),
    next_action = case_when(
      coverage_status == "municipality_available" ~ "none",
      state == "01" & election_year %in% c(1990L, 1994L) ~
        paste0(
          "run a dedicated cell-crop OCR and manual verification wave; the closed ",
          "historical AGS universe resolves all rows but multi-pass OCR leaves ",
          "hundreds of numeric identities unresolved"
        ),
      state == "13" & election_year == 2011L ~
        "retain exact municipality rows and separate postal pools; do not allocate or impute",
      state == "07" & election_year == 1994L ~
        paste0(
          "request official Band 358 exact county/city totals and municipality ",
          "Kreistag contributions; accept CSV/XLSX or scans"
        ),
      state == "07" & election_year %in% c(1999L, 2004L, 2009L, 2014L, 2019L) ~
        paste0(
          "request municipality contributions to Kreistag totals from the ",
          "statistical office; public reports contain only higher-level aggregates"
        ),
      state == "05" & election_year == 2025L ~
        paste0(
          "request the 38 missing municipality rows for Kleve, Viersen, and ",
          "Wesel from IT.NRW; 326 rows in the other 27 counties are recoverable"
        ),
      state == "05" & election_year == 1994L ~
        "retain exact county totals and request municipality Kreistag contributions after higher-priority gaps",
      state == "12" & election_year %in% c(1993L, 1998L) ~
        paste0(
          "continue pagewise OCR with image verification and county reconciliation; ",
          "the four-row urban/rural pilot passed but unattended import is unsafe"
        ),
      state == "08" ~
        "retain exact county totals; municipality contributions are not centrally published",
      state == "09" & election_year == 2026L ~
        paste0(
          "retain exact county totals; recheck the 2026 regional report only if it ",
          "explicitly contains municipality contributions to Kreistag results"
        ),
      state == "09" ~
        "retain exact county totals; municipality contributions are not centrally published",
      state == "14" & election_year == 1994L ~
        "retain exact county totals and defer requests for municipality detail",
      TRUE ~ recommended_action
    ),
    recommended_action = next_action,
    priority = factor(priority, levels = c("P0", "P1", "P2", "defer", "none"))
  ) |>
  arrange(state, election_year)

not_applicable <- tribble(
  ~state, ~state_name,
  "02", "Hamburg",
  "04", "Bremen",
  "11", "Berlin"
) |>
  mutate(
    election_year = NA_integer_,
    event_scope = NA_character_,
    n_municipality_records = 0L,
    n_city_council_records = 0L,
    n_county_records = 0L,
    n_limited_records = 0L,
    observed_result_levels = NA_character_,
    observed_contest_types = NA_character_,
    observed_source_notes = NA_character_,
    raw_files = NA_character_,
    raw_hydration_status = NA_character_,
    source_note = "No county tier; city-state elections are outside scope",
    n_municipalities_2021 = NA_integer_,
    n_counties_current = NA_integer_,
    coverage_status = "not_applicable",
    gap_type = "not_applicable",
    acquisition_feasibility = "not_applicable",
    priority = factor("none", levels = c("P0", "P1", "P2", "defer", "none")),
    next_action = "none",
    recommended_action = "none",
    municipality_record_ratio = NA_real_,
    county_record_ratio = NA_real_
  )

coverage <- bind_rows(coverage, not_applicable) |>
  mutate(
    coverage_status = factor(
      coverage_status,
      levels = c(
        "municipality_available", "municipality_partial", "county_only", "raw_only",
        "missing_source", "not_applicable"
      )
    )
  )

stopifnot(
  sum(!is.na(coverage$election_year)) == nrow(county_election_expected_events()),
  !anyDuplicated(coverage |> filter(!is.na(election_year)) |>
    select(state, election_year)),
  all(as.character(coverage$coverage_status) %in% levels(coverage$coverage_status)),
  !anyNA(coverage$gap_type),
  !anyNA(coverage$acquisition_feasibility),
  !anyNA(coverage$priority),
  !anyNA(coverage$next_action),
  all(as.character(coverage$priority) %in% c("P0", "P1", "P2", "defer", "none"))
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(coverage, output_path, na = "")
dir.create(dirname(additions_path), recursive = TRUE, showWarnings = FALSE)
write_csv(
  coverage |>
    filter(
      coverage_status %in% c(
        "municipality_partial", "county_only", "raw_only", "missing_source"
      )
    ) |>
    arrange(
      priority,
      state,
      election_year
    ),
  additions_path,
  na = ""
)

cat("Wrote", output_path, "\n")
cat("Wrote", additions_path, "\n")
print(coverage |> count(coverage_status, .drop = FALSE))
