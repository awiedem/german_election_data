county_election_expected_events <- function() {
  tibble::tribble(
    ~state, ~state_name, ~election_year, ~event_scope,
    "01", "Schleswig-Holstein", 1990L, "statewide",
    "01", "Schleswig-Holstein", 1994L, "statewide",
    "01", "Schleswig-Holstein", 1998L, "statewide",
    "01", "Schleswig-Holstein", 2003L, "statewide",
    "01", "Schleswig-Holstein", 2008L, "statewide",
    "01", "Schleswig-Holstein", 2013L, "statewide",
    "01", "Schleswig-Holstein", 2018L, "statewide",
    "01", "Schleswig-Holstein", 2023L, "statewide",
    "03", "Lower Saxony", 1991L, "statewide",
    "03", "Lower Saxony", 1996L, "statewide",
    "03", "Lower Saxony", 2001L, "statewide",
    "03", "Lower Saxony", 2006L, "statewide",
    "03", "Lower Saxony", 2011L, "statewide",
    "03", "Lower Saxony", 2016L, "statewide",
    "03", "Lower Saxony", 2021L, "statewide",
    "05", "North Rhine-Westphalia", 1994L, "statewide",
    "05", "North Rhine-Westphalia", 1999L, "statewide",
    "05", "North Rhine-Westphalia", 2004L, "statewide",
    "05", "North Rhine-Westphalia", 2009L, "statewide",
    "05", "North Rhine-Westphalia", 2014L, "statewide",
    "05", "North Rhine-Westphalia", 2020L, "statewide",
    "05", "North Rhine-Westphalia", 2025L, "statewide",
    "06", "Hesse", 1993L, "statewide",
    "06", "Hesse", 1997L, "statewide",
    "06", "Hesse", 2001L, "statewide",
    "06", "Hesse", 2006L, "statewide",
    "06", "Hesse", 2011L, "statewide",
    "06", "Hesse", 2016L, "statewide",
    "06", "Hesse", 2021L, "statewide",
    "06", "Hesse", 2026L, "statewide",
    "07", "Rhineland-Palatinate", 1994L, "statewide",
    "07", "Rhineland-Palatinate", 1999L, "statewide",
    "07", "Rhineland-Palatinate", 2004L, "statewide",
    "07", "Rhineland-Palatinate", 2009L, "statewide",
    "07", "Rhineland-Palatinate", 2014L, "statewide",
    "07", "Rhineland-Palatinate", 2019L, "statewide",
    "07", "Rhineland-Palatinate", 2024L, "statewide",
    "08", "Baden-Württemberg", 1994L, "statewide",
    "08", "Baden-Württemberg", 1999L, "statewide",
    "08", "Baden-Württemberg", 2004L, "statewide",
    "08", "Baden-Württemberg", 2009L, "statewide",
    "08", "Baden-Württemberg", 2014L, "statewide",
    "08", "Baden-Württemberg", 2019L, "statewide",
    "08", "Baden-Württemberg", 2024L, "statewide",
    "09", "Bavaria", 1990L, "statewide",
    "09", "Bavaria", 1996L, "statewide",
    "09", "Bavaria", 2002L, "statewide",
    "09", "Bavaria", 2008L, "statewide",
    "09", "Bavaria", 2014L, "statewide",
    "09", "Bavaria", 2020L, "statewide",
    "09", "Bavaria", 2026L, "statewide",
    "10", "Saarland", 1994L, "statewide",
    "10", "Saarland", 1999L, "statewide",
    "10", "Saarland", 2004L, "statewide",
    "10", "Saarland", 2009L, "statewide",
    "10", "Saarland", 2014L, "statewide",
    "10", "Saarland", 2019L, "statewide",
    "10", "Saarland", 2024L, "statewide",
    "12", "Brandenburg", 1993L, "statewide",
    "12", "Brandenburg", 1998L, "statewide",
    "12", "Brandenburg", 2003L, "statewide",
    "12", "Brandenburg", 2008L, "statewide",
    "12", "Brandenburg", 2014L, "statewide",
    "12", "Brandenburg", 2019L, "statewide",
    "12", "Brandenburg", 2024L, "statewide",
    "13", "Mecklenburg-Vorpommern", 1990L, "statewide",
    "13", "Mecklenburg-Vorpommern", 1994L, "statewide",
    "13", "Mecklenburg-Vorpommern", 1999L, "statewide",
    "13", "Mecklenburg-Vorpommern", 2004L, "statewide",
    "13", "Mecklenburg-Vorpommern", 2009L, "statewide",
    "13", "Mecklenburg-Vorpommern", 2011L, "split_reform",
    "13", "Mecklenburg-Vorpommern", 2014L, "statewide",
    "13", "Mecklenburg-Vorpommern", 2019L, "statewide",
    "13", "Mecklenburg-Vorpommern", 2024L, "statewide",
    "14", "Saxony", 1994L, "split_reform",
    "14", "Saxony", 1995L, "split_reform",
    "14", "Saxony", 1999L, "statewide",
    "14", "Saxony", 2004L, "statewide",
    "14", "Saxony", 2008L, "statewide",
    "14", "Saxony", 2014L, "statewide",
    "14", "Saxony", 2019L, "statewide",
    "14", "Saxony", 2024L, "statewide",
    "15", "Saxony-Anhalt", 1994L, "statewide",
    "15", "Saxony-Anhalt", 1999L, "statewide",
    "15", "Saxony-Anhalt", 2004L, "statewide",
    "15", "Saxony-Anhalt", 2007L, "split_reform",
    "15", "Saxony-Anhalt", 2009L, "split_reform",
    "15", "Saxony-Anhalt", 2014L, "statewide",
    "15", "Saxony-Anhalt", 2019L, "statewide",
    "15", "Saxony-Anhalt", 2024L, "statewide",
    "16", "Thuringia", 1990L, "statewide",
    "16", "Thuringia", 1994L, "statewide",
    "16", "Thuringia", 1999L, "statewide",
    "16", "Thuringia", 2004L, "statewide",
    "16", "Thuringia", 2009L, "statewide",
    "16", "Thuringia", 2014L, "statewide",
    "16", "Thuringia", 2019L, "statewide",
    "16", "Thuringia", 2021L, "special",
    "16", "Thuringia", 2024L, "statewide"
  )
}

county_election_is_lfs_pointer <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    return(FALSE)
  }
  identical(
    readLines(path, n = 1L, warn = FALSE),
    "version https://git-lfs.github.com/spec/v1"
  )
}

assert_county_election_sources <- function(paths, label = "county-election sources") {
  absent <- paths[!file.exists(paths)]
  pointers <- paths[file.exists(paths) &
    vapply(paths[file.exists(paths)], county_election_is_lfs_pointer, logical(1))]

  problems <- c(
    if (length(absent)) paste0("missing: ", paste(absent, collapse = ", ")),
    if (length(pointers)) {
      paste0("Git LFS pointers: ", paste(pointers, collapse = ", "))
    }
  )
  if (length(problems)) {
    stop(label, " unavailable (", paste(problems, collapse = "; "), ").", call. = FALSE)
  }
  invisible(paths)
}

county_election_raw_file_inventory <- function(
    raw_root = "data/county_elections/raw/Kreistagswahlen") {
  if (!dir.exists(raw_root)) {
    stop("County-election raw directory does not exist: ", raw_root, call. = FALSE)
  }

  state_paths <- c(
    "01" = "Schleswig-Holstein",
    "03" = "Niedersachsen",
    "05" = "Nordrhein-Wetfalen",
    "06" = "Hessen",
    "07" = "Rheinland-Pfalz",
    "08" = "Baden-Württemberg",
    "09" = "Bayern",
    "10" = "Saarland",
    "12" = "Brandenburg",
    "13" = "Mecklenburg-Vorpommern",
    "14" = "Sachsen",
    "15" = "Sachsen-Anhalt",
    "16" = "Thüringen"
  )
  files <- list.files(raw_root, recursive = TRUE, full.names = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) {
    return(tibble::tibble(
      state = character(), election_year = integer(), raw_files = character(),
      raw_hydration_status = character()
    ))
  }

  relative <- substring(files, nchar(raw_root) + 2L)
  state <- vapply(relative, function(path) {
    hit <- names(state_paths)[startsWith(path, paste0(state_paths, "/"))]
    if (length(hit)) hit[[1L]] else NA_character_
  }, character(1))
  years <- stringr::str_extract_all(basename(files), "(?:19|20)[0-9]{2}")
  expanded <- lapply(seq_along(files), function(index) {
    if (is.na(state[[index]]) || !length(years[[index]])) {
      return(NULL)
    }
    tibble::tibble(
      state = state[[index]],
      election_year = as.integer(unique(years[[index]])),
      raw_file = relative[[index]],
      is_lfs_pointer = county_election_is_lfs_pointer(files[[index]])
    )
  }) |>
    dplyr::bind_rows()

  if (!nrow(expanded)) {
    return(tibble::tibble(
      state = character(), election_year = integer(), raw_files = character(),
      raw_hydration_status = character()
    ))
  }

  expanded |>
    dplyr::group_by(.data$state, .data$election_year) |>
    dplyr::summarise(
      raw_files = paste(sort(unique(.data$raw_file)), collapse = " | "),
      raw_hydration_status = dplyr::case_when(
        all(.data$is_lfs_pointer) ~ "lfs_pointer",
        any(.data$is_lfs_pointer) ~ "mixed",
        TRUE ~ "available"
      ),
      .groups = "drop"
    )
}

add_county_election_metadata <- function(
    data,
    county_type_path = "data/county_elections/final/county_council_seats.rds") {
  stopifnot(all(c("ags", "county", "state", "election_year") %in% names(data)))

  out <- dplyr::as_tibble(data)
  for (column in c("result_level", "contest_type", "event_scope")) {
    if (!column %in% names(out)) {
      out[[column]] <- NA_character_
    }
  }
  if (!"source_limitation" %in% names(out)) {
    out$source_limitation <- FALSE
  }
  if (!"source_note" %in% names(out)) {
    out$source_note <- NA_character_
  }

  out <- out |>
    dplyr::mutate(
      result_level = dplyr::coalesce(
        as.character(.data$result_level),
        dplyr::if_else(.data$state %in% c("08", "09"), "county", "municipality")
      ),
      event_scope = dplyr::case_when(
        !is.na(.data$event_scope) ~ as.character(.data$event_scope),
        .data$state == "13" & .data$election_year == 2011L ~ "split_reform",
        .data$state == "14" & .data$election_year %in% c(1994L, 1995L) ~ "split_reform",
        .data$state == "15" & .data$election_year %in% c(2007L, 2009L) ~ "split_reform",
        .data$state == "16" & .data$election_year == 2021L ~ "special",
        TRUE ~ "statewide"
      ),
      source_limitation = dplyr::coalesce(
        as.logical(.data$source_limitation),
        FALSE
      )
    )

  if (file.exists(county_type_path) &&
      !county_election_is_lfs_pointer(county_type_path)) {
    county_types <- readRDS(county_type_path) |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        county = as.character(.data$county),
        inferred_contest_type = dplyr::case_when(
          .data$county == "05334" ~ "other_county_equivalent",
          .data$county_type == "kreisfreie Stadt" ~ "kreisfreie_city_council",
          TRUE ~ "kreistag"
        )
      ) |>
      dplyr::distinct(.data$county, .keep_all = TRUE)
    out <- dplyr::left_join(out, county_types, by = "county")
  } else {
    out$inferred_contest_type <- NA_character_
  }

  out |>
    dplyr::mutate(
      contest_type = dplyr::coalesce(
        as.character(.data$contest_type),
        .data$inferred_contest_type,
        "kreistag"
      )
    ) |>
    dplyr::select(-"inferred_contest_type")
}

validate_county_election_parser_output <- function(data, parser_name) {
  required <- c(
    "ags", "ags_name", "county", "state", "election_year",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes", "turnout",
    "result_level", "contest_type", "event_scope"
  )
  absent <- setdiff(required, names(data))
  if (length(absent)) {
    stop(parser_name, " omitted required columns: ", paste(absent, collapse = ", "),
         call. = FALSE)
  }
  if (!nrow(data)) {
    stop(parser_name, " returned zero rows.", call. = FALSE)
  }
  duplicated_keys <- data |>
    dplyr::count(.data$ags, .data$election_year) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicated_keys)) {
    stop(parser_name, " returned duplicate AGS x year keys.", call. = FALSE)
  }
  if (any(!data$result_level %in% c("municipality", "county"))) {
    stop(parser_name, " returned an invalid result_level.", call. = FALSE)
  }
  invisible(data)
}
