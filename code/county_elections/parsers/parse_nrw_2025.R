# Parser for the final 2025 NRW Kreis and kreisfreie-Stadt election pages.
#
# The source pages report one result per county or county-equivalent unit. Party
# columns in the project-wide unharmonized schema are vote shares; the raw party
# counts are used here to calculate those shares and to validate each page.

.nrw2025_require_packages <- function() {
  required <- c("dplyr", "readr", "rvest", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "NRW 2025 parser requires missing package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.nrw2025_polling_district_columns <- function() {
  c(
    "KRS", "GKZ_Gemeinde", "Gemeindenname", "Nr", "Briefwahl",
    "A1", "A2", "A3", "A", "B", "B1", "C", "D",
    sprintf("D%02d", 1:34)
  )
}

.nrw2025_polling_district_party_labels <- function() {
  c(
    "CDU", "SPD", "GRÜNE", "FDP", "AfD", "Die Linke", "Die PARTEI",
    "Volt", "PIRATEN", "ÖDP", "Tierschutzpartei", "T I E R S C H U T Z",
    "FAMILIE", "AUFBRUCH C", "BIG", "Volksabstimmung", "DKP", "ZENTRUM",
    "BSW", "DAVA", "dieBasis", "Die LIEBE", "FREIE WÄHLER", "LD", "LfK",
    "PdH", "Team Todenhöfer", "WerteUnion", "WGR 1", "WGR 2", "WGR 3",
    "WGR 4", "WGR 5", "Einzelbewerber/-innen"
  )
}

.nrw2025_read_polling_districts <- function(source_file) {
  if (!file.exists(source_file)) {
    stop("NRW 2025 polling-district source is missing: ", source_file,
         call. = FALSE)
  }

  title <- readLines(source_file, n = 1L, encoding = "UTF-8", warn = FALSE)
  title <- sub("^\ufeff", "", title)
  if (length(title) != 1L ||
      !grepl("Endgültige Ergebnisse", title, fixed = TRUE) ||
      !grepl("14. September 2025", title, fixed = TRUE)) {
    stop("NRW 2025 polling-district source has an unexpected title row",
         call. = FALSE)
  }

  code_row <- suppressMessages(readr::read_delim(
    source_file,
    delim = ";",
    skip = 2L,
    n_max = 1L,
    col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    trim_ws = TRUE,
    show_col_types = FALSE
  ))
  expected_columns <- .nrw2025_polling_district_columns()
  observed_codes <- as.character(code_row[1, seq_along(expected_columns)])
  if (!identical(observed_codes, expected_columns)) {
    stop("NRW 2025 polling-district source has unexpected field codes",
         call. = FALSE)
  }
  if (ncol(code_row) > length(expected_columns) &&
      any(!is.na(code_row[1, -(seq_along(expected_columns))]))) {
    stop("NRW 2025 polling-district source has non-empty trailing fields",
         call. = FALSE)
  }

  raw <- suppressMessages(readr::read_delim(
    source_file,
    delim = ";",
    skip = 3L,
    col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    trim_ws = TRUE,
    show_col_types = FALSE
  ))
  if (nrow(raw) != 17166L || ncol(raw) < length(expected_columns)) {
    stop(
      "Expected 17,166 NRW 2025 polling-district rows and at least 47 fields; ",
      "found ", nrow(raw), " rows and ", ncol(raw), " fields",
      call. = FALSE
    )
  }
  if (ncol(raw) > length(expected_columns) &&
      any(!is.na(raw[, -(seq_along(expected_columns))]))) {
    stop("NRW 2025 polling-district data has non-empty trailing fields",
         call. = FALSE)
  }

  raw <- raw[, seq_along(expected_columns)]
  names(raw) <- expected_columns
  numeric_columns <- c("A1", "A2", "A3", "A", "B", "B1", "C", "D",
                       sprintf("D%02d", 1:34))
  raw[numeric_columns] <- lapply(raw[numeric_columns], .nrw2025_number)

  raw$KRS <- sprintf("%03d", as.integer(raw$KRS))
  raw$GKZ_Gemeinde <- sprintf("%06d", as.integer(raw$GKZ_Gemeinde))
  if (anyNA(raw[c("KRS", "GKZ_Gemeinde", "Gemeindenname", "Briefwahl")])) {
    stop("NRW 2025 polling-district source has missing geographic fields",
         call. = FALSE)
  }
  if (any(!raw$Briefwahl %in% c("Urne", "Brief"))) {
    stop("NRW 2025 polling-district source has an unexpected vote mode",
         call. = FALSE)
  }
  if (any(substr(raw$GKZ_Gemeinde, 1L, 3L) != raw$KRS)) {
    stop("NRW 2025 municipality and county identifiers are inconsistent",
         call. = FALSE)
  }
  if (anyNA(raw[c("A", "B", "C", "D")])) {
    stop("NRW 2025 polling-district source has missing ballot totals",
         call. = FALSE)
  }
  if (any(raw$C + raw$D != raw$B)) {
    stop("NRW 2025 polling-district ballot identity failed", call. = FALSE)
  }
  if (any(rowSums(raw[sprintf("D%02d", 1:34)], na.rm = TRUE) != raw$D)) {
    stop("NRW 2025 polling-district party counts do not sum to valid votes",
         call. = FALSE)
  }

  tibble::as_tibble(raw)
}

.nrw2025_municipality_counts <- function(source_file) {
  raw <- .nrw2025_read_polling_districts(source_file)
  party_codes <- sprintf("D%02d", 1:34)
  party_labels <- .nrw2025_polling_district_party_labels()
  party_names <- vapply(seq_along(party_labels), function(index) {
    label <- party_labels[[index]]
    if (grepl("^WGR[[:space:]]*[0-9]+$", label)) {
      return("waehlergruppen")
    }
    .nrw2025_normalise_party(label)
  }, character(1))

  party_counts <- lapply(unique(party_names), function(party) {
    codes <- party_codes[party_names == party]
    rowSums(raw[codes], na.rm = TRUE)
  })
  names(party_counts) <- unique(party_names)
  party_counts <- tibble::as_tibble(party_counts)
  if (any(rowSums(party_counts) != raw$D)) {
    stop("NRW 2025 normalized party counts do not sum to valid votes",
         call. = FALSE)
  }

  polling_districts <- dplyr::bind_cols(
    raw[c("KRS", "GKZ_Gemeinde", "Gemeindenname", "A", "B", "C", "D")],
    party_counts
  ) |>
    dplyr::filter(substr(.data$GKZ_Gemeinde, 4L, 6L) != "000") |>
    dplyr::mutate(
      ags = paste0("05", .data$GKZ_Gemeinde),
      county = paste0("05", .data$KRS)
    )

  numeric_columns <- c("A", "B", "C", "D", unique(party_names))
  result <- polling_districts |>
    dplyr::group_by(.data$ags, .data$county) |>
    dplyr::summarise(
      ags_name = dplyr::first(.data$Gemeindenname),
      dplyr::across(dplyr::all_of(numeric_columns), ~sum(.x, na.rm = TRUE)),
      source_rows = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(result) != 374L || dplyr::n_distinct(result$county) != 31L) {
    stop(
      "Expected 374 municipality contributions in 31 NRW county contests; ",
      "found ", nrow(result), " municipalities in ",
      dplyr::n_distinct(result$county), " contests",
      call. = FALSE
    )
  }
  if (anyDuplicated(result$ags)) {
    stop("NRW 2025 municipality aggregation produced duplicate AGS",
         call. = FALSE)
  }
  if (any(result$C + result$D != result$B)) {
    stop("NRW 2025 municipality ballot identity failed", call. = FALSE)
  }
  if (any(result$B > result$A)) {
    stop("NRW 2025 municipality voters exceed eligible voters",
         call. = FALSE)
  }
  if (any(rowSums(result[unique(party_names)]) != result$D)) {
    stop("NRW 2025 municipality party counts do not sum to valid votes",
         call. = FALSE)
  }

  result
}

.nrw2025_html_dir <- function(raw_dir) {
  candidates <- unique(c(
    raw_dir,
    file.path(raw_dir, "2025_html"),
    file.path(raw_dir, "Nordrhein-Wetfalen", "2025_html")
  ))
  hits <- candidates[dir.exists(candidates)]
  hits <- hits[vapply(
    hits,
    function(path) {
      length(list.files(
        path,
        pattern = "^a[0-9]{6}kw2500[.]shtml$",
        full.names = FALSE
      )) > 0L
    },
    logical(1)
  )]

  if (length(hits) != 1L) {
    stop(
      "Could not resolve exactly one NRW 2025 HTML directory below raw_dir: ",
      raw_dir,
      call. = FALSE
    )
  }
  hits[[1]]
}

.nrw2025_number <- function(x) {
  x <- trimws(x)
  x[x %in% c("", "-", "\u2013", "\u2014", "X")] <- NA_character_
  x <- gsub("[[:space:]\u00a0\u202f]", "", x)
  suppressWarnings(as.numeric(x))
}

.nrw2025_percent <- function(x) {
  x <- trimws(x)
  x[x %in% c("", "-", "\u2013", "\u2014", "X")] <- NA_character_
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

.nrw2025_normalise_party <- function(x) {
  x_clean <- tolower(trimws(x))

  if (grepl("^w\u00e4hlergruppe[[:space:]]+[0-9]+$", x_clean)) {
    return("waehlergruppen")
  }
  if (grepl("^einzelbewerber", x_clean)) {
    return("einzelbewerber")
  }

  if (exists("normalise_party_cty", envir = .GlobalEnv, inherits = FALSE)) {
    return(get("normalise_party_cty", envir = .GlobalEnv)(x_clean))
  }

  mapping <- c(
    "gr\u00fcne" = "gruene",
    "die linke" = "linke_pds",
    "die partei" = "die_partei",
    "tierschutzpartei" = "tierschutz",
    "freie w\u00e4hler" = "freie_waehler",
    "diebasis" = "die_basis",
    "\u00f6dp" = "oedp",
    "sonstige" = "other"
  )
  if (x_clean %in% names(mapping)) {
    return(unname(mapping[[x_clean]]))
  }

  ascii <- iconv(x_clean, from = "UTF-8", to = "ASCII//TRANSLIT")
  ascii <- gsub("[^a-z0-9]+", "_", ascii)
  gsub("^_+|_+$", "", ascii)
}

.nrw2025_row_value <- function(rows, label_pattern, value_name, file) {
  hit <- which(grepl(label_pattern, rows$label, ignore.case = TRUE, perl = TRUE))
  if (length(hit) != 1L) {
    stop(
      basename(file), ": expected exactly one row for ", value_name,
      ", found ", length(hit),
      call. = FALSE
    )
  }
  value <- rows$count[[hit]]
  if (is.na(value)) {
    stop(basename(file), ": missing ", value_name, call. = FALSE)
  }
  value
}

.nrw2025_parse_page <- function(file) {
  document <- rvest::read_html(file)
  table <- rvest::html_element(document, "#mainErgTable")
  if (inherits(table, "xml_missing")) {
    stop(basename(file), ": missing #mainErgTable", call. = FALSE)
  }

  result_name_node <- rvest::html_element(document, "#dieserWahlkreis")
  if (inherits(result_name_node, "xml_missing")) {
    stop(basename(file), ": missing #dieserWahlkreis", call. = FALSE)
  }
  result_name <- rvest::html_text2(result_name_node)

  row_nodes <- rvest::html_elements(
    rvest::html_element(table, "tbody"),
    "tr"
  )
  parsed_rows <- lapply(row_nodes, function(row) {
    cells <- rvest::html_text2(rvest::html_elements(row, "td, th"))
    if (length(cells) < 3L) {
      return(NULL)
    }
    data.frame(
      label = cells[[1]],
      count = .nrw2025_number(cells[[2]]),
      official_percent = .nrw2025_percent(cells[[3]]),
      stringsAsFactors = FALSE
    )
  })
  parsed_rows <- parsed_rows[!vapply(parsed_rows, is.null, logical(1))]
  rows <- dplyr::bind_rows(parsed_rows)
  if (nrow(rows) < 9L) {
    stop(basename(file), ": malformed #mainErgTable body", call. = FALSE)
  }

  eligible_voters <- .nrw2025_row_value(
    rows, "^Wahlberechtigte insgesamt$", "eligible voters", file
  )
  number_voters <- .nrw2025_row_value(
    rows, "^W\u00e4hler/-innen$", "voters", file
  )
  invalid_votes <- .nrw2025_row_value(
    rows, "^Ung\u00fcltige Stimmen$", "invalid votes", file
  )
  valid_row <- which(grepl(
    "^G\u00fcltige Stimmen", rows$label, ignore.case = TRUE, perl = TRUE
  ))
  if (length(valid_row) != 1L || is.na(rows$count[[valid_row]])) {
    stop(basename(file), ": missing or ambiguous valid-vote row", call. = FALSE)
  }
  valid_votes <- rows$count[[valid_row]]

  party_rows <- rows[seq.int(valid_row + 1L, nrow(rows)), , drop = FALSE]
  if (nrow(party_rows) == 0L) {
    stop(basename(file), ": no party rows after valid-vote row", call. = FALSE)
  }
  party_rows$party <- vapply(
    party_rows$label, .nrw2025_normalise_party, character(1)
  )
  if (any(is.na(party_rows$party) | party_rows$party == "")) {
    stop(basename(file), ": empty normalized party name", call. = FALSE)
  }

  party_split <- split(party_rows, party_rows$party)
  party_counts <- vapply(party_split, function(x) {
    if (all(is.na(x$count))) NA_real_ else sum(x$count, na.rm = TRUE)
  }, numeric(1))
  counted_votes <- sum(party_counts, na.rm = TRUE)
  if (!isTRUE(all.equal(counted_votes, valid_votes, tolerance = 0))) {
    stop(
      basename(file), ": party counts sum to ", counted_votes,
      " but valid votes equal ", valid_votes,
      call. = FALSE
    )
  }
  if (invalid_votes + valid_votes != number_voters) {
    stop(
      basename(file), ": invalid plus valid votes do not equal voters",
      call. = FALSE
    )
  }

  present <- !is.na(party_rows$count) & !is.na(party_rows$official_percent)
  derived_percent <- 100 * party_rows$count[present] / valid_votes
  if (any(abs(derived_percent - party_rows$official_percent[present]) > 0.051)) {
    stop(
      basename(file), ": a reported party percentage is inconsistent with counts",
      call. = FALSE
    )
  }

  code_match <- regmatches(
    basename(file),
    regexec("^a([0-9]{6})kw2500[.]shtml$", basename(file))
  )[[1]]
  if (length(code_match) != 2L) {
    stop(basename(file), ": filename does not contain a valid unit code", call. = FALSE)
  }
  county <- paste0("05", substr(code_match[[2]], 1L, 3L))

  contest_type <- if (grepl("^Krfr[.] Stadt ", result_name)) {
    "kreisfreie_city_council"
  } else if (identical(result_name, "St\u00e4dteregion Aachen")) {
    "other_county_equivalent"
  } else {
    "kreistag"
  }

  base <- data.frame(
    ags = paste0(county, "000"),
    ags_name = result_name,
    eligible_voters = eligible_voters,
    number_voters = number_voters,
    invalid_votes = invalid_votes,
    valid_votes = valid_votes,
    turnout = number_voters / eligible_voters,
    county = county,
    state = "05",
    election_year = 2025L,
    result_level = "county",
    contest_type = contest_type,
    event_scope = "statewide",
    stringsAsFactors = FALSE
  )

  for (party in names(party_counts)) {
    base[[party]] <- party_counts[[party]] / valid_votes
  }
  tibble::as_tibble(base)
}

#' Parse final NRW 2025 county and county-equivalent election results
#'
#' @param raw_dir Either the NRW directory, its `2025_html` directory, or the
#'   project-wide `Kreistagswahlen` raw directory.
#' @return One row per county or county-equivalent unit in the unharmonized vote
#'   schema. Party columns are shares of valid votes.
parse_nrw_2025_county_elections <- function(raw_dir) {
  .nrw2025_require_packages()
  html_dir <- .nrw2025_html_dir(raw_dir)
  files <- sort(list.files(
    html_dir,
    pattern = "^a[0-9]{6}kw2500[.]shtml$",
    full.names = TRUE
  ))
  if (length(files) != 53L) {
    stop(
      "Expected exactly 53 NRW 2025 county-level pages, found ",
      length(files), " in ", html_dir,
      call. = FALSE
    )
  }

  result <- dplyr::bind_rows(lapply(files, .nrw2025_parse_page))
  if (nrow(result) != 53L) {
    stop("NRW 2025 parser did not return exactly 53 rows", call. = FALSE)
  }
  if (anyDuplicated(result[c("ags", "election_year")])) {
    stop("NRW 2025 parser produced duplicate AGS-year rows", call. = FALSE)
  }

  expected_contests <- c(
    kreistag = 30L,
    kreisfreie_city_council = 22L,
    other_county_equivalent = 1L
  )
  observed_contests <- table(factor(
    result$contest_type,
    levels = names(expected_contests)
  ))
  if (!identical(as.integer(observed_contests), unname(expected_contests))) {
    stop(
      "NRW 2025 contest classification is not 30 Kreise, 22 city councils, ",
      "and 1 other county-equivalent",
      call. = FALSE
    )
  }

  result
}

#' Parse final NRW 2025 municipality contributions to county contests
#'
#' @param source_file Official IT.NRW semicolon-delimited polling-district file.
#' @return One row per municipality participating in a Kreis or Städteregion
#'   contest. The 22 independent-city council rows are intentionally excluded
#'   because the county parser already represents those exact city-level events.
parse_nrw_2025_municipality_elections <- function(source_file) {
  .nrw2025_require_packages()
  counts <- .nrw2025_municipality_counts(source_file)
  metadata <- c(
    "ags", "county", "ags_name", "A", "B", "C", "D", "source_rows"
  )
  party_columns <- setdiff(names(counts), metadata)

  result <- counts |>
    dplyr::transmute(
      ags = .data$ags,
      ags_name = .data$ags_name,
      eligible_voters = .data$A,
      number_voters = .data$B,
      invalid_votes = .data$C,
      valid_votes = .data$D,
      turnout = .data$B / .data$A,
      county = .data$county,
      state = "05",
      election_year = 2025L,
      result_level = "municipality",
      contest_type = dplyr::if_else(
        .data$county == "05334",
        "other_county_equivalent",
        "kreistag"
      ),
      event_scope = "statewide",
      source_limitation = FALSE,
      source_note = paste(
        "Official IT.NRW polling-district extract supplied by the elections",
        "unit; postal-vote rows carry municipality identifiers."
      ),
      dplyr::across(
        dplyr::all_of(party_columns),
        ~dplyr::if_else(.data$D > 0, .x / .data$D, NA_real_)
      )
    )

  if (anyDuplicated(result[c("ags", "election_year")])) {
    stop("NRW 2025 municipality parser produced duplicate AGS-year rows",
         call. = FALSE)
  }
  if (any(result$turnout < 0 | result$turnout > 1)) {
    stop("NRW 2025 municipality parser produced invalid turnout",
         call. = FALSE)
  }
  if (any(abs(rowSums(result[party_columns]) - 1) > 1e-12)) {
    stop("NRW 2025 municipality party shares do not sum to one",
         call. = FALSE)
  }

  tibble::as_tibble(result)
}
