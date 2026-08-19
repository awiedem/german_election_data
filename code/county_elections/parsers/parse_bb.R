# Brandenburg county-council election parser
#
# Public interface:
#   parse_bb_county_elections(raw_dir)
#
# `raw_dir` is the Brandenburg source directory, not the parent
# `Kreistagswahlen` directory. The parser deliberately stops on missing files,
# Git LFS pointer stubs, unexpected headers, or duplicate municipality-years.

.bb_clean_header <- function(x) {
  x <- gsub("-\\s*\r?\n\\s*", "", x)
  x <- gsub("\r?\n", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.bb_is_lfs_pointer <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  marker <- charToRaw("version https://git-lfs.github.com/spec/v1")
  prefix <- readBin(con, what = "raw", n = length(marker))
  length(prefix) == length(marker) && identical(prefix, marker)
}

.bb_require_workbook <- function(path) {
  if (!file.exists(path)) {
    stop("Required Brandenburg source is missing: ", path, call. = FALSE)
  }
  if (.bb_is_lfs_pointer(path)) {
    stop(
      "Brandenburg source is an unhydrated Git LFS pointer: ",
      path,
      call. = FALSE
    )
  }
  invisible(path)
}

.bb_normalise_party <- function(x) {
  if (exists("normalise_party_cty", mode = "function", inherits = TRUE)) {
    return(vapply(
      x,
      get("normalise_party_cty", mode = "function", inherits = TRUE),
      character(1)
    ))
  }

  key <- tolower(trimws(x))
  mapping <- c(
    "cdu" = "cdu",
    "cdu und andere" = "cdu",
    "spd" = "spd",
    "pds" = "linke_pds",
    "die linke" = "linke_pds",
    "fdp" = "fdp",
    "grüne/b90" = "gruene",
    "grüne/b 90" = "gruene",
    "grüne/b90 und andere" = "gruene",
    "grüne/b 90 und andere" = "gruene",
    "afd" = "afd",
    "npd" = "npd",
    "heimat" = "heimat",
    "bvb/50plus" = "bvb_fw",
    "bvb/freie wähler und andere" = "bvb_fw",
    "bvb / freie wähler und andere" = "bvb_fw",
    "bauern und andere" = "bauern",
    "weitere wählergruppen" = "weitere_wg",
    "weitere listenvereinigungen" = "weitere_lv",
    "weitere politische vereinigungen" = "weitere_pv",
    "einzelbewerber" = "einzelbewerber",
    "tierschutzpartei" = "tierschutz",
    "die partei" = "die_partei",
    "piraten" = "piraten",
    "bsw" = "bsw",
    "iii. weg" = "iii_weg",
    "volt" = "volt"
  )
  matched <- unname(mapping[key])
  missing <- is.na(matched)
  fallback <- iconv(key[missing], from = "UTF-8", to = "ASCII//TRANSLIT")
  fallback <- gsub("[^a-z0-9]+", "_", fallback)
  fallback <- gsub("^_|_$", "", fallback)
  matched[missing] <- fallback
  matched
}

.bb_locate_header <- function(headers, pattern, label, fixed = FALSE) {
  hit <- which(grepl(pattern, headers, ignore.case = FALSE, fixed = fixed))
  if (length(hit) == 0L) {
    stop("Could not locate Brandenburg column: ", label, call. = FALSE)
  }
  hit[[1L]]
}

.bb_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "x", ".")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

.bb_sum_preserve_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}

.bb_collapse_duplicate_parties <- function(values, party_names) {
  unique_names <- unique(party_names)
  out <- vector("list", length(unique_names))
  names(out) <- unique_names
  for (party in unique_names) {
    positions <- which(party_names == party)
    if (length(positions) == 1L) {
      out[[party]] <- values[[positions]]
    } else {
      block <- as.data.frame(values[positions], check.names = FALSE)
      out[[party]] <- apply(
        block,
        1L,
        .bb_sum_preserve_na
      )
    }
  }
  as.data.frame(out, check.names = FALSE)
}

.bb_party_columns <- function(headers, first_col, last_col = length(headers)) {
  positions <- integer()
  names_out <- character()
  for (position in seq.int(first_col, last_col)) {
    label <- headers[[position]]
    if (is.na(label) || !nzchar(label)) {
      next
    }
    if (grepl("in Prozent", label, ignore.case = TRUE) ||
        grepl("^Stimmen nach", label, ignore.case = TRUE)) {
      next
    }
    if (grepl("^EB\\b|Einzelbewer", label, ignore.case = TRUE)) {
      party <- "einzelbewerber"
    } else {
      party <- .bb_normalise_party(label)
    }
    if (!nzchar(party)) {
      stop("Empty normalized party name for Brandenburg header: ", label,
           call. = FALSE)
    }
    positions <- c(positions, position)
    names_out <- c(names_out, party)
  }
  if (length(positions) == 0L) {
    stop("No Brandenburg party columns found", call. = FALSE)
  }
  list(positions = positions, names = names_out)
}

.bb_add_shares_and_metadata <- function(df, year, source_limitation) {
  count_cols <- c(
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
  )
  party_cols <- setdiff(
    names(df),
    c("ags", "ags_name", count_cols)
  )

  df$turnout <- ifelse(
    df$eligible_voters > 0,
    df$number_voters / df$eligible_voters,
    NA_real_
  )
  for (party in party_cols) {
    df[[party]] <- ifelse(
      df$valid_votes > 0,
      df[[party]] / df$valid_votes,
      NA_real_
    )
  }

  df$county <- substr(df$ags, 1L, 5L)
  df$election_year <- as.integer(year)
  df$state <- "12"
  df$result_level <- "municipality"
  df$contest_type <- ifelse(
    df$county %in% paste0("12", sprintf("%03d", 51:54)),
    "kreisfreie_city_council",
    "kreistag"
  )
  df$event_scope <- "statewide"
  df$source_limitation <- source_limitation
  df$source_note <- ifelse(
    source_limitation,
    paste0(
      "County-level postal ballots cannot be assigned to rural ",
      "municipalities; rural municipality totals exclude those ballots."
    ),
    NA_character_
  )

  leading <- c(
    "ags", "ags_name", "county", "election_year", "state",
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes",
    "turnout", "result_level", "contest_type", "event_scope",
    "source_limitation", "source_note"
  )
  tibble::as_tibble(df[c(leading, setdiff(names(df), leading))])
}

.bb_parse_district_workbook <- function(path, year) {
  .bb_require_workbook(path)
  sheets <- readxl::excel_sheets(path)
  data_sheet <- if ("Ergebnis_1" %in% sheets) {
    "Ergebnis_1"
  } else if ("Ergebnis" %in% sheets) {
    "Ergebnis"
  } else {
    stop("No result sheet in Brandenburg workbook: ", path, call. = FALSE)
  }

  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet = data_sheet,
    col_names = FALSE,
    col_types = "text"
  ))
  headers <- .bb_clean_header(as.character(unlist(raw[1L, ], use.names = FALSE)))

  stimmart_col <- .bb_locate_header(headers, "^Stimmart$", "Stimmart")
  ags_col <- .bb_locate_header(headers, "^AGS$", "AGS")
  name_col <- .bb_locate_header(headers, "^Gemeindename$", "Gemeindename")
  eligible_col <- .bb_locate_header(
    headers, "^Wahlberechtigte insgesamt$", "Wahlberechtigte insgesamt"
  )
  voters_col <- .bb_locate_header(headers, "^Wähler$", "Wähler")
  invalid_col <- .bb_locate_header(
    headers, "^Ungültige Stimmzettel$", "Ungültige Stimmzettel"
  )
  valid_col <- .bb_locate_header(
    headers, "^Gültige Stimmen$", "Gültige Stimmen"
  )
  parties <- .bb_party_columns(headers, valid_col + 1L)

  keep <- as.character(raw[[stimmart_col]])[-1L] == "Kreistag"
  keep[is.na(keep)] <- FALSE
  extracted <- data.frame(
    ags = as.character(raw[[ags_col]])[-1L][keep],
    ags_name = as.character(raw[[name_col]])[-1L][keep],
    eligible_voters = .bb_numeric(raw[[eligible_col]][-1L][keep]),
    number_voters = .bb_numeric(raw[[voters_col]][-1L][keep]),
    invalid_votes = .bb_numeric(raw[[invalid_col]][-1L][keep]),
    valid_votes = .bb_numeric(raw[[valid_col]][-1L][keep]),
    check.names = FALSE
  )
  party_values <- lapply(
    parties$positions,
    function(position) .bb_numeric(raw[[position]][-1L][keep])
  )
  extracted <- cbind(
    extracted,
    .bb_collapse_duplicate_parties(party_values, parties$names)
  )

  if (nrow(extracted) == 0L) {
    stop("No Kreistag rows parsed for Brandenburg ", year, call. = FALSE)
  }
  standard_ags <- grepl("^12[0-9]{6}$", extracted$ags)
  amt_postal_ags <- grepl("^12[0-9]{6} [0-9]{2}$", extracted$ags)
  if (any(!standard_ags & !amt_postal_ags)) {
    stop("Unknown Brandenburg AGS format parsed for ", year, call. = FALSE)
  }

  # In 2003 and 2008, rural postal districts use artificial *900 AGS. In
  # 2019, some Amt-level postal districts use "<eight-digit key> <Amt>".
  # Neither form can be assigned to municipalities and neither is a real AGS.
  unassigned_postal <- grepl("900$", extracted$ags) | amt_postal_ags
  limited_years <- c(2003L, 2008L, 2019L)
  if (year %in% limited_years && !any(unassigned_postal)) {
    stop("Expected unassigned rural postal rows are absent for ", year,
         call. = FALSE)
  }
  if (!(year %in% limited_years) && any(unassigned_postal)) {
    stop("Unexpected unassigned Brandenburg postal rows for ", year,
         call. = FALSE)
  }
  extracted <- extracted[!unassigned_postal, , drop = FALSE]

  numeric_cols <- setdiff(names(extracted), c("ags", "ags_name"))
  ags_groups <- split(seq_len(nrow(extracted)), extracted$ags)
  aggregated <- lapply(ags_groups, function(rows) {
    names_available <- extracted$ags_name[rows]
    names_available <- names_available[
      !is.na(names_available) &
        nzchar(names_available) &
        !grepl("^Briefwahl$", names_available, ignore.case = TRUE)
    ]
    ags_name <- if (length(names_available)) {
      names(sort(table(names_available), decreasing = TRUE))[[1L]]
    } else {
      NA_character_
    }
    values <- vapply(
      extracted[rows, numeric_cols, drop = FALSE],
      .bb_sum_preserve_na,
      numeric(1)
    )
    data.frame(
      ags = extracted$ags[rows[[1L]]],
      ags_name = ags_name,
      as.list(values),
      check.names = FALSE
    )
  })
  aggregated <- do.call(rbind, aggregated)
  rownames(aggregated) <- NULL
  aggregated <- aggregated[aggregated$eligible_voters > 0, , drop = FALSE]

  limitation <- year %in% limited_years &
    !substr(aggregated$ags, 1L, 5L) %in% paste0("12", sprintf("%03d", 51:54))
  .bb_add_shares_and_metadata(aggregated, year, limitation)
}

.bb_parse_2024_aggregate <- function(path) {
  .bb_require_workbook(path)
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet = "Brandenburg_KW_A",
    col_names = FALSE,
    col_types = "text"
  ))
  headers <- .bb_clean_header(as.character(unlist(raw[1L, ], use.names = FALSE)))

  key_col <- .bb_locate_header(
    headers,
    "Gebietsschlüssel - Regionalschlüssel - Wahlkreisnummer",
    "Gebietsschlüssel",
    fixed = TRUE
  )
  type_col <- .bb_locate_header(headers, "^Gebiet$", "Gebiet")
  name_col <- .bb_locate_header(headers, "^Name des Gebietes$", "Gebietsname")
  eligible_col <- .bb_locate_header(
    headers, "^Wahlberechtigte insgesamt$", "Wahlberechtigte insgesamt"
  )
  voters_col <- .bb_locate_header(headers, "^Wählende$", "Wählende")
  invalid_col <- .bb_locate_header(
    headers, "^Ungültige Stimmzettel$", "Ungültige Stimmzettel"
  )
  valid_col <- .bb_locate_header(
    headers, "^Gültige Stimmen$", "Gültige Stimmen"
  )
  aggregate_marker <- .bb_locate_header(
    headers,
    "aggregierten Wahlvorschlägen",
    "aggregated party marker"
  )
  individual_marker <- .bb_locate_header(
    headers,
    "^Stimmen nach Wahlvorschlägen$",
    "individual party marker"
  )
  parties <- .bb_party_columns(
    headers,
    aggregate_marker + 1L,
    individual_marker - 1L
  )

  region_type <- as.character(raw[[type_col]])[-1L]
  keep <- region_type %in% c("amtsangehörige Gemeinde", "amtsfreie Gemeinde")
  key <- as.character(raw[[key_col]])[-1L][keep]
  if (any(!grepl("^SI12[0-9]{10}$", key))) {
    stop("Unexpected 2024 Brandenburg municipality key", call. = FALSE)
  }
  ags <- paste0(substr(key, 3L, 7L), substr(key, nchar(key) - 2L, nchar(key)))

  extracted <- data.frame(
    ags = ags,
    ags_name = as.character(raw[[name_col]])[-1L][keep],
    eligible_voters = .bb_numeric(raw[[eligible_col]][-1L][keep]),
    number_voters = .bb_numeric(raw[[voters_col]][-1L][keep]),
    invalid_votes = .bb_numeric(raw[[invalid_col]][-1L][keep]),
    valid_votes = .bb_numeric(raw[[valid_col]][-1L][keep]),
    check.names = FALSE
  )
  party_values <- lapply(
    parties$positions,
    function(position) .bb_numeric(raw[[position]][-1L][keep])
  )
  extracted <- cbind(
    extracted,
    .bb_collapse_duplicate_parties(party_values, parties$names)
  )
  if (anyDuplicated(extracted$ags)) {
    stop("Duplicate municipality AGS in 2024 Brandenburg aggregate",
         call. = FALSE)
  }
  if (any(extracted$eligible_voters <= 0 | is.na(extracted$eligible_voters))) {
    stop("Invalid 2024 Brandenburg municipality electorate", call. = FALSE)
  }
  .bb_add_shares_and_metadata(extracted, 2024L, FALSE)
}

parse_bb_county_elections <- function(raw_dir) {
  if (length(raw_dir) != 1L || is.na(raw_dir) || !dir.exists(raw_dir)) {
    stop("Brandenburg raw_dir does not exist: ", raw_dir, call. = FALSE)
  }

  files <- c(
    `2003` = "Brandenburg_2003_KTW.xlsx",
    `2008` = "Brandenburg_2008_KTW.xlsx",
    `2014` = "Brandenburg_2014_KTW.xlsx",
    `2019` = "Brandenburg_2019_KTW.xlsx"
  )
  parsed <- lapply(names(files), function(year) {
    .bb_parse_district_workbook(
      file.path(raw_dir, files[[year]]),
      as.integer(year)
    )
  })
  parsed[["2024"]] <- .bb_parse_2024_aggregate(
    file.path(raw_dir, "Brandenburg_2024_Kreise_Gemeinden.xlsx")
  )

  result <- dplyr::bind_rows(parsed)
  expected_years <- c(2003L, 2008L, 2014L, 2019L, 2024L)
  missing_years <- setdiff(expected_years, unique(result$election_year))
  if (length(missing_years)) {
    stop(
      "Brandenburg parser omitted expected years: ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(result[c("ags", "election_year")])) {
    stop("Duplicate Brandenburg AGS-year rows after parsing", call. = FALSE)
  }
  if (any(result$state != "12") ||
      any(result$result_level != "municipality") ||
      any(result$event_scope != "statewide")) {
    stop("Invalid Brandenburg parser metadata", call. = FALSE)
  }
  result
}
