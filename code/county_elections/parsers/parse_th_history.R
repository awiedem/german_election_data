# Historical Thuringia county-election results, 1990-1999.
#
# The 1990 workbook uses the valid historical municipality identifiers recorded
# by the statistical office at the election date. These predate the 1994 county
# reform. The source documentation also states that postal votes are included
# in municipality totals but cannot be separated into postal-voting districts.

.th_history_assert_source <- function(path) {
  if (!file.exists(path)) {
    stop("Missing Thuringia historical source: ", path, call. = FALSE)
  }
  if (is.na(file.info(path)$size) || file.info(path)$size == 0) {
    stop("Empty Thuringia historical source: ", path, call. = FALSE)
  }
  if (identical(
    readLines(path, n = 1L, warn = FALSE),
    "version https://git-lfs.github.com/spec/v1"
  )) {
    stop("Thuringia historical source is a Git LFS pointer: ", path,
         call. = FALSE)
  }
  invisible(path)
}

.th_history_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "x", "X")] <- NA_character_
  suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
}

.th_history_count <- function(x) {
  out <- .th_history_number(x)
  out[trimws(as.character(x)) == "-"] <- 0
  out
}

.th_history_party_name <- function(x) {
  key <- tolower(trimws(as.character(x)))
  direct <- c(
    "cdu" = "cdu",
    "spd" = "spd",
    "pds" = "linke_pds",
    "f.d.p." = "fdp",
    "fdp" = "fdp",
    "grüne" = "gruene",
    "sonstige" = "other",
    "bauern" = "bauern",
    "bü.90" = "buendnis_90",
    "grünel" = "gruene_liste"
  )
  out <- unname(direct[key])
  missing <- is.na(out)
  if (any(missing)) {
    generic <- iconv(key[missing], from = "", to = "ASCII//TRANSLIT")
    generic <- gsub("[^a-z0-9]+", "_", generic)
    generic <- gsub("^_+|_+$", "", generic)
    generic[generic == ""] <- "unnamed_party"
    out[missing] <- generic
  }
  out
}

.th_history_clean_html_name <- function(row) {
  # The official HTML export omits the closing tag for the name cell. rvest
  # consequently appends all following cells to the municipality name.
  appended <- paste0(ifelse(is.na(row[4:length(row)]), "", row[4:length(row)]),
                     collapse = "")
  value <- as.character(row[3])
  if (!nzchar(appended) || !endsWith(value, appended)) {
    stop("Malformed Thuringia HTML row: municipality name cannot be recovered.",
         call. = FALSE)
  }
  substr(value, 1L, nchar(value) - nchar(appended))
}

.th_history_finish <- function(data, total_party_votes, party_columns, year) {
  if (anyDuplicated(data[c("ags")])) {
    stop("Thuringia ", year, " source contains duplicate municipality IDs.",
         call. = FALSE)
  }
  if (any(is.na(data$ags) | !grepl("^16[0-9]{6}$", data$ags))) {
    stop("Thuringia ", year, " parser produced malformed historical AGS.",
         call. = FALSE)
  }
  if (any(is.na(data$eligible_voters) | is.na(data$number_voters) |
          is.na(data$valid_votes) | is.na(data$invalid_votes))) {
    stop("Thuringia ", year, " has missing core election counts.", call. = FALSE)
  }
  if (any(data$number_voters > data$eligible_voters) ||
      any(data$valid_votes + data$invalid_votes != data$number_voters)) {
    stop("Thuringia ", year, " has internally inconsistent ballot counts.",
         call. = FALSE)
  }

  observed_party_votes <- rowSums(data[party_columns], na.rm = TRUE)
  if (any(abs(observed_party_votes - total_party_votes) > 0.5)) {
    stop("Thuringia ", year, " party counts do not sum to total valid votes.",
         call. = FALSE)
  }

  for (column in party_columns) {
    data[[column]] <- ifelse(
      total_party_votes > 0,
      data[[column]] / total_party_votes,
      NA_real_
    )
  }

  data$election_year <- as.integer(year)
  data$state <- "16"
  data$turnout <- ifelse(
    data$eligible_voters > 0,
    data$number_voters / data$eligible_voters,
    NA_real_
  )
  data$result_level <- "municipality"
  data$contest_type <- ifelse(
    substr(data$ags, 6L, 8L) == "000",
    "kreisfreie_city_council",
    "kreistag"
  )
  data$event_scope <- "statewide"
  data$source_limitation <- TRUE
  data$source_note <- paste(
    "Municipality totals include postal votes;",
    "the source does not separately identify postal-voting districts."
  )

  tibble::as_tibble(data[c(
    "ags", "ags_name", "county", "state", "election_year",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
    "turnout", party_columns, "result_level", "contest_type", "event_scope",
    "source_limitation", "source_note"
  )])
}

.th_history_parse_html <- function(path, year) {
  if (!requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("rvest", quietly = TRUE)) {
    stop("Parsing the Thuringia HTML exports requires xml2 and rvest.",
         call. = FALSE)
  }

  tables <- rvest::html_table(
    xml2::read_html(path, encoding = "ISO-8859-1"),
    fill = TRUE
  )
  if (length(tables) != 1L) {
    stop("Expected one table in Thuringia ", year, " source.", call. = FALSE)
  }
  raw <- tables[[1]]
  expected_columns <- if (year == 1994L) 15L else 16L
  if (ncol(raw) != expected_columns) {
    stop("Unexpected column count in Thuringia ", year, " source.",
         call. = FALSE)
  }

  header <- as.character(raw[1, ])
  party_positions <- if (year == 1994L) 10:15 else 11:16
  party_columns <- .th_history_party_name(header[party_positions])
  if (anyDuplicated(party_columns)) {
    stop("Party normalization collides in Thuringia ", year, " source.",
         call. = FALSE)
  }

  raw <- raw[-c(1L, 2L), , drop = FALSE]
  keep <- grepl("^[0-9]{3}$", raw[[1]]) & grepl("^[0-9]+$", raw[[2]])
  raw <- raw[keep, , drop = FALSE]
  expected_rows <- if (year == 1994L) 1247L else 1019L
  if (nrow(raw) != expected_rows) {
    stop(
      "Thuringia ", year, " row count changed: expected ", expected_rows,
      ", found ", nrow(raw), ".", call. = FALSE
    )
  }

  municipality_names <- vapply(
    seq_len(nrow(raw)),
    function(i) .th_history_clean_html_name(as.character(raw[i, ])),
    character(1)
  )
  eligible_position <- if (year == 1994L) 4L else 5L
  voter_position <- if (year == 1994L) 5L else 6L
  invalid_position <- if (year == 1994L) 7L else 8L
  valid_position <- if (year == 1994L) 8L else 9L
  total_votes_position <- if (year == 1994L) 9L else 10L

  data <- data.frame(
    ags = paste0("160", raw[[2]]),
    ags_name = municipality_names,
    county = paste0("16", raw[[1]]),
    eligible_voters = .th_history_number(raw[[eligible_position]]),
    number_voters = .th_history_number(raw[[voter_position]]),
    valid_votes = .th_history_number(raw[[valid_position]]),
    invalid_votes = .th_history_count(raw[[invalid_position]]),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(party_positions)) {
    data[[party_columns[i]]] <- .th_history_number(raw[[party_positions[i]]])
  }

  .th_history_finish(
    data,
    total_party_votes = .th_history_number(raw[[total_votes_position]]),
    party_columns = party_columns,
    year = year
  )
}

.th_history_parse_1990 <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Parsing the Thuringia 1990 workbook requires readxl.", call. = FALSE)
  }

  sheets <- readxl::excel_sheets(path)
  expected_sheets <- c(
    "Stadtkreise", "Altenburg", "Apolda", "Arnstadt", "Artern",
    "Bad Salzungen", "Eisenach", "Eisenberg", "Erfurt-Land", "Gera-Land",
    "Gotha", "Greiz", "Heiligenstadt", "Hildburghausen", "Ilmenau",
    "Jena-Land", "Langensalza", "Lobenstein", "Meiningen", "Mühlhausen",
    "Neuhaus", "Nordhausen", "Pößneck", "Rudolstadt", "Saalfeld", "Schleiz",
    "Schmalkalden", "Schmölln", "Sömmerda", "Sondershausen", "Sonneberg",
    "Stadtroda", "Suhl-Land", "Weimar-Land", "Worbis", "Zeulenroda"
  )
  absent <- setdiff(expected_sheets, sheets)
  if (length(absent)) {
    stop("Thuringia 1990 workbook omits sheets: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }

  old_counties <- stats::setNames(
    sprintf("160%02d", 11:45),
    expected_sheets[-1]
  )
  city_ids <- c(
    "Erfurt, Stadt" = "16001000",
    "Gera, Stadt" = "16002000",
    "Jena, Stadt" = "16003000",
    "Suhl, Stadt" = "16004000",
    "Weimar, Stadt" = "16005000"
  )

  parsed <- lapply(expected_sheets, function(sheet) {
    raw <- suppressMessages(readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ))
    if (nrow(raw) < 6L || ncol(raw) < 14L) {
      stop("Malformed Thuringia 1990 sheet: ", sheet, call. = FALSE)
    }

    header <- as.character(raw[4, ])
    party_positions <- seq.int(13L, ncol(raw), by = 2L)
    party_positions <- party_positions[
      !is.na(header[party_positions]) & nzchar(trimws(header[party_positions]))
    ]
    # The city sheet reports both an aggregate and its component lists for
    # Bürgervereinigungen and individual candidates. Retain the components.
    party_positions <- party_positions[
      !grepl("^Summe\\s+", header[party_positions], ignore.case = TRUE)
    ]
    party_columns <- .th_history_party_name(header[party_positions])
    if (anyDuplicated(party_columns)) {
      stop("Party normalization collides in Thuringia 1990 sheet: ", sheet,
           call. = FALSE)
    }

    keep <- grepl("^[0-9]+$", raw[[1]]) &
      grepl("^[0-9]+$", raw[[2]]) &
      grepl("^[0-9]+$", raw[[3]]) &
      grepl("^[0-9]+$", raw[[5]]) &
      !is.na(raw[[4]])
    raw <- raw[keep, , drop = FALSE]
    if (!nrow(raw)) {
      stop("No municipality rows in Thuringia 1990 sheet: ", sheet,
           call. = FALSE)
    }

    names_1990 <- trimws(as.character(raw[[4]]))
    if (sheet == "Stadtkreise") {
      ags <- unname(city_ids[names_1990])
      if (anyNA(ags)) {
        stop("Unknown city in Thuringia 1990 city sheet.", call. = FALSE)
      }
    } else {
      ags <- paste0(
        unname(old_counties[sheet]),
        sprintf("%03d", as.integer(raw[[3]]) * 10L)
      )
    }

    data <- data.frame(
      ags = ags,
      ags_name = names_1990,
      county = substr(ags, 1L, 5L),
      eligible_voters = .th_history_number(raw[[5]]),
      number_voters = .th_history_number(raw[[6]]),
      valid_votes = .th_history_number(raw[[8]]),
      invalid_votes = .th_history_count(raw[[10]]),
      stringsAsFactors = FALSE
    )
    for (i in seq_along(party_positions)) {
      data[[party_columns[i]]] <- .th_history_number(raw[[party_positions[i]]])
    }
    list(
      data = data,
      total_party_votes = .th_history_number(raw[[12]]),
      party_columns = party_columns
    )
  })

  all_party_columns <- unique(unlist(lapply(parsed, `[[`, "party_columns")))
  data_parts <- lapply(parsed, function(part) {
    absent <- setdiff(all_party_columns, names(part$data))
    part$data[absent] <- NA_real_
    part$data[all_party_columns] <- lapply(
      part$data[all_party_columns],
      as.numeric
    )
    part$data
  })
  data <- do.call(rbind, lapply(data_parts, function(x) {
    x[c(
      "ags", "ags_name", "county", "eligible_voters", "number_voters",
      "valid_votes", "invalid_votes", all_party_columns
    )]
  }))
  total_party_votes <- unlist(lapply(parsed, `[[`, "total_party_votes"),
                              use.names = FALSE)

  if (nrow(data) != 1707L) {
    stop(
      "Thuringia 1990 row count changed: expected 1707, found ", nrow(data),
      ".", call. = FALSE
    )
  }
  .th_history_finish(
    data,
    total_party_votes = total_party_votes,
    party_columns = all_party_columns,
    year = 1990L
  )
}

#' Parse municipality-level Thuringia county elections for 1990, 1994 and 1999
#'
#' @param raw_dir Root county-election raw directory. It may be either the
#'   `Kreistagswahlen` directory or the Thuringia directory itself.
#' @return A tibble in the unharmonized county-election vote schema.
parse_th_historical_county_elections <- function(raw_dir) {
  th_dir <- if (basename(raw_dir) == "Thüringen") {
    raw_dir
  } else {
    file.path(raw_dir, "Thüringen")
  }
  sources <- c(
    "1990" = file.path(th_dir, "tbc_Thüringen_1990_Kreistagswahl.xlsx"),
    "1994" = file.path(th_dir, "Thüringen_1994_Kreistagswahl.xls"),
    "1999" = file.path(th_dir, "Thüringen_1999_Kreistagswahl.xls")
  )
  invisible(lapply(sources, .th_history_assert_source))

  output <- dplyr::bind_rows(
    .th_history_parse_1990(sources[["1990"]]),
    .th_history_parse_html(sources[["1994"]], 1994L),
    .th_history_parse_html(sources[["1999"]], 1999L)
  )
  if (!identical(sort(unique(output$election_year)), c(1990L, 1994L, 1999L))) {
    stop("Thuringia historical parser did not return all expected years.",
         call. = FALSE)
  }
  if (anyDuplicated(output[c("ags", "election_year")])) {
    stop("Thuringia historical parser returned duplicate AGS x year keys.",
         call. = FALSE)
  }
  output
}
