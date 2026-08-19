# Historical Mecklenburg-Vorpommern county-election results, 1990--2011.
#
# The 1990 workbook uses an Excel format too old for libxls/readxl. The parser
# first tries readxl and, only for that file, falls back to Python's xlrd.
# The 2011 source cannot support complete municipality totals because postal
# votes are reported for administrative offices rather than municipalities.
# It is therefore aggregated to complete county totals.

.mv_sum_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

.mv_as_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "x", "X", "-", ".", "...", "/")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

.mv_ags <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x <- gsub("[^0-9]", "", x)
  ifelse(nchar(x) > 0L, sprintf("%08d", as.integer(x)), NA_character_)
}

.mv_fallback_party_normaliser <- function(x) {
  x <- trimws(tolower(x))
  x <- gsub("\u00e4", "ae", x, fixed = TRUE)
  x <- gsub("\u00f6", "oe", x, fixed = TRUE)
  x <- gsub("\u00fc", "ue", x, fixed = TRUE)
  x <- gsub("\u00df", "ss", x, fixed = TRUE)
  x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
  x <- gsub("-[[:space:]]+", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  mapping <- c(
    "f.d.p." = "fdp", "fdp" = "fdp",
    "grune" = "gruene", "die linke" = "linke_pds",
    "pds" = "linke_pds", "einzelbewerber" = "einzelbewerber"
  )
  hit <- unname(mapping[x])
  fallback <- gsub("[^a-z0-9]+", "_", x)
  fallback <- gsub("^_+|_+$", "", fallback)
  ifelse(!is.na(hit), hit, fallback)
}

.mv_normalise_party <- function(x) {
  if (exists("normalise_party_cty", mode = "function", inherits = TRUE)) {
    normaliser <- get("normalise_party_cty", mode = "function", inherits = TRUE)
    out <- vapply(x, normaliser, character(1L), USE.NAMES = FALSE)
  } else {
    out <- .mv_fallback_party_normaliser(x)
  }
  out <- as.character(out)
  out[is.na(out) | !nzchar(out)] <- "unknown_party"
  out
}

.mv_check_source <- function(path) {
  if (!file.exists(path)) {
    stop("Missing expected MV source: ", path, call. = FALSE)
  }
  first_line <- readLines(path, n = 1L, warn = FALSE)
  if (length(first_line) && identical(first_line, "version https://git-lfs.github.com/spec/v1")) {
    stop("MV source is an unhydrated Git LFS pointer: ", path, call. = FALSE)
  }
  invisible(path)
}

.mv_read_old_xls <- function(path) {
  readxl_result <- tryCatch(
    suppressMessages(
      readxl::read_excel(
        path,
        sheet = 1L,
        col_names = FALSE,
        col_types = "text",
        .name_repair = "minimal"
      )
    ),
    error = identity
  )
  if (!inherits(readxl_result, "error")) {
    return(as.data.frame(readxl_result, stringsAsFactors = FALSE))
  }

  python <- Sys.which("python3")
  if (!nzchar(python)) {
    stop(
      "The 1990 MV workbook requires Python 3 with xlrd because readxl cannot read its old Excel format.",
      call. = FALSE
    )
  }

  script <- tempfile(fileext = ".py")
  output <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(script, output)), add = TRUE)
  writeLines(
    c(
      "import csv, sys",
      "try:",
      "    import xlrd",
      "except ImportError:",
      "    raise SystemExit('Python package xlrd is required for the 1990 MV workbook')",
      "book = xlrd.open_workbook(sys.argv[1])",
      "sheet = book.sheet_by_index(0)",
      "with open(sys.argv[2], 'w', encoding='utf-8', newline='') as handle:",
      "    writer = csv.writer(handle, delimiter='\\t', lineterminator='\\n')",
      "    for row in range(sheet.nrows):",
      "        writer.writerow([sheet.cell_value(row, col) for col in range(sheet.ncols)])"
    ),
    script
  )
  status <- system2(python, c(script, path, output), stdout = TRUE, stderr = TRUE)
  exit_status <- attr(status, "status")
  if (!is.null(exit_status) && exit_status != 0L) {
    stop(
      "Could not read the 1990 MV workbook with readxl or xlrd: ",
      paste(status, collapse = "\n"),
      call. = FALSE
    )
  }
  utils::read.delim(
    output,
    header = FALSE,
    sep = "\t",
    quote = "",
    colClasses = "character",
    check.names = FALSE,
    fill = TRUE,
    stringsAsFactors = FALSE
  )
}

.mv_combine_party_columns <- function(df, party_positions, party_names) {
  party_names <- .mv_normalise_party(party_names)
  unique_names <- unique(party_names)
  out <- vector("list", length(unique_names))
  names(out) <- unique_names

  for (name in unique_names) {
    positions <- party_positions[party_names == name]
    values <- lapply(positions, function(position) .mv_as_numeric(df[[position]]))
    values <- as.data.frame(values, check.names = FALSE)
    all_missing <- apply(is.na(values), 1L, all)
    combined <- rowSums(values, na.rm = TRUE)
    combined[all_missing] <- NA_real_
    out[[name]] <- combined
  }
  as.data.frame(out, check.names = FALSE)
}

.mv_aggregate_units <- function(df, unit_key, party_cols) {
  df$.unit_key <- unit_key
  numeric_cols <- c(
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes",
    party_cols
  )
  groups <- split(seq_len(nrow(df)), df$.unit_key)
  rows <- lapply(groups, function(index) {
    values <- lapply(numeric_cols, function(column) .mv_sum_na(df[[column]][index]))
    names(values) <- numeric_cols
    data.frame(
      ags = df$.unit_key[index[1L]],
      ags_name = {
        available <- df$ags_name[index]
        available <- available[!is.na(available) & nzchar(available)]
        if (length(available)) available[1L] else NA_character_
      },
      as.data.frame(values, check.names = FALSE),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.mv_finish <- function(df, year, result_level, event_scope, party_cols) {
  df$election_year <- as.integer(year)
  df$state <- "13"
  df$county <- substr(df$ags, 1L, 5L)
  df$result_level <- result_level
  df$contest_type <- ifelse(
    df$county %in% sprintf("130%02d", 1:6),
    "kreisfreie_city_council",
    "kreistag"
  )
  df$event_scope <- event_scope
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

  leading <- c(
    "ags", "ags_name", "county", "state", "election_year",
    "result_level", "contest_type", "event_scope",
    "eligible_voters", "number_voters", "turnout",
    "invalid_votes", "valid_votes"
  )
  tibble::as_tibble(df[c(leading, party_cols)])
}

.mv_validate <- function(df, year, party_cols) {
  if (!nrow(df)) stop("MV ", year, " parser returned no rows.", call. = FALSE)
  duplicated_key <- duplicated(paste(df$ags, df$election_year, sep = "-"))
  if (any(duplicated_key)) {
    stop("MV ", year, " has duplicate AGS x year rows.", call. = FALSE)
  }
  required <- c(
    "ags", "county", "state", "election_year", "result_level",
    "contest_type", "event_scope", "eligible_voters", "number_voters",
    "invalid_votes", "valid_votes"
  )
  if (anyNA(df[required])) {
    stop("MV ", year, " has missing values in required columns.", call. = FALSE)
  }
  if (any(df$eligible_voters <= 0) ||
      any(df$number_voters < 0) ||
      any(df$valid_votes < 0)) {
    stop("MV ", year, " has malformed voter or vote totals.", call. = FALSE)
  }
  if (any(df$turnout < 0 | df$turnout > 1.01, na.rm = TRUE)) {
    stop("MV ", year, " has substantively malformed turnout.", call. = FALSE)
  }

  shares <- as.matrix(df[party_cols])
  if (any(shares < 0 | shares > 1, na.rm = TRUE)) {
    stop("MV ", year, " has party shares outside [0, 1].", call. = FALSE)
  }
  observed_sum <- rowSums(shares, na.rm = TRUE)
  expected_one <- df$valid_votes > 0
  if (any(abs(observed_sum[expected_one] - 1) > 1e-7)) {
    stop("MV ", year, " party votes do not sum to valid votes.", call. = FALSE)
  }
  invisible(df)
}

.mv_parse_1990 <- function(path) {
  raw <- .mv_read_old_xls(path)
  if (ncol(raw) < 38L) stop("Malformed MV 1990 workbook.", call. = FALSE)

  current_county <- NA_character_
  records <- vector("list", nrow(raw))
  keep <- logical(nrow(raw))
  for (row in seq_len(nrow(raw))) {
    identifier <- sub("\\.0+$", "", trimws(as.character(raw[[1L]][row])))
    eligible <- .mv_as_numeric(raw[[3L]][row])
    if (grepl("^13[0-9]{3}$", identifier) && is.na(eligible)) {
      current_county <- identifier
    } else if (!is.na(eligible) && eligible > 0 && nzchar(identifier)) {
      if (grepl("^13[0-9]{6}$", identifier)) {
        ags <- identifier
      } else if (grepl("^[0-9]{1,3}$", identifier) && !is.na(current_county)) {
        ags <- paste0(current_county, sprintf("%03d", as.integer(identifier)))
      } else {
        next
      }
      keep[row] <- TRUE
      records[[row]] <- ags
    }
  }
  rows <- which(keep)
  data <- raw[rows, , drop = FALSE]
  party_positions <- seq.int(11L, 37L, by = 2L)
  party_names <- c(
    "Bauern", "B.F.D.", "CDU", "DBD", "DFD", "DSU", "GRÜNE",
    "NF", "PDS", "SPD", "BV", "VS", "EV", "Sonstige"
  )
  parties <- .mv_combine_party_columns(data, party_positions, party_names)
  party_cols <- names(parties)
  df <- data.frame(
    ags = unlist(records[rows], use.names = FALSE),
    ags_name = trimws(as.character(data[[2L]])),
    eligible_voters = .mv_as_numeric(data[[3L]]),
    number_voters = .mv_as_numeric(data[[4L]]),
    invalid_votes = .mv_as_numeric(data[[8L]]),
    valid_votes = .mv_as_numeric(data[[10L]]),
    parties,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  result <- .mv_finish(df, 1990L, "municipality", "statewide", party_cols)
  .mv_validate(result, 1990L, party_cols)
  attr(result, "source_rows") <- nrow(df)
  result
}

.mv_party_header <- function(raw, rows, positions) {
  vapply(positions, function(position) {
    pieces <- trimws(vapply(
      rows,
      function(row) as.character(raw[[position]][row]),
      character(1L)
    ))
    pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
    gsub("-[[:space:]]+", "", paste(pieces, collapse = " "))
  }, character(1L))
}

.mv_parse_later <- function(path, year) {
  settings <- list(
    `1994` = list(sheet = "Ergebnisse nach Wahlbezirken", start = 9L,
                  ags = 2L, name = NA_integer_, eligible = 4L, voters = 5L,
                  invalid = 6L, valid = 7L, party_start = 8L,
                  header = 6L, level = "municipality"),
    `1999` = list(sheet = "B734W 199901", start = 9L,
                  ags = 3L, name = NA_integer_, eligible = 8L, voters = 9L,
                  invalid = 11L, valid = 12L, party_start = 13L,
                  header = c(5L, 6L), level = "municipality"),
    `2004` = list(sheet = "Ergebnisse nach Gemeinden", start = 9L,
                  ags = 4L, name = 5L, eligible = 6L, voters = 7L,
                  invalid = 8L, valid = 9L, party_start = 10L,
                  header = 6L, level = "municipality"),
    `2009` = list(sheet = "Ergebnisse nach Gemeinden", start = 10L,
                  ags = 4L, name = 5L, eligible = 9L, voters = 10L,
                  invalid = 12L, valid = 13L, party_start = 14L,
                  header = c(7L, 8L), level = "municipality"),
    `2011` = list(sheet = "gem", start = 10L,
                  ags = 4L, name = 5L, eligible = 9L, voters = 10L,
                  invalid = 12L, valid = 13L, party_start = 14L,
                  header = c(7L, 8L), level = "county")
  )
  config <- settings[[as.character(year)]]
  raw <- suppressMessages(
    readxl::read_excel(
      path,
      sheet = config$sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (ncol(raw) < config$party_start) {
    stop("Malformed MV ", year, " workbook.", call. = FALSE)
  }

  rows <- config$start:nrow(raw)
  ags <- .mv_ags(raw[[config$ags]][rows])
  valid_row <- grepl("^13[0-9]{6}$", ags)
  data <- raw[rows[valid_row], , drop = FALSE]
  ags <- ags[valid_row]
  if (!length(ags)) stop("MV ", year, " contains no valid AGS rows.", call. = FALSE)

  party_positions <- config$party_start:ncol(raw)
  party_names <- .mv_party_header(raw, config$header, party_positions)
  nonempty <- nzchar(party_names)
  party_positions <- party_positions[nonempty]
  party_names <- party_names[nonempty]
  parties <- .mv_combine_party_columns(data, party_positions, party_names)
  party_cols <- names(parties)
  ags_name <- if (is.na(config$name)) {
    rep(NA_character_, nrow(data))
  } else {
    trimws(as.character(data[[config$name]]))
  }
  df <- data.frame(
    ags = ags,
    ags_name = ags_name,
    eligible_voters = .mv_as_numeric(data[[config$eligible]]),
    number_voters = .mv_as_numeric(data[[config$voters]]),
    invalid_votes = .mv_as_numeric(data[[config$invalid]]),
    valid_votes = .mv_as_numeric(data[[config$valid]]),
    parties,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  source_rows <- nrow(df)
  unallocated_postal_votes <- 0

  if (year == 2009L) {
    unallocatable <- df$eligible_voters == 0 &
      grepl("^Briefwahl ", df$ags_name)
    if (sum(unallocatable) != 1L ||
        df$ags[unallocatable] != "13053711" ||
        df$valid_votes[unallocatable] != 864) {
      stop(
        "MV 2009 pooled postal-vote rows differ from the documented source limitation.",
        call. = FALSE
      )
    }
    unallocated_postal_votes <- sum(df$valid_votes[unallocatable])
    warning(
      "MV 2009: omitting one Bützow-Land postal-vote pool (864 valid votes) ",
      "that the source does not allocate to municipalities.",
      call. = FALSE
    )
    df <- df[!unallocatable, , drop = FALSE]
  }

  if (identical(config$level, "county")) {
    unit_key <- paste0(substr(df$ags, 1L, 5L), "000")
    df <- .mv_aggregate_units(df, unit_key, party_cols)
    county_names <- c(
      "13071000" = "Mecklenburgische Seenplatte",
      "13072000" = "Landkreis Rostock",
      "13073000" = "Vorpommern-R\u00fcgen",
      "13074000" = "Nordwestmecklenburg",
      "13075000" = "Vorpommern-Greifswald",
      "13076000" = "Ludwigslust-Parchim"
    )
    df$ags_name <- unname(county_names[df$ags])
    event_scope <- "split_reform"
  } else {
    df <- .mv_aggregate_units(df, df$ags, party_cols)
    event_scope <- "statewide"
  }
  result <- .mv_finish(df, year, config$level, event_scope, party_cols)
  .mv_validate(result, year, party_cols)
  attr(result, "source_rows") <- source_rows
  attr(result, "unallocated_postal_votes") <- unallocated_postal_votes
  result
}

#' Parse historical Mecklenburg-Vorpommern county elections
#'
#' @param raw_dir Either the `Kreistagswahlen` raw directory or its
#'   `Mecklenburg-Vorpommern` subdirectory.
#' @return A tibble in the unharmonized county-election schema. Elections from
#'   1990--2009 are municipality-level. The 2011 reform election is county-level
#'   because its postal votes cannot be assigned to municipalities.
parse_mv_historical_county_elections <- function(raw_dir) {
  mv_dir <- if (basename(normalizePath(raw_dir, mustWork = FALSE)) ==
                "Mecklenburg-Vorpommern") {
    raw_dir
  } else {
    file.path(raw_dir, "Mecklenburg-Vorpommern")
  }
  expected_years <- c(1990L, 1994L, 1999L, 2004L, 2009L, 2011L)
  paths <- setNames(
    file.path(
      mv_dir,
      paste0("Mecklenburg-Vorpommern_", expected_years, "_Kreistagswahl.xls")
    ),
    expected_years
  )
  invisible(lapply(paths, .mv_check_source))

  results <- lapply(expected_years, function(year) {
    if (year == 1990L) {
      .mv_parse_1990(paths[[as.character(year)]])
    } else {
      .mv_parse_later(paths[[as.character(year)]], year)
    }
  })
  names(results) <- expected_years
  diagnostics <- data.frame(
    election_year = expected_years,
    source_rows = vapply(results, function(x) attr(x, "source_rows"), integer(1L)),
    output_rows = vapply(results, nrow, integer(1L)),
    unallocated_postal_votes = vapply(
      results,
      function(x) {
        value <- attr(x, "unallocated_postal_votes")
        if (is.null(value)) 0 else value
      },
      numeric(1L)
    )
  )
  output <- dplyr::bind_rows(results)
  found_years <- sort(unique(output$election_year))
  if (!identical(found_years, expected_years)) {
    stop(
      "MV parser did not return the exact expected years: ",
      paste(expected_years, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(paste(output$ags, output$election_year, sep = "-"))) {
    stop("MV historical output has duplicate AGS x year rows.", call. = FALSE)
  }
  attr(output, "aggregation_diagnostics") <- diagnostics
  output
}
