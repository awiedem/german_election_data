# Parser for Rhineland-Palatinate county and county-equivalent elections.
#
# The state publishes a "weighted" result because voters may cast different
# numbers of candidate votes across councils of different sizes. The weighted
# party counts allocate valid ballots in proportion to raw candidate votes.
# These are the counts retained here and used to calculate party shares.

set.seed(20260730)

rlp_require_namespaces <- function() {
  required <- c("dplyr", "readxl", "pdftools", "rvest", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "RLP parser requires missing package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

rlp_is_lfs_pointer <- function(path) {
  if (!file.exists(path) || file.info(path)$size > 1024) {
    return(FALSE)
  }
  first_line <- readLines(path, n = 1, warn = FALSE)
  identical(first_line, "version https://git-lfs.github.com/spec/v1")
}

rlp_find_one <- function(candidates, label) {
  existing <- unique(candidates[file.exists(candidates)])
  if (length(existing) == 0) {
    stop("Missing RLP ", label, ". Checked: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  path <- existing[[1]]
  if (rlp_is_lfs_pointer(path)) {
    stop(
      "RLP ", label, " is an unhydrated Git LFS pointer: ", path,
      ". Hydrate the file before running the parser.",
      call. = FALSE
    )
  }
  path
}

rlp_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "\u2013", "\u2014")] <- NA_character_
  x <- gsub("\\.", "", x)
  x <- sub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

rlp_party_name <- function(x) {
  key <- tolower(trimws(x))
  mapping <- c(
    "spd" = "spd",
    "cdu" = "cdu",
    "afd" = "afd",
    "fdp" = "fdp",
    "grüne" = "gruene",
    "gruene" = "gruene",
    "die linke" = "linke_pds",
    "pds" = "linke_pds",
    "freie wähler" = "freie_waehler",
    "freie waehler" = "freie_waehler",
    "piraten" = "piraten",
    "die partei" = "die_partei",
    "big" = "big",
    "bsw" = "bsw",
    "dkp" = "dkp",
    "ödp" = "oedp",
    "odp" = "oedp",
    "lkr" = "lkr",
    "volt" = "volt",
    "npd" = "npd",
    "rep" = "rep",
    "die tierschutzpartei" = "tierschutz",
    "tierschutzpartei" = "tierschutz",
    "wählergruppen" = "waehlergruppen",
    "wähler-gruppen" = "waehlergruppen",
    "wähler-gruppen [summe]" = "waehlergruppen",
    "summe wählergruppen" = "waehlergruppen",
    "sonstige" = "sonstige"
  )
  if (key %in% names(mapping)) {
    return(unname(mapping[[key]]))
  }
  key <- iconv(key, from = "", to = "ASCII//TRANSLIT")
  key <- gsub("[^a-z0-9]+", "_", key)
  key <- gsub("^_|_$", "", key)
  key
}

rlp_header <- function(raw, rows, column) {
  values <- trimws(as.character(unlist(raw[rows, column], use.names = FALSE)))
  values <- values[!is.na(values) & nzchar(values)]
  paste(values, collapse = " | ")
}

rlp_header_party <- function(header) {
  parts <- trimws(strsplit(header, "\\|")[[1]])
  parts <- parts[
    !grepl("^Von den ", parts, ignore.case = TRUE) &
      !parts %in% c("Anzahl", "%", "absolut")
  ]
  if (length(parts) == 0) NA_character_ else tail(parts, 1)
}

rlp_official_ags <- function(source_code) {
  code <- suppressWarnings(as.numeric(source_code))
  local_code <- ifelse(code < 1000, sprintf("%03d", code), substr(sprintf("%.0f", code), 1, 3))
  paste0("07", local_code, "000")
}

rlp_city_counties <- c(
  "07111", "07211", "07311", "07312", "07313", "07314",
  "07315", "07316", "07317", "07318", "07319", "07320"
)

rlp_parse_workbook_sheet <- function(workbook, year) {
  raw <- readxl::read_excel(
    workbook,
    sheet = as.character(year),
    col_names = FALSE,
    .name_repair = "minimal"
  )
  header_rows <- if (year == 2004L) 3:6 else 5:7
  headers <- vapply(
    seq_len(ncol(raw)),
    function(column) rlp_header(raw, header_rows, column),
    character(1)
  )

  code <- suppressWarnings(as.numeric(raw[[1]]))
  row_keep <- if (year == 2004L) {
    !is.na(code) & code >= 100 & code < 900
  } else {
    !is.na(code) & code >= 10000000 & code < 90000000
  }
  if (sum(row_keep) != 36L) {
    stop("Expected 36 RLP units in workbook sheet ", year, "; found ", sum(row_keep), call. = FALSE)
  }

  eligible_col <- grep("^Wahlbe", headers, ignore.case = TRUE)[1]
  voters_col <- grep("^Wähler \\| Anzahl|^Wähler / Anzahl", headers, ignore.case = TRUE)[1]
  invalid_col <- grep("^Ungültige", headers, ignore.case = TRUE)[1]
  valid_col <- if (year == 2004L) {
    grep("^Gültige Stimmen insgesamt \\| Anzahl", headers, ignore.case = TRUE)[1]
  } else {
    grep("^Gültige Stimmzettel \\| Anzahl", headers, ignore.case = TRUE)[1]
  }
  if (any(is.na(c(eligible_col, voters_col, invalid_col, valid_col)))) {
    stop("Could not identify turnout columns in RLP workbook sheet ", year, call. = FALSE)
  }

  party_start <- if (year == 2004L) {
    grep("^Von den gültigen Stimmen entfielen auf", headers, ignore.case = TRUE)[1]
  } else {
    grep("^Von den gewichteten Stimmen entfielen auf", headers, ignore.case = TRUE)[1]
  }
  party_end <- if (year == 2004L) {
    grep("^Ungewichtete Stimmen insgesamt", headers, ignore.case = TRUE)[1] - 1L
  } else {
    grep("^Gültige Stimmen insgesamt", headers, ignore.case = TRUE)[1] - 1L
  }
  if (is.na(party_start) || is.na(party_end) || party_end < party_start) {
    stop("Could not identify party columns in RLP workbook sheet ", year, call. = FALSE)
  }

  count_cols <- seq.int(party_start, party_end)
  count_cols <- count_cols[
    grepl("Anzahl", headers[count_cols], fixed = TRUE) &
      count_cols < ncol(raw) &
      trimws(headers[count_cols + 1L]) == "%"
  ]
  party_labels <- vapply(headers[count_cols], rlp_header_party, character(1))

  # Composite/detailed columns would double count the same votes.
  keep_party <- !grepl(
    "^Sonstige Parteien|^WGr\\.|^Wählergruppe [123]$",
    party_labels,
    ignore.case = TRUE
  )
  count_cols <- count_cols[keep_party]
  party_labels <- party_labels[keep_party]
  party_names <- vapply(party_labels, rlp_party_name, character(1))
  if (any(!nzchar(party_names)) || anyDuplicated(party_names)) {
    stop("Invalid or duplicate normalized party names in RLP workbook sheet ", year, call. = FALSE)
  }

  ags <- rlp_official_ags(code[row_keep])
  result <- tibble::tibble(
    ags = ags,
    ags_name = trimws(as.character(raw[[2]][row_keep])),
    county = substr(ags, 1, 5),
    state = "07",
    election_year = as.integer(year),
    eligible_voters = rlp_number(raw[[eligible_col]][row_keep]),
    number_voters = rlp_number(raw[[voters_col]][row_keep]),
    valid_votes = rlp_number(raw[[valid_col]][row_keep]),
    invalid_votes = rlp_number(raw[[invalid_col]][row_keep])
  )
  result$turnout <- ifelse(
    result$eligible_voters > 0,
    result$number_voters / result$eligible_voters,
    NA_real_
  )
  result$result_level <- "county"
  result$contest_type <- ifelse(
    result$county %in% rlp_city_counties,
    "kreisfreie_city_council",
    "kreistag"
  )
  result$event_scope <- "statewide"

  for (index in seq_along(count_cols)) {
    party <- party_names[[index]]
    counts <- rlp_number(raw[[count_cols[[index]]]][row_keep])
    result[[paste0("vote_count_", party)]] <- counts
    result[[party]] <- ifelse(result$valid_votes > 0, counts / result$valid_votes, NA_real_)
  }
  result
}

rlp_parse_html_1999_file <- function(html_path) {
  source_text <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  if (
    !grepl("Statistisches Landesamt", source_text, fixed = TRUE) ||
      !grepl(
        "/kw/wahlen/2004/kreistagswahlen/ergebnisse/",
        source_text,
        fixed = TRUE
      )
  ) {
    stop("RLP 1999 HTML does not contain the expected official-source markers: ", html_path, call. = FALSE)
  }

  file_name <- basename(html_path)
  county_code <- sub(
    "^Rheinland-Pfalz_1999_(07[0-9]{3})\\.html$",
    "\\1",
    file_name
  )
  if (!grepl("^07[0-9]{3}$", county_code)) {
    stop("Could not recover a five-digit RLP county code from: ", file_name, call. = FALSE)
  }
  ags <- paste0(county_code, "000")
  if (!ags %in% unname(rlp_pdf_ags)) {
    stop("Unexpected RLP 1999 county/county-equivalent code: ", county_code, call. = FALSE)
  }

  document <- rvest::read_html(html_path)
  page_title <- rvest::html_text2(rvest::html_element(document, "title"))
  heading <- rvest::html_text2(rvest::html_element(document, "h2"))
  if (
    length(page_title) != 1L ||
      !grepl("Wahlergebnisse 2004", page_title, fixed = TRUE) ||
      length(heading) != 1L ||
      !grepl("Endgültiges Ergebnis", heading, fixed = TRUE)
  ) {
    stop("RLP 1999 HTML has an unexpected title or result heading: ", html_path, call. = FALSE)
  }
  ags_name <- trimws(sub("\\s*-\\s*Endgültiges Ergebnis.*$", "", heading))

  table_node <- rvest::html_element(document, "table.erg")
  if (inherits(table_node, "xml_missing")) {
    stop("Could not find the official result table in RLP 1999 HTML: ", html_path, call. = FALSE)
  }
  result_table <- rvest::html_table(table_node, fill = TRUE)
  year_columns <- which(grepl("wahl 1999$", names(result_table), ignore.case = TRUE))
  if (
    length(year_columns) != 3L ||
      !identical(
        trimws(as.character(unlist(result_table[1, year_columns], use.names = FALSE))),
        c("Anzahl", "%", "Sitze")
      )
  ) {
    stop("Could not identify the 1999 count/share/seat columns in: ", html_path, call. = FALSE)
  }

  labels <- trimws(as.character(result_table[[1]]))
  metric_row <- function(label) {
    matches <- which(labels == label)
    if (length(matches) != 1L) {
      stop("Expected one '", label, "' row in ", html_path, "; found ", length(matches), call. = FALSE)
    }
    matches[[1]]
  }
  eligible_row <- metric_row("Wahlberechtigte")
  voters_row <- metric_row("Wähler")
  invalid_row <- metric_row("Ungültige Stimmzettel")
  valid_row <- metric_row("Gültige Stimmzettel")
  count_column <- year_columns[[1]]
  share_column <- year_columns[[2]]

  eligible_voters <- rlp_number(result_table[[count_column]][eligible_row])
  number_voters <- rlp_number(result_table[[count_column]][voters_row])
  invalid_votes <- rlp_number(result_table[[count_column]][invalid_row])
  valid_votes <- rlp_number(result_table[[count_column]][valid_row])
  if (
    any(is.na(c(eligible_voters, number_voters, invalid_votes, valid_votes))) ||
      valid_votes + invalid_votes != number_voters
  ) {
    stop("RLP 1999 ballot totals are missing or inconsistent in: ", html_path, call. = FALSE)
  }

  party_rows <- seq.int(valid_row + 1L, nrow(result_table))
  party_rows <- party_rows[nzchar(labels[party_rows])]
  party_rows <- party_rows[
    !grepl("^Wählergruppe [123]$", labels[party_rows], ignore.case = TRUE)
  ]
  party_labels <- sub("\\s+[0-9]+\\)$", "", labels[party_rows])
  party_names <- vapply(party_labels, rlp_party_name, character(1))
  if (any(!nzchar(party_names)) || anyDuplicated(party_names)) {
    stop("Invalid or duplicate normalized party names in RLP 1999 HTML: ", html_path, call. = FALSE)
  }
  party_counts <- rlp_number(result_table[[count_column]][party_rows])
  source_shares <- rlp_number(result_table[[share_column]][party_rows])
  if (sum(party_counts, na.rm = TRUE) != valid_votes) {
    stop("RLP 1999 weighted party counts do not sum to valid ballots in: ", html_path, call. = FALSE)
  }
  comparable <- !is.na(party_counts) & !is.na(source_shares)
  calculated_percent <- 100 * party_counts[comparable] / valid_votes
  if (any(abs(calculated_percent - source_shares[comparable]) > 0.051)) {
    stop("RLP 1999 displayed party shares disagree with weighted counts in: ", html_path, call. = FALSE)
  }

  result <- tibble::tibble(
    ags = ags,
    ags_name = ags_name,
    county = county_code,
    state = "07",
    election_year = 1999L,
    eligible_voters = eligible_voters,
    number_voters = number_voters,
    valid_votes = valid_votes,
    invalid_votes = invalid_votes,
    turnout = number_voters / eligible_voters,
    result_level = "county",
    contest_type = ifelse(
      county_code %in% rlp_city_counties,
      "kreisfreie_city_council",
      "kreistag"
    ),
    event_scope = "statewide"
  )
  for (index in seq_along(party_names)) {
    party <- party_names[[index]]
    count <- party_counts[[index]]
    result[[paste0("vote_count_", party)]] <- count
    result[[party]] <- ifelse(valid_votes > 0, count / valid_votes, NA_real_)
  }
  result
}

rlp_parse_html_1999 <- function(html_dir) {
  html_files <- sort(list.files(
    html_dir,
    pattern = "^Rheinland-Pfalz_1999_07[0-9]{3}\\.html$",
    full.names = TRUE
  ))
  if (length(html_files) != 36L) {
    stop("Expected 36 official RLP 1999 HTML result pages; found ", length(html_files), call. = FALSE)
  }
  result <- dplyr::bind_rows(lapply(html_files, rlp_parse_html_1999_file))
  if (nrow(result) != 36L || anyDuplicated(result$ags)) {
    stop("RLP 1999 HTML parser did not produce 36 unique units", call. = FALSE)
  }
  result
}

rlp_pdf_number_tokens <- function(line) {
  pattern <- "(?<!\\S)(?:-|[0-9][0-9.]*)(?:,[0-9]+)?(?=\\s|$)"
  match <- gregexpr(pattern, line, perl = TRUE)
  tokens <- regmatches(line, match)[[1]]
  if (identical(tokens, character(0))) character(0) else tokens
}

rlp_pdf_ags <- c(
  "city:Frankenthal (Pfalz)" = "07311000",
  "city:Kaiserslautern" = "07312000",
  "city:Koblenz" = "07111000",
  "city:Landau in der Pfalz" = "07313000",
  "city:Ludwigshafen am Rhein" = "07314000",
  "city:Mainz" = "07315000",
  "city:Neustadt an der Weinstraße" = "07316000",
  "city:Pirmasens" = "07317000",
  "city:Speyer" = "07318000",
  "city:Trier" = "07211000",
  "city:Worms" = "07319000",
  "city:Zweibrücken" = "07320000",
  "county:Ahrweiler" = "07131000",
  "county:Altenkirchen" = "07132000",
  "county:Alzey-Worms" = "07331000",
  "county:Bad Dürkheim" = "07332000",
  "county:Bad Kreuznach" = "07133000",
  "county:Bernkastel-Wittlich" = "07231000",
  "county:Birkenfeld" = "07134000",
  "county:Cochem-Zell" = "07135000",
  "county:Donnersbergkreis" = "07333000",
  "county:Eifelkreis Bitburg-Prüm" = "07232000",
  "county:Germersheim" = "07334000",
  "county:Kaiserslautern" = "07335000",
  "county:Kusel" = "07336000",
  "county:Mainz-Bingen" = "07339000",
  "county:Mayen-Koblenz" = "07137000",
  "county:Neuwied" = "07138000",
  "county:Rhein-Hunsrück-Kreis" = "07140000",
  "county:Rhein-Lahn-Kreis" = "07141000",
  "county:Rhein-Pfalz-Kreis" = "07338000",
  "county:Südliche Weinstraße" = "07337000",
  "county:Südwestpfalz" = "07340000",
  "county:Trier-Saarburg" = "07235000",
  "county:Vulkaneifel" = "07233000",
  "county:Westerwaldkreis" = "07143000"
)

rlp_pdf_party_labels <- c(
  "Summe Wählergruppen", "FREIE WÄHLER", "Die PARTEI", "DIE LINKE",
  "PIRATEN", "SONSTIGE", "GRÜNE", "SPD", "CDU", "AfD", "FDP",
  "BIG", "BSW", "DKP", "ÖDP", "Volt"
)

rlp_parse_pdf_2024 <- function(pdf) {
  lines <- unlist(strsplit(paste(pdftools::pdf_text(pdf), collapse = "\n"), "\n", fixed = TRUE))
  section <- NA_character_
  records <- list()
  current_key <- NA_character_

  for (line in lines) {
    trimmed <- trimws(line)
    if (grepl("^T2\\s+Kommunalwahlergebnisse", trimmed)) {
      section <- "city"
      next
    }
    if (grepl("^T3\\s+Kommunalwahlergebnisse", trimmed)) {
      section <- "county"
      next
    }
    if (grepl("^T4\\s+", trimmed)) {
      section <- NA_character_
      current_key <- NA_character_
      next
    }
    candidate_key <- paste0(section, ":", trimmed)
    if (!is.na(section) && candidate_key %in% names(rlp_pdf_ags)) {
      current_key <- candidate_key
      records[[current_key]] <- list(
        ags_name = trimmed,
        ags = unname(rlp_pdf_ags[[current_key]]),
        contest_type = if (section == "city") "kreisfreie_city_council" else "kreistag",
        parties = list()
      )
      next
    }
    if (is.na(current_key)) {
      next
    }

    metric_names <- c(
      "Wahlberechtigte" = "eligible_voters",
      "Wähler" = "number_voters",
      "Ungültige Stimmzettel" = "invalid_votes",
      "Gültige Stimmzettel" = "valid_votes"
    )
    metric <- names(metric_names)[startsWith(trimmed, names(metric_names))]
    if (length(metric) > 0) {
      tokens <- rlp_pdf_number_tokens(trimmed)
      if (length(tokens) == 0) {
        stop("Could not parse 2024 metric line: ", trimmed, call. = FALSE)
      }
      records[[current_key]][[metric_names[[metric[[1]]]]]] <- rlp_number(tokens[[1]])
      next
    }

    party_label <- rlp_pdf_party_labels[
      startsWith(tolower(trimmed), tolower(paste0(rlp_pdf_party_labels, " ")))
    ]
    if (length(party_label) > 0 && !startsWith(trimmed, "darunter")) {
      label <- party_label[[1]]
      tokens <- rlp_pdf_number_tokens(trimmed)
      if (length(tokens) < 3) {
        stop("Could not parse 2024 party line: ", trimmed, call. = FALSE)
      }
      party <- rlp_party_name(label)
      records[[current_key]]$parties[[party]] <- rlp_number(tokens[[2]])
    }
  }

  if (length(records) != 36L || !setequal(names(records), names(rlp_pdf_ags))) {
    missing <- setdiff(names(rlp_pdf_ags), names(records))
    stop(
      "Expected 36 RLP units in 2024 PDF; found ", length(records),
      if (length(missing) > 0) paste0(". Missing: ", paste(missing, collapse = ", ")) else "",
      call. = FALSE
    )
  }
  required_metrics <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")
  incomplete <- names(records)[
    !vapply(records, function(x) all(required_metrics %in% names(x)), logical(1))
  ]
  if (length(incomplete) > 0) {
    stop("Missing 2024 turnout metrics for: ", paste(incomplete, collapse = ", "), call. = FALSE)
  }

  party_universe <- sort(unique(unlist(lapply(records, function(x) names(x$parties)))))
  rows <- lapply(records, function(record) {
    row <- tibble::tibble(
      ags = record$ags,
      ags_name = record$ags_name,
      county = substr(record$ags, 1, 5),
      state = "07",
      election_year = 2024L,
      eligible_voters = record$eligible_voters,
      number_voters = record$number_voters,
      valid_votes = record$valid_votes,
      invalid_votes = record$invalid_votes,
      turnout = record$number_voters / record$eligible_voters,
      result_level = "county",
      contest_type = record$contest_type,
      event_scope = "statewide"
    )
    for (party in party_universe) {
      count <- record$parties[[party]]
      if (is.null(count)) count <- NA_real_
      row[[paste0("vote_count_", party)]] <- count
      row[[party]] <- ifelse(row$valid_votes > 0, count / row$valid_votes, NA_real_)
    }
    row
  })
  dplyr::bind_rows(rows)
}

#' Parse Rhineland-Palatinate county and county-equivalent election results
#'
#' @param raw_dir Either `data/county_elections/raw` or its
#'   `Kreistagswahlen` subdirectory.
#' @return One row per county/county-equivalent and election year. Party columns
#'   are shares; matching `vote_count_<party>` columns retain weighted counts.
parse_rlp_county_elections <- function(raw_dir) {
  rlp_require_namespaces()
  raw_dir <- normalizePath(raw_dir, mustWork = TRUE)
  raw_parent <- if (basename(raw_dir) == "Kreistagswahlen") dirname(raw_dir) else raw_dir
  county_dir <- if (basename(raw_dir) == "Kreistagswahlen") {
    raw_dir
  } else {
    file.path(raw_dir, "Kreistagswahlen")
  }
  zip_path <- rlp_find_one(
    c(file.path(raw_parent, "local_elections_rlp.zip"), file.path(raw_dir, "local_elections_rlp.zip")),
    "2004-2019 ZIP archive"
  )
  pdf_path <- rlp_find_one(
    c(
      file.path(county_dir, "Rheinland-Pfalz", "Rheinland-Pfalz_2024_Ergebnisse_Kreisebene.pdf"),
      file.path(raw_dir, "Rheinland-Pfalz", "Rheinland-Pfalz_2024_Ergebnisse_Kreisebene.pdf")
    ),
    "2024 county-results PDF"
  )
  html_dir_candidates <- c(
    file.path(county_dir, "Rheinland-Pfalz", "1999_html"),
    file.path(raw_dir, "Rheinland-Pfalz", "1999_html")
  )
  html_dir <- unique(html_dir_candidates[dir.exists(html_dir_candidates)])
  if (length(html_dir) == 0L) {
    stop(
      "Missing RLP 1999 official HTML directory. Checked: ",
      paste(html_dir_candidates, collapse = ", "),
      call. = FALSE
    )
  }
  html_dir <- html_dir[[1]]

  extract_dir <- tempfile("rlp-county-elections-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  workbook_name <- "Zeitreihen Kreiswahlen seit 2000.xlsx"
  extracted <- utils::unzip(zip_path, files = workbook_name, exdir = extract_dir)
  workbook <- file.path(extract_dir, workbook_name)
  if (length(extracted) != 1L || !file.exists(workbook)) {
    stop("Failed to extract ", workbook_name, " from ", zip_path, call. = FALSE)
  }

  workbook_results <- lapply(
    c(2004L, 2009L, 2014L, 2019L),
    function(year) rlp_parse_workbook_sheet(workbook, year)
  )
  result <- dplyr::bind_rows(
    rlp_parse_html_1999(html_dir),
    workbook_results,
    rlp_parse_pdf_2024(pdf_path)
  )
  duplicate_keys <- duplicated(result[c("ags", "election_year")])
  if (any(duplicate_keys)) {
    stop("RLP parser produced duplicate AGS-year rows", call. = FALSE)
  }
  expected_years <- c(1999L, 2004L, 2009L, 2014L, 2019L, 2024L)
  if (!identical(sort(unique(result$election_year)), expected_years)) {
    stop("RLP parser did not produce all expected years", call. = FALSE)
  }
  counts_by_year <- table(result$election_year)
  if (any(counts_by_year != 36L)) {
    stop(
      "RLP parser expected 36 units per year; got ",
      paste(names(counts_by_year), counts_by_year, sep = "=", collapse = ", "),
      call. = FALSE
    )
  }
  dplyr::arrange(result, election_year, ags)
}
