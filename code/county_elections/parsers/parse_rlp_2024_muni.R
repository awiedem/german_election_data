# Parser for municipal partial results of the 2024 Rhineland-Palatinate
# Kreistagswahlen.
#
# These are explicitly the municipality-level partial results of each county
# council contest (LK_KREISTAGSWAHL), not the separately held municipal-council
# elections (GD_GEMEINDERATSWAHL). The official portal stores one JSON document
# per Landkreis. Each document contains the county total, Verbandsgemeinde
# aggregates, and leaf results for Ortsgemeinden and verbandsfreie Gemeinden.

set.seed(20260730)

.rlp24_muni_require_packages <- function() {
  required <- c("dplyr", "jsonlite", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "RLP 2024 municipality parser requires missing package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.rlp24_muni_resolve_dir <- function(raw_dir) {
  candidates <- unique(c(
    raw_dir,
    file.path(raw_dir, "2024_municipality"),
    file.path(raw_dir, "Kreistagswahlen", "Rheinland-Pfalz", "2024_municipality")
  ))
  required <- c(
    "official_results_description.html",
    "wahlen-vec-tree.json",
    "wahlart.json",
    "parties.json"
  )
  hits <- candidates[
    dir.exists(candidates) &
      vapply(
        candidates,
        function(path) all(file.exists(file.path(path, required))),
        logical(1)
      )
  ]
  if (length(hits) != 1L) {
    stop(
      "Could not resolve exactly one RLP 2024 municipality source directory ",
      "below raw_dir: ", raw_dir,
      call. = FALSE
    )
  }
  hits[[1]]
}

.rlp24_muni_scalar <- function(x, label) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    stop("Missing or non-scalar ", label, call. = FALSE)
  }
  as.numeric(x)
}

.rlp24_muni_normalise_party <- function(name, is_waehlergruppe = FALSE) {
  if (isTRUE(is_waehlergruppe)) {
    return("waehlergruppen")
  }
  key <- tolower(trimws(as.character(name)))
  mapping <- c(
    "spd" = "spd",
    "cdu" = "cdu",
    "grüne" = "gruene",
    "afd" = "afd",
    "fdp" = "fdp",
    "freie wähler" = "freie_waehler",
    "die linke" = "linke_pds",
    "die partei" = "die_partei",
    "ödp" = "oedp",
    "volt" = "volt",
    "bsw" = "bsw"
  )
  if (key %in% names(mapping)) {
    return(unname(mapping[[key]]))
  }
  ascii <- iconv(key, from = "UTF-8", to = "ASCII//TRANSLIT")
  ascii <- gsub("[^a-z0-9]+", "_", ascii)
  gsub("^_+|_+$", "", ascii)
}

.rlp24_muni_catalog <- function(document, global_parties) {
  local_parties <- document$parties
  all_parties <- c(global_parties, local_parties)
  party_by_id <- setNames(all_parties, vapply(all_parties, `[[`, character(1), "id"))

  suggestions <- document$suggestions
  positions <- vapply(suggestions, function(x) as.integer(x$position), integer(1))
  suggestions <- suggestions[order(positions)]

  rows <- lapply(suggestions, function(suggestion) {
    party <- party_by_id[[as.character(suggestion$partyId)]]
    if (is.null(party)) {
      stop(
        "No party metadata for official party ID ", suggestion$partyId,
        " in ", document$electionId,
        call. = FALSE
      )
    }
    tibble::tibble(
      suggestion_id = as.integer(suggestion$id),
      position = as.integer(suggestion$position),
      official_party_id = as.character(suggestion$partyId),
      official_party_name = as.character(party$name),
      official_party_long_name = as.character(party$longName),
      party = .rlp24_muni_normalise_party(
        party$name,
        isTRUE(party$isWaehlergruppe)
      )
    )
  })
  catalog <- dplyr::bind_rows(rows)
  if (any(!nzchar(catalog$party)) || anyDuplicated(catalog$suggestion_id)) {
    stop("Invalid party catalog in ", document$electionId, call. = FALSE)
  }
  catalog
}

.rlp24_muni_geo_lookup <- function(tree) {
  lookup <- setNames(tree, vapply(tree, `[[`, character(1), "slug"))
  if (anyDuplicated(names(lookup))) {
    stop("Duplicate official geography IDs in wahlen-vec-tree.json", call. = FALSE)
  }
  lookup
}

.rlp24_muni_ags <- function(official_geo_id) {
  if (!grepl("^[0-9]{10}$", official_geo_id)) {
    stop("Invalid official municipality geography ID: ", official_geo_id, call. = FALSE)
  }
  paste0("07", substr(official_geo_id, 1L, 3L), substr(official_geo_id, 6L, 8L))
}

.rlp24_muni_result_row <- function(
    result,
    catalog,
    geo_lookup,
    county_geo_id,
    county_name,
    source_file) {
  geo_id <- as.character(result$geoId)
  geo <- geo_lookup[[geo_id]]
  if (is.null(geo) || !geo$geo_type %in% c("GD", "VF", "VG", "LK")) {
    stop("Unexpected result geography ", geo_id, " in ", source_file, call. = FALSE)
  }

  data_by_suggestion <- setNames(
    result$data,
    vapply(result$data, function(x) as.character(x$suggestionId), character(1))
  )
  if (anyDuplicated(names(data_by_suggestion))) {
    stop("Duplicate suggestion result for ", geo_id, " in ", source_file, call. = FALSE)
  }
  missing_suggestions <- setdiff(as.character(catalog$suggestion_id), names(data_by_suggestion))
  if (length(missing_suggestions) > 0L) {
    stop(
      "Missing party result(s) for ", geo_id, " in ", source_file, ": ",
      paste(missing_suggestions, collapse = ", "),
      call. = FALSE
    )
  }

  party_weighted <- vapply(
    as.character(catalog$suggestion_id),
    function(id) .rlp24_muni_scalar(
      data_by_suggestion[[id]]$weighted,
      paste("weighted party votes for", geo_id, id)
    ),
    numeric(1)
  )
  party_absolute <- vapply(
    as.character(catalog$suggestion_id),
    function(id) .rlp24_muni_scalar(
      data_by_suggestion[[id]]$absolute,
      paste("absolute party votes for", geo_id, id)
    ),
    numeric(1)
  )

  weighted_by_party <- tapply(party_weighted, catalog$party, sum)
  absolute_by_party <- tapply(party_absolute, catalog$party, sum)
  valid_papers <- .rlp24_muni_scalar(
    result$papers$valid$count, paste("valid ballots for", geo_id)
  )
  raw_valid_votes <- .rlp24_muni_scalar(
    result$votes$valid$count, paste("raw valid votes for", geo_id)
  )
  if (sum(weighted_by_party) != valid_papers) {
    stop(
      "Weighted party counts do not equal valid ballots for ", geo_id,
      " in ", source_file,
      call. = FALSE
    )
  }
  if (sum(absolute_by_party) != raw_valid_votes) {
    stop(
      "Raw party counts do not equal raw valid candidate votes for ", geo_id,
      " in ", source_file,
      call. = FALSE
    )
  }

  eligible <- .rlp24_muni_scalar(
    result$votes$total, paste("eligible voters for", geo_id)
  )
  voters <- .rlp24_muni_scalar(
    result$votes$count, paste("voters for", geo_id)
  )
  invalid_papers <- .rlp24_muni_scalar(
    result$papers$invalid$count, paste("invalid ballots for", geo_id)
  )
  if (valid_papers + invalid_papers != voters) {
    stop("Valid plus invalid ballots do not equal voters for ", geo_id, call. = FALSE)
  }

  immediate_parent <- as.character(geo$parent)
  parent_name <- if (!is.null(geo_lookup[[immediate_parent]])) {
    as.character(geo_lookup[[immediate_parent]]$name)
  } else {
    NA_character_
  }
  base <- tibble::tibble(
    ags = if (geo$geo_type %in% c("GD", "VF")) .rlp24_muni_ags(geo_id) else NA_character_,
    ags_name = as.character(geo$name),
    county = paste0("07", substr(county_geo_id, 1L, 3L)),
    county_name = county_name,
    state = "07",
    election_year = 2024L,
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid_papers,
    invalid_votes = invalid_papers,
    raw_valid_candidate_votes = raw_valid_votes,
    turnout = ifelse(eligible > 0, voters / eligible, NA_real_),
    result_level = ifelse(
      geo$geo_type == "LK",
      "county",
      ifelse(geo$geo_type == "VG", "association_aggregate", "municipality")
    ),
    contest_type = "kreistag",
    event_scope = "statewide",
    official_geo_id = geo_id,
    official_geo_type = as.character(geo$geo_type),
    official_parent_geo_id = immediate_parent,
    official_parent_name = parent_name,
    official_county_geo_id = county_geo_id,
    official_party_ids = paste(catalog$official_party_id, collapse = ";"),
    official_party_names = paste(catalog$official_party_name, collapse = ";"),
    source_file = source_file,
    source_url = paste0(
      "https://rlp-kw24.wahlen.23degrees.eu/assets/json/wahlen/",
      "LK_KREISTAGSWAHL/", county_geo_id, ".json"
    ),
    source_portal_url = "https://www.wahlen.rlp.de/ergebnisse-2024",
    source_election_type = "LK_KREISTAGSWAHL",
    source_granularity = "municipality_partial_of_county_contest",
    source_limitation = FALSE,
    source_note = paste(
      "Official municipal partial result of the Kreistagswahl, not the",
      "separate Gemeinde-/Stadtratswahl. Ballots and raw candidate votes",
      "aggregate exactly to Landkreis totals; ballot-equivalent party counts",
      "are independently rounded by the portal at each geographic level."
    ),
    municipality_coverage = 1,
    postal_vote_limitation = FALSE
  )
  for (party in names(weighted_by_party)) {
    count <- unname(weighted_by_party[[party]])
    base[[paste0("vote_count_", party)]] <- count
    base[[paste0("raw_vote_count_", party)]] <- unname(absolute_by_party[[party]])
    base[[party]] <- ifelse(valid_papers > 0, count / valid_papers, NA_real_)
  }
  base
}

.rlp24_muni_assert_aggregation <- function(county_row, municipality_rows, source_file) {
  additive <- c(
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
    "raw_valid_candidate_votes",
    grep("^raw_vote_count_", names(county_row), value = TRUE)
  )
  missing_in_municipalities <- setdiff(additive, names(municipality_rows))
  for (column in missing_in_municipalities) {
    municipality_rows[[column]] <- NA_real_
  }
  for (column in additive) {
    county_value <- county_row[[column]][[1]]
    municipality_value <- sum(municipality_rows[[column]], na.rm = TRUE)
    if (!isTRUE(all.equal(county_value, municipality_value, tolerance = 0))) {
      stop(
        "Municipal ", column, " sums to ", municipality_value,
        " but county total is ", county_value, " in ", source_file,
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

parse_rlp_2024_municipality <- function(raw_dir) {
  .rlp24_muni_require_packages()
  source_dir <- .rlp24_muni_resolve_dir(raw_dir)

  description <- paste(
    readLines(
      file.path(source_dir, "official_results_description.html"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  required_markers <- c(
    "rlp-kw24.wahlen.23degrees.eu",
    "Für die Wahlen auf Kreisebene werden auch Teilergebnisse",
    "Ortsgemeindeebene"
  )
  if (!all(vapply(required_markers, grepl, logical(1), x = description, fixed = TRUE))) {
    stop(
      "Official results description does not verify Kreis-level partial ",
      "results at Ortsgemeinde level.",
      call. = FALSE
    )
  }

  election_types <- jsonlite::read_json(
    file.path(source_dir, "wahlart.json"),
    simplifyVector = FALSE
  )
  kreistag_type <- Filter(
    function(x) identical(x$id, "LK_KREISTAGSWAHL"),
    election_types
  )
  if (
    length(kreistag_type) != 1L ||
      !identical(kreistag_type[[1]]$label, "Kreistagswahl") ||
      !identical(kreistag_type[[1]]$geoType, "LK")
  ) {
    stop("wahlart.json does not identify LK_KREISTAGSWAHL as Kreistagswahl", call. = FALSE)
  }

  tree <- jsonlite::read_json(
    file.path(source_dir, "wahlen-vec-tree.json"),
    simplifyVector = FALSE
  )
  geo_lookup <- .rlp24_muni_geo_lookup(tree)
  global_parties <- jsonlite::read_json(
    file.path(source_dir, "parties.json"),
    simplifyVector = FALSE
  )

  county_nodes <- Filter(
    function(x) {
      identical(x$geo_type, "LK") &&
        any(vapply(
          x$electionTypes,
          function(election) identical(election$id, "LK_KREISTAGSWAHL"),
          logical(1)
        ))
    },
    tree
  )
  county_ids <- sort(vapply(county_nodes, `[[`, character(1), "slug"))
  source_files <- list.files(
    source_dir,
    pattern = "^LK_KREISTAGSWAHL_[0-9]{10}[.]json$",
    full.names = TRUE
  )
  file_ids <- sub(
    "^LK_KREISTAGSWAHL_([0-9]{10})[.]json$",
    "\\1",
    basename(source_files)
  )
  if (
    length(county_ids) != 24L ||
      length(source_files) != 24L ||
      !identical(sort(file_ids), county_ids)
  ) {
    stop(
      "Expected one official JSON file for each of 24 RLP Landkreise.",
      call. = FALSE
    )
  }

  parsed_counties <- lapply(source_files, function(source_file) {
    document <- jsonlite::read_json(source_file, simplifyVector = FALSE)
    county_geo_id <- sub(
      "^LK_KREISTAGSWAHL_([0-9]{10})[.]json$",
      "\\1",
      basename(source_file)
    )
    if (
      !identical(document$electionType, "LK_KREISTAGSWAHL") ||
        !identical(document$rootGeoId, county_geo_id) ||
        !identical(substr(document$date, 1L, 10L), "2024-06-09")
    ) {
      stop(
        "Unexpected election identity in ", basename(source_file),
        "; expected the 2024 LK_KREISTAGSWAHL.",
        call. = FALSE
      )
    }
    catalog <- .rlp24_muni_catalog(document, global_parties)
    result_rows <- lapply(
      document$results,
      .rlp24_muni_result_row,
      catalog = catalog,
      geo_lookup = geo_lookup,
      county_geo_id = county_geo_id,
      county_name = as.character(geo_lookup[[county_geo_id]]$name),
      source_file = basename(source_file)
    )
    rows <- dplyr::bind_rows(result_rows)
    county_row <- rows[rows$official_geo_id == county_geo_id, , drop = FALSE]
    municipality_rows <- rows[
      rows$official_geo_type %in% c("GD", "VF"),
      ,
      drop = FALSE
    ]
    expected_leaf_ids <- vapply(
      Filter(
        function(x) {
          x$geo_type %in% c("GD", "VF") &&
            substr(x$slug, 1L, 3L) == substr(county_geo_id, 1L, 3L)
        },
        tree
      ),
      `[[`,
      character(1),
      "slug"
    )
    if (
      nrow(county_row) != 1L ||
        !setequal(municipality_rows$official_geo_id, expected_leaf_ids)
    ) {
      stop(
        "Municipality coverage in ", basename(source_file),
        " does not match the official geography tree.",
        call. = FALSE
      )
    }
    .rlp24_muni_assert_aggregation(
      county_row,
      municipality_rows,
      basename(source_file)
    )
    municipality_rows
  })

  result <- dplyr::bind_rows(parsed_counties)
  if (anyDuplicated(result[c("ags", "election_year")])) {
    stop("Duplicate municipality-year keys in RLP 2024 results", call. = FALSE)
  }
  if (nrow(result) != 2289L) {
    stop("Expected 2,289 municipality rows; found ", nrow(result), call. = FALSE)
  }
  result
}
