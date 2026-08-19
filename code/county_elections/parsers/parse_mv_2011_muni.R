# Municipality-level Mecklenburg-Vorpommern Kreistag results for 2011.
#
# The official workbook reports municipality rows without the postal votes of
# municipalities belonging to an Amt. Those postal votes appear in 78 separate
# Amt-level pools. This parser returns the exact published municipality rows
# and retains the pools separately. It never allocates pooled votes.

set.seed(20260730)

.mv2011_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "x", "X", "-", ".", "...", "/")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

.mv2011_ags <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x <- gsub("[^0-9]", "", x)
  ifelse(nchar(x) > 0L, sprintf("%08d", as.integer(x)), NA_character_)
}

.mv2011_party_name <- function(x) {
  x <- trimws(tolower(x))
  x <- gsub("\u00e4", "ae", x, fixed = TRUE)
  x <- gsub("\u00f6", "oe", x, fixed = TRUE)
  x <- gsub("\u00fc", "ue", x, fixed = TRUE)
  x <- gsub("\u00df", "ss", x, fixed = TRUE)
  x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
  x <- gsub("[[:space:]]+", " ", x)
  mapping <- c(
    "die linke" = "linke_pds",
    "grune" = "gruene",
    "freie wahler" = "freie_waehler",
    "einzelbewerber" = "einzelbewerber"
  )
  mapped <- unname(mapping[x])
  fallback <- gsub("[^a-z0-9]+", "_", x)
  fallback <- gsub("^_+|_+$", "", fallback)
  ifelse(!is.na(mapped), mapped, fallback)
}

.mv2011_resolve_path <- function(raw_dir) {
  filename <- "Mecklenburg-Vorpommern_2011_Kreistagswahl.xls"
  candidate <- if (basename(raw_dir) == filename) {
    raw_dir
  } else if (basename(raw_dir) == "Mecklenburg-Vorpommern") {
    file.path(raw_dir, filename)
  } else if (basename(raw_dir) == "Kreistagswahlen") {
    file.path(raw_dir, "Mecklenburg-Vorpommern", filename)
  } else {
    file.path(
      raw_dir, "Kreistagswahlen", "Mecklenburg-Vorpommern", filename
    )
  }
  if (!file.exists(candidate)) {
    stop(
      "Could not resolve the MV 2011 workbook below raw_dir: ",
      raw_dir,
      call. = FALSE
    )
  }
  first_line <- readLines(candidate, n = 1L, warn = FALSE)
  if (length(first_line) &&
      identical(first_line, "version https://git-lfs.github.com/spec/v1")) {
    stop("MV 2011 workbook is an unhydrated Git LFS pointer.", call. = FALSE)
  }
  candidate
}

.mv2011_party_headers <- function(raw, positions) {
  vapply(positions, function(position) {
    pieces <- trimws(vapply(
      7:8,
      function(row) as.character(raw[[position]][row]),
      character(1L)
    ))
    pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
    paste(pieces, collapse = " ")
  }, character(1L))
}

.mv2011_combine_party_counts <- function(data, positions, labels) {
  party_names <- .mv2011_party_name(labels)
  if (any(is.na(party_names) | !nzchar(party_names))) {
    stop("MV 2011 has an empty normalized party name.", call. = FALSE)
  }
  unique_names <- unique(party_names)
  out <- vector("list", length(unique_names))
  names(out) <- paste0("vote_count_", unique_names)

  for (index in seq_along(unique_names)) {
    party <- unique_names[[index]]
    source_positions <- positions[party_names == party]
    values <- as.data.frame(
      lapply(source_positions, function(position) {
        .mv2011_number(data[[position]])
      }),
      check.names = FALSE
    )
    all_missing <- apply(is.na(values), 1L, all)
    combined <- rowSums(values, na.rm = TRUE)
    combined[all_missing] <- NA_real_
    out[[index]] <- combined
  }
  as.data.frame(out, check.names = FALSE)
}

.mv2011_read_counts <- function(raw_dir) {
  if (!requireNamespace("readxl", quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("MV 2011 parser requires readxl and tibble.", call. = FALSE)
  }

  path <- .mv2011_resolve_path(raw_dir)
  raw <- suppressMessages(
    readxl::read_excel(
      path,
      sheet = "gem",
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) != 891L || ncol(raw) != 43L) {
    stop(
      "Unexpected MV 2011 workbook dimensions: ",
      nrow(raw), " rows x ", ncol(raw), " columns.",
      call. = FALSE
    )
  }

  source_rows <- 10:nrow(raw)
  ags <- .mv2011_ags(raw[[4L]][source_rows])
  keep <- grepl("^13[0-9]{6}$", ags)
  data <- raw[source_rows[keep], , drop = FALSE]
  ags <- ags[keep]
  if (length(ags) != 882L || anyDuplicated(ags)) {
    stop("Expected 882 unique coded rows in the MV 2011 workbook.", call. = FALSE)
  }

  party_positions <- 14:43
  party_labels <- .mv2011_party_headers(raw, party_positions)
  nonempty <- nzchar(party_labels)
  party_positions <- party_positions[nonempty]
  party_labels <- party_labels[nonempty]
  party_counts <- .mv2011_combine_party_counts(
    data,
    party_positions,
    party_labels
  )

  counts <- data.frame(
    ags = ags,
    ags_name = trimws(as.character(data[[5L]])),
    county = substr(ags, 1L, 5L),
    state = "13",
    election_year = 2011L,
    wahlkreis = trimws(as.character(data[[1L]])),
    wahlbereich = trimws(as.character(data[[2L]])),
    administrative_office_code = trimws(as.character(data[[3L]])),
    eligible_voters = .mv2011_number(data[[9L]]),
    number_voters = .mv2011_number(data[[10L]]),
    invalid_votes = .mv2011_number(data[[12L]]),
    valid_votes = .mv2011_number(data[[13L]]),
    party_counts,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  vote_cols <- grep("^vote_count_", names(counts), value = TRUE)
  is_postal_pool <- counts$eligible_voters == 0 &
    grepl("^Briefwahl ", counts$ags_name)

  if (sum(is_postal_pool) != 78L ||
      sum(!is_postal_pool) != 804L ||
      any(counts$eligible_voters[!is_postal_pool] <= 0)) {
    stop(
      "MV 2011 municipality/postal-pool classification differs from source expectations.",
      call. = FALSE
    )
  }
  if (anyNA(counts[c(
    "ags", "ags_name", "county", "eligible_voters", "number_voters",
    "invalid_votes", "valid_votes"
  )])) {
    stop("MV 2011 has missing values in required count columns.", call. = FALSE)
  }
  party_sums <- rowSums(counts[vote_cols], na.rm = TRUE)
  if (any(abs(party_sums - counts$valid_votes) > 1e-7)) {
    stop("MV 2011 party counts do not sum to valid votes.", call. = FALSE)
  }
  if (any(counts$valid_votes + counts$invalid_votes >
          3 * counts$number_voters)) {
    stop(
      "MV 2011 reports more valid plus invalid candidate votes than three per voter.",
      call. = FALSE
    )
  }

  municipalities <- tibble::as_tibble(counts[!is_postal_pool, , drop = FALSE])
  postal_pools <- tibble::as_tibble(counts[is_postal_pool, , drop = FALSE])
  list(
    municipalities = municipalities,
    postal_pools = postal_pools,
    vote_cols = vote_cols,
    path = path
  )
}

.mv2011_aggregate_count_rows <- function(rows, vote_cols) {
  sum_cols <- c(
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes",
    vote_cols
  )
  groups <- split(seq_len(nrow(rows)), rows$county)
  result <- lapply(groups, function(index) {
    values <- vapply(
      sum_cols,
      function(column) {
        selected <- rows[[column]][index]
        if (all(is.na(selected))) NA_real_ else sum(selected, na.rm = TRUE)
      },
      numeric(1L)
    )
    data.frame(
      county = rows$county[index[[1L]]],
      as.data.frame(as.list(values), check.names = FALSE),
      check.names = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, result))
}

#' Return the 78 unallocated Amt-level postal-vote pools
#'
#' @param raw_dir MV raw directory, project-wide Kreistagswahlen directory, or
#'   the 2011 workbook.
#' @return A tibble of published counts. `ags` is an artificial pool identifier
#'   from the source and must not be treated as a municipality AGS.
parse_mv_2011_postal_pools <- function(raw_dir) {
  parsed <- .mv2011_read_counts(raw_dir)
  pools <- parsed$postal_pools
  pools$result_level <- "administrative_office_postal_pool"
  pools$source_limitation <- TRUE
  pools$source_note <- paste(
    "Official Amt-level postal-vote pool.",
    "The source does not allocate these votes to individual municipalities;",
    "do not treat the pool identifier as a municipality AGS or impute its votes."
  )
  pools
}

#' Parse exact published municipality rows for the 2011 MV Kreistag election
#'
#' @param raw_dir MV raw directory, project-wide Kreistagswahlen directory, or
#'   the 2011 workbook.
#' @return A standardized municipality-result tibble. Party columns are shares
#'   of valid candidate votes. The omitted postal pools, exact municipality
#'   counts, coverage diagnostics, and county reconciliation are attached as
#'   attributes for audit.
parse_mv_2011_municipality_election <- function(raw_dir) {
  parsed <- .mv2011_read_counts(raw_dir)
  municipalities <- parsed$municipalities
  postal_pools <- parsed$postal_pools
  vote_cols <- parsed$vote_cols
  party_cols <- sub("^vote_count_", "", vote_cols)

  result <- municipalities[c(
    "ags", "ags_name", "county", "state", "election_year",
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
  )]
  result$turnout <- ifelse(
    result$eligible_voters > 0,
    result$number_voters / result$eligible_voters,
    NA_real_
  )
  result$result_level <- "municipality"
  result$contest_type <- "kreistag"
  result$event_scope <- "split_reform"
  result$source_limitation <- TRUE
  result$source_note <- paste(
    "Exact published municipality row.",
    "For municipalities belonging to an Amt, postal votes are reported only",
    "in 78 separate Amt-level pools and are not allocated or imputed here."
  )
  for (index in seq_along(vote_cols)) {
    result[[party_cols[[index]]]] <- ifelse(
      result$valid_votes > 0,
      municipalities[[vote_cols[[index]]]] / result$valid_votes,
      NA_real_
    )
  }

  leading <- c(
    "ags", "ags_name", "county", "state", "election_year",
    "result_level", "contest_type", "event_scope",
    "eligible_voters", "number_voters", "turnout",
    "invalid_votes", "valid_votes", "source_limitation", "source_note"
  )
  result <- tibble::as_tibble(result[c(leading, party_cols)])
  if (anyDuplicated(paste(result$ags, result$election_year, sep = "-"))) {
    stop("MV 2011 municipality result has duplicate AGS x year rows.", call. = FALSE)
  }

  all_counts <- rbind(municipalities, postal_pools)
  county_reconciliation <- .mv2011_aggregate_count_rows(all_counts, vote_cols)
  expected_county_valid_votes <- c(
    "13071" = 332859,
    "13072" = 275574,
    "13073" = 270311,
    "13074" = 197233,
    "13075" = 299119,
    "13076" = 286104
  )
  expected <- unname(expected_county_valid_votes[county_reconciliation$county])
  if (nrow(county_reconciliation) != 6L ||
      any(county_reconciliation$valid_votes != expected)) {
    stop("MV 2011 rows do not reconcile to the six published county totals.",
         call. = FALSE)
  }

  municipality_valid <- sum(municipalities$valid_votes)
  postal_valid <- sum(postal_pools$valid_votes)
  diagnostics <- tibble::tibble(
    source_rows = nrow(all_counts),
    municipality_rows = nrow(municipalities),
    postal_pool_rows = nrow(postal_pools),
    excluded_row_share = nrow(postal_pools) / nrow(all_counts),
    municipality_valid_votes = municipality_valid,
    unallocated_postal_valid_votes = postal_valid,
    municipality_valid_vote_share = municipality_valid /
      (municipality_valid + postal_valid),
    source_limitation = TRUE,
    source_note = paste(
      "The municipality result excludes 78 Amt-level postal pools.",
      "The pools are retained separately and no votes are allocated or imputed."
    )
  )

  attr(result, "postal_pools") <- parse_mv_2011_postal_pools(parsed$path)
  attr(result, "municipality_vote_counts") <- municipalities
  attr(result, "county_reconciliation") <- county_reconciliation
  attr(result, "coverage_diagnostics") <- diagnostics
  result
}
