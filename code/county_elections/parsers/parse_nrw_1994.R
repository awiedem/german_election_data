# Parser for the official NRW 1994 county and kreisfreie-city election tables.
#
# The 180-page source PDF is a scan with an embedded OCR text layer. The parser
# uses word coordinates, rather than plain extracted text, because the results
# table continues across facing pages and blank party cells carry information.
# It retains the eight parties printed on the left-hand result pages and
# aggregates all lists on the facing pages as `other`.

.nrw1994_require_packages <- function() {
  required <- c("dplyr", "pdftools", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "NRW 1994 parser requires missing package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.nrw1994_pdf_name <- "Nordrhein-Westfalen_1994_Kreistagswahl.pdf"

.nrw1994_is_pdf <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 5L) {
    return(FALSE)
  }
  identical(readChar(path, nchars = 4L, useBytes = TRUE), "%PDF")
}

.nrw1994_resolve_pdf <- function(raw_dir) {
  candidates <- unique(c(
    raw_dir,
    file.path(raw_dir, .nrw1994_pdf_name),
    file.path(raw_dir, "Nordrhein-Wetfalen", .nrw1994_pdf_name),
    file.path(
      raw_dir, "Kreistagswahlen", "Nordrhein-Wetfalen",
      .nrw1994_pdf_name
    )
  ))
  candidates <- candidates[basename(candidates) == .nrw1994_pdf_name]
  hits <- candidates[vapply(candidates, .nrw1994_is_pdf, logical(1))]

  if (length(hits) != 1L) {
    pointer_hits <- candidates[file.exists(candidates) & !vapply(
      candidates, .nrw1994_is_pdf, logical(1)
    )]
    pointer_note <- if (length(pointer_hits) > 0L) {
      paste0(
        " A non-PDF placeholder exists at ", pointer_hits[[1]],
        "; hydrate it with git lfs pull."
      )
    } else {
      ""
    }
    stop(
      "Could not resolve exactly one hydrated NRW 1994 PDF below raw_dir: ",
      raw_dir, ".", pointer_note,
      call. = FALSE
    )
  }
  hits[[1]]
}

.nrw1994_unit_lookup <- data.frame(
  lfd = seq_len(54L),
  ags = c(
    "05111000", "05112000", "05113000", "05114000", "05116000",
    "05117000", "05119000", "05120000", "05122000", "05124000",
    "05154000", "05158000", "05162000", "05166000", "05170000",
    "05313000", "05314000", "05315000", "05316000", "05354000",
    "05358000", "05362000", "05366000", "05370000", "05374000",
    "05378000", "05382000", "05512000", "05513000", "05515000",
    "05554000", "05558000", "05562000", "05566000", "05570000",
    "05711000", "05754000", "05758000", "05762000", "05766000",
    "05770000", "05774000", "05911000", "05913000", "05914000",
    "05915000", "05916000", "05954000", "05958000", "05962000",
    "05966000", "05970000", "05974000", "05978000"
  ),
  ags_name = c(
    "Düsseldorf, Stadt", "Duisburg, Stadt", "Essen, Stadt",
    "Krefeld, Stadt", "Mönchengladbach, Stadt", "Mülheim an der Ruhr, Stadt",
    "Oberhausen, Stadt", "Remscheid, Stadt", "Solingen, Stadt",
    "Wuppertal, Stadt", "Kleve", "Mettmann", "Neuss", "Viersen", "Wesel",
    "Aachen, Stadt", "Bonn, Stadt", "Köln, Stadt", "Leverkusen, Stadt",
    "Aachen", "Düren", "Erftkreis", "Euskirchen", "Heinsberg",
    "Oberbergischer Kreis", "Rheinisch-Bergischer Kreis", "Rhein-Sieg-Kreis",
    "Bottrop, Stadt", "Gelsenkirchen, Stadt", "Münster, Stadt", "Borken",
    "Coesfeld", "Recklinghausen", "Steinfurt", "Warendorf",
    "Bielefeld, Stadt", "Gütersloh", "Herford", "Höxter", "Lippe",
    "Minden-Lübbecke", "Paderborn", "Bochum, Stadt", "Dortmund, Stadt",
    "Hagen, Stadt", "Hamm, Stadt", "Herne, Stadt", "Ennepe-Ruhr-Kreis",
    "Hochsauerlandkreis", "Märkischer Kreis", "Olpe",
    "Siegen-Wittgenstein", "Soest", "Unna"
  ),
  contest_type = c(
    rep("kreisfreie_city_council", 10L),
    rep("kreistag", 5L),
    rep("kreisfreie_city_council", 4L),
    rep("kreistag", 8L),
    rep("kreisfreie_city_council", 3L),
    rep("kreistag", 5L),
    "kreisfreie_city_council",
    rep("kreistag", 6L),
    rep("kreisfreie_city_council", 5L),
    rep("kreistag", 7L)
  ),
  stringsAsFactors = FALSE
)

.nrw1994_number_at <- function(words, xmin, xmax) {
  values <- words$text[
    words$x >= xmin & words$x < xmax & grepl("^[0-9]+$", words$text)
  ]
  if (length(values) == 0L) {
    return(0)
  }
  as.numeric(paste0(values, collapse = ""))
}

.nrw1994_row_words <- function(page, y) {
  page[abs(page$y - y) <= 1L, , drop = FALSE]
}

.nrw1994_extract_lfd <- function(words) {
  values <- words$text[
    words$x >= 40 & words$x < 74 & grepl("^[0-9]+$", words$text)
  ]
  if (length(values) != 1L) {
    stop("Could not extract exactly one running unit number.", call. = FALSE)
  }
  as.integer(values[[1]])
}

.nrw1994_parse_turnout_page <- function(page) {
  row_y <- page$y[page$text == "Anzahl"]
  if (length(row_y) != 18L) {
    stop("Expected 18 turnout rows on an NRW 1994 table page.", call. = FALSE)
  }
  dplyr::bind_rows(lapply(row_y, function(y) {
    words <- .nrw1994_row_words(page, y)
    anchor <- words$x[words$text == "Anzahl"]
    if (length(anchor) != 1L) {
      stop("Could not locate the turnout row anchor.", call. = FALSE)
    }
    data.frame(
      lfd = .nrw1994_extract_lfd(words),
      eligible_voters = .nrw1994_number_at(
        words, anchor + 175, anchor + 225
      ),
      number_voters = .nrw1994_number_at(
        words, anchor + 225, anchor + 280
      ),
      invalid_votes = .nrw1994_number_at(
        words, anchor + 325, anchor + 375
      )
    )
  }))
}

.nrw1994_parse_vote_page <- function(page) {
  row_y <- page$y[page$text == "Anzahl"]
  if (length(row_y) != 18L) {
    stop("Expected 18 vote rows on an NRW 1994 table page.", call. = FALSE)
  }
  dplyr::bind_rows(lapply(row_y, function(y) {
    words <- .nrw1994_row_words(page, y)
    data.frame(
      lfd = .nrw1994_extract_lfd(words),
      valid_votes = .nrw1994_number_at(words, 240, 286),
      spd_count = .nrw1994_number_at(words, 286, 328),
      cdu_count = .nrw1994_number_at(words, 328, 370),
      gruene_count = .nrw1994_number_at(words, 370, 411),
      fdp_count = .nrw1994_number_at(words, 411, 451),
      rep_count = .nrw1994_number_at(words, 451, 478),
      oedp_count = .nrw1994_number_at(words, 478, 511),
      statt_partei_count = .nrw1994_number_at(words, 511, 539),
      deut_count = .nrw1994_number_at(words, 539, 580)
    )
  }))
}

.nrw1994_validate_totals <- function(result, count_data) {
  published <- c(
    eligible_voters = 12993928,
    number_voters = 10620423,
    invalid_votes = 161801,
    valid_votes = 10458622,
    spd_count = 4423907,
    cdu_count = 4217391,
    gruene_count = 1063195,
    fdp_count = 393937,
    rep_count = 61141,
    oedp_count = 13642,
    statt_partei_count = 13214,
    deut_count = 7001
  )
  observed <- c(
    colSums(result[c(
      "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
    )]),
    colSums(count_data[c(
      "spd_count", "cdu_count", "gruene_count", "fdp_count", "rep_count",
      "oedp_count", "statt_partei_count", "deut_count"
    )])
  )
  if (!identical(unname(observed), unname(published))) {
    mismatch <- names(published)[observed != published]
    stop(
      "NRW 1994 extracted counts disagree with published statewide totals: ",
      paste(mismatch, collapse = ", "),
      call. = FALSE
    )
  }
}

#' Parse final NRW 1994 county and county-equivalent election results
#'
#' @param raw_dir The NRW directory, the project-wide `Kreistagswahlen`
#'   directory, or the hydrated PDF itself.
#' @return One row per Kreis or kreisfreie city in the standard unharmonized
#'   vote schema. Party columns are shares of valid votes.
parse_nrw_1994_county_elections <- function(raw_dir) {
  .nrw1994_require_packages()
  pdf <- .nrw1994_resolve_pdf(raw_dir)
  pages <- pdftools::pdf_data(pdf)
  if (length(pages) != 180L) {
    stop("Expected the official NRW 1994 PDF to contain 180 pages.", call. = FALSE)
  }

  turnout <- dplyr::bind_rows(lapply(pages[25:27], .nrw1994_parse_turnout_page))
  counts <- dplyr::bind_rows(lapply(pages[c(28, 30, 32)], .nrw1994_parse_vote_page))
  if (!identical(sort(turnout$lfd), seq_len(54L)) ||
      !identical(sort(counts$lfd), seq_len(54L))) {
    stop("NRW 1994 tables do not contain running unit numbers 1 through 54.",
         call. = FALSE)
  }
  if (anyDuplicated(turnout$lfd) || anyDuplicated(counts$lfd)) {
    stop("NRW 1994 tables contain duplicate running unit numbers.", call. = FALSE)
  }

  # Kreis Heinsberg's total eligible-voter OCR reading drops 10,000. Its
  # printed components (149,612 + 19,014 + 1) give 168,627.
  if (!identical(turnout$eligible_voters[turnout$lfd == 24L], 158627)) {
    stop("Unexpected eligible-voter OCR reading for Kreis Heinsberg.",
         call. = FALSE)
  }
  turnout$eligible_voters[turnout$lfd == 24L] <- 168627

  # Three party counts in the OCR layer differ visibly from the printed table.
  # Check the exact OCR readings before applying these source-verified fixes.
  ocr_checks <- c(
    counts$spd_count[counts$lfd == 1L],
    counts$cdu_count[counts$lfd == 30L],
    counts$fdp_count[counts$lfd == 32L]
  )
  if (!identical(unname(ocr_checks), c(135705, 74957, 5155))) {
    stop("Unexpected party-count OCR readings in the NRW 1994 table.",
         call. = FALSE)
  }
  counts$spd_count[counts$lfd == 1L] <- 136705
  counts$cdu_count[counts$lfd == 30L] <- 74967
  counts$fdp_count[counts$lfd == 32L] <- 5355

  result <- .nrw1994_unit_lookup |>
    dplyr::left_join(turnout, by = "lfd") |>
    dplyr::left_join(counts, by = "lfd")
  if (any(!stats::complete.cases(result[c(
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
  )]))) {
    stop("NRW 1994 joins lost turnout or vote-count rows.", call. = FALSE)
  }

  # The OCR layer reads Münster's printed 169 950 as 159 950. The same
  # publication reports 171 724 voters and 1 774 invalid ballots, which fixes
  # the valid-vote count exactly. Refuse any different or additional OCR
  # discrepancy so this correction cannot silently spread to another table.
  result$valid_votes_ocr <- result$valid_votes
  valid_from_ballot_identity <- result$number_voters - result$invalid_votes
  discrepancy <- which(result$valid_votes_ocr != valid_from_ballot_identity)
  if (!identical(discrepancy, 30L) ||
      result$valid_votes_ocr[[30L]] != 159950 ||
      valid_from_ballot_identity[[30L]] != 169950) {
    stop("Unexpected valid-vote OCR discrepancy in the NRW 1994 table.",
         call. = FALSE)
  }
  result$valid_votes <- valid_from_ballot_identity

  count_columns <- grep("_count$", names(counts), value = TRUE)
  result$other_count <- result$valid_votes - rowSums(result[count_columns])
  if (any(result$other_count < 0)) {
    stop("Named party counts exceed valid votes in an NRW 1994 unit.",
         call. = FALSE)
  }

  .nrw1994_validate_totals(result, result)

  result$county <- substr(result$ags, 1L, 5L)
  result$state <- "05"
  result$election_year <- 1994L
  result$turnout <- result$number_voters / result$eligible_voters
  result$result_level <- "county"
  result$event_scope <- "statewide"
  result$source_limitation <- TRUE
  result$source_note <- paste(
    "Official scan has a usable OCR layer.",
    "Five OCR readings were checked against the printed page and corrected:",
    "Kreis Heinsberg eligible voters 168,627;",
    "Düsseldorf SPD 136,705; Münster valid votes 169,950 and CDU 74,967;",
    "Coesfeld FDP 5,355.",
    "SPD, CDU, GRÜNE, FDP, REP, ÖDP, STATT Partei and Deutsche Liga are",
    "retained separately; all remaining parties, voter groups and individual",
    "candidates are aggregated as other."
  )

  share_names <- sub("_count$", "", c(count_columns, "other_count"))
  for (i in seq_along(share_names)) {
    source_name <- c(count_columns, "other_count")[[i]]
    result[[share_names[[i]]]] <- result[[source_name]] / result$valid_votes
  }
  result <- result[, !grepl("_count$|_ocr$", names(result)), drop = FALSE]
  result$lfd <- NULL

  metadata <- c(
    "ags", "ags_name", "county", "state", "election_year",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
    "turnout", "result_level", "contest_type", "event_scope",
    "source_limitation", "source_note"
  )
  party_columns <- setdiff(names(result), metadata)
  if (any(abs(rowSums(result[party_columns]) - 1) > 1e-12)) {
    stop("NRW 1994 party shares do not sum to one.", call. = FALSE)
  }
  if (anyDuplicated(result[c("ags", "election_year")])) {
    stop("NRW 1994 parser produced duplicate AGS-year rows.", call. = FALSE)
  }

  tibble::as_tibble(result[c(metadata, party_columns)])
}
