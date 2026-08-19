# Brandenburg 1993/1998 scanned-result extraction scaffold
#
# The historical Brandenburg sources are image-only PDFs. The compact summary
# table on publication pages 6-21 of the first 1998 volume reports both the
# 1998 result and 1993 comparison figures for all counties and county-equivalent
# cities. This module renders and OCRs that bounded table. It intentionally does
# not convert OCR tokens into election data: spot checks found plausible-looking
# digit substitutions, so unvalidated OCR must not enter the final panel.

.bb_ocr_is_lfs_pointer <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  marker <- charToRaw("version https://git-lfs.github.com/spec/v1")
  prefix <- readBin(con, what = "raw", n = length(marker))
  length(prefix) == length(marker) && identical(prefix, marker)
}

.bb_ocr_require_command <- function(command) {
  located <- Sys.which(command)
  if (!nzchar(located)) {
    stop("Required Brandenburg OCR command is unavailable: ", command,
         call. = FALSE)
  }
  unname(located)
}

.bb_ocr_require_pdf <- function(path) {
  if (!file.exists(path)) {
    stop("Required Brandenburg scan is missing: ", path, call. = FALSE)
  }
  if (.bb_ocr_is_lfs_pointer(path)) {
    stop(
      "Brandenburg scan is an unhydrated Git LFS pointer: ",
      path,
      call. = FALSE
    )
  }
  if (file.info(path)$size < 1000) {
    stop("Brandenburg scan is unexpectedly small: ", path, call. = FALSE)
  }
  invisible(path)
}

.bb_ocr_pdf_pages <- function(path) {
  pdfinfo <- .bb_ocr_require_command("pdfinfo")
  info <- system2(pdfinfo, path, stdout = TRUE, stderr = TRUE)
  status <- attr(info, "status")
  if (!is.null(status) && status != 0L) {
    stop("pdfinfo failed for Brandenburg scan: ", path, call. = FALSE)
  }
  page_line <- grep("^Pages:", info, value = TRUE)
  if (length(page_line) != 1L) {
    stop("Could not determine page count for Brandenburg scan: ", path,
         call. = FALSE)
  }
  as.integer(sub("^Pages:\\s*", "", page_line))
}

bb_ocr_source_inventory <- function(raw_dir) {
  files <- c(
    "Brandenburg_1993_KTW.pdf",
    "Brandenburg_1998_KTW_1.pdf",
    "Brandenburg_1998_KTW_2.pdf",
    "Brandenburg_1998_KTW_3.pdf"
  )
  paths <- file.path(raw_dir, files)
  missing <- !file.exists(paths)
  pointers <- vapply(paths, .bb_ocr_is_lfs_pointer, logical(1))
  pages <- rep(NA_integer_, length(paths))
  hydrated <- !missing & !pointers
  pages[hydrated] <- vapply(paths[hydrated], .bb_ocr_pdf_pages, integer(1))

  data.frame(
    file = files,
    year = c(1993L, 1998L, 1998L, 1998L),
    part = c(1L, 1L, 2L, 3L),
    missing = missing,
    lfs_pointer = pointers,
    pages = pages,
    stringsAsFactors = FALSE
  )
}

bb_extract_summary_ocr <- function(
    source_pdf,
    output_dir,
    pages = 6:21,
    dpi = 300L,
    language = "deu") {
  .bb_ocr_require_pdf(source_pdf)
  pdftoppm <- .bb_ocr_require_command("pdftoppm")
  tesseract <- .bb_ocr_require_command("tesseract")

  total_pages <- .bb_ocr_pdf_pages(source_pdf)
  pages <- sort(unique(as.integer(pages)))
  if (length(pages) == 0L || anyNA(pages) ||
      any(pages < 1L | pages > total_pages)) {
    stop("Requested Brandenburg OCR pages are outside the PDF", call. = FALSE)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  records <- vector("list", length(pages))
  for (i in seq_along(pages)) {
    page <- pages[[i]]
    stem <- file.path(output_dir, sprintf("bb_summary_p%03d", page))
    image_path <- paste0(stem, ".png")
    text_path <- paste0(stem, ".txt")
    tsv_path <- paste0(stem, ".tsv")

    render_status <- system2(
      pdftoppm,
      c(
        "-f", page, "-l", page, "-singlefile", "-png",
        "-r", as.integer(dpi), source_pdf, stem
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    render_code <- attr(render_status, "status")
    if (!is.null(render_code) && render_code != 0L) {
      stop("pdftoppm failed on Brandenburg publication page ", page,
           call. = FALSE)
    }

    ocr_status <- system2(
      tesseract,
      c(
        image_path, stem, "-l", language, "--psm", "6", "txt", "tsv"
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    ocr_code <- attr(ocr_status, "status")
    if (!is.null(ocr_code) && ocr_code != 0L) {
      stop("tesseract failed on Brandenburg publication page ", page,
           call. = FALSE)
    }
    if (!file.exists(text_path) || !file.exists(tsv_path)) {
      stop("Tesseract did not create both text and TSV outputs for page ",
           page, call. = FALSE)
    }

    text <- paste(readLines(text_path, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    tsv <- utils::read.delim(
      tsv_path,
      quote = "",
      stringsAsFactors = FALSE,
      na.strings = c("", "-1")
    )
    word_rows <- !is.na(tsv$text) & nzchar(trimws(tsv$text))
    confidence <- suppressWarnings(as.numeric(tsv$conf[word_rows]))
    confidence <- confidence[is.finite(confidence) & confidence >= 0]

    records[[i]] <- data.frame(
      page = page,
      image_path = image_path,
      text_path = text_path,
      tsv_path = tsv_path,
      word_count = sum(word_rows),
      median_confidence = if (length(confidence)) stats::median(confidence)
      else NA_real_,
      has_result_heading = grepl(
        "Ergebnisse der Wahl|Ergebnisse der Wahlen",
        text,
        ignore.case = TRUE
      ),
      has_eligible_voters = grepl(
        "Wahlberechtigte",
        text,
        ignore.case = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, records)
}

bb_assess_summary_ocr <- function(extraction_manifest) {
  required <- c(
    "page", "word_count", "median_confidence",
    "has_result_heading", "has_eligible_voters"
  )
  absent <- setdiff(required, names(extraction_manifest))
  if (length(absent)) {
    stop("OCR manifest is missing columns: ", paste(absent, collapse = ", "),
         call. = FALSE)
  }

  expected_summary_pages <- 6:21
  # Odd-numbered pages continue the same ruled table and therefore repeat
  # values, but not the left-hand "Wahlberechtigte" row label.
  expected_count_label_pages <- c(6L, 14L)
  observed_pages <- sort(unique(extraction_manifest$page))
  covered <- all(expected_summary_pages %in% observed_pages)
  count_headers <- all(
    extraction_manifest$has_eligible_voters[
      match(expected_count_label_pages, extraction_manifest$page)
    ] %in% TRUE
  )
  confidence_ok <- all(
    extraction_manifest$median_confidence >= 75,
    na.rm = TRUE
  ) && !anyNA(extraction_manifest$median_confidence)

  data.frame(
    summary_pages_complete = covered,
    count_pages_detected = count_headers,
    median_confidence_at_least_75 = confidence_ok,
    ready_for_manual_transcription = covered && count_headers,
    ready_for_automatic_import = FALSE,
    blocker = paste(
      "OCR contains plausible digit substitutions and column shifts.",
      "Every count must be reconciled to printed vote shares and state totals",
      "before automatic import."
    ),
    stringsAsFactors = FALSE
  )
}

.bb_historical_expected_totals <- data.frame(
  election_year = c(1993L, 1998L),
  eligible_voters = c(1931789, 2036423),
  number_voters = c(1156918, 1586252),
  invalid_ballots = c(68415, 54004),
  valid_votes = c(3158750, 4381097),
  spd = c(1089649, 1707521),
  linke_pds = c(669359, 947319),
  cdu = c(649291, 938542),
  fdp = c(223939, 181529),
  gruene = c(132485, 181138),
  stringsAsFactors = FALSE
)

.bb_historical_expected_shares <- data.frame(
  election_year = c(1993L, 1998L),
  spd = c(34.50, 38.97),
  linke_pds = c(21.19, 21.62),
  cdu = c(20.56, 21.42),
  fdp = c(7.09, 4.14),
  gruene = c(4.19, 4.13),
  stringsAsFactors = FALSE
)

.bb_validate_historical_summary <- function(raw) {
  required <- c(
    "ags", "ags_name", "election_year", "eligible_voters",
    "number_voters", "invalid_ballots", "valid_votes", "spd",
    "linke_pds", "cdu", "fdp", "gruene", "other_lists",
    "source_pdf", "source_page", "source_rows",
    "transcription_status", "source_note"
  )
  absent <- setdiff(required, names(raw))
  if (length(absent)) {
    stop("Brandenburg historical summary is missing columns: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  if (nrow(raw) != 36L ||
      !identical(sort(unique(raw$election_year)), c(1993L, 1998L)) ||
      any(table(raw$election_year) != 18L)) {
    stop("Expected exactly 18 Brandenburg units in both 1993 and 1998",
         call. = FALSE)
  }
  expected_ags <- c(
    sprintf("120%02d", 51:54),
    sprintf("120%02d", 60:73)
  )
  if (!identical(sort(unique(raw$ags)), expected_ags)) {
    stop("Unexpected Brandenburg county/city AGS coverage", call. = FALSE)
  }
  if (anyDuplicated(raw[c("ags", "election_year")])) {
    stop("Duplicate Brandenburg county-year in historical summary",
         call. = FALSE)
  }

  numeric_cols <- c(
    "eligible_voters", "number_voters", "invalid_ballots", "valid_votes",
    "spd", "linke_pds", "cdu", "fdp", "gruene", "other_lists"
  )
  if (anyNA(raw[numeric_cols]) ||
      any(vapply(raw[numeric_cols], function(x) any(x < 0), logical(1)))) {
    stop("Missing or negative Brandenburg historical counts", call. = FALSE)
  }
  if (any(raw$number_voters > raw$eligible_voters) ||
      any(raw$invalid_ballots > raw$number_voters)) {
    stop("Invalid Brandenburg historical turnout/ballot relationship",
         call. = FALSE)
  }
  party_cols <- c("spd", "linke_pds", "cdu", "fdp", "gruene", "other_lists")
  if (any(rowSums(raw[party_cols]) != raw$valid_votes)) {
    stop("Brandenburg historical party counts do not sum to valid votes",
         call. = FALSE)
  }
  if (any(raw$valid_votes > 3 * (raw$number_voters - raw$invalid_ballots))) {
    stop("Brandenburg historical valid votes exceed three per valid ballot",
         call. = FALSE)
  }
  if (any(raw$transcription_status != "verified_summary")) {
    stop("Unresolved Brandenburg historical transcription cells remain",
         call. = FALSE)
  }

  observed <- stats::aggregate(
    raw[c(
      "eligible_voters", "number_voters", "invalid_ballots", "valid_votes",
      "spd", "linke_pds", "cdu", "fdp", "gruene"
    )],
    list(election_year = raw$election_year),
    sum
  )
  observed <- observed[order(observed$election_year), ]
  expected <- .bb_historical_expected_totals[
    order(.bb_historical_expected_totals$election_year),
  ]
  if (!isTRUE(all.equal(
    unname(as.matrix(observed)),
    unname(as.matrix(expected)),
    check.attributes = FALSE
  ))) {
    stop("Brandenburg historical unit counts do not match printed state totals",
         call. = FALSE)
  }

  share_cols <- c("spd", "linke_pds", "cdu", "fdp", "gruene")
  calculated <- observed[share_cols] / observed$valid_votes * 100
  calculated[] <- lapply(calculated, round, digits = 2L)
  printed <- .bb_historical_expected_shares[share_cols]
  if (!isTRUE(all.equal(
    unname(as.matrix(calculated)),
    unname(as.matrix(printed)),
    check.attributes = FALSE
  ))) {
    stop("Brandenburg historical counts do not reproduce printed state shares",
         call. = FALSE)
  }
  invisible(raw)
}

parse_bb_1993_1998_county_summary <- function(raw_dir) {
  path <- file.path(
    raw_dir, "derived", "brandenburg_county_summary_1993_1998.csv"
  )
  if (!file.exists(path)) {
    stop("Derived Brandenburg historical summary is missing: ", path,
         call. = FALSE)
  }
  raw <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = c(ags = "character"),
    check.names = FALSE
  )
  .bb_validate_historical_summary(raw)

  party_cols <- c("spd", "linke_pds", "cdu", "fdp", "gruene", "other_lists")
  out <- raw
  out$county <- out$ags
  out$ags <- paste0(out$county, "000")
  out$state <- "12"
  out$invalid_votes <- out$invalid_ballots
  out$turnout <- out$number_voters / out$eligible_voters
  for (party in party_cols) {
    out[[party]] <- out[[party]] / out$valid_votes
  }
  out$result_level <- "county"
  out$contest_type <- ifelse(
    out$county %in% sprintf("120%02d", 51:54),
    "kreisfreie_city_council",
    "kreistag"
  )
  out$event_scope <- "statewide"
  out$source_limitation <- FALSE
  out$source_note <- paste0(
    out$source_note,
    "; transcribed from ", out$source_pdf,
    " publication page ", out$source_page
  )

  leading <- c(
    "ags", "ags_name", "county", "election_year", "state",
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes",
    "turnout", "result_level", "contest_type", "event_scope",
    "source_limitation", "source_note"
  )
  # Row-level provenance remains in source_note; return only the standard
  # metadata and party-share columns expected by the combined pipeline.
  keep <- c(leading, party_cols)
  tibble::as_tibble(out[keep])
}
