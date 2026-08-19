# Feasibility guard for the official Schleswig-Holstein 1990 and 1994 PDFs.
#
# Both publications contain municipality-level Kreiswahl results, but their
# embedded OCR layers are not reliable enough to use as election data without
# a municipality-code crosswalk and source-image verification. This module
# deliberately diagnoses that problem and stops instead of returning guessed
# values. It is an integration scaffold for a later OCR/crosswalk pass.

.shhist_require_packages <- function() {
  required <- c("pdftools", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "SH historical PDF feasibility check requires missing package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.shhist_pdf_names <- c(
  `1990` = "Schleswig-Holstein_1990_Kommunalwahl_Endgueltiges_Ergebnis.pdf",
  `1994` = "Schleswig-Holstein_1994_Kommunalwahl_Endgueltiges_Ergebnis.pdf"
)

.shhist_is_pdf <- function(path) {
  file.exists(path) &&
    file.info(path)$size >= 5L &&
    identical(readChar(path, nchars = 4L, useBytes = TRUE), "%PDF")
}

.shhist_resolve_pdf <- function(raw_dir, year) {
  filename <- unname(.shhist_pdf_names[[as.character(year)]])
  candidates <- unique(c(
    raw_dir,
    file.path(raw_dir, filename),
    file.path(raw_dir, "Schleswig-Holstein", filename),
    file.path(raw_dir, "Kreistagswahlen", "Schleswig-Holstein", filename)
  ))
  candidates <- candidates[basename(candidates) == filename]
  hits <- candidates[vapply(candidates, .shhist_is_pdf, logical(1))]
  if (length(hits) != 1L) {
    stop(
      "Could not resolve exactly one hydrated official SH ", year,
      " PDF below raw_dir: ", raw_dir,
      call. = FALSE
    )
  }
  hits[[1]]
}

.shhist_normalize_digits <- function(x, width = NULL) {
  x <- toupper(x)
  # Substitutions observed repeatedly in the embedded OCR layer. They are safe
  # only for diagnosing code-shaped tokens, not for recovering vote counts.
  x <- chartr("OICLZSBG", "01012586", x)
  x <- gsub("[^0-9]", "", x)
  if (!is.null(width) && nchar(x) != width) {
    return(NA_character_)
  }
  x
}

.shhist_candidate_keys <- function(pages) {
  allowed_counties <- c(sprintf("%02d", 1:4), as.character(51:62))
  keys <- character()

  for (page_number in seq(30L, 192L, by = 2L)) {
    page <- pages[[page_number]]
    candidates <- page[page$x < 110 & page$y > 100, , drop = FALSE]
    seen_on_page <- character()

    for (i in seq_len(nrow(candidates))) {
      county <- .shhist_normalize_digits(candidates$text[[i]], 2L)
      if (is.na(county) || !county %in% allowed_counties) {
        next
      }
      same_line <- page[
        abs(page$y - candidates$y[[i]]) <= 3 &
          page$x > candidates$x[[i]] + 8 &
          page$x < candidates$x[[i]] + 25,
        ,
        drop = FALSE
      ]
      municipalities <- unique(stats::na.omit(vapply(
        same_line$text,
        .shhist_normalize_digits,
        character(1),
        width = 3L
      )))
      if (length(municipalities) != 1L) {
        next
      }
      key <- paste0(county, municipalities)
      if (!key %in% seen_on_page) {
        keys <- c(keys, key)
        seen_on_page <- c(seen_on_page, key)
      }
    }
  }
  keys
}

.shhist_recognizable_vote_rows <- function(pages) {
  sum(vapply(seq(31L, 193L, by = 2L), function(page_number) {
    text <- toupper(pages[[page_number]]$text)
    text <- chartr("OIL", "011", text)
    text <- gsub("[^0-9]", "", text)
    sum(text == "1000")
  }, integer(1)))
}

#' Diagnose whether the SH 1990/1994 PDF OCR can be parsed safely
#'
#' @param raw_dir The Schleswig-Holstein raw directory, project-wide
#'   `Kreistagswahlen` directory, or (for a single-year call) a PDF path.
#' @return One diagnostic row per year. `safe_to_parse` is deliberately false
#'   until a verified municipality crosswalk and image-level correction table
#'   are supplied.
assess_sh_1990_1994_pdf_feasibility <- function(raw_dir) {
  .shhist_require_packages()
  diagnostics <- lapply(c(1990L, 1994L), function(year) {
    pdf <- .shhist_resolve_pdf(raw_dir, year)
    pages <- pdftools::pdf_data(pdf)
    if (length(pages) != 194L) {
      stop("Expected the official SH ", year, " PDF to contain 194 pages.",
           call. = FALSE)
    }

    keys <- .shhist_candidate_keys(pages)
    unique_keys <- unique(keys)
    recognizable_vote_rows <- .shhist_recognizable_vote_rows(pages)
    tibble::tibble(
      election_year = year,
      pdf_pages = length(pages),
      turnout_table_pages = length(seq(30L, 192L, by = 2L)),
      vote_table_pages = length(seq(31L, 193L, by = 2L)),
      candidate_code_rows = length(keys),
      unique_candidate_keys = length(unique_keys),
      duplicate_candidate_keys = sum(duplicated(keys)),
      recognizable_vote_100_percent_rows = recognizable_vote_rows,
      safe_to_parse = FALSE,
      source_limitation = TRUE,
      source_note = paste(
        "Official municipality-level scan with embedded OCR.",
        "The OCR confuses digits and letters in AGS fragments and vote counts;",
        "candidate keys are incomplete and duplicated.",
        "Image-level verification plus a historical municipality-code",
        "crosswalk is required before values can enter the election panel."
      )
    )
  })
  do.call(rbind, diagnostics)
}

#' Parse the SH 1990 and 1994 municipality-level Kreiswahl results
#'
#' This function currently refuses to emit observations. Call
#' `assess_sh_1990_1994_pdf_feasibility()` for quantified extraction evidence.
parse_sh_1990_1994_municipality_elections <- function(raw_dir) {
  diagnostics <- assess_sh_1990_1994_pdf_feasibility(raw_dir)
  detail <- paste0(
    diagnostics$election_year, ": ",
    diagnostics$unique_candidate_keys, " unique candidate keys from ",
    diagnostics$candidate_code_rows, " code-shaped rows (",
    diagnostics$duplicate_candidate_keys, " duplicates); ",
    diagnostics$recognizable_vote_100_percent_rows,
    " recognizable vote-row markers"
  )
  stop(
    "Unsafe SH 1990/1994 OCR extraction; no observations returned. ",
    paste(detail, collapse = "; "),
    ". Build and verify a historical municipality crosswalk and an explicit ",
    "image-checked OCR correction table before enabling this parser.",
    call. = FALSE
  )
}
