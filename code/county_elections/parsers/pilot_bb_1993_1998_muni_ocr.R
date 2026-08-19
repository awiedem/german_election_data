# Brandenburg 1993/1998 municipality OCR pilot
#
# This is deliberately a bounded feasibility pilot, not a production parser.
# It extracts two Barnim municipalities (one rural, one urban) in each year,
# records every OCR discrepancy against the source image, and refuses to call
# the result suitable for unattended import.

set.seed(20260731)

.bb_pilot_is_lfs_pointer <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  marker <- charToRaw("version https://git-lfs.github.com/spec/v1")
  prefix <- readBin(con, what = "raw", n = length(marker))
  length(prefix) == length(marker) && identical(prefix, marker)
}

.bb_pilot_resolve_pdf <- function(pointer_path, repo_root = ".") {
  if (!file.exists(pointer_path)) {
    stop("Missing Brandenburg pilot source: ", pointer_path, call. = FALSE)
  }
  if (!.bb_pilot_is_lfs_pointer(pointer_path)) {
    return(normalizePath(pointer_path, mustWork = TRUE))
  }

  pointer <- readLines(pointer_path, n = 3L, warn = FALSE)
  oid_line <- grep("^oid sha256:", pointer, value = TRUE)
  if (length(oid_line) != 1L) {
    stop("Malformed Git LFS pointer: ", pointer_path, call. = FALSE)
  }
  oid <- sub("^oid sha256:", "", oid_line)
  object_path <- file.path(
    repo_root, ".git", "lfs", "objects",
    substr(oid, 1L, 2L), substr(oid, 3L, 4L), oid
  )
  if (!file.exists(object_path)) {
    stop("Local Git LFS object is unavailable for: ", pointer_path,
         call. = FALSE)
  }
  normalizePath(object_path, mustWork = TRUE)
}

.bb_pilot_require_command <- function(command) {
  path <- Sys.which(command)
  if (!nzchar(path)) {
    stop("Required OCR command is unavailable: ", command, call. = FALSE)
  }
  unname(path)
}

.bb_pilot_rows <- function() {
  data.frame(
    ags = c("12060004", "12060052", "12060004", "12060052"),
    ags_name = c(
      "Ahrensfelde", "Eberswalde, Stadt",
      "Ahrensfelde", "Eberswalde, Stadt"
    ),
    unit_type = c("rural", "urban", "rural", "urban"),
    county = "12060",
    election_year = c(1993L, 1993L, 1998L, 1998L),
    eligible_voters = c(1062L, 38872L, 2019L, 37309L),
    number_voters = c(527L, 18665L, 1611L, 24488L),
    invalid_ballots = c(53L, 826L, 45L, 847L),
    invalid_votes = c(193L, 3974L, NA_integer_, NA_integer_),
    valid_votes = c(1388L, 52021L, 4555L, 66863L),
    spd = c(489L, 24768L, 1686L, 28606L),
    linke_pds = c(295L, 11605L, 1421L, 18225L),
    cdu = c(358L, 6402L, 904L, 11243L),
    fdp = c(25L, 3534L, 155L, 2907L),
    gruene = c(0L, 0L, 224L, 3746L),
    bue_barnim = c(151L, 4906L, 0L, 0L),
    bv = c(53L, 806L, 133L, 2136L),
    ev = c(17L, 0L, 0L, 0L),
    eb = c(0L, 0L, 32L, 0L),
    source_pdf = c(
      "Brandenburg_1993_KTW.pdf", "Brandenburg_1993_KTW.pdf",
      "Brandenburg_1998_KTW_1.pdf", "Brandenburg_1998_KTW_1.pdf"
    ),
    source_pdf_pages = c("95;96", "97;98", "132;133", "136;137"),
    source_publication_pages = c("94;95", "96;97", "132;133", "136;137"),
    verification_status = "image_verified_pilot",
    stringsAsFactors = FALSE
  )
}

.bb_pilot_field_specs <- function() {
  records <- list()
  add <- function(year, ags, page, x_min, x_max, fields) {
    for (field in names(fields)) {
      item <- fields[[field]]
      records[[length(records) + 1L]] <<- data.frame(
        election_year = as.integer(year),
        ags = ags,
        source_pdf_page = as.integer(page),
        field = field,
        verified_value = as.integer(item[[2L]]),
        x_min = as.integer(x_min),
        x_max = as.integer(x_max),
        y_top = as.integer(item[[1L]]),
        stringsAsFactors = FALSE
      )
    }
  }

  add(1993L, "12060004", 95L, 1320L, 1430L, list(
    eligible_voters = c(684, 1062), number_voters = c(708, 527),
    invalid_ballots = c(763, 53), invalid_votes = c(789, 193),
    valid_votes = c(815, 1388), cdu = c(896, 358), fdp = c(1132, 25),
    linke_pds = c(1606, 295), spd = c(1687, 489),
    bue_barnim = c(1846, 151), bv = c(2554, 53)
  ))
  add(1993L, "12060004", 96L, 1360L, 1455L, list(ev = c(1795, 17)))
  add(1993L, "12060052", 97L, 2100L, 2240L, list(
    eligible_voters = c(750, 38872), number_voters = c(777, 18665),
    invalid_ballots = c(829, 826), invalid_votes = c(855, 3974),
    valid_votes = c(882, 52021), cdu = c(960, 6402),
    fdp = c(1199, 3534), linke_pds = c(1673, 11605),
    spd = c(1752, 24768), bue_barnim = c(1911, 4906),
    bv = c(2619, 806)
  ))
  add(1998L, "12060004", 132L, 1300L, 1400L, list(
    eligible_voters = c(495, 2019), number_voters = c(533, 1611),
    invalid_ballots = c(607, 45), valid_votes = c(680, 4555),
    spd = c(793, 1686), linke_pds = c(904, 1421), cdu = c(1009, 904),
    fdp = c(1125, 155), gruene = c(1237, 224), bv = c(1348, 133),
    eb = c(1903, 32)
  ))
  add(1998L, "12060052", 136L, 1040L, 1155L, list(
    eligible_voters = c(493, 37309), number_voters = c(530, 24488),
    invalid_ballots = c(599, 847), valid_votes = c(678, 66863),
    spd = c(789, 28606), linke_pds = c(901, 18225),
    cdu = c(1012, 11243), fdp = c(1122, 2907),
    gruene = c(1234, 3746), bv = c(1345, 2136)
  ))
  do.call(rbind, records)
}

.bb_pilot_extract_token <- function(tsv, x_min, x_max, y_top,
                                    y_tolerance = 12L) {
  text <- trimws(as.character(tsv$text))
  centre <- tsv$left + tsv$width / 2
  keep <- nzchar(text) &
    centre >= x_min & centre <= x_max &
    abs(tsv$top - y_top) <= y_tolerance
  hits <- tsv[keep, c("left", "text"), drop = FALSE]
  if (!nrow(hits)) {
    return(NA_character_)
  }
  paste(hits$text[order(hits$left)], collapse = " ")
}

.bb_pilot_render_and_ocr <- function(source_pdf, page, work_dir,
                                     dpi = 350L) {
  pdftoppm <- .bb_pilot_require_command("pdftoppm")
  tesseract <- .bb_pilot_require_command("tesseract")
  stem <- file.path(work_dir, sprintf("page_%03d", page))
  image_path <- paste0(stem, ".png")
  tsv_path <- paste0(stem, ".tsv")

  render <- system2(
    pdftoppm,
    c("-f", page, "-l", page, "-singlefile", "-png", "-r", dpi,
      source_pdf, stem),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(render, "status")
  if (!is.null(status) && status != 0L) {
    stop("pdftoppm failed for Brandenburg page ", page, call. = FALSE)
  }
  ocr <- system2(
    tesseract,
    c(image_path, stem, "-l", "deu", "--psm", "6", "tsv"),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(ocr, "status")
  if (!is.null(status) && status != 0L) {
    stop("tesseract failed for Brandenburg page ", page, call. = FALSE)
  }
  if (!file.exists(tsv_path)) {
    stop("Tesseract did not create TSV for Brandenburg page ", page,
         call. = FALSE)
  }
  utils::read.delim(
    tsv_path, quote = "", stringsAsFactors = FALSE,
    na.strings = character()
  )
}

.bb_pilot_validate_rows <- function(rows) {
  if (nrow(rows) != 4L || anyDuplicated(rows[c("ags", "election_year")])) {
    stop("Pilot must contain four unique municipality-year rows",
         call. = FALSE)
  }
  if (!identical(sort(unique(rows$unit_type)), c("rural", "urban")) ||
      !all(table(rows$election_year, rows$unit_type) == 1L)) {
    stop("Pilot must contain one rural and one urban unit per election",
         call. = FALSE)
  }

  valid_ballots <- rows$number_voters - rows$invalid_ballots
  if (any(valid_ballots <= 0L) || any(rows$valid_votes < valid_ballots) ||
      any(rows$valid_votes > 3L * valid_ballots)) {
    stop("Pilot violates the one-to-three valid votes per valid ballot bound",
         call. = FALSE)
  }
  party_fields <- c(
    "spd", "linke_pds", "cdu", "fdp", "gruene",
    "bue_barnim", "bv", "ev", "eb"
  )
  if (any(rowSums(rows[party_fields]) != rows$valid_votes)) {
    stop("Pilot party counts do not sum exactly to valid votes",
         call. = FALSE)
  }
  invisible(TRUE)
}

.bb_pilot_county_checks <- function(rows, raw_dir) {
  county_path <- file.path(
    raw_dir, "derived", "brandenburg_county_summary_1993_1998.csv"
  )
  if (!file.exists(county_path)) {
    stop("Existing exact Brandenburg county transcription is missing",
         call. = FALSE)
  }
  county <- utils::read.csv(
    county_path, stringsAsFactors = FALSE,
    colClasses = c(ags = "character")
  )
  county <- county[county$ags == "12060", , drop = FALSE]
  if (nrow(county) != 2L) {
    stop("Expected exactly two Barnim county checksum rows", call. = FALSE)
  }

  checks <- list()
  fields <- c(
    "eligible_voters", "number_voters", "invalid_ballots", "valid_votes",
    "spd", "linke_pds", "cdu", "fdp", "gruene"
  )
  for (year in c(1993L, 1998L)) {
    pilot <- rows[rows$election_year == year, , drop = FALSE]
    published <- county[county$election_year == year, , drop = FALSE]
    for (field in fields) {
      county_field <- if (field == "valid_votes") "valid_votes" else field
      checks[[length(checks) + 1L]] <- data.frame(
        election_year = year,
        county = "12060",
        field = field,
        pilot_subtotal = sum(pilot[[field]]),
        county_total = published[[county_field]],
        within_county_total = sum(pilot[[field]]) <= published[[county_field]],
        stringsAsFactors = FALSE
      )
    }
    other_subtotal <- sum(
      pilot$bue_barnim + pilot$bv + pilot$ev + pilot$eb
    )
    checks[[length(checks) + 1L]] <- data.frame(
      election_year = year,
      county = "12060",
      field = "other_lists",
      pilot_subtotal = other_subtotal,
      county_total = published$other_lists,
      within_county_total = other_subtotal <= published$other_lists,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, checks)
  if (!all(out$within_county_total)) {
    stop("Pilot subtotal exceeds an exact Barnim county checksum",
         call. = FALSE)
  }
  out
}

run_bb_1993_1998_municipality_ocr_pilot <- function(
    raw_dir,
    output_dir = file.path(raw_dir, "derived", "municipality_pilot"),
    repo_root = ".") {
  rows <- .bb_pilot_rows()
  .bb_pilot_validate_rows(rows)
  specs <- .bb_pilot_field_specs()

  source_paths <- setNames(
    vapply(unique(rows$source_pdf), function(file) {
      .bb_pilot_resolve_pdf(file.path(raw_dir, file), repo_root)
    }, character(1)),
    unique(rows$source_pdf)
  )
  work_dir <- tempfile("bb_municipality_pilot_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  tsv_cache <- list()
  audit <- specs
  audit$raw_ocr <- NA_character_
  for (i in seq_len(nrow(audit))) {
    row <- rows[
      rows$election_year == audit$election_year[[i]] &
        rows$ags == audit$ags[[i]],
      , drop = FALSE
    ]
    key <- paste(row$source_pdf[[1L]], audit$source_pdf_page[[i]], sep = ":")
    if (is.null(tsv_cache[[key]])) {
      tsv_cache[[key]] <- .bb_pilot_render_and_ocr(
        source_paths[[row$source_pdf[[1L]]]],
        audit$source_pdf_page[[i]],
        work_dir
      )
    }
    audit$raw_ocr[[i]] <- .bb_pilot_extract_token(
      tsv_cache[[key]], audit$x_min[[i]], audit$x_max[[i]],
      audit$y_top[[i]]
    )
  }
  audit$ocr_numeric <- suppressWarnings(as.integer(gsub(
    "[^0-9]", "", audit$raw_ocr
  )))
  audit$corrected <- is.na(audit$ocr_numeric) |
    audit$ocr_numeric != audit$verified_value
  audit$image_verified <- TRUE
  audit$verification_note <- ifelse(
    audit$corrected,
    "Corrected by direct inspection of the rendered source cell.",
    "OCR agrees with direct inspection of the rendered source cell."
  )

  corrections <- audit[audit$corrected, , drop = FALSE]
  county_checks <- .bb_pilot_county_checks(rows, raw_dir)
  feasibility <- data.frame(
    pilot_rows = nrow(rows),
    numeric_fields_checked = nrow(audit),
    corrected_fields = nrow(corrections),
    correction_rate = nrow(corrections) / nrow(audit),
    exact_party_sum_rows = sum(rowSums(rows[c(
      "spd", "linke_pds", "cdu", "fdp", "gruene",
      "bue_barnim", "bv", "ev", "eb"
    )]) == rows$valid_votes),
    ballot_bound_rows = sum(
      rows$valid_votes >= rows$number_voters - rows$invalid_ballots &
        rows$valid_votes <= 3L *
          (rows$number_voters - rows$invalid_ballots)
    ),
    county_subtotals_within_exact_totals = all(
      county_checks$within_county_total
    ),
    manual_verified_extraction_feasible = TRUE,
    automatic_import_ready = FALSE,
    recommendation = paste(
      "Proceed only with pagewise coordinate OCR, image verification of every",
      "numeric cell, and exact full-county reconciliation; unattended import",
      "is not supported by this pilot."
    ),
    stringsAsFactors = FALSE
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    rows, file.path(output_dir, "brandenburg_municipality_pilot_rows.csv"),
    row.names = FALSE, na = ""
  )
  utils::write.csv(
    audit, file.path(output_dir, "brandenburg_municipality_pilot_ocr_audit.csv"),
    row.names = FALSE, na = ""
  )
  utils::write.csv(
    corrections,
    file.path(output_dir, "brandenburg_municipality_pilot_corrections.csv"),
    row.names = FALSE, na = ""
  )
  utils::write.csv(
    county_checks,
    file.path(output_dir, "brandenburg_municipality_pilot_county_checks.csv"),
    row.names = FALSE, na = ""
  )
  utils::write.csv(
    feasibility,
    file.path(output_dir, "brandenburg_municipality_pilot_feasibility.csv"),
    row.names = FALSE, na = ""
  )

  list(
    rows = rows,
    audit = audit,
    corrections = corrections,
    county_checks = county_checks,
    feasibility = feasibility
  )
}
