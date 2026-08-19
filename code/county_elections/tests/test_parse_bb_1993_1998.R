#!/usr/bin/env Rscript

set.seed(20260730)

source("code/county_elections/parsers/parse_bb_1993_1998.R")

stopifnot(nzchar(Sys.which("pdfinfo")))
stopifnot(nzchar(Sys.which("pdftoppm")))
stopifnot(nzchar(Sys.which("tesseract")))

pointer <- tempfile(fileext = ".pdf")
writeLines(
  c(
    "version https://git-lfs.github.com/spec/v1",
    paste0("oid sha256:", paste(rep("0", 64), collapse = "")),
    "size 10000"
  ),
  pointer
)
stopifnot(.bb_ocr_is_lfs_pointer(pointer))
pointer_error <- tryCatch(
  {
    .bb_ocr_require_pdf(pointer)
    ""
  },
  error = conditionMessage
)
stopifnot(grepl("Git LFS pointer", pointer_error, fixed = TRUE))

raw_pointer <- file.path(
  "data", "county_elections", "raw", "Kreistagswahlen", "Brandenburg",
  "Brandenburg_1998_KTW_1.pdf"
)
pointer_lines <- readLines(raw_pointer, n = 3L, warn = FALSE)
oid <- sub("^oid sha256:", "", pointer_lines[grepl("^oid sha256:", pointer_lines)])
object_path <- file.path(
  ".git", "lfs", "objects", substr(oid, 1L, 2L), substr(oid, 3L, 4L), oid
)
if (!file.exists(object_path)) {
  stop("Local LFS object required for Brandenburg OCR test is unavailable")
}

test_dir <- tempfile("bb_ocr_test_")
dir.create(test_dir)
source_pdf <- file.path(test_dir, "Brandenburg_1998_KTW_1.pdf")
stopifnot(file.copy(object_path, source_pdf))

manifest <- bb_extract_summary_ocr(
  source_pdf = source_pdf,
  output_dir = file.path(test_dir, "ocr"),
  pages = 6L,
  dpi = 220L
)
stopifnot(nrow(manifest) == 1L)
stopifnot(manifest$page == 6L)
stopifnot(manifest$word_count > 100L)
stopifnot(manifest$median_confidence > 70)
stopifnot(manifest$has_result_heading)
stopifnot(manifest$has_eligible_voters)
stopifnot(file.exists(manifest$image_path))
stopifnot(file.exists(manifest$text_path))
stopifnot(file.exists(manifest$tsv_path))

incomplete <- bb_assess_summary_ocr(manifest)
stopifnot(!incomplete$summary_pages_complete)
stopifnot(!incomplete$ready_for_automatic_import)

historical <- parse_bb_1993_1998_county_summary(file.path(
  "data", "county_elections", "raw", "Kreistagswahlen", "Brandenburg"
))
stopifnot(nrow(historical) == 36L)
stopifnot(all(table(historical$election_year) == 18L))
stopifnot(!anyDuplicated(historical[c("ags", "election_year")]))
stopifnot(all(historical$ags == paste0(historical$county, "000")))
stopifnot(all(historical$result_level == "county"))
stopifnot(sum(historical$contest_type == "kreisfreie_city_council") == 8L)
stopifnot(sum(historical$contest_type == "kreistag") == 28L)
stopifnot(all(historical$turnout > 0 & historical$turnout <= 1))
stopifnot(all(rowSums(historical[c(
  "spd", "linke_pds", "cdu", "fdp", "gruene", "other_lists"
)]) > 0.999999))
stopifnot(all(rowSums(historical[c(
  "spd", "linke_pds", "cdu", "fdp", "gruene", "other_lists"
)]) < 1.000001))

source_csv <- read.csv(
  file.path(
    "data", "county_elections", "raw", "Kreistagswahlen", "Brandenburg",
    "derived", "brandenburg_county_summary_1993_1998.csv"
  ),
  stringsAsFactors = FALSE,
  colClasses = c(ags = "character")
)
altered <- source_csv
altered$spd[[1L]] <- altered$spd[[1L]] + 1
altered$other_lists[[1L]] <- altered$other_lists[[1L]] - 1
altered_error <- tryCatch(
  {
    .bb_validate_historical_summary(altered)
    ""
  },
  error = conditionMessage
)
stopifnot(grepl("printed state totals", altered_error, fixed = TRUE))

cat("Brandenburg 1993/1998 OCR extraction scaffold test passed.\n")
