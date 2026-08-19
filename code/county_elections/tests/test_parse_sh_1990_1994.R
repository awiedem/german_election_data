set.seed(20260730)

source("code/county_elections/parsers/parse_sh_1990_1994.R")

raw_dir <- "data/county_elections/raw/Kreistagswahlen"
diagnostics <- assess_sh_1990_1994_pdf_feasibility(raw_dir)

stopifnot(
  nrow(diagnostics) == 2L,
  identical(diagnostics$election_year, c(1990L, 1994L)),
  all(diagnostics$pdf_pages == 194L),
  all(diagnostics$turnout_table_pages == 82L),
  all(diagnostics$vote_table_pages == 82L),
  identical(diagnostics$candidate_code_rows, c(1070L, 1118L)),
  identical(diagnostics$unique_candidate_keys, c(1055L, 1112L)),
  identical(diagnostics$duplicate_candidate_keys, c(15L, 6L)),
  identical(
    diagnostics$recognizable_vote_100_percent_rows,
    c(835L, 1125L)
  ),
  all(!diagnostics$safe_to_parse),
  all(diagnostics$source_limitation),
  all(nzchar(diagnostics$source_note))
)

error <- tryCatch(
  {
    parse_sh_1990_1994_municipality_elections(raw_dir)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(error, "error"),
  grepl("Unsafe SH 1990/1994 OCR extraction", conditionMessage(error)),
  grepl("no observations returned", conditionMessage(error))
)

message(
  "SH 1990/1994 feasibility test passed: both official PDFs and all 82 ",
  "turnout/vote page pairs are present; quantified OCR key failures cause ",
  "the parser to stop instead of emitting guessed election values."
)
