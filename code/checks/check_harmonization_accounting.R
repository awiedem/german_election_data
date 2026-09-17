# Independent release check: compare every published state-election count with
# its recorded source contributions, then compare the ledger with raw input.
# Run after all three state harmonizers, from the repository root.
source("code/shared/harmonization_audit.R")

gerda_check_state_accounting <- function(target, output = NULL,
                                         audit_dir = "data/data_checks/harmonization") {
  label <- paste0("state_harm_", target)
  if (is.null(output)) output <- readRDS(paste0("data/state_elections/final/", label, ".rds"))
  read_report <- function(suffix) {
    path <- file.path(audit_dir, paste0(label, "_", suffix, ".csv"))
    fields <- names(data.table::fread(path, nrows = 0))
    as.data.frame(data.table::fread(path, colClasses = list(character =
      intersect(c("ags", "state", paste0("ags_", target)), fields))))
  }
  ledger <- read_report("source_ledger")
  mapping <- read_report("mapping")
  keys <- c("ags", "election_year", "state")
  sk <- gerda_key(ledger, keys); mk <- gerda_key(mapping, keys)
  stopifnot(!anyDuplicated(sk), setequal(sk, mk), all(ledger$status == "mapped"))
  i <- match(mk, sk)
  stopifnot(all(is.finite(mapping$pop_cw)), all(mapping$pop_cw >= 0),
            all(abs(tapply(mapping$pop_cw, mk, sum) - 1) < 1e-7))
  target_col <- paste0("ags_", target)
  mapping$ags <- mapping[[target_col]]
  tk <- gerda_key(mapping, c("ags", "election_year"))
  ok <- gerda_key(output, c("ags", "election_year"))
  stopifnot(!anyDuplicated(ok), setequal(tk, ok))
  counts <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")
  sum_na <- function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
  for (col in counts) {
    expected <- tapply(ledger[[col]][i] * mapping$pop_cw, tk, sum_na)
    expected <- as.numeric(expected[ok])
    actual <- output[[col]]
    stopifnot(identical(is.na(expected), is.na(actual)))
    # CSV serialization / summation order can straddle an exact half vote.
    # Every published integer must still be within half a vote of its allocation.
    stopifnot(all(abs(expected - actual) <= .5 + 1e-7, na.rm = TRUE))
  }
  # This comparison can see a source row missing from all three harmonized
  # products, which output-to-output comparisons cannot detect.
  raw <- readRDS("data/state_elections/final/state_unharm.rds")
  raw <- raw[raw$election_year >= 1990 &
               !(raw$state == "09" & grepl("^09[0-9]{3}444$", raw$ags)), ]
  raw$valid_votes <- with(raw, ifelse(!is.na(valid_votes), valid_votes,
    ifelse(!is.na(number_voters) & number_voters > 0, number_voters,
    ifelse(!is.na(eligible_voters) & eligible_voters > 0, eligible_voters, 1))))
  for (col in counts) {
    expected <- tapply(raw[[col]], raw$election_year, sum_na)
    actual <- tapply(ledger[[col]], ledger$election_year, sum_na)
    stopifnot(identical(names(expected), names(actual)),
              identical(is.na(expected), is.na(actual)),
              all(abs(expected - actual) < 1e-7, na.rm = TRUE))
  }
  cat("PASS:", label, "every target count and complete source ledger\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) for (target in c("21", "23", "25")) gerda_check_state_accounting(target)
