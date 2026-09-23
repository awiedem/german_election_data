#!/usr/bin/env Rscript
# Source reconciliation and regression check. Run from the repository root.
# Optional arguments: --raw-root PATH --baseline-rds PATH --baseline-csv PATH
args <- commandArgs(trailingOnly = TRUE)
if (length(args) %% 2L) stop("Arguments must be --name PATH pairs")
opts <- if (length(args)) setNames(as.list(args[seq(2L, length(args), 2L)]),
                                  args[seq(1L, length(args), 2L)]) else list()
stopifnot(all(names(opts) %in% c("--raw-root", "--baseline-rds", "--baseline-csv")))
raw_root <- if (is.null(opts[["--raw-root"]])) {
  "data/state_elections/raw/Landtagswahlen"
} else opts[["--raw-root"]]
source("code/state_elections/parse_bw_1952.R")
parsed <- read_bw_1952(raw_root)
out <- readRDS("data/state_elections/final/state_unharm.rds")
checks <- data.frame(check = character(), passed = logical(), detail = character())
check <- function(name, passed, detail = "") {
  checks[nrow(checks) + 1L, ] <<- list(name, isTRUE(passed), as.character(detail))
}
is_1952 <- out$state == "08" & out$election_year == 1952
x <- out[is_1952, ]
check("municipality_rows", nrow(x) == 1111L, nrow(x))
check("unique_1952_keys", !anyDuplicated(paste(x$state, x$election_year, x$ags)))
check("unique_election_keys",
      !anyDuplicated(paste(out$state, out$election_date, out$ags)))
check("1952_keys", identical(x$ags, parsed$data$ags))
check("retrospective_keys_match_1956",
      identical(sort(x$ags), sort(out$ags[out$state == "08" & out$election_year == 1956])))
check("constituent_assembly_date", all(x$election_date == as.Date("1952-03-09")))
check("unavailable_full_electorate", all(is.na(x$eligible_voters)))
check("unavailable_comparable_turnout", all(is.na(x$turnout)))
check("not_flagged_as_postal_units", all(x$flag_briefwahl_only == 0))
check("single_vote_accounting", all(x$valid_votes + x$invalid_votes == x$number_voters))
check("source_accounting_and_official_differences",
      all(parsed$reconciliation$difference == parsed$reconciliation$documented_difference))
for (field in c("number_voters", "valid_votes", "invalid_votes")) {
  check(paste0("source_", field), identical(x[[field]], parsed$data[[field]]))
}
for (party in parsed$parties) {
  recovered <- x[[party]] * x$valid_votes
  original <- parsed$source_counts[[party]]
  check(paste0("municipal_counts_", party), all(abs(recovered - original) < 1e-9),
        paste("sum", sum(original), "zero cells", sum(original == 0)))
}
check("no_unexplained_party_residual", all(x$other == 0))
check("cdu_family", identical(x$cdu_csu, x$cdu))
check("new_party_columns", all(c("dg_bhe", "uwg") %in% names(out)))
check("new_parties_missing_elsewhere",
      all(is.na(out$dg_bhe[!is_1952])) && all(is.na(out$uwg[!is_1952])))
meta <- c("ags", "election_year", "election_date", "state", "eligible_voters",
          "number_voters", "valid_votes", "invalid_votes", "turnout")
other_parties <- setdiff(names(x), c(meta, grep("^flag_", names(x), value = TRUE),
                                    parsed$parties, "other", "cdu_csu"))
check("unreported_parties_remain_missing", all(is.na(as.matrix(x[other_parties]))))

csv <- data.table::fread("data/state_elections/final/state_unharm.csv",
                          colClasses = c(ags = "character", state = "character"),
                          data.table = FALSE)
check("csv_schema_and_rows", identical(names(csv), names(out)) && nrow(csv) == nrow(out))
for (field in names(out)) {
  a <- csv[[field]]; b <- out[[field]]
  if (inherits(b, "Date")) { a <- as.character(a); b <- as.character(b) }
  check(paste0("csv_rds_", field), identical(is.na(a), is.na(b)) &&
          isTRUE(all.equal(a, b, check.attributes = FALSE, tolerance = 1e-12)))
}

if (!is.null(opts[["--baseline-rds"]])) {
  baseline <- readRDS(opts[["--baseline-rds"]])
  old <- out[!is_1952, names(baseline)]
  check("existing_rds_identical", identical(old, baseline), nrow(old))
  check("only_two_new_columns", identical(sort(setdiff(names(out), names(baseline))),
                                         c("dg_bhe", "uwg")))
  check("harmonization_input_unchanged",
        identical(old[old$election_year >= 1990, ],
                  baseline[baseline$election_year >= 1990, ]))
  check("one_added_election",
        nrow(unique(out[c("state", "election_year", "election_date")])) ==
          nrow(unique(baseline[c("state", "election_year", "election_date")])) + 1L)
}
if (!is.null(opts[["--baseline-csv"]])) {
  baseline_csv <- data.table::fread(opts[["--baseline-csv"]],
    colClasses = c(ags = "character", state = "character"), data.table = FALSE)
  old_csv <- csv[!is_1952, names(baseline_csv)]
  rownames(old_csv) <- NULL
  check("existing_csv_values_identical", identical(old_csv, baseline_csv))
}

directory <- "data/data_checks/bw_1952"
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(checks, file.path(directory, "validation.csv"), na = "NA")
if (any(!checks$passed)) {
  print(checks[!checks$passed, ])
  stop("BW 1952 checks failed")
}
cat(sprintf("BW 1952: %d checks passed; %d rows, %d columns, %d elections.\n",
            nrow(checks), nrow(out), ncol(out),
            nrow(unique(out[c("state", "election_year", "election_date")]))))
