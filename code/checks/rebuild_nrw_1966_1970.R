# Focused rebuild and regression check. Run from repository root:
# Rscript --vanilla code/checks/rebuild_nrw_1966_1970.R [baseline repository root]
# Reads the baseline only; writes state_unharm CSV/RDS in the current checkout.
# For a full build, 01b_state_unharm_raw.R uses the same nrw_verified.R helper.
args <- commandArgs(trailingOnly = TRUE)
baseline_root <- if (length(args)) args[[1]] else "."
source("code/state_elections/nrw_verified.R")
stopifnot(requireNamespace("data.table", quietly = TRUE))
base_final <- file.path(baseline_root, "data/state_elections/final")
output <- "data/state_elections/final"
audit <- "data/data_checks/nrw_1966_1970"
dir.create(audit, recursive = TRUE, showWarnings = FALSE)
before <- readRDS(file.path(base_final, "state_unharm.rds"))
after <- before
fixed <- do.call(rbind, gerda_nrw_verified_results())
key <- function(x) paste(x$state, x$election_year, x$ags, sep = "/")
affected <- before$state == "05" & before$election_year %in% c(1966L, 1970L)
stopifnot(sum(affected) == 185L, !anyDuplicated(key(fixed)),
          setequal(key(before[affected, ]), key(fixed)))
index <- match(key(fixed), key(before))

# Names in the legacy input define the published synthetic ID assignment.
legacy <- read.csv(file.path(baseline_root,
  "data/state_elections/raw/Landtagswahlen/Nordrhein-Westfalen/nrw_pre1975_kreis.csv"),
  colClasses = "character")
ids <- read.delim("data/state_elections/derived/nrw_1966_1970/unit_identifiers.tsv",
                  colClasses = "character")
for (year in c("1966", "1970")) {
  old <- legacy[legacy$election_year == year, ]
  mapped <- ids[ids$election_year == year, ]
  stopifnot(identical(old$name, mapped$name), identical(old$type, mapped$type),
            identical(mapped$ags, paste0("050", sprintf("%02d", seq_len(nrow(old))), "000")))
}

identifiers <- c("ags", "election_year", "state", "election_date")
for (column in identifiers) {
  stopifnot(identical(as.character(after[[column]][index]), as.character(fixed[[column]])))
}
for (column in setdiff(names(fixed), identifiers)) {
  stopifnot(column %in% names(after))
  after[[column]][index] <- fixed[[column]]
}
flags <- c("flag_briefwahl_only", "flag_naive_turnout_above_1", "flag_no_valid_votes")
for (flag in flags) after[[flag]][index] <- 0
stopifnot(identical(before[!affected, ], after[!affected, ]),
          identical(lapply(before, class), lapply(after, class)),
          identical(before[c("ags", "election_year", "state", "election_date")],
                    after[c("ags", "election_year", "state", "election_date")]))

# Check every reconstructed count against the verified source cells.
counts <- read.csv("data/state_elections/derived/nrw_1966_1970/nrw_1966_1970_kreis.csv",
                   colClasses = c(ags = "character"))
counts <- counts[match(paste(fixed$election_year, fixed$ags),
                       paste(counts$election_year, counts$ags)), ]
parties <- c("cdu", "spd", "fdp", "zentrum", "uap", "fsu", "dkp", "npd")
for (party in parties) {
  stopifnot(max(abs(after[[party]][index] * after$valid_votes[index] - counts[[party]])) < 1e-7)
}
stopifnot(all(after$valid_votes[index] + after$invalid_votes[index] == after$number_voters[index]),
          all(after$turnout[index] == after$number_voters[index] / after$eligible_voters[index]),
          max(abs(rowSums(after[index, parties]) - 1)) < 1e-14,
          all(after$other[index] == 0), all(as.matrix(after[index, flags]) == 0))

# Independent spot controls include party attribution and the two old bad patches.
spot <- function(year, name, field, expected) {
  stopifnot(counts[counts$election_year == year & counts$name == name, field] == expected)
}
spot(1966, "Düsseldorf", "eligible_voters", 485697)
spot(1966, "Düsseldorf", "number_voters", 347196)
spot(1966, "Bonn", "valid_votes", 59589)
spot(1966, "Kreis Schleiden", "eligible_voters", 42123)
spot(1970, "Bottrop", "eligible_voters", 78079)
spot(1970, "Düsseldorf", "spd", 153772)
spot(1970, "Düsseldorf", "cdu", 147112)

data.table::fwrite(after, file.path(output, "state_unharm.csv"))
saveRDS(after, file.path(output, "state_unharm.rds"), compress = "gzip")
stopifnot(identical(readRDS(file.path(output, "state_unharm.rds")), after))
csv <- data.table::fread(file.path(output, "state_unharm.csv"),
                         colClasses = c(ags = "character", state = "character"),
                         na.strings = c("", "NA"), data.table = FALSE)
for (column in names(after)) {
  stopifnot(identical(unname(is.na(csv[[column]])), unname(is.na(after[[column]]))))
  if (is.numeric(after[[column]])) {
    stopifnot(isTRUE(all.equal(as.numeric(csv[[column]]), as.numeric(after[[column]]),
                               tolerance = 1e-14)))
  } else {
    stopifnot(identical(as.character(csv[[column]]), as.character(after[[column]])))
  }
}

# Historical synthetic units do not enter any published harmonized dataset.
scope <- lapply(c("state_harm", "state_harm_21", "state_harm_23", "state_harm_25"), function(stem) {
  d <- readRDS(file.path(base_final, paste0(stem, ".rds")))
  n <- sum(substr(d$ags, 1, 2) == "05" & d$election_year %in% c(1966L, 1970L), na.rm = TRUE)
  stopifnot(n == 0L)
  data.frame(dataset = stem, affected_rows = n, action = "unchanged; affected elections absent")
})
data.table::fwrite(do.call(rbind, scope), file.path(audit, "harmonized_scope.tsv"), sep = "\t")
changed <- vapply(names(after), function(column) {
  a <- before[[column]][index]; b <- after[[column]][index]
  sum(is.na(a) != is.na(b) | (!is.na(a) & !is.na(b) & a != b))
}, integer(1))
data.table::fwrite(data.frame(field = names(changed), changed_cells = changed),
                   file.path(audit, "changed_fields.tsv"), sep = "\t")
summary <- do.call(rbind, lapply(c(1966L, 1970L), function(year) {
  a <- before[affected & before$election_year == year, ]
  b <- after[affected & after$election_year == year, ]
  do.call(rbind, lapply(c("eligible_voters", "number_voters", "valid_votes", "invalid_votes"), function(field) {
    data.frame(election_year = year, field = field, before = sum(a[[field]], na.rm = TRUE),
               after = sum(b[[field]]), rows = nrow(b))
  }))
}))
data.table::fwrite(summary, file.path(audit, "before_after_totals.tsv"), sep = "\t")
cat("PASS: 185 repaired rows; all source counts recovered; CSV/RDS equivalent;\n",
    sum(!affected), "unrelated rows and all column types unchanged.\n")
