# Rscript --vanilla code/checks/rebuild_sh_1983.R [baseline repository root]
# Only replaces SH 1983; the full parser uses the same verified input helper.
pacman::p_load(here, data.table, dplyr, digest)
here::i_am("code/checks/rebuild_sh_1983.R")
args <- commandArgs(trailingOnly = TRUE)
baseline <- if (length(args)) normalizePath(args[[1]]) else here::here()
source(here::here("code/state_elections/sh_1983_verified.R"))
output <- here::here("data/state_elections/final")
audit <- here::here("data/data_checks/sh_1983")
dir.create(audit, recursive = TRUE, showWarnings = FALSE)
before <- readRDS(file.path(baseline, "data/state_elections/final/state_unharm.rds"))
affected <- before$state == "01" & before$election_year == 1983L
stopifnot(sum(affected) %in% c(1079L, 1128L))
fixed <- gerda_sh_1983_verified()
after_template <- before
if (!"llsh" %in% names(after_template)) {
  after_template$llsh <- NA_real_
  # The final pipeline orders all party columns alphabetically.
  front <- names(before)[seq_len(12)]
  parties <- setdiff(names(after_template), c(front, "other", "cdu_csu"))
  after_template <- after_template[c(front, sort(parties), "other", "cdu_csu")]
}
replacement <- after_template[rep(NA_integer_, nrow(fixed)), ]
for (column in names(fixed)) replacement[[column]] <- fixed[[column]]
flags <- c("flag_briefwahl_only", "flag_naive_turnout_above_1", "flag_no_valid_votes")
for (flag in flags) replacement[[flag]] <- 0
first <- which(affected)[1]
after <- bind_rows(after_template[seq_len(first - 1L), ], replacement,
                   after_template[seq_len(nrow(before)) >= first & !affected, ])
new_affected <- after$state == "01" & after$election_year == 1983L
stopifnot(nrow(after) == nrow(before) - sum(affected) + 1128L,
          identical(before[!affected, ], after[!new_affected, names(before)]),
          all(is.na(after$llsh[!new_affected])),
          identical(lapply(before, class), lapply(after[names(before)], class)),
          !anyDuplicated(fixed$ags), all(is.na(fixed$turnout)),
          all(as.matrix(after[new_affected, flags]) == 0))

counts <- read.delim(here::here("data/state_elections/derived/sh_1983/table6_transcription.tsv"),
                     colClasses = c(ags = "character"))
stopifnot(identical(counts$ags, fixed$ags))
for (field in c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")) {
  stopifnot(identical(as.numeric(counts[[field]]), fixed[[field]]))
}
for (party in c("cdu", "spd", "fdp", "ssw", "dkp", "dgl", "gruene", "fp", "fsu")) {
  stopifnot(max(abs(fixed[[party]] * fixed$valid_votes - counts[[party]])) < 1e-7)
}
stopifnot(max(abs((fixed$einzelbewerber + fixed$llsh) * fixed$valid_votes -
                    counts$einzelbewerber_llsh)) < 1e-7,
          all(fixed$number_voters == fixed$valid_votes + fixed$invalid_votes),
          all(fixed$other == 0), all(fixed$eligible_voters > 0))
# Flensburg catches the lost leading digit and DKP/DGL shift in the old parser.
fl <- counts[counts$ags == "01001000", ]
stopifnot(fl$number_voters == 49407, fl$dkp == 42, fl$dgl == 154)
fwrite(after, file.path(output, "state_unharm.csv"))
saveRDS(after, file.path(output, "state_unharm.rds"), compress = "gzip")
stopifnot(identical(readRDS(file.path(output, "state_unharm.rds")), after))
csv <- fread(file.path(output, "state_unharm.csv"),
             colClasses = c(ags = "character", state = "character"),
             na.strings = c("", "NA"), data.table = FALSE)
for (column in names(after)) {
  stopifnot(identical(unname(is.na(csv[[column]])), unname(is.na(after[[column]]))))
  if (is.numeric(after[[column]])) {
    stopifnot(isTRUE(all.equal(as.numeric(csv[[column]]), as.numeric(after[[column]]), tolerance = 1e-14)))
  } else stopifnot(identical(as.character(csv[[column]]), as.character(after[[column]])))
}
summary <- do.call(rbind, lapply(c("eligible_voters", "number_voters", "valid_votes", "invalid_votes"), function(field) {
  data.frame(field, before = sum(before[[field]][affected], na.rm = TRUE),
             after = sum(after[[field]][new_affected]),
             before_missing = sum(is.na(before[[field]][affected])), after_missing = 0)
}))
fwrite(summary, file.path(audit, "before_after_totals.tsv"), sep = "\t")
scope <- lapply(c("state_harm", "state_harm_21", "state_harm_23", "state_harm_25"), function(stem) {
  path <- file.path(output, paste0(stem, ".rds"))
  d <- readRDS(path)
  n <- sum(substr(d$ags, 1, 2) == "01" & d$election_year == 1983L, na.rm = TRUE)
  stopifnot(n == 0)
  data.frame(dataset = stem, affected_rows = n, sha256 = digest(file = path, algo = "sha256"))
})
fwrite(bind_rows(scope), file.path(audit, "harmonized_scope.tsv"), sep = "\t")
cat(sprintf("PASS: 1,128 repaired SH rows; %s unrelated rows unchanged; %s x %s CSV/RDS verified.\n",
            sum(!affected), nrow(after), ncol(after)))
