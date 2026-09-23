# Rscript --vanilla code/checks/check_sh_1983_parser.R [baseline repository root]
# Exercises the actual SH parser section, including the seven other elections.
pacman::p_load(here, dplyr, tibble, readxl)
here::i_am("code/checks/check_sh_1983_parser.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
baseline <- normalizePath(args[[1]])
run_sh <- function(path) {
  text <- readLines(path, warn = FALSE)
  env <- new.env(parent = globalenv())
  start <- grep("^meta_cols <- c", text)
  end <- grep("^## Collector list:", text) - 1L
  eval(parse(text = text[start:end]), env)
  env$all_states <- list()
  start <- grep("^sh_map_party <- function", text)
  end <- grep('^cat\\("Schleswig-Holstein total:', text)
  stopifnot(length(start) == 1L, length(end) == 1L)
  eval(parse(text = text[start:end]), env)
  env$all_states$sh
}
a <- run_sh(file.path(baseline, "code/state_elections/01b_state_unharm_raw.R"))
b <- run_sh(here::here("code/state_elections/01b_state_unharm_raw.R"))
stopifnot(identical(a[a$election_year != 1983L, ], b[b$election_year != 1983L, names(a)]),
          all(is.na(b$llsh[b$election_year != 1983L])))
release <- readRDS(here::here("data/state_elections/final/state_unharm.rds"))
release <- release[release$state == "01" & release$election_year == 1983L, ]
parsed <- b[b$election_year == 1983L, ]
stopifnot(nrow(parsed) == 1128L, nrow(release) == 1128L,
          setequal(parsed$ags, release$ags), !anyDuplicated(parsed$ags))
release <- release[match(parsed$ags, release$ags), ]
for (field in names(parsed)) {
  stopifnot(isTRUE(all.equal(unname(parsed[[field]]), unname(release[[field]]), check.attributes = FALSE)))
}
cat("PASS: actual SH parser agrees with release; all seven other SH elections unchanged.\n")
