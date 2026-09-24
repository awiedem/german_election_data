# Run actual old/new NRW parser sections against the same read-only sources.
# Rscript --vanilla code/checks/check_nrw_parser.R [baseline repository root]
suppressPackageStartupMessages({library(dplyr);library(tibble);library(readxl);library(here)})
args <- commandArgs(trailingOnly=TRUE)
base <- if (length(args)) args[[1]] else '.'
run_nrw <- function(path) {
 text <- readLines(path, warn=FALSE)
 env <- new.env(parent=globalenv())
 helper_start <- grep('^meta_cols <- c',text)
 helper_end <- grep('^## Collector list:',text)-1
 stopifnot(length(helper_start)==1L, length(helper_end)==1L)
 eval(parse(text=text[helper_start:helper_end]),env)
 env$all_states <- list()
 env$raw_path <- file.path(base,'data/state_elections/raw/Landtagswahlen')
 start <- grep('^cat\\("=== NORDRHEIN-WESTFALEN',text)
 end <- grep('^cat\\("NRW total:',text)
 stopifnot(length(start)==1L, length(end)==1L)
 eval(parse(text=text[start:end]),env)
 env$all_states$nrw
}
a <- run_nrw(file.path(base,'code/state_elections/01b_state_unharm_raw.R'))
b <- run_nrw('code/state_elections/01b_state_unharm_raw.R')
keep <- function(x) x[!x$election_year%in%c(1966,1970),]
stopifnot(identical(keep(a),keep(b)))
new <- readRDS('data/state_elections/final/state_unharm.rds')
key <- function(x) paste(x$state,x$election_year,x$ags)
affected <- b$election_year%in%c(1966,1970)
b <- b[affected,]
expected <- new[new$state == "05" & new$election_year %in% c(1966,1970),]
stopifnot(nrow(b) == 185L, nrow(expected) == 185L,
          !anyDuplicated(key(b)), !anyDuplicated(key(expected)),
          setequal(key(b), key(expected)),
          all(intersect(names(a), names(new)) %in% names(b)))
z <- new[match(key(b),key(new)),]
# Final cleanup drops globally empty party columns; they must contain no votes.
for (k in setdiff(names(b),names(z))) stopifnot(all(is.na(b[[k]]) | b[[k]]==0))
for (k in intersect(names(b),names(z))) {
 stopifnot(isTRUE(all.equal(unname(b[[k]]),unname(z[[k]]),check.attributes=FALSE)))
}
cat('PASS: actual NRW parser section; all unaffected NRW years identical; repaired rows equal rebuilt release rows.\n')
