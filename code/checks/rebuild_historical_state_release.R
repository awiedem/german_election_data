# Combine reviewed missingness, NRW, BW 1952 and SH 1983 corrections.
# Run after merging their source helpers:
# Rscript --vanilla code/checks/rebuild_historical_state_release.R ORIGINAL_MAIN_FINAL_DIR
pacman::p_load(here, dplyr, data.table, readxl, digest)
here::i_am('code/checks/rebuild_historical_state_release.R')
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
original <- readRDS(file.path(args[[1]], 'state_unharm.rds'))
source(here('code', 'state_elections', 'nrw_verified.R'))
source(here('code', 'state_elections', 'sh_1983_verified.R'))
source(here('code', 'state_elections', 'parse_bw_1952.R'))
path <- here('data', 'state_elections', 'final')
x <- readRDS(file.path(path, 'state_unharm.rds'))
nrw <- bind_rows(gerda_nrw_verified_results())
sh <- gerda_sh_1983_verified()
bw <- read_bw_1952()
write_bw_1952_provenance(bw)
key <- function(d) paste(d$state, d$election_date, d$ags, sep = '/')
# Replace affected groups only; preserve the missingness-corrected baseline.
replace_group <- function(d, fixed, affected, insert_at = which(affected)[1]) {
  for (nm in setdiff(names(fixed), names(d))) d[[nm]] <- NA_real_
  replacement <- d[rep(NA_integer_, nrow(fixed)), ]
  for (nm in names(fixed)) replacement[[nm]] <- fixed[[nm]]
  for (flag in grep('^flag_', names(d), value = TRUE)) replacement[[flag]] <- 0
  bind_rows(d[seq_len(nrow(d)) < insert_at & !affected, ], replacement,
            d[seq_len(nrow(d)) >= insert_at & !affected, ])
}
affected <- x$state == '05' & x$election_year %in% c(1966L, 1970L)
stopifnot(sum(affected) == 185L, setequal(key(x[affected, ]), key(nrw)))
# Preserve existing row positions for the two separate NRW elections.
for (year in c(1966L, 1970L)) x <- replace_group(x, nrw[nrw$election_year == year, ],
  x$state == '05' & x$election_year == year)
x <- replace_group(x, sh, x$state == '01' & x$election_year == 1983L)
x <- replace_group(x, bw$data, x$state == '08' & x$election_year == 1952L,
                   min(which(x$state == '08')))
front <- names(original)[seq_len(12)]
x <- x[c(front, sort(setdiff(names(x), c(front, 'other', 'cdu_csu'))), 'other', 'cdu_csu')]
stopifnot(nrow(x) == nrow(original) + 1111L + 49L, ncol(x) == ncol(original) + 3L,
          !anyDuplicated(key(x)))
changed_election <- function(d) (d$state == '05' & d$election_year %in% c(1966L, 1970L)) |
  (d$state == '01' & d$election_year == 1983L) | (d$state == '08' & d$election_year == 1952L)
a <- original[!changed_election(original), ]
b <- x[!changed_election(x), names(original)]
stopifnot(identical(key(a), key(b)),
          identical(a[setdiff(names(a), 'invalid_votes')], b[setdiff(names(b), 'invalid_votes')]))
changed_iv <- is.na(a$invalid_votes) != is.na(b$invalid_votes) |
  (!is.na(a$invalid_votes) & !is.na(b$invalid_votes) & a$invalid_votes != b$invalid_votes)
stopifnot(all(a$invalid_votes[changed_iv] == 0), all(is.na(b$invalid_votes[changed_iv])))
# Every repaired group must equal the independently validated source helper.
for (fixed in list(nrw, sh, bw$data)) {
  observed <- x[match(key(fixed), key(x)), ]
  for (field in names(fixed)) stopifnot(isTRUE(all.equal(unname(observed[[field]]),
    unname(fixed[[field]]), check.attributes = FALSE, tolerance = 1e-12)))
}
# Newly introduced parties have no observations in other elections.
stopifnot(all(is.na(x$llsh[!(x$state == '01' & x$election_year == 1983L)])),
          all(is.na(x$dg_bhe[!(x$state == '08' & x$election_year == 1952L)])),
          all(is.na(x$uwg[!(x$state == '08' & x$election_year == 1952L)])))
fwrite(x, file.path(path, 'state_unharm.csv'))
saveRDS(x, file.path(path, 'state_unharm.rds'), compress = 'gzip')
# Historical repairs are absent from harmonized data. Only missing invalid counts change.
harm_checks <- lapply(c('state_harm_21', 'state_harm_23', 'state_harm_25'), function(stem) {
  old <- readRDS(file.path(args[[1]], paste0(stem, '.rds')))
  new <- readRDS(file.path(path, paste0(stem, '.rds')))
  stopifnot(!any(changed_election(new)), identical(names(old), names(new)),
            identical(old[setdiff(names(old), 'invalid_votes')], new[setdiff(names(new), 'invalid_votes')]))
  changed <- is.na(old$invalid_votes) != is.na(new$invalid_votes)
  stopifnot(all(old$invalid_votes[changed] == 0), all(is.na(new$invalid_votes[changed])),
            identical(old$invalid_votes[!changed], new$invalid_votes[!changed]))
  data.frame(dataset = stem, rows = nrow(new), invalid_zero_to_missing = sum(changed))
})
audit <- here('data', 'data_checks', 'historical_state_release')
dir.create(audit, recursive = TRUE, showWarnings = FALSE)
fwrite(bind_rows(data.frame(dataset = 'state_unharm', rows = nrow(x),
                           invalid_zero_to_missing = sum(changed_iv)), bind_rows(harm_checks)),
       file.path(audit, 'integration_summary.tsv'), sep = '\t')
cat(sprintf('PASS: %s x %s combined release; %s unrelated rows preserved except verified missing invalid counts.\n',
            nrow(x), ncol(x), nrow(a)))
