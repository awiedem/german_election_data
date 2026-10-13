# From repository root: Rscript --vanilla code/checks/rebuild_bb_1990_1994.R
#   BASELINE_FINAL_DIR DEPENDENCY_ROOT STAGING_DIR [--resume]
# BASELINE_FINAL_DIR contains the five published state RDS files before repair.
# DEPENDENCY_ROOT contains hydrated crosswalk/covariate files at repository paths.
# Rebuilds maintained outputs in staging; writes final files only after checks.
set.seed(20260928)
library(readxl)
library(data.table)
library(digest)
library(jsonlite)
source("code/state_elections/bb_verified.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 3:4)
resume <- length(args) == 4L && args[4] == "--resume"
stopifnot(length(args) == 3L || resume)
baseline <- normalizePath(args[1])
dependencies <- normalizePath(args[2])
dir.create(args[3], recursive = TRUE, showWarnings = FALSE)
staging <- normalizePath(args[3])
root <- normalizePath(".")
stopifnot(staging != root, baseline != normalizePath("data/state_elections/final"))
audit <- "data/data_checks/bb_1990_1994"
dir.create(audit, recursive = TRUE, showWarnings = FALSE)
key <- function(x) paste(x$ags, x$election_year, sep = "/")
before <- readRDS(file.path(baseline, "state_unharm.rds"))
after <- before
if (!"domowina" %in% names(after)) after$domowina <- NA_real_
results <- list(`1990` = gerda_bb_1990_verified(), `1994` = gerda_bb_1994_verified())
summaries <- reconciliations <- list()
for (yr in names(results)) {
  fit <- results[[yr]]
  fixed <- fit$data
  stopifnot(all(names(fixed) %in% names(after)), !anyDuplicated(key(fixed)),
            !anyNA(fixed), all(fixed$valid_votes > 0), all(fixed$eligible_voters > 0),
            all(fixed$turnout >= 0 & fixed$turnout <= 1))
  affected <- after$state == "12" & after$election_year == as.integer(yr)
  old <- after[affected, ]
  replacement <- after[rep(NA_integer_, nrow(fixed)), ]
  for (nm in names(fixed)) replacement[[nm]] <- fixed[[nm]]
  for (nm in grep("^flag_", names(replacement), value = TRUE)) replacement[[nm]] <- 0
  insertion <- min(which(affected))
  after <- rbind(after[seq_len(nrow(after)) < insertion & !affected, ], replacement,
                 after[seq_len(nrow(after)) >= insertion & !affected, ])
  fields <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")
  summaries[[yr]] <- data.frame(year = yr, field = fields, old_rows = nrow(old),
    new_rows = nrow(fixed), before = colSums(old[fields], na.rm = TRUE),
    after = colSums(fixed[fields]), old_missing = colSums(is.na(old[fields])),
    new_missing = colSums(is.na(fixed[fields])))
  fwrite(fit$counts, file.path(audit, paste0("bb", yr, "_municipal_counts.tsv")), sep = "\t")
  # Published party shares reconstruct the source counts, including 'other'.
  if (yr == "1994") {
    expected <- fit$counts
    expected$other <- rowSums(expected[c("buerger", "dsu", "graue", "odp", "uwvb")])
    expected$einzelbewerber <- 0
  } else { expected <- fit$counts; expected$other <- 0 }
  parties <- setdiff(names(fixed), c("ags", "state", "election_year", "election_date",
                                    fields, "turnout", "cdu_csu"))
  for (p in parties) stopifnot(max(abs(fixed[[p]] * fixed$valid_votes - expected[[p]])) < 1e-7)
  stopifnot(max(abs(rowSums(fixed[parties]) - 1)) < 1e-12)
  # Full official totals provide independent controls. 1990 requires removing
  # MV ballots and restoring Brunow/Dambeck, even before excluding postal votes.
  p <- if (yr == "1990") c(spd=487134,cdu=374572,linke_pds=170804,buendnis_90=81725,
    gruene=36238,fdp=84501,chr_l=2721,dbu=4362,dfp=1823,dsu=12552,rep=14631,
    domowina=1177,npd=1666) else c(spd=580422,cdu=200700,fdp=23541,freie_wahler=2425,
    gruene=31033,buerger=10405,dsu=1932,graue=3199,rep=12140,odp=1354,linke_pds=200628,uwvb=4240)
  all_votes <- colSums(fit$counts[names(p)]) + colSums(fit$stations[fit$stations$postal, names(p)])
  stopifnot(identical(unname(all_votes), unname(as.numeric(p))))
  reconciliations[[yr]] <- data.frame(year=yr, party=names(p),
    in_person=colSums(fit$counts[names(p)]),
    postal=colSums(fit$stations[fit$stations$postal,names(p)]),
    reference_total=unname(p), difference=unname(all_votes-p))
}
rownames(after) <- NULL
unchanged <- function(x) !(x$state == "12" & x$election_year %in% c(1990,1994))
a <- before[unchanged(before), ]; b <- after[unchanged(after), names(before)]; rownames(a) <- rownames(b) <- NULL
stopifnot(identical(a,b), !anyDuplicated(paste(after$ags, after$election_date)),
          identical(lapply(before,class), lapply(after[names(before)],class)),
          all(is.na(after$domowina[unchanged(after)])))
fwrite(do.call(rbind,summaries),file.path(audit,"before_after.tsv"),sep="\t")
fwrite(do.call(rbind,reconciliations),file.path(audit,"party_reconciliation.tsv"),sep="\t")

# Run existing maintained harmonizers on both election years across ALL states:
# some BB municipalities cross today's state border, and target units can also
# contain historical MV contributions. No geographically restricted splice.
scripts <- c("21"="02b_state_harm_21.R", "23"="04_state_harm_23.R", "25"="05_state_harm_25.R")
dir.create(file.path(staging,"code"),recursive=TRUE,showWarnings=FALSE)
for (d in c("shared","state_elections")) {
  stopifnot(file.copy(file.path("code",d),file.path(staging,"code"),recursive=TRUE))
}
for (d in c("crosswalks","covars_municipality")) {
  dir.create(file.path(staging,"data",d),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(file.path(dependencies,"data",d,"final"),
                      file.path(staging,"data",d),recursive=TRUE))
}
stage_final <- file.path(staging,"data/state_elections/final")
dir.create(stage_final,recursive=TRUE,showWarnings=FALSE)
stage_source <- after[after$election_year %in% c(1990,1994),]
if (resume) {
  previous_source <- readRDS(file.path(stage_final, "state_unharm.rds"))
  stopifnot(identical(stage_source, previous_source))
} else {
  saveRDS(stage_source,file.path(stage_final,"state_unharm.rds"))
}
runner <- file.path(staging,"run.R")
writeLines(c("library(tidyverse)","library(data.table)","library(haschaR)","library(conflicted)",
             "conflict_prefer('first','dplyr')", "conflict_prefer('last','dplyr')",
             "args <- commandArgs(trailingOnly=TRUE)","setwd(args[1])", "source(args[2])"),runner)
outputs <- list(state_unharm=after)
changes <- list()
for (boundary in names(scripts)) {
  stem <- paste0("state_harm_",boundary)
  log <- file.path(audit,paste0(stem,".log"))
  status <- if (resume) 0L else system2(file.path(R.home("bin"),"Rscript"),
    c("--vanilla",shQuote(runner),shQuote(staging),shQuote(file.path("code/state_elections",scripts[[boundary]]))),
    stdout=log,stderr=log)
  stopifnot(status == 0L)
  old <- readRDS(file.path(baseline,paste0(stem,".rds")))
  if (!"domowina" %in% names(old)) old$domowina <- NA_real_
  new <- readRDS(file.path(stage_final,paste0(stem,".rds")))
  extra <- setdiff(names(new),names(old))
  stopifnot(all(vapply(new[extra],function(v)all(is.na(v)),logical(1))),
            all(names(old) %in% names(new)))
  new <- new[names(old)]
  stopifnot(!anyDuplicated(key(new)),!anyNA(new$ags))
  # All other years remain byte-for-byte equal at the R-object level.
  affected <- old$election_year %in% c(1990,1994)
  fixed <- rbind(old[!affected,],new)
  fixed <- fixed[order(fixed$ags,fixed$election_year),]
  rownames(fixed) <- NULL
  a <- old[!affected,];b<-fixed[!fixed$election_year%in%c(1990,1994),]
  a<-a[order(key(a)),];b<-b[order(key(b)),];rownames(a)<-rownames(b)<-NULL
  stopifnot(identical(a,b),!anyDuplicated(key(fixed)))
  outputs[[stem]] <- fixed
  changes[[stem]] <- data.frame(dataset=stem,before_rows=nrow(old),after_rows=nrow(fixed))
}
final <- "data/state_elections/final"
for (stem in names(outputs)) {
  x <- outputs[[stem]]
  saveRDS(x,file.path(final,paste0(stem,".rds")),compress="gzip")
  fwrite(x,file.path(final,paste0(stem,".csv")))
  stopifnot(identical(x,readRDS(file.path(final,paste0(stem,".rds")))))
  csv <- fread(file.path(final,paste0(stem,".csv")),colClasses=c(ags="character",state="character"),
                na.strings=c("","NA"),data.table=FALSE)
  for (nm in names(x)) {
    stopifnot(identical(unname(is.na(x[[nm]])),unname(is.na(csv[[nm]]))))
    if (is.numeric(x[[nm]])) stopifnot(isTRUE(all.equal(as.numeric(x[[nm]]),as.numeric(csv[[nm]]),tolerance=1e-12)))
    else stopifnot(identical(unname(as.character(x[[nm]])),unname(as.character(csv[[nm]]))))
  }
}
fwrite(do.call(rbind,changes),file.path(audit,"harmonized_scope.tsv"),sep="\t")
write_json(list(baseline_sha256=digest(file=file.path(baseline,"state_unharm.rds"),algo="sha256"),
  unchanged_unharm_rows=nrow(a <- before[unchanged(before),]),
  repaired_municipalities=c(`1990`=1792,`1994`=1693),
  source_party_reconciliations=nrow(do.call(rbind,reconciliations)),
  output_sha256=setNames(lapply(names(outputs),function(s)digest(file=file.path(final,paste0(s,".rds")),algo="sha256")),names(outputs))),
  file.path(audit,"verification.json"),pretty=TRUE,auto_unbox=TRUE)
cat("PASS: source counts, historical geography, unchanged unrelated elections, and CSV/RDS round trips.\n")
