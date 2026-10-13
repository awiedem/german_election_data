# Source and released-output checks; full bundeslaendeR comparison without a
# 50-million-row long intermediate. Run from repository root after the rebuild.
set.seed(20260928)
library(readxl)
library(data.table)
library(bundeslaendeR)
library(digest)
source("code/state_elections/bb_verified.R")
source("data/state_elections/derived/bb_1990_1994/bb1990_audit_geography.R")
expected_hashes <- c(`1990`="e164fd8bd6a28acd5b78ca1f80b28cd0a1cb321fa96374b249d33b68d839eb1d",
                     `1994`="4ced919d6827d890b618e5840e6c4020a19b1a1e8a65e724a124466c6f24e718")
for (year in names(expected_hashes)) {
  x <- gerda_bb_station_source(as.integer(year))
  stopifnot(digest(file=attr(x,"source_path"),algo="sha256") == expected_hashes[[year]])
}
x <- readRDS("data/state_elections/final/state_unharm.rds")
stopifnot(!anyDuplicated(paste(x$ags,x$election_date)))
for (year in c(1990L,1994L)) {
  expected <- if (year==1990L) gerda_bb_1990_verified()$data else gerda_bb_1994_verified()$data
  actual <- x[x$state=="12" & x$election_year==year,]
  actual <- actual[match(expected$ags,actual$ags),names(expected)]
  rownames(actual) <- rownames(expected) <- NULL
  for (nm in names(expected)) stopifnot(isTRUE(all.equal(actual[[nm]],expected[[nm]],check.attributes=FALSE)))
}
# 1994 codes are independently checked against the BBSR 1994 municipality list.
raw <- gerda_bb_station_source(1994L)
cw <- read_excel("data/crosswalks/raw/gmd_auf_2021/ref-gemeinden-umrech-2021-1990-1999.xlsx",sheet="1994")
stopifnot(all(gerda_bb_1994_verified()$data$ags %in% as.character(cw[[1]])))

meta <- c("ags","election_year","election_date","state","state_name","ags_name",
          "ags_name_21","county","eligible_voters","number_voters","valid_votes","invalid_votes","turnout")
excluded <- grep("^flag_|^perc_total|^total_vote_share|^far_|^cdu_csu$|_name$|area_|pop|employees",names(x),value=TRUE)
parties <- setdiff(names(x),c(meta,excluded))
map <- as.data.frame(bundeslaendeR::link_gerda)
ref <- as.data.frame(bundeslaendeR::ltw_elections)
stopifnot(!anyDuplicated(paste(map$state_gerda,map$election_date,map$partyname_gerda)),
          !anyDuplicated(paste(ref$state,ref$election_date,ref$partyname_short)))
rows <- list()
for (ix in split(seq_len(nrow(x)),paste(x$state,x$election_date))) {
  d <- x[ix,]
  # Preserve the existing diagnostic's treatment of unavailable count weights,
  # but explicitly record how many rows lack valid counts for each comparison.
  totals <- vapply(d[parties],function(p)sum(ifelse(p>0,p,0)*d$valid_votes,na.rm=TRUE),numeric(1))
  if (sum(totals)==0) next
  hit <- which(map$state_gerda==d$state[1] & as.character(map$election_date)==as.character(d$election_date[1]))
  if (!length(hit)) next
  m <- map[hit,];m <- m[m$partyname_gerda %in% names(totals),]
  idx <- match(paste(m$state,m$election_date,m$partyname_short),paste(ref$state,ref$election_date,ref$partyname_short))
  m$gerda_pct <- unname(totals[m$partyname_gerda])/sum(totals)*100
  m$reference_pct <- ref$party_vshare[idx]*100
  m$error_pp <- m$gerda_pct-m$reference_pct
  m$municipalities_missing_valid_votes <- sum(is.na(d$valid_votes))
  rows[[length(rows)+1L]] <- m
}
comparison <- rbindlist(rows)
audit <- "data/data_checks/bb_1990_1994"
dir.create(audit,recursive=TRUE,showWarnings=FALSE)
fwrite(comparison,file.path(audit,"bundeslaendeR_comparison.tsv"),sep="\t")
deviations <- comparison[!is.na(error_pp) & abs(error_pp)>=1]
fwrite(deviations,file.path(audit,"bundeslaendeR_deviations.tsv"),sep="\t")
print(deviations)
bb <- comparison[state_gerda=="12" & as.character(election_date)%in%c("1990-10-14","1994-09-11")]
stopifnot(all(abs(bb$error_pp)<1),all(bb$municipalities_missing_valid_votes==0))
cat("PASS: source hashes, geography, repaired output equality, and no BB1990/94 deviations >=1pp.\n")
