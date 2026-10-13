suppressMessages({library(dplyr);library(tidyr);library(data.table)}); options(width=250)
d <- "data/state_elections/final/"
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character"); pc <- sc$column[sc$dataset=="state_harm_25" & sc$role %in% c("party_share","residual_share")]
tot <- function(x, cols) { x <- as.data.table(x); x <- x[election_year>=1990]; out <- x[, lapply(.SD, function(s) sum(s*valid_votes, na.rm=TRUE)), by=.(state,election_year), .SDcols=cols]; vv <- x[, .(vv=sum(valid_votes,na.rm=TRUE)), by=.(state,election_year)]; merge(out, vv, by=c("state","election_year")) }
u <- readRDS(paste0(d,"state_unharm.rds")); cu <- intersect(pc, names(u)); tu <- tot(u, cu)
for (n in c("21","23","25")) {
  h <- readRDS(sprintf("%sstate_harm_%s.rds",d,n)); th <- tot(h, cu)
  m <- merge(melt(tu, id.vars=c("state","election_year","vv"), variable.name="party", value.name="u"), melt(th, id.vars=c("state","election_year","vv"), variable.name="party", value.name="h"), by=c("state","election_year","party"), suffixes=c("_u","_h"))
  m <- m[vv_u>0]; m[, dif := h-u]; m[, pp := 100*dif/vv_u]
  cat("\n## harm",n,": cells:", nrow(m), "; |diff|>0.05pp:", sum(abs(m$pp)>0.05), "; >0.2pp:", sum(abs(m$pp)>0.2), "; max", max(abs(m$pp)), "\n")
  print(as.data.frame(m[abs(pp)>0.03][order(-abs(pp))][1:8]))
}
