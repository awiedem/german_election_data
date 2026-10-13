suppressMessages({library(dplyr)}); options(width=250)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds"))
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character")
for (n in c("u","h21","h23","h25")) {
  x <- if(n=="u") u else readRDS(sprintf("%sstate_harm_%s.rds",d,sub("h","",n)))
  ds <- if(n=="u") "state_unharm" else paste0("state_harm_",sub("h","",n))
  pc <- sc$column[sc$dataset==ds & sc$role=="party_share"]
  stopifnot(all(pc %in% names(x)))
  M <- as.matrix(x[pc]); 
  nonNAcount <- rowSums(!is.na(M)); S <- rowSums(M,na.rm=TRUE); S[nonNAcount==0] <- NA
  cat("\n####",n,"rows",nrow(x),"; rows with ALL party shares NA:",sum(is.na(S)),"\n")
  print(x[is.na(S),] %>% count(state,election_year) %>% as.data.frame())
  cat("share range: min",min(M,na.rm=T)," max",max(M,na.rm=T),"; negatives:",sum(M<0,na.rm=T)," >1:",sum(M>1+1e-9,na.rm=T),"\n")
  x$S <- S
  # other check
  oth <- pmax(1-S,0); cat("other vs pmax(1-sum): max abs diff", max(abs(x$other-oth),na.rm=T), " n diff>1e-6:", sum(abs(x$other-oth)>1e-6,na.rm=T), "\n")
  cat("sum parties >1.001:", sum(S>1.001,na.rm=T), " <0.999:", sum(S<0.999,na.rm=T), " (of", sum(!is.na(S)),")\n")
  if ("total_vote_share" %in% names(x)) { cat("tvs vs S max diff", max(abs(x$total_vote_share-x$S),na.rm=T),"; flag_other_party_residual==1:", sum(x$flag_other_party_residual==1,na.rm=T), "; flag inconsistent with tvs:", sum((x$flag_other_party_residual==1)!=(x$total_vote_share>1.001|x$total_vote_share<0.999),na.rm=T),"\n") }
  cat("Sum>1.001 by state-year (n rows):\n"); print(x %>% filter(S>1.001) %>% count(state,election_year,sort=TRUE) %>% head(15) %>% as.data.frame())
  cat("Sum<0.99 by state-year (n rows, rows)\n"); print(x %>% filter(S<0.99) %>% count(state,election_year,sort=TRUE) %>% head(25) %>% as.data.frame())
}
