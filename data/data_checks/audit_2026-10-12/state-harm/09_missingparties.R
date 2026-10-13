suppressMessages({library(dplyr)}); options(width=250)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); h25 <- readRDS(paste0(d,"state_harm_25.rds"))
for (p in c("dg_bhe","llsh","uwg")) { i <- which(!is.na(u[[p]])); cat(p, "nonNA rows:", length(i), "; state-years:", paste(unique(paste(u$state[i],u$election_year[i])),collapse=", "), "; sum votes:", sum(u[[p]][i]*u$valid_votes[i],na.rm=T), " max share", max(u[[p]][i],na.rm=T), "\n") }
# Compare totals for these state-years in h25 vs u: 'other'
for (se in list(c("01",1983),c("01",1996),c("01",2000),c("01",2005),c("01",2009),c("01",2012),c("01",2017),c("01",2022))) {
 a <- u %>% filter(state==se[1], election_year==as.numeric(se[2])); b <- h25 %>% filter(state==se[1], election_year==as.numeric(se[2]))
 if(nrow(a)==0) next
 cat(se, "u other votes:", round(sum(a$other*a$valid_votes,na.rm=T)), " h25 other votes:", round(sum(b$other*b$valid_votes,na.rm=T)), "\n")
}
