suppressMessages({library(dplyr);library(tidyr)}); options(width=250, dplyr.print_max=300)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); w <- readRDS(paste0(d,"ltw_wkr_unharm.rds")); wl <- readRDS(paste0(d,"ltw_wkr_unharm_long.rds"))
cat("wide: dim",dim(w),"\n"); print(table(w$stimme, useNA="ifany")); print(w %>% count(state,election_year,stimme) %>% group_by(state) %>% summarise(n_el=n_distinct(election_year), yrs=paste(range(election_year),collapse="-")) %>% as.data.frame())
cat("key dup (state,year,date,wkr_nr,stimme):", sum(duplicated(w[c("state","election_year","election_date","wkr_nr","stimme")])),"\n")
print(w %>% filter(duplicated(w[c("state","election_year","wkr_nr","stimme")])|duplicated(w[c("state","election_year","wkr_nr","stimme")],fromLast=TRUE)) %>% count(state,election_year,election_date) %>% as.data.frame())
cat("state class", class(w$state), unique(w$state)[1:5], "; long state", unique(wl$state)[1:5], unique(wl$state_abbr)[1:5], "\n")
cat("long stimme:"); print(table(wl$stimme,useNA="ifany"))
cat("long party NA:", sum(is.na(wl$party)), " votes NA:", sum(is.na(wl$votes)), " vote_share NA:", sum(is.na(wl$vote_share)), "\n")
