suppressMessages({library(dplyr)}); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds")
sh <- function(st, yr, cols=NULL) { x<-u%>%filter(state==st, election_year==yr); vv<-sum(x$valid_votes,na.rm=T); pc<-setdiff(names(x)[sapply(x,is.numeric)], c("election_year","eligible_voters","number_voters","valid_votes","invalid_votes","turnout","flag_briefwahl_only","flag_naive_turnout_above_1","flag_no_valid_votes"))
  w <- sapply(pc,function(p) if(all(is.na(x[[p]]))) NA else 100*sum(x[[p]]*x$valid_votes,na.rm=T)/vv); w<-sort(w[!is.na(w)&w>0.4],decreasing=TRUE); cat("\n",st,yr,"vv",vv,"\n"); print(round(w,2)) }
sh("04",2003); sh("09",1990); sh("03",1994); sh("04",2015); sh("05",2005); sh("10",2022); sh("04",2011)
