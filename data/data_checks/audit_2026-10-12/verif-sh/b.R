suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
for(y in c(2021,2023)){d<-x%>%filter(state=="11",election_year==y)
 pc<-names(d)[sapply(d,function(c) is.numeric(c)&&any(!is.na(c)))]
 pc<-setdiff(pc,c("election_year","eligible_voters","number_voters","valid_votes","invalid_votes","turnout"))
 r<-sapply(pc,function(p) sum(d[[p]]*d$valid_votes,na.rm=T)/sum(d$valid_votes))
 print(round(sort(r[r>0.001],decreasing=T),4)); cat("sum",sum(r,na.rm=T),"\n")}
