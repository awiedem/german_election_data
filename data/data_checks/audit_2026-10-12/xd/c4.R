suppressMessages(library(dplyr))
setwd("/Users/hanno/Documents/GitHub/german_election_data")
for(n in c("federal_muni_unharm","federal_muni_harm_21","federal_muni_harm_25")){
d<-readRDS(paste0("data/federal_elections/municipality_level/final/",n,".rds"))
cat("\n",n,"\n");print(as.data.frame(d%>%group_by(election_year)%>%summarise(ev=sum(eligible_voters,na.rm=T)/1e6,ev0=sum(eligible_voters_orig,na.rm=T)/1e6,nv=sum(number_voters,na.rm=T)/1e6,share_ev_gt_orig=mean(eligible_voters>eligible_voters_orig*1.001,na.rm=T), med_ratio=median(eligible_voters/eligible_voters_orig,na.rm=T), turn_med=median(turnout,na.rm=T))),digits=4)}
