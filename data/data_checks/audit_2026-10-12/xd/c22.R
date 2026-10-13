suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
for(f in c("state_unharm","state_harm_21","state_harm_25")){s<-readRDS(paste0("data/state_elections/final/",f,".rds"))
x<-s%>%group_by(state,election_year)%>%summarise(ev=sum(eligible_voters,na.rm=T),nv=sum(number_voters,na.rm=T),vv=sum(valid_votes,na.rm=T),inv=sum(invalid_votes,na.rm=T),r=vv/nv,rinv=(vv+inv)/nv,.groups="drop")
cat("\n",f,"\n");print(as.data.frame(x%>%filter(r>1.02|(nv>0&r<.9))),digits=4)}
s<-readRDS("data/state_elections/final/state_unharm.rds")
b<-s%>%filter(state=="09",election_year==2018);print(summary(b$valid_votes/b$number_voters));print(summary(b$turnout))
print(head(as.data.frame(b%>%select(ags,eligible_voters,number_voters,valid_votes,invalid_votes,turnout,csu)),3))
cat("flag counts:");print(table(b$flag_no_valid_votes,useNA="a"))
