suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
for(f in c("state_harm_21","state_harm_25","state_harm")){s<-readRDS(paste0("data/state_elections/final/",f,".rds"))
x<-s%>%filter((state=="07"&election_year<2021)|(state=="09"&election_year<2018))%>%group_by(state,election_year)%>%summarise(n=n(),ev_na=sum(is.na(eligible_voters)),ev_0=sum(eligible_voters==0,na.rm=T),nv_na=sum(is.na(number_voters)),nv0=sum(number_voters==0,na.rm=T),t_na=sum(is.na(turnout)),t0=sum(turnout==0,na.rm=T),.groups="drop")
cat("\n",f,"\n");print(as.data.frame(x))}
