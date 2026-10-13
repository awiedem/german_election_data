suppressMessages(library(dplyr))
setwd("/Users/hanno/Documents/GitHub/german_election_data")
d<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
x<-d%>%group_by(state,election_year)%>%summarise(ev=sum(eligible_voters,na.rm=T),ev0=sum(eligible_voters_orig,na.rm=T),r=ev/ev0,.groups="drop")%>%filter(r>1.04)
print(as.data.frame(x),digits=4)
# 2021 RLP municipality-level: examples
y<-d%>%filter(election_year==2021,state=="07")%>%mutate(r=eligible_voters/eligible_voters_orig)%>%arrange(desc(r))%>%select(ags,ags_name,eligible_voters,eligible_voters_orig,number_voters,number_voters_orig,turnout,r)
print(head(as.data.frame(y),8)); cat("n RLP2021 r>1.5:",sum(y$r>1.5,na.rm=T),"of",nrow(y),"\n")
cat("turnout>1:\n");print(d%>%group_by(election_year)%>%summarise(n=sum(turnout>1,na.rm=T),na=sum(is.na(turnout)),mx=max(turnout,na.rm=T)))
