suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
print(x%>%group_by(state)%>%summarise(mx=max(election_year)))
d<-x%>%filter(state=="08",election_year==2026);print(c(nrow(d),sum(d$valid_votes),sum(d$number_voters),sum(d$eligible_voters,na.rm=T)))
