suppressMessages(library(dplyr))
w<-readRDS("data/state_elections/final/ltw_wkr_unharm.rds")
print(w%>%filter(state%in%c("07","08","06","05","10","12","14"),election_year%in%c(1979,1983,1987,1991,1972,1980,1974,1978,1975,1970,1999,2004))%>%group_by(state,election_year,stimme)%>%summarise(n=n(),va=sum(valid_votes,na.rm=T),el=sum(eligible_voters,na.rm=T),.groups="drop")%>%as.data.frame())
