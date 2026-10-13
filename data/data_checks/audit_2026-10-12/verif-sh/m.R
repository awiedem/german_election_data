suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
sh<-function(s,y,p){d<-x%>%filter(state==s,election_year==y);round(100*sum(d[[p]]*d$valid_votes,na.rm=T)/sum(d$valid_votes,na.rm=T),2)}
cat(sh("12",1994,"spd"),sh("05",1970,"cdu"),sh("12",1990,"spd"),sh("01",1983,"cdu"),"\n")
e<-bundeslaendeR::ltw_elections%>%filter(state%in%c("BB","NW","SH"),format(election_date,"%Y")%in%c("1994","1970","1983","1990"),partyname_short%in%c("SPD","CDU"))%>%mutate(p=round(100*party_vote_count/valid_votes,2))%>%select(state,election_date,partyname_short,p);print(e)
