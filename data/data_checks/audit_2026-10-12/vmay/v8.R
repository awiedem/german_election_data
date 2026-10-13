suppressMessages(library(dplyr))
f<-function(x) readRDS(file.path("data/mayoral_elections/final",paste0(x,".rds")))
p<-f("mayor_panel")%>%arrange(ags,election_date)%>%group_by(ags)%>%mutate(pp=lag(person_id),pw=lag(winner_party))%>%ungroup()
s<-p%>%filter(!is.na(person_id),person_id==pp); cat(nrow(s),sum(s$party_switch==1,na.rm=T),"\n")
b<-s%>%filter(state=="09");cat(nrow(b),sum(b$party_switch==1,na.rm=T),"\n")
print(s%>%filter(party_switch==1)%>%count(pw,winner_party,sort=T)%>%head(8))
# also check party_switch definition
x<-p%>%filter(!is.na(pw),!is.na(winner_party)); cat(mean((x$pw!=x$winner_party)==(x$party_switch==1),na.rm=T),"\n")
# incumbent check
i<-p%>%filter(is_incumbent==1,!is.na(pp),person_id!=pp); cat("inc diff",nrow(i),"\n"); print(table(i$state))
