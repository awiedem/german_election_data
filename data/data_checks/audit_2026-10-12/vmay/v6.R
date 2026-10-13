suppressMessages(library(dplyr))
f<-function(x) readRDS(file.path("data/mayoral_elections/final",paste0(x,".rds")))
h<-f("mayoral_harm"); print(names(h)); 
a<-h%>%filter(flag_aggregated==1); cat(nrow(a),"\n")
d<-abs(a$winner_voteshare-a$winner_votes/a$valid_votes); cat(mean(d,na.rm=T),max(d,na.rm=T),sum(d>.05,na.rm=T),"\n")
print(a%>%filter(ags_21%in%c("03157009","15001000"))%>%select(ags_21,election_date,valid_votes,winner_votes,winner_voteshare)%>%head(6))
