suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
h<-readRDS("data/european_elections/final/european_muni_harm.rds")
print(names(u)[1:20])
u$st<-substr(u$ags,1,2)
print(u%>%group_by(election_year)%>%summarise(el=sum(eligible_voters,na.rm=T),v=sum(number_voters,na.rm=T),va=sum(valid_votes,na.rm=T)))
print(u%>%filter(st=="03")%>%group_by(election_year)%>%summarise(el=sum(eligible_voters,na.rm=T),v=sum(number_voters,na.rm=T),va=sum(valid_votes,na.rm=T)))
# EU-3
print(u%>%group_by(election_year)%>%summarise(n=sum(number_voters>eligible_voters,na.rm=T),mx=max(number_voters/eligible_voters,na.rm=T)))
print(u%>%filter(ags%in%c("16076014","16074055"))%>%select(ags,election_year,eligible_voters,number_voters))
# EU-4
pc<-setdiff(names(u)[sapply(u,is.numeric)],c("election_year","eligible_voters","number_voters","valid_votes","invalid_votes","turnout"))
print(head(pc,60))
# EU-5
h$d<-h$valid_votes+h$invalid_votes-h$number_voters
print(h%>%group_by(election_year)%>%summarise(n=sum(abs(d)>0,na.rm=T),mx=max(abs(d),na.rm=T)))
u$d<-u$valid_votes+u$invalid_votes-u$number_voters
print(sum(abs(u$d)>1,na.rm=T))
# EU-2
for(y in c(2009,2019,2024)) cat(y,n_distinct(h$ags[h$election_year==y]),"\n")
a<-unique(h$ags[h$election_year==2019]);b<-unique(h$ags[h$election_year==2024])
cat(length(setdiff(a,b)),length(setdiff(b,a)),"\n")
