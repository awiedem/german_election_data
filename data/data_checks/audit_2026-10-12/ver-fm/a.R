suppressMessages(library(dplyr))
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds")); h<-readRDS(paste0(d,"federal_muni_harm_21.rds")); h25<-readRDS(paste0(d,"federal_muni_harm_25.rds"))
r<-readRDS(paste0(d,"federal_muni_raw.rds"))
cat("FM1\n")
k<-u%>%filter(ags=="01001000",election_year==2021)%>%select(cdu,number_voters,valid_votes,eligible_voters,eligible_voters_orig,turnout);print(k)
k<-h%>%filter(ags=="01001000",election_year==2021)%>%select(cdu,number_voters,valid_votes,total_votes,eligible_voters,turnout);print(k)
for(y in c(2005,2017,2021,2025)){
 a<-sum(u$cdu[u$election_year==y]*u$number_voters[u$election_year==y],na.rm=T)
 hh<-h[h$election_year==y,]; if(y==2025) hh<-h25[h25$election_year==y,]
 cat(y,a,sum(hh$cdu*hh$number_voters,na.rm=T),sum(hh$cdu*hh$total_votes,na.rm=T),sum(hh$cdu*hh$valid_votes,na.rm=T),"\n")}
cat("FM2\n")
for(y in c(1994,1998,2002,2021)){cat(y,sum(r$valid_votes[r$election_year==y],na.rm=T),sum(u$valid_votes[u$election_year==y],na.rm=T),"\n")}
print(r%>%filter(election_year==1998,substr(ags,1,2)=="12",substr(ags,6,8)=="999")%>%select(ags,valid_votes,number_voters)%>%head())
bb<-function(df,y)df%>%filter(election_year==y,substr(ags,1,2)=="12")
cat("BB98 turnout unharm",sum(bb(u,1998)$number_voters)/sum(bb(u,1998)$eligible_voters_orig),"raw",sum(bb(r,1998)$number_voters,na.rm=T)/sum(bb(r,1998)$eligible_voters,na.rm=T),"\n")
cat("FM3\n")
for(y in c(1980,1990,2021,2025)){x<-u[u$election_year==y,];cat(y,sum(x$eligible_voters,na.rm=T),sum(x$eligible_voters_orig,na.rm=T),"\n")}
print(u%>%filter(ags=="01001000",election_year==1980)%>%select(eligible_voters,eligible_voters_orig,number_voters,turnout))
x<-u%>%filter(election_year==2021,!is.na(turnout));cat("turnout==nv/ev share 2021:",mean(abs(x$turnout-x$number_voters/x$eligible_voters)<1e-4,na.rm=T),"\n")
