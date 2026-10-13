suppressMessages({library(dplyr);library(bundeslaendeR)}); options(width=200)
u <- readRDS("data/state_elections/final/state_unharm.rds")
h25 <- readRDS("data/state_elections/final/state_harm_25.rds")
h21 <- readRDS("data/state_elections/final/state_harm_21.rds")
bl <- as.data.frame(ltw_elections) %>% mutate(election_date=as.Date(election_date))
print(bl%>%filter(state=="BE",election_date==as.Date("2021-09-26"))%>%select(partyname_short,party_vote_count,party_vshare)%>%head(10))
g<-function(st,code,y,col="gruene"){d<-u%>%filter(state==code,election_year==y);sh<-sum(d[[col]]*d$valid_votes,na.rm=T)/sum(d$valid_votes,na.rm=T)
 b<-bl%>%filter(state==st,format(election_date,"%Y")==y,grepl("GR",partyname_short))%>%select(party_vshare);cat(st,y,"gerda",sh,"bl",b$party_vshare,"\n")}
g("BW","08",1992); g("HE","06",1991)
# SH-05
fr<-c("rep","npd","afd","die_republikaner_rep")
cat("rows all comps NA & far_right==0:", sum(rowSums(!is.na(h25[,intersect(fr,names(h25))]))==0 & h25$far_right==0,na.rm=T),"of",nrow(h25),"\n")
cat("BY npd 1994-2013 share:\n"); print(h25%>%filter(state=="09",election_year%in%c(1994,1998,2003,2008,2013))%>%group_by(election_year)%>%summarise(npd=sum(npd,na.rm=T),nonNA=sum(!is.na(npd))))
print(bl%>%filter(state=="BY",election_date>=as.Date("1994-01-01"),election_date<=as.Date("2003-12-31"),grepl("REP|NPD",partyname_short))%>%select(election_date,partyname_short,party_vshare))
# far_right def
cat(grep("far_right",readLines("code/state_elections/01b_state_unharm_raw.R"),value=T)[1:5],sep="\n")
# SH-10
cat("unharm nv>el:",sum(u$number_voters>u$eligible_voters,na.rm=T)," turnout>1:",sum(u$turnout>1,na.rm=T)," turnout NA among:",sum(u$number_voters>u$eligible_voters & is.na(u$turnout),na.rm=T),"\n")
print(h25%>%filter(ags=="14524170")%>%select(ags,election_year,eligible_voters,number_voters,turnout))
cat("h25 nv<valid:",sum(h25$number_voters<h25$valid_votes,na.rm=T),"\n")
# SH-11
cat("flag n:",sum(h25$flag_other_party_residual==1,na.rm=T),"tot>1.001:",sum(h25$total_vote_share>1.001,na.rm=T),"\n")
# SH-12
l<-readRDS("data/state_elections/final/state_harm.rds"); cat("legacy",dim(l),"NA ags:",sum(is.na(l$ags)),"\n")
for(st in c("03")) {a<-l%>%filter(state==st,election_year==1998)%>%summarise(v=sum(valid_votes,na.rm=T));b<-h21%>%filter(state==st,election_year==1998)%>%summarise(v=sum(valid_votes,na.rm=T));cat("NI1998",a$v/b$v-1,"\n")}
# SH-16
d<-u%>%filter(ags=="08337097",election_year==1956);print(d%>%select(valid_votes,cdu,fdp,spd,gb_bhe,kpd,bdd)); 
# SH-17
cat("NA ags_name h25:",sum(is.na(h25$ags_name)),"h21:",sum(is.na(h21$ags_name_21)),"\n")
print(grep("name",names(h25),value=T))
# SH-19
cat("h25 |v+inv-nv|>2:",sum(abs(h25$valid_votes+h25$invalid_votes-h25$number_voters)>2 & !h25$state%in%c("09","02","04"),na.rm=T),"\n")
print(unique(h25$state_name))
cat(class(u$election_year),class(h25$election_year),"\n")
cat(nrow(u),nrow(h21),nrow(readRDS("data/state_elections/final/state_harm_23.rds")),nrow(h25),"\n")
