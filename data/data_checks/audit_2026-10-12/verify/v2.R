suppressMessages(library(dplyr))
u <- readRDS("data/state_elections/final/state_unharm.rds")
h25 <- readRDS("data/state_elections/final/state_harm_25.rds")
w <- readRDS("data/state_elections/final/ltw_wkr_unharm.rds")
print(table(w$stimme))
ws <- function(st,y,sv) w %>% filter(state==st,election_year==y,stimme==sv) %>% summarise(n=n(),el=sum(eligible_voters,na.rm=T),v=sum(number_voters,na.rm=T),va=sum(valid_votes,na.rm=T))
print(ws("12",1999,"zweitstimme")); print(ws("14",2004,"zweitstimme")); print(ws("04",2003,"zweitstimme")); print(ws("04",1999,"zweitstimme"))
for(y in c(1979,1991,1996)) print(ws("07",y,"zweitstimme"))
# SSW
for(y in c(2000,2005,2009,2012)){ d<-u%>%filter(state=="01",election_year==y); cat(y,"ssw nonNA",sum(!is.na(d$ssw)),"ssw votes",sum(d$ssw*d$valid_votes,na.rm=T)/sum(d$valid_votes),"other",sum(d$other*d$valid_votes,na.rm=T)/sum(d$valid_votes),"\n")}
print(grep("ssw|npd",names(u),value=T))
# BE 2021 greens
d<-u%>%filter(state=="11",election_year==2021); cat("BE gruene", sum(d$gruene*d$valid_votes), sum(d$gruene*d$valid_votes)/sum(d$valid_votes),"\n")
x<-w%>%filter(state=="11",election_year==2021,stimme=="zweitstimme"); cat("wkr BE", sum(x$gruene*x$valid_votes,na.rm=T), sum(x$gruene*x$valid_votes,na.rm=T)/sum(x$valid_votes),"\n")
# far_right
print(h25%>%filter(state=="09",election_year %in% c(1994,1998,2003))%>%group_by(election_year)%>%summarise(rr=weighted.mean(die_republikaner_rep,valid_votes,na.rm=T),rep=sum(rep,na.rm=T),fr=mean(far_right,na.rm=T)))
cmp<-intersect(c("rep","npd","afd","die_republikaner_rep"),names(h25)); print(cmp)
fr<-grep("far_right",names(h25),value=T);print(fr)
# ST greens
for(y in c(1990,1994)){d<-h25%>%filter(state=="15",election_year==y); cat(y,intersect(c("gruene","grue_nf","bue90_gruene"),names(d)),"\n"); for(c in intersect(c("gruene","grue_nf","bue90_gruene"),names(d))) cat(c, sum(d[[c]]*d$valid_votes,na.rm=T)/sum(d$valid_votes),"\n")}
# turnout>1
cat("harm25 turnout>1:",sum(h25$turnout>1,na.rm=T),"\n")
print(h25%>%filter(ags_25=="14524170"|ags=="14524170")%>%select(any_of(c("ags","ags_25","election_year","eligible_voters","number_voters","turnout")))%>%head())
cat("NA ags_name h25:\n"); print(table(h25$election_year[is.na(h25$ags_name)]))
cat("nrow",nrow(u),nrow(h25),nrow(readRDS("data/state_elections/final/state_harm_21.rds")),nrow(readRDS("data/state_elections/final/state_harm_23.rds")),"\n")
cat("flag other:", mean(h25$flag_other_party_residual==1,na.rm=T), sum(h25$flag_other_party_residual==1,na.rm=T),"\n")
print(class(u$election_year)); print(class(h25$election_year))
print(unique(h25$state_name))
