suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
w<-readRDS("data/state_elections/final/ltw_wkr_unharm.rds");s<-readRDS("data/state_elections/final/state_unharm.rds")
cat("Bayern 2018 state_unharm: ev,nv,vv,inv, turnout, cdu share parts\n")
b<-s%>%filter(state=="09",election_year==2018);print(c(ev=sum(b$eligible_voters),nv=sum(b$number_voters),vv=sum(b$valid_votes),inv=sum(b$invalid_votes),csu=sum(b$csu,na.rm=T)))
bw<-w%>%filter(state=="09",election_year==2018)%>%group_by(stimme)%>%summarise(ev=sum(eligible_voters),nv=sum(number_voters),vv=sum(valid_votes),inv=sum(invalid_votes),csu=sum(csu,na.rm=T));print(as.data.frame(bw))
print(summary(b$csu/b$valid_votes)); 
cat("Share: sum parties / valid:");pc<-setdiff(names(b)[sapply(b,is.numeric)],c("eligible_voters","number_voters","valid_votes","invalid_votes","turnout","election_year"));pc<-pc[!grepl("flag",pc)]
print(summary(rowSums(b[,pc],na.rm=T)/b$valid_votes))
print(as.data.frame(s%>%group_by(state,election_year)%>%summarise(na_ev=mean(is.na(eligible_voters)),na_nv=mean(is.na(number_voters)),n=n(),.groups="drop")%>%filter(na_ev>.5)))
print(as.data.frame(w%>%filter(state=="14",election_year==1994,stimme=="zweitstimme")%>%summarise(n=n(),nr=paste(sort(as.integer(wkr_nr)),collapse=","))))
print(as.data.frame(w%>%filter(state=="04",election_year==2003,stimme=="zweitstimme")%>%select(wkr_nr,wkr_name,eligible_voters)))
print(as.data.frame(w%>%filter(state=="14",election_year==2004,stimme=="zweitstimme")%>%summarise(ev=sum(eligible_voters))))
print(as.data.frame(s%>%filter(state=="14",election_year %in% c(1999,2004))%>%group_by(election_year)%>%summarise(n=n(),ev=sum(eligible_voters),ev_na=sum(is.na(eligible_voters)))))
