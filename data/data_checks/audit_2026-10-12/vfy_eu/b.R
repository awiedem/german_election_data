suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
pc<-names(u)[which(names(u)=="abg"):ncol(u)]; pc<-pc[sapply(u[pc],is.numeric)]
print(tail(names(u),8))
u$ps<-rowSums(u[pc],na.rm=T)
u$r<-u$ps  # shares?
print(summary(u$ps[u$election_year==2024]))
u$cnt<-u$ps*u$number_voters
u$rel<-abs(u$cnt-u$valid_votes)/pmax(u$valid_votes,1)
print(u%>%group_by(election_year)%>%summarise(n=sum(rel>.02,na.rm=T),lost=sum((valid_votes-cnt)[rel>.02],na.rm=T)))
print(u%>%filter(rel>.02)%>%count(election_year,st=substr(ags,1,2))%>%arrange(-n)%>%head(8))
print(u%>%filter(ags=="07232066")%>%select(election_year,number_voters,valid_votes,ps))
print(u%>%filter(election_year==2024)%>%summarise(cdu=sum(cdu*number_voters,na.rm=T)))
print(u%>%filter(substr(ags,1,2)=="03"|TRUE)%>%filter(election_year==2024)%>%group_by(st=substr(ags,1,2))%>%summarise(v=sum(number_voters),el=sum(eligible_voters))%>%filter(st%in%c("03","05","09")))
print(c(sum(u$npd[u$election_year==2024]>0,na.rm=T),sum(u$heimat[u$election_year==2019]>0,na.rm=T)))
