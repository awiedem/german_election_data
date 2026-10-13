suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
t<-function(s,y){d<-x%>%filter(state==s,election_year==y);c(s=s,y=y,turn=round(sum(d$number_voters,na.rm=T)/sum(d$eligible_voters,na.rm=T),4),cdu=round(sum(d$cdu*d$valid_votes,na.rm=T)/sum(d$valid_votes,na.rm=T),4))}
for(a in list(c("08",1972),c("08",1980),c("08",1964),c("05",1975),c("06",1978),c("10",1970),c("10",1975))) print(t(a[1],as.integer(a[2])))
j<-read.csv(file.path("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/verif-sh","tot.csv"),colClasses=c(state="character"))
print(j%>%filter(state%in%c("08","05","06","10"),election_year<=1984)%>%mutate(tg=nv/el,tb=number_of_voters/electorate)%>%select(state,election_year,tg,tb))
print(j%>%filter(state=="08",election_year>=1984)%>%select(election_year,dv)%>%mutate(dv=round(dv*100,2)))
