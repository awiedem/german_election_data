suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
cat(names(x)[1:40],"\n")
b<-x%>%filter(state=="11",election_year>=2019)
print(b%>%mutate(s=gruene+0)%>%select(election_year,election_date,valid_votes,gruene,graue,spd,any_of(c("die_grauen","diegrauen")))%>%as.data.frame())
print(grep("grau",names(x),value=TRUE,ignore.case=TRUE))
for(y in c(2021,2023)){d<-x%>%filter(state=="11",election_year==y);cat(y,sum(d$valid_votes),sum(d$gruene*d$valid_votes,na.rm=T)/sum(d$valid_votes),sum(d$spd*d$valid_votes,na.rm=T)/sum(d$valid_votes),"\n")}
