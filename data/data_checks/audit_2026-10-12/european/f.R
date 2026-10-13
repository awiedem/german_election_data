suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
ni<-u%>%filter(state=="03")
print(ni%>%group_by(election_year)%>%summarise(n=n(),el=sum(eligible_voters),nv=sum(number_voters),vv=sum(valid_votes),iv=sum(invalid_votes),t=nv/el,A1=sum(voters_wo_sperrvermerk),B1=sum(voters_w_wahlschein)))
x<-ni%>%filter(election_year==2024)
print(summary(x$turnout)); print(summary(x$voters_w_wahlschein/x$eligible_voters))
# compare with other states: share of Wahlschein voters vs voters
print(u%>%filter(election_year==2024)%>%group_by(state_name)%>%summarise(ws=sum(voters_w_wahlschein)/sum(eligible_voters),t=sum(number_voters)/sum(eligible_voters)))
# distribution of ni turnout by county
print(x%>%group_by(county)%>%summarise(n=n(),t=sum(number_voters)/sum(eligible_voters),vv=sum(valid_votes))%>%arrange(t)%>%head(12))
print(x%>%group_by(county)%>%summarise(n=n(),t=sum(number_voters)/sum(eligible_voters))%>%arrange(desc(t))%>%head(5))
print(x%>%filter(county=="03241")%>%select(ags,eligible_voters,number_voters,turnout)%>%head(10))
