suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
print(u%>%group_by(election_year,state_name)%>%summarise(n=n(),el=sum(eligible_voters,na.rm=T),nv=sum(number_voters,na.rm=T),t=nv/el,.groups="drop")%>%filter(election_year==2024))
print(u%>%group_by(election_year)%>%summarise(naEl=sum(is.na(eligible_voters)),el0=sum(eligible_voters==0,na.rm=T),nv0=sum(number_voters==0,na.rm=T),nvNA=sum(is.na(number_voters)),vvNA=sum(is.na(valid_votes)),tNA=sum(is.na(turnout)),t1=sum(turnout>1,na.rm=T),t0=sum(turnout==0,na.rm=T),flag=sum(flag_turnout_above_1,na.rm=T),bad1=sum(valid_votes>number_voters,na.rm=T),bad2=sum(number_voters>eligible_voters,na.rm=T),bad3=sum(abs(valid_votes+invalid_votes-number_voters)>1,na.rm=T)))
print(table(u$election_year[u$eligible_voters%%1000==0&u$eligible_voters>0]))
print(u%>%filter(election_year==2024)%>%arrange(desc(eligible_voters))%>%select(ags,state_name,eligible_voters,number_voters,turnout)%>%head(8))
print(summary(u$turnout[u$election_year==2024]))
# turnout recompute
print(u%>%mutate(d=turnout-number_voters/eligible_voters)%>%group_by(election_year)%>%summarise(maxd=max(abs(d),na.rm=T),n=sum(abs(d)>0.001,na.rm=T)))
# state prefix
print(table(substr(u$ags,1,2)==u$state)) ; print(table(u$state_name,u$election_year))
print(u%>%mutate(s=voters_wo_sperrvermerk+voters_w_sperrvermerk+voters_par24_2)%>%group_by(election_year)%>%summarise(el=sum(eligible_voters,na.rm=T),A1=sum(voters_wo_sperrvermerk,na.rm=T),A2=sum(voters_w_sperrvermerk,na.rm=T),A3=sum(voters_par24_2,na.rm=T),B1=sum(voters_w_wahlschein,na.rm=T),nA1=sum(is.na(voters_wo_sperrvermerk)),nB1=sum(is.na(voters_w_wahlschein))))
