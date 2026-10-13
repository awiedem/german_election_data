suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
h<-readRDS("data/european_elections/final/european_muni_harm.rds")
print(names(h)[1:20]); print(tail(names(h),6))
a<-lapply(split(h$ags,h$election_year),unique)
for(y in c("2009","2014","2019")) cat(y,"in 2024 not y:",length(setdiff(a[["2024"]],a[[y]])),"y not 2024:",length(setdiff(a[[y]],a[["2024"]])),"\n")
cat("2009 vs 2019 differ:",length(setdiff(a[["2009"]],a[["2019"]])),length(setdiff(a[["2019"]],a[["2009"]])),"\n")
print(head(setdiff(a[["2019"]],a[["2024"]]),10)); print(head(setdiff(a[["2024"]],a[["2019"]]),10))
print(table(h$flag_unsuccessful_naive_merge,h$election_year,useNA="a"))
print(table(h$flag_aggregated,h$election_year))
print(h%>%filter(election_year==2024)%>%count(state)%>%head(20))
# fractional counts
fr<-h%>%summarise(across(c(eligible_voters,number_voters,valid_votes,invalid_votes),~sum(abs(.x-round(.x))>1e-6,na.rm=T)));print(fr)
print(h%>%group_by(election_year)%>%summarise(bad1=sum(valid_votes>number_voters),bad2=sum(number_voters>eligible_voters),bad3=sum(abs(valid_votes+invalid_votes-number_voters)>1),t1=sum(turnout>1),tNA=sum(is.na(turnout)),tmax=max(turnout)))
# 2024 harm: rows in unharm 2024 not in harm
print(length(setdiff(u$ags[u$election_year==2024],h$ags[h$election_year==2024])))
print(h%>%filter(election_year==2024)%>%count(ags)%>%filter(n>1)%>%nrow())
# Berlin
print(h%>%filter(substr(ags,1,2)=="11")%>%select(ags,election_year,eligible_voters,number_voters))
# harm vs unharm by state totals
s1<-u%>%group_by(election_year,state)%>%summarise(v=sum(valid_votes),.groups="drop");s2<-h%>%group_by(election_year,state)%>%summarise(v2=sum(valid_votes),.groups="drop")
print(left_join(s1,s2)%>%filter(abs(v-v2)>2))
# 2024 harm state vs ags prefix
print(table(substr(h$ags,1,2)==h$state))
# 2024 harm ags in Thuringia changed codes? ags not in 2019 harm
x<-h%>%filter(election_year==2024,!ags%in%a[["2019"]])%>%select(ags,state_name,n_predecessors,eligible_voters)
print(head(x,15)); print(nrow(x))
