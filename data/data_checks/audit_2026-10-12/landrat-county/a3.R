suppressMessages(library(dplyr))
setwd("/Users/hanno/Documents/GitHub/german_election_data")
l <- as_tibble(readRDS("data/landrat_elections/final/landrat_unharm.rds"))
cd <- readRDS("data/landrat_elections/final/landrat_candidates.rds")
print(l%>%filter(is.na(ags_name))%>%select(ags,state,election_year)%>%distinct())
print(sort(table(l$winner_party),decreasing=T))
print(sort(table(cd$candidate_party),decreasing=T)[1:60])
# hauptwahl <50% no stichwahl
hw <- l%>%filter(round=="hauptwahl",!is.na(winner_voteshare),winner_voteshare<0.5)
sw <- l%>%filter(round=="stichwahl")%>%select(ags,election_year)%>%distinct()
x <- hw%>%anti_join(sw,by=c("ags","election_year")); print(x%>%count(state)); print(head(x%>%select(ags,ags_name,election_year,winner_party,winner_voteshare),15))
# stichwahl without hauptwahl
x2 <- l%>%filter(round=="stichwahl")%>%anti_join(l%>%filter(round=="hauptwahl")%>%select(ags,election_year),by=c("ags","election_year")); print(nrow(x2)); print(head(x2%>%select(ags,ags_name,election_year,state),20))
# candidates: sum of votes
cc <- cd%>%group_by(ags,election_date)%>%summarise(sv=sum(candidate_votes_hw,na.rm=T),vv=first(valid_votes),nw=sum(is_winner),nc=n(),n_hw=first(n_candidates_hw),.groups="drop")
print(cc%>%filter(nw!=1)%>%count(nw)); print(cc%>%filter(nc!=n_hw)%>%nrow())
print(cc%>%filter(!is.na(vv),sv>0,abs(sv-vv)/vv>0.01)%>%nrow()); print(cc%>%filter(!is.na(vv),sv>0,abs(sv-vv)/vv>0.01)%>%head(15))
print(cc%>%filter(!is.na(vv),sv>0,abs(sv-vv)/vv>0.01)%>%mutate(st=substr(ags,1,2))%>%count(st))
print(cc%>%filter(!is.na(vv),sv>0,abs(sv-vv)/vv>0.01)%>%mutate(r=sv/vv)%>%summarise(min(r),max(r)))
# candidates winner vs unharm
cw <- cd%>%filter(is_winner)%>%group_by(ags,election_date)%>%slice(1)%>%ungroup()
hwl <- l%>%filter(round=="hauptwahl")
m <- cw%>%inner_join(hwl,by=c("ags","election_date"),suffix=c("",".l")); print(nrow(m)); print(sum(m$candidate_party!=m$winner_party,na.rm=T)); 
print(m%>%filter(candidate_party!=winner_party)%>%select(ags,election_date,candidate_party,winner_party,round)%>%head(10))
print(nrow(l%>%filter(round=="hauptwahl")%>%anti_join(cd,by=c("ags","election_date"))))
print(nrow(cd%>%distinct(ags,election_date)%>%anti_join(l,by=c("ags","election_date"))))
# candidate share arithmetic
print(cd%>%filter(!is.na(candidate_voteshare_hw),!is.na(valid_votes),abs(candidate_voteshare_hw-candidate_votes_hw/valid_votes)>0.01)%>%nrow())
print(cd%>%filter(candidate_voteshare_hw>1)%>%nrow())
print(table(cd$candidate_gender,useNA="a"))
print(table(cd$office_type,useNA="a"))
print(summary(cd$candidate_birth_year)); print(cd%>%filter(candidate_birth_year<1850|candidate_birth_year>2005)%>%select(ags,election_year,candidate_name,candidate_birth_year))
