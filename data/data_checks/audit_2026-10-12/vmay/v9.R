suppressMessages(library(dplyr))
f<-function(x) readRDS(file.path("data/mayoral_elections/final",paste0(x,".rds")))
u<-f("mayoral_unharm"); c<-f("mayoral_candidates"); p<-f("mayor_panel"); h<-f("mayoral_harm")
e<-c%>%distinct(ags,election_date,election_date_sw,state,has_stichwahl)
cat("sw<=hw",sum(e$election_date_sw<=e$election_date,na.rm=T),"\n");print(table(e$state[which(e$election_date_sw<=e$election_date)]))
print(c%>%filter(ags=="03152001",election_year==2013)%>%select(election_date,election_date_sw,candidate_voteshare_hw,candidate_voteshare_sw,candidate_party))
print(u%>%filter(ags=="03152001",election_year==2013)%>%select(election_date,round,winner_party,winner_voteshare))
# 09
n<-c%>%group_by(ags,election_date)%>%summarise(hs=first(has_stichwahl),sw=any(!is.na(candidate_votes_sw)|!is.na(candidate_voteshare_sw)),w=any(is_winner,na.rm=T),st=first(state),.groups="drop")%>%filter(hs,!sw)
cat("09:",nrow(n),"w",sum(n$w),"\n");print(table(n$st))
# 10
cat("turn>1\n");print(u%>%filter(turnout>1)%>%select(ags,election_date,turnout,eligible_voters,number_voters))
z<-u%>%filter(!is.na(valid_votes),!is.na(invalid_votes),!is.na(number_voters),valid_votes+invalid_votes!=number_voters)
print(z%>%select(ags,ags_name,election_date,round,valid_votes,invalid_votes,number_voters))
# 11
d<-u%>%filter(!is.na(valid_votes))%>%group_by(election_date,round,valid_votes,number_voters,winner_votes)%>%filter(n()>1)%>%ungroup()%>%filter(valid_votes>100)
print(d%>%select(ags,ags_name,election_date,election_type,round,valid_votes,winner_votes))
# 12
nn<-u%>%group_by(ags)%>%summarise(k=n_distinct(ags_name)); cat("ags w/ multiple names",sum(nn$k>1),"of",nrow(nn),"\n")
print(sum(grepl("^Krfr\\. Stadt",u$ags_name)));print(sum(grepl("^Kreisfreie Stadt",u$ags_name)))
print(u%>%filter(ags%in%c("03101000"))%>%distinct(ags_name))
# 13
cat(sum(p$term_start_date!=p$election_date,na.rm=T),nrow(p),"\n")
cat(nrow(u),nrow(h),nrow(c),nrow(p),"\n");print(unique(u$election_type))
