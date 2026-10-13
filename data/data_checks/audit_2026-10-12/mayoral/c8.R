suppressMessages({library(dplyr);library(data.table)})
cc<-readRDS("data/mayoral_elections/final/mayoral_candidates.rds"); setDT(cc)
print(cc[ags=="03241003"&election_date=="2019-05-26",.(candidate_name,candidate_party,candidate_voteshare_hw,candidate_votes_sw,is_winner,candidate_rank_hw)])
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
print(u[ags=="03241003"&election_year==2019,.(election_date,round,winner_party,winner_voteshare)])
x<-cc[!duplicated(paste(ags,election_date,election_type))&!is.na(election_date_sw)&election_date_sw<=election_date,.(ags,ags_name,election_date,election_date_sw,state)]
print(x[,.N,by=state]); print(x[1:8])
# TH >50% with SW
y<-cc[has_stichwahl==TRUE,.(hw=max(candidate_voteshare_hw,na.rm=T),st=state[1],ags=ags[1],ags_name=ags_name[1],d=election_date[1],dsw=election_date_sw[1]),by=.(a=ags,e=election_date,t=election_type)][hw>.5]
print(y[st=="16"][1:6,.(ags,ags_name,d,dsw,hw)])
