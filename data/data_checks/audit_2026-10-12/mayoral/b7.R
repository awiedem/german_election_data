suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
k<-u[!is.na(valid_votes)&valid_votes>500]
d<-k[duplicated(k[,.(election_date,round,valid_votes,winner_votes,eligible_voters,number_voters)])|duplicated(k[,.(election_date,round,valid_votes,winner_votes,eligible_voters,number_voters)],fromLast=T)]
cat("rows sharing identical (date,round,counts) across different ags/type:",nrow(d),"\n")
print(d[order(election_date,valid_votes)][,.(ags,ags_name,election_date,election_type,round,valid_votes,winner_votes,flag_shared_ags)][1:30])
print(d[,.N,by=state])
