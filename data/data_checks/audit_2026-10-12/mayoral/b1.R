suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
cc<-readRDS("data/mayoral_elections/final/mayoral_candidates.rds"); setDT(cc)
r<-u[state=="07"]
print(r[,.N,by=winner_party][order(-N)][1:25])
print(r[,.(distinct=uniqueN(winner_party),pct_pct=mean(winner_voteshare>1,na.rm=T)),by=election_type])
print(r[1:6,.(ags_name,election_date,election_type,round,eligible_voters,turnout,winner_party,winner_votes,winner_voteshare)])
# RLP winner_party that looks like person name vs party
print(cc[state=="07"&is_winner==TRUE][1:8,.(ags_name,election_date,candidate_name,candidate_party)])
cat("RLP counts: NA elig",sum(is.na(r$eligible_voters)),"of",nrow(r),"\n")
print(r[,.(N=.N,na_elig=sum(is.na(eligible_voters)),na_wv=sum(is.na(winner_votes)),na_to=sum(is.na(turnout))),by=election_year][order(election_year)])
# which states winner_party is a surname: compare winner_party to candidate last name
m<-cc[is_winner==TRUE,.(ags,election_date,election_type,candidate_last_name,candidate_party,state)]
x<-merge(u[round=="hauptwahl",.(ags,election_date,election_type,winner_party)],m,by=c("ags","election_date","election_type"))
x[,isname:=!is.na(candidate_last_name)&candidate_last_name!=""&winner_party==candidate_last_name]
print(x[,.(n=.N,isname=sum(isname)),by=state][order(state)])
