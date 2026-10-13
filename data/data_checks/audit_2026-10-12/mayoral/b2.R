suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
cc<-readRDS("data/mayoral_elections/final/mayoral_candidates.rds"); setDT(cc)
x<-u[grepl("^[A-ZÄÖÜ][a-zäöüß]+, [A-ZÄÖÜ][a-zäöüß]+$",winner_party)]
cat("comma-name-like winner_party:",nrow(x),"\n"); print(x[,.N,by=state]); print(x[1:6,.(ags,ags_name,election_date,winner_party)])
# RLP surnames: winner_party equals a candidate last name (any rank) in RLP
r<-u[state=="07"]
ln<-unique(cc[state=="07",candidate_last_name])
r[,sn:=winner_party%in%ln]
cat("RLP rows winner_party is a surname:",sum(r$sn)," of ",nrow(r),"\n")
print(r[sn==TRUE][1:8,.(ags,ags_name,election_date,election_type,winner_party)])
print(r[sn==TRUE,.N,by=election_type])
# candidate side
print(cc[state=="07"&candidate_party%in%ln,.N])
print(cc[state=="07"&candidate_party=="Schneider",.(ags_name,election_date,candidate_name,candidate_party)][1:4])
