suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
h<-readRDS("data/mayoral_elections/final/mayoral_harm.rds"); setDT(h)
miss<-u[!h,on=.(ags,election_date,election_type,round)]
miss<-miss[!election_type%in%c("VG-Bürgermeisterwahl","SG-Bürgermeisterwahl")]
tot<-u[!election_type%in%c("VG-Bürgermeisterwahl","SG-Bürgermeisterwahl"),.N,by=state]
m<-miss[,.N,by=state]; print(merge(tot,m,by="state",all.x=T,suffixes=c("_all","_dropped"))[,pct:=round(100*N_dropped/N_all,1)])
print(miss[state=="15",.N,by=election_year][order(election_year)])
print(miss[state=="14",.N,by=election_year][order(election_year)])
print(miss[state=="14"][1:5,.(ags,ags_name,election_date)]); print(miss[state=="15"][1:5,.(ags,ags_name,election_date)])
# is the ags in crosswalk?
cw<-readRDS("data/crosswalks/final/ags_crosswalks.rds"); setDT(cw); print(names(cw))
cat("miss ags in cw ags:",mean(unique(miss[state=="15"]$ags)%in%cw$ags),"; ST ags in cw (any year)\n")
cat("SN miss ags in cw:",mean(unique(miss[state=="14"]$ags)%in%cw$ags),"\n")
cat("ST miss ags in ags_21 targets:",mean(unique(miss[state=="15"]$ags)%in%cw$ags_21),"\n")
# harm winner_voteshare inconsistent
d<-h[abs(winner_voteshare-winner_votes/valid_votes)>0.01]
print(d[,.(.N,agg=sum(flag_aggregated),pre=sum(flag_pre_1990)),by=state]); print(d[flag_aggregated==0][1:6,.(ags,election_date,valid_votes,winner_votes,winner_voteshare,n_predecessors)])
cat("RLP harm NA counts\n"); print(h[state=="07",.(N=.N,na=sum(is.na(eligible_voters)),pct=sum(flag_pct_only))])
