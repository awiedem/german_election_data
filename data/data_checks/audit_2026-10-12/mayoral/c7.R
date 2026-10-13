suppressMessages({library(dplyr);library(data.table)})
cc<-readRDS("data/mayoral_elections/final/mayoral_candidates.rds"); setDT(cc)
cc[,eid:=paste(ags,election_date,election_type)]
g<-cc[,.(state=state[1],has=has_stichwahl[1],swv=sum(!is.na(candidate_votes_sw)|!is.na(candidate_voteshare_sw)),nw=sum(is_winner,na.rm=T),nwna=sum(is.na(is_winner)),n=.N,dsw=election_date_sw[1],hw1=max(candidate_voteshare_hw,na.rm=T),yr=election_year[1]),by=eid]
x<-g[has==TRUE&swv==0]; print(x[,.N,by=state]); print(x[,.N,by=.(state,nwna>0)])
print(x[1:8,.(eid,dsw,hw1,nw,nwna,n)])
cat("has_stichwahl TRUE but election_date_sw NA:",g[has==TRUE&is.na(dsw),.N],"; FALSE but date_sw non-NA:",g[has==FALSE&!is.na(dsw),.N],"\n")
cat("SW date before/at HW date:",sum(cc[!duplicated(eid)]$election_date_sw<=cc[!duplicated(eid)]$election_date,na.rm=T),"\n")
sd<-cc[!duplicated(eid)&!is.na(election_date_sw),as.numeric(election_date_sw-election_date)]; print(summary(sd)); cat(">42 days:",sum(sd>42),"\n")
cat("nwin=0 groups (nwna==0):",g[nw==0&nwna==0,.N],"\n")
# HW winner >0.5 but SW exists and is_winner differs from HW rank 1
y<-cc[has_stichwahl==TRUE&!is.na(candidate_votes_hw)]
y[,hwmax:=max(candidate_voteshare_hw,na.rm=T),by=eid]
cat("elections where HW leader has >50% yet has_stichwahl TRUE:",uniqueN(y[hwmax>0.5]$eid),"\n")
print(y[hwmax>0.5,.(n=uniqueN(eid)),by=state])
