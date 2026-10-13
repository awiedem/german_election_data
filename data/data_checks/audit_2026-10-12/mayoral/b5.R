suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
h<-readRDS("data/mayoral_elections/final/mayoral_harm.rds"); setDT(h)
uu<-u[!election_type%in%c("VG-Bürgermeisterwahl","SG-Bürgermeisterwahl")]
a<-uu[,.(n_u=.N,v_u=sum(valid_votes,na.rm=T),w_u=sum(winner_votes,na.rm=T),e_u=sum(eligible_voters,na.rm=T)),by=state]
b<-h[,.(n_h=.N,v_h=sum(valid_votes,na.rm=T),w_h=sum(winner_votes,na.rm=T),e_h=sum(eligible_voters,na.rm=T)),by=state]
m<-merge(a,b,by="state"); m[,`:=`(dn=n_h-n_u,dv_pct=round(100*(v_h-v_u)/v_u,2),dw_pct=round(100*(w_h-w_u)/w_u,2),de_pct=round(100*(e_h-e_u)/e_u,2))]
print(m[,.(state,n_u,n_h,dn,dv_pct,dw_pct,de_pct)])
# by state-year for big gaps
a2<-uu[,.(v_u=sum(valid_votes,na.rm=T),n_u=.N),by=.(state,election_year)]
b2<-h[,.(v_h=sum(valid_votes,na.rm=T),n_h=.N),by=.(state,election_year)]
m2<-merge(a2,b2,by=c("state","election_year"),all=TRUE); m2[,d:=100*(v_h-v_u)/v_u]
print(m2[abs(d)>1&v_u>10000][order(-abs(d))][1:20])
# HW rows with no harm counterpart by ags_21? check count of rounds
cat("stichwahl rows u vs h:",nrow(uu[round=="stichwahl"]),nrow(h[round=="stichwahl"]),"\n")
# Aggregated rows where sum of shares: check one aggregated example
print(h[flag_aggregated==1&state=="15"][1:4,.(ags,ags_name,election_date,eligible_voters,valid_votes,winner_votes,winner_voteshare,n_predecessors)])
