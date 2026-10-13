suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
h<-readRDS("data/mayoral_elections/final/mayoral_harm.rds"); setDT(h)
t<-u[state=="16"&election_year==2018]
x<-t[!h,on=.(ags,election_date,election_type,round)]
print(x[,.(ags,ags_name,election_date,round,valid_votes,winner_votes)])
cw<-readRDS("data/crosswalks/final/ags_crosswalks.rds"); setDT(cw)
print(cw[ags%in%x$ags&year>=2017,.(ags,year,ags_21,pop_cw)])
print(h[state=="16"&election_year==2018&flag_aggregated==1,.(ags,ags_name,valid_votes,n_predecessors)])
# agg shares
a<-h[flag_aggregated==1]
a[,impl:=winner_votes/valid_votes]
print(a[,.(n=.N,mean_gap=mean(winner_voteshare-impl,na.rm=T),maxgap=max(abs(winner_voteshare-impl),na.rm=T))])
print(a[abs(winner_voteshare-impl)>0.05][1:4,.(ags,ags_name,election_date,valid_votes,winner_votes,winner_voteshare,impl,n_predecessors)])
cat("harm HW <.5 ,share>1 etc done\n")
