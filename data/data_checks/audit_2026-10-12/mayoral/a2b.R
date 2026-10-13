suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
p<-function(...)cat(...,"\n")
p("empty-string party:",sum(u$winner_party=="",na.rm=T)); print(u[winner_party=="",.N,by=state])
p("turnout>1 rows"); print(u[turnout>1|turnout<0,.(ags,ags_name,election_date,round,eligible_voters,number_voters,turnout)])
p("voters>elig"); print(u[number_voters>eligible_voters,.(ags,ags_name,election_date,round,eligible_voters,number_voters,valid_votes,turnout)])
p("val+inv!=voters"); print(u[abs(valid_votes+invalid_votes-number_voters)>0,.(ags,ags_name,election_date,number_voters,valid_votes,invalid_votes)])
p("turnout==1 by state"); print(u[turnout>=0.999,.N,by=state])
p("turnout>=0.95 & elig>1000"); print(u[turnout>=0.95&eligible_voters>1000,.(ags,ags_name,election_date,eligible_voters,number_voters)][1:10])
# HW <=.5 with no SW
sw<-u[round=="stichwahl",.(ags,election_type,swd=election_date)]
h<-u[round=="hauptwahl"&winner_voteshare<=0.5&!is.na(winner_voteshare)]
h[,hasSW:=mapply(function(a,t,d){any(sw$ags==a&sw$election_type==t&sw$swd>d&sw$swd-d<=70)},ags,election_type,election_date)]
p("HW share<=.5 with no SW within 70d (by state, superseded)");print(h[hasSW==F,.N,by=.(state,flag_superseded)][order(state)])
p("HW share>.5 with SW"); h2<-u[round=="hauptwahl"&winner_voteshare>0.5]
h2[,hasSW:=mapply(function(a,t,d){any(sw$ags==a&sw$election_type==t&sw$swd>d&sw$swd-d<=70)},ags,election_type,election_date)]
print(h2[hasSW==T,.N,by=state])
print(h2[hasSW==T][1:8,.(ags,ags_name,election_date,winner_voteshare,winner_party)])
p("SW without HW")
hw<-u[round=="hauptwahl",.(ags,election_type,hwd=election_date)]
s<-u[round=="stichwahl"]; s[,hasHW:=mapply(function(a,t,d){any(hw$ags==a&hw$election_type==t&hw$hwd<d&d-hw$hwd<=70)},ags,election_type,election_date)]
print(s[,.N,by=.(state,hasHW)][order(state)])
x<-h[hasSW==F&flag_superseded==F]
print(x[,.N,by=.(state,election_year)][order(state,election_year)],nrows=100)
print(x[state=="05"][1:12,.(ags,ags_name,election_date,election_type,winner_party,winner_votes,winner_voteshare)])
print(x[state=="16"][1:8,.(ags,ags_name,election_date,winner_party,winner_voteshare)])
print(x[state=="09"][1:12,.(ags,ags_name,election_date,winner_party,winner_voteshare)])
x<-h[hasSW==F&flag_superseded==F]
print(x[,.N,by=.(state,election_year)][order(state,election_year)],nrows=100)
print(x[state=="05"][1:12,.(ags,ags_name,election_date,election_type,winner_party,winner_votes,winner_voteshare)])
print(x[state=="16"][1:8,.(ags,ags_name,election_date,winner_party,winner_voteshare)])
print(x[state=="09"][1:12,.(ags,ags_name,election_date,winner_party,winner_voteshare)])
