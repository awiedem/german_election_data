suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
cat("Spot checks\n")
print(u[ags%in%c("09162000","05315000","14713000","08111000","11000000","02000000","06412000","05111000")&election_year%in%c(2020,2021,2019,2025),.(ags_name,election_date,round,eligible_voters,turnout,winner_party,winner_voteshare)])
cat("weekday non-Sunday by state\n")
u[,wd:=weekdays(election_date)]
print(u[,.(n=.N,nonsun=sum(wd!="Sonntag"&wd!="Sunday"),pct=round(100*mean(wd!="Sonntag"&wd!="Sunday"),1)),by=state][order(state)])
print(u[wd!="Sunday"&wd!="Sonntag"&election_year>=2000,.N,by=.(state,wd)][order(-N)][1:10])
