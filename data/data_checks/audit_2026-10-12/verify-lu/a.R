suppressMessages(library(dplyr))
u<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
h<-readRDS("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds")
c<-readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
print(names(h)[1:30])
for (d in list(list("unharm",u),list("harm21",h))){x<-d[[2]];x<-x[x$election_year==2021,]
cat(d[[1]],sum(x$spd*x$number_voters,na.rm=T),sum(x$spd*x$valid_votes,na.rm=T),"\n")}
cat("elig sum u 2021",sum(u$eligible_voters[u$election_year==2021],na.rm=T),sum(u$eligible_voters_orig[u$election_year==2021],na.rm=T),"\n")
x<-u[u$election_year==2021,]
cat("turn agg",sum(x$number_voters,na.rm=T)/sum(x$eligible_voters,na.rm=T),sum(x$number_voters,na.rm=T)/sum(x$eligible_voters_orig,na.rm=T),"\n")
d<-abs(u$turnout-u$number_voters/u$eligible_voters);cat("turn diff>.001",sum(d>.001,na.rm=T),sum(!is.na(d)),"\n")
d<-abs(u$turnout-u$number_voters/u$eligible_voters_orig);cat("vs orig>.001",sum(d>.001,na.rm=T),"\n")
print(u[u$ags=="01051013"&u$election_year==2002,c("eligible_voters","eligible_voters_orig","number_voters","turnout")])
print(table(nchar(u$county))); print(unique(u$county[nchar(u$county)!=5]))
print(sum(is.na(h$ags_21))); print(class(h$ags_21))
print(tapply(u$eligible_voters,u$election_year,sum,na.rm=T)/tapply(u$eligible_voters_orig,u$election_year,sum,na.rm=T))
