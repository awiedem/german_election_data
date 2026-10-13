suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
L<-list(unharm=readRDS(paste0(d,"federal_muni_unharm.rds")),h21=readRDS(paste0(d,"federal_muni_harm_21.rds")),h25=readRDS(paste0(d,"federal_muni_harm_25.rds")),raw=readRDS(paste0(d,"federal_muni_raw.rds")))
for(n in names(L)){x<-L[[n]];cat("\n=====",n,"\n")
 cat("dup ags-year:",sum(duplicated(x[,c("ags","election_year")])),"\n")
 if(n=="raw") cat("dup ags-bwbez-year:",sum(duplicated(x[,c("ags","bwbez","election_year")])),"\n")
 cat("ags class",class(x$ags),"nchar",paste(names(table(nchar(x$ags))),table(nchar(x$ags))),"\n")
 cat("NA ags:",sum(is.na(x$ags)),"\n")
 if("state"%in%names(x)){ cat("state vals:",paste(head(sort(unique(x$state)),20),collapse=","),"\n"); print(table(substr(x$ags,1,2),x$state)[1:3,1:3])}
 cat("valid>voters:",sum(x$valid_votes>x$number_voters,na.rm=T)," voters>elig:",sum(x$number_voters>x$eligible_voters,na.rm=T)," valid+inv!=voters:",sum(abs(x$valid_votes+x$invalid_votes-x$number_voters)>1,na.rm=T),
  " neg:",sum(x$valid_votes<0|x$number_voters<0|x$eligible_voters<0|x$invalid_votes<0,na.rm=T)," frac voters:",sum(x$number_voters%%1!=0,na.rm=T)," frac valid:",sum(x$valid_votes%%1!=0,na.rm=T), " NA valid:",sum(is.na(x$valid_votes))," zero valid:",sum(x$valid_votes==0,na.rm=T)," zero elig:",sum(x$eligible_voters==0,na.rm=T),"\n")
 if("turnout"%in%names(x)) cat("turnout>1:",sum(x$turnout>1,na.rm=T)," NA:",sum(is.na(x$turnout))," <0.3:",sum(x$turnout<0.3,na.rm=T),"\n")
}
