suppressMessages(library(dplyr))
setwd("/Users/hanno/Documents/GitHub/german_election_data")
fu<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
su<-readRDS("data/state_elections/final/state_unharm.rds")
for(a in c("12065272","12062372","12064192","14374390","14389200")){
 cat("\n",a,"\n");print(as.data.frame(fu%>%filter(ags==a,election_year>=1990)%>%select(election_year,ags_name,eligible_voters,eligible_voters_orig,number_voters)))
 print(as.data.frame(su%>%filter(ags==a,election_year>=1990)%>%select(election_year,eligible_voters,number_voters)))}
# BB 1998 fed: counts by number and ev distribution
x<-fu%>%filter(state=="12",election_year==1998);cat(nrow(x),sum(x$eligible_voters),"\n")
y<-su%>%filter(state=="12",election_year==1999);cat(nrow(y),sum(y$eligible_voters),"\n")
cat("BB 1998 fed ags not in 1999 state:",length(setdiff(x$ags,y$ags)),"; state not in fed:",length(setdiff(y$ags,x$ags)),"\n")
