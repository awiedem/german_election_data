suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
fu<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
print(as.data.frame(fu%>%filter(state%in%c("02","11"),election_year>=2017)%>%select(state,election_year,eligible_voters_orig,eligible_voters,number_voters_orig,number_voters,turnout,turnout_wo_mailin)))
