suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
fu<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
print(as.data.frame(fu%>%filter(state%in%c("11","02"))%>%group_by(state,election_year)%>%summarise(n=n(),nv=sum(number_voters),nvo=sum(number_voters_orig)),row.names=F))
fc<-readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds");print(as.data.frame(fc%>%filter(state=="11",year>=2013)%>%select(ags,year,eligible_voters,number_voters,turnout)))
