suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
fu<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
print(as.data.frame(fu%>%filter(ags=="07131003")%>%select(election_year,ags_name,eligible_voters_orig,eligible_voters,number_voters_orig,number_voters,valid_votes,pop,unique_mailin)))
su<-readRDS("data/state_elections/final/state_unharm.rds");print(su%>%filter(ags=="07131003")%>%select(election_year,eligible_voters,number_voters)%>%as.data.frame())
eu<-readRDS("data/european_elections/final/european_muni_unharm.rds");print(eu%>%filter(ags=="07131003")%>%select(election_year,eligible_voters,number_voters)%>%as.data.frame())
# other 2021 RLP Landkreis Ahrweiler munis
print(as.data.frame(fu%>%filter(election_year==2021,substr(ags,1,5)=="07131")%>%summarise(n=n(),ev0=sum(eligible_voters_orig),ev=sum(eligible_voters))))
print(as.data.frame(fu%>%filter(election_year==2021,substr(ags,1,5)=="07131")%>%arrange(desc(eligible_voters_orig))%>%select(ags,ags_name,eligible_voters_orig,eligible_voters)%>%head(4)))
