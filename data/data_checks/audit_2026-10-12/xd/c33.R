suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
for(p in c("federal_elections/municipality_level/final/federal_muni_harm_21","state_elections/final/state_harm_21","european_elections/final/european_muni_harm","municipal_elections/final/municipal_harm")){
 r<-readRDS(paste0("data/",p,".rds")); c<-data.table::fread(paste0("data/",p,".csv"),colClasses=list(character="ags"))
 cat(basename(p),"rows rds/csv",nrow(r),nrow(c),"cols",ncol(r),ncol(c),"ev sum",sum(r$eligible_voters,na.rm=T),sum(c$eligible_voters,na.rm=T),"\n")
 d<-setdiff(names(r),names(c));if(length(d))cat(" cols only rds:",d,"\n")}
