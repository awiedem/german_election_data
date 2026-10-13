suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
r<-function(x)readRDS(paste0("data/",x,".rds"))
fu<-r("federal_elections/municipality_level/final/federal_muni_unharm")
print(as.data.frame(fu%>%group_by(election_year)%>%summarise(n=n(),na_name=sum(is.na(ags_name)),pct=round(mean(is.na(ags_name)),3),na_sn=sum(is.na(state_name)))))
for(f in c("federal_elections/municipality_level/final/federal_muni_harm_21","municipal_elections/final/municipal_unharm","municipal_elections/final/municipal_harm","state_elections/final/state_harm_21","county_elections/final/county_elec_unharm","mayoral_elections/final/mayoral_unharm")){d<-r(f);nc<-grep("^ags_name",names(d),value=T)[1];cat(f,nc,"NA",sum(is.na(d[[nc]])),"of",nrow(d),"\n")}
# state_name language
for(f in c("federal_elections/municipality_level/final/federal_muni_unharm","state_elections/final/state_harm_21","european_elections/final/european_muni_unharm","municipal_elections/final/municipal_unharm","mayoral_elections/final/mayoral_unharm","landrat_elections/final/landrat_unharm","federal_elections/county_level/final/federal_cty_unharm","county_elections/final/county_council_seats")){d<-r(f);cat(basename(f),":",paste(sort(unique(d$state_name))[1:16],collapse="|"),"\n")}
