setwd("/Users/hanno/Documents/GitHub/german_election_data")
for(f in c("federal_elections/municipality_level/final/federal_muni_unharm","federal_elections/municipality_level/final/federal_muni_harm_21","state_elections/final/state_unharm","state_elections/final/state_harm_21","european_elections/final/european_muni_unharm","european_elections/final/european_muni_harm","municipal_elections/final/municipal_unharm","municipal_elections/final/municipal_harm","federal_elections/county_level/final/federal_cty_unharm","state_elections/final/ltw_wkr_unharm","covars_municipality/final/ags_area_pop_emp")){
 p<-paste0("data/",f,".rds"); if(!file.exists(p)){cat("MISSING",p,"\n");next}
 d<-readRDS(p); cat("\n##",f,nrow(d),"\n"); cat(head(names(d),30),sep=" "); cat("\n")}
