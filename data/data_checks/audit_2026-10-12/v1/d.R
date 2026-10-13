suppressMessages(library(dplyr))
for (f in c("municipal_unharm","municipal_harm","municipal_harm_25")) { d<-readRDS(paste0("data/municipal_elections/final/",f,".rds")); cat(f, nrow(d), "NA date:", sum(is.na(d$election_date)), "\n"); if(f=="municipal_harm") print(table(d$state[!is.na(d$election_date)], d$election_year[!is.na(d$election_date)])); if(f=="municipal_unharm") print(table(d$election_type))}
w <- readRDS("data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.rds"); print(names(w)[1:12])
