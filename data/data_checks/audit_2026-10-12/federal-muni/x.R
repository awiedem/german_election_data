suppressMessages(library(dplyr));options(width=200,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
print(u|>filter(ags=="11000000")|>select(election_year,pop,eligible_voters)|>as.data.frame()|>tail(3))
print(table(is.na(u$pop),u$election_year))
