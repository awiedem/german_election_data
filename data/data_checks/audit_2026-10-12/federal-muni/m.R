suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
print(u|>filter(election_year %in% c(1994,1998,2002,2005),state%in%c("12","13","15"))|>group_by(election_year,state)|>summarise(turn=sum(number_voters)/sum(eligible_voters_orig),.groups="drop")|>as.data.frame())
