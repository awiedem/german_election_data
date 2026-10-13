suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
j<-u|>filter(election_year %in% c(2017,2021,2025))|>select(ags,ags_name,election_year,number_voters,eligible_voters_orig)|>tidyr::pivot_wider(names_from=election_year,values_from=c(number_voters,eligible_voters_orig))
print(j|>filter(ags %in% c("07131003"))|>as.data.frame())
print(u|>filter(ags=="07131003")|>select(ags,ags_name,election_year,eligible_voters_orig,eligible_voters,number_voters,turnout)|>tail(4)|>as.data.frame())
# same pattern: 2021 turnout extreme high with small pop
print(u|>filter(election_year==2021,number_voters>1.5*eligible_voters_orig,eligible_voters_orig>0)|>nrow())
