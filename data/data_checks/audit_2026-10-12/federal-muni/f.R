suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"));r<-readRDS(paste0(d,"federal_muni_raw.rds"))
print(u|>group_by(election_year)|>summarise(elig=sum(eligible_voters,na.rm=T),elig_orig=sum(eligible_voters_orig,na.rm=T),nv=sum(number_voters),nv_orig=sum(number_voters_orig,na.rm=T),turn_agg=nv/elig,turn_orig=nv/elig_orig,mean_turn=mean(turnout,na.rm=T),max_t=max(turnout,na.rm=T),n_t_gt1=sum(turnout>1,na.rm=T),n_t_na=sum(is.na(turnout)),n_t_eq1=sum(turnout==1,na.rm=T))|>as.data.frame())
