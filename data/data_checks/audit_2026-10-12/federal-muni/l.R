suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"));r<-readRDS(paste0(d,"federal_muni_raw.rds"))
h<-readRDS(paste0(d,"federal_muni_harm_21.rds"))
lost<-r|>filter(substr(ags,6,8)=="999")|>group_by(election_year,state)|>summarise(valid=sum(valid_votes),voters=sum(number_voters),n=n(),.groups="drop")
lost<-lost|>left_join(r|>group_by(election_year,state)|>summarise(tot=sum(valid_votes),.groups="drop"))|>mutate(pct=round(valid/tot*100,2))
print(as.data.frame(lost))
print(u|>filter(substr(ags,6,8)=="999")|>count(election_year))
# which raw ags*year missing from unharm
m<-anti_join(r|>distinct(ags,election_year),u|>distinct(ags,election_year))
print(m|>count(election_year))
tot<-r|>group_by(election_year)|>summarise(rv=sum(valid_votes),.groups="drop")|>left_join(u|>group_by(election_year)|>summarise(uv=sum(valid_votes),.groups="drop"))|>mutate(lost=rv-uv,pct=lost/rv*100)
print(as.data.frame(tot))
