suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
h21<-readRDS(paste0(d,"federal_muni_harm_21.rds"));h25<-readRDS(paste0(d,"federal_muni_harm_25.rds"))
u<-readRDS(paste0(d,"federal_muni_unharm.rds")); r<-readRDS(paste0(d,"federal_muni_raw.rds"))
a<-u|>filter(election_year==2021,ags=="01001000")|>select(ags,cdu,spd,number_voters,valid_votes,eligible_voters,eligible_voters_orig,turnout)
b<-h21|>filter(election_year==2021,ags=="01001000")|>select(ags,cdu,spd,number_voters,valid_votes,eligible_voters,turnout,total_votes)
print(as.data.frame(a));print(as.data.frame(b))
rr<-r|>filter(election_year==2021,ags=="01001000")|>select(ags,cdu,spd,number_voters,valid_votes,eligible_voters,bwbez)
print(as.data.frame(rr))
# implied denominators
for(n in list(u=u,h21=h21,h25=h25)){ x<-n|>filter(election_year==2021)|>summarise(a=sum(cdu*number_voters,na.rm=T),b=sum(cdu*valid_votes,na.rm=T)); print(x)}
# per row: cdu share sum over parties vs 1 and vs valid/number_voters
pc<-c("cdu","csu","spd","gruene","fdp","linke_pds","afd")
chk<-function(x,nm){x$ps<-rowSums(x[,intersect(names(x),pc)],na.rm=T);x|>filter(election_year>=2005)|>group_by(election_year)|>summarise(med_sum=median(ps),med_vv_nv=median(valid_votes/number_voters,na.rm=T),.groups="drop")|>mutate(f=nm)}
print(bind_rows(chk(u,"u"),chk(h21,"h21"),chk(h25,"h25"))|>arrange(election_year,f)|>as.data.frame())
