suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"));r<-readRDS(paste0(d,"federal_muni_raw.rds"))
a<-r|>group_by(election_year,state)|>summarise(rv=sum(valid_votes),rnv=sum(number_voters),rel=sum(eligible_voters),rcdu=sum(cdu+0,na.rm=T),rspd=sum(spd,na.rm=T),.groups="drop")
b<-u|>group_by(election_year,state)|>summarise(uv=sum(valid_votes),unv=sum(number_voters),ucdu=sum(cdu*number_voters,na.rm=T),uspd=sum(spd*number_voters,na.rm=T),n=n(),.groups="drop")
m<-inner_join(a,b)|>mutate(dv=uv-rv,dnv=unv-rnv,pct=dv/rv*100,dcdu=ucdu-rcdu,dspd=uspd-rspd)
print(m|>filter(abs(pct)>0.05)|>as.data.frame())
print(m|>filter(election_year>=2005, abs(dcdu)>2000|abs(dspd)>2000)|>as.data.frame())
