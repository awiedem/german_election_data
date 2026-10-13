suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"));h21<-readRDS(paste0(d,"federal_muni_harm_21.rds"));h25<-readRDS(paste0(d,"federal_muni_harm_25.rds"))
cnt<-function(x,den){x|>group_by(election_year)|>summarise(cdu=sum(cdu*.data[[den]],na.rm=T),csu=sum(csu*.data[[den]],na.rm=T),spd=sum(spd*.data[[den]],na.rm=T),gruene=sum(gruene*.data[[den]],na.rm=T),linke=sum(linke_pds*.data[[den]],na.rm=T),afd=sum(afd*.data[[den]],na.rm=T),fr=sum(far_right*.data[[den]],na.rm=T),.groups="drop")}
U<-cnt(u,"number_voters")|>filter(election_year>=1990)
H1<-cnt(h21,"total_votes");H2<-cnt(h25,"total_votes");H1n<-cnt(h21,"number_voters")
f<-function(H,U)(H[,-1]/U[,-1]-1)*100
print(cbind(year=U$election_year,round(f(H1,U),3)));print(cbind(year=U$election_year,round(f(H2,U),3)));cat("harm21 shares*number_voters (as codebook says):\n");print(cbind(year=U$election_year,round(f(H1n,U),3)))
# state-level harm_21 vs unharm valid votes
s<-inner_join(u|>group_by(election_year,state)|>summarise(u=sum(valid_votes),.groups="drop"),h21|>group_by(election_year,state)|>summarise(h=sum(valid_votes),.groups="drop"))|>mutate(d=h-u)|>filter(abs(d)>50)
print(as.data.frame(s))
