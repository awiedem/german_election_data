suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
h<-readRDS(paste0(d,"federal_muni_harm_21.rds"))
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
print(h |> filter(election_year==2021) |> summarise(sv=sum(valid_votes),nv=sum(number_voters),tv=sum(total_votes,na.rm=T),cduN=sum(cdu*number_voters,na.rm=T), cduV=sum(cdu*valid_votes,na.rm=T),cduT=sum(cdu*total_votes,na.rm=T)))
print(u |> filter(election_year==2021)|>summarise(sv=sum(valid_votes),nv=sum(number_voters),cdu_nv=sum(cdu*number_voters,na.rm=T),cdu_vv=sum(cdu*valid_votes,na.rm=T)))
print(h|>filter(election_year==2021)|>select(ags,cdu,spd,number_voters,valid_votes,total_votes,total_votes_incogruence,perc_total_votes_incogruence,flag_total_votes_incongruent)|>head(5)|>as.data.frame())
print(h |> group_by(election_year)|>summarise(tv=sum(total_votes,na.rm=T),vv=sum(valid_votes),nv=sum(number_voters),ratio_tv_nv=tv/nv,ratio_tv_vv=tv/vv, flag=mean(flag_total_votes_incongruent,na.rm=T)))
