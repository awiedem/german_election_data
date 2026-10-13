suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
x<-u|>mutate(t1=number_voters/eligible_voters,t2=number_voters/eligible_voters_orig, d1=abs(t1-turnout),d2=abs(t2-turnout))
print(x|>group_by(election_year)|>summarise(m1=mean(d1>1e-6,na.rm=T),m2=mean(d2>1e-6,na.rm=T),n=n(),elig_ratio_gt1.05=sum(eligible_voters/eligible_voters_orig>1.05,na.rm=T))|>as.data.frame())
print(x|>filter(election_year==1980)|>mutate(r=eligible_voters/eligible_voters_orig)|>arrange(desc(r))|>select(ags,ags_name,eligible_voters_orig,eligible_voters,number_voters_orig,number_voters,turnout,t1,t2)|>head(8)|>as.data.frame())
print(x|>filter(election_year==1980)|>summarise(mean(number_voters_orig==0),mean(eligible_voters/eligible_voters_orig>1.01)))
print(x|>filter(election_year==1980, ags=="01001000")|>select(ags,ags_name,eligible_voters_orig,eligible_voters,number_voters_orig,number_voters,turnout)|>as.data.frame())
