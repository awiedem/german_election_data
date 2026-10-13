suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
h<-readRDS(paste0(d,"federal_muni_harm_21.rds"));h25<-readRDS(paste0(d,"federal_muni_harm_25.rds"))
cat("harm25 has pop_cw:","pop_cw"%in%names(h25),"\n")
yrs<-sort(unique(h$election_year)); 
ct<-h|>count(election_year); print(ct)
# municipalities in 2017 but not 2021
a<-h|>filter(election_year==2017)|>pull(ags_21); b<-h|>filter(election_year==2021)|>pull(ags_21)
cat("2017 not in 2021:",length(setdiff(a,b)),"; 2021 not in 2017:",length(setdiff(b,a)),"\n")
miss<-setdiff(a,b); 
print(h|>filter(election_year==2017,ags_21%in%miss)|>summarise(n=n(),nv=sum(number_voters),vv=sum(valid_votes)))
print(head(h|>filter(election_year==2017,ags_21%in%miss)|>select(ags,ags_21,ags_name,number_voters),8)|>as.data.frame())
# turnout ranges 2021 harm
print(h|>group_by(election_year)|>summarise(tmin=min(turnout,na.rm=T),tmax=max(turnout,na.rm=T),n_t1=sum(turnout>=0.999,na.rm=T),n_low=sum(turnout<0.4,na.rm=T))|>as.data.frame())
# key col
cat("ags vs ags_21 col in h:", all(c("ags","ags_21")%in%names(h)),"; ags==ags_21:",mean(h$ags==h$ags_21),"\n")
cat("dups ags_21-year:",sum(duplicated(h[,c("ags_21","election_year")])),"\n")
