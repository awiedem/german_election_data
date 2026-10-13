suppressMessages(library(dplyr))
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"));r<-readRDS(paste0(d,"federal_muni_raw.rds"))
print(u%>%filter(election_year%in%c(1994,1998),grepl("^(12|13|14|15)000999$|^1[2-5]999",ags)|grepl("999$",ags)&substr(ags,1,2)%in%c("12","13","14","15"))%>%select(ags,election_year,valid_votes)%>%head(20))
print(r%>%filter(election_year==1998,substr(ags,1,2)%in%c("12","13"),grepl("999$",ags))%>%select(ags,valid_votes))
cat(sum(grepl("999$",u$ags[u$election_year==1998])),"\n")
# 2002 loss source
x<-r%>%filter(election_year==2002,substr(ags,1,2)=="12");print(x%>%filter(!ags%in%u$ags[u$election_year==2002])%>%select(ags,valid_votes)%>%arrange(-valid_votes)%>%head(5))
