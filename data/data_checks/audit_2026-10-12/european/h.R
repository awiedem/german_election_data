suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
h<-readRDS("data/european_elections/final/european_muni_harm.rds")
m<-setdiff(u$ags[u$election_year==2024],h$ags[h$election_year==2024]); print(m)
print(u%>%filter(election_year==2024,ags%in%m)%>%select(ags,state_name,eligible_voters,number_voters,valid_votes))
a24<-h$ags[h$election_year==2024]; a19<-h$ags[h$election_year==2019]
x<-setdiff(a19,a24); print(table(substr(x,1,2)))
print(h%>%filter(election_year==2019,ags%in%head(x,8))%>%select(ags,state_name,eligible_voters,n_predecessors,flag_aggregated))
# extra 2024 harm vs unharm in TH/SN
uu<-u$ags[u$election_year==2024]; print(setdiff(a24,uu)); 
cw<-ls()
for(f in c("ags_crosswalks")){ p<-list.files("data/crosswalks/final",pattern=paste0(f,"\\.rds"),full.names=T); print(p)}
cw<-readRDS("data/crosswalks/final/ags_crosswalks.rds"); print(names(cw)); print(cw%>%filter(ags%in%c("01051008","01051033"))%>%head(6))
print(cw%>%filter(year==2019,ags%in%x)%>%summarise(n=n(),nu=n_distinct(ags_21)))
print(cw%>%filter(year==2019,ags%in%x)%>%head(5))
