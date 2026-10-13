suppressMessages(library(dplyr))
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds")); h<-readRDS(paste0(d,"federal_muni_harm_21.rds"))
r<-readRDS(paste0(d,"federal_muni_raw.rds"))
x17<-u$ags_21[0]
a17<-unique(h$ags[h$election_year==2017]);a21<-unique(h$ags[h$election_year==2021]);cat("harm21 ags in 2017 not 2021:",length(setdiff(a17,a21)),"\n")
miss<-setdiff(a17,a21);print(table(substr(miss,1,2)));cat(sum(h$valid_votes[h$election_year==2017&h$ags%in%miss]),"\n")
print(u%>%filter(election_year==1994,ags=="11000000")%>%select(pop,area))
print(summary(u$pop[u$election_year==2013]));print(u%>%filter(ags=="11000000")%>%select(election_year,pop)%>%head(14))
print(h%>%filter(ags=="11000000",election_year==2017)%>%select(population))
print(names(h)[grepl("flag_total",names(h))])
dv<-h$valid_votes+h$invalid_votes-h$number_voters;print(table(dv)[abs(as.numeric(names(table(dv))))<10])
# 999 rows
x<-r%>%filter(grepl("999$",ags),election_year%in%c(1994,1998,2002));print(table(x$election_year,substr(x$ags,1,2)))
print(r%>%filter(election_year==2002,ags=="12000999")%>%select(ags,valid_votes))
print(sum(r$valid_votes[r$election_year==1998&grepl("999$",r$ags)]))
