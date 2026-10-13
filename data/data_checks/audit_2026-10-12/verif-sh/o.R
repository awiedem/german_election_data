suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
print(x%>%filter(state=="04",election_year %in% c(1979,1983))%>%select(ags,election_year,al,gruene,other)%>%as.data.frame())
for(f in c("state_harm_21","state_harm_25")){h<-readRDS(paste0("data/state_elections/final/",f,".rds"));print(unique(h$election_date[h$state=="04"&h$election_year==1975]))}
print(sapply(c("bayernpartei_bp","bp","die_republikaner_rep","rep","ssw","grue_nf","bue90_gruene","npd","republikaner"),function(p) if(p%in%names(x)) paste(sort(unique(x$state[!is.na(x[[p]])])),collapse=",") else "absent"))
