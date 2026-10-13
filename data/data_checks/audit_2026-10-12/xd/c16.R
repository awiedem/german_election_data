suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
cv<-readRDS("data/covars_municipality/final/ags_area_pop_emp.rds")
cat("Inf density:",sum(is.infinite(cv$pop_density_ags)),"NaN:",sum(is.nan(cv$pop_density_ags)),"area==0:",sum(cv$area_ags==0,na.rm=T),"NA dens",sum(is.na(cv$pop_density_ags)),"\n")
print(head(cv%>%filter(is.infinite(pop_density_ags))%>%as.data.frame(),5))
cv$pop<-cv$population_ags
x<-cv%>%group_by(ags_21)%>%arrange(year)%>%mutate(g=pop/lag(pop))%>%ungroup()%>%filter(pop>1,lag(pop)>1)
print(as.data.frame(x%>%filter(g>2|g<.5)%>%select(ags_21,ags_name_21,year,pop,g)%>%head(10)));cat("n jumps",sum(x$g>2|x$g<.5,na.rm=T),"\n")
