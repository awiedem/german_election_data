suppressMessages(library(dplyr))
r<-function(p)readRDS(p)
# LU-03
for(n in c("state_harm_21","state_harm_23","state_harm_25")){x<-r(sprintf("data/state_elections/final/%s.rds",n));cat(n,median(x$pop_density_ags,na.rm=T),max(x$pop_density_ags,na.rm=T),sum(!is.na(x$pop_density_ags)),"\n")
 if(n=="state_harm_25") print(x[x$ags=="11000000"&x$election_year==2016,c("ags","pop_density_ags","population_ags","area_ags")][1,])}
a<-r("data/covars_municipality/final/ags_area_pop_emp.rds");cat("cov med",median(a$pop_density_ags,na.rm=T),sum(is.infinite(a$pop_density_ags)),sum(a$population==0,na.rm=T),"\n")
print(a[is.infinite(a$pop_density_ags),][1:9,]); 
a3<-r("data/covars_municipality/final/ags_area_pop_emp_2023.rds");cat("cov23 med",median(a3$pop_density_ags,na.rm=T),"\n")
print(grep("pop|area",names(a),value=T))
# LU-04
m<-r("data/federal_elections/municipality_level/final/federal_muni_raw.rds")
cat("raw spd 2021",sum(m$spd[m$election_year==2021],na.rm=T),max(m$spd,na.rm=T),nrow(m),"\n")
print(summary(m$pop)); print(grep("pop|area|mailin",names(m),value=T))
# LU-05
cw<-r("data/crosswalks/final/ags_crosswalks.rds");print(class(cw$ags));print(sum(nchar(as.character(cw$ags))==5)); print(as.character(2000000))
print(sum(cw$ags<1e7))
# LU-06
g<-cw%>%group_by(ags,year)%>%summarise(s=sum(pop_cw),.groups="drop")%>%filter(abs(s-1)>1e-6);print(nrow(g));print(summary(g$s));print(length(unique(g$ags)));print(table(substr(sprintf("%08.0f",g$ags),1,2)))
print(cw[cw$ags==9472444&cw$year==1990,][1:3,])
