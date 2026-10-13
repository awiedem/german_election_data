suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
h<-readRDS("data/european_elections/final/european_muni_harm.rds")
pc<-names(u)[which(names(u)=="abg"):which(names(u)=="x50plus")]
for(d in list(u,h)){
 for(y in c(2009,2014,2019,2024)){x<-d[d$election_year==y,]
  cn<-sapply(pc,function(p) sum(x[[p]]*x$number_voters,na.rm=T))
  cat(y,"sum counts",round(sum(cn)),"valid",sum(x$valid_votes,na.rm=T),"\n")
  print(round(cn[c("cdu","csu","spd","gruene","afd","die_linke","fdp","volt","die_partei","freie_waehler","bsw")]))
 }}
# row-level
x<-u
x$sh<-rowSums(x[pc],na.rm=T)
x$r<-x$sh*x$number_voters/x$valid_votes
print(x%>%group_by(election_year)%>%summarise(min=min(r,na.rm=T),q1=quantile(r,.01,na.rm=T),med=median(r,na.rm=T),q99=quantile(r,.99,na.rm=T),max=max(r,na.rm=T),nNA=sum(is.na(r)),n_out=sum(abs(r-1)>0.02,na.rm=T)))
print(x%>%filter(abs(r-1)>0.02)%>%select(ags,election_year,number_voters,valid_votes,sh,r)%>%arrange(desc(abs(r-1)))%>%head(15))
