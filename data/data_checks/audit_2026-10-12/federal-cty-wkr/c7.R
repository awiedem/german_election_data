suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
pcols <- names(u)[which(names(u)=="cdu"):which(names(u)=="zentrum")]
cat(length(pcols),"party cols\n")
S <- as.matrix(u[,pcols]); 
cat("range:", range(S,na.rm=T),"\n")
cat("share>1 cells:", sum(S>1,na.rm=T), " neg:", sum(S<0,na.rm=T),"\n")
u$sumsh <- rowSums(S,na.rm=T)
u$votes_tot <- u$sumsh*u$number_voters
u$ratio <- u$votes_tot/u$valid_votes
print(u %>% filter(flag_briefwahl_agg==0|is.na(flag_briefwahl_agg)) %>% group_by(year) %>% summarise(min=min(ratio),med=median(ratio),max=max(ratio), n_off=sum(abs(ratio-1)>0.001), n_off1=sum(abs(ratio-1)>0.01)) %>% as.data.frame)
print(u %>% filter(abs(ratio-1)>0.01) %>% select(ags,year,ratio,valid_votes,number_voters) %>% head(20) %>% as.data.frame)
cat("NA vs 0 per year for afd, gruene, linke_pds, bsw, csu\n")
for (p in c("afd","gruene","linke_pds","bsw","csu","cdu","spd","fdp")) {cat(p,"\n"); print(u %>% group_by(year) %>% summarise(na=sum(is.na(.data[[p]])), zero=sum(.data[[p]]==0,na.rm=T), n=n()) %>% filter(year>=2009|p%in%c("cdu","spd","fdp")) %>% as.data.frame %>% head(25))}
