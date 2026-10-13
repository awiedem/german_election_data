suppressMessages(library(dplyr))
x<-readRDS("data/state_elections/final/state_unharm.rds")
yrs<-function(s,p) if(p%in%names(x)) paste(sort(unique(x$election_year[x$state==s&!is.na(x[[p]])])),collapse=" ") else "absent"
for(p in c("gruene","grue_nf","bue90_gruene","b90","b90_gr")) cat("ST",p,yrs("15",p),"\n")
for(p in c("bayernpartei_bp","bp","die_republikaner_rep","rep")) cat("BY",p,yrs("09",p),"\n")
for(p in c("freiewaehler","freie_wahler","freie_waehler")) cat("ST",p,yrs("15",p),"\n")
for(p in c("gruene","b90","b90_gr","bundnis_90_grune")) cat("MV",p,yrs("13",p),"| BE",yrs("11",p),"\n")
cat("max die_rep BY:", max(x$die_republikaner_rep,na.rm=T),"\n")
cat(grep("grue|gruen|b90|bundnis",names(x),value=T),"\n")
