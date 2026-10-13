x<-readRDS("data/state_elections/final/state_unharm.rds"); n<-names(x)
print(n[grepl("gr|b90|linke|pds|fdp|^f_d|afd|npd|rep|^dp|ssw|^fw|freie_w|pirat|schill|wasg|^kpd|^gb|bhe|^bp|zentrum|llsh|statt",n)])
for(v in c("gruene","b90_gr","b90","linke_pds","pds","fdp","fdp_dvp")) if(v %in% n){cat(v,"nonNA by state: "); print(tapply(!is.na(x[[v]]),x$state,sum))}
