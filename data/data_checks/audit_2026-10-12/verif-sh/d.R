suppressMessages({library(readxl);library(dplyr)})
for(f in c("2023","2021")){p<-paste0("data/state_elections/raw/Landtagswahlen/Berlin/Berlin_",f,"_AGH_BVV.xlsx")
d<-read_excel(p,sheet="AGH_W2",.name_repair="minimal")
v<-sum(as.numeric(d[["Gültige Stimmen"]]),na.rm=T)
for(n in c("SPD","GRÜNE","Graue Panther","Die Grauen","CDU")) if(n%in%names(d)) cat(f,n,sum(as.numeric(d[[n]]),na.rm=T),sum(as.numeric(d[[n]]),na.rm=T)/v,"\n")
cat(f,"valid",v,"\n")}
