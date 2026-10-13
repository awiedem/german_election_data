suppressMessages({library(dplyr)}); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds")
print(grep("republik|rep|wut|biw|pirat|linke|wasg|pds|tier|dvu|volksunion|ssw|gruen|b90|buendnis|bündnis|fw|wahler|waehler|npd|schill|offensive|dsu", names(u), value=TRUE))
chk <- function(st, yr, cols){ x<-u%>%filter(state==st,election_year==yr); vv<-sum(x$valid_votes,na.rm=T); cols<-intersect(cols,names(x)); cat(st,yr, paste(cols, round(sapply(cols,function(p) 100*sum(x[[p]]*x$valid_votes,na.rm=T)/vv),2),collapse="; "),"\n")}
for (a in list(c("09",1990),c("09",1994),c("09",1998),c("09",2003),c("03",1994))) chk(a[1],as.numeric(a[2]),c("rep","die_republikaner_rep","republikaner"))
for (a in list(c("04",2015),c("04",2019),c("04",2011))) chk(a[1],as.numeric(a[2]),c("biw","buerger_in_wut","buerger_in_wut_biw","bürger_in_wut"))
for (a in list(c("08",2006),c("07",2006),c("15",1990),c("15",1994),c("12",1990))) chk(a[1],as.numeric(a[2]),c("linke_pds","pds","wasg","gruene","buendnis_90","gruene_buendnis_90","buendnis_90_gruene","neues_forum","b90"))
