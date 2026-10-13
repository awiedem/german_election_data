suppressMessages({library(dplyr)}); options(width=250)
h <- readRDS("data/state_elections/final/state_harm_25.rds")
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character"); pc <- sc$column[sc$dataset=="state_harm_25" & sc$role=="party_share"]
top <- function(st,yr){ x<-h%>%filter(state==st,election_year==yr); vv<-sum(x$valid_votes,na.rm=T); w<-sapply(c(pc,"other"),function(p) 100*sum(x[[p]]*x$valid_votes,na.rm=T)/vv); w<-sort(w[w>0.3],decreasing=T); cat(st,yr,"n",nrow(x),"\n"); print(round(w,2))}
top("15",1990); top("15",1994)
cat("\nfar_right vs rep: BY\n")
print(as.data.frame(h %>% filter(state=="09", election_year %in% c(1994,1998,2003,2008,2013)) %>% group_by(election_year) %>% summarise(far_right=100*sum(far_right*valid_votes,na.rm=T)/sum(valid_votes), die_rep=100*sum(die_republikaner_rep*valid_votes,na.rm=T)/sum(valid_votes), npd=100*sum(npd*valid_votes,na.rm=T)/sum(valid_votes))))
cat("\nfar_right 0 where no right parties NA: rows far_right==0 & afd,npd,rep... all NA?\n")
fr <- c("afd","npd","rep","die_rechte","dvu","iii_weg","fap","ddd","dsu"); fr <- intersect(fr,names(h))
allna <- rowSums(!is.na(h[fr]))==0
cat("rows with all far_right cols NA:", sum(allna), " of which far_right==0:", sum(allna & h$far_right==0, na.rm=T), " far_right NA:", sum(allna & is.na(h$far_right)),"\n")
cat("cdu_csu==0 & cdu,csu NA rows:", sum(h$cdu_csu==0 & is.na(h$cdu) & is.na(h$csu)), "\n")
print(h %>% filter(cdu_csu==0|is.na(cdu_csu)) %>% count(state,election_year) %>% as.data.frame())
