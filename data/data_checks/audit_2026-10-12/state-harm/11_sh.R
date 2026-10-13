suppressMessages({library(dplyr);library(tidyr)}); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds")
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character")
pc <- sc$column[sc$dataset=="state_unharm" & sc$role=="party_share"]
tot <- function(st,yr){ x<-u%>%filter(state==st,election_year==yr); v<-sum(x$valid_votes,na.rm=T); r<-sapply(pc,function(p) sum(x[[p]]*x$valid_votes,na.rm=T)); r<-c(r,other=sum(x$other*x$valid_votes,na.rm=T)); r<-r[r>0]; data.frame(st=st,yr=yr,vv=v,party=names(r),votes=round(r),pct=round(100*r/v,2)) %>% arrange(desc(votes)) }
for (a in list(c("01",2009),c("01",2000),c("12",1994),c("03",1994),c("13",2016))) {cat("\n"); print(tot(a[1],as.numeric(a[2])) %>% head(14))}
