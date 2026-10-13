x<-readRDS("data/state_elections/final/state_unharm.rds")
pc<-names(x)[13:ncol(x)]; pc<-setdiff(pc,"cdu_csu")
show<-function(st,dt,top=25){d<-x[x$state==st&as.character(x$election_date)==dt,]; w<-ifelse(!is.na(d$valid_votes),d$valid_votes,d$number_voters)
 s<-sapply(pc,function(p) if(all(is.na(d[[p]]))) NA else 100*sum(d[[p]]*w,na.rm=TRUE)/sum(w,na.rm=TRUE)); s<-sort(s[!is.na(s)],decreasing=TRUE)
 cat("\n==",st,dt,"rows",nrow(d),"sum of party cols (excl other)",round(sum(s[names(s)!="other"]),2),"\n"); print(round(head(s,top),2))}
show("15","1994-06-26"); show("08","1952-03-09"); show("01","2000-02-27"); show("01","2009-09-27"); show("09","1954-11-28"); show("09","1966-11-20"); show("09","1990-10-14"); show("11","1990-12-02"); show("13","1990-10-14")
