x<-readRDS("data/state_elections/final/state_unharm.rds")
pc<-names(x)[13:ncol(x)]; pc<-setdiff(pc,"cdu_csu")
show<-function(st,dt,top=25){d<-x[x$state==st&as.character(x$election_date)==dt,]; w<-ifelse(!is.na(d$valid_votes),d$valid_votes,d$number_voters)
 s<-sapply(pc,function(p) if(all(is.na(d[[p]]))) NA else 100*sum(d[[p]]*w,na.rm=TRUE)/sum(w,na.rm=TRUE)); s<-sort(s[!is.na(s)],decreasing=TRUE)
 cat("\n==",st,dt,"rows",nrow(d),"sum of party cols (excl other)",round(sum(s[names(s)!="other"]),2),"\n"); print(round(head(s,top),2))}
show<-function(st,dt,top=25){d<-x[x$state==st&as.character(x$election_date)==dt,]; w<-ifelse(!is.na(d$valid_votes),d$valid_votes,d$number_voters)
 s<-sapply(pc,function(p) if(all(is.na(d[[p]]))) NA else 100*sum(d[[p]]*w,na.rm=TRUE)/sum(w,na.rm=TRUE)); s<-sort(s[!is.na(s)],decreasing=TRUE); cat("\n==",st,dt,"\n"); print(round(head(s,top),2))}
for(d in unique(as.character(x$election_date[x$state=="04"&x$election_year%in%c(1967,1983,1979,1971,1999,2019)]))) show("04",d)
