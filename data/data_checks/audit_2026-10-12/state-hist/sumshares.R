x<-readRDS("data/state_elections/final/state_unharm.rds")
pc<-setdiff(names(x)[13:ncol(x)],c("cdu_csu"))
m<-as.matrix(x[pc]); s<-rowSums(m,na.rm=TRUE); allna<-rowSums(!is.na(m))==0
x$s<-s; x$allna<-allna
cat("rows all party NA:",sum(allna),"\n")
cat("rows sum outside [0.99,1.01]:",sum(abs(s-1)>0.01 & !allna),"of",nrow(x),"\n")
cat("rows sum outside [0.95,1.05]:",sum(abs(s-1)>0.05 & !allna),"\n")
cat("max/min sum:",range(s[!allna]),"\n")
a<-aggregate(cbind(bad1=abs(s-1)>0.01&!allna,bad5=abs(s-1)>0.05&!allna,n=1,mean_s=s)~state+election_year,x,function(z) sum(z)); a$mean_s<-aggregate(s~state+election_year,x,mean)$s
a<-a[a$bad1>0,]; a$share_bad<-round(a$bad1/a$n,3); a<-a[order(-a$bad5,-a$share_bad),]
options(width=200); print(head(a,60),row.names=FALSE)
write.csv(a,file.path("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/state-hist","sumshares_by_election.csv"),row.names=FALSE)
