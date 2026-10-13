suppressMessages({library(dplyr);library(data.table)})
p<-readRDS("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/mayoral/p.rds"); setDT(p)
setorder(p,ags,election_date)
p[,prev_party:=shift(winner_party),by=ags]; p[,prev_pid:=shift(person_id),by=ags]
s<-p[which(person_id==prev_pid)]
s[,sw:=party_switch==1]
s[,sub:=mapply(function(a,b)grepl(a,b,fixed=TRUE)||grepl(b,a,fixed=TRUE),winner_party,prev_party)]
print(s[,.(n=.N,switch=sum(sw,na.rm=T),switch_rate=round(mean(sw,na.rm=T),3),sw_labelsub=sum(sw&sub,na.rm=T)),by=state][order(state)])
print(s[state=="09"&sw][sample(.N,10,replace=F)][,.(ags,election_date,prev_party,winner_party)])
print(s[sw==TRUE&state=="09",.N,by=.(prev_party,winner_party)][order(-N)][1:10])
cat("pids: same person & party_switch: across all\n")
