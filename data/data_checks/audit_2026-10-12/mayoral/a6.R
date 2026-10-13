suppressMessages({library(dplyr);library(data.table)})
D<-"/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/mayoral/"
e<-readRDS(paste0(D,"e.rds")); setDT(e)
x<-e[nna==0&s_hw>0]
print(x[,.(N=.N,lt98=sum(s_hw<0.98),gt102=sum(s_hw>1.02),lt90=sum(s_hw<0.9)),by=state][order(state)])
x[,dec:=floor(yr/10)*10]
print(x[state=="09",.(N=.N,lt98=sum(s_hw<0.98),lt90=sum(s_hw<.9),med=median(s_hw)),by=dec][order(dec)])
print(x[state=="09"&s_hw<0.9][order(s_hw)][1:5,.(eid,s_hw,n,nc)])
print(x[state=="08"&s_hw<0.98][1:5,.(eid,s_hw,n,nc,votes_hw,valid)])
print(x[state=="08",.(.N,lt98=mean(s_hw<.98)),by=yr][order(yr)])
print(x[state=="03"&s_hw<0.98][1:5,.(eid,s_hw,n,nc,votes_hw,valid)])
