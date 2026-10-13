suppressMessages({library(dplyr);library(data.table)})
D<-"/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/mayoral/"
e<-readRDS(paste0(D,"e.rds")); setDT(e)
e[,yrb:=cut(yr,c(1944,1959,1979,1999,2009,2019,2030))]
print(e[state=="09",.(N=.N,off02=mean(abs(s_hw-1)>0.02),med=median(s_hw),shortfall=mean(rel< -0.02)),by=yrb])
print(e[state!="09",.(N=.N,off02=mean(abs(s_hw-1)>0.02),shortfall=mean(rel< -0.02,na.rm=T)),by=state][order(state)])
# non-Bayern examples
print(e[state%in%c("03","05","06","08")&abs(s_hw-1)>0.05][order(-abs(s_hw-1))][1:3, .(eid,s_hw,votes_hw,valid,n,nc)])
print(e[state%in%c("03","05","06","08")&abs(s_hw-1)>0.05,.N,by=.(state,yr)][order(state,yr)],nrows=60)
