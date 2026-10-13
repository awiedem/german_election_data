suppressMessages(library(dplyr))
j<-readRDS("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/xd/j_fs.rds")
# state-level sums, matched municipalities only
g<-j%>%filter(abs(fy-sy)<=1)%>%group_by(state,fy,sy)%>%summarise(n=n(),fed=sum(fe),st=sum(se),r=st/fed,sh_out=mean(r_ <- (se/fe)<.67|(se/fe)>1.5),.groups="drop")
print(as.data.frame(g%>%filter(abs(r-1)>0.08|sh_out>0.05)),row.names=F)
