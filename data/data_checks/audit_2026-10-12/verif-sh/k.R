suppressMessages(library(dplyr))
j<-read.csv(file.path("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/verif-sh","tot.csv"),colClasses=c(state="character"))
print(j%>%filter((state=="12"&election_year%in%c(1990,1994,1999))|(state=="14"&election_year==2004)|(state=="13"&election_year==1990))%>%select(state,election_year,va,valid_votes,el,electorate))
