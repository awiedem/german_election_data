suppressMessages({library(dplyr);library(data.table)})
cc<-readRDS("data/mayoral_elections/final/mayoral_candidates.rds"); setDT(cc)
cat("=== party labels (top by state-agnostic) ===\n")
tab<-cc[,.N,by=candidate_party][order(-N)]
print(tab[1:60])
cat("distinct parties:",nrow(tab),"\n")
# case/whitespace variants
tab[,k:=tolower(gsub("[^a-z0-9äöü]","",tolower(candidate_party)))]
v<-tab[!is.na(k),.(n=.N,labs=paste(candidate_party,collapse=" | ")),by=k][n>1][order(-n)]
print(v[1:40]); cat("normalized-collision keys:",nrow(v),"\n")
cat("empty/whitespace parties:",sum(trimws(cc$candidate_party)=="",na.rm=T)," NA:",sum(is.na(cc$candidate_party)),"\n")
print(cc[,.(na=sum(is.na(candidate_party)),empty=sum(candidate_party=="",na.rm=T),N=.N),by=state][order(state)])
