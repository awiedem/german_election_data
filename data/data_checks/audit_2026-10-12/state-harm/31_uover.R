suppressMessages({library(dplyr)}); options(width=250, dplyr.print_max=300)
u <- readRDS("data/state_elections/final/state_unharm.rds")
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character"); pc <- sc$column[sc$dataset=="state_unharm" & sc$role=="party_share"]
M <- as.matrix(u[pc]); u$S <- rowSums(M,na.rm=T); u$S[rowSums(!is.na(M))==0] <- NA
cat("unharm S>1.001:",sum(u$S>1.001,na.rm=T)," >1.01:",sum(u$S>1.01,na.rm=T)," >1.05:",sum(u$S>1.05,na.rm=T),"\n")
print(u %>% filter(S>1.01) %>% select(ags,election_year,valid_votes,S) %>% arrange(desc(S)) %>% head(15) %>% as.data.frame())
print(u %>% filter(S>1.01) %>% count(state,election_year,sort=T) %>% head(10) %>% as.data.frame())
# state-year weighted
sy <- u %>% group_by(state,election_year) %>% summarise(S=sum((S+other)*valid_votes,na.rm=T)/sum(valid_votes[!is.na(S)],na.rm=T),.groups="drop"); print(sy %>% filter(abs(S-1)>0.005) %>% as.data.frame())
