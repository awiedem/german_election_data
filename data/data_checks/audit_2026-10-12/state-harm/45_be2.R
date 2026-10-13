suppressMessages({library(dplyr);library(bundeslaendeR)}); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds"); x <- u %>% filter(state=="11", election_year==2021)
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character"); pc <- sc$column[sc$dataset=="state_unharm" & sc$role=="party_share"]
v <- unlist(x[pc]); v <- v[!is.na(v)&v>0]; print(round(100*sort(v,decreasing=TRUE),2)); cat("sum",round(100*sum(v),2),"\n")
b <- as.data.frame(ltw_elections) %>% filter(state=="BE", election_date==as.Date("2021-09-26")) %>% arrange(desc(party_vote_count)); print(b %>% select(partyname_short,party_vote_count,party_vshare) %>% head(12)); cat("bl sum vshare", sum(b$party_vshare),"; sum votes", sum(b$party_vote_count), " vv", b$valid_votes[1], "\n")
w <- readRDS("data/state_elections/final/ltw_wkr_unharm.rds"); z <- w %>% filter(state=="11",election_year==2021,stimme=="zweitstimme"); cat("wkr gruene votes:", round(sum(z$gruene*z$valid_votes)), " u gruene votes:", round(x$gruene*x$valid_votes), " bl:", b$party_vote_count[b$partyname_short=="Grüne"], "\n")
