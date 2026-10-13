suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
options(width=220)
u$vv <- u$valid_votes/u$number_voters
u$to2 <- u$number_voters/u$eligible_voters
cat("turnout mismatch:", sum(abs(u$turnout-u$to2)>1e-6,na.rm=T),"\n")
print(u %>% filter(abs(turnout-to2)>1e-6) %>% select(ags,ags_name,election_year,turnout,to2,eligible_voters,number_voters) %>% head(20) %>% as.data.frame)
cat("turnout NA:", sum(is.na(u$turnout)), " to2>1:", sum(u$to2>1,na.rm=T),"\n")
print(u %>% filter(!is.na(to2),(to2>0.97|to2<0.15)) %>% select(ags,ags_name,election_year,eligible_voters,number_voters,turnout) %>% arrange(-turnout) %>% head(40) %>% as.data.frame)
print(u %>% filter(turnout>=0.97) %>% count(state,election_year) %>% as.data.frame)
cat("valid>voters in single-vote states:\n")
print(u %>% filter(state %in% c("05","07","09","10"), vv>1.0001) %>% count(state,election_year) %>% as.data.frame)
print(u %>% filter(valid_votes==0|number_voters==0|eligible_voters==0) %>% count(state,election_year))
print(u %>% filter(is.na(eligible_voters)) %>% count(state,election_year))
