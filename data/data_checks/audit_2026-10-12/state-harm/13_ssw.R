suppressMessages({library(dplyr)}); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds")
x <- u %>% filter(state=="01") %>% group_by(election_year) %>% summarise(vv=sum(valid_votes,na.rm=T), ssw_pct=100*sum(ssw*valid_votes,na.rm=T)/sum(valid_votes,na.rm=T), n_ssw=sum(!is.na(ssw)), npd_pct=100*sum(npd*valid_votes,na.rm=T)/sum(valid_votes,na.rm=T), other_pct=100*sum(other*valid_votes,na.rm=T)/sum(valid_votes,na.rm=T), n=n())
print(as.data.frame(x))
