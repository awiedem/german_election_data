suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
options(width=250)
u$vv <- u$valid_votes/u$number_voters
print(u %>% filter(state=="01") %>% group_by(election_year) %>% summarise(q10=quantile(vv,.1),q50=median(vv),q90=quantile(vv,.9),share_lt1.05=mean(vv<1.05),n=n()) %>% as.data.frame)
print(u %>% filter(state=="01") %>% mutate(sz=cut(eligible_voters,c(0,500,2000,10000,1e7))) %>% group_by(election_year,sz) %>% summarise(m=median(vv),.groups="drop") %>% tidyr::pivot_wider(names_from=sz,values_from=m) %>% as.data.frame)
# BW: vv by size
print(u %>% filter(state=="08") %>% group_by(election_year) %>% summarise(q10=quantile(vv,.1),q50=median(vv),q90=quantile(vv,.9)) %>% as.data.frame)
