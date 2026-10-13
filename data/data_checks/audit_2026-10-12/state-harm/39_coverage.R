suppressMessages({library(dplyr)}); options(width=250, dplyr.print_max=300)
for (n in c("21","25")) {
h <- readRDS(sprintf("data/state_elections/final/state_harm_%s.rds",n))
c1 <- h %>% count(state,election_year) %>% group_by(state) %>% mutate(mx=max(n), r=n/mx) %>% ungroup()
cat("\nharm",n,": state-years with n < 98% of that state's max:\n"); print(c1 %>% filter(r<0.98) %>% as.data.frame())
}
# zero valid_votes rows, NA valid
h <- readRDS("data/state_elections/final/state_harm_25.rds")
cat("\nharm25 rows valid_votes==0:", sum(h$valid_votes==0,na.rm=T), " NA:", sum(is.na(h$valid_votes)), "; eligible==0:", sum(h$eligible_voters==0,na.rm=T), "\n")
print(h %>% filter(valid_votes==0 | eligible_voters==0) %>% count(state,election_year) %>% as.data.frame() %>% head(20))
cat("\nlarge jumps: eligible ratio vs previous election within ags (harm25), ratio>3 or <1/3 (ev>=500)\n")
j <- h %>% filter(!is.na(eligible_voters), eligible_voters>0) %>% arrange(ags, election_year) %>% group_by(ags) %>% mutate(prev=lag(eligible_voters), prevy=lag(election_year), r=eligible_voters/prev) %>% ungroup() %>% filter(!is.na(r), (r>3|r<1/3), eligible_voters>=500|prev>=500)
print(j %>% count(state, election_year, sort=TRUE) %>% head(15) %>% as.data.frame()); print(head(j %>% select(ags,ags_name,election_year,prevy,prev,eligible_voters,r) %>% arrange(desc(abs(log(r)))),12) %>% as.data.frame())
