suppressMessages({library(dplyr)}); options(width=250, dplyr.print_max=300)
for (n in c("25")) {
h <- readRDS(sprintf("data/state_elections/final/state_harm_%s.rds",n))
cat("harm",n,": flag_other_party_residual==1:", sum(h$flag_other_party_residual==1,na.rm=T), "of", nrow(h), sprintf("(%.1f%%)\n",100*mean(h$flag_other_party_residual==1,na.rm=T)))
cat("tvs>1.001:", sum(h$total_vote_share>1.001,na.rm=T), " tvs>1.01:", sum(h$total_vote_share>1.01,na.rm=T), " tvs>1.05:", sum(h$total_vote_share>1.05,na.rm=T), " max", max(h$total_vote_share,na.rm=T), "\n")
cat("with valid_votes>=500 and tvs>1.01:\n"); print(h %>% filter(valid_votes>=500, total_vote_share>1.01) %>% select(ags,election_year,valid_votes,total_vote_share) %>% arrange(desc(total_vote_share)) %>% head(15) %>% as.data.frame())
cat("by state-year, valid>=500 & tvs>1.01:\n"); print(h %>% filter(valid_votes>=500, total_vote_share>1.01) %>% count(state,election_year) %>% as.data.frame())
cat("state-year level tvs weighted:\n"); sy <- h %>% group_by(state,election_year) %>% summarise(tvs=sum(total_vote_share*valid_votes,na.rm=T)/sum(valid_votes,na.rm=T),.groups="drop"); print(sy %>% filter(tvs>1.002|tvs<0.9) %>% as.data.frame())
}
