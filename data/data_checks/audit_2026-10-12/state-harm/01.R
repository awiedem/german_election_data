suppressMessages(library(dplyr))
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); h25 <- readRDS(paste0(d,"state_harm_25.rds"))
print(tail(names(h25),25)); print(tail(names(u),25))
print(setdiff(names(h25), names(u))); print(setdiff(names(u), names(h25)))
x <- u %>% filter(state=="08", election_year==2021) %>% slice(1:3)
print(as.data.frame(x[,c("ags","valid_votes","afd","cdu","spd","gruene")]))
y <- h25 %>% filter(state=="08", election_year==2021) %>% slice(1:3)
print(as.data.frame(y[,c("ags","valid_votes","afd","cdu","spd","gruene","other","total_vote_share")]))
