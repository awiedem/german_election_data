suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
options(width=250)
print(u %>% filter(turnout>=0.97, state %in% c("09","16","08","14"), election_year>=1990) %>% select(ags,ags_name,election_year,eligible_voters,number_voters,valid_votes,cdu_csu,spd,other) %>% as.data.frame)
