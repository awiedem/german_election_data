suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
options(width=250)
print(u %>% filter(ags %in% c("01002000","01003000","01001000")) %>% mutate(vv=valid_votes/number_voters) %>% select(ags,election_year,eligible_voters,number_voters,valid_votes,vv,cdu_csu,spd,gruene,other,seats_cdu_csu) %>% as.data.frame)
