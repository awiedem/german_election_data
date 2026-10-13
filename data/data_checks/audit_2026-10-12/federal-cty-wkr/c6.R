suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
pc <- c("cdu","csu","spd","fdp","gruene","linke_pds","afd","npd")
r <- u %>% group_by(year) %>% summarise(elig=sum(eligible_voters,na.rm=T), nv=sum(number_voters), valid=sum(valid_votes), inv=sum(invalid_votes), across(all_of(pc), ~sum(.x*number_voters,na.rm=T)))
print(as.data.frame(r %>% mutate(turn=nv/elig, across(all_of(pc), ~round(.x/valid*100,2)))), digits=10)
# any share >1, share sums
sh <- u %>% mutate(s=rowSums(across(cdu:zentrum),na.rm=T))
