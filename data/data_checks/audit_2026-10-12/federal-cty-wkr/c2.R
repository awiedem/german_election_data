suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
print(u %>% filter(number_voters>eligible_voters | (valid_votes+invalid_votes)!=number_voters | is.infinite(turnout)) %>% select(ags,year,eligible_voters,number_voters,valid_votes,invalid_votes,turnout) %>% as.data.frame)
cat("flag briefwahl:\n"); print(table(u$year[u$flag_briefwahl_agg==1], u$ags[u$flag_briefwahl_agg==1]))
# 999 rows
print(u %>% filter(grepl("999$",ags)) %>% count(year,flag_briefwahl_agg))
m <- readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
cat(names(m)[1:30],"\n")
print(table(m$election_year))
