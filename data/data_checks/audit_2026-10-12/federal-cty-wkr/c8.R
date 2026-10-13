suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
print(u %>% filter(year %in% c(1957,1961,1965,1969)) %>% group_by(year) %>% summarise(across(c(cdu,csu,spd,fdp,dp,gb_bhe), ~round(sum(.x*number_voters,na.rm=T))), valid=sum(valid_votes), nv=sum(number_voters)) %>% as.data.frame)
print(u %>% filter(ags=="01001", year %in% 1957:1965) %>% select(year,number_voters,valid_votes,cdu,spd,fdp,dp,gb_bhe) %>% as.data.frame)
