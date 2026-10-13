suppressMessages(library(dplyr))
h <- readRDS("data/federal_elections/county_level/final/federal_cty_harm.rds")
print(h %>% group_by(election_year) %>% summarise(pop=sum(population), area=sum(area), nflag=sum(!is.na(flag_unsuccessful_naive_merge)), tmin=min(number_voters/population), tmax=max(number_voters/population), elmax=max(eligible_voters/population)) %>% as.data.frame)
print(h %>% filter(election_year==2021) %>% mutate(r=eligible_voters/population) %>% arrange(desc(r)) %>% select(county_code,population,eligible_voters,r,area) %>% head(3) %>% as.data.frame)
print(h %>% filter(election_year==2021) %>% mutate(r=eligible_voters/population) %>% arrange(r) %>% select(county_code,population,eligible_voters,r,area) %>% head(3) %>% as.data.frame)
# within-county jumps in eligible voters 
j <- h %>% arrange(county_code,election_year) %>% group_by(county_code) %>% mutate(g=eligible_voters/lag(eligible_voters)) %>% ungroup()
print(j %>% filter(g>1.25|g<0.8) %>% select(county_code,election_year,eligible_voters,g) %>% as.data.frame)
# turnout jumps
j2 <- h %>% arrange(county_code,election_year) %>% group_by(county_code) %>% mutate(d=turnout-lag(turnout)) %>% ungroup()
print(j2 %>% filter(abs(d)>0.12) %>% select(county_code,election_year,turnout,d) %>% as.data.frame)
