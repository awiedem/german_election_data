suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
cw <- readRDS("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds")
cwc <- cw %>% distinct(ags,year)
options(width=200)
a <- u %>% filter(state=="16",election_year==2019) %>% anti_join(cwc %>% filter(year==2019),by="ags")
print(a %>% select(ags,ags_name,eligible_voters,turnout) %>% head(40))
# sachsen 1994
b <- u %>% filter(state=="14",election_year==1994); print(head(b$ags,10)); print(head(cwc %>% filter(year==1994,substr(ags,1,2)=="14") %>% pull(ags),10))
# TH 2024 vs cw 2024 missing
c24 <- cwc %>% filter(year==2024, substr(ags,1,2)=="16") %>% anti_join(u %>% filter(state=="16",election_year==2024),by="ags")
print(nrow(c24)); 
c24n <- cw %>% filter(year==2024, ags %in% c24$ags) %>% distinct(ags,ags_name); print(head(c24n,40))
