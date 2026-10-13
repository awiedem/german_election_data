suppressMessages(library(dplyr)); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds")
cw <- readRDS("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds"); print(names(cw))
x <- cw %>% filter(ags_25 %in% c("14524170","14627020"), year==1994); print(as.data.frame(x))
src <- unique(x$ags)
print(u %>% filter(ags %in% src, election_year==1994) %>% select(ags,eligible_voters,number_voters,valid_votes,invalid_votes,turnout) %>% as.data.frame())
# all sources of Lichtentanne weights
