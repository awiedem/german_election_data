suppressMessages({library(dplyr)})
r <- readRDS("data/federal_elections/wahlkreis_level/final/federal_wkr_2021_on_2025.rds")
cw <- readRDS("data/federal_elections/wahlkreis_level/final/wkr_2021_to_2025_crosswalk.rds")
w <- readRDS("data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.rds") %>% filter(election_year==2021)
j <- r %>% inner_join(cw %>% select(wkr_nr,prior=prior_2021_wkr_nr),by="wkr_nr") %>% filter(boundary_change=="unchanged") %>% inner_join(w %>% select(wkr_nr,stimme,wkr_name_a=wkr_name,eligible_a=eligible_voters,valid_a=valid_votes,spd_a=spd,cdu_a=cdu,afd_a=afd),by=c("prior"="wkr_nr","stimme"))
cat(nrow(j),"\n")
print(j %>% summarise(dE=max(abs(eligible_voters-eligible_a)), dV=max(abs(valid_votes-valid_a)), dspd=max(abs(spd-spd_a)), dafd=max(abs(afd-afd_a))))
print(j %>% filter(abs(valid_votes-valid_a)>0) %>% select(wkr_nr,prior,wkr_name,wkr_name_a,valid_votes,valid_a) %>% head() %>% as.data.frame)
# name consistency
print(j %>% filter(stimme=="erststimme", gsub("[–-]"," ",wkr_name)!=gsub("[–-]"," ",wkr_name_a)) %>% select(wkr_nr,prior,wkr_name,wkr_name_a) %>% as.data.frame)
# sanity: redrawn rows, shares of valid/eligible
x <- r %>% filter(boundary_change %in% c("redrawn","new")) %>% mutate(v=valid_votes/eligible_voters) %>% select(wkr_nr,wkr_name,stimme,boundary_change,eligible_voters,valid_votes,v)
print(summary(x$v)); print(x %>% filter(v<0.6|v>0.9) %>% as.data.frame)
cat("r turnout columns absent: number_voters/invalid/turnout? ", c("number_voters","invalid_votes","turnout") %in% names(r),"\n")
# party columns in r with fractional vote counts
pc <- c("spd","cdu","csu","afd","gruene","fdp","linke_pds")
print(sapply(pc, function(p) max(abs(r[[p]]*r$valid_votes - round(r[[p]]*r$valid_votes)),na.rm=T)))
print(head(r[r$boundary_change=="redrawn", c("wkr_nr","stimme","valid_votes","spd")],4))
# Redrawn: Zweitstimme cdu+csu sums plausibility: csu outside Bavaria
print(r %>% filter(state!="09", csu>0) %>% nrow)
print(r %>% filter(state=="09", cdu>0) %>% nrow)
