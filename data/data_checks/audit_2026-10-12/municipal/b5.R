suppressMessages(library(dplyr))
options(width=250)
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds") %>% filter(state=="09")
w <- u %>% select(ags,ags_name,election_year,eligible_voters,turnout) %>% tidyr::pivot_wider(names_from=election_year,values_from=c(eligible_voters,turnout))
w$r14_20 <- w$eligible_voters_2020/w$eligible_voters_2014; w$r20_26 <- w$eligible_voters_2026/w$eligible_voters_2020; w$r08_14 <- w$eligible_voters_2014/w$eligible_voters_2008;w$r02_08 <- w$eligible_voters_2008/w$eligible_voters_2002
for(v in c("r02_08","r08_14","r14_20","r20_26")) cat(v,"outside [0.75,1.33]:",sum(w[[v]]<0.75|w[[v]]>1.33,na.rm=T),"of",sum(!is.na(w[[v]])),"\n")
# both sides: 2020 value is off vs both 2014 and 2026
bad <- w %>% filter((eligible_voters_2020/eligible_voters_2014<0.75|eligible_voters_2020/eligible_voters_2014>1.33),(eligible_voters_2020/eligible_voters_2026<0.75|eligible_voters_2020/eligible_voters_2026>1.33))
cat("2020 outlier vs both neighbours:",nrow(bad),"\n"); print(bad %>% select(ags,ags_name,eligible_voters_2014,eligible_voters_2020,eligible_voters_2026,turnout_2020) %>% head(25) %>% as.data.frame)
# is the 2020 value some other municipality's value? check match to another ags's 2014/2026 eligible in same county
print(w %>% filter(ags %in% c("09161000","09171116","09171131")) %>% select(ags,ags_name,starts_with("eligible")) %>% as.data.frame)
tot <- u %>% group_by(election_year) %>% summarise(e=sum(eligible_voters),n=n()); print(as.data.frame(tot))
