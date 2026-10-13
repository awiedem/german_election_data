suppressMessages({library(dplyr);library(tidyr)}); options(width=250, dplyr.print_max=300)
d <- "data/state_elections/final/"
w <- readRDS(paste0(d,"ltw_wkr_unharm.rds")); wl <- readRDS(paste0(d,"ltw_wkr_unharm_long.rds"))
meta <- c("flag_no_valid_votes","flag_naive_turnout_above_1","flag_wkr_boundaries_recomputed","state","election_year","election_date","wkr_nr","wkr_name","stimme","eligible_voters","number_voters","valid_votes","invalid_votes","turnout")
pc <- setdiff(names(w), meta); cat("wide party cols:",length(pc),"; non-numeric:", pc[!sapply(w[pc],is.numeric)], "\n")
cat("wide: other & cdu_csu present?", c("other","cdu_csu") %in% pc, "\n")
cat("long parties:", n_distinct(wl$party), " long party not in wide:", setdiff(unique(wl$party), pc), " wide parties absent in long:", length(setdiff(pc,unique(wl$party))), head(setdiff(pc,unique(wl$party))), "\n")
# key
cat("wide rows",nrow(w),"; wide key unique rows:", nrow(distinct(w[c("state","election_year","wkr_nr","stimme")])), "\n")
cat("long wkr-stimme combos:", nrow(distinct(wl[c("state","election_year","wkr_nr","stimme")])), "\n")
# long vote_share = votes/valid
z <- wl %>% filter(!is.na(votes)); cat("long votes/valid vs vote_share max diff:", max(abs(z$votes/z$valid_votes - z$vote_share),na.rm=T), "\n")
# wide -> long compare
lw <- w %>% pivot_longer(all_of(pc), names_to="party", values_to="share_w") %>% filter(!is.na(share_w))
cat("wide non-NA party cells:", nrow(lw), " long non-NA vote_share cells:", sum(!is.na(wl$vote_share)), " long NA rows:", sum(is.na(wl$vote_share)), "\n")
j <- inner_join(lw %>% select(state,election_year,wkr_nr,stimme,party,share_w,valid_votes), wl %>% select(state,election_year,wkr_nr,stimme,party,vote_share,votes), by=c("state","election_year","wkr_nr","stimme","party"))
cat("joined",nrow(j),"; share diff>1e-8:", sum(abs(j$share_w-j$vote_share)>1e-8,na.rm=T), " ; votes vs share*valid diff>1:", sum(abs(j$votes-j$share_w*j$valid_votes)>1,na.rm=T), "\n")
cat("wide cells absent in long:", nrow(anti_join(lw, wl, by=c("state","election_year","wkr_nr","stimme","party"))), "\n")
x <- wl %>% filter(is.na(vote_share)); print(x %>% count(state,election_year,sort=T) %>% head(5)); cat("NA rows party examples:", head(unique(x$party)), " share of NA rows from zero-vote recode? \n")
# wide: party share sums
S <- rowSums(w[pc], na.rm=TRUE); cat("wide share sums: min",min(S),"max",max(S), " <0.9:",sum(S<0.9)," >1.001:",sum(S>1.001),"\n")
print(w %>% mutate(S=S) %>% filter(S>1.001|S<0.97) %>% select(state,election_year,wkr_nr,stimme,S) %>% arrange(desc(S)) %>% head(10) %>% as.data.frame())
cat("long vote_share sums by wkr:\n"); ls <- wl %>% group_by(state,election_year,wkr_nr,stimme) %>% summarise(S=sum(vote_share,na.rm=T),.groups="drop"); print(summary(ls$S)); 
cat("turnout formula wide:", max(abs(w$turnout-w$number_voters/w$eligible_voters)),"; nv>ev:",sum(w$number_voters>w$eligible_voters),"; vv+iv!=nv (non BY/HH/HB) |d|>1:", sum(abs(w$valid_votes+w$invalid_votes-w$number_voters)>1 & !w$state %in% c("09","02","04")), "\n")
print(w %>% filter(abs(valid_votes+invalid_votes-number_voters)>1, !state %in% c("09","02","04")) %>% count(state,election_year,stimme) %>% as.data.frame())
