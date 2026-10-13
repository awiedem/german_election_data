suppressMessages({library(dplyr);library(tidyr)}); options(width=250, dplyr.print_max=300)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); w <- readRDS(paste0(d,"ltw_wkr_unharm.rds"))
P <- c("spd","cdu","csu","fdp","gruene","linke_pds","afd","other")
sh <- function(x, key) x %>% group_by(across(all_of(key))) %>% summarise(across(any_of(P), ~ 100*sum(.x*valid_votes,na.rm=T)/sum(valid_votes,na.rm=T)), .groups="drop") %>% pivot_longer(any_of(P), names_to="party", values_to="pct")
a <- sh(u, c("state","election_year")); b <- sh(w %>% filter(stimme %in% c("zweitstimme","einzelstimme")), c("state","election_year","stimme"))
m <- inner_join(a,b,by=c("state","election_year","party"), suffix=c("_u","_w")) %>% mutate(dev=pct_w-pct_u)
cat("compared cells", nrow(m), "; >0.5pp:", sum(abs(m$dev)>0.5,na.rm=T), "\n")
print(m %>% filter(abs(dev)>0.5) %>% mutate(across(where(is.numeric),~round(.x,2))) %>% arrange(desc(abs(dev))) %>% as.data.frame())
