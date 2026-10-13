suppressMessages({library(dplyr);library(tidyr);library(bundeslaendeR)}); options(width=260, dplyr.print_max=500)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); h25 <- readRDS(paste0(d,"state_harm_25.rds"))
bl <- as.data.frame(ltw_elections); lk <- as.data.frame(link_gerda)
cat("link cols:",names(lk),"\n")
# party-level
stat <- function(x){ x %>% group_by(state, election_date) %>% group_modify(~{
   vv <- sum(.x$valid_votes,na.rm=TRUE); pc <- setdiff(names(.x), c("ags","election_year","election_date","state","state_name","ags_name","eligible_voters","number_voters","valid_votes","invalid_votes","turnout"))
   pc <- pc[sapply(.x[pc], is.numeric)]
   w <- sapply(pc, function(p){ if(all(is.na(.x[[p]]))) NA_real_ else sum(.x[[p]]*.x$valid_votes,na.rm=TRUE)/vv})
   data.frame(party=pc, share=w)}) %>% ungroup() }
su <- stat(u); sh <- stat(h25)
bl2 <- bl %>% select(state_bl=state, election_date, partyname_short, party_vshare, party_vote_count)
m <- lk %>% mutate(election_date=as.Date(election_date)) %>% left_join(bl2 %>% mutate(election_date=as.Date(election_date)), by=c("state"="state_bl","election_date","partyname_short"))
m <- m %>% left_join(su %>% rename(state_gerda=state, partyname_gerda=party, share_u=share), by=c("state_gerda","election_date"="election_date","partyname_gerda")) 
m <- m %>% left_join(sh %>% rename(state_gerda=state, partyname_gerda=party, share_h=share), by=c("state_gerda","election_date","partyname_gerda"))
m <- m %>% mutate(dev_u=100*(share_u-party_vshare), dev_h=100*(share_h-party_vshare))
saveRDS(m, file.path("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/state-harm","bl_party.rds"))
cat("link rows",nrow(lk)," with bl share:",sum(!is.na(m$party_vshare)), " with gerda share u:",sum(!is.na(m$share_u)), "\n")
cat("\nrows where bl has share but GERDA share NA (party missing in GERDA), year>=1990:\n")
print(m %>% filter(!is.na(party_vshare), is.na(share_u), election_date>=as.Date("1990-01-01")) %>% select(state_gerda,election_date,partyname_gerda,partyname_short,party_vshare) %>% arrange(desc(party_vshare)) %>% head(60) %>% as.data.frame())
cat("\n|dev|>0.5pp (unharm), year>=1990:\n")
print(m %>% filter(abs(dev_u)>0.5, election_date>=as.Date("1990-01-01")) %>% select(state_gerda,election_date,partyname_gerda,partyname_short,share_u,party_vshare,dev_u,dev_h) %>% arrange(desc(abs(dev_u))) %>% mutate(across(where(is.numeric),~round(.x,3))) %>% as.data.frame())
