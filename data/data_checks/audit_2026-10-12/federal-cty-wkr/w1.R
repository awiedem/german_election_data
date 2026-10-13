suppressMessages(library(dplyr))
w <- readRDS("data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.rds")
cat("dup key:", sum(duplicated(w[,c("election_year","wkr_nr","stimme")])),"\n")
print(table(w$election_year, w$stimme))
print(w %>% filter(stimme=="erststimme") %>% count(election_year))
print(w %>% distinct(election_year,election_date))
print(table(nchar(w$wkr_nr))); cat("state mismatch? wkr_nr range by state\n")
print(w %>% filter(election_year==2021,stimme=="erststimme") %>% group_by(state,state_name) %>% summarise(n=n(),mn=min(wkr_nr),mx=max(wkr_nr)) %>% as.data.frame)
print(table(w$flag_no_valid_votes, w$flag_naive_turnout_above_1))
print(w %>% filter(flag_no_valid_votes==1|flag_naive_turnout_above_1==1) %>% select(election_year,wkr_nr,wkr_name,stimme,eligible_voters,number_voters,valid_votes,turnout) %>% head(20) %>% as.data.frame)
# identities
w$ch2 <- w$number_voters - w$valid_votes - w$invalid_votes
print(w %>% filter(abs(ch2)>0 | number_voters>eligible_voters) %>% select(election_year,wkr_nr,stimme,eligible_voters,number_voters,valid_votes,invalid_votes) %>% head(20) %>% as.data.frame)
cat("turnout summary by stimme/year\n")
print(w %>% group_by(election_year,stimme) %>% summarise(tmin=min(turnout,na.rm=T),tmax=max(turnout,na.rm=T),nNA=sum(is.na(turnout)),.groups="drop") %>% as.data.frame)
pc <- names(w)[which(names(w)=="50plus"):which(names(w)=="other")]
w$sh <- rowSums(w[,pc],na.rm=T)
print(w %>% group_by(election_year,stimme) %>% summarise(smin=min(sh),smed=median(sh),smax=max(sh),.groups="drop") %>% as.data.frame)
cat("elected_party by stimme\n"); print(table(w$stimme, !is.na(w$elected_party)))
