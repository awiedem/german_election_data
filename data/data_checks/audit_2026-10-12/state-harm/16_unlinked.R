suppressMessages({library(dplyr);library(bundeslaendeR)}); options(width=250, dplyr.print_max=500)
bl <- as.data.frame(ltw_elections); lk <- as.data.frame(link_gerda) %>% mutate(election_date=as.Date(election_date))
x <- bl %>% mutate(election_date=as.Date(election_date)) %>% filter(election_date>=as.Date("1990-01-01"), party_vshare>=0.015) %>%
  left_join(lk %>% select(state, election_date, partyname_short, partyname_gerda), by=c("state","election_date","partyname_short"))
print(x %>% filter(is.na(partyname_gerda)) %>% select(state, election_date, partyname_short, party_vshare) %>% arrange(desc(party_vshare)) %>% as.data.frame())
