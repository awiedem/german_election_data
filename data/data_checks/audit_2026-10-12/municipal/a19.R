suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
options(width=250)
pcs <- c("cdu_csu","spd","linke_pds","gruene","fdp","afd","freie_wahler","piraten","die_partei","bsw")
# fraction NA by state-year for each party; flag state-years where party is 100% NA but was non-NA in both adjacent election years of that state
long <- u %>% filter(!state %in% c("02","04","11")) %>% group_by(state,election_year) %>% summarise(across(all_of(pcs),~mean(is.na(.))),n=n(),.groups="drop") %>% filter(n>40)
out <- list()
for(p in pcs){ x <- long %>% arrange(state,election_year) %>% group_by(state) %>% mutate(prv=lag(.data[[p]]),nxt=lead(.data[[p]])) %>% ungroup() %>% filter(.data[[p]]>0.97, (!is.na(prv)&prv<0.9) | (!is.na(nxt)&nxt<0.9)) %>% transmute(state,election_year,party=p,na=.data[[p]],prv,nxt,n); out[[p]] <- x }
print(as.data.frame(bind_rows(out)),digits=2)
# BB 2014 cdu
print(u %>% filter(state=="12",election_year==2014) %>% summarise(cduNA=mean(is.na(cdu_csu)),spdNA=mean(is.na(spd)),seats_gruene_pos=sum(seats_gruene>0,na.rm=T),seats_cdu_pos=sum(seats_cdu_csu>0,na.rm=T),otherMean=mean(other,na.rm=T)))
print(u %>% filter(state=="12",election_year %in% c(2008,2019)) %>% group_by(election_year) %>% summarise(otherMean=mean(other,na.rm=T), cduNA=mean(is.na(cdu_csu))))
