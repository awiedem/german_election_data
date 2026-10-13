suppressMessages(library(dplyr))
w <- readRDS("data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.rds")
u <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
print(w %>% filter(election_year>=2002) %>% group_by(election_year,stimme) %>% summarise(elig=sum(eligible_voters),nv=sum(number_voters),valid=sum(valid_votes),inv=sum(invalid_votes), spd=round(sum(spd*valid_votes,na.rm=T)),cdu=round(sum(cdu*valid_votes,na.rm=T)),csu=round(sum(csu*valid_votes,na.rm=T)),afd=round(sum(afd*valid_votes,na.rm=T)),.groups="drop") %>% as.data.frame)
print(u %>% filter(year>=2002) %>% group_by(year) %>% summarise(valid=sum(valid_votes),spd=round(sum(spd*number_voters,na.rm=T)),cdu=round(sum(cdu*number_voters,na.rm=T)),afd=round(sum(afd*number_voters,na.rm=T))) %>% as.data.frame)
# elected_party equal across stimme
e <- w %>% select(election_year,wkr_nr,stimme,elected_party) %>% tidyr::pivot_wider(names_from=stimme,values_from=elected_party)
cat("elected_party differs between stimme rows:", sum(e$erststimme!=e$zweitstimme),"\n")
print(table(e$erststimme, e$election_year))
