suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
h<-readRDS("data/european_elections/final/european_muni_harm.rds")
print(u%>%mutate(raw=number_voters/eligible_voters)%>%filter(raw>1.05)%>%select(ags,election_year,eligible_voters,number_voters,raw)%>%arrange(desc(raw))%>%head(8))
print(h%>%filter(abs(valid_votes+invalid_votes-number_voters)>1)%>%mutate(d=valid_votes+invalid_votes-number_voters)%>%summarise(n=n(),max=max(abs(d))))
# small municipalities
print(u%>%filter(eligible_voters<10)%>%count(election_year,state))
# RLP / NI 'r' outliers by state
pc<-names(u)[which(names(u)=="abg"):which(names(u)=="x50plus")]
u$r<-rowSums(u[pc],na.rm=T)*u$number_voters/u$valid_votes
print(u%>%filter(abs(r-1)>0.02)%>%count(state,election_year)%>%tidyr::pivot_wider(names_from=election_year,values_from=n))
print(u%>%filter(abs(r-1)>0.02)%>%group_by(election_year)%>%summarise(med_valid=median(valid_votes),max_valid=max(valid_votes),lost=sum((1-r)*valid_votes)))
# sum of party shares > 1.0 + tol
print(sum(u$r>1.02))
