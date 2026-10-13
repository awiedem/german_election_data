suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
s <- as_tibble(readRDS("data/county_elections/final/county_council_seats.rds"))
str(s)
print(s%>%count(county,year)%>%filter(n>1)%>%nrow()); print(table(nchar(s$county))); print(sum(substr(s$county,1,2)!=s$state))
print(s%>%count(state,state_name))
print(table(s$county_type))
seatc <- grep("^seats_",names(s),value=T); print(seatc)
party <- c("seats_spd","seats_cdu_csu","seats_fdp","seats_gruene","seats_freie_wahler","seats_linke_pds","seats_afd","seats_regional","seats_other")
s$sm <- rowSums(s[party],na.rm=T)
print(colSums(is.na(s[seatc])))
print(s%>%filter(!is.na(seats_total),sm!=seats_total)%>%select(county,county_name,year,seats_total,sm))
print(table(s$flag_seats_total_incongruent,useNA="a"))
print(s%>%filter(!is.na(seats_total),sm!=seats_total)%>%distinct(county,county_name,seats_total,sm))
print(summary(s$seats_total)); 
print(s%>%filter(seats_total<10|seats_total>130)%>%distinct(county,county_name,seats_total))
# local_other
print(sum(s$seats_local_other!=s$seats_freie_wahler+s$seats_regional+s$seats_other,na.rm=T))
# NA seats_total
x <- s%>%filter(is.na(seats_total)); print(x%>%count(state,year)%>%tidyr::pivot_wider(names_from=year,values_from=n))
print(x%>%filter(state!="13")%>%count(county,county_name)%>%head(20))
# NA vs 0: rows where total NA, are party seats NA?
print(x%>%summarise(across(all_of(party),~sum(is.na(.)))))
