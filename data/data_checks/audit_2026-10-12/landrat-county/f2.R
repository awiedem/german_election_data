suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
s <- as_tibble(readRDS("data/county_elections/final/county_council_seats.rds"))
print(sort(table(s$government_party,useNA="a"),decreasing=T))
print(s%>%filter(state%in%c("02","04","11"),year%in%c(2008,2015,2025))%>%select(county,county_name,county_type,year,seats_total,seats_spd,seats_cdu_csu,comment))
print(s%>%filter(!is.na(comment))%>%count(comment)%>%arrange(-n)%>%head(20))
# jumps in seats_total
j <- s%>%arrange(county,year)%>%group_by(county)%>%mutate(d=seats_total-lag(seats_total))%>%filter(!is.na(d),d!=0)%>%ungroup(); print(nrow(j)); print(j%>%filter(abs(d)>=10)%>%select(county,county_name,year,seats_total,d),n=40)
# year consistency: government_party NA by year
print(s%>%group_by(year)%>%summarise(naGov=sum(is.na(government_party)),naTot=sum(is.na(seats_total))))
# 2025 NRW: vs last_checked
print(table(s$last_checked,useNA="a"))
# party seats > total?
party <- c("seats_spd","seats_cdu_csu","seats_fdp","seats_gruene","seats_freie_wahler","seats_linke_pds","seats_afd","seats_regional","seats_other")
# AfD seats before 2013
print(s%>%filter(year<2013,seats_afd>0)%>%select(county,county_name,year,seats_afd)%>%head())
# Linke in West before 2008
# seats_total zero rows
print(s%>%filter(seats_total==0|(seats_total>0&rowSums(s[party])==0))%>%nrow())
# counties whose party composition identical across all years 2008-2025
x <- s%>%group_by(county)%>%summarise(nd=n_distinct(paste(seats_spd,seats_cdu_csu,seats_afd,seats_total)),.groups="drop"); print(table(x$nd))
