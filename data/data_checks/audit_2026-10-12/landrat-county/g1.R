suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
s <- as_tibble(readRDS("data/county_elections/final/county_council_seats.rds"))
l <- as_tibble(readRDS("data/landrat_elections/final/landrat_unharm.rds"))
# latest decisive winner per county-year
w <- l%>%filter(round=="stichwahl"|!(paste(ags,election_year)%in%paste(ags[round=="stichwahl"],election_year[round=="stichwahl"])))%>%mutate(county=substr(ags,1,5))%>%select(county,election_date,winner_party)
out <- list()
for (yy in c(2010,2014,2018,2021)) {
 d <- w%>%filter(format(election_date,"%Y")<=yy)%>%group_by(county)%>%slice_max(election_date,n=1,with_ties=FALSE)%>%ungroup()
 out[[as.character(yy)]] <- s%>%filter(year==yy,county%in%d$county)%>%inner_join(d,by="county")%>%select(county,county_name,year,government_party,winner_party)
}
o <- bind_rows(out)
o$wp <- ifelse(grepl("^CSU|CSU",o$winner_party),"CDU",ifelse(grepl("^CDU",o$winner_party),"CDU",o$winner_party))
print(nrow(o)); print(table(o$government_party==o$wp, useNA="a"))
print(o%>%filter(government_party!=wp)%>%count(government_party,winner_party,sort=T)%>%head(25))
print(o%>%filter(government_party!=wp,winner_party %in% c("CDU","SPD","CSU"),government_party%in%c("CDU","SPD"))%>%head(20))
