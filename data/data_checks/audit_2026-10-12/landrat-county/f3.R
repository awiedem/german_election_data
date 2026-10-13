suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
s <- as_tibble(readRDS("data/county_elections/final/county_council_seats.rds"))
print(s%>%filter(county=="11000")%>%select(year,seats_total,seats_spd,seats_cdu_csu,seats_fdp,seats_gruene,seats_linke_pds,seats_afd,seats_other,government_party,comment,source),n=20)
print(s%>%filter(county=="16052")%>%select(year,seats_total,seats_spd,seats_cdu_csu,seats_afd,seats_other,seats_regional,comment),n=20)
print(s%>%filter(county=="02000",year%in%c(2008,2011,2012,2015,2020,2025))%>%select(year,seats_total,seats_spd,seats_cdu_csu,seats_afd,seats_other,source))
# AfD before 2013 anywhere
print(s%>%filter(year<2014,seats_afd>0)%>%select(county,county_name,year,seats_afd,seats_total))
# Piraten etc ok. Cities with "kreisfreie" in 2008 and seats vs county_elec: compare seats_total to numbers? skip
# county codes in seats not in county_elec harm cty
hc <- readRDS("data/county_elections/final/county_elec_harm_21_cty.rds")
print(setdiff(unique(s$county),unique(hc$county_code)))
print(length(setdiff(unique(hc$county_code),unique(s$county)))); print(setdiff(unique(hc$county_code),unique(s$county)))
