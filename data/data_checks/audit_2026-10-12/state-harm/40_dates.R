suppressMessages({library(dplyr);library(bundeslaendeR)}); options(width=250)
bl <- as.data.frame(ltw_elections); print(unique(bl[bl$state=="HB" & format(as.Date(bl$election_date),"%Y") %in% c("1971","1975","1979"),c("state","election_date")]))
u <- readRDS("data/state_elections/final/state_unharm.rds"); print(u %>% filter(state=="04", election_year %in% c(1971,1975,1979)) %>% distinct(election_year,election_date))
print(u %>% filter(state=="07") %>% distinct(election_year) %>% tail(3)); w <- readRDS("data/state_elections/final/ltw_wkr_unharm.rds"); print(w %>% filter(state=="07", election_year==2026) %>% distinct(election_date))
# weekday check: all dates Sundays?
x <- u %>% distinct(state, election_year, election_date); x$wd <- weekdays(x$election_date); print(table(x$wd)); print(x %>% filter(wd!="Sunday") %>% as.data.frame())
