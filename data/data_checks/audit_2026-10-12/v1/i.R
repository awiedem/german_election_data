suppressMessages(library(dplyr))
s <- readRDS("data/state_elections/final/state_unharm.rds"); 
for (a in c("12062372","12065272","12064192")) print(s |> filter(ags==a, state=="12") |> select(ags,election_year,eligible_voters) |> arrange(election_year) |> as.data.frame())
sn <- s |> filter(state=="14", election_year %in% c(1999,2004)) |> group_by(election_year) |> summarise(n=n(), ev=sum(eligible_voters,na.rm=T)); print(sn)
w <- readRDS("data/state_elections/final/ltw_wkr_unharm.rds"); print(w |> filter(state=="14", election_year==2004) |> names() |> head(12))
