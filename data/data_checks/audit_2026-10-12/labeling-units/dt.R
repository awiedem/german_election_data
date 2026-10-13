suppressMessages(library(dplyr))
f <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds"); print(f |> distinct(year, election_date) |> arrange(year) |> as.data.frame() |> tail(25))
m <- readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds"); print(m |> distinct(election_year, election_date) |> arrange(election_year) |> as.data.frame())
e <- readRDS("data/european_elections/final/european_muni_unharm.rds"); print(e |> distinct(election_year, election_date))
w <- readRDS("data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.rds"); print(w |> distinct(election_year, election_date) |> arrange(election_year))
# state: multiple dates per state-year?
s <- readRDS("data/state_elections/final/state_unharm.rds"); x <- s |> distinct(state, election_year, election_date) |> count(state, election_year) |> filter(n>1); print(x)
mu <- readRDS("data/municipal_elections/final/municipal_unharm.rds"); x <- mu |> distinct(state, election_year, election_date) |> count(state, election_year) |> filter(n>1); print(head(x,10)); print(nrow(x))
