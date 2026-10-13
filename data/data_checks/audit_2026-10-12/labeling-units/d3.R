suppressMessages(library(dplyr))
mh <- readRDS("data/municipal_elections/final/municipal_harm.rds")
print(table(mh$election_year > 2021))
print(names(mh)); 
sh <- readRDS("data/state_elections/final/state_harm.rds"); 
print(sh |> filter(is.na(ags)) |> select(1:8) |> head(3)); print(table(is.na(sh$ags)))
x <- sh |> filter(!is.na(ags), election_year==1990) |> select(ags, election_date) |> head(3); print(x)
print(sh |> filter(!is.na(ags)) |> summarise(across(everything(), ~mean(is.na(.))))|> select(election_date, number_voters, eligible_voters, turnout))
