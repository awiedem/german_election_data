suppressMessages(library(dplyr))
for (n in c("state_unharm","state_harm_21","state_harm_25")) { s <- readRDS(paste0("data/state_elections/final/",n,".rds"))
 cat(n, " dup (ags,election_year):", sum(duplicated(s[,c("ags","election_year")])), " dup (ags,election_year,election_date):", sum(duplicated(s[,c("ags","election_year","election_date")])), "\n")
 print(s |> filter(state=="02", election_year==1982) |> select(ags, election_year, election_date, valid_votes) ) }
