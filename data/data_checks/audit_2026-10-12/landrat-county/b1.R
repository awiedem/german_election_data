suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
u <- as_tibble(readRDS("data/county_elections/final/county_elec_unharm.rds"))
cat(dim(u),"\n"); print(names(u)[1:50]); 
nm <- names(u); cat(length(nm),"\n")
print(sapply(u[1:40],class))
print(table(u$state,u$election_year))
