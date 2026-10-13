suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
print(names(u)); str(u[,1:12])
print(table(u$state, u$election_year))
