suppressMessages(library(dplyr))
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
print(summary(u[,c("cdu","spd","afd","turnout","eligible_voters","number_voters","valid_votes","cdu_csu","far_right")]))
print(u[1:3,c("ags","election_year","cdu","spd","turnout","number_voters","valid_votes","eligible_voters")])
print(class(u$ags)); print(table(nchar(u$ags)))
