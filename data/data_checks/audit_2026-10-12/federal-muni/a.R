suppressMessages(library(dplyr))
d<-"data/federal_elections/municipality_level/final/"
for(n in c("raw","unharm","harm_21","harm_25")){x<-readRDS(paste0(d,"federal_muni_",n,".rds"));cat("\n==",n,dim(x),"\n");print(names(x));print(table(x$election_year))}
