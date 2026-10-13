suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
n<-u[,.(k=uniqueN(ags_name),nm=paste(unique(ags_name)[1:2],collapse=" | ")),by=ags]
cat("ags with >1 ags_name:",sum(n$k>1),"of",nrow(n),"\n"); print(n[k>1][1:8])
print(u[grepl("Krfr|Kreisfreie",ags_name),.N,by=.(state,form=ifelse(grepl("^Krfr",ags_name),"Krfr.",ifelse(grepl("^Kreisfreie",ags_name),"Kreisfreie","other")))])
cat("ags_name NA/empty:",sum(is.na(u$ags_name)|u$ags_name==""),"\n")
cat("election_type char: umlaut forms:\n"); print(unique(u$election_type))
# README says election_type 'Buergermeisterwahl' ascii
cat("docs say ASCII 'Buergermeisterwahl'; data uses umlaut\n")
