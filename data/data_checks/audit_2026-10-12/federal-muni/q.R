suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
for(n in c("unharm","harm_21","harm_25","raw")){u<-readRDS(paste0(d,"federal_muni_",n,".rds"));cat("\n==",n,"\n")
 print(names(u)[grepl("name|date|pop|area|state|county",names(u))])
 if("ags_name"%in%names(u)) cat("ags_name NA:",sum(is.na(u$ags_name)),"of",nrow(u),"\n")
 if("state_name"%in%names(u)) {cat("state_name NA:",sum(is.na(u$state_name)),"\n"); print(table(u$state_name))}
 print(u|>group_by(election_year)|>summarise(dates=paste(unique(as.character(election_date)),collapse=","))|>as.data.frame())
 print(class(u$election_date))
 for(v in intersect(c("pop","area","population"),names(u))) {cat(v,"\n");print(summary(u[[v]]))}
}
