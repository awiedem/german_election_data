suppressMessages({library(dplyr);library(data.table)})
r <- readRDS("data/crosswalks/final/ags_crosswalks.rds"); c <- fread("data/crosswalks/final/ags_crosswalks.csv", colClasses="character")
i <- which(ifelse(is.na(r$ags_name),"",r$ags_name)!=ifelse(is.na(c$ags_name),"",c$ags_name)); print(data.frame(ags=r$ags[i],yr=r$year[i],rds=r$ags_name[i],csv=c$ags_name[i]))
print(head(c$ags)); print(unique(nchar(c$ags)))
for(f in c("ags_1990_to_2023_crosswalk","ags_1990_to_2025_crosswalk")){ r <- readRDS(sprintf("data/crosswalks/final/%s.rds",f)); c <- fread(sprintf("data/crosswalks/final/%s.csv",f), colClasses="character")
i <- which(ifelse(is.na(r$ags_name),"",r$ags_name)!=ifelse(is.na(c$ags_name),"",c$ags_name)); print(data.frame(ags=r$ags[i],yr=r$year[i],rds=r$ags_name[i],csv=c$ags_name[i]))}
