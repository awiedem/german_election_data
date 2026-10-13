suppressMessages({library(dplyr);library(tidyr)}); options(width=250)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); h25 <- readRDS(paste0(d,"state_harm_25.rds")); h21<-readRDS(paste0(d,"state_harm_21.rds")); h23<-readRDS(paste0(d,"state_harm_23.rds"))
sc <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character")
print(table(sc$dataset, sc$role))
for (ds in unique(sc$dataset)) { x<-sc[sc$dataset==ds,]; cat(ds, "roles for other/cdu_csu/far_*/total_vote_share:\n"); print(x[x$column %in% c("other","cdu_csu","far_right","far_left","far_left_w_linke","total_vote_share","dg_bhe","llsh","uwg","domowina"),c("column","role")]) }
