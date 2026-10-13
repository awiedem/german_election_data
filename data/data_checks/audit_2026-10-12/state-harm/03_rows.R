suppressMessages(library(dplyr)); options(width=250)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds"))
H <- list(u=u,h21=readRDS(paste0(d,"state_harm_21.rds")),h23=readRDS(paste0(d,"state_harm_23.rds")),h25=readRDS(paste0(d,"state_harm_25.rds")))
schema <- read.csv("data/state_elections/metadata/column_schema.csv", colClasses="character")
for (n in names(H)) {
  x <- H[[n]]; cat("\n######", n, nrow(x), "\n")
  # keys
  key <- if (n=="u") c("ags","election_year") else c("ags","election_year")
  cat("dup keys:", sum(duplicated(x[key])), "\n")
  if (n=="u") print(x %>% filter(duplicated(x[key])|duplicated(x[key],fromLast=TRUE)) %>% count(state,election_year) %>% head(10))
  cat("ags nchar:"); print(table(nchar(x$ags)))
  cat("state prefix mismatch:", sum(substr(x$ags,1,2)!=x$state), "\n")
  cat("election_year class", class(x$election_year), " year of date mismatch:", sum(as.integer(format(as.Date(x$election_date),"%Y"))!=x$election_year, na.rm=TRUE), " NA date:", sum(is.na(x$election_date)), "\n")
  cat("ev<nv:", sum(x$eligible_voters<x$number_voters,na.rm=TRUE), " nv<vv(not BY/HH/HB):", sum(x$number_voters<x$valid_votes & !x$state %in% c("09","02","04"),na.rm=TRUE),"\n")
  cat("turnout>1:", sum(x$turnout>1,na.rm=TRUE), " turnout<0:", sum(x$turnout<0,na.rm=TRUE), " turnout NA:", sum(is.na(x$turnout)), "\n")
  cat("turnout NA by state-year:\n"); print(x %>% filter(is.na(turnout)) %>% count(state,election_year) %>% as.data.frame())
}
