suppressMessages(library(dplyr))
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); h21 <- readRDS(paste0(d,"state_harm_21.rds")); h23 <- readRDS(paste0(d,"state_harm_23.rds")); h25 <- readRDS(paste0(d,"state_harm_25.rds"))
w <- readRDS(paste0(d,"ltw_wkr_unharm.rds")); wl <- readRDS(paste0(d,"ltw_wkr_unharm_long.rds"))
for (n in c("u","h21","h23","h25","w","wl")) {x<-get(n); cat("\n==",n,dim(x),"\n"); print(head(names(x),40)); }
cat("\nharm25 cols:\n"); print(names(h25))
cat("\nclasses\n"); print(sapply(h25[1:30], class))
