suppressMessages({library(dplyr)}); options(width=250)
u <- readRDS("data/state_elections/final/state_unharm.rds")
x <- u %>% filter(ags=="08337097", election_year==1956); nz <- names(x)[sapply(x, function(v) is.numeric(v) && !is.na(v) && v>0)]; print(as.data.frame(x[nz]))
y <- u %>% filter(state=="08", election_year==1956); cat("n BW 1956:",nrow(y),"\n"); print(summary(y$valid_votes))
