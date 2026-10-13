suppressMessages(library(dplyr))
f <- list.files("data/crosswalks/final", pattern="rds$", full.names=TRUE)
for (x in f){ d <- readRDS(x); cat("\n==", basename(x), nrow(d), "x", ncol(d), "\n"); print(sapply(d, function(v) class(v)[1])); print(head(d,3)) }
